# ============================================================================
# R/cohorts.R -- turning the YAML cohort definitions into SQL and materialising
# them into a cohort table.
#
# WHY HAND-WRITTEN SQL RATHER THAN CIRCE/ATLAS JSON
# ATLAS produces cohort definitions as Circe JSON, and CohortGenerator runs
# them. That is the right choice for a network study, where definitions must be
# shared and rendered identically everywhere. It is the wrong choice here for
# two reasons: Circe JSON is not readable by a human reviewer, and the point of
# this project is that the definition is legible. So the definition lives in
# YAML, this file compiles it to OHDSI SQL, and SqlRender translates that to
# whatever dialect the CDM happens to speak.
#
# The compiled SQL is written to output/cohorts/ for every cohort, so the
# generated code is auditable rather than a black box.
#
# STRUCTURE
# Each cohort is built as a chain of CTEs, one per criterion, in a fixed order:
#
#   entry_events -> index_event -> [inclusion_*] -> [exclusion_*] -> cohort
#
# Because each stage is a named CTE, subject counts can be taken at every stage
# using the same SQL, which is what produces the attrition table. Attrition is
# not decoration: it is how a reader sees which criterion actually did the
# work, and it is the first thing to check when a cohort comes out the wrong
# size.
# ============================================================================

# ----------------------------------------------------------------------------
# Domain metadata -- which columns hold the concept and the dates
# ----------------------------------------------------------------------------
domain_columns <- function(domain) {
  spec <- cdm_event_tables()
  row <- spec[spec$table == domain, ]
  assert(nrow(row) == 1, "Unsupported domain: ", domain)
  list(
    table       = row$table,
    concept_col = row$concept_col,
    start_col   = row$start_col,
    # Tables without an end date (procedures, measurements) are point events;
    # treat the start date as the end date.
    end_col     = if (is.na(row$end_col)) row$start_col else row$end_col
  )
}

# ----------------------------------------------------------------------------
# SQL builders for the individual stages
# ----------------------------------------------------------------------------

#' All events matching the entry concept set.
sql_entry_events <- function(cohort, resolved_sets, schema) {
  d <- domain_columns(cohort$entry$domain)
  ids <- ids_for_sql(resolved_sets[[cohort$entry$concept_set]])
  sprintf(
    "  SELECT e.person_id,
          e.%s AS event_date,
          e.%s AS event_end_date
   FROM %s.%s e
   WHERE e.%s IN (%s)",
    d$start_col, d$end_col, schema, d$table, d$concept_col, ids)
}

#' Reduce to one index event per person.
#'
#' first_occurrence_only: TRUE is the new-user design. FALSE would keep every
#' qualifying event, which means one person can enter repeatedly -- a
#' prevalent-user or episode-level design, and a different study.
sql_index_event <- function(cohort) {
  if (isTRUE(cohort$entry$first_occurrence_only)) {
    "  SELECT person_id, MIN(event_date) AS cohort_start_date
   FROM entry_events
   GROUP BY person_id"
  } else {
    "  SELECT person_id, event_date AS cohort_start_date
   FROM entry_events"
  }
}

#' Prior/post observation requirement.
#'
#' Evaluated against OBSERVATION_PERIOD. The index date must sit inside a
#' single continuous period that began at least `days_before` earlier. Using
#' one period rather than the union of several is deliberate: a gap in
#' enrolment is a gap in what the data can see, and a covariate window that
#' spans it is measuring nothing for part of its length.
sql_prior_observation <- function(prev, criterion, schema) {
  days_before <- criterion$days_before %||% 0
  days_after  <- criterion$days_after  %||% 0
  sprintf(
    "  SELECT p.person_id, p.cohort_start_date
   FROM %s p
   INNER JOIN %s.observation_period op
     ON op.person_id = p.person_id
    AND p.cohort_start_date >= op.observation_period_start_date
    AND p.cohort_start_date <= op.observation_period_end_date
   WHERE DATEDIFF(DAY, op.observation_period_start_date, p.cohort_start_date) >= %d
     AND DATEDIFF(DAY, p.cohort_start_date, op.observation_period_end_date) >= %d",
    prev, schema, as.integer(days_before), as.integer(days_after))
}

#' Age at index.
#'
#' Computed as year(index) - year_of_birth. That is the OHDSI convention and is
#' what the vocabulary supports reliably: month and day of birth are frequently
#' missing or masked for disclosure control, so a precise age is not generally
#' available. The cost is up to a year of misclassification at the boundary,
#' which is immaterial for an 18-year threshold and would matter for a study of
#' neonates.
sql_age <- function(prev, criterion, schema) {
  conds <- character(0)
  if (!is.null(criterion$min_value))
    conds <- c(conds, sprintf("(YEAR(p.cohort_start_date) - pe.year_of_birth) >= %d",
                              as.integer(criterion$min_value)))
  if (!is.null(criterion$max_value))
    conds <- c(conds, sprintf("(YEAR(p.cohort_start_date) - pe.year_of_birth) <= %d",
                              as.integer(criterion$max_value)))
  where <- if (length(conds)) paste("WHERE", paste(conds, collapse = "\n     AND ")) else ""
  sprintf(
    "  SELECT p.person_id, p.cohort_start_date
   FROM %s p
   INNER JOIN %s.person pe ON pe.person_id = p.person_id
   %s", prev, schema, where)
}

#' Exclusion on the presence of a concept set before index.
#'
#' `relation` decides whether an event ON the index date counts:
#'   before_index        -- strictly earlier (event_date < index)
#'   on_or_before_index  -- earlier or same day (event_date <= index)
#' Stated explicitly in the config because the two give different cohorts and
#' the right answer depends on the criterion. For a prior-outcome washout,
#' same-day events are ambiguous and are better handled by the time-at-risk
#' start; for concomitant exposure, a same-day start is exactly what you want
#' to exclude.
sql_exclude_concept_set <- function(prev, criterion, resolved_sets, schema) {
  d <- domain_columns(criterion$domain)
  ids <- ids_for_sql(resolved_sets[[criterion$concept_set]])
  op <- switch(criterion$relation %||% "on_or_before_index",
               "before_index" = "<", "on_or_before_index" = "<=",
               stop("Unknown relation '", criterion$relation, "' in criterion '",
                    criterion$name, "'. Use before_index or on_or_before_index.",
                    call. = FALSE))
  lookback <- if (is.null(criterion$lookback_days)) "" else
    sprintf("\n        AND x.%s >= DATEADD(DAY, %d, p.cohort_start_date)",
            d$start_col, -as.integer(criterion$lookback_days))

  sprintf(
    "  SELECT p.person_id, p.cohort_start_date
   FROM %s p
   WHERE NOT EXISTS (
     SELECT 1 FROM %s.%s x
      WHERE x.person_id = p.person_id
        AND x.%s IN (%s)
        AND x.%s %s p.cohort_start_date%s
   )",
    prev, schema, d$table, d$concept_col, ids, d$start_col, op, lookback)
}

#' Cohort exit -- attach cohort_end_date.
sql_cohort_exit <- function(prev, cohort, resolved_sets, schema) {
  rule <- cohort$exit$rule

  if (identical(rule, "same_day")) {
    return(sprintf(
      "  SELECT person_id, cohort_start_date, cohort_start_date AS cohort_end_date
   FROM %s", prev))
  }

  if (identical(rule, "end_of_observation_period")) {
    return(sprintf(
      "  SELECT p.person_id, p.cohort_start_date, op.observation_period_end_date AS cohort_end_date
   FROM %s p
   INNER JOIN %s.observation_period op
     ON op.person_id = p.person_id
    AND p.cohort_start_date >= op.observation_period_start_date
    AND p.cohort_start_date <= op.observation_period_end_date", prev, schema))
  }

  if (identical(rule, "fixed_days")) {
    return(sprintf(
      "  SELECT person_id, cohort_start_date,
          DATEADD(DAY, %d, cohort_start_date) AS cohort_end_date
   FROM %s", as.integer(cohort$exit$days), prev))
  }

  if (identical(rule, "end_of_continuous_exposure")) {
    # Drug-era construction: collapse successive exposures into a continuous
    # episode, allowing gaps of up to persistence_window_days.
    #
    # This is the classic gaps-and-islands problem. An exposure starts a NEW
    # era if it begins after the furthest end date reached by any earlier
    # exposure for that person, padded by the persistence window. A running
    # sum of that flag numbers the eras; grouping by it gives their extents.
    #
    # The running maximum matters: exposures can be nested (a 90-day supply
    # followed by a 7-day supply that ends earlier), so comparing only against
    # the immediately preceding row would split an era incorrectly.
    d <- domain_columns(cohort$entry$domain)
    ids <- ids_for_sql(resolved_sets[[cohort$entry$concept_set]])
    gap <- as.integer(cohort$exit$persistence_window_days)
    return(sprintf(
      "  SELECT p.person_id, p.cohort_start_date, e.era_end_date AS cohort_end_date
   FROM %s p
   INNER JOIN (
     SELECT person_id, MIN(exp_start) AS era_start_date, MAX(exp_end) AS era_end_date
     FROM (
       SELECT person_id, exp_start, exp_end,
              SUM(is_new_era) OVER (PARTITION BY person_id ORDER BY exp_start
                                    ROWS UNBOUNDED PRECEDING) AS era_number
       FROM (
         SELECT person_id, exp_start, exp_end,
                CASE WHEN prior_max_end IS NULL
                       OR exp_start > DATEADD(DAY, %d, prior_max_end)
                     THEN 1 ELSE 0 END AS is_new_era
         FROM (
           SELECT person_id,
                  %s AS exp_start,
                  %s AS exp_end,
                  MAX(%s) OVER (PARTITION BY person_id ORDER BY %s
                                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
                    AS prior_max_end
           FROM %s.%s
           WHERE %s IN (%s)
         ) t1
       ) t2
     ) t3
     GROUP BY person_id, era_number
   ) e
     ON e.person_id = p.person_id
    AND e.era_start_date = p.cohort_start_date",
      prev, gap, d$start_col, d$end_col, d$end_col, d$start_col,
      schema, d$table, d$concept_col, ids))
  }

  stop("Unsupported exit rule: ", rule, call. = FALSE)
}

# ----------------------------------------------------------------------------
# Assembling the CTE chain
# ----------------------------------------------------------------------------

#' Build the ordered list of named CTEs for one cohort.
#'
#' @return list of list(name, label, sql), in evaluation order
build_cohort_stages <- function(cohort, resolved_sets, schema) {
  stages <- list()
  add <- function(name, label, sql) {
    stages[[length(stages) + 1]] <<- list(name = name, label = label, sql = sql)
  }

  add("entry_events", "All events matching the entry concept set",
      sql_entry_events(cohort, resolved_sets, schema))

  add("index_event",
      if (isTRUE(cohort$entry$first_occurrence_only))
        "First occurrence per person (new-user design)"
      else "All occurrences (prevalent-user design)",
      sql_index_event(cohort))

  prev <- "index_event"
  i <- 0L
  for (cr in cohort$inclusion_criteria) {
    i <- i + 1L
    nm <- sprintf("inc_%02d_%s", i, cr$name)
    sql <- switch(
      cr$name,
      "prior_observation"       = sql_prior_observation(prev, cr, schema),
      "observation_after_index" = sql_prior_observation(prev, cr, schema),
      "age_at_index"            = sql_age(prev, cr, schema),
      stop("Unknown inclusion criterion '", cr$name, "'. Add a builder in ",
           "R/cohorts.R if this is intentional.", call. = FALSE)
    )
    add(nm, describe_criterion(cr, "include"), sql)
    prev <- nm
  }

  i <- 0L
  for (cr in cohort$exclusion_criteria) {
    i <- i + 1L
    nm <- sprintf("exc_%02d_%s", i, cr$name)
    add(nm, describe_criterion(cr, "exclude"),
        sql_exclude_concept_set(prev, cr, resolved_sets, schema))
    prev <- nm
  }

  add("cohort_final", paste0("Cohort exit: ", cohort$exit$rule),
      sql_cohort_exit(prev, cohort, resolved_sets, schema))

  stages
}

#' Human-readable description of a criterion, for the attrition table.
describe_criterion <- function(cr, direction) {
  switch(
    cr$name,
    "prior_observation" = sprintf("Require %d days of prior continuous observation",
                                  cr$days_before %||% 0),
    "observation_after_index" = sprintf("Require %d day(s) of observation after index",
                                        cr$days_after %||% 0),
    "age_at_index" = sprintf("Age at index %s%s",
                             if (!is.null(cr$min_value)) paste0(">= ", cr$min_value) else "",
                             if (!is.null(cr$max_value)) paste0(", <= ", cr$max_value) else ""),
    sprintf("%s: %s in '%s' %s%s",
            if (direction == "exclude") "Exclude" else "Require",
            cr$name, cr$concept_set,
            if (identical(cr$relation %||% "on_or_before_index", "before_index"))
              "before index" else "on or before index",
            if (is.null(cr$lookback_days)) " (all history)"
            else sprintf(" (%d-day lookback)", cr$lookback_days))
  )
}

#' Compose the WITH clause from a list of stages, up to and including `upto`.
compose_with <- function(stages, upto = NULL) {
  if (is.null(upto)) upto <- stages[[length(stages)]]$name
  keep <- seq_len(which(vapply(stages, function(s) s$name, character(1)) == upto))
  parts <- vapply(stages[keep], function(s) sprintf("%s AS (\n%s\n)", s$name, s$sql),
                  character(1))
  paste0("WITH ", paste(parts, collapse = ",\n"))
}

# ----------------------------------------------------------------------------
# Generation
# ----------------------------------------------------------------------------

#' Create the cohort table using CohortGenerator's standard schema.
create_cohort_table <- function(connection, cfg) {
  tbl <- cfg$analysis$database$cohort_table
  names_list <- CohortGenerator::getCohortTableNames(cohortTable = tbl)
  CohortGenerator::createCohortTables(
    connection            = connection,
    cohortDatabaseSchema  = cfg$analysis$database$results_database_schema,
    cohortTableNames      = names_list,
    incremental           = FALSE
  )
  log_info("Created cohort tables (", tbl, " and companions) via CohortGenerator")
  names_list
}

#' Generate every cohort defined in the config.
#'
#' @return list with `attrition` (data frame) and `counts` (data frame)
generate_cohorts <- function(connection, cfg, resolved_sets) {
  log_step("Generating cohorts from config/cohorts.yml")

  schema   <- cfg$analysis$database$cdm_database_schema
  res_schema <- cfg$analysis$database$results_database_schema
  tbl      <- cfg$analysis$database$cohort_table

  table_names <- create_cohort_table(connection, cfg)

  attrition <- list()

  for (ch in cfg$cohorts$cohorts) {
    log_info("")
    log_info("Cohort ", ch$id, ": ", ch$label)

    stages <- build_cohort_stages(ch, resolved_sets, schema)

    # --- attrition: count subjects surviving each stage ---------------------
    prev_n <- NA_integer_
    for (s in stages) {
      sql <- paste0(compose_with(stages, upto = s$name),
                    sprintf("\nSELECT COUNT(DISTINCT person_id) AS n_subjects, COUNT(*) AS n_records FROM %s",
                            s$name))
      r <- query(connection, sql)
      n <- as.integer(r$N_SUBJECTS)
      lost <- if (is.na(prev_n)) NA_integer_ else prev_n - n
      attrition[[length(attrition) + 1]] <- data.frame(
        cohort_id = ch$id, cohort_name = ch$name, stage = s$name,
        criterion = s$label, n_subjects = n, n_records = as.integer(r$N_RECORDS),
        n_lost = lost, stringsAsFactors = FALSE)
      cat(sprintf("      %-34s %7s subjects%s\n", substr(s$label, 1, 34),
                  format(n, big.mark = ","),
                  if (is.na(lost)) "" else sprintf("   (-%s)", format(lost, big.mark = ","))))
      prev_n <- n
    }

    # --- materialise --------------------------------------------------------
    insert_sql <- paste0(
      compose_with(stages),
      sprintf("\nSELECT CAST(%d AS INT) AS cohort_definition_id,
       person_id AS subject_id,
       cohort_start_date,
       cohort_end_date
FROM cohort_final", ch$id))

    execute(connection, sprintf(
      "INSERT INTO %s.%s (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)\n%s",
      res_schema, tbl, insert_sql))

    # Persist the generated SQL so the definition is auditable as code, not
    # only as config.
    writeLines(
      c(sprintf("-- Cohort %d: %s", ch$id, ch$label),
        sprintf("-- Generated by R/cohorts.R from config/cohorts.yml on %s",
                format(Sys.Date())),
        "-- OHDSI SQL, before dialect translation.", "",
        insert_sql),
      out_path("cohorts", sprintf("cohort_%02d_%s.sql", ch$id, ch$name))
    )
  }

  attrition_df <- do.call(rbind, attrition)

  counts <- query(connection, "
    SELECT cohort_definition_id, COUNT(*) AS n_records,
           COUNT(DISTINCT subject_id) AS n_subjects,
           MIN(cohort_start_date) AS first_start, MAX(cohort_start_date) AS last_start
    FROM @schema.@table GROUP BY cohort_definition_id ORDER BY cohort_definition_id
  ", schema = res_schema, table = tbl)
  names(counts) <- tolower(names(counts))

  cat("\n")
  log_info("Final cohort table contents:")
  print_df(counts)

  cat("\n")
  log_note("Read the attrition column downwards. The criterion that removes ",
           "the most people is the one that defines your study population, ",
           "whatever the protocol says the study is about. If a criterion ",
           "removes nobody it is documenting intent rather than doing work -- ",
           "which is fine, as long as you know which is which.")

  list(attrition = attrition_df, counts = counts)
}

#' Sanity checks on the generated cohort table.
#'
#' Cheap, and they catch the errors that otherwise surface much later as
#' inexplicable results.
validate_cohorts <- function(connection, cfg) {
  log_step("Validating generated cohorts")
  res_schema <- cfg$analysis$database$results_database_schema
  tbl <- cfg$analysis$database$cohort_table

  checks <- query(connection, "
    SELECT
      SUM(CASE WHEN cohort_end_date < cohort_start_date THEN 1 ELSE 0 END) AS end_before_start,
      SUM(CASE WHEN cohort_start_date IS NULL OR cohort_end_date IS NULL THEN 1 ELSE 0 END) AS null_dates,
      COUNT(*) AS n_rows
    FROM @schema.@table
  ", schema = res_schema, table = tbl)

  dupes <- query(connection, "
    SELECT COUNT(*) AS n_dupes FROM (
      SELECT cohort_definition_id, subject_id, cohort_start_date
      FROM @schema.@table
      GROUP BY cohort_definition_id, subject_id, cohort_start_date
      HAVING COUNT(*) > 1) t
  ", schema = res_schema, table = tbl)

  tgt <- get_cohort(cfg, "target"); cmp <- get_cohort(cfg, "comparator")
  overlap <- query(connection, "
    SELECT COUNT(*) AS n_overlap FROM (
      SELECT subject_id FROM @schema.@table WHERE cohort_definition_id = @target_id
      INTERSECT
      SELECT subject_id FROM @schema.@table WHERE cohort_definition_id = @comparator_id) t
  ", schema = res_schema, table = tbl, target_id = tgt$id, comparator_id = cmp$id)

  ok <- TRUE
  report <- function(label, value, expect_zero = TRUE) {
    bad <- expect_zero && value > 0
    if (bad) ok <<- FALSE
    cat(sprintf("      %-42s %8s  %s\n", label, format(value, big.mark = ","),
                if (bad) "<-- PROBLEM" else "ok"))
  }
  report("Rows with cohort_end_date < start_date", checks$END_BEFORE_START)
  report("Rows with NULL dates",                   checks$NULL_DATES)
  report("Duplicate (cohort, subject, start) rows", dupes$N_DUPES)
  report("Subjects in BOTH target and comparator",  overlap$N_OVERLAP)

  cat("\n")
  if (ok) {
    log_note("All checks passed. The overlap check matters most: a subject in ",
             "both arms would be counted twice by the outcome model, breaking ",
             "the independence assumption. It is zero here because the ",
             "no_comparator_exposure exclusion enforces it by design.")
  } else {
    warning("Cohort validation found problems -- see the table above.", call. = FALSE)
  }
  invisible(ok)
}
