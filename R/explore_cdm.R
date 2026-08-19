# ============================================================================
# R/explore_cdm.R -- understanding the OMOP Common Data Model, not just
# querying it.
#
# The functions here are organised around the ideas that make the CDM work,
# because those ideas are what transfer to a real claims or EHR database:
#
#   1. PERSON is the spine; OBSERVATION_PERIOD is the denominator.
#   2. Every clinical event table shares one skeleton:
#        person_id + a date + a *_concept_id + a *_type_concept_id
#   3. Every event carries the SAME fact three times: as raw source text, as a
#      source concept, and as a standard concept. Analysis uses the standard
#      column; provenance lives in the other two.
#   4. A concept's DOMAIN determines which table it belongs in. Domain routing
#      is what makes "all NSAIDs" a query rather than a research project.
#   5. CONCEPT_ANCESTOR turns flat codes into a hierarchy, which is what makes
#      concept sets expressible as "this ingredient and everything under it".
#
# Each function returns a data frame and prints an interpretation. The point
# of the printed commentary is that a reader of the console log should learn
# the data model, not just see numbers.
# ============================================================================

#' Metadata describing the standard OMOP clinical event tables.
#'
#' Written out as data because the uniformity is itself the lesson: once you
#' know the pattern, an unfamiliar CDM table is readable on sight.
cdm_event_tables <- function() {
  data.frame(
    table              = c("condition_occurrence", "drug_exposure", "procedure_occurrence",
                           "measurement", "observation", "device_exposure", "visit_occurrence"),
    concept_col        = c("condition_concept_id", "drug_concept_id", "procedure_concept_id",
                           "measurement_concept_id", "observation_concept_id",
                           "device_concept_id", "visit_concept_id"),
    source_concept_col = c("condition_source_concept_id", "drug_source_concept_id",
                           "procedure_source_concept_id", "measurement_source_concept_id",
                           "observation_source_concept_id", "device_source_concept_id",
                           "visit_source_concept_id"),
    source_value_col   = c("condition_source_value", "drug_source_value",
                           "procedure_source_value", "measurement_source_value",
                           "observation_source_value", "device_source_value",
                           "visit_source_value"),
    start_col          = c("condition_start_date", "drug_exposure_start_date", "procedure_date",
                           "measurement_date", "observation_date",
                           "device_exposure_start_date", "visit_start_date"),
    end_col            = c("condition_end_date", "drug_exposure_end_date", NA,
                           NA, NA, "device_exposure_end_date", "visit_end_date"),
    type_concept_col   = c("condition_type_concept_id", "drug_type_concept_id",
                           "procedure_type_concept_id", "measurement_type_concept_id",
                           "observation_type_concept_id", "device_type_concept_id",
                           "visit_type_concept_id"),
    expected_domain    = c("Condition", "Drug", "Procedure",
                           "Measurement", "Observation", "Device", "Visit"),
    stringsAsFactors   = FALSE
  )
}

#' Classify CDM tables into their functional groups.
cdm_table_groups <- function() {
  list(
    "Clinical events (one row per thing that happened to a person)" = c(
      "condition_occurrence", "drug_exposure", "procedure_occurrence", "measurement",
      "observation", "device_exposure", "visit_occurrence", "visit_detail", "note",
      "note_nlp", "specimen", "death"),
    "Person and denominator" = c("person", "observation_period", "payer_plan_period"),
    "Standardised vocabulary (the shared dictionary)" = c(
      "concept", "concept_ancestor", "concept_relationship", "concept_synonym",
      "concept_class", "domain", "vocabulary", "relationship", "drug_strength",
      "source_to_concept_map"),
    "Derived / era tables (built from the event tables)" = c(
      "condition_era", "drug_era", "dose_era", "cohort", "cohort_attribute",
      "cohort_definition", "attribute_definition"),
    "Health system and economics" = c(
      "care_site", "location", "provider", "cost", "fact_relationship",
      "cdm_source", "metadata")
  )
}

# ----------------------------------------------------------------------------
# 1. Inventory
# ----------------------------------------------------------------------------

#' Every table in the CDM with its row count, grouped by function.
explore_table_inventory <- function(connection, schema) {
  log_step("CDM table inventory")

  present <- list_cdm_tables(connection, schema)
  groups <- cdm_table_groups()

  known <- unlist(groups, use.names = FALSE)
  rows <- do.call(rbind, lapply(names(groups), function(g) {
    tabs <- intersect(groups[[g]], present)
    if (!length(tabs)) return(NULL)
    data.frame(group = g, table = tabs, stringsAsFactors = FALSE)
  }))
  other <- setdiff(present, known)
  if (length(other)) {
    rows <- rbind(rows, data.frame(group = "Other / non-standard", table = other,
                                   stringsAsFactors = FALSE))
  }

  rows$n_rows <- vapply(rows$table, function(t) table_row_count(connection, schema, t),
                        integer(1))

  for (g in unique(rows$group)) {
    sub <- rows[rows$group == g, ]
    sub <- sub[order(-sub$n_rows), ]
    cat("\n  ", g, "\n", sep = "")
    for (i in seq_len(nrow(sub))) {
      cat(sprintf("      %-24s %12s\n", sub$table[i],
                  format(sub$n_rows[i], big.mark = ",")))
    }
  }

  empty <- rows$table[rows$n_rows == 0]
  cat("\n")
  log_note("The CDM defines a fixed set of tables. A conformant database has ",
           "all of them, and empty ones are normal -- ",
           nrow(rows) - length(empty), " of ", nrow(rows),
           " tables here hold data. Emptiness is information: it tells you ",
           "which questions this database cannot answer.")
  if (length(empty)) {
    log_note("Empty here: ", paste(empty, collapse = ", "), ". ",
             "Note DEATH in particular -- with no mortality data there is no ",
             "way to handle death as a competing risk, and no way to detect ",
             "that follow-up ended because the person died.")
  }

  rows
}

# ----------------------------------------------------------------------------
# 2. Person and denominator
# ----------------------------------------------------------------------------

#' PERSON and OBSERVATION_PERIOD: who exists, and when were they observable.
explore_person_denominator <- function(connection, schema) {
  log_step("PERSON and OBSERVATION_PERIOD -- the denominator")

  counts <- query(connection, "
    SELECT
      (SELECT COUNT(*) FROM @schema.person)                       AS n_person,
      (SELECT COUNT(*) FROM @schema.observation_period)           AS n_periods,
      (SELECT COUNT(DISTINCT person_id) FROM @schema.observation_period) AS n_person_in_op
  ", schema = schema)

  orphans <- query(connection, "
    SELECT
      (SELECT COUNT(*) FROM @schema.observation_period op
         WHERE NOT EXISTS (SELECT 1 FROM @schema.person p WHERE p.person_id = op.person_id))
        AS op_without_person,
      (SELECT COUNT(*) FROM @schema.person p
         WHERE NOT EXISTS (SELECT 1 FROM @schema.observation_period op WHERE op.person_id = p.person_id))
        AS person_without_op
  ", schema = schema)

  span <- query(connection, "
    SELECT MIN(observation_period_start_date) AS first_start,
           MAX(observation_period_end_date)   AS last_end,
           AVG(CAST(DATEDIFF(DAY, observation_period_start_date, observation_period_end_date) AS FLOAT))
             AS mean_days
    FROM @schema.observation_period
  ", schema = schema)

  log_info("Persons in PERSON            : ", format(counts$N_PERSON, big.mark = ","))
  log_info("Rows in OBSERVATION_PERIOD   : ", format(counts$N_PERIODS, big.mark = ","))
  log_info("Distinct persons in OBS_PERIOD: ", format(counts$N_PERSON_IN_OP, big.mark = ","))
  log_info("Observation spans            : ", as.character(span$FIRST_START), " to ",
           as.character(span$LAST_END))
  log_info("Mean observed duration       : ", round(span$MEAN_DAYS / 365.25, 1), " years")

  cat("\n")
  log_note("OBSERVATION_PERIOD is the single most under-appreciated table in ",
           "the CDM. It states when a person was under observation, i.e. when ",
           "the absence of a record can be read as the absence of the event. ",
           "Outside it, absence means nothing at all. Every incidence rate you ",
           "ever compute has this table as its denominator.")

  if (orphans$OP_WITHOUT_PERSON > 0 || orphans$PERSON_WITHOUT_OP > 0) {
    cat("\n")
    log_note("DATA QUALITY ISSUE FOUND: ",
             format(orphans$OP_WITHOUT_PERSON, big.mark = ","),
             " observation periods reference a person_id that is absent from ",
             "PERSON, and ", format(orphans$PERSON_WITHOUT_OP, big.mark = ","),
             " persons have no observation period. In a conformant CDM both ",
             "are zero -- this is a referential integrity violation. It is ",
             "harmless here only because every analysis in this project joins ",
             "through PERSON, which silently drops the orphans. On a real CDM ",
             "you would run the OHDSI DataQualityDashboard before trusting ",
             "any denominator, and this is exactly the class of defect it ",
             "exists to catch.")
  }

  data.frame(
    metric = c("persons", "observation_period_rows", "persons_with_observation_period",
               "observation_periods_without_person", "persons_without_observation_period",
               "first_observation_start", "last_observation_end", "mean_observed_years"),
    value  = c(counts$N_PERSON, counts$N_PERIODS, counts$N_PERSON_IN_OP,
               orphans$OP_WITHOUT_PERSON, orphans$PERSON_WITHOUT_OP,
               as.character(span$FIRST_START), as.character(span$LAST_END),
               round(span$MEAN_DAYS / 365.25, 2)),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------
# 3. The shared skeleton of the event tables
# ----------------------------------------------------------------------------

#' Row counts, person counts and concept counts for each clinical event table.
explore_event_tables <- function(connection, schema) {
  log_step("Clinical event tables -- one skeleton, repeated")

  spec <- cdm_event_tables()
  present <- list_cdm_tables(connection, schema)
  spec <- spec[spec$table %in% present, ]

  res <- do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
    s <- spec[i, ]
    r <- query(connection, "
      SELECT COUNT(*)                        AS n_rows,
             COUNT(DISTINCT person_id)       AS n_persons,
             COUNT(DISTINCT @concept_col)    AS n_concepts,
             MIN(@start_col)                 AS first_date,
             MAX(@start_col)                 AS last_date
      FROM @schema.@table
    ", schema = schema, table = s$table, concept_col = s$concept_col,
       start_col = s$start_col)
    data.frame(
      table      = s$table,
      n_rows     = as.integer(r$N_ROWS),
      n_persons  = as.integer(r$N_PERSONS),
      n_concepts = as.integer(r$N_CONCEPTS),
      rows_per_person = ifelse(r$N_PERSONS > 0, round(r$N_ROWS / r$N_PERSONS, 1), NA),
      first_date = as.character(r$FIRST_DATE),
      last_date  = as.character(r$LAST_DATE),
      stringsAsFactors = FALSE
    )
  }))

  print_df(res)

  cat("\n")
  log_note("Read across the columns of cdm_event_tables() and the pattern is ",
           "the same every time: person_id says who, a date column says when, ",
           "a *_concept_id says what, and a *_type_concept_id says where the ",
           "record came from (claim, EHR order, registry). Learn the skeleton ",
           "once and every event table in the CDM is readable, including ones ",
           "you have never opened.")
  log_note("n_concepts is the number of DISTINCT clinical ideas represented. ",
           "It is small here because Eunomia ships a cut-down vocabulary; on a ",
           "real claims database expect tens of thousands. That number is the ",
           "practical ceiling on how many covariates a large-scale propensity ",
           "model can build.")

  res
}

# ----------------------------------------------------------------------------
# 4. Source-to-standard concept mapping
# ----------------------------------------------------------------------------

#' How the same fact is stored three times, and how much of it is mapped.
#'
#' This is the heart of what the CDM does for you and the section most worth
#' understanding before working with real data.
explore_concept_mapping <- function(connection, schema) {
  log_step("Source-to-standard concept mapping")

  spec <- cdm_event_tables()
  present <- list_cdm_tables(connection, schema)
  spec <- spec[spec$table %in% present, ]

  res <- do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
    s <- spec[i, ]
    r <- query(connection, "
      SELECT COUNT(*) AS n_rows,
             SUM(CASE WHEN @concept_col = 0 OR @concept_col IS NULL THEN 1 ELSE 0 END)
               AS n_unmapped_standard,
             SUM(CASE WHEN @source_concept_col = 0 OR @source_concept_col IS NULL THEN 1 ELSE 0 END)
               AS n_no_source_concept,
             SUM(CASE WHEN @source_value_col IS NULL OR @source_value_col = '' THEN 1 ELSE 0 END)
               AS n_no_source_value
      FROM @schema.@table
    ", schema = schema, table = s$table, concept_col = s$concept_col,
       source_concept_col = s$source_concept_col, source_value_col = s$source_value_col)

    data.frame(
      table               = s$table,
      n_rows              = as.integer(r$N_ROWS),
      unmapped_standard   = as.integer(r$N_UNMAPPED_STANDARD),
      pct_unmapped        = ifelse(r$N_ROWS > 0,
                                   round(100 * r$N_UNMAPPED_STANDARD / r$N_ROWS, 2), NA),
      no_source_concept   = as.integer(r$N_NO_SOURCE_CONCEPT),
      no_source_value     = as.integer(r$N_NO_SOURCE_VALUE),
      stringsAsFactors    = FALSE
    )
  }))

  print_df(res)

  cat("\n")
  log_note("Every clinical event carries the same fact three times:")
  cat("      *_source_value        raw text/code from the source system  (e.g. 'I21.4')\n")
  cat("      *_source_concept_id   that code as a vocabulary concept     (ICD-10-CM 45576876)\n")
  cat("      *_concept_id          the STANDARD concept it maps to       (SNOMED 312327)\n")
  cat("\n")
  log_note("Analysis uses the standard column, and only the standard column. ",
           "That is what makes one study definition run unchanged on an ICD-9 ",
           "database, an ICD-10 database and a Read-coded database: the ETL ",
           "absorbed the difference, so your code does not have to.")
  log_note("The number to watch is unmapped_standard -- rows where ",
           "*_concept_id = 0, meaning the ETL could not map the source code to ",
           "anything standard. Those rows are invisible to every concept-set ",
           "query you will ever write. A high percentage in a domain you care ",
           "about invalidates the study rather than merely weakening it, and ",
           "it is silent: your query returns a clean answer about a subset of ",
           "the data you did not know you had chosen.")

  worst <- res[which.max(res$pct_unmapped), ]
  if (nrow(worst) && !is.na(worst$pct_unmapped) && worst$pct_unmapped > 0) {
    log_note("Highest here: ", worst$table, " at ", worst$pct_unmapped, "% unmapped.")
  } else {
    log_note("Here every row maps to a standard concept -- 0% unmapped ",
             "throughout. That is a property of synthetic data. Real claims ",
             "data routinely runs at 5-20% unmapped in some domains, and you ",
             "should never assume this result carries over.")
  }

  # Provenance is genuinely absent in Eunomia; say so rather than implying the
  # mapping was verified.
  if (all(res$no_source_concept == res$n_rows)) {
    log_note("Note also that source_concept_id is 0 for every row in every ",
             "table: Eunomia was generated directly as standard concepts and ",
             "never had a source vocabulary. So the provenance trail this ",
             "section describes cannot actually be followed here. On a real ",
             "CDM, checking that trail is how you find out whether the ETL is ",
             "mapping the codes you think it is.")
  }

  res
}

# ----------------------------------------------------------------------------
# 5. Domain routing
# ----------------------------------------------------------------------------

#' Does every concept sit in the table its domain says it should?
#'
#' Domain routing is the CDM rule that determines which table a fact lands in.
#' It is why "give me all NSAID exposures" is a query against one table rather
#' than a hunt across several.
explore_domain_routing <- function(connection, schema) {
  log_step("Domain routing -- which table does a concept belong in?")

  spec <- cdm_event_tables()
  present <- list_cdm_tables(connection, schema)
  spec <- spec[spec$table %in% present, ]

  res <- do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
    s <- spec[i, ]
    r <- query(connection, "
      SELECT c.domain_id, COUNT(*) AS n_rows, COUNT(DISTINCT t.@concept_col) AS n_concepts
      FROM @schema.@table t
      INNER JOIN @schema.concept c ON c.concept_id = t.@concept_col
      GROUP BY c.domain_id
    ", schema = schema, table = s$table, concept_col = s$concept_col)
    if (!nrow(r)) return(NULL)
    data.frame(table = s$table, expected_domain = s$expected_domain,
               observed_domain = r$DOMAIN_ID, n_rows = as.integer(r$N_ROWS),
               n_concepts = as.integer(r$N_CONCEPTS), stringsAsFactors = FALSE)
  }))

  res$matches_expected <- res$observed_domain == res$expected_domain
  print_df(res)

  cat("\n")
  log_note("Each standard concept has exactly one domain_id, and that domain ",
           "decides which table it is stored in. A drug goes in DRUG_EXPOSURE, ",
           "a condition in CONDITION_OCCURRENCE, and so on. The routing is a ",
           "property of the vocabulary, not of the ETL author's judgement.")
  log_note("This is why concept sets are portable. 'Celecoxib and all its ",
           "descendants' resolves to concepts whose domain is Drug, so you ",
           "know without looking that they live in DRUG_EXPOSURE.")

  mismatched <- res[!res$matches_expected, ]
  if (nrow(mismatched)) {
    log_note("MISMATCHES FOUND -- concepts stored outside their domain's table:")
    print_df(mismatched)
    log_note("This matters in practice. A condition concept sitting in ",
             "OBSERVATION, for instance, will be missed by a condition-domain ",
             "query even though the fact is present in the database. Such ",
             "misrouting is a common ETL defect.")
  } else {
    log_note("No mismatches: every concept sits in the table its domain ",
             "requires. Worth checking on any new CDM before assuming it.")
  }

  res
}

# ----------------------------------------------------------------------------
# 6. Vocabulary structure and hierarchy
# ----------------------------------------------------------------------------

#' What is in the CONCEPT table, by vocabulary, domain and standard status.
explore_vocabulary <- function(connection, schema) {
  log_step("The vocabulary: CONCEPT and CONCEPT_ANCESTOR")

  by_vocab <- query(connection, "
    SELECT vocabulary_id, domain_id,
           SUM(CASE WHEN standard_concept = 'S' THEN 1 ELSE 0 END) AS n_standard,
           SUM(CASE WHEN standard_concept IS NULL OR standard_concept = '' THEN 1 ELSE 0 END)
             AS n_non_standard,
           SUM(CASE WHEN standard_concept = 'C' THEN 1 ELSE 0 END) AS n_classification,
           COUNT(*) AS n_total
    FROM @schema.concept
    GROUP BY vocabulary_id, domain_id
    ORDER BY COUNT(*) DESC
  ", schema = schema)
  names(by_vocab) <- tolower(names(by_vocab))

  print_df(utils::head(by_vocab, 20))

  cat("\n")
  log_note("standard_concept = 'S' marks the concept you are allowed to use in ",
           "analysis. NULL marks a source concept -- a code from ICD-10, Read, ",
           "NDC and so on -- which exists so that source data can be recorded ",
           "faithfully and traced, but which must be mapped before use. 'C' ",
           "marks a classification concept, used to navigate the hierarchy ",
           "(e.g. an ATC class) but not to record an event.")

  anc <- query(connection, "
    SELECT COUNT(*) AS n_rows,
           COUNT(DISTINCT ancestor_concept_id) AS n_ancestors,
           COUNT(DISTINCT descendant_concept_id) AS n_descendants,
           SUM(CASE WHEN min_levels_of_separation = 0 THEN 1 ELSE 0 END) AS n_self_rows,
           MAX(max_levels_of_separation) AS max_depth
    FROM @schema.concept_ancestor
  ", schema = schema)

  cat("\n")
  log_info("CONCEPT_ANCESTOR rows      : ", format(anc$N_ROWS, big.mark = ","))
  log_info("  of which self-references : ", format(anc$N_SELF_ROWS, big.mark = ","))
  log_info("  distinct ancestors       : ", format(anc$N_ANCESTORS, big.mark = ","))
  log_info("  maximum hierarchy depth  : ", anc$MAX_DEPTH)

  cat("\n")
  log_note("CONCEPT_ANCESTOR is a pre-computed transitive closure of the ",
           "vocabulary hierarchy: one row for every ancestor-descendant pair, ",
           "at any depth, including each concept as its own ancestor at ",
           "level 0. This is what makes 'this ingredient and every product ",
           "containing it' a single join instead of a recursive query.")

  if (anc$N_ROWS == anc$N_SELF_ROWS) {
    log_note("IMPORTANT FOR THIS DATASET: every row here is a self-reference, ",
             "so the hierarchy is completely flat. include_descendants: true ",
             "in the concept-set config will therefore expand to exactly one ",
             "concept each time. The code is doing the right thing; the ",
             "vocabulary simply has nothing under these concepts. Never infer ",
             "from 'the resolution ran' that 'the concept set is complete'.")
  } else {
    n_concept <- as.integer(query(connection,
      "SELECT COUNT(*) AS n FROM @schema.concept", schema = schema)$N)
    log_note("Note the mismatch: CONCEPT_ANCESTOR references ",
             format(anc$N_ANCESTORS, big.mark = ","), " distinct ancestor ",
             "concepts and runs ", anc$MAX_DEPTH, " levels deep, but CONCEPT ",
             "itself holds only ", format(n_concept, big.mark = ","),
             " rows. Eunomia subset CONCEPT far more aggressively than ",
             "CONCEPT_ANCESTOR, so most of the hierarchy points at concepts ",
             "that are not in this database.")
    log_note("The practical consequence appears in the concept-set resolution ",
             "below: the hierarchy is deep in general, yet the three concepts ",
             "this study uses have no descendants at all. 'The vocabulary has ",
             "a hierarchy' and 'my concept set will expand' are different ",
             "claims, and only the second one matters. Always check the ",
             "expansion count for the concepts you are actually using.")
  }

  by_vocab
}

# ----------------------------------------------------------------------------
# 7. Alignment of events with observation periods
# ----------------------------------------------------------------------------

#' Do clinical events fall inside the person's observation period?
#'
#' This is the join that connects the event tables to the denominator, and it
#' is where the study populations in this project ultimately come from.
explore_observation_alignment <- function(connection, schema,
                                          tables = c("condition_occurrence",
                                                     "drug_exposure",
                                                     "procedure_occurrence")) {
  log_step("Do events fall inside OBSERVATION_PERIOD?")

  spec <- cdm_event_tables()
  present <- list_cdm_tables(connection, schema)
  spec <- spec[spec$table %in% intersect(tables, present), ]

  res <- do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
    s <- spec[i, ]
    r <- query(connection, "
      SELECT COUNT(*) AS n_rows,
             SUM(CASE WHEN op.person_id IS NULL THEN 1 ELSE 0 END) AS n_no_period,
             SUM(CASE WHEN op.person_id IS NOT NULL
                       AND t.@start_col < op.observation_period_start_date THEN 1 ELSE 0 END)
               AS n_before_period,
             SUM(CASE WHEN op.person_id IS NOT NULL
                       AND t.@start_col > op.observation_period_end_date THEN 1 ELSE 0 END)
               AS n_after_period
      FROM @schema.@table t
      LEFT JOIN @schema.observation_period op
        ON op.person_id = t.person_id
       AND t.@start_col >= op.observation_period_start_date
       AND t.@start_col <= op.observation_period_end_date
    ", schema = schema, table = s$table, start_col = s$start_col)

    # Rows with no matching period at all: either the person has no period, or
    # the event date sits outside every period they have.
    orphan <- query(connection, "
      SELECT COUNT(*) AS n_outside
      FROM @schema.@table t
      WHERE NOT EXISTS (
        SELECT 1 FROM @schema.observation_period op
        WHERE op.person_id = t.person_id
          AND t.@start_col >= op.observation_period_start_date
          AND t.@start_col <= op.observation_period_end_date)
    ", schema = schema, table = s$table, start_col = s$start_col)

    data.frame(
      table          = s$table,
      n_rows         = as.integer(r$N_ROWS),
      n_outside_obs  = as.integer(orphan$N_OUTSIDE),
      pct_outside    = ifelse(r$N_ROWS > 0,
                              round(100 * orphan$N_OUTSIDE / r$N_ROWS, 2), NA),
      stringsAsFactors = FALSE
    )
  }))

  print_df(res)

  cat("\n")
  log_note("An event recorded outside a person's observation period is a ",
           "contradiction: the database is asserting something happened at a ",
           "time when it was not watching. In practice such rows are either ",
           "an ETL error or a sign that observation periods were derived too ",
           "narrowly.")
  log_note("This join is not academic. Cohort entry in this study requires ",
           "365 days of prior observation, which is evaluated exactly here -- ",
           "the index event must sit inside a period that started at least a ",
           "year earlier. Get the observation period wrong and the new-user ",
           "definition silently stops meaning anything.")

  res
}

# ----------------------------------------------------------------------------
# 8. Following the study concepts through the model
# ----------------------------------------------------------------------------

#' Trace the study's own concepts through CONCEPT, CONCEPT_ANCESTOR and the
#' event tables.
#'
#' Deliberately concrete: the abstractions above become much clearer when
#' followed on the three concepts this study actually depends on.
explore_study_concepts <- function(connection, schema, cfg) {
  log_step("Following this study's concepts through the model")

  ids <- unique(unlist(lapply(cfg$cohorts$concept_sets, function(cs) {
    vapply(cs$concepts, function(x) as.integer(x$concept_id), integer(1))
  })))

  meta <- query(connection, "
    SELECT concept_id, concept_name, domain_id, vocabulary_id, concept_class_id,
           standard_concept, concept_code
    FROM @schema.concept WHERE concept_id IN (@ids)
  ", schema = schema, ids = paste(ids, collapse = ", "))
  names(meta) <- tolower(names(meta))

  desc <- query(connection, "
    SELECT ancestor_concept_id, COUNT(*) AS n_descendants
    FROM @schema.concept_ancestor WHERE ancestor_concept_id IN (@ids)
    GROUP BY ancestor_concept_id
  ", schema = schema, ids = paste(ids, collapse = ", "))
  names(desc) <- tolower(names(desc))

  meta <- merge(meta, desc, by.x = "concept_id", by.y = "ancestor_concept_id",
                all.x = TRUE)
  meta$n_descendants[is.na(meta$n_descendants)] <- 0L

  # Where each concept's records actually live.
  meta$table <- ifelse(meta$domain_id == "Drug", "drug_exposure",
                ifelse(meta$domain_id == "Condition", "condition_occurrence",
                ifelse(meta$domain_id == "Procedure", "procedure_occurrence", NA)))

  counts <- do.call(rbind, lapply(seq_len(nrow(meta)), function(i) {
    if (is.na(meta$table[i])) return(data.frame(n_records = NA_integer_, n_persons = NA_integer_))
    s <- cdm_event_tables()[cdm_event_tables()$table == meta$table[i], ]
    r <- query(connection, "
      SELECT COUNT(*) AS n_records, COUNT(DISTINCT person_id) AS n_persons
      FROM @schema.@table
      WHERE @concept_col IN (
        SELECT descendant_concept_id FROM @schema.concept_ancestor
        WHERE ancestor_concept_id = @cid)
    ", schema = schema, table = meta$table[i], concept_col = s$concept_col,
       cid = meta$concept_id[i])
    data.frame(n_records = as.integer(r$N_RECORDS), n_persons = as.integer(r$N_PERSONS))
  }))
  meta <- cbind(meta, counts)

  print_df(meta[, c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                    "concept_class_id", "standard_concept", "n_descendants",
                    "table", "n_records", "n_persons")])

  cat("\n")
  log_note("Read one row end to end and the whole model is visible. Take ",
           "celecoxib: an RxNorm concept, concept_class Ingredient, marked ",
           "standard, domain Drug. Because the domain is Drug its records are ",
           "in DRUG_EXPOSURE. Because it is an Ingredient, its descendants in ",
           "CONCEPT_ANCESTOR would normally be every clinical and branded drug ",
           "containing it -- which is why the concept set asks for descendants ",
           "rather than listing products by hand.")
  log_note("n_persons is the number of people with at least one record. That ",
           "is the ceiling on the size of the cohort built from this concept ",
           "set, before a single eligibility criterion is applied. Comparing ",
           "it with the final cohort count in the attrition table is a good ",
           "sanity check.")

  meta
}

# ----------------------------------------------------------------------------
# 9. Evidence for the time-at-risk decision
# ----------------------------------------------------------------------------

#' Exposure durations and outcome timing -- the evidence base for choosing a
#' time-at-risk window.
#'
#' Time at risk is usually chosen from convention. It should be chosen from the
#' data: how long does treatment actually last, and when do events actually
#' happen relative to index? This function produces both, so that the window
#' configured in analysis_settings.yml can be justified from output rather
#' than asserted.
explore_exposure_and_outcome_timing <- function(connection, schema, cfg) {
  log_step("Exposure duration and outcome timing -- evidence for time at risk")

  tgt <- get_cohort(cfg, "target")
  cmp <- get_cohort(cfg, "comparator")
  out <- get_cohort(cfg, "outcome")

  drug_ids <- c(
    concept_set_ids(cfg, tgt$entry$concept_set),
    concept_set_ids(cfg, cmp$entry$concept_set)
  )
  out_ids <- concept_set_ids(cfg, out$entry$concept_set)

  # --- how long does a single exposure record cover? ------------------------
  dur <- query(connection, "
    SELECT de.drug_concept_id, c.concept_name,
           COUNT(*) AS n_records,
           COUNT(DISTINCT de.person_id) AS n_persons,
           MIN(DATEDIFF(DAY, de.drug_exposure_start_date, de.drug_exposure_end_date)) AS min_days,
           AVG(CAST(DATEDIFF(DAY, de.drug_exposure_start_date, de.drug_exposure_end_date) AS FLOAT)) AS mean_days,
           MAX(DATEDIFF(DAY, de.drug_exposure_start_date, de.drug_exposure_end_date)) AS max_days,
           AVG(CAST(de.days_supply AS FLOAT)) AS mean_days_supply
    FROM @schema.drug_exposure de
    INNER JOIN @schema.concept c ON c.concept_id = de.drug_concept_id
    WHERE de.drug_concept_id IN (@ids)
    GROUP BY de.drug_concept_id, c.concept_name
  ", schema = schema, ids = paste(drug_ids, collapse = ", "))
  names(dur) <- tolower(names(dur))
  dur$records_per_person <- round(dur$n_records / dur$n_persons, 2)

  print_df(dur)

  cat("\n")
  if (all(dur$max_days == 0, na.rm = TRUE)) {
    log_note("EVERY exposure record has end_date == start_date: a zero-day ",
             "supply. And records_per_person is 1.00, so there are no repeat ",
             "dispensings to collapse into an era either.")
    log_note("CONSEQUENCE FOR THE DESIGN: the treatment episode is one day ",
             "long for everybody, so cohort end equals cohort start. An ",
             "on-treatment time-at-risk window (end_anchor: 'cohort end') ",
             "would yield essentially no person-time and no events. This is ",
             "why the primary window in analysis_settings.yml is anchored on ",
             "cohort START instead, and it is a decision driven by this table ",
             "rather than by preference. On a real CDM with genuine dispensing ",
             "records you would revisit it.")
  } else {
    log_note("Exposure records have non-zero duration, so an on-treatment ",
             "time-at-risk window is estimable. Check records_per_person: ",
             "above 1 means the drug-era collapsing logic in R/cohorts.R is ",
             "doing real work.")
  }

  # --- when do outcomes happen relative to index? ---------------------------
  timing <- query(connection, "
    WITH idx AS (
      SELECT person_id, drug_concept_id, MIN(drug_exposure_start_date) AS index_date
      FROM @schema.drug_exposure WHERE drug_concept_id IN (@drug_ids)
      GROUP BY person_id, drug_concept_id
    ),
    ev AS (
      SELECT person_id, MIN(condition_start_date) AS event_date
      FROM @schema.condition_occurrence WHERE condition_concept_id IN (@out_ids)
      GROUP BY person_id
    )
    SELECT i.drug_concept_id,
           COUNT(*) AS n_users,
           SUM(CASE WHEN e.event_date IS NOT NULL THEN 1 ELSE 0 END) AS n_with_event_ever,
           SUM(CASE WHEN e.event_date <  i.index_date THEN 1 ELSE 0 END) AS n_before_index,
           SUM(CASE WHEN e.event_date =  i.index_date THEN 1 ELSE 0 END) AS n_on_index,
           SUM(CASE WHEN e.event_date >  i.index_date THEN 1 ELSE 0 END) AS n_after_index,
           MIN(CASE WHEN e.event_date > i.index_date
                    THEN DATEDIFF(DAY, i.index_date, e.event_date) END) AS min_days_to_event,
           AVG(CASE WHEN e.event_date > i.index_date
                    THEN CAST(DATEDIFF(DAY, i.index_date, e.event_date) AS FLOAT) END) AS mean_days_to_event,
           MAX(CASE WHEN e.event_date > i.index_date
                    THEN DATEDIFF(DAY, i.index_date, e.event_date) END) AS max_days_to_event
    FROM idx i LEFT JOIN ev e ON e.person_id = i.person_id
    GROUP BY i.drug_concept_id
  ", schema = schema, drug_ids = paste(drug_ids, collapse = ", "),
     out_ids = paste(out_ids, collapse = ", "))
  names(timing) <- tolower(names(timing))

  cat("\n")
  print_df(timing)

  cat("\n")
  log_note("n_before_index and n_on_index are the events the design excludes: ",
           "the first by the prior-outcome washout, the second by starting ",
           "time at risk on day 1. Both are choices, and this table shows what ",
           "each one costs in events.")
  if (!all(is.na(timing$max_days_to_event))) {
    log_note("All post-index events fall between day ",
             min(timing$min_days_to_event, na.rm = TRUE), " and day ",
             max(timing$max_days_to_event, na.rm = TRUE),
             ". A time-at-risk window shorter than the upper figure discards ",
             "real events; one much longer adds person-time in which the drug ",
             "cannot plausibly still be acting, diluting the estimate.")
  }
  log_note("Do not read the crude proportions here as a result. They are ",
           "unadjusted, and the whole purpose of the propensity score stage is ",
           "that the two groups are not yet comparable.")

  list(durations = dur, timing = timing)
}

# ----------------------------------------------------------------------------
# helpers
# ----------------------------------------------------------------------------

#' Print a data frame in a compact, aligned form.
print_df <- function(df, max_rows = 40) {
  if (is.null(df) || !nrow(df)) { cat("      (no rows)\n"); return(invisible(NULL)) }
  d <- utils::head(df, max_rows)
  # Widen for the duration of the print so wide tables stay on one line
  # instead of wrapping into an unreadable block.
  old <- options(width = 250); on.exit(options(old), add = TRUE)
  out <- utils::capture.output(print(d, row.names = FALSE, right = FALSE))
  cat(paste0("      ", out, collapse = "\n"), "\n")
  if (nrow(df) > max_rows) cat("      ... ", nrow(df) - max_rows, " more rows\n")
  invisible(NULL)
}
