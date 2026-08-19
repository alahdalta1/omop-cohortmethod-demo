# ============================================================================
# R/concept_sets.R -- turning the concept sets in config/cohorts.yml into
# explicit lists of concept IDs.
#
# A concept set is a clinical idea ("celecoxib, in any form"). Resolving it is
# the step where that idea becomes a set of integers the database understands.
# It is worth doing visibly, because almost every serious error in an
# observational study is upstream of the statistics: the concept set was too
# narrow, too broad, or expanded to something nobody checked.
#
# So resolution here always writes an audit trail: which concepts were asked
# for, which were pulled in by descendant expansion, which were excluded, and
# how many records each one actually has in this database.
# ============================================================================

#' Resolve one concept set against the vocabulary.
#'
#' @param connection open DatabaseConnector connection
#' @param schema CDM schema
#' @param concept_set one element of cfg$cohorts$concept_sets
#' @return data frame, one row per resolved concept
resolve_concept_set <- function(connection, schema, concept_set) {

  seeds <- concept_set$concepts
  inc <- Filter(function(x) !isTRUE(x$is_excluded), seeds)
  exc <- Filter(function(x)  isTRUE(x$is_excluded), seeds)
  assert(length(inc) > 0, "Concept set '", concept_set$id,
         "' has no included concepts (all entries are is_excluded: true).")

  expand <- function(entries) {
    if (!length(entries)) return(integer(0))
    direct <- vapply(entries, function(x) as.integer(x$concept_id), integer(1))
    with_desc <- vapply(entries, function(x) isTRUE(x$include_descendants), logical(1))

    ids <- direct[!with_desc]
    if (any(with_desc)) {
      anc <- paste(direct[with_desc], collapse = ", ")
      d <- query(connection, "
        SELECT DISTINCT descendant_concept_id AS concept_id
        FROM @schema.concept_ancestor
        WHERE ancestor_concept_id IN (@anc)
      ", schema = schema, anc = anc)
      ids <- c(ids, as.integer(d$CONCEPT_ID))
      # CONCEPT_ANCESTOR always contains the self-row, so the seed concepts
      # come back from the join. Union defensively in case a vocabulary is
      # missing them.
      ids <- c(ids, direct[with_desc])
    }
    sort(unique(ids))
  }

  included <- expand(inc)
  excluded <- expand(exc)
  final <- setdiff(included, excluded)

  assert(length(final) > 0, "Concept set '", concept_set$id,
         "' resolved to zero concepts after exclusions.")

  meta <- query(connection, "
    SELECT concept_id, concept_name, domain_id, vocabulary_id, concept_class_id,
           standard_concept, concept_code, invalid_reason
    FROM @schema.concept
    WHERE concept_id IN (@ids)
  ", schema = schema, ids = paste(final, collapse = ", "))
  names(meta) <- tolower(names(meta))

  # Concepts asked for that the vocabulary does not contain at all. Silent
  # absence is the dangerous case: the study runs, and quietly measures less
  # than intended.
  missing <- setdiff(final, meta$concept_id)
  if (length(missing)) {
    warning("Concept set '", concept_set$id, "': ", length(missing),
            " concept id(s) not present in CONCEPT: ",
            paste(utils::head(missing, 10), collapse = ", "), call. = FALSE)
  }

  seed_ids <- vapply(inc, function(x) as.integer(x$concept_id), integer(1))
  meta$concept_set_id   <- concept_set$id
  meta$concept_set_name <- concept_set$name
  meta$origin <- ifelse(meta$concept_id %in% seed_ids,
                        "seed (listed in config)", "descendant (expanded)")
  meta$n_excluded_by_config <- length(setdiff(included, final))

  meta[order(meta$origin != "seed (listed in config)", meta$concept_id), ]
}

#' Resolve every concept set in the config, with an audit report.
#'
#' @return list with `sets` (named list of data frames) and `audit`
#'   (single data frame across all sets)
resolve_all_concept_sets <- function(connection, schema, cfg, count_records = TRUE) {
  log_step("Resolving concept sets")

  sets <- list()
  for (cs in cfg$cohorts$concept_sets) {
    r <- resolve_concept_set(connection, schema, cs)
    if (count_records) r <- add_record_counts(connection, schema, r)
    sets[[cs$id]] <- r

    n_seed <- sum(r$origin == "seed (listed in config)")
    n_desc <- sum(r$origin == "descendant (expanded)")
    log_info(sprintf("%-14s %2d concept(s)  [%d seed + %d descendant]  domain: %s",
                     cs$id, nrow(r), n_seed, n_desc,
                     paste(unique(r$domain_id), collapse = "/")))

    if (n_desc == 0 && any(vapply(cs$concepts, function(x) isTRUE(x$include_descendants), logical(1)))) {
      log_info(sprintf("%-14s  ^ include_descendants was requested but added nothing -- ", ""))
      log_info(sprintf("%-14s    the vocabulary has no children for these concepts.", ""))
    }

    # A non-standard concept in a concept set is nearly always a mistake: event
    # tables store standard concepts, so a non-standard entry matches nothing.
    non_std <- r[is.na(r$standard_concept) | r$standard_concept != "S", ]
    if (nrow(non_std)) {
      warning("Concept set '", cs$id, "' contains ", nrow(non_std),
              " non-standard concept(s) (", paste(non_std$concept_id, collapse = ", "),
              "). Event tables store STANDARD concepts, so these will match no records.",
              call. = FALSE)
    }
    invalid <- r[!is.na(r$invalid_reason) & r$invalid_reason != "", ]
    if (nrow(invalid)) {
      warning("Concept set '", cs$id, "' contains ", nrow(invalid),
              " concept(s) marked invalid/deprecated in the vocabulary.", call. = FALSE)
    }
  }

  audit <- do.call(rbind, lapply(names(sets), function(n) sets[[n]]))
  rownames(audit) <- NULL

  cat("\n")
  log_note("The resolved membership is written to ",
           "output/explore/concept_set_resolution.csv. That file, not the ",
           "YAML, is what a reviewer should be shown when they ask what ",
           "'celecoxib' meant in this study -- the YAML states the intent, the ",
           "CSV states what the database actually matched.")

  list(sets = sets, audit = audit)
}

#' Attach record and person counts to a resolved concept set.
#'
#' A concept set that resolves to 40 concepts of which 38 have zero records is
#' not a 40-concept set in any practical sense. This makes that visible.
add_record_counts <- function(connection, schema, resolved) {
  spec <- cdm_event_tables()
  resolved$n_records <- NA_integer_
  resolved$n_persons <- NA_integer_

  domain_to_table <- c(Drug = "drug_exposure", Condition = "condition_occurrence",
                       Procedure = "procedure_occurrence", Measurement = "measurement",
                       Observation = "observation", Device = "device_exposure",
                       Visit = "visit_occurrence")

  for (dom in unique(resolved$domain_id)) {
    tbl <- domain_to_table[[dom]]
    if (is.null(tbl) || is.na(tbl)) next
    s <- spec[spec$table == tbl, ]
    if (!nrow(s)) next
    idx <- which(resolved$domain_id == dom)
    r <- query(connection, "
      SELECT @concept_col AS concept_id, COUNT(*) AS n_records,
             COUNT(DISTINCT person_id) AS n_persons
      FROM @schema.@table
      WHERE @concept_col IN (@ids)
      GROUP BY @concept_col
    ", schema = schema, table = tbl, concept_col = s$concept_col,
       ids = paste(resolved$concept_id[idx], collapse = ", "))
    names(r) <- tolower(names(r))
    m <- match(resolved$concept_id[idx], r$concept_id)
    resolved$n_records[idx] <- ifelse(is.na(m), 0L, r$n_records[m])
    resolved$n_persons[idx] <- ifelse(is.na(m), 0L, r$n_persons[m])
  }
  resolved
}

#' Comma-separated ID list for injecting into SQL.
ids_for_sql <- function(resolved) paste(resolved$concept_id, collapse = ", ")
