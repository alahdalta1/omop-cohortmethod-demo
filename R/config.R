# ============================================================================
# R/config.R -- read and validate config/*.yml
#
# The configuration files are the study protocol. This file is the parser that
# enforces that the protocol is well formed before any database work starts.
#
# The validation is deliberately strict and fails early. A study that runs to
# completion using a silently defaulted washout period is far more dangerous
# than one that refuses to start.
# ============================================================================

#' Load and validate both configuration files.
#'
#' @return a list with elements `cohorts` and `analysis`
load_config <- function(cohorts_file  = proj_path("config", "cohorts.yml"),
                        analysis_file = proj_path("config", "analysis_settings.yml")) {
  assert(file.exists(cohorts_file),  "Missing config file: ", cohorts_file)
  assert(file.exists(analysis_file), "Missing config file: ", analysis_file)

  cfg <- list(
    cohorts  = yaml::read_yaml(cohorts_file),
    analysis = yaml::read_yaml(analysis_file)
  )

  validate_cohort_config(cfg$cohorts)
  validate_analysis_config(cfg$analysis)
  cross_validate(cfg)

  cfg
}

# ----------------------------------------------------------------------------
# Cohort configuration
# ----------------------------------------------------------------------------
validate_cohort_config <- function(cc) {
  assert(!is.null(cc$concept_sets), "config/cohorts.yml: no concept_sets defined")
  assert(!is.null(cc$cohorts),      "config/cohorts.yml: no cohorts defined")

  # --- concept sets ---------------------------------------------------------
  ids <- vapply(cc$concept_sets, function(cs) cs$id %||% NA_character_, character(1))
  assert(!anyNA(ids), "Every concept set needs an `id`.")
  assert(!anyDuplicated(ids), "Duplicate concept set id: ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "))

  for (cs in cc$concept_sets) {
    assert(length(cs$concepts) > 0, "Concept set '", cs$id, "' contains no concepts.")
    for (cpt in cs$concepts) {
      assert(!is.null(cpt$concept_id) && is.numeric(cpt$concept_id),
             "Concept set '", cs$id, "': every entry needs a numeric concept_id.")
      # include_descendants must be stated explicitly. Defaulting it is exactly
      # the kind of silent choice this project exists to avoid -- the two
      # possible defaults give materially different cohorts.
      assert(!is.null(cpt$include_descendants),
             "Concept set '", cs$id, "', concept ", cpt$concept_id,
             ": include_descendants must be stated explicitly (true or false).")
    }
  }

  # --- cohorts --------------------------------------------------------------
  cids <- vapply(cc$cohorts, function(x) as.integer(x$id), integer(1))
  assert(!anyDuplicated(cids), "Duplicate cohort id.")

  valid_domains <- c("drug_exposure", "condition_occurrence", "procedure_occurrence",
                     "observation", "measurement", "device_exposure")
  valid_exits   <- c("end_of_continuous_exposure", "same_day",
                     "end_of_observation_period", "fixed_days")
  valid_types   <- c("target", "comparator", "outcome")

  for (ch in cc$cohorts) {
    where <- paste0("cohort ", ch$id, " ('", ch$name, "')")
    assert(!is.null(ch$name), "Every cohort needs a `name`.")
    assert(ch$type %in% valid_types, where, ": type must be one of ",
           paste(valid_types, collapse = "/"))
    assert(!is.null(ch$entry$concept_set), where, ": entry.concept_set is required.")
    assert(ch$entry$concept_set %in% ids, where, ": entry references unknown concept set '",
           ch$entry$concept_set, "'.")
    assert(ch$entry$domain %in% valid_domains, where, ": entry.domain must be one of ",
           paste(valid_domains, collapse = "/"))
    assert(!is.null(ch$entry$first_occurrence_only), where,
           ": entry.first_occurrence_only must be stated explicitly. ",
           "It is the new-user vs prevalent-user decision and must not be defaulted.")
    assert(ch$exit$rule %in% valid_exits, where, ": exit.rule must be one of ",
           paste(valid_exits, collapse = "/"))
    if (identical(ch$exit$rule, "end_of_continuous_exposure")) {
      assert(is.numeric(ch$exit$persistence_window_days), where,
             ": exit.persistence_window_days is required for end_of_continuous_exposure.")
    }
    if (identical(ch$exit$rule, "fixed_days")) {
      assert(is.numeric(ch$exit$days), where, ": exit.days is required for fixed_days.")
    }

    # Criteria must reference concept sets that exist.
    for (cr in c(ch$inclusion_criteria, ch$exclusion_criteria)) {
      if (!is.null(cr$concept_set)) {
        assert(cr$concept_set %in% ids, where, ": criterion '", cr$name,
               "' references unknown concept set '", cr$concept_set, "'.")
      }
    }
    # Exclusion criteria must say whether a same-day event disqualifies.
    # Defaulting this silently changes the cohort, so it is required.
    for (cr in ch$exclusion_criteria) {
      assert(!is.null(cr$relation), where, ": exclusion criterion '", cr$name,
             "' must state `relation:` explicitly (before_index or ",
             "on_or_before_index).")
      assert(cr$relation %in% c("before_index", "on_or_before_index"), where,
             ": criterion '", cr$name, "' has invalid relation '", cr$relation, "'.")
      assert(cr$domain %in% valid_domains, where, ": criterion '", cr$name,
             "' has invalid domain '", cr$domain, "'.")
    }
  }

  # Exactly one target and one comparator; at least one outcome.
  types <- vapply(cc$cohorts, function(x) x$type, character(1))
  assert(sum(types == "target") == 1,     "Exactly one cohort must have type: target.")
  assert(sum(types == "comparator") == 1, "Exactly one cohort must have type: comparator.")
  assert(sum(types == "outcome") >= 1,    "At least one cohort must have type: outcome.")

  invisible(TRUE)
}

# ----------------------------------------------------------------------------
# Analysis configuration
# ----------------------------------------------------------------------------
validate_analysis_config <- function(ac) {
  assert(ac$database$dbms %in% c("sqlite", "duckdb"),
         "database.dbms must be 'sqlite' or 'duckdb'.")

  tars <- c(list(ac$time_at_risk$primary), ac$time_at_risk$sensitivity)
  for (tar in tars) {
    validate_tar(tar)
  }

  assert(ac$propensity_score$strategy %in% c("matching", "stratification", "weighting"),
         "propensity_score.strategy must be matching / stratification / weighting.")

  if (identical(ac$propensity_score$strategy, "matching")) {
    assert(is.numeric(ac$propensity_score$matching$caliper),
           "matching.caliper is required.")
    assert(ac$propensity_score$matching$max_ratio >= 0,
           "matching.max_ratio must be >= 0 (0 means variable ratio).")
  }

  assert(ac$outcome_model$model_type %in% c("cox", "logistic", "poisson"),
         "outcome_model.model_type must be cox / logistic / poisson.")

  # A matched analysis with an unstratified outcome model understates the
  # standard error. Variable-ratio matching makes it worse. Refuse the
  # combination rather than quietly producing over-confident intervals.
  if (identical(ac$propensity_score$strategy, "matching") &&
      isTRUE(ac$propensity_score$matching$max_ratio == 0) &&
      !isTRUE(ac$outcome_model$stratified)) {
    stop("Variable-ratio matching (max_ratio: 0) requires outcome_model.stratified: true, ",
         "otherwise the matched-set structure is ignored and the confidence interval ",
         "will be too narrow.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_tar <- function(tar) {
  where <- paste0("time_at_risk '", tar$label %||% "(unlabelled)", "'")
  valid_anchors <- c("cohort start", "cohort end")
  assert(tar$start_anchor %in% valid_anchors, where, ": start_anchor must be ",
         paste(valid_anchors, collapse = " or "))
  assert(tar$end_anchor %in% valid_anchors, where, ": end_anchor must be ",
         paste(valid_anchors, collapse = " or "))
  assert(is.numeric(tar$risk_window_start) && is.numeric(tar$risk_window_end),
         where, ": risk_window_start/end must be numeric.")

  # A window that both starts and ends on the same anchor must not run
  # backwards. Anchored on different endpoints the ordering is data-dependent
  # and cannot be checked here, so CohortMethod handles it per subject.
  if (identical(tar$start_anchor, tar$end_anchor)) {
    assert(tar$risk_window_end >= tar$risk_window_start,
           where, ": risk window ends before it starts (",
           tar$risk_window_start, " to ", tar$risk_window_end, ").")
  }
  invisible(TRUE)
}

# ----------------------------------------------------------------------------
# Cross-file checks
# ----------------------------------------------------------------------------
cross_validate <- function(cfg) {
  cc <- cfg$cohorts
  ac <- cfg$analysis

  # The covariate windows must not look past index. A covariate measured after
  # time zero may be a consequence of treatment (a mediator) or of the outcome
  # (a collider); adjusting for either introduces bias rather than removing it.
  # This is the single check in this file most worth having.
  for (nm in names(ac$covariates$windows)) {
    w <- ac$covariates$windows[[nm]]
    assert(w$end <= 0,
           "covariates.windows.", nm, ".end is ", w$end, ", which is after index. ",
           "Baseline covariate windows must end at or before day 0, otherwise the ",
           "propensity model can condition on post-treatment information.")
    assert(w$start <= w$end,
           "covariates.windows.", nm, ": start (", w$start, ") is after end (", w$end, ").")
  }

  # Warn -- not fail -- if the outcome washout demands more history than
  # eligibility guarantees. It is legitimate (all-history washout), but the
  # rule is then applied unevenly across people, and that should be a
  # conscious choice.
  target <- Filter(function(x) x$type == "target", cc$cohorts)[[1]]
  prior_obs <- Find(function(x) identical(x$name, "prior_observation"),
                    target$inclusion_criteria)
  washout <- Find(function(x) identical(x$name, "no_prior_outcome"),
                  target$exclusion_criteria)
  if (!is.null(prior_obs) && !is.null(washout) && is.null(washout$lookback_days)) {
    message(
      "NOTE: the prior-outcome washout uses all available history, while ",
      "eligibility guarantees only ", prior_obs$days_before, " days. ",
      "People with longer records are therefore screened more thoroughly. ",
      "Legitimate, but be ready to say why."
    )
  }

  invisible(TRUE)
}

# ----------------------------------------------------------------------------
# Accessors -- keep config shape out of the analysis scripts
# ----------------------------------------------------------------------------
get_cohort <- function(cfg, type) {
  m <- Filter(function(x) identical(x$type, type), cfg$cohorts$cohorts)
  assert(length(m) >= 1, "No cohort of type '", type, "' in config.")
  m[[1]]
}

get_concept_set <- function(cfg, id) {
  m <- Filter(function(x) identical(x$id, id), cfg$cohorts$concept_sets)
  assert(length(m) == 1, "No concept set with id '", id, "'.")
  m[[1]]
}

#' The concept IDs literally written in the config for a concept set.
#'
#' These are the *seed* concepts, before any descendant expansion. Use
#' resolve_concept_set() (R/concept_sets.R) for the resolved membership.
concept_set_ids <- function(cfg, id, include_excluded = FALSE) {
  cs <- get_concept_set(cfg, id)
  keep <- Filter(function(x) include_excluded || !isTRUE(x$is_excluded), cs$concepts)
  vapply(keep, function(x) as.integer(x$concept_id), integer(1))
}

#' All time-at-risk windows, primary first, each tagged with is_primary.
get_all_tars <- function(cfg) {
  p <- cfg$analysis$time_at_risk$primary
  p$is_primary <- TRUE
  s <- lapply(cfg$analysis$time_at_risk$sensitivity, function(x) { x$is_primary <- FALSE; x })
  c(list(p), s)
}

#' Print the protocol as a human-readable summary.
#'
#' Run at the top of every analysis script so that the console log records
#' exactly which protocol produced the results underneath it.
print_protocol <- function(cfg) {
  cc <- cfg$cohorts; ac <- cfg$analysis
  tgt <- get_cohort(cfg, "target"); cmp <- get_cohort(cfg, "comparator")
  out <- get_cohort(cfg, "outcome"); tar <- ac$time_at_risk$primary

  log_header("STUDY PROTOCOL: ", cc$meta$study_name)
  cat(strwrap(trimws(cc$meta$title), width = 78, prefix = "  "), sep = "\n")
  cat("\n")
  log_info("Design            : new-user, active-comparator cohort study")
  log_info("Target            : ", tgt$label)
  log_info("Comparator        : ", cmp$label)
  log_info("Outcome           : ", out$label)
  log_info("Time at risk      : ", tar$label)
  log_info("                    day ", tar$risk_window_start, " after ", tar$start_anchor,
           " to day ", tar$risk_window_end, " after ", tar$end_anchor)
  log_info("Covariates        : ",
           if (isTRUE(ac$covariates$use_large_scale)) "large-scale (data-driven)" else "manually specified")
  log_info("Adjustment        : ", ac$propensity_score$strategy,
           if (identical(ac$propensity_score$strategy, "matching"))
             paste0(" (", ac$propensity_score$matching$max_ratio, ":1, caliper ",
                    ac$propensity_score$matching$caliper, " ",
                    ac$propensity_score$matching$caliper_scale, ")") else "")
  log_info("Outcome model     : ", ac$outcome_model$model_type,
           if (isTRUE(ac$outcome_model$stratified)) ", stratified by matched set" else "",
           if (isTRUE(ac$outcome_model$use_covariates)) ", covariate-adjusted" else "")
  log_info("Database          : Eunomia '", ac$database$dataset_name, "' via ", ac$database$dbms)
  invisible(NULL)
}
