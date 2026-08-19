# ============================================================================
# R/cohort_method.R -- the comparative cohort analysis.
#
# The pipeline, and what each step is for:
#
#   1. Covariate construction   thousands of baseline features per person,
#                               all measured strictly before time zero
#   2. Study population         apply time at risk and population restrictions
#   3. Propensity score         P(treated | baseline covariates), by
#                               L1-regularised logistic regression
#   4. Adjustment               matching / stratification / weighting on the PS
#   5. Outcome model            Cox proportional hazards on the adjusted set
#
# The order matters and is not arbitrary. Covariates come from before index so
# they cannot be affected by treatment. The study population is defined before
# the PS is fitted, so the PS is estimated in the population it will be used
# in. The outcome model is fitted last and, by default, sees no covariates --
# all confounding control has already happened, which keeps the two jobs
# (adjustment, estimation) separable and inspectable.
#
# Everything here is driven from config/analysis_settings.yml.
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Covariates
# ----------------------------------------------------------------------------

#' Build FeatureExtraction covariate settings from the config.
#'
#' The single most important property of this function is that every window
#' ends at or before day 0. R/config.R enforces it; this is where it takes
#' effect.
build_covariate_settings <- function(cfg, resolved_sets) {
  cv <- cfg$analysis$covariates
  w  <- cv$windows

  # FeatureExtraction uses ONE end day for all three windows. Config allows a
  # separate end per window, so check they agree rather than silently using
  # the first.
  ends <- vapply(w, function(x) as.numeric(x$end), numeric(1))
  assert(length(unique(ends)) == 1,
         "FeatureExtraction applies a single endDays to all covariate windows, ",
         "but config specifies different ends (", paste(ends, collapse = ", "),
         "). Make covariates.windows.*.end identical.")
  end_days <- unique(ends)

  if (!isTRUE(cv$use_large_scale)) {
    ids <- as.integer(cv$manual_covariate_concept_ids)
    assert(length(ids) > 0,
           "covariates.use_large_scale is false but manual_covariate_concept_ids ",
           "is empty -- there would be no covariates at all.")
    log_info("Covariates: MANUAL, ", length(ids), " concept(s)")
    return(FeatureExtraction::createCovariateSettings(
      useDemographicsGender = isTRUE(cv$demographics$gender),
      useDemographicsAgeGroup = isTRUE(cv$demographics$age_group),
      useConditionOccurrenceLongTerm = TRUE,
      useDrugExposureLongTerm = TRUE,
      longTermStartDays = as.numeric(w$long_term$start),
      endDays = end_days,
      includedCovariateConceptIds = ids,
      addDescendantsToInclude = TRUE
    ))
  }

  # --- concepts to exclude from the PS model --------------------------------
  # The exposures themselves. Leaving them in lets the model predict treatment
  # from the treatment, giving complete separation and a useless PS.
  excluded <- integer(0)
  if (isTRUE(cv$exclude_exposure_concepts)) {
    tgt <- get_cohort(cfg, "target"); cmp <- get_cohort(cfg, "comparator")
    excluded <- c(
      resolved_sets[[tgt$entry$concept_set]]$concept_id,
      resolved_sets[[cmp$entry$concept_set]]$concept_id
    )
  }
  excluded <- unique(c(excluded, as.integer(cv$additional_excluded_concept_ids)))

  dom <- cv$domains
  dg  <- cv$demographics
  rs  <- cv$risk_scores
  on  <- function(x) isTRUE(x)

  settings <- FeatureExtraction::createCovariateSettings(
    # -- demographics --------------------------------------------------------
    useDemographicsGender               = on(dg$gender),
    useDemographicsAgeGroup             = on(dg$age_group),
    useDemographicsRace                 = on(dg$race),
    useDemographicsEthnicity            = on(dg$ethnicity),
    useDemographicsIndexYear            = on(dg$index_year),
    useDemographicsIndexMonth           = on(dg$index_month),
    useDemographicsPriorObservationTime = on(dg$prior_observation_time),
    useDemographicsTimeInCohort         = on(dg$time_in_cohort),
    # useDemographicsPostObservationTime is deliberately never enabled: it is
    # measured after index and is therefore not a baseline characteristic.

    # -- conditions ----------------------------------------------------------
    useConditionOccurrenceLongTerm   = on(dom$condition_occurrence),
    useConditionOccurrenceMediumTerm = on(dom$condition_occurrence),
    useConditionOccurrenceShortTerm  = on(dom$condition_occurrence),
    useConditionGroupEraLongTerm     = on(dom$condition_era_group),

    # -- drugs ---------------------------------------------------------------
    useDrugExposureLongTerm   = on(dom$drug_exposure),
    useDrugExposureMediumTerm = on(dom$drug_exposure),
    useDrugExposureShortTerm  = on(dom$drug_exposure),
    useDrugGroupEraLongTerm   = on(dom$drug_era_group),

    # -- procedures, measurements, observations, devices ---------------------
    useProcedureOccurrenceLongTerm   = on(dom$procedure_occurrence),
    useProcedureOccurrenceMediumTerm = on(dom$procedure_occurrence),
    useProcedureOccurrenceShortTerm  = on(dom$procedure_occurrence),
    useMeasurementLongTerm           = on(dom$measurement),
    useMeasurementShortTerm          = on(dom$measurement),
    useObservationLongTerm           = on(dom$observation),
    useObservationShortTerm          = on(dom$observation),
    useDeviceExposureLongTerm        = on(dom$device_exposure),

    # -- utilisation (a handle on surveillance intensity) --------------------
    useVisitCountLongTerm        = on(dom$visit_count),
    useVisitConceptCountLongTerm = on(dom$visit_concept_count),

    # -- interpretable risk scores -------------------------------------------
    useCharlsonIndex = on(rs$charlson_comorbidity_index),
    useDcsi          = on(rs$dcsi),
    useChads2        = on(rs$chads2),

    # -- windows -------------------------------------------------------------
    longTermStartDays   = as.numeric(w$long_term$start),
    mediumTermStartDays = as.numeric(w$medium_term$start),
    shortTermStartDays  = as.numeric(w$short_term$start),
    endDays             = end_days,

    # -- exclusions ----------------------------------------------------------
    excludedCovariateConceptIds = excluded,
    addDescendantsToExclude     = TRUE
  )

  log_info("Covariates: LARGE-SCALE")
  log_info("  windows (days rel. index): long ", w$long_term$start, " to ", end_days,
           " | medium ", w$medium_term$start, " to ", end_days,
           " | short ", w$short_term$start, " to ", end_days)
  log_info("  excluded concepts (+descendants): ",
           if (length(excluded)) paste(excluded, collapse = ", ") else "(none)")

  cat("\n")
  log_note("The excluded concepts are the exposures themselves. Without this ",
           "exclusion the propensity model would include 'was dispensed ",
           "celecoxib' as a predictor of receiving celecoxib, separate the ",
           "groups perfectly, and leave no overlap to match on. It is the ",
           "most common way a first large-scale PS attempt fails.")
  log_note("Every window ends at day ", end_days, ". Nothing measured after ",
           "index enters the model, so no covariate can be a mediator of ",
           "treatment or a collider on the outcome path.")

  settings
}

# ----------------------------------------------------------------------------
# 2. Extract the analysis dataset
# ----------------------------------------------------------------------------

#' Pull cohorts, outcomes and covariates into a CohortMethodData object.
extract_cohort_method_data <- function(cfg, connection_details, covariate_settings) {
  log_step("Extracting CohortMethod data")

  db  <- cfg$analysis$database
  sp  <- cfg$analysis$study_population
  tgt <- get_cohort(cfg, "target")
  cmp <- get_cohort(cfg, "comparator")
  out <- get_cohort(cfg, "outcome")

  args <- CohortMethod::createGetDbCohortMethodDataArgs(
    # firstExposureOnly / washoutPeriod are FALSE / 0 here because the cohort
    # definitions in config/cohorts.yml already enforce first-ever exposure and
    # the 365-day prior-observation requirement. Applying them twice would be
    # harmless but would hide where the restriction actually comes from, and
    # the attrition table would no longer explain the cohort size.
    firstExposureOnly       = FALSE,
    washoutPeriod           = 0,
    removeDuplicateSubjects = sp$remove_duplicate_subjects,
    restrictToCommonPeriod  = isTRUE(sp$restrict_to_common_period),
    maxCohortSize           = 0,
    covariateSettings       = covariate_settings
  )

  cm_data <- CohortMethod::getDbCohortMethodData(
    connectionDetails         = connection_details,
    cdmDatabaseSchema         = db$cdm_database_schema,
    targetId                  = tgt$id,
    comparatorId              = cmp$id,
    outcomeIds                = out$id,
    exposureDatabaseSchema    = db$results_database_schema,
    exposureTable             = db$cohort_table,
    outcomeDatabaseSchema     = db$results_database_schema,
    outcomeTable              = db$cohort_table,
    getDbCohortMethodDataArgs = args
  )

  # cohorts/covariateRef are Andromeda (disk-backed) tables, so nrow() does not
  # apply -- they must be counted through dplyr's database backend.
  n_cov <- as.integer(dplyr::pull(dplyr::count(cm_data$covariateRef), "n"))
  n_pop <- as.integer(dplyr::pull(dplyr::count(cm_data$cohorts), "n"))
  log_info("Subjects extracted : ", format(n_pop, big.mark = ","))
  log_info("Covariates built   : ", format(n_cov, big.mark = ","))

  cat("\n")
  log_note("Each covariate is a binary or count feature such as 'condition X ",
           "recorded in the 365 days before index'. The propensity model will ",
           "consider all ", format(n_cov, big.mark = ","), " of them and, ",
           "through L1 regularisation, keep only those that carry information ",
           "about treatment choice.")

  cm_data
}

# ----------------------------------------------------------------------------
# 3-5. One full analysis for a given time-at-risk window
# ----------------------------------------------------------------------------

#' Run study population -> PS -> adjustment -> outcome model for one TAR.
#'
#' @return list of intermediate objects plus a one-row summary
run_tar_analysis <- function(cm_data, cfg, tar) {
  ac  <- cfg$analysis
  sp  <- ac$study_population
  ps_cfg <- ac$propensity_score
  om  <- ac$outcome_model
  out <- get_cohort(cfg, "outcome")

  log_step("Time at risk: ", tar$label)
  log_info("day ", tar$risk_window_start, " after ", tar$start_anchor,
           "  ->  day ", tar$risk_window_end, " after ", tar$end_anchor)

  # --- 3a. Study population -------------------------------------------------
  pop_args <- CohortMethod::createCreateStudyPopulationArgs(
    removeSubjectsWithPriorOutcome = isTRUE(sp$remove_subjects_with_prior_outcome),
    priorOutcomeLookback           = as.numeric(sp$prior_outcome_lookback_days),
    minDaysAtRisk                  = as.numeric(ac$time_at_risk$minimum_days_at_risk),
    riskWindowStart                = as.numeric(tar$risk_window_start),
    startAnchor                    = tar$start_anchor,
    riskWindowEnd                  = as.numeric(tar$risk_window_end),
    endAnchor                      = tar$end_anchor,
    censorAtNewRiskWindow          = FALSE
  )

  study_pop <- CohortMethod::createStudyPopulation(
    cohortMethodData          = cm_data,
    outcomeId                 = out$id,
    createStudyPopulationArgs = pop_args
  )

  n_t <- sum(study_pop$treatment == 1); n_c <- sum(study_pop$treatment == 0)
  e_t <- sum(study_pop$outcomeCount[study_pop$treatment == 1] > 0)
  e_c <- sum(study_pop$outcomeCount[study_pop$treatment == 0] > 0)
  log_info("Study population : ", format(n_t, big.mark = ","), " target / ",
           format(n_c, big.mark = ","), " comparator")
  log_info("Outcome events   : ", e_t, " target / ", e_c, " comparator")

  if (e_t + e_c < ac$diagnostics$min_outcome_events) {
    log_note("WARNING: only ", e_t + e_c, " events, below the configured ",
             "minimum of ", ac$diagnostics$min_outcome_events, ". Any hazard ",
             "ratio from this window is uninterpretable however narrow its ",
             "confidence interval looks. This is expected for the degenerate ",
             "on-treatment window and is the point of keeping it in the table.")
  }

  if (n_t == 0 || n_c == 0 || (e_t + e_c) == 0) {
    log_note("Cannot fit a model for this window -- one arm is empty or there ",
             "are no events. Reporting it as not estimable rather than ",
             "dropping it silently.")
    return(list(
      tar = tar, study_pop = study_pop, ps = NULL, adjusted = NULL,
      model = NULL, balance = NULL,
      summary = estimate_row(tar, cfg, n_t, n_c, e_t, e_c, NULL, NULL,
                             status = "not estimable (no events / empty arm)")
    ))
  }

  # --- 3b. Propensity score -------------------------------------------------
  log_info("Fitting propensity model (L1-regularised logistic regression)...")
  ps_args <- CohortMethod::createCreatePsArgs(
    errorOnHighCorrelation = isTRUE(ps_cfg$estimation$error_on_high_correlation),
    stopOnError            = FALSE,
    prior = Cyclops::createPrior(
      priorType = ps_cfg$estimation$prior,
      exclude   = c(0),
      useCrossValidation = isTRUE(ps_cfg$estimation$use_cross_validation)
    ),
    control = Cyclops::createControl(
      noiseLevel    = "silent",
      cvType        = "auto",
      cvRepetitions = as.integer(ps_cfg$estimation$cv_repetitions),
      tolerance     = as.numeric(ps_cfg$estimation$control_tolerance),
      maxIterations = as.integer(ps_cfg$estimation$control_max_iterations),
      seed          = 1,
      startingVariance = 0.01
    )
  )

  ps <- try(CohortMethod::createPs(cohortMethodData = cm_data,
                                   population = study_pop,
                                   createPsArgs = ps_args), silent = TRUE)

  if (inherits(ps, "try-error")) {
    log_note("Propensity model FAILED: ", conditionMessage(attr(ps, "condition")))
    return(list(tar = tar, study_pop = study_pop, ps = NULL, adjusted = NULL,
                model = NULL, balance = NULL,
                summary = estimate_row(tar, cfg, n_t, n_c, e_t, e_c, NULL, NULL,
                                       status = "PS model failed")))
  }

  auc <- CohortMethod::computePsAuc(ps)
  auc_val <- if (is.data.frame(auc)) auc[[1]][1] else as.numeric(auc)[1]
  equipoise <- CohortMethod::computeEquipoise(
    ps, equipoiseBounds = c(0.3, 0.7))

  log_info("PS AUC           : ", sprintf("%.3f", auc_val))
  log_info("Equipoise (0.3-0.7 pref. score): ", sprintf("%.3f", equipoise))

  interpret_ps(auc_val, equipoise, cfg)

  # --- 3c. Trimming (optional) ---------------------------------------------
  trimmed <- ps
  if (isTRUE(ps_cfg$trim_to_equipoise)) {
    trimmed <- CohortMethod::trimByPs(
      population = ps,
      trimByPsArgs = CohortMethod::createTrimByPsArgs(
        trimMethod = "equipoise",
        equipoiseBounds = as.numeric(ps_cfg$equipoise_bounds))
    )
    log_info("Trimmed to equipoise: ", format(nrow(ps), big.mark = ","), " -> ",
             format(nrow(trimmed), big.mark = ","), " subjects")
  }

  # --- 4. Adjustment --------------------------------------------------------
  strategy <- ps_cfg$strategy
  if (identical(strategy, "matching")) {
    m <- ps_cfg$matching
    adjusted <- CohortMethod::matchOnPs(
      population = trimmed,
      matchOnPsArgs = CohortMethod::createMatchOnPsArgs(
        caliper      = as.numeric(m$caliper),
        caliperScale = m$caliper_scale,
        maxRatio     = as.integer(m$max_ratio),
        allowReverseMatch = FALSE)
    )
    log_info("Matched          : ", format(nrow(trimmed), big.mark = ","), " -> ",
             format(nrow(adjusted), big.mark = ","), " subjects (",
             length(unique(adjusted$stratumId)), " matched sets)")
  } else if (identical(strategy, "stratification")) {
    adjusted <- CohortMethod::stratifyByPs(
      population = trimmed,
      stratifyByPsArgs = CohortMethod::createStratifyByPsArgs(
        numberOfStrata = as.integer(ps_cfg$stratification$number_of_strata),
        baseSelection  = ps_cfg$stratification$base_selection)
    )
    log_info("Stratified into ", ps_cfg$stratification$number_of_strata, " strata")
  } else {
    adjusted <- trimmed   # IPTW: weights applied inside the outcome model
    log_info("Weighting (IPTW) -- no subjects removed")
  }

  n_t2 <- sum(adjusted$treatment == 1); n_c2 <- sum(adjusted$treatment == 0)
  e_t2 <- sum(adjusted$outcomeCount[adjusted$treatment == 1] > 0)
  e_c2 <- sum(adjusted$outcomeCount[adjusted$treatment == 0] > 0)
  log_info("After adjustment : ", format(n_t2, big.mark = ","), " target / ",
           format(n_c2, big.mark = ","), " comparator; events ", e_t2, " / ", e_c2)

  # --- 4b. Balance ----------------------------------------------------------
  balance <- try(CohortMethod::computeCovariateBalance(
    population = adjusted, cohortMethodData = cm_data), silent = TRUE)
  if (inherits(balance, "try-error")) balance <- NULL

  # --- 5. Outcome model -----------------------------------------------------
  log_info("Fitting outcome model (", om$model_type,
           if (isTRUE(om$stratified)) ", stratified" else "", ")...")

  fit_args <- CohortMethod::createFitOutcomeModelArgs(
    modelType          = om$model_type,
    stratified         = isTRUE(om$stratified) && !identical(strategy, "weighting"),
    useCovariates      = isTRUE(om$use_covariates),
    inversePtWeighting = identical(strategy, "weighting"),
    profileBounds      = if (isTRUE(om$profile_likelihood)) c(log(0.1), log(10)) else NULL,
    prior   = Cyclops::createPrior("laplace", useCrossValidation = TRUE),
    control = Cyclops::createControl(cvType = "auto", seed = 1, resetCoefficients = TRUE,
                                     startingVariance = 0.01, tolerance = 2e-07,
                                     noiseLevel = "quiet")
  )

  model <- try(CohortMethod::fitOutcomeModel(
    population = adjusted, cohortMethodData = cm_data,
    fitOutcomeModelArgs = fit_args), silent = TRUE)

  if (inherits(model, "try-error")) {
    log_note("Outcome model FAILED: ", conditionMessage(attr(model, "condition")))
    model <- NULL
  }

  list(
    tar = tar, study_pop = study_pop, ps = ps, adjusted = adjusted,
    model = model, balance = balance, auc = auc_val, equipoise = equipoise,
    summary = estimate_row(tar, cfg, n_t2, n_c2, e_t2, e_c2, model, adjusted,
                           auc = auc_val, equipoise = equipoise)
  )
}

#' Interpret the PS diagnostics against the configured thresholds.
interpret_ps <- function(auc, equipoise, cfg) {
  d <- cfg$analysis$diagnostics
  cat("\n")
  if (auc > d$max_ps_auc) {
    log_note("AUC of ", sprintf("%.3f", auc), " exceeds the threshold of ",
             d$max_ps_auc, ". Treatment is highly predictable from baseline ",
             "characteristics, which means the two groups are substantially ",
             "different populations. Matching may still work, but it will ",
             "discard many people and the matched population may no longer ",
             "resemble either original arm.")
  } else {
    log_note("An AUC of ", sprintf("%.3f", auc), " means treatment is only ",
             "moderately predictable from baseline data, which is the ",
             "comfortable case: the arms overlap. Note that LOW AUC is good ",
             "here -- the opposite of how AUC is read in a prediction model. ",
             "An AUC near 1.0 would mean no comparable comparator exists.")
  }
  if (equipoise < d$min_equipoise) {
    log_note("Equipoise of ", sprintf("%.3f", equipoise), " is below ",
             d$min_equipoise, ": few people had a genuine chance of either ",
             "treatment, so the comparison is only weakly identifiable.")
  } else {
    log_note("Equipoise of ", sprintf("%.3f", equipoise), " means that share ",
             "of subjects had a preference score between 0.3 and 0.7 -- ",
             "people for whom either drug was a plausible choice. That is the ",
             "population in which the comparison is actually answerable.")
  }
  invisible(NULL)
}

#' One row of the results table.
estimate_row <- function(tar, cfg, n_t, n_c, e_t, e_c, model, population,
                         auc = NA_real_, equipoise = NA_real_, status = "ok") {

  hr <- lb <- ub <- p <- se <- NA_real_
  pt_t <- pt_c <- NA_real_

  if (!is.null(population)) {
    pt_t <- sum(population$timeAtRisk[population$treatment == 1]) / 365.25
    pt_c <- sum(population$timeAtRisk[population$treatment == 0]) / 365.25
  }

  if (!is.null(model)) {
    # CohortMethod's OutcomeModel keeps the treatment effect in
    # outcomeModelTreatmentEstimate. There is no summary() method for the
    # class, so read the data frame directly rather than relying on generics.
    te <- model$outcomeModelTreatmentEstimate
    if (!is.null(te) && nrow(te) >= 1) {
      hr <- exp(te$logRr[1])
      if (!is.null(te$logLb95)) lb <- exp(te$logLb95[1])
      if (!is.null(te$logUb95)) ub <- exp(te$logUb95[1])
      if (!is.null(te$seLogRr)) se <- te$seLogRr[1]
      # Two-sided Wald p-value from the log hazard ratio and its standard
      # error. Note the mild inconsistency worth knowing about: the interval
      # above comes from likelihood profiling (asymmetric) while this p-value
      # is Wald (symmetric). With few events they can disagree slightly, and
      # the interval is the more trustworthy of the two -- report it, and
      # treat the p-value as secondary.
      if (!is.na(se) && se > 0) p <- 2 * stats::pnorm(-abs(te$logRr[1] / se))
    }
    if (!is.null(model$outcomeModelStatus) &&
        !identical(model$outcomeModelStatus, "OK")) {
      status <- as.character(model$outcomeModelStatus)
    }
  }

  data.frame(
    time_at_risk        = tar$label,
    is_primary          = isTRUE(tar$is_primary),
    window              = sprintf("day %g (%s) to day %g (%s)",
                                  tar$risk_window_start, tar$start_anchor,
                                  tar$risk_window_end, tar$end_anchor),
    status              = status,
    n_target            = n_t,
    n_comparator        = n_c,
    events_target       = e_t,
    events_comparator   = e_c,
    person_years_target     = round(pt_t, 1),
    person_years_comparator = round(pt_c, 1),
    rate_target_per_1000py     = round(1000 * e_t / pt_t, 2),
    rate_comparator_per_1000py = round(1000 * e_c / pt_c, 2),
    hazard_ratio        = round(hr, 3),
    ci_lower            = round(lb, 3),
    ci_upper            = round(ub, 3),
    se_log_hr           = round(se, 4),
    p_value             = signif(p, 3),
    ps_auc              = round(auc, 3),
    equipoise           = round(equipoise, 3),
    stringsAsFactors    = FALSE
  )
}

#' Run every configured time-at-risk window.
run_all_analyses <- function(cm_data, cfg) {
  tars <- get_all_tars(cfg)
  results <- list()
  for (tar in tars) {
    results[[tar$label]] <- run_tar_analysis(cm_data, cfg, tar)
  }
  results
}
