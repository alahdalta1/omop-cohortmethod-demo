# ============================================================================
# analysis/03_run_cohort_method.R
#
# The comparative cohort analysis: covariates, propensity score, matching,
# and a Cox outcome model, for the primary time-at-risk window and every
# configured sensitivity window.
#
#   Rscript analysis/03_run_cohort_method.R
#
# Requires 02_build_cohorts.R to have been run (it reads the cohort table).
#
# Outputs:
#   output/cohort_method/analysis_objects.rds   all intermediate objects
#   output/cohort_method/estimates.csv          one row per time-at-risk window
#   output/cohort_method/attrition_*.csv        CohortMethod's own attrition
# ============================================================================

source("R/init.R")

log_header("03 -- COMPARATIVE COHORT ANALYSIS WITH PROPENSITY SCORE MATCHING")
print_protocol(cfg)

schema             <- cfg$analysis$database$cdm_database_schema
connection_details <- get_connection_details(cfg)

# The concept sets are needed again here, to tell FeatureExtraction which
# concepts to exclude from the covariate set.
connection   <- db_connect(connection_details)
concept_sets <- resolve_all_concept_sets(connection, schema, cfg, count_records = FALSE)
db_disconnect(connection)

# ----------------------------------------------------------------------------
# 1. Covariate construction
# ----------------------------------------------------------------------------
log_step("Building covariate settings")
covariate_settings <- build_covariate_settings(cfg, concept_sets$sets)

# ----------------------------------------------------------------------------
# 2. Extract target, comparator, outcome and covariates in one object
# ----------------------------------------------------------------------------
cm_data <- extract_cohort_method_data(cfg, connection_details, covariate_settings)

attrition <- CohortMethod::getAttritionTable(cm_data)
save_table(as.data.frame(attrition), "cohort_method", "attrition_extraction.csv")

# ----------------------------------------------------------------------------
# 3. Run the analysis for every configured time-at-risk window
# ----------------------------------------------------------------------------
log_header("RUNNING ANALYSES ACROSS TIME-AT-RISK WINDOWS")

log_note("The primary window runs first, then each sensitivity window. They ",
         "share the same cohorts and the same covariates -- only the ",
         "follow-up definition changes -- so any difference between them is ",
         "attributable to time at risk alone.")

results <- run_all_analyses(cm_data, cfg)

# ----------------------------------------------------------------------------
# 4. Collect the estimates
# ----------------------------------------------------------------------------
estimates <- do.call(rbind, lapply(results, function(r) r$summary))
rownames(estimates) <- NULL
save_table(estimates, "cohort_method", "estimates.csv")

log_step("Estimates across time-at-risk windows")
print_df(estimates[, c("time_at_risk", "is_primary", "n_target", "n_comparator",
                       "events_target", "events_comparator",
                       "hazard_ratio", "ci_lower", "ci_upper", "p_value", "status")])

# ----------------------------------------------------------------------------
# 5. Persist everything for the diagnostics script
# ----------------------------------------------------------------------------
# The CohortMethodData object is an Andromeda (disk-backed) object and cannot
# be serialised with saveRDS, so it is saved separately in its own format.
CohortMethod::saveCohortMethodData(
  cm_data, out_path("cohort_method", "cohort_method_data.zip"))

saveRDS(
  lapply(results, function(r) r[c("tar", "study_pop", "ps", "adjusted",
                                  "model", "balance", "auc", "equipoise",
                                  "summary")]),
  out_path("cohort_method", "analysis_objects.rds")
)
log_info("Saved analysis objects for 04_diagnostics.R")

log_header("03 COMPLETE -- see output/cohort_method/")
