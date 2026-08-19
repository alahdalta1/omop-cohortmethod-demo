# ============================================================================
# analysis/04_diagnostics.R
#
# Study diagnostics for the primary analysis: propensity score overlap and
# equipoise, covariate balance, follow-up comparability and statistical power.
#
#   Rscript analysis/04_diagnostics.R
#
# Requires 03_run_cohort_method.R.
#
# Read this output BEFORE the effect estimate. If the diagnostics fail, the
# hazard ratio is not a finding, it is an artefact -- and knowing that only
# after you have seen the number is much harder than knowing it before.
#
# Outputs: output/figures/*.png and output/tables/*.csv
# ============================================================================

source("R/init.R")

log_header("04 -- STUDY DIAGNOSTICS")

results <- readRDS(out_path("cohort_method", "analysis_objects.rds"))

# The primary analysis is the one flagged is_primary in the config.
primary_name <- cfg$analysis$time_at_risk$primary$label
primary <- results[[primary_name]]
assert(!is.null(primary), "Primary analysis '", primary_name,
       "' not found in saved results. Re-run 03_run_cohort_method.R.")

log_info("Diagnostics for the primary window: ", primary_name)

# ----------------------------------------------------------------------------
# 1. Was there anyone to compare? -- PS overlap and equipoise
# ----------------------------------------------------------------------------
ps_diag <- summarise_ps_diagnostics(primary, cfg)
if (!is.null(ps_diag)) save_table(ps_diag, "tables", "ps_diagnostics.csv")

plot_ps_overlap(primary, cfg)
plot_ps_after(primary, cfg)

# ----------------------------------------------------------------------------
# 2. Did adjustment make the groups comparable? -- covariate balance
# ----------------------------------------------------------------------------
bal <- summarise_balance(primary, cfg)
if (!is.null(bal)) {
  save_table(bal$summary, "tables", "balance_summary.csv")
  save_table(bal$worst,   "tables", "balance_worst_covariates.csv")
  save_table(bal$full,    "tables", "balance_all_covariates.csv")
}

plot_balance(primary, cfg)
plot_top_covariates(primary)

# Table 1: baseline characteristics before and after matching.
t1 <- build_table1(primary, cfg)
if (!is.null(t1)) {
  save_table(t1, "tables", "table1_baseline_characteristics.csv")
  log_info("Wrote Table 1 (baseline characteristics before/after matching)")
}

# ----------------------------------------------------------------------------
# 3. Is follow-up comparable between arms?
# ----------------------------------------------------------------------------
fu <- summarise_followup(primary, cfg)
if (!is.null(fu)) save_table(fu, "tables", "followup_distribution.csv")
plot_followup(primary, cfg)

# ----------------------------------------------------------------------------
# 4. Is there enough signal to conclude anything?
# ----------------------------------------------------------------------------
power <- summarise_power(primary, cfg)
if (!is.null(power)) save_table(power, "tables", "power_mdrr.csv")

# ----------------------------------------------------------------------------
# 5. Attrition through the analysis stages
# ----------------------------------------------------------------------------
att <- build_attrition(primary)
if (!is.null(att)) {
  log_step("Attrition through the CohortMethod stages")
  print_df(att)
  save_table(att, "tables", "attrition_analysis.csv")
  cat("\n")
  log_note("This is CohortMethod's own attrition, downstream of the cohort ",
           "definitions. The largest drop is matching, which is expected and ",
           "is not a defect: unmatched subjects are people for whom no ",
           "comparable counterpart exists, and including them would mean ",
           "extrapolating rather than comparing. It does change the estimand ",
           "-- the result applies to the matched population, not to everyone ",
           "who started celecoxib.")
}

# ----------------------------------------------------------------------------
# 6. Survival curves
# ----------------------------------------------------------------------------
plot_km(primary, cfg)

# ----------------------------------------------------------------------------
# Verdict
# ----------------------------------------------------------------------------
log_header("DIAGNOSTIC VERDICT")

d <- cfg$analysis$diagnostics
checks <- list()
if (!is.null(ps_diag)) {
  checks$auc <- list(
    name = paste0("PS AUC <= ", d$max_ps_auc),
    pass = ps_diag$value[ps_diag$metric == "ps_auc"] <= d$max_ps_auc,
    value = ps_diag$value[ps_diag$metric == "ps_auc"])
  checks$equipoise <- list(
    name = paste0("Equipoise >= ", d$min_equipoise),
    pass = ps_diag$value[ps_diag$metric == "equipoise_0.3_0.7"] >= d$min_equipoise,
    value = ps_diag$value[ps_diag$metric == "equipoise_0.3_0.7"])
}
if (!is.null(bal)) {
  mx <- bal$summary$value[bal$summary$metric == "max_abs_smd_after"]
  checks$balance <- list(
    name = paste0("Max |SMD| after adjustment <= ", d$max_standardized_difference),
    pass = mx <= d$max_standardized_difference, value = mx)
}
n_ev <- primary$summary$events_target + primary$summary$events_comparator
checks$events <- list(
  name = paste0("Outcome events >= ", d$min_outcome_events),
  pass = n_ev >= d$min_outcome_events, value = n_ev)

for (ck in checks) {
  cat(sprintf("      %-46s %10s   %s\n", ck$name, format(ck$value),
              if (isTRUE(ck$pass)) "PASS" else "FAIL"))
}

n_fail <- sum(!vapply(checks, function(x) isTRUE(x$pass), logical(1)))
cat("\n")
if (n_fail == 0) {
  log_note("All configured diagnostic thresholds are met.")
} else {
  log_note(n_fail, " threshold(s) not met. That does not automatically ",
           "invalidate the study, but it does mean you must say which, and ",
           "why you are proceeding. A failed balance threshold driven by ",
           "covariates unrelated to bleeding is a different matter from one ",
           "driven by prior anticoagulant use.")
}

log_decision(
  "COVARIATE SELECTION -- revisit now that you can see the balance",
  "The balance plot shows what the large-scale propensity model achieved. ",
  "Two things to weigh. (1) Residual imbalance: check ",
  "output/tables/balance_worst_covariates.csv and ask whether the covariates ",
  "still above threshold are prognostic for GI bleeding, or merely numerous. ",
  "(2) Instruments: a covariate that strongly predicts treatment but has no ",
  "path to the outcome amplifies unmeasured confounding rather than reducing ",
  "it. The large-scale approach cannot distinguish an instrument from a ",
  "confounder, which is the main argument for a hypothesis-driven covariate ",
  "set you can draw as a DAG. Running both and comparing is the strongest ",
  "version of this project."
)

log_header("04 COMPLETE -- see output/figures/ and output/tables/")
