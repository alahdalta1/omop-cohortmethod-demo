# ============================================================================
# analysis/05_results.R
#
# Assemble the final results table and the summary figure.
#
#   Rscript analysis/05_results.R
#
# Requires 03_run_cohort_method.R (and, for context, 04_diagnostics.R).
#
# Outputs:
#   output/tables/results_main.csv        the headline table
#   output/tables/results_full.csv        every column, for the record
#   output/figures/estimates_forest.png   HR across time-at-risk windows
# ============================================================================

source("R/init.R")

log_header("05 -- RESULTS")

results   <- readRDS(out_path("cohort_method", "analysis_objects.rds"))
estimates <- utils::read.csv(out_path("cohort_method", "estimates.csv"),
                             stringsAsFactors = FALSE)

primary_label <- cfg$analysis$time_at_risk$primary$label
primary <- estimates[estimates$time_at_risk == primary_label, ]

# ----------------------------------------------------------------------------
# 1. The headline table
# ----------------------------------------------------------------------------
fmt_ci <- function(hr, lo, hi) {
  ifelse(is.na(hr), "not estimable",
         sprintf("%.2f (%.2f-%.2f)", hr, lo, hi))
}

main <- data.frame(
  `Time at risk`        = estimates$time_at_risk,
  `Analysis`            = ifelse(estimates$is_primary, "Primary", "Sensitivity"),
  `Window`              = estimates$window,
  `N celecoxib`         = estimates$n_target,
  `N diclofenac`        = estimates$n_comparator,
  `Events celecoxib`    = estimates$events_target,
  `Events diclofenac`   = estimates$events_comparator,
  `PY celecoxib`        = estimates$person_years_target,
  `PY diclofenac`       = estimates$person_years_comparator,
  `Rate celecoxib /1000PY`  = estimates$rate_target_per_1000py,
  `Rate diclofenac /1000PY` = estimates$rate_comparator_per_1000py,
  `Hazard ratio (95% CI)`   = fmt_ci(estimates$hazard_ratio,
                                     estimates$ci_lower, estimates$ci_upper),
  `p`                   = ifelse(is.na(estimates$p_value), "-",
                                 format(estimates$p_value, digits = 2)),
  `Status`              = estimates$status,
  check.names = FALSE, stringsAsFactors = FALSE
)

save_table(main, "tables", "results_main.csv")
save_table(estimates, "tables", "results_full.csv")

log_step("Main results table")
print_df(main[, c("Time at risk", "Analysis", "Events celecoxib", "Events diclofenac",
                  "Rate celecoxib /1000PY", "Rate diclofenac /1000PY",
                  "Hazard ratio (95% CI)", "p")])

# ----------------------------------------------------------------------------
# 2. Forest plot
# ----------------------------------------------------------------------------
plot_estimates(estimates, cfg)

# ----------------------------------------------------------------------------
# 3. How to read this
# ----------------------------------------------------------------------------
log_header("HOW TO READ THESE RESULTS")

if (nrow(primary) && !is.na(primary$hazard_ratio)) {
  hr <- primary$hazard_ratio; lo <- primary$ci_lower; hi <- primary$ci_upper
  log_info("Primary estimate: HR ", sprintf("%.2f", hr),
           " (95% CI ", sprintf("%.2f", lo), " to ", sprintf("%.2f", hi), ")")
  cat("\n")

  direction <- if (hi < 1) "lower" else if (lo > 1) "higher" else "no clear difference in"
  if (lo <= 1 && hi >= 1) {
    log_note("The interval crosses 1, so this analysis does not distinguish ",
             "the two drugs on GI bleeding risk. Resist the phrase 'no ",
             "association'. The correct statement is that no association was ",
             "DETECTED, and the minimum detectable hazard ratio in ",
             "output/tables/power_mdrr.csv says how large an effect would ",
             "have had to be before this study could have seen it. Absence of ",
             "evidence is a claim about the study, not about the drugs.")
  } else {
    log_note("The interval excludes 1, indicating ", direction, " hazard on ",
             "celecoxib in this analysis. Before treating that as a finding, ",
             "check the diagnostics from 04: an estimate from a study with ",
             "poor overlap or residual imbalance is precise but not correct.")
  }
}

# --- the sensitivity comparison ---------------------------------------------
est_ok <- estimates[!is.na(estimates$hazard_ratio), ]
if (nrow(est_ok) > 1) {
  spread <- max(est_ok$hazard_ratio) - min(est_ok$hazard_ratio)
  cat("\n")
  if (spread < 0.01) {
    log_note("Every estimable time-at-risk window returns an IDENTICAL hazard ",
             "ratio, while the incidence rates differ by orders of magnitude ",
             "(", min(est_ok$rate_target_per_1000py), " to ",
             max(est_ok$rate_target_per_1000py), " per 1,000 person-years). ",
             "That is not a bug, and being able to explain why is worth more ",
             "than the estimate itself.")
    log_note("The Cox partial likelihood depends only on the composition of ",
             "the risk sets at the moments events occur. In this dataset every ",
             "bleed happens within 89 days of index, so extending follow-up ",
             "from 90 days to 365 days to all-available time adds person-time ",
             "during which nothing happens. That extra time changes no risk ",
             "set at any event time, so the partial likelihood -- and ",
             "therefore the hazard ratio -- is unchanged.")
    log_note("Incidence RATES behave completely differently, because ",
             "person-time is their denominator. This is the practical lesson: ",
             "a hazard ratio is robust to how long you follow people past the ",
             "last event, while any rate you quote alongside it is entirely an ",
             "artefact of the window you chose. Quote the window whenever you ",
             "quote a rate.")
    log_note("Do not generalise this to real data, where events accrue ",
             "throughout follow-up and the windows would genuinely disagree.")
  } else {
    log_note("The hazard ratio varies by ", sprintf("%.2f", spread),
             " across time-at-risk windows. Disagreement between windows is ",
             "informative rather than embarrassing: it usually points to ",
             "either informative censoring or a latency structure the ",
             "constant-hazard assumption does not capture. Report all of ",
             "them, and say which you pre-specified.")
  }
}

# --- the limitations that belong in any write-up ----------------------------
cat("\n")
log_header("LIMITATIONS TO STATE BEFORE ANYONE ASKS")

lims <- c(
  "SYNTHETIC DATA. Eunomia is simulated. This estimate is not evidence about celecoxib or diclofenac, and the association it contains is an artefact of the simulation. What is demonstrated is the design and the diagnostics.",
  "UNMEASURED CONFOUNDING. Smoking, alcohol, over-the-counter NSAID use, Helicobacter pylori status and frailty are not in this CDM and are not in the propensity model. Balance on measured covariates says nothing about them.",
  "NO NEGATIVE CONTROLS. Without a set of outcomes known to have a true hazard ratio of 1, there is no empirical estimate of residual systematic error, and no calibrated confidence interval. This is the single biggest methodological gap in the study as configured -- see negative_controls in analysis_settings.yml.",
  "TIME AT RISK WAS DATA-DRIVEN, NOT DESIGN-DRIVEN. On-treatment follow-up was the right design for this mechanism and was not estimable because exposure records have zero duration. The fixed window dilutes any acute effect.",
  "NO COMPETING RISK HANDLING. The DEATH table is empty, so death is treated as ordinary censoring. Where mortality is substantial this overstates the cumulative incidence a clinician would observe.",
  "OUTCOME MISCLASSIFICATION. The GI bleed definition is a single concept with no descendants available in this vocabulary and no validation against chart review. Positive predictive value is unknown.",
  "GENERALISABILITY. The estimate applies to the MATCHED population, not to everyone who initiated celecoxib. Compare the matched cohort with the original arms before describing who the result is about."
)
for (i in seq_along(lims)) {
  cat("\n  ", i, ". ", sep = "")
  cat(strwrap(lims[i], width = 74, prefix = "", initial = ""), sep = "\n     ")
}

cat("\n\n")
log_decision(
  "WHAT TO ADD NEXT, IF YOU WANT THIS TO STAND OUT",
  "Negative control outcomes are the highest-value addition. Run this exact ",
  "pipeline for 50-100 outcomes that neither drug can plausibly cause, and ",
  "the distribution of those estimates is a direct measurement of your ",
  "study's residual systematic error. You can then calibrate the real ",
  "estimate against that empirical null with EmpiricalCalibration, which is ",
  "already installed. This converts 'I hope I adjusted for enough' into a ",
  "number, and it is the methodological move that most distinguishes OHDSI ",
  "work from conventional pharmacoepidemiology. Eunomia's vocabulary is too ",
  "small to supply a good control set -- which is itself a good answer to ",
  "give when asked why it is not here."
)

log_header("05 COMPLETE -- see output/tables/ and output/figures/")
