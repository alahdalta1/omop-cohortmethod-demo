# ============================================================================
# R/diagnostics.R -- study diagnostics and plots.
#
# In a comparative cohort study the diagnostics are not an appendix. They are
# the evidence that the estimate means anything, and they should be inspected
# BEFORE the hazard ratio is looked at. The OHDSI convention of blinding
# yourself to the effect estimate until the diagnostics pass is a good habit
# precisely because it removes the temptation to accept a study whose
# diagnostics are marginal but whose result is interesting.
#
# The four questions these diagnostics answer:
#
#   1. Was there anyone to compare?          PS overlap, preference score,
#                                            equipoise, AUC
#   2. Did adjustment make the groups alike? standardised mean differences
#   3. Is the follow-up comparable?          time-at-risk distribution by arm
#   4. Is there enough signal to say         event counts, minimum detectable
#      anything at all?                      relative risk
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Propensity score overlap and equipoise
# ----------------------------------------------------------------------------

#' Preference score distribution before adjustment.
#'
#' The preference score is the propensity score rescaled to remove the effect
#' of the overall prevalence of treatment. That matters: with 1,800 celecoxib
#' users and 830 diclofenac users, a raw PS of 0.68 does not mean the clinician
#' leaned toward celecoxib -- it partly reflects that most people in the data
#' got celecoxib. The preference score adjusts for that, so 0.5 means genuine
#' clinical indifference regardless of the arm sizes. It is the right scale on
#' which to judge overlap.
plot_ps_overlap <- function(result, cfg, file = "ps_preference_before.png") {
  if (is.null(result$ps)) return(invisible(NULL))
  p <- CohortMethod::plotPs(
    data = result$ps,
    scale = "preference",
    type = "density",
    targetLabel = get_cohort(cfg, "target")$label,
    comparatorLabel = get_cohort(cfg, "comparator")$label,
    showAucLabel = TRUE,
    showEquipoiseLabel = TRUE,
    showCountsLabel = TRUE,
    title = "Preference score distribution before matching"
  )
  save_plot(p, file, width = 8, height = 5)
  invisible(p)
}

#' Preference score distribution after adjustment, against the unadjusted set.
plot_ps_after <- function(result, cfg, file = "ps_preference_after.png") {
  if (is.null(result$adjusted) || is.null(result$ps)) return(invisible(NULL))
  p <- CohortMethod::plotPs(
    data = result$adjusted,
    unfilteredData = result$ps,
    scale = "preference",
    type = "density",
    targetLabel = get_cohort(cfg, "target")$label,
    comparatorLabel = get_cohort(cfg, "comparator")$label,
    showCountsLabel = TRUE,
    title = "Preference score after matching (shaded = before)"
  )
  save_plot(p, file, width = 8, height = 5)
  invisible(p)
}

#' Numeric overlap diagnostics with interpretation.
summarise_ps_diagnostics <- function(result, cfg) {
  log_step("Propensity score diagnostics")
  d <- cfg$analysis$diagnostics
  if (is.null(result$ps)) { log_info("No PS model for this window."); return(NULL) }

  ps <- result$ps
  eq_default <- CohortMethod::computeEquipoise(ps, equipoiseBounds = c(0.3, 0.7))
  auc <- result$auc

  # Where does each arm sit on the preference scale?
  q <- stats::quantile(ps$preferenceScore[ps$treatment == 1], c(0.05, 0.5, 0.95))
  qc <- stats::quantile(ps$preferenceScore[ps$treatment == 0], c(0.05, 0.5, 0.95))

  out <- data.frame(
    metric = c("ps_auc", "equipoise_0.3_0.7",
               "target_pref_p05", "target_pref_median", "target_pref_p95",
               "comparator_pref_p05", "comparator_pref_median", "comparator_pref_p95"),
    value  = round(c(auc, eq_default, q, qc), 4),
    threshold = c(d$max_ps_auc, d$min_equipoise, rep(NA, 6)),
    stringsAsFactors = FALSE
  )
  out$passes <- c(auc <= d$max_ps_auc, eq_default >= d$min_equipoise, rep(NA, 6))
  print_df(out)

  cat("\n")
  log_note("Read the preference score plot for OVERLAP, not for separation. ",
           "Two curves sitting on top of each other means the treatment ",
           "decision was near-arbitrary given what was recorded, which is the ",
           "closest an observational study gets to randomisation. Two curves ",
           "at opposite ends means the drugs went to different people for ",
           "reasons the data can see -- and probably for reasons it cannot.")
  log_note("The equipoise figure is the share of subjects with a preference ",
           "score between 0.3 and 0.7. It is the empirical analogue of trial ",
           "eligibility: the subpopulation in which the comparison is ",
           "answerable. Outside it, an estimate is extrapolation.")
  out
}

# ----------------------------------------------------------------------------
# 2. Covariate balance
# ----------------------------------------------------------------------------

#' Summarise standardised mean differences before and after adjustment.
summarise_balance <- function(result, cfg) {
  log_step("Covariate balance (standardised mean differences)")
  b <- result$balance
  if (is.null(b) || !nrow(b)) { log_info("No balance object."); return(NULL) }

  thr <- cfg$analysis$diagnostics$max_standardized_difference
  before <- abs(b$beforeMatchingStdDiff); after <- abs(b$afterMatchingStdDiff)

  # A covariate whose SMD is undefined after matching -- because it is zero
  # for everyone in the matched set, giving zero pooled variance -- cannot be
  # judged balanced or unbalanced. Count those separately rather than folding
  # them into the denominator, which is also why CohortMethod's own plot
  # reports a smaller covariate count than the raw balance table.
  n_eval <- sum(!is.na(after))

  s <- data.frame(
    metric = c("n_covariates_total", "n_covariates_with_defined_smd",
               "max_abs_smd_before", "max_abs_smd_after",
               "mean_abs_smd_before", "mean_abs_smd_after",
               "n_above_threshold_before", "n_above_threshold_after",
               "pct_above_threshold_after"),
    value = c(nrow(b), n_eval,
              round(max(before, na.rm = TRUE), 4), round(max(after, na.rm = TRUE), 4),
              round(mean(before, na.rm = TRUE), 4), round(mean(after, na.rm = TRUE), 4),
              sum(before > thr, na.rm = TRUE), sum(after > thr, na.rm = TRUE),
              round(100 * sum(after > thr, na.rm = TRUE) / n_eval, 2)),
    stringsAsFactors = FALSE
  )
  print_df(s)
  if (n_eval < nrow(b)) {
    log_info(nrow(b) - n_eval, " covariate(s) have no defined SMD after ",
             "matching (zero variance in the matched set) and are excluded ",
             "from the percentages above.")
  }

  # The covariates that remain imbalanced are the ones worth naming.
  worst <- b[order(-after), ]
  worst <- worst[seq_len(min(10, nrow(worst))), ]
  worst_df <- data.frame(
    covariate = substr(worst$covariateName, 1, 78),
    smd_before = round(worst$beforeMatchingStdDiff, 3),
    smd_after  = round(worst$afterMatchingStdDiff, 3),
    stringsAsFactors = FALSE
  )
  cat("\n      Covariates with the largest residual imbalance:\n")
  print_df(worst_df)

  cat("\n")
  n_after <- sum(after > thr, na.rm = TRUE)
  if (n_after == 0) {
    log_note("Every covariate is balanced below the ", thr, " threshold.")
  } else {
    log_note(n_after, " of ", nrow(b), " covariates remain above the ", thr,
             " threshold after adjustment. Do not report that as a failure or ",
             "a success without looking at the list above. With this many ",
             "covariates a handful will exceed any fixed threshold by chance ",
             "alone. What matters is whether the ones that remain imbalanced ",
             "are plausibly PROGNOSTIC for the outcome -- an imbalance in a ",
             "covariate unrelated to GI bleeding costs you nothing, while an ",
             "imbalance in prior anticoagulant use would matter a great deal.")
  }
  log_note("And the standing caveat: SMD measures balance on covariates you ",
           "MEASURED. Perfect balance on all of them is entirely compatible ",
           "with severe confounding by something the database never recorded ",
           "-- smoking, alcohol, over-the-counter NSAID use, frailty. The ",
           "propensity score cannot fix what was never observed, and no ",
           "diagnostic in this file can detect it.")

  list(summary = s, worst = worst_df,
       full = data.frame(
         covariate_id = b$covariateId,
         covariate_name = b$covariateName,
         mean_target_before = round(b$beforeMatchingMeanTarget, 4),
         mean_comparator_before = round(b$beforeMatchingMeanComparator, 4),
         smd_before = round(b$beforeMatchingStdDiff, 4),
         mean_target_after = round(b$afterMatchingMeanTarget, 4),
         mean_comparator_after = round(b$afterMatchingMeanComparator, 4),
         smd_after = round(b$afterMatchingStdDiff, 4),
         stringsAsFactors = FALSE))
}

#' Before/after balance scatter plot -- the single most informative diagnostic.
plot_balance <- function(result, cfg, file = "covariate_balance_scatter.png") {
  b <- result$balance
  if (is.null(b) || !nrow(b)) return(invisible(NULL))
  p <- CohortMethod::plotCovariateBalanceScatterPlot(
    balance = b, absolute = TRUE,
    threshold = cfg$analysis$diagnostics$max_standardized_difference,
    beforeLabel = "Before matching", afterLabel = "After matching",
    showCovariateCountLabel = TRUE, showMaxLabel = TRUE,
    title = "Covariate balance before and after propensity score matching"
  )
  save_plot(p, file, width = 7, height = 6.5)
  invisible(p)
}

#' The covariates with the largest imbalance, named.
plot_top_covariates <- function(result, file = "covariate_balance_top.png") {
  b <- result$balance
  if (is.null(b) || !nrow(b)) return(invisible(NULL))
  p <- try(CohortMethod::plotCovariateBalanceOfTopVariables(
    balance = b, n = 20, maxNameWidth = 80,
    beforeLabel = "Before matching", afterLabel = "After matching",
    title = "Largest covariate imbalances"), silent = TRUE)
  if (inherits(p, "try-error")) return(invisible(NULL))
  save_plot(p, file, width = 10, height = 8)
  invisible(p)
}

#' Table 1 -- baseline characteristics before and after adjustment.
build_table1 <- function(result, cfg) {
  b <- result$balance
  if (is.null(b) || !nrow(b)) return(NULL)
  pop <- result$adjusted; ps <- result$ps
  t1 <- try(CohortMethod::createCmTable1(
    balance = b,
    specifications = CohortMethod::getDefaultCmTable1Specifications(),
    beforeTargetPopSize     = sum(ps$treatment == 1),
    beforeComparatorPopSize = sum(ps$treatment == 0),
    afterTargetPopSize      = sum(pop$treatment == 1),
    afterComparatorPopSize  = sum(pop$treatment == 0),
    targetLabel = "Celecoxib", comparatorLabel = "Diclofenac",
    beforeLabel = "Before matching", afterLabel = "After matching"
  ), silent = TRUE)
  if (inherits(t1, "try-error")) return(NULL)
  as.data.frame(t1)
}

# ----------------------------------------------------------------------------
# 3. Follow-up
# ----------------------------------------------------------------------------

#' Distribution of time at risk by arm.
#'
#' Directly relevant to whether censoring is informative: if one arm is
#' systematically followed for less time, ask why before believing the hazard
#' ratio.
summarise_followup <- function(result, cfg) {
  log_step("Follow-up (time at risk) by arm")
  pop <- result$adjusted
  if (is.null(pop)) { log_info("No adjusted population."); return(NULL) }

  fu <- CohortMethod::getFollowUpDistribution(pop)
  fu_df <- as.data.frame(fu)

  q <- c(0, 0.25, 0.5, 0.75, 1)
  tgt <- stats::quantile(pop$timeAtRisk[pop$treatment == 1], q)
  cmp <- stats::quantile(pop$timeAtRisk[pop$treatment == 0], q)
  out <- data.frame(
    quantile = c("min", "p25", "median", "p75", "max"),
    target_days = as.numeric(tgt), comparator_days = as.numeric(cmp),
    stringsAsFactors = FALSE
  )
  print_df(out)

  cat("\n")
  med_diff <- abs(out$target_days[3] - out$comparator_days[3])
  log_note("Compare the two columns. Similar distributions mean both arms ",
           "were observed for a comparable length of time, so the comparison ",
           "is not being driven by differential follow-up. Median difference ",
           "here: ", round(med_diff), " days.")
  log_note("This diagnostic earns its place in an on-treatment analysis, ",
           "where follow-up ends at discontinuation and people discontinue for ",
           "reasons connected to the outcome. Under the fixed-window design ",
           "used here, follow-up ends at a fixed horizon or at the end of the ",
           "observation period, so the two arms should look alike -- and if ",
           "they did not, that would point to differential data capture rather ",
           "than to differential treatment persistence.")
  out
}

plot_followup <- function(result, cfg, file = "followup_distribution.png") {
  pop <- result$adjusted
  if (is.null(pop)) return(invisible(NULL))
  p <- try(CohortMethod::plotFollowUpDistribution(
    population = pop,
    targetLabel = "Celecoxib", comparatorLabel = "Diclofenac",
    title = "Distribution of time at risk"), silent = TRUE)
  if (inherits(p, "try-error")) return(invisible(NULL))
  save_plot(p, file, width = 8, height = 5)
  invisible(p)
}

#' Kaplan-Meier curves for the matched population.
#'
#' NOTE ON A CONFUSING MESSAGE. CohortMethod prints "No strata or 1-on-1
#' matching detected. Using unadjusted KM." Read the "or" carefully: it fires
#' when there are no strata OR when matching is exactly 1-on-1. With 1:1
#' matching every subject carries weight 1, so an unweighted Kaplan-Meier
#' curve on the matched set IS the adjusted curve -- there is nothing left to
#' weight. The message is not warning you that adjustment failed. It would
#' matter under variable-ratio matching or stratification, where strata have
#' unequal sizes and the curves must be weighted accordingly.
plot_km <- function(result, cfg, file = "kaplan_meier.png") {
  pop <- result$adjusted
  if (is.null(pop) || sum(pop$outcomeCount > 0) == 0) return(invisible(NULL))
  p <- try(CohortMethod::plotKaplanMeier(
    population = pop, censorMarks = FALSE, confidenceIntervals = TRUE,
    includeZero = FALSE,
    targetLabel = "Celecoxib", comparatorLabel = "Diclofenac",
    title = "Kaplan-Meier: gastrointestinal bleeding, matched population"),
    silent = TRUE)
  if (inherits(p, "try-error")) return(invisible(NULL))
  save_plot(p, file, width = 8, height = 6.5)
  invisible(p)
}

# ----------------------------------------------------------------------------
# 4. Statistical power
# ----------------------------------------------------------------------------

#' Minimum detectable relative risk, given the observed follow-up and events.
#'
#' Reported so that a null result can be read correctly. "No evidence of an
#' effect" and "evidence of no effect" are different claims, and the MDRR is
#' what separates them.
summarise_power <- function(result, cfg) {
  log_step("Statistical power")
  pop <- result$adjusted
  if (is.null(pop)) return(NULL)
  mdrr <- try(CohortMethod::computeMdrr(
    population = pop, alpha = 0.05, power = 0.8, twoSided = TRUE,
    modelType = cfg$analysis$outcome_model$model_type), silent = TRUE)
  if (inherits(mdrr, "try-error")) return(NULL)
  m <- as.data.frame(mdrr)
  print_df(m)
  cat("\n")
  if ("mdrr" %in% names(m)) {
    log_note("With this many events the study has 80% power to detect a ",
             "hazard ratio of ", round(m$mdrr[1], 2), " or more extreme. ",
             "A confidence interval that includes 1 therefore does not ",
             "establish equivalence -- it establishes that any effect smaller ",
             "than this was never detectable. Say so explicitly rather than ",
             "writing 'no association was found'.")
  }
  m
}

# ----------------------------------------------------------------------------
# 5. Results across time-at-risk windows
# ----------------------------------------------------------------------------

#' Forest plot of the hazard ratio under each time-at-risk window.
plot_estimates <- function(estimates, cfg, file = "estimates_forest.png") {
  e <- estimates[!is.na(estimates$hazard_ratio), ]
  if (!nrow(e)) return(invisible(NULL))
  e$label <- factor(e$time_at_risk, levels = rev(e$time_at_risk))
  e$kind <- ifelse(e$is_primary, "Primary", "Sensitivity")

  p <- ggplot2::ggplot(e, ggplot2::aes(x = hazard_ratio, y = label,
                                       colour = kind)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    # geom_errorbarh() was deprecated in ggplot2 4.0; the horizontal form is
    # now geom_errorbar() with orientation = "y".
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                           orientation = "y", width = 0.18, linewidth = 0.7) +
    ggplot2::geom_point(size = 3.2) +
    # Limits are derived from the data with a margin, so the intervals never
    # run into the panel edge. Breaks are restricted to those inside the range,
    # otherwise a fixed break vector either crowds the axis or leaves it bare.
    ggplot2::scale_x_continuous(
      transform = "log",
      breaks = local({
        cand <- c(0.1, 0.25, 0.5, 0.67, 0.8, 0.9, 1, 1.1, 1.25, 1.5, 2, 4, 10)
        cand[cand >= min(e$ci_lower, na.rm = TRUE) * 0.95 &
             cand <= max(e$ci_upper, na.rm = TRUE) * 1.05]
      }),
      limits = c(min(e$ci_lower, na.rm = TRUE) * 0.92,
                 max(e$ci_upper, na.rm = TRUE) * 1.08),
      labels = function(x) format(x, drop0trailing = TRUE)
    ) +
    ggplot2::scale_colour_manual(values = c(Primary = "#B2182B", Sensitivity = "#2166AC")) +
    ggplot2::labs(
      x = "Hazard ratio (log scale)", y = NULL, colour = NULL,
      title = "Gastrointestinal bleeding: celecoxib vs diclofenac",
      subtitle = "Propensity score matched, stratified Cox model",
      caption = "HR < 1 favours celecoxib. Synthetic data -- not evidence about these drugs."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top",
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(hjust = 0, colour = "grey35"))
  save_plot(p, file, width = 9, height = 4.2)
  invisible(p)
}

#' Attrition through the CohortMethod stages.
build_attrition <- function(result) {
  if (is.null(result$model) || is.null(result$model$attrition)) return(NULL)
  a <- as.data.frame(result$model$attrition)
  a
}
