# ============================================================================
# analysis/02_build_cohorts.R
#
# Resolve the concept sets, compile the YAML cohort definitions to SQL, and
# materialise the three cohorts (target, comparator, outcome) into a cohort
# table.
#
#   Rscript analysis/02_build_cohorts.R
#
# Outputs:
#   output/cohorts/cohort_*.sql        the generated SQL, for audit
#   output/cohorts/attrition.csv       subjects remaining after each criterion
#   output/cohorts/cohort_counts.csv   final cohort sizes
#   output/explore/concept_set_resolution.csv
# ============================================================================

source("R/init.R")

log_header("02 -- BUILDING COHORTS FROM THE CONFIGURATION")
print_protocol(cfg)

schema     <- cfg$analysis$database$cdm_database_schema
connection <- db_connect(get_connection_details(cfg))
on.exit(db_disconnect(connection), add = TRUE)

# ----------------------------------------------------------------------------
# 1. Concept sets
#
# Resolve the clinical ideas in config/cohorts.yml into concrete concept IDs
# before any cohort logic runs, so that the same resolved sets are used by the
# entry criteria, the exclusion criteria and (later) the covariate exclusions.
# ----------------------------------------------------------------------------
concept_sets <- resolve_all_concept_sets(connection, schema, cfg)
save_table(concept_sets$audit, "explore", "concept_set_resolution.csv")

# ----------------------------------------------------------------------------
# 2. Generate the cohorts
#
# Each criterion in the YAML becomes one CTE, and subjects are counted after
# each, which is what produces the attrition table below.
# ----------------------------------------------------------------------------
result <- generate_cohorts(connection, cfg, concept_sets$sets)

save_table(result$attrition, "cohorts", "attrition.csv")
save_table(result$counts,    "cohorts", "cohort_counts.csv")

# ----------------------------------------------------------------------------
# 3. Validate
# ----------------------------------------------------------------------------
validate_cohorts(connection, cfg)

# ----------------------------------------------------------------------------
# 4. What the attrition means for the study
# ----------------------------------------------------------------------------
log_header("READING THE ATTRITION TABLE")

att <- result$attrition
tgt <- get_cohort(cfg, "target"); cmp <- get_cohort(cfg, "comparator")

for (cid in c(tgt$id, cmp$id)) {
  a <- att[att$cohort_id == cid, ]
  first <- a$n_subjects[1]; final <- a$n_subjects[nrow(a)]
  cat("\n")
  log_info(a$cohort_name[1], ": ", format(first, big.mark = ","),
           " candidates -> ", format(final, big.mark = ","), " in cohort (",
           round(100 * final / first), "% retained)")
  worst <- a[which.max(ifelse(is.na(a$n_lost), -1, a$n_lost)), ]
  if (!is.na(worst$n_lost) && worst$n_lost > 0) {
    log_info("  largest single loss: ", format(worst$n_lost, big.mark = ","),
             " at '", worst$criterion, "'")
  } else {
    log_info("  no criterion removed anyone")
  }
}

cat("\n")
log_note("If no criterion removes anyone, the eligibility rules are ",
         "documenting intent rather than shaping the population. That is an ",
         "honest outcome on a small synthetic dataset, but do not present the ",
         "cohort as though the criteria were doing work. On real data the ",
         "prior-observation requirement alone typically removes a large ",
         "fraction, and that fraction is not a random sample -- people with ",
         "less enrolment history are younger, more mobile and differently ",
         "insured, which is a generalisability question you should raise ",
         "before a reviewer does.")

log_decision(
  "COHORT ENTRY -- confirm before running the analysis",
  "The population is now fixed. Check output/cohorts/attrition.csv and ",
  "satisfy yourself about three things. (1) New-user status: entry is the ",
  "FIRST ever qualifying exposure, so anyone already on the drug is out -- ",
  "confirm that is the question you mean to ask. (2) The 365-day prior ",
  "observation requirement: it defines what 'new' means and gives the ",
  "covariate windows something to look at. (3) The active comparator: ",
  "diclofenac initiators, not non-users, which is what keeps confounding by ",
  "indication tractable. Each is defensible; none is automatic."
)

log_header("02 COMPLETE -- see output/cohorts/")
