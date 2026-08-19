# ============================================================================
# analysis/01_explore_cdm.R
#
# Load the Eunomia synthetic OMOP CDM and work through its structure.
#
#   Rscript analysis/01_explore_cdm.R
#
# The aim is to understand the data model rather than to query it. Everything
# printed here has a purpose downstream: the row counts bound what the study
# can detect, the mapping rates decide whether the concept sets mean anything,
# the observation-period join is the mechanism behind the new-user definition,
# and the exposure-duration table is the evidence for the time-at-risk choice.
#
# Outputs: output/explore/*.csv and a full console log worth reading top to
# bottom.
# ============================================================================

source("R/init.R")

log_header("01 -- EXPLORING THE OMOP COMMON DATA MODEL")
print_protocol(cfg)

schema <- cfg$analysis$database$cdm_database_schema
connection_details <- get_connection_details(cfg)
connection <- db_connect(connection_details)
on.exit(db_disconnect(connection), add = TRUE)

# ----------------------------------------------------------------------------
# 1. What tables are here, and how big are they?
# ----------------------------------------------------------------------------
inventory <- explore_table_inventory(connection, schema)
save_table(inventory, "explore", "01_table_inventory.csv")

# ----------------------------------------------------------------------------
# 2. The denominator: who exists and when were they observable?
# ----------------------------------------------------------------------------
denominator <- explore_person_denominator(connection, schema)
save_table(denominator, "explore", "02_person_denominator.csv")

# ----------------------------------------------------------------------------
# 3. The clinical event tables and their shared skeleton
# ----------------------------------------------------------------------------
events <- explore_event_tables(connection, schema)
save_table(events, "explore", "03_event_tables.csv")

# ----------------------------------------------------------------------------
# 4. How the same fact is stored three times: source value, source concept,
#    standard concept
# ----------------------------------------------------------------------------
mapping <- explore_concept_mapping(connection, schema)
save_table(mapping, "explore", "04_concept_mapping.csv")

# ----------------------------------------------------------------------------
# 5. Domain routing: why a concept lives in the table it lives in
# ----------------------------------------------------------------------------
routing <- explore_domain_routing(connection, schema)
save_table(routing, "explore", "05_domain_routing.csv")

# ----------------------------------------------------------------------------
# 6. The vocabulary itself, and the hierarchy that makes concept sets possible
# ----------------------------------------------------------------------------
vocab <- explore_vocabulary(connection, schema)
save_table(vocab, "explore", "06_vocabulary.csv")

# ----------------------------------------------------------------------------
# 7. Do events sit inside observation periods? (the join behind eligibility)
# ----------------------------------------------------------------------------
alignment <- explore_observation_alignment(connection, schema)
save_table(alignment, "explore", "07_observation_alignment.csv")

# ----------------------------------------------------------------------------
# 8. The study's own concepts, traced through the model
# ----------------------------------------------------------------------------
study_concepts <- explore_study_concepts(connection, schema, cfg)
save_table(study_concepts, "explore", "08_study_concepts.csv")

# ----------------------------------------------------------------------------
# 9. Resolve the concept sets declared in config/cohorts.yml
# ----------------------------------------------------------------------------
concept_sets <- resolve_all_concept_sets(connection, schema, cfg)
save_table(concept_sets$audit, "explore", "concept_set_resolution.csv")

# ----------------------------------------------------------------------------
# 10. Evidence for the time-at-risk decision
# ----------------------------------------------------------------------------
timing <- explore_exposure_and_outcome_timing(connection, schema, cfg)
save_table(timing$durations, "explore", "09_exposure_durations.csv")
save_table(timing$timing,    "explore", "10_outcome_timing.csv")

# ----------------------------------------------------------------------------
# Decisions this script hands back to the analyst
# ----------------------------------------------------------------------------
log_header("DECISIONS ARISING FROM THIS EXPLORATION")

log_decision(
  "TIME AT RISK (config/analysis_settings.yml -> time_at_risk)",
  "Exposure records here have zero duration and there are no repeat ",
  "dispensings, so cohort end == cohort start and an on-treatment window is ",
  "not estimable. The configured primary window is therefore a fixed 1-365 ",
  "days from cohort start. On clinical grounds on-treatment would be the ",
  "better primary design for an acute, reversible mechanism like NSAID ",
  "gastropathy. Confirm you are content with the fixed window, and be ready ",
  "to explain why the data forced it."
)

log_decision(
  "COHORT ENTRY (config/cohorts.yml -> entry, inclusion_criteria)",
  "Currently: first-ever exposure (new-user), 365 days of prior observation, ",
  "adults only. The prior-observation requirement is what makes 'new user' ",
  "meaningful; check the attrition table from 02_build_cohorts.R to see ",
  "what it costs. Note that in this dataset the age criterion excludes nobody ",
  "(all users are 31-47), so it is doing no work here beyond documenting ",
  "intent."
)

log_decision(
  "COVARIATE SELECTION (config/analysis_settings.yml -> covariates)",
  "Currently: large-scale, data-driven, with all baseline windows ending at ",
  "day 0 and the exposure concepts excluded. The alternative is a ",
  "hypothesis-driven list you can draw as a DAG. Given the vocabulary here is ",
  "small (roughly 80 condition and 113 drug concepts), the large-scale model ",
  "will build far fewer covariates than it would on real data -- so this ",
  "demonstrates the machinery rather than its full benefit."
)

log_header("01 COMPLETE -- see output/explore/")
