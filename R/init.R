# ============================================================================
# R/init.R -- common preamble sourced by every script in analysis/.
#
#   source("R/init.R")
#
# Loads packages, sources the function library, and returns the validated
# config. Keeping this in one place means the analysis scripts start with the
# analysis rather than with twenty lines of setup.
# ============================================================================

suppressPackageStartupMessages({
  library(DatabaseConnector)
  library(SqlRender)
  library(Eunomia)
  library(CohortGenerator)
  library(FeatureExtraction)
  library(CohortMethod)
  library(Cyclops)
  library(ggplot2)
  library(yaml)
})

# ParallelLogger is chatty by default; keep the console for our own narrative.
ParallelLogger::clearLoggers()
ParallelLogger::registerLogger(
  ParallelLogger::createLogger(
    name = "SIMPLE", threshold = "WARN",
    appenders = list(ParallelLogger::createConsoleAppender(
      layout = ParallelLogger::layoutSimple))
  )
)

# Source the function library in dependency order.
for (f in c("utils.R", "config.R", "database.R", "explore_cdm.R",
            "concept_sets.R", "cohorts.R", "cohort_method.R", "diagnostics.R")) {
  p <- file.path("R", f)
  if (file.exists(p)) source(p)
}

# Fail fast and loudly if the working directory is wrong -- every path in this
# project is relative to the project root.
if (!dir.exists("config") || !dir.exists("R")) {
  stop("Run scripts with the project root as the working directory.\n",
       "  Currently: ", getwd(), call. = FALSE)
}

cfg <- load_config()
