# ---------------------------------------------------------------------------
# 00_install_dependencies.R
#
# Bootstraps the project library with renv.
#
# Run ONCE, from a clean R session, with the project root as the working
# directory:
#
#   Rscript analysis/00_install_dependencies.R
#
# After this has run, every later script picks the pinned library up
# automatically via .Rprofile -> renv/activate.R. Collaborators who clone the
# repository do not run this script at all; they run renv::restore(), which
# reads renv.lock and installs exactly the versions recorded there.
#
# WHY A DATED SNAPSHOT REPOSITORY
# renv.lock records package *versions*, but CRAN only serves the *current*
# version of each package -- older versions are moved to the Archive and are
# no longer installable as binaries. Pinning the repository to a dated Posit
# Package Manager snapshot means the exact versions in renv.lock stay
# resolvable (and stay available as pre-built Windows binaries) indefinitely.
# This is the difference between "we wrote down what we used" and "someone
# else can actually reinstall it in 2029".
# ---------------------------------------------------------------------------

SNAPSHOT_DATE <- "2026-08-01"
REPO <- c(P3M = paste0("https://packagemanager.posit.co/cran/", SNAPSHOT_DATE))

options(
  repos = REPO,
  # Ask for pre-built binaries where the platform has them. Cyclops and
  # CohortMethod contain C++ and would otherwise need Rtools to compile.
  pkgType = if (.Platform$OS.type == "windows") "both" else getOption("pkgType"),
  install.packages.check.source = "no",
  timeout = 1200
)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = REPO)
}

project_root <- normalizePath(".", winslash = "/")
message("Initialising renv in: ", project_root)

# bare = TRUE: create the project library but do not try to discover and
# install dependencies by scanning the source tree. We want to state the
# dependency set explicitly, below, rather than have it inferred.
renv::init(project = project_root, bare = TRUE, restart = FALSE)

# renv writes a .Rprofile containing only `source("renv/activate.R")`.
# Prepend the repository pin so that the snapshot repo is in effect for every
# future session in this project, not just this one.
rprofile <- file.path(project_root, ".Rprofile")
existing <- if (file.exists(rprofile)) readLines(rprofile, warn = FALSE) else character()
existing <- existing[!grepl("packagemanager.posit.co", existing, fixed = TRUE)]
writeLines(
  c(
    sprintf('# Pinned CRAN snapshot -- see analysis/00_install_dependencies.R'),
    sprintf('options(repos = c(P3M = "https://packagemanager.posit.co/cran/%s"))', SNAPSHOT_DATE),
    existing
  ),
  rprofile
)

# ---------------------------------------------------------------------------
# The dependency set, grouped by what each package is actually for.
# ---------------------------------------------------------------------------
pkgs <- c(
  # --- OHDSI: data access and the CDM -------------------------------------
  "DatabaseConnector",   # uniform connection layer over the CDM
  "SqlRender",           # write SQL once, translate to the target dialect
  "Eunomia",             # synthetic OMOP CDM in a local SQLite/DuckDB file

  # --- OHDSI: cohorts, covariates, estimation -----------------------------
  "CohortGenerator",     # materialise cohort definitions into a cohort table
  "FeatureExtraction",   # build covariates from the CDM on a standard grammar
  "CohortMethod",        # new-user comparative cohort design + PS + outcome model
  "Cyclops",             # large-scale regularised regression (PS and Cox fits)
  "EmpiricalCalibration",# calibrate estimates against negative controls
  "Andromeda",           # out-of-memory data objects used by the above
  "ParallelLogger",      # structured logging used throughout OHDSI

  # --- Local database backends -------------------------------------------
  "RSQLite",
  "duckdb",

  # --- Java toolchain -----------------------------------------------------
  # DatabaseConnector loads rJava when its namespace loads, even for a local
  # SQLite/DuckDB file that never opens a JDBC connection. rJavaEnv installs a
  # self-contained JDK into a per-user cache, so no system-wide Java install
  # and no administrator rights are needed.
  "rJavaEnv",

  # --- Analysis, config and reporting -------------------------------------
  "yaml",                # cohort / analysis configuration lives in YAML
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2",
  "scales",
  "survival",
  "knitr"
)

message("Installing ", length(pkgs), " top-level packages (plus dependencies)...")
renv::install(pkgs, prompt = FALSE)

# ---------------------------------------------------------------------------
# Java runtime.
# ---------------------------------------------------------------------------
java_home <- Sys.getenv("JAVA_HOME")
java_ok <- nzchar(java_home) && dir.exists(java_home)
if (!java_ok) {
  message("No JDK found -- installing a project-local JDK 21 (~200 MB download).")
  # java_quick_install() appends a hard-coded, machine-specific JAVA_HOME block
  # to .Rprofile. That path must not be committed, and .Rprofile already
  # resolves the JDK dynamically, so restore the file afterwards.
  before <- readLines(rprofile, warn = FALSE)
  rJavaEnv::java_quick_install(version = 21, project = project_root)
  writeLines(before, rprofile)
  message("JDK installed; .Rprofile restored to its portable form.")
}

# Record everything actually installed, with versions and source repository.
# type = "all" records the full library rather than only packages renv can see
# referenced in the source tree -- OHDSI packages are often reached indirectly.
renv::settings$snapshot.type("all")
renv::snapshot(project = project_root, type = "all", prompt = FALSE)

message("\nDone. Installed versions:")
ip <- installed.packages()[, "Version", drop = TRUE]
print(ip[intersect(pkgs, names(ip))])
