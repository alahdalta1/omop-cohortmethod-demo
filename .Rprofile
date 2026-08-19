# ============================================================================
# .Rprofile -- runs automatically at the start of every R session whose working
# directory is this project. Keep it small and side-effect-free; anything that
# does real work belongs in analysis/.
# ============================================================================

# --- 1. Pinned package repository -------------------------------------------
# See analysis/00_install_dependencies.R for why this is a dated snapshot
# rather than plain CRAN.
options(repos = c(P3M = "https://packagemanager.posit.co/cran/2026-08-01"))

# --- 2. renv ----------------------------------------------------------------
# Puts the project-private library on .libPaths(). Must come before anything
# that loads a package.
source("renv/activate.R")

# --- 3. Java ----------------------------------------------------------------
# DatabaseConnector loads rJava at namespace load time, even when the target is
# a local SQLite/DuckDB file that never touches JDBC. So a JDK has to be
# discoverable or nothing in this project will start.
#
# The JDK is managed by rJavaEnv, which keeps a per-user cache outside the
# repository. Resolve it dynamically -- never hard-code an absolute path here,
# because this file is committed and the path is machine-specific.
local({
  java_ok <- function(home) {
    nzchar(home) && dir.exists(home) &&
      file.exists(file.path(home, "bin", if (.Platform$OS.type == "windows") "java.exe" else "java"))
  }

  if (java_ok(Sys.getenv("JAVA_HOME"))) return(invisible(NULL))

  {
    # Pick the highest JDK version already present in the rJavaEnv cache.
    # tools::R_user_dir is base R, so this works before any package loads.
    root <- file.path(
      tools::R_user_dir("rJavaEnv", which = "cache"), "installed",
      if (.Platform$OS.type == "windows") "windows" else tolower(Sys.info()[["sysname"]]),
      "x64"
    )
    if (dir.exists(root)) {
      versions <- sort(suppressWarnings(as.integer(list.files(root))), decreasing = TRUE)
      versions <- versions[!is.na(versions)]
      for (v in versions) {
        home <- file.path(root, v)
        if (java_ok(home)) {
          Sys.setenv(JAVA_HOME = home)
          Sys.setenv(PATH = paste(file.path(home, "bin"), Sys.getenv("PATH"),
                                  sep = .Platform$path.sep))
          break
        }
      }
    }
  }

  if (!java_ok(Sys.getenv("JAVA_HOME"))) {
    packageStartupMessage(
      "No JDK found. DatabaseConnector will fail to load.\n",
      "  Fix with:  Rscript analysis/00_install_dependencies.R\n",
      "  or:        rJavaEnv::java_quick_install(version = 21)"
    )
  }
})

# --- 4. Eunomia data location -----------------------------------------------
# Eunomia downloads and unpacks its synthetic CDM here. Kept inside the project
# (and gitignored) so the repository is self-contained and a failed run can be
# reset by deleting one directory.
local({
  d <- file.path(normalizePath(".", winslash = "/", mustWork = FALSE), "data")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  Sys.setenv(EUNOMIA_DATA_FOLDER = d)
})

# --- 5. Session hygiene -----------------------------------------------------
options(
  stringsAsFactors        = FALSE,
  scipen                  = 999,   # no scientific notation in printed results
  dplyr.summarise.inform  = FALSE,
  # Andromeda/CohortMethod write large temporary files; keep them in the
  # project so they are cleaned up with it rather than filling the system temp.
  andromedaTempFolder     = file.path(normalizePath(".", winslash = "/", mustWork = FALSE),
                                      "data", "andromedaTemp")
)
dir.create(getOption("andromedaTempFolder"), showWarnings = FALSE, recursive = TRUE)
