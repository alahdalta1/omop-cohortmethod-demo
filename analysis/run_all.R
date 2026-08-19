# ============================================================================
# analysis/run_all.R -- run the whole study, in order.
#
#   Rscript analysis/run_all.R
#
# Does NOT run 00_install_dependencies.R. That is a one-off bootstrap; from a
# fresh clone run renv::restore() instead.
#
# Each script is run in its own R session (via callr-style Rscript) so that
# nothing carries over between steps in memory. Anything a later script needs
# from an earlier one must have been written to output/ -- which is the whole
# point of a reproducible pipeline, and the fastest way to find out that it
# is not one.
# ============================================================================

scripts <- c(
  "analysis/01_explore_cdm.R",
  "analysis/02_build_cohorts.R",
  "analysis/03_run_cohort_method.R",
  "analysis/04_diagnostics.R",
  "analysis/05_results.R"
)

rscript <- file.path(R.home("bin"), "Rscript")
started <- Sys.time()

for (s in scripts) {
  cat("\n\n", strrep("#", 78), "\n# RUNNING: ", s, "\n", strrep("#", 78), "\n", sep = "")
  status <- system2(rscript, args = s)
  if (!identical(status, 0L)) {
    stop("FAILED: ", s, " (exit status ", status, ")\n",
         "Fix the error above before continuing -- later scripts depend on it.",
         call. = FALSE)
  }
}

cat("\n\n", strrep("=", 78), "\n", sep = "")
cat("PIPELINE COMPLETE in ",
    round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1),
    " minutes\n", sep = "")
cat(strrep("=", 78), "\n\n")
cat("Outputs:\n")
for (d in c("explore", "cohorts", "cohort_method", "figures", "tables")) {
  f <- list.files(file.path("output", d))
  f <- f[f != ".gitkeep"]
  cat(sprintf("  output/%-14s %2d file(s)\n", paste0(d, "/"), length(f)))
}
