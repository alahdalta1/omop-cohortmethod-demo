# ============================================================================
# R/utils.R -- paths, logging and small helpers.
#
# Nothing epidemiological lives here. Kept separate so the analysis functions
# read as analysis rather than as plumbing.
# ============================================================================

#' Project root
#'
#' Every analysis script assumes it is run with the project root as the working
#' directory (that is also what .Rprofile and renv assume). This resolves paths
#' relative to it and creates directories on demand.
#'
#' @param ... path components, passed to file.path()
#' @param create create the directory if it does not exist
proj_path <- function(..., create = FALSE) {
  p <- file.path(normalizePath(".", winslash = "/", mustWork = FALSE), ...)
  if (create) dir.create(p, showWarnings = FALSE, recursive = TRUE)
  p
}

#' Path inside output/, creating the parent directory
out_path <- function(subdir, file = NULL) {
  d <- proj_path("output", subdir, create = TRUE)
  if (is.null(file)) d else file.path(d, file)
}

# --- Logging ----------------------------------------------------------------
# Deliberately plain. The analysis scripts print a running narrative because
# the whole point of the project is that a reader can follow the reasoning, so
# the console output is part of the deliverable, not debug noise.

log_header <- function(...) {
  msg <- paste0(...)
  cat("\n", strrep("=", 78), "\n", msg, "\n", strrep("=", 78), "\n", sep = "")
}

log_step <- function(...) cat("\n--- ", paste0(...), "\n", sep = "")
log_info <- function(...) cat("    ", paste0(...), "\n", sep = "")

#' Print an interpretation note.
#'
#' Used to attach the "so what" to a number the moment it is printed, rather
#' than leaving it to a reader to work out later.
log_note <- function(...) {
  txt <- paste0(...)
  cat(strwrap(txt, width = 78, prefix = "    | "), sep = "\n")
}

#' Flag a decision point that the analyst must own.
#'
#' These are the lines to grep for when preparing to defend the study:
#'   grep -rn "DECISION REQUIRED" output/
log_decision <- function(topic, ...) {
  cat("\n", strrep("-", 78), "\n", sep = "")
  cat("  DECISION REQUIRED -- ", topic, "\n", sep = "")
  cat(strwrap(paste0(...), width = 78, prefix = "  "), sep = "\n")
  cat(strrep("-", 78), "\n", sep = "")
}

#' Write a data frame to output/ as CSV and echo where it went.
save_table <- function(x, subdir, file, quiet = FALSE) {
  path <- out_path(subdir, file)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  if (!quiet) log_info("wrote ", nrow(x), " rows -> ", sub(proj_path(), ".", path, fixed = TRUE))
  invisible(path)
}

#' Save a ggplot with consistent dimensions.
save_plot <- function(plot, file, width = 8, height = 5.5, dpi = 200) {
  path <- out_path("figures", file)
  suppressWarnings(
    ggplot2::ggsave(path, plot = plot, width = width, height = height,
                    dpi = dpi, bg = "white")
  )
  log_info("wrote figure -> ", sub(proj_path(), ".", path, fixed = TRUE))
  invisible(path)
}

#' Stop with a clear message when a config assumption is violated.
assert <- function(condition, ...) {
  if (!isTRUE(condition)) stop(paste0(...), call. = FALSE)
  invisible(TRUE)
}

#' Format a count with a percentage of a denominator.
pct <- function(n, d, digits = 1) {
  if (is.na(d) || d == 0) return(sprintf("%d (-)", n))
  sprintf("%d (%.*f%%)", n, digits, 100 * n / d)
}

#' Null-coalescing operator (R < 4.4 has no %||% in base for all cases).
`%||%` <- function(a, b) if (is.null(a)) b else a
