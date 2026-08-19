# ============================================================================
# R/database.R -- getting a connection to the CDM.
#
# Everything downstream talks to the CDM through DatabaseConnector, which is
# the point: the same analysis code runs unchanged against Eunomia here, or
# against Postgres/Redshift/Databricks/SQL Server holding a real CDM, because
# SqlRender translates the SQL and DatabaseConnector abstracts the driver.
#
# That portability is the main practical argument for the OHDSI stack, and it
# only holds if you never write dialect-specific SQL by hand. All SQL in this
# project is written in OHDSI SQL and translated -- see R/cohorts.R.
# ============================================================================

#' Ensure the Eunomia CDM exists locally, and return its file path.
#'
#' Eunomia downloads a zipped set of CSVs on first use and loads them into a
#' local database file. Both steps are cached: the download lands in
#' EUNOMIA_DATA_FOLDER (set by .Rprofile), and the database file is only
#' rebuilt if it is missing or `refresh = TRUE`.
#'
#' @param cfg full config list
#' @param refresh rebuild the database file even if it already exists
ensure_eunomia_database <- function(cfg, refresh = FALSE) {
  db <- cfg$analysis$database
  data_dir <- proj_path("data", create = TRUE)
  db_file <- file.path(data_dir, paste0("eunomia_", db$dataset_name, ".", db$dbms))

  if (file.exists(db_file) && !refresh) {
    log_info("Using cached CDM: ", basename(db_file),
             " (", round(file.size(db_file) / 1024^2, 1), " MB)")
    return(db_file)
  }

  log_info("Building Eunomia CDM '", db$dataset_name, "' (CDM v", db$cdm_version,
           ") as ", db$dbms, " -- first run downloads ~35 MB")
  db_file <- Eunomia::getDatabaseFile(
    datasetName  = db$dataset_name,
    cdmVersion   = db$cdm_version,
    pathToData   = Sys.getenv("EUNOMIA_DATA_FOLDER"),
    dbms         = db$dbms,
    databaseFile = db_file,
    overwrite    = TRUE,
    verbose      = FALSE
  )
  log_info("Built: ", basename(db_file))
  db_file
}

#' Connection details for the Eunomia CDM.
get_connection_details <- function(cfg, refresh = FALSE) {
  db_file <- ensure_eunomia_database(cfg, refresh = refresh)
  DatabaseConnector::createConnectionDetails(
    dbms   = cfg$analysis$database$dbms,
    server = db_file
  )
}

#' Connect, suppressing the driver's first-run chatter.
#'
#' DuckDB prints an extension/secret-storage notice on every connection that
#' has nothing to do with the analysis. Suppressed so the console log stays
#' readable, which matters because the log is part of what this project
#' communicates.
db_connect <- function(connection_details) {
  suppressWarnings(suppressMessages(
    DatabaseConnector::connect(connection_details)
  ))
}

db_disconnect <- function(connection) {
  if (!is.null(connection)) {
    suppressWarnings(suppressMessages(DatabaseConnector::disconnect(connection)))
  }
  invisible(NULL)
}

#' Run a SELECT and return a data frame, with parameter substitution and
#' dialect translation.
#'
#' Always goes through SqlRender rather than pasting values into strings:
#' `@parameter` substitution is what makes the same SQL reusable, and
#' `translate()` is what makes it portable off SQLite/DuckDB.
#'
#' @param connection an open DatabaseConnector connection
#' @param sql OHDSI SQL, using @parameters
#' @param ... named values substituted into the SQL
#' Column-name casing varies between DatabaseConnector versions and backends
#' (some return upper case, some lower). Rather than guess, normalise to UPPER
#' here so downstream code can rely on it. Callers that prefer lower case do
#' `names(x) <- tolower(names(x))` explicitly.
#'
#' NOTE ON THE ARGUMENT NAMES. The formals are `.connection` and `.sql`, with
#' leading dots, because everything after them is a SQL @parameter supplied by
#' name. R performs PARTIAL matching on named arguments, so a parameter called
#' `c` would silently match a formal called `connection`, and a parameter
#' called `s` would match `sql` -- shifting every remaining argument by one and
#' producing an error a long way from its cause. OHDSI SQL parameters are
#' always bare words, never dotted, so a leading dot makes the collision
#' impossible. This bit us once already; the dots are not cosmetic.
query <- function(.connection, .sql, ...) {
  rendered <- SqlRender::render(.sql, ...)
  translated <- SqlRender::translate(rendered, targetDialect = .connection@dbms)
  res <- suppressMessages(
    DatabaseConnector::querySql(.connection, translated, snakeCaseToCamelCase = FALSE)
  )
  names(res) <- toupper(names(res))
  res
}

#' Execute DDL/DML (no result set). Dotted formals for the same reason as
#' query() above.
execute <- function(.connection, .sql, ...) {
  rendered <- SqlRender::render(.sql, ...)
  translated <- SqlRender::translate(rendered, targetDialect = .connection@dbms)
  suppressMessages(
    DatabaseConnector::executeSql(.connection, translated,
                                  progressBar = FALSE,
                                  reportOverallTime = FALSE)
  )
  invisible(NULL)
}

#' Row count for a table, or NA if it does not exist.
table_row_count <- function(connection, schema, table) {
  res <- try(
    query(connection, "SELECT COUNT(*) AS n FROM @schema.@table",
          schema = schema, table = table),
    silent = TRUE
  )
  if (inherits(res, "try-error")) return(NA_integer_)
  as.integer(res[[1]][1])
}

#' Which of the CDM tables actually exist in this database.
list_cdm_tables <- function(connection, schema) {
  sort(tolower(DatabaseConnector::getTableNames(connection, schema)))
}
