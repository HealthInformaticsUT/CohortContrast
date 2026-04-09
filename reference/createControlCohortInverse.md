# Function for creating automatic matches based on inverse control logic

Function for creating automatic matches based on inverse control logic

## Usage

``` r
createControlCohortInverse(cdm, targetTable)
```

## Arguments

- cdm:

  Connection to the database (package CDMConnector)

- targetTable:

  A cohort tibble which contains subjects' cohort data

## Value

A data frame representing inverse control time windows for the target
subjects. The returned table contains cohort_definition_id, subject_id,
cohort_start_date, and cohort_end_date columns, where each row captures
an observation-period segment outside the target cohort interval.

## Examples

``` r
# \donttest{
if (requireNamespace("CDMConnector", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("duckdb", quietly = TRUE) &&
    nzchar(Sys.getenv("EUNOMIA_DATA_FOLDER")) &&
    isTRUE(tryCatch(
      CDMConnector::eunomiaIsAvailable("GiBleed"),
      error = function(...) FALSE
    ))) {
  pathToJSON <- system.file(
    "example", "example_json", "diclofenac",
    package = "CohortContrast"
  )
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = CDMConnector::eunomiaDir("GiBleed")
  )
  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmName = "eunomia",
    cdmSchema = "main",
    writeSchema = "main"
  )

  targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
  controlTable <- createControlCohortInverse(cdm = cdm, targetTable = targetTable)
  head(controlTable)

  DBI::dbDisconnect(con, shutdown = TRUE)
}
# }
```
