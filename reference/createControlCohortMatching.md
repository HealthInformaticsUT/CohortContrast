# Function for creating automatic matches based on age and sex

Function for creating automatic matches based on age and sex

## Usage

``` r
createControlCohortMatching(
  cdm,
  targetTable,
  ratio = 1,
  max = NULL,
  min = NULL
)
```

## Arguments

- cdm:

  Connection to the database (package CDMConnector)

- targetTable:

  A cohort tibble which contains subjects' cohort data

- ratio:

  ratio for the number of matches generated

- max:

  Maximum ratio to use

- min:

  Minimum ratio to use

## Value

A data frame representing the matched control cohort. The returned table
contains cohort_definition_id, subject_id, cohort_start_date, and
cohort_end_date columns, with one row per matched control interval
aligned to the target cohort follow-up logic.

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
  controlTable <- createControlCohortMatching(
    cdm = cdm,
    targetTable = targetTable,
    ratio = 1
  )
  head(controlTable)

  DBI::dbDisconnect(con, shutdown = TRUE)
}
# }
```
