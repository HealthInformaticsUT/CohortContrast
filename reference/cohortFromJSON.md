# Read cohort from JSON

Read cohort from JSON

## Usage

``` r
cohortFromJSON(pathToJSON, cdm, cohortId = NULL)
```

## Arguments

- pathToJSON:

  Path to the cohort data JSON file

- cdm:

  Connection to the database (package CDMConnector)

- cohortId:

  The id for cohort in cohorts' table, if NULL whole table will be
  imported

## Value

a tbl object for further CohortContrast usage

## Examples

``` r
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
  targetTable

  DBI::dbDisconnect(con, shutdown = TRUE)
}
```
