# Read cohort from database cohort table

Read cohort from database cohort table

## Usage

``` r
cohortFromCohortTable(
  cdm,
  db,
  tableName = NULL,
  schemaName = NULL,
  cohortId = NULL
)
```

## Arguments

- cdm:

  CDMConnector object

- db:

  Database instance (DBI)

- tableName:

  Name of the table where the cohort is defined

- schemaName:

  Name of the schema where the cohort table is defined

- cohortId:

  The id for cohort in cohorts' table, if NULL whole table will be
  imported

## Value

a tbl object for further CohortContrast usage

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("CDMConnector", quietly = TRUE)) {
  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(db, "CREATE SCHEMA example")

  cohort <- data.frame(
    cohort_definition_id = c(1L, 2L),
    subject_id = c(101L, 202L),
    cohort_start_date = as.Date(c("2020-01-01", "2020-02-01")),
    cohort_end_date = as.Date(c("2020-01-10", "2020-02-10"))
  )
  DBI::dbWriteTable(db, DBI::SQL('"example"."cohort"'), cohort)

  targetTable <- cohortFromCohortTable(
    cdm = NULL,
    db = db,
    tableName = "cohort",
    schemaName = "example",
    cohortId = 2
  )
  targetTable

  DBI::dbDisconnect(db, shutdown = TRUE)
}
```
