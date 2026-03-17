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
if (FALSE) { # \dontrun{
targetTable <- cohortFromCohortTable(cdm = cdm, db = db,
 tableName = "cohort", schemaName = 'ohdsi_results', cohortId = 1389)
} # }
if (FALSE) { # \dontrun{
targetTable <- cohortFromCohortTable(cdm = cdm, db = db,
 tableName = "asthma", schemaName = 'user_peter')
} # }
```
