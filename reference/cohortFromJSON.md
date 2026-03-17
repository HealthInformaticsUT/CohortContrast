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
if (FALSE) { # \dontrun{
pathToJSON = './JSON/'
targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm, cohortId = 2)
} # }
```
