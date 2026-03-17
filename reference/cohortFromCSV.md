# Read cohort from CSV

Read cohort from CSV

## Usage

``` r
cohortFromCSV(pathToCsv, cohortId = NULL)
```

## Arguments

- pathToCsv:

  Path to the cohort data CSV file

- cohortId:

  The id for cohort in cohorts' table, if NULL whole table will be
  imported

## Value

a tbl object for further CohortContrast usage

## Examples

``` r
if (FALSE) { # \dontrun{
pathToCsv = './cohorts.csv'
targetTable <- cohortFromCSV(pathToCsv = pathToCsv, cohortId = 2)
} # }
```
