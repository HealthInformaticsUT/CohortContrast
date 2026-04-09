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
if (requireNamespace("readr", quietly = TRUE)) {
  pathToCsv <- tempfile(fileext = ".csv")
  cohort <- data.frame(
    cohort_definition_id = c(1L, 2L),
    subject_id = c(101L, 202L),
    cohort_start_date = as.Date(c("2020-01-01", "2020-02-01")),
    cohort_end_date = as.Date(c("2020-01-10", "2020-02-10"))
  )
  readr::write_csv(cohort, pathToCsv)

  targetTable <- cohortFromCSV(pathToCsv = pathToCsv, cohortId = 2)
  targetTable
}
#> # A tibble: 1 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <dbl>      <dbl> <date>            <date>         
#> 1                    2        202 2020-02-01        2020-02-10     
```
