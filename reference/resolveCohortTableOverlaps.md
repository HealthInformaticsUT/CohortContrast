# Resolve overlaps inside the cohort table

Resolve overlaps inside the cohort table

## Usage

``` r
resolveCohortTableOverlaps(cohortTable, cdm)
```

## Arguments

- cohortTable:

  A table with cohort table characteristics

- cdm:

  CDMConnector object: connection to the database

## Value

A dataframe like cohort table with resolved overlaps

## Examples

``` r
cohortTable <- data.frame(
  cohort_definition_id = c(1L, 1L, 1L),
  subject_id = c(1L, 1L, 2L),
  cohort_start_date = as.Date(c("2020-01-01", "2020-01-05", "2020-02-01")),
  cohort_end_date = as.Date(c("2020-01-10", "2020-01-20", "2020-02-10"))
)
cdm <- list(
  observation_period = data.frame(
    person_id = c(1L, 2L),
    observation_period_start_date = as.Date(c("2020-01-03", "2020-01-15")),
    observation_period_end_date = as.Date(c("2020-01-18", "2020-02-20"))
  )
)

resolveCohortTableOverlaps(cohortTable, cdm)
#> # A tibble: 3 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>         
#> 1                    1          1 2020-01-03        2020-01-10     
#> 2                    1          1 2020-01-11        2020-01-18     
#> 3                    1          2 2020-02-01        2020-02-10     
```
