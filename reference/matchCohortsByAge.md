# Function for matching the control to target by age

Function for matching the control to target by age

## Usage

``` r
matchCohortsByAge(cdm, sourceTable, tableToMatch, maxAllowedAgeDifference = 0)
```

## Arguments

- cdm:

  Connection to the database (package CDMConnector)

- sourceTable:

  Table which is used as reference for matching

- tableToMatch:

  Table which is matched

- maxAllowedAgeDifference:

  Value for maximum allowed age difference to be mapped to

## Value

A list with two data frames named source and tableToMatch. Each data
frame contains the matched rows from the corresponding input table after
age-based sampling, preserving the original column structure of the
input cohort tables.

## Examples

``` r
cdm <- list(
  person = tibble::tibble(
    person_id = 1:13,
    year_of_birth = c(1980L, 1981L, 1982L, 1983L, 1984L,
                      1980L, 1981L, 1982L, 1985L, 1986L, 1982L, 1983L, 1984L)
  )
)
sourceTable <- tibble::tibble(
  cohort_definition_id = 1L,
  subject_id = c(1L, 2L, 3L, 4L, 5L),
  cohort_start_date = as.Date(rep("2020-01-01", 5)),
  cohort_end_date = as.Date(rep("2020-01-10", 5))
)
tableToMatch <- tibble::tibble(
  cohort_definition_id = 2L,
  subject_id = c(6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L),
  cohort_start_date = as.Date(rep("2020-01-01", 8)),
  cohort_end_date = as.Date(rep("2020-01-10", 8))
)

matched <- matchCohortsByAge(
  cdm = cdm,
  sourceTable = sourceTable,
  tableToMatch = tableToMatch,
  maxAllowedAgeDifference = 1
)
#> Average age difference before matching: 2.02 years
#> Matched 5 pairs
#> Average age difference after matching: 0.40 years (max allowed: 1)
nrow(tableToMatch)
#> [1] 8
matched
#> $source
#> # A tibble: 5 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>         
#> 1                    1          5 2020-01-01        2020-01-10     
#> 2                    1          4 2020-01-01        2020-01-10     
#> 3                    1          3 2020-01-01        2020-01-10     
#> 4                    1          2 2020-01-01        2020-01-10     
#> 5                    1          1 2020-01-01        2020-01-10     
#> 
#> $tableToMatch
#> # A tibble: 5 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>         
#> 1                    2         12 2020-01-01        2020-01-10     
#> 2                    2          8 2020-01-01        2020-01-10     
#> 3                    2         11 2020-01-01        2020-01-10     
#> 4                    2          7 2020-01-01        2020-01-10     
#> 5                    2          6 2020-01-01        2020-01-10     
#> 
```
