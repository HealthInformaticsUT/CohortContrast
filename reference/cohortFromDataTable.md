# Read cohort from data.frame object

Read cohort from data.frame object

## Usage

``` r
cohortFromDataTable(data, cohortId = NULL)
```

## Arguments

- data:

  A data frame with cohort data

- cohortId:

  The id for cohort in cohorts' table, if NULL whole table will be
  imported

## Value

a tbl object for further CohortContrast usage

## Examples

``` r
data <- tibble::tribble(
  ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
  1, 4804, '1997-03-23', '2018-10-29',
  1, 4861, '1982-06-02', '2019-05-23',
  1, 1563, '1977-06-25', '2019-04-20',
  1, 2830, '2006-08-11', '2019-01-14',
  1, 1655, '2004-09-29', '2019-05-24',
  2, 5325, '1982-06-02', '2019-03-17',
  2, 3743, '1997-03-23', '2018-10-07',
  2, 2980, '2004-09-29', '2018-04-01',
  2, 1512, '2006-08-11', '2017-11-29',
  2, 2168, '1977-06-25', '2018-11-22'
)
targetTable <- cohortFromDataTable(data = data, cohortId = 2)
targetTable
#> # A tibble: 5 × 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <dbl>      <dbl> <chr>             <chr>          
#> 1                    2       5325 1982-06-02        2019-03-17     
#> 2                    2       3743 1997-03-23        2018-10-07     
#> 3                    2       2980 2004-09-29        2018-04-01     
#> 4                    2       1512 2006-08-11        2017-11-29     
#> 5                    2       2168 1977-06-25        2018-11-22     
```
