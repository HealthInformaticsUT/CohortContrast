# This function performs CHI2Y tests (chi-squared test for two proportions with Yates continuity correction) on patient prevalence data (target vs control).

This function performs CHI2Y tests (chi-squared test for two proportions
with Yates continuity correction) on patient prevalence data (target vs
control).

## Usage

``` r
performPrevalenceAnalysis(
  data_patients,
  data_initial,
  targetCohortId,
  presenceFilter,
  numCores = parallel::detectCores() - 1
)
```

## Arguments

- data_patients:

  Prevalence data for patients

- data_initial:

  Imported cohort dataframe

- targetCohortId:

  Target cohort id

- presenceFilter:

  Presence filter 0-1, if 0.1 then feature has to be present for at
  least 10 percent of patients

- numCores:

  Number of cores to allocate to parallel processing
