# This function learns separate logistic regression models on the patient prevalence data target vs control

This function learns separate logistic regression models on the patient
prevalence data target vs control

## Usage

``` r
performPrevalenceAnalysisLogistic(
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
