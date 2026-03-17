# Function for handling tests

Function for handling tests

## Usage

``` r
handleTests(
  data,
  targetCohortId,
  presenceFilter,
  runChi2YTests = TRUE,
  runLogitTests = TRUE,
  numCores = parallel::detectCores() - 1
)
```

## Arguments

- data:

  Data list object

- targetCohortId:

  Target cohort id

- presenceFilter:

  numeric \> if set, removes all features represented less than the
  given percentage

- runChi2YTests:

  Boolean for running the CHI2Y test (chi-squared test for two
  proportions with Yates continuity correction).

- runLogitTests:

  boolean for logit-tests
