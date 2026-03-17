# Function for handling feature selection

Function for handling feature selection

## Usage

``` r
handleFeatureSelection(
  data,
  pathToResults,
  topK,
  prevalenceCutOff,
  targetCohortId,
  runChi2YTests,
  runLogitTests,
  createOutputFiles = FALSE
)
```

## Arguments

- data:

  Data list object

- pathToResults:

  Path to the results folder, can be project's working directory

- topK:

  numeric \> if set, keeps this number of features in the analysis.
  Maximum number of features exported.

- prevalenceCutOff:

  numeric \> if set, removes all of the concepts which are not present
  (in target) more than prevalenceCutOff times

- targetCohortId:

  Target cohort id

- runChi2YTests:

  Boolean for running the CHI2Y test (chi-squared test for two
  proportions with Yates continuity correction).

- runLogitTests:

  boolean for logit-tests

- createOutputFiles:

  Boolean for creating output files, the default value is TRUE
