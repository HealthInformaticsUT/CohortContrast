# Function for creating automatic matches based on age and sex

Function for creating automatic matches based on age and sex

## Usage

``` r
createControlCohortMatching(
  cdm,
  targetTable,
  ratio = 1,
  max = NULL,
  min = NULL
)
```

## Arguments

- cdm:

  Connection to the database (package CDMConnector)

- targetTable:

  A cohort tibble which contains subjects' cohort data

- ratio:

  ratio for the number of matches generated

- max:

  Maximum ratio to use

- min:

  Minimum ratio to use

## Examples
