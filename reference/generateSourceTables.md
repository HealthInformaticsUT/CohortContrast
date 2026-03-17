# Generate source code analysis tables

Generate source code analysis tables

## Usage

``` r
generateSourceTables(
  data,
  domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure",
    "Visit", "Visit detail", "Death")
)
```

## Arguments

- data:

  CohortContrast object

- domainsIncluded:

  list of CDM domains to include

## Value

object of dataframes and updated cdm object
