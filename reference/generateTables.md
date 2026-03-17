# Generate analysis tables Create relations of cohorts (target and control) to the database

Generate analysis tables Create relations of cohorts (target and
control) to the database

## Usage

``` r
generateTables(
  cdm,
  domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure",
    "Visit", "Visit detail", "Death")
)
```

## Arguments

- cdm:

  CDMConnector object: connection to the database

- domainsIncluded:

  list of CDM domains to include

## Value

object of dataframes and updated cdm object
