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
