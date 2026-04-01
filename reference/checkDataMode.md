# Check if a Data Directory Contains Summary Mode Data

Determines whether a data directory contains pre-computed summary data
(summary mode) or patient-level data (patient mode).

## Usage

``` r
checkDataMode(dataDir)
```

## Arguments

- dataDir:

  Path to the data directory

## Value

A list with:

- mode:

  "summary" or "patient"

- has_clustering:

  Logical, whether clustering data is available

- clusterKValues:

  Vector of available k values for clustering

## Examples

``` r
if (FALSE) { # \dontrun{
info <- checkDataMode("results_parquet/Breast_cancer")
print(info$mode)  # "patient" or "summary"
} # }
```
