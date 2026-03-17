# Check if a Data Directory Contains Summary Mode Data

Determines whether a data directory contains pre-computed summary data
(summary mode) or patient-level data (patient mode).

## Usage

``` r
checkDataMode(dataDir, ...)
```

## Arguments

- dataDir:

  Path to the data directory

- ...:

  Backward-compatible alias: \`data_dir\`.

## Value

A list with:

- mode:

  "summary" or "patient"

- has_clustering:

  Logical, whether clustering data is available

- clusterKValues:

  Vector of available k values for clustering (canonical name; legacy
  \`cluster_k_values\` is also returned).

## Examples

``` r
if (FALSE) { # \dontrun{
info <- checkDataMode("results_parquet/Breast_cancer")
print(info$mode)  # "patient" or "summary"
} # }
```
