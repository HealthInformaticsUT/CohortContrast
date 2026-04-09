# Pre-compute Summary Data for a Study

Generates aggregated summary data from patient-level parquet files,
removing all individual patient information. This creates a "summary
mode" dataset that can be shared without privacy concerns.

## Usage

``` r
precomputeSummary(
  studyPath,
  outputPath = NULL,
  clusterKValues = c(2, 3, 4, 5),
  conceptLimit = 60,
  minCellCount = 0,
  maxParallelJobs = 1,
  minibatchKMeansCutoffPatients = 50000
)
```

## Arguments

- studyPath:

  Path to directory containing patient-level parquet files
  (data_patients.parquet, data_features.parquet, etc.)

- outputPath:

  Path for output summary files. Default is studyPath + "\_summary"

- clusterKValues:

  Vector of k values to pre-compute clustering for. Default is c(2, 3,
  4, 5).

- conceptLimit:

  Maximum number of concepts to use for clustering. Default is 60.

- minCellCount:

  Minimum patient count for small cell suppression (privacy). Counts
  between 1 and (minCellCount-1) are rounded up to minCellCount. Default
  is 0 (disabled). Set to e.g. 5 to apply suppression.

- maxParallelJobs:

  Maximum number of parallel clustering jobs. Default is 1 (sequential)
  to avoid out-of-memory errors on servers. Set to 2-4 on machines with
  ample RAM for faster execution.

- minibatchKMeansCutoffPatients:

  If target patient count is greater than this value, clustering uses
  MiniBatchKMeans instead of KMedoids. Default is 50000.

## Value

A list with:

- outputPath:

  Path to the generated summary directory.

- files:

  Named list of generated file paths

- metadata:

  Study metadata including demographics and clustering info

## Details

The function pre-computes: - Concept-level time distribution statistics
(for violin/box plots) - Age and gender statistics per concept - Ordinal
concept summaries (1st, 2nd, 3rd occurrences) - Clustering results for
k=2,3,4,5 clusters - Cluster overlap and differentiation metrics

## Examples

``` r
# \donttest{
if (requireNamespace("reticulate", quietly = TRUE) &&
    (nzchar(Sys.which("python3")) || nzchar(Sys.which("python")))) {
  configurePython(createVenv = FALSE)
  deps <- checkPythonDeps()
  if (all(deps$installed)) {
    studyPath <- system.file("example", "st", "lc500", package = "CohortContrast")
    outputPath <- file.path(tempdir(), "lc500_summary")
    result <- precomputeSummary(
      studyPath = studyPath,
      outputPath = outputPath,
      clusterKValues = c(2, 3),
      conceptLimit = 20,
      maxParallelJobs = 1
    )
    result$outputPath
  }
}
#> Configuring Python environment for CohortContrast Viewer...
#> Using system Python: /opt/homebrew/bin/python3
#> Python configuration complete.
#> Python version: 3.13
#> Python path: /opt/homebrew/bin/python3
#> Missing packages: dash, dash_bootstrap_components, dash_ag_grid, pandas, numpy, plotly, scipy, sklearn, sklearn_extra, diskcache
#> Run installPythonDeps() to install missing packages.
# }
```
