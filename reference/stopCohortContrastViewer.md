# Stop CohortContrast Viewer Dashboard

Stops a running CohortContrast Viewer dashboard instance.

## Usage

``` r
stopCohortContrastViewer()
```

## Value

Invisibly returns TRUE if the server was stopped.

## Examples

``` r
# \donttest{
if (interactive() &&
    requireNamespace("reticulate", quietly = TRUE) &&
    requireNamespace("processx", quietly = TRUE) &&
    (nzchar(Sys.which("python3")) || nzchar(Sys.which("python")))) {
  configurePython(createVenv = FALSE)
  deps <- checkPythonDeps()
  if (all(deps$installed)) {
    summaryDir <- system.file("example", "st", "lc500s", package = "CohortContrast")
    runCohortContrastViewer(
      dataDir = summaryDir,
      openBrowser = FALSE,
      background = TRUE
    )
    stopCohortContrastViewer()
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
