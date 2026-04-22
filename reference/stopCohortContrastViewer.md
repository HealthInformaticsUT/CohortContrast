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
#> Using system Python: /Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python3
#> Python configuration complete.
#> Python version: 3.13
#> Python path: /Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python
#> All required Python packages are installed.
#> Starting CohortContrast Viewer...
#> App directory: /Users/markushaug/UT/R-packages/Develop/CohortContrast/inst/python
#> Data directory: /Users/markushaug/UT/R-packages/Develop/CohortContrast/inst/example/st/lc500s
#> Mode: simple
#> Port: 8050
#> CohortContrast Viewer is running!
#> URL: http://127.0.0.1:8050
#> 
#> Use stopCohortContrastViewer() to stop the server.
#> Stopping CohortContrast Viewer...
#> CohortContrast Viewer stopped successfully.
# }
```
