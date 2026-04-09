# Run CohortContrast Viewer Dashboard

Launches the CohortContrast Viewer interactive dashboard.

## Usage

``` r
runCohortContrastViewer(
  dataDir = NULL,
  port = 8050,
  host = "127.0.0.1",
  mode = c("simple", "server", "debug"),
  logFile = NULL,
  allowExports = TRUE,
  openBrowser = TRUE,
  background = TRUE
)
```

## Arguments

- dataDir:

  Path to the directory containing parquet data files. If NULL, defaults
  to the current working directory (\`getwd()\`).

- port:

  Port number for the Dash server. Default is 8050.

- host:

  Host address. Default is "127.0.0.1" (localhost).

- mode:

  Run mode. One of \`"simple"\`, \`"server"\`, \`"debug"\`. \`"simple"\`
  hides debug-style output and is the default. \`"server"\` enables file
  logging suitable for hosted/server runs. \`"debug"\` enables maximum
  debug features.

- logFile:

  Optional log file path for \`"server"\` (or \`"debug"\`) mode. If NULL
  in \`"server"\` mode, defaults to \`file.path(dataDir,
  "contrast_viewer.log")\`.

- allowExports:

  Logical. If FALSE, disables export actions (TSV/PNG) in the UI.
  Default is TRUE.

- openBrowser:

  Logical. If TRUE, opens the dashboard in the default browser. Default
  is TRUE.

- background:

  Logical. If TRUE, runs the server in the background. Default is TRUE.

## Value

If background is TRUE, invisibly returns the process object. If FALSE,
blocks until the server is stopped.

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
