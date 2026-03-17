# Run CohortContrast Viewer Dashboard

Launches the CohortContrast Viewer interactive dashboard.

## Usage

``` r
runCohortContrastViewer(
  dataDir = NULL,
  port = 8050,
  host = "127.0.0.1",
  mode = c("simple", "server", "debug"),
  debug = FALSE,
  logFile = NULL,
  allowExports = TRUE,
  openBrowser = TRUE,
  background = TRUE,
  ...
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

- debug:

  Logical. Backward-compatible alias for debug behavior. If TRUE, mode
  is forced to \`"debug"\`.

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

- ...:

  Backward-compatible aliases: \`data_dir\`, \`log_file\`,
  \`allow_exports\`, \`open_browser\`.

## Value

If background is TRUE, invisibly returns the process object. If FALSE,
blocks until the server is stopped.

## Examples

``` r
if (FALSE) { # \dontrun{
# Configure and launch with defaults
configurePython()
installPythonDeps()
runCohortContrastViewer()

# Launch with custom data directory
runCohortContrastViewer(dataDir = "/path/to/my/data")

# Launch on a different port
runCohortContrastViewer(port = 8080)
} # }
```
