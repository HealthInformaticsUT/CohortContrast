# Air-gapped Server Setup

## Scope

This guide covers CohortContrast deployment on air-gapped servers where
internet access is not available at runtime.

It applies to both Python-backed workflows:

- [`runCohortContrastViewer()`](https://healthinformaticsut.github.io/CohortContrast/reference/runCohortContrastViewer.md)
  (GUI)
- [`precomputeSummary()`](https://healthinformaticsut.github.io/CohortContrast/reference/precomputeSummary.md)
  (summary-mode artifacts)

## What differs in air-gapped environments

- You cannot use
  [`installPythonDeps()`](https://healthinformaticsut.github.io/CohortContrast/reference/installPythonDeps.md)
  against online package indexes.
- You must provide local wheel files (`.whl`) for all required Python
  dependencies.
- Wheels must match:
  - target operating system
  - target CPU architecture
  - target Python major/minor version
- You should validate the Python environment before running GUI or
  precompute.

## Offline setup workflow

### 1. Prepare wheels on a connected machine

Use a machine that matches the target server runtime (OS, architecture,
Python version).

``` bash
mkdir -p /tmp/ccv_wheels/linux_x86_64
python3 -m pip download \
  --dest /tmp/ccv_wheels/linux_x86_64 \
  dash dash-bootstrap-components dash-ag-grid \
  pandas pyarrow numpy scipy \
  plotly diskcache psutil multiprocess \
  scikit-learn scikit-learn-extra

# Optional: compress for transfer
cd /tmp/ccv_wheels
zip -r linux_x86_64.zip linux_x86_64
```

Supported offline layouts for
[`installPythonDepsOffline()`](https://healthinformaticsut.github.io/CohortContrast/reference/installPythonDepsOffline.md):

- `packagesDir/linux_x86_64.zip`
- `packagesDir/linux_x86_64/*.whl`

### 2. Transfer wheels to the air-gapped server

Copy either:

- `linux_x86_64.zip` into a directory such as
  `/opt/cohortcontrast/packages`, or
- the wheel directory as
  `/opt/cohortcontrast/packages/linux_x86_64/*.whl`.

### 3. Configure Python in R

If virtual environments are available:

``` r
configurePython(
  pythonPath = "/usr/bin/python3",
  virtualenvName = "r-cohortcontrast-viewer",
  createVenv = TRUE
)
```

If `venv`/`ensurepip` is unavailable on the server:

``` r
configurePython(
  pythonPath = "/usr/bin/python3",
  createVenv = FALSE
)
```

### 4. Install dependencies from local wheels

``` r
installPythonDepsOffline(
  packagesDir = "/opt/cohortcontrast/packages",
  platform = "linux_x86_64"
)
```

### 5. Validate before production usage

``` r
checkPythonDeps()
```

Confirm all required modules report as installed.

## Offline smoke test

### Summary precompute

``` r
summaryResult <- precomputeSummary(
  studyPath = "/data/studies/LungCancer_1Y",
  outputPath = "/data/studies/LungCancer_1Y_summary",
  clusterKValues = c(2, 3, 4, 5)
)
```

### GUI in server context

On servers, prefer `mode = "server"` and `openBrowser = FALSE`.

``` r
runCohortContrastViewer(
  dataDir = "/data/studies",
  host = "0.0.0.0",
  port = 8050,
  mode = "server",
  logFile = "/data/studies/contrast_viewer.log",
  openBrowser = FALSE,
  background = TRUE
)
```

## Runtime mode guidance for servers

- `simple`: minimal logging/noise; good for local ad hoc use.
- `server`: file logging and stable hosted operation; recommended on
  air-gapped servers.
- `debug`: maximum diagnostics; use for troubleshooting only.

Optional export lockdown (common in controlled environments):

``` r
runCohortContrastViewer(
  dataDir = "/data/studies",
  mode = "server",
  allowExports = FALSE,
  openBrowser = FALSE
)
```

## Common failures and fixes

- `not a supported wheel on this platform`:
  - regenerate wheels for the server’s exact Python version, OS, and
    architecture.
- `venv`/`ensurepip` errors:
  - use `createVenv = FALSE` and system Python.
- Missing modules after install:
  - rerun
    [`checkPythonDeps()`](https://healthinformaticsut.github.io/CohortContrast/reference/checkPythonDeps.md),
    verify wheel directory contents, reinstall with
    [`installPythonDepsOffline()`](https://healthinformaticsut.github.io/CohortContrast/reference/installPythonDepsOffline.md).
- Wrong interpreter selected:
  - set explicit `pythonPath` in `configurePython(...)`.
- Viewer starts but no logs written:
  - use `mode = "server"` and set `logFile` explicitly.

## Minimal offline checklist

1.  Prepare matching wheelhouse on connected machine.
2.  Transfer wheelhouse to server.
3.  `configurePython(...)` with explicit interpreter.
4.  `installPythonDepsOffline(...)`.
5.  [`checkPythonDeps()`](https://healthinformaticsut.github.io/CohortContrast/reference/checkPythonDeps.md).
6.  Run `precomputeSummary(...)` and `runCohortContrastViewer(...)`
    smoke tests.
