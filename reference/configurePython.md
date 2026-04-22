# Configure Python Environment for CohortContrast Viewer

Sets up the Python environment for running the CohortContrast Viewer
dashboard. This function can create a new virtual environment or use an
existing Python installation.

## Usage

``` r
configurePython(
  pythonPath = NULL,
  virtualenvName = "r-cohortcontrast-viewer",
  createVenv = TRUE,
  force = FALSE
)
```

## Arguments

- pythonPath:

  Optional path to a specific Python executable. If NULL, reticulate
  will auto-detect Python.

- virtualenvName:

  Name for the virtual environment. Default is
  "r-cohortcontrast-viewer".

- createVenv:

  Logical. If TRUE, creates a new virtual environment. Default is TRUE.
  Set to FALSE on systems without python3-venv package.

- force:

  Logical. If TRUE, recreates the virtual environment even if it exists.
  Default is FALSE.

## Value

Invisibly returns TRUE if configuration was successful.

## Examples

``` r
# \donttest{
if (requireNamespace("reticulate", quietly = TRUE) &&
    (nzchar(Sys.which("python3")) || nzchar(Sys.which("python")))) {
  configurePython(createVenv = FALSE)
  getPythonInfo()
}
#> Configuring Python environment for CohortContrast Viewer...
#> Using system Python: /Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python3
#> Python configuration complete.
#> Python version: 3.13
#> Python path: /Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python
#> $python_version
#> [1] ‘3.13’
#> 
#> $pythonPath
#> [1] "/Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python"
#> 
#> $virtualenv
#> NULL
#> 
#> $configured
#> [1] TRUE
#> 
#> $numpy_available
#> [1] TRUE
#> 
#> $pandas_available
#> [1] TRUE
#> 
#> $dash_available
#> [1] TRUE
#> 
# }
```
