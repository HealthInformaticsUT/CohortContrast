# Check Python Dependencies

Checks if all required Python packages are installed.

## Usage

``` r
checkPythonDeps()
```

## Value

A data frame with package names and their installation status.

## Examples

``` r
# \donttest{
if (requireNamespace("reticulate", quietly = TRUE) &&
    (nzchar(Sys.which("python3")) || nzchar(Sys.which("python")))) {
  configurePython(createVenv = FALSE)
  checkPythonDeps()
}
#> Configuring Python environment for CohortContrast Viewer...
#> Using system Python: /Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python3
#> Python configuration complete.
#> Python version: 3.13
#> Python path: /Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python
#> All required Python packages are installed.
#>                                             package installed
#> dash                                           dash      TRUE
#> dash_bootstrap_components dash_bootstrap_components      TRUE
#> dash_ag_grid                           dash_ag_grid      TRUE
#> pandas                                       pandas      TRUE
#> numpy                                         numpy      TRUE
#> plotly                                       plotly      TRUE
#> scipy                                         scipy      TRUE
#> sklearn                                     sklearn      TRUE
#> sklearn_extra                         sklearn_extra      TRUE
#> diskcache                                 diskcache      TRUE
# }
```
