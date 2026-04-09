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
#> Using system Python: /opt/homebrew/bin/python3
#> Python configuration complete.
#> Python version: 3.13
#> Python path: /opt/homebrew/bin/python3
#> Missing packages: dash, dash_bootstrap_components, dash_ag_grid, pandas, numpy, plotly, scipy, sklearn, sklearn_extra, diskcache
#> Run installPythonDeps() to install missing packages.
#>                                             package installed
#> dash                                           dash     FALSE
#> dash_bootstrap_components dash_bootstrap_components     FALSE
#> dash_ag_grid                           dash_ag_grid     FALSE
#> pandas                                       pandas     FALSE
#> numpy                                         numpy     FALSE
#> plotly                                       plotly     FALSE
#> scipy                                         scipy     FALSE
#> sklearn                                     sklearn     FALSE
#> sklearn_extra                         sklearn_extra     FALSE
#> diskcache                                 diskcache     FALSE
# }
```
