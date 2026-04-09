# Get Python Configuration Information

Returns information about the current Python configuration for the
CohortContrast Viewer.

## Usage

``` r
getPythonInfo()
```

## Value

A list with Python configuration details

## Examples

``` r
# \donttest{
if (requireNamespace("reticulate", quietly = TRUE) &&
    (nzchar(Sys.which("python3")) || nzchar(Sys.which("python")))) {
  configurePython(createVenv = FALSE)
  getPythonInfo()
}
#> Configuring Python environment for CohortContrast Viewer...
#> Using system Python: /opt/homebrew/bin/python3
#> Python configuration complete.
#> Python version: 3.13
#> Python path: /opt/homebrew/bin/python3
#> $python_version
#> [1] ‘3.13’
#> 
#> $pythonPath
#> [1] "/opt/homebrew/bin/python3"
#> 
#> $virtualenv
#> NULL
#> 
#> $configured
#> [1] TRUE
#> 
#> $numpy_available
#> [1] FALSE
#> 
#> $pandas_available
#> [1] FALSE
#> 
#> $dash_available
#> [1] FALSE
#> 
# }
```
