# Install Python Dependencies

Installs the required Python packages for the CohortContrast Viewer.

## Usage

``` r
installPythonDeps(upgrade = FALSE, quiet = FALSE, user = NULL)
```

## Arguments

- upgrade:

  Logical. If TRUE, upgrades existing packages. Default is FALSE.

- quiet:

  Logical. If TRUE, suppresses pip output. Default is FALSE.

- user:

  Logical. If TRUE, installs to user site-packages (–user flag). Useful
  when not using a virtual environment. Default is NULL (auto-detect).

## Value

Invisibly returns TRUE if installation was successful.

## Examples

``` r
# \donttest{
if (interactive() &&
    requireNamespace("reticulate", quietly = TRUE) &&
    (nzchar(Sys.which("python3")) || nzchar(Sys.which("python")))) {
  configurePython(createVenv = FALSE)
  installPythonDeps(user = TRUE)
}
#> Configuring Python environment for CohortContrast Viewer...
#> Using system Python: /opt/homebrew/bin/python3
#> Python configuration complete.
#> Python version: 3.13
#> Python path: /opt/homebrew/bin/python3
#> Installing Python dependencies from: /Users/markushaug/UT/R-packages/Develop/CohortContrast/inst/python/requirements.txt
#> Using Python: /opt/homebrew/Cellar/python@3.13/3.13.2/Frameworks/Python.framework/Versions/3.13/bin/python3.13
#> Creating virtual environment '~/.virtualenvs/r-reticulate' ... 
#> + '/opt/homebrew/Cellar/python@3.13/3.13.2/Frameworks/Python.framework/Versions/3.13/bin/python3.13' -m venv /Users/markushaug/.virtualenvs/r-reticulate
#> Done!
#> Installing packages: pip, wheel, setuptools
#> + /Users/markushaug/.virtualenvs/r-reticulate/bin/python -m pip install --upgrade pip wheel setuptools
#> Virtual environment '~/.virtualenvs/r-reticulate' successfully created.
#> Using virtual environment '~/.virtualenvs/r-reticulate' ...
#> + /Users/markushaug/.virtualenvs/r-reticulate/bin/python -m pip install --upgrade --no-user -r /Users/markushaug/UT/R-packages/Develop/CohortContrast/inst/python/requirements.txt --user
#> Trying alternative installation method...
#> Error in value[[3L]](cond): Failed to install Python dependencies.
#> Try running manually: /opt/homebrew/bin/python3 -m pip install -r '/Users/markushaug/UT/R-packages/Develop/CohortContrast/inst/python/requirements.txt' --user
#> Original error: Error installing package(s): 
# }
```
