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
if (FALSE) { # \dontrun{
# First configure Python
configurePython()

# Then install dependencies
installPythonDeps()

# Install to user directory (no admin rights needed)
installPythonDeps(user = TRUE)
} # }
```
