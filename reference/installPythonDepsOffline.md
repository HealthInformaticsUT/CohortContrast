# Install Python Dependencies Offline

Installs Python packages from pre-downloaded wheel files (offline
installation). This is useful for air-gapped servers without internet
access.

## Usage

``` r
installPythonDepsOffline(
  packagesDir = NULL,
  platform = "linux_x86_64",
  cleanup = TRUE
)
```

## Arguments

- packagesDir:

  Path to directory containing wheel files or a .zip archive. This
  argument is required in CRAN builds.

- platform:

  Platform identifier. Default is "linux_x86_64".

- cleanup:

  Logical. If TRUE, removes extracted files after installation. Default
  is TRUE.

## Value

Invisibly returns TRUE if installation was successful.

## Details

The function supports two formats:

- A .zip file (e.g., "linux_x86_64.zip") containing wheel files

- A directory containing .whl files directly

For offline installation:

1.  Download wheel files on a machine with internet access

2.  Copy the packages folder to the offline server

3.  Run this function to install from local files

## Examples

``` r
if (FALSE) { # \dontrun{
# Install from custom location
configurePython(createVenv = FALSE)
installPythonDepsOffline(packagesDir = "/path/to/packages")
} # }
```
