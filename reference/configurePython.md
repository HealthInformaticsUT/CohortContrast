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
  force = FALSE,
  ...
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

- ...:

  Backward-compatible aliases: \`python_path\`, \`virtualenv_name\`,
  \`create_venv\`.

## Value

Invisibly returns TRUE if configuration was successful.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use auto-detected Python with a new virtual environment
configurePython()

# Use a specific Python installation without virtual environment
# (useful on servers without python3-venv package)
configurePython(pythonPath = "/usr/bin/python3", createVenv = FALSE)

# Use an existing conda environment
configurePython(virtualenvName = "my-conda-env", createVenv = FALSE)
} # }
```
