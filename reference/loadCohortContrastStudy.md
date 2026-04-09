# Load a Saved CohortContrast Study

Loads a saved study folder produced by \`CohortContrast()\` and
reconstructs a \`CohortContrastObject\` for downstream post-processing.

## Usage

``` r
loadCohortContrastStudy(studyName, pathToResults = getwd())
```

## Arguments

- studyName:

  Name of the study folder inside \`pathToResults\`.

- pathToResults:

  Path to the parent directory containing study folders. Defaults to
  current working directory.

## Value

A \`CohortContrastObject\`.

## Examples

``` r
if (requireNamespace("nanoparquet", quietly = TRUE)) {
  studyDir <- system.file("example", "st", package = "CohortContrast")
  study <- loadCohortContrastStudy("lc500", pathToResults = studyDir)
  class(study)
}
#> [1] "CohortContrastObject"
```
