# Remove Temporal Bias from CohortContrast Analysis

This function identifies and optionally removes concepts that may
represent temporal bias in a CohortContrast analysis. It works by
creating age/sex matched controls from the general population for the
same time periods as the target cohort, then using a proportion test to
identify concepts where the matched cohort has greater or equal
prevalence compared to the target. These concepts likely represent
temporal trends (e.g., seasonal effects, healthcare changes) rather than
condition-specific features.

## Usage

``` r
removeTemporalBias(
  data,
  cdm,
  ratio = 1,
  alpha = 0.05,
  domainsIncluded = NULL,
  removeIdentified = FALSE
)
```

## Arguments

- data:

  A CohortContrast result object (returned from CohortContrast function)

- cdm:

  Connection to the database (package CDMConnector)

- ratio:

  Matching ratio for control cohort generation (default: 1)

- alpha:

  Significance level for the proportion test before Bonferroni
  correction (default: 0.05)

- domainsIncluded:

  Domains to analyze for temporal bias (default: same as original
  analysis)

- removeIdentified:

  If TRUE, automatically remove identified temporal bias concepts from
  the data (default: FALSE)

## Value

A list containing:

- temporal_bias_concepts:

  A data frame of concepts identified as potential temporal bias

- data:

  The original or filtered CohortContrast data object (if
  removeIdentified = TRUE)

- matched_control_prevalences:

  Prevalence data from the matched control cohort

## Details

The function applies Bonferroni correction for multiple testing,
adjusting the significance level by dividing alpha by the number of
concepts being tested.

## Examples

``` r
# \donttest{
if (requireNamespace("CDMConnector", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("duckdb", quietly = TRUE) &&
    nzchar(Sys.getenv("EUNOMIA_DATA_FOLDER")) &&
    isTRUE(tryCatch(
      CDMConnector::eunomiaIsAvailable("GiBleed"),
      error = function(...) FALSE
    ))) {
  pathToJSON <- system.file(
    "example", "example_json", "diclofenac",
    package = "CohortContrast"
  )
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = CDMConnector::eunomiaDir("GiBleed")
  )
  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmName = "eunomia",
    cdmSchema = "main",
    writeSchema = "main"
  )

  targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
  controlTable <- createControlCohortInverse(cdm = cdm, targetTable = targetTable)
  data <- CohortContrast(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable,
    pathToResults = tempdir(),
    prevalenceCutOff = 1,
    topK = 3,
    presenceFilter = FALSE,
    runChi2YTests = TRUE,
    runLogitTests = TRUE,
    createOutputFiles = FALSE,
    numCores = 1
  )

  result_filtered <- removeTemporalBias(
    data = data,
    cdm = cdm,
    ratio = 1,
    removeIdentified = TRUE
  )
  head(result_filtered$data$data_features)

  DBI::dbDisconnect(con, shutdown = TRUE)
}
# }
```
