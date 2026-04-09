# Run CohortContrast Analysis

Run CohortContrast Analysis

## Usage

``` r
CohortContrast(
  cdm,
  targetTable = NULL,
  controlTable = NULL,
  pathToResults = getwd(),
  domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure",
    "Visit", "Visit detail", "Death"),
  prevalenceCutOff = 10,
  topK = FALSE,
  presenceFilter = 0.005,
  complementaryMappingTable = NULL,
  runChi2YTests = TRUE,
  runLogitTests = TRUE,
  getAllAbstractions = FALSE,
  getSourceData = FALSE,
  maximumAbstractionLevel = 5,
  createOutputFiles = TRUE,
  complName = NULL,
  runRemoveTemporalBias = FALSE,
  removeTemporalBiasArgs = list(),
  runAutomaticHierarchyCombineConcepts = FALSE,
  automaticHierarchyCombineConceptsArgs = list(),
  runAutomaticCorrelationCombineConcepts = FALSE,
  automaticCorrelationCombineConceptsArgs = list(),
  numCores = max(1L, ceiling(0.2 * parallel::detectCores()), na.rm = TRUE)
)
```

## Arguments

- cdm:

  Connection to database

- targetTable:

  Table for target cohort (tbl)

- controlTable:

  Table for control cohort (tbl)

- pathToResults:

  Path to the results folder, can be project's working directory

- domainsIncluded:

  list of CDM domains to include

- prevalenceCutOff:

  numeric \> if set, removes all of the concepts which are not present
  (in target) more than prevalenceCutOff times

- topK:

  numeric \> if set, keeps this number of features in the analysis.
  Maximum number of features exported.

- presenceFilter:

  numeric \> if set, removes all features represented less than the
  given percentage

- complementaryMappingTable:

  Mappingtable for mapping concept_ids if present, columns CONCEPT_ID,
  CONCEPT_NAME, NEW_CONCEPT_ID, NEW_CONCEPT_NAME, ABSTRACTION_LEVEL,
  TYPE

- runChi2YTests:

  Boolean for running the CHI2Y test (chi-squared test for two
  proportions with Yates continuity correction).

- runLogitTests:

  boolean for logit-tests

- getAllAbstractions:

  boolean for creating abstractions' levels for the imported data, this
  is useful when using GUI and exploring data

- getSourceData:

  boolean for fetching source data

- maximumAbstractionLevel:

  Maximum level of abstraction allowed

- createOutputFiles:

  Boolean for creating output files, the default value is TRUE

- complName:

  Name of the output study directory

- runRemoveTemporalBias:

  Logical; when \`TRUE\`, runs \`removeTemporalBias()\` as an optional
  post-processing step.

- removeTemporalBiasArgs:

  A list of additional arguments passed to \`removeTemporalBias()\` (for
  example \`ratio\`, \`alpha\`, \`domainsIncluded\`,
  \`removeIdentified\`). Missing arguments default to \`ratio = 1\`,
  \`alpha = 0.05\`, \`domainsIncluded = NULL\`, and \`removeIdentified =
  TRUE\`.

- runAutomaticHierarchyCombineConcepts:

  Logical; when \`TRUE\`, runs \`automaticHierarchyCombineConcepts()\`
  after temporal-bias processing.

- automaticHierarchyCombineConceptsArgs:

  A list of additional arguments passed to
  \`automaticHierarchyCombineConcepts()\` (for example
  \`abstractionLevel\`, \`minDepthAllowed\`, \`allowOnlyMinors\`).
  Missing arguments default to \`abstractionLevel = -1\`,
  \`minDepthAllowed = 0\`, and \`allowOnlyMinors = TRUE\`.

- runAutomaticCorrelationCombineConcepts:

  Logical; when \`TRUE\`, runs \`automaticCorrelationCombineConcepts()\`
  after hierarchy combining.

- automaticCorrelationCombineConceptsArgs:

  A list of additional arguments passed to
  \`automaticCorrelationCombineConcepts()\` (for example
  \`abstractionLevel\`, \`minCorrelation\`, \`maxDaysInBetween\`,
  \`heritageDriftAllowed\`). Missing arguments default to
  \`abstractionLevel = -1\`, \`minCorrelation = 0.7\`,
  \`maxDaysInBetween = 1\`, and \`heritageDriftAllowed = FALSE\`.

- numCores:

  Number of cores to allocate to parallel processing. Defaults to 20
  percent of detected cores (minimum 1).

## Value

A CohortContrastObject. This is a list with the main analysis tables
data_patients, data_initial, data_person, data_features, conceptsData,
complementaryMappingTable, selectedFeatureData, trajectoryDataList, and
config. Together these components contain the processed cohort-level,
person-level, feature-level, optional mapping, and configuration outputs
produced by the workflow.

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

  result <- CohortContrast(
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

  head(result$data_features)
  DBI::dbDisconnect(con, shutdown = TRUE)
}
# }
```
