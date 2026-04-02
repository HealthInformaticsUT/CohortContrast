# Run CohortContrast Analysis

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

Run CohortContrast Analysis
