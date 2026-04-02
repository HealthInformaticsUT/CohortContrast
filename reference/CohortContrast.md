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

## Examples

``` r
if (FALSE) { # \dontrun{
control <- data.frame(
  cohort_definition_id = c(1, 1, 1, 1, 1),
  subject_id = c(5325, 3743, 2980, 1512, 2168),
  cohort_start_date = as.Date(c("1982-06-02", "1997-03-23",
   "2004-09-29", "2006-08-11", "1977-06-25")),
  cohort_end_date = as.Date(c("2019-03-17", "2018-10-07",
   "2018-04-01", "2017-11-29", "2018-11-22"))
)

target <- data.frame(
  cohort_definition_id = c(1, 1, 1, 1, 1),
  subject_id = c(4804, 4861, 1563, 2830, 1655),
  cohort_start_date = as.Date(c("1997-03-23", "1982-06-02",
   "1977-06-25", "2006-08-11", "2004-09-29")),
  cohort_end_date = as.Date(c("2018-10-29", "2019-05-23",
   "2019-04-20", "2019-01-14", "2019-05-24"))
)

control$cohort_definition_id = 100
target$cohort_definition_id = 500

cohort = rbind(control, target)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS example")
DBI::dbWriteTable(con,   DBI::SQL('"example"."cohort"'), cohort)

cdm <- CDMConnector::cdmFromCon(con = con, cdmName = "eunomia",
 cdmSchema = "main", writeSchema = "main")

 targetTable <- cohortFromCohortTable(cdm = cdm, db = con,
  tableName = "cohort", schemaName = 'example', cohortId = 500)
controlTable <- cohortFromCohortTable(cdm = cdm, db = con,
 tableName = "cohort", schemaName = 'example', cohortId = 100)


pathToResults = getwd()

data = CohortContrast(
  cdm,
  targetTable = targetTable,
  controlTable = controlTable,
  pathToResults,
  domainsIncluded = c(
    "Drug"
  ),
  prevalenceCutOff = 0.1,
  topK = FALSE,
  presenceFilter = 0.005,
  complementaryMappingTable = NULL,
  runChi2YTests = FALSE,
  runLogitTests = FALSE,
  createOutputFiles = FALSE,
  numCores = 1
)

DBI::dbDisconnect(con)
} # }
```
