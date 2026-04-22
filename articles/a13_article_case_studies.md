# Article Case Study Generation

## Goal

This vignette documents the code used to generate the article-style
`CohortContrast` studies shown at
<http://omop-apps.cloud.ut.ee/CohortContrast/>.

The workflow uses cohort JSON definitions exported from ATLAS and runs
against the OPTIMA OMOP Common Data Model database.

The workflow is designed to produce:

- one final summary-mode study folder per case

## Expected JSON layout

Each cohort JSON definition should live in its own directory, because
[`CohortContrast::cohortFromJSON()`](https://healthinformaticsut.github.io/CohortContrast/reference/cohortFromJSON.md)
expects a directory containing the JSON cohort definition files.

Recommended package layout:

- `inst/extdata/article_cases/lung/cohort.json`
- `inst/extdata/article_cases/prostate/cohort.json`

When the package is installed, these paths can be resolved with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

## Notes

- This vignette is shown as code only and is not executed during package
  checks, because it depends on a real OMOP Common Data Model database.
- The workflow below uses one clean
  [`CohortContrast()`](https://healthinformaticsut.github.io/CohortContrast/reference/CohortContrast.md)
  analysis followed by a single export of the final filtered and mapped
  study object to a temporary patient-mode folder. That temporary folder
  is used only to create the summary-mode study and is then removed. The
  retained study folders therefore contain only the final concepts that
  passed the statistical selection steps.

## Required environment variables

``` r

# Database connection settings expected by the helper below.
Sys.getenv("DB_NAME")
Sys.getenv("DB_HOST")
Sys.getenv("DB_PORT")
Sys.getenv("DB_USERNAME")
Sys.getenv("DB_PASSWORD")

# OMOP schema settings.
Sys.getenv("OHDSI_CDM")
Sys.getenv("OHDSI_RESULTS")
Sys.getenv("OHDSI_WRITE")
```

## Connect to the OMOP database

``` r

library(CohortContrast)
library(CDMConnector)
library(DBI)
library(RPostgres)
library(dplyr)
library(purrr)
library(tibble)

connectArticleCdm <- function() {
  # Open the database connection using environment variables instead of
  # hard-coded credentials.
  db <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USERNAME"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT"))
  )

  # Create the CDMConnector object used by CohortContrast.
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = Sys.getenv("OHDSI_CDM"),
    achillesSchema = Sys.getenv("OHDSI_RESULTS"),
    writeSchema = c(
      schema = Sys.getenv("OHDSI_WRITE"),
      prefix = "cc_"
    )
  )

  list(db = db, cdm = cdm)
}
```

## Define study cases

``` r

# Each case points to a directory containing one ATLAS cohort JSON definition.
# Add future studies by inserting a new row here.
cases <- tibble::tribble(
  ~case_name, ~json_dir, ~study_name,
  "lung",
  system.file("extdata", "article_cases", "lung", package = "CohortContrast"),
  "malignant_neoplasm_of_lung_1year_after",
  "prostate",
  system.file("extdata", "article_cases", "prostate", package = "CohortContrast"),
  "malignant_neoplasm_of_prostate_1year_after"
)
```

## Build the target cohort

``` r

buildTargetTable <- function(cdm, jsonDir) {
  # Generate the target cohort from the JSON definition.
  targetTable <- CohortContrast::cohortFromJSON(
    pathToJSON = jsonDir,
    cdm = cdm
  ) %>%
    # Shift the cohort start slightly earlier and define a 1-year follow-up.
    dplyr::mutate(
      cohort_start_date = cohort_start_date - 14L,
      cohort_end_date = cohort_start_date + 365L
    )

  # Resolve any overlaps before continuing with the analysis.
  CohortContrast::resolveCohortTableOverlaps(
    cohortTable = targetTable,
    cdm = cdm
  )
}
```

## Build the visit-based control cohort

``` r

buildVisitBasedControl <- function(cdm, targetTable, lookbackDays = 1095L, followupDays = 365L) {
  # Shift the target windows backwards to define the control-search anchor.
  controlSeed <- targetTable %>%
    dplyr::mutate(
      cohort_start_date = cohort_start_date - as.integer(lookbackDays),
      cohort_end_date = cohort_start_date + as.integer(followupDays)
    )

  # Insert the shifted target windows as a temporary table used to retrieve the
  # latest eligible visit before the control anchor date.
  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "article_control_seed",
    table = controlSeed,
    overwrite = TRUE,
    temporary = TRUE
  )

  controlTable <- cdm$visit_occurrence %>%
    dplyr::inner_join(cdm$article_control_seed, by = c("person_id" = "subject_id")) %>%
    dplyr::filter(visit_start_date <= cohort_start_date) %>%
    dplyr::group_by(person_id) %>%
    # To reduce bias from differential encounter density, align each patient's
    # control window to the clinical visit start closest to the baseline anchor
    # date, operationalized here as the latest eligible visit on or before that
    # anchor date.
    dplyr::slice_max(order_by = visit_start_date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      subject_id = person_id,
      cohort_start_date = visit_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      cohort_definition_id = "control",
      cohort_end_date = .data$cohort_start_date + as.integer(followupDays)
    ) %>%
    dplyr::select(
      cohort_definition_id,
      subject_id,
      cohort_start_date,
      cohort_end_date
    )

  CohortContrast::resolveCohortTableOverlaps(
    cohortTable = controlTable,
    cdm = cdm
  )
}
```

## Keep only the final selected concepts

``` r

trimFinalStudy <- function(data) {
  # selectedFeatureData is the final selected concept set after statistical
  # filtering and optional post-processing.
  finalIds <- unique(data$selectedFeatureData$selectedFeatureIds)

  # Keep only the final selected concepts in the exported feature table.
  data$data_features <- data$data_features %>%
    dplyr::filter(.data$CONCEPT_ID %in% finalIds)

  # Keep only patient-level rows belonging to the final selected concepts.
  data$data_patients <- data$data_patients %>%
    dplyr::filter(.data$CONCEPT_ID %in% finalIds)

  # Keep only mapping rows connected to the final selected concepts.
  data$complementaryMappingTable <- data$complementaryMappingTable %>%
    dplyr::filter(
      .data$CONCEPT_ID %in% finalIds |
        .data$NEW_CONCEPT_ID %in% finalIds
    )

  # Keep the selected-feature payload aligned with the final study subset.
  data$selectedFeatureData$selectedFeatures <- data$selectedFeatureData$selectedFeatures %>%
    dplyr::filter(.data$CONCEPT_ID %in% finalIds)
  data$selectedFeatureData$selectedFeatureIds <- finalIds
  data$selectedFeatureData$selectedFeatureNames <- unique(
    data$selectedFeatureData$selectedFeatures$CONCEPT_NAME
  )
  data$trajectoryDataList <- data$selectedFeatureData

  data
}
```

## Run one final mapped study

``` r

runArticleCase <- function(cdm, jsonDir, studyName, summaryRoot, scratchRoot) {
  # Build the target and control cohorts for the selected case.
  targetTable <- buildTargetTable(cdm = cdm, jsonDir = jsonDir)
  controlTable <- buildVisitBasedControl(cdm = cdm, targetTable = targetTable)

  # Run the full CohortContrast workflow in memory first.
  # Output writing is delayed until the final selected/mapped study object has
  # been trimmed to the published concept subset.
  result <- CohortContrast::CohortContrast(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable,
    pathToResults = scratchRoot,
    prevalenceCutOff = 1,
    presenceFilter = 0.01,
    getSourceData = FALSE,
    runChi2YTests = TRUE,
    runLogitTests = TRUE,
    createOutputFiles = FALSE,
    complName = studyName,
    runRemoveTemporalBias = TRUE,
    removeTemporalBiasArgs = list(
      removeIdentified = TRUE
    ),
    runAutomaticHierarchyCombineConcepts = TRUE,
    automaticHierarchyCombineConceptsArgs = list(
      abstractionLevel = -1,
      minDepthAllowed = 0,
      allowOnlyMinors = TRUE
    ),
    runAutomaticCorrelationCombineConcepts = TRUE,
    automaticCorrelationCombineConceptsArgs = list(
      abstractionLevel = -1,
      minCorrelation = 0.7,
      maxDaysInBetween = 1,
      heritageDriftAllowed = FALSE
    )
  )

  # Keep only the concepts that survived the statistical and post-processing
  # pipeline, then write one temporary patient-mode study folder.
  result <- trimFinalStudy(result)
  CohortContrast:::saveResult(result, scratchRoot)

  # Create the summary-mode study from the temporary patient-mode study.
  CohortContrast::precomputeSummary(
    studyPath = file.path(scratchRoot, studyName),
    outputPath = file.path(summaryRoot, paste0(studyName, "_summary")),
    clusterKValues = c(2, 3, 4, 5)
  )

  # Remove the temporary patient-mode folder and keep only the summary.
  unlink(file.path(scratchRoot, studyName), recursive = TRUE, force = TRUE)

  invisible(result)
}
```

## Run all configured cases

``` r

# Directory that will keep the published summary-mode study folders.
summaryRoot <- "article_studies"

# Temporary directory used only while precomputeSummary() is running.
scratchRoot <- file.path(tempdir(), "cohortcontrast_article_scratch")

dir.create(summaryRoot, recursive = TRUE, showWarnings = FALSE)
dir.create(scratchRoot, recursive = TRUE, showWarnings = FALSE)

conn <- connectArticleCdm()
db <- conn$db
cdm <- conn$cdm

purrr::pwalk(
  cases,
  \(case_name, json_dir, study_name) {
    # Run one complete article workflow per case.
    runArticleCase(
      cdm = cdm,
      jsonDir = json_dir,
      studyName = study_name,
      summaryRoot = summaryRoot,
      scratchRoot = scratchRoot
    )
  }
)

# Close the database connection when finished.
DBI::dbDisconnect(db)
```

## Resulting study folders

After the workflow completes, the output directory will contain:

- `<study_name>_summary/` for the corresponding summary-mode study

Temporary patient-mode study folders are created only while
[`precomputeSummary()`](https://healthinformaticsut.github.io/CohortContrast/reference/precomputeSummary.md)
is running and are removed afterward.

The retained summary-mode study folders therefore contain the final
published outputs.

For the two configured cases, that means:

- `malignant_neoplasm_of_lung_1year_after_summary/`
- `malignant_neoplasm_of_prostate_1year_after_summary/`

These summary folders can then be opened directly in the Viewer.
