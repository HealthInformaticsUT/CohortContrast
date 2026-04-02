# Interpreting Results with lc500

## Goal

This vignette documents the information stored in each output object of
a `CohortContrastObject`, using the bundled generated `lc500` study.

It is written as a practical data dictionary:

- What each object contains
- What each column means
- How to interpret values in the `lc500` example

``` r

isMissingOrEmpty <- function(x) {
  length(x) == 0 || is.na(x[1]) || !nzchar(x[1])
}

readParquetIfExists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  as.data.frame(nanoparquet::read_parquet(path), stringsAsFactors = FALSE)
}

deserializeTimeColumn <- function(df) {
  if (!is.data.frame(df) || !("TIME_TO_EVENT" %in% colnames(df))) {
    return(df)
  }
  if (!is.character(df$TIME_TO_EVENT)) {
    return(df)
  }
  df$TIME_TO_EVENT <- lapply(df$TIME_TO_EVENT, function(x) {
    if (is.null(x) || (length(x) == 1 && is.na(x)) || !nzchar(x)) {
      return(numeric(0))
    }
    parsed <- tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
    if (is.null(parsed)) x else parsed
  })
  df
}

loadStudyFallback <- function(root, studyName) {
  studyPathLocal <- file.path(root, studyName)
  dataPatients <- deserializeTimeColumn(readParquetIfExists(file.path(studyPathLocal, "data_patients.parquet")))
  dataFeatures <- deserializeTimeColumn(readParquetIfExists(file.path(studyPathLocal, "data_features.parquet")))
  dataInitial <- readParquetIfExists(file.path(studyPathLocal, "data_initial.parquet"))
  dataPerson <- readParquetIfExists(file.path(studyPathLocal, "data_person.parquet"))
  mapping <- readParquetIfExists(file.path(studyPathLocal, "complementaryMappingTable.parquet"))
  if (!is.data.frame(mapping)) {
    mapping <- data.frame()
  }

  metadataPath <- file.path(studyPathLocal, "metadata.json")
  metadata <- if (file.exists(metadataPath)) jsonlite::fromJSON(metadataPath, simplifyVector = TRUE) else NULL

  selectedFeatures <- readParquetIfExists(file.path(studyPathLocal, "selected_features.parquet"))
  if (!is.data.frame(selectedFeatures)) {
    selectedFeatures <- dataFeatures
  }

  selectedFeatureData <- list(
    selectedFeatureNames = if (is.data.frame(selectedFeatures) && "CONCEPT_NAME" %in% colnames(selectedFeatures)) unique(selectedFeatures$CONCEPT_NAME) else character(0),
    selectedFeatureIds = if (is.data.frame(selectedFeatures) && "CONCEPT_ID" %in% colnames(selectedFeatures)) selectedFeatures$CONCEPT_ID else numeric(0),
    selectedFeatures = if (is.data.frame(selectedFeatures)) selectedFeatures else data.frame()
  )

  conceptAncestor <- readParquetIfExists(file.path(studyPathLocal, "concepts_concept_ancestor.parquet"))
  concept <- readParquetIfExists(file.path(studyPathLocal, "concepts_concept.parquet"))

  obj <- list(
    data_patients = if (is.data.frame(dataPatients)) dataPatients else data.frame(),
    data_initial = if (is.data.frame(dataInitial)) dataInitial else data.frame(),
    data_person = if (is.data.frame(dataPerson)) dataPerson else data.frame(),
    data_features = if (is.data.frame(dataFeatures)) dataFeatures else data.frame(),
    conceptsData = list(concept_ancestor = conceptAncestor, concept = concept),
    complementaryMappingTable = mapping,
    selectedFeatureData = selectedFeatureData,
    trajectoryDataList = selectedFeatureData,
    config = list(complName = studyName, metadata = metadata)
  )
  class(obj) <- "CohortContrastObject"
  obj
}

exampleRoot <- system.file("example", "st", package = "CohortContrast")
if (isMissingOrEmpty(exampleRoot) && dir.exists("inst/example/st")) {
  exampleRoot <- normalizePath("inst/example/st")
}
studyPath <- file.path(exampleRoot, "lc500")

if (isMissingOrEmpty(exampleRoot) || !dir.exists(studyPath)) {
  cat("Bundled example study 'lc500' is not available in this build.\n")
  knitr::knit_exit()
}

data <- tryCatch(
  CohortContrast::loadCohortContrastStudy(
    studyName = "lc500",
    pathToResults = exampleRoot
  ),
  error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("topKInt", msg, fixed = TRUE) || grepl("missing value where TRUE/FALSE needed", msg, fixed = TRUE)) {
      loadStudyFallback(exampleRoot, "lc500")
    } else {
      stop(e)
    }
  }
)
```

## Object Inventory

The loaded object contains:

``` r

names(data)
#> [1] "data_patients"             "data_initial"             
#> [3] "data_person"               "data_features"            
#> [5] "conceptsData"              "complementaryMappingTable"
#> [7] "selectedFeatureData"       "trajectoryDataList"       
#> [9] "config"
```

### Shape Summary

``` r

objectSummary <- data.frame(
  component = c(
    "data_initial",
    "data_person",
    "data_features",
    "data_patients",
    "complementaryMappingTable",
    "selectedFeatureData$selectedFeatures"
  ),
  class = c(
    class(data$data_initial)[1],
    class(data$data_person)[1],
    class(data$data_features)[1],
    class(data$data_patients)[1],
    class(data$complementaryMappingTable)[1],
    class(data$selectedFeatureData$selectedFeatures)[1]
  ),
  nrow = c(
    nrow(data$data_initial),
    nrow(data$data_person),
    nrow(data$data_features),
    nrow(data$data_patients),
    nrow(data$complementaryMappingTable),
    if (is.data.frame(data$selectedFeatureData$selectedFeatures)) nrow(data$selectedFeatureData$selectedFeatures) else 0L
  ),
  ncol = c(
    ncol(data$data_initial),
    ncol(data$data_person),
    ncol(data$data_features),
    ncol(data$data_patients),
    ncol(data$complementaryMappingTable),
    if (is.data.frame(data$selectedFeatureData$selectedFeatures)) ncol(data$selectedFeatureData$selectedFeatures) else 0L
  ),
  stringsAsFactors = FALSE
)
objectSummary
#>                              component      class nrow ncol
#> 1                         data_initial data.frame 1000    4
#> 2                          data_person data.frame 1000    3
#> 3                        data_features data.frame   16   14
#> 4                        data_patients data.frame 4656    8
#> 5            complementaryMappingTable data.frame    0    6
#> 6 selectedFeatureData$selectedFeatures data.frame   16   14
```

### Helper for Column Dictionaries

``` r

buildColumnDictionary <- function(df, descriptionMap, exampleMap = NULL) {
  cols <- colnames(df)
  out <- data.frame(
    column = cols,
    type = vapply(df, function(x) paste(class(x), collapse = "|"), character(1)),
    description = unname(descriptionMap[cols]),
    stringsAsFactors = FALSE
  )
  if (!is.null(exampleMap)) {
    out$lc500_example <- unname(exampleMap[cols])
  }
  out$description[is.na(out$description)] <- "Not documented in this vignette."
  if (!is.null(exampleMap)) {
    out$lc500_example[is.na(out$lc500_example)] <- ""
  }
  out
}
```

## `data_initial`: Cohort Membership Table

### Interpretation

Row grain: one subject-level cohort episode (in `lc500`, one row per
subject).

Purpose:

- Defines target vs control assignment
- Defines the study time window used for downstream feature extraction

``` r

initialDescriptions <- c(
  COHORT_DEFINITION_ID = "Cohort label used throughout the package (typically 'target' or 'control').",
  SUBJECT_ID = "Subject identifier in the cohort table.",
  COHORT_START_DATE = "Observation start date for the cohort episode.",
  COHORT_END_DATE = "Observation end date for the cohort episode."
)

initialExamples <- c(
  COHORT_DEFINITION_ID = paste("Values:", paste(unique(data$data_initial$COHORT_DEFINITION_ID), collapse = ", ")),
  SUBJECT_ID = paste("Unique subjects:", dplyr::n_distinct(data$data_initial$SUBJECT_ID)),
  COHORT_START_DATE = paste("Range:", paste(range(data$data_initial$COHORT_START_DATE), collapse = " to ")),
  COHORT_END_DATE = paste("Range:", paste(range(data$data_initial$COHORT_END_DATE), collapse = " to "))
)

buildColumnDictionary(data$data_initial, initialDescriptions, initialExamples)
#>                                    column      type
#> COHORT_DEFINITION_ID COHORT_DEFINITION_ID character
#> SUBJECT_ID                     SUBJECT_ID   numeric
#> COHORT_START_DATE       COHORT_START_DATE      Date
#> COHORT_END_DATE           COHORT_END_DATE      Date
#>                                                                                      description
#> COHORT_DEFINITION_ID Cohort label used throughout the package (typically 'target' or 'control').
#> SUBJECT_ID                                               Subject identifier in the cohort table.
#> COHORT_START_DATE                                 Observation start date for the cohort episode.
#> COHORT_END_DATE                                     Observation end date for the cohort episode.
#>                                        lc500_example
#> COHORT_DEFINITION_ID         Values: target, control
#> SUBJECT_ID                     Unique subjects: 1000
#> COHORT_START_DATE    Range: 2019-01-01 to 2020-12-29
#> COHORT_END_DATE      Range: 2020-01-01 to 2021-12-29
```

### Example Rows

``` r

utils::head(data$data_initial, 10)
#>    COHORT_DEFINITION_ID SUBJECT_ID COHORT_START_DATE COHORT_END_DATE
#> 1                target     100001        2019-06-14      2020-06-13
#> 2                target     100002        2020-10-05      2021-10-05
#> 3                target     100003        2020-04-09      2021-04-09
#> 4                target     100004        2019-03-24      2020-03-23
#> 5                target     100005        2020-03-28      2021-03-28
#> 6                target     100006        2020-11-15      2021-11-15
#> 7                target     100007        2019-07-30      2020-07-29
#> 8                target     100008        2019-07-22      2020-07-21
#> 9                target     100009        2019-07-19      2020-07-18
#> 10               target     100010        2020-07-11      2021-07-11
```

## `data_person`: Demographic Table

### Interpretation

Row grain: one person.

Purpose:

- Stores demographics used in interpretation and viewer panels.

``` r

personDescriptions <- c(
  PERSON_ID = "Person identifier. This links to SUBJECT_ID/PERSON_ID usage in other tables.",
  GENDER_CONCEPT_ID = "OMOP concept identifier for gender.",
  YEAR_OF_BIRTH = "Birth year used in age-related summaries."
)

personExamples <- c(
  PERSON_ID = paste("Unique persons:", dplyr::n_distinct(data$data_person$PERSON_ID)),
  GENDER_CONCEPT_ID = paste("Values:", paste(sort(unique(data$data_person$GENDER_CONCEPT_ID)), collapse = ", ")),
  YEAR_OF_BIRTH = paste("Range:", paste(range(data$data_person$YEAR_OF_BIRTH), collapse = " to "))
)

buildColumnDictionary(data$data_person, personDescriptions, personExamples)
#>                              column    type
#> PERSON_ID                 PERSON_ID numeric
#> GENDER_CONCEPT_ID GENDER_CONCEPT_ID numeric
#> YEAR_OF_BIRTH         YEAR_OF_BIRTH numeric
#>                                                                                    description
#> PERSON_ID         Person identifier. This links to SUBJECT_ID/PERSON_ID usage in other tables.
#> GENDER_CONCEPT_ID                                          OMOP concept identifier for gender.
#> YEAR_OF_BIRTH                                        Birth year used in age-related summaries.
#>                          lc500_example
#> PERSON_ID         Unique persons: 1000
#> GENDER_CONCEPT_ID   Values: 8507, 8532
#> YEAR_OF_BIRTH      Range: 1931 to 1972
```

### Example Rows

``` r

utils::head(data$data_person, 10)
#>    PERSON_ID GENDER_CONCEPT_ID YEAR_OF_BIRTH
#> 1     100001              8507          1958
#> 2     100002              8507          1967
#> 3     100003              8507          1967
#> 4     100004              8507          1962
#> 5     100005              8532          1955
#> 6     100006              8507          1963
#> 7     100007              8507          1957
#> 8     100008              8507          1968
#> 9     100009              8532          1955
#> 10    100010              8507          1971
```

## `data_features`: Concept-Level Feature Summary

### Interpretation

Row grain: one concept at one abstraction level.

Purpose:

- Core statistical summary table for concept enrichment
- Feeds ranking, significance flags, and viewer concept-level displays

``` r

featureDescriptions <- c(
  CONCEPT_ID = "OMOP concept identifier (or mapped concept id after merges).",
  CONCEPT_NAME = "Human-readable concept label.",
  ABSTRACTION_LEVEL = "Concept abstraction level. -1 indicates cdm abstraction level concepts. Other abstraction levels are optional.",
  TARGET_SUBJECT_COUNT = "Number of target subjects with the concept present.",
  CONTROL_SUBJECT_COUNT = "Number of control subjects with the concept present.",
  TIME_TO_EVENT = "List-column of event times (days from cohort start) pooled for the concept.",
  TARGET_SUBJECT_PREVALENCE = "Target prevalence proportion (TARGET_SUBJECT_COUNT / n_target_subjects).",
  CONTROL_SUBJECT_PREVALENCE = "Control prevalence proportion (CONTROL_SUBJECT_COUNT / n_control_subjects).",
  PREVALENCE_DIFFERENCE_RATIO = "Target/control prevalence ratio used for ranking and filtering.",
  CHI2Y = "Boolean significance flag from chi-squared two-proportion test with Yates correction.",
  CHI2Y_P_VALUE = "P-value from the chi-squared test used for CHI2Y.",
  LOGITTEST = "Boolean significance flag from logistic regression test.",
  LOGITTEST_P_VALUE = "P-value from logistic regression test.",
  HERITAGE = "Source domain (heritage), such as condition_occurrence or visit_detail."
)

featureExamples <- c(
  CONCEPT_ID = paste("Unique concepts:", dplyr::n_distinct(data$data_features$CONCEPT_ID)),
  CONCEPT_NAME = paste("Rows:", nrow(data$data_features)),
  ABSTRACTION_LEVEL = paste("Values:", paste(sort(unique(data$data_features$ABSTRACTION_LEVEL)), collapse = ", ")),
  TARGET_SUBJECT_COUNT = paste("Range:", paste(range(data$data_features$TARGET_SUBJECT_COUNT), collapse = " to ")),
  CONTROL_SUBJECT_COUNT = paste("Range:", paste(range(data$data_features$CONTROL_SUBJECT_COUNT), collapse = " to ")),
  TIME_TO_EVENT = paste("List column; first concept has", length(data$data_features$TIME_TO_EVENT[[1]]), "time points."),
  TARGET_SUBJECT_PREVALENCE = paste("Range:", paste(range(data$data_features$TARGET_SUBJECT_PREVALENCE), collapse = " to ")),
  CONTROL_SUBJECT_PREVALENCE = paste("Range:", paste(range(data$data_features$CONTROL_SUBJECT_PREVALENCE), collapse = " to ")),
  PREVALENCE_DIFFERENCE_RATIO = paste("Range:", paste(range(data$data_features$PREVALENCE_DIFFERENCE_RATIO), collapse = " to ")),
  CHI2Y = paste("TRUE count:", sum(as.logical(data$data_features$CHI2Y), na.rm = TRUE)),
  CHI2Y_P_VALUE = paste("Range:", paste(range(data$data_features$CHI2Y_P_VALUE), collapse = " to ")),
  LOGITTEST = paste("TRUE count:", sum(as.logical(data$data_features$LOGITTEST), na.rm = TRUE)),
  LOGITTEST_P_VALUE = paste("Range:", paste(range(data$data_features$LOGITTEST_P_VALUE), collapse = " to ")),
  HERITAGE = paste("Values:", paste(sort(unique(data$data_features$HERITAGE)), collapse = ", "))
)

buildColumnDictionary(data$data_features, featureDescriptions, featureExamples)
#>                                                  column      type
#> CONCEPT_ID                                   CONCEPT_ID   numeric
#> CONCEPT_NAME                               CONCEPT_NAME character
#> ABSTRACTION_LEVEL                     ABSTRACTION_LEVEL   numeric
#> TARGET_SUBJECT_COUNT               TARGET_SUBJECT_COUNT   numeric
#> CONTROL_SUBJECT_COUNT             CONTROL_SUBJECT_COUNT   numeric
#> TIME_TO_EVENT                             TIME_TO_EVENT      list
#> TARGET_SUBJECT_PREVALENCE     TARGET_SUBJECT_PREVALENCE   numeric
#> CONTROL_SUBJECT_PREVALENCE   CONTROL_SUBJECT_PREVALENCE   numeric
#> PREVALENCE_DIFFERENCE_RATIO PREVALENCE_DIFFERENCE_RATIO   numeric
#> CHI2Y                                             CHI2Y   logical
#> CHI2Y_P_VALUE                             CHI2Y_P_VALUE   numeric
#> LOGITTEST                                     LOGITTEST   logical
#> LOGITTEST_P_VALUE                     LOGITTEST_P_VALUE   numeric
#> HERITAGE                                       HERITAGE character
#>                                                                                                                                description
#> CONCEPT_ID                                                                    OMOP concept identifier (or mapped concept id after merges).
#> CONCEPT_NAME                                                                                                 Human-readable concept label.
#> ABSTRACTION_LEVEL           Concept abstraction level. -1 indicates cdm abstraction level concepts. Other abstraction levels are optional.
#> TARGET_SUBJECT_COUNT                                                                   Number of target subjects with the concept present.
#> CONTROL_SUBJECT_COUNT                                                                 Number of control subjects with the concept present.
#> TIME_TO_EVENT                                                  List-column of event times (days from cohort start) pooled for the concept.
#> TARGET_SUBJECT_PREVALENCE                                         Target prevalence proportion (TARGET_SUBJECT_COUNT / n_target_subjects).
#> CONTROL_SUBJECT_PREVALENCE                                     Control prevalence proportion (CONTROL_SUBJECT_COUNT / n_control_subjects).
#> PREVALENCE_DIFFERENCE_RATIO                                                Target/control prevalence ratio used for ranking and filtering.
#> CHI2Y                                                Boolean significance flag from chi-squared two-proportion test with Yates correction.
#> CHI2Y_P_VALUE                                                                            P-value from the chi-squared test used for CHI2Y.
#> LOGITTEST                                                                         Boolean significance flag from logistic regression test.
#> LOGITTEST_P_VALUE                                                                                   P-value from logistic regression test.
#> HERITAGE                                                           Source domain (heritage), such as condition_occurrence or visit_detail.
#>                                                                                                                 lc500_example
#> CONCEPT_ID                                                                                                Unique concepts: 16
#> CONCEPT_NAME                                                                                                         Rows: 16
#> ABSTRACTION_LEVEL                                                                                                  Values: -1
#> TARGET_SUBJECT_COUNT                                                                                        Range: 147 to 483
#> CONTROL_SUBJECT_COUNT                                                                                         Range: 0 to 231
#> TIME_TO_EVENT                                                                 List column; first concept has 483 time points.
#> TARGET_SUBJECT_PREVALENCE                                                                               Range: 0.294 to 0.966
#> CONTROL_SUBJECT_PREVALENCE                                                                                  Range: 0 to 0.462
#> PREVALENCE_DIFFERENCE_RATIO                                                                    Range: 1.96969696969697 to 165
#> CHI2Y                                                                                                          TRUE count: 16
#> CHI2Y_P_VALUE                                                                                           Range: 0.001 to 0.001
#> LOGITTEST                                                                                                      TRUE count: 16
#> LOGITTEST_P_VALUE                                                                                       Range: 0.002 to 0.002
#> HERITAGE                    Values: condition_occurrence, death, measurement, observation, procedure_occurrence, visit_detail
```

### Example Rows

``` r

utils::head(data$data_features, 10)
#>    CONCEPT_ID               CONCEPT_NAME ABSTRACTION_LEVEL TARGET_SUBJECT_COUNT
#> 1      443388    Malignant tumor of lung                -1                  483
#> 2       32280                      Death                -1                  156
#> 3       32815          Death Certificate                -1                  147
#> 4     4182985 Diffusion capacity of lung                -1                  371
#> 5     4306655                      Death                -1                  150
#> 6     4014023            Palliative care                -1                  147
#> 7     4032404               Bronchoscopy                -1                  462
#> 8     4008226               Chemotherapy                -1                  164
#> 9     2107967                  Lobectomy                -1                  165
#> 10    2107968          Lobectomy of lung                -1                  168
#>    CONTROL_SUBJECT_COUNT
#> 1                      6
#> 2                      0
#> 3                      0
#> 4                     50
#> 5                      2
#> 6                      1
#> 7                     10
#> 8                      2
#> 9                      1
#> 10                     3
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                TIME_TO_EVENT
#> 1                                                                                                                      4, 1, 4, 2, 6, 0, 6, 5, 5, 3, 5, 5, 8, 7, 6, 8, 4, 0, 10, 2, 7, 0, 7, 5, 2, 1, 9, 3, 4, 3, 3, 14, 2, 6, 2, 3, 6, 0, 8, 10, 5, 14, 0, 6, 5, 11, 6, 7, 6, 8, 4, 7, 0, 5, 7, 9, 8, 9, 0, 6, 8, 6, 4, 3, 16, 11, 4, 4, 5, 11, 3, 6, 6, 5, 8, 0, 2, 1, 4, 4, 5, 3, 5, 9, 10, 4, 6, 6, 10, 3, 4, 0, 5, 4, 0, 8, 5, 9, 5, 14, 0, 0, 4, 10, 5, 4, 13, 3, 1, 5, 2, 3, 0, 4, 11, 7, 0, 0, 13, 6, 4, 8, 6, 8, 9, 5, 0, 2, 4, 3, 13, 1, 5, 7, 3, 9, 5, 0, 3, 10, 3, 7, 8, 8, 6, 1, 3, 10, 8, 8, 4, 0, 15, 9, 0, 5, 4, 11, 7, 8, 6, 4, 1, 12, 0, 1, 3, 1, 12, 0, 6, 5, 11, 2, 0, 6, 2, 0, 6, 5, 6, 2, 11, 9, 1, 8, 3, 3, 5, 12, 3, 1, 6, 10, 8, 10, 4, 8, 3, 9, 11, 5, 3, 7, 7, 8, 2, 5, 10, 7, 4, 1, 5, 9, 6, 4, 2, 2, 13, 9, 16, 1, 2, 7, 3, 4, 3, 10, 2, 6, 7, 7, 6, 6, 3, 6, 4, 0, 6, 5, 10, 2, 9, 7, 3, 0, 6, 4, 10, 2, 5, 8, 1, 0, 2, 6, 5, 6, 3, 9, 5, 10, 11, 6, 6, 8, 0, 1, 8, 0, 0, 1, 1, 0, 2, 3, 5, 1, 4, 5, 6, 6, 9, 5, 1, 0, 0, 5, 11, 0, 7, 4, 2, 0, 5, 5, 11, 0, 3, 3, 2, 5, 7, 5, 7, 1, 6, 5, 3, 4, 6, 1, 10, 1, 4, 7, 3, 16, 4, 5, 0, 5, 5, 6, 3, 2, 8, 7, 3, 3, 0, 7, 10, 8, 3, 0, 7, 5, 8, 3, 3, 6, 5, 4, 6, 12, 6, 5, 3, 11, 7, 9, 14, 4, 0, 10, 1, 6, 11, 10, 2, 6, 2, 0, 7, 2, 0, 2, 1, 1, 5, 2, 3, 2, 7, 4, 6, 0, 7, 4, 10, 8, 5, 6, 1, 0, 18, 2, 2, 0, 2, 1, 10, 2, 9, 2, 4, 5, 5, 0, 0, 4, 4, 2, 13, 4, 13, 5, 6, 8, 3, 4, 7, 7, 1, 11, 13, 10, 3, 6, 1, 8, 2, 7, 8, 1, 3, 5, 4, 10, 2, 6, 0, 4, 2, 8, 1, 7, 0, 3, 6, 0, 10, 1, 8, 3, 7, 4, 1, 4, 4, 9, 1, 5, 3, 7, 9, 7, 6, 2, 7, 11, 3, 0, 7, 10, 1, 4, 13, 0, 9, 4, 2, 5, 4, 7, 13, 0, 0, 11, 2, 0, 2
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  11, 244, 150, 165, 204, 148, 224, 191, 199, 214, 170, 212, 213, 200, 206, 190, 259, 192, 193, 232, 187, 191, 219, 180, 193, 157, 178, 158, 175, 175, 214, 183, 189, 201, 202, 194, 185, 166, 180, 212, 171, 171, 198, 193, 214, 224, 170, 168, 145, 187, 171, 213, 224, 203, 171, 209, 212, 195, 205, 270, 216, 213, 215, 183, 167, 178, 192, 152, 219, 177, 175, 190, 192, 193, 193, 173, 185, 250, 177, 160, 183, 173, 204, 174, 194, 189, 192, 186, 264, 181, 123, 176, 193, 260, 208, 226, 224, 211, 202, 243, 214, 187, 201, 217, 206, 164, 272, 189, 224, 167, 208, 216, 156, 151, 156, 199, 205, 175, 192, 170, 169, 224, 126, 176, 190, 187, 196, 173, 175, 209, 174, 150, 198, 232, 196, 178, 172, 219, 199, 243, 235, 190, 187, 217, 230, 173, 185, 212, 216, 203, 223, 211, 192, 168, 206, 200
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 19, 90, 27, 220, 225, 267, 272, 244, 262, 299, 258, 245, 250, 247, 235, 237, 257, 207, 235, 224, 253, 254, 225, 248, 251, 235, 261, 253, 245, 230, 276, 230, 240, 237, 255, 256, 245, 260, 200, 224, 241, 240, 280, 113, 240, 192, 220, 227, 279, 255, 253, 242, 258, 234, 249, 198, 235, 263, 234, 259, 247, 260, 265, 226, 258, 214, 247, 273, 276, 248, 229, 227, 258, 235, 262, 253, 249, 279, 220, 248, 213, 292, 267, 247, 229, 249, 257, 255, 230, 231, 262, 226, 233, 268, 252, 244, 264, 211, 263, 249, 231, 243, 254, 267, 232, 250, 242, 271, 257, 217, 266, 251, 213, 256, 243, 286, 256, 281, 249, 264, 262, 226, 248, 262, 243, 254, 281, 235, 223, 276, 230, 293, 261, 245, 222, 220, 255, 237, 233, 268, 244, 265, 269, 235, 251, 255, 253
#> 4                                                                                                                                                                                                                                11, 18, 23, 23, 2, 13, 19, 48, 23, 21, 0, 28, 14, 17, 0, 42, 9, 18, 0, 28, 13, 0, 6, 27, 12, 16, 24, 21, 34, 29, 16, 28, 25, 18, 18, 20, 20, 21, 27, 30, 23, 17, 1, 30, 13, 9, 13, 16, 15, 23, 6, 40, 21, 24, 22, 10, 12, 37, 2, 18, 27, 15, 24, 26, 7, 10, 19, 19, 16, 26, 0, 7, 10, 12, 24, 9, 11, 13, 20, 24, 0, 33, 7, 2, 0, 32, 2, 8, 24, 14, 34, 14, 33, 4, 3, 22, 25, 9, 30, 40, 35, 20, 6, 11, 36, 18, 33, 22, 2, 14, 15, 24, 16, 24, 0, 16, 11, 7, 7, 27, 9, 11, 28, 20, 15, 22, 17, 8, 33, 18, 15, 3, 11, 9, 28, 13, 25, 29, 9, 20, 28, 18, 2, 7, 15, 19, 29, 22, 25, 8, 34, 15, 7, 23, 23, 23, 9, 20, 21, 23, 7, 14, 28, 14, 26, 16, 32, 19, 0, 18, 30, 1, 7, 26, 28, 2, 33, 24, 23, 0, 6, 34, 8, 29, 18, 26, 6, 21, 17, 12, 11, 39, 12, 9, 17, 10, 37, 28, 24, 28, 32, 9, 26, 13, 18, 15, 28, 18, 29, 15, 40, 22, 3, 12, 37, 32, 7, 0, 20, 10, 38, 0, 29, 25, 9, 29, 27, 25, 34, 31, 31, 26, 22, 1, 14, 15, 16, 13, 18, 21, 0, 13, 9, 23, 15, 27, 19, 28, 5, 14, 5, 37, 8, 36, 16, 0, 26, 31, 43, 33, 20, 0, 52, 30, 32, 25, 0, 13, 10, 23, 1, 40, 5, 20, 18, 12, 14, 18, 17, 18, 28, 16, 32, 24, 3, 15, 22, 23, 20, 16, 0, 23, 24, 2, 31, 56, 23, 24, 7, 18, 18, 0, 38, 0, 11, 21, 9, 25, 28, 42, 18, 3, 23, 25, 32, 4, 23, 22, 30, 20, 11, 21, 0, 9, 27, 16, 5, 39, 26, 21, 38, 16, 11, 48, 16, 16, 22, 45, 19, 11, 29, 0, 22, 23, 19, 31, 12, 18, 25, 19, 31, 24, 34, 21, 27, 19, 29, 35, 33, 19, 30, 12, 13, 11, 24, 33, 33, 29, 13, 29, 29
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  69, 53, 100, 89, 225, 216, 206, 192, 182, 171, 172, 208, 116, 177, 165, 217, 168, 221, 192, 199, 213, 189, 262, 178, 175, 208, 191, 189, 163, 132, 212, 226, 201, 232, 161, 187, 174, 139, 195, 181, 157, 152, 240, 177, 156, 210, 187, 213, 144, 212, 198, 177, 191, 212, 187, 189, 199, 218, 144, 226, 140, 139, 206, 167, 214, 224, 194, 221, 196, 223, 209, 224, 192, 195, 228, 170, 228, 205, 178, 217, 172, 199, 191, 212, 227, 209, 195, 209, 201, 214, 171, 232, 177, 181, 193, 163, 203, 177, 209, 177, 237, 164, 198, 167, 255, 213, 171, 244, 197, 236, 171, 206, 220, 217, 181, 201, 176, 193, 228, 170, 148, 197, 217, 180, 188, 200, 167, 234, 181, 234, 230, 189, 255, 151, 193, 154, 209, 244, 209, 203, 194, 230, 159, 281, 170, 300, 206, 200, 158, 182
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 121, 39, 63, 209, 224, 283, 221, 201, 165, 180, 192, 226, 232, 217, 183, 152, 229, 166, 233, 226, 216, 234, 215, 250, 202, 266, 223, 261, 175, 268, 183, 184, 244, 186, 242, 235, 199, 227, 159, 226, 265, 234, 217, 214, 276, 241, 203, 209, 197, 202, 257, 164, 267, 300, 136, 186, 168, 173, 250, 237, 201, 166, 269, 217, 228, 202, 198, 212, 228, 224, 166, 180, 187, 199, 220, 185, 229, 236, 206, 220, 168, 204, 222, 187, 209, 165, 204, 242, 190, 162, 250, 227, 190, 210, 248, 176, 209, 262, 193, 170, 243, 162, 75, 227, 219, 190, 207, 195, 193, 189, 220, 217, 222, 219, 219, 194, 226, 224, 190, 213, 193, 169, 195, 216, 189, 206, 226, 194, 176, 192, 199, 234, 252, 217, 173, 214, 255, 171, 263, 251, 203, 172, 208, 196, 161, 214, 226
#> 7  0, 3, 5, 7, 7, 16, 7, 29, 0, 7, 10, 5, 4, 2, 8, 12, 4, 16, 12, 17, 10, 16, 0, 7, 14, 8, 12, 11, 18, 10, 3, 2, 13, 15, 6, 21, 10, 23, 15, 13, 5, 14, 20, 10, 10, 10, 5, 0, 11, 2, 9, 0, 8, 11, 11, 25, 8, 3, 0, 0, 4, 0, 16, 16, 0, 21, 21, 12, 0, 16, 1, 11, 12, 17, 8, 16, 7, 13, 9, 12, 4, 19, 3, 16, 17, 7, 15, 8, 10, 13, 12, 12, 16, 12, 9, 6, 9, 17, 7, 19, 11, 17, 7, 11, 10, 18, 10, 9, 0, 19, 8, 6, 18, 0, 11, 9, 3, 8, 31, 0, 0, 17, 16, 0, 9, 4, 12, 20, 0, 1, 6, 9, 13, 12, 0, 11, 17, 2, 14, 0, 7, 16, 19, 6, 7, 4, 1, 16, 10, 2, 25, 0, 3, 1, 11, 14, 0, 13, 11, 9, 14, 9, 10, 5, 0, 18, 18, 8, 31, 7, 11, 10, 8, 13, 10, 1, 0, 0, 14, 17, 16, 8, 8, 10, 0, 4, 17, 5, 19, 9, 11, 5, 2, 11, 22, 16, 15, 11, 5, 10, 7, 0, 13, 4, 15, 0, 8, 24, 12, 7, 15, 0, 7, 13, 17, 0, 18, 20, 2, 13, 14, 14, 5, 8, 0, 4, 12, 13, 12, 0, 10, 4, 15, 17, 19, 14, 10, 2, 0, 9, 1, 10, 9, 15, 11, 24, 5, 0, 16, 10, 13, 3, 0, 9, 22, 12, 10, 2, 22, 6, 5, 9, 16, 13, 3, 6, 12, 20, 9, 16, 11, 0, 8, 30, 11, 30, 15, 6, 7, 13, 12, 19, 27, 7, 0, 11, 10, 11, 7, 5, 7, 28, 0, 9, 13, 18, 11, 16, 6, 28, 24, 14, 4, 16, 9, 9, 18, 11, 0, 11, 7, 13, 23, 13, 8, 4, 0, 0, 10, 4, 5, 13, 5, 6, 21, 0, 12, 9, 17, 12, 11, 13, 15, 7, 3, 14, 25, 2, 23, 9, 14, 18, 15, 17, 9, 6, 7, 0, 18, 17, 6, 9, 24, 10, 3, 14, 10, 6, 0, 10, 14, 8, 12, 16, 16, 0, 16, 6, 22, 5, 7, 7, 3, 18, 4, 18, 11, 7, 4, 12, 10, 15, 5, 11, 22, 1, 25, 3, 8, 17, 8, 23, 9, 11, 15, 9, 22, 14, 16, 11, 27, 19, 13, 4, 9, 13, 6, 8, 9, 21, 3, 16, 11, 23, 7, 12, 21, 7, 23, 6, 13, 14, 18, 11, 1, 9, 11, 7, 11, 7, 10, 20, 5, 8, 5, 7, 14, 14, 7, 11, 13, 7, 5, 2, 0, 15, 5, 9, 9, 9, 8, 11, 8, 5, 4, 23, 10, 0, 6, 1, 3, 8
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      24, 27, 6, 32, 20, 17, 18, 11, 0, 21, 25, 19, 26, 10, 9, 22, 13, 10, 13, 0, 20, 0, 29, 29, 0, 16, 24, 26, 16, 18, 18, 18, 21, 16, 20, 25, 18, 23, 17, 21, 22, 14, 21, 16, 33, 25, 12, 12, 15, 29, 28, 23, 13, 24, 27, 16, 22, 23, 29, 28, 25, 23, 21, 8, 16, 30, 13, 11, 20, 22, 24, 9, 0, 18, 18, 28, 7, 5, 16, 16, 5, 12, 25, 27, 15, 17, 0, 16, 26, 36, 13, 26, 23, 22, 16, 25, 6, 25, 13, 8, 23, 23, 14, 26, 0, 2, 30, 25, 0, 15, 48, 6, 16, 41, 0, 11, 18, 29, 16, 20, 28, 20, 11, 31, 3, 18, 22, 7, 12, 21, 32, 10, 6, 22, 27, 16, 18, 21, 29, 13, 42, 3, 25, 9, 22, 15, 27, 28, 0, 12, 11, 21, 17, 20, 36, 13, 29, 23, 11, 21, 21, 15, 151, 28
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  113, 80, 120, 22, 22, 31, 19, 20, 22, 19, 20, 14, 17, 27, 6, 33, 6, 30, 28, 34, 11, 25, 0, 14, 11, 27, 18, 29, 17, 8, 24, 26, 14, 33, 21, 18, 27, 17, 14, 14, 15, 9, 13, 17, 26, 28, 26, 24, 18, 12, 14, 23, 12, 17, 18, 14, 17, 18, 19, 25, 22, 21, 17, 23, 20, 17, 16, 36, 30, 27, 17, 38, 16, 15, 25, 27, 22, 7, 23, 24, 17, 10, 30, 11, 25, 8, 23, 16, 19, 29, 22, 19, 20, 18, 32, 12, 26, 21, 6, 16, 18, 15, 23, 20, 18, 28, 20, 25, 35, 30, 13, 23, 30, 15, 26, 12, 12, 14, 25, 20, 16, 25, 12, 1, 16, 11, 26, 36, 22, 26, 21, 19, 18, 19, 21, 21, 18, 23, 12, 18, 1, 6, 17, 29, 17, 27, 21, 10, 26, 16, 24, 15, 26, 18, 32, 15, 29, 13, 16, 23, 92, 28, 30, 35, 75
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           104, 65, 47, 121, 8, 25, 11, 19, 12, 28, 11, 24, 21, 14, 15, 16, 6, 44, 31, 28, 10, 20, 13, 16, 7, 16, 15, 12, 14, 20, 8, 18, 22, 23, 25, 13, 24, 15, 16, 11, 19, 5, 25, 33, 12, 11, 82, 20, 15, 20, 20, 29, 9, 14, 26, 18, 13, 11, 25, 24, 13, 16, 25, 14, 24, 16, 26, 17, 30, 8, 18, 2, 4, 19, 23, 15, 8, 26, 18, 31, 24, 11, 18, 0, 22, 17, 19, 21, 17, 20, 16, 23, 29, 30, 17, 21, 15, 19, 17, 32, 19, 17, 13, 13, 16, 38, 12, 9, 15, 10, 17, 22, 32, 28, 26, 24, 27, 13, 22, 16, 8, 10, 19, 18, 17, 22, 20, 15, 29, 13, 21, 16, 16, 7, 26, 24, 27, 7, 11, 24, 16, 14, 23, 16, 21, 13, 15, 11, 25, 9, 29, 22, 13, 23, 12, 18, 22, 24, 22, 28, 6, 13, 3, 25, 46, 59, 116, 19
#>    TARGET_SUBJECT_PREVALENCE CONTROL_SUBJECT_PREVALENCE
#> 1                      0.966                      0.012
#> 2                      0.312                      0.000
#> 3                      0.294                      0.000
#> 4                      0.742                      0.100
#> 5                      0.300                      0.004
#> 6                      0.294                      0.002
#> 7                      0.924                      0.020
#> 8                      0.328                      0.004
#> 9                      0.330                      0.002
#> 10                     0.336                      0.006
#>    PREVALENCE_DIFFERENCE_RATIO CHI2Y CHI2Y_P_VALUE LOGITTEST LOGITTEST_P_VALUE
#> 1                        80.50  TRUE         0.001      TRUE             0.002
#> 2                       100.00  TRUE         0.001      TRUE             0.002
#> 3                       100.00  TRUE         0.001      TRUE             0.002
#> 4                         7.42  TRUE         0.001      TRUE             0.002
#> 5                        75.00  TRUE         0.001      TRUE             0.002
#> 6                       147.00  TRUE         0.001      TRUE             0.002
#> 7                        46.20  TRUE         0.001      TRUE             0.002
#> 8                        82.00  TRUE         0.001      TRUE             0.002
#> 9                       165.00  TRUE         0.001      TRUE             0.002
#> 10                       56.00  TRUE         0.001      TRUE             0.002
#>                HERITAGE
#> 1  condition_occurrence
#> 2                 death
#> 3                 death
#> 4           measurement
#> 5           observation
#> 6           observation
#> 7  procedure_occurrence
#> 8  procedure_occurrence
#> 9  procedure_occurrence
#> 10 procedure_occurrence
```

## `data_patients`: Patient-Concept Event Table

### Interpretation

Row grain: one patient-concept-abstraction record.

Purpose:

- Retains patient-level event structure
- Supports overlap, trajectories, and correlation workflows

``` r

patientDescriptions <- c(
  COHORT_DEFINITION_ID = "Cohort membership for the record ('target'/'control').",
  PERSON_ID = "Patient identifier.",
  CONCEPT_ID = "Concept identifier observed for the patient.",
  CONCEPT_NAME = "Concept name observed for the patient.",
  HERITAGE = "Domain/heritage of the concept.",
  ABSTRACTION_LEVEL = "Abstraction level for the concept instance.",
  PREVALENCE = "Count of occurrences of this concept for the patient in the observation window.",
  TIME_TO_EVENT = "List-column of event times (days from cohort start) for this patient-concept."
)

patientExamples <- c(
  COHORT_DEFINITION_ID = paste("Values:", paste(unique(data$data_patients$COHORT_DEFINITION_ID), collapse = ", ")),
  PERSON_ID = paste("Unique patients represented:", dplyr::n_distinct(data$data_patients$PERSON_ID)),
  CONCEPT_ID = paste("Unique concepts:", dplyr::n_distinct(data$data_patients$CONCEPT_ID)),
  CONCEPT_NAME = paste("Rows:", nrow(data$data_patients)),
  HERITAGE = paste("Values:", paste(sort(unique(data$data_patients$HERITAGE)), collapse = ", ")),
  ABSTRACTION_LEVEL = paste("Values:", paste(sort(unique(data$data_patients$ABSTRACTION_LEVEL)), collapse = ", ")),
  PREVALENCE = paste("Range:", paste(range(data$data_patients$PREVALENCE), collapse = " to ")),
  TIME_TO_EVENT = paste("List column; first row times:", paste(data$data_patients$TIME_TO_EVENT[[1]], collapse = ", "))
)

buildColumnDictionary(data$data_patients, patientDescriptions, patientExamples)
#>                                    column      type
#> COHORT_DEFINITION_ID COHORT_DEFINITION_ID character
#> PERSON_ID                       PERSON_ID   numeric
#> CONCEPT_ID                     CONCEPT_ID   numeric
#> CONCEPT_NAME                 CONCEPT_NAME character
#> HERITAGE                         HERITAGE character
#> ABSTRACTION_LEVEL       ABSTRACTION_LEVEL   numeric
#> PREVALENCE                     PREVALENCE   numeric
#> TIME_TO_EVENT               TIME_TO_EVENT      list
#>                                                                                          description
#> COHORT_DEFINITION_ID                          Cohort membership for the record ('target'/'control').
#> PERSON_ID                                                                        Patient identifier.
#> CONCEPT_ID                                              Concept identifier observed for the patient.
#> CONCEPT_NAME                                                  Concept name observed for the patient.
#> HERITAGE                                                             Domain/heritage of the concept.
#> ABSTRACTION_LEVEL                                        Abstraction level for the concept instance.
#> PREVALENCE           Count of occurrences of this concept for the patient in the observation window.
#> TIME_TO_EVENT          List-column of event times (days from cohort start) for this patient-concept.
#>                                                                                                          lc500_example
#> COHORT_DEFINITION_ID                                                                           Values: target, control
#> PERSON_ID                                                                             Unique patients represented: 846
#> CONCEPT_ID                                                                                         Unique concepts: 16
#> CONCEPT_NAME                                                                                                Rows: 4656
#> HERITAGE             Values: condition_occurrence, death, measurement, observation, procedure_occurrence, visit_detail
#> ABSTRACTION_LEVEL                                                                                           Values: -1
#> PREVALENCE                                                                                               Range: 1 to 6
#> TIME_TO_EVENT                                                                          List column; first row times: 4
```

### Example Rows

``` r

utils::head(data$data_patients, 10)
#>    COHORT_DEFINITION_ID PERSON_ID CONCEPT_ID
#> 1                target    100001     443388
#> 2                target    100001       9201
#> 3                target    100001       9202
#> 4                target    100001    4008211
#> 5                target    100001    4008226
#> 6                target    100001    4176729
#> 7                target    100001    4032404
#> 8                target    100001    4167262
#> 9                target    100002     443388
#> 10               target    100002    4182985
#>                                              CONCEPT_NAME             HERITAGE
#> 1                                 Malignant tumor of lung condition_occurrence
#> 2                                         Inpatient Visit         visit_detail
#> 3                                        Outpatient Visit         visit_detail
#> 4                                            Radiotherapy procedure_occurrence
#> 5                                            Chemotherapy procedure_occurrence
#> 6  Treatment planning for external beam radiation therapy procedure_occurrence
#> 7                                            Bronchoscopy procedure_occurrence
#> 8                                   Needle biopsy of lung procedure_occurrence
#> 9                                 Malignant tumor of lung condition_occurrence
#> 10                             Diffusion capacity of lung          measurement
#>    ABSTRACTION_LEVEL PREVALENCE  TIME_TO_EVENT
#> 1                 -1          1              4
#> 2                 -1          2         32, 52
#> 3                 -1          3     34, 36, 64
#> 4                 -1          3     22, 25, 42
#> 5                 -1          4 24, 31, 33, 39
#> 6                 -1          1             34
#> 7                 -1          2          0, 15
#> 8                 -1          2          2, 18
#> 9                 -1          2           1, 6
#> 10                -1          2         11, 23
```

## `complementaryMappingTable`: Mapping History

### Interpretation

Row grain: one mapping action (original concept -\> new/merged concept).

Purpose:

- Stores manual and automatic merges
- Enables reproducibility of concept remapping

``` r

mappingDescriptions <- c(
  CONCEPT_ID = "Original/source concept id.",
  CONCEPT_NAME = "Original/source concept name.",
  NEW_CONCEPT_ID = "Mapped (merged) concept id.",
  NEW_CONCEPT_NAME = "Mapped (merged) concept name.",
  TYPE = "Mapping type (for example custom, hierarchy, correlation).",
  HERITAGE = "Heritage/domain context of mapping when available."
)

mappingExamples <- c(
  CONCEPT_ID = paste("Rows:", nrow(data$complementaryMappingTable)),
  CONCEPT_NAME = if (nrow(data$complementaryMappingTable) == 0) "No mappings in lc500 example." else "",
  NEW_CONCEPT_ID = "",
  NEW_CONCEPT_NAME = "",
  TYPE = "",
  HERITAGE = ""
)

buildColumnDictionary(data$complementaryMappingTable, mappingDescriptions, mappingExamples)
#>                            column    type
#> CONCEPT_ID             CONCEPT_ID integer
#> CONCEPT_NAME         CONCEPT_NAME integer
#> NEW_CONCEPT_ID     NEW_CONCEPT_ID integer
#> NEW_CONCEPT_NAME NEW_CONCEPT_NAME integer
#> TYPE                         TYPE integer
#> HERITAGE                 HERITAGE integer
#>                                                                 description
#> CONCEPT_ID                                      Original/source concept id.
#> CONCEPT_NAME                                  Original/source concept name.
#> NEW_CONCEPT_ID                                  Mapped (merged) concept id.
#> NEW_CONCEPT_NAME                              Mapped (merged) concept name.
#> TYPE             Mapping type (for example custom, hierarchy, correlation).
#> HERITAGE                 Heritage/domain context of mapping when available.
#>                                  lc500_example
#> CONCEPT_ID                             Rows: 0
#> CONCEPT_NAME     No mappings in lc500 example.
#> NEW_CONCEPT_ID                                
#> NEW_CONCEPT_NAME                              
#> TYPE                                          
#> HERITAGE
```

### Example Rows

``` r

utils::head(data$complementaryMappingTable, 10)
#> [1] CONCEPT_ID       CONCEPT_NAME     NEW_CONCEPT_ID   NEW_CONCEPT_NAME
#> [5] TYPE             HERITAGE        
#> <0 rows> (or 0-length row.names)
```

## `selectedFeatureData`: Final Feature Selection Object

`selectedFeatureData` is a list with:

- `selectedFeatureNames`: selected concept names used in matrix/panel
  contexts
- `selectedFeatureIds`: selected concept ids
- `selectedFeatures`: full feature table for selected concepts

``` r

data.frame(
  item = c("selectedFeatureNames", "selectedFeatureIds", "selectedFeatures"),
  type = c(
    class(data$selectedFeatureData$selectedFeatureNames)[1],
    class(data$selectedFeatureData$selectedFeatureIds)[1],
    class(data$selectedFeatureData$selectedFeatures)[1]
  ),
  size = c(
    length(data$selectedFeatureData$selectedFeatureNames),
    length(data$selectedFeatureData$selectedFeatureIds),
    nrow(data$selectedFeatureData$selectedFeatures)
  ),
  stringsAsFactors = FALSE
)
#>                   item       type size
#> 1 selectedFeatureNames  character   15
#> 2   selectedFeatureIds    numeric   16
#> 3     selectedFeatures data.frame   16
```

### Why names and IDs can have different lengths

In `lc500`, there are 16 selected IDs but 15 selected names. This can
happen when multiple selected IDs share the same name (for example
concepts from different heritages with identical names).

``` r

cat("selectedFeatureNames length:", length(data$selectedFeatureData$selectedFeatureNames), "\n")
#> selectedFeatureNames length: 15
cat("selectedFeatureIds length:", length(data$selectedFeatureData$selectedFeatureIds), "\n")
#> selectedFeatureIds length: 16
```

### `selectedFeatures` Data Dictionary

`selectedFeatures` has the same schema as `data_features` and contains
only the selected subset.

``` r

buildColumnDictionary(data$selectedFeatureData$selectedFeatures, featureDescriptions)
#>                                                  column      type
#> CONCEPT_ID                                   CONCEPT_ID   numeric
#> CONCEPT_NAME                               CONCEPT_NAME character
#> ABSTRACTION_LEVEL                     ABSTRACTION_LEVEL   numeric
#> TARGET_SUBJECT_COUNT               TARGET_SUBJECT_COUNT   numeric
#> CONTROL_SUBJECT_COUNT             CONTROL_SUBJECT_COUNT   numeric
#> TIME_TO_EVENT                             TIME_TO_EVENT      list
#> TARGET_SUBJECT_PREVALENCE     TARGET_SUBJECT_PREVALENCE   numeric
#> CONTROL_SUBJECT_PREVALENCE   CONTROL_SUBJECT_PREVALENCE   numeric
#> PREVALENCE_DIFFERENCE_RATIO PREVALENCE_DIFFERENCE_RATIO   numeric
#> CHI2Y                                             CHI2Y   logical
#> CHI2Y_P_VALUE                             CHI2Y_P_VALUE   numeric
#> LOGITTEST                                     LOGITTEST   logical
#> LOGITTEST_P_VALUE                     LOGITTEST_P_VALUE   numeric
#> HERITAGE                                       HERITAGE character
#>                                                                                                                                description
#> CONCEPT_ID                                                                    OMOP concept identifier (or mapped concept id after merges).
#> CONCEPT_NAME                                                                                                 Human-readable concept label.
#> ABSTRACTION_LEVEL           Concept abstraction level. -1 indicates cdm abstraction level concepts. Other abstraction levels are optional.
#> TARGET_SUBJECT_COUNT                                                                   Number of target subjects with the concept present.
#> CONTROL_SUBJECT_COUNT                                                                 Number of control subjects with the concept present.
#> TIME_TO_EVENT                                                  List-column of event times (days from cohort start) pooled for the concept.
#> TARGET_SUBJECT_PREVALENCE                                         Target prevalence proportion (TARGET_SUBJECT_COUNT / n_target_subjects).
#> CONTROL_SUBJECT_PREVALENCE                                     Control prevalence proportion (CONTROL_SUBJECT_COUNT / n_control_subjects).
#> PREVALENCE_DIFFERENCE_RATIO                                                Target/control prevalence ratio used for ranking and filtering.
#> CHI2Y                                                Boolean significance flag from chi-squared two-proportion test with Yates correction.
#> CHI2Y_P_VALUE                                                                            P-value from the chi-squared test used for CHI2Y.
#> LOGITTEST                                                                         Boolean significance flag from logistic regression test.
#> LOGITTEST_P_VALUE                                                                                   P-value from logistic regression test.
#> HERITAGE                                                           Source domain (heritage), such as condition_occurrence or visit_detail.
```

### Example Rows

``` r

utils::head(data$selectedFeatureData$selectedFeatures, 10)
#>    CONCEPT_ID               CONCEPT_NAME ABSTRACTION_LEVEL TARGET_SUBJECT_COUNT
#> 1      443388    Malignant tumor of lung                -1                  483
#> 2       32280                      Death                -1                  156
#> 3       32815          Death Certificate                -1                  147
#> 4     4182985 Diffusion capacity of lung                -1                  371
#> 5     4306655                      Death                -1                  150
#> 6     4014023            Palliative care                -1                  147
#> 7     4032404               Bronchoscopy                -1                  462
#> 8     4008226               Chemotherapy                -1                  164
#> 9     2107967                  Lobectomy                -1                  165
#> 10    2107968          Lobectomy of lung                -1                  168
#>    CONTROL_SUBJECT_COUNT
#> 1                      6
#> 2                      0
#> 3                      0
#> 4                     50
#> 5                      2
#> 6                      1
#> 7                     10
#> 8                      2
#> 9                      1
#> 10                     3
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                TIME_TO_EVENT
#> 1                                                                                                                      4, 1, 4, 2, 6, 0, 6, 5, 5, 3, 5, 5, 8, 7, 6, 8, 4, 0, 10, 2, 7, 0, 7, 5, 2, 1, 9, 3, 4, 3, 3, 14, 2, 6, 2, 3, 6, 0, 8, 10, 5, 14, 0, 6, 5, 11, 6, 7, 6, 8, 4, 7, 0, 5, 7, 9, 8, 9, 0, 6, 8, 6, 4, 3, 16, 11, 4, 4, 5, 11, 3, 6, 6, 5, 8, 0, 2, 1, 4, 4, 5, 3, 5, 9, 10, 4, 6, 6, 10, 3, 4, 0, 5, 4, 0, 8, 5, 9, 5, 14, 0, 0, 4, 10, 5, 4, 13, 3, 1, 5, 2, 3, 0, 4, 11, 7, 0, 0, 13, 6, 4, 8, 6, 8, 9, 5, 0, 2, 4, 3, 13, 1, 5, 7, 3, 9, 5, 0, 3, 10, 3, 7, 8, 8, 6, 1, 3, 10, 8, 8, 4, 0, 15, 9, 0, 5, 4, 11, 7, 8, 6, 4, 1, 12, 0, 1, 3, 1, 12, 0, 6, 5, 11, 2, 0, 6, 2, 0, 6, 5, 6, 2, 11, 9, 1, 8, 3, 3, 5, 12, 3, 1, 6, 10, 8, 10, 4, 8, 3, 9, 11, 5, 3, 7, 7, 8, 2, 5, 10, 7, 4, 1, 5, 9, 6, 4, 2, 2, 13, 9, 16, 1, 2, 7, 3, 4, 3, 10, 2, 6, 7, 7, 6, 6, 3, 6, 4, 0, 6, 5, 10, 2, 9, 7, 3, 0, 6, 4, 10, 2, 5, 8, 1, 0, 2, 6, 5, 6, 3, 9, 5, 10, 11, 6, 6, 8, 0, 1, 8, 0, 0, 1, 1, 0, 2, 3, 5, 1, 4, 5, 6, 6, 9, 5, 1, 0, 0, 5, 11, 0, 7, 4, 2, 0, 5, 5, 11, 0, 3, 3, 2, 5, 7, 5, 7, 1, 6, 5, 3, 4, 6, 1, 10, 1, 4, 7, 3, 16, 4, 5, 0, 5, 5, 6, 3, 2, 8, 7, 3, 3, 0, 7, 10, 8, 3, 0, 7, 5, 8, 3, 3, 6, 5, 4, 6, 12, 6, 5, 3, 11, 7, 9, 14, 4, 0, 10, 1, 6, 11, 10, 2, 6, 2, 0, 7, 2, 0, 2, 1, 1, 5, 2, 3, 2, 7, 4, 6, 0, 7, 4, 10, 8, 5, 6, 1, 0, 18, 2, 2, 0, 2, 1, 10, 2, 9, 2, 4, 5, 5, 0, 0, 4, 4, 2, 13, 4, 13, 5, 6, 8, 3, 4, 7, 7, 1, 11, 13, 10, 3, 6, 1, 8, 2, 7, 8, 1, 3, 5, 4, 10, 2, 6, 0, 4, 2, 8, 1, 7, 0, 3, 6, 0, 10, 1, 8, 3, 7, 4, 1, 4, 4, 9, 1, 5, 3, 7, 9, 7, 6, 2, 7, 11, 3, 0, 7, 10, 1, 4, 13, 0, 9, 4, 2, 5, 4, 7, 13, 0, 0, 11, 2, 0, 2
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  11, 244, 150, 165, 204, 148, 224, 191, 199, 214, 170, 212, 213, 200, 206, 190, 259, 192, 193, 232, 187, 191, 219, 180, 193, 157, 178, 158, 175, 175, 214, 183, 189, 201, 202, 194, 185, 166, 180, 212, 171, 171, 198, 193, 214, 224, 170, 168, 145, 187, 171, 213, 224, 203, 171, 209, 212, 195, 205, 270, 216, 213, 215, 183, 167, 178, 192, 152, 219, 177, 175, 190, 192, 193, 193, 173, 185, 250, 177, 160, 183, 173, 204, 174, 194, 189, 192, 186, 264, 181, 123, 176, 193, 260, 208, 226, 224, 211, 202, 243, 214, 187, 201, 217, 206, 164, 272, 189, 224, 167, 208, 216, 156, 151, 156, 199, 205, 175, 192, 170, 169, 224, 126, 176, 190, 187, 196, 173, 175, 209, 174, 150, 198, 232, 196, 178, 172, 219, 199, 243, 235, 190, 187, 217, 230, 173, 185, 212, 216, 203, 223, 211, 192, 168, 206, 200
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 19, 90, 27, 220, 225, 267, 272, 244, 262, 299, 258, 245, 250, 247, 235, 237, 257, 207, 235, 224, 253, 254, 225, 248, 251, 235, 261, 253, 245, 230, 276, 230, 240, 237, 255, 256, 245, 260, 200, 224, 241, 240, 280, 113, 240, 192, 220, 227, 279, 255, 253, 242, 258, 234, 249, 198, 235, 263, 234, 259, 247, 260, 265, 226, 258, 214, 247, 273, 276, 248, 229, 227, 258, 235, 262, 253, 249, 279, 220, 248, 213, 292, 267, 247, 229, 249, 257, 255, 230, 231, 262, 226, 233, 268, 252, 244, 264, 211, 263, 249, 231, 243, 254, 267, 232, 250, 242, 271, 257, 217, 266, 251, 213, 256, 243, 286, 256, 281, 249, 264, 262, 226, 248, 262, 243, 254, 281, 235, 223, 276, 230, 293, 261, 245, 222, 220, 255, 237, 233, 268, 244, 265, 269, 235, 251, 255, 253
#> 4                                                                                                                                                                                                                                11, 18, 23, 23, 2, 13, 19, 48, 23, 21, 0, 28, 14, 17, 0, 42, 9, 18, 0, 28, 13, 0, 6, 27, 12, 16, 24, 21, 34, 29, 16, 28, 25, 18, 18, 20, 20, 21, 27, 30, 23, 17, 1, 30, 13, 9, 13, 16, 15, 23, 6, 40, 21, 24, 22, 10, 12, 37, 2, 18, 27, 15, 24, 26, 7, 10, 19, 19, 16, 26, 0, 7, 10, 12, 24, 9, 11, 13, 20, 24, 0, 33, 7, 2, 0, 32, 2, 8, 24, 14, 34, 14, 33, 4, 3, 22, 25, 9, 30, 40, 35, 20, 6, 11, 36, 18, 33, 22, 2, 14, 15, 24, 16, 24, 0, 16, 11, 7, 7, 27, 9, 11, 28, 20, 15, 22, 17, 8, 33, 18, 15, 3, 11, 9, 28, 13, 25, 29, 9, 20, 28, 18, 2, 7, 15, 19, 29, 22, 25, 8, 34, 15, 7, 23, 23, 23, 9, 20, 21, 23, 7, 14, 28, 14, 26, 16, 32, 19, 0, 18, 30, 1, 7, 26, 28, 2, 33, 24, 23, 0, 6, 34, 8, 29, 18, 26, 6, 21, 17, 12, 11, 39, 12, 9, 17, 10, 37, 28, 24, 28, 32, 9, 26, 13, 18, 15, 28, 18, 29, 15, 40, 22, 3, 12, 37, 32, 7, 0, 20, 10, 38, 0, 29, 25, 9, 29, 27, 25, 34, 31, 31, 26, 22, 1, 14, 15, 16, 13, 18, 21, 0, 13, 9, 23, 15, 27, 19, 28, 5, 14, 5, 37, 8, 36, 16, 0, 26, 31, 43, 33, 20, 0, 52, 30, 32, 25, 0, 13, 10, 23, 1, 40, 5, 20, 18, 12, 14, 18, 17, 18, 28, 16, 32, 24, 3, 15, 22, 23, 20, 16, 0, 23, 24, 2, 31, 56, 23, 24, 7, 18, 18, 0, 38, 0, 11, 21, 9, 25, 28, 42, 18, 3, 23, 25, 32, 4, 23, 22, 30, 20, 11, 21, 0, 9, 27, 16, 5, 39, 26, 21, 38, 16, 11, 48, 16, 16, 22, 45, 19, 11, 29, 0, 22, 23, 19, 31, 12, 18, 25, 19, 31, 24, 34, 21, 27, 19, 29, 35, 33, 19, 30, 12, 13, 11, 24, 33, 33, 29, 13, 29, 29
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  69, 53, 100, 89, 225, 216, 206, 192, 182, 171, 172, 208, 116, 177, 165, 217, 168, 221, 192, 199, 213, 189, 262, 178, 175, 208, 191, 189, 163, 132, 212, 226, 201, 232, 161, 187, 174, 139, 195, 181, 157, 152, 240, 177, 156, 210, 187, 213, 144, 212, 198, 177, 191, 212, 187, 189, 199, 218, 144, 226, 140, 139, 206, 167, 214, 224, 194, 221, 196, 223, 209, 224, 192, 195, 228, 170, 228, 205, 178, 217, 172, 199, 191, 212, 227, 209, 195, 209, 201, 214, 171, 232, 177, 181, 193, 163, 203, 177, 209, 177, 237, 164, 198, 167, 255, 213, 171, 244, 197, 236, 171, 206, 220, 217, 181, 201, 176, 193, 228, 170, 148, 197, 217, 180, 188, 200, 167, 234, 181, 234, 230, 189, 255, 151, 193, 154, 209, 244, 209, 203, 194, 230, 159, 281, 170, 300, 206, 200, 158, 182
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 121, 39, 63, 209, 224, 283, 221, 201, 165, 180, 192, 226, 232, 217, 183, 152, 229, 166, 233, 226, 216, 234, 215, 250, 202, 266, 223, 261, 175, 268, 183, 184, 244, 186, 242, 235, 199, 227, 159, 226, 265, 234, 217, 214, 276, 241, 203, 209, 197, 202, 257, 164, 267, 300, 136, 186, 168, 173, 250, 237, 201, 166, 269, 217, 228, 202, 198, 212, 228, 224, 166, 180, 187, 199, 220, 185, 229, 236, 206, 220, 168, 204, 222, 187, 209, 165, 204, 242, 190, 162, 250, 227, 190, 210, 248, 176, 209, 262, 193, 170, 243, 162, 75, 227, 219, 190, 207, 195, 193, 189, 220, 217, 222, 219, 219, 194, 226, 224, 190, 213, 193, 169, 195, 216, 189, 206, 226, 194, 176, 192, 199, 234, 252, 217, 173, 214, 255, 171, 263, 251, 203, 172, 208, 196, 161, 214, 226
#> 7  0, 3, 5, 7, 7, 16, 7, 29, 0, 7, 10, 5, 4, 2, 8, 12, 4, 16, 12, 17, 10, 16, 0, 7, 14, 8, 12, 11, 18, 10, 3, 2, 13, 15, 6, 21, 10, 23, 15, 13, 5, 14, 20, 10, 10, 10, 5, 0, 11, 2, 9, 0, 8, 11, 11, 25, 8, 3, 0, 0, 4, 0, 16, 16, 0, 21, 21, 12, 0, 16, 1, 11, 12, 17, 8, 16, 7, 13, 9, 12, 4, 19, 3, 16, 17, 7, 15, 8, 10, 13, 12, 12, 16, 12, 9, 6, 9, 17, 7, 19, 11, 17, 7, 11, 10, 18, 10, 9, 0, 19, 8, 6, 18, 0, 11, 9, 3, 8, 31, 0, 0, 17, 16, 0, 9, 4, 12, 20, 0, 1, 6, 9, 13, 12, 0, 11, 17, 2, 14, 0, 7, 16, 19, 6, 7, 4, 1, 16, 10, 2, 25, 0, 3, 1, 11, 14, 0, 13, 11, 9, 14, 9, 10, 5, 0, 18, 18, 8, 31, 7, 11, 10, 8, 13, 10, 1, 0, 0, 14, 17, 16, 8, 8, 10, 0, 4, 17, 5, 19, 9, 11, 5, 2, 11, 22, 16, 15, 11, 5, 10, 7, 0, 13, 4, 15, 0, 8, 24, 12, 7, 15, 0, 7, 13, 17, 0, 18, 20, 2, 13, 14, 14, 5, 8, 0, 4, 12, 13, 12, 0, 10, 4, 15, 17, 19, 14, 10, 2, 0, 9, 1, 10, 9, 15, 11, 24, 5, 0, 16, 10, 13, 3, 0, 9, 22, 12, 10, 2, 22, 6, 5, 9, 16, 13, 3, 6, 12, 20, 9, 16, 11, 0, 8, 30, 11, 30, 15, 6, 7, 13, 12, 19, 27, 7, 0, 11, 10, 11, 7, 5, 7, 28, 0, 9, 13, 18, 11, 16, 6, 28, 24, 14, 4, 16, 9, 9, 18, 11, 0, 11, 7, 13, 23, 13, 8, 4, 0, 0, 10, 4, 5, 13, 5, 6, 21, 0, 12, 9, 17, 12, 11, 13, 15, 7, 3, 14, 25, 2, 23, 9, 14, 18, 15, 17, 9, 6, 7, 0, 18, 17, 6, 9, 24, 10, 3, 14, 10, 6, 0, 10, 14, 8, 12, 16, 16, 0, 16, 6, 22, 5, 7, 7, 3, 18, 4, 18, 11, 7, 4, 12, 10, 15, 5, 11, 22, 1, 25, 3, 8, 17, 8, 23, 9, 11, 15, 9, 22, 14, 16, 11, 27, 19, 13, 4, 9, 13, 6, 8, 9, 21, 3, 16, 11, 23, 7, 12, 21, 7, 23, 6, 13, 14, 18, 11, 1, 9, 11, 7, 11, 7, 10, 20, 5, 8, 5, 7, 14, 14, 7, 11, 13, 7, 5, 2, 0, 15, 5, 9, 9, 9, 8, 11, 8, 5, 4, 23, 10, 0, 6, 1, 3, 8
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      24, 27, 6, 32, 20, 17, 18, 11, 0, 21, 25, 19, 26, 10, 9, 22, 13, 10, 13, 0, 20, 0, 29, 29, 0, 16, 24, 26, 16, 18, 18, 18, 21, 16, 20, 25, 18, 23, 17, 21, 22, 14, 21, 16, 33, 25, 12, 12, 15, 29, 28, 23, 13, 24, 27, 16, 22, 23, 29, 28, 25, 23, 21, 8, 16, 30, 13, 11, 20, 22, 24, 9, 0, 18, 18, 28, 7, 5, 16, 16, 5, 12, 25, 27, 15, 17, 0, 16, 26, 36, 13, 26, 23, 22, 16, 25, 6, 25, 13, 8, 23, 23, 14, 26, 0, 2, 30, 25, 0, 15, 48, 6, 16, 41, 0, 11, 18, 29, 16, 20, 28, 20, 11, 31, 3, 18, 22, 7, 12, 21, 32, 10, 6, 22, 27, 16, 18, 21, 29, 13, 42, 3, 25, 9, 22, 15, 27, 28, 0, 12, 11, 21, 17, 20, 36, 13, 29, 23, 11, 21, 21, 15, 151, 28
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  113, 80, 120, 22, 22, 31, 19, 20, 22, 19, 20, 14, 17, 27, 6, 33, 6, 30, 28, 34, 11, 25, 0, 14, 11, 27, 18, 29, 17, 8, 24, 26, 14, 33, 21, 18, 27, 17, 14, 14, 15, 9, 13, 17, 26, 28, 26, 24, 18, 12, 14, 23, 12, 17, 18, 14, 17, 18, 19, 25, 22, 21, 17, 23, 20, 17, 16, 36, 30, 27, 17, 38, 16, 15, 25, 27, 22, 7, 23, 24, 17, 10, 30, 11, 25, 8, 23, 16, 19, 29, 22, 19, 20, 18, 32, 12, 26, 21, 6, 16, 18, 15, 23, 20, 18, 28, 20, 25, 35, 30, 13, 23, 30, 15, 26, 12, 12, 14, 25, 20, 16, 25, 12, 1, 16, 11, 26, 36, 22, 26, 21, 19, 18, 19, 21, 21, 18, 23, 12, 18, 1, 6, 17, 29, 17, 27, 21, 10, 26, 16, 24, 15, 26, 18, 32, 15, 29, 13, 16, 23, 92, 28, 30, 35, 75
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           104, 65, 47, 121, 8, 25, 11, 19, 12, 28, 11, 24, 21, 14, 15, 16, 6, 44, 31, 28, 10, 20, 13, 16, 7, 16, 15, 12, 14, 20, 8, 18, 22, 23, 25, 13, 24, 15, 16, 11, 19, 5, 25, 33, 12, 11, 82, 20, 15, 20, 20, 29, 9, 14, 26, 18, 13, 11, 25, 24, 13, 16, 25, 14, 24, 16, 26, 17, 30, 8, 18, 2, 4, 19, 23, 15, 8, 26, 18, 31, 24, 11, 18, 0, 22, 17, 19, 21, 17, 20, 16, 23, 29, 30, 17, 21, 15, 19, 17, 32, 19, 17, 13, 13, 16, 38, 12, 9, 15, 10, 17, 22, 32, 28, 26, 24, 27, 13, 22, 16, 8, 10, 19, 18, 17, 22, 20, 15, 29, 13, 21, 16, 16, 7, 26, 24, 27, 7, 11, 24, 16, 14, 23, 16, 21, 13, 15, 11, 25, 9, 29, 22, 13, 23, 12, 18, 22, 24, 22, 28, 6, 13, 3, 25, 46, 59, 116, 19
#>    TARGET_SUBJECT_PREVALENCE CONTROL_SUBJECT_PREVALENCE
#> 1                      0.966                      0.012
#> 2                      0.312                      0.000
#> 3                      0.294                      0.000
#> 4                      0.742                      0.100
#> 5                      0.300                      0.004
#> 6                      0.294                      0.002
#> 7                      0.924                      0.020
#> 8                      0.328                      0.004
#> 9                      0.330                      0.002
#> 10                     0.336                      0.006
#>    PREVALENCE_DIFFERENCE_RATIO CHI2Y CHI2Y_P_VALUE LOGITTEST LOGITTEST_P_VALUE
#> 1                        80.50  TRUE         0.001      TRUE             0.002
#> 2                       100.00  TRUE         0.001      TRUE             0.002
#> 3                       100.00  TRUE         0.001      TRUE             0.002
#> 4                         7.42  TRUE         0.001      TRUE             0.002
#> 5                        75.00  TRUE         0.001      TRUE             0.002
#> 6                       147.00  TRUE         0.001      TRUE             0.002
#> 7                        46.20  TRUE         0.001      TRUE             0.002
#> 8                        82.00  TRUE         0.001      TRUE             0.002
#> 9                       165.00  TRUE         0.001      TRUE             0.002
#> 10                       56.00  TRUE         0.001      TRUE             0.002
#>                HERITAGE
#> 1  condition_occurrence
#> 2                 death
#> 3                 death
#> 4           measurement
#> 5           observation
#> 6           observation
#> 7  procedure_occurrence
#> 8  procedure_occurrence
#> 9  procedure_occurrence
#> 10 procedure_occurrence
```

## `conceptsData`: Ontology Context

`conceptsData` is a list with:

- `concept_ancestor`: concept hierarchy links
- `concept`: concept metadata table (may be missing in some saved
  studies)

``` r

data.frame(
  component = c("concept_ancestor", "concept"),
  class = c(
    class(data$conceptsData$concept_ancestor)[1],
    if (is.null(data$conceptsData$concept)) "NULL" else class(data$conceptsData$concept)[1]
  ),
  nrow = c(
    if (is.data.frame(data$conceptsData$concept_ancestor)) nrow(data$conceptsData$concept_ancestor) else 0L,
    if (is.data.frame(data$conceptsData$concept)) nrow(data$conceptsData$concept) else 0L
  ),
  stringsAsFactors = FALSE
)
#>          component      class nrow
#> 1 concept_ancestor data.frame    4
#> 2          concept       NULL    0
```

### `concept_ancestor` Data Dictionary

``` r

if (is.data.frame(data$conceptsData$concept_ancestor)) {
  ancestorDescriptions <- c(
    ancestor_concept_id = "Ancestor concept id in OMOP hierarchy.",
    descendant_concept_id = "Descendant concept id in OMOP hierarchy.",
    min_levels_of_separation = "Minimum hierarchy distance between ancestor and descendant.",
    max_levels_of_separation = "Maximum hierarchy distance between ancestor and descendant."
  )
  buildColumnDictionary(data$conceptsData$concept_ancestor, ancestorDescriptions)
} else {
  cat("No concept_ancestor table available.\n")
}
#>                                            column    type
#> ancestor_concept_id           ancestor_concept_id numeric
#> descendant_concept_id       descendant_concept_id numeric
#> min_levels_of_separation min_levels_of_separation numeric
#> max_levels_of_separation max_levels_of_separation numeric
#>                                                                          description
#> ancestor_concept_id                           Ancestor concept id in OMOP hierarchy.
#> descendant_concept_id                       Descendant concept id in OMOP hierarchy.
#> min_levels_of_separation Minimum hierarchy distance between ancestor and descendant.
#> max_levels_of_separation Maximum hierarchy distance between ancestor and descendant.
```

### Example Rows

``` r

if (is.data.frame(data$conceptsData$concept_ancestor)) {
  utils::head(data$conceptsData$concept_ancestor, 10)
} else {
  cat("No concept_ancestor table available.\n")
}
#>   ancestor_concept_id descendant_concept_id min_levels_of_separation
#> 1             4008211               4176729                        1
#> 2             2107967               2107968                        1
#> 3             2107967               2108158                        1
#> 4               32280                 32815                        1
#>   max_levels_of_separation
#> 1                        1
#> 2                        1
#> 3                        1
#> 4                        1
```

## `config` and Metadata

`config` stores study-level settings and sidecar metadata loaded with
the study.

``` r

str(data$config)
#> List of 2
#>  $ complName: chr "lc500"
#>  $ metadata :List of 4
#>   ..$ study           : chr "lc500"
#>   ..$ target_patients : int 500
#>   ..$ control_patients: int 500
#>   ..$ chi2y_count     : int 16
```

``` r

if (is.list(data$config$metadata)) {
  data.frame(
    key = names(data$config$metadata),
    value = unlist(data$config$metadata, use.names = FALSE),
    stringsAsFactors = FALSE
  )
} else {
  cat("No metadata found in config.\n")
}
#>                key value
#> 1            study lc500
#> 2  target_patients   500
#> 3 control_patients   500
#> 4      chi2y_count    16
```

## Practical Reading Order for Interpretation

For interpretation work, this order is recommended:

1.  `data_initial` and `data_person`: verify cohort composition and
    demographics.
2.  `data_features`: inspect effect sizes and significance at concept
    level.
3.  `data_patients`: inspect patient-level event timing and prevalence
    counts.
4.  `selectedFeatureData$selectedFeatures`: focus on the final selected
    concept set.
5.  `complementaryMappingTable`: review whether any concept merges
    changed semantics.
6.  `conceptsData$concept_ancestor`: inspect hierarchy context where
    needed.

&nbsp;


    ## Cohort Membership: `data_initial`

    Each row is one subject with cohort assignment (`target`/`control`) and follow-up
    window.


    ``` r
    utils::head(data$data_initial, 10)
    #>    COHORT_DEFINITION_ID SUBJECT_ID COHORT_START_DATE COHORT_END_DATE
    #> 1                target     100001        2019-06-14      2020-06-13
    #> 2                target     100002        2020-10-05      2021-10-05
    #> 3                target     100003        2020-04-09      2021-04-09
    #> 4                target     100004        2019-03-24      2020-03-23
    #> 5                target     100005        2020-03-28      2021-03-28
    #> 6                target     100006        2020-11-15      2021-11-15
    #> 7                target     100007        2019-07-30      2020-07-29
    #> 8                target     100008        2019-07-22      2020-07-21
    #> 9                target     100009        2019-07-19      2020-07-18
    #> 10               target     100010        2020-07-11      2021-07-11

## Subject Demographics: `data_person`

Subject-level demographics used in downstream interpretation and
plotting.

``` r

utils::head(data$data_person, 10)
#>    PERSON_ID GENDER_CONCEPT_ID YEAR_OF_BIRTH
#> 1     100001              8507          1958
#> 2     100002              8507          1967
#> 3     100003              8507          1967
#> 4     100004              8507          1962
#> 5     100005              8532          1955
#> 6     100006              8507          1963
#> 7     100007              8507          1957
#> 8     100008              8507          1968
#> 9     100009              8532          1955
#> 10    100010              8507          1971
```

## Concept-Level Summary: `data_features`

Feature-level summary statistics by concept, including prevalence and
test flags.

``` r

utils::head(data$data_features, 10)
#>    CONCEPT_ID               CONCEPT_NAME ABSTRACTION_LEVEL TARGET_SUBJECT_COUNT
#> 1      443388    Malignant tumor of lung                -1                  483
#> 2       32280                      Death                -1                  156
#> 3       32815          Death Certificate                -1                  147
#> 4     4182985 Diffusion capacity of lung                -1                  371
#> 5     4306655                      Death                -1                  150
#> 6     4014023            Palliative care                -1                  147
#> 7     4032404               Bronchoscopy                -1                  462
#> 8     4008226               Chemotherapy                -1                  164
#> 9     2107967                  Lobectomy                -1                  165
#> 10    2107968          Lobectomy of lung                -1                  168
#>    CONTROL_SUBJECT_COUNT
#> 1                      6
#> 2                      0
#> 3                      0
#> 4                     50
#> 5                      2
#> 6                      1
#> 7                     10
#> 8                      2
#> 9                      1
#> 10                     3
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                TIME_TO_EVENT
#> 1                                                                                                                      4, 1, 4, 2, 6, 0, 6, 5, 5, 3, 5, 5, 8, 7, 6, 8, 4, 0, 10, 2, 7, 0, 7, 5, 2, 1, 9, 3, 4, 3, 3, 14, 2, 6, 2, 3, 6, 0, 8, 10, 5, 14, 0, 6, 5, 11, 6, 7, 6, 8, 4, 7, 0, 5, 7, 9, 8, 9, 0, 6, 8, 6, 4, 3, 16, 11, 4, 4, 5, 11, 3, 6, 6, 5, 8, 0, 2, 1, 4, 4, 5, 3, 5, 9, 10, 4, 6, 6, 10, 3, 4, 0, 5, 4, 0, 8, 5, 9, 5, 14, 0, 0, 4, 10, 5, 4, 13, 3, 1, 5, 2, 3, 0, 4, 11, 7, 0, 0, 13, 6, 4, 8, 6, 8, 9, 5, 0, 2, 4, 3, 13, 1, 5, 7, 3, 9, 5, 0, 3, 10, 3, 7, 8, 8, 6, 1, 3, 10, 8, 8, 4, 0, 15, 9, 0, 5, 4, 11, 7, 8, 6, 4, 1, 12, 0, 1, 3, 1, 12, 0, 6, 5, 11, 2, 0, 6, 2, 0, 6, 5, 6, 2, 11, 9, 1, 8, 3, 3, 5, 12, 3, 1, 6, 10, 8, 10, 4, 8, 3, 9, 11, 5, 3, 7, 7, 8, 2, 5, 10, 7, 4, 1, 5, 9, 6, 4, 2, 2, 13, 9, 16, 1, 2, 7, 3, 4, 3, 10, 2, 6, 7, 7, 6, 6, 3, 6, 4, 0, 6, 5, 10, 2, 9, 7, 3, 0, 6, 4, 10, 2, 5, 8, 1, 0, 2, 6, 5, 6, 3, 9, 5, 10, 11, 6, 6, 8, 0, 1, 8, 0, 0, 1, 1, 0, 2, 3, 5, 1, 4, 5, 6, 6, 9, 5, 1, 0, 0, 5, 11, 0, 7, 4, 2, 0, 5, 5, 11, 0, 3, 3, 2, 5, 7, 5, 7, 1, 6, 5, 3, 4, 6, 1, 10, 1, 4, 7, 3, 16, 4, 5, 0, 5, 5, 6, 3, 2, 8, 7, 3, 3, 0, 7, 10, 8, 3, 0, 7, 5, 8, 3, 3, 6, 5, 4, 6, 12, 6, 5, 3, 11, 7, 9, 14, 4, 0, 10, 1, 6, 11, 10, 2, 6, 2, 0, 7, 2, 0, 2, 1, 1, 5, 2, 3, 2, 7, 4, 6, 0, 7, 4, 10, 8, 5, 6, 1, 0, 18, 2, 2, 0, 2, 1, 10, 2, 9, 2, 4, 5, 5, 0, 0, 4, 4, 2, 13, 4, 13, 5, 6, 8, 3, 4, 7, 7, 1, 11, 13, 10, 3, 6, 1, 8, 2, 7, 8, 1, 3, 5, 4, 10, 2, 6, 0, 4, 2, 8, 1, 7, 0, 3, 6, 0, 10, 1, 8, 3, 7, 4, 1, 4, 4, 9, 1, 5, 3, 7, 9, 7, 6, 2, 7, 11, 3, 0, 7, 10, 1, 4, 13, 0, 9, 4, 2, 5, 4, 7, 13, 0, 0, 11, 2, 0, 2
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  11, 244, 150, 165, 204, 148, 224, 191, 199, 214, 170, 212, 213, 200, 206, 190, 259, 192, 193, 232, 187, 191, 219, 180, 193, 157, 178, 158, 175, 175, 214, 183, 189, 201, 202, 194, 185, 166, 180, 212, 171, 171, 198, 193, 214, 224, 170, 168, 145, 187, 171, 213, 224, 203, 171, 209, 212, 195, 205, 270, 216, 213, 215, 183, 167, 178, 192, 152, 219, 177, 175, 190, 192, 193, 193, 173, 185, 250, 177, 160, 183, 173, 204, 174, 194, 189, 192, 186, 264, 181, 123, 176, 193, 260, 208, 226, 224, 211, 202, 243, 214, 187, 201, 217, 206, 164, 272, 189, 224, 167, 208, 216, 156, 151, 156, 199, 205, 175, 192, 170, 169, 224, 126, 176, 190, 187, 196, 173, 175, 209, 174, 150, 198, 232, 196, 178, 172, 219, 199, 243, 235, 190, 187, 217, 230, 173, 185, 212, 216, 203, 223, 211, 192, 168, 206, 200
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 19, 90, 27, 220, 225, 267, 272, 244, 262, 299, 258, 245, 250, 247, 235, 237, 257, 207, 235, 224, 253, 254, 225, 248, 251, 235, 261, 253, 245, 230, 276, 230, 240, 237, 255, 256, 245, 260, 200, 224, 241, 240, 280, 113, 240, 192, 220, 227, 279, 255, 253, 242, 258, 234, 249, 198, 235, 263, 234, 259, 247, 260, 265, 226, 258, 214, 247, 273, 276, 248, 229, 227, 258, 235, 262, 253, 249, 279, 220, 248, 213, 292, 267, 247, 229, 249, 257, 255, 230, 231, 262, 226, 233, 268, 252, 244, 264, 211, 263, 249, 231, 243, 254, 267, 232, 250, 242, 271, 257, 217, 266, 251, 213, 256, 243, 286, 256, 281, 249, 264, 262, 226, 248, 262, 243, 254, 281, 235, 223, 276, 230, 293, 261, 245, 222, 220, 255, 237, 233, 268, 244, 265, 269, 235, 251, 255, 253
#> 4                                                                                                                                                                                                                                11, 18, 23, 23, 2, 13, 19, 48, 23, 21, 0, 28, 14, 17, 0, 42, 9, 18, 0, 28, 13, 0, 6, 27, 12, 16, 24, 21, 34, 29, 16, 28, 25, 18, 18, 20, 20, 21, 27, 30, 23, 17, 1, 30, 13, 9, 13, 16, 15, 23, 6, 40, 21, 24, 22, 10, 12, 37, 2, 18, 27, 15, 24, 26, 7, 10, 19, 19, 16, 26, 0, 7, 10, 12, 24, 9, 11, 13, 20, 24, 0, 33, 7, 2, 0, 32, 2, 8, 24, 14, 34, 14, 33, 4, 3, 22, 25, 9, 30, 40, 35, 20, 6, 11, 36, 18, 33, 22, 2, 14, 15, 24, 16, 24, 0, 16, 11, 7, 7, 27, 9, 11, 28, 20, 15, 22, 17, 8, 33, 18, 15, 3, 11, 9, 28, 13, 25, 29, 9, 20, 28, 18, 2, 7, 15, 19, 29, 22, 25, 8, 34, 15, 7, 23, 23, 23, 9, 20, 21, 23, 7, 14, 28, 14, 26, 16, 32, 19, 0, 18, 30, 1, 7, 26, 28, 2, 33, 24, 23, 0, 6, 34, 8, 29, 18, 26, 6, 21, 17, 12, 11, 39, 12, 9, 17, 10, 37, 28, 24, 28, 32, 9, 26, 13, 18, 15, 28, 18, 29, 15, 40, 22, 3, 12, 37, 32, 7, 0, 20, 10, 38, 0, 29, 25, 9, 29, 27, 25, 34, 31, 31, 26, 22, 1, 14, 15, 16, 13, 18, 21, 0, 13, 9, 23, 15, 27, 19, 28, 5, 14, 5, 37, 8, 36, 16, 0, 26, 31, 43, 33, 20, 0, 52, 30, 32, 25, 0, 13, 10, 23, 1, 40, 5, 20, 18, 12, 14, 18, 17, 18, 28, 16, 32, 24, 3, 15, 22, 23, 20, 16, 0, 23, 24, 2, 31, 56, 23, 24, 7, 18, 18, 0, 38, 0, 11, 21, 9, 25, 28, 42, 18, 3, 23, 25, 32, 4, 23, 22, 30, 20, 11, 21, 0, 9, 27, 16, 5, 39, 26, 21, 38, 16, 11, 48, 16, 16, 22, 45, 19, 11, 29, 0, 22, 23, 19, 31, 12, 18, 25, 19, 31, 24, 34, 21, 27, 19, 29, 35, 33, 19, 30, 12, 13, 11, 24, 33, 33, 29, 13, 29, 29
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  69, 53, 100, 89, 225, 216, 206, 192, 182, 171, 172, 208, 116, 177, 165, 217, 168, 221, 192, 199, 213, 189, 262, 178, 175, 208, 191, 189, 163, 132, 212, 226, 201, 232, 161, 187, 174, 139, 195, 181, 157, 152, 240, 177, 156, 210, 187, 213, 144, 212, 198, 177, 191, 212, 187, 189, 199, 218, 144, 226, 140, 139, 206, 167, 214, 224, 194, 221, 196, 223, 209, 224, 192, 195, 228, 170, 228, 205, 178, 217, 172, 199, 191, 212, 227, 209, 195, 209, 201, 214, 171, 232, 177, 181, 193, 163, 203, 177, 209, 177, 237, 164, 198, 167, 255, 213, 171, 244, 197, 236, 171, 206, 220, 217, 181, 201, 176, 193, 228, 170, 148, 197, 217, 180, 188, 200, 167, 234, 181, 234, 230, 189, 255, 151, 193, 154, 209, 244, 209, 203, 194, 230, 159, 281, 170, 300, 206, 200, 158, 182
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 121, 39, 63, 209, 224, 283, 221, 201, 165, 180, 192, 226, 232, 217, 183, 152, 229, 166, 233, 226, 216, 234, 215, 250, 202, 266, 223, 261, 175, 268, 183, 184, 244, 186, 242, 235, 199, 227, 159, 226, 265, 234, 217, 214, 276, 241, 203, 209, 197, 202, 257, 164, 267, 300, 136, 186, 168, 173, 250, 237, 201, 166, 269, 217, 228, 202, 198, 212, 228, 224, 166, 180, 187, 199, 220, 185, 229, 236, 206, 220, 168, 204, 222, 187, 209, 165, 204, 242, 190, 162, 250, 227, 190, 210, 248, 176, 209, 262, 193, 170, 243, 162, 75, 227, 219, 190, 207, 195, 193, 189, 220, 217, 222, 219, 219, 194, 226, 224, 190, 213, 193, 169, 195, 216, 189, 206, 226, 194, 176, 192, 199, 234, 252, 217, 173, 214, 255, 171, 263, 251, 203, 172, 208, 196, 161, 214, 226
#> 7  0, 3, 5, 7, 7, 16, 7, 29, 0, 7, 10, 5, 4, 2, 8, 12, 4, 16, 12, 17, 10, 16, 0, 7, 14, 8, 12, 11, 18, 10, 3, 2, 13, 15, 6, 21, 10, 23, 15, 13, 5, 14, 20, 10, 10, 10, 5, 0, 11, 2, 9, 0, 8, 11, 11, 25, 8, 3, 0, 0, 4, 0, 16, 16, 0, 21, 21, 12, 0, 16, 1, 11, 12, 17, 8, 16, 7, 13, 9, 12, 4, 19, 3, 16, 17, 7, 15, 8, 10, 13, 12, 12, 16, 12, 9, 6, 9, 17, 7, 19, 11, 17, 7, 11, 10, 18, 10, 9, 0, 19, 8, 6, 18, 0, 11, 9, 3, 8, 31, 0, 0, 17, 16, 0, 9, 4, 12, 20, 0, 1, 6, 9, 13, 12, 0, 11, 17, 2, 14, 0, 7, 16, 19, 6, 7, 4, 1, 16, 10, 2, 25, 0, 3, 1, 11, 14, 0, 13, 11, 9, 14, 9, 10, 5, 0, 18, 18, 8, 31, 7, 11, 10, 8, 13, 10, 1, 0, 0, 14, 17, 16, 8, 8, 10, 0, 4, 17, 5, 19, 9, 11, 5, 2, 11, 22, 16, 15, 11, 5, 10, 7, 0, 13, 4, 15, 0, 8, 24, 12, 7, 15, 0, 7, 13, 17, 0, 18, 20, 2, 13, 14, 14, 5, 8, 0, 4, 12, 13, 12, 0, 10, 4, 15, 17, 19, 14, 10, 2, 0, 9, 1, 10, 9, 15, 11, 24, 5, 0, 16, 10, 13, 3, 0, 9, 22, 12, 10, 2, 22, 6, 5, 9, 16, 13, 3, 6, 12, 20, 9, 16, 11, 0, 8, 30, 11, 30, 15, 6, 7, 13, 12, 19, 27, 7, 0, 11, 10, 11, 7, 5, 7, 28, 0, 9, 13, 18, 11, 16, 6, 28, 24, 14, 4, 16, 9, 9, 18, 11, 0, 11, 7, 13, 23, 13, 8, 4, 0, 0, 10, 4, 5, 13, 5, 6, 21, 0, 12, 9, 17, 12, 11, 13, 15, 7, 3, 14, 25, 2, 23, 9, 14, 18, 15, 17, 9, 6, 7, 0, 18, 17, 6, 9, 24, 10, 3, 14, 10, 6, 0, 10, 14, 8, 12, 16, 16, 0, 16, 6, 22, 5, 7, 7, 3, 18, 4, 18, 11, 7, 4, 12, 10, 15, 5, 11, 22, 1, 25, 3, 8, 17, 8, 23, 9, 11, 15, 9, 22, 14, 16, 11, 27, 19, 13, 4, 9, 13, 6, 8, 9, 21, 3, 16, 11, 23, 7, 12, 21, 7, 23, 6, 13, 14, 18, 11, 1, 9, 11, 7, 11, 7, 10, 20, 5, 8, 5, 7, 14, 14, 7, 11, 13, 7, 5, 2, 0, 15, 5, 9, 9, 9, 8, 11, 8, 5, 4, 23, 10, 0, 6, 1, 3, 8
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      24, 27, 6, 32, 20, 17, 18, 11, 0, 21, 25, 19, 26, 10, 9, 22, 13, 10, 13, 0, 20, 0, 29, 29, 0, 16, 24, 26, 16, 18, 18, 18, 21, 16, 20, 25, 18, 23, 17, 21, 22, 14, 21, 16, 33, 25, 12, 12, 15, 29, 28, 23, 13, 24, 27, 16, 22, 23, 29, 28, 25, 23, 21, 8, 16, 30, 13, 11, 20, 22, 24, 9, 0, 18, 18, 28, 7, 5, 16, 16, 5, 12, 25, 27, 15, 17, 0, 16, 26, 36, 13, 26, 23, 22, 16, 25, 6, 25, 13, 8, 23, 23, 14, 26, 0, 2, 30, 25, 0, 15, 48, 6, 16, 41, 0, 11, 18, 29, 16, 20, 28, 20, 11, 31, 3, 18, 22, 7, 12, 21, 32, 10, 6, 22, 27, 16, 18, 21, 29, 13, 42, 3, 25, 9, 22, 15, 27, 28, 0, 12, 11, 21, 17, 20, 36, 13, 29, 23, 11, 21, 21, 15, 151, 28
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  113, 80, 120, 22, 22, 31, 19, 20, 22, 19, 20, 14, 17, 27, 6, 33, 6, 30, 28, 34, 11, 25, 0, 14, 11, 27, 18, 29, 17, 8, 24, 26, 14, 33, 21, 18, 27, 17, 14, 14, 15, 9, 13, 17, 26, 28, 26, 24, 18, 12, 14, 23, 12, 17, 18, 14, 17, 18, 19, 25, 22, 21, 17, 23, 20, 17, 16, 36, 30, 27, 17, 38, 16, 15, 25, 27, 22, 7, 23, 24, 17, 10, 30, 11, 25, 8, 23, 16, 19, 29, 22, 19, 20, 18, 32, 12, 26, 21, 6, 16, 18, 15, 23, 20, 18, 28, 20, 25, 35, 30, 13, 23, 30, 15, 26, 12, 12, 14, 25, 20, 16, 25, 12, 1, 16, 11, 26, 36, 22, 26, 21, 19, 18, 19, 21, 21, 18, 23, 12, 18, 1, 6, 17, 29, 17, 27, 21, 10, 26, 16, 24, 15, 26, 18, 32, 15, 29, 13, 16, 23, 92, 28, 30, 35, 75
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           104, 65, 47, 121, 8, 25, 11, 19, 12, 28, 11, 24, 21, 14, 15, 16, 6, 44, 31, 28, 10, 20, 13, 16, 7, 16, 15, 12, 14, 20, 8, 18, 22, 23, 25, 13, 24, 15, 16, 11, 19, 5, 25, 33, 12, 11, 82, 20, 15, 20, 20, 29, 9, 14, 26, 18, 13, 11, 25, 24, 13, 16, 25, 14, 24, 16, 26, 17, 30, 8, 18, 2, 4, 19, 23, 15, 8, 26, 18, 31, 24, 11, 18, 0, 22, 17, 19, 21, 17, 20, 16, 23, 29, 30, 17, 21, 15, 19, 17, 32, 19, 17, 13, 13, 16, 38, 12, 9, 15, 10, 17, 22, 32, 28, 26, 24, 27, 13, 22, 16, 8, 10, 19, 18, 17, 22, 20, 15, 29, 13, 21, 16, 16, 7, 26, 24, 27, 7, 11, 24, 16, 14, 23, 16, 21, 13, 15, 11, 25, 9, 29, 22, 13, 23, 12, 18, 22, 24, 22, 28, 6, 13, 3, 25, 46, 59, 116, 19
#>    TARGET_SUBJECT_PREVALENCE CONTROL_SUBJECT_PREVALENCE
#> 1                      0.966                      0.012
#> 2                      0.312                      0.000
#> 3                      0.294                      0.000
#> 4                      0.742                      0.100
#> 5                      0.300                      0.004
#> 6                      0.294                      0.002
#> 7                      0.924                      0.020
#> 8                      0.328                      0.004
#> 9                      0.330                      0.002
#> 10                     0.336                      0.006
#>    PREVALENCE_DIFFERENCE_RATIO CHI2Y CHI2Y_P_VALUE LOGITTEST LOGITTEST_P_VALUE
#> 1                        80.50  TRUE         0.001      TRUE             0.002
#> 2                       100.00  TRUE         0.001      TRUE             0.002
#> 3                       100.00  TRUE         0.001      TRUE             0.002
#> 4                         7.42  TRUE         0.001      TRUE             0.002
#> 5                        75.00  TRUE         0.001      TRUE             0.002
#> 6                       147.00  TRUE         0.001      TRUE             0.002
#> 7                        46.20  TRUE         0.001      TRUE             0.002
#> 8                        82.00  TRUE         0.001      TRUE             0.002
#> 9                       165.00  TRUE         0.001      TRUE             0.002
#> 10                       56.00  TRUE         0.001      TRUE             0.002
#>                HERITAGE
#> 1  condition_occurrence
#> 2                 death
#> 3                 death
#> 4           measurement
#> 5           observation
#> 6           observation
#> 7  procedure_occurrence
#> 8  procedure_occurrence
#> 9  procedure_occurrence
#> 10 procedure_occurrence
```

## Event-Level Data: `data_patients`

Patient-concept events used for prevalence and timing analyses.

``` r

utils::head(data$data_patients, 10)
#>    COHORT_DEFINITION_ID PERSON_ID CONCEPT_ID
#> 1                target    100001     443388
#> 2                target    100001       9201
#> 3                target    100001       9202
#> 4                target    100001    4008211
#> 5                target    100001    4008226
#> 6                target    100001    4176729
#> 7                target    100001    4032404
#> 8                target    100001    4167262
#> 9                target    100002     443388
#> 10               target    100002    4182985
#>                                              CONCEPT_NAME             HERITAGE
#> 1                                 Malignant tumor of lung condition_occurrence
#> 2                                         Inpatient Visit         visit_detail
#> 3                                        Outpatient Visit         visit_detail
#> 4                                            Radiotherapy procedure_occurrence
#> 5                                            Chemotherapy procedure_occurrence
#> 6  Treatment planning for external beam radiation therapy procedure_occurrence
#> 7                                            Bronchoscopy procedure_occurrence
#> 8                                   Needle biopsy of lung procedure_occurrence
#> 9                                 Malignant tumor of lung condition_occurrence
#> 10                             Diffusion capacity of lung          measurement
#>    ABSTRACTION_LEVEL PREVALENCE  TIME_TO_EVENT
#> 1                 -1          1              4
#> 2                 -1          2         32, 52
#> 3                 -1          3     34, 36, 64
#> 4                 -1          3     22, 25, 42
#> 5                 -1          4 24, 31, 33, 39
#> 6                 -1          1             34
#> 7                 -1          2          0, 15
#> 8                 -1          2          2, 18
#> 9                 -1          2           1, 6
#> 10                -1          2         11, 23
```

## Applied Mappings: `complementaryMappingTable`

Manual or automatic concept merges recorded during the workflow.

``` r

utils::head(data$complementaryMappingTable, 10)
#> [1] CONCEPT_ID       CONCEPT_NAME     NEW_CONCEPT_ID   NEW_CONCEPT_NAME
#> [5] TYPE             HERITAGE        
#> <0 rows> (or 0-length row.names)
```

## Final Selected Features: `selectedFeatureData$selectedFeatures`

The final selected feature set used for downstream trajectory and viewer
analysis.

``` r

if (is.data.frame(data$selectedFeatureData$selectedFeatures)) {
  utils::head(data$selectedFeatureData$selectedFeatures, 10)
} else {
  cat("No selected feature table available.\n")
}
#>    CONCEPT_ID               CONCEPT_NAME ABSTRACTION_LEVEL TARGET_SUBJECT_COUNT
#> 1      443388    Malignant tumor of lung                -1                  483
#> 2       32280                      Death                -1                  156
#> 3       32815          Death Certificate                -1                  147
#> 4     4182985 Diffusion capacity of lung                -1                  371
#> 5     4306655                      Death                -1                  150
#> 6     4014023            Palliative care                -1                  147
#> 7     4032404               Bronchoscopy                -1                  462
#> 8     4008226               Chemotherapy                -1                  164
#> 9     2107967                  Lobectomy                -1                  165
#> 10    2107968          Lobectomy of lung                -1                  168
#>    CONTROL_SUBJECT_COUNT
#> 1                      6
#> 2                      0
#> 3                      0
#> 4                     50
#> 5                      2
#> 6                      1
#> 7                     10
#> 8                      2
#> 9                      1
#> 10                     3
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                TIME_TO_EVENT
#> 1                                                                                                                      4, 1, 4, 2, 6, 0, 6, 5, 5, 3, 5, 5, 8, 7, 6, 8, 4, 0, 10, 2, 7, 0, 7, 5, 2, 1, 9, 3, 4, 3, 3, 14, 2, 6, 2, 3, 6, 0, 8, 10, 5, 14, 0, 6, 5, 11, 6, 7, 6, 8, 4, 7, 0, 5, 7, 9, 8, 9, 0, 6, 8, 6, 4, 3, 16, 11, 4, 4, 5, 11, 3, 6, 6, 5, 8, 0, 2, 1, 4, 4, 5, 3, 5, 9, 10, 4, 6, 6, 10, 3, 4, 0, 5, 4, 0, 8, 5, 9, 5, 14, 0, 0, 4, 10, 5, 4, 13, 3, 1, 5, 2, 3, 0, 4, 11, 7, 0, 0, 13, 6, 4, 8, 6, 8, 9, 5, 0, 2, 4, 3, 13, 1, 5, 7, 3, 9, 5, 0, 3, 10, 3, 7, 8, 8, 6, 1, 3, 10, 8, 8, 4, 0, 15, 9, 0, 5, 4, 11, 7, 8, 6, 4, 1, 12, 0, 1, 3, 1, 12, 0, 6, 5, 11, 2, 0, 6, 2, 0, 6, 5, 6, 2, 11, 9, 1, 8, 3, 3, 5, 12, 3, 1, 6, 10, 8, 10, 4, 8, 3, 9, 11, 5, 3, 7, 7, 8, 2, 5, 10, 7, 4, 1, 5, 9, 6, 4, 2, 2, 13, 9, 16, 1, 2, 7, 3, 4, 3, 10, 2, 6, 7, 7, 6, 6, 3, 6, 4, 0, 6, 5, 10, 2, 9, 7, 3, 0, 6, 4, 10, 2, 5, 8, 1, 0, 2, 6, 5, 6, 3, 9, 5, 10, 11, 6, 6, 8, 0, 1, 8, 0, 0, 1, 1, 0, 2, 3, 5, 1, 4, 5, 6, 6, 9, 5, 1, 0, 0, 5, 11, 0, 7, 4, 2, 0, 5, 5, 11, 0, 3, 3, 2, 5, 7, 5, 7, 1, 6, 5, 3, 4, 6, 1, 10, 1, 4, 7, 3, 16, 4, 5, 0, 5, 5, 6, 3, 2, 8, 7, 3, 3, 0, 7, 10, 8, 3, 0, 7, 5, 8, 3, 3, 6, 5, 4, 6, 12, 6, 5, 3, 11, 7, 9, 14, 4, 0, 10, 1, 6, 11, 10, 2, 6, 2, 0, 7, 2, 0, 2, 1, 1, 5, 2, 3, 2, 7, 4, 6, 0, 7, 4, 10, 8, 5, 6, 1, 0, 18, 2, 2, 0, 2, 1, 10, 2, 9, 2, 4, 5, 5, 0, 0, 4, 4, 2, 13, 4, 13, 5, 6, 8, 3, 4, 7, 7, 1, 11, 13, 10, 3, 6, 1, 8, 2, 7, 8, 1, 3, 5, 4, 10, 2, 6, 0, 4, 2, 8, 1, 7, 0, 3, 6, 0, 10, 1, 8, 3, 7, 4, 1, 4, 4, 9, 1, 5, 3, 7, 9, 7, 6, 2, 7, 11, 3, 0, 7, 10, 1, 4, 13, 0, 9, 4, 2, 5, 4, 7, 13, 0, 0, 11, 2, 0, 2
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  11, 244, 150, 165, 204, 148, 224, 191, 199, 214, 170, 212, 213, 200, 206, 190, 259, 192, 193, 232, 187, 191, 219, 180, 193, 157, 178, 158, 175, 175, 214, 183, 189, 201, 202, 194, 185, 166, 180, 212, 171, 171, 198, 193, 214, 224, 170, 168, 145, 187, 171, 213, 224, 203, 171, 209, 212, 195, 205, 270, 216, 213, 215, 183, 167, 178, 192, 152, 219, 177, 175, 190, 192, 193, 193, 173, 185, 250, 177, 160, 183, 173, 204, 174, 194, 189, 192, 186, 264, 181, 123, 176, 193, 260, 208, 226, 224, 211, 202, 243, 214, 187, 201, 217, 206, 164, 272, 189, 224, 167, 208, 216, 156, 151, 156, 199, 205, 175, 192, 170, 169, 224, 126, 176, 190, 187, 196, 173, 175, 209, 174, 150, 198, 232, 196, 178, 172, 219, 199, 243, 235, 190, 187, 217, 230, 173, 185, 212, 216, 203, 223, 211, 192, 168, 206, 200
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 19, 90, 27, 220, 225, 267, 272, 244, 262, 299, 258, 245, 250, 247, 235, 237, 257, 207, 235, 224, 253, 254, 225, 248, 251, 235, 261, 253, 245, 230, 276, 230, 240, 237, 255, 256, 245, 260, 200, 224, 241, 240, 280, 113, 240, 192, 220, 227, 279, 255, 253, 242, 258, 234, 249, 198, 235, 263, 234, 259, 247, 260, 265, 226, 258, 214, 247, 273, 276, 248, 229, 227, 258, 235, 262, 253, 249, 279, 220, 248, 213, 292, 267, 247, 229, 249, 257, 255, 230, 231, 262, 226, 233, 268, 252, 244, 264, 211, 263, 249, 231, 243, 254, 267, 232, 250, 242, 271, 257, 217, 266, 251, 213, 256, 243, 286, 256, 281, 249, 264, 262, 226, 248, 262, 243, 254, 281, 235, 223, 276, 230, 293, 261, 245, 222, 220, 255, 237, 233, 268, 244, 265, 269, 235, 251, 255, 253
#> 4                                                                                                                                                                                                                                11, 18, 23, 23, 2, 13, 19, 48, 23, 21, 0, 28, 14, 17, 0, 42, 9, 18, 0, 28, 13, 0, 6, 27, 12, 16, 24, 21, 34, 29, 16, 28, 25, 18, 18, 20, 20, 21, 27, 30, 23, 17, 1, 30, 13, 9, 13, 16, 15, 23, 6, 40, 21, 24, 22, 10, 12, 37, 2, 18, 27, 15, 24, 26, 7, 10, 19, 19, 16, 26, 0, 7, 10, 12, 24, 9, 11, 13, 20, 24, 0, 33, 7, 2, 0, 32, 2, 8, 24, 14, 34, 14, 33, 4, 3, 22, 25, 9, 30, 40, 35, 20, 6, 11, 36, 18, 33, 22, 2, 14, 15, 24, 16, 24, 0, 16, 11, 7, 7, 27, 9, 11, 28, 20, 15, 22, 17, 8, 33, 18, 15, 3, 11, 9, 28, 13, 25, 29, 9, 20, 28, 18, 2, 7, 15, 19, 29, 22, 25, 8, 34, 15, 7, 23, 23, 23, 9, 20, 21, 23, 7, 14, 28, 14, 26, 16, 32, 19, 0, 18, 30, 1, 7, 26, 28, 2, 33, 24, 23, 0, 6, 34, 8, 29, 18, 26, 6, 21, 17, 12, 11, 39, 12, 9, 17, 10, 37, 28, 24, 28, 32, 9, 26, 13, 18, 15, 28, 18, 29, 15, 40, 22, 3, 12, 37, 32, 7, 0, 20, 10, 38, 0, 29, 25, 9, 29, 27, 25, 34, 31, 31, 26, 22, 1, 14, 15, 16, 13, 18, 21, 0, 13, 9, 23, 15, 27, 19, 28, 5, 14, 5, 37, 8, 36, 16, 0, 26, 31, 43, 33, 20, 0, 52, 30, 32, 25, 0, 13, 10, 23, 1, 40, 5, 20, 18, 12, 14, 18, 17, 18, 28, 16, 32, 24, 3, 15, 22, 23, 20, 16, 0, 23, 24, 2, 31, 56, 23, 24, 7, 18, 18, 0, 38, 0, 11, 21, 9, 25, 28, 42, 18, 3, 23, 25, 32, 4, 23, 22, 30, 20, 11, 21, 0, 9, 27, 16, 5, 39, 26, 21, 38, 16, 11, 48, 16, 16, 22, 45, 19, 11, 29, 0, 22, 23, 19, 31, 12, 18, 25, 19, 31, 24, 34, 21, 27, 19, 29, 35, 33, 19, 30, 12, 13, 11, 24, 33, 33, 29, 13, 29, 29
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  69, 53, 100, 89, 225, 216, 206, 192, 182, 171, 172, 208, 116, 177, 165, 217, 168, 221, 192, 199, 213, 189, 262, 178, 175, 208, 191, 189, 163, 132, 212, 226, 201, 232, 161, 187, 174, 139, 195, 181, 157, 152, 240, 177, 156, 210, 187, 213, 144, 212, 198, 177, 191, 212, 187, 189, 199, 218, 144, 226, 140, 139, 206, 167, 214, 224, 194, 221, 196, 223, 209, 224, 192, 195, 228, 170, 228, 205, 178, 217, 172, 199, 191, 212, 227, 209, 195, 209, 201, 214, 171, 232, 177, 181, 193, 163, 203, 177, 209, 177, 237, 164, 198, 167, 255, 213, 171, 244, 197, 236, 171, 206, 220, 217, 181, 201, 176, 193, 228, 170, 148, 197, 217, 180, 188, 200, 167, 234, 181, 234, 230, 189, 255, 151, 193, 154, 209, 244, 209, 203, 194, 230, 159, 281, 170, 300, 206, 200, 158, 182
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 121, 39, 63, 209, 224, 283, 221, 201, 165, 180, 192, 226, 232, 217, 183, 152, 229, 166, 233, 226, 216, 234, 215, 250, 202, 266, 223, 261, 175, 268, 183, 184, 244, 186, 242, 235, 199, 227, 159, 226, 265, 234, 217, 214, 276, 241, 203, 209, 197, 202, 257, 164, 267, 300, 136, 186, 168, 173, 250, 237, 201, 166, 269, 217, 228, 202, 198, 212, 228, 224, 166, 180, 187, 199, 220, 185, 229, 236, 206, 220, 168, 204, 222, 187, 209, 165, 204, 242, 190, 162, 250, 227, 190, 210, 248, 176, 209, 262, 193, 170, 243, 162, 75, 227, 219, 190, 207, 195, 193, 189, 220, 217, 222, 219, 219, 194, 226, 224, 190, 213, 193, 169, 195, 216, 189, 206, 226, 194, 176, 192, 199, 234, 252, 217, 173, 214, 255, 171, 263, 251, 203, 172, 208, 196, 161, 214, 226
#> 7  0, 3, 5, 7, 7, 16, 7, 29, 0, 7, 10, 5, 4, 2, 8, 12, 4, 16, 12, 17, 10, 16, 0, 7, 14, 8, 12, 11, 18, 10, 3, 2, 13, 15, 6, 21, 10, 23, 15, 13, 5, 14, 20, 10, 10, 10, 5, 0, 11, 2, 9, 0, 8, 11, 11, 25, 8, 3, 0, 0, 4, 0, 16, 16, 0, 21, 21, 12, 0, 16, 1, 11, 12, 17, 8, 16, 7, 13, 9, 12, 4, 19, 3, 16, 17, 7, 15, 8, 10, 13, 12, 12, 16, 12, 9, 6, 9, 17, 7, 19, 11, 17, 7, 11, 10, 18, 10, 9, 0, 19, 8, 6, 18, 0, 11, 9, 3, 8, 31, 0, 0, 17, 16, 0, 9, 4, 12, 20, 0, 1, 6, 9, 13, 12, 0, 11, 17, 2, 14, 0, 7, 16, 19, 6, 7, 4, 1, 16, 10, 2, 25, 0, 3, 1, 11, 14, 0, 13, 11, 9, 14, 9, 10, 5, 0, 18, 18, 8, 31, 7, 11, 10, 8, 13, 10, 1, 0, 0, 14, 17, 16, 8, 8, 10, 0, 4, 17, 5, 19, 9, 11, 5, 2, 11, 22, 16, 15, 11, 5, 10, 7, 0, 13, 4, 15, 0, 8, 24, 12, 7, 15, 0, 7, 13, 17, 0, 18, 20, 2, 13, 14, 14, 5, 8, 0, 4, 12, 13, 12, 0, 10, 4, 15, 17, 19, 14, 10, 2, 0, 9, 1, 10, 9, 15, 11, 24, 5, 0, 16, 10, 13, 3, 0, 9, 22, 12, 10, 2, 22, 6, 5, 9, 16, 13, 3, 6, 12, 20, 9, 16, 11, 0, 8, 30, 11, 30, 15, 6, 7, 13, 12, 19, 27, 7, 0, 11, 10, 11, 7, 5, 7, 28, 0, 9, 13, 18, 11, 16, 6, 28, 24, 14, 4, 16, 9, 9, 18, 11, 0, 11, 7, 13, 23, 13, 8, 4, 0, 0, 10, 4, 5, 13, 5, 6, 21, 0, 12, 9, 17, 12, 11, 13, 15, 7, 3, 14, 25, 2, 23, 9, 14, 18, 15, 17, 9, 6, 7, 0, 18, 17, 6, 9, 24, 10, 3, 14, 10, 6, 0, 10, 14, 8, 12, 16, 16, 0, 16, 6, 22, 5, 7, 7, 3, 18, 4, 18, 11, 7, 4, 12, 10, 15, 5, 11, 22, 1, 25, 3, 8, 17, 8, 23, 9, 11, 15, 9, 22, 14, 16, 11, 27, 19, 13, 4, 9, 13, 6, 8, 9, 21, 3, 16, 11, 23, 7, 12, 21, 7, 23, 6, 13, 14, 18, 11, 1, 9, 11, 7, 11, 7, 10, 20, 5, 8, 5, 7, 14, 14, 7, 11, 13, 7, 5, 2, 0, 15, 5, 9, 9, 9, 8, 11, 8, 5, 4, 23, 10, 0, 6, 1, 3, 8
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      24, 27, 6, 32, 20, 17, 18, 11, 0, 21, 25, 19, 26, 10, 9, 22, 13, 10, 13, 0, 20, 0, 29, 29, 0, 16, 24, 26, 16, 18, 18, 18, 21, 16, 20, 25, 18, 23, 17, 21, 22, 14, 21, 16, 33, 25, 12, 12, 15, 29, 28, 23, 13, 24, 27, 16, 22, 23, 29, 28, 25, 23, 21, 8, 16, 30, 13, 11, 20, 22, 24, 9, 0, 18, 18, 28, 7, 5, 16, 16, 5, 12, 25, 27, 15, 17, 0, 16, 26, 36, 13, 26, 23, 22, 16, 25, 6, 25, 13, 8, 23, 23, 14, 26, 0, 2, 30, 25, 0, 15, 48, 6, 16, 41, 0, 11, 18, 29, 16, 20, 28, 20, 11, 31, 3, 18, 22, 7, 12, 21, 32, 10, 6, 22, 27, 16, 18, 21, 29, 13, 42, 3, 25, 9, 22, 15, 27, 28, 0, 12, 11, 21, 17, 20, 36, 13, 29, 23, 11, 21, 21, 15, 151, 28
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  113, 80, 120, 22, 22, 31, 19, 20, 22, 19, 20, 14, 17, 27, 6, 33, 6, 30, 28, 34, 11, 25, 0, 14, 11, 27, 18, 29, 17, 8, 24, 26, 14, 33, 21, 18, 27, 17, 14, 14, 15, 9, 13, 17, 26, 28, 26, 24, 18, 12, 14, 23, 12, 17, 18, 14, 17, 18, 19, 25, 22, 21, 17, 23, 20, 17, 16, 36, 30, 27, 17, 38, 16, 15, 25, 27, 22, 7, 23, 24, 17, 10, 30, 11, 25, 8, 23, 16, 19, 29, 22, 19, 20, 18, 32, 12, 26, 21, 6, 16, 18, 15, 23, 20, 18, 28, 20, 25, 35, 30, 13, 23, 30, 15, 26, 12, 12, 14, 25, 20, 16, 25, 12, 1, 16, 11, 26, 36, 22, 26, 21, 19, 18, 19, 21, 21, 18, 23, 12, 18, 1, 6, 17, 29, 17, 27, 21, 10, 26, 16, 24, 15, 26, 18, 32, 15, 29, 13, 16, 23, 92, 28, 30, 35, 75
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           104, 65, 47, 121, 8, 25, 11, 19, 12, 28, 11, 24, 21, 14, 15, 16, 6, 44, 31, 28, 10, 20, 13, 16, 7, 16, 15, 12, 14, 20, 8, 18, 22, 23, 25, 13, 24, 15, 16, 11, 19, 5, 25, 33, 12, 11, 82, 20, 15, 20, 20, 29, 9, 14, 26, 18, 13, 11, 25, 24, 13, 16, 25, 14, 24, 16, 26, 17, 30, 8, 18, 2, 4, 19, 23, 15, 8, 26, 18, 31, 24, 11, 18, 0, 22, 17, 19, 21, 17, 20, 16, 23, 29, 30, 17, 21, 15, 19, 17, 32, 19, 17, 13, 13, 16, 38, 12, 9, 15, 10, 17, 22, 32, 28, 26, 24, 27, 13, 22, 16, 8, 10, 19, 18, 17, 22, 20, 15, 29, 13, 21, 16, 16, 7, 26, 24, 27, 7, 11, 24, 16, 14, 23, 16, 21, 13, 15, 11, 25, 9, 29, 22, 13, 23, 12, 18, 22, 24, 22, 28, 6, 13, 3, 25, 46, 59, 116, 19
#>    TARGET_SUBJECT_PREVALENCE CONTROL_SUBJECT_PREVALENCE
#> 1                      0.966                      0.012
#> 2                      0.312                      0.000
#> 3                      0.294                      0.000
#> 4                      0.742                      0.100
#> 5                      0.300                      0.004
#> 6                      0.294                      0.002
#> 7                      0.924                      0.020
#> 8                      0.328                      0.004
#> 9                      0.330                      0.002
#> 10                     0.336                      0.006
#>    PREVALENCE_DIFFERENCE_RATIO CHI2Y CHI2Y_P_VALUE LOGITTEST LOGITTEST_P_VALUE
#> 1                        80.50  TRUE         0.001      TRUE             0.002
#> 2                       100.00  TRUE         0.001      TRUE             0.002
#> 3                       100.00  TRUE         0.001      TRUE             0.002
#> 4                         7.42  TRUE         0.001      TRUE             0.002
#> 5                        75.00  TRUE         0.001      TRUE             0.002
#> 6                       147.00  TRUE         0.001      TRUE             0.002
#> 7                        46.20  TRUE         0.001      TRUE             0.002
#> 8                        82.00  TRUE         0.001      TRUE             0.002
#> 9                       165.00  TRUE         0.001      TRUE             0.002
#> 10                       56.00  TRUE         0.001      TRUE             0.002
#>                HERITAGE
#> 1  condition_occurrence
#> 2                 death
#> 3                 death
#> 4           measurement
#> 5           observation
#> 6           observation
#> 7  procedure_occurrence
#> 8  procedure_occurrence
#> 9  procedure_occurrence
#> 10 procedure_occurrence
```
