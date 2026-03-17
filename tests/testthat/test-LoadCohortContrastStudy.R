library(testthat)
library(CohortContrast)

test_that("loadCohortContrastStudy reloads saved study state", {
  skip_if_not_installed("nanoparquet")

  root <- tempfile("cc_load_")
  dir.create(root, recursive = TRUE)

  study <- "ReloadStudy"
  study_obj <- structure(
    list(
      data_patients = data.frame(
        COHORT_DEFINITION_ID = c("target", "target", "control"),
        PERSON_ID = c(1L, 1L, 2L),
        CONCEPT_ID = c(101L, 102L, 101L),
        CONCEPT_NAME = c("Concept A", "Concept B", "Concept A"),
        PREVALENCE = c(1, 1, 1),
        HERITAGE = c("drug_exposure", "drug_exposure", "drug_exposure"),
        TIME_TO_EVENT = I(list(c(1, 2), c(3), c(4))),
        ABSTRACTION_LEVEL = c(-1L, -1L, -1L),
        stringsAsFactors = FALSE
      ),
      data_initial = data.frame(
        COHORT_DEFINITION_ID = c("target", "control"),
        SUBJECT_ID = c(1L, 2L),
        COHORT_START_DATE = as.Date(c("2020-01-01", "2020-01-01")),
        COHORT_END_DATE = as.Date(c("2020-12-31", "2020-12-31")),
        stringsAsFactors = FALSE
      ),
      data_person = data.frame(
        PERSON_ID = c(1L, 2L),
        GENDER_CONCEPT_ID = c(8507L, 8532L),
        YEAR_OF_BIRTH = c(1980L, 1975L),
        stringsAsFactors = FALSE
      ),
      data_features = data.frame(
        CONCEPT_ID = c(101L, 102L),
        CONCEPT_NAME = c("Concept A", "Concept B"),
        ABSTRACTION_LEVEL = c(-1L, -1L),
        HERITAGE = c("drug_exposure", "drug_exposure"),
        PREVALENCE_DIFFERENCE_RATIO = c(2, 1.5),
        CHI2Y = c(TRUE, FALSE),
        LOGITTEST = c(FALSE, FALSE),
        TIME_TO_EVENT = I(list(c(1, 2, 4), c(3))),
        stringsAsFactors = FALSE
      ),
      complementaryMappingTable = data.frame(
        CONCEPT_ID = integer(),
        CONCEPT_NAME = character(),
        NEW_CONCEPT_ID = integer(),
        NEW_CONCEPT_NAME = character(),
        ABSTRACTION_LEVEL = integer(),
        TYPE = character(),
        stringsAsFactors = FALSE
      ),
      selectedFeatureData = list(
        selectedFeatureNames = c("Concept A"),
        selectedFeatureIds = c(101L),
        selectedFeatures = data.frame(
          CONCEPT_ID = c(101L),
          CONCEPT_NAME = c("Concept A"),
          ABSTRACTION_LEVEL = c(-1L),
          PREVALENCE_DIFFERENCE_RATIO = c(2),
          CHI2Y = c(TRUE),
          LOGITTEST = c(FALSE),
          stringsAsFactors = FALSE
        )
      ),
      trajectoryDataList = NULL,
      config = list(
        complName = study,
        topK = FALSE,
        prevalenceCutOff = 1,
        runChi2YTests = TRUE,
        runLogitTests = FALSE,
        patientLevelData = TRUE
      ),
      conceptsData = list(
        concept_ancestor = data.frame(
          ancestor_concept_id = c(1L),
          descendant_concept_id = c(101L),
          min_levels_of_separation = c(1L),
          max_levels_of_separation = c(2L),
          stringsAsFactors = FALSE
        ),
        concept = data.frame(
          concept_id = c(101L),
          concept_name = c("Concept A"),
          invalid_reason = c(NA_character_),
          stringsAsFactors = FALSE
        )
      )
    ),
    class = "CohortContrastObject"
  )
  study_obj$trajectoryDataList <- study_obj$selectedFeatureData

  CohortContrast:::saveResult(study_obj, root)

  loaded <- loadCohortContrastStudy(studyName = study, pathToResults = root)

  expect_s3_class(loaded, "CohortContrastObject")
  expect_equal(loaded$config$complName, study)
  expect_true(is.data.frame(loaded$data_patients))
  expect_true(is.data.frame(loaded$data_features))
  expect_equal(loaded$selectedFeatureData$selectedFeatureIds, c(101L))
  expect_equal(loaded$selectedFeatureData$selectedFeatureNames, c("Concept A"))
  expect_true(is.list(loaded$data_patients$TIME_TO_EVENT))
  expect_true(is.list(loaded$conceptsData))
  expect_true(is.data.frame(loaded$conceptsData$concept))
})
