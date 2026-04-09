library(CohortContrast)

test_that("automaticHierarchyCombineConcepts does not map parent to itself or misalign concept names", {
  study <- structure(
    list(
      data_initial = data.frame(
        COHORT_DEFINITION_ID = c("target", "target", "control", "control"),
        SUBJECT_ID = 1:4,
        COHORT_START_DATE = as.Date(rep("2020-01-01", 4)),
        COHORT_END_DATE = as.Date(rep("2020-01-10", 4))
      ),
      data_patients = data.frame(
        COHORT_DEFINITION_ID = c(
          "target", "target", "target", "target", "target", "target",
          "control", "control", "control", "control", "control", "control"
        ),
        PERSON_ID = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
        CONCEPT_ID = c(1, 2, 10, 1, 2, 10, 1, 2, 10, 1, 2, 10),
        CONCEPT_NAME = c(
          "Concept A", "Concept B", "Parent concept",
          "Concept A", "Concept B", "Parent concept",
          "Concept A", "Concept B", "Parent concept",
          "Concept A", "Concept B", "Parent concept"
        ),
        HERITAGE = rep("drug_exposure", 12),
        ABSTRACTION_LEVEL = rep(-1, 12),
        PREVALENCE = rep(1, 12),
        TIME_TO_EVENT = I(rep(list(c(0, 2)), 12))
      ),
      data_features = data.frame(
        CONCEPT_ID = c(1, 2, 10),
        CONCEPT_NAME = c("Concept A", "Concept B", "Parent concept"),
        ABSTRACTION_LEVEL = c(-1, -1, -1),
        TARGET_SUBJECT_COUNT = c(2, 2, 2),
        CONTROL_SUBJECT_COUNT = c(2, 2, 2),
        TIME_TO_EVENT = I(list(c(0, 2), c(0, 2), c(0, 2))),
        TARGET_SUBJECT_PREVALENCE = c(1, 1, 1),
        CONTROL_SUBJECT_PREVALENCE = c(1, 1, 1),
        PREVALENCE_DIFFERENCE_RATIO = c(1, 1, 1),
        CHI2Y = c(TRUE, TRUE, TRUE),
        CHI2Y_P_VALUE = c(1, 1, 1),
        LOGITTEST = c(FALSE, FALSE, FALSE),
        LOGITTEST_P_VALUE = c(1, 1, 1),
        HERITAGE = c("drug_exposure", "drug_exposure", "drug_exposure")
      ),
      data_person = data.frame(),
      conceptsData = list(
        concept = data.frame(
          concept_id = c(1, 2, 10),
          concept_name = c("Concept A", "Concept B", "Parent concept"),
          invalid_reason = c(NA, NA, NA)
        ),
        concept_ancestor = data.frame(
          ancestor_concept_id = c(1, 2, 10, 10, 10),
          descendant_concept_id = c(1, 2, 10, 1, 2),
          min_levels_of_separation = c(0, 0, 0, 1, 1),
          max_levels_of_separation = c(0, 0, 0, 1, 1)
        )
      ),
      complementaryMappingTable = data.frame(
        CONCEPT_ID = integer(),
        CONCEPT_NAME = character(),
        NEW_CONCEPT_ID = integer(),
        NEW_CONCEPT_NAME = character(),
        ABSTRACTION_LEVEL = integer(),
        TYPE = character()
      )
    ),
    class = "CohortContrastObject"
  )

  combined <- suppressWarnings(
    automaticHierarchyCombineConcepts(study, abstractionLevel = -1)
  )

  expect_equal(combined$data_features$CONCEPT_ID, 10)
  expect_equal(combined$data_features$CONCEPT_NAME, "Parent concept")

  mapping <- combined$complementaryMappingTable[order(combined$complementaryMappingTable$CONCEPT_ID), ]

  expect_equal(mapping$CONCEPT_ID, c(1, 2))
  expect_equal(mapping$CONCEPT_NAME, c("Concept A", "Concept B"))
  expect_equal(mapping$NEW_CONCEPT_ID, c(10, 10))
  expect_equal(mapping$NEW_CONCEPT_NAME, c("Parent concept", "Parent concept"))
})
