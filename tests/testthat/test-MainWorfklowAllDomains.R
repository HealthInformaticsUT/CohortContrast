library(testthat)
library(CohortContrast)

testthat::skip_on_cran()
skip_if_no_integration()

test_that("All domains workflow", {
  pathToResults <<- getwd() #pathToResults = paste(getwd(), "/tests",sep="")

  ################################################################################
  #
  # Database credentials
  #
  ################################################################################
  pathToJSON <- paste(pathToResults, '/inst/JSON/', sep = '')

  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir("GiBleed"))

  cdm <- CDMConnector::cdmFromCon(con = db, cdmName = "eunomia", cdmSchema = "main", writeSchema = "main")

  targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
  targetTable$cohort_end_date <- targetTable$cohort_end_date + 30
  targetTable <- resolveCohortTableOverlaps(targetTable, cdm)

  controlTable <- targetTable
  controlTable$cohort_start_date <- controlTable$cohort_start_date - 365
  controlTable$cohort_end_date <- controlTable$cohort_end_date - 365
  controlTable <- resolveCohortTableOverlaps(controlTable, cdm)

  ################################################################################
  #
  # Run the study
  #
  ################################################################################
  data = CohortContrast(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable,
    pathToResults =  getwd(),
    domainsIncluded =  c("Drug", "Condition", "Measurement", "Observation",
                         "Procedure", "Visit", "Visit detail", "Death"),
    prevalenceCutOff = 0,
    presenceFilter = FALSE, # 0-1, percentage of people who must have the chosen feature present
    complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
    runChi2YTests = TRUE,
    runLogitTests = TRUE,
    createOutputFiles = FALSE,
    numCores = 1)

  expect_equal(length(data$selectedFeatureData$selectedFeatures$CONCEPT_NAME) == 5, TRUE)
  expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", "TARGET_SUBJECT_COUNT"]) == 830, TRUE)
  expect_equal(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", "CHI2Y"] == TRUE, TRUE)
  expect_equal(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", "LOGITTEST"] == FALSE, TRUE)
  expect_equal(length(data$data_patients[data$data_patients$CONCEPT_NAME == "Gastrointestinal hemorrhage", "TIME_TO_EVENT"]), 46)
  expect_equal(nrow(data$data_initial) == nrow(targetTable) + nrow(controlTable), TRUE)
  expect_equal(nrow(data$data_person) == nrow(dplyr::distinct(dplyr::select(rbind(targetTable, controlTable), subject_id))), TRUE)
  expect_equal(nrow(data$data_patients) == 1976, TRUE)

  DBI::dbDisconnect(db)
})
#> Test passed 🥇
