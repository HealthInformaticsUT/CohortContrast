library(testthat)
library(CohortContrast)

test_that("Created features table is correct.", {
  pathToResults <<- getwd() #pathToResults = paste(getwd(), "/tests",sep="")

  ################################################################################
  #
  # Database credentials
  #
  ################################################################################
  pathToJSON <- paste(pathToResults, '/inst/JSON/', sep = '')

  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))

  cdm <- CDMConnector::cdm_from_con(db, cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")

  targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
  controlTable <- createControlCohortInverse(cdm = cdm, targetTable = targetTable)

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
    domainsIncluded = c("Drug"),
    prevalenceCutOff = 0,
    topK = 15, # Number of features to export
    presenceFilter = FALSE, # 0-1, percentage of people who must have the chosen feature present
    complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
    runZTests = FALSE,
    runLogitTests = FALSE,
    createOutputFiles = FALSE,
    numCores = 1)

  expect_equal(length(data$trajectoryDataList$selectedFeatures$CONCEPT_NAME) == 15, TRUE)
  expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", 4]) == 830, TRUE)
  expect_equal(nrow(data$data_initial) == 2490, TRUE)
  expect_equal(nrow(data$data_person) == 830, TRUE)
  expect_equal(nrow(data$data_patients) == 3199, TRUE)



  controlTable <- createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 5)
  controlTable <- controlTable %>% dplyr::mutate(cohort_end_date = cohort_start_date + 10)

  ################################################################################
  #
  # Run the study
  #
  ################################################################################
  data = CohortContrast(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable,
    pathToResults =getwd(), #paste(getwd(), '/tests/testthat', sep = ''),
    domainsIncluded = c(
      "Drug",
      "Condition",
      "Measurement",
      "Observation",
      "Procedure",
      "Visit",
      "Visit detail"
    ),
    prevalenceCutOff = 0,
    topK = FALSE, # Number of features to export
    presenceFilter = FALSE, # 0-1, percentage of people who must have the chosen feature present
    complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
    runZTests = TRUE,
    runLogitTests = TRUE,
    createOutputFiles = FALSE,
    numCores = 1)

  expect_equal(sum(data$data_features$ZTEST) == 1, TRUE)
  expect_equal(sum(data$data_features$LOGITTEST) == 0, TRUE)

  DBI::dbDisconnect(db)
})
#> Test passed 🥇
