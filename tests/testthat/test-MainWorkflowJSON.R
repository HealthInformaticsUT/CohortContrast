library(testthat)
library(CohortContrast)

test_that("JSON workflow", {
  pathToResults <<- getwd() #pathToResults = paste(getwd(), "/tests",sep="")

  ################################################################################
  #
  # Database credentials
  #
  ################################################################################
  pathToJSON <- paste(pathToResults, '/inst/JSON/', sep = '')

  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir("GiBleed"))

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
  #expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", 4]) == 830, TRUE)
  expect_equal(nrow(data$data_initial) == nrow(targetTable) + nrow(controlTable), TRUE)
  expect_equal(nrow(data$data_person) == rbind(targetTable, controlTable) %>% dplyr::select(subject_id) %>% dplyr::distinct() %>% nrow(), TRUE)
  expect_equal(nrow(data$data_patients) == 3199, TRUE)



  controlTable <- createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 5)
  controlTable <- controlTable %>% dplyr::mutate(cohort_end_date = cohort_start_date + 10)
  controlTable <- resolveCohortTableOverlaps(cohortTable = controlTable, cdm = cdm)

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

  expect_gte(sum(data$data_features$ZTEST),0)
  expect_gte(sum(data$data_features$LOGITTEST),0)

  DBI::dbDisconnect(db)
})
#> Test passed 🥇
