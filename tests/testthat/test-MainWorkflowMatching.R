library(testthat)
library(CohortContrast)

test_that("Created features table is correct with JSON & patient matching", {
  pathToResults <<- getwd() #pathToResults = paste(getwd(), "/tests",sep="")
  pathToResults = pathToResults = paste(getwd(), "/tests/testthat",sep="")
  ################################################################################
  #
  # Database credentials
  #
  ################################################################################

  target <- readr::read_csv('./inst/CSV/target/target.csv')
  db  <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
  DBI::dbExecute(db , "CREATE SCHEMA IF NOT EXISTS testthat")
  DBI::dbWriteTable(db ,   DBI::SQL('"testthat"."target_mock"'), target)

  cdm <- CDMConnector::cdm_from_con(db , cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")

  targetTable <- cohortFromCohortTable(cdm = cdm, db = db, tableName = "target_mock", schemaName = 'testthat')
  controlTable <- createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 3)

  ################################################################################
  #
  # Run the study
  #
  ################################################################################
  data = CohortContrast(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable,
    pathToResults = getwd(),
    domainsIncluded = c("Drug", "Measurement", "Observation"),
    prevalenceCutOff = 0,
    topK = 15, # Number of features to export
    presenceFilter = FALSE, # 0-1, percentage of people who must have the chosen feature present
    complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
    runZTests = FALSE,
    runLogitTests = FALSE,
   createOutputFiles = FALSE,
   numCores = 1)

  expect_equal(length(data$trajectoryDataList$selectedFeatures$CONCEPT_NAME) == 15, TRUE)
#  expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", 3]) == 2, TRUE)
#  expect_equal(nrow(data$trajectoryDataList$trajectoryData) == 73, TRUE)
  expect_equal(nrow(data$data_initial) == nrow(targetTable) + nrow(controlTable), TRUE)
  expect_equal(nrow(data$data_person) == nrow(targetTable) + nrow(controlTable), TRUE)
#  expect_equal(nrow(data$data_patients) == 60, TRUE)
  DBI::dbDisconnect(db)
})
#> Test passed 🥇
test_that("Created features table is correct with Cohorts table & patient matching.", {
  pathToResults <<- dirname(dirname(getwd())) #pathToResults = paste(getwd(), "/tests",sep="")

  ################################################################################
  #
  # Database credentials
  #
  ################################################################################

  control <- readr::read_csv('./inst/CSV/control/control.csv')
  target <- readr::read_csv('./inst/CSV/target/target.csv')
  control$cohort_definition_id = 100
  target$cohort_definition_id = 500

  cohort = rbind(control, target)

  db  <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
  DBI::dbExecute(db , "CREATE SCHEMA IF NOT EXISTS testthat")
  DBI::dbWriteTable(db ,   DBI::SQL('"testthat"."cohort"'), cohort)

  cdm <- CDMConnector::cdm_from_con(db , cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")

  targetTable <- cohortFromCohortTable(cdm = cdm, db = db, tableName = "cohort", schemaName = 'testthat', cohortId = 500)
  controlTable <- createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 1)

  ################################################################################
  #
  # Run the study
  #
  ################################################################################
  data = CohortContrast(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable,
    pathToResults = getwd(),
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
  #  expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", 3]) == 2, TRUE)
  #  expect_equal(nrow(data$trajectoryDataList$trajectoryData) == 70, TRUE)
  expect_equal(nrow(data$data_initial) == nrow(targetTable) + nrow(controlTable), TRUE)
  expect_equal(nrow(data$data_person) == nrow(targetTable) + nrow(controlTable), TRUE)
  #  expect_equal(nrow(data$data_patients) == 57, TRUE)
  DBI::dbDisconnect(db)
})
#> Test passed 🥇
test_that("Test patient matching with min and max values", {
  pathToResults <<- dirname(dirname(getwd())) #pathToResults = paste(getwd(), "/tests",sep="")

  ################################################################################
  #
  # Database credentials
  #
  ################################################################################

  control <- readr::read_csv('./inst/CSV/control/control.csv')
  target <- readr::read_csv('./inst/CSV/target/target.csv')
  control$cohort_definition_id = 100
  target$cohort_definition_id = 500

  cohort = rbind(control, target)

  db  <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
  DBI::dbExecute(db , "CREATE SCHEMA IF NOT EXISTS testthat")
  DBI::dbWriteTable(db ,   DBI::SQL('"testthat"."cohort"'), cohort)

  cdm <- CDMConnector::cdmFromCon(db , cdmName = "eunomia", cdmSchema = "main", writeSchema = "main")

  targetTable <- cohortFromCohortTable(cdm = cdm, db = db, tableName = "cohort", schemaName = 'testthat', cohortId = 500)
  controlTable <- createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 3, min = 1, max = 2)

  ################################################################################
  #
  # Run the study
  #
  ################################################################################
  data = CohortContrast(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable,
    pathToResults = getwd(),
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
  #  expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", 3]) == 2, TRUE)
  #  expect_equal(nrow(data$trajectoryDataList$trajectoryData) == 70, TRUE)
  expect_equal(nrow(data$data_initial) == nrow(targetTable) + nrow(controlTable), TRUE)
  expect_equal(nrow(data$data_person) == nrow(targetTable) + nrow(controlTable), TRUE)
  #  expect_equal(nrow(data$data_patients) == 57, TRUE)
  DBI::dbDisconnect(db)
})
#> Test passed 🥇
