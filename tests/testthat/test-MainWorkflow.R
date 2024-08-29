library(testthat)
library(CohortContrast)

test_that("Created features table is correct.", {
  pathToResults <<- getwd() #pathToResults = paste(getwd(), "/tests",sep="")

  ################################################################################
  #
  # Database credentials
  #
  ################################################################################

  control <- readr::read_csv('./inst/CSV/control/control.csv')
  target <- readr::read_csv('./inst/CSV/target/target.csv')
  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
  DBI::dbExecute(db , "CREATE SCHEMA IF NOT EXISTS testthat")
  DBI::dbWriteTable(db ,   DBI::SQL('"testthat"."target_mock"'), target)
  DBI::dbWriteTable(db ,   DBI::SQL('"testthat"."control_mock"'), control)

  cdm <- CDMConnector::cdm_from_con(db , cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")

  targetTable <- cohortFromCohortTable(cdm = cdm, db = db, tableName = "target_mock", schemaName = 'testthat')
  controlTable <- cohortFromCohortTable(cdm = cdm, db = db, tableName = "control_mock", schemaName = 'testthat')

  cdm <- createCohortContrastCdm(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable
  )

  ################################################################################
  #
  # Run the study
  #
  ################################################################################
  data = CohortContrast(
    cdm = cdm,
    pathToResults =getwd(), #paste(getwd(), '/tests/testthat', sep = ''),
    domainsIncluded = c("Drug"),
    prevalenceCutOff = 0,
    topK = 10, # Number of features to export
    presenceFilter = FALSE, # 0-1, percentage of people who must have the chosen feature present
    complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
    createC2TInput = TRUE,
    runZTests = FALSE,
    runLogitTests = FALSE,
    createOutputFiles = FALSE)

  expect_equal(length(data$resultList$selectedFeatures$CONCEPT_NAME) == 10, TRUE)
  expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", 3]) == 2, TRUE)
  expect_equal(nrow(data$resultList$trajectoryData) == 41, TRUE)
  expect_equal(nrow(data$data_initial) == 10, TRUE)
  expect_equal(nrow(data$data_person) == 2694, TRUE)
  expect_equal(nrow(data$data_patients) == 34, TRUE)

  DBI::dbDisconnect(db)
})
#> Test passed 🥇
