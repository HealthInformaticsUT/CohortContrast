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
  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir("GiBleed"))
  DBI::dbExecute(db , "CREATE SCHEMA IF NOT EXISTS testthat")
  DBI::dbWriteTable(db ,   DBI::SQL('"testthat"."target_mock"'), target)
  DBI::dbWriteTable(db ,   DBI::SQL('"testthat"."control_mock"'), control)

  cdm <- CDMConnector::cdm_from_con(db , cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")

  targetTable <- cohortFromCohortTable(cdm = cdm, db = db, tableName = "target_mock", schemaName = 'testthat')
  controlTable <- cohortFromCohortTable(cdm = cdm, db = db, tableName = "control_mock", schemaName = 'testthat')

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
    domainsIncluded = c("Drug"),
    prevalenceCutOff = 0,
    topK = 10, # Number of features to export
    presenceFilter = FALSE, # 0-1, percentage of people who must have the chosen feature present
    complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
    runZTests = FALSE,
    runLogitTests = FALSE,
    createOutputFiles = FALSE,
    numCores = 1,
    complName = "test-C2TWrapper")

  data = createC2TInput(data = data, cdm = cdm)

  expect_equal(nrow(data$trajectoryDataList$trajectoryData) == 7351, TRUE)

  trajectories = C2TCaller(data = data, pathToResults = pathToResults)

  expect_equal(nrow(trajectories) == 3999, TRUE)
  DBI::dbDisconnect(db)
})
#> Test passed 🥇
