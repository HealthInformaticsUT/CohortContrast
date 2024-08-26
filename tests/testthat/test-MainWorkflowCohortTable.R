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
  control$cohort_definition_id = 100
  target$cohort_definition_id = 500

  cohort = rbind(control, target)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS testthat")
  DBI::dbWriteTable(con,   DBI::SQL('"testthat"."cohort"'), cohort)

  cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")

  cdm <- createCohortContrastCohorts(
    cdm,
    con,
    targetTableName = NULL,
    controlTableName = NULL,
    targetTableSchemaName = NULL,
    controlTableSchemaName = NULL,
    cohortsTableSchemaName = 'testthat',
    cohortsTableName = 'cohort',
    targetCohortId = 500,
    controlCohortId = 100,
    nudgeTarget = FALSE,
    nudgeControl = FALSE,
    useInverseControls = FALSE,
    useTargetMatching = FALSE
  )
  ################################################################################
  #
  # Run the study
  #
  ################################################################################
  data = CohortContrast(
    cdm = cdm,
    pathToResults =  getwd(),
    domainsIncluded = c("Drug"),
    prevalenceCutOff = 0,
    topK = 15, # Number of features to export
    presenceFilter = FALSE, # 0-1, percentage of people who must have the chosen feature present
    complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
    createC2TInput = TRUE,
    runZTests = FALSE,
    runLogitTests = FALSE,
    createOutputFiles = FALSE)

  expect_equal(length(data$resultList$selectedFeatures$CONCEPT_NAME) == 15, TRUE)
  expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", 3]) == 2, TRUE)
  expect_equal(nrow(data$resultList$trajectoryData) == 70, TRUE)
  expect_equal(nrow(data$data_initial) == 10, TRUE)
  expect_equal(nrow(data$data_person) == 2694, TRUE)
  expect_equal(nrow(data$data_patients) == 61, TRUE)
})
#> Test passed 🥇
