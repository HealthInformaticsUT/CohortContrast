library(testthat)
library(CohortContrast)

test_that("Created features table is correct.", {
  studyName = "TestCohortContrast"
  pathToResults <<- dirname(dirname(getwd())) #pathToResults = paste(getwd(), "/tests",sep="")

  ################################################################################
  #
  # Database credentials
  #
  ################################################################################

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
  cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia", cdm_schema = "main", write_schema = "main")
  ################################################################################
  #
  # Run the study
  #
  ################################################################################
  data = CohortContrast(
    cdm = cdm,
    pathToResults =getwd(), #paste(getwd(), '/tests/testthat', sep = ''),
    studyName = studyName,
    domainsIncluded = c("Drug"),
    readFromCSV = FALSE,
    prevalenceCutOff = 0,
    topDogs = 10, # Number of features to export
    presenceFilter = FALSE, # 0-1, percentage of people who must have the chosen feature present
    complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
    nudgeTarget = FALSE, # nudge target cohort start date (days)
    nudgeControl = FALSE,# nudge control cohort start date (days)
    createC2TInput = TRUE,
    useInverseControls = FALSE,
    runZTests = FALSE,
    runLogitTests = FALSE)

  expect_equal(length(data$resultList$selectedFeatures$CONCEPT_NAME) == 10, TRUE)
  expect_equal(as.numeric(data$data_features[data$data_features$CONCEPT_NAME == "Diclofenac", 3]) == 2, TRUE)
  expect_equal(nrow(data$resultList$trajectoryData) == 37, TRUE)
  expect_equal(nrow(data$data_initial) == 646, TRUE)
  expect_equal(nrow(data$data_person) == 2694, TRUE)
  expect_equal(nrow(data$data_patients) == 818, TRUE)
})
#> Test passed 🥇
