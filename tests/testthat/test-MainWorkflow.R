library(testthat)
library(Eunomia)
library(CohortContrast)

test_that("Created cohort table is with correct.", {
  studyName = "TestCohortContrast"
  pathToResults <<- dirname(dirname(getwd())) #pathToResults = paste(getwd(), "/tests",sep="")

  ################################################################################
  #
  # Database credentials
  #
  ################################################################################
  pathToDriver <- './Drivers'

  cdmSchema <- "main"  # Schema which contains the OHDSI Common Data Model
  cdmTmpSchema <- "main" # Schema for temporary tables, will be deleted # should be ohdsi_temp
  cdmResultsSchema <- "main" # Schema which will contain the final results
  cdmVocabSchema <- "main"

  baseUrl <- NULL  # WebAPI URL is not needed when jsons' are already imported


  ################################################################################
  #
  # Initiate the database connection
  #
  ################################################################################
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  conn <- DatabaseConnector::connect(connectionDetails)
  Eunomia::createCohorts(connectionDetails)


  ################################################################################
  #
  # Run the study
  #
  ################################################################################

  data = CohortContrast(
    connection = conn,
    connectionDetails,
    cdmSchema,
    cdmVocabSchema,
    cdmTmpSchema,
    pathToResults,
    studyName,
    domainsIncluded =  c("Drug"),
    generateTables = TRUE,
    readFromCSV = FALSE,
    prevalenceCutOff = 0.1,
    topDogs =10,
    presenceFilter = FALSE,
    complementaryMappingTable = FALSE,
    nudgeTarget = 30,
    nudgeControl = FALSE
  )
  expect_equal(length(data$resultList$selectedFeatures$CONCEPT_NAME) == 0, TRUE)
})
#> Test passed 🥇

# test_that("danazol target count.", {
#   print(head(data$data_features))
#   expect_equal(data$data_features[data$data_features$CONCEPT_NAME == "danazol", 4] == 1, TRUE)
# })
#> Test passed 🥇
