#' Get data from OMOP CDM specified in imported JSON files
#'
#' This function outputs a dataframe with columns SUBJECT_ID, COHORT_DEFINITION_ID, COHORT_START_DATE, COHORT_END_DATE
#' @param connection Connection to database
#' @param connectionDetails An object of class connectionDetails as created by the DatabaseConnector::createConnectionDetails function.
#' @param jsons JSONs from which cohorts will be created
#' @param names Names of the cohorts
#' @param cdmDataSchema Schema which contains the OHDSI Common Data Model.
#' @param cdmTempSchema Schema for temporary tables
#' @param studyName Customized name for the study
#' @keywords internal
getJSONData <- function(connection,
                        connectionDetails,
                        jsons,
                        names,
                        cdmDataSchema,
                        cdmTempSchema,
                        studyName) {
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in 1:length(jsons)) {
    cohortJson <- jsons[i]
    cohortName <- names[i]

    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <-
      CirceR::buildCohortQuery(cohortExpression,
                               options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <-
      rbind(
        cohortsToCreate,
        data.frame(
          cohortId = i,
          cohortName = cohortName,
          sql = cohortSql,
          stringsAsFactors = FALSE
        )
      )
  }
  # Create the cohort tables to hold the cohort generation results
  cohortTableNames <-
    CohortGenerator::getCohortTableNames(cohortTable = studyName)
  CohortGenerator::createCohortTables(
    connection = connection,
    cohortDatabaseSchema = cdmTempSchema,
    cohortTableNames = cohortTableNames
  )
  # Generate the cohorts
  generateCohortSet(
    connection = connection,
    cdmDatabaseSchema = cdmDataSchema,
    cdmVocabSchema = cdmVocabSchema,
    cohortDatabaseSchema = cdmTempSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate
  )

  # sql <-
  #   loadRenderTranslateSql(
  #     dbms = connectionDetails$dbms,
  #     "SELECT * FROM @cdmTempSchema.@studyName",
  #     cdmTempSchema = cdmTempSchema,
  #     studyName = studyName
  #   )
  # data <- DatabaseConnector::querySql(connection, sql)
  # # Apply state names
  # names <- c("0", names)
  # data$COHORT_DEFINITION_ID <- plyr::mapvalues(
  #   x = data$COHORT_DEFINITION_ID,
  #   from = 1:length(names),
  #   to = names,
  #   warn_missing = FALSE
  # )
  #
  # data <- dplyr::select(data,
  #                       SUBJECT_ID,
  #                       COHORT_DEFINITION_ID,
  #                       COHORT_START_DATE,
  #                       COHORT_END_DATE)

  return(TRUE)
}


#' Load settings of the study from trajectorySettings.csv according to the customized paramater studyName
#'
#' @param studyName Customized name for the study
#' @param pathToResults Path to the working directory, must include 'inst' folder
#' @keywords internal
loadJsons <- function(studyName, pathToResults) {
  env <-
    rlang::new_environment(data = list(), parent = rlang::empty_env())

  jsonFiles = list.files(
    path = paste(pathToResults, "/inst/JSON", sep = ""),
    pattern = '*.json',
    all.files = FALSE,
    full.names = TRUE
  )
  stateNamesJSON <- list.files(
    path = paste(pathToResults, "/inst/JSON", sep = ""),
    pattern = '*.json',
    all.files = FALSE,
    full.names = FALSE
  )
  stateNamesJSON <-
    substr(stateNamesJSON, 1, nchar(stateNamesJSON) - 5)
  insertedJSONs <- c()

  csvFiles = list.files(
    path = paste(pathToResults, "/inst/CSV", sep = ""),
    pattern = NULL,
    all.files = FALSE,
    full.names = TRUE
  )
  printCustomMessage("JSONs successfully imported from inst repository!")
  for (jsonFile in jsonFiles) {
    insertedJSONs <-
      c(insertedJSONs, paste(readLines(jsonFile), collapse = "\n"))
  }
  insertedCSV = c()
  if (length(csvFiles) > 1) {
    printCustomMessage("There should be a maximum of one CSV file reported under CSV repository!")
  }
  else if (length(csvFiles) == 1) {
  insertedCSV <- readr::read_csv(csvFiles)
  colnames(insertedCSV) <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")

  # Find the minimum value of cohort_definition_id
  min_value <- min(insertedCSV$cohort_definition_id)

  # Create a new column or modify an existing one with mapped values
  insertedCSV$cohort_definition_id <- ifelse(insertedCSV$cohort_definition_id == min_value, 1, 2)


  printCustomMessage("CSV file successfully imported from inst repository!")
  }

  env$stateNamesJSON <- stateNamesJSON
  env$insertedJSONs <- insertedJSONs
  env$insertedCSV <- insertedCSV
  env$studyName <- studyName

  return(env)
}
