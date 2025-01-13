
#' This function acts as a Cohort2Trajectory wrapper
#' @param data CohortContrast object
#' @param pathToResults Path where data should be saved to
#' @param trajectoryType Type of trajectories to generate (Discrete - 0, continuous - 1 )
#' @param lengthOfStay If discrete trajectory is generated we have to indicate the state length in days (integer)
#' @param stateSelectionType State selection type (First occurring - 1, Largest overlap - 2, Priority - 3)
#' @param mergeStates Boolean for merging overlapping states
#' @param mergeThreshold A value 0 to 1 indicating the threshold of overlap for merging - percentage
#' @param outOfCohortAllowed Boolean for allowing to include state occurrences outside the cohort period
#' @import Cohort2Trajectory
#' @export
#' @examples \dontrun{
#'   data <- readRDS("./snapshots/CohortContrastDataSnapshot.rds")
#'   user <- Sys.getenv("DB_USERNAME")
#'   pw <- Sys.getenv("DB_PASSWORD")
#'   server <- stringr::str_c(Sys.getenv("DB_HOST"), "/", Sys.getenv("DB_NAME"))
#'   port <- Sys.getenv("DB_PORT")
#'
#'   cdmSchema <-
#'     Sys.getenv("OHDSI_CDM")
#'   cdmVocabSchema <-
#'     Sys.getenv("OHDSI_VOCAB")
#'   cdmResultsSchema <-
#'     Sys.getenv("OHDSI_RESULTS")
#'   writeSchema <-
#'     Sys.getenv("OHDSI_WRITE")
#'   writePrefix <- "CohortContrast"
#'
#'   db = DBI::dbConnect(
#'     RPostgres::Postgres(),
#'     dbname = Sys.getenv("DB_NAME"),
#'     host = Sys.getenv("DB_HOST"),
#'     user = Sys.getenv("DB_USERNAME"),
#'     password = Sys.getenv("DB_PASSWORD"),
#'     port = port
#'   )
#'
#'   cdm <- CDMConnector::cdmFromCon(
#'     con = db,
#'     cdmSchema = cdmSchema,
#'     achillesSchema = cdmResultsSchema,
#'     writeSchema = list(schema = writeSchema, prefix = writePrefix)
#'   )
#'
#'   data = CohortContrast::createC2TInput(data, cdm)
#'
#'   trajectories = CohortContrast::C2TCaller <- function(data, pathToResults = getwd())
#' }
C2TCaller <- function(data, pathToResults,
                      trajectoryType = 1,
                      stateSelectionType = 1,
                      lengthOfStay = 30,
                      mergeStates = FALSE,
                      mergeThreshold = 0.5,
                      outOfCohortAllowed = FALSE
                      ) {
  if (is.null(data$trajectoryDataList$trajectoryData)) {
    stop("ERROR: No trajectory information in data object. You have to run createC2TInput beforehand!")
  } else if (is.null(data$trajectoryDataList$selectedFeatureNames)){
    stop("ERROR: No feature information in data object. Maybe use more lenient filters?")
  } else {

    trajectoryDataObject <- rbind(data$trajectoryDataList$trajectoryData)
    trajectoryDataObject$COHORT_DEFINITION_ID = sanitize(trajectoryDataObject$COHORT_DEFINITION_ID)
    trajectoryDataObject$SUBJECT_ID = as.integer(trajectoryDataObject$SUBJECT_ID)
    stateCohortLabels <- sanitize(dplyr::setdiff(unique(trajectoryDataObject$COHORT_DEFINITION_ID), c("0")))
    allowedStatesList = Cohort2Trajectory::createStateList(stateCohortLabels)
    trajectoriesDataframe <- Cohort2Trajectory::Cohort2Trajectory(useCDM = FALSE,
                                         studyName = data$config$complName,
                                         stateCohortLabels = stateCohortLabels,
                                         trajectoryDataObject = trajectoryDataObject,
                                         stateCohortPriorityOrder = stateCohortLabels,
                                         stateSelectionType = stateSelectionType,
                                         trajectoryType = trajectoryType,
                                         lengthOfStay = lengthOfStay,
                                         outOfCohortAllowed = outOfCohortAllowed,
                                         pathToResults = pathToResults,
                                         allowedStatesList = allowedStatesList,
                                         mergeStates = mergeStates,
                                         mergeThreshold = mergeThreshold,
                                         runSavedStudy = FALSE,
                                         saveSettings = FALSE
                                         )
    return(trajectoriesDataframe)
  }
}

#' This function start up TrajectoryViz package GUI with the C2T output
#' @param data Cohort2Trajectory output object
#' @import TrajectoryViz
#' @export
#' @examples \dontrun{
#'   data <- readRDS("./snapshots/CohortContrastDataSnapshot.rds")
#'   user <- Sys.getenv("DB_USERNAME")
#'   pw <- Sys.getenv("DB_PASSWORD")
#'   server <- stringr::str_c(Sys.getenv("DB_HOST"), "/", Sys.getenv("DB_NAME"))
#'   port <- Sys.getenv("DB_PORT")
#'
#'   cdmSchema <-
#'     Sys.getenv("OHDSI_CDM")
#'   cdmVocabSchema <-
#'     Sys.getenv("OHDSI_VOCAB")
#'   cdmResultsSchema <-
#'     Sys.getenv("OHDSI_RESULTS")
#'   writeSchema <-
#'     Sys.getenv("OHDSI_WRITE")
#'   writePrefix <- "cc_"
#'
#'   db = DBI::dbConnect(
#'     RPostgres::Postgres(),
#'     dbname = Sys.getenv("DB_NAME"),
#'     host = Sys.getenv("DB_HOST"),
#'     user = Sys.getenv("DB_USERNAME"),
#'     password = Sys.getenv("DB_PASSWORD"),
#'     port = port
#'   )
#'
#'   cdm <- CDMConnector::cdmFromCon(
#'     con = db,
#'     cdmSchema = cdmSchema,
#'     achillesSchema = cdmResultsSchema,
#'     writeSchema = list(schema = writeSchema, prefix = writePrefix)
#'   )
#'
#'   data = CohortContrast::createC2TInput(data, cdm)
#'
#'   trajectories = CohortContrast::C2TCaller <- function(data, pathToResults = getwd())
#'
#'   CohortContrast::runTrajectoryViz(trajectories)
#' }
runTrajectoryViz <- function(data){
  TrajectoryViz::trajectoryViz(data)
}

#' Function for creating dataset which can be used as input for Cohort2Trajectory package
#'
#' @param data Data list object
#' @param cdm Connection to database (CDMConnector)
#' @param targetCohortId Target cohort id
#' @param complementaryMappingTable Mapping table for mapping concept_ids if present
#'
#' @export
#' @examples \dontrun{
#'   data <- readRDS("./snapshots/CohortContrastDataSnapshot.rds")
#'   user <- Sys.getenv("DB_USERNAME")
#'   pw <- Sys.getenv("DB_PASSWORD")
#'   server <- stringr::str_c(Sys.getenv("DB_HOST"), "/", Sys.getenv("DB_NAME"))
#'   port <- Sys.getenv("DB_PORT")
#'
#'   cdmSchema <-
#'     Sys.getenv("OHDSI_CDM")
#'   cdmVocabSchema <-
#'     Sys.getenv("OHDSI_VOCAB")
#'   cdmResultsSchema <-
#'     Sys.getenv("OHDSI_RESULTS")
#'   writeSchema <-
#'     Sys.getenv("OHDSI_WRITE")
#'   writePrefix <- "cc_"
#'
#'   db = DBI::dbConnect(
#'     RPostgres::Postgres(),
#'     dbname = Sys.getenv("DB_NAME"),
#'     host = Sys.getenv("DB_HOST"),
#'     user = Sys.getenv("DB_USERNAME"),
#'     password = Sys.getenv("DB_PASSWORD"),
#'     port = port
#'   )
#'
#'   cdm <- CDMConnector::cdmFromCon(
#'     con = db,
#'     cdmSchema = cdmSchema,
#'     achillesSchema = cdmResultsSchema,
#'     writeSchema = list(schema = writeSchema, prefix = writePrefix)
#'   )
#'
#'   data = createC2TInput(data, cdm)
#' }
createC2TInput <-
  function(data,
           cdm,
           targetCohortId = "target",
           complementaryMappingTable = NULL) {

      if(length(data$trajectoryDataList$selectedFeatureNames) < 1) {
        printCustomMessage("WARNING: No features left for creating Cohort2Trajectory input.")
        return(data)
      }
    abstraction_level = ifelse(is.null(data$config$abstractionLevel), -1,data$config$abstractionLevel)

    if (!is.data.frame(complementaryMappingTable)){
      if(is.data.frame(data$complementaryMappingTable)){
        complementaryMappingTable = data$complementaryMappingTable %>% dplyr::filter(.data$ABSTRACTION_LEVEL == abstraction_level)
      }
      else {
        complementaryMappingTable = NULL
      }
    }
      data_selected_patients = dplyr::select(
        dplyr::filter(
          data$data_patients,
          .data$ABSTRACTION_LEVEL == abstraction_level,
          .data$CONCEPT_NAME %in% data$trajectoryDataList$selectedFeatureNames,
          .data$COHORT_DEFINITION_ID == targetCohortId
        ),
        .data$CONCEPT_ID,
        .data$CONCEPT_NAME,
        .data$PERSON_ID,
        .data$HERITAGE
      )
      # Creating target cohort and eligible patients dataset
      data_target = dplyr::mutate(
        dplyr::filter(
          data$data_initial,
          .data$COHORT_DEFINITION_ID == targetCohortId,
          .data$SUBJECT_ID %in% unique(data_selected_patients$PERSON_ID)
        ),
        COHORT_DEFINITION_ID = "0"
      )

      printCustomMessage("Creating a dataset for eligible patients only (Cohort2Trajectory input) ...")
      # Creating state cohorts / eligible patients dataset
      # Split the data by heritage
      split_data <-
        split(data_selected_patients, data_selected_patients$HERITAGE)
      # Recreate cohortcontrast_cohorts table
      targetTable = data$data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID == "target") %>% dplyr::mutate(COHORT_DEFINITION_ID = 2)
      colnames(targetTable) = tolower(colnames(targetTable))

      controlTable = data$data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID == "control") %>% dplyr::mutate(COHORT_DEFINITION_ID = 1)
      colnames(controlTable) = tolower(colnames(controlTable))

      cdm = createCohortContrastCdm(
        cdm = cdm,
        targetTable = targetTable,
        controlTable = controlTable
      )
      # Iterate over each heritage type and construct then execute SQL queries
      results_list <-
        queryHeritageData(
          dataPatient = data_selected_patients,
          cdm = cdm,
          split_data = split_data,
          complementaryMappingTable = complementaryMappingTable
        )

      data_states <- do.call(rbind, results_list)

      colnames(data_states) <-
        c("COHORT_DEFINITION_ID",
          "SUBJECT_ID",
          "COHORT_START_DATE",
          "COHORT_END_DATE")

      data$trajectoryDataList$trajectoryData = rbind(as.data.frame(data_target), data_states)
      # Convert from int64 to integer
      data$trajectoryDataList$trajectoryData$SUBJECT_ID = as.integer(data$trajectoryDataList$trajectoryData$SUBJECT_ID)
      printCustomMessage("Cohort2Trajectory input can be now found under data$trajectoryDataList$trajectoryData")
    return(data)
  }
