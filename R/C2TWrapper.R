
#' This function acts as a Cohort2Trajectory wrapper
#' @param data CohortContrast object
#' @param pathToResults Path where data should be saved to
#' @param trajectoryType Type of trajectories to generate ("Discrete", "Continuous")
#' @param lengthOfStay If discrete trajectory is generated we have to indicate the state length in days (integer)
#' @param stateSelectionType State selection type (First occurring - "First", Largest overlap - "Overlap", Priority - "Priority")
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
C2TCaller <- function(data,
                      pathToResults,
                      trajectoryType = "Continuous",
                      stateSelectionType = "Priority",
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
    trajectoryDataObject$cohort_definition_id = sanitize(trajectoryDataObject$cohort_definition_id)
    trajectoryDataObject$subject_id = as.integer(trajectoryDataObject$subject_id)
    stateCohortLabels <- sanitize(dplyr::setdiff(unique(trajectoryDataObject$cohort_definition_id), c("0")))
    allowedStatesList = Cohort2Trajectory::createStateList(stateCohortLabels)

    studyEnv <- Cohort2Trajectory::cohort2TrajectoryConfiguration(
      baseUrl = NULL,
      useCDM = FALSE,
      trajectoryDataObject = trajectoryDataObject,
      studyName = data$config$complName,
      pathToStudy = pathToResults,
      outOfCohortAllowed = outOfCohortAllowed,
      trajectoryType = trajectoryType,
      lengthOfStay = lengthOfStay,
      stateSelectionType = stateSelectionType,
      stateCohortPriorityOrder = stateCohortLabels,
      allowedStatesList = allowedStatesList,
      runSavedStudy = FALSE,
      batchSize = 10,
      mergeStates = mergeStates,
      mergeThreshold = mergeThreshold
    )

    Cohort2Trajectory::getDataForStudy(studyEnv = studyEnv, trajectoryDataObject = trajectoryDataObject)

    trajectoriesDataframe <- Cohort2Trajectory::createTrajectories(studyEnv = studyEnv)

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
        c("cohort_definition_id",
          "subject_id",
          "cohort_start_date",
          "cohort_end_date")
      # Ensure only highlighted concepts are used
      data_states <- data_states %>% dplyr::filter(.data$cohort_definition_id %in% data$trajectoryDataList$selectedFeatureNames)
      data_states <- resolveMissingDates(data_states)

      colnames(data_target) = tolower(colnames(data_target))

      data$trajectoryDataList$trajectoryData = rbind(as.data.frame(data_target), data_states)
      # Convert from int64 to integer
      data$trajectoryDataList$trajectoryData$subject_id = as.integer(data$trajectoryDataList$trajectoryData$subject_id)
      printCustomMessage("Cohort2Trajectory input can be now found under data$trajectoryDataList$trajectoryData")
    return(data)
  }


#' Function for creating dataset which can be used as input for Cohort2Trajectory package
#'
#' @param data Cohort data with columns cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#' @param afterStart integer to add to cohort_start_date if cohort_end_date is missing
#' @param beforeEnd integer to subtract from cohort_end_date if cohort_start_date is missing
#' @keywords internal
resolveMissingDates <- function(data,
                                afterStart = 1,
                                beforeEnd = 1){
  if (!"cohort_definition_id" %in% names(data) ||
      !"subject_id" %in% names(data) ||
      !"cohort_start_date" %in% names(data) ||
      !"cohort_end_date" %in% names(data)) {
    stop("The input data must contain the columns cohort_definition_id, subject_id, cohort_start_date, cohort_end_date")
  }

  data_updated <- data %>% dplyr::mutate(
    cohort_end_date = dplyr::if_else(is.na(.data$cohort_end_date) & !is.na(.data$cohort_start_date),
                                     .data$cohort_start_date + afterStart, .data$cohort_end_date),
    cohort_start_date = dplyr::if_else(is.na(.data$cohort_start_date) & !is.na(.data$cohort_end_date),
                                       .data$cohort_end_date - beforeEnd, .data$cohort_start_date)
  ) %>% dplyr::filter(!is.na(.data$cohort_start_date) & !is.na(.data$cohort_end_date))


  # Notify the user about changes only if they are made
  if (any(is.na(data %>% dplyr::select(.data$cohort_start_date,.data$cohort_end_date)))) {
    cli::cli_alert_success("Resolved missing dates in the dataset for generating trajectories.")
  }

  return(data_updated)
}
