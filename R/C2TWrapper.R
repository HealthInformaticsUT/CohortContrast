#' This function acts as a Cohort2Trajectory wrpper
#'
#' @param data CohortContrast object
#' @param pathToResults Path where data should be saved to
#' @import Cohort2Trajectory
#' @export
#'
#'
c2tCaller <- function(data, pathToResults){
  if (is.null(data$trajectoryDataList$trajectoryData)) {
    stop("ERROR: No trajectory information in data object. Check if CohortContrast was run with createC2TInput == TRUE!")
  } else if (is.null(data$trajectoryDataList$selectedFeatureNames)){
    stop("ERROR: No feature information in data object. Maybe use more lenient filters?")
  } else {

    targetCohort <- data$data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID == "target") %>% dplyr::mutate(COHORT_DEFINITION_ID = 0)
    trajectoryDataObject <- rbind(targetCohort, data$trajectoryDataList$trajectoryData)
    trajectoryDataObject$COHORT_DEFINITION_ID = sanitize(trajectoryDataObject$COHORT_DEFINITION_ID)
    trajectoryDataObject$SUBJECT_ID = as.integer(trajectoryDataObject$SUBJECT_ID)
    stateCohortLabels <- sanitize(data$trajectoryDataList$selectedFeatureNames)
    allowedStatesList = Cohort2Trajectory::createStateList(stateCohortLabels)
    trajectoriesDataframe <- Cohort2Trajectory::Cohort2Trajectory(useCDM = FALSE,
                                         studyName = data$config$complName,
                                         stateCohortLabels = stateCohortLabels,
                                         trajectoryDataObject = trajectoryDataObject,
                                         stateCohortPriorityOrder = stateCohortLabels,
                                         stateSelectionType = 1,
                                         trajectoryType = 1,
                                         outOfCohortAllowed = FALSE,
                                         pathToResults = pathToResults,
                                         allowedStatesList = allowedStatesList,
                                         mergeStates = FALSE,
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
runTrajectoryViz <- function(data){
  TrajectoryViz::trajectoryViz(data)
}
