################################################################################
#
# Functions related to initiating connection with database
#
################################################################################

#' This function initiates the connection with database and starts Shiny application
#'
#' @param pathToResults Path to target directory where results will be saved
#' @param cdmResultsSchema Schema which has the information about the cohorts created in Atlas
#' @example man/examples/runGUI.R
#'
#' @export
runGUI <- function(pathToResults = NULL,
                   studyName = "Cohort2Trajectory") {
  ################################################################################
  #
  # Creating global variables
  #
  ################################################################################

  if (!is.null(pathToResults)) {
    pathToResults <<- pathToResults
  }
  else {
    pathToResults <<- paste(getwd(), "/tmp", sep = "")
  }
  studyName <<- studyName
  ###############################################################################
  #
  # Creating mandatory directories if they do not exist
  #
  ###############################################################################

  createMandatorySubDirs(pathToResults)

  ################################################################################
  #
  # Opening shiny app
  #
  ################################################################################

  shiny::runApp("./shiny")

}
