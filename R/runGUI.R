################################################################################
#
# Functions related to initiating connection with database
#
################################################################################

#' This function initiates the connection with database and starts Shiny application
#'
#' @param pathToResults Path to target directory where results will be saved
#' @param studyName The study to load first in the GUI
#' @import ggplot2
#' @import patchwork
#' @export
runGUI <- function(pathToResults = NULL,
                   studyName = "CohortContrast") {
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

  appDir <- system.file("shiny", package = "CohortContrast")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }

  shiny::runApp(appDir)

}
