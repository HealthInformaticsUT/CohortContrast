################################################################################
#
# Functions related to initiating connection with database
#
################################################################################

#' This function initiates the connection with database and starts Shiny application
#'
#' @param pathToResults Path to target directory where results will be saved
#' @import ggplot2
#' @import patchwork
#' @export
runCohortContrastGUI <- function(pathToResults = NULL) {
  ################################################################################
  #
  # Creating global variables
  #
  ################################################################################

  if (!is.null(pathToResults)) {
    pathToResults <<- pathToResults
  }
  else {
    printCustomMessage("ERROR: pathToResults variable not defined!")
  }
  ###############################################################################
  #
  # Creating mandatory directories if they do not exist
  #
  ###############################################################################

  createPathToResults(pathToResults)

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
