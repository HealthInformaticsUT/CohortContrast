################################################################################
#
# Functions related to initiating connection with database
#
################################################################################

#' This function initiates the connection with database and starts Shiny application
#'
#' @param pathToDriver Path to a folder containing the JDBC driver JAR files. See downloadJdbcDrivers for instructions on how to download the relevant drivers.
#' @param pathToResults Path to target directory where results will be saved
#' @param dbms The type of DBMS running on the server. Valid values are: 'oracle','postgresql','redshift','sql server','pdw', 'netezza','bigquery','sqlite', 'sqlite extended','spark'
#' @param cdmSchema Schema which contains the OHDSI Common Data Model.
#' @param cdmTmpSchema Schema for temporary tables
#' @param cdmResultsSchema Schema which has the information about the cohorts created in Atlas
#' @example man/examples/runGUI.R
#'
#' @export
runGUI <- function(conn,
                   connectionDetails,
                   pathToDriver = './Drivers',
                   pathToResults = NULL,
                   dbms = "postgresql",
                   # user = '',
                   # pw = '',
                   # server = 'localhost/postgres',
                   # port = '5432',
                   cdmSchema = "ohdsi_cdm",
                   cdmVocabSchema = "ohdsi_vocab",
                   cdmTmpSchema = "ohdsi_temp",
                   cdmResultsSchema = "ohdsi_results",
                   studyName = "Cohort2Trajectory") {
  ################################################################################
  #
  # Creating global variables
  #
  ################################################################################

  #' @param user 	The user name used to access the server.
  #' @param pw The password for that user.
  #' @param server 	The name of the server.
  #' @param port  The port on the server to connect to.
  dbms <<- dbms
  connectionDetails <<- connectionDetails
  conn <<- conn
  cdmSchema <<- cdmSchema
  cdmVocabSchema <<- cdmVocabSchema
  cdmTmpSchema <<- cdmTmpSchema
  cdmResultsSchema <<- cdmResultsSchema
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

  DatabaseConnector::disconnect(conn)
  print("The database conncetion has been closed")
}
