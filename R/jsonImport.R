#' Load settings of the study from trajectorySettings.csv according to the customized paramater studyName
#'
#' @param studyName Customized name for the study
#' @param pathToResults Path to the working directory, must include 'inst' folder
#' @keywords internal
loadJsons <- function(studyName, pathToResults) {
  env <-
    rlang::new_environment(data = list(), parent = rlang::empty_env())

  targetCohortSet <- CDMConnector::readCohortSet(paste(pathToResults,"/inst/JSON/target", sep = ""))
  controlCohortSet <- CDMConnector::readCohortSet(paste(pathToResults,"/inst/JSON/control", sep = ""))

  printCustomMessage("JSONs successfully imported from inst repository!")

  csvFiles = c(paste(pathToResults,"/inst/CSV/control/control.csv", sep = ""),paste(pathToResults,"/inst/CSV/target/target.csv", sep = ""))

  controlCohortDataframe <- readr::read_csv(paste(pathToResults,"/inst/CSV/control/control.csv", sep = ""))
  targetCohortDataframe <- readr::read_csv(paste(pathToResults,"/inst/CSV/target/target.csv", sep = ""))

  printCustomMessage("CSV file successfully imported from inst repository!")

  env$targetCohortSet <- targetCohortSet
  env$targetCohortDataframe <- targetCohortDataframe
  env$controlCohortSet <- controlCohortSet
  env$controlCohortDataframe <- controlCohortDataframe

  return(env)
}

