# Output persistence helpers
#' Save workflow outputs as parquet files
#' @param data Data list object
#' @param pathToResults Path to the results folder, can be project's working directory
#'
#' @keywords internal

saveResult <- function(data, pathToResults) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  studyName <- data$config$complName
  if (is.null(studyName) || !nzchar(studyName)) {
    studyName <- paste0("CohortContrast_", timestamp)
    data$config$complName <- studyName
  }

  studyPath <- file.path(pathToResults, studyName)
  if (dir.exists(studyPath)) {
    unlink(studyPath, recursive = TRUE, force = TRUE)
  }
  dir.create(studyPath, recursive = TRUE, showWarnings = FALSE)

  writeStudyStateArtifacts(data = data, studyPath = studyPath, studyName = studyName)

  if (isFALSE(data$config$patientLevelData)) {
    # Remove patient level data
    data$data_patients = NULL
    data$data_initial = NULL
    data$data_person = NULL
    data$conceptsData = NULL
  }

  writeParquetComponents(data = data, outputDir = studyPath, verbose = FALSE)
  printCustomMessage(paste("Saved parquet study folder to ", studyPath, sep = ""))
}
