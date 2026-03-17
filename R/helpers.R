################################################################################
#
# Some regularly used functions to help code cleanness
#
################################################################################

#' Function for saving summary tables to path
#'
#' @param object Object to save
#' @param path Path to the file saved
#' @keywords internal

save_object <- function(object, path) {
  if (is.data.frame(object)) {
    utils::write.csv(object, path, row.names = FALSE)
  }
  else {
    base::saveRDS(object, file = path)
  }
}


#' Function for saving metadata table to path
#'
#' @param object Object to save
#' @param path Path to the file saved
#' @keywords internal

save_object_metadata <- function(object, path, studyName = NULL) {

  # Calculate the number of unique patients in target and control cohorts
  rows_target <- object$data_initial %>%
    dplyr::filter(.data$COHORT_DEFINITION_ID == "target") %>%
    dplyr::summarise(unique_patients = dplyr::n_distinct(.data$SUBJECT_ID)) %>%
    dplyr::pull(.data$unique_patients)
  rows_control <- object$data_initial %>%
    dplyr::filter(.data$COHORT_DEFINITION_ID == "control") %>%
    dplyr::summarise(unique_patients = dplyr::n_distinct(.data$SUBJECT_ID)) %>%
    dplyr::pull(.data$unique_patients)
  # Calculate the number of significant differences in Z-Test
  ztest_significant <- object$data_features %>%
    dplyr::filter(.data$ZTEST == TRUE & .data$ABSTRACTION_LEVEL == -1)
  ztest_significant_count <- nrow(ztest_significant)
  # Prepare the metadata summary data frame
  temp <- data.frame(
    study = if(is.null(studyName)) object$config$complName else studyName,
    target_patients = rows_target,
    control_patients = rows_control,
    z_count = ztest_significant_count,
    stringsAsFactors = FALSE
  )
  # Mutate file
  path <- sub("\\.rds$", ".csv", path)
  # Save the updated metadata back to the specified path
  utils::write.csv(temp, path, row.names = F)
}

#' Function to ensure that the path to results exists, creating mandatory subdirectories if necessary
#' @param pathToResults The path where results will be stored
#' @keywords internal

createPathToResults <- function(pathToResults) {
  # Check if the main directory exists
  if (!dir.exists(pathToResults)) {
    # If it doesn't exist, create the directory along with any necessary subdirectories
    dir.create(pathToResults, recursive = TRUE)
    print(paste("Created the directory:", pathToResults))
  } else {
    print(paste("Directory already exists:", pathToResults))
  }
}

#' Function for logging prints
#'
#' @param message Message to show
#'
#' @keywords internal

printCustomMessage <- function(message) {
  # Calculate the length of the message to dynamically create the border
  border <- paste(rep("-", nchar(message)), collapse = "")

  # Get the current timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Print the messages and timestamp
  cat(border, "\n")
  cat(message, "\n")
  cat(border, "\n")
  cat("Timestamp: ", timestamp, "\n")
  cat("\n") # Print a new line
}

#' Function to sanitize a single string
#' @param input_string A  state label name
#' @keywords internal

sanitize_single <- function(input_string) {
  safe_string <- gsub("[^A-Za-z0-9_.-]", "_", input_string)  # Corrected regex
  if (substr(safe_string, 1, 1) == '.') {
    safe_string <- paste0('_', safe_string)
  }
  return(safe_string)
}

#' Sanitize filenames not in correct format
#'
#' @param input_strings A vector of state label names
#' @keywords internal

sanitize <- function(input_strings) {
  # Apply the sanitization function to each element of the vector
  sapply(input_strings, sanitize_single)
}

#' @keywords internal
assertRequiredCohortTable <- function(data) {
  if (is.null(data)) {
    stop("Table does not exist")
  }
  # Check if all required columns are present
  required_columns <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following required columns are missing:", paste(missing_columns, collapse = ", ")))
  }
  if (nrow(data) == 0) {
    stop(paste("The cohort table is empty!"))
  }
}

#' @keywords internal
create_CohortContrast_object <- function(data) {
  obj <- list(
    data_patients = data$data_patients,
    data_initial = data$data_initial,
    data_person = data$data_person,
    data_features = data$data_features,
    conceptsData = data$conceptsData,
    complementaryMappingTable = data$complementaryMappingTable,
    trajectoryDataList = data$trajectoryDataList,
    config = data$config
  )

  # Set the class
  class(obj) <- "CohortContrastObject"

  return(obj)
}

#' @export
print.CohortContrastObject <- function(x, ...) {
  cat("CohortContrastObject:\n\n")

  if (!is.null(x$data_patients)) {
    cat("data_patients (Data Frame): A dataframe with person-level data, data about concepts imported from CDM\n")
    print(utils::head(x$data_patients))
    cat("\n---\n")
  }

  if (!is.null(x$data_initial)) {
    cat("data_initial (Data Frame): Cohort table for target and control patients\n")
    print(utils::head(x$data_initial))
    cat("\n---\n")
  }

  if (!is.null(x$data_person)) {
    cat("data_person (Data Frame): Subjects' demographic data from CDM\n")
    print(utils::head(x$data_person))
    cat("\n---\n")
  }

  if (!is.null(x$data_features)) {
    cat("data_features (Data Frame): Feature level data about concepts imported from CDM\n")
    print(utils::head(x$data_features))
    cat("\n---\n")
  }

  if (!is.null(x$trajectoryDataList)) {
    cat("trajectoryDataList (List): Data that can be used for Cohort2Trajectory analysis\n")
    for (name in names(x$trajectoryDataList)) {
      cat(paste0(name, ":\n"))

      item <- x$trajectoryDataList[[name]]

      if (name == "selectedFeatureNames") {
        cat("  Selected Feature Names (Character Vector):\n")
        print(item)
      } else if (name == "selectedFeatureIds") {
        cat("  Selected Feature IDs (Integer Vector):\n")
        print(item)
     }
       else if (name == "selectedFeatures") {
        cat("  Selected Features (Data Frame):\n")
        print(utils::head(item))
      } else if (name == "trajectoryData") {
        cat("  Trajectory Data (Data Frame or List):\n")
        print(utils::head(item))
      }
      cat("\n---\n")
    }
  }
}

#' Convert a relative path to an absolute path
#'
#' This internal function converts a given relative path into an absolute path
#' using the built-in `normalizePath()` function.
#'
#' @param path A string representing the file or directory path, which may be relative or absolute.
#'
#' @return A string with the absolute path. If the file or directory does not exist,
#'         `normalizePath()` will throw an error due to `mustWork = TRUE`.
#'
#' @details The function utilizes `normalizePath()` to handle both Windows and Unix-based
#'          systems. It converts the provided path, even if it is relative, into
#'          an absolute path. It ensures that paths are formatted with forward slashes
#'          on Windows systems by setting `winslash = "/"`.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   relative_path <- "relative/path/to/file.txt"
#'   absolute_path <- convertToAbsolutePath(relative_path)
#'   print(absolute_path)
#' }
convertToAbsolutePath <- function(path) {
  # normalizePath() will convert a relative path to an absolute one
  absolute_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  return(absolute_path)
}

#' Convert int64 to integer
#' @keywords internal
  convert_int64 <- function(x) {
    if (inherits(x, "integer64")) {
      return(as.integer(bit64::as.integer64(x)))
    } else if (inherits(x, "integer") || inherits(x, "numeric")) {
      return(as.integer(x))
    }
    return(x)  # If not an integer/numeric, return as is
  }
