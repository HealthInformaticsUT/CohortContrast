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


#' Function for controlling whether patient exists in a cohort
#'
#' @param data A dataframe object with SUBJECT_ID values
#' @param id The subject ID
#' @keywords internal

idExists <- function(data, id) {
  if (as.character(id) %in% unique(as.character(data$SUBJECT_ID)))
    return(TRUE)
  return(FALSE)
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

#' # Function to normalize and scale a vector
#'
#' @param x Array of numeric
#' @keywords internal
scale_to_1_0 <- function(x) {
  # Normalize to 0-1
  #x_norm <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  x_norm <- log10(x + 1)
  # Scale to -1 to 0
  return(x_norm)
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

#' Function for creating the complementary mapping table which can be used in CohortContrast execution function
#'
#' @param conceptIds Array of concept ids to use
#' @param conceptNames Matching array of concept names to use
#' @keywords internal

createComplementaryMappingTable <-
  function(conceptIds, conceptNames) {
    # Check if vectors are of the same length
    if (length(conceptIds) != length(conceptNames)) {
      print("Error: Vectors 'conceptIds' and 'conceptNames' must be of the same length.")
      return(NULL)
    }

    # Check if concept names are all strings
    if (!all(sapply(conceptNames, is.character))) {
      print("Error: All 'conceptNames' must be strings.")
      return(NULL)
    }

    # Check if all concept IDs are unique
    if (length(unique(conceptIds)) != length(conceptIds)) {
      print("Error: All 'conceptIds' must be unique.")
      return(NULL)
    }

    # Create the dataframe
    complementaryMappingTable <-
      data.frame(
        CONCEPT_ID = conceptIds,
        CONCEPT_NAME = conceptNames,
        stringsAsFactors = FALSE
      )

    return(complementaryMappingTable)
  }

#'Function for loading all of the study names saved in ./tmp/datasets
#'
#' @param pathToResults Path to the results folder, can be project's working directory
#' @keywords internal

get_study_names <- function(pathToResults) {
  # List all files in the specified directory
  files <- list.files(pathToResults, full.names = TRUE, pattern = "\\.rds$")

  # Pattern to extract the study name from the filename
  study_name_pattern <- "(?<=/)([^/]+)(?=\\.rds$)"

  # Extract study names from the filenames
  study_names <- stringr::str_extract(files, study_name_pattern)

  # Return the unique study names
  return(unique(study_names))
}

#'Function for calculating inverse dates
#'
#' @param observation_period_start_date Observation period start date
#' @param observation_period_end_date Observation period end date
#' @param cohort_start_date Cohort inclusion period start date
#' @param cohort_end_date Cohort inclusion end date
#' @keywords internal

calculate_inverse_dates <- function(observation_period_start_date, observation_period_end_date, cohort_start_date, cohort_end_date) {
  inverse_date_ranges <- list()

  # Check if cohort dates fully overlap with observation dates
  if (cohort_start_date <= observation_period_start_date & cohort_end_date >= observation_period_end_date) {
    return(inverse_date_ranges)  # No date range to return
  }

  # Calculate inverse date ranges
  if (cohort_start_date > observation_period_start_date) {
    inverse_date_ranges[[length(inverse_date_ranges) + 1]] <- list(observation_period_start_date, cohort_start_date - 1)
  }

  if (cohort_end_date < observation_period_end_date) {
    inverse_date_ranges[[length(inverse_date_ranges) + 1]] <- list(cohort_end_date + 1, observation_period_end_date)
  }

  return(inverse_date_ranges)
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

#' Match Pattern in Column Names
#'
#' This function searches for specified patterns within column names provided.
#' It takes one or more patterns as input and returns a list of matches. Each pattern
#' is applied to the list of column names, and if a pattern does not match any column,
#' an error is thrown.
#'
#' @param ... Character, patterns to search for within the column names. You can
#'   provide multiple patterns as separate arguments.
#' @param cols A character vector of column names against which the patterns will
#'   be matched. Defaults to an empty character vector, and it expects the actual
#'   column names to be provided to function properly.
#'
#' @return A list where each element corresponds to a pattern provided in `...`.
#'   Each list element contains the indices of the column names in `cols` that match
#'   the respective pattern. If a pattern does not match any column names, the function
#'   will stop and throw an error indicating which pattern(s) did not match.
#' @keywords internal
patterns <- function (..., cols = character(0L))
{
  L = list(...)
  p = unlist(L, use.names = any(nzchar(names(L))))
  if (!is.character(p))
    stop_with_format("Input patterns must be of type character.")
  matched = lapply(p, grep, cols)
  if (length(idx <- which(sapply(matched, length) == 0L)))
  matched
}


#' Stop with Formatted and Translated Error Message
#'
#' This function stops execution by throwing an error with a formatted message.
#' The message can be translated based on the specified domain. This is typically
#' used within a package to standardize error messages and potentially support
#' multilingual error outputs.
#'
#' @param fmt A character string providing a formatting template for the message.
#'   This string can include placeholders that will be filled with data from
#'   additional arguments passed to `...`.
#' @param ... Additional arguments to be used for formatting the message string.
#'   These are typically values or variables that are inserted into the `fmt`
#'   template.
#' @param domain Character string specifying the translation domain. This is useful
#'   for packages that provide translations for their messages. Defaults to
#'   "R-data.table", which should be changed to reflect the domain of your package
#'   if used for translation.
#'
#' @return This function does not return a value; it interrupts function execution
#'   by throwing an error with a formatted and possibly translated message.
#'
#' @keywords internal
stop_with_format <- function (fmt, ..., domain = "R-data.table")
{
  stop(gettextf(fmt, ..., domain = domain), domain = NA, call. = FALSE)
}


#' Function to check if target and control schemas and tables are defined sufficiently
#' @param targetTableName Name of the table where target cohort is defined
#' @param controlTableName Name of the table where control cohort is defined
#' @param targetTableSchemaName Name of the schema where target cohort table is defined
#' @param controlTableSchemaName Name of the schema where control cohort table is defined
#' @param cohortsTableSchemaName Name of the schema where cohorts' table is defined
#' @param cohortsTableName Name of the table where cohorts are defined
#' @param targetCohortId The id for target cohort in cohorts' table
#' @param controlCohortId The id for control cohort in cohorts' table
#' @param pathToCohortsCSVFile The path to a CSV file that has data table for cohorts
#' @keywords internal

checkForCorrectRelationDefinitions <- function(
    targetTableName = NULL,
    controlTableName = NULL,
    targetTableSchemaName = NULL,
    controlTableSchemaName = NULL,
    cohortsTableSchemaName = NULL,
    cohortsTableName = NULL,
    targetCohortId = NULL,
    controlCohortId = NULL,
    pathToCohortsCSVFile = NULL
) {

  # Check if target definitions are provided
  if (!is.null(targetTableName) && !is.null(targetTableSchemaName)) {
    targetDefined <- TRUE
  } else if ((!is.null(cohortsTableSchemaName) && !is.null(cohortsTableName) && !is.null(targetCohortId)) || (!is.null(targetCohortId) && !is.null(pathToCohortsCSVFile)))  {
    targetDefined <- TRUE
  } else {
    printCustomMessage("ERROR: Target definitions are missing. Check your target cohort schema and relation names!
                       At least targetTableName and targetTableSchemaName OR cohortsTableSchemaName, cohortsTableName and targetCohortId OR pathToCohortsCSVFile and targetCohortId must be defined!")
    targetDefined <- FALSE
  }

  # Check if control definitions are provided
  if (is.null(controlTableName) || is.null(controlTableSchemaName)) {
    if (is.null(controlCohortId)) {
      printCustomMessage("WARNING: Control definitions are missing. Inverse controls or patient matching will be used.")
    }
  }
  return(targetDefined)
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
