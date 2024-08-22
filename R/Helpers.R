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
    save(object, file = path)
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

#' Function which creates mandatory subdirectories and files to the pathToResults directory
#'
#' @param pathToResults Path to the package results
#' @keywords internal
createMandatorySubDirs <- function(pathToResults) {
  dir.create(file.path(pathToResults, "tmp"), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/tmp', sep = ""), 'datasets'), showWarnings = FALSE)

  dir.create(file.path(pathToResults, "inst"), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/inst', sep = ""), 'JSON'), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/inst', sep = ""), 'CSV'), showWarnings = FALSE)
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
  tmpdir = paste(pathToResults,'/tmp/datasets/', sep = "" )
  files <- list.files(tmpdir, full.names = TRUE, pattern = "_CC_medData\\.rdata$")

  # Pattern to extract the study name from the filename
  study_name_pattern <- "(?<=/)([^/]+)(?=_CC_medData\\.rdata$)"

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

