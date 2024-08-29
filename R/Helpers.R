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
assertRequiredColumns <- function(data) {
  # Check if all required columns are present
  required_columns <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following required columns are missing:", paste(missing_columns, collapse = ", ")))
  }
}
