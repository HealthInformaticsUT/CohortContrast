################################################################################
#
# Some regularly used functions to help code cleanness
#
################################################################################

#' @keywords internal
assertPackagesAvailable <- function(packages, feature) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) {
    return(invisible(TRUE))
  }

  pkg_list <- paste0("'", missing, "'", collapse = ", ")
  stop(
    "Package(s) ", pkg_list, " are required for ", feature, ". ",
    "Install with: install.packages(c(",
    paste0("\"", missing, "\"", collapse = ", "),
    "))",
    call. = FALSE
  )
}

#' Build study metadata for CSV/JSON sidecar files
#'
#' @param object Object to save
#' @param studyName Optional study name override
#' @keywords internal

buildStudyMetadata <- function(object, studyName = NULL) {

  # Calculate the number of unique patients in target and control cohorts
  rows_target <- object$data_initial %>%
    dplyr::filter(.data$COHORT_DEFINITION_ID == "target") %>%
    dplyr::summarise(unique_patients = dplyr::n_distinct(.data$SUBJECT_ID)) %>%
    dplyr::pull(.data$unique_patients)
  rows_control <- object$data_initial %>%
    dplyr::filter(.data$COHORT_DEFINITION_ID == "control") %>%
    dplyr::summarise(unique_patients = dplyr::n_distinct(.data$SUBJECT_ID)) %>%
    dplyr::pull(.data$unique_patients)
  # Calculate the number of significant differences in CHI2Y test
  chi2y_significant <- object$data_features %>%
    dplyr::filter(.data$CHI2Y == TRUE & .data$ABSTRACTION_LEVEL == -1)
  chi2y_significant_count <- nrow(chi2y_significant)
  # Prepare the metadata summary data frame
  list(
    study = if(is.null(studyName)) object$config$complName else studyName,
    target_patients = rows_target,
    control_patients = rows_control,
    chi2y_count = chi2y_significant_count
  )
}

#' Save study metadata to CSV
#'
#' @param object Object to save
#' @param path Path to CSV file
#' @param studyName Optional study name override
#' @keywords internal
saveObjectMetadata <- function(object, path, studyName = NULL) {
  metadata <- buildStudyMetadata(object = object, studyName = studyName)
  temp <- as.data.frame(metadata, stringsAsFactors = FALSE)
  # Mutate file
  path <- sub("\\.rds$", ".csv", path)
  # Save the updated metadata back to the specified path
  utils::write.csv(temp, path, row.names = F)
  invisible(metadata)
}

#' Save study metadata to JSON
#'
#' @param object Object to save
#' @param path Path to JSON file
#' @param studyName Optional study name override
#' @keywords internal
saveObjectMetadataJson <- function(object, path, studyName = NULL) {
  metadata <- buildStudyMetadata(object = object, studyName = studyName)
  jsonlite::write_json(metadata, path = path, auto_unbox = TRUE, pretty = TRUE)
  invisible(metadata)
}

#' @keywords internal
writeStudyStateArtifacts <- function(data, studyPath, studyName = NULL) {
  if (is.null(studyName) || !nzchar(studyName)) {
    if (is.list(data$config) && !is.null(data$config$complName) && nzchar(data$config$complName)) {
      studyName <- data$config$complName
    } else {
      studyName <- basename(normalizePath(studyPath, mustWork = FALSE))
    }
  }

  saveObjectMetadataJson(
    object = data,
    path = file.path(studyPath, "metadata.json"),
    studyName = studyName
  )

  if (is.list(data$config)) {
    jsonlite::write_json(
      data$config,
      path = file.path(studyPath, "config.json"),
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
  }

  selectedFeatureData <- data$selectedFeatureData
  if (is.null(selectedFeatureData) && !is.null(data$trajectoryDataList)) {
    selectedFeatureData <- data$trajectoryDataList
  }

  if (!is.list(selectedFeatureData)) {
    return(invisible(NULL))
  }

  selectedFeaturePayload <- list(
    selectedFeatureNames = unname(as.character(selectedFeatureData$selectedFeatureNames)),
    selectedFeatureIds = selectedFeatureData$selectedFeatureIds
  )

  jsonlite::write_json(
    selectedFeaturePayload,
    path = file.path(studyPath, "selected_feature_data.json"),
    auto_unbox = FALSE,
    pretty = TRUE,
    null = "null"
  )

  if (is.data.frame(selectedFeatureData$selectedFeatures)) {
    if (!requireNamespace("nanoparquet", quietly = TRUE)) {
      stop(
        "Package 'nanoparquet' is required for writing parquet output.\n",
        "Install it with: install.packages('nanoparquet')"
      )
    }
    selectedFeaturesPrepared <- prepareForNanoparquet(selectedFeatureData$selectedFeatures)
    nanoparquet::write_parquet(
      selectedFeaturesPrepared,
      file.path(studyPath, "selected_features.parquet")
    )
  }

  if (is.list(data$conceptsData)) {
    if (is.data.frame(data$conceptsData$concept_ancestor)) {
      conceptAncestorPrepared <- prepareForNanoparquet(data$conceptsData$concept_ancestor)
      nanoparquet::write_parquet(
        conceptAncestorPrepared,
        file.path(studyPath, "concepts_concept_ancestor.parquet")
      )
    }
    if (is.data.frame(data$conceptsData$concept)) {
      conceptPrepared <- prepareForNanoparquet(data$conceptsData$concept)
      nanoparquet::write_parquet(
        conceptPrepared,
        file.path(studyPath, "concepts_concept.parquet")
      )
    }
  }

  invisible(NULL)
}

#' @keywords internal
getParquetComponents <- function() {
  c(
    "data_patients",
    "data_initial",
    "data_person",
    "data_features",
    "complementaryMappingTable"
  )
}

#' @keywords internal
prepareForNanoparquet <- function(df) {
  # Check if any column is integer64 and load bit64 if needed
  hasInteger64 <- any(vapply(df, inherits, logical(1), "integer64"))
  if (hasInteger64 && requireNamespace("bit64", quietly = TRUE) && !isNamespaceLoaded("bit64")) {
    loadNamespace("bit64")
  }

  for (colName in names(df)) {
    col <- df[[colName]]

    # Convert list columns to JSON strings for parquet compatibility
    if (is.list(col) && !is.data.frame(col)) {
      df[[colName]] <- vapply(col, function(x) {
        if (is.null(x) || (length(x) == 1 && is.na(x))) {
          return(NA_character_)
        }
        jsonlite::toJSON(x, auto_unbox = FALSE)
      }, character(1))
      next
    }

    # nanoparquet does not reliably handle integer64 columns
    if (inherits(col, "integer64")) {
      if (isNamespaceLoaded("bit64")) {
        df[[colName]] <- bit64::as.double.integer64(col)
      } else {
        df[[colName]] <- as.numeric(as.character(col))
      }
    }
  }

  df
}

#' @keywords internal
writeParquetComponents <- function(data, outputDir, verbose = FALSE) {
  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    stop(
      "Package 'nanoparquet' is required for writing parquet output.\n",
      "Install it with: install.packages('nanoparquet')"
    )
  }

  components <- getParquetComponents()
  written <- character(0)

  for (component in components) {
    if (!(component %in% names(data))) {
      if (isTRUE(verbose)) {
        message("  [WARN] ", component, " not found, skipping")
      }
      next
    }

    df <- data[[component]]
    if (!is.data.frame(df)) {
      if (isTRUE(verbose)) {
        message("  [WARN] ", component, " is not a data frame, skipping")
      }
      next
    }

    if (nrow(df) == 0) {
      if (isTRUE(verbose)) {
        message("  [WARN] ", component, " has 0 rows, skipping")
      }
      next
    }

    parquetPath <- file.path(outputDir, paste0(component, ".parquet"))
    dfPrepared <- prepareForNanoparquet(df)
    nanoparquet::write_parquet(dfPrepared, parquetPath)
    written <- c(written, parquetPath)

    if (isTRUE(verbose)) {
      message("  [OK] ", component, ".parquet (", nrow(df), " rows)")
    }
  }

  invisible(written)
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
#' @param inputString A state label name
#' @keywords internal

sanitizeSingle <- function(inputString) {
  safe_string <- gsub("[^A-Za-z0-9_.-]", "_", inputString)  # Corrected regex
  if (substr(safe_string, 1, 1) == '.') {
    safe_string <- paste0('_', safe_string)
  }
  return(safe_string)
}

#' Sanitize filenames not in correct format
#'
#' @param inputStrings A vector of state label names
#' @keywords internal

sanitize <- function(inputStrings) {
  # Apply the sanitization function to each element of the vector
  sapply(inputStrings, sanitizeSingle)
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
createCohortContrastObject <- function(data) {
  selectedFeatureData <- data$selectedFeatureData
  if (is.null(selectedFeatureData) && !is.null(data$trajectoryDataList)) {
    selectedFeatureData <- data$trajectoryDataList
  }

  obj <- list(
    data_patients = data$data_patients,
    data_initial = data$data_initial,
    data_person = data$data_person,
    data_features = data$data_features,
    conceptsData = data$conceptsData,
    complementaryMappingTable = data$complementaryMappingTable,
    selectedFeatureData = selectedFeatureData,
    trajectoryDataList = selectedFeatureData,
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

  selectedFeatureData <- x$selectedFeatureData
  if (is.null(selectedFeatureData) && !is.null(x$trajectoryDataList)) {
    selectedFeatureData <- x$trajectoryDataList
  }

  if (!is.null(selectedFeatureData)) {
    cat("selectedFeatureData (List): Feature selection outputs from the workflow\n")
    for (name in names(selectedFeatureData)) {
      cat(paste0(name, ":\n"))

      item <- selectedFeatureData[[name]]

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
