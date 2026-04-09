#' Load a Saved CohortContrast Study
#'
#' Loads a saved study folder produced by `CohortContrast()` and reconstructs a
#' `CohortContrastObject` for downstream post-processing.
#'
#' @param studyName Name of the study folder inside `pathToResults`.
#' @param pathToResults Path to the parent directory containing study folders.
#'   Defaults to current working directory.
#'
#' @return A `CohortContrastObject`.
#' @examples
#' if (requireNamespace("nanoparquet", quietly = TRUE)) {
#'   studyDir <- system.file("example", "st", package = "CohortContrast")
#'   study <- loadCohortContrastStudy("lc500", pathToResults = studyDir)
#'   class(study)
#' }
#' @export
loadCohortContrastStudy <- function(studyName, pathToResults = getwd()) {
  if (!is.character(studyName) || length(studyName) != 1 || !nzchar(studyName)) {
    stop("`studyName` must be a non-empty character string.")
  }

  pathToResults <- normalizePath(pathToResults, mustWork = TRUE)
  studyPath <- file.path(pathToResults, studyName)
  if (!dir.exists(studyPath)) {
    stop("Study folder does not exist: ", studyPath)
  }

  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    stop(
      "Package 'nanoparquet' is required for loading parquet output.\n",
      "Install it with: install.packages('nanoparquet')"
    )
  }

  data <- list(
    data_patients = readStudyParquetComponent(studyPath, "data_patients"),
    data_initial = readStudyParquetComponent(studyPath, "data_initial"),
    data_person = readStudyParquetComponent(studyPath, "data_person"),
    data_features = readStudyParquetComponent(studyPath, "data_features"),
    complementaryMappingTable = readStudyParquetComponent(studyPath, "complementaryMappingTable")
  )

  required <- c("data_patients", "data_initial", "data_person", "data_features")
  missingRequired <- required[vapply(required, function(x) is.null(data[[x]]), logical(1))]
  if (length(missingRequired) > 0) {
    stop(
      "Study folder is missing required parquet files: ",
      paste(paste0(missingRequired, ".parquet"), collapse = ", ")
    )
  }

  if (!is.data.frame(data$complementaryMappingTable)) {
    data$complementaryMappingTable <- emptyComplementaryMappingTable()
  }

  metadata <- readJsonFileIfExists(file.path(studyPath, "metadata.json"))
  config <- readJsonFileIfExists(file.path(studyPath, "config.json"))
  if (!is.list(config)) {
    config <- list()
  }
  if (is.null(config$complName) || !nzchar(config$complName)) {
    config$complName <- studyName
  }
  if (is.list(metadata)) {
    config$metadata <- metadata
  }
  data$config <- config

  selectedFeatureData <- loadSelectedFeatureData(studyPath = studyPath)
  if (is.null(selectedFeatureData)) {
    selectedFeatureData <- reconstructSelectedFeatureData(
      dataFeatures = data$data_features,
      dataPatients = data$data_patients,
      config = data$config
    )
  }

  data$selectedFeatureData <- selectedFeatureData
  data$trajectoryDataList <- selectedFeatureData
  data$conceptsData <- loadConceptsData(studyPath)

  createCohortContrastObject(data)
}

#' @keywords internal
readJsonFileIfExists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE),
    error = function(e) {
      warning("Failed to read JSON file: ", path, " (", conditionMessage(e), ")")
      NULL
    }
  )
}

#' @keywords internal
deserializeJsonArrayColumn <- function(column) {
  lapply(column, function(value) {
    if (is.null(value) || (length(value) == 1 && is.na(value))) {
      return(numeric(0))
    }
    if (!is.character(value) || !nzchar(value)) {
      return(value)
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(value),
      error = function(e) value
    )

    if (is.null(parsed)) {
      return(numeric(0))
    }
    parsed
  })
}

#' @keywords internal
readStudyParquetComponent <- function(studyPath, component) {
  componentPath <- file.path(studyPath, paste0(component, ".parquet"))
  if (!file.exists(componentPath)) {
    return(NULL)
  }

  data <- as.data.frame(nanoparquet::read_parquet(componentPath), stringsAsFactors = FALSE)

  if ("TIME_TO_EVENT" %in% colnames(data) && is.character(data$TIME_TO_EVENT)) {
    data$TIME_TO_EVENT <- deserializeJsonArrayColumn(data$TIME_TO_EVENT)
  }

  data
}

#' @keywords internal
loadSelectedFeatureData <- function(studyPath) {
  payload <- readJsonFileIfExists(file.path(studyPath, "selected_feature_data.json"))
  selectedFeatures <- readStudyParquetComponent(studyPath, "selected_features")

  if (!is.list(payload) && !is.data.frame(selectedFeatures)) {
    return(NULL)
  }

  selectedFeatureNames <- character(0)
  selectedFeatureIds <- numeric(0)

  if (is.list(payload)) {
    if (!is.null(payload$selectedFeatureNames)) {
      selectedFeatureNames <- as.character(unlist(payload$selectedFeatureNames, use.names = FALSE))
    }
    if (!is.null(payload$selectedFeatureIds)) {
      selectedFeatureIds <- unlist(payload$selectedFeatureIds, use.names = FALSE)
    }
  }

  if (!is.data.frame(selectedFeatures)) {
    selectedFeatures <- data.frame()
  }
  if (length(selectedFeatureIds) == 0 && "CONCEPT_ID" %in% colnames(selectedFeatures)) {
    selectedFeatureIds <- selectedFeatures$CONCEPT_ID
  }
  if (length(selectedFeatureNames) == 0 && "CONCEPT_NAME" %in% colnames(selectedFeatures)) {
    selectedFeatureNames <- unique(selectedFeatures$CONCEPT_NAME)
  }

  list(
    selectedFeatureNames = selectedFeatureNames,
    selectedFeatureIds = selectedFeatureIds,
    selectedFeatures = selectedFeatures
  )
}

#' @keywords internal
reconstructSelectedFeatureData <- function(dataFeatures, dataPatients, config) {
  if (!is.data.frame(dataFeatures) || nrow(dataFeatures) == 0) {
    return(list(
      selectedFeatureNames = character(0),
      selectedFeatureIds = numeric(0),
      selectedFeatures = data.frame()
    ))
  }

  runChi2YTests <- isTRUE(config$runChi2YTests)
  runLogitTests <- isTRUE(config$runLogitTests)
  prevalenceCutOff <- config$prevalenceCutOff
  if (is.null(prevalenceCutOff) || !is.numeric(prevalenceCutOff)) {
    prevalenceCutOff <- 0
  }
  topK <- config$topK
  if (is.null(topK) && is.list(config$metadata) && !is.null(config$metadata$topKInt)) {
    topK <- config$metadata$topKInt
  }

  topKInt <- 0L
  useTopK <- FALSE
  if (!is.null(topK) && length(topK) > 0 && !isFALSE(topK)) {
    parsedTopK <- suppressWarnings(as.integer(topK[[1]]))
    if (!is.na(parsedTopK) && parsedTopK > 0) {
      topKInt <- parsedTopK
      useTopK <- TRUE
    }
  }

  features <- dataFeatures
  if (runChi2YTests || runLogitTests) {
    keep <- rep(FALSE, nrow(features))
    if ("CHI2Y" %in% colnames(features)) {
      keep <- keep | as.logical(features$CHI2Y)
    }
    if ("LOGITTEST" %in% colnames(features)) {
      keep <- keep | as.logical(features$LOGITTEST)
    }
    features <- features[keep, , drop = FALSE]
  }

  if (!useTopK) {
    if ("PREVALENCE_DIFFERENCE_RATIO" %in% colnames(features)) {
      features <- features[features$PREVALENCE_DIFFERENCE_RATIO > prevalenceCutOff, , drop = FALSE]
    }
  } else {
    if (topKInt > 0 && "PREVALENCE_DIFFERENCE_RATIO" %in% colnames(features)) {
      ordered <- features[order(features$PREVALENCE_DIFFERENCE_RATIO, decreasing = TRUE), , drop = FALSE]
      topConceptIds <- utils::head(ordered$CONCEPT_ID, topKInt)
      keep <- features$CONCEPT_ID %in% topConceptIds
      if ("ABSTRACTION_LEVEL" %in% colnames(features)) {
        keep <- keep & features$ABSTRACTION_LEVEL == -1
      }
      features <- features[keep, , drop = FALSE]
    }
  }

  selectedFeatureNames <- character(0)
  if (is.data.frame(dataPatients) && nrow(dataPatients) > 0 && nrow(features) > 0) {
    targetMask <- dataPatients$COHORT_DEFINITION_ID %in% c("target", "2", 2)
    abstractionMask <- TRUE
    if ("ABSTRACTION_LEVEL" %in% colnames(dataPatients)) {
      abstractionMask <- dataPatients$ABSTRACTION_LEVEL == -1
    }
    conceptMask <- dataPatients$CONCEPT_ID %in% features$CONCEPT_ID

    targetPatients <- dataPatients[targetMask & abstractionMask & conceptMask, c("PERSON_ID", "CONCEPT_NAME", "PREVALENCE"), drop = FALSE]
    if (nrow(targetPatients) > 0) {
      summarized <- stats::aggregate(PREVALENCE ~ PERSON_ID + CONCEPT_NAME, data = targetPatients, FUN = mean)
      if (nrow(summarized) > 0) {
        transformed <- tidyr::pivot_wider(
          summarized,
          names_from = "CONCEPT_NAME",
          values_from = "PREVALENCE",
          values_fill = 0
        )
        if (ncol(transformed) > 1) {
          selectedFeatureNames <- colnames(transformed)[-1]
        }
      }
    }
  }

  list(
    selectedFeatureNames = selectedFeatureNames,
    selectedFeatureIds = features$CONCEPT_ID,
    selectedFeatures = features
  )
}

#' @keywords internal
loadConceptsData <- function(studyPath) {
  conceptAncestor <- readStudyParquetComponent(studyPath, "concepts_concept_ancestor")
  concept <- readStudyParquetComponent(studyPath, "concepts_concept")

  if (!is.data.frame(conceptAncestor) && !is.data.frame(concept)) {
    return(NULL)
  }

  list(
    concept_ancestor = conceptAncestor,
    concept = concept
  )
}
