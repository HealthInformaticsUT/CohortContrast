
#' Run CohortContrast Analysis
#' @param cdm Connection to database
#' @param targetTable Table for target cohort (tbl)
#' @param controlTable Table for control cohort (tbl)
#' @param pathToResults Path to the results folder, can be project's working directory
#' @param domainsIncluded list of CDM domains to include
#' @param prevalenceCutOff numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param topK numeric > if set, keeps this number of features in the analysis. Maximum number of features exported.
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present, columns CONCEPT_ID, CONCEPT_NAME, NEW_CONCEPT_ID, NEW_CONCEPT_NAME, ABSTRACTION_LEVEL, TYPE
#' @param runChi2YTests Boolean for running the CHI2Y test (chi-squared test
#'   for two proportions with Yates continuity correction).
#' @param runLogitTests boolean for logit-tests
#' @param getAllAbstractions boolean for creating abstractions' levels for the imported data, this is useful when using GUI and exploring data
#' @param getSourceData boolean for fetching source data
#' @param maximumAbstractionLevel Maximum level of abstraction allowed
#' @param createOutputFiles Boolean for creating output files, the default value is TRUE
#' @param complName Name of the output study directory
#' @param runRemoveTemporalBias Logical; when `TRUE`, runs `removeTemporalBias()`
#'   as an optional post-processing step.
#' @param removeTemporalBiasArgs A list of additional arguments passed to
#'   `removeTemporalBias()` (for example `ratio`, `alpha`, `domainsIncluded`,
#'   `removeIdentified`). Missing arguments default to `ratio = 1`,
#'   `alpha = 0.05`, `domainsIncluded = NULL`, and `removeIdentified = TRUE`.
#' @param runAutomaticHierarchyCombineConcepts Logical; when `TRUE`, runs
#'   `automaticHierarchyCombineConcepts()` after temporal-bias processing.
#' @param automaticHierarchyCombineConceptsArgs A list of additional arguments
#'   passed to `automaticHierarchyCombineConcepts()` (for example
#'   `abstractionLevel`, `minDepthAllowed`, `allowOnlyMinors`). Missing
#'   arguments default to `abstractionLevel = -1`, `minDepthAllowed = 0`,
#'   and `allowOnlyMinors = TRUE`.
#' @param runAutomaticCorrelationCombineConcepts Logical; when `TRUE`, runs
#'   `automaticCorrelationCombineConcepts()` after hierarchy combining.
#' @param automaticCorrelationCombineConceptsArgs A list of additional arguments
#'   passed to `automaticCorrelationCombineConcepts()` (for example
#'   `abstractionLevel`, `minCorrelation`, `maxDaysInBetween`,
#'   `heritageDriftAllowed`). Missing arguments default to
#'   `abstractionLevel = -1`, `minCorrelation = 0.7`,
#'   `maxDaysInBetween = 1`, and `heritageDriftAllowed = FALSE`.
#' @param numCores Number of cores to allocate to parallel processing.
#'   Defaults to 20 percent of detected cores (minimum 1).
#' @return A CohortContrastObject. This is a list with the main analysis tables
#'   data_patients, data_initial, data_person, data_features, conceptsData,
#'   complementaryMappingTable, selectedFeatureData, trajectoryDataList, and
#'   config. Together these components contain the processed cohort-level,
#'   person-level, feature-level, optional mapping, and configuration outputs
#'   produced by the workflow.
#' @importFrom dplyr %>%
#' @importFrom foreach %dopar%
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("CDMConnector", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("duckdb", quietly = TRUE) &&
#'     CDMConnector::eunomiaIsAvailable("GiBleed")) {
#'   pathToJSON <- system.file(
#'     "example", "example_json", "diclofenac",
#'     package = "CohortContrast"
#'   )
#'   con <- DBI::dbConnect(
#'     duckdb::duckdb(),
#'     dbdir = CDMConnector::eunomiaDir("GiBleed")
#'   )
#'   cdm <- CDMConnector::cdmFromCon(
#'     con = con,
#'     cdmName = "eunomia",
#'     cdmSchema = "main",
#'     writeSchema = "main"
#'   )
#'
#'   targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
#'   controlTable <- createControlCohortInverse(cdm = cdm, targetTable = targetTable)
#'
#'   result <- CohortContrast(
#'     cdm = cdm,
#'     targetTable = targetTable,
#'     controlTable = controlTable,
#'     pathToResults = tempdir(),
#'     prevalenceCutOff = 1,
#'     topK = 3,
#'     presenceFilter = FALSE,
#'     runChi2YTests = TRUE,
#'     runLogitTests = TRUE,
#'     createOutputFiles = FALSE,
#'     numCores = 1
#'   )
#'
#'   head(result$data_features)
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
#'
CohortContrast <- function(cdm,
                           targetTable = NULL,
                           controlTable = NULL,
                           pathToResults = getwd(),
                           domainsIncluded = c(
                             "Drug",
                             "Condition",
                             "Measurement",
                             "Observation",
                             "Procedure",
                             "Visit",
                             "Visit detail",
                             "Death"
                           ),
                           prevalenceCutOff = 10,
                           topK = FALSE,
                           presenceFilter = 0.005,
                           complementaryMappingTable = NULL,
                           runChi2YTests = TRUE,
                           runLogitTests = TRUE,
                           getAllAbstractions = FALSE,
                           getSourceData = FALSE,
                           maximumAbstractionLevel = 5,
                           createOutputFiles = TRUE,
                           complName = NULL,
                           runRemoveTemporalBias = FALSE,
                           removeTemporalBiasArgs = list(),
                           runAutomaticHierarchyCombineConcepts = FALSE,
                           automaticHierarchyCombineConceptsArgs = list(),
                           runAutomaticCorrelationCombineConcepts = FALSE,
                           automaticCorrelationCombineConceptsArgs = list(),
                           numCores = max(1L, ceiling(0.2 * parallel::detectCores()), na.rm = TRUE)
                           ) {

  preparedInputs <- prepareCohortInputs(
    cdm = cdm,
    targetTable = targetTable,
    controlTable = controlTable,
    pathToResults = pathToResults
  )
  cdm <- preparedInputs$cdm
  pathToResults <- preparedInputs$pathToResults
  targetCohortId <- 2

  complementaryMappingTable <- normalizeComplementaryMappingTable(complementaryMappingTable)
  config <- buildCohortContrastConfig(
    complName = complName,
    domainsIncluded = domainsIncluded,
    topK = topK,
    prevalenceCutOff = prevalenceCutOff,
    presenceFilter = presenceFilter,
    runChi2YTests = runChi2YTests,
    runLogitTests = runLogitTests,
    getAllAbstractions = getAllAbstractions,
    getSourceData = getSourceData,
    runRemoveTemporalBias = runRemoveTemporalBias,
    runAutomaticHierarchyCombineConcepts = runAutomaticHierarchyCombineConcepts,
    runAutomaticCorrelationCombineConcepts = runAutomaticCorrelationCombineConcepts
  )

  data <- generateTables(
    cdm = cdm,
    domainsIncluded = domainsIncluded
  )
  data <- initializeCohortContrastData(
    data = data,
    cdm = cdm,
    config = config,
    complementaryMappingTable = complementaryMappingTable
  )
  data <- applyAbstractionMappings(
    data = data,
    cdm = cdm,
    complementaryMappingTable = complementaryMappingTable,
    getAllAbstractions = getAllAbstractions,
    maximumAbstractionLevel = maximumAbstractionLevel,
    numCores = numCores
  )

  if (getSourceData) {
    data <- generateSourceTables(
      data = data,
      domainsIncluded = domainsIncluded
    )
  }

  data <- createDataFeatures(data, topK)
  data <- handleTests(
    data = data,
    targetCohortId = targetCohortId,
    presenceFilter = presenceFilter,
    runChi2YTests = runChi2YTests,
    runLogitTests = runLogitTests,
    numCores = numCores
  )
  data <- handleFeatureSelection(
    data = data,
    pathToResults = pathToResults,
    topK = topK,
    prevalenceCutOff = prevalenceCutOff,
    targetCohortId = targetCohortId,
    runChi2YTests = runChi2YTests,
    runLogitTests = runLogitTests,
    createOutputFiles = createOutputFiles
  )
  data <- finalizeCohortContrastData(data)

  optionalProcessing <- runOptionalWorkflowSteps(
    data = data,
    cdm = cdm,
    runRemoveTemporalBias = runRemoveTemporalBias,
    removeTemporalBiasArgs = removeTemporalBiasArgs,
    runAutomaticHierarchyCombineConcepts = runAutomaticHierarchyCombineConcepts,
    automaticHierarchyCombineConceptsArgs = automaticHierarchyCombineConceptsArgs,
    runAutomaticCorrelationCombineConcepts = runAutomaticCorrelationCombineConcepts,
    automaticCorrelationCombineConceptsArgs = automaticCorrelationCombineConceptsArgs
  )
  data <- optionalProcessing$data

  if (optionalProcessing$stepsApplied) {
    data <- handleFeatureSelection(
      data = data,
      pathToResults = pathToResults,
      topK = topK,
      prevalenceCutOff = prevalenceCutOff,
      targetCohortId = "target",
      runChi2YTests = runChi2YTests,
      runLogitTests = runLogitTests,
      createOutputFiles = FALSE
    )
  }

  if (createOutputFiles) {
    createPathToResults(pathToResults = pathToResults)
    saveResult(data, pathToResults)
  }

  return(data)
}

emptyComplementaryMappingTable <- function() {
  data.frame(
    CONCEPT_ID = integer(),
    CONCEPT_NAME = character(),
    NEW_CONCEPT_ID = integer(),
    NEW_CONCEPT_NAME = character(),
    ABSTRACTION_LEVEL = integer(),
    TYPE = character(),
    stringsAsFactors = FALSE
  )
}

prepareCohortInputs <- function(cdm,
                                targetTable,
                                controlTable,
                                pathToResults) {
  cli::cli_alert_info("Making sure target and cohort tables are correct")
  assertRequiredCohortTable(targetTable)
  assertRequiredCohortTable(controlTable)
  pathToResults <- convertToAbsolutePath(pathToResults)

  list(
    cdm = createCohortContrastCdm(cdm = cdm, targetTable = targetTable, controlTable = controlTable),
    pathToResults = pathToResults
  )
}

normalizeComplementaryMappingTable <- function(complementaryMappingTable) {
  requiredColumns <- c(
    "CONCEPT_ID",
    "CONCEPT_NAME",
    "NEW_CONCEPT_ID",
    "NEW_CONCEPT_NAME",
    "ABSTRACTION_LEVEL",
    "TYPE"
  )
  if (!is.data.frame(complementaryMappingTable)) {
    return(emptyComplementaryMappingTable())
  }

  colnames(complementaryMappingTable) <- toupper(colnames(complementaryMappingTable))
  missingColumns <- setdiff(requiredColumns, colnames(complementaryMappingTable))
  if (length(missingColumns) > 0) {
    cli::cli_warn("`complementaryMappingTable` is missing required columns: {paste(missingColumns, collapse = ', ')}")
    for (column in missingColumns) {
      if (column == "TYPE") {
        complementaryMappingTable[[column]] <- rep("custom", nrow(complementaryMappingTable))
      } else if (column %in% c("CONCEPT_ID", "NEW_CONCEPT_ID", "ABSTRACTION_LEVEL")) {
        complementaryMappingTable[[column]] <- rep(NA_integer_, nrow(complementaryMappingTable))
      } else {
        complementaryMappingTable[[column]] <- rep(NA_character_, nrow(complementaryMappingTable))
      }
    }
  } else {
    cli::cli_alert_success("Inserted `complementaryMappingTable` has the correct format")
  }

  complementaryMappingTable[, requiredColumns, drop = FALSE]
}

buildCohortContrastConfig <- function(complName,
                                      domainsIncluded,
                                      topK,
                                      prevalenceCutOff,
                                      presenceFilter,
                                      runChi2YTests,
                                      runLogitTests,
                                      getAllAbstractions,
                                      getSourceData,
                                      runRemoveTemporalBias,
                                      runAutomaticHierarchyCombineConcepts,
                                      runAutomaticCorrelationCombineConcepts) {
  list(
    complName = complName,
    domainsIncluded = domainsIncluded,
    topK = topK,
    prevalenceCutOff = prevalenceCutOff,
    presenceFilter = presenceFilter,
    runChi2YTests = runChi2YTests,
    runLogitTests = runLogitTests,
    getAllAbstractions = getAllAbstractions,
    getSourceData = getSourceData,
    runRemoveTemporalBias = runRemoveTemporalBias,
    runAutomaticHierarchyCombineConcepts = runAutomaticHierarchyCombineConcepts,
    runAutomaticCorrelationCombineConcepts = runAutomaticCorrelationCombineConcepts
  )
}

normalizeOptionalStepArgs <- function(args, argName) {
  if (is.null(args)) {
    return(list())
  }
  if (!is.list(args)) {
    stop(paste0("`", argName, "` must be a list."))
  }
  args
}

formatOptionalArgValueForLog <- function(value) {
  if (is.null(value)) {
    return("NULL")
  }
  if (length(value) == 1) {
    if (is.character(value)) {
      return(paste0("\"", value, "\""))
    }
    return(as.character(value))
  }
  paste0(utils::capture.output(dput(value)), collapse = " ")
}

applyOptionalStepDefaults <- function(args, defaults, stepLabel) {
  argsWithDefaults <- args
  for (name in names(defaults)) {
    useDefault <- !(name %in% names(argsWithDefaults)) || is.null(argsWithDefaults[[name]])
    if (isTRUE(useDefault)) {
      argsWithDefaults[[name]] <- defaults[[name]]
      cli::cli_alert_info(
        "Using default {stepLabel} argument `{name} = {formatOptionalArgValueForLog(defaults[[name]])}` (not provided by user)."
      )
    }
  }
  argsWithDefaults
}

dropReservedOptionalArgs <- function(args, reservedNames, stepLabel) {
  if (length(args) == 0) {
    return(args)
  }
  argNames <- names(args)
  if (is.null(argNames)) {
    return(args)
  }

  reservedPresent <- intersect(argNames, reservedNames)
  if (length(reservedPresent) > 0) {
    cli::cli_warn(
      "Ignoring reserved arguments for {stepLabel}: {paste(reservedPresent, collapse = ', ')}"
    )
    args <- args[setdiff(argNames, reservedPresent)]
  }
  args
}

runOptionalWorkflowSteps <- function(data,
                                     cdm,
                                     runRemoveTemporalBias = FALSE,
                                     removeTemporalBiasArgs = list(),
                                     runAutomaticHierarchyCombineConcepts = FALSE,
                                     automaticHierarchyCombineConceptsArgs = list(),
                                     runAutomaticCorrelationCombineConcepts = FALSE,
                                     automaticCorrelationCombineConceptsArgs = list()) {
  removeTemporalBiasDefaults <- list(
    ratio = 1,
    alpha = 0.05,
    domainsIncluded = NULL,
    removeIdentified = TRUE
  )
  automaticHierarchyDefaults <- list(
    abstractionLevel = -1,
    minDepthAllowed = 0,
    allowOnlyMinors = TRUE
  )
  automaticCorrelationDefaults <- list(
    abstractionLevel = -1,
    minCorrelation = 0.7,
    maxDaysInBetween = 1,
    heritageDriftAllowed = FALSE
  )

  stepsApplied <- FALSE

  if (isTRUE(runRemoveTemporalBias)) {
    removeTemporalBiasArgs <- normalizeOptionalStepArgs(removeTemporalBiasArgs, "removeTemporalBiasArgs")
    removeTemporalBiasArgs <- applyOptionalStepDefaults(
      args = removeTemporalBiasArgs,
      defaults = removeTemporalBiasDefaults,
      stepLabel = "removeTemporalBias()"
    )
    cli::cli_alert_info("Running optional step: removeTemporalBias()")
    callArgs <- dropReservedOptionalArgs(
      args = removeTemporalBiasArgs,
      reservedNames = c("data", "cdm"),
      stepLabel = "removeTemporalBias()"
    )
    temporalBiasResult <- do.call(removeTemporalBias, c(list(data = data, cdm = cdm), callArgs))
    if (is.list(temporalBiasResult) && !is.null(temporalBiasResult$data)) {
      data <- temporalBiasResult$data
    } else {
      cli::cli_warn("removeTemporalBias() did not return a list with a `data` element. Keeping the existing object.")
    }
    stepsApplied <- TRUE
  }

  if (isTRUE(runAutomaticHierarchyCombineConcepts)) {
    automaticHierarchyCombineConceptsArgs <- normalizeOptionalStepArgs(
      automaticHierarchyCombineConceptsArgs,
      "automaticHierarchyCombineConceptsArgs"
    )
    automaticHierarchyCombineConceptsArgs <- applyOptionalStepDefaults(
      args = automaticHierarchyCombineConceptsArgs,
      defaults = automaticHierarchyDefaults,
      stepLabel = "automaticHierarchyCombineConcepts()"
    )
    cli::cli_alert_info("Running optional step: automaticHierarchyCombineConcepts()")
    callArgs <- dropReservedOptionalArgs(
      args = automaticHierarchyCombineConceptsArgs,
      reservedNames = "data",
      stepLabel = "automaticHierarchyCombineConcepts()"
    )
    data <- do.call(automaticHierarchyCombineConcepts, c(list(data = data), callArgs))
    stepsApplied <- TRUE
  }

  if (isTRUE(runAutomaticCorrelationCombineConcepts)) {
    automaticCorrelationCombineConceptsArgs <- normalizeOptionalStepArgs(
      automaticCorrelationCombineConceptsArgs,
      "automaticCorrelationCombineConceptsArgs"
    )
    automaticCorrelationCombineConceptsArgs <- applyOptionalStepDefaults(
      args = automaticCorrelationCombineConceptsArgs,
      defaults = automaticCorrelationDefaults,
      stepLabel = "automaticCorrelationCombineConcepts()"
    )
    cli::cli_alert_info("Running optional step: automaticCorrelationCombineConcepts()")
    callArgs <- dropReservedOptionalArgs(
      args = automaticCorrelationCombineConceptsArgs,
      reservedNames = "data",
      stepLabel = "automaticCorrelationCombineConcepts()"
    )
    data <- do.call(automaticCorrelationCombineConcepts, c(list(data = data), callArgs))
    stepsApplied <- TRUE
  }

  list(data = data, stepsApplied = stepsApplied)
}

initializeCohortContrastData <- function(data, cdm, config, complementaryMappingTable) {
  data$config <- config
  data$complementaryMappingTable <- complementaryMappingTable
  data$conceptsData <- list(
    concept_ancestor = cdm$concept_ancestor %>% as.data.frame(),
    concept = cdm$concept %>% as.data.frame() %>% dplyr::filter(is.na(.data$invalid_reason))
  )
  data
}

applyAbstractionMappings <- function(data,
                                     cdm,
                                     complementaryMappingTable,
                                     getAllAbstractions,
                                     maximumAbstractionLevel,
                                     numCores) {
  needsMapping <- getAllAbstractions || nrow(complementaryMappingTable) > 0
  if (!needsMapping) {
    return(data)
  }

  cl <- parallel::makeCluster(numCores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  doParallel::registerDoParallel(cl)

  if (getAllAbstractions) {
    data$data_patients$PERSON_ID <- as.integer(data$data_patients$PERSON_ID)
    assertAncestryCompleteness(cdm)
    printCustomMessage("Get data for all concept abstraction levels ...")
    maxMinLevels <- data$conceptsData$concept_ancestor %>%
      dplyr::group_by(.data$descendant_concept_id) %>%
      dplyr::summarise(maximum_minimal_separation = max(.data$max_levels_of_separation))

    maxAbstraction <- NULL
    newDataPatients <- foreach::foreach(
      maxAbstraction = maximumAbstractionLevel:0,
      .combine = rbind,
      .packages = c("CohortContrast", "dplyr"),
      .export = c("maxAbstraction")
    ) %dopar% {
      abstractionMapping <- generateMappingTable(
        abstractionLevel = maxAbstraction,
        data = data,
        maxMinDataFrame = maxMinLevels
      )
      abstractionMapping <- updateMapping(abstractionMapping)
      newData <- handleMapping(
        data = data,
        complementaryMappingTable = abstractionMapping,
        abstractionLevel = maxAbstraction
      )
      newData$PERSON_ID <- as.integer(newData$PERSON_ID)
      newData
    }
    data$data_patients <- rbind(data$data_patients, newDataPatients)
  }

  if (nrow(complementaryMappingTable) == 0) {
    return(data)
  }

  complementaryMappingTable <- updateMapping(complementaryMappingTable)
  dataMapped <- data$data_patients[0, ]
  for (abstractionLevel in unique(complementaryMappingTable$ABSTRACTION_LEVEL)) {
    dataMappedAbstraction <- handleMapping(data, complementaryMappingTable, abstractionLevel)
    dataMapped <- rbind(dataMapped, dataMappedAbstraction)
    conceptIdsToRemove <- complementaryMappingTable %>%
      dplyr::filter(.data$ABSTRACTION_LEVEL == abstractionLevel) %>%
      dplyr::pull(.data$CONCEPT_ID)
    data$data_patients <- data$data_patients %>%
      dplyr::filter(!(.data$CONCEPT_ID %in% conceptIdsToRemove & .data$ABSTRACTION_LEVEL == abstractionLevel))
  }
  data$data_patients <- rbind(data$data_patients, dataMapped)
  data$complementaryMappingTable <- complementaryMappingTable
  data
}

finalizeCohortContrastData <- function(data) {
  data$data_patients <- dplyr::mutate(
    data$data_patients,
    COHORT_DEFINITION_ID = dplyr::if_else(.data$COHORT_DEFINITION_ID == 2, "target", "control")
  )
  data$data_initial <- dplyr::mutate(
    data$data_initial,
    COHORT_DEFINITION_ID = dplyr::if_else(.data$COHORT_DEFINITION_ID == 2, "target", "control")
  )
  if (!is.data.frame(data$complementaryMappingTable)) {
    data$complementaryMappingTable <- emptyComplementaryMappingTable()
  }
  if (is.null(data$selectedFeatureData) && !is.null(data$trajectoryDataList)) {
    data$selectedFeatureData <- data$trajectoryDataList
  }
  if (is.null(data$trajectoryDataList) && !is.null(data$selectedFeatureData)) {
    data$trajectoryDataList <- data$selectedFeatureData
  }
  createCohortContrastObject(data)
}
