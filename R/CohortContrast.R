
#' Run CohortContrast Analysis
#' @param cdm Connection to database
#' @param targetTable Table for target cohort (tbl)
#' @param controlTable Table for control cohort (tbl)
#' @param pathToResults Path to the results folder, can be project's working directory
#' @param domainsIncluded list of CDM domains to include
#' @param prevalenceCutOff numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param topK numeric > if set, keeps this number of features in the analysis. Maximum number of features exported.
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present, columns CONCEPT_ID, CONCEPT_NAME, NEW_CONCEPT_ID, NEW_CONCEPT_NAME, ABSTRACTION_LEVEL,
#' @param runZTests boolean for Z-tests
#' @param runLogitTests boolean for logit-tests
#' @param runKSTests boolean for Kolmogorov-Smirnov tests
#' @param getAllAbstractions boolean for creating abstractions' levels for the imported data, this is useful when using GUI and exploring data
#' @param getSourceData boolean for fetching source data
#' @param maximumAbstractionLevel Maximum level of abstraction allowed
#' @param createOutputFiles Boolean for creating output files, the default value is TRUE
#' @param lookbackDays FALSE or an integer stating the lookback period for cohort index date
#' @param complName Name of the output file
#' @param numCores Number of cores to allocate to parallel processing
#' @importFrom dplyr %>%
#' @importFrom foreach %dopar%
#' @export
#' @examples
#' \dontrun{
#' control <- data.frame(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(5325, 3743, 2980, 1512, 2168),
#'   cohort_start_date = as.Date(c("1982-06-02", "1997-03-23",
#'    "2004-09-29", "2006-08-11", "1977-06-25")),
#'   cohort_end_date = as.Date(c("2019-03-17", "2018-10-07",
#'    "2018-04-01", "2017-11-29", "2018-11-22"))
#' )
#'
#' target <- data.frame(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(4804, 4861, 1563, 2830, 1655),
#'   cohort_start_date = as.Date(c("1997-03-23", "1982-06-02",
#'    "1977-06-25", "2006-08-11", "2004-09-29")),
#'   cohort_end_date = as.Date(c("2018-10-29", "2019-05-23",
#'    "2019-04-20", "2019-01-14", "2019-05-24"))
#' )
#'
#' control$cohort_definition_id = 100
#' target$cohort_definition_id = 500
#'
#' cohort = rbind(control, target)
#'
#' con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
#' DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS example")
#' DBI::dbWriteTable(con,   DBI::SQL('"example"."cohort"'), cohort)
#'
#' cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia",
#'  cdm_schema = "main", write_schema = "main")
#'
#'  targetTable <- cohortFromCohortTable(cdm = cdm, db = con,
#'   tableName = "cohort", schemaName = 'example', cohortId = 500)
#' controlTable <- cohortFromCohortTable(cdm = cdm, db = con,
#'  tableName = "cohort", schemaName = 'example', cohortId = 100)
#'
#'
#' pathToResults = getwd()
#'
#' data = CohortContrast(
#'   cdm,
#'   targetTable = targetTable,
#'   controlTable = controlTable,
#'   pathToResults,
#'   domainsIncluded = c(
#'     "Drug"
#'   ),
#'   prevalenceCutOff = 0.1,
#'   topK = FALSE,
#'   presenceFilter = 0.005,
#'   complementaryMappingTable = NULL,
#'   runZTests = FALSE,
#'   runLogitTests = FALSE,
#'   createOutputFiles = FALSE,
#'   numCores = 1
#' )
#'
#' DBI::dbDisconnect(con)
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
                           lookbackDays = FALSE,
                           complementaryMappingTable = NULL,
                           runZTests = TRUE,
                           runLogitTests = TRUE,
                           runKSTests = TRUE,
                           getAllAbstractions = FALSE,
                           getSourceData = FALSE,
                           maximumAbstractionLevel = 5,
                           createOutputFiles = TRUE,
                           complName = NULL,
                           numCores = parallel::detectCores() - 1
                           ) {
  # Setup
  cli::cli_alert_info("Making sure target and cohort tables are correct")
  assertRequiredCohortTable(targetTable)
  assertRequiredCohortTable(controlTable)
  pathToResults = convertToAbsolutePath(pathToResults)
  if(!isFALSE(lookbackDays)){
    cli::cli_alert_info("Implementing lookback days")

    targetTable = targetTable %>% dplyr::mutate(cohort_start_date - lookbackDays)
    targetTable = resolveCohortTableOverlaps(targetTable, cdm = cdm)
    controlTable = controlTable %>% dplyr::mutate(cohort_start_date - lookbackDays)
    controlTable = resolveCohortTableOverlaps(controlTable, cdm = cdm)
  }

  cdm <- createCohortContrastCdm(cdm = cdm, targetTable = targetTable, controlTable = controlTable)
  targetCohortId = 2

  if(is.null(complementaryMappingTable)){
    complementaryMappingTable = data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), NEW_CONCEPT_ID = integer(), NEW_CONCEPT_NAME = character() , ABSTRACTION_LEVEL = integer(), stringsAsFactors = FALSE)
  }

  config = list(
    complName = complName,
    domainsIncluded = domainsIncluded,
    topK = topK,
    prevalenceCutOff = prevalenceCutOff,
    presenceFilter = presenceFilter,
    runZTests = runZTests,
    runLogitTests = runLogitTests,
    runKSTests = runKSTests,
    getAllAbstractions = getAllAbstractions,
    getSourceData = getSourceData,
    lookbackDays = lookbackDays
  )

  data = generateTables(
    cdm = cdm,
    domainsIncluded = domainsIncluded
  )

  data$config = config
  data$complementaryMappingTable = complementaryMappingTable
  cl <- parallel::makeCluster(numCores)
  doParallel::registerDoParallel(cl)

  if (is.data.frame(complementaryMappingTable) | getAllAbstractions) {
    # Get stable tables
    # TODO Foward the datasets as cdm, fetch data later when they are smaller datasets
    if (getAllAbstractions) {
      concept_ancestor = cdm$concept_ancestor %>% as.data.frame()
      concept = cdm$concept %>% as.data.frame()

      data$data_patients$PERSON_ID = as.integer(data$data_patients$PERSON_ID)
      #assertAncestryCompleteness(cdm)
      printCustomMessage("Get data for all concept abstraction levels ...")
      max_min_levels <- concept_ancestor %>%
        dplyr::group_by(.data$descendant_concept_id) %>%
        dplyr::summarise(maximum_minimal_separation = max(.data$max_levels_of_separation))
      updateMapping <- updateMapping
      generateMappingTable <- generateMappingTable
      handleMapping <- handleMapping
      maxAbstraction <- NULL
      new_data_patients <- foreach::foreach(maxAbstraction = maximumAbstractionLevel:0, .combine = rbind, .packages = c("CohortContrast", "dplyr"),
                                            .export = c("maxAbstraction")) %dopar% {
                                              # Load necessary libraries in each worker
                                              complementaryMappingTable = generateMappingTable(cdm = cdm, abstractionLevel = maxAbstraction, data = data, concept_ancestor = concept_ancestor, concept = concept, maxMinDataFrame =max_min_levels)
                                              complementaryMappingTable1 <- updateMapping(complementaryMappingTable)
                                              newData <- handleMapping(data = data, complementaryMappingTable = complementaryMappingTable, abstractionLevel = maxAbstraction)
                                              newData$PERSON_ID = as.integer(newData$PERSON_ID)
                                              return(newData)
                                            }
      data$data_patients <- rbind(data$data_patients, new_data_patients)
    }
    if (is.data.frame(complementaryMappingTable)) {
      # Create empty dataframe
      complementaryMappingTable = updateMapping(complementaryMappingTable)
      data_mapped = data$data_patients[0, ]
      # Add mappings and remove old
      for (abstraction_level in unique(complementaryMappingTable$ABSTRACTION_LEVEL)) {
        # Add mappings
        data_mapped_abstarction_level = handleMapping(data, complementaryMappingTable, abstraction_level)
        data_mapped = rbind(data_mapped, data_mapped_abstarction_level)
        # Remove old
        concept_ids_to_remove = complementaryMappingTable %>% dplyr::filter(.data$ABSTRACTION_LEVEL == abstraction_level) %>% dplyr::pull(.data$CONCEPT_ID)
        data$data_patients = data$data_patients %>% dplyr::filter(!(.data$CONCEPT_ID %in% concept_ids_to_remove & .data$ABSTRACTION_LEVEL == abstraction_level))
      }
      # Add new mappings
      data$data_patients <- rbind(data$data_patients, data_mapped)
    }
  }
  parallel::stopCluster(cl)

  #?
  if (getSourceData) {
    data = generateSourceTables(
      data = data,
      domainsIncluded = domainsIncluded
    )
  }


  data = createDataFeatures(data, topK)
  data = handleTests(data,
                     targetCohortId,
                     presenceFilter,
                     runZTests,
                     runLogitTests,
                     runKSTests,
                     numCores)

  data = handleFeatureSelection(data = data, pathToResults = pathToResults, topK = topK, prevalenceCutOff = prevalenceCutOff, targetCohortId = targetCohortId, runZTests = runZTests, runLogitTests = runLogitTests, createOutputFiles = createOutputFiles)

  # Change ids to explicitly state "target" or "cohort"
  data$data_patients = dplyr::mutate(data$data_patients, COHORT_DEFINITION_ID = dplyr::if_else(.data$COHORT_DEFINITION_ID == 2, "target", "control"))
  data$data_initial = dplyr::mutate(data$data_initial, COHORT_DEFINITION_ID = dplyr::if_else(.data$COHORT_DEFINITION_ID == 2, "target", "control"))

  data$complementaryMappingTable = if (!(is.null(complementaryMappingTable) | isFALSE(complementaryMappingTable))) complementaryMappingTable else data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), NEW_CONCEPT_ID = integer(), NEW_CONCEPT_NAME = character(), ABSTRACTION_LEVEL = integer(), stringsAsFactors = FALSE)

  data = create_CohortContrast_object(data)

  if (createOutputFiles){
    createPathToResults(pathToResults = pathToResults)
    saveResult(data, pathToResults)
  }

  return(data)
}


#' This function outputs a list of domain specigic data which is filtered by the worfklow configuration
#' @param dataPatient Data of the selected patients
#' @param cdm Connection to database
#' @param split_data Data split by heritage
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present
#'
#' @keywords internal

queryHeritageData <-
  function(dataPatient,
           cdm,
           split_data,
           complementaryMappingTable = NULL) {
    results_list <- lapply(names(split_data), function(heritage) {
      printCustomMessage(paste("Querying eligible ", heritage, " data ...", sep = ""))
      unique_person_ids <- unique(split_data[[heritage]]$PERSON_ID)
      complementaryMappingTable_ids = NULL
      if(is.data.frame(complementaryMappingTable)){
      complementaryMappingTable_ids = complementaryMappingTable$CONCEPT_ID
      }
      # Filter for tests
      unique_concept_ids <- unique(
        c(unique(split_data[[heritage]]$CONCEPT_ID), unique(complementaryMappingTable_ids)))


      query_result <- NULL
      if (heritage == 'condition_occurrence') {
        query_result <-
          cdm$condition_occurrence %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                         by = c("person_id" = "subject_id")) %>% dplyr::filter(.data$condition_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'drug_exposure') {
        query_result <-
          cdm$drug_exposure %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                  by = c("person_id" = "subject_id")) %>% dplyr::filter(.data$drug_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'measurement') {
        query_result <-
          cdm$measurement %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                by = c("person_id" = "subject_id")) %>% dplyr::filter(.data$measurement_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'procedure_occurrence') {
        query_result <-
          cdm$procedure_occurrence %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                         by = c("person_id" = "subject_id")) %>% dplyr::filter(.data$procedure_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'observation') {
        query_result <-
          cdm$observation %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                by = c("person_id" = "subject_id")) %>% dplyr::filter(.data$observation_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'visit_occurrence') {
        query_result <-
          cdm$visit_occurrence %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                     by = c("person_id" = "subject_id")) %>% dplyr::filter(.data$visit_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'visit_detail') {
        query_result <-
          cdm$visit_detail %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                 by = c("person_id" = "subject_id")) %>% dplyr::filter(.data$visit_detail_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'death') {
        query_result <-
          cdm$death %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                 by = c("person_id" = "subject_id")) %>% dplyr::filter(.data$death_type_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      # Further processing...
      colnames(query_result) = toupper(colnames(query_result))
      concept_string = toupper(sub("_occurrence_", "_", sub(
        "_exposure_", "_", paste(heritage, 'concept_id', sep = "_")
      )))
      # Ensure concept_map is created correctly
      concept_map <- dataPatient %>% dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME) %>% dplyr::distinct()
        # unique(dataPatient[c("CONCEPT_ID", "CONCEPT_NAME")])

      if (is.data.frame(complementaryMappingTable)) {
        # Join the dataframes on CONCEPT_ID
        concept_map <-
          dplyr::select(
            dplyr::mutate(
              dplyr::full_join(
                concept_map,
                complementaryMappingTable,
                by = "CONCEPT_NAME",
                suffix = c("", ".old")
              ),
              # Choose the complementaryMappingTable CONCEPT_NAME if it's not NA; otherwise, use the original
              CONCEPT_ID.old = dplyr::if_else(
                is.na(.data$CONCEPT_ID.old),
                .data$CONCEPT_ID,
                .data$CONCEPT_ID.old
              )
            ),
            # Select and rename the columns to match the original concept_map structure
            CONCEPT_ID = .data$CONCEPT_ID.old,
            .data$CONCEPT_NAME
          )

        # Ensuring that the updated_concept_map contains unique CONCEPT_ID entries
      }
      concept_map[[concept_string]] <-
        as.character(concept_map[['CONCEPT_ID']]) # Ensure consistent data type
      #colnames(concept_map) <- c(concept_string, "CONCEPT_NAME")
      # Before applying the mapping, check if query_result is not empty and has 'CONCEPT_ID'
      if (nrow(query_result) > 0 &&
          concept_string %in% names(query_result)) {
        query_result[[concept_string]] <-
          as.character(query_result[[concept_string]]) # Consistent data type for mapping

        # Perform the mapping using a safe merge, with checks
        if (nrow(concept_map) > 0) {
          query_result_mapped <-
            merge(query_result,
                  concept_map,
                  by = concept_string,
                  all.x = TRUE)
        } else {
          # Handle the case where concept_map is empty
          warning("Concept map is empty. Cannot perform mapping.")
          query_result_mapped <- query_result
        }
      } else {
        # Handle cases where query_result is empty or does not have 'CONCEPT_ID'
        warning("Query result is empty or lacks CONCEPT_ID. Skipping mapping.")
        query_result_mapped <- query_result
      }

      # Use grep to find column names that end with "DATETIME"
      columns_ending_with_datetime <-
        grep("DATETIME$", names(query_result), value = TRUE)

      # Check if we found any columns and get the first one
      if (length(columns_ending_with_datetime) > 1) {
        first_column_ending_with_datetime <- columns_ending_with_datetime[1]
        second_column_ending_with_datetime <- columns_ending_with_datetime[2]
      } else if (length(columns_ending_with_datetime) == 1) {
        first_column_ending_with_datetime <- columns_ending_with_datetime[1]
        second_column_ending_with_datetime <- columns_ending_with_datetime[1]
      } else {
        first_column_ending_with_datetime <-
          NULL # or any other appropriate action
        second_column_ending_with_datetime <-
          NULL
      }
      query_result_mapped$START_DATE = query_result_mapped[[first_column_ending_with_datetime]]
      query_result_mapped$END_DATE = query_result_mapped[[second_column_ending_with_datetime]]
      query_result_mapped = query_result_mapped %>% dplyr::mutate(
        END_DATE = dplyr::if_else(
          .data$START_DATE == .data$END_DATE,
          .data$END_DATE + lubridate::days(1),
          .data$END_DATE
        )
      )
      query_result_mapped$CONCEPT_ID = query_result_mapped[["CONCEPT_NAME"]]
      query_result_mapped = dplyr::select(
        query_result_mapped,
        .data$CONCEPT_ID,
        .data$PERSON_ID,
        .data$START_DATE,
        .data$END_DATE
      )
      query_result_mapped = dplyr::filter(query_result_mapped,
                                          .data$PERSON_ID %in% as.integer(unique_person_ids))
      printCustomMessage(paste("Quering eligible ", heritage, " data finished.", sep = ""))
      return(query_result_mapped)
    })

    return(results_list)
  }

#'  This function performs Z-tests on the patient prevalence data target vs control
#'
#' @param data_patients Prevalence data for patients
#' @param data_initial Imported cohort dataframe
#' @param targetCohortId Target cohort id
#' @param presenceFilter Presence filter 0-1, if 0.1 then feature has to be present for at least 10 percent of patients
#' @param numCores Number of cores to allocate to parallel processing
#' @importFrom foreach %dopar%
#'
#' @keywords internal
performPrevalenceAnalysis <- function(data_patients,
                                      data_initial,
                                      targetCohortId,
                                      presenceFilter,
                                      numCores = parallel::detectCores() - 1) {
  printCustomMessage("Running Z-tests ...")
  # Aggregate the prevalence data for each concept within each cohort
  agg_data <- dplyr::summarise(
    dplyr::group_by(
      dplyr::mutate(data_patients, PREVALENCE = dplyr::if_else(.data$PREVALENCE > 0, 1, 0)),
      .data$COHORT_DEFINITION_ID,
      .data$ABSTRACTION_LEVEL,
      .data$CONCEPT_ID
    ),
    TOTAL_PREVALENCE = sum(.data$PREVALENCE),
    .groups = 'drop'
  )
  # Separate the data for each cohort for easier analysis
  cohort_1 <- dplyr::filter(agg_data, .data$COHORT_DEFINITION_ID != targetCohortId)
  cohort_2 <- dplyr::filter(agg_data, .data$COHORT_DEFINITION_ID == targetCohortId)

  # Count of patients in each cohort
  sample_1_n <- nrow(dplyr::filter(data_initial, .data$COHORT_DEFINITION_ID != targetCohortId))
  sample_2_n <- nrow(dplyr::filter(data_initial, .data$COHORT_DEFINITION_ID == targetCohortId))

  # Prepare a data frame to hold the results
  significant_concepts <- data.frame(CONCEPT_ID = integer(), ZTEST = logical(), ZTEST_P_VALUE = double(), ABSTRACTION_LEVEL = integer())
  # Setup cores
  cl <- parallel::makeCluster(numCores)
  doParallel::registerDoParallel(cl)
  abstraction_level = NULL
  # Parallelize the loop over abstraction levels
  significant_concepts <- foreach::foreach(abstraction_level = unique(agg_data$ABSTRACTION_LEVEL),
                                           .combine = rbind, .packages = c("dplyr"),.export = c("abstraction_level")) %dopar% {
    agg_data_abstraction_subset <- dplyr::filter(agg_data, .data$ABSTRACTION_LEVEL == abstraction_level)
    alpha <- 0.05 / length(unique(agg_data_abstraction_subset$CONCEPT_ID))

    level_results <- data.frame(CONCEPT_ID = integer(), ZTEST = logical(), ZTEST_P_VALUE = double(), ABSTRACTION_LEVEL = integer())

    # Perform statistical test for each CONCEPT_ID within the abstraction level
    for (concept_id in unique(agg_data_abstraction_subset$CONCEPT_ID)) {
      prevalence_cohort_1 <- dplyr::filter(cohort_1, .data$CONCEPT_ID == concept_id, .data$ABSTRACTION_LEVEL == abstraction_level)$TOTAL_PREVALENCE
      prevalence_cohort_2 <- dplyr::filter(cohort_2, .data$CONCEPT_ID == concept_id, .data$ABSTRACTION_LEVEL == abstraction_level)$TOTAL_PREVALENCE

      if ((length(prevalence_cohort_1) == 0 || length(prevalence_cohort_2) == 0) ||
          (prevalence_cohort_2 / sample_2_n < presenceFilter) ||
          (prevalence_cohort_2 > sample_2_n || prevalence_cohort_1 > sample_1_n) ||
          (prevalence_cohort_2 == prevalence_cohort_1)) {

        if (length(prevalence_cohort_2) == 0 || length(prevalence_cohort_1) == 0 ) {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              ZTEST = TRUE,
              ZTEST_P_VALUE = NA,
              ABSTRACTION_LEVEL = abstraction_level
            )
          )
        } else {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              ZTEST = FALSE,
              ZTEST_P_VALUE = 1,
              ABSTRACTION_LEVEL = abstraction_level
            )
          )
        }
      } else {
        test_result <- stats::prop.test(
          c(prevalence_cohort_1, prevalence_cohort_2),
          c(sample_1_n, sample_2_n),
          conf.level = 0.95
        )
        if (is.na(test_result$p.value)) {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              ZTEST = FALSE,
              ZTEST_P_VALUE = 1,
              ABSTRACTION_LEVEL = abstraction_level
            )
          )
        } else if (test_result$p.value < alpha) {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              ZTEST = TRUE,
              ZTEST_P_VALUE = test_result$p.value,
              ABSTRACTION_LEVEL = abstraction_level
            )
          )
        }
        else if (prevalence_cohort_1 == 0 | prevalence_cohort_2 == 0) {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              ZTEST = TRUE,
              ZTEST_P_VALUE = NA,
              ABSTRACTION_LEVEL = abstraction_level
            )
          )
        } else {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              ZTEST = FALSE,
              ZTEST_P_VALUE = test_result$p.value,
              ABSTRACTION_LEVEL = abstraction_level
            )
          )
        }
      }
    }
    return(level_results)
  }
  parallel::stopCluster(cl)
  return(significant_concepts)
}

#'  This function learns separate logistic regression models on the patient prevalence data target vs control
#'
#' @param data_patients Prevalence data for patients
#' @param data_initial Imported cohort dataframe
#' @param targetCohortId Target cohort id
#' @param presenceFilter Presence filter 0-1, if 0.1 then feature has to be present for at least 10 percent of patients
#' @param numCores Number of cores to allocate to parallel processing
#' @importFrom foreach %dopar%
#' @keywords internal

performPrevalenceAnalysisLogistic <- function(data_patients,
                                              data_initial,
                                              targetCohortId,
                                              presenceFilter,
                                              numCores = parallel::detectCores() - 1) {
  printCustomMessage("Running Logit-test ...")
  # Aggregate the prevalence data for each concept within each cohort
  agg_data <- data_patients %>%
    dplyr::mutate(PREVALENCE = dplyr::if_else(.data$PREVALENCE > 0, 1, 0)) %>%
    dplyr::group_by(.data$ABSTRACTION_LEVEL, .data$COHORT_DEFINITION_ID, .data$CONCEPT_ID) %>%
    dplyr::summarise(TOTAL_PREVALENCE = sum(.data$PREVALENCE),
                     .groups = 'drop')

  # # Separate the data for each cohort for easier analysis
  # # TODO: is it even used?
  cohort_1 <- dplyr::filter(agg_data, .data$COHORT_DEFINITION_ID != targetCohortId)
  cohort_2 <- dplyr::filter(agg_data, .data$COHORT_DEFINITION_ID == targetCohortId)

  # Count of patients in each cohort
  sample_1_n <- sum(data_initial$COHORT_DEFINITION_ID != targetCohortId)
  sample_2_n <- sum(data_initial$COHORT_DEFINITION_ID == targetCohortId)

  # Setup cores
  cl <- parallel::makeCluster(numCores)
  doParallel::registerDoParallel(cl)
  abstraction_level = NULL
  # Parallelize the loop over abstraction levels
  significant_concepts <- foreach::foreach(abstraction_level = unique(agg_data$ABSTRACTION_LEVEL), .combine = rbind, .packages = c("dplyr", "stats"), .export = c("abstraction_level")) %dopar% {

    agg_data_abstraction_subset <- dplyr::filter(agg_data, .data$ABSTRACTION_LEVEL == abstraction_level)
    alpha <- 0.05 / length(unique(agg_data_abstraction_subset$CONCEPT_ID))

    level_results <- data.frame(CONCEPT_ID = integer(), LOGITTEST = logical(), LOGITTEST_P_VALUE = double(), ABSTRACTION_LEVEL = integer())

    # Perform logistic regression for each CONCEPT_ID within the abstraction level
    for (concept_id in unique(agg_data_abstraction_subset$CONCEPT_ID)) {
      # Create the dataset for logistic regression
      concept_data <- data_patients %>%
        dplyr::filter(.data$ABSTRACTION_LEVEL == abstraction_level, .data$CONCEPT_ID == concept_id) %>%
        dplyr::group_by(.data$PERSON_ID, .data$COHORT_DEFINITION_ID) %>%
        dplyr::mutate(
          PREVALENCE = 1,
          TARGET = dplyr::if_else(.data$COHORT_DEFINITION_ID == targetCohortId, 1, 0),
          CONTROL = dplyr::if_else(.data$COHORT_DEFINITION_ID == targetCohortId, 0, 1)
        ) %>% dplyr::ungroup() %>% dplyr::select(.data$COHORT_DEFINITION_ID, .data$PREVALENCE, .data$TARGET, .data$CONTROL)


      no_match_target = data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID == targetCohortId) %>% nrow() - concept_data %>% dplyr::filter(.data$COHORT_DEFINITION_ID == targetCohortId) %>% nrow()
      no_match_control = data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID != targetCohortId) %>% nrow() - concept_data %>% dplyr::filter(.data$COHORT_DEFINITION_ID != targetCohortId) %>% nrow()


      no_match_target_df = NULL
      no_match_control_df = NULL
      if(no_match_target != 0){

      # print(concept_id)
      # print(no_match_target)
      no_match_target_df = data.frame(COHORT_DEFINITION_ID = "target", PREVALENCE = 0, TARGET = rep(1, times = no_match_target), CONTROL = 0)
      }
      if(no_match_control != 0){
      # print(no_match_control)
      no_match_control_df = data.frame(COHORT_DEFINITION_ID = "control", PREVALENCE = 0, TARGET = 0, CONTROL = rep(1, times = no_match_control))
      }
      concept_data = rbind(rbind(no_match_target_df, no_match_control_df), concept_data)

      prevalence_cohort_2 <- ifelse(is.na(sum(concept_data$PREVALENCE[concept_data$TARGET == 1])), 0, sum(concept_data$PREVALENCE[concept_data$TARGET == 1]))
      prevalence_cohort_1 <- ifelse(is.na(sum(concept_data$PREVALENCE[concept_data$CONTROL == 1])), 0, sum(concept_data$PREVALENCE[concept_data$CONTROL == 1]))

      if (prevalence_cohort_2 / sample_2_n < presenceFilter | prevalence_cohort_1 == 0 | prevalence_cohort_2 == 0) {
        level_results <- rbind(
          level_results,
          data.frame(
            CONCEPT_ID = concept_id,
            LOGITTEST = FALSE,
            LOGITTEST_P_VALUE = 1,
            ABSTRACTION_LEVEL = abstraction_level
          )
        )
        next
      }

      # Perform logistic regression
      model <- stats::glm(TARGET ~ PREVALENCE, data = concept_data, family = stats::binomial)
      summary_model <- summary(model)
      # Check if the presence of the concept is significant
      p_value <- tryCatch({
        # Attempt to access the p-value
        p_value <- summary_model$coefficients[2, 4]
        p_value  # Return the value if successful
      }, error = function(e) {
        # Return NA or another indicator if there is an error
        NA
      })

      if (!is.na(p_value) && p_value < alpha) {
       # print("Times 1")
        level_results <- rbind(
          level_results,
          data.frame(
            CONCEPT_ID = concept_id,
            LOGITTEST = TRUE,
            LOGITTEST_P_VALUE = p_value,
            ABSTRACTION_LEVEL = abstraction_level
          )
        )
      }
      else if (prevalence_cohort_1 == 0 | prevalence_cohort_2 == 0) {
       # print("Times 2")
        level_results <- rbind(
          level_results,
          data.frame(
            CONCEPT_ID = concept_id,
            LOGITTEST = TRUE,
            LOGITTEST_P_VALUE = NA,
            ABSTRACTION_LEVEL = abstraction_level
          )
        )
      }
      else {
      #  print("Times 3")
        level_results <- rbind(
          level_results,
          data.frame(
            CONCEPT_ID = concept_id,
            LOGITTEST = FALSE,
            LOGITTEST_P_VALUE = p_value,
            ABSTRACTION_LEVEL = abstraction_level
          )
        )
      }
    }
    return(level_results)
  }
  parallel::stopCluster(cl)
  return(significant_concepts)
}


#'  This function performs kolmogorov-smirnov tests on the patient concept occurrence date data target vs uniform distribution
#'
#' @param data_features Summarised data of features
#' @param targetCohortId Target cohort id
#' @param numCores Number of cores to allocate to parallel processing
#' @importFrom foreach %dopar%
#'
#' @keywords internal
performKSAnalysis <- function(data_features,
                                      targetCohortId,
                                      numCores = parallel::detectCores() - 1) {
  printCustomMessage("Running KS-tests ...")
  # Aggregate the ocurrence date data for each concept within each cohort
  agg_data <- data_features %>% dplyr::select(.data$CONCEPT_ID, .data$ABSTRACTION_LEVEL, .data$TIME_TO_EVENT)


  # Prepare a data frame to hold the results
  significant_concepts <- data.frame(CONCEPT_ID = integer(), KSTEST = logical(), KSTEST_P_VALUE = double(), ABSTRACTION_LEVEL = integer())
  # Setup cores
  cl <- parallel::makeCluster(numCores)
  doParallel::registerDoParallel(cl)
  abstraction_level = NULL

  # Parallelize the loop over abstraction levels
  significant_concepts <- foreach::foreach(abstraction_level = unique(agg_data$ABSTRACTION_LEVEL), .combine = rbind, .packages = c("dplyr"),.export = c("abstraction_level")) %dopar% {
    agg_data_abstraction_subset <- dplyr::filter(agg_data, .data$ABSTRACTION_LEVEL == abstraction_level)
    alpha <- 0.05 / length(unique(agg_data_abstraction_subset$CONCEPT_ID))

    level_results <- data.frame(CONCEPT_ID = integer(), KSTEST = logical(), KSTEST_P_VALUE = double(), ABSTRACTION_LEVEL = integer())


    # Perform statistical test for each CONCEPT_ID within the abstraction level
    for (concept_id in unique(agg_data_abstraction_subset$CONCEPT_ID)) {

      concept_date_array =  unlist(dplyr::filter(agg_data_abstraction_subset, .data$CONCEPT_ID == concept_id, .data$ABSTRACTION_LEVEL == abstraction_level)$TIME_TO_EVENT)
      #concept_date_array_min = min(concept_date_array)

      if (is.null(concept_date_array)) {

        level_results <- rbind(
          level_results,
          data.frame(
            CONCEPT_ID = concept_id,
            KSTEST = FALSE,
            KSTEST_P_VALUE = 1,
            ABSTRACTION_LEVEL = abstraction_level
          )
        )
      } else {
        concept_date_array_min = min(concept_date_array) # 0
        concept_date_array_max = max(concept_date_array)
        ks_result <- stats::ks.test(jitter(concept_date_array, amount=0.1), "punif", min=concept_date_array_min, max=concept_date_array_max)
                level_results <- rbind(
          level_results,
          data.frame(
            CONCEPT_ID = concept_id,
            KSTEST = ks_result$p.value < alpha,
            KSTEST_P_VALUE = ks_result$p.value,
            ABSTRACTION_LEVEL = abstraction_level
          )
        )
      }
    }
    return(level_results)
  }
  parallel::stopCluster(cl)
  return(significant_concepts)
}


#' This filters data based on prevalence difference ratio and return the nHighestPrevalenceDifference greates differences
#' @param data Data list object
#' @param nHighestPrevalenceDifference Number of features with highest prevalence difference ratio to keep
#' @importFrom dplyr %>%
#'
#' @keywords internal

calculate_data_features <-
  function(data, nHighestPrevalenceDifference) {
    # Calculate number of patients per cohort
    n_patients <- data$data_initial %>%
      dplyr::group_by(.data$COHORT_DEFINITION_ID) %>%
      dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
      tidyr::pivot_wider(
        names_from = .data$COHORT_DEFINITION_ID,
        values_from = .data$count,
        values_fill = list(count = 0)
      )

    # Resolve possible conflicts in concept names
    data = resolveConceptNameOverlap(data)

    count_target <- n_patients$`2`
    count_control <- n_patients$`1`
    # Update data features with prevalence calculations
    data_features <- data$data_patients %>%
      dplyr::group_by(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$ABSTRACTION_LEVEL, .data$HERITAGE) %>%
      dplyr::summarise(
        TARGET_SUBJECT_COUNT = sum(.data$COHORT_DEFINITION_ID == 2 &
                                     .data$PREVALENCE > 0),
        CONTROL_SUBJECT_COUNT = sum(.data$COHORT_DEFINITION_ID == 1 &
                                      .data$PREVALENCE > 0),
        TIME_TO_EVENT = list(unlist(.data$TIME_TO_EVENT[.data$COHORT_DEFINITION_ID == 2])),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(
        TARGET_SUBJECT_PREVALENCE = .data$TARGET_SUBJECT_COUNT / count_target,
        CONTROL_SUBJECT_PREVALENCE = .data$CONTROL_SUBJECT_COUNT / count_control,
        PREVALENCE_DIFFERENCE_RATIO = dplyr::case_when(
          is.na(.data$TARGET_SUBJECT_PREVALENCE) |
            .data$TARGET_SUBJECT_PREVALENCE == 0 ~ 0,
          (
            is.na(.data$CONTROL_SUBJECT_PREVALENCE) |
              .data$CONTROL_SUBJECT_PREVALENCE == 0
          ) & is.na(.data$TARGET_SUBJECT_PREVALENCE) ~ -1,
          is.na(.data$CONTROL_SUBJECT_PREVALENCE) |
            .data$CONTROL_SUBJECT_PREVALENCE == 0 ~ 100,
          TRUE ~ .data$TARGET_SUBJECT_PREVALENCE / .data$CONTROL_SUBJECT_PREVALENCE
        )
      )

    if (nHighestPrevalenceDifference != FALSE) {
      # Keep only the nHighestPrevalenceDifference rows with highest values of PREVALENCE_DIFFERENCE_RATIO
      data_features <- data_features %>%
        dplyr::group_by(.data$ABSTRACTION_LEVEL) %>%
        dplyr::arrange(dplyr::desc(.data$PREVALENCE_DIFFERENCE_RATIO)) %>%
        dplyr::slice_head(n = nHighestPrevalenceDifference) %>%
        dplyr::ungroup()
    }
    data$data_features <- data_features

    filtered_keys <- data_features %>%
      dplyr::select(.data$ABSTRACTION_LEVEL, .data$CONCEPT_ID, .data$HERITAGE) %>%
      dplyr::distinct()

    data$data_patients <- data$data_patients %>%
      dplyr::inner_join(filtered_keys, by = c("ABSTRACTION_LEVEL", "CONCEPT_ID", "HERITAGE"))

    return(data)
  }

#' This function uses complementaryMappingTable to map concepts to custom names
#' @param data Data list object
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present, columns CONCEPT_ID, CONCEPT_NAME, NEW_CONCEPT_ID, NEW_CONCEPT_NAME, ABSTRACTION_LEVEL,
#' @param abstractionLevel Level of abstraction, by default -1 (imported data level)
#'
#' @keywords internal

handleMapping <- function(data, complementaryMappingTable, abstractionLevel = -1) {
  printCustomMessage("Mapping according to predefined complementaryMappingTable...")
  if(nrow(complementaryMappingTable %>% dplyr::filter(.data$ABSTRACTION_LEVEL == abstractionLevel)) == 0){
    # Return if mapping table is empty
    return(data$data_patients[0,])
  }
  # Remove data not affiliated with mappings
  data_patients <- data$data_patients %>% dplyr::filter(.data$ABSTRACTION_LEVEL == abstractionLevel)

  # If new abstraction level get default abstraction level data
  if(nrow(data_patients) == 0){
    data_patients <- data$data_patients %>% dplyr::filter(.data$ABSTRACTION_LEVEL == -1) %>% dplyr::mutate(ABSTRACTION_LEVEL = abstractionLevel)
  }
  data_patients = data_patients %>% dplyr::filter(.data$CONCEPT_ID %in% complementaryMappingTable$CONCEPT_ID)
  # Step 2: Replace CONCEPT_ID in data_patients with the mapped CONCEPT_ID for each CONCEPT_NAME
  data_patients <- data_patients %>%
    dplyr::left_join(complementaryMappingTable,
                     by = "CONCEPT_ID", relationship = "many-to-many") %>%
    dplyr:: mutate(CONCEPT_ID = dplyr::if_else(is.na(.data$NEW_CONCEPT_ID), .data$CONCEPT_ID, .data$NEW_CONCEPT_ID)) %>%
    dplyr:: mutate(CONCEPT_NAME = dplyr::if_else(is.na(.data$NEW_CONCEPT_NAME), .data$CONCEPT_NAME.x, .data$NEW_CONCEPT_NAME)) %>%
  dplyr::select(COHORT_DEFINITION_ID = .data$COHORT_DEFINITION_ID,
                PERSON_ID = .data$PERSON_ID,
                CONCEPT_ID = .data$CONCEPT_ID,
                CONCEPT_NAME = .data$CONCEPT_NAME,
                PREVALENCE = .data$PREVALENCE,
                HERITAGE = .data$HERITAGE,
                TIME_TO_EVENT = .data$TIME_TO_EVENT,
                ABSTRACTION_LEVEL= .data$ABSTRACTION_LEVEL.x)

  # Step 3: Summarize data_patients to aggregate PREVALENCE
  final_data <- data_patients %>%
    dplyr::group_by(.data$COHORT_DEFINITION_ID,
                    .data$PERSON_ID,
                    .data$CONCEPT_ID,
                    .data$CONCEPT_NAME,
                    .data$HERITAGE) %>%
    dplyr::summarise(PREVALENCE = sum(.data$PREVALENCE, na.rm = TRUE),
                     TIME_TO_EVENT = list(unlist(.data$TIME_TO_EVENT)),
                     .groups = 'drop')
  final_data_summarized <- final_data %>%
    dplyr::group_by(.data$CONCEPT_ID, .data$HERITAGE) %>%
    dplyr::summarise(Summed_Prevalence = sum(.data$PREVALENCE, na.rm = TRUE),
                     .groups = 'drop')
  # Assign the most prevalent heritage to each concept ID and PERSON_ID
  most_prevalent_heritage <- final_data_summarized %>%
    dplyr::group_by(.data$CONCEPT_ID) %>%
    dplyr::filter(.data$Summed_Prevalence == max(.data$Summed_Prevalence)) %>%
    dplyr::slice(1) %>%  # In case of tie, take the first occurrence
    dplyr::select(.data$CONCEPT_ID, .data$HERITAGE)

  # Merge the most prevalent heritage back to the original dataframe and sum the PREVALENCE
  result <- final_data %>%
    dplyr::select(-.data$HERITAGE) %>%
    dplyr::left_join(most_prevalent_heritage, by = "CONCEPT_ID") %>%
    dplyr::group_by(.data$COHORT_DEFINITION_ID,
                    .data$PERSON_ID,
                    .data$CONCEPT_ID,
                    .data$CONCEPT_NAME,
                    .data$HERITAGE) %>%
    dplyr::summarise(PREVALENCE = sum(.data$PREVALENCE, na.rm = TRUE),
                     TIME_TO_EVENT = list(unlist(.data$TIME_TO_EVENT)),
                     .groups = 'drop') %>% as.data.frame()
  # Add abstraction level identifier
  result$ABSTRACTION_LEVEL = abstractionLevel
  result$PERSON_ID = as.integer(result$PERSON_ID)
  return(result)
}

#' Function for extracting n features
#' @param data Data list object
#' @param topK numeric > if set, keeps this number of features in the analysis. Maximum number of features exported.
#'
#' @keywords internal

createDataFeatures <- function(data, topK) {
  printCustomMessage("Get top n features ...")
  data = calculate_data_features(data, topK)
  return(data)
}

#' Function for handling tests
#' @param data Data list object
#' @param targetCohortId Target cohort id
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param runZTests boolean for Z-tests
#' @param runLogitTests boolean for logit-tests
#' @param runKSTests boolean for KS-tests
#'
#' @keywords internal

handleTests <-
  function(data,
           targetCohortId,
           presenceFilter,
           runZTests = TRUE,
           runLogitTests = TRUE,
           runKSTests = TRUE,
           numCores = parallel::detectCores() - 1) {
    data_features = data$data_features
    data_patients = data$data_patients
    data_initial = data$data_initial

    if (runZTests) {
      significant_concepts <-
        performPrevalenceAnalysis(data_patients,
                                  data_initial,
                                  targetCohortId,
                                  presenceFilter,
                                  numCores = numCores)
        data_features <- data_features %>%
        dplyr::left_join(significant_concepts,by = c("ABSTRACTION_LEVEL", "CONCEPT_ID"))
      printCustomMessage("Z-test on data executed!")
    }
    else {
      data_features <- data_features %>%
        dplyr::mutate(ZTEST = FALSE, ZTEST_P_VALUE = 1)
      printCustomMessage("Z-tests were disabled!")
    }
    # Logit test
    if (runLogitTests) {
      significant_concepts <-
        performPrevalenceAnalysisLogistic(data_patients,
                                          data_initial,
                                          targetCohortId,
                                          presenceFilter,
                                          numCores = numCores)
        data_features <- data_features %>%
        dplyr::left_join(significant_concepts, by = c("ABSTRACTION_LEVEL", "CONCEPT_ID"))
      printCustomMessage("Logit-test on data executed!")
    }  else {
      data_features <- data_features %>%
        dplyr::mutate(LOGITTEST = FALSE, LOGITTEST_P_VALUE = 1)
      printCustomMessage("Logit-tests were disabled!")
    }

    # KS test
    if (runKSTests) {
      significant_concepts <-
        performKSAnalysis(data_features,
                          numCores = numCores)
        data_features <- data_features %>%
        dplyr::left_join(significant_concepts, by = c("ABSTRACTION_LEVEL", "CONCEPT_ID"))
      printCustomMessage("Kolmogorov-Smirnov tests on data executed!")
    }  else {
      data_features <- data_features %>%
        dplyr::mutate(KSTEST = FALSE, KSTEST_P_VALUE = 1)
      printCustomMessage("Kolmogorov-Smirnov tests were disabled!")
    }

    # Overwrite data_features to apply tests
    data$data_features = data_features
    return(data)
  }

#' Function for handling feature selection
#' @param data Data list object
#' @param pathToResults Path to the results folder, can be project's working directory
#' @param targetCohortId Target cohort id
#' @param topK numeric > if set, keeps this number of features in the analysis. Maximum number of features exported.
#' @param prevalenceCutOff numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param runZTests boolean for Z-tests
#' @param runLogitTests boolean for logit-tests
#' @param createOutputFiles Boolean for creating output files, the default value is TRUE
#'
#' @keywords internal

handleFeatureSelection <-
  function(data,
           pathToResults,
           topK,
           prevalenceCutOff,
           targetCohortId,
           runZTests,
           runLogitTests,
           createOutputFiles = FALSE
           ) {
    data_features = NULL
    if (!(runZTests | runLogitTests)) {
    data_features = data$data_features
    }
    else{
    data_features = data$data_features %>% dplyr::filter(.data$ZTEST | .data$LOGITTEST)
    }
    n_features_left = nrow(data_features)
    trajectoryDataList = list()

    # Now, significant_concepts_data contains only the concepts significantly over-represented in target cohort
    if (n_features_left == 0) {
      printCustomMessage("Running analysis END!")
      printCustomMessage("No features left. Perhaps use more lenient filters!")
      ################################################################################
      #
      # Save
      #
      ################################################################################

      if (createOutputFiles){
        createPathToResults(pathToResults = pathToResults)
        saveResult(data, pathToResults)
      }
      warning("No features left. Perhaps use more lenient filters! Exiting ...")
      return(data)
    }


    if (topK == FALSE) {
      features = dplyr::filter(data_features,
                               .data$PREVALENCE_DIFFERENCE_RATIO > prevalenceCutOff)
      n_features_left = nrow(features)
      printCustomMessage(
        paste(
          "After filtering for prevalence cutoff of ",
          prevalenceCutOff,
          ", there are ",
          n_features_left,
          " features left!",
          sep = ""
        )
      )
    }
    else {
      topNFeatures <-
        dplyr::pull(dplyr::slice(dplyr::arrange(
          data_features, dplyr::desc(.data$PREVALENCE_DIFFERENCE_RATIO)
        ), 1:topK), .data$CONCEPT_ID)
      features = dplyr::filter(data_features, .data$CONCEPT_ID %in% topNFeatures, .data$ABSTRACTION_LEVEL == -1)
      n_features_left = nrow(features)
      printCustomMessage(
        paste(
          "After filtering for top ",
          topK,
          " features, there are ",
          n_features_left,
          " features left!",
          sep = ""
        )
      )
    }

    patients = dplyr::select(
      dplyr::filter(
        dplyr::filter(data$data_patients, .data$COHORT_DEFINITION_ID == targetCohortId, .data$ABSTRACTION_LEVEL == -1),
        .data$CONCEPT_ID %in% features$CONCEPT_ID
      ),
      .data$PERSON_ID,
      .data$CONCEPT_NAME,
      .data$PREVALENCE
    )

    patients <-
      dplyr::summarise(dplyr::group_by(patients, .data$PERSON_ID, .data$CONCEPT_NAME),
                       PREVALENCE = mean(.data$PREVALENCE))

    transformed_data <-
      tidyr::spread(patients, .data$CONCEPT_NAME, .data$PREVALENCE, fill = 0)
    patients_data <-
      transformed_data[,-1] # Remove the first column (patient IDs)
    # export selected features
    trajectoryDataList$selectedFeatureNames = colnames(patients_data)
    trajectoryDataList$selectedFeatureIds = features$CONCEPT_ID
    trajectoryDataList$selectedFeatures = features
    data$trajectoryDataList = trajectoryDataList
    return(data)
  }


#' This function saves the resulting data object
#' @param data Data list object
#' @param pathToResults Path to the results folder, can be project's working directory
#' @param complName Optional, name of the rds file to be saved
#'
#' @keywords internal

saveResult <- function(data, pathToResults) {
  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  # Create the full file path with the time-stamp included in the filename
  filePath <- file.path(pathToResults, paste0("CohortContrast_", timestamp, ".rds"))
  if(!is.null(data$config$complName)){
    filePath <- file.path(pathToResults, paste0(data$config$complName, ".rds"))
  }
  else{data$config$complName = paste0("CohortContrast_", timestamp)}
  save_object_metadata(data, path = filePath)

  save_object(data, path = filePath)
  if (isFALSE(data$config$patientLevelData)) {
    # Remove patient level data
    data$data_patients = NULL
    data$data_initial = NULL
    data$data_person = NULL
  }
  save_object(data, path = filePath)
  printCustomMessage(paste("Saved the result to ", filePath, sep = ""))
}

#' This function resolves concept name/label overlaps
#' @param data Data list object
#' @keywords internal
resolveConceptNameOverlap <- function(data) {
  # Get the data from the input
  df <- data$data_patients
  conflict_check <- df %>%
    dplyr::group_by(.data$CONCEPT_NAME, .data$ABSTRACTION_LEVEL) %>%
    dplyr::summarise(count = dplyr::n_distinct(.data$CONCEPT_ID), .groups = "drop") %>%
    dplyr::filter(.data$count > 1)

  if (nrow(conflict_check) > 0) {
    # If conflicts exist, resolve by renaming concept names
    df <- df %>%
      dplyr::group_by(.data$CONCEPT_NAME,.data$ABSTRACTION_LEVEL) %>%
      dplyr::mutate(
        ordinal = dplyr::dense_rank(.data$CONCEPT_ID), # Assign a unique ordinal for each CONCEPT_ID
        CONCEPT_NAME = dplyr::if_else( ordinal != 1, paste0(.data$CONCEPT_NAME, " (", ordinal, ")"), .data$CONCEPT_NAME)
      ) %>%
      dplyr::ungroup()

    cmt <- df %>% dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$ordinal, .data$ABSTRACTION_LEVEL) %>%
      dplyr::distinct() %>%
      dplyr::filter(ordinal > 1) %>%
      dplyr::mutate(CONCEPT_ID = .data$CONCEPT_ID,
                    NEW_CONCEPT_NAME = .data$CONCEPT_NAME,
                    NEW_CONCEPT_ID = .data$CONCEPT_ID,
                    CONCEPT_NAME = gsub("\\s*\\([^()]*\\)$", "", .data$CONCEPT_NAME),
                    ABSTRACTION_LEVEL = .data$ABSTRACTION_LEVEL) %>%
      dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$NEW_CONCEPT_ID, .data$NEW_CONCEPT_NAME, .data$ABSTRACTION_LEVEL)

    for (i in 1:nrow(cmt)) {
      row = cmt[i,]
      cli::cli_alert_warning(paste0("Mapped '", gsub("\\{", "{{", gsub("\\}", "}}", row$CONCEPT_NAME)), "' to '",  gsub("\\{", "{{", gsub("\\}", "}}", row$NEW_CONCEPT_NAME)), "' because of duplicate concept names for differing ids!"))
    }

    data$complementaryMappingTable = rbind(cmt,  data$complementaryMappingTable)
    df <- df %>%  dplyr::select(-ordinal)
  }

  # Update the data list
  data$data_patients <- df

  # We have also seen an occurrence where same id and name among different heritage
  conflict_check2 <- df %>% #dplyr::mutate(TMP_ID = paste0(CONCEPT_NAME, CONCEPT_ID)) %>%
    dplyr::group_by(.data$CONCEPT_NAME,.data$ABSTRACTION_LEVEL, .data$CONCEPT_ID) %>%
    dplyr::summarise(count = dplyr::n_distinct(.data$HERITAGE), .groups = "drop") %>%
    dplyr::filter(.data$count > 1)

  if (nrow(conflict_check2) > 0) {
    # If conflicts exist, resolve by renaming concept names
    df <- df %>%
      dplyr::group_by(.data$CONCEPT_NAME,.data$ABSTRACTION_LEVEL, .data$CONCEPT_ID) %>%
      dplyr::mutate(
        ordinal = dplyr::dense_rank(.data$HERITAGE), # Assign a unique ordinal for each CONCEPT_ID
        CONCEPT_ID = dplyr::if_else( ordinal != 1, .data$CONCEPT_ID + 1000000000*ordinal, .data$CONCEPT_ID),
                                     CONCEPT_NAME = dplyr::if_else( .data$ordinal != 1, paste0(.data$CONCEPT_NAME, " (", .data$HERITAGE, ")"), .data$CONCEPT_NAME)) %>%
      dplyr::ungroup()

    cmt <- df %>% dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$ordinal, .data$ABSTRACTION_LEVEL) %>%
      dplyr::distinct() %>%
      dplyr::filter(.data$ordinal > 1) %>%
      dplyr::mutate(                    NEW_CONCEPT_ID = .data$CONCEPT_ID,
        CONCEPT_ID = CONCEPT_ID - 1000000000*.data$ordinal,
                    NEW_CONCEPT_NAME = .data$CONCEPT_NAME,
                    CONCEPT_NAME = gsub("\\s*\\([^()]*\\)$", "", .data$CONCEPT_NAME),
                    ABSTRACTION_LEVEL = .data$ABSTRACTION_LEVEL) %>%
      dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$NEW_CONCEPT_ID, .data$NEW_CONCEPT_NAME, .data$ABSTRACTION_LEVEL)

    for (i in 1:nrow(cmt)) {
      row = cmt[i,]
      cli::cli_alert_warning(paste0("Mapped '", gsub("\\{", "{{", gsub("\\}", "}}", row$CONCEPT_NAME)), "' to '", gsub("\\{", "{{", gsub("\\}", "}}", row$NEW_CONCEPT_NAME)), "' because of concept non-uniqueness over domains!"))
    }

    data$complementaryMappingTable = rbind(cmt,  data$complementaryMappingTable)

    df <- df %>%  dplyr::select(-ordinal)
    }
  data$data_patients <- df
  return(data)
}
