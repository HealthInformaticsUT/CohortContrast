#' @title CohortContrast analysis function
#' After running this function on your CDM instance successfully you can either use the extracted outputs or run the CohortContrast shiny app.
#' @param cdm Connection to database
#' @param pathToResults Path to the results folder, can be project's working directory
#' @param domainsIncluded list of CDM domains to include
#' @param prevalenceCutOff numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param topK numeric > if set, keeps this number of features in the analysis. Maximum number of features exported.
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present
#' @param createC2TInput Boolean for creating Cohort2Trajectory input dataframe
#' @param runZTests boolean for Z-tests
#' @param runLogitTests boolean for logit-tests
#' @param createOutputFiles Boolean for creating output files, the default value is TRUE
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#' # Example of setting up CohortContrast tables with a dummy database
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
#' cdm <- createCohortContrastCdm(
#'   cdm = cdm,
#'   targetTable = targetTable,
#'   controlTable = controlTable
#' )
#'
#' pathToResults = getwd()
#'
#' data = CohortContrast(
#'   cdm,
#'   pathToResults,
#'   domainsIncluded = c(
#'     "Drug"
#'   ),
#'   prevalenceCutOff = 0.1,
#'   topK = FALSE,
#'   presenceFilter = 0.005,
#'   complementaryMappingTable = FALSE,
#'   createC2TInput = FALSE,
#'   runZTests = FALSE,
#'   runLogitTests = FALSE,
#'   createOutputFiles = FALSE
#' )
#'
#' DBI::dbDisconnect(con)
CohortContrast <- function(cdm,
                           pathToResults,
                           domainsIncluded = c(
                             "Drug",
                             "Condition",
                             "Measurement",
                             "Observation",
                             "Procedure",
                             "Visit",
                             "Visit detail"
                           ),
                           prevalenceCutOff = 10,
                           topK = FALSE,
                           presenceFilter = 0.005,
                           complementaryMappingTable = FALSE,
                           createC2TInput = FALSE,
                           runZTests = TRUE,
                           runLogitTests = TRUE,
                           createOutputFiles = TRUE) {

  targetCohortId = 2

  data = generateTables(
    cdm = cdm,
    domainsIncluded = domainsIncluded,
  )

  if (is.data.frame(complementaryMappingTable)) {
    data = handleMapping(data, complementaryMappingTable)
  }

  data = createDataFeatures(data, topK)
  data = handleTests(data,
                     targetCohortId,
                     presenceFilter,
                     runZTests,
                     runLogitTests)

  data = handleFeatureSelection(data = data, pathToResults = pathToResults, topK = topK, prevalenceCutOff = prevalenceCutOff, targetCohortId = targetCohortId, runZTests = runZTests, runLogitTests = runLogitTests,createOutputFiles = createOutputFiles)

  # Maybe create separate. function for this
  data = createC2TInput(data,
                        targetCohortId,
                        createC2TInput,
                        complementaryMappingTable)

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
           complementaryMappingTable = FALSE) {
    results_list <- lapply(names(split_data), function(heritage) {
      printCustomMessage(paste("Querying eligible ", heritage, " data ...", sep = ""))
      unique_person_ids <- unique(split_data[[heritage]]$PERSON_ID)
      # Filter for tests
      unique_concept_ids <-
        unique(split_data[[heritage]]$CONCEPT_ID)


      query_result <- NULL
      if (heritage == 'condition_occurrence') {
        query_result <-
          cdm$condition_occurrence %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                         by = c("person_id" = "subject_id")) %>% dplyr::filter(condition_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'drug_exposure') {
        query_result <-
          cdm$drug_exposure %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                  by = c("person_id" = "subject_id")) %>% dplyr::filter(drug_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'measurement') {
        query_result <-
          cdm$measurement %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                by = c("person_id" = "subject_id")) %>% dplyr::filter(measurement_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'procedure_occurrence') {
        query_result <-
          cdm$procedure_occurrence %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                         by = c("person_id" = "subject_id")) %>% dplyr::filter(procedure_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'observation') {
        query_result <-
          cdm$observation %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                by = c("person_id" = "subject_id")) %>% dplyr::filter(observation_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'visit_occurrence') {
        query_result <-
          cdm$visit_occurrence %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                     by = c("person_id" = "subject_id")) %>% dplyr::filter(visit_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      else if (heritage == 'visit_detail') {
        query_result <-
          cdm$visit_detail %>% dplyr::inner_join(cdm$cohortcontrast_cohorts,
                                                 by = c("person_id" = "subject_id")) %>% dplyr::filter(visit_detail_concept_id %in% unique_concept_ids) %>% as.data.frame()
      }
      # Further processing...
      colnames(query_result) = toupper(colnames(query_result))
      concept_string = toupper(sub("_occurrence_", "_", sub(
        "_exposure_", "_", paste(heritage, 'concept_id', sep = "_")
      )))
      # Ensure concept_map is created correctly
      concept_map <-
        unique(dataPatient[c("CONCEPT_ID", "CONCEPT_NAME")])

      if (is.data.frame(complementaryMappingTable)) {
        # Join the dataframes on CONCEPT_ID
        concept_map <-
          dplyr::select(
            dplyr::mutate(
              dplyr::left_join(
                concept_map,
                complementaryMappingTable,
                by = "CONCEPT_ID",
                suffix = c("", ".compl")
              ),
              # Choose the complementaryMappingTable CONCEPT_NAME if it's not NA; otherwise, use the original
              CONCEPT_NAME = dplyr::if_else(
                is.na(CONCEPT_NAME.compl),
                CONCEPT_NAME,
                CONCEPT_NAME.compl
              )
            ),
            # Select and rename the columns to match the original concept_map structure
            CONCEPT_ID,
            CONCEPT_NAME
          )

        # Ensuring that the updated_concept_map contains unique CONCEPT_ID entries
      }
      concept_map[[concept_string]] <-
        as.character(concept_map[['CONCEPT_ID']]) # Ensure consistent data type
      colnames(concept_map) <- c(concept_string, "CONCEPT_NAME")
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
      if (length(columns_ending_with_datetime) > 0) {
        first_column_ending_with_datetime <- columns_ending_with_datetime[1]
      } else {
        first_column_ending_with_datetime <-
          NULL # or any other appropriate action
      }
      query_result_mapped$START_DATE = query_result_mapped[[first_column_ending_with_datetime]]
      query_result_mapped$CONCEPT_ID = query_result_mapped[["CONCEPT_NAME"]]
      query_result_mapped = dplyr::select(query_result_mapped, CONCEPT_ID, PERSON_ID, START_DATE)
      query_result_mapped = dplyr::filter(query_result_mapped, PERSON_ID %in% as.integer(unique_person_ids))
      printCustomMessage(paste("Quering eligible ", heritage, " data finished.", sep = ""))
      return(query_result_mapped) # Assuming `query_result_mapped` is the final processed result
    })

    return(results_list)
  }

#' @title This function performs Z-tests on the patient prevalence data target vs control
#'
#' @param data_patients Prevalence data for patients
#' @param data_initial Imported cohort dataframe
#' @param targetCohortId Target cohort id
#' @param presenceFilter Presence filter 0-1, if 0.1 then feature has to be present for at least 10 percent of patients
#'
#' @keywords internal

performPrevalenceAnalysis <- function(data_patients,
           data_initial,
           targetCohortId,
           presenceFilter) {
    # Aggregate the prevalence data for each concept within each cohort
    agg_data <- dplyr::summarise(
      dplyr::group_by(
        dplyr::mutate(data_patients, PREVALENCE = dplyr::if_else(PREVALENCE > 0, 1, 0)),
        COHORT_DEFINITION_ID,
        CONCEPT_ID
      ),
      TOTAL_PREVALENCE = sum(PREVALENCE),
      .groups = 'drop'
    )

    # Separate the data for each cohort for easier analysis
    cohort_1 <-
      dplyr::filter(agg_data, COHORT_DEFINITION_ID != targetCohortId)
    cohort_2 <-
      dplyr::filter(agg_data, COHORT_DEFINITION_ID == targetCohortId)

    # Count of patients in each cohort
    sample_1_n <-
      nrow(dplyr::filter(data_initial, COHORT_DEFINITION_ID != targetCohortId))
    sample_2_n <-
      nrow(dplyr::filter(data_initial, COHORT_DEFINITION_ID == targetCohortId))

    # Prepare a data frame to hold the results
    significant_concepts <-
      data.frame(CONCEPT_ID = integer(), P_VALUE = double())

    # We implement Bonferroni correction
    printCustomMessage("Starting running Z-tests on data...")
    alpha <- 0.05 / length(unique(agg_data$CONCEPT_ID))

    # Perform statistical test for each CONCEPT_ID
    for (concept_id in unique(agg_data$CONCEPT_ID)) {
      prevalence_cohort_1 <-
        dplyr::filter(cohort_1, CONCEPT_ID == concept_id)$TOTAL_PREVALENCE
      prevalence_cohort_2 <-
        dplyr::filter(cohort_2, CONCEPT_ID == concept_id)$TOTAL_PREVALENCE

      if (length(prevalence_cohort_1) == 0 ||
          length(prevalence_cohort_2) == 0) {
        next
      } else if (prevalence_cohort_2 / sample_2_n < presenceFilter) {
        next
      } else if (prevalence_cohort_2 > sample_2_n ||
                 prevalence_cohort_1 > sample_1_n) {
        next
      } else if (prevalence_cohort_2 == prevalence_cohort_1) {
        next
      } else {
        test_result <-
          stats::prop.test(
            c(prevalence_cohort_1, prevalence_cohort_2),
            c(sample_1_n, sample_2_n),
            conf.level = 0.95
          )
        if (is.na(test_result$p.value)) {
          next
        }
        if (test_result$p.value < alpha) {
          significant_concepts <-
            rbind(
              significant_concepts,
              data.frame(CONCEPT_ID = concept_id, P_VALUE = test_result$p.value)
            )
        }
      }
    }

    return(significant_concepts)
  }

#' @title This function learns separate logistic regression models on the patient prevalence data target vs control
#'
#' @param data_patients Prevalence data for patients
#' @param data_initial Imported cohort dataframe
#' @param targetCohortId Target cohort id
#' @param presenceFilter Presence filter 0-1, if 0.1 then feature has to be present for at least 10 percent of patients
#'
#' @keywords internal

performPrevalenceAnalysisLogistic <-
  function(data_patients,
           data_initial,
           targetCohortId,
           presenceFilter) {
    # Aggregate the prevalence data for each concept within each cohort
    agg_data <- data_patients %>%
      dplyr::mutate(PREVALENCE = dplyr::if_else(PREVALENCE > 0, 1, 0)) %>%
      dplyr::group_by(COHORT_DEFINITION_ID, CONCEPT_ID) %>%
      dplyr::summarise(TOTAL_PREVALENCE = sum(PREVALENCE),
                       .groups = 'drop')

    # Separate the data for each cohort for easier analysis
    cohort_1 <-
      dplyr::filter(agg_data, COHORT_DEFINITION_ID != targetCohortId)
    cohort_2 <-
      dplyr::filter(agg_data, COHORT_DEFINITION_ID == targetCohortId)

    # Count of patients in each cohort
    sample_1_n <-
      sum(data_initial$COHORT_DEFINITION_ID != targetCohortId)
    sample_2_n <-
      sum(data_initial$COHORT_DEFINITION_ID == targetCohortId)

    # Prepare a data frame to hold the results
    significant_concepts <-
      data.frame(
        CONCEPT_ID = integer(),
        P_VALUE = double(),
        ODDS_RATIO = double()
      )

    # Implement Bonferroni correction
    printCustomMessage("Starting running logistic regressions on data...")
    alpha <- 0.05 / length(unique(agg_data$CONCEPT_ID))

    # Get unique concept IDs
    concept_ids <- unique(agg_data$CONCEPT_ID)

    # Perform logistic regression for each CONCEPT_ID
    for (i in seq_along(concept_ids)) {
      concept_id <- concept_ids[i]

      # Create the dataset for logistic regression
      concept_data <- data_patients %>%
        dplyr::mutate(
          PREVALENCE = dplyr::if_else(CONCEPT_ID == concept_id &
                                        PREVALENCE > 0, 1, 0),
          TARGET = dplyr::if_else(COHORT_DEFINITION_ID == targetCohortId, 1, 0)
        )

      # Ensure presence filter and valid sample sizes
      prevalence_cohort_2 <-
        sum(concept_data$PREVALENCE[concept_data$TARGET == 1])
      if (prevalence_cohort_2 / sample_2_n < presenceFilter) {
        next
      }

      # Perform logistic regression
      model <-
        stats::glm(TARGET ~ PREVALENCE, data = concept_data, family = stats::binomial)
      summary_model <- summary(model)

      # Check if the presence of the concept is significant
      p_value <- summary_model$coefficients[2, 4]
      odds_ratio <- exp(stats::coef(model)[2])

      if (!is.na(p_value) && p_value < alpha) {
        significant_concepts <- rbind(
          significant_concepts,
          data.frame(
            CONCEPT_ID = concept_id,
            P_VALUE = p_value,
            ODDS_RATIO = odds_ratio
          )
        )
      }
    }

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
      dplyr::group_by(COHORT_DEFINITION_ID) %>%
      dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
      tidyr::pivot_wider(
        names_from = COHORT_DEFINITION_ID,
        values_from = count,
        values_fill = list(count = 0)
      )

    count_target <- n_patients$`2`
    count_control <- n_patients$`1`
    # Update data features with prevalence calculations
    data_features <- data$data_patients %>%
      dplyr::group_by(CONCEPT_ID, CONCEPT_NAME) %>%
      dplyr::summarise(
        TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == 2 &
                                     PREVALENCE > 0),
        CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == 1 &
                                      PREVALENCE > 0),
        .groups = 'drop'
      ) %>%
      dplyr::mutate(
        TARGET_SUBJECT_PREVALENCE = TARGET_SUBJECT_COUNT / count_target,
        CONTROL_SUBJECT_PREVALENCE = CONTROL_SUBJECT_COUNT / count_control,
        PREVALENCE_DIFFERENCE_RATIO = dplyr::case_when(
          is.na(TARGET_SUBJECT_PREVALENCE) |
            TARGET_SUBJECT_PREVALENCE == 0 ~ 0,
          (
            is.na(CONTROL_SUBJECT_PREVALENCE) |
              CONTROL_SUBJECT_PREVALENCE == 0
          ) & is.na(TARGET_SUBJECT_PREVALENCE) ~ -1,
          is.na(CONTROL_SUBJECT_PREVALENCE) |
            CONTROL_SUBJECT_PREVALENCE == 0 ~ 100,
          TRUE ~ TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
        )
      )

    if (nHighestPrevalenceDifference != FALSE) {
      # Keep only the nHighestPrevalenceDifference rows with highest values of PREVALENCE_DIFFERENCE_RATIO
      data_features <- data_features %>%
        dplyr::arrange(dplyr::desc(PREVALENCE_DIFFERENCE_RATIO)) %>%
        dplyr::slice_head(n = nHighestPrevalenceDifference)
    }

    return(data_features)
  }

#' This function uses complementaryMappingTable to map concepts to custom names
#' @param data Data list object
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present
#'
#' @keywords internal

handleMapping <- function(data, complementaryMappingTable) {
  printCustomMessage("Mapping according to predefined complementaryMappingTable...")
  # Join the dataframes on CONCEPT_ID
  data$data_patients <-
    dplyr::select(
      dplyr::mutate(
        dplyr::left_join(
          data$data_patients,
          complementaryMappingTable,
          by = "CONCEPT_ID",
          suffix = c("", ".compl")
        ),
        # Choose the complementaryMappingTable CONCEPT_NAME if it's not NA; otherwise, use the original
        CONCEPT_NAME = dplyr::if_else(
          is.na(CONCEPT_NAME.compl),
          CONCEPT_NAME,
          CONCEPT_NAME.compl
        )
      ),
      # Select and rename the columns to match the original concept_map structure
      COHORT_DEFINITION_ID,
      PERSON_ID,
      CONCEPT_ID,
      CONCEPT_NAME,
      PREVALENCE,
      HERITAGE
    )

  data_patients <- data$data_patients

  # Step 1: Create a mapping of CONCEPT_NAME to the first occurrence of CONCEPT_ID in complementaryMappingTable
  unique_concept_ids <- complementaryMappingTable %>%
    dplyr::group_by(CONCEPT_NAME) %>%
    dplyr::slice(1) %>%
    dplyr::select(CONCEPT_NAME, CONCEPT_ID)

  # Step 2: Replace CONCEPT_ID in data_patients with the mapped CONCEPT_ID for each CONCEPT_NAME
  data_patients <- data_patients %>%
    dplyr::left_join(unique_concept_ids,
              by = "CONCEPT_NAME",
              suffix = c("", ".new")) %>%
   dplyr:: mutate(CONCEPT_ID = dplyr::if_else(is.na(CONCEPT_ID.new), CONCEPT_ID, CONCEPT_ID.new)) %>%
    dplyr::select(-CONCEPT_ID.new)

  # Step 3: Summarize data_patients to aggregate PREVALENCE
  final_data <- data_patients %>%
    dplyr::group_by(COHORT_DEFINITION_ID,
                    PERSON_ID,
                    CONCEPT_ID,
                    CONCEPT_NAME,
                    HERITAGE) %>%
    dplyr::summarise(PREVALENCE = sum(PREVALENCE, na.rm = TRUE),
              .groups = 'drop')

  final_data_summarized <- final_data %>%
    dplyr::group_by(CONCEPT_ID, HERITAGE) %>%
    dplyr::summarise(Summed_Prevalence = sum(PREVALENCE, na.rm = TRUE),
              .groups = 'drop')

  # Assign the most prevalent heritage to each concept ID and PERSON_ID
  most_prevalent_heritage <- final_data_summarized %>%
    dplyr::group_by(CONCEPT_ID) %>%
    dplyr::filter(Summed_Prevalence == max(Summed_Prevalence)) %>%
    dplyr::slice(1) %>%  # In case of tie, take the first occurrence
    dplyr::select(CONCEPT_ID, HERITAGE)

  # Merge the most prevalent heritage back to the original dataframe and sum the PREVALENCE
  result <- final_data %>%
    dplyr::select(-HERITAGE) %>%
    dplyr::left_join(most_prevalent_heritage, by = "CONCEPT_ID") %>%
    dplyr::group_by(COHORT_DEFINITION_ID,
                    PERSON_ID,
                    CONCEPT_ID,
                    CONCEPT_NAME,
                    HERITAGE) %>%
    dplyr::summarise(PREVALENCE = sum(PREVALENCE, na.rm = TRUE),
              .groups = 'drop')

  # Assign the result back to data$data_patients
  data$data_patients <- result
  return(data)
}

#' Function for extracting n features
#' @param data Data list object
#' @param topK numeric > if set, keeps this number of features in the analysis. Maximum number of features exported.
#'
#' @keywords internal

createDataFeatures <- function(data, topK) {
  printCustomMessage("Get top n features ...")
  data$data_features = calculate_data_features(data, topK)
  data$data_patients <-
    data$data_patients %>% dplyr::filter(CONCEPT_ID %in% unique(data$data_features$CONCEPT_ID))
  return(data)
}

#' Function for handling tests
#' @param data Data list object
#' @param targetCohortId Target cohort id
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param runZTests boolean for Z-tests
#' @param runLogitTests boolean for logit-tests
#'
#' @keywords internal

handleTests <-
  function(data,
           targetCohortId,
           presenceFilter,
           runZTests = TRUE,
           runLogitTests = TRUE) {
    data_features = data$data_features
    data_patients = data$data_patients
    data_initial = data$data_initial

    if (runZTests) {
      significant_concepts <-
        performPrevalenceAnalysis(data_patients,
                                  data_initial,
                                  targetCohortId,
                                  presenceFilter)
      data_features <- data_features %>%
        dplyr::mutate(ZTEST = dplyr::if_else(
          CONCEPT_ID %in% significant_concepts$CONCEPT_ID,
          TRUE,
          FALSE
        ))
      printCustomMessage("Z-test on data executed!")
    }
    else {
      data_features <- data_features %>%
        dplyr::mutate(ZTEST = FALSE)
      printCustomMessage("Z-tests were disabled!")
    }
    # Logit test
    if (runLogitTests) {
      significant_concepts <-
        performPrevalenceAnalysisLogistic(data_patients,
                                          data_initial,
                                          targetCohortId,
                                          presenceFilter)
      data_features <-
        data_features <- data_features %>%
        dplyr::mutate(
          LOGITTEST = dplyr::if_else(
            CONCEPT_ID %in% significant_concepts$CONCEPT_ID,
            TRUE,
            FALSE
          )
        )
      printCustomMessage("Logit-test on data executed!")
    }  else {
      data_features <- data_features %>%
        dplyr::mutate(LOGITTEST = FALSE)
      printCustomMessage("Logit-tests were disabled!")
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
    data_features = data$data_features %>% dplyr::filter(ZTEST | LOGITTEST)
    }
    n_features_left = nrow(data_features)
    resultList = list()

    # Now, significant_concepts_data contains only the concepts significantly overrepresented in target cohort
    if (n_features_left == 0) {
      printCustomMessage("No features left. Perhaps use more lenient filters! Exiting ...")
      printCustomMessage("Running analysis END!")
      ################################################################################
      #
      # Save
      #
      ################################################################################

      if (createOutputFiles){
        createPathToResults(pathToResults = pathToResults)
        saveResult(data, pathToResults)      }
      return(data)
    }


    if (topK == FALSE) {
      features = dplyr::filter(data_features,
                               PREVALENCE_DIFFERENCE_RATIO > prevalenceCutOff)
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
          data_features, dplyr::desc(PREVALENCE_DIFFERENCE_RATIO)
        ), 1:topK), CONCEPT_ID)
      features = dplyr::filter(data_features, CONCEPT_ID %in% topNFeatures)
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
        dplyr::filter(data$data_patients, COHORT_DEFINITION_ID == targetCohortId),
        CONCEPT_ID %in% features$CONCEPT_ID
      ),
      PERSON_ID,
      CONCEPT_NAME,
      PREVALENCE
    )

    patients <-
      dplyr::summarise(dplyr::group_by(patients, PERSON_ID, CONCEPT_NAME),
                       PREVALENCE = mean(PREVALENCE))

    transformed_data <-
      tidyr::spread(patients, CONCEPT_NAME, PREVALENCE, fill = 0)
    patients_data <-
      transformed_data[,-1] # Remove the first column (patient IDs)
    # export selected features
    resultList$selectedFeatureNames = colnames(patients_data)
    resultList$selectedFeatureIds = features$CONCEPT_ID
    resultList$selectedFeatures = features
    data$resultList = resultList
    return(data)
  }

#' Function for creating dataset which can be used as input for Cohort2Trajectory package
#' @param data Data list object
#' @param targetCohortId Target cohort id
#' @param createC2TInput Boolean for creating Cohort2Trajectory input dataframe
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present
#'
#' @keywords internal

createC2TInput <-
  function(data,
           targetCohortId,
           createC2TInput,
           complementaryMappingTable = FALSE) {
    if (createC2TInput) {
      data_selected_patients = dplyr::select(
        dplyr::filter(
          data$data_patients,
          CONCEPT_NAME %in% data$resultList$selectedFeatureNames,
          COHORT_DEFINITION_ID == targetCohortId
        ),
        CONCEPT_ID,
        CONCEPT_NAME,
        PERSON_ID,
        HERITAGE
      )
      # Creating target cohort & eligible patients dataset
      data_target = dplyr::mutate(
        dplyr::filter(
          data$data_initial,
          COHORT_DEFINITION_ID == targetCohortId,
          SUBJECT_ID %in% unique(data_selected_patients$PERSON_ID)
        ),
        COHORT_DEFINITION_ID = 0
      )

      printCustomMessage("Creating a dataset for eligible patients only (Cohort2Trajectory input) ...")
      # Creating state cohorts / eligible patients dataset
      # Split the data by heritage
      split_data <-
        split(data_selected_patients, data_selected_patients$HERITAGE)
      # Iterate over each heritage type and construct then execute SQL queries
      results_list <-
        queryHeritageData(
          dataPatient = data_selected_patients,
          cdm = data$cdm,
          split_data = split_data,
          complementaryMappingTable = complementaryMappingTable
        )

      data_states <- do.call(rbind, results_list)
      data_states$END_DATE = data_states$START_DATE
      # Assuming you want to combine all results into a single data fram
      colnames(data_states) <-
        c("COHORT_DEFINITION_ID",
          "SUBJECT_ID",
          "COHORT_START_DATE",
          "COHORT_END_DATE")

      data$resultList$trajectoryData = rbind(data_target, data_states)
    }
    # Convert from int64 to integer
    data$resultList$trajectoryData$SUBJECT_ID = as.integer(data$resultList$trajectoryData$SUBJECT_ID)
    return(data)
  }


#' This function saves the resulting data object
#' @param data Data list object
#' @param pathToResults Path to the results folder, can be project's working directory
#'
#' @keywords internal

saveResult <- function(data, pathToResults) {
  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  # Create the full file path with the timestamp included in the filename
  filePath <- file.path(pathToResults, paste0("CohortContrast_", timestamp, ".rdata"))
  data$data_patients = dplyr::mutate(data$data_patients, COHORT_DEFINITION_ID = dplyr::if_else(COHORT_DEFINITION_ID == 2, "target", "control"))
  data$data_patients = dplyr::mutate(data$data_initial, COHORT_DEFINITION_ID = dplyr::if_else(COHORT_DEFINITION_ID == 2, "target", "control"))
  save_object(data, path = filePath)
  printCustomMessage(paste("Saved the result to ", filePath, sep = ""))
}
