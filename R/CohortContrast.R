#' Get data from OMOP CDM specified in imported JSON files
#'
#' This function outputs a dataframe with columns SUBJECT_ID, COHORT_DEFINITION_ID, COHORT_START_DATE, COHORT_END_DATE
#' @param connection Connection to database
#' @param connectionDetails An object of class connectionDetails as created by the DatabaseConnector::createConnectionDetails function.
#' @param cdmSchema JSONs from which cohorts will be created
#' @param cdmDataSchema Schema which contains the OHDSI Common Data Model.
#' @param cdmVocabSchema Schema for temporary tables
#' @param studyName Customized name for the study
#' @param domainsIncluded list of CDM domains to include
#' @param generateTables boolean > if TRUE new tables will be generated to the database.
#' @param readFromCSV boolean > if TRUE file from ./inst/CSV will be used as cohort data, otherwise JSONs will be used.
#' @param prevalenceCutOff numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param topDogs numeric > if set, keeps this number of features in the analysis, top n prevalence difference
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param nudgeTarget number of days you would like to nudge the target cohort start day
#' @param nudgeControl number of days you would like to nudge the control cohort start day
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present
#'
#' @keywords internal
CohortContrast <- function(connection,
                           connectionDetails,
                           cdmSchema,
                           cdmVocabSchema,
                           cdmTmpSchema,
                           pathToResults,
                           studyName,
                           domainsIncluded = c(
                             "Drug",
                             "Condition",
                             "Measurement",
                             "Observation",
                             "Procedure",
                             "Visit",
                             "Visit detail"
                           ),
                           generateTables = TRUE,
                           readFromCSV = FALSE,
                           prevalenceCutOff = 10,
                           topDogs = FALSE,
                           presenceFilter = 0.005,
                           nudgeControl = FALSE,
                           nudgeTarget = FALSE,
                           complementaryMappingTable = FALSE,
                           createC2TInput = FALSE) {
  printCustomMessage("Creating mandatory subdirectories ...")
  createMandatorySubDirs(pathToResults)
  targetCohortId = 2

  data = generateTables(
    connection = connection,
    dbms = connectionDetails$dbms,
    cdmSchema = cdmSchema,
    cdmVocabSchema = cdmVocabSchema,
    cdmTmpSchema = cdmTmpSchema,
    pathToResults = pathToResults,
    studyName = studyName,
    generateTables = generateTables,
    readFromCSV = readFromCSV,
    domainsIncluded = domainsIncluded,
    nudgeControl = nudgeControl,
    nudgeTarget = nudgeTarget
  )

  if (is.data.frame(complementaryMappingTable)) {
    data = handleMapping(data, complementaryMappingTable)
  }

  data = createDataFeatures(data, topDogs)
  data = handleTests(data, targetCohortId, presenceFilter)
  data = handleFeatureSelection(data, topDogs, prevalenceCutOff, targetCohortId)
  data = createC2TInput(data, targetCohortId, createC2TInput, complementaryMappingTable)
  data = saveResult(data, pathToResults, studyName)

  return(data)
}

queryHeritageData <-
  function(dataPatient,
           connection,
           cdmSchema,
           cdmTmpSchema,
           studyName,
           split_data,
           complementaryMappingTable = FALSE) {
    results_list <- lapply(names(split_data), function(heritage) {
      printCustomMessage(paste("Querying eligible ", heritage, " data ...", sep = ""))
      unique_person_ids <- unique(split_data[[heritage]]$PERSON_ID)
      unique_concept_ids <-
        unique(split_data[[heritage]]$CONCEPT_ID)

      # Construct the SQL query
      sql_query <- sprintf(
        "SELECT base.* FROM %s AS base JOIN %s ON base.person_id = %s WHERE %s IN (%s)",
        paste(cdmSchema, heritage, sep = "."),
        paste(cdmTmpSchema, studyName, sep = "."),
        paste(cdmTmpSchema, studyName, "subject_id", sep = "."),
        sub("_occurrence_", "_", sub(
          "_exposure_", "_", paste(heritage, 'concept_id', sep = "_")
        )),
        paste(unique_concept_ids, collapse = ", ")
      )

      # Execute SQL query
      query_result <-
        DatabaseConnector::querySql(connection, sql_query)

      # Further processing...
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
              CONCEPT_NAME = ifelse(
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
      # concept_map$CONCEPT_NAME <-
      #   substr(concept_map$CONCEPT_NAME, 1, 20)
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
      query_result_mapped = dplyr::filter(query_result_mapped, PERSON_ID %in% unique_person_ids)

      printCustomMessage(paste("Quering eligible ", heritage, " data finished.", sep = ""))

      return(query_result_mapped) # Assuming `query_result_mapped` is the final processed result
    })

    return(results_list)
  }


performPrevalenceAnalysis <-
  function(data_patients,
           data_initial,
           targetCohortId,
           presenceFilter) {
    # Aggregate the prevalence data for each concept within each cohort
    agg_data <- dplyr::summarise(
      dplyr::group_by(
        dplyr::mutate(data_patients, PREVALENCE = ifelse(PREVALENCE > 0, 1, 0)),
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
          prop.test(
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

performPrevalenceAnalysisLogistic <-
  function(data_patients,
           data_initial,
           targetCohortId,
           presenceFilter) {
    # Aggregate the prevalence data for each concept within each cohort
    agg_data <- data_patients %>%
      dplyr::mutate(PREVALENCE = ifelse(PREVALENCE > 0, 1, 0)) %>%
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
          PREVALENCE = ifelse(CONCEPT_ID == concept_id &
                                PREVALENCE > 0, 1, 0),
          TARGET = ifelse(COHORT_DEFINITION_ID == targetCohortId, 1, 0)
        )

      # Ensure presence filter and valid sample sizes
      prevalence_cohort_2 <-
        sum(concept_data$PREVALENCE[concept_data$TARGET == 1])
      if (prevalence_cohort_2 / sample_2_n < presenceFilter) {
        next
      }

      # Perform logistic regression
      model <-
        glm(TARGET ~ PREVALENCE, data = concept_data, family = binomial)
      summary_model <- summary(model)

      # Check if the presence of the concept is significant
      p_value <- summary_model$coefficients[2, 4]
      odds_ratio <- exp(coef(model)[2])

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
        dplyr::arrange(desc(PREVALENCE_DIFFERENCE_RATIO)) %>%
        dplyr::slice_head(n = nHighestPrevalenceDifference)
    }

    return(data_features)
  }

# Define a function to handle the mapping
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
          CONCEPT_NAME = ifelse(
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
      group_by(CONCEPT_NAME) %>%
      slice(1) %>%
      select(CONCEPT_NAME, CONCEPT_ID)

  # Step 2: Replace CONCEPT_ID in data_patients with the mapped CONCEPT_ID for each CONCEPT_NAME
  data_patients <- data_patients %>%
      left_join(unique_concept_ids,
                by = "CONCEPT_NAME",
                suffix = c("", ".new")) %>%
      mutate(CONCEPT_ID = ifelse(is.na(CONCEPT_ID.new), CONCEPT_ID, CONCEPT_ID.new)) %>%
      select(-CONCEPT_ID.new)

  # Step 3: Summarize data_patients to aggregate PREVALENCE
  final_data <- data_patients %>%
      group_by(COHORT_DEFINITION_ID,
               PERSON_ID,
               CONCEPT_ID,
               CONCEPT_NAME,
               HERITAGE) %>%
      summarise(PREVALENCE = sum(PREVALENCE, na.rm = TRUE),
                .groups = 'drop')

  final_data_summarized <- final_data %>%
    group_by(CONCEPT_ID, HERITAGE) %>%
    summarise(Summed_Prevalence = sum(PREVALENCE, na.rm = TRUE), .groups = 'drop')

  # Assign the most prevalent heritage to each concept ID and PERSON_ID
  most_prevalent_heritage <- final_data_summarized %>%
    group_by(CONCEPT_ID) %>%
    filter(Summed_Prevalence == max(Summed_Prevalence)) %>%
    slice(1) %>%  # In case of tie, take the first occurrence
    select(CONCEPT_ID, HERITAGE)

  # Merge the most prevalent heritage back to the original dataframe and sum the PREVALENCE
  result <- final_data %>%
    select(-HERITAGE) %>%
    left_join(most_prevalent_heritage, by = "CONCEPT_ID") %>%
    group_by(COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, CONCEPT_NAME, HERITAGE) %>%
    summarise(PREVALENCE = sum(PREVALENCE, na.rm = TRUE), .groups = 'drop')

  # Assign the result back to data$data_patients
  data$data_patients <- result
  return(data)
}

# Define a function to handle the creation of data_features dataframe
createDataFeatures <- function(data, topDogs) {
  printCustomMessage("Get top n features ...")
  data$data_features = calculate_data_features(data, topDogs)
  data$data_patients <-
    data$data_patients %>% dplyr::filter(CONCEPT_ID %in% unique(data$data_features$CONCEPT_ID))
  return(data)
}

# Define a function to handle the tests
handleTests <- function(data, targetCohortId, presenceFilter) {
  data_features = data$data_features
  data_patients = data$data_patients
  data_initial = data$data_initial

  significant_concepts <-
    performPrevalenceAnalysis(data_patients,
                              data_initial,
                              targetCohortId,
                              presenceFilter)
  data_features <- data_features %>%
    dplyr::mutate(ZTEST = dplyr::if_else(CONCEPT_ID %in% significant_concepts$CONCEPT_ID, TRUE, FALSE))
  printCustomMessage("Z-test on data executed!")
  # Logit test
  significant_concepts <-
    performPrevalenceAnalysisLogistic(data_patients,
                                      data_initial,
                                      targetCohortId,
                                      presenceFilter)
  data_features <-
    data_features <- data_features %>%
    dplyr::mutate(
      LOGITTEST = dplyr::if_else(CONCEPT_ID %in% significant_concepts$CONCEPT_ID, TRUE, FALSE)
    )
  printCustomMessage("Logit-test on data executed!")

  # Overwrite data_features to apply tests
  data$data_features = data_features
  return(data)
}

# Define a function to handle the feature selection
handleFeatureSelection <- function(data, topDogs, prevalenceCutOff, targetCohortId) {
  data_features = data$data_features
  n_features_left = nrow(data_features)
  resultList = list()

  # Now, significant_concepts_data contains only the concepts significantly overrepresented in cohort 2
  if (n_features_left == 0) {
    printCustomMessage("No features left. Perhaps use more lenient filters! Exiting ...")
    printCustomMessage("Running analysis END!")
    ################################################################################
    #
    # Save
    #
    ################################################################################

    filePath = paste(pathToResults,
                     "/tmp/datasets/",
                     studyName,
                     "_CC_medData.rdata",
                     sep = "")
    save_object(data, path = filePath)
    printCustomMessage(paste("Saved the result to ", filePath, sep = ""))
    return(data)
  }


  if (topDogs == FALSE) {
    features = dplyr::filter(data_features,
                             PREVALENCE_DIFFERENCE_RATIO > prevalenceCutOff)
    n_features_left = nrow(features)
    printCustomMessage(
      paste(
        "After filtering for prevalence cutoff of",
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
        data_features, desc(PREVALENCE_DIFFERENCE_RATIO)
      ), 1:topDogs), CONCEPT_ID)
    features = dplyr::filter(data_features, CONCEPT_ID %in% topNFeatures)
    n_features_left = nrow(features)
    printCustomMessage(
      paste(
        "After filtering for top ",
        topDogs,
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
    transformed_data[, -1] # Remove the first column (patient IDs)
  # export selected features
  resultList$selectedFeatureNames = colnames(patients_data)
  resultList$selectedFeatureIds = features$CONCEPT_ID
  resultList$selectedFeatures = features
  data$resultList = resultList
  return(data)
}

# Define a function to handle the creation of C2T input
createC2TInput <- function(data, targetCohortId, createC2TInput,complementaryMappingTable = FALSE) {
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
    #
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
        connection,
        cdmSchema,
        cdmTmpSchema,
        studyName,
        split_data,
        complementaryMappingTable
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
    #data$resultList$trajectoryData$COHORT_DEFINITION_ID = trimws(data$resultList$trajectoryData$COHORT_DEFINITION_ID)
  }
  return(data)
}

# Define a function to save the result
saveResult <- function(data, pathToResults, studyName) {
  filePath = paste(pathToResults,
                   "/tmp/datasets/",
                   studyName,
                   "_CC_medData.rdata",
                   sep = "")
  save_object(data, path = filePath)
  printCustomMessage(paste("Saved the result to ", filePath, sep = ""))
  return(data)
}
