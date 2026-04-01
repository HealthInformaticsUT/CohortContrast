# Feature engineering, concept mapping and feature selection internals
#' This filters data based on prevalence difference ratio and return the nHighestPrevalenceDifference greates differences
#' @param data Data list object
#' @param nHighestPrevalenceDifference Number of features with highest prevalence difference ratio to keep
#' @importFrom dplyr %>%
#'
#' @keywords internal

calculateDataFeatures <-
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
                ABSTRACTION_LEVEL = .data$ABSTRACTION_LEVEL.x)

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
  data = calculateDataFeatures(data, topK)
  return(data)
}


#' Function for handling feature selection
#' @param data Data list object
#' @param pathToResults Path to the results folder, can be project's working directory
#' @param targetCohortId Target cohort id
#' @param topK numeric > if set, keeps this number of features in the analysis. Maximum number of features exported.
#' @param prevalenceCutOff numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param runChi2YTests Boolean for running the CHI2Y test (chi-squared test
#'   for two proportions with Yates continuity correction).
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
           runChi2YTests,
           runLogitTests,
           createOutputFiles = FALSE
           ) {
    data_features = NULL
    if (!(runChi2YTests | runLogitTests)) {
    data_features = data$data_features
    }
    else{
    data_features = data$data_features %>% dplyr::filter(.data$CHI2Y | .data$LOGITTEST)
    }
    n_features_left = nrow(data_features)
    selectedFeatureData <- list()

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
    selectedFeatureData$selectedFeatureNames <- colnames(patients_data)
    selectedFeatureData$selectedFeatureIds <- features$CONCEPT_ID
    selectedFeatureData$selectedFeatures <- features
    data$selectedFeatureData <- selectedFeatureData
    data$trajectoryDataList <- selectedFeatureData
    return(data)
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
                    ABSTRACTION_LEVEL = .data$ABSTRACTION_LEVEL,
                    TYPE = "conflict") %>%
      dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$NEW_CONCEPT_ID, .data$NEW_CONCEPT_NAME, .data$ABSTRACTION_LEVEL, .data$TYPE)

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
                    ABSTRACTION_LEVEL = .data$ABSTRACTION_LEVEL,
                    TYPE = "conflict") %>%
      dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$NEW_CONCEPT_ID, .data$NEW_CONCEPT_NAME, .data$ABSTRACTION_LEVEL, .data$TYPE)

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
