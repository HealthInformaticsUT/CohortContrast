#' @keywords internal
combineSelectedConcepts <- function(new_concept_name, new_concept_id = NULL, selected_ids = NULL, abstraction_level = -1, data = NULL) {
    if(is.null(data)) return(NULL)

    data_features <- data.table::as.data.table(data$data_features)
    data_patients <- data.table::as.data.table(data$data_patients)
    data_initial <- data.table::as.data.table(data$data_initial)
    data_person <- data.table::as.data.table(data$data_person)
    complementary_mapping <- data.table::as.data.table(data$complementaryMappingTable)
    # Assuming data_initial, data_features, and processed_table are data.tables

    # check if new_concept_name can be used
    used_concept_names <- unique(data_patients %>% dplyr::filter(!(.data$CONCEPT_ID %in% selected_ids)) %>%  dplyr::pull(CONCEPT_NAME))
    counter = 2
    maybe_new_concept_name = new_concept_name
    while(maybe_new_concept_name %in% used_concept_names){
      maybe_new_concept_name = paste(new_concept_name, counter, sep = " ")
      counter = counter + 1
      cli::cli_alert_warning(paste0("Mapped '", gsub("\\{", "{{", gsub("\\}", "}}", new_concept_name)), "' to '",  gsub("\\{", "{{", gsub("\\}", "}}", maybe_new_concept_name)), "' because of duplicate concept names for differing ids!"))
    }
    new_concept_name = maybe_new_concept_name

    # Step 1: Grouping and summarizing, followed by reshaping
    n_patients_temp <- data_initial[, .N, by = COHORT_DEFINITION_ID]
    n_patients <- reshape2::dcast(n_patients_temp, formula = 1 ~ COHORT_DEFINITION_ID, value.var = "N", fill = 0)
    # Step 2: Extracting count values
    count_target <- n_patients$target
    count_control <- n_patients$control

    processed_table <- data_features[ABSTRACTION_LEVEL == abstraction_level]

    selected_concept_ids = NA
    selected_concept_names = NA
    representingConceptId = new_concept_id
    representingHeritage = NA
    representingZTest = NA
    representingLogitTest = NA
    representingKSTest = NA

    # Step 3: Filtering processed_table and selecting concept IDs
    selected_concept_ids <- as.numeric(selected_ids)
    selected_concept_names <- processed_table[CONCEPT_ID %in% selected_concept_ids, CONCEPT_NAME]

    if(is.null(representingConceptId)){
      representingConceptId <- selected_concept_ids[
        which.max(as.numeric(processed_table[CONCEPT_ID %in% selected_concept_ids, PREVALENCE_DIFFERENCE_RATIO]))
      ]
    }
    representingHeritage <- processed_table[CONCEPT_ID %in% selected_concept_ids, HERITAGE][
      which.max(as.numeric(processed_table[CONCEPT_ID %in% selected_concept_ids, PREVALENCE_DIFFERENCE_RATIO]))]

    # Step 4: Determining ZTEST, KSTEST and LOGITTEST values
    representingZTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, ZTEST])
    representingLogitTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, LOGITTEST])
    representingKSTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, KSTEST])

    new_row <- data.table::data.table(
      CONCEPT_ID = representingConceptId,
      CONCEPT_NAME = new_concept_name,
      ABSTRACTION_LEVEL = abstraction_level,
      ZTEST = representingZTest,
      LOGITTEST = representingLogitTest,
      KSTEST = representingKSTest,
      HERITAGE = representingHeritage
    )

    # Remove row if there was for representingConceptId
    data_features <- data_features[!(CONCEPT_ID %in% c(selected_concept_ids, representingConceptId) & ABSTRACTION_LEVEL == abstraction_level)]

    data_features <- rbind(data_features, new_row, fill = TRUE)
    rows_to_update <- data_patients[
      , CONCEPT_ID %in% selected_concept_ids & ABSTRACTION_LEVEL == abstraction_level
    ]

    # Step 4: Update rows and group by relevant columns, then summarize
    data_patients[
      rows_to_update,
      `:=`(
        CONCEPT_ID = representingConceptId,
        CONCEPT_NAME = new_concept_name,
        HERITAGE = representingHeritage
      )
    ]
    # Step 5: Summarize by grouping and calculating prevalence
    data_patients <- data_patients[
      , .(PREVALENCE = sum(PREVALENCE),
          TIME_TO_EVENT = list(unlist(TIME_TO_EVENT))),
      by = .(COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, CONCEPT_NAME, HERITAGE, ABSTRACTION_LEVEL)
    ]


    # Assuming data_features and data_patients are data.tables
    # Step 1: Select specific columns from data_features
    data_features_temp <- data_features[
      , .(CONCEPT_ID, ABSTRACTION_LEVEL, ZTEST,ZTEST_P_VALUE, LOGITTEST,LOGITTEST_P_VALUE, KSTEST, KSTEST_P_VALUE, HERITAGE)
    ]
    # Step 2: Summarize data_patients by CONCEPT_ID, CONCEPT_NAME, and ABSTRACTION_LEVEL

    data_features <- data_patients[
      , .(
        TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "target" & PREVALENCE > 0),
        CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "control" & PREVALENCE > 0),
        TIME_TO_EVENT = list(unlist(TIME_TO_EVENT))
      ),
      by = .(CONCEPT_ID, CONCEPT_NAME, ABSTRACTION_LEVEL)
    ]

    # Step 3: Add new columns using mutate-like operations
    data_features <- data_features[
      , `:=`(
        TARGET_SUBJECT_PREVALENCE = TARGET_SUBJECT_COUNT / count_target,
        CONTROL_SUBJECT_PREVALENCE = CONTROL_SUBJECT_COUNT / count_control
      )
    ]

    data_features <- data_features[
      , `:=`(
        PREVALENCE_DIFFERENCE_RATIO = data.table::fifelse(
          is.na(TARGET_SUBJECT_PREVALENCE) | TARGET_SUBJECT_PREVALENCE == 0, 0,
          data.table::fifelse(
            is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0,
            data.table::fifelse(TARGET_SUBJECT_PREVALENCE == 0, -1, 100),
            TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
          )
        )
      )
    ]

    # Step 4: Join with data_features_temp
    data_features <- data_features[
      data_features_temp,
      on = .(CONCEPT_ID, ABSTRACTION_LEVEL)
    ]

    data_features <- data_features[!is.na(CONCEPT_NAME)]

    # Step 5: Determining ZTEST, KSTEST and LOGITTEST values
    total_concepts = data_features %>% dplyr::filter(.data$ABSTRACTION_LEVEL == abstraction_level) %>% nrow()
    # ZTEST
    concept_row <- data_features[CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstraction_level, .(TARGET_SUBJECT_COUNT, CONTROL_SUBJECT_COUNT)]
    target_subject_count <- concept_row$TARGET_SUBJECT_COUNT
    control_subject_count <- concept_row$CONTROL_SUBJECT_COUNT

    representingZTest = FALSE
    representingZTestPValue = 1

    ztest_result <- stats::prop.test(
      c(target_subject_count, control_subject_count),
      c(count_target, count_control),
      conf.level = 0.95
    )
    if (!(is.na(ztest_result$p.value))) {
      representingZTestPValue = ztest_result$p.value
      representingZTest = if(representingZTestPValue < 0.05/total_concepts) TRUE else FALSE
    }

    # Logit Test
    representingLogitTest = FALSE
    representingLogitTestPValue = 1
    # Create the dataset for logistic regression
    concept_data <- data_patients %>%
      dplyr::filter(.data$ABSTRACTION_LEVEL == abstraction_level, .data$CONCEPT_ID == representingConceptId) %>%
      dplyr::mutate(
        PREVALENCE = 1,
        TARGET = dplyr::if_else(.data$COHORT_DEFINITION_ID == "target", 1, 0),
        CONTROL = dplyr::if_else(.data$COHORT_DEFINITION_ID != "target", 0, 1)
      ) %>% dplyr::select(.data$COHORT_DEFINITION_ID, .data$PREVALENCE, .data$TARGET, .data$CONTROL)

    no_match_target = data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID == "target") %>% nrow() - concept_data %>% dplyr::filter(.data$COHORT_DEFINITION_ID == "target") %>% nrow()
    no_match_control = data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID != "target") %>% nrow() - concept_data %>% dplyr::filter(.data$COHORT_DEFINITION_ID != "target") %>% nrow()

    no_match_target_df = NULL
    no_match_control_df = NULL
    if(no_match_target != 0){
      no_match_target_df = data.frame(COHORT_DEFINITION_ID = "target", PREVALENCE = 0, TARGET = rep(1, no_match_target), CONTROL = 0)
    }
    if(no_match_control != 0){
      no_match_control_df = data.frame(COHORT_DEFINITION_ID = "control", PREVALENCE = 0, TARGET = 0, CONTROL = rep(1, no_match_control))
    }

    concept_data = rbind(rbind(no_match_target_df, no_match_control_df), concept_data)

    prevalence_cohort_2 <- ifelse(is.na(sum(concept_data$PREVALENCE[concept_data$TARGET == 1])), 0, sum(concept_data$PREVALENCE[concept_data$TARGET == 1]))
    prevalence_cohort_1 <- ifelse(is.na(sum(concept_data$PREVALENCE[concept_data$CONTROL == 1])), 0, sum(concept_data$PREVALENCE[concept_data$CONTROL == 1]))

    if (prevalence_cohort_1 == 0 | prevalence_cohort_2 == 0) {
      representingLogitTestPValue = NA
      representingLogitTest = TRUE
    } else {
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

      if (!(is.na(p_value))) {
        representingLogitTestPValue = p_value
        representingLogitTest = if(representingLogitTestPValue < 0.05/total_concepts) TRUE else FALSE
      }
    }
    # KS Test
    agg_data <- data_features %>% dplyr::select(.data$CONCEPT_ID, .data$ABSTRACTION_LEVEL, .data$TIME_TO_EVENT)
    concept_date_array =  unlist(dplyr::filter(agg_data, .data$CONCEPT_ID == representingConceptId, .data$ABSTRACTION_LEVEL == abstraction_level)$TIME_TO_EVENT)
    concept_date_array_min = min(concept_date_array)
    concept_date_array_max = max(concept_date_array)
    ks_result <- stats::ks.test(jitter(concept_date_array, amount=0.1), "punif", min=concept_date_array_min, max=concept_date_array_max)

    representingKSTest = ks_result$p.value < 0.05/total_concepts
    representingKSTestPValue = ks_result$p.value

    # Update a specific row with new values for example columns
    data_features[CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstraction_level, `:=`(
      ZTEST = representingZTest,
      ZTEST_P_VALUE = representingZTestPValue,
      LOGITTEST = representingLogitTest,
      LOGITTEST_P_VALUE = representingLogitTestPValue,
      KSTEST = representingKSTest,
      KSTEST_P_VALUE = representingKSTestPValue,
      HERITAGE = representingHeritage
    )]

    # Update complementaryMappingTable
    new_rows <- data.frame(CONCEPT_ID = selected_concept_ids, CONCEPT_NAME = selected_concept_names, NEW_CONCEPT_ID = representingConceptId, NEW_CONCEPT_NAME = new_concept_name, ABSTRACTION_LEVEL = abstraction_level, stringsAsFactors = FALSE)
    complementary_mapping <- rbind(complementary_mapping, new_rows)

    # Step 1: Identify related concepts
    related_concepts <- complementary_mapping[
      complementary_mapping$CONCEPT_ID %in% selected_concept_ids & complementary_mapping$ABSTRACTION_LEVEL == abstraction_level,
      "NEW_CONCEPT_NAME"
    ]

    # Step 2: Update CONCEPT_NAME for related concepts
    complementary_mapping = data.table::as.data.table(complementary_mapping)
    complementary_mapping[
      complementary_mapping$NEW_CONCEPT_NAME %in% related_concepts & complementary_mapping$ABSTRACTION_LEVEL == abstraction_level,
      NEW_CONCEPT_NAME := new_concept_name
    ]

    # Step 3: Remove duplicates
    complementary_mapping <- unique(complementary_mapping)
    data$data_initial = data_initial
    data$data_patients = data_patients
    data$data_features = data_features
    data$data_person = data_person
    data$complementaryMappingTable = complementary_mapping

    return(data)
  }

#' Function for automatically combining concepts by hierarchy mapping
#'
#' @param data CohortContrastObject
#' @param abstraction_level abstraction level to use for mapping
#' @param minDepthAllowed integer for restricting the mapping, if a concept is part of a hierarchy tree then minDepthAllowed value will prune the tree from said depth value upwards
#' @export
automaticHierarchyCombineConcepts <- function(data, abstraction_level = -1, minDepthAllowed = 0) {

  concept_table = data.table::as.data.table(data$conceptsData$concept)
  concept_ancestor = data.table::as.data.table(data$conceptsData$concept_ancestor)

  concept_ancestor_summarised_descendants = concept_ancestor %>%
    dplyr::filter(ancestor_concept_id != descendant_concept_id) %>%
    dplyr::group_by(descendant_concept_id) %>%
    dplyr::summarise(distToRoot = max(min_levels_of_separation)) %>%
    dplyr::select(concept_id = descendant_concept_id, distToRoot)

  concept_ancestor_summarised_roots = concept_ancestor %>%
    dplyr::filter(ancestor_concept_id != descendant_concept_id) %>%
    dplyr::pull(ancestor_concept_id) %>% unique() %>% setdiff(concept_ancestor_summarised_descendants$concept_id) %>%
    tibble::as_tibble()
  colnames(concept_ancestor_summarised_roots) = c("concept_id")
  concept_ancestor_summarised_roots$distToRoot = 0

  concept_ancestor_summarised_alone = concept_ancestor %>%
    dplyr::filter(ancestor_concept_id == descendant_concept_id,
                  !(ancestor_concept_id %in% concept_ancestor_summarised_roots$concept_id),
                  !(ancestor_concept_id %in% concept_ancestor_summarised_descendants$concept_id)) %>%
    dplyr::select(concept_id = ancestor_concept_id) %>%
    dplyr::mutate(distToRoot = 999999999)

  concept_ancestor_allowed = rbind(concept_ancestor_summarised_descendants, concept_ancestor_summarised_roots, concept_ancestor_summarised_alone) %>%
    dplyr::filter(distToRoot >= minDepthAllowed) %>% dplyr::pull(concept_id)

  counter <- 1

  while (TRUE) {
    cli::cli_alert_warning(paste0("Automatic hierarchy mapping iteration ", counter))

    data_features = data$data_features %>% dplyr::filter(ABSTRACTION_LEVEL == abstraction_level,
                                                         CONCEPT_ID %in% concept_ancestor_allowed)
    counter = counter + 1

    mappingTable <- CohortContrast:::getAncestorMappings(
      active_concept_ids = data_features %>% dplyr::pull(CONCEPT_ID) %>% interaction(concept_ancestor_allowed$concept_id),
      concept_table = concept_table,
      concept_ancestor = concept_ancestor
    )

    mappingsToExecute <- CohortContrast:::filterHeritagePriorityMappings(mappingTable)

    if(nrow(mappingsToExecute) == 0){
      break
    }

    # Explicit for loop replaces apply
    for (i in seq_len(nrow(mappingsToExecute))) {
      row <- mappingsToExecute[i, ]
      selected_concept_ids <- as.numeric(unlist(row[['CONCEPT_IDS']]))

      if (!any(is.na(selected_concept_ids))) {
        selected_parent_id <- as.numeric(row[['PARENT_ID']])
        selected_parent_name <- as.character(row[['PARENT_NAME']])

        data <- combineSelectedConcepts(
          new_concept_name = selected_parent_name,
          new_concept_id = selected_parent_id,
          selected_ids = selected_concept_ids,
          abstraction_level = abstraction_level,
          data = data
        )
      }
    }
  }


  return(data)
}


#' Function for automatically combining concepts by hierarchy mapping
#'
#' @param data CohortContrastObject
#' @param abstraction_level abstraction level to use for mapping
#' @param minCorrelation minimum correlation to use for automatic concept combining
#' @param maxDaysInbetween minimum days inbetween concepts to use for automatic concept combining
#' @export
automaticCorrelationCombineConcepts <- function(data, abstraction_level = -1, minCorrelation = 0.7, maxDaysInbetween = 1) {

  counter <- 1

  while (TRUE) {
    cli::cli_alert_warning(paste0("Automatic correlation mapping iteration ", counter))

    target_filtered = CohortContrast:::format_results(data = data,
                                                      autoScaleRate = FALSE,
                                                      applyInverseTarget = FALSE,
                                                      applyZTest = data$config$runZTests,
                                                      applyLogitTest = data$config$runLogitTests,
                                                      abstractionLevel = abstraction_level)
    filter_target = CohortContrast:::filter_target(target = target_filtered, prevalence_threshold = data$config$presenceFilter,
                                                   prevalence_ratio_threshold = data$config$prevalenceCutOff,removeUntreated = FALSE)

    filter_target = prepare_filtered_target(filtered_target = filter_target, correlation_threshold = 0)

    result_corr <- CohortContrast:::computePairwiseCorrelations(filter_target$correlation_analysis$ordered_matrix,filter_target$target_row_annotation)
    result_time <- CohortContrast:::calculateMedianTransitions(filter_target$target_time_annotation)
    correlationSuggestionsTable <- CohortContrast:::mergeCorrelationWithTransitions(correlation_data = result_corr, transition_data = result_time)


    data_features = data$data_features %>% dplyr::filter(ABSTRACTION_LEVEL == abstraction_level)
    counter = counter + 1

    mappingsToExecute <- filterCorrelationMappings(correlationSuggestionsTable, data_features = data_features, minCorrelation = minCorrelation, maxDaysInbetween = maxDaysInbetween)

    if(nrow(mappingsToExecute) == 0){
      break
    }
    traversedConceptIds = c()
    # Explicit for loop replaces apply
    for (i in seq_len(nrow(mappingsToExecute))) {
      row <- mappingsToExecute[i, ]
      selected_concept_ids <- as.numeric(unlist(row[['CONCEPT_IDS']]))

      # We cannot allow for one concept to be merged multiple times during one iteration
      if (any(selected_concept_ids %in% traversedConceptIds)) next
      traversedConceptIds <- c(traversedConceptIds, selected_concept_ids)

      if (!any(is.na(selected_concept_ids))) {
        selected_parent_id <- as.numeric(unlist(row[['CONCEPT_IDS']]))[1]
        selected_parent_name <- as.character(unlist(row[['CONCEPT_NAMES']]))[1]

        data <- combineSelectedConcepts(
          new_concept_name = selected_parent_name,
          new_concept_id = selected_parent_id,
          selected_ids = selected_concept_ids,
          abstraction_level = abstraction_level,
          data = data
        )
      }
    }
  }

  return(data)
}

