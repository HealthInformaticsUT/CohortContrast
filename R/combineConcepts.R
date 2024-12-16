combineSelectedConcepts <- function(new_concept_name, concept_ids, abstraction_level, data) {
  selected_concept_ids <- concept_ids
  abstraction_level <- abstraction_level
  data_features <- data$data_features
  data_patients <- data$data_patients
  data_initial <- data$data_initial
  complementary_mapping <- data$complementaryMappingTable

  # Assuming data_initial, data_features, and processed_table are data.tables
  # Step 1: Grouping and summarizing, followed by reshaping
  n_patients_temp <- data_initial[, .N, by = COHORT_DEFINITION_ID]
  n_patients <- reshape2::dcast(n_patients_temp, formula = 1 ~ COHORT_DEFINITION_ID, value.var = "N", fill = 0)
  # Step 2: Extracting count values
  count_target <- n_patients$target
  count_control <- n_patients$control

  # Step 3: Filtering processed_table and selecting concept IDs
  processed_table <- data$data_features[ABSTRACTION_LEVEL == abstraction_level]
  #selected_concept_ids <- as.numeric(processed_table$CONCEPT_ID[selected_rows])
  filtered_table <- processed_table[processed_table$CONCEPT_ID %in% selected_concept_ids, ]

  representingConceptId <- selected_concept_ids[
    which.max(as.numeric(filtered_table$TARGET_SUBJECT_COUNT))
  ]
  representingHeritage <- filtered_table$HERITAGE[
    which.max(as.numeric(filtered_table$TARGET_SUBJECT_COUNT))]

  # Step 4: Determining ZTEST, KSTEST and LOGITTEST values
  representingZTest <- any(filtered_table$ZTEST)
  representingLogitTest <- any(filtered_table$LOGITTEST)
  representingKSTest <- any(filtered_table$KSTEST)

  # Step 5: Updating data_features with new values
  data_features[
    CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstraction_level,
    `:=`(
      ZTEST = representingZTest,
      LOGITTEST = representingLogitTest,
      KSTEST = representingKSTest,
      HERITAGE = representingHeritage
    )
  ]

  # target_mod_data <- data_patients
  # Step 1: Select and filter relevant rows
  selected_heritage <- data_patients[
    CONCEPT_ID %in% selected_concept_ids & ABSTRACTION_LEVEL == abstraction_level,
    unique(HERITAGE)
  ]

  # Step 2: Find the most frequent heritage
  most_frequent_heritage <- names(sort(table(selected_heritage), decreasing = TRUE)[1])

  # Step 3: Identify rows to update
  rows_to_update <- data_patients[
    , CONCEPT_ID %in% selected_concept_ids & ABSTRACTION_LEVEL == abstraction_level
  ]

  # Step 4: Update rows and group by relevant columns, then summarize
  data_patients[
    rows_to_update,
    `:=`(
      CONCEPT_ID = representingConceptId,
      CONCEPT_NAME = new_concept_name,
      HERITAGE = most_frequent_heritage
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
  concept_date_array_min = min(concept_date_array) # 0
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

  # Tests end

  # Update complementaryMappingTable
  new_rows <- data.frame(CONCEPT_ID = selected_concept_ids, CONCEPT_NAME = new_concept_name, ABSTRACTION_LEVEL = abstraction_level, stringsAsFactors = FALSE)
  complementary_mapping <- rbind(complementary_mapping, new_rows)
  # Update all related concept names in complementaryMappingTable
  # Step 1: Identify related concepts
  related_concepts <- complementary_mapping[
    complementary_mapping$CONCEPT_ID %in% selected_concept_ids & complementary_mapping$ABSTRACTION_LEVEL == abstraction_level,
    "CONCEPT_NAME"
  ]
  # Step 2: Update CONCEPT_NAME for related concepts
  complementary_mapping = data.table::as.data.table(complementary_mapping)
  complementary_mapping[
    complementary_mapping$CONCEPT_NAME %in% related_concepts & complementary_mapping$ABSTRACTION_LEVEL == abstraction_level,
    CONCEPT_NAME := new_concept_name
  ]
  # Step 3: Remove duplicates
  complementary_mapping <- unique(complementary_mapping)

  data$data_initial = data_initial
  data$data_patients = data_patients
  data$data_features = data_features
  data$complementaryMappingTable = complementary_mapping

  return(data)
}
