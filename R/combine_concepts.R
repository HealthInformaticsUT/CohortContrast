#' @keywords internal
combineSelectedConcepts <- function(newConceptName, newConceptId = NULL, selectedIds = NULL, abstractionLevel = -1, data = NULL, type = "custom") {
    if(is.null(data)) return(NULL)
    assertPackagesAvailable("reshape2", "concept aggregation in combineSelectedConcepts()")

    # TODO: make tests modular and apply
    data_features <- data.table::as.data.table(data$data_features)
    data_patients <- data.table::as.data.table(data$data_patients)
    data_initial <- data.table::as.data.table(data$data_initial)
    data_person <- data.table::as.data.table(data$data_person)
    complementary_mapping <- data.table::as.data.table(data$complementaryMappingTable)
    # Assuming data_initial, data_features, and processed_table are data.tables

    # Step 1: Grouping and summarizing, followed by reshaping
    n_patients_temp <- data_initial[, .N, by = COHORT_DEFINITION_ID]
    n_patients <- reshape2::dcast(n_patients_temp, formula = 1 ~ COHORT_DEFINITION_ID, value.var = "N", fill = 0)
    # Step 2: Extracting count values
    count_target <- n_patients$target
    count_control <- n_patients$control

    processed_table <- data_features[ABSTRACTION_LEVEL == abstractionLevel]

    selected_concept_ids = NA
    concept_ids_to_merge = NA
    selected_concept_names = NA
    representingConceptId = newConceptId
    representingHeritage = NA
    representingChi2Y = NA
    representingLogitTest = NA

    # Step 3: Filtering processed_table and selecting concept IDs
    selected_concept_ids <- unique(as.numeric(selectedIds))
    concept_ids_to_merge <- unique(c(selected_concept_ids, representingConceptId))
    selected_concept_names <- processed_table[
      match(selected_concept_ids, CONCEPT_ID),
      CONCEPT_NAME
    ]

    # check if newConceptName can be used
    used_concept_names <- unique(
      data_patients %>%
        dplyr::filter(!(.data$CONCEPT_ID %in% concept_ids_to_merge)) %>%
        dplyr::pull(CONCEPT_NAME)
    )
    counter = 2
    maybe_new_concept_name = newConceptName
    while(maybe_new_concept_name %in% used_concept_names){
      maybe_new_concept_name = paste(newConceptName, counter, sep = " ")
      counter = counter + 1
      cli::cli_alert_warning(paste0("Mapped '", gsub("\\{", "{{", gsub("\\}", "}}", newConceptName)), "' to '",  gsub("\\{", "{{", gsub("\\}", "}}", maybe_new_concept_name)), "' because of duplicate concept names for differing ids!"))
    }
    newConceptName = maybe_new_concept_name

    if(is.null(representingConceptId)){
      representingConceptId <- selected_concept_ids[
        which.max(as.numeric(processed_table[CONCEPT_ID %in% selected_concept_ids, PREVALENCE_DIFFERENCE_RATIO]))
      ]
      concept_ids_to_merge <- unique(c(selected_concept_ids, representingConceptId))
    }
    representingHeritage <- processed_table[CONCEPT_ID %in% concept_ids_to_merge, HERITAGE][
      which.max(as.numeric(processed_table[CONCEPT_ID %in% concept_ids_to_merge, PREVALENCE_DIFFERENCE_RATIO]))]

    # Step 4: Determining CHI2Y and LOGITTEST values
    representingChi2Y <- any(processed_table[CONCEPT_ID %in% concept_ids_to_merge, CHI2Y])
    representingLogitTest <- any(processed_table[CONCEPT_ID %in% concept_ids_to_merge, LOGITTEST])

    new_row <- data.table::data.table(
      CONCEPT_ID = representingConceptId,
      CONCEPT_NAME = newConceptName,
      ABSTRACTION_LEVEL = abstractionLevel,
      CHI2Y = representingChi2Y,
      LOGITTEST = representingLogitTest,
      HERITAGE = representingHeritage
    )

    # Remove row if there was for representingConceptId
    data_features <- data_features[!(CONCEPT_ID %in% concept_ids_to_merge & ABSTRACTION_LEVEL == abstractionLevel)]

    data_features <- rbind(data_features, new_row, fill = TRUE)
    rows_to_update <- data_patients[
      , CONCEPT_ID %in% concept_ids_to_merge & ABSTRACTION_LEVEL == abstractionLevel
    ]

    # Step 4: Update rows and group by relevant columns, then summarize
    data_patients[
      rows_to_update,
      `:=`(
        CONCEPT_ID = representingConceptId,
        CONCEPT_NAME = newConceptName,
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
      , .(CONCEPT_ID, ABSTRACTION_LEVEL, CHI2Y,CHI2Y_P_VALUE, LOGITTEST,LOGITTEST_P_VALUE, HERITAGE)
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

    # Step 5: Determining CHI2Y and LOGITTEST values
    total_concepts = data_features %>% dplyr::filter(.data$ABSTRACTION_LEVEL == abstractionLevel) %>% nrow()
    # CHI2Y
    concept_row <- data_features[CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstractionLevel, .(TARGET_SUBJECT_COUNT, CONTROL_SUBJECT_COUNT)]
    target_subject_count <- concept_row$TARGET_SUBJECT_COUNT
    control_subject_count <- concept_row$CONTROL_SUBJECT_COUNT

    representingChi2Y = FALSE
    representingChi2YPValue = 1

    chi2y_result <- stats::prop.test(
      c(target_subject_count, control_subject_count),
      c(count_target, count_control),
      conf.level = 0.95,
      correct = TRUE
    )
    if (!(is.na(chi2y_result$p.value))) {
      representingChi2YPValue = chi2y_result$p.value
      representingChi2Y = if(representingChi2YPValue < 0.05/total_concepts) TRUE else FALSE
    }

    # Logit Test
    representingLogitTest = FALSE
    representingLogitTestPValue = 1
    # Create the dataset for logistic regression
    concept_data <- data_patients %>%
      dplyr::filter(.data$ABSTRACTION_LEVEL == abstractionLevel, .data$CONCEPT_ID == representingConceptId) %>%
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
    # Update a specific row with new values for example columns
    data_features[CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstractionLevel, `:=`(
      CHI2Y = representingChi2Y,
      CHI2Y_P_VALUE = representingChi2YPValue,
      LOGITTEST = representingLogitTest,
      LOGITTEST_P_VALUE = representingLogitTestPValue,
      HERITAGE = representingHeritage
    )]

    # Update complementaryMappingTable
    new_rows <- data.frame(CONCEPT_ID = selected_concept_ids, CONCEPT_NAME = selected_concept_names, NEW_CONCEPT_ID = representingConceptId, NEW_CONCEPT_NAME = newConceptName, ABSTRACTION_LEVEL = abstractionLevel, TYPE = type, stringsAsFactors = FALSE)
    complementary_mapping <- rbind(complementary_mapping, new_rows)

    # Step 1: Identify related concepts
    related_concepts <- complementary_mapping[
      complementary_mapping$CONCEPT_ID %in% selected_concept_ids & complementary_mapping$ABSTRACTION_LEVEL == abstractionLevel,
      "NEW_CONCEPT_NAME"
    ]

    # Step 2: Update CONCEPT_NAME for related concepts
    complementary_mapping = data.table::as.data.table(complementary_mapping)
    complementary_mapping[
      complementary_mapping$NEW_CONCEPT_NAME %in% related_concepts & complementary_mapping$ABSTRACTION_LEVEL == abstractionLevel,
      NEW_CONCEPT_NAME := newConceptName
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
#' @param abstractionLevel abstraction level to use for mapping
#' @param minDepthAllowed integer for restricting the mapping, if a concept is part of a hierarchy tree then minDepthAllowed value will prune the tree from said depth value upwards
#' @param allowOnlyMinors allows only mappping if child has smaller prevalence than parent
#' @return A CohortContrastObject with hierarchy-based concept merges applied.
#'   The returned object keeps the same overall structure as the input, while
#'   updating the patient-, feature-, and cohort-level tables together with the
#'   complementary mapping table to reflect the executed hierarchy mappings.
#' @examples
#' study <- structure(
#'   list(
#'     data_initial = data.frame(
#'       COHORT_DEFINITION_ID = c("target", "target", "control", "control"),
#'       SUBJECT_ID = 1:4,
#'       COHORT_START_DATE = as.Date(rep("2020-01-01", 4)),
#'       COHORT_END_DATE = as.Date(rep("2020-01-10", 4))
#'     ),
#'     data_patients = data.frame(
#'       COHORT_DEFINITION_ID = c(
#'         "target", "target", "target", "target", "target", "target",
#'         "control", "control", "control", "control", "control", "control"
#'       ),
#'       PERSON_ID = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
#'       CONCEPT_ID = c(1, 2, 10, 1, 2, 10, 1, 2, 10, 1, 2, 10),
#'       CONCEPT_NAME = c(
#'         "Concept A", "Concept B", "Parent concept",
#'         "Concept A", "Concept B", "Parent concept",
#'         "Concept A", "Concept B", "Parent concept",
#'         "Concept A", "Concept B", "Parent concept"
#'       ),
#'       HERITAGE = rep("drug_exposure", 12),
#'       ABSTRACTION_LEVEL = rep(-1, 12),
#'       PREVALENCE = rep(1, 12),
#'       TIME_TO_EVENT = I(rep(list(c(0, 2)), 12))
#'     ),
#'     data_features = data.frame(
#'       CONCEPT_ID = c(1, 2, 10),
#'       CONCEPT_NAME = c("Concept A", "Concept B", "Parent concept"),
#'       ABSTRACTION_LEVEL = c(-1, -1, -1),
#'       TARGET_SUBJECT_COUNT = c(2, 2, 2),
#'       CONTROL_SUBJECT_COUNT = c(2, 2, 2),
#'       TIME_TO_EVENT = I(list(c(0, 2), c(0, 2), c(0, 2))),
#'       TARGET_SUBJECT_PREVALENCE = c(1, 1, 1),
#'       CONTROL_SUBJECT_PREVALENCE = c(1, 1, 1),
#'       PREVALENCE_DIFFERENCE_RATIO = c(1, 1, 1),
#'       CHI2Y = c(TRUE, TRUE, TRUE),
#'       CHI2Y_P_VALUE = c(1, 1, 1),
#'       LOGITTEST = c(FALSE, FALSE, FALSE),
#'       LOGITTEST_P_VALUE = c(1, 1, 1),
#'       HERITAGE = c("drug_exposure", "drug_exposure", "drug_exposure")
#'     ),
#'     data_person = data.frame(),
#'     conceptsData = list(
#'       concept = data.frame(
#'         concept_id = c(1, 2, 10),
#'         concept_name = c("Concept A", "Concept B", "Parent concept"),
#'         invalid_reason = c(NA, NA, NA)
#'       ),
#'       concept_ancestor = data.frame(
#'         ancestor_concept_id = c(1, 2, 10, 10, 10),
#'         descendant_concept_id = c(1, 2, 10, 1, 2),
#'         min_levels_of_separation = c(0, 0, 0, 1, 1),
#'         max_levels_of_separation = c(0, 0, 0, 1, 1)
#'       )
#'     ),
#'     complementaryMappingTable = data.frame(
#'       CONCEPT_ID = integer(),
#'       CONCEPT_NAME = character(),
#'       NEW_CONCEPT_ID = integer(),
#'       NEW_CONCEPT_NAME = character(),
#'       ABSTRACTION_LEVEL = integer(),
#'       TYPE = character()
#'     )
#'   ),
#'   class = "CohortContrastObject"
#' )
#'
#' combined <- suppressWarnings(
#'   automaticHierarchyCombineConcepts(study, abstractionLevel = -1)
#' )
#' combined$data_features
#' combined$complementaryMappingTable
#' @export
automaticHierarchyCombineConcepts <- function(data, abstractionLevel = -1, minDepthAllowed = 0, allowOnlyMinors = FALSE) {

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

    data_features = data$data_features %>% dplyr::filter(ABSTRACTION_LEVEL == abstractionLevel,
                                                         CONCEPT_ID %in% concept_ancestor_allowed)
    counter = counter + 1

    mappingTable <- getAncestorMappings(
      active_concept_ids = data_features %>% dplyr::pull(CONCEPT_ID) %>% intersect(concept_ancestor_allowed),
      concept_table = concept_table,
      concept_ancestor = concept_ancestor
    )

    if (allowOnlyMinors) {
      prevalence_lookup <- data_features %>%
        dplyr::select(CONCEPT_ID, TARGET_SUBJECT_PREVALENCE)

      mappingtable_filtered <- mappingTable %>%
        tidyr::unnest_longer(CONCEPT_IDS, indices_include = TRUE) %>%
        dplyr::rename(child_id = CONCEPT_IDS, idx = CONCEPT_IDS_id) %>%
        dplyr::mutate(child_name = purrr::map2_chr(CONCEPT_NAMES, idx, ~ .x[.y])) %>%

        # Add prevalence info
        dplyr::left_join(prevalence_lookup, by = c("PARENT_ID" = "CONCEPT_ID")) %>%
        dplyr::rename(parent_prevalence = TARGET_SUBJECT_PREVALENCE) %>%
        dplyr::left_join(prevalence_lookup, by = c("child_id" = "CONCEPT_ID")) %>%
        dplyr::rename(child_prevalence = TARGET_SUBJECT_PREVALENCE) %>%

        # Filter rows where child prevalence is not greater than parent
        dplyr::filter(
          is.na(child_prevalence) |
            is.na(parent_prevalence) |
            child_prevalence <= parent_prevalence
        ) %>%

        # Group back to original list structure
        dplyr::group_by(PARENT_ID, PARENT_NAME) %>%
        dplyr::summarise(
          CONCEPT_IDS = list(child_id),
          CONCEPT_NAMES = list(child_name),
          .groups = "drop"
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(COUNT = length(CONCEPT_IDS)) %>%
        dplyr::filter(COUNT > 1) %>%
        dplyr::select(PARENT_ID, PARENT_NAME, COUNT, CONCEPT_IDS, CONCEPT_NAMES)

      mappingTable <- as.data.frame(mappingtable_filtered)
    }

    mappingsToExecute <- filterHeritagePriorityMappings(mappingTable)

    if(nrow(mappingsToExecute) == 0){
      break
    }

    # Explicit for loop replaces apply
    for (i in seq_len(nrow(mappingsToExecute))) {
      row <- mappingsToExecute[i, ]
      selected_parent_id <- as.numeric(row[['PARENT_ID']])
      selected_concept_ids <- as.numeric(unlist(row[['CONCEPT_IDS']]))
      selected_concept_ids <- selected_concept_ids[selected_concept_ids != selected_parent_id]

      if (length(selected_concept_ids) == 0) {
        next
      }

      if (!any(is.na(selected_concept_ids))) {
        selected_parent_name <- as.character(row[['PARENT_NAME']])

        data <- combineSelectedConcepts(
          newConceptName = selected_parent_name,
          newConceptId = selected_parent_id,
          selectedIds = selected_concept_ids,
          abstractionLevel = abstractionLevel,
          data = data,
          type = "hierarchy"
        )
      }
    }
  }


  return(data)
}


#' Function for automatically combining concepts by hierarchy mapping
#'
#' @param data CohortContrastObject
#' @param abstractionLevel abstraction level to use for mapping
#' @param minCorrelation minimum correlation to use for automatic concept combining
#' @param maxDaysInBetween minimum days between concepts to use for automatic concept combining
#' @param heritageDriftAllowed boolean for allowing heritage drift (combining concepts from differing heritages)
#' @return A CohortContrastObject with correlation-based concept merges applied.
#'   The returned object keeps the same overall structure as the input, while
#'   updating the patient-, feature-, and cohort-level tables together with the
#'   complementary mapping table to reflect the executed correlation mappings.
#' @examples
#' study <- structure(
#'   list(
#'     data_initial = data.frame(
#'       COHORT_DEFINITION_ID = c(rep("target", 4), rep("control", 4)),
#'       SUBJECT_ID = 1:8,
#'       COHORT_START_DATE = as.Date(rep("2020-01-01", 8)),
#'       COHORT_END_DATE = as.Date(rep("2020-01-10", 8))
#'     ),
#'     data_patients = data.frame(
#'       COHORT_DEFINITION_ID = c(
#'         "target", "target", "target", "target", "target", "target",
#'         "control", "control", "control"
#'       ),
#'       PERSON_ID = c(1, 1, 2, 2, 3, 4, 5, 6, 7),
#'       CONCEPT_ID = c(1, 2, 1, 2, 1, 2, 1, 2, 1),
#'       CONCEPT_NAME = c(
#'         "Concept A", "Concept B", "Concept A", "Concept B", "Concept A",
#'         "Concept B", "Concept A", "Concept B", "Concept A"
#'       ),
#'       HERITAGE = rep("drug_exposure", 9),
#'       ABSTRACTION_LEVEL = rep(-1, 9),
#'       PREVALENCE = rep(1, 9),
#'       TIME_TO_EVENT = I(list(0, 1, 0, 1, 0, 1, 0, 1, 0))
#'     ),
#'     data_features = data.frame(
#'       CONCEPT_ID = c(1, 2),
#'       CONCEPT_NAME = c("Concept A", "Concept B"),
#'       ABSTRACTION_LEVEL = c(-1, -1),
#'       TARGET_SUBJECT_COUNT = c(3, 3),
#'       CONTROL_SUBJECT_COUNT = c(2, 1),
#'       TIME_TO_EVENT = I(list(c(0, 0, 0), c(1, 1, 1))),
#'       TARGET_SUBJECT_PREVALENCE = c(0.75, 0.75),
#'       CONTROL_SUBJECT_PREVALENCE = c(0.5, 0.25),
#'       PREVALENCE_DIFFERENCE_RATIO = c(1.5, 3),
#'       CHI2Y = c(TRUE, TRUE),
#'       CHI2Y_P_VALUE = c(0.1, 0.01),
#'       LOGITTEST = c(FALSE, FALSE),
#'       LOGITTEST_P_VALUE = c(1, 1),
#'       HERITAGE = c("drug_exposure", "drug_exposure")
#'     ),
#'     data_person = data.frame(
#'       PERSON_ID = 1:8,
#'       YEAR_OF_BIRTH = 1980:1987,
#'       GENDER_CONCEPT_ID = c(8507, 8532, 8507, 8532, 8507, 8532, 8507, 8532)
#'     ),
#'     complementaryMappingTable = data.frame(
#'       CONCEPT_ID = integer(),
#'       CONCEPT_NAME = character(),
#'       NEW_CONCEPT_ID = integer(),
#'       NEW_CONCEPT_NAME = character(),
#'       ABSTRACTION_LEVEL = integer(),
#'       TYPE = character()
#'     ),
#'     config = list(
#'       runChi2YTests = TRUE,
#'       runLogitTests = FALSE,
#'       presenceFilter = 0,
#'       prevalenceCutOff = 0
#'     )
#'   ),
#'   class = "CohortContrastObject"
#' )
#'
#' combined <- automaticCorrelationCombineConcepts(
#'   study,
#'   abstractionLevel = -1,
#'   minCorrelation = 0.5,
#'   maxDaysInBetween = 2
#' )
#' combined$data_features
#' combined$complementaryMappingTable
#' @export
automaticCorrelationCombineConcepts <- function(data,
                                                abstractionLevel = -1,
                                                minCorrelation = 0.7,
                                                maxDaysInBetween = 1,
                                                heritageDriftAllowed = FALSE) {

  counter <- 1

  while (TRUE) {
    cli::cli_alert_warning(paste0("Automatic correlation mapping iteration ", counter))

    run_chi2y <- data$config$runChi2YTests

    target_filtered = format_results(data = data,
                                                      autoScaleRate = FALSE,
                                                      applyInverseTarget = FALSE,
                                                      applyChi2YTest = run_chi2y,
                                                      applyLogitTest = data$config$runLogitTests,
                                                      abstractionLevel = abstractionLevel)
    filter_target = filter_target(target = target_filtered, prevalence_threshold = data$config$presenceFilter,
                                                   prevalence_ratio_threshold = data$config$prevalenceCutOff,removeUntreated = FALSE)

    filter_target = prepareFilteredTarget(filtered_target = filter_target, correlation_threshold = 0)

    result_corr <- computePairwiseCorrelations(filter_target$correlation_analysis$ordered_matrix,filter_target$target_row_annotation)
    result_time <- calculateMedianTransitions(filter_target$target_time_annotation)
    correlationSuggestionsTable <- mergeCorrelationWithTransitions(correlation_data = result_corr, transition_data = result_time)


    data_features = data$data_features %>% dplyr::filter(ABSTRACTION_LEVEL == abstractionLevel)
    counter = counter + 1

    mappingsToExecute <- filterCorrelationMappings(
      correlationSuggestionsTable,
      data_features = data_features,
      minCorrelation = minCorrelation,
      maxDaysInBetween = maxDaysInBetween,
      heritageDriftAllowed = heritageDriftAllowed
    )

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
        selected_parent_name <- paste(as.character(unlist(row[['CONCEPT_NAMES']])), collapse = " + ")

        data <- combineSelectedConcepts(
          newConceptName = selected_parent_name,
          newConceptId = selected_parent_id,
          selectedIds = selected_concept_ids,
          abstractionLevel = abstractionLevel,
          data = data,
          type = "correlation"
        )
      }
    }
  }

  return(data)
}
