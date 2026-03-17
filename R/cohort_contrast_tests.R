# Statistical testing internals for the CohortContrast workflow
#' This function performs CHI2Y tests (chi-squared test for two proportions with
#' Yates continuity correction) on patient prevalence data (target vs control).
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
  printCustomMessage("Running CHI2Y tests ...")
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
  significant_concepts <- data.frame(CONCEPT_ID = integer(), CHI2Y = logical(), CHI2Y_P_VALUE = double(), ABSTRACTION_LEVEL = integer())
  # Setup cores
  cl <- parallel::makeCluster(numCores)
  doParallel::registerDoParallel(cl)
  abstraction_level = NULL
  # Parallelize the loop over abstraction levels
  significant_concepts <- foreach::foreach(abstraction_level = unique(agg_data$ABSTRACTION_LEVEL),
                                           .combine = rbind, .packages = c("dplyr"),.export = c("abstraction_level")) %dopar% {
    agg_data_abstraction_subset <- dplyr::filter(agg_data, .data$ABSTRACTION_LEVEL == abstraction_level)
    alpha <- 0.05 / length(unique(agg_data_abstraction_subset$CONCEPT_ID))

    level_results <- data.frame(CONCEPT_ID = integer(), CHI2Y = logical(), CHI2Y_P_VALUE = double(), ABSTRACTION_LEVEL = integer())

    # Perform statistical test for each CONCEPT_ID within the abstraction level
    for (concept_id in unique(agg_data_abstraction_subset$CONCEPT_ID)) {
      prevalence_cohort_1 <- dplyr::filter(cohort_1, .data$CONCEPT_ID == concept_id, .data$ABSTRACTION_LEVEL == abstraction_level)$TOTAL_PREVALENCE
      prevalence_cohort_2 <- dplyr::filter(cohort_2, .data$CONCEPT_ID == concept_id, .data$ABSTRACTION_LEVEL == abstraction_level)$TOTAL_PREVALENCE

      # Correction
      prevalence_cohort_1 = ifelse(length(prevalence_cohort_1) == 0, 0, prevalence_cohort_1)
      prevalence_cohort_2 = ifelse(length(prevalence_cohort_2) == 0, 0, prevalence_cohort_2)

      test_result <- stats::prop.test(
          c(prevalence_cohort_1, prevalence_cohort_2),
          c(sample_1_n, sample_2_n),
          conf.level = 0.95,
          correct = TRUE
        )

      if (is.na(test_result$p.value)) {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              CHI2Y = FALSE,
              CHI2Y_P_VALUE = 1,
              ABSTRACTION_LEVEL = abstraction_level
            )
          )
        } else if (test_result$p.value < alpha) {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              CHI2Y = TRUE,
              CHI2Y_P_VALUE = test_result$p.value,
              ABSTRACTION_LEVEL = abstraction_level
            )
          )
        } else {
          level_results <- rbind(
            level_results,
            data.frame(
              CONCEPT_ID = concept_id,
              CHI2Y = FALSE,
              CHI2Y_P_VALUE = test_result$p.value,
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
      no_match_target_df = data.frame(COHORT_DEFINITION_ID = "target", PREVALENCE = 0, TARGET = rep(1, times = no_match_target), CONTROL = 0)
      }
      if(no_match_control != 0){
      no_match_control_df = data.frame(COHORT_DEFINITION_ID = "control", PREVALENCE = 0, TARGET = 0, CONTROL = rep(1, times = no_match_control))
      }
      concept_data = rbind(rbind(no_match_target_df, no_match_control_df), concept_data)

      prevalence_cohort_2 <- ifelse(is.na(sum(concept_data$PREVALENCE[concept_data$TARGET == 1])), 0, sum(concept_data$PREVALENCE[concept_data$TARGET == 1]))
      prevalence_cohort_1 <- ifelse(is.na(sum(concept_data$PREVALENCE[concept_data$CONTROL == 1])), 0, sum(concept_data$PREVALENCE[concept_data$CONTROL == 1]))

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

      if (!is.na(p_value) & p_value < alpha) {
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
      else {
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

#' Function for handling tests
#' @param data Data list object
#' @param targetCohortId Target cohort id
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param runChi2YTests Boolean for running the CHI2Y test (chi-squared test
#'   for two proportions with Yates continuity correction).
#' @param runLogitTests boolean for logit-tests
#'
#' @keywords internal

handleTests <-
  function(data,
           targetCohortId,
           presenceFilter,
           runChi2YTests = TRUE,
           runLogitTests = TRUE,
           numCores = parallel::detectCores() - 1) {
    data_features = data$data_features
    data_patients = data$data_patients
    data_initial = data$data_initial

    if (runChi2YTests) {
      significant_concepts <-
        performPrevalenceAnalysis(data_patients,
                                  data_initial,
                                  targetCohortId,
                                  presenceFilter,
                                  numCores = numCores)
        data_features <- data_features %>%
        dplyr::left_join(significant_concepts,by = c("ABSTRACTION_LEVEL", "CONCEPT_ID"))
      printCustomMessage("CHI2Y test on data executed!")
    }
    else {
      data_features <- data_features %>%
        dplyr::mutate(CHI2Y = FALSE, CHI2Y_P_VALUE = 1)
      printCustomMessage("CHI2Y tests were disabled!")
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

    # Overwrite data_features to apply tests
    data$data_features = data_features
    return(data)
  }
