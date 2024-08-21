# Cohort creation

#' Create relations of cohorts (target and control) to the database
#'
#' @param cdm CDMConnector object: connection to the database
#' @param pathToResults Path to the mandatory subdirectories such as 'inst'
#' @param studyName Name of the study being run, will be used in file and relation names
#' @param domainsIncluded list of CDM domains to include
#' @param readFromCSV boolean > if TRUE file from ./inst/CSV will be used as cohort data, otherwise JSONs will be used.
#' @param nudgeTarget number of days you would like to nudge the target cohort start day
#' @param nudgeControl number of days you would like to nudge the control cohort start day
#' @param useInverseControls Boolean for using inverse controls (target cohort's observation period which is not included)
#' @param useTargetMatching Boolean for using patient matching for controls
#'
#' @keywords internal
generateTables <- function(cdm,
                           pathToResults = getwd(),
                           studyName = "SampleStudy",
                           domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure", "Visit", "Visit detail"),
                           readFromCSV = FALSE,
                           nudgeTarget = FALSE,
                           nudgeControl = FALSE,
                           useInverseControls = FALSE,
                           useTargetMatching = FALSE
                           ) {
    # loading settings
    settings <- loadJsons(studyName, pathToResults)
    if (readFromCSV) {
      # Read the CSV file and return inverseControl value
      cdm = processCSVData(cdm, useInverseControls, useTargetMatching, insertedCSVTarget = settings$targetCohortDataframe, insertedCSVControl = settings$controlCohortDataframe, nudgeTarget, nudgeControl)
      printCustomMessage("Cohort generation from CSV files was successful")
    }
    else {
      cdm = processJSONData(cdm, useInverseControls, useTargetMatching, insertedJSONTarget = settings$targetCohortSet, insertedJSONControl = settings$controlCohortSet, nudgeTarget, nudgeControl)
      printCustomMessage("Cohort generation from JSON files was successful")
    }
    # Combine the target and control tables
    cdm$target <- cdm$target %>% dplyr::mutate(cohort_definition_id = as.integer(2))
    cdm$control <- cdm$control %>% dplyr::mutate(cohort_definition_id = as.integer(1))
    cohortcontrast_cohorts <- dplyr::bind_rows(cdm$target %>% as.data.frame(), cdm$control %>% as.data.frame())

    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = "cohortcontrast_cohorts",
                                     table = cohortcontrast_cohorts,
                                     overwrite = T)

    cohort_size <- cdm$cohortcontrast_cohorts %>%
      dplyr::group_by(cohort_definition_id) %>%
      dplyr::summarize(total_count = dplyr::n_distinct(subject_id), .groups = 'drop')

    # TODO: remove dead code
    ####################################

    # Drug Exposure Patient Prevalence
    patient_drug_prevalence_table <- cdm$cohortcontrast_cohorts %>%
      dplyr::inner_join(cdm$drug_exposure, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(
        drug_exposure_start_date >= cohort_start_date &
          drug_exposure_start_date <= cohort_end_date
      ) %>%
      dplyr::select(cohort_definition_id,
             subject_id,
             drug_concept_id,
             drug_exposure_id) %>%
      dplyr::group_by(cohort_definition_id, subject_id, drug_concept_id) %>%
      dplyr::summarize(prevalence = dplyr::n_distinct(drug_exposure_id),
                .groups = 'drop') %>%
      dplyr::select(cohort_definition_id, subject_id, drug_concept_id, prevalence) %>% dplyr::left_join(cdm$concept, by = c("drug_concept_id" = "concept_id")) %>%
      dplyr::select(cohort_definition_id, subject_id, concept_id = drug_concept_id, concept_name, prevalence) %>% dplyr::mutate(heritage = 'drug_exposure')

    # Condition Occurrence Patient Prevalence
    patient_condition_prevalence_table <- cdm$cohortcontrast_cohorts %>%
      dplyr::inner_join(cdm$condition_occurrence, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(
        condition_start_date >= cohort_start_date &
          condition_start_date <= cohort_end_date
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, condition_concept_id, condition_occurrence_id) %>%
      dplyr::group_by(cohort_definition_id, subject_id, condition_concept_id) %>%
      dplyr::summarize(
        prevalence = dplyr::n_distinct(condition_occurrence_id),
        .groups = 'drop'
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, condition_concept_id, prevalence)  %>% dplyr::left_join(cdm$concept, by = c("condition_concept_id" = "concept_id")) %>%
      dplyr::select(cohort_definition_id, subject_id, concept_id = condition_concept_id, concept_name, prevalence) %>% dplyr::mutate(heritage = 'condition_occurrence')

    # Measurement Patient Prevalence
    patient_measurement_prevalence_table <- cdm$cohortcontrast_cohorts %>%
      dplyr::inner_join(cdm$measurement, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(
        measurement_date >= cohort_start_date &
          measurement_date <= cohort_end_date
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, measurement_concept_id, measurement_id) %>%
      dplyr::group_by(cohort_definition_id, subject_id, measurement_concept_id) %>%
      dplyr::summarize(
        prevalence = dplyr::n_distinct(measurement_id),
        .groups = 'drop'
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, measurement_concept_id, prevalence) %>% dplyr::left_join(cdm$concept, by = c("measurement_concept_id" = "concept_id")) %>%
      dplyr::select(cohort_definition_id, subject_id, concept_id = measurement_concept_id, concept_name, prevalence) %>% dplyr::mutate(heritage = 'measurement')

    # Procedure Occurrence Patient Prevalence
    patient_procedure_prevalence_table <- cdm$cohortcontrast_cohorts %>%
      dplyr::inner_join(cdm$procedure_occurrence, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(
        procedure_date >= cohort_start_date &
          procedure_date <= cohort_end_date
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, procedure_concept_id, procedure_occurrence_id) %>%
      dplyr::group_by(cohort_definition_id, subject_id, procedure_concept_id) %>%
      dplyr::summarize(
        prevalence = dplyr::n_distinct(procedure_occurrence_id),
        .groups = 'drop'
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, procedure_concept_id, prevalence) %>% dplyr::left_join(cdm$concept, by = c("procedure_concept_id" = "concept_id")) %>%
      dplyr::select(cohort_definition_id, subject_id, concept_id = procedure_concept_id, concept_name, prevalence) %>% dplyr::mutate(heritage = 'procedure_occurrence')

    # Observation Patient Prevalence
    patient_observation_prevalence_table <- cdm$cohortcontrast_cohorts %>%
      dplyr::inner_join(cdm$observation, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(
        observation_date >= cohort_start_date &
          observation_date <= cohort_end_date
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, observation_concept_id, observation_id) %>%
      dplyr::group_by(cohort_definition_id, subject_id, observation_concept_id) %>%
      dplyr::summarize(
        prevalence = dplyr::n_distinct(observation_id),
        .groups = 'drop'
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, observation_concept_id, prevalence) %>% dplyr::left_join(cdm$concept, by = c("observation_concept_id" = "concept_id")) %>%
      dplyr::select(cohort_definition_id, subject_id, concept_id = observation_concept_id, concept_name, prevalence) %>% dplyr::mutate(heritage = 'observation')

    # Visit Occurrence Patient Prevalence
    patient_visit_prevalence_table <- cdm$cohortcontrast_cohorts %>%
      dplyr::inner_join(cdm$visit_occurrence, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(
        visit_start_date >= cohort_start_date &
          visit_start_date <= cohort_end_date
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, visit_concept_id, visit_occurrence_id) %>%
      dplyr::group_by(cohort_definition_id, subject_id, visit_concept_id) %>%
      dplyr::summarize(
        prevalence = dplyr::n_distinct(visit_occurrence_id),
        .groups = 'drop'
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, visit_concept_id, prevalence) %>% dplyr::left_join(cdm$concept, by = c("visit_concept_id" = "concept_id")) %>%
      dplyr::select(cohort_definition_id, subject_id, concept_id = visit_concept_id, concept_name, prevalence) %>% dplyr::mutate(heritage = 'visit_occurrence')

    # Visit Detail Patient Prevalence
    patient_visit_detail_prevalence_table <- cdm$cohortcontrast_cohorts %>%
      dplyr::inner_join(cdm$visit_detail, by = c("subject_id" = "person_id")) %>%
      dplyr::filter(
        visit_detail_start_date >= cohort_start_date &
          visit_detail_start_date <= cohort_end_date
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, visit_detail_concept_id, visit_detail_id) %>%
      dplyr::group_by(cohort_definition_id, subject_id, visit_detail_concept_id) %>%
      dplyr::summarize(
        prevalence = dplyr::n_distinct(visit_detail_id),
        .groups = 'drop'
      ) %>%
      dplyr::select(cohort_definition_id, subject_id, visit_detail_concept_id, prevalence) %>% dplyr::left_join(cdm$concept, by = c("visit_detail_concept_id" = "concept_id")) %>%
      dplyr::select(cohort_definition_id, subject_id, concept_id = visit_detail_concept_id, concept_name, prevalence) %>% dplyr::mutate(heritage = 'visit_detail')

  ############################################################################
  #
  # Query data from database
  #
  ############################################################################
    printCustomMessage("Querying patient prevalence data from database ...")
  data_patients <- tibble::tibble(
    cohort_definition_id = integer(),
    subject_id = integer(),
    concept_id = integer(),
    concept_name = character(),
    prevalence = numeric(),
    heritage = character()
  )

  domains <- c("Drug", "Measurement", "Condition", "Observation", "Visit", "Visit detail", "Procedure")
  for (domain in domains) {
    if (domain %in% domainsIncluded) {
      if (domain == "Visit") {
        printCustomMessage("Querying visit occurrence data from database ...")
        data_to_add <- patient_visit_prevalence_table %>%
          dplyr::group_by(cohort_definition_id,subject_id,concept_id,concept_name,heritage) %>%
          dplyr::summarize(prevalence = sum(prevalence), .groups = 'drop') %>% dplyr::filter(concept_id != 0)
          data_patients <- rbind(data_patients, data_to_add %>% as.data.frame())
      }
      else if (domain == "Visit detail") {
        printCustomMessage("Querying visit detail data from database ...")
        data_to_add <- patient_visit_detail_prevalence_table %>%
          dplyr::group_by(cohort_definition_id,subject_id,concept_id,concept_name,heritage) %>%
          dplyr::summarize(prevalence = sum(prevalence), .groups = 'drop') %>% dplyr::filter(concept_id != 0)
        data_patients <- rbind(data_patients, data_to_add %>% as.data.frame())
      }
      else if (domain == "Drug") {
        printCustomMessage("Querying drug exposure data from database ...")
        data_to_add <- patient_drug_prevalence_table %>%
          dplyr::group_by(cohort_definition_id,subject_id,concept_id,concept_name,heritage) %>%
          dplyr::summarize(prevalence = sum(prevalence), .groups = 'drop') %>% dplyr::filter(concept_id != 0)
        data_patients <- rbind(data_patients, data_to_add %>% as.data.frame())
      }
      else if (domain == "Measurement") {
        printCustomMessage("Querying measurement data from database ...")
        data_to_add <- patient_measurement_prevalence_table %>%
          dplyr::group_by(cohort_definition_id,subject_id,concept_id,concept_name,heritage) %>%
          dplyr::summarize(prevalence = sum(prevalence), .groups = 'drop') %>% dplyr::filter(concept_id != 0)
        data_patients <- rbind(data_patients, data_to_add %>% as.data.frame())
      }
      else if (domain == "Procedure") {
        printCustomMessage("Querying procedure occurrence data from database ...")
        data_to_add <- patient_procedure_prevalence_table %>%
          dplyr::group_by(cohort_definition_id,subject_id,concept_id,concept_name,heritage) %>%
          dplyr::summarize(prevalence = sum(prevalence), .groups = 'drop') %>% dplyr::filter(concept_id != 0)
        data_patients <- rbind(data_patients, data_to_add %>% as.data.frame())
      }
      else if (domain == "Observation") {
        printCustomMessage("Querying observation data from database ...")
        data_to_add <- patient_observation_prevalence_table %>%
          dplyr::group_by(cohort_definition_id,subject_id,concept_id,concept_name,heritage) %>%
          dplyr::summarize(prevalence = sum(prevalence), .groups = 'drop') %>% dplyr::filter(concept_id != 0)
        data_patients <- rbind(data_patients, data_to_add %>% as.data.frame())
      }
      else if (domain == "Condition") {
        printCustomMessage("Querying condition occurrence data from database ...")
        data_to_add <- patient_condition_prevalence_table %>%
          dplyr::group_by(cohort_definition_id,subject_id,concept_id,concept_name,heritage) %>%
          dplyr::summarize(prevalence = sum(prevalence), .groups = 'drop') %>% dplyr::filter(concept_id != 0)
        data_patients <- rbind(data_patients, data_to_add %>% as.data.frame())
      }
    }
  }
  printCustomMessage("Querying patient prevalence data from database completed.")

  colnames(data_patients) <- c('COHORT_DEFINITION_ID','PERSON_ID','CONCEPT_ID','CONCEPT_NAME','HERITAGE','PREVALENCE')

  # aggregate rows in case of multiple observation periods in same cohort + remove void concepts


  printCustomMessage("Querying initial data from database ...")

  data_initial <- cdm$cohortcontrast_cohorts %>% as.data.frame()
  colnames(data_initial) <- toupper(colnames(data_initial))


  printCustomMessage("Querying person data from database ...")
  # Get person data

  data_person <-
    cdm$person %>% dplyr::select(person_id, gender_concept_id, year_of_birth) %>% as.data.frame()
  colnames(data_person) <- toupper(colnames(data_person))

  printCustomMessage("Data imported from the database!")
  # Setting names for each list element
  return(
    list(
  #data_features = data_features,
      data_patients = data_patients,
      data_initial = data_initial,
      data_person = data_person,
      cdm = cdm
    )
  )
}

processCSVData <- function(cdm, useInverseControls = FALSE, useTargetMatching = FALSE, insertedCSVTarget, insertedCSVControl, nudgeTarget, nudgeControl) {

  if(is.null(insertedCSVTarget))
  {
    printCustomMessage("Target cohort CSV import unsuccessful! Please check target cohort configuration!")
    return(NULL)
  }
  else{
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = "target",
                                   table = insertedCSVTarget,
                                   overwrite = T)
  cdm$target <- omopgenerics::newCohortTable(cdm$target)
  }
  if (is.null(insertedCSVControl) | useInverseControls | useTargetMatching) {
    if(useInverseControls & useTargetMatching) {
      printCustomMessage('WARNING: Both useInverseControls & useTargetMatching are marked as TRUE. Using useInverseControls as default!')
    }
    if(useInverseControls){
      printCustomMessage("Inverse control cohort creation initiated ...")
      cdm <- createControlCohortInverse(cdm)
    }
    else {
      printCustomMessage("Control cohort CSV import unsuccessful! Automatic control cohort creation initiated ...")
      cdm <- createControlCohortMatching(cdm)
    }
  }
  else {
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = "control",
                                     table = insertedCSVControl,
                                     overwrite = T)
    cdm$control <- omopgenerics::newCohortTable(cdm$control)
  }

  nudgeCohorts(cdm, nudgeTarget, nudgeControl)

  printCustomMessage("Cohort import from CSV successful!")

  return(cdm)
}

# Function to nudge cohorts
nudgeCohorts <- function(cdm, nudgeTarget, nudgeControl) {
  if (nudgeTarget != FALSE) {
    cdm$target <-  cdm$target |>
      dplyr::mutate(cohort_start_date + nudgeTarget)
    printCustomMessage("Nudging target cohort EXECUTED!")
  }
  if (nudgeControl != FALSE) {
    cdm$control <-  cdm$control |>
      dplyr::mutate(cohort_start_date + nudgeTarget)
    printCustomMessage("Nudging control cohort EXECUTED!")
  }
}

# Main function
processJSONData <- function(cdm, useInverseControls = FALSE, useTargetMatching = FALSE, insertedJSONTarget, insertedJSONControl, nudgeTarget, nudgeControl) {
  if(is.null(insertedJSONTarget))
  {
    printCustomMessage("Target cohort JSON import unsuccessful! Please check target cohort configuration!")
    return(NULL)
  }
  else{
    printCustomMessage("Generating target cohort from JSON ...")
    cdm = CDMConnector::generateCohortSet(
      cdm,
      cohortSet = insertedJSONTarget,
      name = "target",
      computeAttrition = TRUE,
      overwrite = TRUE
    )
    #cdm$target <- omopgenerics::newCohortTable(cdm$target)
  }
  if (is.null(insertedJSONControl) | useInverseControls | useTargetMatching) {
    if(useInverseControls & useTargetMatching) {
      printCustomMessage('WARNING: Both useInverseControls & useTargetMatching are marked as TRUE. Using useInverseControls as default!')
    }
    if(useInverseControls){
      printCustomMessage("Inverse control cohort creation initiated ...")
      cdm <- createControlCohortInverse(cdm)
    }
    else {
      printCustomMessage("Control cohort JSON import unsuccessful! Automatic control cohort creation initiated ...")
      cdm <- createControlCohortMatching(cdm)
    }
  }
  else {
    printCustomMessage("Generating control cohort from JSON ...")
    cdm = CDMConnector::generateCohortSet(
      cdm,
      cohortSet = insertedJSONControl,
      name = "control",
      computeAttrition = TRUE,
      overwrite = TRUE
    )
  }
  nudgeCohorts(cdm, nudgeTarget, nudgeControl)

  return(cdm)
}

#' Function for creating automatic matches based on age and sex
#'
#' @param cdm Connection to the database (package CDMConnector)
#' @param ratio ratio for the number of matches generated
#' @keywords internal
createControlCohortMatching <- function(cdm, ratio = 1) {
  # 1. Create a match cohort of the target cohort based on all the people in the database ----
  cdm$control <-
    CohortConstructor::matchCohorts(cdm$target, ratio = ratio, name = "control")
  # 2. Restrict the people in the matched cohort to those in the control cohort ----
  # cdi_control <-
  #   omopgenerics::getCohortId(cdm$control, cohortName = "cohort_2_matched")
  cdm$control <- cdm$control |>
    # Filter to people in the matched cohort
    dplyr::filter(cohort_definition_id == 2) |>
    dplyr::select(-cluster_id)
  printCustomMessage("Automatic control cohort creation completed")
  return(cdm)
}

#' Function for creating automatic matches based on inverse control logic
#'
#' @param cdm Connection to the database (package CDMConnector)
#' @keywords internal
createControlCohortInverse <- function(cdm) {
  # Left join with observation periods
  result <- cdm$target %>%
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) %>%
    dplyr::mutate(
      start_period = paste(observation_period_start_date, cohort_start_date, sep = " "),
      end_period = paste(cohort_end_date, observation_period_end_date, sep = " ")
    ) %>%
    dplyr::select(cohort_definition_id, subject_id, start_period, end_period) %>%
    as.data.frame() # We have to create a dataframe because of the usage of separate_rows func

  # Create table for inverse dates used in the target cohort
  long_result <- result %>%
    tidyr::pivot_longer(
      cols = c(start_period, end_period),
      names_to = "period_type",
      values_to = "period"
    ) %>%
    tidyr::separate_rows(period, sep = " ") %>%
    dplyr::group_by(cohort_definition_id, subject_id, period_type) %>%
    dplyr::summarize(
      cohort_start_date = as.Date(dplyr::first(period)),
      cohort_end_date = as.Date(dplyr::last(period)),
      .groups = 'drop'
    ) %>% dplyr::select(-period_type) %>%
    dplyr::filter(cohort_start_date < cohort_end_date)

  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "control",
    table = long_result,
    overwrite = T
  )
  cdm$control <- omopgenerics::newCohortTable(cdm$control)
  printCustomMessage("Inverse control cohort creation completed")
  return(cdm)
}
