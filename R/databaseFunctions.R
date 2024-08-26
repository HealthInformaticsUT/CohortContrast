#' @importFrom dplyr %>%

#' @title Generate analysis tables
#' Create relations of cohorts (target and control) to the database
#' @param cdm CDMConnector object: connection to the database
#' @param domainsIncluded list of CDM domains to include
#'
#' @return object of dataframes and updated cdm object
#'
#' @keywords internal
generateTables <- function(cdm,
                           pathToResults = getwd(),
                           domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure", "Visit", "Visit detail")
                           ) {
   cohort_size <- cdm$cohortcontrast_cohorts %>%
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::summarize(total_count = dplyr::n_distinct(subject_id), .groups = 'drop')

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
      data_patients = data_patients,
      data_initial = data_initial,
      data_person = data_person,
      cdm = cdm
    )
  )
}
