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
                           domainsIncluded =
                             c(
                               "Drug",
                               "Condition",
                               "Measurement",
                               "Observation",
                               "Procedure",
                               "Visit",
                               "Visit detail"
                             )) {
  # Drug Exposure Patient Prevalence
  cdmConcepts = cdm$concept %>% dplyr::collect()

  patient_drug_prevalence_table <- cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$drug_exposure, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$drug_exposure_start_date >= .data$cohort_start_date &
        .data$drug_exposure_start_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$drug_concept_id,
      .data$drug_exposure_id,
      .data$drug_exposure_start_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$drug_concept_id) %>%
    dplyr::summarize(
      prevalence = dplyr::n_distinct(.data$drug_exposure_id),
      time_to_event = list(stats::na.omit(.data$drug_exposure_start_date - .data$cohort_start_date)),
      .groups = "drop"
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$drug_concept_id,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmConcepts, by = c("drug_concept_id" = "concept_id")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$drug_concept_id,
      .data$concept_name,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "drug_exposure")

  # Condition Occurrence Patient Prevalence
  patient_condition_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$condition_occurrence, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$condition_start_date >= .data$cohort_start_date &
        .data$condition_start_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$condition_concept_id,
      .data$condition_occurrence_id,
      .data$condition_start_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$condition_concept_id) %>%
    dplyr::summarize(
      prevalence = dplyr::n_distinct(.data$condition_occurrence_id),
      time_to_event = list(stats::na.omit(.data$condition_start_date - .data$cohort_start_date)),
      .groups = "drop"
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$condition_concept_id,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmConcepts, by = c("condition_concept_id" = "concept_id")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$condition_concept_id,
      .data$concept_name,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "condition_occurrence")

  # Measurement Patient Prevalence
  patient_measurement_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$measurement, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$measurement_date >= .data$cohort_start_date &
        .data$measurement_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$measurement_concept_id,
      .data$measurement_id,
      .data$measurement_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$measurement_concept_id) %>%
    dplyr::summarize(prevalence = dplyr::n_distinct(.data$measurement_id),
                     time_to_event = list(stats::na.omit(.data$measurement_date - .data$cohort_start_date)),
                     .groups = "drop") %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$measurement_concept_id,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmConcepts, by = c("measurement_concept_id" = "concept_id")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$measurement_concept_id,
      .data$concept_name,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "measurement")

  # Procedure Occurrence Patient Prevalence
  patient_procedure_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$procedure_occurrence, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$procedure_date >= .data$cohort_start_date &
        .data$procedure_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$procedure_concept_id,
      .data$procedure_occurrence_id,
      .data$procedure_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$procedure_concept_id) %>%
    dplyr::summarize(
      prevalence = dplyr::n_distinct(.data$procedure_occurrence_id),
      time_to_event = list(stats::na.omit(.data$procedure_date - .data$cohort_start_date)),
      .groups = "drop"
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$procedure_concept_id,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmConcepts, by = c("procedure_concept_id" = "concept_id")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$procedure_concept_id,
      .data$concept_name,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "procedure_occurrence")

  # Observation Patient Prevalence
  patient_observation_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$observation, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$observation_date >= .data$cohort_start_date &
        .data$observation_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$observation_concept_id,
      .data$observation_id,
      .data$observation_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$observation_concept_id) %>%
    dplyr::summarize(prevalence = dplyr::n_distinct(.data$observation_id),
                     time_to_event = list(stats::na.omit(.data$observation_date - .data$cohort_start_date)),
                     .groups = "drop") %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$observation_concept_id,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmConcepts, by = c("observation_concept_id" = "concept_id")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$observation_concept_id,
      .data$concept_name,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "observation")

  # Visit Occurrence Patient Prevalence
  patient_visit_prevalence_table <- cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$visit_occurrence, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$visit_start_date >= .data$cohort_start_date &
        .data$visit_start_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$visit_concept_id,
      .data$visit_occurrence_id,
      .data$visit_start_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$visit_concept_id) %>%
    dplyr::summarize(
      prevalence = dplyr::n_distinct(.data$visit_occurrence_id),
      time_to_event = list(stats::na.omit(.data$visit_start_date - .data$cohort_start_date)),
      .groups = "drop"
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$visit_concept_id,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmConcepts, by = c("visit_concept_id" = "concept_id")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$visit_concept_id,
      .data$concept_name,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "visit_occurrence")

  # Visit Detail Patient Prevalence
  patient_visit_detail_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$visit_detail, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$visit_detail_start_date >= .data$cohort_start_date &
        .data$visit_detail_start_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$visit_detail_concept_id,
      .data$visit_detail_id,
      .data$visit_detail_start_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$visit_detail_concept_id) %>%
    dplyr::summarize(prevalence = dplyr::n_distinct(.data$visit_detail_id),
                     time_to_event = list(stats::na.omit(.data$visit_detail_start_date - .data$cohort_start_date)),
                     .groups = "drop") %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$visit_detail_concept_id,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmConcepts, by = c("visit_detail_concept_id" = "concept_id")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$visit_detail_concept_id,
      .data$concept_name,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "visit_detail")

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
    time_to_event = list(),
    heritage = character()
  )

  domains <-
    c(
      "Drug",
      "Measurement",
      "Condition",
      "Observation",
      "Visit",
      "Visit detail",
      "Procedure"
    )
  for (domain in domains) {
    if (domain %in% domainsIncluded) {
      if (domain == "Visit") {
        printCustomMessage("Querying visit occurrence data from database ...")
        data_to_add <- patient_visit_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Visit detail") {
        printCustomMessage("Querying visit detail data from database ...")
        data_to_add <- patient_visit_detail_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Drug") {
        printCustomMessage("Querying drug exposure data from database ...")
        data_to_add <- patient_drug_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Measurement") {
        printCustomMessage("Querying measurement data from database ...")
        data_to_add <- patient_measurement_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Procedure") {
        printCustomMessage("Querying procedure occurrence data from database ...")
        data_to_add <- patient_procedure_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Observation") {
        printCustomMessage("Querying observation data from database ...")
        data_to_add <- patient_observation_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Condition") {
        printCustomMessage("Querying condition occurrence data from database ...")
        data_to_add <- patient_condition_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      }
    }
  }
  printCustomMessage("Querying patient prevalence data from database completed.")

  colnames(data_patients) <-
    c(
      "COHORT_DEFINITION_ID",
      "PERSON_ID",
      "CONCEPT_ID",
      "CONCEPT_NAME",
      "HERITAGE",
      "PREVALENCE",
      "TIME_TO_EVENT"
    )

  # aggregate rows in case of multiple observation periods in same cohort + remove void concepts

  printCustomMessage("Querying initial data from database ...")

  data_initial <- cdm$cohortcontrast_cohorts %>% as.data.frame()
  colnames(data_initial) <- toupper(colnames(data_initial))


  printCustomMessage("Querying person data from database ...")
  # Get person data

  data_person <-
    cdm$person %>%
    dplyr::select(.data$person_id,
                  .data$gender_concept_id,
                  .data$year_of_birth) %>%
    dplyr::filter(.data$person_id %in% data_initial$SUBJECT_ID) %>%
    as.data.frame()
  colnames(data_person) <- toupper(colnames(data_person))

  printCustomMessage("Data imported from the database!")
  # Add abstraction level
  data_patients$ABSTRACTION_LEVEL <- -1
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



#' @importFrom dplyr %>%

#' @title Resolve overlaps inside the cohort table
#' @param cohortTable A table with cohort table characteristics
#' @param cdm CDMConnector object: connection to the database
#'
#' @return A dataframe like cohort table with resolved overlaps
#'
#' @export
resolveCohortTableOverlaps <- function(cohortTable, cdm){

# Get observation periods
observation_period <- cdm$observation_period %>% as.data.frame()
# Check for conflicts with observation period
resolvedTable <- cohortTable %>%
  # Join with observation_period table based on subject_id and person_id
  dplyr::left_join(observation_period, by = c("subject_id" = "person_id")) %>%
  # Adjust cohort dates to fit within the observation period
  dplyr::mutate(
    cohort_start_date = pmax(.data$cohort_start_date, .data$observation_period_start_date),
    cohort_end_date = pmin(.data$cohort_end_date, .data$observation_period_end_date)
  ) %>%
  # Select only the relevant columns to return the original structure
  dplyr::select(.data$cohort_definition_id, .data$subject_id,.data$cohort_start_date, .data$cohort_end_date)

# View the updated table


resolvedTable <- resolvedTable %>%
  dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
  dplyr::arrange(.data$cohort_start_date, .by_group = TRUE) %>%
  dplyr::mutate(cohort_start_date = dplyr::if_else(!is.na(dplyr::lag(.data$cohort_end_date)) &
                                                      dplyr::lag(.data$cohort_end_date) >= .data$cohort_start_date,
                                                    dplyr::lag(.data$cohort_end_date) + 1,
                                                   .data$cohort_start_date)) %>%
  dplyr::ungroup()

return(resolvedTable)
}





#' @title Generate source code analysis tables
#' @param data CohortContrast object
#' @param domainsIncluded list of CDM domains to include
#'
#' @return object of dataframes and updated cdm object
#'
#' @keywords internal

generateSourceTables <- function(data,
                           domainsIncluded =
                             c(
                               "Drug",
                               "Condition",
                               "Measurement",
                               "Observation",
                               "Procedure",
                               "Visit",
                               "Visit detail"
                             )) {
  # Drug Exposure Patient Prevalence
  cdm = data$cdm
  cdmSourceConcepts = cdm$source_to_concept_map %>% dplyr::collect()

  patient_drug_prevalence_table <- cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$drug_exposure, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$drug_exposure_start_date >= .data$cohort_start_date &
        .data$drug_exposure_start_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$drug_source_value,
      .data$drug_exposure_id,
      .data$drug_exposure_start_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$drug_source_value) %>%
    dplyr::summarize(
      prevalence = dplyr::n_distinct(.data$drug_exposure_id),
      time_to_event = list(stats::na.omit(.data$drug_exposure_start_date - .data$cohort_start_date)),
      .groups = "drop"
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$drug_source_value,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmSourceConcepts, by = c("drug_source_value" = "source_code")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$drug_source_value,
      concept_name =.data$source_code_description,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "drug_exposure")

  # Condition Occurrence Patient Prevalence
  patient_condition_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$condition_occurrence, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$condition_start_date >= .data$cohort_start_date &
        .data$condition_start_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$condition_source_value,
      .data$condition_occurrence_id,
      .data$condition_start_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$condition_source_value) %>%
    dplyr::summarize(
      prevalence = dplyr::n_distinct(.data$condition_occurrence_id),
      time_to_event = list(stats::na.omit(.data$condition_start_date - .data$cohort_start_date)),
      .groups = "drop"
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$condition_source_value,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmSourceConcepts, by = c("condition_source_value" = "source_code")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$condition_source_value,
      concept_name = .data$source_code_description,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "condition_occurrence")

  # Measurement Patient Prevalence
  patient_measurement_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$measurement, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$measurement_date >= .data$cohort_start_date &
        .data$measurement_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$measurement_source_value,
      .data$measurement_id,
      .data$measurement_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$measurement_source_value) %>%
    dplyr::summarize(prevalence = dplyr::n_distinct(.data$measurement_id),
                     time_to_event = list(stats::na.omit(.data$measurement_date - .data$cohort_start_date)),
                     .groups = "drop") %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$measurement_source_value,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmSourceConcepts, by = c("measurement_source_value" = "source_code")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$measurement_source_value,
      concept_name = .data$source_code_description,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "measurement")

  # Procedure Occurrence Patient Prevalence
  patient_procedure_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$procedure_occurrence, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$procedure_date >= .data$cohort_start_date &
        .data$procedure_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$procedure_source_value,
      .data$procedure_occurrence_id,
      .data$procedure_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$procedure_source_value) %>%
    dplyr::summarize(
      prevalence = dplyr::n_distinct(.data$procedure_occurrence_id),
      time_to_event = list(stats::na.omit(.data$procedure_date - .data$cohort_start_date)),
      .groups = "drop"
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$procedure_source_value,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmSourceConcepts, by = c("procedure_source_value" = "source_code")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$procedure_source_value,
      concept_name =.data$source_code_description,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "procedure_occurrence")

  # Observation Patient Prevalence
  patient_observation_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$observation, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$observation_date >= .data$cohort_start_date &
        .data$observation_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$observation_source_value,
      .data$observation_id,
      .data$observation_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$observation_source_value) %>%
    dplyr::summarize(prevalence = dplyr::n_distinct(.data$observation_id),
                     time_to_event = list(stats::na.omit(.data$observation_date - .data$cohort_start_date)),
                     .groups = "drop") %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$observation_source_value,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmSourceConcepts, by = c("observation_source_value" = "source_code")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$observation_source_value,
      concept_name =.data$source_code_description,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "observation")

  # Visit Occurrence Patient Prevalence
  patient_visit_prevalence_table <- cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$visit_occurrence, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$visit_start_date >= .data$cohort_start_date &
        .data$visit_start_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$visit_source_value,
      .data$visit_occurrence_id,
      .data$visit_start_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$visit_source_value) %>%
    dplyr::summarize(
      prevalence = dplyr::n_distinct(.data$visit_occurrence_id),
      time_to_event = list(stats::na.omit(.data$visit_start_date - .data$cohort_start_date)),
      .groups = "drop"
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$visit_source_value,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmSourceConcepts, by = c("visit_source_value" = "source_code")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$visit_source_value,
      concept_name =.data$source_code_description,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "visit_occurrence")

  # Visit Detail Patient Prevalence
  patient_visit_detail_prevalence_table <-
    cdm$cohortcontrast_cohorts %>%
    dplyr::inner_join(cdm$visit_detail, by = c("subject_id" = "person_id")) %>%
    dplyr::filter(
      .data$visit_detail_start_date >= .data$cohort_start_date &
        .data$visit_detail_start_date <= .data$cohort_end_date
    ) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$visit_detail_source_value,
      .data$visit_detail_id,
      .data$visit_detail_start_date,
      .data$cohort_start_date
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(.data$cohort_definition_id,
                    .data$subject_id,
                    .data$visit_detail_source_value) %>%
    dplyr::summarize(prevalence = dplyr::n_distinct(.data$visit_detail_id),
                     time_to_event = list(stats::na.omit(.data$visit_detail_start_date - .data$cohort_start_date)),
                     .groups = "drop") %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      .data$visit_detail_source_value,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::left_join(cdmSourceConcepts, by = c("visit_detail_source_value" = "source_code")) %>%
    dplyr::select(
      .data$cohort_definition_id,
      .data$subject_id,
      concept_id = .data$visit_detail_source_value,
      concept_name =.data$source_code_description,
      .data$prevalence,
      .data$time_to_event
    ) %>%
    dplyr::mutate(heritage = "visit_detail")

  ############################################################################
  #
  # Query data from database
  #
  ############################################################################
  printCustomMessage("Querying patient prevalence source data from database ...")
  data_patients <- tibble::tibble(
    cohort_definition_id = integer(),
    subject_id = integer(),
    concept_id = integer(),
    concept_name = character(),
    prevalence = numeric(),
    time_to_event = list(),
    heritage = character()
  )

  domains <-
    c(
      "Drug",
      "Measurement",
      "Condition",
      "Observation",
      "Visit",
      "Visit detail",
      "Procedure"
    )
  for (domain in domains) {
    if (domain %in% domainsIncluded) {
      if (domain == "Visit") {
        printCustomMessage("Querying source visit occurrence data from database ...")
        data_to_add <- patient_visit_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Visit detail") {
        printCustomMessage("Querying source visit detail data from database ...")
        data_to_add <- patient_visit_detail_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Drug") {
        printCustomMessage("Querying source drug exposure data from database ...")
        data_to_add <- patient_drug_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Measurement") {
        printCustomMessage("Querying source measurement data from database ...")
        data_to_add <- patient_measurement_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Procedure") {
        printCustomMessage("Querying source procedure occurrence data from database ...")
        data_to_add <- patient_procedure_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Observation") {
        printCustomMessage("Querying source observation data from database ...")
        data_to_add <- patient_observation_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      } else if (domain == "Condition") {
        printCustomMessage("Querying source condition occurrence data from database ...")
        data_to_add <- patient_condition_prevalence_table %>%
          dplyr::group_by(
            .data$cohort_definition_id,
            .data$subject_id,
            .data$concept_id,
            .data$concept_name,
            .data$heritage
          ) %>%
          dplyr::summarize(prevalence = sum(.data$prevalence, na.rm = TRUE),
                           time_to_event = list(unlist(.data$time_to_event)),
                           .groups = "drop") %>%
          dplyr::filter(.data$concept_id != 0)
        data_patients <-
          rbind(data_patients, data_to_add %>% as.data.frame())
      }
    }
  }
  printCustomMessage("Querying source patient prevalence data from database completed.")

  colnames(data_patients) <-
    c(
      "COHORT_DEFINITION_ID",
      "PERSON_ID",
      "CONCEPT_ID",
      "CONCEPT_NAME",
      "HERITAGE",
      "PREVALENCE",
      "TIME_TO_EVENT"
    )
  # Add abstraction level
  data_patients$ABSTRACTION_LEVEL <- -2

  # Map source concept to integers and save the concept_id mapping
  unique_combinations <- unique(data_patients[, c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE")])

  # Step 2: Add a new column with integer IDs to the unique combinations
  unique_combinations$new_concept_id <- seq_len(nrow(unique_combinations))

  # Step 3: Merge only the new_concept_id back into data_patients

  data_patients <- merge(data_patients, unique_combinations,
                         by = c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE"),
                         all.x = TRUE)
  # Save source value in the name
  data_patients <- data_patients %>%  dplyr::mutate(CONCEPT_NAME = paste0(CONCEPT_NAME, "/C:", CONCEPT_ID))

  # Replace the old CONCEPT_ID with the new_concept_id
  data_patients$CONCEPT_ID <- data_patients$new_concept_id

  # Drop the temporary new_concept_id column
  data_patients$new_concept_id <- NULL

  # aggregate rows in case of multiple observation periods in same cohort + remove void concepts
  data$data_patients = rbind(data$data_patients, data_patients)
  data$sourceConceptIds = unique_combinations
  printCustomMessage("Source data imported from the database!")
  # Setting names for each list element
  return(data)
}
