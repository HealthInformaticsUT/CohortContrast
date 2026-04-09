#' Function for creating automatic matches based on age and sex
#'
#' @param cdm Connection to the database (package CDMConnector)
#' @param ratio ratio for the number of matches generated
#' @param targetTable A cohort tibble which contains subjects' cohort data
#' @param max Maximum ratio to use
#' @param min Minimum ratio to use
#' @return A data frame representing the matched control cohort. The returned
#'   table contains cohort_definition_id, subject_id, cohort_start_date, and
#'   cohort_end_date columns, with one row per matched control interval aligned
#'   to the target cohort follow-up logic.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("CDMConnector", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("duckdb", quietly = TRUE) &&
#'     nzchar(Sys.getenv("EUNOMIA_DATA_FOLDER")) &&
#'     isTRUE(tryCatch(
#'       CDMConnector::eunomiaIsAvailable("GiBleed"),
#'       error = function(...) FALSE
#'     ))) {
#'   pathToJSON <- system.file(
#'     "example", "example_json", "diclofenac",
#'     package = "CohortContrast"
#'   )
#'   con <- DBI::dbConnect(
#'     duckdb::duckdb(),
#'     dbdir = CDMConnector::eunomiaDir("GiBleed")
#'   )
#'   cdm <- CDMConnector::cdmFromCon(
#'     con = con,
#'     cdmName = "eunomia",
#'     cdmSchema = "main",
#'     writeSchema = "main"
#'   )
#'
#'   targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
#'   controlTable <- createControlCohortMatching(
#'     cdm = cdm,
#'     targetTable = targetTable,
#'     ratio = 1
#'   )
#'   head(controlTable)
#'
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
createControlCohortMatching <-
  function(cdm, targetTable, ratio = 1, max = NULL, min = NULL) {
    cdm <- omopgenerics::insertTable(
      cdm = cdm,
      name = "target",
      table = targetTable %>% as.data.frame() %>% dplyr::mutate(cohort_definition_id = 1),
      overwrite = T
    )
    cdm$target = omopgenerics::newCohortTable(cdm$target)
    target_with_clusters = NULL
    # 1. Create a match cohort of the target cohort based on all the people in the database ----
    if (is.null(max) && is.null(min)) {
    cdm$control <-
      CohortConstructor::matchCohorts(cdm$target, ratio = ratio, name = "control")
    target_with_clusters = cdm$control %>% dplyr::collect() %>% dplyr::filter(.data$cohort_definition_id == 1)
    cdm$control <- dplyr::filter(cdm$control, .data$cohort_definition_id == 2)
    }
    else {
      cdm$control <-
        CohortConstructor::matchCohorts(cdm$target, ratio = Inf, name = "control")
      target_with_clusters = cdm$control %>% dplyr::collect() %>% dplyr::filter(.data$cohort_definition_id == 1)
      cdm$control <- dplyr::filter(cdm$control, .data$cohort_definition_id == 2)
      # Group by person and filter by max and min criteria
      matched_counts <- cdm$control %>%
        dplyr::group_by(.data$cluster_id) %>%
        dplyr::summarise(match_count = dplyr::n()) %>% as.data.frame()

      if (!is.null(min)) {
        # Check if any person has fewer than min matches
        insufficient_matches <- matched_counts %>% dplyr::filter(.data$match_count < min)
        if (nrow(insufficient_matches) > 0) {
          stop("Some persons have fewer than the minimum required matches.")
        }
        cdm$control <- cdm$control %>%
          dplyr::group_by(.data$cluster_id) %>%
          dplyr::mutate(row_number = dplyr::row_number()) %>%
          dplyr::filter(.data$row_number <= ratio) %>%
          dplyr::select(-.data$row_number)
      }

      if (!is.null(max)) {
        # Limit the matches per person to the specified maximum
        cdm$control <- cdm$control %>%
          dplyr::group_by(.data$cluster_id) %>%
          dplyr::mutate(row_number = dplyr::row_number()) %>%
          dplyr::filter(.data$row_number <= max) %>%
          dplyr::select(-.data$row_number)
      }
    }
    # Step 1: Get target's start and duration
    # Maybe apply same dates as in target match
    target_dates <- target_with_clusters %>%
      dplyr::mutate(
        duration = cohort_end_date - cohort_start_date
      ) %>%
      dplyr::select(cluster_id, start_date_target = cohort_start_date, duration) %>%
      dplyr::distinct()

    # Step 2: Join to all controls
    control <- cdm$control %>%
      dplyr::collect() %>%
      dplyr::filter(.data$cohort_definition_id == 2) %>%
      dplyr::left_join(target_dates, by = "cluster_id") %>%
      dplyr::mutate(
        # Target end date (to be tested)
        end_date_target = start_date_target + lubridate::days(as.integer(duration)),

        # Condition: can we apply the target start + end date to the control?
        use_target_dates = cohort_start_date <= start_date_target & cohort_end_date >= end_date_target,

        # Apply either target dates or fallback to control start + target duration
        cohort_start_date = dplyr::if_else(
          use_target_dates,
          start_date_target,
          cohort_start_date
        ),
        cohort_end_date = dplyr::if_else(
          use_target_dates,
          end_date_target,
          cohort_start_date + lubridate::days(as.integer(duration))
        )
      )

    result <- control  %>%
      dplyr::ungroup() %>%
      dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
      dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) %>%
      as.data.frame()

    return(result)
  }


#' Function for creating automatic matches based on inverse control logic
#'
#' @param cdm Connection to the database (package CDMConnector)
#' @param targetTable A cohort tibble which contains subjects' cohort data
#' @return A data frame representing inverse control time windows for the target
#'   subjects. The returned table contains cohort_definition_id, subject_id,
#'   cohort_start_date, and cohort_end_date columns, where each row captures an
#'   observation-period segment outside the target cohort interval.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("CDMConnector", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("duckdb", quietly = TRUE) &&
#'     nzchar(Sys.getenv("EUNOMIA_DATA_FOLDER")) &&
#'     isTRUE(tryCatch(
#'       CDMConnector::eunomiaIsAvailable("GiBleed"),
#'       error = function(...) FALSE
#'     ))) {
#'   pathToJSON <- system.file(
#'     "example", "example_json", "diclofenac",
#'     package = "CohortContrast"
#'   )
#'   con <- DBI::dbConnect(
#'     duckdb::duckdb(),
#'     dbdir = CDMConnector::eunomiaDir("GiBleed")
#'   )
#'   cdm <- CDMConnector::cdmFromCon(
#'     con = con,
#'     cdmName = "eunomia",
#'     cdmSchema = "main",
#'     writeSchema = "main"
#'   )
#'
#'   targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
#'   controlTable <- createControlCohortInverse(cdm = cdm, targetTable = targetTable)
#'   head(controlTable)
#'
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }

createControlCohortInverse <- function(cdm, targetTable) {
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "target",
    table = targetTable %>% as.data.frame(),
    overwrite = T
  )
  cdm$target = omopgenerics::newCohortTable(cdm$target)
  # Left join with observation periods
  result <- cdm$target %>%
    dplyr::left_join(cdm$observation_period, by = c("subject_id" = "person_id")) %>%
    dplyr::mutate(
      start_period = paste(.data$observation_period_start_date, as.Date(.data$cohort_start_date - lubridate::days(1)), sep = " "),
      end_period = paste(as.Date(.data$cohort_end_date + lubridate::days(1)), .data$observation_period_end_date, sep = " ")
    ) %>%
    dplyr::select(.data$cohort_definition_id, .data$subject_id, .data$start_period, .data$end_period) %>%
    as.data.frame() # We have to create a dataframe because of the usage of separate_rows func
  # Create table for inverse dates used in the target cohort
  result <- result %>%
    tidyr::pivot_longer(
      cols = c(.data$start_period, .data$end_period),
      names_to = "period_type",
      values_to = "period"
    ) %>%
    tidyr::separate_rows(.data$period, sep = " ") %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id, .data$period_type) %>%
    dplyr::summarize(
      cohort_start_date = as.Date(dplyr::first(.data$period)),
      cohort_end_date = as.Date(dplyr::last(.data$period)),
      .groups = 'drop'
    ) %>% dplyr::select(-.data$period_type) %>%
    dplyr::filter(.data$cohort_start_date < .data$cohort_end_date)

  return(result)
}

#'  Read cohort from database cohort table
#' @param cdm CDMConnector object
#' @param db Database instance (DBI)
#' @param tableName Name of the table where the cohort is defined
#' @param schemaName Name of the schema where the cohort table is defined
#' @param cohortId The id for cohort in cohorts' table, if NULL whole table will be imported
#'
#' @return a tbl object for further CohortContrast usage
#'
#' @export
#' @examples
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("duckdb", quietly = TRUE) &&
#'     requireNamespace("CDMConnector", quietly = TRUE)) {
#'   db <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
#'   DBI::dbExecute(db, "CREATE SCHEMA example")
#'
#'   cohort <- data.frame(
#'     cohort_definition_id = c(1L, 2L),
#'     subject_id = c(101L, 202L),
#'     cohort_start_date = as.Date(c("2020-01-01", "2020-02-01")),
#'     cohort_end_date = as.Date(c("2020-01-10", "2020-02-10"))
#'   )
#'   DBI::dbWriteTable(db, DBI::SQL('"example"."cohort"'), cohort)
#'
#'   targetTable <- cohortFromCohortTable(
#'     cdm = NULL,
#'     db = db,
#'     tableName = "cohort",
#'     schemaName = "example",
#'     cohortId = 2
#'   )
#'   targetTable
#'
#'   DBI::dbDisconnect(db, shutdown = TRUE)
#' }
cohortFromCohortTable <- function(cdm,
                                  db,
                                  tableName = NULL,
                                  schemaName = NULL,
                                  cohortId = NULL) {
  cohortTable <-
    dplyr::tbl(db,
               CDMConnector::inSchema(schemaName, tableName))
  if (!is.null(cohortId)) {
    cohortTable <-
      dplyr::filter(cohortTable, .data$cohort_definition_id == cohortId)
  }
  cohortTable <- tibble::as_tibble(cohortTable)
  assertRequiredCohortTable(cohortTable)
  return(cohortTable)
}


#'  Read cohort from data.frame object
#' @param data A data frame with cohort data
#' @param cohortId The id for cohort in cohorts' table, if NULL whole table will be imported
#'
#' @return a tbl object for further CohortContrast usage
#'
#' @export
#' @examples
#' data <- tibble::tribble(
#'   ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
#'   1, 4804, '1997-03-23', '2018-10-29',
#'   1, 4861, '1982-06-02', '2019-05-23',
#'   1, 1563, '1977-06-25', '2019-04-20',
#'   1, 2830, '2006-08-11', '2019-01-14',
#'   1, 1655, '2004-09-29', '2019-05-24',
#'   2, 5325, '1982-06-02', '2019-03-17',
#'   2, 3743, '1997-03-23', '2018-10-07',
#'   2, 2980, '2004-09-29', '2018-04-01',
#'   2, 1512, '2006-08-11', '2017-11-29',
#'   2, 2168, '1977-06-25', '2018-11-22'
#' )
#' targetTable <- cohortFromDataTable(data = data, cohortId = 2)
#' targetTable
cohortFromDataTable <- function(data, cohortId = NULL) {
  cohortTable = data
  if (!is.null(cohortId)) {
    cohortTable <-
      dplyr::filter(cohortTable, .data$cohort_definition_id == cohortId)
  }
  cohortTable <- tibble::as_tibble(cohortTable)
  assertRequiredCohortTable(cohortTable)
  return(cohortTable)
}

#' Read cohort from CSV
#' @param pathToCsv Path to the cohort data CSV file
#' @param cohortId The id for cohort in cohorts' table, if NULL whole table will be imported
#'
#' @return a tbl object for further CohortContrast usage
#'
#' @export
#' @examples
#' if (requireNamespace("readr", quietly = TRUE)) {
#'   pathToCsv <- tempfile(fileext = ".csv")
#'   cohort <- data.frame(
#'     cohort_definition_id = c(1L, 2L),
#'     subject_id = c(101L, 202L),
#'     cohort_start_date = as.Date(c("2020-01-01", "2020-02-01")),
#'     cohort_end_date = as.Date(c("2020-01-10", "2020-02-10"))
#'   )
#'   readr::write_csv(cohort, pathToCsv)
#'
#'   targetTable <- cohortFromCSV(pathToCsv = pathToCsv, cohortId = 2)
#'   targetTable
#' }
cohortFromCSV <- function(pathToCsv, cohortId = NULL) {
  cohortTable = readr::read_csv(pathToCsv, show_col_types = FALSE)
  if (!is.null(cohortId)) {
    cohortTable <-
      dplyr::filter(cohortTable, .data$cohort_definition_id == cohortId)
  }
  cohortTable <- tibble::as_tibble(cohortTable)
  assertRequiredCohortTable(cohortTable)
  return(cohortTable)
}

#'  Read cohort from JSON
#' @param pathToJSON Path to the cohort data JSON file
#' @param cdm  Connection to the database (package CDMConnector)
#' @param cohortId The id for cohort in cohorts' table, if NULL whole table will be imported
#'
#' @return a tbl object for further CohortContrast usage
#'
#' @export
#' @examples
#' if (requireNamespace("CDMConnector", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("duckdb", quietly = TRUE) &&
#'     nzchar(Sys.getenv("EUNOMIA_DATA_FOLDER")) &&
#'     isTRUE(tryCatch(
#'       CDMConnector::eunomiaIsAvailable("GiBleed"),
#'       error = function(...) FALSE
#'     ))) {
#'   pathToJSON <- system.file(
#'     "example", "example_json", "diclofenac",
#'     package = "CohortContrast"
#'   )
#'   con <- DBI::dbConnect(
#'     duckdb::duckdb(),
#'     dbdir = CDMConnector::eunomiaDir("GiBleed")
#'   )
#'   cdm <- CDMConnector::cdmFromCon(
#'     con = con,
#'     cdmName = "eunomia",
#'     cdmSchema = "main",
#'     writeSchema = "main"
#'   )
#'
#'   targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
#'   targetTable
#'
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
cohortFromJSON <- function(pathToJSON, cdm, cohortId = NULL) {
  cohortSet = CDMConnector::readCohortSet(pathToJSON)
  cdm = CDMConnector::generateCohortSet(cdm, cohortSet, "target")

  cohortTable = cdm$target %>% as.data.frame()
  if (!is.null(cohortId)) {
    cohortTable <-
      dplyr::filter(cohortTable, .data$cohort_definition_id == cohortId)
  }
  cohortTable <- tibble::as_tibble(cohortTable)
  assertRequiredCohortTable(cohortTable)
  return(cohortTable)
}

#'  Insert cohort tables to CDM instance, preparing them from CohortContrast analysis
#' @param cdm CDMConnector object
#' @param targetTable Table for target cohort (tbl)
#' @param controlTable Table for control cohort (tbl)
#'
#' @return object of dataframes and updated cdm object
#'
#' @keywords internal
createCohortContrastCdm <- function(cdm,
                                    targetTable = NULL,
                                    controlTable = NULL) {
  cli::cli_alert_info("Inserting target and control tables to CDM object")
  targetTable <-
    targetTable %>% as.data.frame() %>% dplyr::mutate(cohort_definition_id = 2)
  controlTable <-
    controlTable %>% as.data.frame() %>% dplyr::mutate(cohort_definition_id = 1)
  cohortcontrast_cohorts <-
    dplyr::bind_rows(targetTable,
                     controlTable)
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "cohortcontrast_cohorts",
    table = cohortcontrast_cohorts,
    overwrite = T
  )

  cdm$cohortcontrast_cohorts = omopgenerics::newCohortTable(cdm$cohortcontrast_cohorts)
  return(cdm)
}


#' Remove Temporal Bias from CohortContrast Analysis
#'
#' This function identifies and optionally removes concepts that may represent temporal bias
#' in a CohortContrast analysis. It works by creating age/sex matched controls from the
#' general population for the same time periods as the target cohort, then using a
#' proportion test to identify concepts where the matched cohort has greater or equal
#' prevalence compared to the target. These concepts likely represent temporal trends
#' (e.g., seasonal effects, healthcare changes) rather than condition-specific features.
#'
#' The function applies Bonferroni correction for multiple testing, adjusting the
#' significance level by dividing alpha by the number of concepts being tested.
#'
#' @param data A CohortContrast result object (returned from CohortContrast function)
#' @param cdm Connection to the database (package CDMConnector)
#' @param ratio Matching ratio for control cohort generation (default: 1)
#' @param alpha Significance level for the proportion test before Bonferroni correction (default: 0.05)
#' @param domainsIncluded Domains to analyze for temporal bias (default: same as original analysis)
#' @param removeIdentified If TRUE, automatically remove identified temporal bias concepts from the data (default: FALSE)
#'
#' @return A list containing:
#'   \item{temporal_bias_concepts}{A data frame of concepts identified as potential temporal bias}
#'   \item{data}{The original or filtered CohortContrast data object (if removeIdentified = TRUE)}
#'   \item{matched_control_prevalences}{Prevalence data from the matched control cohort}
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("CDMConnector", quietly = TRUE) &&
#'     requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("duckdb", quietly = TRUE) &&
#'     nzchar(Sys.getenv("EUNOMIA_DATA_FOLDER")) &&
#'     isTRUE(tryCatch(
#'       CDMConnector::eunomiaIsAvailable("GiBleed"),
#'       error = function(...) FALSE
#'     ))) {
#'   pathToJSON <- system.file(
#'     "example", "example_json", "diclofenac",
#'     package = "CohortContrast"
#'   )
#'   con <- DBI::dbConnect(
#'     duckdb::duckdb(),
#'     dbdir = CDMConnector::eunomiaDir("GiBleed")
#'   )
#'   cdm <- CDMConnector::cdmFromCon(
#'     con = con,
#'     cdmName = "eunomia",
#'     cdmSchema = "main",
#'     writeSchema = "main"
#'   )
#'
#'   targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm = cdm)
#'   controlTable <- createControlCohortInverse(cdm = cdm, targetTable = targetTable)
#'   data <- CohortContrast(
#'     cdm = cdm,
#'     targetTable = targetTable,
#'     controlTable = controlTable,
#'     pathToResults = tempdir(),
#'     prevalenceCutOff = 1,
#'     topK = 3,
#'     presenceFilter = FALSE,
#'     runChi2YTests = TRUE,
#'     runLogitTests = TRUE,
#'     createOutputFiles = FALSE,
#'     numCores = 1
#'   )
#'
#'   result_filtered <- removeTemporalBias(
#'     data = data,
#'     cdm = cdm,
#'     ratio = 1,
#'     removeIdentified = TRUE
#'   )
#'   head(result_filtered$data$data_features)
#'
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
removeTemporalBias <- function(data,
                               cdm,
                               ratio = 1,
                               alpha = 0.05,
                               domainsIncluded = NULL,
                               removeIdentified = FALSE) {

  cli::cli_alert_info("Starting temporal bias detection...")

  # Validate input
  if (!inherits(data, "CohortContrastObject")) {
    cli::cli_alert_warning("Input data is not a CohortContrastObject. Attempting to proceed anyway.")
  }

  if (is.null(data$data_initial)) {
    stop("data_initial is missing from the CohortContrast result. Cannot perform temporal bias analysis.")
  }

  # Use domains from original analysis if not specified
  if (is.null(domainsIncluded)) {
    domainsIncluded <- data$config$domainsIncluded
  }

  # Extract target cohort data
  target_initial <- data$data_initial %>%
    dplyr::filter(.data$COHORT_DEFINITION_ID == "target")

  if (nrow(target_initial) == 0) {
    stop("No target cohort patients found in data_initial.")
  }

  n_target <- nrow(target_initial)
  cli::cli_alert_info(paste0("Found ", n_target, " target cohort patients."))

  # Prepare target table for matching (convert to format expected by createControlCohortMatching)
  target_table <- target_initial %>%
    dplyr::transmute(
      cohort_definition_id = 1L,
      subject_id = .data$SUBJECT_ID,
      cohort_start_date = .data$COHORT_START_DATE,
      cohort_end_date = .data$COHORT_END_DATE
    )

  # Create matched control cohort based on age and sex
  cli::cli_alert_info("Creating age/sex matched control cohort for the same time periods...")
  matched_control <- createControlCohortMatching(
    cdm = cdm,
    targetTable = target_table,
    ratio = ratio
  )

  if (nrow(matched_control) == 0) {
    cli::cli_alert_warning("No matched controls found. Cannot perform temporal bias analysis.")
    return(list(
      temporal_bias_concepts = data.frame(),
      data = data,
      matched_control_prevalences = data.frame()
    ))
  }

  # Explicitly exclude any target subjects from matched cohort (safety check)
  target_subject_ids <- target_initial$SUBJECT_ID
  matched_control <- matched_control %>%
    dplyr::filter(!.data$subject_id %in% target_subject_ids)

  if (nrow(matched_control) == 0) {
    cli::cli_alert_warning("No matched controls remaining after excluding target subjects.")
    return(list(
      temporal_bias_concepts = data.frame(),
      data = data,
      matched_control_prevalences = data.frame()
    ))
  }

  n_matched <- dplyr::n_distinct(matched_control$subject_id)
  cli::cli_alert_info(paste0("Created ", nrow(matched_control), " matched control records (", n_matched, " unique subjects, excluding target)."))

  # Query concept prevalences for matched control cohort
  cli::cli_alert_info("Querying concept prevalences for matched control cohort...")
  matched_prevalences <- queryMatchedControlPrevalences(
    cdm = cdm,
    matchedControl = matched_control,
    domainsIncluded = domainsIncluded
  )

  if (nrow(matched_prevalences) == 0) {
    cli::cli_alert_warning("No concept prevalences found for matched control cohort.")
    return(list(
      temporal_bias_concepts = data.frame(),
      data = data,
      matched_control_prevalences = data.frame()
    ))
  }

  # Calculate prevalences for matched cohort
  matched_prevalence_summary <- matched_prevalences %>%
    dplyr::group_by(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$HERITAGE) %>%
    dplyr::summarise(
      MATCHED_SUBJECT_COUNT = dplyr::n_distinct(.data$PERSON_ID),
      .groups = "drop"
    ) %>%
    dplyr::mutate(MATCHED_PREVALENCE = .data$MATCHED_SUBJECT_COUNT / n_matched)

  # Get target prevalences from original data
  target_prevalences <- data$data_features %>%
    dplyr::filter(.data$ABSTRACTION_LEVEL == -1) %>%
    dplyr::select(
      .data$CONCEPT_ID,
      .data$CONCEPT_NAME,
      .data$HERITAGE,
      .data$TARGET_SUBJECT_PREVALENCE,
      .data$TARGET_SUBJECT_COUNT
    )

  # Join prevalences
  cli::cli_alert_info("Running proportion tests to identify temporal bias...")
  comparison <- target_prevalences %>%
    dplyr::inner_join(
      matched_prevalence_summary,
      by = c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE")
    )

  # Apply Bonferroni correction for multiple testing
  n_tests <- nrow(comparison)
  alpha_corrected <- alpha / n_tests
  cli::cli_alert_info(paste0("Applying Bonferroni correction: alpha = ", alpha, " / ", n_tests, " = ", format(alpha_corrected, scientific = TRUE)))

  # Perform one-sided proportion test for each concept
  # H0: matched prevalence >= target prevalence (temporal bias)
  # H1: target prevalence > matched prevalence (condition-specific)
  # If we fail to reject H0 (p >= alpha_corrected), concept is flagged as temporal bias
  comparison <- comparison %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      P_VALUE = tryCatch({
        test_result <- stats::prop.test(
          x = c(.data$TARGET_SUBJECT_COUNT, .data$MATCHED_SUBJECT_COUNT),
          n = c(n_target, n_matched),
          alternative = "greater",
          conf.level = 1 - alpha_corrected
        )
        test_result$p.value
      }, error = function(e) NA_real_),
      IS_TEMPORAL_BIAS = is.na(.data$P_VALUE) | .data$P_VALUE >= alpha_corrected | .data$MATCHED_PREVALENCE >= .data$TARGET_SUBJECT_PREVALENCE
    ) %>%
    dplyr::ungroup()

  # Identify temporal bias concepts
  temporal_bias_concepts <- comparison %>%
    dplyr::filter(.data$IS_TEMPORAL_BIAS) %>%
    dplyr::select(
      .data$CONCEPT_ID,
      .data$CONCEPT_NAME,
      .data$HERITAGE,
      .data$TARGET_SUBJECT_PREVALENCE,
      .data$MATCHED_PREVALENCE,
      .data$TARGET_SUBJECT_COUNT,
      .data$MATCHED_SUBJECT_COUNT,
      .data$P_VALUE
    ) %>%
    dplyr::arrange(dplyr::desc(.data$MATCHED_PREVALENCE))

  n_temporal_bias <- nrow(temporal_bias_concepts)
  cli::cli_alert_success(paste0("Identified ", n_temporal_bias, " concepts as potential temporal bias (matched >= target)."))

  if (n_temporal_bias > 0) {
    cli::cli_alert_info("Top temporal bias concepts (highest matched prevalence):")
    print(utils::head(temporal_bias_concepts, 10))
  }

  # Optionally remove temporal bias concepts from data
  result_data <- data
  if (removeIdentified && n_temporal_bias > 0) {
    cli::cli_alert_info("Removing temporal bias concepts from data...")
    result_data <- filterTemporalBiasConcepts(data, temporal_bias_concepts)
    cli::cli_alert_success("Temporal bias concepts removed from data.")
  }

  return(list(
    temporal_bias_concepts = temporal_bias_concepts,
    data = result_data,
    matched_control_prevalences = comparison
  ))
}


#' Query concept prevalences for matched control cohort
#'
#' @param cdm CDMConnector object
#' @param matchedControl Matched control cohort table
#' @param domainsIncluded List of domains to include
#'
#' @keywords internal
queryMatchedControlPrevalences <- function(cdm, matchedControl, domainsIncluded) {
  cli::cli_alert_info("Collecting concept table for matched control analysis.")
  cdmConcepts <- cdm$concept %>% dplyr::collect()
  matchedControl = resolveCohortTableOverlaps(cohortTable = matchedControl  %>% as.data.frame(), cdm = cdm)
  # Insert matched control into CDM
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "temporal_bias_matched",
    table = matchedControl,
    overwrite = TRUE
  )
  cdm$temporal_bias_matched <- omopgenerics::newCohortTable(cdm$temporal_bias_matched)

  # Initialize result
  data_patients <- tibble::tibble(
    PERSON_ID = integer(),
    CONCEPT_ID = integer(),
    CONCEPT_NAME = character(),
    HERITAGE = character()
  )

  domains <- c("Drug", "Measurement", "Condition", "Observation", "Visit", "Visit detail", "Procedure", "Death")

  for (domain in domains) {
    if (domain %in% domainsIncluded) {
      if (domain == "Drug") {
        printCustomMessage("Querying drug exposure data for matched control...")
        data_to_add <- cdm$temporal_bias_matched %>%
          dplyr::inner_join(cdm$drug_exposure, by = c("subject_id" = "person_id")) %>%
          dplyr::filter(
            .data$drug_exposure_start_date >= .data$cohort_start_date &
              .data$drug_exposure_start_date <= .data$cohort_end_date
          ) %>%
          dplyr::select(.data$subject_id, .data$drug_concept_id) %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::left_join(cdmConcepts, by = c("drug_concept_id" = "concept_id")) %>%
          dplyr::transmute(
            PERSON_ID = .data$subject_id,
            CONCEPT_ID = .data$drug_concept_id,
            CONCEPT_NAME = .data$concept_name,
            HERITAGE = "drug_exposure"
          ) %>%
          dplyr::filter(.data$CONCEPT_ID != 0)
        data_patients <- dplyr::bind_rows(data_patients, data_to_add)
      } else if (domain == "Condition") {
        printCustomMessage("Querying condition occurrence data for matched control...")
        data_to_add <- cdm$temporal_bias_matched %>%
          dplyr::inner_join(cdm$condition_occurrence, by = c("subject_id" = "person_id")) %>%
          dplyr::filter(
            .data$condition_start_date >= .data$cohort_start_date &
              .data$condition_start_date <= .data$cohort_end_date
          ) %>%
          dplyr::select(.data$subject_id, .data$condition_concept_id) %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::left_join(cdmConcepts, by = c("condition_concept_id" = "concept_id")) %>%
          dplyr::transmute(
            PERSON_ID = .data$subject_id,
            CONCEPT_ID = .data$condition_concept_id,
            CONCEPT_NAME = .data$concept_name,
            HERITAGE = "condition_occurrence"
          ) %>%
          dplyr::filter(.data$CONCEPT_ID != 0)
        data_patients <- dplyr::bind_rows(data_patients, data_to_add)
      } else if (domain == "Measurement") {
        printCustomMessage("Querying measurement data for matched control...")
        data_to_add <- cdm$temporal_bias_matched %>%
          dplyr::inner_join(cdm$measurement, by = c("subject_id" = "person_id")) %>%
          dplyr::filter(
            .data$measurement_date >= .data$cohort_start_date &
              .data$measurement_date <= .data$cohort_end_date
          ) %>%
          dplyr::select(.data$subject_id, .data$measurement_concept_id) %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::left_join(cdmConcepts, by = c("measurement_concept_id" = "concept_id")) %>%
          dplyr::transmute(
            PERSON_ID = .data$subject_id,
            CONCEPT_ID = .data$measurement_concept_id,
            CONCEPT_NAME = .data$concept_name,
            HERITAGE = "measurement"
          ) %>%
          dplyr::filter(.data$CONCEPT_ID != 0)
        data_patients <- dplyr::bind_rows(data_patients, data_to_add)
      } else if (domain == "Observation") {
        printCustomMessage("Querying observation data for matched control...")
        data_to_add <- cdm$temporal_bias_matched %>%
          dplyr::inner_join(cdm$observation, by = c("subject_id" = "person_id")) %>%
          dplyr::filter(
            .data$observation_date >= .data$cohort_start_date &
              .data$observation_date <= .data$cohort_end_date
          ) %>%
          dplyr::select(.data$subject_id, .data$observation_concept_id) %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::left_join(cdmConcepts, by = c("observation_concept_id" = "concept_id")) %>%
          dplyr::transmute(
            PERSON_ID = .data$subject_id,
            CONCEPT_ID = .data$observation_concept_id,
            CONCEPT_NAME = .data$concept_name,
            HERITAGE = "observation"
          ) %>%
          dplyr::filter(.data$CONCEPT_ID != 0)
        data_patients <- dplyr::bind_rows(data_patients, data_to_add)
      } else if (domain == "Procedure") {
        printCustomMessage("Querying procedure occurrence data for matched control...")
        data_to_add <- cdm$temporal_bias_matched %>%
          dplyr::inner_join(cdm$procedure_occurrence, by = c("subject_id" = "person_id")) %>%
          dplyr::filter(
            .data$procedure_date >= .data$cohort_start_date &
              .data$procedure_date <= .data$cohort_end_date
          ) %>%
          dplyr::select(.data$subject_id, .data$procedure_concept_id) %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::left_join(cdmConcepts, by = c("procedure_concept_id" = "concept_id")) %>%
          dplyr::transmute(
            PERSON_ID = .data$subject_id,
            CONCEPT_ID = .data$procedure_concept_id,
            CONCEPT_NAME = .data$concept_name,
            HERITAGE = "procedure_occurrence"
          ) %>%
          dplyr::filter(.data$CONCEPT_ID != 0)
        data_patients <- dplyr::bind_rows(data_patients, data_to_add)
      } else if (domain == "Visit") {
        printCustomMessage("Querying visit occurrence data for matched control...")
        data_to_add <- cdm$temporal_bias_matched %>%
          dplyr::inner_join(cdm$visit_occurrence, by = c("subject_id" = "person_id")) %>%
          dplyr::filter(
            .data$visit_start_date >= .data$cohort_start_date &
              .data$visit_start_date <= .data$cohort_end_date
          ) %>%
          dplyr::select(.data$subject_id, .data$visit_concept_id) %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::left_join(cdmConcepts, by = c("visit_concept_id" = "concept_id")) %>%
          dplyr::transmute(
            PERSON_ID = .data$subject_id,
            CONCEPT_ID = .data$visit_concept_id,
            CONCEPT_NAME = .data$concept_name,
            HERITAGE = "visit_occurrence"
          ) %>%
          dplyr::filter(.data$CONCEPT_ID != 0)
        data_patients <- dplyr::bind_rows(data_patients, data_to_add)
      } else if (domain == "Visit detail") {
        printCustomMessage("Querying visit detail data for matched control...")
        data_to_add <- cdm$temporal_bias_matched %>%
          dplyr::inner_join(cdm$visit_detail, by = c("subject_id" = "person_id")) %>%
          dplyr::filter(
            .data$visit_detail_start_date >= .data$cohort_start_date &
              .data$visit_detail_start_date <= .data$cohort_end_date
          ) %>%
          dplyr::select(.data$subject_id, .data$visit_detail_concept_id) %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::left_join(cdmConcepts, by = c("visit_detail_concept_id" = "concept_id")) %>%
          dplyr::transmute(
            PERSON_ID = .data$subject_id,
            CONCEPT_ID = .data$visit_detail_concept_id,
            CONCEPT_NAME = .data$concept_name,
            HERITAGE = "visit_detail"
          ) %>%
          dplyr::filter(.data$CONCEPT_ID != 0)
        data_patients <- dplyr::bind_rows(data_patients, data_to_add)
      } else if (domain == "Death") {
        printCustomMessage("Querying death data for matched control...")
        data_to_add <- cdm$temporal_bias_matched %>%
          dplyr::inner_join(cdm$death, by = c("subject_id" = "person_id")) %>%
          dplyr::filter(
            .data$death_date >= .data$cohort_start_date &
              .data$death_date <= .data$cohort_end_date
          ) %>%
          dplyr::select(.data$subject_id, .data$death_type_concept_id) %>%
          dplyr::distinct() %>%
          dplyr::collect() %>%
          dplyr::left_join(cdmConcepts, by = c("death_type_concept_id" = "concept_id")) %>%
          dplyr::transmute(
            PERSON_ID = .data$subject_id,
            CONCEPT_ID = .data$death_type_concept_id,
            CONCEPT_NAME = .data$concept_name,
            HERITAGE = "death"
          ) %>%
          dplyr::filter(.data$CONCEPT_ID != 0)
        data_patients <- dplyr::bind_rows(data_patients, data_to_add)
      }
    }
  }

  return(data_patients)
}


#' Filter temporal bias concepts from CohortContrast data
#'
#' @param data CohortContrast result object
#' @param temporalBiasConcepts Data frame of temporal bias concepts to remove
#'
#' @keywords internal
filterTemporalBiasConcepts <- function(data, temporalBiasConcepts) {
  concepts_to_remove <- temporalBiasConcepts %>%
    dplyr::select(.data$CONCEPT_ID, .data$HERITAGE) %>%
    dplyr::distinct()

  # Filter data_features
  if ("data_features" %in% names(data)) {
    data$data_features <- dplyr::anti_join(
      data$data_features,
      concepts_to_remove,
      by = c("CONCEPT_ID", "HERITAGE")
    )
  }

  # Filter data_patients
  if ("data_patients" %in% names(data)) {
    data$data_patients <- dplyr::anti_join(
      data$data_patients,
      concepts_to_remove,
      by = c("CONCEPT_ID", "HERITAGE")
    )
  }

  selectedFeatureData <- data$selectedFeatureData
  if (is.null(selectedFeatureData) && "trajectoryDataList" %in% names(data)) {
    selectedFeatureData <- data$trajectoryDataList
  }

  if (!is.null(selectedFeatureData) && "selectedFeatures" %in% names(selectedFeatureData)) {
    selectedFeatureData$selectedFeatures <- dplyr::anti_join(
      selectedFeatureData$selectedFeatures,
      concepts_to_remove,
      by = c("CONCEPT_ID", "HERITAGE")
    )

    selectedFeatureData$selectedFeatureNames <- selectedFeatureData$selectedFeatures$CONCEPT_NAME
    selectedFeatureData$selectedFeatureIds <- selectedFeatureData$selectedFeatures$CONCEPT_ID
    data$selectedFeatureData <- selectedFeatureData
    data$trajectoryDataList <- selectedFeatureData
  }

  # Add info to config
  data$config$temporalBiasRemoved <- TRUE
  data$config$temporalBiasConceptsRemoved <- nrow(concepts_to_remove)

  return(data)
}


#' Function for matching the control to target by age
#'
#' @param cdm Connection to the database (package CDMConnector)
#' @param sourceTable Table which is used as reference for matching
#' @param tableToMatch Table which is matched
#' @param maxAllowedAgeDifference Value for maximum allowed age difference to be mapped to
#' @return A list with two data frames named source and tableToMatch. Each data
#'   frame contains the matched rows from the corresponding input table after
#'   age-based sampling, preserving the original column structure of the input
#'   cohort tables.
#' @examples
#' cdm <- list(
#'   person = tibble::tibble(
#'     person_id = 1:13,
#'     year_of_birth = c(1980L, 1981L, 1982L, 1983L, 1984L,
#'                       1980L, 1981L, 1982L, 1985L, 1986L, 1982L, 1983L, 1984L)
#'   )
#' )
#' sourceTable <- tibble::tibble(
#'   cohort_definition_id = 1L,
#'   subject_id = c(1L, 2L, 3L, 4L, 5L),
#'   cohort_start_date = as.Date(rep("2020-01-01", 5)),
#'   cohort_end_date = as.Date(rep("2020-01-10", 5))
#' )
#' tableToMatch <- tibble::tibble(
#'   cohort_definition_id = 2L,
#'   subject_id = c(6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L),
#'   cohort_start_date = as.Date(rep("2020-01-01", 8)),
#'   cohort_end_date = as.Date(rep("2020-01-10", 8))
#' )
#'
#' matched <- matchCohortsByAge(
#'   cdm = cdm,
#'   sourceTable = sourceTable,
#'   tableToMatch = tableToMatch,
#'   maxAllowedAgeDifference = 1
#' )
#' nrow(tableToMatch)
#' matched
#' @export
matchCohortsByAge <- function(cdm, sourceTable, tableToMatch, maxAllowedAgeDifference = 0) {
  cdmPerson <- cdm$person %>% dplyr::collect()
  # Step 1: Calculate age at cohort start
  # Join and calculate age
  source <- dplyr::left_join(sourceTable, cdmPerson, by = c("subject_id" = "person_id")) %>%
    dplyr::mutate(age = lubridate::year(cohort_start_date) - year_of_birth)

  match <- dplyr::left_join(tableToMatch, cdmPerson, by = c("subject_id" = "person_id")) %>%
    dplyr::mutate(age = lubridate::year(cohort_start_date) - year_of_birth)

  source_sample <- source
  match_sample <- match


  age_diffs_before <- outer(source_sample$age, match_sample$age, FUN = function(x, y) abs(x - y))
  avg_age_diff_before <- mean(age_diffs_before, na.rm = TRUE)
  cat(sprintf("Average age difference before matching: %.2f years\n", avg_age_diff_before))

  matched_source_list <- list()
  matched_match_list <- list()

  for (age_val in sort(unique(source$age))) {
    source_subset <- dplyr::filter(source, age == age_val)
    match_subset <- dplyr::filter(match, dplyr::between(age, age_val - maxAllowedAgeDifference, age_val + maxAllowedAgeDifference))

    n_to_match <- min(nrow(source_subset), nrow(match_subset))
    if (n_to_match == 0) next

    source_sample <- dplyr::slice_sample(source_subset, n = n_to_match)
    match_sample <- dplyr::slice_sample(match_subset, n = n_to_match)

    matched_source_list[[length(matched_source_list) + 1]] <- source_sample
    matched_match_list[[length(matched_match_list) + 1]] <- match_sample

    match <- dplyr::anti_join(match, match_sample, by = "subject_id")
  }

  matched_source <- dplyr::bind_rows(matched_source_list) %>%
    dplyr::select(dplyr::all_of(names(sourceTable)))

  matched_match <- dplyr::bind_rows(matched_match_list) %>%
    dplyr::select(dplyr::all_of(names(tableToMatch)))

  # Compute post-match age difference
  age1 <- lubridate::year(matched_source$cohort_start_date) -
    cdmPerson$year_of_birth[match(matched_source$subject_id, cdmPerson$person_id)]

  age2 <- lubridate::year(matched_match$cohort_start_date) -
    cdmPerson$year_of_birth[match(matched_match$subject_id, cdmPerson$person_id)]

  age_diff_after <- abs(age1 - age2)
  avg_age_diff_after <- mean(age_diff_after, na.rm = TRUE)

  cat(sprintf("Matched %d pairs\n", nrow(matched_source)))
  cat(sprintf("Average age difference after matching: %.2f years (max allowed: %d)\n",
              avg_age_diff_after, maxAllowedAgeDifference))

  return(list(
    source = matched_source,
    tableToMatch = matched_match
  ))
}
