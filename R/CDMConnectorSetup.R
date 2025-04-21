#' Function for creating automatic matches based on age and sex
#'
#' @param cdm Connection to the database (package CDMConnector)
#' @param ratio ratio for the number of matches generated
#' @param targetTable A cohort tibble which contains subjects' cohort data
#' @param max Maximum ratio to use
#' @param min Minimum ratio to use
#' @export
#' @examples
#' \dontrun{createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 2))}
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
#' @export
#' @examples
#' \dontrun{createControlCohortInverse(cdm = cdm, targetTable = targetTable)}

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
#' \dontrun{
#' targetTable <- cohortFromCohortTable(cdm = cdm, db = db,
#'  tableName = "cohort", schemaName = 'ohdsi_results', cohortId = 1389)
#'}
#' \dontrun{
#' targetTable <- cohortFromCohortTable(cdm = cdm, db = db,
#'  tableName = "asthma", schemaName = 'user_peter')
#'}
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
#' \dontrun{
#' # Load necessary package
#' library(tibble)
#'
#' # Create the dataframe
#' data <- tribble(
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
#'}
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
#' @examples \dontrun{
#' pathToCsv = './cohorts.csv'
#' targetTable <- cohortFromCSV(pathToCsv = pathToCsv, cohortId = 2)
#'}
cohortFromCSV <- function(pathToCsv, cohortId = NULL) {
  cohortTable = readr::read_csv(pathToCsv)
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
#' \dontrun{
#' pathToJSON = './JSON/'
#' targetTable <- cohortFromJSON(pathToJSON = pathToJSON, cdm, cohortId = 2)
#'}
cohortFromJSON <- function(pathToJSON, cdm, cohortId = NULL) {
  cohortSet = CDMConnector::read_cohort_set(pathToJSON)
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
#'
#' @examples
#' \dontrun{
#' user <- Sys.getenv("DB_USERNAME") #TODO
#' pw <- Sys.getenv("DB_PASSWORD") #TODO
#' server <- stringr::str_c(Sys.getenv("DB_HOST"), "/", Sys.getenv("DB_NAME")) #TODO
#' port <- Sys.getenv("DB_PORT") #TODO
#
#' cdmSchema <-
#'   Sys.getenv("OHDSI_CDM")
#' cdmVocabSchema <-
#'   Sys.getenv("OHDSI_VOCAB")
#' cdmResultsSchema <-
#'   Sys.getenv("OHDSI_RESULTS")
#' writeSchema <-
#'   Sys.getenv("OHDSI_WRITE")
#' writePrefix <- "cc_"
#'
#' db = DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = Sys.getenv("DB_NAME"),
#'   host = Sys.getenv("DB_HOST"),
#'   user = Sys.getenv("DB_USERNAME"),
#'   password = Sys.getenv("DB_PASSWORD"),
#'   port  = port
#' )
#'
#' cdm <- CDMConnector::cdmFromCon(
#'   con = db,
#'   cdmSchema = cdmSchema,
#'   achillesSchema = cdmResultsSchema,
#'   writeSchema = c(schema = writeSchema, prefix = writePrefix),
#' )
#'
#'
#' targetTable <- cohortFromCohortTable(cdm = cdm, db = db,
#'  tableName = "cohort", schemaName = cdmResultsSchema, cohortId = 1)
#' controlTable <- cohortFromCohortTable(cdm = cdm, db = db,
#'  tableName = "cohort", schemaName = cdmResultsSchema, cohortId = 2)
#'
#' cdm <- createCohortContrastCdm(cdm = cdm, targetTable = targetTable, controlTable = controlTable)
#' }
#'
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


#' Function for matching the control to target by age
#'
#' @param cdm Connection to the database (package CDMConnector)
#' @param sourceTable Table which is used as reference for matching
#' @param tableToMatch Table which is matched
#' @param maxAllowedAgeDifference Value for maximum allowed age difference to be mapped to
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
  cat(sprintf("🔹 Average age difference before matching: %.2f years\n", avg_age_diff_before))

  # For reproducibility
  set.seed(2025)

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

  cat(sprintf("✅ Matched %d pairs\n", nrow(matched_source)))
  cat(sprintf("✅ Average age difference after matching: %.2f years (max allowed: %d)\n",
              avg_age_diff_after, maxAllowedAgeDifference))

  return(list(
    source = matched_source,
    tableToMatch = matched_match
  ))
}
