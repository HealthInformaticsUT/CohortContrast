#' #' Function to nudge cohorts
#' #'
#' #' This function outputs a dataframe with columns SUBJECT_ID, COHORT_DEFINITION_ID, COHORT_START_DATE, COHORT_END_DATE
#' #' @param cdm Connection to database
#' #' @param nudgeTarget number of days you would like to nudge the target cohort start day
#' #' @param nudgeControl number of days you would like to nudge the control cohort start day
#' #'
#' #' @keywords internal
#'
#' nudgeCohorts <- function(cdm, nudgeTarget, nudgeControl) {
#'   if (nudgeTarget != FALSE) {
#'     cdm$target <-  cdm$target |>
#'       dplyr::mutate(cohort_start_date + nudgeTarget)
#'     printCustomMessage("Nudging target cohort EXECUTED!")
#'   }
#'   if (nudgeControl != FALSE) {
#'     cdm$control <-  cdm$control |>
#'       dplyr::mutate(cohort_start_date + nudgeTarget)
#'     printCustomMessage("Nudging control cohort EXECUTED!")
#'   }
#' }

#' Function for creating automatic matches based on age and sex
#'
#' @param cdm Connection to the database (package CDMConnector)
#' @param ratio ratio for the number of matches generated
#' @param targetTable A cohort tibble which contains subjects' cohort data
#' @export
#' @examples
#' \dontrun{createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 2))}
createControlCohortMatching <-
  function(cdm, targetTable, ratio = 1) {
    cdm <- omopgenerics::insertTable(
      cdm = cdm,
      name = "target",
      table = targetTable %>% as.data.frame(),
      overwrite = T
    )
    cdm$target = omopgenerics::newCohortTable(cdm$target)
    # 1. Create a match cohort of the target cohort based on all the people in the database ----
    cdm$control <-
      CohortConstructor::matchCohorts(cdm$target, ratio = ratio, name = "control")
    # 2. Restrict the people in the matched cohort to those in the control cohort ----
    cdm$control <- cdm$control |>
      # Filter to people in the matched cohort
      dplyr::filter(cohort_definition_id == 2) |>
      dplyr::select(-cluster_id)
    result <- cdm$control %>% as.data.frame()
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
      start_period = paste(observation_period_start_date, cohort_start_date, sep = " "),
      end_period = paste(cohort_end_date, observation_period_end_date, sep = " ")
    ) %>%
    dplyr::select(cohort_definition_id, subject_id, start_period, end_period) %>%
    as.data.frame() # We have to create a dataframe because of the usage of separate_rows func

  # Create table for inverse dates used in the target cohort
  result <- result %>%
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
  return(result)
}

#' @title Read cohort from database cohort table
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
               CDMConnector::in_schema(schemaName, tableName))
  if (!is.null(cohortId)) {
    cohortTable <-
      dplyr::filter(cohortTable, cohort_definition_id == cohortId)
  }
  assertRequiredColumns(cohortTable)
  return(cohortTable)
}


#' @title Read cohort from data.frame object
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
      dplyr::filter(cohortTable, cohort_definition_id == cohortId)
  }
  cohortTable <- tibble::as_tibble(cohortTable)
  assertRequiredColumns(cohortTable)
  return(cohortTable)
}


#' @title Read cohort from CSV
#' @param pathToCsv Path to the cohort data CSV file
#' @param cohortId The id for cohort in cohorts' table, if NULL whole table will be imported
#'
#' @return a tbl object for further CohortContrast usage
#'
#' @export
#' @examples
#' \dontrun{
#' pathToCsv = './cohorts.csv'
#' targetTable <- cohortFromCSV(pathToCsv = pathToCsv, cohortId = 2)
#'}
cohortFromCSV <- function(pathToCsv, cohortId = NULL) {
  cohortTable = readr::read_csv(pathToCsv)
  if (!is.null(cohortId)) {
    cohortTable <-
      dplyr::filter(cohortTable, cohort_definition_id == cohortId)
  }
  cohortTable <- tibble::as_tibble(cohortTable)
  assertRequiredColumns(cohortTable)
  return(cohortTable)
}


#' @title Read cohort from JSON
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
      dplyr::filter(cohortTable, cohort_definition_id == cohortId)
  }
  cohortTable <- tibble::as_tibble(cohortTable)
  assertRequiredColumns(cohortTable)
  return(cohortTable)
}

#' @title Insert cohort tables to CDM instance, preparing them from CohortContrast analysis
#' @param cdm CDMConnector object
#' @param targetTable Table for target cohort (tbl)
#' @param controlTable Table for control cohort (tbl)
#'
#' @return object of dataframes and updated cdm object
#'
#' @export
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
  # TODO: assert tbl for  targetTable and controlTable
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
