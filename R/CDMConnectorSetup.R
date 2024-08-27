#' @importFrom dplyr %>%

#' @title Generate cohort tables for CohortContrast analysis
#' Create relations of cohorts (target and control) to the database. If this function is run successfully, then CohortContrast package can be run.
#' @param cdm CDMConnector object
#' @param db Database instance (DBI)
#' @param targetTableName Name of the table where target cohort is defined
#' @param controlTableName Name of the table where control cohort is defined
#' @param targetTableSchemaName Name of the schema where target cohort table is defined
#' @param controlTableSchemaName Name of the schema where control cohort table is defined
#' @param cohortsTableSchemaName Name of the schema where cohorts' table is defined
#' @param cohortsTableName Name of the table where cohorts are defined
#' @param targetCohortId The id for target cohort in cohorts' table
#' @param controlCohortId The id for control cohort in cohorts' table
#' @param pathToCohortsCSVFile The path to a CSV file that has data table for cohorts
#' @param nudgeTarget number of days you would like to nudge the target cohort start day
#' @param nudgeControl number of days you would like to nudge the control cohort start day
#' @param useInverseControls Boolean for using inverse controls (target cohort's observation period which is not included)
#' @param useTargetMatching Boolean for using patient matching for controls
#'
#' @return object of dataframes and updated cdm object
#'
#' @keywords internal
#'
#' @examples
#' # Example usage of createCohortContrastCohorts
#' control <- data.frame(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(5325, 3743, 2980, 1512, 2168),
#'   cohort_start_date = as.Date(c("1982-06-02", "1997-03-23",
#'    "2004-09-29", "2006-08-11", "1977-06-25")),
#'   cohort_end_date = as.Date(c("2019-03-17", "2018-10-07",
#'    "2018-04-01", "2017-11-29", "2018-11-22"))
#' )
#'
#' target <- data.frame(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(4804, 4861, 1563, 2830, 1655),
#'   cohort_start_date = as.Date(c("1997-03-23", "1982-06-02",
#'    "1977-06-25", "2006-08-11", "2004-09-29")),
#'   cohort_end_date = as.Date(c("2018-10-29", "2019-05-23",
#'    "2019-04-20", "2019-01-14", "2019-05-24"))
#' )
#'
#' control$cohort_definition_id = 100
#' target$cohort_definition_id = 500
#'
#' cohort = rbind(control, target)
#'
#' con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
#' DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS testthat")
#' DBI::dbWriteTable(con,   DBI::SQL('"testthat"."cohort"'), cohort)
#'
#' cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia",
#'  cdm_schema = "main", write_schema = "main")
#'
#' cdm <- createCohortContrastCohorts(
#'   cdm,
#'   con,
#'   targetTableName = NULL,
#'   controlTableName = NULL,
#'   targetTableSchemaName = NULL,
#'   controlTableSchemaName = NULL,
#'   cohortsTableSchemaName = 'testthat',
#'   cohortsTableName = 'cohort',
#'   targetCohortId = 500,
#'   controlCohortId = 100,
#'   nudgeTarget = FALSE,
#'   nudgeControl = FALSE,
#'   useInverseControls = FALSE,
#'   useTargetMatching = FALSE
#' )
#'
#' DBI::dbDisconnect(con)
createCohortContrastCohorts <- function(cdm,
                                        db,
                                        targetTableName = NULL,
                                        controlTableName = NULL,
                                        targetTableSchemaName = NULL,
                                        controlTableSchemaName = NULL,
                                        cohortsTableSchemaName = NULL,
                                        cohortsTableName = NULL,
                                        targetCohortId = NULL,
                                        controlCohortId = NULL,
                                        pathToCohortsCSVFile = NULL,
                                        nudgeTarget = FALSE,
                                        nudgeControl = FALSE,
                                        useInverseControls = FALSE,
                                        useTargetMatching = FALSE) {
  areRelationsDefined <- checkForCorrectRelationDefinitions(targetTableName = targetTableName,
                                                            controlTableName = controlTableName,
                                                            targetTableSchemaName = targetTableSchemaName,
                                                            controlTableSchemaName = controlTableSchemaName,
                                                            cohortsTableSchemaName = cohortsTableSchemaName,
                                                            cohortsTableName = cohortsTableName,
                                                            targetCohortId = targetCohortId,
                                                            controlCohortId = controlCohortId,
                                                            pathToCohortsCSVFile = pathToCohortsCSVFile)
  if(!areRelationsDefined) {
    printCustomMessage("Incorrect target and control cohort inputs, exiting ...")
    return(cdm)
  }

  targetTable = NULL
  cohortTable = NULL

  if (!is.null(targetTableName) ||
      (!is.null(targetCohortId) && !is.null(cohortsTableName)) || (!is.null(targetCohortId) && !is.null(pathToCohortsCSVFile))) {
    # Creating target cohort table
    if (!is.null(targetTableName)) {
      printCustomMessage("Creating target cohort table based on target cohort's table ...")
      targetTable <-
        dplyr::tbl(db,
                   CDMConnector::in_schema(targetTableSchemaName, targetTableName)) %>% as.data.frame()
      cdm <- omopgenerics::insertTable(cdm = cdm,
                                       name = "target",
                                       table = targetTable,
                                       overwrite = T)
      cdm$target = omopgenerics::newCohortTable(cdm$target)

    } else{
      printCustomMessage("Creating target cohort table based on cohorts' id ...")
      if ((!is.null(targetCohortId) && !is.null(cohortsTableName))){
      targetTable <-
        dplyr::tbl(db,
                   CDMConnector::in_schema(cohortsTableSchemaName, cohortsTableName))
      targetTable = targetTable %>% dplyr::filter(.data$cohort_definition_id == targetCohortId) %>% as.data.frame()
      }
      else{
        printCustomMessage("Reading target data from cohort CSV ...")
      cohortTable = readr::read_csv(pathToCohortsCSVFile)
      targetTable = cohortTable %>% dplyr::filter(.data$cohort_definition_id == targetCohortId)
      }
      cdm <- omopgenerics::insertTable(cdm = cdm,
                                       name = "target",
                                       table = targetTable,
                                       overwrite = T)
      cdm$target = omopgenerics::newCohortTable(cdm$target)
    }
    # Creating control cohort table
    if (!is.null(controlTableName)) {
      printCustomMessage("Creating control cohort table based on control cohort's table ...")
      controlTable <-
        dplyr::tbl(db,
                   CDMConnector::in_schema(controlTableSchemaName, controlTableName)) %>% as.data.frame()
      cdm <- omopgenerics::insertTable(cdm = cdm,
                                       name = "control",
                                       table = controlTable,
                                       overwrite = T)
      cdm$control = omopgenerics::newCohortTable(cdm$control)

    }
    else if ((!is.null(controlCohortId) &&
              !is.null(cohortsTableName))) {
      printCustomMessage("Creating control cohort table based on cohorts' id ...")
      controlTable <-
        dplyr::tbl(db,
                   CDMConnector::in_schema(cohortsTableSchemaName, cohortsTableName)) %>% as.data.frame()
      controlTable = controlTable %>% dplyr::filter(.data$cohort_definition_id == controlCohortId)
      cdm <- omopgenerics::insertTable(cdm = cdm,
                                       name = "control",
                                       table = controlTable,
                                       overwrite = T)
      cdm$control = omopgenerics::newCohortTable(cdm$control)
    }
    else if (!is.null(controlCohortId) && !is.null(pathToCohortsCSVFile)){
      printCustomMessage("Reading control data from cohort CSV ...")
      controlTable = cohortTable %>% dplyr::filter(.data$cohort_definition_id == controlCohortId)
      cdm <- omopgenerics::insertTable(cdm = cdm,
                                       name = "control",
                                       table = controlTable,
                                       overwrite = T)
      cdm$control = omopgenerics::newCohortTable(cdm$control)
    }
    else {
      if (useInverseControls & useTargetMatching) {
        printCustomMessage(
          'WARNING: Both useInverseControls & useTargetMatching are marked as TRUE. Using useInverseControls as default!'
        )
      }
      if (useInverseControls) {
        printCustomMessage("Inverse control cohort creation initiated ...")
        cdm <- createControlCohortInverse(cdm)
      }
      else {
        printCustomMessage("Matching (based on AGE and SEX) control cohort creation initiated ...")
        cdm <- createControlCohortMatching(cdm)
      }
    }

  }
  else {
    printCustomMessage(
      "ERROR: Something went wrong during cohorts' initiation, check your cohort tables configuration!"
    )
  }
  # # Combine the target and control tables
  cdm$target <-
    cdm$target %>% dplyr::mutate(cohort_definition_id = as.integer(2))
  cdm$control <-
    cdm$control %>% dplyr::mutate(cohort_definition_id = as.integer(1))

  if(is.numeric(nudgeTarget) | is.numeric(nudgeControl)){
    nudgeCohorts(cdm = cdm, nudgeTarget = nudgeTarget, nudgeControl = nudgeControl)
  }

  cohortcontrast_cohorts <-
    dplyr::bind_rows(cdm$target %>% as.data.frame(),
                     cdm$control %>% as.data.frame())

  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = "cohortcontrast_cohorts",
    table = cohortcontrast_cohorts,
    overwrite = T
  )

  printCustomMessage("Cohort table initiation successful! You can now execute CohortContrast")

  return(cdm)
}

#' Function to nudge cohorts
#'
#' This function outputs a dataframe with columns SUBJECT_ID, COHORT_DEFINITION_ID, COHORT_START_DATE, COHORT_END_DATE
#' @param cdm Connection to database
#' @param nudgeTarget number of days you would like to nudge the target cohort start day
#' @param nudgeControl number of days you would like to nudge the control cohort start day
#'
#' @keywords internal
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
