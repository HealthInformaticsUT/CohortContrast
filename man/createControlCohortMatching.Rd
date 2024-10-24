% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CDMConnectorSetup.R
\name{createControlCohortMatching}
\alias{createControlCohortMatching}
\title{#' Function to nudge cohorts
#'
#' This function outputs a dataframe with columns SUBJECT_ID, COHORT_DEFINITION_ID, COHORT_START_DATE, COHORT_END_DATE
#' @param cdm Connection to database
#' @param nudgeTarget number of days you would like to nudge the target cohort start day
#' @param nudgeControl number of days you would like to nudge the control cohort start day
#'
#' @keywords internal}
\usage{
createControlCohortMatching(
  cdm,
  targetTable,
  ratio = 1,
  max = NULL,
  min = NULL
)
}
\arguments{
\item{cdm}{Connection to the database (package CDMConnector)}

\item{targetTable}{A cohort tibble which contains subjects' cohort data}

\item{ratio}{ratio for the number of matches generated}

\item{max}{Maximum ratio to use}

\item{min}{Minimum ratio to use}
}
\description{
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
Function for creating automatic matches based on age and sex
}
\examples{
\dontrun{createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 2))}
}
