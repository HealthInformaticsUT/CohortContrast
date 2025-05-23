% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CDMConnectorSetup.R
\name{createControlCohortMatching}
\alias{createControlCohortMatching}
\title{Function for creating automatic matches based on age and sex}
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
Function for creating automatic matches based on age and sex
}
\examples{
\dontrun{createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 2))}
}
