% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CohortContrast.R
\name{CohortContrast}
\alias{CohortContrast}
\title{Run CohortContrast Analysis}
\usage{
CohortContrast(
  cdm,
  targetTable = NULL,
  controlTable = NULL,
  pathToResults = getwd(),
  domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure",
    "Visit", "Visit detail", "Death"),
  prevalenceCutOff = 10,
  topK = FALSE,
  presenceFilter = 0.005,
  lookbackDays = FALSE,
  complementaryMappingTable = NULL,
  runZTests = TRUE,
  runLogitTests = TRUE,
  runKSTests = TRUE,
  getAllAbstractions = FALSE,
  getSourceData = FALSE,
  maximumAbstractionLevel = 5,
  createOutputFiles = TRUE,
  complName = NULL,
  numCores = parallel::detectCores() - 1
)
}
\arguments{
\item{cdm}{Connection to database}

\item{targetTable}{Table for target cohort (tbl)}

\item{controlTable}{Table for control cohort (tbl)}

\item{pathToResults}{Path to the results folder, can be project's working directory}

\item{domainsIncluded}{list of CDM domains to include}

\item{prevalenceCutOff}{numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times}

\item{topK}{numeric > if set, keeps this number of features in the analysis. Maximum number of features exported.}

\item{presenceFilter}{numeric > if set, removes all features represented less than the given percentage}

\item{lookbackDays}{FALSE or an integer stating the lookback period for cohort index date}

\item{complementaryMappingTable}{Mappingtable for mapping concept_ids if present, columns CONCEPT_ID, CONCEPT_NAME, NEW_CONCEPT_ID, NEW_CONCEPT_NAME, ABSTRACTION_LEVEL, TYPE}

\item{runZTests}{boolean for Z-tests}

\item{runLogitTests}{boolean for logit-tests}

\item{runKSTests}{boolean for Kolmogorov-Smirnov tests}

\item{getAllAbstractions}{boolean for creating abstractions' levels for the imported data, this is useful when using GUI and exploring data}

\item{getSourceData}{boolean for fetching source data}

\item{maximumAbstractionLevel}{Maximum level of abstraction allowed}

\item{createOutputFiles}{Boolean for creating output files, the default value is TRUE}

\item{complName}{Name of the output file}

\item{numCores}{Number of cores to allocate to parallel processing}
}
\description{
Run CohortContrast Analysis
}
\examples{
\dontrun{
control <- data.frame(
  cohort_definition_id = c(1, 1, 1, 1, 1),
  subject_id = c(5325, 3743, 2980, 1512, 2168),
  cohort_start_date = as.Date(c("1982-06-02", "1997-03-23",
   "2004-09-29", "2006-08-11", "1977-06-25")),
  cohort_end_date = as.Date(c("2019-03-17", "2018-10-07",
   "2018-04-01", "2017-11-29", "2018-11-22"))
)

target <- data.frame(
  cohort_definition_id = c(1, 1, 1, 1, 1),
  subject_id = c(4804, 4861, 1563, 2830, 1655),
  cohort_start_date = as.Date(c("1997-03-23", "1982-06-02",
   "1977-06-25", "2006-08-11", "2004-09-29")),
  cohort_end_date = as.Date(c("2018-10-29", "2019-05-23",
   "2019-04-20", "2019-01-14", "2019-05-24"))
)

control$cohort_definition_id = 100
target$cohort_definition_id = 500

cohort = rbind(control, target)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS example")
DBI::dbWriteTable(con,   DBI::SQL('"example"."cohort"'), cohort)

cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia",
 cdm_schema = "main", write_schema = "main")

 targetTable <- cohortFromCohortTable(cdm = cdm, db = con,
  tableName = "cohort", schemaName = 'example', cohortId = 500)
controlTable <- cohortFromCohortTable(cdm = cdm, db = con,
 tableName = "cohort", schemaName = 'example', cohortId = 100)


pathToResults = getwd()

data = CohortContrast(
  cdm,
  targetTable = targetTable,
  controlTable = controlTable,
  pathToResults,
  domainsIncluded = c(
    "Drug"
  ),
  prevalenceCutOff = 0.1,
  topK = FALSE,
  presenceFilter = 0.005,
  complementaryMappingTable = NULL,
  runZTests = FALSE,
  runLogitTests = FALSE,
  createOutputFiles = FALSE,
  numCores = 1
)

DBI::dbDisconnect(con)
}

}
