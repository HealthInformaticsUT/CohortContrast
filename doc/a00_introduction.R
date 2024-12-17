## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = TRUE, eval=FALSE, echo=TRUE-----------------------------------
#  ################################################################################
#  #
#  # Initiate the database connection
#  #
#  #################################################################################
#  
#  user <- Sys.getenv("DB_USERNAME")
#  pw <- Sys.getenv("DB_PASSWORD")
#  server <- stringr::str_c(Sys.getenv("DB_HOST"), "/", Sys.getenv("DB_NAME"))
#  port <- Sys.getenv("DB_PORT")
#  
#  cdmSchema <-
#    Sys.getenv("OHDSI_CDM") # Schema which contains the OHDSI Common Data Model
#  cdmVocabSchema <-
#    Sys.getenv("OHDSI_VOCAB") # Schema which contains the OHDSI Common Data Model vocabulary tables.
#  cdmResultsSchema <-
#    Sys.getenv("OHDSI_RESULTS") # Schema which contains "cohort" table (is not mandatory)
#  writeSchema <-
#    Sys.getenv("OHDSI_WRITE") # Schema for temporary tables, will be deleted
#  writePrefix <- "cc_"
#  
#  db = DBI::dbConnect(
#    RPostgres::Postgres(),
#    dbname = Sys.getenv("DB_NAME"),
#    host = Sys.getenv("DB_HOST"),
#    user = Sys.getenv("DB_USERNAME"),
#    password = Sys.getenv("DB_PASSWORD"),
#    port  = port
#  )
#  
#  cdm <- CDMConnector::cdmFromCon(
#    con = db,
#    cdmSchema = cdmSchema,
#    achillesSchema = cdmResultsSchema,
#    writeSchema = c(schema = writeSchema, prefix = writePrefix),
#  )

## ---- include = TRUE, eval=FALSE, echo=TRUE-----------------------------------
#  ################################################################################
#  #
#  # Create target table from OMOP CDM instance (ATLAS's cohort id)
#  #
#  #################################################################################
#  
#  cohortsTableName = 'cohort'
#  targetCohortId = 1403
#  
#  targetTable <- CohortContrast::cohortFromCohortTable(cdm = cdm, db = db,
#     tableName = cohortsTableName, schemaName = cdmResultsSchema, cohortId = targetCohortId)

## ---- include = TRUE, eval=FALSE, echo=TRUE-----------------------------------
#  ################################################################################
#  #
#  # Create target table from a JSON
#  #
#  #################################################################################
#   pathToJSON = '/Users/markushaug/UT/R-packages/Develop/Git/CohortContrast/tests/testthat/inst/JSON'
#   targetTable <- CohortContrast::cohortFromJSON(pathToJSON = pathToJSON, cdm)

## ---- include = TRUE, eval=FALSE, echo=TRUE-----------------------------------
#  ################################################################################
#  #
#  # Create target table from a CSV
#  #
#  #################################################################################
#  pathToCsv = '/Users/markushaug/UT/R-packages/Develop/Git/CohortContrast/tests/testthat/inst/CSV/cohort/cohort.csv'
#  targetTable <- CohortContrast::cohortFromCSV(pathToCsv = pathToCsv, cohortId = 2)

## ---- include = TRUE, eval=FALSE, echo=TRUE-----------------------------------
#  ################################################################################
#  #
#  # Create target table
#  #
#  #################################################################################
#  library(tibble)
#  #Create the dataframe
#   data <- tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
#     1, 4804, '1997-03-23', '2018-10-29',
#     1, 4861, '1982-06-02', '2019-05-23',
#     1, 1563, '1977-06-25', '2019-04-20',
#     1, 2830, '2006-08-11', '2019-01-14',
#     1, 1655, '2004-09-29', '2019-05-24',
#     2, 5325, '1982-06-02', '2019-03-17',
#     2, 3743, '1997-03-23', '2018-10-07',
#     2, 2980, '2004-09-29', '2018-04-01',
#     2, 1512, '2006-08-11', '2017-11-29',
#     2, 2168, '1977-06-25', '2018-11-22'
#  )
#  targetTable <- cohortFromDataTable(data = data, cohortId = 2)

## ---- include = TRUE, eval=FALSE, echo=TRUE-----------------------------------
#  ################################################################################
#  #
#  # Create control cohort table based on matches
#  #
#  #################################################################################
#  controlTable = CohortContrast::createControlCohortMatching(cdm = cdm, targetTable = targetTable, ratio = 4)

## ---- include = TRUE, eval=FALSE, echo=TRUE-----------------------------------
#  ################################################################################
#  #
#  # Create control cohort table based on inverse controls
#  #
#  #################################################################################
#  controlTable = CohortContrast::createControlCohortInverse(cdm = cdm, targetTable = targetTable)

## ---- include = TRUE, eval=FALSE, echo=TRUE-----------------------------------
#  ################################################################################
#  #
#  # Resolve conflicts
#  #
#  #################################################################################
#  targetTable = CohortContrast::resolveCohortTableOverlaps(cdm = cdm, cohortTable = targetTable)
#  controlTable = CohortContrast::resolveCohortTableOverlaps(cdm = cdm, cohortTable = controlTable)

