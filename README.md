# CohortContrast

CohortContrast is a package designed to facilitate the comparison between cohorts in specified domains across all OMOP CDM datasets. It enables users to analyze and visualize the contrast between target and control cohorts effectively.

## Usage

To use CohortContrast, follow these steps to configure your environment and input data:

1. **Credentials**: Make sure you can create a connection to your OHDS CDM instance using CDMConnector package.

2. **Create target and control tables** 
    - Use functions `cohortFromCohortTable`, `cohortFromDataTable`, `cohortFromJSON` or `cohortFromCSV` for indicating your target and control cohort tables.
    - You can use `createControlCohortInverse` or `cohortFromCohortTable` for generating control tables.

3. **Run the Study**: Execute the study by using the CohortContrast functions.
```
pathToResults = getwd()

################################################################################
#
# Initiate the database connection
#
#################################################################################

user <- Sys.getenv("DB_USERNAME") #TODO
pw <- Sys.getenv("DB_PASSWORD") #TODO
server <- stringr::str_c(Sys.getenv("DB_HOST"), "/", Sys.getenv("DB_NAME")) #TODO
port <- Sys.getenv("DB_PORT") #TODO

cdmSchema <-
  Sys.getenv("OHDSI_CDM") #TODO # Schema which contains the OHDSI Common Data Model
cdmVocabSchema <-
  Sys.getenv("OHDSI_VOCAB") #TODO # Schema which contains the OHDSI Common Data Model vocabulary tables.
cdmResultsSchema <-
  Sys.getenv("OHDSI_RESULTS") #TODO # Schema which will contain the final results
writeSchema <-
  Sys.getenv("OHDSI_WRITE") #TODO # Schema for temporary tables, will be deleted
writePrefix <- "cc_"

cohortsTableSchemaName = cdmResultsSchema
cohortsTableName = 'cohort'
targetCohortId = 568
controlCohortId = 571

db = DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USERNAME"),
  password = Sys.getenv("DB_PASSWORD"),
  port  = port
)

cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  achillesSchema = cdmResultsSchema,
  writeSchema = c(schema = writeSchema, prefix = writePrefix),
)

################################################################################
#
# CDM target and control modula
#
################################################################################

targetTable <- CohortContrast::cohortFromCohortTable(cdm = cdm, db = db,
   tableName = cohortsTableName, schemaName = cdmResultsSchema, cohortId = targetCohortId)
   
 controlTable <- CohortContrast::cohortFromCohortTable(cdm = cdm, db = db,
  tableName = cohortsTableName, schemaName = cdmResultsSchema, cohortId = controlCohortId)
################################################################################
#
# Execute
#
################################################################################

data = CohortContrast::CohortContrast(
  cdm,
  targetTable = targetTable,
  controlTable = controlTable,
  pathToResults = getwd(),
  domainsIncluded = c(
    "Drug",
    "Condition",
    "Measurement",
    "Observation",
    "Procedure",
    "Visit",
    "Visit detail"
  ),
  prevalenceCutOff = 2.5,
  topK = FALSE, # Number of features to export
  presenceFilter = 0.2, # 0-1, percentage of people who must have the chosen feature present
  complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
  createC2TInput = FALSE,
  runZTests = TRUE,
  runLogitTests = FALSE,
  createOutputFiles = TRUE)
  
 CohortContrast::runCohortContrastGUI(
 pathToResults = pathToResults
)
 ```

## Outputs

The CohortContrast package generates the following outputs:

1. Running `CohortContrast` returns a list of tables (patient level summarised data for target and control) as well as saves the object. These can be analysed in the GUI.
2. Using GUI with `runCohortContrastGUI` generates plots as well as saves the last state of your analysis in the GUI.
