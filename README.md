# CohortContrast

CohortContrast is a package designed to facilitate the comparison between cohorts in specified domains across all OMOP CDM datasets. It enables users to analyze and visualize the contrast between target and control cohorts effectively.

## Usage

To use CohortContrast, follow these steps to configure your environment and input data:

1. **Credentials**: Make sure you can create a connection to your OHDS CDM instance using CDMConnector package.

2a. **Cohort inputs from custom tables**:
   - Populate a table with the target patients' cohort data. Register its schema and name as a variable.
   - Populate a table with the control patients' cohort data. Register its schema and name as a variable.
   
   **OR**

2b. **Cohorts from a cohort table**:
   - Generate cohorts using CohortConstructor package or ATLAS tool. Register the cohorts' table schema and name as a variable. Make sure of the ids the cohorts have.
   
   **OR**

2c. **Having only the target cohort**:
   - Generate the target cohort as in 2a or 2b. The control cohort can be later automatically generated using the CohortContrast package with inverse controls or patient matching.

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

targetTableName = NULL
controlTableName = NULL
targetTableSchemaName = NULL
controlTableSchemaName = NULL

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

cdm <- CohortContrast::createCohortContrastCohorts(
  cdm,
  db,
  targetTableName = NULL,
  controlTableName = NULL,
  targetTableSchemaName = NULL,
  controlTableSchemaName = NULL,
  cohortsTableSchemaName = cdmResultsSchema,
  cohortsTableName = cohortsTableName,
  targetCohortId = targetCohortId,
  controlCohortId = controlCohortId,
  nudgeTarget = FALSE,
  nudgeControl = FALSE,
  useInverseControls = FALSE,
  useTargetMatching = FALSE
)

################################################################################
#
# Execute
#
################################################################################

data = CohortContrast::CohortContrast(
  cdm,
  pathToResults,
  studyName,
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
  topDogs = FALSE, # Number of features to export
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

1. **PCA Analysis Plot**: A principal component analysis plot to visualize the data dimensionality reduction.
2. **Feature Prevalence Heatmap**: A heatmap showing the prevalence of selected features across the target cohort.
3. **Selected Features**: A list of features selected for contrasting the cohorts.
4. **Dataframe with Selected Features**: A dataframe containing the selected features, ready for further analysis or usage in the Cohort2Trajectory package.
5. **Feature Prevalence Heatmap**: A heatmap showing the presence of selected features across the target cohort.****
