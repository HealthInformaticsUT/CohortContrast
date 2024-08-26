devtools::install_github("HealthInformaticsUT/CohortContrast") # Run for installing the HEAD
#library(CohortContrast)

devtools::load_all()
renv::restore()

studyName <- "TestCohortContrast" # TODO
pathToResults <- getwd()   # TODO should be a path to the directory containing the inst directory
################################################################################
#
# Initiate the database connection
#
################################################################################
pathToDriver <- './Drivers'
dbms <- "postgresql" #TODO
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
writePrefix <- paste("cc_", tolower(studyName), "_", sep = "")

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
  writeSchema = c(schema = writeSchema, prefix = writePrefix)
)

################################################################################
#
# Execute
#
################################################################################

##################
#   CCCC    CCCC
#  CC  CC  CC  CC
#  CC      CC
#  CC  CC  CC  CC
#   CCCC    CCCC
##################
data = CohortContrast(
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
  readFromCSV = FALSE,
  prevalenceCutOff = 2.5,
  topDogs = FALSE, # Number of features to export
  presenceFilter = 0.2, # 0-1, percentage of people who must have the chosen feature present
  complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
  nudgeTarget = FALSE, # nudge target cohort start date (days)
  nudgeControl = FALSE,# nudge control cohort start date (days)
  createC2TInput = TRUE,
  useInverseControls = TRUE,
  useTargetMatching = FALSE,
  runZTests = TRUE,
  runLogitTests = TRUE)

# GUI

runGUI(
  pathToResults = pathToResults,
  studyName = studyName
)
############################
#   CCCC   2222   TTTTTTTT
#  CC  CC     22  T  TT. T
#  CC       222      TT
#  CC  CC  22        TT
#   CCCC   22222     TT
############################

# devtools::install_github("HealthInformaticsUT/Cohort2Trajectory") # Run for installing the HEAD
#
library(Cohort2Trajectory)
stateCohortLabels = setdiff(unique(data$resultList$trajectoryData$COHORT_DEFINITION_ID), c("0"))
stateCohortLabels = as.vector(Cohort2Trajectory::sanitize_filenames(stateCohortLabels))
allowedStatesList = Cohort2Trajectory::createStateList(stateCohortLabels) # Creates a list allowing all transitions from each state

dataC2T = Cohort2Trajectory::Cohort2Trajectory(
  studyName = paste(studyName, "CohortContrast", sep = "_"),
  runSavedStudy = F,
  stateCohortPriorityOrder = stateCohortLabels,
  # Priority order of states
  stateCohortMandatory = c("Negative_for_intraepithelial_lesion_or_malignancy"),
  # Mandatory states
  stateCohortAbsorbing = NULL,
  # Absorbing states
  ##############################################################################
  # stateSelectionTypes
  # 1 - First occurring
  # 2 - Largest overlap
  # 3 - Priority ordering
  ##############################################################################
  stateSelectionType = 1,
  ##############################################################################
  # trajectoryType
  # 0 - Discrete time
  # 1 - Continuous time
  ##############################################################################
  trajectoryType = 1,
  outOfCohortAllowed = F,
  pathToResults = pathToResults,
  useCDM = FALSE,
  pathToData = NULL,
  trajectoryDataObject = data$resultList$trajectoryData,
  allowedStatesList = allowedStatesList,
  oocFix = "None",
  mergeStates = FALSE,
  mergeThreshold = 0.5
)

#####################################################################################################
# TTTTTTTT. VV       VV   II.  ZZZZZZZ
# T  TT  T.  VV     VV.   II.  Z  ZZZ
#.   TT.      VV   VV     II     ZZ
#.   TT.       VV VV.     II    ZZ   Z
#.   TT.         V        II.  ZZZZZZZZ
#####################################################################################################
## Package by Maarja Pajusalu https://github.com/HealthInformaticsUT/TrajectoryViz import the Cohort2Trajectory output for visualisations

# devtools::install_github("https://github.com/HealthInformaticsUT/TrajectoryViz")
# library(TrajectoryViz)
# trajectoryViz()

################################################################################
#
# Disconnect
#
################################################################################

#DatabaseConnector::disconnect(connection)
