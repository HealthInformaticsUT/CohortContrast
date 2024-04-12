devtools::install_github("HealthInformaticsUT/CohortContrast") # Run for installing the HEAD

studyName <- "TempStudy" # TODO
pathToResults <- getwd()   # TODO should be a path to the directory containing the inst directory
################################################################################
#
# Initiate the database connection
#
################################################################################
pathToDriver <- './Drivers'
dbms <- "postgresql" #TODO
user <- 'user' #TODO
pw <- "pw" #TODO
server <- 'localhost/database' #TODO
port <- '5432' #TODO

cdmSchema <-
  "ohdsi_cdm" #TODO # Schema which contains the OHDSI Common Data Model
cdmVocabSchema <-
  "ohdsi_vocab" #TODO # Schema which contains the OHDSI Common Data Model vocabulary tables.
cdmTmpSchema <-
  "ohdsi_temp" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <-
  "ohdsi_results" #TODO # Schema which will contain the final results

connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = server,
    user = user,
    password = pw,
    port = port,
    pathToDriver = "./Drivers/"
  )

connection <- DatabaseConnector::connect(connectionDetails)

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
  connection,
  connectionDetails,
  cdmSchema,
  cdmVocabSchema,
  cdmTmpSchema,
  pathToResults,
  studyName,
  domainsIncluded =   c("Drug", "Condition", "Measurement", "Observation", "Procedure"),
  generateTables = TRUE,
  readFromCSV = FALSE,
  prevalenceCutOff = 1.2,
  topDogs = 100, # Number of features to export
  presenceFilter = 0.05, # 0-1, percentage of people who must have the chosen feature present
  removeOutliers = FALSE, # Remove people who have outlier-like feature values according to PCA
  complementaryMappingTable = FALSE, # A table for manual concept_id and concept_name mapping (merge)
  nudgeTarget = FALSE, # nudge target cohort start date (days)
  nudgeControl = FALSE,# nudge control cohort start date (days)
  runPCA = TRUE,
  runPCAClusters = 4
)

data$resultList$pcaPlot1
data$resultList$heatmapPlot1
data$resultList$selectedFeatureNames

# # You can also use a complementary mapping table to map some concept ids to same name
# # You can use vectors for that
# complementaryMappingTable = createComplementaryMappingTable(conceptIds = c(1,2,3), conceptNames = c("Drug1", "Drug2", "Drug1"))


# Heatmap 2
heatmapResults <- createHeatmap(
  data = data,
  cohortDefinitionId = 2,
  prevalenceRatioThreshold = 5, # Example threshold
  prevalenceThreshold = 0.05, # Example threshold
  cdmSchema = cdmSchema,
  connection = connection,
  complementaryMappingTable = FALSE
)

# Extracting the heatmap plot and other results
heatmapPlot <- heatmapResults$heatmapPlot
targetMatrix <- heatmapResults$targetMatrix
personData <- heatmapResults$personData


############################
#   CCCC   2222   TTTTTTTT
#  CC  CC     22  T  TT. T
#  CC       222      TT
#  CC  CC  22        TT
#   CCCC   22222     TT
############################

# devtools::install_github("HealthInformaticsUT/Cohort2Trajectory") # Run for installing the HEAD
#
# library(Cohort2Trajectory)
#
# stateCohortLabels = setdiff(unique(data$resultList$trajectoryData$COHORT_DEFINITION_ID), c("0"))
# stateCohortLabels = as.vector(Cohort2Trajectory::sanitize_filenames(stateCohortLabels))
# allowedStatesList = Cohort2Trajectory::createStateList(stateCohortLabels) # Creates a list allowing all transitions from each state
#
# dataC2T = Cohort2Trajectory::Cohort2Trajectory(
#   studyName = paste(studyName, "CohortContrast", sep = "_"),
#   runSavedStudy = F,
#   stateCohortPriorityOrder = stateCohortLabels,
#   # Priority order of states
#   stateCohortMandatory = NULL,
#   # Mandatory states
#   stateCohortAbsorbing = NULL,
#   # Absorbing states
#   ##############################################################################
#   # stateSelectionTypes
#   # 1 - First occurring
#   # 2 - Largest overlap
#   # 3 - Priority ordering
#   ##############################################################################
#   stateSelectionType = 1,
#   ##############################################################################
#   # trajectoryType
#   # 0 - Discrete time
#   # 1 - Continuous time
#   ##############################################################################
#   trajectoryType = 1,
#   outOfCohortAllowed = T,
#   pathToResults = pathToResults,
#   useCDM = FALSE,
#   pathToData = NULL,
#   trajectoryDataObject = data$resultList$trajectoryData,
#   allowedStatesList = allowedStatesList,
#   oocFix = "None",
#   mergeStates = FALSE,
#   mergeThreshold = 0.5
# )

#####################################################################################################
# TTTTTTTT. VV       VV   II.  ZZZZZZZZZ
# T  TT  T.  VV     VV.   II.  Z  ZZZZZ
#.   TT.      VV   VV     II     ZZZZ
#.   TT.       VV VV.     II    ZZZZ. Z
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
