studyName <- "TempStudy" # TODOx
pathToResults <- getwd()   # TODO
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
  domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure"),
  generateTables = TRUE,
  readFromCSV = FALSE,
  prevalenceCutOff = 0,
  topDogs = 20,
  presenceFilter = 0.1,
  removeOutliers = FALSE
)


data$pcaPlot1
data$heatmapPlot1
data$selectedFeatures


data$trajectoryData$COHORT_DEFINITION_ID = trimws(data$trajectoryData$COHORT_DEFINITION_ID)
stateCohortLabels = setdiff(unique(data$trajectoryData$COHORT_DEFINITION_ID), c("0"))
stateCohortLabels = Cohort2Trajectory::sanitize_filenames(stateCohortLabels)
allowedStatesList = Cohort2Trajectory::createStateList(stateCohortLabels) # Creates a list allowing all transitions from each state



############################
#   CCCC   2222   TTTTTTTT
#  CC  CC     22  T  TT. T
#  CC       222      TT
#  CC  CC  22        TT
#   CCCC   22222     TT
############################


Cohort2Trajectory::Cohort2Trajectory(
  studyName = paste(studyName, "CC", sep = "_"),
  runSavedStudy = F,
  stateCohortPriorityOrder = stateCohortLabels,
  # Priority order of states
  stateCohortMandatory = NULL,
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
  outOfCohortAllowed = T,
  pathToResults = pathToResults,
  useCDM = FALSE,
  pathToData = NULL,
  trajectoryDataObject = data$trajectoryData,
  allowedStatesList = allowedStatesList,
  oocFix = "None",
  mergeStates = FALSE,
  mergeThreshold = 0.5
)
length(unique(data$trajectoryData$SUBJECT_ID))
#####################################################################################################
# TTTTTTTT. VV       VV   II.  ZZZZZZZZZ
# T  TT  T.  VV     VV.   II.  Z  ZZZZZ
#.   TT.      VV   VV     II     ZZZZ
#.   TT.       VV VV.     II    ZZZZ. Z
#.   TT.         V        II.  ZZZZZZZZ
#####################################################################################################



devtools::install_github("https://github.com/MaarjaPajusalu/trajectoryViz")
trajectoryViz::trajectoryViz()
#####################################################################################################
#   CCCC   2222   TTTTTTTT       A     NNN    NN      A     LL     YY    YY  SSSSSS  EEEEEE RRRRRR
#  CC  CC     22  T  TT  T     AA AA.  NN N   NN    AA AA   LL      YY  YY  SS       EE     RR  RR
#  CC       222      TT       AA.  AA. NN  N  NN.  AA   AA. LL        YY     SSSSS   EEEE   RRRRRR
#  CC  CC  22        TT      AAAAAAAAA NN   N NN  AAAAAAAAA LL        YY         SS  EE     RR RR
#   CCCC   22222     TT      AA.    AA NN    NNN  AA     AA LLLLLL    YY    SSSSSS   EEEEEE RR  RR
#####################################################################################################

################################################################################
#
# Code for starting the application
#
################################################################################

# Fill if you have your C2T table in a server or want to use remote SQL server for computing
#
# server = NULL # TODO if exists
# database = NULL # TODO if exists
# password = NULL # TODO if exists
# user = NULL # TODO if exists
# port = NULL # TODO if exists
# dbms = NULL # TODO if exists
# schema = NULL # TODO if exists
# tableName = NULL # TODO if exists
# pathToDriver = './Drivers'
#
# # Possible modifications
#
# removeID = TRUE
# removeSTART = FALSE
# removeEXIT = FALSE
# fixGender = TRUE
# showOccurrance = FALSE
#
# # Run app
#
# healthTrajectories::runGUI(
#   server = server,
#   database = database,
#   password = password,
#   user = user,
#   port = port,
#   dbms = dbms,
#   schema = schema,
#   pathToDriver = pathToDriver,
#   tableName = tableName,
#   removeID = removeID,
#   removeSTART = removeSTART,
#   removeEXIT = removeEXIT,
#   fixGender = fixGender,
#   showOccurrance = showOccurrance
# )

################################################################################
#
# Disconnect
#
################################################################################

DatabaseConnector::disconnect(connection)
