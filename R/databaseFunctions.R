# Cohort creation

#' Create relations of cohorts (target and control) to the database
#'
#' @param connection DatabaseConnector object: connection to the database
#' @param dbms DatabaseConnector connection database dialect
#' @param cdmSchema OHDSI CDM data schema
#' @param cdmVocabSchema OHDSI CDM vocabulary schema
#' @param cdmTmpSchema OHDSI CDM temp schema, user has write permissions
#' @param pathToResults Path to the mandatory subdirectories such as 'inst'
#' @param studyName Name of the study being run, will be used in file and relation names
#' @param generateTables Boolean for recreating table
#' @param domainsIncluded list of CDM domains to include
#' @param domainLimit nr of instances allowed from a domain
#' @param readFromCSV boolean > if TRUE file from ./inst/CSV will be used as cohort data, otherwise JSONs will be used.
#' @param nudgeTarget number of days you would like to nudge the target cohort start day
#' @param nudgeControl number of days you would like to nudge the control cohort start day
#' @keywords internal
generateTables <- function(connection,
                           dbms,
                           cdmSchema = "ohdsi_cdm",
                           cdmVocabSchema = "ohdsi_vocab",
                           cdmTmpSchema = "ohdsi_tmp",
                           pathToResults = getwd(),
                           studyName = "SampleStudy",
                           generateTables = TRUE,
                           domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure", "Visit", "Visit detail"),
                           domainLimit = 1000,
                           readFromCSV = FALSE,
                           nudgeTarget = FALSE,
                           nudgeControl = FALSE) {
  if (generateTables) {
    cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
    # loading settings
    settings <- loadJsons(studyName, pathToResults)
    stateNamesJSON <- settings$stateNamesJSON
    insertedJSONs <- settings$insertedJSONs
    insertedCSV <- settings$insertedCSV
    # Placeholder for inverse control
    inverseControl = FALSE


    if (readFromCSV) {
      # Read the CSV file and return inverseControl value
      inverseControl = processCSVData(connection, dbms, cdmTmpSchema, studyName, insertedCSV, nudgeTarget, nudgeControl)
    }
    else {
      inverseControl = processJSONData(connection, dbms, cdmTmpSchema, studyName, cdmSchema, cdmVocabSchema, stateNamesJSON, insertedJSONs, nudgeTarget, nudgeControl)
    }
    if (inverseControl) {
      createInverseSelfControls(connection, dbms, cdmTmpSchema, studyName, cdmSchema, inverseControl)
    }

    ####################################
    # For "Drug" domain
    sqlQuery1 = "SELECT * INTO @cdmTmpSchema.cohortcontrast_drug_exposure_prevalence FROM (SELECT c.cohort_definition_id, c.drug_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT)) AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.drug_exposure d ON g.subject_id = d.person_id WHERE d.drug_exposure_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.drug_concept_id, cohort_size.total_count) a;"
    sqlQuery2 = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_drug_exposure_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.drug_concept_id, COUNT(DISTINCT c.drug_exposure_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.drug_concept_id, d.drug_exposure_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.drug_exposure d ON g.subject_id = d.person_id WHERE d.drug_exposure_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT drug_exposure_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.drug_exposure d ON g.subject_id = d.person_id WHERE d.drug_exposure_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.drug_concept_id, person_cohort.total_count) tmp;"

    generatePrevalenceTables(connection, dbms, cdmTmpSchema, studyName, cdmSchema, "Drug", domainsIncluded,
                         c("cohortcontrast_drug_exposure_prevalence", "cohortcontrast_drug_exposure_patient_prevalence"),
                         c(sqlQuery1, sqlQuery2))

    sqlQuery1 = "SELECT * INTO @cdmTmpSchema.cohortcontrast_condition_occurrence_prevalence FROM (SELECT c.cohort_definition_id, c.condition_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT))AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.condition_occurrence d ON g.subject_id = d.person_id WHERE d.condition_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.condition_concept_id, cohort_size.total_count) a;"
    sqlQuery2 = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_condition_occurrence_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.condition_concept_id, COUNT(DISTINCT c.condition_occurrence_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.condition_concept_id, d.condition_occurrence_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.condition_occurrence d ON g.subject_id = d.person_id WHERE d.condition_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT condition_occurrence_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.condition_occurrence d ON g.subject_id = d.person_id WHERE d.condition_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.condition_concept_id, person_cohort.total_count) tmp;"

    # For "Condition" domain
    generatePrevalenceTables(connection, dbms, cdmTmpSchema, studyName, cdmSchema, "Condition", domainsIncluded,
                         c("cohortcontrast_condition_occurrence_prevalence", "cohortcontrast_condition_occurrence_patient_prevalence"),
                         c(sqlQuery1, sqlQuery2))

    sqlQuery1 = "SELECT * INTO @cdmTmpSchema.cohortcontrast_measurement_prevalence FROM (SELECT c.cohort_definition_id, c.measurement_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT))AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.measurement d ON g.subject_id = d.person_id WHERE d.measurement_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.measurement_concept_id, cohort_size.total_count) a;"
    sqlQuery2 = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_measurement_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.measurement_concept_id, COUNT(DISTINCT c.measurement_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.measurement_concept_id, d.measurement_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.measurement d ON g.subject_id = d.person_id WHERE d.measurement_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT measurement_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.measurement d ON g.subject_id = d.person_id WHERE d.measurement_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.measurement_concept_id, person_cohort.total_count) tmp;"

    # For "Measurement" domain
    generatePrevalenceTables(connection, dbms, cdmTmpSchema, studyName, cdmSchema, "Measurement", domainsIncluded,
                         c("cohortcontrast_measurement_prevalence", "cohortcontrast_measurement_patient_prevalence"),
                         c(sqlQuery1, sqlQuery2))



    sqlQuery1 = "SELECT * INTO @cdmTmpSchema.cohortcontrast_procedure_occurrence_prevalence FROM (SELECT c.cohort_definition_id, c.procedure_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT))AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.procedure_occurrence d ON g.subject_id = d.person_id WHERE d.procedure_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.procedure_concept_id, cohort_size.total_count) a;"
    sqlQuery2 = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_procedure_occurrence_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.procedure_concept_id, COUNT(DISTINCT c.procedure_occurrence_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.procedure_concept_id, d.procedure_occurrence_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.procedure_occurrence d ON g.subject_id = d.person_id WHERE d.procedure_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT procedure_occurrence_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.procedure_occurrence d ON g.subject_id = d.person_id WHERE d.procedure_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.procedure_concept_id, person_cohort.total_count) tmp;"

    # For "Procedure" domain
    generatePrevalenceTables(connection, dbms, cdmTmpSchema, studyName, cdmSchema, "Procedure", domainsIncluded,
                         c("cohortcontrast_procedure_occurrence_prevalence", "cohortcontrast_procedure_occurrence_patient_prevalence"),
                         c(sqlQuery1, sqlQuery2))

    # For "Observation" domain
    sqlQuery1 = "SELECT * INTO @cdmTmpSchema.cohortcontrast_observation_prevalence FROM (SELECT c.cohort_definition_id, c.observation_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT))AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.observation d ON g.subject_id = d.person_id WHERE d.observation_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.observation_concept_id, cohort_size.total_count) a;"
    sqlQuery2 = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_observation_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.observation_concept_id, COUNT(DISTINCT c.observation_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.observation_concept_id, d.observation_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.observation d ON g.subject_id = d.person_id WHERE d.observation_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT observation_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.observation d ON g.subject_id = d.person_id WHERE d.observation_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.observation_concept_id, person_cohort.total_count) tmp;"

    generatePrevalenceTables(connection, dbms, cdmTmpSchema, studyName, cdmSchema, "Observation", domainsIncluded,
                         c("cohortcontrast_observation_prevalence", "cohortcontrast_observation_patient_prevalence"),
                         c(sqlQuery1, sqlQuery2))

    # For "Visit" domain
    sqlQuery1 = "SELECT * INTO @cdmTmpSchema.cohortcontrast_visit_occurrence_prevalence FROM (SELECT c.cohort_definition_id, c.visit_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT)) AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.visit_occurrence d ON g.subject_id = d.person_id WHERE d.visit_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.visit_concept_id, cohort_size.total_count) a;"
    sqlQuery2 = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_visit_occurrence_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.visit_concept_id, COUNT(DISTINCT c.visit_occurrence_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.visit_concept_id, d.visit_occurrence_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.visit_occurrence d ON g.subject_id = d.person_id WHERE d.visit_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT visit_occurrence_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.visit_occurrence d ON g.subject_id = d.person_id WHERE d.visit_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.visit_concept_id, person_cohort.total_count) tmp;"

    generatePrevalenceTables(connection, dbms, cdmTmpSchema, studyName, cdmSchema, "Visit", domainsIncluded,
                         c("cohortcontrast_visit_occurrence_prevalence", "cohortcontrast_visit_occurrence_patient_prevalence"),
                         c(sqlQuery1, sqlQuery2))
    # For "Visit detail" domain
    sqlQuery1 = "SELECT * INTO @cdmTmpSchema.cohortcontrast_visit_detail_prevalence FROM (SELECT c.cohort_definition_id, c.visit_detail_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT)) AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.visit_detail d ON g.subject_id = d.person_id WHERE d.visit_detail_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.visit_detail_concept_id, cohort_size.total_count) a;"
    sqlQuery2 = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_visit_detail_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.visit_detail_concept_id, COUNT(DISTINCT c.visit_detail_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.visit_detail_concept_id, d.visit_detail_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.visit_detail d ON g.subject_id = d.person_id WHERE d.visit_detail_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT visit_detail_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.visit_detail d ON g.subject_id = d.person_id WHERE d.visit_detail_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.visit_detail_concept_id, person_cohort.total_count) tmp;"

    generatePrevalenceTables(connection, dbms, cdmTmpSchema, studyName, cdmSchema, "Visit detail", domainsIncluded,
                         c("cohortcontrast_visit_detail_prevalence", "cohortcontrast_visit_detail_patient_prevalence"),
                         c(sqlQuery1, sqlQuery2))
  }
  ############################################################################
  #
  # Query data from database
  #
  ############################################################################

  printCustomMessage("Querying prevalence difference data from database ...")

  sqlQuery = ""
  c = 0

  domains <- c("Drug", "Measurement", "Condition", "Observation", "Visit", "Visit detail", "Procedure")

  for (domain in domains) {
    result <- generateSqlQuery(domain, sqlQuery, c, domainsIncluded)
    sqlQuery <- result$sql
    c <- result$counter
  }

  sqlQuery <- paste(sqlQuery, ";", sep = "")

  printCustomMessage("Querying person prevalence data from database ...")

  sql <- loadRenderTranslateSql(
  dbms = dbms,
  sqlQuery,
  cdmTmpSchema = cdmTmpSchema,
  cdmVocabSchema = cdmVocabSchema
  )
  data_patients <- DatabaseConnector::querySql(connection, sql)

  printCustomMessage("Querying initial data from database ...")

  sql <- loadRenderTranslateSql(
  dbms = dbms,
  "SELECT * FROM @cdmTmpSchema.@studyTable;",
  cdmTmpSchema = cdmTmpSchema,
  studyTable = tolower(studyName)
  )
  data_initial <- DatabaseConnector::querySql(connection, sql)


  printCustomMessage("Querying person data from database ...")
  # Get person data
  # Render and translate the SQL query
  sql_query <- SqlRender::render(sql = "SELECT person_id, gender_concept_id, year_of_birth FROM @cdmSchema.person;",
                                 cdmSchema = cdmSchema)
  sql_query_translated <-
    SqlRender::translate(sql = sql_query, targetDialect = dbms)

  # Execute the SQL query
  data_person <-
    DatabaseConnector::querySql(connection, sql_query_translated)

  printCustomMessage("Data imported from the database!")
  # Setting names for each list element
  return(
    list(
  #data_features = data_features,
      data_patients = data_patients,
      data_initial = data_initial,
      data_person = data_person
    )
  )
}



#' Function for deleting temporary tables from user's db
#'
#' @param connection Connection to the database (package DatabaseConnector)
#' @param dbms Database dialect
#' @param schema Schema in which the targeted table resides
#' @param relationName Name of the targeted table which will be dropped
#' @keywords internal
dropRelation <-
  function(connection,
           dbms = "postgresql",
           schema = "",
           relationName) {
    printCustomMessage(paste(
      "Start execution of: DROP TABLE IF EXISTS ",
      ifelse(
        schema == "",
        relationName,
        paste(schema,
              ".",
              relationName, sep = "")
      ),
      " !",
      sep = ""
    ))
    if (schema == "") {
      DatabaseConnector::executeSql(connection,
                                    SqlRender::translate(
                                      targetDialect = dbms,
                                      sql = SqlRender::render(sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @relationName;",
                                                              relationName = relationName)
                                    ))
    }
    else {
      DatabaseConnector::executeSql(connection,
                                    SqlRender::translate(
                                      targetDialect = dbms,
                                      sql = SqlRender::render(
                                        sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @cdmTmpSchema.@relationName;",
                                        cdmTmpSchema = schema,
                                        relationName = relationName
                                      )
                                    ))
    }

    printCustomMessage(paste("DROP TABLE ",
                             schema,
                             ".",
                             relationName,
                             " EXECUTED!",
                             sep = ""))
  }

processCSVData <- function(connection, dbms, cdmTmpSchema, studyName, insertedCSV, nudgeTarget, nudgeControl) {
  dropRelation(
    connection = connection,
    dbms = dbms,
    schema = cdmTmpSchema,
    relationName = tolower(studyName)
  )

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cdmTmpSchema,
    tableName = tolower(studyName),
    data = insertedCSV,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE
  )

  nudgeCohorts(connection, dbms, cdmTmpSchema, studyName, nudgeTarget, nudgeControl)


  inverseControl <- if (length(unique(insertedCSV$COHORT)) == 1) TRUE else FALSE

  printCustomMessage(paste(
    "Creation of table",
    cdmTmpSchema,
    ".",
    tolower(studyName),
    "EXECUTED!",
    sep = " "
  ))

  return(inverseControl)
}

# Function to create cohorts
createCohorts <- function(stateNamesJSON, insertedJSONs) {
  cohortsToCreate <- data.frame()
  for (i in 1:length(stateNamesJSON)) {
    cohortJson <- insertedJSONs[i]
    cohortName <- stateNamesJSON[i]
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <- rbind(cohortsToCreate, data.frame(cohortId = i, cohortName = cohortName, sql = cohortSql, stringsAsFactors = FALSE))
  }
  return(cohortsToCreate)
}

# Function to nudge cohorts
nudgeCohorts <- function(connection, dbms, cdmTmpSchema, studyName, nudgeTarget, nudgeControl) {
  if (nudgeTarget != FALSE) {
    sql <- SqlRender::translate(targetDialect = dbms, sql = SqlRender::render(sql = "UPDATE @cdmTmpSchema.@cohortTable SET cohort_start_date = CASE WHEN cohort_definition_id = 2 THEN DATEADD(day, @nudgeTarget, cohort_start_date) ELSE cohort_start_date END, cohort_end_date = CASE WHEN cohort_definition_id = 2 THEN DATEADD(day, @nudgeTarget, cohort_end_date) ELSE cohort_end_date END;", cohortTable = studyName, cdmTmpSchema = cdmTmpSchema, nudgeTarget = nudgeTarget))
    DatabaseConnector::executeSql(connection, sql)
    printCustomMessage("Nudging target cohort EXECUTED!")
  }
  if (nudgeControl != FALSE) {
    sql <- SqlRender::translate(targetDialect = dbms, sql = SqlRender::render(sql = "UPDATE @cdmTmpSchema.@cohortTable SET cohort_start_date = CASE WHEN cohort_definition_id = 1 THEN DATEADD(day, @nudgeControl, cohort_start_date) ELSE cohort_start_date END, cohort_end_date = CASE WHEN cohort_definition_id = 1 THEN DATEADD(day, @nudgeControl, cohort_end_date) ELSE cohort_end_date END;", cohortTable = studyName, cdmTmpSchema = cdmTmpSchema, nudgeControl = nudgeControl))
    DatabaseConnector::executeSql(connection, sql)
    printCustomMessage("Nudging control cohort EXECUTED!")
  }
}

# Function to check for the need of inverse control
checkInverseControl <- function(connection, dbms, cdmTmpSchema, studyName) {
  sql <- loadRenderTranslateSql(dbms = dbms, "SELECT * FROM @cdmTmpSchema.@studyTable;", cdmTmpSchema = cdmTmpSchema, studyTable = tolower(studyName))
  data_initial <- DatabaseConnector::querySql(connection, sql)
  return(length(unique(data_initial$COHORT_DEFINITION_ID)) == 1)
}

# Main function
processJSONData <- function(connection, dbms, cdmTmpSchema, studyName, cdmSchema, cdmVocabSchema, stateNamesJSON, insertedJSONs, nudgeTarget, nudgeControl) {
  cohortsToCreate <- createCohorts(stateNamesJSON, insertedJSONs)
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = studyName)
  CohortGenerator::createCohortTables(connection = connection, cohortDatabaseSchema = cdmTmpSchema, cohortTableNames = cohortTableNames)
  generateCohortSet(connection = connection, cdmDatabaseSchema = cdmSchema, cdmVocabSchema = cdmVocabSchema, cohortDatabaseSchema = cdmTmpSchema, cohortTableNames = cohortTableNames, cohortDefinitionSet = cohortsToCreate)
  nudgeCohorts(connection, dbms, cdmTmpSchema, studyName, nudgeTarget, nudgeControl)
  inverseControl <- checkInverseControl(connection, dbms, cdmTmpSchema, studyName)
  return(inverseControl)
}

createInverseSelfControls <- function(connection, dbms, cdmTmpSchema, studyName, cdmSchema, inverseControl) {
  if (inverseControl) {
    printCustomMessage("Creating inverse self-controls as control cohort is missing!")
    sql <- SqlRender::translate(
      targetDialect = dbms,
      sql = SqlRender::render(
        sql = "UPDATE @cdmTmpSchema.@cohortTable SET cohort_definition_id = 2 WHERE cohort_definition_id = 1;",
        cohortTable = studyName,
        cdmTmpSchema = cdmTmpSchema
      )
    )
    DatabaseConnector::executeSql(connection, sql)
    update_cohort_table(connection, cdmTmpSchema, studyName, cdmSchema)
  }
}

generatePrevalenceTables <- function(connection, dbms, cdmTmpSchema, studyName, cdmSchema, domain, domainsIncluded, relationNames, sqlQueries) {
  if (domain %in% domainsIncluded) {
    for (i in seq_along(relationNames)) {

      dropRelation(
        connection = connection,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = relationNames[i]
      )

      sql <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = customSQLRender(
            sqlQueries[i],
            dbms
          ),
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )
      )

      DatabaseConnector::executeSql(connection, sql)

      printCustomMessage(paste("Creation of table", relationNames[i], "EXECUTED!"))
    }
  }
}
generateSqlQuery <- function(domain, sql, counter, domainsIncluded) {
  if (domain %in% domainsIncluded) {
    if (counter != 0) {
      sql <- paste(sql, " UNION ", sep = "")
    }
    domainShort = domain
    if (domain == "Visit") {
      domain = 'visit_occurrence'
      domainShort = 'visit'
    }
    else if (domain == "Visit detail") {
      domain = 'visit_detail'
      domainShort = 'visit_detail'
    }
    else if (domain == "Drug") {
      domain = 'drug_exposure'
      domainShort = 'drug'
    }
    else if (domain == "Measurement") {
      domain = 'measurement'
      domainShort = 'measurement'
    }
    else if (domain == "Procedure") {
      domain = 'procedure_occurrence'
      domainShort = 'procedure'
    }
    else if (domain == "Observationl") {
      domain = 'observation'
      domainShort = 'observation'
    }
    else if (domain == "Condition") {
      domain = 'condition_occurrence'
      domainShort = 'condition'
    }

    sql <- paste(
      sql,
      sprintf(
        "SELECT cohort_definition_id, person_id, %s_concept_id as concept_id, v.concept_name, prevalence, '%s' as heritage FROM @cdmTmpSchema.cohortcontrast_%s_patient_prevalence LEFT JOIN @cdmVocabSchema.concept v ON %s_concept_id = v.concept_id",
        domainShort,
        domain,
        domain,
        domainShort
      ),
      sep = ""
    )


    counter <- counter + 1
  }

  return(list(sql = sql, counter = counter))
}
