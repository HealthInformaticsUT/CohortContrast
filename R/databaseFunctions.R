# Cohort creation
# TODO: Should we change 0 values with 1'ns so that we would get some kind of prevalence, i think it is stupid

#' Create relations of cohorts (target and control) to the database
#'
#' @param connection DatabaseConnector object: connection to the database
#' @param dbms DatabaseConnector connection database dialect
#' @param cdmSchema OHDSI CDM data schema
#' @param cdmVocabSchema OHDSI CDM vocabulary schema
#' @param cdmTmpSchema OHDSI CDM temp schema, user has write permissions
#' @param pathToResults Path to the neede subfolders such as 'inst'
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
                           domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure"),
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

    if (readFromCSV) {
      #################################
      #   CCCCC     SSSSS   VV    VV
      #  CC       SS        VV    VV
      # CC          SSSS     VV  VV
      #  CC            SS     VVVV
      #   CCCCC   SSSSS        VV
      #################################
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = tolower(studyName)
      )
      if (nudgeTarget != FALSE) {
        insertedCSV = dplyr::mutate(
          insertedCSV,
          cohort_start_date = dplyr::if_else(
            cohort_definition_id == 2,
            as.Date(cohort_start_date) + nudgeTarget,
            as.Date(cohort_start_date)
          )
        )
        insertedCSV = dplyr::mutate(
          insertedCSV,
          cohort_end_date = dplyr::if_else(
            cohort_definition_id == 2,
            as.Date(cohort_end_date) + nudgeTarget,
            as.Date(cohort_end_date)
          )
        )
      }
      if (nudgeControl != FALSE) {
        insertedCSV = dplyr::mutate(
          insertedCSV,
          cohort_start_date = dplyr::if_else(
            cohort_definition_id == 1,
            as.Date(cohort_start_date) + nudgeTarget,
            as.Date(cohort_start_date)
          )
        )
        insertedCSV = dplyr::mutate(
          insertedCSV,
          cohort_end_date = dplyr::if_else(
            cohort_definition_id == 1,
            as.Date(cohort_end_date) + nudgeTarget,
            as.Date(cohort_end_date)
          )
        )
      }
      DatabaseConnector::insertTable(
        connection = connection,
        databaseSchema = cdmTmpSchema,
        tableName = tolower(studyName),
        data = insertedCSV,
        dropTableIfExists = TRUE,
        createTable = TRUE,
        tempTable = FALSE
      )


      printCustomMessage(paste(
        "Creation of table",
        cdmTmpSchema,
        ".",
        tolower(studyName),
        "EXECUTED!",
        sep = " "
      ))
    }
    else {
      #########################################
      #  JJJJJJ     SSSSS  OOOO    NN    NN
      #      JJ   SS      OO  OO   NNN   NN
      #      JJ    SSSS  OO    OO  NN N  NN
      # JJ   JJ       SS  OO  OO   NN  N NN
      #  JJJJJ    SSSSS    OOOO    NN   NNN
      #########################################
      for (i in 1:length(stateNamesJSON)) {
        cohortJson <- insertedJSONs[i]
        cohortName <- stateNamesJSON[i]
        # creating cohorts
        cohortExpression <-
          CirceR::cohortExpressionFromJson(cohortJson)

        cohortSql <-
          CirceR::buildCohortQuery(cohortExpression,
                                   options = CirceR::createGenerateOptions(generateStats = FALSE))

        cohortsToCreate <-
          rbind(
            cohortsToCreate,
            data.frame(
              cohortId = i,
              cohortName = cohortName,
              sql = cohortSql,
              stringsAsFactors = FALSE
            )
          )
      }
      ############################################################################
      #
      # Generate the saved states in database
      #
      ############################################################################
      # Create the cohort tables to hold the cohort generation results
      cohortTableNames <-
        CohortGenerator::getCohortTableNames(cohortTable = studyName)
      CohortGenerator::createCohortTables(
        connection = connection,
        cohortDatabaseSchema = cdmTmpSchema,
        cohortTableNames = cohortTableNames
      )
      # Generate the cohorts
      generateCohortSet(
        connection = connection,
        cdmDatabaseSchema = cdmSchema,
        cdmVocabSchema = cdmVocabSchema,
        cohortDatabaseSchema = cdmTmpSchema,
        cohortTableNames = cohortTableNames,
        cohortDefinitionSet = cohortsToCreate
      )

      # If need to nudge
      if (nudgeTarget != FALSE) {
        sql <- SqlRender::translate(
          targetDialect = dbms,
          sql = SqlRender::render(
            sql = "UPDATE @cdmTmpSchema.@cohortTable SET cohort_start_date = CASE WHEN cohort_definition_id = 2 THEN DATEADD(day, @nudgeTarget, cohort_start_date) ELSE cohort_start_date END, cohort_end_date = CASE WHEN cohort_definition_id = 2 THEN DATEADD(day, @nudgeTarget, cohort_end_date) ELSE cohort_end_date END;",
            cohortTable = studyName,
            cdmTmpSchema = cdmTmpSchema,
            nudgeTarget = nudgeTarget
          )

        )
        DatabaseConnector::executeSql(connection,
                                      sql)

        printCustomMessage("Nudging target cohort EXECUTED!")

      }
      if (nudgeControl != FALSE) {
        sql <- SqlRender::translate(
          targetDialect = dbms,
          sql = SqlRender::render(
            sql = "UPDATE @cdmTmpSchema.@cohortTable SET cohort_start_date = CASE WHEN cohort_definition_id = 1 THEN DATEADD(day, @nudgeControl, cohort_start_date) ELSE cohort_start_date END, cohort_end_date = CASE WHEN cohort_definition_id = 1 THEN DATEADD(day, @nudgeControl, cohort_end_date) ELSE cohort_end_date END;",
            cohortTable = studyName,
            cdmTmpSchema = cdmTmpSchema,
            nudgeControl = nudgeControl
          )

        )
        DatabaseConnector::executeSql(connection,
                                      sql)

        printCustomMessage("Nudging control cohort EXECUTED!")

      }

    }

    ####################################
    #  DDDDD    RRRRR    UU  UU  GGGGG
    #  DD   DD  RR  RRR  UU  UU GG
    #  DD    DD RRRRRR   UU  UU GG GGGG
    #  DD   DD  RR   RR  UU  UU GG   GG
    #  DDDDD    RR    RR  UUUU   GGGGG
    ####################################
    if ("Drug" %in% domainsIncluded) {
      ############################################################################
      #
      # Generate drug prevalence table
      #
      ############################################################################
      printCustomMessage("Creating temp tables, this can take a while...")

      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_drug_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = customSQLRender(
            "SELECT * INTO @cdmTmpSchema.cohortcontrast_drug_prevalence FROM (SELECT c.cohort_definition_id, c.drug_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT)) AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.drug_exposure d ON g.subject_id = d.person_id WHERE d.drug_exposure_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.drug_concept_id, cohort_size.total_count) a;",
            dbms
          ),
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_drug_prevalence EXECUTED!")

      ############################################################################
      #
      # Generate drug prevalence difference ratio table
      #
      ############################################################################

      # Updated because some errors were occurring not checked
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_drug_prevalence_differences"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = customSQLRender(
            "SELECT * INTO @cdmTmpSchema.cohortcontrast_drug_prevalence_differences FROM ( SELECT CASE WHEN a.drug_concept_id IS NOT NULL THEN a.drug_concept_id ELSE b.drug_concept_id END AS drug_concept_id, CASE WHEN CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END = 0 THEN NULL ELSE (CASE WHEN b.prevalence IS NOT NULL THEN b.prevalence ELSE 0 END - CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence END) / CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END END        AS prevalence_difference_ratio, b.person_count as target_subject_count, a.person_count as control_subject_count FROM (SELECT * FROM @cdmTmpSchema.cohortcontrast_drug_prevalence WHERE cohort_definition_id = 1) a FULL OUTER JOIN (SELECT * FROM @cdmTmpSchema.cohortcontrast_drug_prevalence WHERE cohort_definition_id = 2) b ON a.drug_concept_id = b.drug_concept_id order by prevalence_difference_ratio desc LIMIT @domainLimit) tmp;",
            dbms
          ),
          cdmTmpSchema = cdmTmpSchema,
          domainLimit = domainLimit
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_drug_prevalence_differences EXECUTED!")


      ############################################################################
      #
      # Generate drug prevalence table
      #
      ############################################################################

      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_drug_patient_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = customSQLRender(
            "SELECT * INTO  @cdmTmpSchema.cohortcontrast_drug_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.drug_concept_id, COUNT(DISTINCT c.drug_exposure_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.drug_concept_id, d.drug_exposure_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.drug_exposure d ON g.subject_id = d.person_id WHERE d.drug_exposure_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT drug_exposure_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.drug_exposure d ON g.subject_id = d.person_id WHERE d.drug_exposure_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.drug_concept_id, person_cohort.total_count) tmp;",
            dbms
          ),
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_drug_prevalence EXECUTED!")
    }
    ############################################################################
    #   CCCC   OOOOO   NN   NN  DDDDD   IIII  TTTTTT  IIII   OOOOO   NN   NN
    #  CC    OO     OO NNN  NN  DD  DD   II     TT     II  OO     OO NNN  NN
    # CC     OO     OO NNNN NN  DD   DD  II     TT     II  OO     OO NNNN NN
    #  CC    OO     OO NN NNNN  DD  DD   II     TT     II  OO     OO NN NNNN
    #   CCCC   OOOOO   NN  NNN  DDDDD   IIII    TT    IIII   OOOOO   NN   NN
    ############################################################################
    if ("Condition" %in% domainsIncluded) {
      ############################################################################
      #
      # Generate condition prevalence table
      #
      ############################################################################
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_condition_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = customSQLRender(
            "SELECT * INTO @cdmTmpSchema.cohortcontrast_condition_prevalence FROM (SELECT c.cohort_definition_id, c.condition_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT))AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.condition_occurrence d ON g.subject_id = d.person_id WHERE d.condition_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.condition_concept_id, cohort_size.total_count) a;",
            dbms
          ),
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_condition_prevalence EXECUTED!")

      ############################################################################
      #
      # Generate condition prevalence difference ratio table
      #
      ############################################################################

      # Updated because some errors were occurring not checked
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_condition_prevalence_differences"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = customSQLRender(
            "SELECT * INTO @cdmTmpSchema.cohortcontrast_condition_prevalence_differences FROM ( SELECT CASE WHEN a.condition_concept_id IS NOT NULL THEN a.condition_concept_id ELSE b.condition_concept_id END AS condition_concept_id, CASE WHEN CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END = 0 THEN NULL ELSE (CASE WHEN b.prevalence IS NOT NULL THEN b.prevalence ELSE 0 END - CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence END) / CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END END        AS prevalence_difference_ratio,  b.person_count as target_subject_count, a.person_count as control_subject_count FROM (SELECT * FROM @cdmTmpSchema.cohortcontrast_condition_prevalence WHERE cohort_definition_id = 1) a FULL OUTER JOIN (SELECT * FROM @cdmTmpSchema.cohortcontrast_condition_prevalence WHERE cohort_definition_id = 2) b ON a.condition_concept_id = b.condition_concept_id order by prevalence_difference_ratio desc LIMIT @domainLimit) tmp;",
            dbms
          ),
          cdmTmpSchema = cdmTmpSchema,
          domainLimit = domainLimit
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage(
        "Creation of table cohortcontrast_condition_prevalence_differences EXECUTED!"
      )



      ############################################################################
      #
      # Generate condition prevalence table
      #
      ############################################################################
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_condition_patient_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_condition_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.condition_concept_id, COUNT(DISTINCT c.condition_occurrence_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.condition_concept_id, d.condition_occurrence_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.condition_occurrence d ON g.subject_id = d.person_id WHERE d.condition_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT condition_occurrence_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.condition_occurrence d ON g.subject_id = d.person_id WHERE d.condition_start_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.condition_concept_id, person_cohort.total_count) tmp;",
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_condition_prevalence EXECUTED!")
    }
    ################################################################################################################
    # MMMM   MMMM  EEEEEEEE    AAAA   SSSSSS  UU    UU  RRRRRR   EEEEEEEE  MMMM   MMMM  EEEEEEEE  NN    NN   TTTTTT
    # MM MM MM MM  EE         AA  AA  SS      UU    UU  RR   RR  EE        MM MM MM MM  EE        NNN   NN     TT
    # MM  MMM  MM  EEEEE     AAAAAAA   SSSSS  UU    UU  RRRRRR   EEEEE     MM  MMM  MM  EEEEE     NN NN NN     TT
    # MM       MM  EE        AA   AA      SS  UU    UU  RR  RR   EE        MM       MM  EE        NN   NNN     TT
    # MM       MM  EEEEEEEE  AA   AA  SSSSSS   UUUUUU   RR   RR  EEEEEEEE  MM       MM  EEEEEEEE  NN    NN     TT
    ################################################################################################################
    if ("Measurement" %in% domainsIncluded) {
      ############################################################################
      #
      # Generate measurement prevalence table
      #
      ############################################################################

      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_measurement_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO @cdmTmpSchema.cohortcontrast_measurement_prevalence FROM (SELECT c.cohort_definition_id, c.measurement_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT))AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.measurement d ON g.subject_id = d.person_id WHERE d.measurement_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.measurement_concept_id, cohort_size.total_count) a;",
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_measurement_prevalence EXECUTED!")

      ############################################################################
      #
      # Generate measurement prevalence difference ratio table
      #
      ############################################################################

      # Updated because some errors were occurring not checked
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_measurement_prevalence_differences"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO @cdmTmpSchema.cohortcontrast_measurement_prevalence_differences FROM ( SELECT CASE WHEN a.measurement_concept_id IS NOT NULL THEN a.measurement_concept_id ELSE b.measurement_concept_id END AS measurement_concept_id, CASE WHEN CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END = 0 THEN NULL ELSE (CASE WHEN b.prevalence IS NOT NULL THEN b.prevalence ELSE 0 END - CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence END) / CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END END        AS prevalence_difference_ratio,  b.person_count as target_subject_count, a.person_count as control_subject_count FROM (SELECT * FROM @cdmTmpSchema.cohortcontrast_measurement_prevalence WHERE cohort_definition_id = 1) a FULL OUTER JOIN (SELECT * FROM @cdmTmpSchema.cohortcontrast_measurement_prevalence WHERE cohort_definition_id = 2) b ON a.measurement_concept_id = b.measurement_concept_id order by prevalence_difference_ratio desc LIMIT @domainLimit) tmp;",
          cdmTmpSchema = cdmTmpSchema,
          domainLimit = domainLimit
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage(
        "Creation of table cohortcontrast_measurement_prevalence_differences EXECUTED!"
      )



      ############################################################################
      #
      # Generate measurement prevalence table
      #
      ############################################################################
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_measurement_patient_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_measurement_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.measurement_concept_id, COUNT(DISTINCT c.measurement_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.measurement_concept_id, d.measurement_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.measurement d ON g.subject_id = d.person_id WHERE d.measurement_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT measurement_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.measurement d ON g.subject_id = d.person_id WHERE d.measurement_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.measurement_concept_id, person_cohort.total_count) tmp;",
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_measurement_prevalence EXECUTED!")
    }
    ############################################################################
    #  PPPPP  RRRRR     OOOOO    CCCC  EEEEE  DDDDD  UU   UU  RRRRR   EEEEE
    #  PP  PP RR  RR  OO     OO CC     EE     DD  DD UU   UU  RR  RR  EE
    #  PPPPP  RRRRR   OO     OO C      EEEE   DD  DD UU   UU  RRRRR   EEEE
    #  PP     RR  RR  OO     OO CC     EE     DD  DD UU   UU  RR  RR  EE
    #  PP     RR   RR   OOOOO    CCCC  EEEEE  DDDDD   UUUUU   RR   RR EEEEE
    ############################################################################
    if ("Procedure" %in% domainsIncluded) {
      ############################################################################
      #
      # Generate procedure prevalence table
      #
      ############################################################################

      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_procedure_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO @cdmTmpSchema.cohortcontrast_procedure_prevalence FROM (SELECT c.cohort_definition_id, c.procedure_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT))AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.procedure_occurrence d ON g.subject_id = d.person_id WHERE d.procedure_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.procedure_concept_id, cohort_size.total_count) a;",
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_procedure_prevalence EXECUTED!")

      ############################################################################
      #
      # Generate procedure prevalence difference ratio table
      #
      ############################################################################

      # Updated because some errors were occurring not checked
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_procedure_prevalence_differences"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO @cdmTmpSchema.cohortcontrast_procedure_prevalence_differences FROM ( SELECT CASE WHEN a.procedure_concept_id IS NOT NULL THEN a.procedure_concept_id ELSE b.procedure_concept_id END AS procedure_concept_id, CASE WHEN CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END = 0 THEN NULL ELSE (CASE WHEN b.prevalence IS NOT NULL THEN b.prevalence ELSE 0 END - CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence END) / CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END END        AS prevalence_difference_ratio,  b.person_count as target_subject_count, a.person_count as control_subject_count FROM (SELECT * FROM @cdmTmpSchema.cohortcontrast_procedure_prevalence WHERE cohort_definition_id = 1) a FULL OUTER JOIN (SELECT * FROM @cdmTmpSchema.cohortcontrast_procedure_prevalence WHERE cohort_definition_id = 2) b ON a.procedure_concept_id = b.procedure_concept_id order by prevalence_difference_ratio desc LIMIT @domainLimit) tmp;",
          cdmTmpSchema = cdmTmpSchema,
          domainLimit = domainLimit
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage(
        "Creation of table cohortcontrast_procedure_prevalence_differences EXECUTED!"
      )


      ############################################################################
      #
      # Generate procedure prevalence table
      #
      ############################################################################
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_procedure_patient_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_procedure_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.procedure_concept_id, COUNT(DISTINCT c.procedure_occurrence_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.procedure_concept_id, d.procedure_occurrence_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.procedure_occurrence d ON g.subject_id = d.person_id WHERE d.procedure_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT procedure_occurrence_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.procedure_occurrence d ON g.subject_id = d.person_id WHERE d.procedure_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.procedure_concept_id, person_cohort.total_count) tmp;",
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_procedure_prevalence EXECUTED!")
    }
    ############################################################################################
    #    OOOOO   BBBBB   SSSS  EEEEE  RRRRR  VV     VV   AAA   TTTTTT IIIIII   OOOOO   NN    NN
    #  OO     OO BB  BB SS     EE     RR  RR  VV   VV   AA AA    TT     II   OO     OO NNN   NN
    #  OO     OO BBBBB   SSSS  EEEE   RRRRR    VV VV   AA   AA   TT     II   OO     OO NNNN. NN
    #  OO     OO BB  BB     SS EE     RR  RR    VVV    AAAAAAA   TT     II   OO     OO NN NN NN
    #    OOOOO   BBBBB  SSSSS  EEEEE  RR   RR    V     AA   AA   TT   IIIIII   OOOOO   NN   NNN
    ############################################################################################
    if ("Observation" %in% domainsIncluded) {
      ############################################################################
      #
      # Generate observation prevalence table
      #
      ############################################################################

      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_observation_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO @cdmTmpSchema.cohortcontrast_observation_prevalence FROM (SELECT c.cohort_definition_id, c.observation_concept_id, (COUNT(DISTINCT c.person_id) / CAST(cohort_size.total_count AS FLOAT))AS prevalence, COUNT(DISTINCT c.person_id) AS person_count FROM (SELECT * FROM @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.observation d ON g.subject_id = d.person_id WHERE d.observation_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS total_count FROM @cdmTmpSchema.@cohortTable GROUP BY cohort_definition_id) cohort_size ON c.cohort_definition_id = cohort_size.cohort_definition_id GROUP BY c.cohort_definition_id, c.observation_concept_id, cohort_size.total_count) a;",
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_observation_prevalence EXECUTED!")

      ############################################################################
      #
      # Generate observation prevalence difference ratio table
      #
      ############################################################################

      # Updated because some errors were occurring not checked
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_observation_prevalence_differences"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO @cdmTmpSchema.cohortcontrast_observation_prevalence_differences FROM ( SELECT CASE WHEN a.observation_concept_id IS NOT NULL THEN a.observation_concept_id ELSE b.observation_concept_id END AS observation_concept_id, CASE WHEN CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END = 0 THEN NULL ELSE (CASE WHEN b.prevalence IS NOT NULL THEN b.prevalence ELSE 0 END - CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence END) / CASE WHEN a.prevalence IS NOT NULL THEN a.prevalence ELSE 0 END END        AS prevalence_difference_ratio,  b.person_count as target_subject_count, a.person_count as control_subject_count FROM (SELECT * FROM @cdmTmpSchema.cohortcontrast_observation_prevalence WHERE cohort_definition_id = 1) a FULL OUTER JOIN (SELECT * FROM @cdmTmpSchema.cohortcontrast_observation_prevalence WHERE cohort_definition_id = 2) b ON a.observation_concept_id = b.observation_concept_id order by prevalence_difference_ratio desc LIMIT @domainLimit) tmp;",
          cdmTmpSchema = cdmTmpSchema,
          domainLimit = domainLimit
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage(
        "Creation of table cohortcontrast_observation_prevalence_differences EXECUTED!"
      )



      ############################################################################
      #
      # Generate drug prevalence table
      #
      ############################################################################
      dropRelation(
        connection = connection ,
        dbms = dbms,
        schema = cdmTmpSchema,
        relationName = "cohortcontrast_observation_patient_prevalence"
      )

      sql1 <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = "SELECT * INTO  @cdmTmpSchema.cohortcontrast_observation_patient_prevalence FROM (SELECT c.cohort_definition_id, c.person_id, c.observation_concept_id, COUNT(DISTINCT c.observation_id)  AS prevalence FROM (SELECT g.cohort_definition_id, g.subject_id AS person_id, d.observation_concept_id, d.observation_id FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.observation d ON g.subject_id = d.person_id WHERE d.observation_date BETWEEN g.cohort_start_date AND g.cohort_end_date) c JOIN (SELECT cohort_definition_id, subject_id, COUNT(DISTINCT observation_id) AS total_count FROM  @cdmTmpSchema.@cohortTable g JOIN @cdmSchema.observation d ON g.subject_id = d.person_id WHERE d.observation_date BETWEEN g.cohort_start_date AND g.cohort_end_date GROUP BY cohort_definition_id, subject_id) person_cohort ON c.cohort_definition_id = person_cohort.cohort_definition_id AND c.person_id = person_cohort.subject_id GROUP BY c.cohort_definition_id, c.person_id, c.observation_concept_id, person_cohort.total_count) tmp;",
          cohortTable = studyName,
          cdmTmpSchema = cdmTmpSchema,
          cdmSchema = cdmSchema
        )

      )
      DatabaseConnector::executeSql(connection,
                                    sql1)

      printCustomMessage("Creation of table cohortcontrast_observation_patient_prevalence EXECUTED!")

    }

  }
  ############################################################################
  #  ######  ##  ###   ##    ###    ##
  #  ##      ##  ####  ##    ###    ##
  #  ####    ##  ## ## ##   ## ##   ##
  #  ##      ##  ##  ####  #######  ##
  #  ##      ##  ##   ### ##     ## #####
  ############################################################################


  ############################################################################
  #
  # Query data from database
  #
  ############################################################################
  printCustomMessage("Querying prevalence difference data from database ...")

  sql1 = ""
  sql2 = ""
  c = 0
  if ("Drug" %in% domainsIncluded) {
    sql1 <-
      paste(
        sql1,
        "SELECT drug_concept_id as concept_id, v.concept_name, prevalence_difference_ratio, target_subject_count, control_subject_count FROM @cdmTmpSchema.cohortcontrast_drug_prevalence_differences LEFT JOIN @cdmVocabSchema.concept v ON drug_concept_id = v.concept_id",
        sep = ""
      )

    sql2 <-
      paste(
        sql2,
        "SELECT cohort_definition_id, person_id, drug_concept_id as concept_id, v.concept_name, prevalence, 'drug_exposure' as heritage FROM @cdmTmpSchema.cohortcontrast_drug_patient_prevalence LEFT JOIN @cdmVocabSchema.concept v ON drug_concept_id = v.concept_id",
        sep = ""
      )

    c = c + 1
  }
  if ("Measurement" %in% domainsIncluded) {
    if (c != 0) {
      sql1 <- paste(sql1, " UNION ", sep = "")

      sql2 <- paste(sql2, " UNION ", sep = "")

    }
    sql1 <-
      paste(
        sql1,
        "SELECT measurement_concept_id as concept_id, v.concept_name, prevalence_difference_ratio, target_subject_count, control_subject_count FROM @cdmTmpSchema.cohortcontrast_measurement_prevalence_differences LEFT JOIN @cdmVocabSchema.concept v ON measurement_concept_id = v.concept_id",
        sep = ""
      )

    sql2 <-
      paste(
        sql2,
        "SELECT cohort_definition_id, person_id, measurement_concept_id as concept_id, v.concept_name, prevalence, 'measurement' as heritage FROM @cdmTmpSchema.cohortcontrast_measurement_patient_prevalence LEFT JOIN @cdmVocabSchema.concept v ON measurement_concept_id = v.concept_id",
        sep = ""
      )

    c = c + 1
  }
  if ("Condition" %in% domainsIncluded) {
    if (c != 0) {
      sql1 <- paste(sql1, " UNION ", sep = "")

      sql2 <- paste(sql2, " UNION ", sep = "")

    }
    sql1 <-
      paste(
        sql1,
        "SELECT condition_concept_id as concept_id, v.concept_name, prevalence_difference_ratio, target_subject_count, control_subject_count FROM @cdmTmpSchema.cohortcontrast_condition_prevalence_differences LEFT JOIN @cdmVocabSchema.concept v ON condition_concept_id = v.concept_id",
        sep = ""
      )

    sql2 <-
      paste(
        sql2,
        "SELECT cohort_definition_id, person_id, condition_concept_id as concept_id, v.concept_name, prevalence, 'condition_occurrence' as heritage FROM @cdmTmpSchema.cohortcontrast_condition_patient_prevalence LEFT JOIN @cdmVocabSchema.concept v ON condition_concept_id = v.concept_id",
        sep = ""
      )

    c = c + 1
  }
  if ("Observation" %in% domainsIncluded) {
    if (c != 0) {
      sql1 <- paste(sql1, " UNION ", sep = "")

      sql2 <- paste(sql2, " UNION ", sep = "")

    }
    sql1 <-
      paste(
        sql1,
        "SELECT observation_concept_id as concept_id, v.concept_name, prevalence_difference_ratio, target_subject_count, control_subject_count FROM @cdmTmpSchema.cohortcontrast_observation_prevalence_differences LEFT JOIN @cdmVocabSchema.concept v ON observation_concept_id = v.concept_id",
        sep = ""
      )

    sql2 <-
      paste(
        sql2,
        "SELECT cohort_definition_id, person_id, observation_concept_id as concept_id, v.concept_name, prevalence, 'observation' as heritage FROM @cdmTmpSchema.cohortcontrast_observation_patient_prevalence LEFT JOIN @cdmVocabSchema.concept v ON observation_concept_id = v.concept_id",
        sep = ""
      )

    c = c + 1
  }
  if ("Procedure" %in% domainsIncluded) {
    if (c != 0) {
      sql1 <- paste(sql1, " UNION ", sep = "")

      sql2 <- paste(sql2, " UNION ", sep = "")

    }
    sql1 <-
      paste(
        sql1,
        "SELECT procedure_concept_id as concept_id, v.concept_name, prevalence_difference_ratio, target_subject_count, control_subject_count FROM @cdmTmpSchema.cohortcontrast_procedure_prevalence_differences LEFT JOIN @cdmVocabSchema.concept v ON procedure_concept_id = v.concept_id",
        sep = ""
      )

    sql2 <-
      paste(
        sql2,
        "SELECT cohort_definition_id, person_id, procedure_concept_id as concept_id, v.concept_name, prevalence, 'procedure_occurrence' as heritage FROM @cdmTmpSchema.cohortcontrast_procedure_patient_prevalence LEFT JOIN @cdmVocabSchema.concept v ON procedure_concept_id = v.concept_id",
        sep = ""
      )

    c = c + 1
  }
  sql1 <- paste(sql1, ";", sep = "")

  sql2 <- paste(sql2, ";", sep = "")


  sql <-
    loadRenderTranslateSql(
      dbms = dbms,
      sql1,
      cdmTmpSchema = cdmTmpSchema,
      cdmVocabSchema = cdmVocabSchema
    )
  data_features <- DatabaseConnector::querySql(connection, sql)

  printCustomMessage("Querying person prevalence data from database ...")

  sql <-
    loadRenderTranslateSql(
      dbms = dbms,
      sql2,
      cdmTmpSchema = cdmTmpSchema,
      cdmVocabSchema = cdmVocabSchema
    )
  data_patients <- DatabaseConnector::querySql(connection, sql)

  printCustomMessage("Querying initial data from database ...")

  sql <-
    loadRenderTranslateSql(
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
      data_features = data_features,
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
