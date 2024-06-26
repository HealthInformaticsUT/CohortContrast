# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of CohortGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Update by Markus Haug
# For both underlying functions I added a specific parameter for vocabulary schema

#' Generate a set of cohorts, overwritten from original package CohortGenerator
#' @keywords internal
generateCohortSet <- function(connectionDetails = NULL,
                              connection = NULL,
                              cdmDatabaseSchema,
                              tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                              cohortDatabaseSchema = cdmDatabaseSchema,
                              cdmVocabSchema = cdmVocabSchema,
                              cohortTableNames = getCohortTableNames(),
                              cohortDefinitionSet = NULL,
                              stopOnError = TRUE,
                              incremental = FALSE,
                              incrementalFolder = NULL) {
  checkmate::assertDataFrame(cohortDefinitionSet,
                             min.rows = 1,
                             col.names = "named")
  checkmate::assertNames(colnames(cohortDefinitionSet),
                         must.include = c("cohortId",
                                          "cohortName",
                                          "sql"))
  if (is.null(connection) && is.null(connectionDetails)) {
    stop("You must provide either a database connection or the connection details.")
  }
  if (incremental) {
    if (is.null(incrementalFolder)) {
      stop("Must specify incrementalFolder when incremental = TRUE")
    }
    if (!file.exists(incrementalFolder)) {
      dir.create(incrementalFolder, recursive = TRUE)
    }
  }

  start <- Sys.time()
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  # Verify the cohort tables exist and if they do not
  # stop the generation process
  tableExistsFlagList <-
    lapply(
      cohortTableNames,
      FUN = function(x) {
        x <- FALSE
      }
    )
  tables <-
    DatabaseConnector::getTableNames(connection, cohortDatabaseSchema)
  for (i in 1:length(cohortTableNames)) {
    if (toupper(cohortTableNames[i]) %in% toupper(tables)) {
      tableExistsFlagList[i] <- TRUE
    }
  }

  if (!all(unlist(tableExistsFlagList, use.names = FALSE))) {
    errorMsg <- "The following tables have not been created: \n"
    for (i in 1:length(cohortTableNames)) {
      if (!tableExistsFlagList[[i]]) {
        errorMsg <- paste0(errorMsg, "   - ", cohortTableNames[i], "\n")
      }
    }
    errorMsg <-
      paste(
        errorMsg,
        "Please use the createCohortTables function to ensure all tables exist before generating cohorts.",
        sep = "\n"
      )
    stop(errorMsg)
  }


  if (incremental) {
    cohortDefinitionSet$checksum <-
      computeChecksum(cohortDefinitionSet$sql)
    recordKeepingFile <-
      file.path(incrementalFolder, "GeneratedCohorts.csv")
  }

  # Create the cluster
  cluster <- ParallelLogger::makeCluster(numberOfThreads = 1)
  on.exit(ParallelLogger::stopCluster(cluster), add = TRUE)

  # Apply the generation operation to the cluster
  cohortsGenerated <- ParallelLogger::clusterApply(
    cluster,
    cohortDefinitionSet$cohortId,
    generateCohort,
    cohortDefinitionSet = cohortDefinitionSet,
    connection = connection,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cdmVocabSchema = cdmVocabSchema,
    tempEmulationSchema = tempEmulationSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    stopIfError = stopOnError,
    incremental = incremental,
    recordKeepingFile = recordKeepingFile,
    stopOnError = stopOnError,
    progressBar = TRUE
  )

  # Convert the list to a data frame
  cohortsGenerated <- do.call(rbind, cohortsGenerated)

  delta <- Sys.time() - start
  writeLines(paste(
    "Generating cohort set took",
    round(delta, 2),
    attr(delta, "units")
  ))
  invisible(cohortsGenerated)
}

#' Generates a cohort, overwritten from original package CohortGenerator
#'
#' @keywords internal
generateCohort <- function(cohortId = NULL,
                           cohortDefinitionSet,
                           connection = NULL,
                           connectionDetails = NULL,
                           cdmDatabaseSchema,
                           cdmVocabSchema,
                           tempEmulationSchema,
                           cohortDatabaseSchema,
                           cohortTableNames,
                           stopIfError = TRUE,
                           incremental,
                           recordKeepingFile) {
  # Get the index of the cohort record for the current cohortId
  i <- which(cohortDefinitionSet$cohortId == cohortId)
  cohortName <- cohortDefinitionSet$cohortName[i]
  if (!incremental || isTaskRequired(
    cohortId = cohortDefinitionSet$cohortId[i],
    checksum = cohortDefinitionSet$checksum[i],
    recordKeepingFile = recordKeepingFile
  )) {
    if (is.null(connection)) {
      # Establish the connection and ensure the cleanup is performed
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    }
    ParallelLogger::logInfo(i,
                            "/",
                            nrow(cohortDefinitionSet),
                            "- Generating cohort: ",
                            cohortName)
    sql <- cohortDefinitionSet$sql[i]
    sql <- SqlRender::render(
      sql = sql,
      cdm_database_schema = cdmDatabaseSchema,
      vocabulary_database_schema = cdmVocabSchema,
      target_database_schema = cohortDatabaseSchema,
      results_database_schema = cohortDatabaseSchema,
      target_cohort_table = cohortTableNames$cohortTable,
      target_cohort_id = cohortDefinitionSet$cohortId[i],
      results_database_schema.cohort_inclusion = paste(
        cohortDatabaseSchema,
        cohortTableNames$cohortInclusionTable,
        sep = "."
      ),
      results_database_schema.cohort_inclusion_result = paste(
        cohortDatabaseSchema,
        cohortTableNames$cohortInclusionResultTable,
        sep = "."
      ),
      results_database_schema.cohort_inclusion_stats = paste(
        cohortDatabaseSchema,
        cohortTableNames$cohortInclusionStatsTable,
        sep = "."
      ),
      results_database_schema.cohort_summary_stats = paste(
        cohortDatabaseSchema,
        cohortTableNames$cohortSummaryStatsTable,
        sep = "."
      ),
      results_database_schema.cohort_censor_stats = paste(
        cohortDatabaseSchema,
        cohortTableNames$cohortCensorStatsTable,
        sep = "."
      ),
      warnOnMissingParameters = FALSE
    )
    sql <- SqlRender::translate(
      sql = sql,
      targetDialect = connection@dbms,
      tempEmulationSchema = tempEmulationSchema
    )

    # Helper function used within the tryCatch block below
    runCohortSql <-
      function(sql,
               startTime,
               incremental,
               cohortId,
               checksum,
               recordKeepingFile) {
        DatabaseConnector::executeSql(connection, sql)
        endTime <- lubridate::now()

        if (incremental) {
          recordTasksDone(
            cohortId = cohortId,
            checksum = checksum,
            recordKeepingFile = recordKeepingFile
          )
        }

        return(list(
          generationStatus = "COMPLETE",
          startTime = startTime,
          endTime = endTime
        ))
      }

    # This syntax is strange so leaving a note.
    # generationInfo is assigned based on the evaluation of
    # the expression in the tryCatch(). If there is an error, the
    # outermost assignment will assign generationInfo based on the return
    # value in the error() block. If the expr() function evaluates without
    # error, the inner most assignment of generationInfo will take place.
    generationInfo <- tryCatch(
      expr = {
        startTime <- lubridate::now()
        generationInfo <- runCohortSql(
          sql = sql,
          startTime = startTime,
          incremental = incremental,
          cohortId = cohortDefinitionSet$cohortId[i],
          checksum = cohortDefinitionSet$checksum[i],
          recordKeepingFile = recordKeepingFile
        )
      },
      error = function(e) {
        endTime <- lubridate::now()
        ParallelLogger::logError("An error occurred while generating cohortName = ",
                                 cohortName,
                                 ". Error: ",
                                 e)
        if (stopIfError) {
          stop()
        }
        return(list(
          generationStatus = "FAILED",
          startTime = startTime,
          endTime = endTime
        ))
      }
    )
  } else {
    generationInfo <- list(
      generationStatus = "SKIPPED",
      startTime = NA,
      endTime = NA
    )
  }

  summary <- data.frame(
    cohortId = cohortId,
    cohortName = cohortName,
    generationStatus = generationInfo$generationStatus,
    startTime = generationInfo$startTime,
    endTime = generationInfo$endTime
  )
  return(summary)
}
