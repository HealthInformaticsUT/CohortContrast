################################################################################
#
# Some regularly used functions to help code cleanness
#
################################################################################


#' Load and translate SQL file or an explicit SQL query to desired dialect.
#'
#' @param sql SQL file name or SQL query
#' @param warnOnMissingParameters Should a warning be raised when parameters provided to this function do not appear in the parameterized SQL that is being rendered? By default, this is TRUE.
#' @param output Should there be a .sql file created of the result
#' @param outputFile Name of the output file
#' @keywords internal
loadRenderTranslateSql <- function(sql,
                                   dbms = "postgresql",
                                   warnOnMissingParameters = TRUE,
                                   output = FALSE,
                                   outputFile,
                                   ...) {
  if (grepl('.sql', sql)) {
    pathToSql <- paste("inst/SQL/", sql, sep = "")
    parameterizedSql <-
      readChar(pathToSql, file.info(pathToSql)$size)[1]
  }
  else {
    parameterizedSql <- sql
  }
  renderedSql <-
    SqlRender::render(sql = parameterizedSql, warnOnMissingParameters = warnOnMissingParameters, ...)
  renderedSql <-
    SqlRender::translate(sql = renderedSql, targetDialect = dbms)

  if (output == TRUE) {
    SqlRender::writeSql(renderedSql, outputFile)
    writeLines(paste("Created file '", outputFile, "'", sep = ""))
  }

  return(renderedSql)
}

#' Function for finding NaN values in a data.frame object
#'
#' @param data SQL data.frame object
#' @keywords internal
is.nan.data.frame <- function(data) {
  do.call(cbind, lapply(data, is.nan))
}

#' Function for saving summary tables to path
#'
#' @param object Object to save
#' @param path Path to the file saved
#' @keywords internal
save_object <- function(object, path) {
  if (is.data.frame(object)) {
    utils::write.csv(object, path, row.names = FALSE)
  }
  else {
    save(object, file = path)
  }
}


#' Function for controlling whether patient exists in a cohort
#'
#' @param data A dataframe object with SUBJECT_ID values
#' @param id The subject ID
#' @keywords internal
idExists <- function(data, id) {
  if (as.character(id) %in% unique(as.character(data$SUBJECT_ID)))
    return(TRUE)
  return(FALSE)
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
    ParallelLogger::logInfo(paste(
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

    ParallelLogger::logInfo(paste("DROP TABLE ",
                                  schema,
                                  ".",
                                  relationName,
                                  " EXECUTED!",
                                  sep = ""))
  }

#' Function which converts text formatted JSON to digestible JSON
#'
#' @param input Text formatted JSON which needs conversion to digestible JSON
#' @keywords internal
.toJSON <- function(input, pretty = FALSE) {
  return(RJSONIO::toJSON(
    x = input,
    digits = 23,
    pretty = pretty
  ))
}

#' Function which creates mandatory subdirectories and files to the pathToResults directory
#'
#' @param pathToResults Path to the package results
#' @keywords internal
createMandatorySubDirs <- function(pathToResults) {
  dir.create(file.path(pathToResults, "tmp"), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/tmp', sep = ""), 'datasets'), showWarnings = FALSE)

  dir.create(file.path(pathToResults, "inst"), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/inst', sep = ""), 'JSON'), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/inst', sep = ""), 'CSV'), showWarnings = FALSE)
}


# Function to normalize and scale a vector
scale_to_1_0 <- function(x) {
  # Normalize to 0-1
  #x_norm <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  x_norm <- log10(x + 1)
  # Scale to -1 to 0
  return(x_norm)
}


printCustomMessage <- function(message) {
  # Calculate the length of the message to dynamically create the border
  border <- paste(rep("-", nchar(message)), collapse = "")

  # Get the current timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Print the messages and timestamp
  cat(border, "\n")
  cat(message, "\n")
  cat(border, "\n")
  cat("Timestamp: ", timestamp, "\n")
  cat("\n") # Print a new line
}


customSQLRender <- function(sql, dbms) {
  if (dbms == 'sqlite' && startsWith(sql, "SELECT * INTO")) {
    # Pattern to match "schemaName.tableName"
    pattern <-
      "SELECT \\* INTO\\s+([a-zA-Z0-9_@]+)\\.([a-zA-Z0-9_]+)\\s+FROM"


    # Replacement template for SQLite
    replacementTemplate <- "CREATE TABLE \\1.\\2 AS SELECT * FROM"

    # Use gsub to replace based on the captured groups in the pattern
    sql <- gsub(pattern, replacementTemplate, sql, perl = TRUE)
  }
  return(sql)
}

createComplementaryMappingTable <-
  function(conceptIds, conceptNames) {
    # Check if vectors are of the same length
    if (length(conceptIds) != length(conceptNames)) {
      print("Error: Vectors 'conceptIds' and 'conceptNames' must be of the same length.")
      return(NULL)
    }

    # Check if concept names are all strings
    if (!all(sapply(conceptNames, is.character))) {
      print("Error: All 'conceptNames' must be strings.")
      return(NULL)
    }

    # Check if all concept IDs are unique
    if (length(unique(conceptIds)) != length(conceptIds)) {
      print("Error: All 'conceptIds' must be unique.")
      return(NULL)
    }

    # Create the dataframe
    complementaryMappingTable <-
      data.frame(
        CONCEPT_ID = conceptIds,
        CONCEPT_NAME = conceptNames,
        stringsAsFactors = FALSE
      )

    return(complementaryMappingTable)
  }


get_study_names <- function(pathToResults) {
  # List all files in the specified directory
  tmpdir = paste(pathToResults,'/tmp/datasets/', sep = "" )
  files <- list.files(tmpdir, full.names = TRUE, pattern = "_CC_medData\\.rdata$")

  # Pattern to extract the study name from the filename
  study_name_pattern <- "(?<=/)([^/]+)(?=_CC_medData\\.rdata$)"

  # Extract study names from the filenames
  study_names <- str_extract(files, study_name_pattern)

  # Return the unique study names
  return(unique(study_names))
}

# Function to calculate inverse dates
calculate_inverse_dates <- function(observation_period_start_date, observation_period_end_date, cohort_start_date, cohort_end_date) {
  inverse_date_ranges <- list()

  # Check if cohort dates fully overlap with observation dates
  if (cohort_start_date <= observation_period_start_date & cohort_end_date >= observation_period_end_date) {
    return(inverse_date_ranges)  # No date range to return
  }

  # Calculate inverse date ranges
  if (cohort_start_date > observation_period_start_date) {
    inverse_date_ranges[[length(inverse_date_ranges) + 1]] <- list(observation_period_start_date, cohort_start_date - 1)
  }

  if (cohort_end_date < observation_period_end_date) {
    inverse_date_ranges[[length(inverse_date_ranges) + 1]] <- list(cohort_end_date + 1, observation_period_end_date)
  }

  return(inverse_date_ranges)
}


# Main function to update cohort table
update_cohort_table <- function(connection, cdmTmpSchema, studyName, cdmSchema) {
  # Query data from @cdmTmpSchema.@cohortTable where cohort_definition_id = 2
  sql_select <- SqlRender::translate(
    targetDialect = dbms,
    sql = SqlRender::render(
      sql = "SELECT c.subject_id, o.observation_period_start_date, o.observation_period_end_date, c.cohort_start_date, c.cohort_end_date
             FROM @cdmTmpSchema.@cohortTable c
             JOIN @cdmSchema.observation_period o ON c.subject_id = o.person_id
             WHERE c.cohort_definition_id = 2;",
      cdmTmpSchema = cdmTmpSchema,
      cohortTable = studyName,
      cdmSchema = cdmSchema
    )
  )

  data <- DatabaseConnector::querySql(connection, sql_select)


  # Iterate over each row, calculate inverse dates, and insert new rows
  for (i in 1:nrow(data)) {
    subject_id <- data[i, "SUBJECT_ID"]
    observation_period_start_date <- data[i, "OBSERVATION_PERIOD_START_DATE"]
    observation_period_end_date <- data[i, "OBSERVATION_PERIOD_END_DATE"]
    cohort_start_date <- data[i, "COHORT_START_DATE"]
    cohort_end_date <- data[i, "COHORT_END_DATE"]

    inverse_date_ranges <- calculate_inverse_dates(observation_period_start_date, observation_period_end_date, cohort_start_date, cohort_end_date)
    # Insert new rows with cohort_definition_id = 1
    for (date_range in inverse_date_ranges) {
      inverse_start_date <- date_range[[1]]
      inverse_end_date <- date_range[[2]]

      sql_insert <- SqlRender::translate(
        targetDialect = dbms,
        sql = SqlRender::render(
          sql = paste0("INSERT INTO @cdmTmpSchema.@cohortTable (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) VALUES
                        (1, ", subject_id, ", '", inverse_start_date, "', '", inverse_end_date, "');"),
          cdmTmpSchema = cdmTmpSchema,
          cohortTable = studyName
        )
      )

      DatabaseConnector::executeSql(connection, sql_insert)
      printCustomMessage("Updated cohort table for inverse control!")
    }
  }
}

#' Function to sanitize a single string
#' @param input_string A  state label name
#' @keywords internal
sanitize_single <- function(input_string) {
  safe_string <- gsub("[^A-Za-z0-9_.-]", "_", input_string)  # Corrected regex
  if (substr(safe_string, 1, 1) == '.') {
    safe_string <- paste0('_', safe_string)
  }
  return(safe_string)
}

#' Sanitize filenames not in correct format
#'
#' @param input_strings A vector of state label names
#' @keywords internal
sanitize <- function(input_strings) {
  # Apply the sanitization function to each element of the vector
  sapply(input_strings, sanitize_single)
}

