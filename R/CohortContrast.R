#' Get data from OMOP CDM specified in imported JSON files
#'
#' This function outputs a dataframe with columns SUBJECT_ID, COHORT_DEFINITION_ID, COHORT_START_DATE, COHORT_END_DATE
#' @param connection Connection to database
#' @param connectionDetails An object of class connectionDetails as created by the DatabaseConnector::createConnectionDetails function.
#' @param cdmSchema JSONs from which cohorts will be created
#' @param cdmDataSchema Schema which contains the OHDSI Common Data Model.
#' @param cdmVocabSchema Schema for temporary tables
#' @param studyName Customized name for the study
#' @param domainsIncluded list of CDM domains to include
#' @param generateTables boolean > if TRUE new tables will be generated to the database.
#' @param readFromCSV boolean > if TRUE file from ./inst/CSV will be used as cohort data, otherwise JSONs will be used.
#' @param prevalenceCutOff numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param topDogs numeric > if set, keeps this number of features in the analysis, top n prevalence difference
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param removeOutliers boolean > if true this removes outlier patients from the target cohort, might cause loss of features
#'
#' @keywords internal
CohortContrast <- function(connection,
                         connectionDetails,
                         cdmSchema,
                         cdmVocabSchema,
                         cdmTmpSchema,
                         pathToResults,
                         studyName,
                         domainsIncluded = c("Drug", "Condition", "Measurement", "Observation", "Procedure"),
                         generateTables = TRUE,
                         readFromCSV = FALSE,
                         prevalenceCutOff = 10,
                         topDogs = FALSE,
                         presenceFilter = 0.005,
                         removeOutliers = TRUE) {
  # List of hard-coded objects
  targetCohortId = 2  # cohort 2 is always the target because of alphabetical reasons
  clustersCentersNo = 4 # PCA plot cluster number
  stdDevThreshold <- 6 # Value for outlier detection and removal

  # Result of the function

  resultList = list()

  data = generateTables(
    connection = connection,
    dbms = connectionDetails$dbms,
    cdmSchema = cdmSchema,
    cdmVocabSchema = cdmVocabSchema,
    cdmTmpSchema = cdmTmpSchema,
    pathToResults = pathToResults,
    studyName = studyName,
    generateTables = generateTables,
    readFromCSV = readFromCSV,
    domainsIncluded = domainsIncluded
  )

  printCustomMessage("Starting running analysis on data...")
  data_features = data$data_features
  data_patients = data$data_patients
  data_initial = data$data_initial



  ###########################################
  # TTTTTTTT  EEEEE  SSSSS  TTTTTTTT   SSSSS
  # T  TT  T  EE    SS      T  TT  T  SS
  #.   TT     EEEE   SSSS      TT      SSSS
  #.   TT     EE        SS     TT         SS
  #.   TT     EEEEE SSSSS      TT     SSSSS
  ###########################################
  # Assuming data_patients is your dataframe

  # Step 2: Aggregate the prevalence data for each concept within each cohort
  agg_data <-
    dplyr::summarise(
      dplyr::group_by(
        dplyr::mutate(data_patients, PREVALENCE = ifelse(PREVALENCE > 0, 1, 0)),
        COHORT_DEFINITION_ID,
        CONCEPT_ID
      ),
      TOTAL_PREVALENCE = sum(PREVALENCE),
      .groups = 'drop'
    )

  # Separate the data for each cohort for easier analysis
  cohort_1 <- dplyr::filter(agg_data, COHORT_DEFINITION_ID == 1)
  cohort_2 <- dplyr::filter(agg_data, COHORT_DEFINITION_ID == 2)

  # Count of patients in each cohort
  sample_1_n = nrow(dplyr::filter(data_initial, COHORT_DEFINITION_ID == 1))
  sample_2_n = nrow(dplyr::filter(data_initial, COHORT_DEFINITION_ID == 2))
  # Prepare a data frame to hold the results
  significant_concepts <-
    data.frame(CONCEPT_ID = integer(), P_VALUE = double())
  # We implement Bonferroni correction
  printCustomMessage("Starting running Mann-Whitney U test on data...")
  alpha = 0.05/length(unique(agg_data$CONCEPT_ID))
  # Step 3: Perform statistical test for each CONCEPT_ID
  for (concept_id in unique(agg_data$CONCEPT_ID)) {
    prevalence_cohort_1 <-
      dplyr::filter(cohort_1, CONCEPT_ID == concept_id)$TOTAL_PREVALENCE
    prevalence_cohort_2 <-
      dplyr::filter(cohort_2, CONCEPT_ID == concept_id)$TOTAL_PREVALENCE
     if (length(prevalence_cohort_1) == 0 |
        length(prevalence_cohort_2) == 0) {
      # Test throws error if one of those is numeric(0)
      next
     }
    else if (prevalence_cohort_2/sample_2_n < presenceFilter) {
      next
    }
    else if (prevalence_cohort_2 > sample_2_n || prevalence_cohort_1 > sample_1_n) {
      # In some databases there might be unmapped codes or wrongly mapped codes in multiple tables
      # therefore there is a chance that codes such as "0" aggregate to more than sample_1_n or sample_2_n
      # we do not analyse those codes
      next
    }
    else if (prevalence_cohort_2 == prevalence_cohort_1) {
      # Test throws error if these are equal
      next
    }
    else {
      # Mann-Whitney U test using stats package
      test_result <-
        prop.test(c(prevalence_cohort_1, prevalence_cohort_2), c(sample_1_n, sample_2_n), conf.level = 0.95)

      # Append results if significant
      # We implement Bonferroni correction
      if (test_result$p.value < alpha) {
        significant_concepts <-
          rbind(
            significant_concepts,
            data.frame(CONCEPT_ID = concept_id, P_VALUE = test_result$p.value)
          )
      }
    }
  }

  # Step 4: Filter concepts based on significance
  data_features <-
    dplyr::filter(data_features,
                  CONCEPT_ID %in% significant_concepts$CONCEPT_ID)
  printCustomMessage("Mann-Whitney U test on data executed!")

  n_features_left = nrow(data_features)

  printCustomMessage(paste("We have ", n_features_left, " features left!", sep = ""))
  # Now, significant_concepts_data contains only the concepts significantly overrepresented in cohort 2
  if (n_features_left == 0) {
    printCustomMessage("No features left. Perhaps use more lenient filters! Exiting ...")
    return(NULL)
  }


  if (topDogs == FALSE) {
    features = dplyr::filter(data_features,
                             PREVALENCE_DIFFERENCE_RATIO > prevalenceCutOff)
    n_features_left = nrow(features)
    printCustomMessage(paste("After filtering for prevalence cutoff of", prevalenceCutOff, ", there are ", n_features_left, " features left!", sep = ""))
  }
  else{
    topNFeatures <-
      dplyr::pull(dplyr::slice(dplyr::arrange(
        data_features, desc(PREVALENCE_DIFFERENCE_RATIO)
      ), 1:topDogs), CONCEPT_ID)
    features = dplyr::filter(data_features, CONCEPT_ID %in% topNFeatures)
    n_features_left = nrow(features)
    printCustomMessage(paste("After filtering for top ", topDogs, " features, there are ", n_features_left, " features left!", sep = ""))
  }
  patients = dplyr::select(
    dplyr::filter(
      dplyr::filter(data_patients, COHORT_DEFINITION_ID == targetCohortId),
      CONCEPT_ID %in% features$CONCEPT_ID
    ),
    PERSON_ID,
    CONCEPT_NAME,
    PREVALENCE
  )
  #TODO we should also save the concept id's for cohorts #TODO:
  resultList$selectedFeatureIds = features
  # Väike vaheküsimus, siin me kaotame pooled asmaatikutest, kas siis umbes pooltele tehakse vöga sarnast ravi ja pooltele midagi muud sama populatsiooniga sarnast?
  patients <-dplyr::summarise(dplyr::group_by(patients, PERSON_ID, CONCEPT_NAME),
    PREVALENCE = mean(PREVALENCE))

  transformed_data <-
    tidyr::spread(patients, CONCEPT_NAME, PREVALENCE, fill = 0)
  patients_data <-
    transformed_data[,-1]  # Remove the first column (patient IDs)
  patients_data <- as.data.frame(lapply(patients_data, scale_to_1_0))
  # Perform PCA
  printCustomMessage("Running PCA analysis ...")
  # Removing features with 0 instances
  # transformed_data = transformed_data[, apply(transformed_data, 2, function(x)
  #   ! all(x == 0))]
  # Filter out columns with zero variance
  has_variance <- sapply(patients_data, function(x) var(x, na.rm = TRUE) != 0)
  # Check if there are any columns left
  if (!any(is.na(has_variance))) {
    # Proceed with PCA on the filtered data
    filtered_patients_data <- patients_data[, sapply(patients_data, function(x) var(x, na.rm = TRUE) != 0)]
    pca_result <- stats::prcomp(filtered_patients_data, scale. = TRUE)
    printCustomMessage(paste("Creating ", clustersCentersNo, " clusters  ...", sep = ""))

    # Create a data frame for plotting

    clusters <-
      stats::kmeans(transformed_data, centers = clustersCentersNo)
    plot_data <-
      data.frame(PC1 = pca_result$x[, 1],
                 PC2 = pca_result$x[, 2],
                 Cluster = clusters$cluster)

    # Plotting
    pcaPlot1 <-
      ggplot2::ggplot(plot_data, ggplot2::aes(
        x = PC1,
        y = PC2,
        color = as.factor(Cluster)
      )) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::labs(color = "Cluster")

    resultList$pcaPlot1 = pcaPlot1

    # Assuming pca_result is the output from prcomp()
    scores <- pca_result$x

    # Calculate the standard deviations of the scores
    std_devs <- apply(scores, 2, sd)
    # Setting a threshold, e.g., points that are more than 3 standard deviations away
    threshold <- stdDevThreshold


    # Identify outliers
    outliers <-
      apply(scores, 1, function(x)
        any(abs(x) > (std_devs * threshold)))

    # Assuming 'data' is your original dataset
    if (removeOutliers) {
      printCustomMessage("Removing outliers ...")
      filtered_data <- transformed_data[!outliers,-1]
      # removing all the columns that become 0
      filtered_data <-
        filtered_data[, apply(filtered_data, 2, function(x)
          ! all(x == 0))]

    }
    else{
      filtered_data <- transformed_data[,-1]
    }
  } else {
    cat("No columns left after filtering for non-zero variance. PCA cannot be performed.\n")
    filtered_data <- transformed_data[,-1]

  }



  # export selected features

  resultList$selectedFeatures = colnames(filtered_data)
  colnames(data$data_patient) = c(
    "COHORT_DEFINITION_ID",
    "PERSON_ID",
    "CONCEPT_ID",
    "CONCEPT_NAME",
    "PREVALENCE",
    "HERITAGE"
  )
  data_selected_patients = dplyr::select(
    dplyr::filter(
      data$data_patient,
      CONCEPT_NAME %in% resultList$selectedFeatures
    ),
    CONCEPT_ID,
    CONCEPT_NAME,
    PERSON_ID,
    HERITAGE
  )
  # Creating target cohort & eligible patients dataset
  #
  data_target = dplyr::mutate(
    dplyr::filter(
      data_initial,
      COHORT_DEFINITION_ID == targetCohortId,
      ,
      SUBJECT_ID %in% unique(data_selected_patients$PERSON_ID)
    ),
    COHORT_DEFINITION_ID = 0
  )
  printCustomMessage("Creating a dataset for eligible patients only (Cohort2Trajectory input) ...")
  # Creating state cohorts / eligible patients dataset
  # Split the data by heritage
  split_data <-
    split(data_selected_patients, data_selected_patients$HERITAGE)

  # Iterate over each heritage type and construct then execute SQL queries
  results_list <- lapply(names(split_data), function(heritage) {
    printCustomMessage(paste("Quering eligible ", heritage, " data ...", sep = ""))
    # Get unique person_ids and concept_ids for the current heritage
    unique_person_ids <- unique(split_data[[heritage]]$PERSON_ID)
    unique_concept_ids <- unique(split_data[[heritage]]$CONCEPT_ID)

    # Construct SQL query
    sql_query <-
      sprintf(
        "SELECT base.* FROM %s AS base JOIN %s ON base.person_id = %s WHERE %s IN (%s)",
        paste(cdmSchema, heritage, sep = "."),
        paste(cdmTmpSchema, studyName, sep = "."),
        paste(cdmTmpSchema, studyName, "subject_id", sep = "."),
        sub("_occurrence_", "_", sub(
          "_exposure_", "_", paste(heritage, 'concept_id', sep = "_")
        )),
        paste(unique_concept_ids, collapse = ", ")
      )


    # Execute SQL query
    query_result <- DatabaseConnector::querySql(connection, sql_query)
    concept_string = toupper(sub("_occurrence_", "_", sub(
      "_exposure_", "_", paste(heritage, 'concept_id', sep = "_")
    )))
    # Ensure concept_map is created correctly
    concept_map <-
      unique(data_selected_patients[c("CONCEPT_ID", "CONCEPT_NAME")])
    concept_map[[concept_string]] <-
      as.character(concept_map[['CONCEPT_ID']]) # Ensure consistent data type
    colnames(concept_map) <- c(concept_string, "CONCEPT_NAME")
    concept_map$CONCEPT_NAME <-
      substr(concept_map$CONCEPT_NAME, 1, 20)
    # Before applying the mapping, check if query_result is not empty and has 'CONCEPT_ID'
    if (nrow(query_result) > 0 &&
        concept_string %in% names(query_result)) {
      query_result[[concept_string]] <-
        as.character(query_result[[concept_string]]) # Consistent data type for mapping

      # Perform the mapping using a safe merge, with checks
      if (nrow(concept_map) > 0) {
        query_result_mapped <-
          merge(query_result,
                concept_map,
                by = concept_string,
                all.x = TRUE)
      } else {
        # Handle the case where concept_map is empty
        warning("Concept map is empty. Cannot perform mapping.")
        query_result_mapped <- query_result
      }
    } else {
      # Handle cases where query_result is empty or does not have 'CONCEPT_ID'
      warning("Query result is empty or lacks CONCEPT_ID. Skipping mapping.")
      query_result_mapped <- query_result
    }

    # Use grep to find column names that end with "DATETIME"
    columns_ending_with_datetime <-
      grep("DATETIME$", names(query_result), value = TRUE)

    # Check if we found any columns and get the first one
    if (length(columns_ending_with_datetime) > 0) {
      first_column_ending_with_datetime <- columns_ending_with_datetime[1]
    } else {
      first_column_ending_with_datetime <-
        NULL # or any other appropriate action
    }

    query_result_mapped$START_DATE = query_result_mapped[[first_column_ending_with_datetime]]
    query_result_mapped$CONCEPT_ID = query_result_mapped[["CONCEPT_NAME"]]
    query_result_mapped = dplyr::select(query_result_mapped, CONCEPT_ID, PERSON_ID, START_DATE)

    printCustomMessage(paste("Quering eligible ", heritage, " data finished.", sep = ""))
    return(query_result_mapped)
  })

  data_states <- do.call(rbind, results_list)
  data_states$END_DATE = data_states$START_DATE

  # Assuming you want to combine all results into a single data fram
  colnames(data_states) <-
    c("COHORT_DEFINITION_ID",
      "SUBJECT_ID",
      "COHORT_START_DATE",
      "COHORT_END_DATE")

  resultList$trajectoryData = rbind(data_target, data_states)
  resultList$trajectoryData$COHORT_DEFINITION_ID = trimws(resultList$trajectoryData$COHORT_DEFINITION_ID)
  # Scaling
  filtered_data_scaled <-
    as.data.frame(lapply(filtered_data, scale_to_1_0))
  if(!any(is.na(has_variance))) {
  pca_result <- stats::prcomp(filtered_data_scaled, scale. = TRUE)
  # summary(pca_result)

  set.seed(2023)  # for reproducibility
  clusters <-
    stats::kmeans(filtered_data_scaled, centers = clustersCentersNo)

  # Create a data frame for plotting
  plot_data <-
    data.frame(PC1 = pca_result$x[, 1],
               PC2 = pca_result$x[, 2],
               Cluster = clusters$cluster)

  # Plotting
  pcaPlot2 <-
    ggplot2::ggplot(plot_data, ggplot2::aes(
      x = PC1,
      y = PC2,
      color = as.factor(Cluster)
    )) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(color = "Cluster")

  resultList$pcaPlot2 = pcaPlot2
  }
  else {
    resultList$pcaPlot1 = message("No variance for PCA")
    resultList$pcaPlot2 = message("No variance for PCA")
  }
  # Assuming your data frame is named 'filtered_data'
  # Apply the function to each column of the DataFrame
  printCustomMessage("Creating the heatmap ...")

  if (ncol(filtered_data_scaled) < 2) {
    message("Too few features for a heatmap!")
    resultList$heatmapPlot1 <- NULL
  }
  else {
  colnames(filtered_data_scaled) <-
    substr(colnames(filtered_data_scaled), 1, 15)

  resultList$heatmapPlot1 <-
    pheatmap::pheatmap(filtered_data_scaled,
                       color = colorRampPalette(c("blue", "white", "red"))(100),
                       show_rownames = F)

}
  printCustomMessage("Running analysis END!")
  ################################################################################
  #
  # Disconnect
  #
  ################################################################################

  # DatabaseConnector::disconnect(connection)

  ################################################################################
  #
  # Save
  #
  ################################################################################

  # TODO: save a list object to path
  filePath = paste(pathToResults,
                   "/tmp/datasets/",
                   studyName,
                   "_C2G_medData.rdata",
                   sep = "")
  save_object(data, path = filePath)
  printCustomMessage(paste("Saved the result to ", filePath, sep = ""))
  ################################################################################
  #
  # Return
  #
  ################################################################################

  return(resultList)
}
