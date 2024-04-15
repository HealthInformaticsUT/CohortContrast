

performPCAAnalysis <-
  function(data,
           runPCAClusters = 4,
           scaleData = TRUE,
           removeOutliers = TRUE,
           stdDevThreshold = 6) {
    printCustomMessage("Running PCA analysis ...")

    # Determine if scaling is needed
    if (scaleData) {
      has_variance <- sapply(data, function(x)
        var(x, na.rm = TRUE) != 0)
      if (!any(has_variance)) {
        printCustomMessage("No columns left after filtering for non-zero variance. PCA cannot be performed.")
        return(NULL)
      }
      data <- data[, has_variance]
      data <- scale(data)  # Scale the data if required
    }

    set.seed(2023)  # for reproducibility
    pca_result <- stats::prcomp(data, scale. = scaleData)

    # Perform clustering
    clusters <-
      stats::kmeans(data, centers = min(nrow(dplyr::distinct(data)), runPCAClusters))
    plot_data <-
      data.frame(PC1 = pca_result$x[, 1],
                 PC2 = pca_result$x[, 2],
                 Cluster = clusters$cluster)
    # Generate PCA plot
    pcaPlot <-
      ggplot2::ggplot(plot_data, ggplot2::aes(
        x = PC1,
        y = PC2,
        color = as.factor(Cluster)
      )) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::labs(color = "Cluster")
    # Optionally identify and remove outliers
    if (removeOutliers) {
      scores <- pca_result$x
      std_devs <- apply(scores, 2, sd)
      outliers <-
        apply(scores, 1, function(x)
          any(abs(x) > (std_devs * stdDevThreshold)))
      data <- data[!outliers,]
      printCustomMessage("Removing outliers ...")
    }

    return(list(filteredData = data, pcaPlot = pcaPlot))
  }



createTargetMatrixForHeatmap <- function(data,
                                         targetCohortId,
                                         prevalenceCutOff,
                                         presenceFilter,
                                         cdmSchema,
                                         connection,
                                         complementaryMappingTable = FALSE) {
  # Load necessary libraries for database connection and heatmap drawing
  # Assume library(pheatmap) and DBI or similar for database interaction are loaded elsewhere
  if (is.data.frame(complementaryMappingTable)) {
    # Join the dataframes on CONCEPT_ID
    data$data_patients <-
      dplyr::select(
        dplyr::mutate(
          dplyr::left_join(
            data$data_patients,
            complementaryMappingTable,
            by = "CONCEPT_ID",
            suffix = c("", ".compl")
          ),
          # Choose the complementaryMappingTable CONCEPT_NAME if it's not NA; otherwise, use the original
          CONCEPT_NAME = ifelse(
            is.na(CONCEPT_NAME.compl),
            CONCEPT_NAME,
            CONCEPT_NAME.compl
          )
        ),
        # Select and rename the columns to match the original concept_map structure
        COHORT_DEFINITION_ID,
        PERSON_ID,
        CONCEPT_ID,
        CONCEPT_NAME,
        PREVALENCE,
        HERITAGE
      )

    concept_mapping <-
      dplyr::ungroup(dplyr::summarize(
        dplyr::group_by(data$data_patients, CONCEPT_NAME, HERITAGE),
        CONCEPT_ID = CONCEPT_ID[which.max(PREVALENCE)]
      ))

    # Identify duplicates in CONCEPT_NAME
    duplicates <-
      concept_mapping$CONCEPT_NAME[duplicated(concept_mapping$CONCEPT_NAME)]

    # Update CONCEPT_NAME for duplicates
    concept_mapping <-
      dplyr::select(dplyr::mutate(
        concept_mapping,
        CONCEPT_NAME = ifelse(
          CONCEPT_NAME %in% duplicates,
          paste(CONCEPT_NAME, HERITAGE, sep = " "),
          CONCEPT_NAME
        )
      ),
      -HERITAGE)

    # Merge the concept mapping with the original data
    data$data_patients <-
      dplyr::left_join(dplyr::select(data$data_patients,-CONCEPT_ID),
                       concept_mapping,
                       by = "CONCEPT_NAME")

    # Group by PERSON_ID, COHORT_DEFINITION_ID, and CONCEPT_ID
    # Summarize PREVALENCE and HERITAGE
    data$data_patients <-
      dplyr::ungroup(dplyr::summarize(
        dplyr::group_by(
          data$data_patients,
          PERSON_ID,
          COHORT_DEFINITION_ID,
          CONCEPT_ID,
          CONCEPT_NAME
        ),
        PREVALENCE = sum(PREVALENCE),
        HERITAGE = first(HERITAGE)
      ))


    data$data_features <-
      dplyr::select(
        dplyr::mutate(
          dplyr::left_join(
            data$data_features,
            complementaryMappingTable,
            by = "CONCEPT_ID",
            suffix = c("", ".compl")
          ),
          # Choose the complementaryMappingTable CONCEPT_NAME if it's not NA; otherwise, use the original
          CONCEPT_NAME = ifelse(
            is.na(CONCEPT_NAME.compl),
            CONCEPT_NAME,
            CONCEPT_NAME.compl
          )
        ),
        # Select and rename the columns to match the original concept_map structure
        CONCEPT_ID,
        CONCEPT_NAME,
        PREVALENCE_DIFFERENCE_RATIO,
        TARGET_SUBJECT_COUNT,
        CONTROL_SUBJECT_COUNT
      )

    # Use the same concept mapping for data$data_features
    data$data_features <-
      dplyr::ungroup(dplyr::summarize(
        dplyr::group_by(
          dplyr::left_join(
            dplyr::select(data$data_features,-CONCEPT_ID), concept_mapping, by = "CONCEPT_NAME"),
            CONCEPT_ID,
            CONCEPT_NAME
          ),
          PREVALENCE_DIFFERENCE_RATIO = mean(PREVALENCE_DIFFERENCE_RATIO),
          TARGET_SUBJECT_COUNT = sum(TARGET_SUBJECT_COUNT),
          CONTROL_SUBJECT_COUNT = sum(CONTROL_SUBJECT_COUNT)
        )
      )

    # Ensuring that the updated_concept_map contains unique CONCEPT_ID entries

  }
  # Extracting and joining concepts
  concepts <- dplyr::filter(
    dplyr::inner_join(
      dplyr::distinct(dplyr::filter(
        dplyr::select(data$data_patients, CONCEPT_ID, CONCEPT_NAME, HERITAGE),
        CONCEPT_ID != 0
      )),
      data$data_features,
      by = c("CONCEPT_ID", "CONCEPT_NAME")
    ),!is.na(PREVALENCE_DIFFERENCE_RATIO)
  )



  # Creating target dataframe
  target <- dplyr::left_join(
    dplyr::select(
      dplyr::filter(data$data_patients,
                    COHORT_DEFINITION_ID == targetCohortId),
      -COHORT_DEFINITION_ID
    ),
    concepts,
    by = c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE")
  )

  # Processing target dataframe
  # Step 2: Group by CONCEPT_ID, calculate NCONCEPTS and NPATIENTS
  target_df =  dplyr::filter(dplyr::ungroup(dplyr::mutate(
    dplyr::group_by(
      dplyr::mutate(
        dplyr::mutate(
          dplyr::filter(target,
                        #CONCEPT_ID %in% dataRaw$selectedFeatures$CONCEPT_ID
                        PREVALENCE_DIFFERENCE_RATIO > prevalenceCutOff),
          PRESENT = 1
        ),
        NPATIENTS = length(unique(PERSON_ID))
      ),
      CONCEPT_ID
    ),
    NCONCEPTS = dplyr::n()
  )),
  NCONCEPTS / NPATIENTS > presenceFilter)

  # Step 4: Create a summary for patients without concepts
  no_concept_summary <- dplyr::summarize(
    dplyr::group_by(target, PERSON_ID),
    CONCEPT_ID = 999999999,
    CONCEPT_NAME = "None",
    PREVALENCE = 99999,
    HERITAGE = "none",
    PREVALENCE_DIFFERENCE_RATIO = -1
  )

  # Step 5: Combine and pivot wider
  combined_data <- dplyr::bind_rows(target_df, no_concept_summary)

  # Now pivot wider without risk of creating duplicate columns
  wide_data <- tidyr::pivot_wider(
    combined_data,
    id_cols = c(
      CONCEPT_ID,
      CONCEPT_NAME,
      HERITAGE,
      PREVALENCE_DIFFERENCE_RATIO
    ),
    names_from = PERSON_ID,
    values_from = PRESENT,
    names_prefix = "PID_",
    values_fill = 0
  )

  # Step 6: Filter out placeholder and convert CONCEPT_ID to character
  target_df <-
    dplyr::filter(wide_data, CONCEPT_ID != '999999999' &
                    CONCEPT_ID != '0')
  target_df$CONCEPT_ID <- as.character(target_df$CONCEPT_ID)

  target_row_annotation = dplyr::select(tibble::column_to_rownames(as.data.frame(target_df), "CONCEPT_ID"),
                                        -starts_with("PID_"))

  target_matrix = as.matrix(dplyr::select(
    tibble::column_to_rownames(as.data.frame(target_df), "CONCEPT_ID"),
    starts_with("PID_")
  ))


  # Get person data
  # Render and translate the SQL query
  sql_query <- SqlRender::render(sql = "SELECT person_id, gender_concept_id, year_of_birth FROM @cdmSchema.person;",
                                 cdmSchema = cdmSchema)
  sql_query_translated <-
    SqlRender::translate(sql = sql_query, targetDialect = dbms)

  # Execute the SQL query
  person_data <-
    DatabaseConnector::querySql(connection, sql_query_translated)

  # Prefix 'person_id' with "PID_"
  person_data$PERSON_ID <-
    stringr::str_c("PID_", person_data$PERSON_ID)

  # Convert 'gender_concept_id' to a factor with levels and labels
  person_data$GENDER <-
    factor(
      person_data$GENDER_CONCEPT_ID,
      levels = c(8507, 8532),
      labels = c("Male", "Female")
    )

  # Select and rename columns as necessary
  # Assuming 'year_of_birth' does not need renaming and is directly used
  selected_person_data <-
    dplyr::select(person_data, PERSON_ID, GENDER, YEAR_OF_BIRTH)

  # Convert 'person_id' to row names
  person <-
    tibble::column_to_rownames(selected_person_data, var = "PERSON_ID")

  return(
    list(
      target_matrix = target_matrix,
      demographics = person,
      target_row_annotation = target_row_annotation
    )
  )
}

createHeatmap <-
  function(data,
           cohortDefinitionId,
           prevalenceRatioThreshold,
           prevalenceThreshold,
           cdmSchema,
           connection,
           complementaryMappingTable = NULL) {
    heatmapData <- createTargetMatrixForHeatmap(
      data = data,
      targetCohortId = cohortDefinitionId,
      prevalenceCutOff = prevalenceRatioThreshold,
      # Example threshold
      presenceFilter = prevalenceThreshold,
      # Example threshold
      cdmSchema = cdmSchema,
      connection = connection,
      complementaryMappingTable = complementaryMappingTable
    )
    target_matrix = heatmapData$target_matrix
    person = heatmapData$demographics
    target_row_annotation = heatmapData$target_row_annotation
    col_clustering = stats::hclust(dist(t(target_matrix)))


    # Row reordering
    reordering =   dplyr::mutate(
      dplyr::mutate(
        dplyr::summarize(
          dplyr::group_by(
            tibble::rownames_to_column(target_row_annotation, "CONCEPT_ID"),
            HERITAGE
          ),
          CONCEPT_ID = list(CONCEPT_ID)
        ),
        MATRIX = purrr::map(CONCEPT_ID, ~ target_matrix[.x, , drop = F])
      ),
      MATRIX = purrr::map(MATRIX,
                          function(x) {
                            if (nrow(x) > 1) {
                              x = x[hclust(dist(x))$order,]
                            }
                            return(x)
                          })
    )

    tm = do.call(rbind, reordering$MATRIX)
    tm_gaps = cumsum(purrr::map_int(reordering$MATRIX, nrow))

    rownames(tm) = target_row_annotation[rownames(tm),]$CONCEPT_NAME
    # # Create target matrix for heatmap
    # targetMatrix <- data %>%
    #   filter(COHORT_DEFINITION_ID == cohortDefinitionId) %>%
    #   select(-COHORT_DEFINITION_ID) %>%
    #   left_join(concepts, by = c("CONCEPT_ID", "CONCEPT_NAME")) %>%
    #   pivot_wider(names_from = PERSON_ID, values_from = PRESENT, values_fill = list(PRESENT = 0))
    #
    # # Fetch and process demographic data for column annotations
    # personData <- fetchPersonData(cdmSchema, connection)

    # Create the heatmap
    heatmapPlot <- pheatmap::pheatmap(
      tm,
      show_colnames = F,
      annotation_row = dplyr::select(
        tibble::column_to_rownames(
          tibble::remove_rownames(target_row_annotation),
          "CONCEPT_NAME"
        ),
        -PREVALENCE_DIFFERENCE_RATIO
      ),
      annotation_col = person,
      cluster_cols = col_clustering,
      cluster_rows = FALSE,
      gaps_row = tm_gaps,
      color = c("#e5f5f9", "#2ca25f"),
      legend_breaks = c(0.25, 0.75),
      legend_labels = c("Absent", "Present"),
      fontsize = 10 # Adjust this value as needed to make the text bigger
    )

    return(list(
      heatmapPlot = heatmapPlot,
      targetMatrix = target_matrix,
      personData = person
    ))
  }

# Function to fetch and process person demographic data
fetchPersonData <- function(cdmSchema, connection) {
  sqlQuery <-
    paste("SELECT person_id, gender_concept_id, year_of_birth FROM",
          cdmSchema,
          ".person;")
  personData <- dbGetQuery(connection, sqlQuery)

  # Process personData as necessary
  # This might include renaming columns, converting IDs to factors, etc.

  return(personData)
}
