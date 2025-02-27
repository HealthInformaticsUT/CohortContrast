
#' Filter CohortContrast Analysis with safety measures, will guarantee small cell supression between smallCellSuppression and 2*smallCellSuppression
#' @param data Returned list from CohortContrast call
#' @param smallCellSuppression The rule of five integer, results under the threshold will be removed
#' @param complName Name of the output file
#' @param activeConceptKNN Use active feature values for clustering patients
#' @param pathToResults Path to the results folder, can be project's working directory
#' @importFrom dplyr %>%
#' @export
#' @examples
#' \dontrun{
#' control <- data.frame(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(5325, 3743, 2980, 1512, 2168),
#'   cohort_start_date = as.Date(c("1982-06-02", "1997-03-23",
#'    "2004-09-29", "2006-08-11", "1977-06-25")),
#'   cohort_end_date = as.Date(c("2019-03-17", "2018-10-07",
#'    "2018-04-01", "2017-11-29", "2018-11-22"))
#' )
#'
#' target <- data.frame(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(4804, 4861, 1563, 2830, 1655),
#'   cohort_start_date = as.Date(c("1997-03-23", "1982-06-02",
#'    "1977-06-25", "2006-08-11", "2004-09-29")),
#'   cohort_end_date = as.Date(c("2018-10-29", "2019-05-23",
#'    "2019-04-20", "2019-01-14", "2019-05-24"))
#' )
#'
#' control$cohort_definition_id = 100
#' target$cohort_definition_id = 500
#'
#' cohort = rbind(control, target)
#'
#' con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
#' DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS example")
#' DBI::dbWriteTable(con,   DBI::SQL('"example"."cohort"'), cohort)
#'
#' cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia",
#'  cdm_schema = "main", write_schema = "main")
#'
#'  targetTable <- cohortFromCohortTable(cdm = cdm, db = con,
#'   tableName = "cohort", schemaName = 'example', cohortId = 500)
#' controlTable <- cohortFromCohortTable(cdm = cdm, db = con,
#'  tableName = "cohort", schemaName = 'example', cohortId = 100)
#'
#'
#' pathToResults = getwd()
#'
#' data = CohortContrast(
#'   cdm,
#'   targetTable = targetTable,
#'   controlTable = controlTable,
#'   pathToResults,
#'   domainsIncluded = c(
#'     "Drug"
#'   ),
#'   prevalenceCutOff = 0.1,
#'   topK = FALSE,
#'   presenceFilter = 0.005,
#'   complementaryMappingTable = NULL,
#'   runZTests = FALSE,
#'   runLogitTests = FALSE,
#'   createOutputFiles = FALSE,
#'   numCores = 1
#' )
#'
#' CohortContrastSynthetic(data)
#'
#' DBI::dbDisconnect(con)
#' }
#'
CohortContrastSynthetic <- function(data,
                               smallCellSuppression = 5,
                               complName = NULL,
                               activeConceptKNN = FALSE,
                               pathToResults = getwd()) {
  # Copy data
  data_safe <- data

  # Set completion name
  data_safe$config$complName <- if (!is.null(complName)) {
    complName
  } else {
    paste0(data_safe$config$complName, "_Synthetic")
  }
  # Ensure PERSON_ID and SUBJECT_ID are integers
  data_safe$data_person$PERSON_ID <- convert_int64(data_safe$data_person$PERSON_ID)
  data_safe$data_initial$SUBJECT_ID <- convert_int64(data_safe$data_initial$SUBJECT_ID)
  data_safe$data_patients$PERSON_ID <- convert_int64(data_safe$data_patients$PERSON_ID)


  # Apply small cell suppression
  data_safe <- filterDataFeatures(data_safe, smallCellSuppression)
  data_safe <- filterDataPatients(data_safe)
  data_safe <- filterPatientsByCohort(data_safe)

  # Create patient clusters
  patient_cluster_mapping <- createPatientClusters(
    data = data_safe,
    smallCellSuppression = smallCellSuppression,
    activeConceptKNN = activeConceptKNN
  )

  # Apply mappings
  data_safe <- updateDataWithClusters(data_safe, patient_cluster_mapping)

  # Store smallCellSuppression in config
  data_safe$config$smallCellSuppression <- smallCellSuppression

  # Save results
  saveResult(data = data_safe, pathToResults = pathToResults)

  # Return modified data
  return(data_safe)
}



#' Filter CohortContrast Analysis with safety measures, will guarantee small cell supression smallCellSuppression, removes any patient level data
#' @param data Returned list from CohortContrast call
#' @param smallCellSuppression The rule of five integer, results under the threshold will be removed
#' @param complName Name of the output file
#' @param pathToResults Path to the results folder, can be project's working directory
#' @importFrom dplyr %>%
#' @export
#' @examples
#' \dontrun{
#' control <- data.frame(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(5325, 3743, 2980, 1512, 2168),
#'   cohort_start_date = as.Date(c("1982-06-02", "1997-03-23",
#'    "2004-09-29", "2006-08-11", "1977-06-25")),
#'   cohort_end_date = as.Date(c("2019-03-17", "2018-10-07",
#'    "2018-04-01", "2017-11-29", "2018-11-22"))
#' )
#'
#' target <- data.frame(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(4804, 4861, 1563, 2830, 1655),
#'   cohort_start_date = as.Date(c("1997-03-23", "1982-06-02",
#'    "1977-06-25", "2006-08-11", "2004-09-29")),
#'   cohort_end_date = as.Date(c("2018-10-29", "2019-05-23",
#'    "2019-04-20", "2019-01-14", "2019-05-24"))
#' )
#'
#' control$cohort_definition_id = 100
#' target$cohort_definition_id = 500
#'
#' cohort = rbind(control, target)
#'
#' con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir("GiBleed"))
#' DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS example")
#' DBI::dbWriteTable(con,   DBI::SQL('"example"."cohort"'), cohort)
#'
#' cdm <- CDMConnector::cdm_from_con(con, cdm_name = "eunomia",
#'  cdm_schema = "main", write_schema = "main")
#'
#'  targetTable <- cohortFromCohortTable(cdm = cdm, db = con,
#'   tableName = "cohort", schemaName = 'example', cohortId = 500)
#' controlTable <- cohortFromCohortTable(cdm = cdm, db = con,
#'  tableName = "cohort", schemaName = 'example', cohortId = 100)
#'
#'
#' pathToResults = getwd()
#'
#' data = CohortContrast(
#'   cdm,
#'   targetTable = targetTable,
#'   controlTable = controlTable,
#'   pathToResults,
#'   domainsIncluded = c(
#'     "Drug"
#'   ),
#'   prevalenceCutOff = 0.1,
#'   topK = FALSE,
#'   presenceFilter = 0.005,
#'   complementaryMappingTable = NULL,
#'   runZTests = FALSE,
#'   runLogitTests = FALSE,
#'   createOutputFiles = FALSE,
#'   numCores = 1
#' )
#'
#' CohortContrastSafe(data)
#'
#' DBI::dbDisconnect(con)
#' }
#'
CohortContrastSafe <- function(data,
                              smallCellSuppression = 5,
                              complName = NULL,
                              pathToResults = getwd()) {
  # Copy data
  data_safe <- data

  # Set completion name
  data_safe$config$complName <- if (!is.null(complName)) {
    complName
  } else {
    paste0(data_safe$config$complName, "_SmallCellSuppression")
  }

  # Ensure PERSON_ID and SUBJECT_ID are integers
  data_safe$data_person$PERSON_ID <- convert_int64(data_safe$data_person$PERSON_ID)
  data_safe$data_initial$SUBJECT_ID <- convert_int64(data_safe$data_initial$SUBJECT_ID)
  data_safe$data_patients$PERSON_ID <- convert_int64(data_safe$data_patients$PERSON_ID)


  # Apply small cell suppression
  data_safe <- filterDataFeatures(data_safe, smallCellSuppression)
  data_safe <- filterDataPatients(data_safe)
  # TODO: precalculate data for plots so that it is possible to filter later
   for (abstractionLevel in unique(data_safe$data_features$ABSTRACTION_LEVEL)) {
     data_safe <- calculatePlotData(data_safe, abstractionLevel = abstractionLevel)
   }
  # Store smallCellSuppression in config
  data_safe$config$smallCellSuppression <- smallCellSuppression
  # Store boolean for no patient level data in config
  data_safe$config$patientLevelData <- FALSE

  # Save results
  saveResult(data = data_safe, pathToResults = pathToResults)

  # Remove patient level data
  data_safe$data_patients = NULL
  data_safe$data_initial = NULL
  data_safe$data_person = NULL
  # Return modified data
  return(data_safe)
}

#' @keywords internal
filterDataFeatures <- function(data, smallCellSuppression = 5) {
  if ("data_features" %in% names(data)) {
    # Filter rows where the sum of target and control counts is >= smallCellSuppression
    data$data_features <- dplyr::filter(
      data$data_features,
      (.data$TARGET_SUBJECT_COUNT >= smallCellSuppression & .data$CONTROL_SUBJECT_COUNT >= smallCellSuppression) |
      (.data$TARGET_SUBJECT_COUNT >= smallCellSuppression & .data$CONTROL_SUBJECT_COUNT == 0) |
      (.data$TARGET_SUBJECT_COUNT == 0 & .data$CONTROL_SUBJECT_COUNT >= smallCellSuppression)
    )
  }

  if ("selectedFeatures" %in% names(data$trajectoryDataList)) {
    # Filter rows in selectedFeatures based on the same condition
    data$trajectoryDataList$selectedFeatures <- dplyr::filter(
      data$trajectoryDataList$selectedFeatures,
      (.data$TARGET_SUBJECT_COUNT >= smallCellSuppression & .data$CONTROL_SUBJECT_COUNT >= smallCellSuppression) |
      (.data$TARGET_SUBJECT_COUNT >= smallCellSuppression & .data$CONTROL_SUBJECT_COUNT == 0) |
      (.data$TARGET_SUBJECT_COUNT == 0 & .data$CONTROL_SUBJECT_COUNT >= smallCellSuppression)
    )

    # Update selectedFeatureNames
    data$trajectoryDataList$selectedFeatureNames <- dplyr::filter(
      data$trajectoryDataList$selectedFeatures,
      .data$CONCEPT_NAME %in% data$trajectoryDataList$selectedFeatureNames
    ) %>%
      dplyr::pull(.data$CONCEPT_NAME)

    # Update selectedFeatureIds
    data$trajectoryDataList$selectedFeatureIds <- dplyr::filter(
      data$trajectoryDataList$selectedFeatures,
      .data$CONCEPT_ID %in% data$trajectoryDataList$selectedFeatureIds
    ) %>%
      dplyr::pull(.data$CONCEPT_ID)
  }

  return(data)
}

#' @keywords internal
filterDataPatients <- function(data) {
  if ("data_patients" %in% names(data) & "data_features" %in% names(data)) {
    # Extract valid concept combinations from data_features
    valid_concepts <- data$data_features %>%
      dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$HERITAGE, .data$ABSTRACTION_LEVEL) %>%
      dplyr::distinct()

    # Filter data_patients to keep only rows that match valid_concepts
    data$data_patients <- dplyr::semi_join(
      data$data_patients,
      valid_concepts,
      by = c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE", "ABSTRACTION_LEVEL")
    )
  }

  return(data)
}


#' @keywords internal
filterPatientsByCohort <- function(data) {
  if ("data_patients" %in% names(data)) {
    # Extract valid patient-cohort pairs
    valid_patients <- data$data_patients %>%
      dplyr::select(.data$COHORT_DEFINITION_ID, .data$PERSON_ID) %>%
      dplyr::distinct()

    # Filter data_initial to retain only valid SUBJECT_IDs and COHORT_DEFINITION_IDs
    if ("data_initial" %in% names(data)) {
      data$data_initial <- dplyr::semi_join(
        data$data_initial,
        valid_patients,
        by = c("COHORT_DEFINITION_ID", "SUBJECT_ID" = "PERSON_ID")
      )
    }

    # Filter data_person to retain only valid PERSON_IDs
    if ("data_person" %in% names(data)) {
      valid_person_ids <- valid_patients %>%
        dplyr::select(.data$PERSON_ID) %>%
        dplyr::distinct()

      data$data_person <- dplyr::semi_join(
        data$data_person,
        valid_person_ids,
        by = "PERSON_ID"
      )
    }
  }

  return(data)
}




# Function to create patient clusters for mapping
#' @keywords internal
createPatientClusters <- function(data, smallCellSuppression = 5, activeConceptKNN = FALSE, num_clusters = NULL) {
  if ("data_person" %in% names(data) & "data_patients" %in% names(data)) {
    # Extract distinct PERSON_ID and COHORT_DEFINITION_ID
    cohort_mapping <- data$data_patients %>%
      dplyr::distinct(.data$PERSON_ID, .data$COHORT_DEFINITION_ID)

    if (activeConceptKNN) {
      # Add features from selectedFeatures where ABSTRACTION_LEVEL == -1
      relevant_concepts <- data$trajectoryDataList$selectedFeatures %>%
        dplyr::filter(.data$ABSTRACTION_LEVEL == -1) %>%
        dplyr::select(.data$CONCEPT_ID) %>%
        dplyr::distinct()

      filtered_patients <- dplyr::inner_join(data$data_patients, relevant_concepts, by = "CONCEPT_ID")

      prevalence_matrix <- filtered_patients %>%
        dplyr::select(.data$PERSON_ID, .data$CONCEPT_ID, .data$PREVALENCE) %>%
        dplyr::group_by(.data$PERSON_ID, .data$CONCEPT_ID) %>%
        dplyr::summarise(PREVALENCE = sum(.data$PREVALENCE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = .data$CONCEPT_ID, values_from = .data$PREVALENCE, values_fill = 0)

      data$data_person <- dplyr::left_join(data$data_person, prevalence_matrix, by = "PERSON_ID")
    }

    # Ensure we only use relevant columns for clustering (excluding PERSON_ID)
    clustering_columns <- setdiff(names(data$data_person), c("PERSON_ID"))

    # Merge distinct cohort information with data_person
    data_person <- dplyr::inner_join(data$data_person, cohort_mapping, by = "PERSON_ID")

    # Convert categorical variables to numeric, but exclude COHORT_DEFINITION_ID
    data_person <- data_person %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.character) & !dplyr::all_of("COHORT_DEFINITION_ID"), as.factor)) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.factor) & !dplyr::all_of("COHORT_DEFINITION_ID"), as.numeric))

    # Initialize cluster mapping table
    cluster_mapping <- data.frame()

    # Perform clustering separately for each COHORT_DEFINITION_ID
    for (cohort in unique(data_person$COHORT_DEFINITION_ID)) {
      cohort_data <- dplyr::filter(data_person, .data$COHORT_DEFINITION_ID == cohort)

      if (nrow(cohort_data) >= smallCellSuppression) {
        # Define number of clusters dynamically
        max_clusters <- floor(nrow(cohort_data) / smallCellSuppression)
        if (is.null(num_clusters)) {
          num_clusters <- max(1, max_clusters)
        }

        # Ensure we don't request more clusters than unique data points
        possible_clusters <- nrow(dplyr::distinct(dplyr::select(cohort_data, -PERSON_ID, -COHORT_DEFINITION_ID)))
        num_clusters <- min(num_clusters, possible_clusters)

        cohort_data <- cohort_data %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.infinite(.x), 0, .x)))

        # Apply K-means clustering
        set.seed(42)
        kmeans_result <- stats::kmeans(dplyr::select(cohort_data, dplyr::all_of(clustering_columns)), centers = num_clusters)

        # Assign clusters
        cohort_data <- dplyr::mutate(cohort_data, Cluster_ID = paste0(cohort, "_Cluster", kmeans_result$cluster))

        ## --- Step 1: Break Down Large Clusters (> 2 * smallCellSuppression) --- ##
        cluster_sizes <- dplyr::count(cohort_data, .data$Cluster_ID, name = "cluster_size")
        large_clusters <- dplyr::filter(cluster_sizes, .data$cluster_size > (2 * smallCellSuppression))

        if (nrow(large_clusters) > 0) {
          for (large_cluster in large_clusters$Cluster_ID) {
            large_cluster_data <- dplyr::filter(cohort_data, .data$Cluster_ID == large_cluster)

            # Perform hierarchical clustering to split large clusters
            dist_matrix <- stats::dist(dplyr::select(large_cluster_data, dplyr::all_of(clustering_columns)))
            hc <- stats::hclust(dist_matrix, method = "ward.D2")

            # Calculate the exact number of sub-clusters required for even distribution
            num_splits <- ceiling(nrow(large_cluster_data) / smallCellSuppression)

            # Assign sub-clusters to ensure even splitting
            split_labels <- stats::cutree(hc, k = num_splits)

            # Assign hierarchical clustering labels
            large_cluster_data <- large_cluster_data %>%
              dplyr::mutate(Cluster_ID = paste0(large_cluster, "_Split", split_labels))

            # Remove old oversized cluster and add new split clusters
            cohort_data <- dplyr::filter(cohort_data, .data$Cluster_ID != large_cluster) %>%
              dplyr::bind_rows(large_cluster_data)
          }
        }

        ## --- Step 2: Merge Small Clusters (< smallCellSuppression) --- ##
        repeat {
          # Identify small clusters
          cluster_sizes <- dplyr::count(cohort_data, .data$Cluster_ID, name = "cluster_size")
          small_clusters <- dplyr::filter(cluster_sizes, .data$cluster_size < smallCellSuppression)

          # Stop iterating when there are no more small clusters
          if (nrow(small_clusters) == 0) {
            break
          }

          # Filter only the patients in small clusters
          small_cluster_data <- dplyr::filter(cohort_data, .data$Cluster_ID %in% small_clusters$Cluster_ID)

          # If there are fewer than smallCellSuppression points left, merge them into new small clusters
          if (nrow(small_cluster_data) < smallCellSuppression) {
            # Identify large clusters
            large_clusters <- dplyr::filter(cluster_sizes, .data$cluster_size >= smallCellSuppression)

            if (nrow(large_clusters) > 0) {
              # Compute centroids for each large cluster
              large_cluster_centroids <- large_clusters %>%
                dplyr::inner_join(cohort_data, by = "Cluster_ID") %>%
                dplyr::group_by(.data$Cluster_ID) %>%
                dplyr::summarise(dplyr::across(dplyr::all_of(clustering_columns), mean, na.rm = TRUE), .groups = "drop")

              # Compute nearest large cluster for each point in small clusters
              for (i in 1:nrow(small_cluster_data)) {
                small_point <- small_cluster_data[i, clustering_columns, drop = FALSE] %>% as.numeric()

                # Calculate distances to large clusters
                distances <- apply(large_cluster_centroids[, -1], 1, function(row) sum((small_point - row)^2))

                # Assign to the nearest large cluster
                nearest_large_cluster <- large_cluster_centroids$Cluster_ID[which.min(distances)]
                small_cluster_data$Cluster_ID[i] <- nearest_large_cluster
              }
            } else {
              # No large clusters exist → Merge all small clusters into a single group
              cli::cli_alert_warning("The clustering of patients failed. Maybe use a smaller smallCellSuppression value. Returning a single merged cluster.")

              # Assign all small clusters to one single merged cluster
              merged_cluster_id <- paste0(cohort, "_FinalMergedCluster")
              small_cluster_data <- small_cluster_data %>%
                dplyr::mutate(Cluster_ID = merged_cluster_id)
            }

          } else {
            # Perform hierarchical clustering only on small clusters
            dist_matrix <- stats::dist(dplyr::select(small_cluster_data, dplyr::all_of(clustering_columns)))
            hc <- stats::hclust(dist_matrix, method = "ward.D2")

            # Merge small clusters iteratively until all clusters have at least smallCellSuppression members
            num_merges <- max(1, ceiling(nrow(small_cluster_data) / smallCellSuppression))
            merge_labels <- stats::cutree(hc, k = num_merges)

            # Assign merged clusters to the data
            small_cluster_data <- small_cluster_data %>%
              dplyr::mutate(Cluster_ID = paste0(cohort, "_MergedCluster", merge_labels))
          }

          # Remove old small clusters and replace them with the merged ones
          cohort_data <- dplyr::filter(cohort_data, !.data$Cluster_ID %in% small_clusters$Cluster_ID) %>%
            dplyr::bind_rows(small_cluster_data)
        }

        ## --- Step 3: Random Allocation for Remaining Large Clusters (> 2 * smallCellSuppression) --- ##
        cluster_sizes <- dplyr::count(cohort_data, .data$Cluster_ID, name = "cluster_size")
        remaining_large_clusters <- dplyr::filter(cluster_sizes, .data$cluster_size > (2 * smallCellSuppression))

        if (nrow(remaining_large_clusters) > 0) {
          for (large_cluster in remaining_large_clusters$Cluster_ID) {
            large_cluster_data <- dplyr::filter(cohort_data, .data$Cluster_ID == large_cluster)

            # Randomly split into evenly sized groups
            num_random_splits <- floor(nrow(large_cluster_data) / smallCellSuppression)

            # Generate random group assignments
            random_labels <- sample(rep(1:num_random_splits, each = smallCellSuppression, length.out = nrow(large_cluster_data)))

            # Assign new cluster labels based on random splitting
            large_cluster_data <- large_cluster_data %>%
              dplyr::mutate(Cluster_ID = paste0(large_cluster, "_RandomSplit", random_labels))

            # Remove old oversized cluster and add new split clusters
            cohort_data <- dplyr::filter(cohort_data, .data$Cluster_ID != large_cluster) %>%
              dplyr::bind_rows(large_cluster_data)
          }
        }
      } else {
        # If the cohort size is below the threshold, assign all to one group
        cohort_data <- dplyr::mutate(cohort_data, Cluster_ID = paste0(cohort, "_Cluster1"))
      }

      # Append to the cluster mapping table
      cluster_mapping <- dplyr::bind_rows(
        cluster_mapping,
        dplyr::select(cohort_data, .data$PERSON_ID, .data$COHORT_DEFINITION_ID, .data$Cluster_ID)
      )
    }

    return(cluster_mapping)
  } else {
    stop("Required tables (data_person and data_patients) are missing in the data list.")
  }
}


# Function to update data_initial and data_person with grouped mappings
#' @keywords internal
updateDataWithClusters <- function(data, patient_cluster_mapping) {
  # Step 1: Select a representative SUBJECT_ID per Cluster
  representative_subjects <- patient_cluster_mapping %>%
    dplyr::group_by(.data$Cluster_ID) %>%
    dplyr::summarise(
      COHORT_DEFINITION_ID = dplyr::first(.data$COHORT_DEFINITION_ID),  # Retain cohort info
      .groups = "drop"
    )

  # Randomly allocate one ID
  representative_subjects$REPRESENTATIVE_SUBJECT_ID <- 1:nrow(representative_subjects)

  # Step 2: Update data_initial
  if ("data_initial" %in% names(data)) {
    data$data_initial <- data$data_initial %>%
      dplyr::left_join(patient_cluster_mapping, by = c("SUBJECT_ID" = "PERSON_ID", "COHORT_DEFINITION_ID")) %>%
      dplyr::left_join(representative_subjects, by = "Cluster_ID") %>%
      dplyr::group_by(.data$Cluster_ID, .data$REPRESENTATIVE_SUBJECT_ID, COHORT_DEFINITION_ID = .data$COHORT_DEFINITION_ID.y) %>%
      dplyr::summarise(
        COHORT_START_DATE = min(.data$COHORT_START_DATE),  # Earliest start date
        COHORT_END_DATE = max(.data$COHORT_END_DATE),      # Latest end date
        .groups = "drop"
      ) %>%
      dplyr::rename(SUBJECT_ID = .data$REPRESENTATIVE_SUBJECT_ID) %>%
      dplyr::select(.data$SUBJECT_ID, .data$COHORT_DEFINITION_ID, .data$COHORT_START_DATE, .data$COHORT_END_DATE)
  }

  # Step 3: Update data_person
  if ("data_person" %in% names(data)) {
    data$data_person <- data$data_person %>%
      dplyr::left_join(patient_cluster_mapping, by = "PERSON_ID") %>%
      dplyr::left_join(representative_subjects, by = "Cluster_ID") %>%
      dplyr::group_by(.data$Cluster_ID, .data$REPRESENTATIVE_SUBJECT_ID, COHORT_DEFINITION_ID = .data$COHORT_DEFINITION_ID.y) %>%
      dplyr::summarise(
        GENDER_CONCEPT_ID = names(sort(table(.data$GENDER_CONCEPT_ID), decreasing = TRUE))[1],  # Most common gender
        YEAR_OF_BIRTH = round(stats::median(.data$YEAR_OF_BIRTH)),  # Median year of birth
        .groups = "drop"
      ) %>%
      dplyr::rename(PERSON_ID = .data$REPRESENTATIVE_SUBJECT_ID) %>%
      dplyr::select(.data$PERSON_ID, .data$GENDER_CONCEPT_ID, .data$YEAR_OF_BIRTH) %>%
      dplyr::distinct()
  }

  # Step 4: Update data_patients
  if ("data_patients" %in% names(data)) {
    data$data_patients <- data$data_patients %>%
      dplyr::left_join(patient_cluster_mapping, by = c("PERSON_ID", "COHORT_DEFINITION_ID")) %>%
      dplyr::left_join(representative_subjects, by = "Cluster_ID") %>%
      dplyr::mutate(
        PERSON_ID = as.integer(dplyr::if_else(!is.na(.data$REPRESENTATIVE_SUBJECT_ID), .data$REPRESENTATIVE_SUBJECT_ID, .data$PERSON_ID)),
        COHORT_DEFINITION_ID = .data$COHORT_DEFINITION_ID.y
      ) %>%
      dplyr::select(-.data$Cluster_ID, -.data$REPRESENTATIVE_SUBJECT_ID, -.data$COHORT_DEFINITION_ID.x, -.data$COHORT_DEFINITION_ID.y)

    data$data_patients <- data$data_patients %>%
      dplyr::group_by(
        .data$COHORT_DEFINITION_ID,
        .data$PERSON_ID,
        .data$CONCEPT_ID,
        .data$CONCEPT_NAME,
        .data$ABSTRACTION_LEVEL,
        .data$HERITAGE
      ) %>%
      dplyr::summarise(
        PREVALENCE = sum(.data$PREVALENCE, na.rm = TRUE),
        TIME_TO_EVENT = list(unlist(.data$TIME_TO_EVENT)),
        .groups = "drop"
      )
  }

  # Step 5: Convert COHORT_DEFINITION_ID to numeric, run additional calculations
  data$data_patients <- dplyr::mutate(
    data$data_patients,
    COHORT_DEFINITION_ID = dplyr::if_else(.data$COHORT_DEFINITION_ID == "target", 2, 1)
  )
  data$data_initial <- dplyr::mutate(
    data$data_initial,
    COHORT_DEFINITION_ID = dplyr::if_else(.data$COHORT_DEFINITION_ID == "target", 2, 1)
  )

  data <- calculate_data_features(data = data, nHighestPrevalenceDifference = FALSE)

  data <- handleTests(
    data,
    targetCohortId = 2,
    presenceFilter = data$config$presenceFilter,
    runZTests = data$config$runZTests,
    runLogitTests = data$config$runLogitTests,
    runKSTests = data$config$runKSTests
  )

  # Step 6: Convert COHORT_DEFINITION_ID back to character format
  data$data_patients <- dplyr::mutate(
    data$data_patients,
    COHORT_DEFINITION_ID = dplyr::if_else(.data$COHORT_DEFINITION_ID == 2, "target", "control")
  )
  data$data_initial <- dplyr::mutate(
    data$data_initial,
    COHORT_DEFINITION_ID = dplyr::if_else(.data$COHORT_DEFINITION_ID == 2, "target", "control")
  )

  return(data)
}

#' Function to calculate plot data for CCObject
#' @keywords internal
calculatePlotData <- function(data, abstractionLevel = -1){
  data_copy = data
  # Initialize filtering
  autoScaleRate = if(is.null(data_copy$config$autoScaleRate)) FALSE else data_copy$config$autoScaleRate
  applyInverseTarget = if(is.null(data_copy$config$applyInverseTarget)) FALSE else data_copy$config$applyInverseTarget
  applyZTest = if(is.null(data_copy$config$applyZTest)) FALSE else data_copy$config$applyZTest
  applyLogitTest = if(is.null(data_copy$config$applyLogitTest)) FALSE else data_copy$config$applyLogitTest
  #abstractionLevel = if(is.null(data$config$abstractionLevel)) -1 else data$config$abstractionLevel

  presenceFilter = if(is.null(data_copy$config$presenceFilter)) 0 else data_copy$config$presenceFilter
  prevalenceCutOff = if(is.null(data_copy$config$prevalenceCutOff)) 0 else data_copy$config$prevalenceCutOff
  # domainsIncluded = if(is.null(data$config$domainsIncluded)) NULL else data$config$domainsIncluded
  # removeUntreated = if(is.null(data$config$removeUntreated)) FALSE else data$config$removeUntreated

  correlationThreshold = if(is.null(data_copy$config$correlationThreshold)) 0.7 else data_copy$config$correlationThreshold

  formattedData <- format_results(data = data_copy,
                                  autoScaleRate = FALSE,
                                  applyInverseTarget = FALSE,
                                  applyZTest = FALSE,
                                  applyLogitTest = FALSE,
                                  abstractionLevel = abstractionLevel)

  filteredTarget <- filter_target(target = formattedData,
                                  prevalence_threshold = 0, # TODO: Change to 0
                                  prevalence_ratio_threshold = 0) # TODO: Change to 0

  filteredTarget <- prepare_filtered_target(filtered_target = filteredTarget,
                                            correlation_threshold = 1)



  prevalencePlotData <- getPrevalencePlotData(filtered_target = filteredTarget, patientDataAllowed = FALSE, isCorrelationView = FALSE)
  timePlotData <- getTimePlotData(filtered_target = filteredTarget, patientDataAllowed = FALSE, isCorrelationView = FALSE)
  heatmapPlotData <- getHeatmapPlotDataNoPatientDataAllowed(dataPatients = data_copy$data_patients, isCorrelationView = FALSE, abstractionLevel = abstractionLevel)
  heatmapPlotData <- prepare_heatmap_correlation_groups(heatmapData = heatmapPlotData, correlation_threshold = 0.95)

  data_copy$compressedDatas[[as.character(abstractionLevel)]]$prevalencePlotData = prevalencePlotData
  data_copy$compressedDatas[[as.character(abstractionLevel)]]$timePlotData = timePlotData
  data_copy$compressedDatas[[as.character(abstractionLevel)]]$heatmapPlotData = heatmapPlotData
  return(data_copy)
}
