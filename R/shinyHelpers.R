#' @importFrom data.table fread
#' @importFrom data.table :=
#' @importFrom grDevices colorRampPalette

#' @keywords internal
format_results <-
  function(data,
           autoScaleRate = FALSE,
           applyInverseTarget = FALSE,
           applyZTest = FALSE,
           applyLogitTest = FALSE,
           abstractionLevel = -1) {
    data_copy = data

    data_copy$data_initial <-
      data.table::as.data.table(data_copy$data_initial)
    data_copy$data_features <-
      data.table::as.data.table(data_copy$data_features)
    data_copy$data_patients <-
      data.table::as.data.table(data_copy$data_patients)
    data_copy$data_person <- data.table::as.data.table(data_copy$data_person)
    # Calculate the number of patients in target and control groups
    n_patients <- data_copy$data_initial[, .N, by = COHORT_DEFINITION_ID]

    n_patients <-
      reshape2::dcast(n_patients,
                      1 ~ COHORT_DEFINITION_ID,
                      value.var = "N",
                      fill = 0)

    count_target <- n_patients$target

    count_control <- n_patients$control



    # Update data features with prevalence calculations
    data_features_temp <-
      data_copy$data_features[ABSTRACTION_LEVEL == abstractionLevel, .(
        CONCEPT_ID,
        ZTEST,
        LOGITTEST,
        KSTEST,
        ABSTRACTION_LEVEL,
        HERITAGE
      )]

    # First, calculate the counts for target and control subjects
    data_copy$data_features <- data_copy$data_patients[ABSTRACTION_LEVEL == abstractionLevel,
                                             .(
                                               TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "target" &
                                                                            PREVALENCE > 0),
                                               CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "control" &
                                                                             PREVALENCE > 0)
                                             ),
                                             by = .(CONCEPT_ID, CONCEPT_NAME)]

    # Now, calculate the prevalences
    data_copy$data_features[, `:=`(
      TARGET_SUBJECT_PREVALENCE = TARGET_SUBJECT_COUNT / count_target,
      CONTROL_SUBJECT_PREVALENCE = CONTROL_SUBJECT_COUNT / count_control
    )]

    # Finally, calculate the PREVALENCE_DIFFERENCE_RATIO
    data_copy$data_features[, PREVALENCE_DIFFERENCE_RATIO := data.table::fifelse(
      is.na(TARGET_SUBJECT_PREVALENCE) |
        TARGET_SUBJECT_PREVALENCE == 0,
      0,
      data.table::fifelse(
        (
          is.na(CONTROL_SUBJECT_PREVALENCE) |
            CONTROL_SUBJECT_PREVALENCE == 0
        ) & is.na(TARGET_SUBJECT_PREVALENCE),
        -1,
        data.table::fifelse(
          is.na(CONTROL_SUBJECT_PREVALENCE) |
            CONTROL_SUBJECT_PREVALENCE == 0,
          100,
          TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
        )
      )
    )]

    # Join with data_features_temp
    data_copy$data_features <-
      data_copy$data_features[data_features_temp, on = .(CONCEPT_ID), nomatch = 0]

    if (applyZTest) {
      data_copy$data_features <- data_copy$data_features[ZTEST == TRUE]
    }

    if (applyLogitTest) {
      data_copy$data_features <- data_copy$data_features[LOGITTEST == TRUE]
    }

    if (applyInverseTarget) {
      # Invert target and control groups
      data_copy$data_patients[ABSTRACTION_LEVEL == abstractionLevel, COHORT_DEFINITION_ID := data.table::fifelse(COHORT_DEFINITION_ID == "control", "target", "control")]
      data_copy$data_initial[, COHORT_DEFINITION_ID := data.table::fifelse(COHORT_DEFINITION_ID == "control", "target", "control")]

      data_copy$data_features <-
        data_copy$data_features[ABSTRACTION_LEVEL == abstractionLevel, `:=`(
          TARGET_SUBJECT_COUNT = CONTROL_SUBJECT_COUNT,
          CONTROL_SUBJECT_COUNT = TARGET_SUBJECT_COUNT,
          TARGET_SUBJECT_PREVALENCE = CONTROL_SUBJECT_PREVALENCE,
          CONTROL_SUBJECT_PREVALENCE = TARGET_SUBJECT_PREVALENCE
        )][, PREVALENCE_DIFFERENCE_RATIO := data.table::fifelse(
          is.na(CONTROL_SUBJECT_PREVALENCE) |
            CONTROL_SUBJECT_PREVALENCE == 0 &
            is.na(TARGET_SUBJECT_PREVALENCE),
          -1,
          data.table::fifelse(
            is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0,
            100,#TARGET_SUBJECT_PREVALENCE / (1 / count_control),
            data.table::fifelse(
              is.na(TARGET_SUBJECT_PREVALENCE) | TARGET_SUBJECT_PREVALENCE == 0,
              0,#(1 / count_target) / CONTROL_SUBJECT_PREVALENCE,
              TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
            )
          )
        )]
    }

    if (autoScaleRate) {
      # Calculate the duration in the cohort in days
      data_copy$data_initial[, COHORT_DURATION := as.integer(difftime(
        as.Date(COHORT_END_DATE),
        as.Date(COHORT_START_DATE),
        units = "days"
      ))]
      # Calculate the scaled prevalence for each PERSON_ID
      scaled_prevalence <- merge(
        data_copy$data_patients[ABSTRACTION_LEVEL == abstractionLevel],
        data_copy$data_initial[, .(SUBJECT_ID, COHORT_DURATION)],
        by.x = "PERSON_ID",
        by.y = "SUBJECT_ID",
        allow.cartesian = TRUE
      )[, SCALED_PREVALENCE := PREVALENCE / (COHORT_DURATION / 365)]
      # Update features with scaled prevalence
      data_copy$data_features <-
        update_features(data_copy$data_features, scaled_prevalence)[, .(
          CONCEPT_ID,
          CONCEPT_NAME,
          PREVALENCE_DIFFERENCE_RATIO,
          TARGET_SUBJECT_COUNT,
          CONTROL_SUBJECT_COUNT,
          TARGET_SUBJECT_PREVALENCE,
          CONTROL_SUBJECT_PREVALENCE,
          ZTEST,
          LOGITTEST,
          KSTEST,
          ABSTRACTION_LEVEL,
          HERITAGE
        )]
    }

    concepts <-
      unique(data_copy$data_patients[ABSTRACTION_LEVEL == abstractionLevel, .(CONCEPT_ID, CONCEPT_NAME, HERITAGE, ABSTRACTION_LEVEL)]
             [CONCEPT_ID != 0])[data_copy$data_features, on = .(CONCEPT_ID, CONCEPT_NAME, ABSTRACTION_LEVEL), nomatch = 0][!is.na(PREVALENCE_DIFFERENCE_RATIO)]
    target <- merge(
      data_copy$data_patients[ABSTRACTION_LEVEL == abstractionLevel &
                           COHORT_DEFINITION_ID == "target"],
      concepts,
      by = c(
        "CONCEPT_ID",
        "CONCEPT_NAME",
        "HERITAGE",
        "ABSTRACTION_LEVEL"
      )
    )[!is.na(PREVALENCE_DIFFERENCE_RATIO)]

    # Calculate target_df with appropriate type handling
    target_dt <- data.table::as.data.table(target)
    # Calculate NPATIENTS and PRESENT
    target_dt[, NPATIENTS := data.table::uniqueN(PERSON_ID)]
    target_dt[, PRESENT := 1]

    # Calculate NCONCEPTS by grouping by CONCEPT_ID
    target_dt[, NCONCEPTS := .N, by = CONCEPT_ID]

    # Calculate PREVALENCE
    target_dt[, PREVALENCE := NCONCEPTS / NPATIENTS]

    # Pivot wider
    wider_dt <- data.table::dcast(
      target_dt,
      CONCEPT_ID + CONCEPT_NAME + HERITAGE + PREVALENCE_DIFFERENCE_RATIO + PREVALENCE + ZTEST + LOGITTEST  ~ PERSON_ID,
      value.var = "PRESENT",
      fill = 0
    )
    data.table::setkey(wider_dt, CONCEPT_ID)
    data.table::setkey(data_copy$data_features, CONCEPT_ID)


    # Store the result of grep in a variable
    columns_to_rename <-
      grep("^[0-9]", names(wider_dt), value = TRUE)
    # Check if there are any columns to rename
    if (length(columns_to_rename) > 0) {
      # Rename the columns that start with a number
      data.table::setnames(wider_dt,
                           old = columns_to_rename,
                           new = paste0("PID_", columns_to_rename))
    } else {
      message("No columns starting with a number were found.")
    }
    # Filter and mutate the results
    target_df <- data.table::as.data.table(wider_dt["CONCEPT_ID" != 999999999 &
                                                      "CONCEPT_ID" != "0"])
    target_df[, CONCEPT_ID := as.character(CONCEPT_ID)]

    target_df[, CONCEPT_NAME := gsub("(.{60})", "\\1\n", CONCEPT_NAME)] # nolint
    # Create target_row_annotation as a data.frame with rownames

    target_row_annotation <-
      as.data.frame(target_df[, .SD, .SDcols = !"CONCEPT_ID"])
    rownames(target_row_annotation) <- target_df$CONCEPT_ID
    target_row_annotation$CONCEPT_ID <- NULL

    pid_cols <- grep("^PID_", colnames(target_df), value = TRUE)
    target_matrix <- as.matrix(target_df[, ..pid_cols])
    rownames(target_matrix) <- target_df$CONCEPT_ID
    # Demographics
    target_col_annotation <- merge(data_copy$data_person,
                                   data_copy$data_initial[COHORT_DEFINITION_ID == "target", .SD[which.min(COHORT_START_DATE)], by = SUBJECT_ID][, .(PERSON_ID = SUBJECT_ID, COHORT_START_DATE)],
                                   by = "PERSON_ID")[, `:=`(
                                     AGE = floor(as.numeric(
                                       difftime(COHORT_START_DATE, as.Date(paste0(
                                         YEAR_OF_BIRTH, "-01-01"
                                       )), units = "days")
                                     ) / 365.25),
                                     GENDER = factor(
                                       GENDER_CONCEPT_ID,
                                       levels = c(8507, 8532),
                                       labels = c("Male", "Female")
                                     )
                                   )][, PERSON_ID := paste0("PID_", PERSON_ID)][, .SD, .SDcols = !c("GENDER_CONCEPT_ID")]
    target_col_annotation = as.data.frame(target_col_annotation)
    rownames(target_col_annotation) = target_col_annotation$PERSON_ID

    ### Time target

    target_time_annotation <- data_copy$data_patients[
      COHORT_DEFINITION_ID == "target" & ABSTRACTION_LEVEL == abstractionLevel,
      .(PERSON_ID, CONCEPT_ID, CONCEPT_NAME, TIME_TO_EVENT, HERITAGE)
    ]

    target_time_annotation <- target_time_annotation[
      data_copy$data_features,
      on = .(CONCEPT_ID),
      .(PERSON_ID, CONCEPT_ID, CONCEPT_NAME, TIME_TO_EVENT, HERITAGE),#, KSTEST),
      nomatch = 0L  # 0L in nomatch ensures all rows in x are returned, including those without a match in y
    ]

    target_time_annotation[, CONCEPT_NAME := gsub("(.{60})", "\\1\n", CONCEPT_NAME)] # nolint


    res <- list(
      target_matrix = target_matrix,
      target_row_annotation = target_row_annotation,
      target_col_annotation = target_col_annotation,
      target_time_annotation = target_time_annotation
    )
    return(res)
  }

filter_target <-
  function(target,
           prevalence_threshold = 0.1,
           prevalence_ratio_threshold = 2,
           domains = c(
             "procedure_occurrence",
             "condition_occurrence",
             "drug_exposure",
             "measurement",
             "observation",
             "visit_occurrence",
             "visit_detail",
             "death"
           ),
           removeUntreated = FALSE) {
    res <- target
    # Apply filters using data.table
    res$target_row_annotation <- res$target_row_annotation[res$target_row_annotation$PREVALENCE >= prevalence_threshold &
                                                             res$target_row_annotation$PREVALENCE_DIFFERENCE_RATIO > prevalence_ratio_threshold &
                                                             res$target_row_annotation$HERITAGE %in% domains
                                                           , ]

    # Ensure rownames are aligned with res$target_matrix
    if (!all(rownames(res$target_row_annotation) %in% rownames(res$target_matrix))) {
      stop("Error: Row names in target_row_annotation do not match those in target_matrix.")
    }

    # Update target_matrix to only include rows from filtered target_row_annotation
    res$target_matrix <-
      res$target_matrix[rownames(res$target_row_annotation),]


    # Update target_time_annotation to only include rows from filtered target_row_annotation
    res$target_time_annotation <- res$target_time_annotation[
      CONCEPT_ID %in% rownames(res$target_row_annotation)
    ]

    if (removeUntreated) {
      # Calculate column sums and filter out columns with zero sum
      column_sums <- colSums(res$target_matrix, na.rm = TRUE)
      non_zero_colnames <-
        colnames(res$target_matrix)[column_sums != 0]

      # Check if any columns have non-zero sums
      if (length(non_zero_colnames) == 0) {
        stop("Error: No columns with non-zero sums found in target_matrix.")
      }

      # Subset the matrix to keep only columns where the sum is not zero
      res$target_matrix <-
        res$target_matrix[, non_zero_colnames, drop = FALSE]

      # Filter target_col_annotation to match the filtered target_matrix
      res$target_col_annotation <- res$target_col_annotation[rownames(res$target_col_annotation) %in% colnames(res$target_matrix), ,
                                                             drop = FALSE]
    }

    return(res)
  }

#' @keywords internal
update_features <- function(features, scaled_prev) {
  # Convert scaled_prev to data.table if not already
  scaled_prev <- data.table::as.data.table(scaled_prev)

  # Calculate AVG_SCALED_PREVALENCE by grouping by CONCEPT_ID and COHORT_DEFINITION_ID
  scaled_prev <-
    scaled_prev[, .(AVG_SCALED_PREVALENCE = stats::median(SCALED_PREVALENCE, na.rm = TRUE)),
                by = .(CONCEPT_ID, COHORT_DEFINITION_ID)]

  # Pivot to wide format
  scaled_prev <- data.table::dcast(
    scaled_prev,
    CONCEPT_ID ~ COHORT_DEFINITION_ID,
    value.var = "AVG_SCALED_PREVALENCE",
    fill = -1
  )

  # Add the prefix "COHORT_" to the new columns (excluding the first column, which is CONCEPT_ID)
  data.table::setnames(scaled_prev,
                       old = names(scaled_prev)[-1],
                       new = paste0("COHORT_", names(scaled_prev)[-1]))
  # Calculate PREVALENCE_DIFFERENCE_RATIO
  scaled_prev[, PREVALENCE_DIFFERENCE_RATIO := data.table::fifelse(
    COHORT_control == -1,
    COHORT_target / 1,
    data.table::fifelse(
      COHORT_target == -1,
      1 / COHORT_control,
      COHORT_target / COHORT_control
    )
  )]
  # Select relevant columns
  scaled_prev <-
    scaled_prev[, .(CONCEPT_ID, PREVALENCE_DIFFERENCE_RATIO)]
  # Convert features to data.table if not already
  features <- data.table::as.data.table(features)
  # Remove existing PREVALENCE_DIFFERENCE_RATIO column if it exists
  features <-
    features[,!"PREVALENCE_DIFFERENCE_RATIO", with = FALSE]
  # Join the updated scaled_prev data back into the features
  features <-
    features[scaled_prev, on = "CONCEPT_ID", nomatch = 0]
  return(features)
}

#' @keywords internal
prepare_filtered_target <- function(filtered_target = NULL, correlation_threshold = 0.95) {
  if (is.null(filtered_target) || nrow(filtered_target$target_row_annotation) == 0) {
    filtered_target$correlation_analysis$ordered_matrix = NULL
    warning("After filtering there are no concepts left.")
    return(filtered_target)
  }
  if (nrow(filtered_target$target_row_annotation) < 2) {
    filtered_target$correlation_analysis$ordered_matrix = NULL
    warning("Must have n >= 2 concepts to cluster.")
    return(filtered_target)
  }
  if (is.null(nrow(filtered_target$target_matrix))) {
    filtered_target$target_matrix <-
      t(as.matrix(filtered_target$target_matrix)[1:length(filtered_target$target_matrix), , drop = FALSE])
    rownames(filtered_target$target_matrix) <-
      rownames(filtered_target$target_row_annotation)
  }
  target_matrix <- t(as.matrix(filtered_target$target_matrix))
  # Calculate the correlation matrix
  correlation_matrix <- stats::cor(target_matrix, method = "pearson", use = "pairwise.complete.obs")
  # Replace NA values with 0
  correlation_matrix[is.na(correlation_matrix)] <- 0
  # Set diagonal elements to 1
  diag(correlation_matrix) <- 1
  # Create a graph from the correlation matrix where edges only exist for correlation > threshold
  graph <- igraph::graph_from_adjacency_matrix(correlation_matrix > correlation_threshold,
                                               mode = "undirected",
                                               weighted = TRUE)
  # Use a community detection algorithm to find clusters of highly inter-correlated concepts
  clusters <- igraph::cluster_walktrap(graph)
  # Map concept IDs to names
  concept_ids <- rownames(filtered_target$target_row_annotation)
  concept_names <- filtered_target$target_row_annotation$CONCEPT_NAME
  names_vector <- stats::setNames(concept_names, concept_ids)
  groups <- lapply(unique(clusters$membership), function(cluster_id) {
    ids <- which(clusters$membership == cluster_id)
    group_names <- names_vector[ids]
    stats::setNames(group_names, concept_ids[ids])
  })

  groups <- groups[order(sapply(groups, length), decreasing = TRUE)]
  # Perform inter-group clustering
  ordered_groups <- lapply(groups, function(group) {
    group_ids <- names(group)
    group_matrix <- target_matrix[, group_ids, drop = FALSE]
    # Skip clustering if group has only one concept or insufficient variability
    if (all(apply(group_matrix, 1, var) == 0, na.rm = TRUE) || ncol(group_matrix) <= 1) {
      return(group)
    }
    # Compute a distance matrix within the group
    group_distance <- stats::as.dist(1 - stats::cor(group_matrix, method = "pearson", use = "pairwise.complete.obs"))

    # Check for NA/NaN in distance matrix
    if (any(is.na(group_distance))) {
      return(group) # Return as-is if clustering is not possible
    }
    # Perform hierarchical clustering
    hc <- stats::hclust(group_distance)

    # Order group IDs based on hierarchical clustering
    ordered_ids <- group_ids[hc$order]
    stats::setNames(group[ordered_ids], ordered_ids)
  })
  # Calculate inter-group correlations
  group_correlations <- matrix(0, nrow = length(ordered_groups), ncol = length(ordered_groups))
  for (i in seq_along(ordered_groups)) {
    for (j in seq_along(ordered_groups)) {
      if (i != j) {
        group_i <- names(ordered_groups[[i]])
        group_j <- names(ordered_groups[[j]])
        group_correlations[i, j] <- mean(correlation_matrix[group_i, group_j], na.rm = TRUE)
      }
    }
  }

  hc_groups = NULL
  if(nrow(group_correlations) > 1){
  # Perform clustering on the group correlation matrix
  group_distance <- stats::as.dist(1 - group_correlations)
  hc_groups <- stats::hclust(group_distance)
  ordered_group_indices <- hc_groups$order
  ordered_groups <- ordered_groups[ordered_group_indices]
}
  ordered_concept_ids <- unlist(ordered_groups, use.names = TRUE)
  ordered_concept_names <- unname(ordered_concept_ids)

  # Reorder annotations and matrix based on clustered order
  ordered_annotation <- filtered_target$target_row_annotation[match(names(ordered_concept_ids),
                                                                    rownames(filtered_target$target_row_annotation)), ]
  ordered_matrix <- as.matrix(filtered_target$target_matrix[names(ordered_concept_ids), , drop = FALSE])
  rownames(ordered_matrix) <- ordered_concept_names  # Set ordered concept names as rownames

  # Determine gap positions for visualizations
  group_lengths <- sapply(ordered_groups, length)
  gaps_row <- cumsum(group_lengths)[-length(group_lengths)]

  filtered_target$correlation_analysis <- list()
  filtered_target$correlation_analysis$ordered_matrix <- ordered_matrix
  filtered_target$correlation_analysis$groups <- ordered_groups
  filtered_target$correlation_analysis$gaps_row <- gaps_row
  filtered_target$correlation_analysis$hc_rows <- hc_groups

  return(filtered_target)
}

#' @keywords internal
prepare_heatmap_correlation_groups <- function(heatmapData, correlation_threshold = 0.95) {

  # Compute Pearson correlation matrix
  correlation_matrix <- heatmapData$correlation_matrix

  # Replace NA values with 0
  correlation_matrix[is.na(correlation_matrix)] <- 0

  # Set diagonal elements to 1 (self-correlations)
  diag(correlation_matrix) <- 1

  # Create a graph from the correlation matrix where edges exist for correlation > threshold
  graph <- igraph::graph_from_adjacency_matrix(
    correlation_matrix > correlation_threshold,
    mode = "undirected",
    weighted = TRUE
  )

  # Detect correlation-based clusters using Walktrap algorithm
  clusters <- igraph::cluster_walktrap(graph)

  # Extract concept IDs (since they are unique)
  concept_ids <- rownames(heatmapData$correlation_matrix)

  # Assign clusters to groups using concept IDs
  groups <- lapply(unique(clusters$membership), function(cluster_id) {
    ids <- which(clusters$membership == cluster_id)
    concept_ids[ids]
  })

  # Order groups by size (largest first)
  groups <- groups[order(sapply(groups, length), decreasing = TRUE)]

  # Determine gaps between groups
  group_lengths <- sapply(groups, length)
  gaps_row <- cumsum(group_lengths)[-length(group_lengths)]

  # Reorder matrix according to grouped concept IDs
  ordered_concept_ids <- unlist(groups, use.names = FALSE)
  ordered_matrix <- heatmapData$correlation_matrix[ordered_concept_ids, ordered_concept_ids]

  # Store results in heatmapData
  heatmapData$correlation_analysis <- list()
  heatmapData$correlation_analysis$ordered_matrix <- ordered_matrix
  heatmapData$correlation_analysis$groups <- groups
  heatmapData$correlation_analysis$gaps_row <- gaps_row

  return(heatmapData)
}

#' @keywords internal
getErrorPlot <- function(message = "After filtering there are no concepts left") {
  return(
    ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = message,
        hjust = 0.5,
        vjust = 0.5,
        size = 12,
        fontface = "bold",
        color = "black"
      ) +
      ggplot2::theme_void()
  )
}

#' @keywords internal
getHeritageColors <- function(asList = FALSE) {
  heritage_colors <-
    data.frame(
      HERITAGE = c(
        "procedure_occurrence",
        "condition_occurrence",
        "drug_exposure",
        "measurement",
        "observation",
        "visit_occurrence",
        "visit_detail",
        "death"
      ),
      color = c(
        "#4B99C9",
        "#FFB347",
        "#B39EB5",
        "#FFB3AB",
        "#77DD77",
        "#FDFD96",
        "#AEC6CF",
        "#3A3B3C"
      )
    )
  if(asList){
    heritage_colors <- c(
      procedure_occurrence = "#4B99C9",
      condition_occurrence = "#FFB347",
      drug_exposure = "#B39EB5",
      measurement = "#FFB3AB",
      observation = "#77DD77",
      visit_occurrence = "#FDFD96",
      visit_detail = "#AEC6CF",
      death = "#3A3B3C"
    )
  }
  return(heritage_colors)
}

#' @keywords internal
showNoPatientDataAllowedWarning <- function(message = "This feature is not supported without patient level data"){
  shiny::showNotification(message, type = "warning")
}

#' @keywords internal
createFullScreenWaiter <- function(message = "Loading, please wait...") {
  waiter::Waiter$new(
    html = shiny::tagList(
      shiny::h4(message, style = "color: white;"),
      waiter::spin_3()
    ),
    color = "rgba(0, 0, 0, 0.75)" # Semi-transparent black background
  )
}


#' @keywords internal
safe_dev_off <- function() {
  tryCatch(
    {
      grDevices::dev.off()  # Attempt to turn off the device
    },
    error = function(e) {
      message("No active graphics device to turn off.")  # Catch and log the error
    }
  )
}

#' @keywords internal
convert_abstraction_levels <- function(levels) {
  # Convert numeric vector to character
  levels <- as.character(levels)

  # Check for presence of -1 and -2
  has_original <- "-1" %in% levels
  has_source <- "-2" %in% levels

  # Replace -1 with "original" and -2 with "source"
  levels[levels == "-1"] <- "original"
  levels[levels == "-2"] <- "source"

  # Remove "original" and "source" temporarily for sorting
  non_special_levels <- levels[!levels %in% c("original", "source")]

  # Sort numerically
  sorted_levels <- as.character(sort(as.numeric(non_special_levels), decreasing = FALSE))

  # Construct the final output based on the presence of special values
  final_levels <- c(if (has_original) "original", sorted_levels, if (has_source) "source")

  return(final_levels)
}


#' @keywords internal
implementComplementaryMappingTable <- function(data, complementaryMappingTable, abstractionLevel = -1) {
if (is.data.frame(complementaryMappingTable)) {
  # Create empty dataframe
  complementaryMappingTable = updateMapping(complementaryMappingTable)
  data_mapped = data$data_patients[0, ]
  # Add mappings and remove old
    # Add mappings
  data_mapped_abstarction_level = handleMapping(data, complementaryMappingTable, abstractionLevel)
  data_mapped = rbind(data_mapped, data_mapped_abstarction_level)
    # Remove old
  concept_ids_to_remove = complementaryMappingTable %>% dplyr::filter(.data$ABSTRACTION_LEVEL == abstractionLevel) %>% dplyr::pull(.data$CONCEPT_ID)
  data$data_patients = data$data_patients %>% dplyr::filter(!(.data$CONCEPT_ID %in% concept_ids_to_remove & .data$ABSTRACTION_LEVEL == abstractionLevel))
  # Add new mappings
  data_mapped$ABSTRACTION_LEVEL = as.character(data_mapped$ABSTRACTION_LEVEL)
  data$data_patients <- rbind(data$data_patients, data_mapped)
}
 return(data)
}

#' @keywords internal
mergeCorrelationWithTransitions <- function(correlation_data, transition_data) {
  # Ensure CONCEPT_IDS is a list of exactly two elements
  correlation_data <- correlation_data %>%
    dplyr::mutate(
      CONCEPT_IDS = purrr::map(CONCEPT_IDS, function(x) {
        if (!is.list(x) && length(x) == 2) return(as.list(x)) # Convert vectors to lists
        if (is.list(x) && length(x) == 2) return(x)            # Already a list with 2 elements
        return(list(NA, NA))                                   # Handle unexpected cases
      })
    )

  # Extract concept IDs and normalize order for joining
  correlation_data <- correlation_data %>%
    dplyr::mutate(
      ID1 = purrr::map_int(CONCEPT_IDS, ~ as.integer(.x[[1]])),
      ID2 = purrr::map_int(CONCEPT_IDS, ~ as.integer(.x[[2]]))
    ) %>%
    dplyr::mutate(
      CONCEPT_ID_1 = pmin(ID1, ID2),
      CONCEPT_ID_2 = pmax(ID1, ID2)
    )

  # Join with undirected transition_data (CONCEPT_ID_1, CONCEPT_ID_2)
  result <- correlation_data %>%
    dplyr::left_join(transition_data, by = c("CONCEPT_ID_1", "CONCEPT_ID_2")) %>%
    dplyr::select(
      CONCEPT_NAMES,
      CONCEPT_IDS,
      CORRELATION,
      P_VALUE,
      MEDIAN_DAYS_INBETWEEN = MEDIAN_TRANSITION_TIME
    )

  return(result)
}


#' @keywords internal
calculateMedianTransitions <- function(data) {
  dt <- data.table::as.data.table(data)

  # Step 1: Unnest TIME_TO_EVENT (assumes CONCEPT_ID is scalar per row)
  dt <- dt[, .(TIME_TO_EVENT = unlist(TIME_TO_EVENT)), by = .(PERSON_ID, CONCEPT_ID)]

  # Step 2: Sort by person and time
  data.table::setorder(dt, PERSON_ID, TIME_TO_EVENT)

  # Step 3: For each person, get adjacent concept transitions
  dt[, `:=`(
    NEXT_CONCEPT_ID = data.table::shift(CONCEPT_ID, type = "lead"),
    NEXT_TIME = data.table::shift(TIME_TO_EVENT, type = "lead")
  ), by = PERSON_ID]

  # Step 4: Filter out invalid transitions
  dt <- dt[!is.na(NEXT_CONCEPT_ID) & CONCEPT_ID != NEXT_CONCEPT_ID]

  # Step 5: Normalize concept pairs (unordered)
  dt[, `:=`(
    CONCEPT_ID_1 = pmin(CONCEPT_ID, NEXT_CONCEPT_ID),
    CONCEPT_ID_2 = pmax(CONCEPT_ID, NEXT_CONCEPT_ID),
    TRANSITION_TIME = NEXT_TIME - TIME_TO_EVENT
  )]

  # Step 6: Remove NA or negative transition times (optional)
  dt <- dt[!is.na(TRANSITION_TIME) & TRANSITION_TIME >= 0]

  # Step 7: Aggregate median per concept pair
  result <- dt[
    , .(MEDIAN_TRANSITION_TIME = stats::median(TRANSITION_TIME, na.rm = TRUE)),
    by = .(CONCEPT_ID_1, CONCEPT_ID_2)
  ]

  return(result[])
}


#' @keywords internal
calculateMedianTransitionsPairwise <- function(data) {

  # Step 1: Unnest TIME_TO_EVENT (concepts are already 1 per row)
  dt <- data.table::as.data.table(data)
  dt <- dt[, .(TIME_TO_EVENT = unlist(TIME_TO_EVENT)), by = .(PERSON_ID, CONCEPT_ID)]
  dt <- dt[!is.na(TIME_TO_EVENT)]
  data.table::setorder(dt, PERSON_ID, TIME_TO_EVENT)

  # Step 2: Create a nested table: list of times per person+concept
  nested <- dt[, .(TIMES = list(TIME_TO_EVENT)), by = .(PERSON_ID, CONCEPT_ID)]

  # Step 3: Split by person
  person_groups <- split(nested, by = "PERSON_ID", keep.by = TRUE)
  all_results <- list()

  for (person_data in person_groups) {
    concepts <- person_data$CONCEPT_ID
    if (length(concepts) < 2) next

    # Create all unordered concept pairs
    concept_pairs <- t(utils::combn(concepts, 2, simplify = TRUE))
    person_result <- list()

    for (k in seq_len(nrow(concept_pairs))) {
      c1 <- concept_pairs[k, 1]
      c2 <- concept_pairs[k, 2]

      times1 <- person_data[CONCEPT_ID == c1, TIMES][[1]]
      times2 <- person_data[CONCEPT_ID == c2, TIMES][[1]]

      if (length(times1) == 0 || length(times2) == 0) next

      # Greedy pair matching
      n1 <- length(times1)
      n2 <- length(times2)
      i <- j <- 1
      diffs <- numeric()

      while (i <= n1 && j <= n2) {
        diffs <- c(diffs, times2[j] - times1[i])
        i <- i + 1
        j <- j + 1
      }

      if (length(diffs)) {
        person_result[[length(person_result) + 1]] <- data.table::data.table(
          CONCEPT_ID_1 = min(c1, c2),
          CONCEPT_ID_2 = max(c1, c2),
          TRANSITION_TIME = diffs
        )
      }
    }

    if (length(person_result)) {
      all_results[[length(all_results) + 1]] <- data.table::rbindlist(person_result)
    }
  }

  # Combine and compute medians
  if (length(all_results) == 0) {
    return(data.table::data.table(
      CONCEPT_ID_1 = integer(),
      CONCEPT_ID_2 = integer(),
      MEDIAN_TRANSITION_TIME = numeric()
    ))
  }

  all_dt <- data.table::rbindlist(all_results)
  result <- all_dt[
    , .(MEDIAN_TRANSITION_TIME = stats::median(TRANSITION_TIME, na.rm = TRUE)),
    by = .(CONCEPT_ID_1, CONCEPT_ID_2)
  ]

  return(result[])
}

#' @keywords internal
computePairwiseCorrelations <- function(ordered_matrix, target_row_annotation) {
  # Create mapping table (Concept Name <-> Concept ID)
  mapping_table <- dplyr::tibble(
    concept_id = base::rownames(target_row_annotation),
    concept_name = target_row_annotation$CONCEPT_NAME
  )

  # Convert to matrix (if not already)
  matrix_data <- base::as.matrix(ordered_matrix)

  # Get row names (concept names)
  concept_names <- base::rownames(matrix_data)

  # Compute pairwise correlations from binary patient vectors
  correlation_matrix <- stats::cor(base::t(matrix_data), method = "pearson") # Transpose to correlate rows

  # Convert correlation matrix to a long format table
  correlation_table <- data.table::as.data.table(as.table(correlation_matrix))

  # Rename columns
  data.table::setnames(correlation_table, c("concept_name_1", "concept_name_2", "CORRELATION"))

  # Remove diagonal (self-correlations where concept_name_1 == concept_name_2)
  correlation_table <- correlation_table[concept_name_1 != concept_name_2]

  # Keep only unique pairs (avoid duplicates by ensuring concept_name_1 < concept_name_2)
  correlation_table <- correlation_table[concept_name_1 < concept_name_2]

  # Compute Fisher Transformation & P-Value
  sample_size <- ncol(matrix_data)  # Number of patients (length of the binary vectors)

  correlation_table[, FISHER_Z := 0.5 * log((1 + CORRELATION) / (1 - CORRELATION))] # Fisher's Z
  correlation_table[, STANDARD_ERROR := 1 / sqrt(sample_size - 3)] # Standard error using correct sample size
  correlation_table[, Z_SCORE := FISHER_Z / STANDARD_ERROR] # Compute Z-score
  correlation_table[, P_VALUE := round(2 * (1 - stats::pnorm(abs(Z_SCORE))),8)] # Two-tailed p-value

  # Assign Concept Names as Lists
  correlation_table[, CONCEPT_NAMES := list(.(list(concept_name_1, concept_name_2))),
                    by = .(concept_name_1, concept_name_2)]

  # Map Concept Names to Concept IDs
  correlation_table <- correlation_table %>%
    dplyr::left_join(mapping_table, by = c("concept_name_1" = "concept_name")) %>%
    dplyr::rename(concept_id_1 = concept_id) %>%
    dplyr::left_join(mapping_table, by = c("concept_name_2" = "concept_name")) %>%
    dplyr::rename(concept_id_2 = concept_id)

  # Create CONCEPT_IDS column as a list
  correlation_table[, CONCEPT_IDS := list(.(list(concept_id_1, concept_id_2))),
                    by = .(concept_name_1, concept_name_2)]

  # Select only required columns
  correlation_table <- correlation_table[, .(CONCEPT_NAMES, CONCEPT_IDS, CORRELATION, P_VALUE)]
  return(correlation_table)
}

