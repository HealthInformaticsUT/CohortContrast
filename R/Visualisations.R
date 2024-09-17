#' @importFrom data.table fread
#' @importFrom data.table :=
#' @importFrom grDevices colorRampPalette


#' @keywords internal
format_results <- function(data, autoScaleRate, applyInverseTarget, applyZTest, applyLogitTest, abstractionLevel) {
  # Convert data frames to data.table
  data$data_initial <- data.table::as.data.table(data$data_initial)
  data$data_features <- data.table::as.data.table(data$data_features)
  data$data_patients <- data.table::as.data.table(data$data_patients)
  data$data_person <- data.table::as.data.table(data$data_person)
  # Calculate the number of patients in target and control groups
  n_patients <- data$data_initial[, data.table::.N, by = get("COHORT_DEFINITION_ID")]

  n_patients <- reshape2::dcast(n_patients, 1 ~ get("COHORT_DEFINITION_ID"), value.var = "N", fill = 0)

  count_target <- n_patients$target

  count_control <- n_patients$control



  # Update data features with prevalence calculations
  data_features_temp <- data$data_features[get("ABSTRACTION_LEVEL") == abstractionLevel, list(get("CONCEPT_ID"), get("ZTEST"), get("LOGITTEST"), get("ABSTRACTION_LEVEL"))]

  # First, calculate the counts for target and control subjects
  data$data_features <- data$data_patients[
    get("ABSTRACTION_LEVEL") == abstractionLevel,
    list(
      TARGET_SUBJECT_COUNT = sum(get("COHORT_DEFINITION_ID") == "target" & get("PREVALENCE") > 0),
      CONTROL_SUBJECT_COUNT = sum(get("COHORT_DEFINITION_ID") == "control" & get("PREVALENCE") > 0)
    ),
    by = list(get("CONCEPT_ID"), get("CONCEPT_NAME"))
  ]

  # Now, calculate the prevalences
  data$data_features[, `:=`(
    TARGET_SUBJECT_PREVALENCE = get("TARGET_SUBJECT_COUNT") / count_target,
    CONTROL_SUBJECT_PREVALENCE = get("CONTROL_SUBJECT_COUNT") / count_control
  )]

  # Finally, calculate the PREVALENCE_DIFFERENCE_RATIO
  data$data_features[, get("PREVALENCE_DIFFERENCE_RATIO") := data.table::fifelse(
    is.na(get("TARGET_SUBJECT_PREVALENCE")) | get("TARGET_SUBJECT_PREVALENCE") == 0, 0,
    data.table::fifelse(
      (is.na(get("CONTROL_SUBJECT_PREVALENCE")) | get("CONTROL_SUBJECT_PREVALENCE") == 0) & is.na(get("TARGET_SUBJECT_PREVALENCE")), -1,
      data.table::fifelse(
        is.na(get("CONTROL_SUBJECT_PREVALENCE")) | get("CONTROL_SUBJECT_PREVALENCE") == 0, 100,
        get("TARGET_SUBJECT_PREVALENCE") / get("CONTROL_SUBJECT_PREVALENCE")
      )
    )
  )]

  # Join with data_features_temp
  data$data_features <- data$data_features[data_features_temp, on = list(get("CONCEPT_ID")), nomatch = 0]

  if (applyZTest) {
    data$data_features <- data$data_features[get("ZTEST") == TRUE]
  }

  if (applyLogitTest) {
    data$data_features <- data$data_features[get("LOGITTEST") == TRUE]
  }

  if (applyInverseTarget) {
    # Invert target and control groups
    data$data_patients[get("ABSTRACTION_LEVEL") == abstractionLevel, get("COHORT_DEFINITION_ID") := data.table::fifelse(get("COHORT_DEFINITION_ID") == "control", "target", "control")]
    data$data_initial[, get("COHORT_DEFINITION_ID") := data.table::fifelse(get("COHORT_DEFINITION_ID") == "control", "target", "control")]

    data$data_features <- data$data_features[get("ABSTRACTION_LEVEL") == abstractionLevel, `:=`(
      TARGET_SUBJECT_COUNT = get("CONTROL_SUBJECT_COUNT"),
      CONTROL_SUBJECT_COUNT = get("TARGET_SUBJECT_COUNT"),
      TARGET_SUBJECT_PREVALENCE = get("CONTROL_SUBJECT_PREVALENCE"),
      CONTROL_SUBJECT_PREVALENCE = get("TARGET_SUBJECT_PREVALENCE")
    )][, get("PREVALENCE_DIFFERENCE_RATIO") := data.table::fifelse(
      is.na(get("CONTROL_SUBJECT_PREVALENCE")) | get("CONTROL_SUBJECT_PREVALENCE") == 0 & is.na(get("TARGET_SUBJECT_PREVALENCE")), -1,
      data.table::fifelse(
        is.na(get("CONTROL_SUBJECT_PREVALENCE")) | get("CONTROL_SUBJECT_PREVALENCE") == 0,
        get("TARGET_SUBJECT_PREVALENCE") / (1 / count_control),
        data.table::fifelse(
          is.na(get("TARGET_SUBJECT_PREVALENCE")) | get("TARGET_SUBJECT_PREVALENCE") == 0,
          (1 / count_target) / get("CONTROL_SUBJECT_PREVALENCE"),
          get("TARGET_SUBJECT_PREVALENCE") / get("CONTROL_SUBJECT_PREVALENCE")
        )
      )
    )]
  }

  if (autoScaleRate) {
    # Calculate the duration in the cohort in days
    data$data_initial[, get("COHORT_DURATION") := as.integer(difftime(as.Date(get("COHORT_END_DATE")), as.Date(get("COHORT_START_DATE")), units = "days"))]

    # Calculate the scaled prevalence for each PERSON_ID
    scaled_prevalence <- merge(
      data$data_patients[get("ABSTRACTION_LEVEL") == abstractionLevel],
      data$data_initial[, list(get("SUBJECT_ID"), get("COHORT_DURATION"))],
      by.x = "PERSON_ID", by.y = "SUBJECT_ID"
    )[, get("SCALED_PREVALENCE") := get("PREVALENCE") / (get("COHORT_DURATION") / 365)]

    # Update features with scaled prevalence
    data$data_features <- update_features(data$data_features, scaled_prevalence)[, list(
      get("CONCEPT_ID"),
      get("CONCEPT_NAME"),
      get("PREVALENCE_DIFFERENCE_RATIO"),
      get("TARGET_SUBJECT_COUNT"),
      get("CONTROL_SUBJECT_COUNT"),
      get("TARGET_SUBJECT_PREVALENCE"),
      get("CONTROL_SUBJECT_PREVALENCE"),
      get("ABSTRACTION_LEVEL")
    )]
  }

  concepts <- unique(data$data_patients[get("ABSTRACTION_LEVEL") == abstractionLevel, list(get("CONCEPT_ID"), get("CONCEPT_NAME"), get("HERITAGE"), get("ABSTRACTION_LEVEL"))]
                     [get("CONCEPT_ID") != 0])[data$data_features, on = list(get("CONCEPT_ID"), get("CONCEPT_NAME"), get("ABSTRACTION_LEVEL")), nomatch = 0][
                       !is.na(get("PREVALENCE_DIFFERENCE_RATIO"))
                     ]
  target <- merge(
    data$data_patients[get("ABSTRACTION_LEVEL") == abstractionLevel & get("COHORT_DEFINITION_ID") == "target"],
    concepts,
    by = c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE", "ABSTRACTION_LEVEL")
  )[!is.na(get("PREVALENCE_DIFFERENCE_RATIO"))]

  # Calculate target_df with appropriate type handling
  target_dt <- data.table::as.data.table(target)

  # Calculate NPATIENTS and PRESENT
  target_dt[, get("NPATIENTS") := data.table::uniqueN(get("PERSON_ID"))]
  target_dt[, get("PRESENT") := 1]

  # Calculate NCONCEPTS by grouping by CONCEPT_ID
  target_dt[, get("NCONCEPTS") := data.table::.N, by = get("CONCEPT_ID")]

  # Calculate PREVALENCE
  target_dt[, get("PREVALENCE") := get("NCONCEPTS") / get("NPATIENTS")]

  # Pivot wider
  wider_dt <- data.table::dcast(
    target_dt,
    get("CONCEPT_ID") + get("CONCEPT_NAME") + get("HERITAGE") + get("PREVALENCE_DIFFERENCE_RATIO") + get("PREVALENCE") ~ get("PERSON_ID"),
    value.var = "PRESENT",
    fill = 0
  )

  # Store the result of grep in a variable
  columns_to_rename <- grep("^[0-9]", names(wider_dt), value = TRUE)
  # Check if there are any columns to rename
  if (length(columns_to_rename) > 0) {
    # Rename the columns that start with a number
    data.table::setnames(wider_dt,
                         old = columns_to_rename,
                         new = paste0("PID_", columns_to_rename)
    )
  } else {
    message("No columns starting with a number were found.")
  }
  # Filter and mutate the results
  target_df <- data.table::as.data.table(wider_dt[
    "CONCEPT_ID" != 999999999 & "CONCEPT_ID" != "0"
  ])
  target_df[, get("CONCEPT_ID") := as.character(get("CONCEPT_ID"))]

  target_df[, get("CONCEPT_NAME") := gsub("(.{45})", "\\1\n", get("CONCEPT_NAME"))] # nolint
  # Create target_row_annotation as a data.frame with rownames
  target_row_annotation <- as.data.frame(target_df[, data.table::.SD, .SDcols = !"CONCEPT_ID"])
  rownames(target_row_annotation) <- target_df$CONCEPT_ID
  target_row_annotation$CONCEPT_ID <- NULL

  target_matrix <- as.matrix(target_df[, data.table::.SD, .SDcols = patterns("^PID_")])
  rownames(target_matrix) <- target_df$CONCEPT_ID
  # Demographics
  target_col_annotation <- merge(
    data$data_person,
    data$data_initial[get("COHORT_DEFINITION_ID") == "target", data.table::.SD[which.min(get("COHORT_START_DATE"))], by = get("SUBJECT_ID")][, list(PERSON_ID = get("SUBJECT_ID"), get("COHORT_START_DATE"))],
    by = "PERSON_ID"
  )[, `:=`(
    AGE = floor(as.numeric(difftime(get("COHORT_START_DATE"), as.Date(paste0(get("YEAR_OF_BIRTH"), "-01-01")), units = "days")) / 365.25),
    GENDER = factor(get("GENDER_CONCEPT_ID"), levels = c(8507, 8532), labels = c("Male", "Female"))
  )][, get("PERSON_ID") := paste0("PID_", get("PERSON_ID"))][, data.table::.SD, .SDcols = !c("GENDER_CONCEPT_ID")]
  target_col_annotation = as.data.frame(target_col_annotation)
  rownames(target_col_annotation) = target_col_annotation$PERSON_ID
  res <- list(
    target_matrix = target_matrix,
    target_row_annotation = target_row_annotation,
    target_col_annotation = target_col_annotation
  )
  return(res)
}

filter_target <- function(target, prevalence_threshold = 0.01, prevalence_ratio_threshold = 1, domains, removeUntreated = FALSE) {
  res <- target
  # Apply filters using data.table
  res$target_row_annotation <- res$target_row_annotation[
    res$target_row_annotation$PREVALENCE >= prevalence_threshold &
      res$target_row_annotation$PREVALENCE_DIFFERENCE_RATIO > prevalence_ratio_threshold &
      res$target_row_annotation$HERITAGE %in% domains
    ,]

  # Ensure rownames are aligned with res$target_matrix
  if (!all(rownames(res$target_row_annotation) %in% rownames(res$target_matrix))) {
    stop("Error: Row names in target_row_annotation do not match those in target_matrix.")
  }

  # Update target_matrix to only include rows from filtered target_row_annotation
  res$target_matrix <- res$target_matrix[rownames(res$target_row_annotation), ]

  if (removeUntreated) {
    # Calculate column sums and filter out columns with zero sum
    column_sums <- colSums(res$target_matrix, na.rm = TRUE)
    non_zero_colnames <- colnames(res$target_matrix)[column_sums != 0]

    # Check if any columns have non-zero sums
    if (length(non_zero_colnames) == 0) {
      stop("Error: No columns with non-zero sums found in target_matrix.")
    }

    # Subset the matrix to keep only columns where the sum is not zero
    res$target_matrix <- res$target_matrix[, non_zero_colnames, drop = FALSE]

    # Filter target_col_annotation to match the filtered target_matrix
    res$target_col_annotation <- res$target_col_annotation[
      rownames(res$target_col_annotation) %in% colnames(res$target_matrix), ,
      drop = FALSE
    ]
  }

  return(res)
}

#' @keywords internal
plot_prevalence <- function(filtered_target) {
  # Check if the input is NULL or empty
  if (is.null(filtered_target) || nrow(filtered_target$target_row_annotation) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text",
                               x = 0.5, y = 0.5, label = "After filtering there are no concepts left",
                               hjust = 0.5, vjust = 0.5, size = 20, fontface = "bold", color = "black"
             ) +
             ggplot2::theme_void())
  }
  # TODO: IF nrow(filtered_target$target_matrix) is NULL print some relevant wanring message, it means too harsh filters 0 people will remain
  plotdata <- as.data.frame(filtered_target$target_row_annotation) %>%
    tibble::rownames_to_column("CONCEPT_ID") %>%
    tibble::as_tibble() %>%
    dplyr::left_join(
      tibble::tibble(
        CONCEPT_ID = rownames(filtered_target$target_row_annotation),
        PRESENCE = if (is.null(nrow(filtered_target$target_matrix)) || nrow(filtered_target$target_matrix) == 1) {
          list(as.matrix(filtered_target$target_matrix)[, 1])
        } else {
          lapply(seq_len(nrow(filtered_target$target_matrix)), function(i) as.matrix(filtered_target$target_matrix)[i, ])
        }
      ),
      by = "CONCEPT_ID"
    ) %>%
    dplyr::mutate(PREVALENCE = purrr::map_dbl(.data$PRESENCE, mean)) %>%
    dplyr::mutate(AVERAGE_AGE = purrr::map_dbl(.data$PRESENCE, ~ mean(
      filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)],
      na.rm = TRUE
    ))) %>%
    dplyr::mutate(AVERAGE_AGE_OVERALL = mean(filtered_target$target_col_annotation$AGE, na.rm = TRUE)) %>%
    dplyr::mutate(AGE_DIFF = purrr::map(.data$PRESENCE, ~ {
      data <- filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)]
      if (length(data[!is.na(data)]) >= 2 && length(unique(data[!is.na(data)])) > 1) { # Ensure there are at least two non-NA observations
        t.test(data)
      } else {
        list(estimate = mean(data, na.rm = TRUE), conf.int = c(mean(data, na.rm = TRUE), mean(data, na.rm = TRUE)), p.value = NA) # Returning a list similar to t.test output structure
      }
    })) %>%
    dplyr::mutate(AGE_DIFF_ESTIMATE = purrr::map_dbl(.data$AGE_DIFF, ~ .x$estimate[1])) %>%
    dplyr::mutate(AGE_DIFF_LOW = purrr::map_dbl(.data$AGE_DIFF, ~ .x$conf.int[1])) %>%
    dplyr::mutate(AGE_DIFF_HIGH = purrr::map_dbl(.data$AGE_DIFF, ~ .x$conf.int[2])) %>%
    dplyr::mutate(AGE_DIFF_SIGNIFICANT = dplyr::if_else((.data$AGE_DIFF_HIGH > .data$AVERAGE_AGE_OVERALL) & (.data$AGE_DIFF_LOW < .data$AVERAGE_AGE_OVERALL), FALSE, TRUE)) %>%
    dplyr::mutate(MALE_PROP = purrr::map_dbl(.data$PRESENCE, ~ mean(filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male", na.rm = TRUE))) %>%
    dplyr::mutate(MALE_PROP_OVERALL = mean(filtered_target$target_col_annotation$GENDER == "Male", na.rm = TRUE)) %>%
    dplyr::mutate(MALE_PROP_DIFF = purrr::map(.data$PRESENCE, ~ {
      data <- filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male"
      if (sum(data, na.rm = TRUE) >= 2 && length(unique(data[!is.na(data)])) > 1) { # Ensure there are at least two non-NA observations
        t.test(data)
      } else {
        list(estimate = mean(data, na.rm = TRUE), conf.int = c(mean(data, na.rm = TRUE), mean(data, na.rm = TRUE)), p.value = NA)
      }
    })) %>%
    dplyr::mutate(MALE_PROP_DIFF_ESTIMATE = purrr::map_dbl(.data$MALE_PROP_DIFF, ~ .x$estimate)) %>%
    dplyr::mutate(MALE_PROP_DIFF_LOW = purrr::map_dbl(.data$MALE_PROP_DIFF, ~ .x$conf.int[1])) %>%
    dplyr::mutate(MALE_PROP_DIFF_HIGH = purrr::map_dbl(.data$MALE_PROP_DIFF, ~ .x$conf.int[2])) %>%
    dplyr::mutate(MALE_PROP_DIFF_SIGNIFICANT = dplyr::if_else((.data$MALE_PROP_DIFF_HIGH > .data$MALE_PROP_OVERALL) & (.data$MALE_PROP_DIFF_LOW < .data$MALE_PROP_OVERALL), FALSE, TRUE)) %>%
    dplyr::mutate(PREVALENCE_LOG = dplyr::if_else(.data$PREVALENCE_DIFFERENCE_RATIO < 1, 0, dplyr::if_else(.data$PREVALENCE_DIFFERENCE_RATIO > 100, 2, log10(.data$PREVALENCE_DIFFERENCE_RATIO))))
  # Plot
  p1 <- ggplot2::ggplot(plotdata, ggplot2::aes(x = .data$PREVALENCE, y = .data$CONCEPT_NAME, fill = .data$PREVALENCE_LOG)) +
    geom_bar(stat = "identity") +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    scale_fill_viridis_c(
      "Risk ratio (log10-scaled)\ncompared to background",
      limits = c(0, 2),
      oob = scales::squish
    ) + # Ensure fill values are between 0 and 3
    scale_x_continuous(labels = scales::label_percent()) +
    ggtitle("Prevalence") +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.text.y = element_text(size = 15) # Adjust the size as needed
    )

  p2 <- ggplot2::ggplot(plotdata, ggplot2::aes(y = .data$CONCEPT_NAME, color = .data$AGE_DIFF_SIGNIFICANT)) +
    ggplot2::geom_point(ggplot2::aes(x = .data$AGE_DIFF_ESTIMATE)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$AGE_DIFF_LOW, xmax = .data$AGE_DIFF_HIGH), linewidth = 5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$AVERAGE_AGE_OVERALL), color = "darkgreen") +
    ggplot2::scale_color_manual(values = c("grey60", "blue"), breaks = c(FALSE, TRUE)) +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::ggtitle("AGE in group") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      legend.position = "none"
    )

  p3 <- ggplot2::ggplot(plotdata, ggplot2::aes(y = .data$CONCEPT_NAME, color = .data$MALE_PROP_DIFF_SIGNIFICANT)) +
    ggplot2::geom_point(ggplot2::aes(x = .data$MALE_PROP_DIFF_ESTIMATE)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$MALE_PROP_DIFF_LOW, xmax = .data$MALE_PROP_DIFF_HIGH), linewidth = 5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$MALE_PROP_OVERALL), color = "darkgreen") +
    ggplot2::scale_color_manual(values = c("grey60", "blue"), breaks = c(FALSE, TRUE)) +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::ggtitle("Male percentage in group") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "none"
    )

  # Combine plots
  p <- p1 + p2 + p3 + patchwork::plot_layout(nrow = 1, heights = c(1, 1, 1))

  return(p)
}
#' @keywords internal
plot_heatmap <- function(filtered_target) {
  # Check if the input is NULL or empty
  if (is.null(filtered_target) || nrow(filtered_target$target_row_annotation) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text",
                               x = 0.5, y = 0.5, label = "After filtering there are no concepts left",
                               hjust = 0.5, vjust = 0.5, size = 12, fontface = "bold", color = "black"
             ) +
             ggplot2::theme_void())
  } else if (is.null(nrow(filtered_target$target_matrix))){
    filtered_target$target_matrix <- t(as.matrix(filtered_target$target_matrix)[1:length(filtered_target$target_matrix),, drop = FALSE])
    rownames(filtered_target$target_matrix) <- rownames(filtered_target$target_row_annotation)
  }

  filtered_target$target_row_annotation <- as.data.frame(filtered_target$target_row_annotation)
  filtered_target$target_col_annotation <- as.data.frame(filtered_target$target_col_annotation)
  rownames(filtered_target$target_col_annotation) <- filtered_target$target_col_annotation$PERSON_ID

  # Patient clustering
  col_clustering <- stats::hclust(stats::dist(t(filtered_target$target_matrix)))
  # Row reordering
  reordering <- filtered_target$target_row_annotation %>%
    tibble::rownames_to_column("CONCEPT_ID") %>%
    dplyr::group_by(.data$HERITAGE) %>%
    dplyr::arrange(dplyr::desc(.data$CONCEPT_NAME)) %>%
    dplyr::summarize(CONCEPT_ID = list(.data$CONCEPT_ID)) %>%
    dplyr::mutate(MATRIX = purrr::map(.data$CONCEPT_ID, ~ filtered_target$target_matrix[.x, , drop = F])) %>%
    dplyr::mutate(MATRIX = purrr::map(
      .data$MATRIX,
      function(x) {
        if (nrow(x) > 1) {
          x <- x[stats::hclust(stats::dist(x))$order, ]
        }

        return(x)
      }
    ))
  tm <- as.data.frame(do.call(rbind, reordering$MATRIX))
  tm_gaps <- purrr::map_int(reordering$MATRIX, nrow) %>% cumsum()
  tm2 <- as.matrix(tm)
  rownames(tm2) <- filtered_target$target_row_annotation[rownames(tm), ]$CONCEPT_NAME
  annotation_row <- filtered_target$target_row_annotation %>%
    dplyr::arrange(.data$CONCEPT_NAME) %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames("CONCEPT_NAME") %>%
    dplyr::select(-.data$PREVALENCE_DIFFERENCE_RATIO)
  annotation_col <- filtered_target$target_col_annotation
  # Heritage colors pre-defined
  heritage_colors <- c(
    procedure_occurrence = "darkblue",
    condition_occurrence = "orange",
    drug_exposure = "lightblue",
    measurement = "pink",
    observation = "brown",
    visit_occurrence = "darkgreen",
    visit_detail = "lightgreen"
  )

  active_gender_colors <- c(Male = "purple", Female = "pink", Other = "lightblue")

  # Filter heritage colors based on what's present in annotation_row
  active_heritage_colors <- heritage_colors[names(heritage_colors) %in% unique(annotation_row$HERITAGE)]
  active_gender_colors <- active_gender_colors[names(active_gender_colors) %in% unique(annotation_col$GENDER)]


  # Specify colors including a dynamic gradient for AGE
  annotation_colors <- list(
    AGE = grDevices::colorRampPalette(c("lightblue", "firebrick"))(length(unique(annotation_col$AGE))),
    GENDER = active_gender_colors,
    HERITAGE = active_heritage_colors,
    PREVALENCE = grDevices::colorRampPalette(c("white", "purple"))(length(unique(annotation_row$PREVALENCE)))
  )

  # tm2 <- tm2[order(rownames(tm2), decreasing = TRUE), ]
  # annotation_row
  annotation_row <- annotation_row %>%
    tibble::rownames_to_column("CONCEPT_NAME") %>%
    dplyr::arrange(.data$HERITAGE, dplyr::desc(toupper(.data$CONCEPT_NAME))) %>%
    tibble::column_to_rownames("CONCEPT_NAME")
  # Apply the same order to the matrix
  tm2 <- tm2[rownames(annotation_row), ]

  pheatmap::pheatmap(
    tm2,
    show_colnames = FALSE,
    annotation_row = annotation_row %>% dplyr::select(.data$HERITAGE),
    annotation_col = annotation_col %>% dplyr::select(.data$AGE, .data$GENDER),
    annotation_colors = annotation_colors,
    cluster_cols = col_clustering,
    cluster_rows = FALSE,
    gaps_row = tm_gaps,
    color = c("#e5f5f9", "#2ca25f"),
    legend_breaks = c(0.25, 0.75),
    legend_labels = c("Absent", "Present")
  )
}

#' @keywords internal
update_features <- function(features, scaled_prev) {
  # Convert scaled_prev to data.table if not already
  scaled_prev <- data.table::as.data.table(scaled_prev)

  # Calculate AVG_SCALED_PREVALENCE by grouping by CONCEPT_ID and COHORT_DEFINITION_ID
  scaled_prev <- scaled_prev[, list(AVG_SCALED_PREVALENCE = stats::median(get("SCALED_PREVALENCE"), na.rm = TRUE)),
                             by = list(get("CONCEPT_ID"), get("COHORT_DEFINITION_ID"))
  ]

  # Pivot to wide format
  scaled_prev <- data.table::dcast(scaled_prev, get("CONCEPT_ID") ~ get("COHORT_DEFINITION_ID"),
                                   value.var = "AVG_SCALED_PREVALENCE",
                                   fill = -1,
                                   prefix = "COHORT_"
  )

  # Calculate PREVALENCE_DIFFERENCE_RATIO
  scaled_prev[, get("PREVALENCE_DIFFERENCE_RATIO") := data.table::fcase(
    get("COHORT_control") == -1, get("COHORT_target") / 1,
    get("COHORT_target") == -1, 1 / get("COHORT_control"),
    default = get("COHORT_target") / get("COHORT_control")
  )]

  # Select relevant columns
  scaled_prev <- scaled_prev[, list(get("CONCEPT_ID"), get("PREVALENCE_DIFFERENCE_RATIO"))]

  # Convert features to data.table if not already
  features <- data.table::as.data.table(features)

  # Remove existing PREVALENCE_DIFFERENCE_RATIO column if it exists
  features <- features[, !"PREVALENCE_DIFFERENCE_RATIO", with = FALSE]

  # Join the updated scaled_prev data back into the features
  features <- features[scaled_prev, on = "CONCEPT_ID", nomatch = 0]

  return(features)
}
