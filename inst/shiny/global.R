

format_results <-
  function(data,
           autoScaleRate,
           applyInverseTarget,
           applyZTest,
           applyLogitTest,
           abstractionLevel) {
    if (data$config$safeRun)
    {
      return(data$formattedResults)
    }
    # Convert data frames to data.table
    data$data_initial <-
      data.table::as.data.table(data$data_initial)
    data$data_features <-
      data.table::as.data.table(data$data_features)
    data$data_patients <-
      data.table::as.data.table(data$data_patients)
    data$data_person <- data.table::as.data.table(data$data_person)
    # Calculate the number of patients in target and control groups
    n_patients <- data$data_initial[, .N, by = COHORT_DEFINITION_ID]

    n_patients <-
      reshape2::dcast(n_patients,
                      1 ~ COHORT_DEFINITION_ID,
                      value.var = "N",
                      fill = 0)

    count_target <- n_patients$target

    count_control <- n_patients$control



    # Update data features with prevalence calculations
    data_features_temp <-
      data$data_features[ABSTRACTION_LEVEL == abstractionLevel, .(
        CONCEPT_ID,
        ZTEST,
        LOGITTEST,
        KSTEST,
        ABSTRACTION_LEVEL
      )]

    # First, calculate the counts for target and control subjects
    data$data_features <- data$data_patients[ABSTRACTION_LEVEL == abstractionLevel,
                                             .(
                                               TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "target" &
                                                                            PREVALENCE > 0),
                                               CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "control" &
                                                                             PREVALENCE > 0)
                                             ),
                                             by = .(CONCEPT_ID, CONCEPT_NAME)]

    # Now, calculate the prevalences
    data$data_features[, `:=`(
      TARGET_SUBJECT_PREVALENCE = TARGET_SUBJECT_COUNT / count_target,
      CONTROL_SUBJECT_PREVALENCE = CONTROL_SUBJECT_COUNT / count_control
    )]

    # Finally, calculate the PREVALENCE_DIFFERENCE_RATIO
    data$data_features[, PREVALENCE_DIFFERENCE_RATIO := data.table::fifelse(
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
    data$data_features <-
      data$data_features[data_features_temp, on = .(CONCEPT_ID), nomatch = 0]

    if (applyZTest) {
      data$data_features <- data$data_features[ZTEST == TRUE]
    }

    if (applyLogitTest) {
      data$data_features <- data$data_features[LOGITTEST == TRUE]
    }

    if (applyInverseTarget) {
      # Invert target and control groups
      data$data_patients[ABSTRACTION_LEVEL == abstractionLevel, COHORT_DEFINITION_ID := data.table::fifelse(COHORT_DEFINITION_ID == "control", "target", "control")]
      data$data_initial[, COHORT_DEFINITION_ID := data.table::fifelse(COHORT_DEFINITION_ID == "control", "target", "control")]

      data$data_features <-
        data$data_features[ABSTRACTION_LEVEL == abstractionLevel, `:=`(
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
            TARGET_SUBJECT_PREVALENCE / (1 / count_control),
            data.table::fifelse(
              is.na(TARGET_SUBJECT_PREVALENCE) | TARGET_SUBJECT_PREVALENCE == 0,
              (1 / count_target) / CONTROL_SUBJECT_PREVALENCE,
              TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
            )
          )
        )]
    }

    if (autoScaleRate) {
      # Calculate the duration in the cohort in days
      data$data_initial[, COHORT_DURATION := as.integer(difftime(
        as.Date(COHORT_END_DATE),
        as.Date(COHORT_START_DATE),
        units = "days"
      ))]
      # Calculate the scaled prevalence for each PERSON_ID
      scaled_prevalence <- merge(
        data$data_patients[ABSTRACTION_LEVEL == abstractionLevel],
        data$data_initial[, .(SUBJECT_ID, COHORT_DURATION)],
        by.x = "PERSON_ID",
        by.y = "SUBJECT_ID",
        allow.cartesian = TRUE
      )[, SCALED_PREVALENCE := PREVALENCE / (COHORT_DURATION / 365)]
      # Update features with scaled prevalence
      data$data_features <-
        update_features(data$data_features, scaled_prevalence)[, .(
          CONCEPT_ID,
          CONCEPT_NAME,
          PREVALENCE_DIFFERENCE_RATIO,
          TARGET_SUBJECT_COUNT,
          CONTROL_SUBJECT_COUNT,
          TARGET_SUBJECT_PREVALENCE,
          CONTROL_SUBJECT_PREVALENCE,
          ABSTRACTION_LEVEL
        )]
    }

    concepts <-
      unique(data$data_patients[ABSTRACTION_LEVEL == abstractionLevel, .(CONCEPT_ID, CONCEPT_NAME, HERITAGE, ABSTRACTION_LEVEL)]
             [CONCEPT_ID != 0])[data$data_features, on = .(CONCEPT_ID, CONCEPT_NAME, ABSTRACTION_LEVEL), nomatch = 0][!is.na(PREVALENCE_DIFFERENCE_RATIO)]
    target <- merge(
      data$data_patients[ABSTRACTION_LEVEL == abstractionLevel &
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
      CONCEPT_ID + CONCEPT_NAME + HERITAGE + PREVALENCE_DIFFERENCE_RATIO + PREVALENCE  ~ PERSON_ID,
      value.var = "PRESENT",
      fill = 0
    )
    data.table::setkey(wider_dt, CONCEPT_ID)
    data.table::setkey(data$data_features, CONCEPT_ID)

    # # Perform the left join and select specific columns
    # wider_dt <-
    #   wider_dt[data$data_features[, .(CONCEPT_ID, TIME_TO_EVENT)], nomatch = 0]

    #wider_dt = wider_dt %>% dplyr::left_join(data$data_features %>% dplyr::select(CONCEPT_ID, TIME_TO_EVENT), by = 'CONCEPT_ID')
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

    target_df[, CONCEPT_NAME := gsub("(.{30})", "\\1\n", CONCEPT_NAME)] # nolint
    # Create target_row_annotation as a data.frame with rownames

    target_row_annotation <-
      as.data.frame(target_df[, .SD, .SDcols = !"CONCEPT_ID"])
    rownames(target_row_annotation) <- target_df$CONCEPT_ID
    target_row_annotation$CONCEPT_ID <- NULL

    pid_cols <- grep("^PID_", colnames(target_df), value = TRUE)
    target_matrix <- as.matrix(target_df[, ..pid_cols])
    rownames(target_matrix) <- target_df$CONCEPT_ID
    # Demographics
    target_col_annotation <- merge(data$data_person,
                                   data$data_initial[COHORT_DEFINITION_ID == "target", .SD[which.min(COHORT_START_DATE)], by = SUBJECT_ID][, .(PERSON_ID = SUBJECT_ID, COHORT_START_DATE)],
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

    target_time_annotation <- data$data_patients[
      COHORT_DEFINITION_ID == "target" & ABSTRACTION_LEVEL == abstractionLevel,
      .(PERSON_ID, CONCEPT_ID, CONCEPT_NAME, TIME_TO_EVENT, HERITAGE)
    ]
    target_time_annotation <- target_time_annotation[
      data$data_features,
      on = .(CONCEPT_ID),
      .(PERSON_ID, CONCEPT_ID, CONCEPT_NAME, TIME_TO_EVENT, HERITAGE, KSTEST),
      nomatch = 0L  # 0L in nomatch ensures all rows in x are returned, including those without a match in y
    ]

    target_time_annotation[, CONCEPT_NAME := gsub("(.{30})", "\\1\n", CONCEPT_NAME)] # nolint


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
             "visit_detail"
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


plot_prevalence <- function(filtered_target) {
  # Check if the input is NULL or empty
  if (is.null(filtered_target) ||
      nrow(filtered_target$target_row_annotation) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "After filtering there are no concepts left",
          hjust = 0.5,
          vjust = 0.5,
          size = 20,
          fontface = "bold",
          color = "black"
        ) +
        ggplot2::theme_void()
    )
  }
  # TODO: IF nrow(filtered_target$target_matrix) is NULL print some relevant wanring message, it means too harsh filters 0 people will remain
  plotdata <-
    as.data.frame(filtered_target$target_row_annotation) %>%
    tibble::rownames_to_column("CONCEPT_ID") %>%
    tibble::as_tibble() %>%
    dplyr::left_join(tibble::tibble(
      CONCEPT_ID = rownames(filtered_target$target_row_annotation),
      PRESENCE = if (is.null(nrow(filtered_target$target_matrix)) ||
                     nrow(filtered_target$target_matrix) == 1) {
        list(as.matrix(filtered_target$target_matrix)[, 1])
      } else {
        lapply(seq_len(nrow(filtered_target$target_matrix)), function(i)
          as.matrix(filtered_target$target_matrix)[i, ])
      }
    ),
    by = "CONCEPT_ID") %>%
    dplyr::mutate(PREVALENCE = purrr::map_dbl(.data$PRESENCE, mean)) %>%
    dplyr::mutate(AVERAGE_AGE = purrr::map_dbl(
      .data$PRESENCE,
      ~ mean(filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)],
             na.rm = TRUE)
    )) %>%
    dplyr::mutate(AVERAGE_AGE_OVERALL = mean(filtered_target$target_col_annotation$AGE, na.rm = TRUE)) %>%
    dplyr::mutate(AGE_DIFF = purrr::map(.data$PRESENCE, ~ {
      data <-
        filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)]
      if (length(data[!is.na(data)]) >= 2 &&
          length(unique(data[!is.na(data)])) > 1) {
        # Ensure there are at least two non-NA observations
        t.test(data)
      } else {
        list(
          estimate = mean(data, na.rm = TRUE),
          conf.int = c(mean(data, na.rm = TRUE), mean(data, na.rm = TRUE)),
          p.value = NA
        ) # Returning a list similar to t.test output structure
      }
    })) %>%
    dplyr::mutate(AGE_DIFF_ESTIMATE = purrr::map_dbl(.data$AGE_DIFF, ~ .x$estimate[1])) %>%
    dplyr::mutate(AGE_DIFF_LOW = purrr::map_dbl(.data$AGE_DIFF, ~ .x$conf.int[1])) %>%
    dplyr::mutate(AGE_DIFF_HIGH = purrr::map_dbl(.data$AGE_DIFF, ~ .x$conf.int[2])) %>%
    dplyr::mutate(AGE_DIFF_SIGNIFICANT = dplyr::if_else((.data$AGE_DIFF_HIGH > .data$AVERAGE_AGE_OVERALL) &
                                                          (.data$AGE_DIFF_LOW < .data$AVERAGE_AGE_OVERALL),
                                                        FALSE,
                                                        TRUE
    )) %>%
    dplyr::mutate(MALE_PROP = purrr::map_dbl(
      .data$PRESENCE,
      ~ mean(filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male", na.rm = TRUE)
    )) %>%
    dplyr::mutate(
      MALE_PROP_OVERALL = mean(
        filtered_target$target_col_annotation$GENDER == "Male",
        na.rm = TRUE
      )
    ) %>%
    dplyr::mutate(MALE_PROP_DIFF = purrr::map(.data$PRESENCE, ~ {
      data <-
        filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male"
      if (sum(data, na.rm = TRUE) >= 2 &&
          length(unique(data[!is.na(data)])) > 1) {
        # Ensure there are at least two non-NA observations
        t.test(data)
      } else {
        list(
          estimate = mean(data, na.rm = TRUE),
          conf.int = c(mean(data, na.rm = TRUE), mean(data, na.rm = TRUE)),
          p.value = NA
        )
      }
    })) %>%
    dplyr::mutate(MALE_PROP_DIFF_ESTIMATE = purrr::map_dbl(.data$MALE_PROP_DIFF, ~ .x$estimate)) %>%
    dplyr::mutate(MALE_PROP_DIFF_LOW = purrr::map_dbl(.data$MALE_PROP_DIFF, ~ .x$conf.int[1])) %>%
    dplyr::mutate(MALE_PROP_DIFF_HIGH = purrr::map_dbl(.data$MALE_PROP_DIFF, ~ .x$conf.int[2])) %>%
    dplyr::mutate(MALE_PROP_DIFF_SIGNIFICANT = dplyr::if_else((.data$MALE_PROP_DIFF_HIGH > .data$MALE_PROP_OVERALL) &
                                                                (.data$MALE_PROP_DIFF_LOW < .data$MALE_PROP_OVERALL),
                                                              FALSE,
                                                              TRUE
    )) %>%
    dplyr::mutate(
      PREVALENCE_LOG = dplyr::if_else(
        .data$PREVALENCE_DIFFERENCE_RATIO < 1,
        0,
        dplyr::if_else(
          .data$PREVALENCE_DIFFERENCE_RATIO > 100,
          2,
          log10(.data$PREVALENCE_DIFFERENCE_RATIO)
        )
      )
    )
  # Plot

  heritage_colors <-
    data.frame(
      HERITAGE = c(
        "procedure_occurrence",
        "condition_occurrence",
        "drug_exposure",
        "measurement",
        "observation",
        "visit_occurrence",
        "visit_detail"
      ),
      color = c(
        "#FFFFCC",  # Lighter version of #4B99C9
        "#FFE1B4",  # Lighter version of #FFB347
        "#D7CDE1",  # Lighter version of #B39EB5
        "#FFD9D5",  # Lighter version of #FFB3AB
        "#C3ECC3",  # Lighter version of #77DD77
        "#A1C8E4",  # Lighter version of #FDFD96
        "#D6E5EC"   # Lighter version of #AEC6CF
      )
    )
  # Merge the colors into the main dataset
  plotdata <-
    merge(plotdata, heritage_colors, by = "HERITAGE", all.x = TRUE)
  # Plot
  # Plot
  p1 <-
    ggplot2::ggplot(
      plotdata,
      ggplot2::aes(
        x = .data$PREVALENCE,
        y = forcats::fct_reorder(stringr::str_sub(.data$CONCEPT_NAME, 1, 60), .data$PREVALENCE_LOG),
        fill = .data$PREVALENCE_LOG
      )
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::scale_fill_gradientn(
      name = "Risk ratio (1-100+ scale)",
      limits = c(0, 2),  # Assuming PREVALENCE_LOG is log10-scaled and ranges from 0 to log10(100)
      oob = scales::squish,  # To keep values within limits
      colours = c("#a7c7e7", "#e6ccb2", "#8e5a3b"), # Green to Red gradient
      breaks = log10(c(1, 10, 100)),  # Convert non-log breaks (1, 10, 100) to log scale
      labels = c("1", "10", "100")  # Labels corresponding to non-log values
    ) + # Labels corresponding to non-log values
    ggplot2::scale_x_continuous(labels = scales::label_percent(),
                                sec.axis = ggplot2::dup_axis()  # Duplicate axis on the top
    ) +
    ggplot2::ggtitle("Prevalence") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      legend.position = "top",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.5, "lines"),
      axis.text.y = ggplot2::element_text(size = 15) # Adjust the size as needed
    )

  p2 <-
    ggplot2::ggplot(plotdata,
                    ggplot2::aes(
                      y = forcats::fct_reorder(stringr::str_sub(.data$CONCEPT_NAME, 1, 60), .data$PREVALENCE_LOG),
                      color = .data$AGE_DIFF_SIGNIFICANT
                    )) +
    ggplot2::geom_rect(ggplot2::aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = .data$HERITAGE
    ),
    alpha = 0.5) +  # Use alpha to adjust visibility
    ggplot2::geom_point(ggplot2::aes(x = .data$AGE_DIFF_ESTIMATE)) +
    ggplot2::geom_errorbar(ggplot2::aes(
      xmin = .data$AGE_DIFF_LOW,
      xmax = .data$AGE_DIFF_HIGH
    ),
    linewidth = 5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$AVERAGE_AGE_OVERALL),
                        color = "darkgreen",linewidth = 2) +
    ggplot2::scale_fill_manual(values = stats::setNames(heritage_colors$color, heritage_colors$HERITAGE)) +  # Assign colors
    # ggplot2::scale_color_manual(values = c("#a7c7e7", "#8e5a3b"),
    #                             breaks = c(FALSE, TRUE)) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#8e5a3b", "FALSE" = "#a7c7e7"),
      name = "Difference from mean",  # Custom legend title
      labels = c("FALSE" = "Insignificant", "TRUE" = "Significant"),  # Custom labels
      breaks = c("TRUE", "FALSE"),  # Ensure both TRUE and FALSE are included in the legend
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis()  # Duplicate axis on the top
    ) +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::ggtitle("AGE in group") +
    ggplot2::theme_bw() +
    #scale_color_manual(values = setNames(heritage_colors$color, heritage_colors$HERITAGE)) +
    ggplot2::guides(
      fill = "none",
      color = ggplot2::guide_legend(ncol = 1)  # Force the fill legend to be vertical (1 column)
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      legend.position = "top",
      panel.spacing = unit(0.5, "lines")
    )


  # Create the plot with backgrounds
  p3 <-
    ggplot2::ggplot(
      plotdata,
      ggplot2::aes(
        y = forcats::fct_reorder(stringr::str_sub(.data$CONCEPT_NAME, 1, 60), .data$PREVALENCE_LOG),
        color = .data$MALE_PROP_DIFF_SIGNIFICANT
      )
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf,
        fill = .data$HERITAGE
      ),
      alpha = 0.5
    ) +  # Use alpha to adjust visibility
    ggplot2::geom_point(ggplot2::aes(x = .data$MALE_PROP_DIFF_ESTIMATE)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data$MALE_PROP_DIFF_LOW,
        xmax = .data$MALE_PROP_DIFF_HIGH
      ),
      linewidth = 5
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$MALE_PROP_OVERALL),
                        color = "darkgreen", linewidth = 2) +
    ggplot2::scale_fill_manual(values = stats::setNames(heritage_colors$color, heritage_colors$HERITAGE)) +  # Assign colors for HERITAGE
    ggplot2::scale_color_manual(values = c("#a7c7e7", "#8e5a3b"),
                                breaks = c(FALSE, TRUE)) +  # Colors for significant differences
    ggplot2::scale_x_continuous(labels = scales::label_percent(),
                                sec.axis = ggplot2::dup_axis()  # Duplicate axis on the top
    ) +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::ggtitle("Male percentage in group") +
    ggplot2::theme_bw() +
    ggplot2::guides(
      color = "none",  # Hide the color guide
      fill = ggplot2::guide_legend(ncol = 1)  # Force the fill legend to be vertical (1 column)
    ) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(0.5, "lines"),
      strip.background = ggplot2::element_blank(),
      # Hide the strip background
      legend.position = "top",
      strip.text.x = ggplot2::element_blank(),
      # Remove facet labels
      # Place the fill guide (HERITAGE) at the bottom
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )

  # Combine plots
  p <-
    p1 + p2 + p3 + patchwork::plot_layout(nrow = 1, heights = c(1, 1, 1))

  return(p)
}


plot_heatmap <- function(filtered_target) {
  # Check if the input is NULL or empty
  if (is.null(filtered_target) ||
      nrow(filtered_target$target_row_annotation) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "After filtering there are no concepts left",
          hjust = 0.5,
          vjust = 0.5,
          size = 12,
          fontface = "bold",
          color = "black"
        ) +
        ggplot2::theme_void()
    )
  } else if (is.null(nrow(filtered_target$target_matrix))) {
    filtered_target$target_matrix <-
      t(as.matrix(filtered_target$target_matrix)[1:length(filtered_target$target_matrix), , drop = FALSE])
    rownames(filtered_target$target_matrix) <-
      rownames(filtered_target$target_row_annotation)
  }

  filtered_target$target_row_annotation <-
    as.data.frame(filtered_target$target_row_annotation)
  filtered_target$target_col_annotation <-
    as.data.frame(filtered_target$target_col_annotation)
  rownames(filtered_target$target_col_annotation) <-
    filtered_target$target_col_annotation$PERSON_ID

  # Patient clustering
  col_clustering <-
    stats::hclust(stats::dist(t(filtered_target$target_matrix)))
  # Row reordering
  reordering <- filtered_target$target_row_annotation %>%
    tibble::rownames_to_column("CONCEPT_ID") %>%
    dplyr::group_by(.data$HERITAGE) %>%
    # Arranging with respect to other plots
    dplyr::arrange(dplyr::desc(.data$PREVALENCE_DIFFERENCE_RATIO)) %>%
    dplyr::summarize(CONCEPT_ID = list(.data$CONCEPT_ID)) %>%
    dplyr::mutate(MATRIX = purrr::map(.data$CONCEPT_ID, ~ filtered_target$target_matrix[.x, , drop = F])) %>%
    dplyr::mutate(MATRIX = purrr::map(.data$MATRIX,
                                      function(x) {
                                        if (nrow(x) > 1) {
                                          x <- x[stats::hclust(stats::dist(x))$order, ]
                                        }

                                        return(x)
                                      }))
  tm <- as.data.frame(do.call(rbind, reordering$MATRIX))
  tm_gaps <- purrr::map_int(reordering$MATRIX, nrow) %>% cumsum()
  tm2 <- as.matrix(tm)
  rownames(tm2) <-
    filtered_target$target_row_annotation[rownames(tm), ]$CONCEPT_NAME
  annotation_row <- filtered_target$target_row_annotation %>%
    # Arranging with respect to other plots
    dplyr::arrange(dplyr::desc(.data$PREVALENCE_DIFFERENCE_RATIO)) %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames("CONCEPT_NAME") %>%
    dplyr::select(-.data$PREVALENCE_DIFFERENCE_RATIO)
  annotation_col <- filtered_target$target_col_annotation
  # Heritage colors pre-defined
  heritage_colors <- c(
    procedure_occurrence = "#FFFFCC",
    # Pastel blue
    condition_occurrence = "#FFE1B4",
    # Pastel orange
    drug_exposure = "#D7CDE1",
    # Pastel purple
    measurement = "#FFD9D5",
    # Pastel red
    observation = "#C3ECC3",
    # Pastel green
    visit_occurrence = "#A1C8E4",
    # Pastel yellow
    visit_detail = "#D6E5EC"           # Pastel pink
  )

  active_gender_colors <-
    c(
      Male = "purple",
      Female = "pink",
      Other = "lightblue"
    )

  # Filter heritage colors based on what's present in annotation_row
  active_heritage_colors <-
    heritage_colors[names(heritage_colors) %in% unique(annotation_row$HERITAGE)]
  active_gender_colors <-
    active_gender_colors[names(active_gender_colors) %in% unique(annotation_col$GENDER)]


  # Specify colors including a dynamic gradient for AGE
  annotation_colors <- list(
    AGE = colorRampPalette(c("lightblue", "firebrick"))(length(unique(
      annotation_col$AGE
    ))),
    GENDER = active_gender_colors,
    HERITAGE = active_heritage_colors,
    PREVALENCE = colorRampPalette(c("white", "purple"))(length(unique(
      annotation_row$PREVALENCE
    )))
  )

  annotation_row <- annotation_row %>%
    tibble::rownames_to_column("CONCEPT_NAME") %>%
    dplyr::arrange(.data$HERITAGE) %>%
    tibble::column_to_rownames("CONCEPT_NAME")
  # Apply the same order to the matrix
  tm2 <- tm2[rownames(annotation_row), ]

  # Truncate row names to 60 characters and ensure uniqueness
  unique_names <- function(names) {
    short_names <- stringr::str_sub(names, 1, 60)
    # Append a suffix if there are duplicates
    if (length(unique(short_names)) < length(short_names)) {
      counts <- as.numeric(stats::ave(short_names, short_names, FUN = seq_along))
      short_names <- paste(short_names, counts, sep = "_")
    }
    return(short_names)
  }

  rownames(tm2) <- unique_names(rownames(tm2))
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

plot_time <- function(filtered_target) {
  # Check if the input is NULL or empty
  if (is.null(filtered_target) ||
      nrow(filtered_target$target_row_annotation) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "After filtering there are no concepts left",
          hjust = 0.5,
          vjust = 0.5,
          size = 12,
          fontface = "bold",
          color = "black"
        ) +
        ggplot2::theme_void()
    )
  } else if (is.null(nrow(filtered_target$target_matrix))) {
    filtered_target$target_matrix <-
      t(as.matrix(filtered_target$target_matrix)[1:length(filtered_target$target_matrix), , drop = FALSE])
    rownames(filtered_target$target_matrix) <-
      rownames(filtered_target$target_row_annotation)
  }

  filtered_target$target_time_annotation <-
    as.data.frame(filtered_target$target_time_annotation)

  # Heritage colors pre-defined
  heritage_colors <- c(
    procedure_occurrence = "#FFFFCC",
    # Pastel blue
    condition_occurrence = "#FFE1B4",
    # Pastel orange
    drug_exposure = "#D7CDE1",
    # Pastel purple
    measurement = "#FFD9D5",
    # Pastel red
    observation = "#C3ECC3",
    # Pastel green
    visit_occurrence = "#A1C8E4",
    # Pastel yellow
    visit_detail = "#D6E5EC"
  )

  plot_data <-
    filtered_target$target_time_annotation %>% dplyr::mutate(TIME_MEDIAN = sapply(.data$TIME_TO_EVENT, stats::median, na.rm = TRUE)) %>%
    dplyr::group_by(.data$CONCEPT_ID) %>%
    dplyr::summarise(
      CONCEPT_NAME = dplyr::first(.data$CONCEPT_NAME),
      # Taking the first instance assuming uniformity across the group
      HERITAGE = dplyr::first(.data$HERITAGE),
      # Similarly for HERITAGE
      TIME_TO_EVENT = list(unlist(.data$TIME_TO_EVENT)),
      TIME_MEDIAN = list(.data$TIME_MEDIAN),
      KSTEST = dplyr::first(.data$KSTEST),
      # Similarly for KSTEST
      .groups = "drop"                     # Drop grouping information after summarising
    )
  # Ordering in accordance with other plots
  concept_ordering <- filtered_target$target_row_annotation %>% dplyr::arrange(dplyr::desc(.data$PREVALENCE_DIFFERENCE_RATIO)) %>% dplyr::pull(CONCEPT_NAME)

  plot_data <- plot_data %>%
    dplyr::mutate(CONCEPT_NAME = factor(CONCEPT_NAME, levels = concept_ordering)) %>%
    dplyr::arrange(CONCEPT_NAME)

  # plot_data2 = plot_data %>% dplyr::select(.data$CONCEPT_NAME, .data$TIME_MEDIAN, .data$HERITAGE) %>% tidyr::unnest(.data$TIME_MEDIAN)  # Make sure TIME_MEDIAN is no longer a list
  plot_data = plot_data %>% dplyr::select(.data$CONCEPT_NAME,
                                          .data$TIME_TO_EVENT,
                                          .data$HERITAGE,
                                          .data$KSTEST) %>% tidyr::unnest(.data$TIME_TO_EVENT)  # Make sure TIME_MEDIAN is no longer a list

  # Filter heritage colors based on what's present in annotation_row
  active_heritage_colors <-
    heritage_colors[names(heritage_colors) %in% unique(filtered_target$target_time_annotation$HERITAGE)]

  # p1 <-
  #   ggplot2::ggplot(
  #     plot_data2,
  #     ggplot2::aes(
  #       x = .data$TIME_MEDIAN,
  #       y = forcats::fct_inorder(stringr::str_sub(.data$CONCEPT_NAME, 1, 60)),
  #       fill = .data$HERITAGE
  #     )
  #   ) +
  #   ggplot2::geom_boxplot(outlier.shape = NA) +  # Hide outliers
  #   ggplot2::scale_fill_manual(values = heritage_colors) +  # Apply custom colors
  #   ggplot2::facet_grid(.data$HERITAGE ~ ., scales = "free_y", space = "free") +  # Use facet_grid for better control
  #   ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis()  # Duplicate axis on the top
  #   ) +
  #   ggplot2::theme_bw()  +  # Apply minimal theme
  #   ggplot2::labs(x = "Time (days)") +
  #   ggplot2::labs(title = "Median first event",
  #                 x = "Time to Event",
  #                 y = "") +
  #   ggplot2::guides(
  #     fill = ggplot2::guide_legend(ncol = 1)  # Force the fill legend to be vertical (1 column)
  #   ) +
  #   ggplot2::theme(
  #     # strip.text.x = element_blank(),  # Remove facet labels
  #     strip.background = ggplot2::element_blank(),
  #     strip.text.y = ggplot2::element_blank(),
  #     # axis.title = ggplot2::element_blank(),
  #     axis.text.y = ggplot2::element_text(size = 15),
  #     panel.spacing = ggplot2::unit(0.5, "lines"),
  #     # Reduce spacing between panels
  #     legend.position = "top"
  #   )

  time_min <- min(plot_data$TIME_TO_EVENT, na.rm = TRUE)
  time_max <- max(plot_data$TIME_TO_EVENT, na.rm = TRUE)

  p2 <-
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        y = forcats::fct_inorder(stringr::str_sub(.data$CONCEPT_NAME, 1, 60)),
        x = .data$TIME_TO_EVENT,
        group = .data$CONCEPT_NAME
      )
    ) +
    # Add horizontal violin plot
    ggplot2::geom_violin(ggplot2::aes(fill = .data$HERITAGE),
                         alpha = 0.5,
                         trim = FALSE) +
    ggplot2::geom_boxplot(fill = NA, width = 0.3, outlier.shape = NA) +
    # Apply custom colors based on HERITAGE
    ggplot2::scale_fill_manual(values = heritage_colors) +
    # Add small lines for each occurrence, color based on KSTEST true/false
    ggplot2::geom_segment(
      ggplot2::aes(
        y = forcats::fct_inorder(stringr::str_sub(.data$CONCEPT_NAME, 1, 60)),
        yend = forcats::fct_inorder(stringr::str_sub(.data$CONCEPT_NAME, 1, 60)),
        x = .data$TIME_TO_EVENT,
        xend = .data$TIME_TO_EVENT - 5,
        color = as.factor(.data$KSTEST)
      ),
      size = 1
    ) +
    # Custom colors for KSTEST values with legend customization
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#8e5a3b", "FALSE" = "#a7c7e7"),
      name = "KS-test (vs uniform)",  # Custom legend title
      labels = c("FALSE" = "Insignificant", "TRUE" = "Significant")  # Custom labels
    ) +
    # Draw connecting lines through the points, color based on KSTEST
    ggplot2::geom_line(ggplot2::aes(
      color = as.factor(.data$KSTEST),
      group = .data$CONCEPT_NAME
    )) +
    # Faceting by HERITAGE
    ggplot2::facet_grid(.data$HERITAGE ~ ., scales = "free_y", space = "free") +
    ggplot2::scale_x_continuous(
      sec.axis = ggplot2::dup_axis(),  # Duplicate axis on the top
      limits = c(time_min, time_max)   # Set the x-axis limits here
    ) +
    # Enhance the plot appearance with a minimal theme
    ggplot2::theme_bw() +
    ggplot2::labs(title = "All event occurrences",
                  x = "Time to Event",
                  y = "") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(ncol = 1),  # Force the fill legend to be vertical (1 column)
      color = ggplot2::guide_legend(ncol = 1)  # Force the color legend to be vertical (1 column)
    ) +
    ggplot2::theme(
      # Adjust y-axis text size
      legend.position = "top",
      # Hide legend as needed
      strip.background = ggplot2::element_blank(),
      # Clean up strip background
      strip.text.y = ggplot2::element_blank(),
      #axis.text.y = ggplot2::element_blank(),
      # Hide strip text
      panel.spacing = unit(0.5, "lines")  # Reduce spacing between panels
    )
  # Combine plots
  # p <- p1 + p2 +
  #   patchwork::plot_layout(nrow = 1, widths = c(1, 2))
  return(p2)
}

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
