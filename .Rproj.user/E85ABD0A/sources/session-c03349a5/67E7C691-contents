library(ggplot2)
library(dplyr)

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
        ABSTRACTION_LEVEL,
        HERITAGE
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
          ABSTRACTION_LEVEL,
          HERITAGE
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


plot_prevalence <- function(filtered_target, isCorrelationView = FALSE) {
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
  }
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
        "#4B99C9",
        "#FFB347",
        "#B39EB5",
        "#FFB3AB",
        "#77DD77",
        "#FDFD96",
        "#AEC6CF"
      )
    )
  # Merge the colors into the main dataset
  plotdata <-
    merge(plotdata, heritage_colors, by = "HERITAGE", all.x = TRUE)

  # Initialize plots
  p1 <- NULL
  p2 <- NULL
  p3 <- NULL

  if(!isCorrelationView){
  # Plot
  p1 <-
    ggplot2::ggplot(
      plotdata,
      ggplot2::aes(
        x = .data$PREVALENCE,
        y = stringr::str_sub(.data$CONCEPT_NAME, 1, 60),
        fill = 10 ** .data$PREVALENCE_LOG
      )
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::label_comma(accuracy = 0.01)(10 ** .data$PREVALENCE_LOG),
        x = .data$PREVALENCE + 0.01  # Adjust position as needed
      ),
      hjust = 0, # Position text slightly to the right of the bar
      size = 3   # Adjust text size as desired
    ) +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::scale_fill_viridis_c(
      "\nRisk ratio (log10 scaled)\ncompared to background",
      limits = c(1, 100),
      trans = "log10",
      oob = scales::squish
    ) + # Ensure fill values are between 0 and 3
    ggplot2::scale_x_continuous(labels = scales::label_percent(),
                                sec.axis = ggplot2::dup_axis()) +
    ggplot2::ggtitle("Prevalence") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1),
      axis.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 10),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.5, "lines"),
      axis.text.y = ggplot2::element_text(size = 10) # Adjust the size as needed
    )

  p2 <-
    ggplot2::ggplot(plotdata,
                    ggplot2::aes(
                      y = stringr::str_sub(.data$CONCEPT_NAME, 1, 60),
                      color = .data$AGE_DIFF_SIGNIFICANT
                    )) +
    ggplot2::geom_point(ggplot2::aes(x = .data$AGE_DIFF_ESTIMATE)) +
    ggplot2::geom_errorbar(ggplot2::aes(
      xmin = .data$AGE_DIFF_LOW,
      xmax = .data$AGE_DIFF_HIGH
    ),
    linewidth = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$AVERAGE_AGE_OVERALL),
                        color = "darkgreen") +
    ggplot2::scale_color_manual(values = c("grey60", "blue"),
                                breaks = c(FALSE, TRUE)) +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis()) +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::ggtitle("AGE in group") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = element_text(hjust = 1),
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      legend.position = "none",
      panel.spacing = ggplot2::unit(0.5, "lines")
    )

  # Heritage text dataset
  heritage_annot = plotdata %>%
    dplyr::mutate(CONCEPT_NAME = stringr::str_sub(.data$CONCEPT_NAME, 1, 60)) %>%
    dplyr::group_by(.data$HERITAGE) %>%
    dplyr::summarise(CONCEPT_NAME = max(.data$CONCEPT_NAME)) %>%
    dplyr::ungroup()

  # Create the plot with backgrounds
  p3 <-
    ggplot2::ggplot(
      plotdata,
      ggplot2::aes(
        y = stringr::str_sub(.data$CONCEPT_NAME, 1, 60),
        color = .data$MALE_PROP_DIFF_SIGNIFICANT
      )
    ) +
    ggplot2::geom_text(aes(label = .data$HERITAGE), x = 1, hjust = 1, size = 6, fontface = "bold", color = "navy", alpha = 0.5, data = heritage_annot) +
    ggplot2::geom_point(ggplot2::aes(x = .data$MALE_PROP_DIFF_ESTIMATE)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data$MALE_PROP_DIFF_LOW,
        xmax = .data$MALE_PROP_DIFF_HIGH
      ),
      linewidth = 1
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = .data$MALE_PROP_OVERALL),
                        color = "darkgreen") +
    ggplot2::scale_color_manual(values = c("grey60", "blue"),
                                breaks = c(FALSE, TRUE)) +  # Colors for significant differences
    ggplot2::scale_x_continuous(labels = scales::label_percent(),
                                sec.axis = ggplot2::dup_axis(),
                                limits = c(0,1)) +
    ggplot2::facet_grid(.data$HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::ggtitle("Male percentage in group") +
    ggplot2::theme_bw() +
    ggplot2::guides(color = "none") +  # Hide the color guide
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1),
      panel.spacing = ggplot2::unit(0.5, "lines"),
      strip.background = ggplot2::element_blank(),
      # Hide the strip background
      # legend.position = "top",
      legend.position = "none",
      strip.text.x = ggplot2::element_blank(),
      # Remove facet labels
      # Place the fill guide (HERITAGE) at the bottom
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  } else {
    # Extract ordered data
    ordered_matrix <- filtered_target$correlation_analysis$ordered_matrix
    gaps_row <- filtered_target$correlation_analysis$gaps_row

    # Reorder plotdata based on ordered_matrix row order
    ordered_indices <- match(rownames(ordered_matrix), plotdata$CONCEPT_NAME)
    plotdata <- plotdata[ordered_indices, ]

    # Add gaps as a grouping variable
    plotdata$Group <- cut(seq_len(nrow(plotdata)), breaks = c(0, gaps_row, nrow(plotdata)), labels = FALSE)

    # Plot grouped heatmap with gaps_row handling
    p1 <- ggplot2::ggplot(
      plotdata,
      ggplot2::aes(
        x = PREVALENCE,
        y = stringr::str_sub(CONCEPT_NAME, 1, 60),
        fill = 10 ** PREVALENCE_LOG
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(
        ggplot2::aes(
          label = scales::label_comma(accuracy = 0.01)(10 ** PREVALENCE_LOG),
          x = PREVALENCE + 0.01
        ),
        hjust = 0,
        size = 3
      ) +
      ggplot2::facet_grid(Group ~ ., space = "free_y", scales = "free_y") +
      ggplot2::scale_fill_viridis_c(
        "\nRisk ratio (log10 scaled)\ncompared to background",
        limits = c(1, 100),
        trans = "log10",
        oob = scales::squish
      ) +
      ggplot2::scale_x_continuous(labels = scales::label_percent(),
                                  sec.axis = ggplot2::dup_axis()) +
      ggplot2::ggtitle("Prevalence") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 1),
        axis.title = ggplot2::element_blank(),
        legend.position = "top",
        legend.title = ggplot2::element_text(size = 10),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(0.5, "lines"),
        axis.text.y = ggplot2::element_text(size = 10)
      )

    p2 <- ggplot2::ggplot(
      plotdata,
      ggplot2::aes(
        y = CONCEPT_NAME,
        x = AGE_DIFF_ESTIMATE,
        color = AGE_DIFF_SIGNIFICANT
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          xmin = AGE_DIFF_LOW,
          xmax = AGE_DIFF_HIGH
        ),
        linewidth = 1
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = AVERAGE_AGE_OVERALL),
        color = "darkgreen"
      ) +
      ggplot2::facet_grid(Group ~ ., space = "free_y", scales = "free_y") +
      ggplot2::scale_color_manual(values = c("grey60", "blue")) +
      ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis()) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = element_text(hjust = 1),
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_blank(),
        legend.position = "none",
        panel.spacing = ggplot2::unit(0.5, "lines")
      ) +
      ggplot2::ggtitle("Age in group")


    # Plot male proportion differences with gaps_row handling
    p3 <- ggplot2::ggplot(
      plotdata,
      ggplot2::aes(
        y = CONCEPT_NAME,
        x = MALE_PROP_DIFF_ESTIMATE,
        color = MALE_PROP_DIFF_SIGNIFICANT
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          xmin = MALE_PROP_DIFF_LOW,
          xmax = MALE_PROP_DIFF_HIGH
        ),
        linewidth = 1
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = MALE_PROP_OVERALL),
        color = "darkgreen"
      ) +
      ggplot2::facet_grid(Group ~ ., space = "free_y", scales = "free_y") +
      ggplot2::scale_color_manual(values = c("grey60", "blue")) +
      ggplot2::scale_x_continuous(labels = scales::label_percent(),
                                  sec.axis = ggplot2::dup_axis(),
                                  limits = c(0,1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = element_text(hjust = 1),
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_blank(),
        legend.position = "none",
        panel.spacing = ggplot2::unit(0.5, "lines")
      ) +
      ggplot2::ggtitle("Male percentage in group")

    }
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
    dplyr::arrange(dplyr::desc(.data$CONCEPT_NAME)) %>%
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
    dplyr::arrange(.data$CONCEPT_NAME) %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames("CONCEPT_NAME") %>%
    dplyr::select(-.data$PREVALENCE_DIFFERENCE_RATIO)
  annotation_col <- filtered_target$target_col_annotation
  # Heritage colors pre-defined
  heritage_colors <- c(
    procedure_occurrence = "#4B99C9",
    condition_occurrence = "#FFB347",
    drug_exposure = "#B39EB5",
    measurement = "#FFB3AB",
    observation = "#77DD77",
    visit_occurrence = "#FDFD96",
    visit_detail = "#AEC6CF"
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

  # tm2 <- tm2[order(rownames(tm2), decreasing = TRUE), ]
  # annotation_row
  annotation_row <- annotation_row %>%
    tibble::rownames_to_column("CONCEPT_NAME") %>%
    dplyr::arrange(.data$HERITAGE, dplyr::desc(toupper(.data$CONCEPT_NAME))) %>%
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

  if(is.null(rownames(tm2))){
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Can not cluster less than 2 concepts",
          hjust = 0.5,
          vjust = 0.5,
          size = 20,
          fontface = "bold",
          color = "black"
        ) +
        ggplot2::theme_void()
    )
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
    legend_labels = c("Absent", "Present"),
    cellheight = 18,

  )
}

plot_time <- function(filtered_target, isCorrelationView = FALSE) {
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
    procedure_occurrence = "#4B99C9",
    condition_occurrence = "#FFB347",
    drug_exposure = "#B39EB5",
    measurement = "#FFB3AB",
    observation = "#77DD77",
    visit_occurrence = "#FDFD96",
    visit_detail = "#AEC6CF"
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
      #KSTEST = dplyr::first(.data$KSTEST),
      # Similarly for KSTEST
      .groups = "drop"                     # Drop grouping information after summarising
    )
  # initialize p2
  p2 <- NULL
  if(!isCorrelationView) {
  plot_data3 = plot_data %>% dplyr::select(.data$CONCEPT_NAME,
                                           .data$TIME_TO_EVENT,
                                           .data$HERITAGE,
                                           #.data$KSTEST,
                                           ) %>% tidyr::unnest(.data$TIME_TO_EVENT)  # Make sure TIME_MEDIAN is no longer a list

  time_min <- min(plot_data3$TIME_TO_EVENT, na.rm = TRUE)
  time_max <- max(plot_data3$TIME_TO_EVENT, na.rm = TRUE)
  # Heritage text dataset
  heritage_annot = plot_data3 %>%
    dplyr::mutate(CONCEPT_NAME = stringr::str_sub(.data$CONCEPT_NAME, 1, 60)) %>%
    dplyr::group_by(.data$HERITAGE) %>%
    dplyr::summarise(CONCEPT_NAME = max(.data$CONCEPT_NAME)) %>%
    dplyr::ungroup()

  p2 <-
    ggplot2::ggplot(
      plot_data3,
      ggplot2::aes(
        y = .data$CONCEPT_NAME,
        x = .data$TIME_TO_EVENT,
        group = .data$CONCEPT_NAME
      )
    ) +
    # Add horizontal violin plot
    ggplot2::geom_violin(ggplot2::aes(fill = .data$HERITAGE), width = 1.2, color = NA, alpha = 0.5, trim = FALSE) +
    ggplot2::geom_text(aes(label = .data$HERITAGE), x = time_max, hjust = 1, size = 6, fontface = "bold", color = "navy", alpha = 0.5, data = heritage_annot) +
  ggplot2::geom_boxplot(fill = NA, outliers = FALSE, width = 0.3) +
    # Faceting by HERITAGE
    ggplot2::facet_grid(.data$HERITAGE ~ ., scales = "free_y", space = "free") +
    ggplot2::scale_x_continuous(limit = c(time_min, time_max),
                                sec.axis = ggplot2::dup_axis(),
                                expand = ggplot2::expansion(mult = 0.01)) +
    # Enhance the plot appearance with a minimal theme
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Event occurrences timing (days since index)",
                  x = "",
                  y = "") +
    ggplot2::theme(
      # Adjust y-axis text size
      legend.position = "none",
      # Hide legend as needed
      strip.background = ggplot2::element_blank(),
      # Clean up strip background
      strip.text.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_blank(),
      # Hide strip text
      panel.spacing = ggplot2::unit(0.5, "lines")  # Reduce spacing between panels
    )
  } else {
    # Extract ordered data
    ordered_matrix <- filtered_target$correlation_analysis$ordered_matrix
    gaps_row <- filtered_target$correlation_analysis$gaps_row

    # Reorder plot_data based on ordered_matrix row order
    ordered_indices <- match(rownames(ordered_matrix), plot_data$CONCEPT_NAME)
    plot_data <- plot_data[ordered_indices, ]
    plot_data$Group <- cut(seq_len(nrow(plot_data)), breaks = c(0, gaps_row, nrow(plot_data)), labels = FALSE)

    # Unnest TIME_TO_EVENT
    plot_data3 <- plot_data %>%
      dplyr::select(.data$CONCEPT_NAME, .data$TIME_TO_EVENT, .data$Group) %>%
      tidyr::unnest(.data$TIME_TO_EVENT)  # Flatten TIME_TO_EVENT list

    # Get time range
    time_min <- min(plot_data3$TIME_TO_EVENT, na.rm = TRUE)
    time_max <- max(plot_data3$TIME_TO_EVENT, na.rm = TRUE)

    # Create a palette that repeats as needed for the number of groups
    num_groups <- length(unique(plot_data3$Group))
    palette_colors <- RColorBrewer::brewer.pal(min(num_groups, 12), "Set3")
    palette_colors <- rep(palette_colors, length.out = num_groups)  # Repeat colors to match groups

    # Create the plot with repeated fill colors for groups
    p2 <- ggplot2::ggplot(
      plot_data3,
      ggplot2::aes(
        y = .data$CONCEPT_NAME,
        x = .data$TIME_TO_EVENT,
        group = .data$CONCEPT_NAME,
        fill = factor(.data$Group)  # Map Group to fill
      )
    ) +
      # Add horizontal violin plot
      ggplot2::geom_violin(
        width = 1.2,
        color = NA,
        alpha = 0.5,
        trim = FALSE
      ) +
      ggplot2::geom_boxplot(
        fill = NA,  # Keep boxplot borders
        outliers = FALSE,
        width = 0.3
      ) +
      # Faceting by Group
      ggplot2::facet_grid(
        .data$Group ~ .,
        scales = "free_y",
        space = "free"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(time_min, time_max),
        sec.axis = ggplot2::dup_axis(),
        expand = ggplot2::expansion(mult = 0.01)
      ) +
      # Manually define fill colors for groups
      ggplot2::scale_fill_manual(values = palette_colors) +
      # Enhance the plot appearance with a minimal theme
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = "Event occurrences timing (days since index)",
        x = "",
        y = ""
      ) +
      ggplot2::theme(
        legend.position = "none",
        strip.background = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(0.5, "lines")
      )
  }
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



# Prepare filtered_target for correlation analysis and trajectories

# Prepare filtered_target for correlation analysis and trajectories
prepare_filtered_target <- function(filtered_target, correlation_threshold = 0.95) {
  if (is.null(filtered_target) || nrow(filtered_target$target_row_annotation) == 0) {
    stop("After filtering there are no concepts left.")
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

  # Perform intra-group clustering
  ordered_groups <- lapply(groups, function(group) {
    group_ids <- names(group)
    group_matrix <- target_matrix[, group_ids, drop = FALSE]

    # Skip clustering if group has only one concept or insufficient variability
    if (all(apply(group_matrix, 1, var) == 0, na.rm = TRUE) || ncol(group_matrix) <= 1) {
      return(group)
    }

    # Compute a distance matrix within the group
    group_distance <- as.dist(1 - stats::cor(group_matrix, method = "pearson", use = "pairwise.complete.obs"))

    # Check for NA/NaN in distance matrix
    if (any(is.na(group_distance))) {
      return(group) # Return as-is if clustering is not possible
    }
    # Perform hierarchical clustering
    hc <- hclust(group_distance)

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

  # Perform clustering on the group correlation matrix
  group_distance <- as.dist(1 - group_correlations)
  hc_groups <- hclust(group_distance)
  ordered_group_indices <- hc_groups$order
  ordered_groups <- ordered_groups[ordered_group_indices]

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

  return(filtered_target)
}

plot_correlation_heatmap <- function(filtered_target) {
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
  }

  ordered_matrix <- filtered_target$correlation_analysis$ordered_matrix
  gaps_row <- filtered_target$correlation_analysis$gaps_row

  # Compute row-wise clustering
  dist_matrix <- stats::as.dist(1 - stats::cor(t(ordered_matrix), method = "pearson", use = "pairwise.complete.obs"))
  hc_rows <- stats::hclust(dist_matrix)

  # Cut the dendrogram into clusters
  num_clusters <- length(gaps_row) + 1 # Define number of clusters based on gaps
  cluster_labels <- stats::cutree(hc_rows, k = num_clusters)

  # Plot heatmap with cutree_rows
  heatmap <- pheatmap::pheatmap(
    ordered_matrix,
    cluster_rows = hc_rows,            # Pass the row dendrogram
    cluster_cols = TRUE,               # Cluster columns
    cutree_rows = num_clusters,        # Number of row clusters (defines gaps)
    show_colnames = FALSE,             # Hide column names
    color = c("#e5f5f9", "#2ca25f"),   # Custom colors
    treeheight_row = 50                # Height of the row dendrogram
  )

  return(heatmap)
}

plot_correlation_trajectory_graph <- function(filtered_target, selectedIds = NULL, edgePrevalence = 0.5, selectionList, corrInitiated = FALSE) {
  if (isFALSE(corrInitiated)){
    plot <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = "Correlation view must be enabled",
        hjust = 0.5,
        vjust = 0.5,
        size = 12,
        fontface = "bold",
        color = "black"
      ) +
      ggplot2::theme_void()
    return(plot)
  }
  else if (is.null(selectedIds) | length(selectedIds) == 0) {
    plot <- ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = "A correlation group must be selected",
        hjust = 0.5,
        vjust = 0.5,
        size = 12,
        fontface = "bold",
        color = "black"
      ) +
      ggplot2::theme_void()
    return(plot)
  }

  patients_data <- filtered_target$target_time_annotation %>%
    dplyr::filter(CONCEPT_ID %in% selectedIds)

  result <- patients_data %>%
    tidyr::unnest(TIME_TO_EVENT) %>%
    dplyr::select(PERSON_ID, CONCEPT_NAME, TIME_TO_EVENT) %>%
    dplyr::mutate(TIME_TO_EVENT = TIME_TO_EVENT + 1) %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::mutate(
      MAX_TIME_TO_EVENT = max(TIME_TO_EVENT) # Find the maximum time for each person
    ) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(
      # Add a START event for each patient
      patients_data %>%
        tidyr::unnest(TIME_TO_EVENT) %>%
        dplyr::distinct(PERSON_ID) %>%
        dplyr::mutate(CONCEPT_NAME = "START", TIME_TO_EVENT = 0),

      # Add an EXIT event for each patient
      patients_data %>%
        tidyr::unnest(TIME_TO_EVENT) %>%
        dplyr::group_by(PERSON_ID) %>%
        dplyr::summarise(TIME_TO_EVENT = max(TIME_TO_EVENT) + 1, .groups = "drop") %>%
        dplyr::mutate(CONCEPT_NAME = "EXIT")
    ) %>%
    dplyr::arrange(PERSON_ID, TIME_TO_EVENT)

  # use selectionList if initialized
  if(length(selectionList) > 1){
  `%||%` <- function(x, y) if (!is.null(x)) x else y
  selectionList[["START"]] = "All events"
  selectionList[["EXIT"]] = "All events"

  result <- result %>%
    dplyr::mutate(selection = sapply(CONCEPT_NAME, function(x) selectionList[[x]] %||% "All events"))

  # Filter based on the selection column
 result = result %>%
   dplyr::group_by(PERSON_ID, CONCEPT_NAME) %>%
   dplyr::filter(case_when(
      selection == "All events" ~ TRUE,
      selection == "First occurrence" ~ TIME_TO_EVENT == min(TIME_TO_EVENT),
      selection == "Last occurrence" ~ TIME_TO_EVENT == max(TIME_TO_EVENT),
      TRUE ~ FALSE
    )) %>%
   dplyr::distinct(PERSON_ID, CONCEPT_NAME, TIME_TO_EVENT, .keep_all = TRUE) %>% # Ensure no duplicates
   dplyr::ungroup() %>%
   dplyr::select(-selection) # Drop the helper column
}

  total_patients <- result %>%
    dplyr::distinct(PERSON_ID) %>%
    nrow()
  edges <- result %>%
    dplyr::arrange(PERSON_ID, TIME_TO_EVENT) %>%
    dplyr::group_by(PERSON_ID) %>%
    dplyr::mutate(
      NEXT_CONCEPT_NAME = dplyr::lead(CONCEPT_NAME),
      TRANSITION_TIME = dplyr::lead(TIME_TO_EVENT) - TIME_TO_EVENT
    ) %>%
    dplyr::filter(!is.na(NEXT_CONCEPT_NAME)) %>%
    dplyr::ungroup() %>%
    dplyr::select(PERSON_ID, CONCEPT_NAME, NEXT_CONCEPT_NAME, TRANSITION_TIME)

  edges_summary <- edges %>%
    dplyr::group_by(CONCEPT_NAME, NEXT_CONCEPT_NAME) %>%
    dplyr::summarise(
      weight = dplyr::n(),
      prevalence_perc = (dplyr::n_distinct(PERSON_ID) / total_patients),
      median_transition_time = mean(TRANSITION_TIME, na.rm = TRUE),
      .groups = "drop"
    )

  filtered_edges <- edges_summary %>%
    dplyr::filter(prevalence_perc >= edgePrevalence)

  nodes <- unique(c(filtered_edges$CONCEPT_NAME, filtered_edges$NEXT_CONCEPT_NAME)) %>%
    tibble::tibble(id = ., label = as.character(.)) %>%
    dplyr::mutate(size = 15)

  edges_vis <- filtered_edges %>%
    dplyr::rename(from = CONCEPT_NAME, to = NEXT_CONCEPT_NAME) %>%
    dplyr::mutate(
      label = paste("Time:", round(median_transition_time, 1), " day(s)", "\nPrev.:", round(prevalence_perc*100, 1), "%"),
      length = 100
    )

  graph <- visNetwork::visNetwork(nodes, edges_vis, height = "1000px", width = "100%") %>%
    visNetwork::visEdges(
      arrows = "to",
      smooth = list(
        type = "curvedCW", # Use clockwise curvature for smoother edges
        roundness = 0.2    # Adjust the curvature amount (higher values = more curve)
      )
    ) %>%
    visNetwork::visOptions(
      highlightNearest = TRUE,
      nodesIdSelection = FALSE
    ) %>%
    visNetwork::visPhysics(enabled = FALSE) %>% # Disable physics completely
    visNetwork::visInteraction(
      dragNodes = TRUE,  # Allow manual dragging of nodes
      dragView = TRUE,   # Enable panning of the graph
      zoomView = TRUE    # Enable zooming
    )

  return(graph)
}


