if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 is required for this app but is not installed. Please install it.")
}

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("patchwork is required for this app but is not installed. Please install it.")
}

#' @importFrom dplyr %>%

############################## Shiny functions

# Functions for data loading and formatting ------------------------------------

#' @title Function for formatting CohortContrast results for plotting
#'
#' @param object CohortContrast returned object
#' @param pathToResults Path to the results folder, can be project's working directory
#' @param autoScaleRate Boolean for scaling the prevalences
#' @param applyInverseTarget Boolean for applying inverse target
#' @param applyZTest Boolean for applying Z tests
#' @param applyLogitTest Boolean for applying Logit tests
#'
#' @keywords internal
format_results <- function(
    object, pathToResults, autoScaleRate, applyInverseTarget, applyZTest, applyLogitTest) {

  # Calculate the number of patients in target and control groups
  n_patients <- object$data_initial %>%
    dplyr::group_by(COHORT_DEFINITION_ID) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = COHORT_DEFINITION_ID, values_from = count, values_fill = 0)

  count_target <- n_patients$`2`
  count_control <- n_patients$`1`

  # Update data features with prevalence calculations
  data_features_temp = object$data_features %>% dplyr::select(CONCEPT_ID, ZTEST, LOGITTEST)
  object$data_features <- object$data_patients %>%
    dplyr::group_by(CONCEPT_ID, CONCEPT_NAME) %>%
    dplyr::summarise(
      TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == 2 & PREVALENCE > 0),
      CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == 1 & PREVALENCE > 0),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      TARGET_SUBJECT_PREVALENCE = TARGET_SUBJECT_COUNT / count_target,
      CONTROL_SUBJECT_PREVALENCE = CONTROL_SUBJECT_COUNT / count_control,
      PREVALENCE_DIFFERENCE_RATIO = dplyr::case_when(
        is.na(TARGET_SUBJECT_PREVALENCE) | TARGET_SUBJECT_PREVALENCE == 0 ~ 0,
        (is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0) & is.na(TARGET_SUBJECT_PREVALENCE) ~ -1,
        is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0 ~ 100,
        TRUE ~ TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
      )
    ) %>% dplyr::left_join(data_features_temp, by = "CONCEPT_ID", keep = FALSE)

  if (applyZTest) {
    object$data_features = object$data_features %>% dplyr::filter(ZTEST)
  }
  if (applyLogitTest) {
    object$data_features = object$data_features %>% dplyr::filter(LOGITTEST)
  }
  if (applyInverseTarget) {
    # Invert target and control groups
    object$data_patients <- object$data_patients %>% dplyr::mutate(COHORT_DEFINITION_ID = dplyr::if_else(COHORT_DEFINITION_ID == 1, 2, 1))
    object$data_initial <- object$data_initial %>% dplyr::mutate(COHORT_DEFINITION_ID = dplyr::if_else(COHORT_DEFINITION_ID == 1, 2, 1))
    object$data_features <- object$data_features %>%
      dplyr::mutate(
        TEMP = TARGET_SUBJECT_COUNT,
        TARGET_SUBJECT_COUNT = CONTROL_SUBJECT_COUNT,
        CONTROL_SUBJECT_COUNT = TEMP,
        TEMP = TARGET_SUBJECT_PREVALENCE,
        TARGET_SUBJECT_PREVALENCE = CONTROL_SUBJECT_PREVALENCE,
        CONTROL_SUBJECT_PREVALENCE = TEMP
      ) %>%
      dplyr::select(-TEMP) %>%
      dplyr::mutate(PREVALENCE_DIFFERENCE_RATIO = dplyr::if_else(
        is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0 & is.na(TARGET_SUBJECT_PREVALENCE),
        -1,
        dplyr::if_else(
          is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0,
          TARGET_SUBJECT_PREVALENCE / (1 / count_control),
          dplyr::if_else(
            is.na(TARGET_SUBJECT_PREVALENCE) | TARGET_SUBJECT_PREVALENCE == 0,
            (1 / count_target) / CONTROL_SUBJECT_PREVALENCE,
            TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
          )
        )
      ))
  }

  if (autoScaleRate) {
    # Calculate the duration in the cohort in days
    object$data_initial <- object$data_initial %>%
      dplyr::mutate(COHORT_DURATION = as.integer(difftime(
        as.Date(COHORT_END_DATE),
        as.Date(COHORT_START_DATE),
        units = "days"
      )))

    # Calculate the scaled prevalence for each PERSON_ID
    scaled_prevalence <- object$data_patients %>%
      dplyr::left_join(
        object$data_initial %>% dplyr::select(SUBJECT_ID, COHORT_DURATION),
        by = c("PERSON_ID" = "SUBJECT_ID")
      ) %>%
      dplyr::group_by(PERSON_ID, COHORT_DEFINITION_ID, CONCEPT_ID, CONCEPT_NAME, HERITAGE) %>%
      dplyr::mutate(
        SCALED_PREVALENCE = PREVALENCE / (COHORT_DURATION / 365)
      ) %>%
      dplyr::ungroup()

    # Update features with scaled prevalence
    object$data_features <- update_features(object$data_features, scaled_prevalence) %>%
      dplyr::select(
        CONCEPT_ID,
        CONCEPT_NAME,
        PREVALENCE_DIFFERENCE_RATIO,
        TARGET_SUBJECT_COUNT,
        CONTROL_SUBJECT_COUNT,
        TARGET_SUBJECT_PREVALENCE,
        CONTROL_SUBJECT_PREVALENCE
      )
  }

  concepts <- object$data_patients %>%
    dplyr::select(CONCEPT_ID, CONCEPT_NAME, HERITAGE) %>%
    dplyr::distinct() %>%
    dplyr::filter(CONCEPT_ID != 0) %>%
    dplyr::inner_join(object$data_features, by = c("CONCEPT_ID", "CONCEPT_NAME")) %>%
    dplyr::filter(!is.na(PREVALENCE_DIFFERENCE_RATIO))

  target <- object$data_patients %>%
    dplyr::filter(COHORT_DEFINITION_ID == 2) %>%
    dplyr::select(-COHORT_DEFINITION_ID) %>%
    dplyr::left_join(concepts, by = c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE")) %>%
    dplyr::filter(!is.na(PREVALENCE_DIFFERENCE_RATIO))

  target_df <- target %>%
    dplyr::mutate(NPATIENTS = length(unique(PERSON_ID))) %>%
    dplyr::mutate(PRESENT = 1) %>%
    dplyr::group_by(CONCEPT_ID) %>%
    dplyr::mutate(NCONCEPTS = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PREVALENCE = NCONCEPTS / NPATIENTS) %>%
    dplyr::bind_rows(
      target %>%
        dplyr::group_by(PERSON_ID) %>%
        dplyr::summarise(
          CONCEPT_ID = 999999999,
          CONCEPT_NAME = "None",
          PREVALENCE = 99999,
          HERITAGE = "none",
          PREVALENCE_DIFFERENCE_RATIO = -1,
          .groups = 'drop'
        )
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(
        CONCEPT_ID,
        CONCEPT_NAME,
        HERITAGE,
        PREVALENCE_DIFFERENCE_RATIO,
        PREVALENCE
      ),
      values_from = PRESENT,
      values_fill = 0,
      names_from = PERSON_ID,
      names_prefix = "PID_"
    ) %>%
    dplyr::filter(CONCEPT_ID != 999999999) %>%
    dplyr::mutate(CONCEPT_ID = as.character(CONCEPT_ID)) %>%
    dplyr::filter(CONCEPT_ID != "0") %>%
    dplyr::mutate(CONCEPT_NAME = gsub("(.{45})", "\\1\n", CONCEPT_NAME)) # Wrap names

  target_row_annotation <- target_df %>%
    as.data.frame() %>%
    tibble::column_to_rownames("CONCEPT_ID") %>%
    dplyr::select(-dplyr::starts_with("PID_"))

  target_matrix <- target_df %>%
    as.data.frame() %>%
    tibble::column_to_rownames("CONCEPT_ID") %>%
    dplyr::select(dplyr::starts_with("PID_")) %>%
    as.matrix()

  # Demographics
  # Process the data to ensure one row per person using the earliest cohort start date
  target_col_annotation <- object$data_person %>%
    dplyr::inner_join(
      object$data_initial %>%
        dplyr::filter(COHORT_DEFINITION_ID == 2) %>%
        dplyr::group_by(SUBJECT_ID) %>%
        dplyr::filter(COHORT_START_DATE == min(COHORT_START_DATE)) %>%
        dplyr::select(PERSON_ID = SUBJECT_ID, COHORT_START_DATE),
      by = "PERSON_ID"
    ) %>%
    dplyr::mutate(AGE = floor(as.numeric(
      COHORT_START_DATE - lubridate::as_date(stringr::str_glue("{YEAR_OF_BIRTH}-01-01"))
    ) / 365.25)) %>%
    dplyr::select(PERSON_ID, GENDER_CONCEPT_ID, AGE) %>%
    dplyr::mutate(GENDER = factor(
      GENDER_CONCEPT_ID,
      levels = c(8507, 8532),
      labels = c("Male", "Female")
    )) %>%
    dplyr::mutate(PERSON_ID = stringr::str_c("PID_", PERSON_ID)) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("PERSON_ID") %>%
    dplyr::select(-GENDER_CONCEPT_ID)

  res <- list(
    target_matrix = target_matrix,
    target_row_annotation = target_row_annotation,
    target_col_annotation = target_col_annotation
  )

  return(res)
}

#' @title Function for filtering CohortContrast results for plotting
#'
#' @param target Formatted CohortContrast object
#' @param prevalence_thereshold Prevalence threshold
#' @param prevalence_ratio_threshold Prevalence ratio threshold
#' @param removeUntreated Boolean for removing untreated patients
#'
#' @keywords internal
filter_target = function(target, prevalence_thereshold = 0.01, prevalence_ratio_threshold = 1, domain, removeUntreated){
  res = target
  res$target_row_annotation = res$target_row_annotation %>%
    dplyr::filter(PREVALENCE > prevalence_thereshold) %>%
    dplyr::filter(PREVALENCE_DIFFERENCE_RATIO > prevalence_ratio_threshold) %>%
    dplyr::filter(HERITAGE %in% domain)
  res$target_matrix =  res$target_matrix[rownames(res$target_row_annotation), ]
  if (removeUntreated) {
    column_sums <- colSums(res$target_matrix, na.rm = TRUE)
    non_zero_colnames <- colnames(res$target_matrix)[column_sums != 0]
    # Subset the matrix to keep only columns where the sum is not zero
    res$target_matrix = res$target_matrix[, non_zero_colnames]
    res$target_col_annotation <- res$target_col_annotation[rownames(res$target_col_annotation) %in% colnames(res$target_matrix),]
  }
  return(res)
}

#' @title Function for prevalence plot
#'
#' @param filtered_target filtered target object
#'
#' @keywords internal
plot_prevalence = function(filtered_target){
  # Check if the input is NULL or empty
  if (is.null(filtered_target) || nrow(filtered_target$target_row_annotation) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "After filtering there are no concepts left",
                      hjust = 0.5, vjust = 0.5, size = 20, fontface = "bold", color = "black") +
             ggplot2::theme_void())
  }
  # TODO: IF nrow(filtered_target$target_matrix) is NULL print some relevant wanring message, it means too harsh filters 0 people will remain
  plotdata = filtered_target$target_row_annotation %>%
    tibble::rownames_to_column("CONCEPT_ID") %>%
    tibble::as_tibble() %>%
    dplyr::left_join(
      tibble::tibble(
        CONCEPT_ID = rownames(filtered_target$target_matrix),
        PRESENCE = lapply(seq_len(nrow(filtered_target$target_matrix)), function(i) filtered_target$target_matrix[i, ])
      ),
      by = "CONCEPT_ID"
    ) %>%
    dplyr::mutate(PREVALENCE = purrr::map_dbl(PRESENCE, mean)) %>%
    dplyr::mutate(AVERAGE_AGE = purrr::map_dbl(PRESENCE, ~ mean(filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)], na.rm = TRUE))) %>%
    dplyr::mutate(AVERAGE_AGE_OVERALL = mean(filtered_target$target_col_annotation$AGE, na.rm = TRUE)) %>%
    dplyr::mutate(AGE_DIFF = purrr::map(PRESENCE, ~ {
      data = filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)]
      if(length(data[!is.na(data)]) >= 2 && length(unique(data[!is.na(data)])) > 1) {  # Ensure there are at least two non-NA observations
        t.test(data)
      } else {
        list(estimate = mean(data, na.rm = TRUE), conf.int = c(mean(data, na.rm = TRUE), mean(data, na.rm = TRUE)), p.value = NA)  # Returning a list similar to t.test output structure
      }
    })) %>%
    dplyr::mutate(AGE_DIFF_ESTIMATE = purrr::map_dbl(AGE_DIFF, ~ .x$estimate[1])) %>%
    dplyr::mutate(AGE_DIFF_LOW = purrr::map_dbl(AGE_DIFF, ~ .x$conf.int[1])) %>%
    dplyr::mutate(AGE_DIFF_HIGH = purrr::map_dbl(AGE_DIFF, ~ .x$conf.int[2])) %>%
    dplyr::mutate(AGE_DIFF_SIGNIFICANT = dplyr::if_else((AGE_DIFF_HIGH > AVERAGE_AGE_OVERALL) & (AGE_DIFF_LOW < AVERAGE_AGE_OVERALL), FALSE, TRUE)) %>%
    dplyr::mutate(MALE_PROP = purrr::map_dbl(PRESENCE, ~ mean(filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male", na.rm = TRUE))) %>%
    dplyr::mutate(MALE_PROP_OVERALL = mean(filtered_target$target_col_annotation$GENDER == "Male", na.rm = TRUE)) %>%
    dplyr::mutate(MALE_PROP_DIFF = purrr::map(PRESENCE, ~ {
      data = filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male"
      if(sum(data, na.rm = TRUE) >= 2 && length(unique(data[!is.na(data)])) > 1) {  # Ensure there are at least two non-NA observations
        t.test(data)
      } else {
        list(estimate = mean(data, na.rm = TRUE), conf.int = c(mean(data, na.rm = TRUE), mean(data, na.rm = TRUE)), p.value = NA)
      }
    })) %>%
    dplyr::mutate(MALE_PROP_DIFF_ESTIMATE = purrr::map_dbl(MALE_PROP_DIFF, ~ .x$estimate)) %>%
    dplyr::mutate(MALE_PROP_DIFF_LOW = purrr::map_dbl(MALE_PROP_DIFF, ~ .x$conf.int[1])) %>%
    dplyr::mutate(MALE_PROP_DIFF_HIGH = purrr::map_dbl(MALE_PROP_DIFF, ~ .x$conf.int[2])) %>%
    dplyr::mutate(MALE_PROP_DIFF_SIGNIFICANT = dplyr::if_else((MALE_PROP_DIFF_HIGH > MALE_PROP_OVERALL) & (MALE_PROP_DIFF_LOW < MALE_PROP_OVERALL), FALSE, TRUE)) %>%
    dplyr::mutate(PREVALENCE_LOG = dplyr::if_else(PREVALENCE_DIFFERENCE_RATIO < 1, 0, dplyr::if_else(PREVALENCE_DIFFERENCE_RATIO > 100, 2, log10(PREVALENCE_DIFFERENCE_RATIO))))
  # Plot
  p1 <- ggplot2::ggplot(plotdata, ggplot2::aes(x = PREVALENCE, y = CONCEPT_NAME, fill = PREVALENCE_LOG)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::facet_grid(HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggplot2::scale_fill_viridis_c(
      "Risk ratio (log10-scaled)\ncompared to background",
      limits = c(0, 2),
      oob = scales::squish
    ) + # Ensure fill values are between 0 and 3
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::ggtitle("Prevalence") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 15) # Adjust the size as needed
    )

  p2 <- ggplot2::ggplot(plotdata, ggplot2::aes(y = CONCEPT_NAME, color = AGE_DIFF_SIGNIFICANT)) +
    ggplot2::geom_point(ggplot2::aes(x = AGE_DIFF_ESTIMATE)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = AGE_DIFF_LOW, xmax = AGE_DIFF_HIGH), linewidth = 5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = AVERAGE_AGE_OVERALL), color = "darkgreen") +
    ggplot2::scale_color_manual(values = c("grey60", "blue"), breaks = c(FALSE, TRUE)) +
    ggplot2::facet_grid(HERITAGE ~ ., space = "free_y", scales = "free_y") +
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

  p3 <- ggplot2::ggplot(plotdata, ggplot2::aes(y = CONCEPT_NAME, color = MALE_PROP_DIFF_SIGNIFICANT)) +
    ggplot2::geom_point(ggplot2::aes(x = MALE_PROP_DIFF_ESTIMATE)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = MALE_PROP_DIFF_LOW, xmax = MALE_PROP_DIFF_HIGH), linewidth = 5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = MALE_PROP_OVERALL), color = "darkgreen") +
    ggplot2::scale_color_manual(values = c("grey60", "blue"), breaks = c(FALSE, TRUE)) +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::facet_grid(HERITAGE ~ ., space = "free_y", scales = "free_y") +
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

#' @title Function for prevalence heatmap plot
#'
#' @param filtered_target filtered target object
#'
#' @keywords internal
plot_heatmap = function(filtered_target){
  # Check if the input is NULL or empty
  if (is.null(filtered_target) || nrow(filtered_target$target_row_annotation) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "After filtering there are no concepts left",
                      hjust = 0.5, vjust = 0.5, size = 12, fontface = "bold", color = "black") +
             ggplot2::theme_void())
  }
  # Patient clustering
  col_clustering = stats::hclust(dist(t(filtered_target$target_matrix)))

  # Row reordering
  reordering = filtered_target$target_row_annotation %>%
    tibble::rownames_to_column("CONCEPT_ID") %>%
    dplyr::group_by(HERITAGE) %>% dplyr::arrange(dplyr::desc(CONCEPT_NAME)) %>%
    dplyr::summarise(CONCEPT_ID = list(CONCEPT_ID)) %>%
    dplyr::mutate(MATRIX = purrr::map(CONCEPT_ID, ~ filtered_target$target_matrix[.x, , drop = F])) %>%
    dplyr::mutate(MATRIX = purrr::map(
      MATRIX,
      function(x){
        if(nrow(x) > 1){
          x = x[stats::hclust(dist(x))$order, ]
        }

        return(x)
      }
    ))

  tm = do.call(rbind, reordering$MATRIX)
  tm_gaps = purrr::map_int(reordering$MATRIX, nrow) %>% cumsum()

  tm2 = tm
  rownames(tm2) = filtered_target$target_row_annotation[rownames(tm), ]$CONCEPT_NAME

  annotation_row = filtered_target$target_row_annotation %>%
    dplyr::arrange(CONCEPT_NAME) %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames("CONCEPT_NAME") %>%
    dplyr::select(-PREVALENCE_DIFFERENCE_RATIO)
  annotation_col = filtered_target$target_col_annotation

  # Heritage colors pre-defined
  heritage_colors <- c(
    procedure_occurrence = "darkblue",
    condition_occurrence = "orange",
    drug_exposure = "lightblue",
    measurement = "pink",
    observation = "brown",
    visit_occurrence ="darkgreen",
    visit_detail = "lightgreen"
  )

  active_gender_colors <-  c(Male = "purple", Female = "pink", Other = "lightblue")

  # Filter heritage colors based on what's present in annotation_row
  active_heritage_colors <- heritage_colors[names(heritage_colors) %in% unique(annotation_row$HERITAGE)]
  active_gender_colors <- active_gender_colors[names(active_gender_colors) %in% unique(annotation_col$GENDER)]


  # Specify colors including a dynamic gradient for AGE
  annotation_colors = list(
    AGE = grDevices::colorRampPalette(c("lightblue", "firebrick"))(length(unique(annotation_col$AGE))),
    GENDER = active_gender_colors,
    HERITAGE = active_heritage_colors,
    PREVALENCE = grDevices::colorRampPalette(c("white", "purple"))(length(unique(annotation_row$PREVALENCE)))
  )


  annotation_row <- annotation_row %>%
    tibble::rownames_to_column("CONCEPT_NAME") %>%
    dplyr::arrange(HERITAGE, dplyr::desc(toupper(CONCEPT_NAME))) %>%
    tibble::column_to_rownames("CONCEPT_NAME")

  # Apply the same order to the matrix
  tm2 <- tm2[rownames(annotation_row), ]


  pheatmap::pheatmap(
    tm2,
    show_colnames = F,
    annotation_row = annotation_row,
    annotation_col = annotation_col,
    annotation_colors = annotation_colors,
    cluster_cols = col_clustering,
    cluster_rows = FALSE,
    gaps_row = tm_gaps,
    color = c("#e5f5f9", "#2ca25f"),
    legend_breaks = c(0.25, 0.75),
    legend_labels = c("Absent", "Present")
  )
}

#' @title Function for interactively updating plots
#'
#' @param features features
#' @param scaled_prev scaled prev
#'
#' @keywords internal
update_features <- function(features, scaled_prev) {
  scaled_prev <- scaled_prev %>% dplyr::group_by(CONCEPT_ID, COHORT_DEFINITION_ID) %>%
    dplyr::summarise(AVG_SCALED_PREVALENCE = stats::median(SCALED_PREVALENCE, na.rm = TRUE)) %>%
    # Pivot to wide format to separate the prevalences for different cohort IDs
    tidyr::pivot_wider(
      names_from = COHORT_DEFINITION_ID,
      values_from = AVG_SCALED_PREVALENCE,
      names_prefix = "COHORT_"
    )

  # Replace NA with specific values to handle cases where a cohort might be missing
  scaled_prev = scaled_prev %>% tidyr::replace_na(list(COHORT_1 = -1, COHORT_2 = -1)) %>%
    # Calculate the ratio or set to -1 if any cohort data is missing
    dplyr::mutate(
      PREVALENCE_DIFFERENCE_RATIO = dplyr::if_else(COHORT_1 == -1, COHORT_2 / 1, dplyr::if_else(COHORT_2 == -1, 1/COHORT_1, COHORT_2 / COHORT_1)
      )) %>%
    # Optionally, select relevant columns
    dplyr::select(CONCEPT_ID, PREVALENCE_DIFFERENCE_RATIO)
  features = features %>% dplyr::select(-PREVALENCE_DIFFERENCE_RATIO) %>%
    dplyr::left_join(scaled_prev, by = "CONCEPT_ID")
}
