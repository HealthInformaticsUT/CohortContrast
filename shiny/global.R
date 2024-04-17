############################### Global variables
library(dplyr)
library(tibble)
library(tidyr)
############################## Shiny functions

# Functions for data loading and formatting ------------------------------------
format_results = function(pathToResults, studyName){
  load(stringr::str_c(pathToResults, "/tmp/datasets/", studyName, "_CC_medData.rdata"))

  concepts = object$data_patients %>%
    select(CONCEPT_ID, CONCEPT_NAME, HERITAGE) %>%
    distinct() %>%
    filter(CONCEPT_ID != 0) %>%
    inner_join(object$data_features, by = c("CONCEPT_ID", "CONCEPT_NAME")) %>%
    filter(!is.na(PREVALENCE_DIFFERENCE_RATIO))

  target = object$data_patients %>%
    filter(COHORT_DEFINITION_ID == 2) %>%
    select(-COHORT_DEFINITION_ID) %>%
    left_join(concepts, by = c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE")) %>%
    filter(!is.na(PREVALENCE_DIFFERENCE_RATIO))

  target_df = target %>%
    mutate(NPATIENTS = length(unique(PERSON_ID))) %>%
    mutate(PRESENT = 1) %>%
    group_by(CONCEPT_ID) %>%
    mutate(NCONCEPTS = n()) %>%
    ungroup() %>%
    mutate(PREVALENCE = NCONCEPTS / NPATIENTS) %>%
    bind_rows(
      target %>%
        group_by(PERSON_ID) %>%
        summarize(CONCEPT_ID = 999999999, CONCEPT_NAME = "None", PREVALENCE = 99999, HERITAGE = "none", PREVALENCE_DIFFERENCE_RATIO = -1)
    ) %>%
    pivot_wider(id_cols = c(CONCEPT_ID, CONCEPT_NAME, HERITAGE, PREVALENCE_DIFFERENCE_RATIO, PREVALENCE), values_from = PRESENT, values_fill = 0, names_from = PERSON_ID, names_prefix = "PID_") %>%
    filter(CONCEPT_ID != 999999999) %>%
    mutate(CONCEPT_ID = as.character(CONCEPT_ID)) %>%
    filter(CONCEPT_ID != "0")

  target_row_annotation = target_df %>%
    as.data.frame() %>%
    column_to_rownames("CONCEPT_ID") %>%
    select(-starts_with("PID_"))

  target_matrix = target_df %>%
    as.data.frame() %>%
    column_to_rownames("CONCEPT_ID") %>%
    select(starts_with("PID_")) %>%
    as.matrix()

  # Demographics
  target_col_annotation = object$data_person %>%
    inner_join(
      object$data_initial %>%
        filter(COHORT_DEFINITION_ID == 2) %>%
        select(PERSON_ID = SUBJECT_ID, COHORT_START_DATE),
      by = "PERSON_ID"
    ) %>%
    mutate(AGE = floor(as.numeric(COHORT_START_DATE - lubridate::as_date(stringr::str_glue("{YEAR_OF_BIRTH}-01-01"))) / 365.25)) %>%
    select(PERSON_ID, GENDER_CONCEPT_ID, AGE) %>%
    mutate(GENDER = factor(GENDER_CONCEPT_ID, levels = c(8507, 8532), labels = c("Male", "Female"))) %>%
    mutate(PERSON_ID = stringr::str_c("PID_", PERSON_ID)) %>%
    as.data.frame() %>%
    column_to_rownames("PERSON_ID") %>%
    select(-GENDER_CONCEPT_ID)

  res = list(target_matrix = target_matrix, target_row_annotation = target_row_annotation, target_col_annotation = target_col_annotation)

  return(res)
}

filter_target = function(target, prevalence_thereshold, prevalence_ratio_threshold, domain){
  res = target
  res$target_row_annotation = target$target_row_annotation %>%
    filter(PREVALENCE > prevalence_thereshold) %>%
    filter(PREVALENCE_DIFFERENCE_RATIO > prevalence_ratio_threshold) %>%
    filter(HERITAGE %in% domain)

  res$target_matrix = target$target_matrix[rownames(res$target_row_annotation), ]

  return(res)
}

plot_prevalence = function(filtered_target){
  plotdata = filtered_target$target_row_annotation %>%
    rownames_to_column("CONCEPT_ID") %>%
    as_tibble() %>%
    left_join(
      tibble(
        CONCEPT_ID = rownames(filtered_target$target_matrix),
        PRESENCE = lapply(seq_len(nrow(filtered_target$target_matrix)), function(i) filtered_target$target_matrix[i,])
      ),
      by = "CONCEPT_ID"
    ) %>%
    mutate(PREVALENCE = map_dbl(PRESENCE, mean)) %>%
    mutate(AVERAGE_AGE = map_dbl(PRESENCE, ~ mean(filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)]))) %>%
    mutate(AVERAGE_AGE_OVERALL = mean(filtered_target$target_col_annotation$AGE)) %>%
    mutate(AGE_DIFF = map(PRESENCE, ~ t.test(filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)], filtered_target$target_col_annotation[names(.x), "AGE"]))) %>%
    mutate(AGE_DIFF_ESTIMATE = map_dbl(AGE_DIFF, ~ .x$estimate[1])) %>%
    mutate(AGE_DIFF_LOW = map_dbl(AGE_DIFF, ~ .x$estimate[2] + .x$conf.int[1])) %>%
    mutate(AGE_DIFF_HIGH = map_dbl(AGE_DIFF, ~ .x$estimate[2] + .x$conf.int[2])) %>%
    mutate(AGE_DIFF_SIGNIFICANT = map_lgl(AGE_DIFF, ~ .x$p.value < 0.05)) %>%
    mutate(MALE_PROP = map_dbl(PRESENCE, ~ mean(filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male"))) %>%
    mutate(MALE_PROP_OVERALL = mean(filtered_target$target_col_annotation$GENDER == "Male")) %>%
    mutate(MALE_PROP_DIFF = map(PRESENCE, ~ t.test(filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male", filtered_target$target_col_annotation[names(.x), "GENDER"] == "Male"))) %>%
    mutate(MALE_PROP_DIFF_ESTIMATE = map_dbl(MALE_PROP_DIFF, ~ .x$estimate[1])) %>%
    mutate(MALE_PROP_DIFF_LOW = map_dbl(MALE_PROP_DIFF, ~ .x$estimate[2] + .x$conf.int[1])) %>%
    mutate(MALE_PROP_DIFF_HIGH = map_dbl(MALE_PROP_DIFF, ~ .x$estimate[2] + .x$conf.int[2])) %>%
    mutate(MALE_PROP_DIFF_SIGNIFICANT = map_lgl(MALE_PROP_DIFF, ~ .x$p.value < 0.05))

  p1 = ggplot(plotdata, aes(x = PREVALENCE, y = CONCEPT_NAME, fill = PREVALENCE_DIFFERENCE_RATIO)) +
    geom_bar(stat = "identity") +
    facet_grid(HERITAGE ~ ., space = "free_y", scales = "free_y") +
    scale_fill_viridis_c("Prevalence ratio \ncompared to background", limits = c(1, 70)) + #,transform = "log10") +
    scale_x_continuous(labels = scales::label_percent()) +
    ggtitle("Prevalence") +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text = element_blank()
    )

  p2 = ggplot(plotdata, aes(y = CONCEPT_NAME, color = AGE_DIFF_SIGNIFICANT)) +
    geom_point(aes(x = AGE_DIFF_ESTIMATE)) +
    geom_errorbar(aes(xmin = AGE_DIFF_LOW, xmax = AGE_DIFF_HIGH)) +
    geom_vline(aes(xintercept = AVERAGE_AGE_OVERALL), color = "grey60") +
    scale_color_manual(values = c("grey60", "black"), breaks = c(FALSE, TRUE)) +
    facet_grid(HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggtitle("AGE in group") +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none"
    )

  p3 = ggplot(plotdata, aes(y = CONCEPT_NAME, color = MALE_PROP_DIFF_SIGNIFICANT)) +
    geom_point(aes(x = MALE_PROP_DIFF_ESTIMATE)) +
    geom_errorbar(aes(xmin = MALE_PROP_DIFF_LOW, xmax = MALE_PROP_DIFF_HIGH)) +
    geom_vline(aes(xintercept = MALE_PROP_OVERALL), color = "grey60") +
    scale_color_manual(values = c("grey60", "black"), breaks = c(FALSE, TRUE)) +
    scale_x_continuous(labels = scales::label_percent()) +
    facet_grid(HERITAGE ~ ., space = "free_y", scales = "free_y") +
    ggtitle("Male percentage in group") +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )

  p = p1 + p2 + p3

  return(p)
  # ggsave(str_glue("{pathToResults}/prevalence_{name}.pdf"), plot = p, height = min(c(nrow(plotdata) * 0.35 + 2, 49)),  width = 12)
}

plot_heatmap = function(filtered_target){
  # Patient clustering
  col_clustering = hclust(dist(t(filtered_target$target_matrix)))

  # Row reordering
  reordering = filtered_target$target_row_annotation %>%
    rownames_to_column("CONCEPT_ID") %>%
    group_by(HERITAGE) %>%
    summarize(CONCEPT_ID = list(CONCEPT_ID)) %>%
    mutate(MATRIX = map(CONCEPT_ID, ~ filtered_target$target_matrix[.x, , drop = F])) %>%
    mutate(MATRIX = map(
      MATRIX,
      function(x){
        if(nrow(x) > 1){
          x = x[hclust(dist(x))$order, ]
        }

        return(x)
      }
    ))

  tm = do.call(rbind, reordering$MATRIX)
  tm_gaps = map_int(reordering$MATRIX, nrow) %>% cumsum()

  # save(tm, target_row_annotation, person, file = str_c(pathToResults, "/matrix.RData"))

  tm2 = tm
  rownames(tm2) = filtered_target$target_row_annotation[rownames(tm), ]$CONCEPT_NAME


  # #######################
  # # Define colors for Gender
  # Define gender colors ----------
  # gender_colors <- c("Male" = "blue", "Female" = "red")
  #
  # # Check unique values in the GENDER column
  # print(unique(filtered_target$target_col_annotation$GENDER))
  #
  # # Apply the gender color mapping with a safeguard for unhandled values
  # filtered_target$target_col_annotation$GENDER <- ifelse(
  #   filtered_target$target_col_annotation$GENDER %in% names(gender_colors),
  #   gender_colors[filtered_target$target_col_annotation$GENDER],
  #   "grey"  # Default color for unhandled or NA values
  # )
  # # Define a color gradient for Age (example uses Age from 0 to 100) --------
  # age_colors <- grDevices::colorRampPalette(c("white", "purple"))(101)  # Adjust the number as needed
  #
  # # Define colors for Heritage
  # heritage_colors <- c(
  #   "procedure_occurrence" = "purple",
  #   "condition_occurrence" = "orange",
  #   "drug_exposure" = "lightblue",
  #   "measurement" = "pink",
  #   "observation" = "brown"
  # )
  # # Map ages to colors (assuming Age is a numeric column)
  # # Assuming your age data ranges from a minimum age to a maximum age
  # min_age <- min(filtered_target$target_col_annotation$AGE)
  # max_age <- max(filtered_target$target_col_annotation$AGE)
  #
  # # Create a color palette for age that fits the actual data range
  # age_colors <- colorRampPalette(c("white", "purple"))(max_age - min_age + 1)
  #
  # # Scale the age values to the index of the color array
  # scaled_ages <- round((filtered_target$target_col_annotation$AGE - min_age) / (max_age - min_age) * (length(age_colors) - 1)) + 1
  # filtered_target$target_col_annotation$AGE <- age_colors[scaled_ages]
  #
  # # Map genders to colors
  #
  # # Map heritage to colors, handling missing values if they occur
  # filtered_target$target_row_annotation$HERITAGE <- heritage_colors[filtered_target$target_row_annotation$HERITAGE]
  #
  #
  # # Prepare annotations for columns and rows
  # col_annotations <- as.data.frame(t(filtered_target$target_col_annotation))
  # row_annotations <- as.data.frame(filtered_target$target_row_annotation)
  #
  #
  # default_color <- "gray"
  # col_annotations["GENDER",][is.na( col_annotations["GENDER",])] <- default_color
  # row_annotations$HERITAGE[is.na(row_annotations$HERITAGE)] <- default_color
  #
  # # Create the heatmap
  # # Re-attempt to plot with annotations
  # pheatmap(
  #   filtered_target$target_matrix,
  #   annotation_col = col_annotations,
  #   annotation_row = row_annotations,
  #   cluster_cols = hclust(dist(t(filtered_target$target_matrix))),
  #   cluster_rows = FALSE,
  #   show_colnames = FALSE,
  #   color = colorRampPalette(c("#e5f5f9", "#2ca25f"))(256),
  #   gaps_row = c(0, cumsum(sapply(reordering$MATRIX, nrow))),
  #   legend_breaks = c(0.25, 0.75),
  #   legend_labels = c("Absent", "Present")
  # )

  ########################

  #
  pheatmap(
    tm2,
    show_colnames = F,
    annotation_row = filtered_target$target_row_annotation %>%
      remove_rownames() %>%
      column_to_rownames("CONCEPT_NAME") %>%
      select(-PREVALENCE_DIFFERENCE_RATIO),
    annotation_col = filtered_target$target_col_annotation,
    cluster_cols = col_clustering,
    cluster_rows = FALSE,
    gaps_row = tm_gaps,
    color = c("#e5f5f9", "#2ca25f"),
    legend_breaks = c(0.25, 0.75),
    legend_labels = c("Absent", "Present")
    # cellheight = 15,
    # width = 10,
    # filename = str_glue("{pathToResults}/heatmap_{name}.pdf")
  )
}
