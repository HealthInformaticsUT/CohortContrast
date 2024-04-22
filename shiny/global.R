############################### Global variables
library(ggplot2)
############################## Shiny functions

# Functions for data loading and formatting ------------------------------------
format_results = function(pathToResults, studyName){
  load(stringr::str_c(pathToResults, "/tmp/datasets/", studyName, "_CC_medData.rdata"))

  concepts = dplyr::filter(
    dplyr::inner_join(
      dplyr::filter(dplyr::distinct(
        dplyr::select(object$data_patients, CONCEPT_ID, CONCEPT_NAME, HERITAGE)
      ), CONCEPT_ID != 0),
      object$data_features,
      by = c("CONCEPT_ID", "CONCEPT_NAME")
    ) ,
    !is.na(PREVALENCE_DIFFERENCE_RATIO)
  )

  target = dplyr::filter(
    dplyr::left_join(
      dplyr::select(
        dplyr::filter(object$data_patients, COHORT_DEFINITION_ID == 2),
        -COHORT_DEFINITION_ID
      ),
      concepts,
      by = c("CONCEPT_ID", "CONCEPT_NAME", "HERITAGE")
    ),
    !is.na(PREVALENCE_DIFFERENCE_RATIO)
  )

  target_df = dplyr::mutate(dplyr::filter(
    dplyr::mutate(
      dplyr::filter(
        tidyr::pivot_wider(
          dplyr::bind_rows(
            dplyr::mutate(dplyr::ungroup(
              dplyr::mutate(dplyr::group_by(
                dplyr::mutate(dplyr::mutate(target, NPATIENTS = length(unique(
                  PERSON_ID
                ))), PRESENT = 1), CONCEPT_ID
              ), NCONCEPTS = dplyr::n())
            ), PREVALENCE = NCONCEPTS / NPATIENTS),
            dplyr::summarize(
              dplyr::group_by(target, PERSON_ID),
              CONCEPT_ID = 999999999,
              CONCEPT_NAME = "None",
              PREVALENCE = 99999,
              HERITAGE = "none",
              PREVALENCE_DIFFERENCE_RATIO = -1
            )
          ),
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
        ) ,
        CONCEPT_ID != 999999999
      ),
      CONCEPT_ID = as.character(CONCEPT_ID)
    ),
    CONCEPT_ID != "0"
  ),
  CONCEPT_NAME = substr(CONCEPT_NAME, 1, 30))


  # TODO
  target_row_annotation = dplyr::select(
    tibble::column_to_rownames(as.data.frame(target_df), "CONCEPT_ID"),
    -dplyr::starts_with("PID_")
  )

  target_matrix = as.matrix(dplyr::select(
    tibble::column_to_rownames(as.data.frame(target_df), "CONCEPT_ID"),
    dplyr::starts_with("PID_")
  ))

  # Demographics
  target_col_annotation =  dplyr::select(tibble::column_to_rownames(as.data.frame(
    dplyr::mutate(
      dplyr::mutate(
        dplyr::select(
          dplyr::mutate(
            dplyr::inner_join(
              object$data_person,
              dplyr::select(
                dplyr::filter(object$data_initial, COHORT_DEFINITION_ID == 2),
                PERSON_ID = SUBJECT_ID,
                COHORT_START_DATE
              ),
              by = "PERSON_ID"
            ),
            AGE = floor(as.numeric(
              COHORT_START_DATE - lubridate::as_date(stringr::str_glue("{YEAR_OF_BIRTH}-01-01"))
            ) / 365.25)
          ),
          PERSON_ID,
          GENDER_CONCEPT_ID,
          AGE
        ) ,
        GENDER = factor(
          GENDER_CONCEPT_ID,
          levels = c(8507, 8532),
          labels = c("Male", "Female")
        )
      ),
      PERSON_ID = stringr::str_c("PID_", PERSON_ID)
    )
  ) , "PERSON_ID"),
  -GENDER_CONCEPT_ID)

  res = list(target_matrix = target_matrix, target_row_annotation = target_row_annotation, target_col_annotation = target_col_annotation)

  return(res)
}

filter_target = function(target, prevalence_thereshold, prevalence_ratio_threshold, domain){
  res = target
  res$target_row_annotation =  dplyr::filter(
    dplyr::filter(
      dplyr::filter(
        target$target_row_annotation ,
        PREVALENCE > prevalence_thereshold
      ),
      PREVALENCE_DIFFERENCE_RATIO > prevalence_ratio_threshold
    ),
    HERITAGE %in% domain
  )

  res$target_matrix = target$target_matrix[rownames(res$target_row_annotation), ]

  return(res)
}

plot_prevalence = function(filtered_target){
  plotdata =
    dplyr::mutate(
      dplyr::left_join(
        tibble::as_tibble(
          tibble::rownames_to_column(filtered_target$target_row_annotation, "CONCEPT_ID")
        ),
        tibble::tibble(
          CONCEPT_ID = rownames(filtered_target$target_matrix),
          PRESENCE = lapply(seq_len(nrow(
            filtered_target$target_matrix
          )), function(i)
            filtered_target$target_matrix[i, ])
        ),
        by = "CONCEPT_ID"
      ),
      PREVALENCE = purrr::map_dbl(PRESENCE, mean),
      AVERAGE_AGE = purrr::map_dbl(PRESENCE, ~ mean(
        filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)]
      )),
      AVERAGE_AGE_OVERALL = mean(filtered_target$target_col_annotation$AGE),
      AGE_DIFF = map(
        PRESENCE,
        ~ t.test(
          filtered_target$target_col_annotation[names(.x), "AGE"][as.logical(.x)],
          filtered_target$target_col_annotation[names(.x), "AGE"]
        )
      ),
      AGE_DIFF_ESTIMATE = purrr::map_dbl(AGE_DIFF, ~ .x$estimate[1]),
      AGE_DIFF_LOW = purrr::map_dbl(AGE_DIFF, ~ .x$estimate[2] + .x$conf.int[1]),
      AGE_DIFF_HIGH = purrr::map_dbl(AGE_DIFF, ~ .x$estimate[2] + .x$conf.int[2]),
      AGE_DIFF_SIGNIFICANT = purrr::map_lgl(AGE_DIFF, ~ .x$p.value < 0.05),
      MALE_PROP = purrr::map_dbl(
        PRESENCE,
        ~ mean(filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male")
      ),
      MALE_PROP_OVERALL = mean(filtered_target$target_col_annotation$GENDER == "Male"),
      MALE_PROP_DIFF = map(
        PRESENCE,
        ~ t.test(
          filtered_target$target_col_annotation[names(.x), "GENDER"][as.logical(.x)] == "Male",
          filtered_target$target_col_annotation[names(.x), "GENDER"] == "Male"
        )
      ),
      MALE_PROP_DIFF_ESTIMATE = purrr::map_dbl(MALE_PROP_DIFF, ~ .x$estimate[1]),
      MALE_PROP_DIFF_LOW = purrr::map_dbl(MALE_PROP_DIFF, ~ .x$estimate[2] + .x$conf.int[1]),
      MALE_PROP_DIFF_HIGH = purrr::map_dbl(MALE_PROP_DIFF, ~ .x$estimate[2] + .x$conf.int[2]),
      MALE_PROP_DIFF_SIGNIFICANT = map_lgl(MALE_PROP_DIFF, ~ .x$p.value < 0.05)
    )

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
  col_clustering = stats::hclust(stats::dist(t(filtered_target$target_matrix)))

  # Row reordering
  reordering = dplyr::mutate(
      dplyr::mutate(
        dplyr::summarize(
          dplyr::group_by(
            tibble::rownames_to_column(filtered_target$target_row_annotation, "CONCEPT_ID"),
            HERITAGE
          ),
          CONCEPT_ID = list(CONCEPT_ID)
        ),
        MATRIX = purrr::map(CONCEPT_ID, ~ filtered_target$target_matrix[.x, , drop = F])
      ),
      MATRIX = purrr::map(MATRIX,
                          function(x) {
                            if (nrow(x) > 1) {
                              x = x[stats::hclust(stats::dist(x))$order,]
                            }

                            return(x)
                          })
    )

  tm = do.call(rbind, reordering$MATRIX)
  tm_gaps = cumsum(purrr::map_int(reordering$MATRIX, nrow))

  # save(tm, target_row_annotation, person, file = str_c(pathToResults, "/matrix.RData"))

  tm2 = tm
  rownames(tm2) = filtered_target$target_row_annotation[rownames(tm), ]$CONCEPT_NAME

  annotation_row = dplyr::select(
    tibble::column_to_rownames(
      tibble::remove_rownames(filtered_target$target_row_annotation) ,
      "CONCEPT_NAME"
    ),
    -PREVALENCE_DIFFERENCE_RATIO
  )
  annotation_col = filtered_target$target_col_annotation

  # Heritage colors pre-defined
  heritage_colors <- c(
    procedure_occurrence = "darkblue",
    condition_occurrence = "orange",
    drug_exposure = "lightblue",
    measurement = "pink",
    observation = "brown"
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
