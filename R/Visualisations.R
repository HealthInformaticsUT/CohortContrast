#' @importFrom dplyr %>%

#' @title Function for creating matrix for the heatmap which will be shown in shiny
#'
#' @param data data object returned by CohortContrast function
#' @param targetCohortId Target cohort id
#' @param prevalenceCutOff numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param presenceFilter numeric > if set, removes all features represented less than the given percentage
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present
#' @keywords internal

createTargetMatrixForHeatmap <- function(data,
                                         targetCohortId,
                                         prevalenceCutOff,
                                         presenceFilter,
                                         complementaryMappingTable = FALSE) {
  # Extracting and joining concepts
  concepts <- dplyr::filter(
    dplyr::inner_join(
      dplyr::distinct(dplyr::filter(
        dplyr::select(data$data_patients, .data$CONCEPT_ID, .data$CONCEPT_NAME, .data$HERITAGE),
        .data$CONCEPT_ID != 0
      )),
      data$data_features,
      by = c("CONCEPT_ID", "CONCEPT_NAME")
    ),!is.na(.data$PREVALENCE_DIFFERENCE_RATIO)
  )



  # Creating target dataframe
  target <- dplyr::left_join(
    dplyr::select(
      dplyr::filter(data$data_patients,
                    .data$COHORT_DEFINITION_ID == targetCohortId),
      -.data$COHORT_DEFINITION_ID
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
                        .data$PREVALENCE_DIFFERENCE_RATIO > prevalenceCutOff),
          PRESENT = 1
        ),
        NPATIENTS = length(unique(.data$PERSON_ID))
      ),
      .data$ CONCEPT_ID
    ),
    NCONCEPTS = dplyr::n()
  )),
  .data$NCONCEPTS / .data$NPATIENTS > presenceFilter)
  # Step 4: Create a summary for patients without concepts
  no_concept_summary <- dplyr::summarize(
    dplyr::group_by(.data$target, .data$PERSON_ID),
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
      .data$CONCEPT_ID,
      .data$CONCEPT_NAME,
      .data$HERITAGE,
      .data$PREVALENCE_DIFFERENCE_RATIO
    ),
    names_from = .data$PERSON_ID,
    values_from = .data$PRESENT,
    names_prefix = "PID_",
    values_fill = 0
  )
  # Step 6: Filter out placeholder and convert CONCEPT_ID to character
  target_df <-
    dplyr::filter(wide_data, .data$CONCEPT_ID != '999999999' &
                    .data$CONCEPT_ID != '0')

  target_df =  dplyr::distinct(dplyr::summarise(dplyr::group_by(target_df, .data$CONCEPT_NAME, .data$HERITAGE),
                                             CONCEPT_ID = dplyr::first(.data$CONCEPT_ID),
                                             PREVALENCE_DIFFERENCE_RATIO = sum(.data$PREVALENCE_DIFFERENCE_RATIO, na.rm = T),
                                             dplyr::across(tidyr::starts_with("PID_"),
                                                    function(x) ifelse(sum(x, na.rm = TRUE) > 0, 1, 0), # Use explicit function definition
                                                    .names = "{.col}"),
                                             .groups = 'drop'
  ))
  target_df$CONCEPT_ID <- as.character(target_df$CONCEPT_ID)

  target_row_annotation = dplyr::select(tibble::column_to_rownames(as.data.frame(target_df), "CONCEPT_ID"),
                                        -tidyr::starts_with("PID_"))
  target_matrix = as.matrix(dplyr::select(
    tibble::column_to_rownames(as.data.frame(target_df), "CONCEPT_ID"),
    tidyr::starts_with("PID_")
  ))

  person_data <- data$data_person

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
    dplyr::select(person_data, .data$PERSON_ID, .data$GENDER, .data$YEAR_OF_BIRTH)
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

#' Function for creating the heatmap which will be shown in shiny
#'
#' @param data data object returned by CohortContrast function
#' @param cohortDefinitionId Target cohort id
#' @param prevalenceRatioThreshold numeric > if set, removes all of the concepts which are not present (in target) more than prevalenceCutOff times
#' @param prevalenceThreshold numeric > if set, removes all features represented less than the given percentage
#' @param complementaryMappingTable Mappingtable for mapping concept_ids if present
#' @keywords internal

createHeatmap <-
  function(data,
           cohortDefinitionId,
           prevalenceRatioThreshold,
           prevalenceThreshold,
           complementaryMappingTable = NULL) {
    heatmapData <- createTargetMatrixForHeatmap(
      data = data,
      targetCohortId = cohortDefinitionId,
      prevalenceCutOff = prevalenceRatioThreshold,
      presenceFilter = prevalenceThreshold,
      complementaryMappingTable = complementaryMappingTable
    )
    target_matrix = heatmapData$target_matrix
    person = heatmapData$demographics
    target_row_annotation = heatmapData$target_row_annotation
    col_clustering = stats::hclust(stats::dist(t(target_matrix)))


    # Row reordering
    reordering =   dplyr::mutate(
      dplyr::mutate(
        dplyr::summarize(
          dplyr::group_by(
            tibble::rownames_to_column(target_row_annotation, "CONCEPT_ID"),
            .data$HERITAGE
          ),
          CONCEPT_ID = list(.data$CONCEPT_ID)
        ),
        MATRIX = purrr::map(.data$CONCEPT_ID, ~ target_matrix[.x, , drop = F])
      ),
      MATRIX = purrr::map(.data$MATRIX,
                          function(x) {
                            if (nrow(x) > 1) {
                              x = x[stats::hclust(stats::dist(x))$order,]
                            }
                            return(x)
                          })
    )

    tm = do.call(rbind, reordering$MATRIX)
    tm_gaps = cumsum(purrr::map_int(reordering$MATRIX, nrow))

    rownames(tm) = target_row_annotation[rownames(tm),]$CONCEPT_NAME

    # Create the heatmap
    heatmapPlot <- pheatmap::pheatmap(
      tm,
      show_colnames = F,
      annotation_row = dplyr::select(
        tibble::column_to_rownames(
          tibble::remove_rownames(target_row_annotation),
          "CONCEPT_NAME"
        ),
        -.data$PREVALENCE_DIFFERENCE_RATIO
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
