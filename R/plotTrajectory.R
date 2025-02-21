#' @keywords internal
createCorrelationTrajectoryGraph <- function(filtered_target, selectedIds = NULL, edgePrevalence = 0.5, selectionList, corrInitiated = FALSE, isPatientLevelDataPresent = TRUE) {
  if (isFALSE(corrInitiated)){
    plot <- getErrorPlot(message = "Correlation view must be enabled")
    return(plot)
  } else if(!isPatientLevelDataPresent) {
    plot <- getErrorPlot(message = "This feature is not supported without patient level data")
    return(plot)
  } else if (is.null(selectedIds) | length(selectedIds) == 0) {
    plot <- getErrorPlot(message = "A correlation group must be selected")
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
      dplyr::filter(dplyr::case_when(
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
