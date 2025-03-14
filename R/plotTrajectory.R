#' @keywords internal
createBPMNTrajectoryData <- function(filtered_target = NULL, selectedIds = NULL, edgePrevalence = 0.5, selectionList = NULL, isPatientLevelDataPresent = TRUE) {

  if (is.null(edgePrevalence)) {
    edgePrevalence = 0.5
  }

  patients_data <- filtered_target$target_time_annotation
  if(!is.null(selectedIds)) {
    patients_data <- filtered_target$target_time_annotation %>%
      dplyr::filter(CONCEPT_ID %in% selectedIds)
  }

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
    dplyr::mutate(NEXT_CONCEPT_NAME = dplyr::lead(CONCEPT_NAME),
                  TRANSITION_TIME = dplyr::lead(TIME_TO_EVENT) - TIME_TO_EVENT,
                  TIME_OF_OCCURRENCE = TIME_TO_EVENT) %>%
    dplyr::filter(!is.na(NEXT_CONCEPT_NAME)) %>%
    dplyr::ungroup() %>%
    dplyr::select(PERSON_ID, CONCEPT_NAME, NEXT_CONCEPT_NAME, TRANSITION_TIME, TIME_OF_OCCURRENCE)

  edges_summary <- edges %>%
    dplyr::group_by(CONCEPT_NAME, NEXT_CONCEPT_NAME) %>%
    dplyr::summarise(WEIGHT = dplyr::n(),
                     PREVALENCE_PERC = (dplyr::n_distinct(PERSON_ID) / total_patients),
                     MEDIAN_TRANSITION_TIME = mean(TRANSITION_TIME, na.rm = TRUE),
                     MEDIAN_TIME_OF_OCCURRENCE = mean(TIME_OF_OCCURRENCE, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::filter(PREVALENCE_PERC >= edgePrevalence)

  colnames(edges_summary) = tolower(colnames(edges_summary))
  nodes <- edges_summary %>%
    dplyr::group_by(concept_name) %>%
    dplyr::summarize(median_time_of_occurrence = median(median_time_of_occurrence, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    rename(label = concept_name)

  nodes_endless <- setdiff(edges_summary$next_concept_name, nodes$label)
  for (endless_node in nodes_endless) {
    occurrence_time <- max(nodes$median_time_of_occurrence, na.rm = TRUE) + 1
    nodes <- rbind(nodes, c(endless_node, occurrence_time))
    nodes$median_time_of_occurrence <- as.numeric(nodes$median_time_of_occurrence)
  }

  # Assign unique IDs
  nodes <- nodes %>%
    dplyr::arrange(median_time_of_occurrence) %>%
    dplyr::mutate(id = paste0("Node_", row_number()))

  # Assign BPMN Types
  nodes <- nodes %>%
    dplyr::mutate(
      type = dplyr::case_when(
        label == "START" ~ "bpmn:StartEvent",
        grepl("EXIT", label) ~ "bpmn:EndEvent",
        TRUE ~ "bpmn:Task"
      )
    )

  # Step 2: Assign X-Coordinates Based on Order (100px apart)
  nodes <- nodes %>%
    mutate(position_x = row_number() * 250)  # ✅ Every node is spaced 100px apart

  # Step 3: Improve Spreading of Overlapping Nodes (Stagger Overlapping)
  threshold <- 30  # Time similarity threshold for stacking
  stagger_spacing <- 100  # Distance between stacked nodes

  current_y_positions <- list()

  for (i in seq_len(nrow(nodes))) {
    current_time <- nodes$median_time_of_occurrence[i]

    # Find similar times and stagger them
    similar_times <- names(current_y_positions)[
      abs(as.numeric(names(current_y_positions)) - current_time) < threshold
    ]

    if (length(similar_times) > 0) {
      # Stack slightly below existing group
      existing_y_values <- unlist(current_y_positions[similar_times])
      nodes$position_y[i] <- max(existing_y_values) + stagger_spacing
    } else {
      # Default Y level
      nodes$position_y[i] <- 100
    }

    # Store new Y position for this time group
    current_y_positions[[as.character(current_time)]] <- nodes$position_y[i]
  }

  colnames(edges_summary) = tolower(colnames(edges_summary))

  # Step 4: Improve Edge Structure (Add Prevalence & Transition Time)
  edges <- edges_summary %>%
    dplyr::left_join(nodes, by = c("concept_name" = "label")) %>%
    dplyr::left_join(nodes, by = c("next_concept_name" = "label"), suffix = c("_source", "_target")) %>%
    dplyr::filter(!is.na(id_source) & !is.na(id_target)) %>%
    dplyr::mutate(
      id = paste0("Edge_", id_source, "_", id_target),  # ✅ Unique edge ID
      source = id_source,
      target = id_target,
      label = paste0("Prevalence: ", round(prevalence_perc * 100, 1), "%\nTime: ", round(median_transition_time, 2), " days"),
      prevalence = round(prevalence_perc * 100, 1)
    ) %>%
    dplyr::select(id, source, target, label, prevalence)

  # Step 5: Convert to JSON for BPMN-js
  bpmn_json <- list(nodes = nodes, edges = edges)

  return(bpmn_json)
}
