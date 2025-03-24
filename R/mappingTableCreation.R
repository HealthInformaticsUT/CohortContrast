#' Create a mapping table with predefined maximum abstraction level
#' @param abstractionLevel Maximum abstraction level allowed
#' @param data CohortContrastObject returned by CohortContrast function
#' @param maxMinDataFrame A dataframe with columns descendant_concept_id and max_levels_of_separation
#' @export
generateMappingTable <- function(abstractionLevel = 10, data = NULL, maxMinDataFrame = NULL){
  printCustomMessage("Generating mapping table list ...")
  if(is.null(maxMinDataFrame)){
    maxMinDataFrame <-  data$conceptsData$concept_ancestor %>%
    dplyr::group_by(.data$descendant_concept_id) %>%
    dplyr::summarise(maximum_minimal_separation = max(.data$max_levels_of_separation))
 }
  need_to_map_concept_ids <- maxMinDataFrame %>% dplyr::filter(.data$maximum_minimal_separation > abstractionLevel)

  # if(!is.null(data)){
  #   need_to_map_concept_ids <- need_to_map_concept_ids %>%  dplyr::filter(.data$descendant_concept_id %in% data$data_patients$CONCEPT_ID)
  # }

  max_minimal_separation_levels = unique(need_to_map_concept_ids %>% dplyr::select(.data$maximum_minimal_separation) %>% dplyr::pull())

  # Create an empty dataframe with the specified columns
  complementaryMappingTable = data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), NEW_CONCEPT_ID = integer(), NEW_CONCEPT_NAME = character() , ABSTRACTION_LEVEL = integer(), stringsAsFactors = FALSE)

  for(min_separation_level in sort(max_minimal_separation_levels)){
    printCustomMessage(paste("Generating mapping for abstraction level",min_separation_level,'...', sep = " " ))
    # Create an empty dataframe with the specified columns

    concept_ids_to_map <- need_to_map_concept_ids %>% dplyr::filter(.data$maximum_minimal_separation == min_separation_level)

    mappings <- data$conceptsData$concept_ancestor %>% dplyr::filter(.data$max_levels_of_separation == min_separation_level - abstractionLevel) %>%
      dplyr::inner_join(maxMinDataFrame %>% dplyr::filter(.data$maximum_minimal_separation == abstractionLevel), by = c("ancestor_concept_id" = "descendant_concept_id")) %>%
      dplyr::inner_join(concept_ids_to_map, by = "descendant_concept_id") %>% dplyr::select(concept_id = .data$descendant_concept_id,
                                                                                            concept_id.new = .data$ancestor_concept_id)

    abstractionLevelMappingTable = mappings %>% dplyr::left_join(data$conceptsData$concept %>% dplyr::filter(is.na(.data$invalid_reason)), by = c("concept_id.new" = "concept_id"), suffix = c("", ".concept")) %>%
      dplyr::inner_join(data$data_patients %>% dplyr::filter(ABSTRACTION_LEVEL == -1) %>% dplyr::select(CONCEPT_ID, CONCEPT_NAME) %>% dplyr::distinct() , by = c("concept_id" = "CONCEPT_ID"), suffix = c("old", "")) %>%
      dplyr::select(CONCEPT_ID = .data$concept_id,
                    CONCEPT_NAME = .data$CONCEPT_NAME,
                    NEW_CONCEPT_ID = .data$concept_id.new,
                    NEW_CONCEPT_NAME = .data$concept_name) %>% as.data.frame() %>%
                    dplyr::mutate(ABSTRACTION_LEVEL = abstractionLevel)
    #complementaryMappingTable <- updateMapping(abstractionLevelMappingTable)
    complementaryMappingTable <- rbind(complementaryMappingTable, abstractionLevelMappingTable)
  }
  # complementaryMappingTable <- complementaryMappingTable %>% dplyr::select(CONCEPT_ID = .data$concept_id,
  #                                                                          CONCEPT_ID.new = .data$concept_id.new,
  #                                                                          CONCEPT_NAME.new = .data$concept_name.new)
  complementaryMappingTable <- complementaryMappingTable %>% dplyr::mutate(NEW_CONCEPT_NAME = dplyr::if_else(is.na(.data$NEW_CONCEPT_NAME), paste("Unnamed concept", .data$NEW_CONCEPT_ID), .data$NEW_CONCEPT_NAME))

  return(complementaryMappingTable)
}

#' @keywords internal
assertAncestryCompleteness <- function(cdm){
  concept_ids_to_check <- cdm$concept_ancestor %>% dplyr::select(.data$ancestor_concept_id) %>% dplyr::distinct()

  joined_data <- concept_ids_to_check %>%
    dplyr::left_join(cdm$concept %>% dplyr::filter(is.na(.data$invalid_reason)), by = c("ancestor_concept_id" = "concept_id")) %>% dplyr::select(.data$ancestor_concept_id, .data$concept_name) %>% as.data.frame()
  boolean_name <- is.na(joined_data$concept_name)
  all_rows_have_concept_name <- all(!boolean_name)
  if(!all_rows_have_concept_name){
    rows_not_complete <- joined_data$ancestor_concept_id[boolean_name]
    warning(paste("All concept ids do not have names in the concept table:", paste(rows_not_complete, collapse = ",")))
  }
  }


updateMapping <- function(mappingTable) {
  required_columns <- c("CONCEPT_ID", "CONCEPT_NAME", "NEW_CONCEPT_ID", "NEW_CONCEPT_NAME", "ABSTRACTION_LEVEL")
  # Check if all required columns are present
  if (!all(required_columns %in% colnames(mappingTable))) {
    return(mappingTable)
  }
  mappingChanged <- TRUE

  while (mappingChanged) {
    mappingChanged <- FALSE

    # Create a copy of the original concept_id.new mappings for comparison after updating
    originalMapping <- mappingTable$NEW_CONCEPT_ID

    # Join the table to itself to find secondary mappings
    mappingTable <- mappingTable %>%
      dplyr::left_join(mappingTable, by = c("NEW_CONCEPT_ID" = "CONCEPT_ID"), suffix = c("", ".copy"), relationship = "many-to-many") %>% dplyr::mutate(
        CONCEPT_ID.new = dplyr::if_else(!is.na(.data$NEW_CONCEPT_ID.copy), .data$NEW_CONCEPT_ID.copy, .data$NEW_CONCEPT_ID),
        CONCEPT_NAME.new = dplyr::if_else(!is.na(.data$NEW_CONCEPT_NAME.copy), .data$NEW_CONCEPT_NAME.copy, .data$NEW_CONCEPT_NAME)
      ) %>%
      dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, NEW_CONCEPT_ID = .data$CONCEPT_ID.new, NEW_CONCEPT_NAME = .data$CONCEPT_NAME.new, .data$ABSTRACTION_LEVEL) %>%
      dplyr::distinct()
    # dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$NEW_CONCEPT_ID, .data$NEW_CONCEPT_NAME, .data$ABSTRACTION_LEVEL)

    # Check if any mappings were updated
    if (length(originalMapping) == length(mappingTable$NEW_CONCEPT_ID)){
      if (!all(originalMapping == mappingTable$NEW_CONCEPT_ID)) {
        mappingChanged <- TRUE
      }
    } else {
      mappingChanged <- TRUE
    }
  }
  # Determine the most popular NEW_CONCEPT_ID globally
  global_popularity <- mappingTable %>%
    dplyr::count(.data$NEW_CONCEPT_ID, .data$NEW_CONCEPT_NAME, .data$ABSTRACTION_LEVEL, name = "global_count")

  # Keep only the most globally popular NEW_CONCEPT_ID per CONCEPT_ID
  mappingTable <- mappingTable %>%
    dplyr::left_join(global_popularity, by = c("NEW_CONCEPT_ID", "NEW_CONCEPT_NAME", "ABSTRACTION_LEVEL")) %>%
    dplyr::group_by(.data$CONCEPT_ID) %>%
    dplyr::slice_max(order_by = .data$global_count, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$global_count)

  return(mappingTable)
}

#' @keywords internal
getAncestorMappings <- function(active_concept_ids, concept_table, concept_ancestor, allowed_parents = NULL) {
  # Step 1: Filter concept_ancestor for relevant descendant concepts
  filtered_ancestor_data <- concept_ancestor[
    descendant_concept_id %in% active_concept_ids
  ]

  # Step 2: Group by ancestor and keep only those with >1 descendant
  ancestor_groups <- filtered_ancestor_data[
    , .(
      CONCEPT_IDS = list(unique(descendant_concept_id)),
      COUNT = data.table::uniqueN(descendant_concept_id)
    ),
    by = ancestor_concept_id
  ][COUNT > 1]

  # Step 3: Get concept names for the CONCEPT_IDS (descendants)
  concept_names_lookup <- concept_table[, .(concept_id, concept_name)]
  data.table::setkey(concept_names_lookup, concept_id)

  ancestor_groups[, CONCEPT_NAMES := lapply(CONCEPT_IDS, function(ids) {
    concept_names_lookup[
      data.table::data.table(concept_id = ids),
      on = "concept_id",
      concept_name,
      nomatch = 0L
    ]
  })]

  # Step 4: Add parent concept name using a fast join
  concept_table_subset <- concept_table[, .(concept_id, concept_name)]
  data.table::setnames(concept_table_subset, "concept_id", "ancestor_concept_id")

  ancestor_groups <- concept_table_subset[
    ancestor_groups,
    on = "ancestor_concept_id",
    nomatch = 0L
  ][
    , .(PARENT_ID = ancestor_concept_id,
        PARENT_NAME = concept_name,
        COUNT, CONCEPT_IDS, CONCEPT_NAMES)
  ]

  # Step 5: Keep only rows where PARENT_ID is in allowed_parents
  if (is.null(allowed_parents)) allowed_parents <- active_concept_ids

  ancestor_groups <- ancestor_groups[PARENT_ID %in% allowed_parents]

  return(ancestor_groups)
}



#' @keywords internal
filterHeritagePriorityMappings <- function(df) {
  # Ensure the dataframe is sorted by COUNT in descending order
  df <- df %>%
    dplyr::arrange(dplyr::desc(COUNT))

  # Identify rows where PARENT_ID is present in CONCEPT_IDS
  df <- df %>%
    dplyr::mutate(PARENT_IN_CONCEPTS = purrr::map2_lgl(PARENT_ID, CONCEPT_IDS,
                                                       ~ .x %in% .y))

  # Filter to keep only rows where PARENT_ID appears in CONCEPT_IDS
  sub_df <- df %>% dplyr::filter(PARENT_IN_CONCEPTS)

  # Initialize selected rows and tracking vector for removed concept IDs
  selected_rows <- list()
  removed_concepts <- c()

  # Iterate through sorted sub_df, selecting highest-priority rows first
  for (i in seq_len(nrow(sub_df))) {
    row <- sub_df[i, ]

    # Extract concept IDs
    concept_ids <- unlist(row$CONCEPT_IDS)

    # If none of the concept_ids are in removed_concepts, keep this row
    if (!any(concept_ids %in% removed_concepts)) {
      selected_rows <- append(selected_rows, list(row))
      removed_concepts <- c(removed_concepts, concept_ids)  # Track used concept IDs
    }
  }

  # Combine selected rows into a dataframe
  final_df <- dplyr::bind_rows(selected_rows)

  return(final_df)
}

#' @keywords internal
filterCorrelationMappings <- function(df, data_features, minCorrelation = 0.8, maxDaysInbetween = 1) {
  if (nrow(df) == 0 || is.null(df) || nrow(data_features) == 0 || is.null(data_features)) {
    return(df)
  }

  # Step 1: Filter according to thresholds
  df <- df %>%
    dplyr::filter(CORRELATION >= minCorrelation, MEDIAN_DAYS_INBETWEEN <= maxDaysInbetween)

  # Step 2: Keep only rows where all CONCEPT_IDS have the same HERITAGE
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::filter({
      concept_ids <- CONCEPT_IDS
      heritages <- data_features %>%
        dplyr::filter(CONCEPT_ID %in% concept_ids) %>%
        dplyr::pull(HERITAGE) %>%
        unique()
      length(heritages) == 1
    }) %>%
    dplyr::ungroup()

  # Step 3: Unnest to long format with row_id
  df_long <- df %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    tidyr::unnest_longer(CONCEPT_IDS) %>%
    dplyr::rename(CONCEPT_ID = CONCEPT_IDS)

  # Step 4: Prioritize the best row per CONCEPT_ID
  best_rows <- df_long %>%
    dplyr::arrange(CONCEPT_ID, dplyr::desc(CORRELATION), MEDIAN_DAYS_INBETWEEN) %>%
    dplyr::group_by(CONCEPT_ID) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(row_id) %>%
    unique()

  # Step 5: Keep only rows where all concept_ids are among the best row_ids
  df <- df %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::filter(row_id %in% best_rows) %>%
    dplyr::select(-row_id)

  return(df)
}


