#' Create a mapping table with predefined maximum abstraction level
#' @param cdm Connection to database
#' @param abstractionLevel Maximum abstraction level allowed
#' @param data CohortContrastObject returned by CohortContrast function
#' @param concept_ancestor Data frame of concept_ancestor table
#' @param concept Data frame of concept table
#' @param maxMinDataFrame A dataframe with columns descendant_concept_id and max_levels_of_separation
#' @export
generateMappingTable <- function(cdm, abstractionLevel = 10, data = NULL, concept_ancestor = NULL, concept = NULL, maxMinDataFrame = NULL){
  printCustomMessage("Generating mapping table list ...")
  if(is.null(maxMinDataFrame)){
    maxMinDataFrame <- concept_ancestor %>%
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

    mappings <- concept_ancestor %>% dplyr::filter(.data$max_levels_of_separation == min_separation_level - abstractionLevel) %>%
      dplyr::inner_join(maxMinDataFrame %>% dplyr::filter(.data$maximum_minimal_separation == abstractionLevel), by = c("ancestor_concept_id" = "descendant_concept_id"))%>%
      dplyr::inner_join(concept_ids_to_map, by = "descendant_concept_id") %>% dplyr::select(concept_id = .data$descendant_concept_id,
                                                                                            concept_id.new = .data$ancestor_concept_id)

    abstractionLevelMappingTable = mappings %>% dplyr::left_join(concept %>% dplyr::filter(is.na(.data$invalid_reason)), by = c("concept_id.new" = "concept_id"), suffix = c("", ".concept")) %>%
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
      dplyr::select(.data$CONCEPT_ID, .data$CONCEPT_NAME, .data$NEW_CONCEPT_ID, .data$NEW_CONCEPT_NAME, .data$ABSTRACTION_LEVEL)

    # Check if any mappings were updated
    if (length(originalMapping) == length(mappingTable$NEW_CONCEPT_ID)){
      if (!all(originalMapping == mappingTable$NEW_CONCEPT_ID)) {
        mappingChanged <- TRUE
      }
    }
  }

  return(mappingTable)
}
