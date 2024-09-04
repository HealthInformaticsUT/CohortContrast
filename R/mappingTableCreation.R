#' Create a mapping table with predefined mmaximum abstraction level
#' @param cdm Connection to database
#' @param maxAbstraction Maximum abstraction level allowed
#' @param data CohortContrastObject returned by CohortContrast function
#' @export
generateMappingTable <- function(cdm, maxAbstraction = 10, data = NULL){

  max_min_levels <- cdm$concept_ancestor %>%
    dplyr::group_by(.data$descendant_concept_id) %>%
    dplyr::summarise(maximum_minimal_separation = max(.data$min_levels_of_separation))

  need_to_map_concept_ids <- max_min_levels %>% dplyr::filter(.data$maximum_minimal_separation > maxAbstraction)

  if(!is.null(data)){
   need_to_map_concept_ids <- need_to_map_concept_ids %>%  dplyr::filter(.data$descendant_concept_id %in% data$data_patients$CONCEPT_ID)
  }

  max_minimal_separation_levels = unique(need_to_map_concept_ids %>% dplyr::select(.data$maximum_minimal_separation) %>% dplyr::pull())

  # Create an empty dataframe with the specified columns
  complementaryMappingTable <- data.frame(
    concept_id = integer(),
    concept_id.new = integer(),
    concept_name.new = character(),
    stringsAsFactors = FALSE
  )

  for(min_separation_level in sort(max_minimal_separation_levels)){
  printCustomMessage(paste("Generating mapping for abstraction level",min_separation_level,'...', sep = " " ))

  concept_ids_to_map <- need_to_map_concept_ids %>% dplyr::filter(.data$maximum_minimal_separation == min_separation_level - maxAbstraction)

  mappings <- cdm$concept_ancestor %>% dplyr::filter(.data$min_levels_of_separation == min_separation_level - maxAbstraction) %>%
    dplyr::inner_join(concept_ids_to_map, by = "descendant_concept_id") %>% dplyr::select(concept_id = .data$descendant_concept_id,
                                                        concept_id.new = .data$ancestor_concept_id)

  abstractionLevelMappingTable = mappings %>% dplyr::left_join(cdm$concept %>% dplyr::filter(is.na(.data$invalid_reason)), by = c("concept_id.new" = "concept_id"), suffix = c("", ".concept")) %>%
    dplyr::select(concept_id = .data$concept_id,
           concept_id.new = .data$concept_id.new,
           concept_name.new = .data$concept_name) %>% as.data.frame()
  complementaryMappingTable <- rbind(complementaryMappingTable,abstractionLevelMappingTable)
  }

  complementaryMappingTable <- complementaryMappingTable %>% dplyr::select(CONCEPT_ID = .data$concept_id,
                                                                           CONCEPT_ID.new = .data$concept_id.new,
                                                                           CONCEPT_NAME.new = .data$concept_name.new)
  complementaryMappingTable <- complementaryMappingTable %>% dplyr::mutate(CONCEPT_NAME.new = dplyr::if_else(is.na(.data$CONCEPT_NAME.new), paste("Unnamed concept", .data$CONCEPT_ID.new), .data$CONCEPT_NAME.new))
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
