#' @keywords internal
getDemographyYearOfBirthPlot <- function(plot_data = NULL) {


  # Prepare demographic data for both cohorts (target and control)
  demographic_data <- plot_data %>%
    dplyr::group_by(COHORT_DEFINITION_ID, YEAR_OF_BIRTH, GENDER_CONCEPT_ID) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(relative_size = count / sum(count))  # Normalize by total count

  # Separate Target & Control Groups
  target_data <- demographic_data %>% dplyr::filter(COHORT_DEFINITION_ID == "target")
  control_data <- demographic_data %>% dplyr::filter(COHORT_DEFINITION_ID == "control")

  # Process Male and Female separately for both groups
  process_gender_data <- function(data, gender_id, group_name) {
    data %>%
      dplyr::filter(GENDER_CONCEPT_ID == gender_id) %>%
      dplyr::select(YEAR_OF_BIRTH, relative_size) %>%
      dplyr::arrange(YEAR_OF_BIRTH) %>%
      dplyr::mutate(Group = group_name, Gender = ifelse(gender_id == 8532, "Female", "Male"))  # Add gender column
  }

  # Target group
  female_target <- process_gender_data(target_data, 8532, "Target")
  male_target <- process_gender_data(target_data, 8507, "Target") %>%
    dplyr::mutate(relative_size = -relative_size)  # Flip for visualization

  # Control group (lighter colors)
  female_control <- process_gender_data(control_data, 8532, "Control")
  male_control <- process_gender_data(control_data, 8507, "Control") %>%
    dplyr::mutate(relative_size = -relative_size)

  # Combine all gender-group data
  combined_data <- dplyr::bind_rows(female_target, male_target, female_control, male_control)

  # Define colors for both genders in both cohorts
  color_palette <- c(
    "Target_Female" = "purple", "Control_Female" = "orchid1",  # Female colors
    "Target_Male" = "darkgreen", "Control_Male" = "lightgreen"  # Male colors
  )

  # Create the plot
  plot <- ggplot2::ggplot() +
    # Female Data (above x-axis)
    ggplot2::geom_ribbon(data = combined_data %>% dplyr::filter(Gender == "Female"),
                         ggplot2::aes(x = YEAR_OF_BIRTH, ymin = 0, ymax = relative_size, fill = interaction(Group, Gender, sep = "_")), alpha = 0.5) +
    # Male Data (below x-axis)
    ggplot2::geom_ribbon(data = combined_data %>% dplyr::filter(Gender == "Male"),
                         ggplot2::aes(x = YEAR_OF_BIRTH, ymin = relative_size, ymax = 0, fill = interaction(Group, Gender, sep = "_")), alpha = 0.5) +
    # Correct color mapping
    ggplot2::scale_fill_manual(values = color_palette, name = "Cohort") +

    # Labels and Titles
    ggplot2::labs(
      title = "Relative Population Distribution by Gender and Year of Birth",
      subtitle = "Target vs Control cohort",
      x = "Year of Birth",
      y = "Relative Size"
    ) +
    # Annotate Labels for Male & Female
    ggplot2::annotate("text", x = quantile(demographic_data$YEAR_OF_BIRTH, probs = 0.8), y = 0.012,
                      label = "Female", color = "purple", size = 5, fontface = "bold") +
    ggplot2::annotate("text", x = quantile(demographic_data$YEAR_OF_BIRTH, probs = 0.8), y = -0.012,
                      label = "Male", color = "darkgreen", size = 5, fontface = "bold") +
    # Custom Theme for Bigger Axis Labels
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),  # X-axis tick numbers
      axis.text.y = ggplot2::element_text(size = 14),  # Y-axis tick numbers
      axis.title.x = ggplot2::element_text(size = 16),  # X-axis label
      axis.title.y = ggplot2::element_text(size = 16),  # Y-axis label
      plot.title = ggplot2::element_text(size = 18, face = "bold"),  # Bigger title
      plot.subtitle = ggplot2::element_text(size = 16)
    )
return(plot)
}

#' @keywords internal
getDemographyAgeAtIndexPlot <- function(plot_data = NULL,
                                        title = "Relative Population Distribution by Gender and Age at Index"
                                        ) {

  upperPlotGroupId <- "Target"
  lowerPlotGroupId <- "Control"
  # TODO logic
  groups = plot_data$Group %>% unique()
  if ("Group1" %in% groups) {upperPlotGroupId <- "Group1"}
  if ("Group2" %in% groups) {lowerPlotGroupId <- "Group2"}
  if (!("Control" %in% groups) && !("Group2" %in% groups)) {lowerPlotGroupId <- "Target"}
  subtitle = paste(upperPlotGroupId, "vs", lowerPlotGroupId)

  # Prepare demographic data for both cohorts (target and control)
  demographic_data <- plot_data %>%
    dplyr::group_by(Group, AGE_AT_INDEX, GENDER_CONCEPT_ID) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(relative_size = count / sum(count))  # Normalize by total count

  # Separate Target & Control Groups
  target_data <- demographic_data %>% dplyr::filter(Group == upperPlotGroupId)
  control_data <- demographic_data %>% dplyr::filter(Group == lowerPlotGroupId)

  # Process Male and Female separately for both groups
  process_gender_data <- function(data, gender_id, group_name) {
    data %>%
      dplyr::filter(GENDER_CONCEPT_ID == gender_id) %>%
      dplyr::select(AGE_AT_INDEX, relative_size) %>%
      dplyr::arrange(AGE_AT_INDEX) %>%
      dplyr::mutate(Group = group_name, Gender = ifelse(gender_id == 8532, "Female", "Male"))  # Add gender column
  }
  # Target group
  female_target <- process_gender_data(target_data, 8532, upperPlotGroupId)
  male_target <- process_gender_data(target_data, 8507, upperPlotGroupId) %>%
    dplyr::mutate(relative_size = -relative_size)  # Flip for visualization

  # Control group (lighter colors)
  female_control <- process_gender_data(control_data, 8532, lowerPlotGroupId)
  male_control <- process_gender_data(control_data, 8507, lowerPlotGroupId) %>%
    dplyr::mutate(relative_size = -relative_size)

  # Combine all gender-group data
  combined_data <- dplyr::bind_rows(female_target, male_target, female_control, male_control)

  # Define colors for both genders in both cohorts
  color_palette <- c("purple", "orchid1", "darkgreen", "lightgreen")

  names(color_palette) <- c(
    interaction(upperPlotGroupId, "Female", sep = "_"),
    interaction(lowerPlotGroupId, "Female", sep = "_"),
    interaction(upperPlotGroupId, "Male", sep = "_"),
    interaction(lowerPlotGroupId, "Male", sep = "_")
  )

  # Create the plot
  plot <- ggplot2::ggplot() +
    # Female Data (above x-axis)
    ggplot2::geom_ribbon(data = combined_data %>% dplyr::filter(Gender == "Female"),
                         ggplot2::aes(x = AGE_AT_INDEX, ymin = 0, ymax = relative_size, fill = interaction(Group, Gender, sep = "_")), alpha = 0.5) +
    # Male Data (below x-axis)
    ggplot2::geom_ribbon(data = combined_data %>% dplyr::filter(Gender == "Male"),
                         ggplot2::aes(x = AGE_AT_INDEX, ymin = relative_size, ymax = 0, fill = interaction(Group, Gender, sep = "_")), alpha = 0.5) +
    # Correct color mapping
    ggplot2::scale_fill_manual(values = color_palette, name = "Cohort") +

    # Labels and Titles
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Age",
      y = "Relative Size"
    ) +
    # Annotate Labels for Male & Female
    ggplot2::annotate("text", x = quantile(demographic_data$AGE_AT_INDEX, probs = 0.2), y = 0.012,
                      label = "Female", color = "purple", size = 5, fontface = "bold") +
    ggplot2::annotate("text", x = quantile(demographic_data$AGE_AT_INDEX, probs = 0.2), y = -0.012,
                      label = "Male", color = "darkgreen", size = 5, fontface = "bold") +
    # Custom Theme for Bigger Axis Labels
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14),  # X-axis tick numbers
      axis.text.y = ggplot2::element_text(size = 14),  # Y-axis tick numbers
      axis.title.x = ggplot2::element_text(size = 16),  # X-axis label
      axis.title.y = ggplot2::element_text(size = 16),  # Y-axis label
      plot.title = ggplot2::element_text(size = 18, face = "bold"),  # Bigger title
      plot.subtitle = ggplot2::element_text(size = 16)
    )

  return(plot)
}


#' @keywords internal
getDemographyPlotData <- function(data_patients = NULL, data_person = NULL, data_initial = NULL, groupOneIds = NULL, groupTwoIds = NULL) {
  if(is.null(data_patients) ||is.null(data_person) ||is.null(data_initial)) {
    return(NULL)
  }
  target_person = NULL
  control_person = NULL

  if(!is.null(groupOneIds) && length(groupOneIds) > 0){
    target <- data_patients %>% dplyr::filter(COHORT_DEFINITION_ID == "target", PERSON_ID %in% groupOneIds) %>% dplyr::pull(PERSON_ID) %>% unique()
    target_person <- data_person %>% dplyr::filter(PERSON_ID %in% target)
    target_person$Group <- "Group1"
  } else {
    # Filter target group
    target <- data_patients %>% dplyr::filter(COHORT_DEFINITION_ID == "target") %>% dplyr::pull(PERSON_ID) %>% unique()
    target_person <- data_person %>% dplyr::filter(PERSON_ID %in% target)
    target_person$Group <- "Target"
  }
  if (!is.null(groupTwoIds) && length(groupTwoIds) > 0) {
    # If comparing we always compare to target
    control <- data_patients %>% dplyr::filter(COHORT_DEFINITION_ID == "target", PERSON_ID %in% groupTwoIds) %>% dplyr::pull(PERSON_ID) %>% unique()
    control_person <- data_person %>% dplyr::filter(PERSON_ID %in% control)
    control_person$Group <- "Group2"
  } else {
    if(is.null(groupOneIds)) {
    control <- data_patients %>% dplyr::filter(COHORT_DEFINITION_ID == "control") %>% dplyr::pull(PERSON_ID) %>% unique()
    control_person <- data_person %>% dplyr::filter(PERSON_ID %in% control)
    control_person$Group <- "Control"
    } else {
      # If a group one is selected we compare to target
      control <- data_patients %>% dplyr::filter(COHORT_DEFINITION_ID == "target") %>% dplyr::pull(PERSON_ID) %>% unique()
      control_person <- data_person %>% dplyr::filter(PERSON_ID %in% control)
      control_person$Group <- "Target"
    }
  }
  # Combine datasets
  combined_data <- dplyr::bind_rows(target_person, control_person)

  # Merge with initial dataset
  combined_data <- data_initial %>% dplyr::left_join(combined_data, by = c("SUBJECT_ID" = "PERSON_ID")) %>%
    dplyr::filter(!is.na(YEAR_OF_BIRTH), !is.na(Group))


  # Calculate Age at Index
  combined_data <- combined_data %>%
    dplyr::mutate(
      BIRTH_DATE = as.Date(paste0(YEAR_OF_BIRTH, "-07-01")),
      AGE_AT_INDEX = round(as.numeric(difftime(COHORT_START_DATE, BIRTH_DATE, units = "days")) / 365.25,0)
    )
  return(combined_data)
}
