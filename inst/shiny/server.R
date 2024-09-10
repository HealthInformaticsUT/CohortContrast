if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 is required for this app but is not installed. Please install it.")
}

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("patchwork is required for this app but is not installed. Please install it.")
}
library(shiny)
library(DT)
library(pheatmap)
library(patchwork)
library(tidyverse)

################################################################################
#
# Server content
#
################################################################################

server = function(input, output, session) {
  # Reactive values
  study_info <- reactiveVal(list())
  studyName <- reactiveVal()
  target_mod <- reactiveVal()
  original_data <- reactiveVal(NULL)
  loaded_data <- reactiveVal(NULL)
  data_features <- reactiveVal(NULL)
  data_patients <- reactiveVal(NULL)
  complementaryMappingTable <- reactiveVal(data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), ABSTRACTION_LEVEL = integer(), stringsAsFactors = FALSE))

  # Waiters
  fullScreenWaiter <- waiter::Waiter$new(
    html = tagList(
      h4("Loading, please wait...", style = "color: white;"),
      waiter::spin_3()
    ),
    color = "rgba(0, 0, 0, 0.75)"  # Semi-transparent black background
  )
  snapshotWaiter <- waiter::Waiter$new(
    html = tagList(
      h4("Saving the snapshot, please wait...", style = "color: white;"),
      waiter::spin_3()
    ),
    color = "rgba(0, 0, 0, 0.75)"  # Semi-transparent black background
  )
  combiningWaiter <- waiter::Waiter$new(
    html = tagList(
      h4("Combining concepts, please wait...", style = "color: white;"),
      waiter::spin_3()
    ),
    color = "rgba(0, 0, 0, 0.75)"  # Semi-transparent black background
  )
  prevalencePlotWaiter <- waiter::Waiter$new(
    id = "prevalence",  # Targeting the prevalence plot
    html = tagList(
      div(style = "color: white; font-size: 18px; text-align: center;", "Loading Prevalence Plot..."),
      waiter::spin_3()
    )
  )

  heatmapPlotWaiter <- waiter::Waiter$new(
    id = "heatmap",  # Targeting the heatmap plot
    html = tagList(
      div(style = "color: white; font-size: 18px; text-align: center;", "Loading Heatmap Plot..."),
      waiter::spin_3()
    )
  )

  # Function to load data and update study_info
  load_study_data <- function() {
    names_and_rows <- list()
    CohortContrast:::printCustomMessage("EXECUTION: Loading studies from working directory ...")
    study_names <- get_study_names(pathToResults)
    for (study_name in study_names) {
      file_path <- str_c(pathToResults, "/", study_name, ".rds")

      if (file.exists(file_path)) {
        object <- readRDS(file_path)
        loaded_data(object)
        original_data(object)
        rows <- nrow(distinct(select(filter(object$data_initial, COHORT_DEFINITION_ID == 'target'), SUBJECT_ID)))
        names_and_rows[[study_name]] <- rows
      }
    }
    study_info(names_and_rows)
    CohortContrast:::printCustomMessage("COMPLETED: Loading studies from working directory")
  }
  # Initialize data on app start
  observe({
    fullScreenWaiter$show()  # Show the loading screen
    load_study_data()
  })

  # Update dropdown choices
  observe({
    choices <- sapply(names(study_info()), function(name) {
      paste(name, "(", study_info()[[name]], "patients)")
    })
    updateSelectInput(session, "studyName", choices = as.vector(choices))
  })

  # Load data based on selection
  observeEvent(input$studyName, {
    fullScreenWaiter$show()
    req(input$studyName)
    split_name <- unlist(strsplit(input$studyName, " ", fixed = TRUE))
    correct_study_name <- split_name[1]
    CohortContrast:::printCustomMessage(paste("EXECUTION: Loading study", correct_study_name, "from working directory ...", sep = " "))
    file_path <- str_c(pathToResults, "/", correct_study_name, ".rds")
    studyName(correct_study_name)
    if (file.exists(file_path)) {
      object <- readRDS(file_path)
      loaded_data(list(
        data_initial = object$data_initial,
        data_patients = object$data_patients,
        data_features = object$data_features,
        data_person = object$data_person,
        target_matrix = object$target_matrix,
        target_row_annotation = object$target_row_annotation,
        target_col_annotation = object$target_col_annotation
      ))
      original_data(list(
        data_initial = object$data_initial,
        data_patients = object$data_patients,
        data_features = object$data_features,
        data_person = object$data_person,
        target_matrix = object$target_matrix,
        target_row_annotation = object$target_row_annotation,
        target_col_annotation = object$target_col_annotation
      ))
      data_features(object$data_features)
      data_patients(object$data_patients)
      target_mod(object$data_features)
      if(!is.null(object$complementaryMappingTable)) complementaryMappingTable(object$complementaryMappingTable)
      CohortContrast:::printCustomMessage(paste("COMPLETED: Loading study", correct_study_name, "from working directory", sep = " "))
    } else {
      print("File not found")
      fullScreenWaiter$hide()  # Hide the loading screen
    }
  })

  # Reactive expressions
  target <- reactive({
    fullScreenWaiter$show()
    req(studyName(), pathToResults, loaded_data())

    autoScaleRate <- if (!is.null(input$scaleRate) && input$scaleRate) TRUE else FALSE
    applyInverseTarget <- if (!is.null(input$applyInverseTarget) && input$applyInverseTarget) TRUE else FALSE
    applyZTest <- if (!is.null(input$applyZTest) && input$applyZTest) TRUE else FALSE
    applyLogitTest <- if (!is.null(input$applyLogitTest) && input$applyLogitTest) TRUE else FALSE
    abstractionLevel <- if(!is.null(input$applyLogitTest)) input$abstraction_lvl else 0
    result <- format_results(
      object = loaded_data(),
      pathToResults = pathToResults,
      studyName = studyName(),
      autoScaleRate = autoScaleRate,
      applyInverseTarget = applyInverseTarget,
      applyZTest = applyZTest,
      applyLogitTest = applyLogitTest,
      abstractionLevel = abstractionLevel
    )
    # fullScreenWaiter$hide()
    result
  })

  target_filtered <- reactive({
    # fullScreenWaiter$show()
    result <- filter_target(
      target(),
      input$prevalence,
      input$prevalence_ratio,
      input$domain,
      removeUntreated = if (input$removeUntreated) TRUE else FALSE
    )
    fullScreenWaiter$hide()
    result
  })

  # Render plots
  output$prevalence <- renderPlot({
    prevalencePlotWaiter$show()
    result <- tryCatch({
      plot_prevalence(target_filtered())
    }, error = function(e) {
      print(e)
      plot_prevalence(NULL)
    })
    prevalencePlotWaiter$hide()
    result
  }, height = 950)

  output$heatmap <- renderPlot({
    heatmapPlotWaiter$show()
    result <- tryCatch({
      plot_heatmap(target_filtered())
    }, error = function(e) {
      print(e)
      plot_heatmap(NULL)
    })
    heatmapPlotWaiter$hide()
    result
  }, height = 950)

  # Mapping table related stuff
  observe({
    req(data_features())
    target_mod(data_features())
  })

  filtered_data <- reactive({
    # Filter the data based on the user's selected abstraction level
    req(input$abstraction_lvl)  # Ensure the input is available before proceeding
    target_mod() %>%
      dplyr::filter(.data$ABSTRACTION_LEVEL == input$abstraction_lvl)
  })

  output$concept_table <- DT::renderDT({
    datatable(
      filtered_data(),
      selection = 'multiple',
      filter = 'top'
    )
  }, server = TRUE)

  # Combine concepts
  observeEvent(input$accept_btn, {
    combiningWaiter$show()
    removeModal()
    new_concept_name <- input$new_concept_name
    combineSelectedConcepts(new_concept_name)
    target_mod(data_features())

    loaded_data(list(
      data_initial = loaded_data()$data_initial,
      data_patients = data_patients(),
      data_features = data_features(),
      data_person = loaded_data()$data_person,
      target_matrix = loaded_data()$target_matrix,
      target_row_annotation = loaded_data()$target_row_annotation,
      target_col_annotation = loaded_data()$target_col_annotation
    ))
    combiningWaiter$hide()
  })

  observeEvent(input$combine_btn, {
    if (length(input$concept_table_rows_selected) > 1) {
      showModal(modalDialog(
        title = "Combine Concepts",
        textInput("new_concept_name", "Enter New Concept Name:", ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("accept_btn", "Accept")
        )
      ))
    } else {
      showNotification("Select at least two rows to combine", type = "warning")
    }
  })

  # Reset data to original
  observeEvent(input$reset_btn, {
    initial_data <- list(
      data_initial = original_data()$data_initial,
      data_patients = original_data()$data_patients,
      data_features = original_data()$data_features,
      data_person = original_data()$data_person,
      target_matrix = original_data()$data_matrix,
      target_row_annotation = original_data()$target_row_annotation,
      target_col_annotation = original_data()$target_col_annotation
    )

    loaded_data(initial_data)
    data_features(original_data()$data_features)
    data_patients(original_data()$data_patients)
    target_mod(original_data()$data_features)

    data_features(data_features())
    complementaryMappingTable(if (!is.null(original_data()$complementaryMappingTable)) complementaryMappingTable(original_data()$complementaryMappingTable) else data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), ABSTRACTION_LEVE = integer(), stringsAsFactors = FALSE))
  })

  # Save data to file on button press
  observeEvent(input$save_btn, {
    snapshotWaiter$show()
    # Create the base file path
    file_base <- str_c(pathToResults, "/", studyName(), "_Snapshot")
    file_path <- str_c(file_base, ".rds")
    counter <- 1

    # Check if file exists and append increasing numbers if necessary
    while (file.exists(file_path)) {
      file_path <- str_c(file_base, "_", counter, ".rds")
      counter <- counter + 1
    }
    # Extract the data from the reactive
    object <- loaded_data()
    object$complementaryMappingTable <- complementaryMappingTable()

    # Save the actual data to the file
    saveRDS(object, file = file_path)
    snapshotWaiter$hide()
    # Notify the user
    showNotification(paste("Data saved to", file_path))
  })

  # Function to combine selected concepts
  combineSelectedConcepts <- function(new_concept_name) {
    selected_rows <- input$concept_table_rows_selected
    abstraction_level <- input$abstraction_lvl
    data_features <- data_features()
    data_patients <- data_patients()
    data_initial <- loaded_data()$data_initial
    complementary_mapping <- complementaryMappingTable()

    n_patients <- data_initial %>%
      group_by(COHORT_DEFINITION_ID) %>%
      summarise(count = n(), .groups = 'drop') %>%
      spread(COHORT_DEFINITION_ID, count, fill = 0)

    count_target <- n_patients$`target`
    count_control <- n_patients$`control`
    processed_table = target_mod() %>% dplyr::filter(ABSTRACTION_LEVEL == abstraction_level)
    selected_concept_ids <- as.numeric(processed_table$CONCEPT_ID[selected_rows])
    representingConceptId <- selected_concept_ids[which.max(as.numeric(processed_table$PREVALENCE_DIFFERENCE_RATIO[selected_rows]))]

    representingZTest <- any(processed_table$ZTEST[selected_rows])
    representingLogitTest <- any(processed_table$LOGITTEST[selected_rows])
    data_features <- data_features %>% dplyr::mutate(
      ZTEST = dplyr::if_else(CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstraction_level, representingZTest, ZTEST),
      LOGITTEST = dplyr::if_else(CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstraction_level, representingLogitTest, LOGITTEST)
    )

    # target_mod_data <- data_patients
    selected_heritage <- as.vector(data_patients %>% select(CONCEPT_ID, HERITAGE, ABSTRACTION_LEVEL) %>% distinct() %>% filter(CONCEPT_ID %in% selected_concept_ids, ABSTRACTION_LEVEL == abstraction_level) %>% select(HERITAGE))
    most_frequent_heritage <- names(sort(table(selected_heritage), decreasing = TRUE)[1])

    rows_to_update <- data_patients$CONCEPT_ID %in% selected_concept_ids &
      data_patients$ABSTRACTION_LEVEL == abstraction_level

    data_patients <- data_patients %>%
      mutate(
        CONCEPT_ID = replace(CONCEPT_ID, rows_to_update, representingConceptId),
        CONCEPT_NAME = replace(CONCEPT_NAME, rows_to_update, new_concept_name),
        HERITAGE = replace(HERITAGE, rows_to_update, most_frequent_heritage)
      ) %>%
      group_by(COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, CONCEPT_NAME, HERITAGE, ABSTRACTION_LEVEL) %>%
      summarise(PREVALENCE = sum(PREVALENCE), .groups = 'drop')

    data_features_temp <- data_features %>% dplyr::select(CONCEPT_ID, ABSTRACTION_LEVEL, ZTEST, LOGITTEST)
    data_features <- data_patients %>%
      group_by(CONCEPT_ID, CONCEPT_NAME, ABSTRACTION_LEVEL) %>%
      summarise(
        TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == 'target' & PREVALENCE > 0),
        CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == 'control' & PREVALENCE > 0),
        .groups = 'drop'
      ) %>%
      mutate(
        TARGET_SUBJECT_PREVALENCE = TARGET_SUBJECT_COUNT / count_target,
        CONTROL_SUBJECT_PREVALENCE = CONTROL_SUBJECT_COUNT / count_control,
        PREVALENCE_DIFFERENCE_RATIO = case_when(
          (is.na(TARGET_SUBJECT_PREVALENCE) | TARGET_SUBJECT_PREVALENCE == 0) ~ 0,
          (is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0) & is.na(TARGET_SUBJECT_PREVALENCE) ~ -1,
          (is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0) ~ 100,
          TRUE ~ TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
        )
      ) %>% left_join(data_features_temp, by = c("CONCEPT_ID", "ABSTRACTION_LEVEL"), keep = FALSE)

    data_features(data_features)
    data_patients(data_patients)

    # Update complementaryMappingTable
    new_rows <- data.frame(CONCEPT_ID = selected_concept_ids, CONCEPT_NAME = new_concept_name,ABSTRACTION_LEVEL = abstraction_level, stringsAsFactors = FALSE)
    complementary_mapping <- rbind(complementary_mapping, new_rows)

    # Update all related concept names in complementaryMappingTable
    related_concepts <- complementary_mapping$CONCEPT_NAME %in% complementary_mapping$CONCEPT_NAME[complementary_mapping$CONCEPT_ID %in% selected_concept_ids] &
      complementary_mapping$ABSTRACTION_LEVEL == abstraction_level
    complementary_mapping$CONCEPT_NAME[related_concepts] <- new_concept_name
    complementary_mapping <- complementary_mapping %>% distinct() # Ensure no duplicates

    complementaryMappingTable(complementary_mapping)

    loaded_data(list(
      data_initial = data_initial,
      data_patients = data_patients,
      data_features = data_features,
      data_person = loaded_data()$data_person,
      target_matrix = loaded_data()$target_matrix,
      target_row_annotation = loaded_data()$target_row_annotation,
      target_col_annotation = loaded_data()$target_col_annotation,
      complementaryMappingTable = complementary_mapping
    ))
  }
}
