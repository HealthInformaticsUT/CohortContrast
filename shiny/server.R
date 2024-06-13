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
  original_data = reactiveVal(NULL)
  loaded_data <- reactiveVal(NULL)
  data_features <- reactiveVal(NULL)
  data_patients <- reactiveVal(NULL)

  # Function to load data and update study_info
  load_study_data <- function() {
    names_and_rows <- list()
    study_names <- get_study_names(pathToResults)
   for (study_name in study_names) {
      file_path <- str_c(pathToResults, "/tmp/datasets/", study_name, "_CC_medData.rdata")

      if (file.exists(file_path)) {
        load(file_path)
       loaded_data(object)
       original_data(object)
        rows <- nrow(distinct(select(filter(object$data_patients, COHORT_DEFINITION_ID == 2), PERSON_ID)))
        names_and_rows[[study_name]] <- rows
      }
    }
    study_info(names_and_rows)
  }

  # Initialize data on app start
  observe({
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
    req(input$studyName)
    split_name <- unlist(strsplit(input$studyName, " ", fixed = TRUE))
    correct_study_name <- split_name[1]
    file_path <- str_c(pathToResults, "/tmp/datasets/", correct_study_name, "_CC_medData.rdata")
    studyName(correct_study_name)

    if (file.exists(file_path)) {
      load(file_path)
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
      print("Data loaded")
    } else {
      print("File not found")
    }
  })

  # Reactive expressions
  target <- reactive({
    req(studyName(), pathToResults, loaded_data())

    autoscaleRate <- if (!is.null(input$scaleRate) && input$scaleRate) TRUE else FALSE
    applyInverseTarget <- if (!is.null(input$applyInverseTarget) && input$applyInverseTarget) TRUE else FALSE
    applyZTest = if (!is.null(input$applyZTest) && input$applyZTest) TRUE else FALSE
    applyLogitTest = if (!is.null(input$applyLogitTest) && input$applyLogitTest) TRUE else FALSE
    format_results(
      object = loaded_data(),
      pathToResults = pathToResults,
      studyName = studyName(),
      autoScaleRate = autoScaleRate,
      applyInverseTarget = applyInverseTarget,
      applyZTest = applyZTest,
      applyLogitTest = applyLogitTest
    )
  })

  target_filtered <- reactive({
    filter_target(
      target(),
      input$prevalence,
      input$prevalence_ratio,
      input$domain,
      removeUntreated = if (input$removeUntreated) TRUE else FALSE
    )
  })

  # Render plots
  output$prevalence <- renderPlot({
    tryCatch({
      plot_prevalence(target_filtered())
    }, error = function(e) {
      print(e)
      plot_prevalence(NULL)
    })
  }, height = 950)

  output$heatmap <- renderPlot({
    tryCatch({
      plot_heatmap(target_filtered())
    }, error = function(e) {
      print(e)
      plot_heatmap(NULL)
    })
  }, height = 950)

  # Mapping table related stuff
  observe({
    req(data_features())
    target_mod(data_features())
  })

  output$concept_table <- DT::renderDT({
    datatable(
      target_mod(),
      selection = 'multiple',
      filter = 'top'
    )
  }, server = TRUE)

  # Combine concepts
  observeEvent(input$accept_btn, {
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
  })

  # Function to combine selected concepts
  combineSelectedConcepts <- function(new_concept_name) {

    selected_rows <- input$concept_table_rows_selected

    data_features <- data_features()

    data_patients <- data_patients()

    data_initial <- loaded_data()$data_initial

    n_patients <- data_initial %>%
      group_by(COHORT_DEFINITION_ID) %>%
      summarise(count = n(), .groups = 'drop') %>%
      spread(COHORT_DEFINITION_ID, count, fill = 0)

    count_target <- n_patients$`2`
    count_control <- n_patients$`1`
    selected_concept_ids <- as.numeric(target_mod()$CONCEPT_ID[selected_rows])

    representingConceptId <-selected_concept_ids[which.max(as.numeric(target_mod()$PREVALENCE_DIFFERENCE_RATIO[selected_rows]))]

    # TODO: maybe run tests again instead of making assuption for all?
    representingZTest <- any(target_mod()$ZTEST[selected_rows])
    representingLogitTest <- any(target_mod()$LOGITTEST[selected_rows])

    data_features <-
      data_features %>% dplyr::mutate(
        ZTEST = dplyr::if_else(CONCEPT_ID == representingConceptId, representingZTest, ZTEST),
        LOGITTEST = dplyr::if_else(
          CONCEPT_ID == representingConceptId,
          representingLogitTest,
          LOGITTEST
        )
      )

    target_mod_data <- data_patients
    selected_heritage <- as.vector(data_patients %>% select(CONCEPT_ID, HERITAGE) %>% distinct() %>%  filter(CONCEPT_ID %in% selected_concept_ids) %>% select(HERITAGE))
    # Count occurrences of each heritage value and select the most frequent one
    most_frequent_heritage <- names(sort(table(selected_heritage), decreasing = TRUE)[1])

   # Identify rows to update
    rows_to_update <- data_patients$CONCEPT_ID %in% selected_concept_ids

    # Update rows in one go
    data_patients <- data_patients %>%
      mutate(
        CONCEPT_ID = replace(CONCEPT_ID, rows_to_update, representingConceptId),
        CONCEPT_NAME = replace(CONCEPT_NAME, rows_to_update, new_concept_name),
        HERITAGE = replace(HERITAGE, rows_to_update, most_frequent_heritage)
      ) %>%
      group_by(COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, CONCEPT_NAME, HERITAGE) %>%
      summarise(PREVALENCE = sum(PREVALENCE), .groups = 'drop')

    data_features_temp = data_features %>% dplyr::select(CONCEPT_ID, ZTEST, LOGITTEST)
    data_features <- data_patients %>%
      group_by(CONCEPT_ID, CONCEPT_NAME) %>%
      summarise(
        TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == 2 & PREVALENCE > 0),
        CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == 1 & PREVALENCE > 0),
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
      ) %>% left_join(data_features_temp, by = "CONCEPT_ID", keep = FALSE)

    data_features(data_features)
    data_patients(data_patients)

      loaded_data(list(
      data_initial = data_initial,
      data_patients = data_patients,
      data_features = data_features,
      data_person = loaded_data()$data_person,
      target_matrix = loaded_data()$target_matrix,
      target_row_annotation = loaded_data()$target_row_annotation,
      target_col_annotation = loaded_data()$target_col_annotation
    ))
  }

}
