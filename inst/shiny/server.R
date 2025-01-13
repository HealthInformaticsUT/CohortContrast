if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 is required for this app but is not installed. Please install it.")
}

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("patchwork is required for this app but is not installed. Please install it.")
}

################################################################################
#
# Server content
#
################################################################################

server <- function(input, output, session) {
  # Reactive values
  study_info <- shiny::reactiveVal(NULL)
  studyName <- shiny::reactiveVal()
  target_mod <- shiny::reactiveVal()
  original_data <- shiny::reactiveVal(NULL)
  loaded_data <- shiny::reactiveVal(NULL)
  data_features <- shiny::reactiveVal(NULL)
  data_patients <- shiny::reactiveVal(NULL)
  data_initial <- shiny::reactiveVal(NULL)
  correlation_groups <- shiny::reactiveVal(NULL)
  selected_correlation_group <- shiny::reactiveVal(NULL)
  selected_correlation_group_labels <- shiny::reactiveVal(list(labels = character(0), labelsTrimmed = character(0)))
  target_row_annotation <- shiny::reactiveVal(NULL)
  target_col_annotation <- shiny::reactiveVal(NULL)
  target_time_annotation <- shiny::reactiveVal(NULL)
  target_matrix <- shiny::reactiveVal(NULL)
  selected_filters <- shiny::reactiveVal(list())
  selected_patient_filters <- shiny::reactiveVal(list())
  complementaryMappingTable <- shiny::reactiveVal(data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), NEW_CONCEPT_ID = integer(), NEW_CONCEPT_NAME = character() , ABSTRACTION_LEVEL = integer(), stringsAsFactors = FALSE))
  removeButtonCounter <- shiny::reactiveVal(0)
  removeButtonCounterMap <- shiny::reactiveValues()
  filterButtonCounter <- shiny::reactiveVal(0)
  filterButtonCounterMap <- shiny::reactiveValues()
  # Reactive values to store the last state of input counters
  last_add_filter_value <- reactiveVal(0)
  last_disregard_filter_value <- reactiveVal(0)
  lookback_days <- reactiveVal(NULL)


# Plots' height coefficient
  nrOfConcepts <- shiny::reactive({
    if (is.data.frame(target_row_annotation())) {
      nrow(target_row_annotation())
    } else {
      0  # Return 0 if data_features() is not yet a dataframe
    }
  })

  # Waiters
  fullScreenWaiter <- waiter::Waiter$new(
    html = htmltools::tagList(
      h4("Loading, please wait...", style = "color: white;"),
      waiter::spin_3()
    ),
    color = "rgba(0, 0, 0, 0.75)" # Semi-transparent black background
  )
  snapshotWaiter <- waiter::Waiter$new(
    html = htmltools::tagList(
      h4("Saving the snapshot, please wait...", style = "color: white;"),
      waiter::spin_3()
    ),
    color = "rgba(0, 0, 0, 0.75)" # Semi-transparent black background
  )
  combiningWaiter <- waiter::Waiter$new(
    html = htmltools::tagList(
      h4("Combining concepts, please wait...", style = "color: white;"),
      waiter::spin_3()
    ),
    color = "rgba(0, 0, 0, 0.75)" # Semi-transparent black background
  )
  prevalencePlotWaiter <- waiter::Waiter$new(
    id = "prevalencePlot", # Targeting the prevalence plot
    html = htmltools::tagList(
      htmltools::div(style = "color: white; font-size: 18px; text-align: center;", "Loading Prevalence Plot..."),
      waiter::spin_3()
    )
  )

  heatmapPlotWaiter <- waiter::Waiter$new(
    id = "heatmapPlot", # Targeting the heatmap plot
    html = htmltools::tagList(
      htmltools::div(style = "color: white; font-size: 18px; text-align: center;", "Loading Heatmap Plot..."),
      waiter::spin_3()
    )
  )

  correlationHeatmapPlotWaiter <- waiter::Waiter$new(
    id = "correlationHeatmapPlot", # Targeting the correlation heatmap plot
    html = htmltools::tagList(
      htmltools::div(style = "color: white; font-size: 18px; text-align: center;", "Loading Correlation Heatmap Plot..."),
      waiter::spin_3()
    )
  )

  timePanelWaiter <- waiter::Waiter$new(
    id = "time_panelPlot", # Targeting the time plot
    html = htmltools::tagList(
      htmltools::div(style = "color: white; font-size: 18px; text-align: center;", "Loading Time Panel..."),
      waiter::spin_3()
    )
  )

  # Function to load data and update study_info
  load_study_data <- function() {
    fullScreenWaiter$show()
    study_df_temp = data.frame("Study" = character(),
                               "Target patients" = numeric(),
                               "Control patients" = numeric(),
                               "ZTEST" = numeric())
    names(study_df_temp) <- c("Study", "Target patients", "Control patients", "Significant differences (Z-Test)")
    CohortContrast:::printCustomMessage("EXECUTION: Loading studies from working directory ...")
    study_names <- CohortContrast:::get_study_names(pathToResults)
    for (study_name in study_names) {
      file_path <- stringr::str_c(pathToResults, "/", study_name, ".rds")

      if (file.exists(file_path)) {
        # Mutate file
        file_path <- sub("\\.rds$", ".csv", file_path)
        if (file.exists(file_path)) {
        metadata <- utils::read.csv(file_path)
        temp = data.frame("Study" = metadata$study,
                   "Target patients" = metadata$target_patients,
                   "Control patients" = metadata$control_patients,
                   "ZTEST" = metadata$z_count)
        names(temp) <- c("Study", "Target patients", "Control patients", "Significant differences (Z-Test)")
        study_df_temp = rbind(study_df_temp, temp)
      }
      }
    }

    # Use isolate to prevent reactivity triggers
    shiny::isolate({
      study_info(study_df_temp)
    })
    fullScreenWaiter$hide()
    CohortContrast:::printCustomMessage("COMPLETED: Loading studies from working directory")
  }

  # Initialize data on app start (load once)
  shiny::observe({
    load_study_data()
  })

  study_df <- reactive({
    study_info()
  })

  output$study_table <- DT::renderDataTable({
    # Create a datatable
    DT::datatable(
      study_df(),
      selection = "single",  # Allow selecting a single row
      options = list(
        paging = TRUE,
        searching = TRUE,
        lengthChange = FALSE,
        pageLength = 10
      )
    )
  })

  # Observe the row selection and update the hidden text input
  observeEvent(input$study_table_rows_selected, {
    selected_row <- input$study_table_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      # Get the study name from the reactive data frame based on the selected row index
      study_name <- study_df()$Study[selected_row]
      updateTextInput(session, "studyName", value = study_name)
    }
  })

  # Load data based on selection
  shiny::observeEvent(input$studyName, {
    shiny::req(input$studyName)
    fullScreenWaiter$show()
    #split_name <- input$studyName
      #unlist(strsplit(input$studyName, " ", fixed = TRUE))
    correct_study_name <- input$studyName# split_name[1]
    CohortContrast:::printCustomMessage(paste("EXECUTION: Loading study", correct_study_name, "from working directory ...", sep = " "))
    file_path <- stringr::str_c(pathToResults, "/", correct_study_name, ".rds")
    studyName(correct_study_name)
    if (file.exists(file_path)) {
      object <- readRDS(file_path)

      # Ensure that all relevant components are converted to data.table
      object$data_initial <- data.table::as.data.table(object$data_initial)
      object$data_patients <- data.table::as.data.table(object$data_patients)
      object$data_features <- data.table::as.data.table(object$data_features)
      object$data_person <- data.table::as.data.table(object$data_person)
      object$target_matrix <- data.table::as.data.table(object$target_matrix)
      object$target_row_annotation <- data.table::as.data.table(object$target_row_annotation)
      object$target_col_annotation <- data.table::as.data.table(object$target_col_annotation)

      # Forward the converted data.tables
      shiny::isolate({
        loaded_data(list(
          data_initial = object$data_initial,
          data_patients = object$data_patients,
          data_features = object$data_features,
          data_person = object$data_person,
          target_matrix = object$target_matrix,
          target_row_annotation = object$target_row_annotation,
          target_col_annotation = object$target_col_annotation,
          config = object$config
        ))

        original_data(list(
          data_initial = object$data_initial,
          data_patients = object$data_patients,
          data_features = object$data_features,
          data_person = object$data_person,
          target_matrix = object$target_matrix,
          target_row_annotation = object$target_row_annotation,
          target_col_annotation = object$target_col_annotation,
          config = object$config
        ))

        # Update reactive values with the converted data.tables
        data_features(object$data_features)
        data_patients(object$data_patients)
        data_initial(object$data_initial)
        target_mod(object$data_features)

        lookback_days(loaded_data()$config$lookbackDays)
      })
      fullScreenWaiter$hide()
      if (!is.null(object$complementaryMappingTable)) complementaryMappingTable(object$complementaryMappingTable)
      CohortContrast:::printCustomMessage(paste("COMPLETED: Loading study", correct_study_name, "from working directory", sep = " "))
    } else {
      print("File not found")
    }
  })

  # Reactive expressions
  target <- shiny::reactive({
    shiny::req(studyName(), pathToResults, loaded_data())
    fullScreenWaiter$show()
    autoScaleRate <- if (!is.null(input$scaleRate) && input$scaleRate) TRUE else FALSE
    applyInverseTarget <- if (!is.null(input$applyInverseTarget) && input$applyInverseTarget) TRUE else FALSE
    applyZTest <- if (!is.null(input$applyZTest) && input$applyZTest) TRUE else FALSE
    applyLogitTest <- if (!is.null(input$applyLogitTest) && input$applyLogitTest) TRUE else FALSE
    parsed_abstraction_value <- ifelse(input$abstraction_lvl == "original", -1, ifelse(input$abstraction_lvl == "source", -2, as.numeric(input$abstraction_lvl)))
    abstractionLevel <- if (!is.null(parsed_abstraction_value)) parsed_abstraction_value else 0
    result <- format_results(
      data = loaded_data(),
      autoScaleRate = autoScaleRate,
      applyInverseTarget = applyInverseTarget,
      applyZTest = applyZTest,
      applyLogitTest = applyLogitTest,
      abstractionLevel = abstractionLevel
    )
    fullScreenWaiter$hide()
    result
  })

  target_filtered <- shiny::reactive({
    # fullScreenWaiter$show()
    correlation_cutoff <- if (!is.null(input$correlation_threshold)) input$correlation_threshold else 0.95
    result <- filter_target(
      target(),
      input$prevalence,
      input$prevalence_ratio,
      input$domain,
      removeUntreated = if (input$removeUntreated) TRUE else FALSE
    )
    result <- prepare_filtered_target(result, correlation_cutoff)
    target_row_annotation(result$target_row_annotation)
    target_col_annotation(result$target_col_annotation)
    target_time_annotation(result$target_time_annotation)
    target_matrix(result$target_matrix)
    correlation_groups(result$correlation_analysis$groups)
    # fullScreenWaiter$hide()
    result
  })


  output$lookBackSlider <- renderUI({
    req(lookback_days()) # Ensure lookback_days is available
    if (is.numeric(lookback_days())) {
      sliderInput(
        inputId = "lookback_slider",
        h3("Lookback days"),
        min = -lookback_days(),
        max = 0,
        value = 0,
        step = ceiling(lookback_days() / 10)
      )
    } else {
      NULL
    }
  })

  shiny::observe({
    if (!is.null(input$lookback_slider)) {
      # Use isolate to prevent unnecessary reactivity
      threshold <- abs(isolate(input$lookback_slider))
      # Filter data based on the slider value
      updated_data <- original_data()$data_patients %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          TIME_TO_EVENT = list(TIME_TO_EVENT[TIME_TO_EVENT >= lookback_days() - threshold])
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(lengths(TIME_TO_EVENT) > 0) # Remove rows where TIME_TO_EVENT is empty

      # Update the reactive value without triggering unnecessary reactivity
      isolate({
        data_patients(data.table::as.data.table(updated_data))
      })
    }
  })


  # Render plots
  output$prevalencePlot <- shiny::renderPlot(
    {
      prevalencePlotWaiter$show()
      result <- tryCatch(
        {
          plot_prevalence(target_filtered(), isCorrelationView = input$correlationView)
        },
        error = function(e) {
          print(e)
          plot_prevalence(NULL, isCorrelationView = FALSE)
        }
      )
      prevalencePlotWaiter$hide()
      result
    },
    height = function() min(160 + nrOfConcepts() * 24, 10000)
    )

  output$heatmapPlot <- shiny::renderPlot(
    {
      heatmapPlotWaiter$show()
      result <- NULL
      if (isFALSE(input$correlationView)) {
      result <- tryCatch(
        {
          plot_heatmap(target_filtered())
        },
        error = function(e) {
          print(e)
          plot_heatmap(NULL)
        }
      )
      }
      else {
        result <- tryCatch(
          {
            plot_correlation_heatmap(target_filtered())
          },
          error = function(e) {
            print(e)
            plot_correlation_heatmap(NULL)
          }
        )
      }
      heatmapPlotWaiter$hide()
      result
    },
    height = function() min(160 + nrOfConcepts() * 24, 10000)
    )

  output$time_panelPlot <- shiny::renderPlot(
    {
      timePanelWaiter$show()
      result <- tryCatch(
        {
          plot_time(target_filtered(), isCorrelationView = input$correlationView)
        },
        error = function(e) {
          print(e)
          plot_time(NULL)
        }
      )
      timePanelWaiter$hide()
      result
    },
    height = function() min(160 + nrOfConcepts() * 24, 10000)
  )

  output$correlationHeatmapPlot <- shiny::renderPlot(
    {
      correlationHeatmapPlotWaiter$show()
      result <- tryCatch(
        {
          plot_correlation_heatmap(target_filtered())
        },
        error = function(e) {
          print(e)
          plot_correlation_heatmap(NULL)
        }
      )
      correlationHeatmapPlotWaiter$hide()
      result
    },
    height = function() min(160 + nrOfConcepts() * 24, 10000)
  )

  # # Server
  output$trajectoryGraph <- visNetwork::renderVisNetwork({
    # Ensure required inputs are available
    req(target_filtered(), selected_correlation_group(), input$edge_prevalence_threshold)
    # Call the function to create the graph
    plot_correlation_trajectory_graph(
      target_filtered(),
      selected_correlation_group(),
      input$edge_prevalence_threshold,
      selected_trajectory_filtering_values(),
      input$correlationView
    )
  })

  output$trajectoryGraphPlot <-  shiny::renderPlot({
    # Ensure required inputs are available
    # Call the function to create the graph
    plot_correlation_trajectory_graph(
      target_filtered(),
      selected_correlation_group(),
      input$edge_prevalence_threshold,
      selected_trajectory_filtering_values(),
      input$correlationView
    )
  },
  height = 160)

  output$dynamicGraphUI <- renderUI({
    if (isFALSE(input$correlationView) | is.null(selected_correlation_group()) | length(selected_correlation_group()) == 0) {
      # Use ggplot2 output
      plotOutput("trajectoryGraphPlot")
    } else {
      # Use visNetwork output
      visNetwork::visNetworkOutput("trajectoryGraph")
    }
  })

  shiny::observeEvent(input$visual_snapshot, {
      shiny::showModal(shiny::modalDialog(
        title = "Save snapshot",
        textInput("new_visual_snapshot_name", "Enter New Snapshot Name:", ""),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("accept_btn_visual_snaphot", "Accept")
        )
      ))
  })

  # Combine concepts
  shiny::observeEvent(input$accept_btn_visual_snaphot, {
    combiningWaiter$show()
    shiny::removeModal()
    CohortContrast:::createPathToResults(paste0(pathToResults, "/visual_snapshots"))
    fileName <- paste(pathToResults, "/visual_snapshots/",CohortContrast:::sanitize_single(input$new_visual_snapshot_name),".rds", sep = "")

    config <- loaded_data()$config
    config$prevalenceCutOff <- input$prevalence_ratio
    config$presenceFilter <- input$prevalence
    config$domainsIncluded <- input$domain

    temp_filtered_target = list(target_matrix = target_matrix(),
                                target_row_annotation = target_row_annotation(),
                                target_col_annotation = target_col_annotation(),
                                target_time_annotation = target_time_annotation()
    )
    temp_filtered_target = prepare_filtered_target(temp_filtered_target, input$correlation_threshold)
    parsed_abstraction_value <- ifelse(input$abstraction_lvl == "original", -1, ifelse(input$abstraction_lvl == "source", -2, as.numeric(input$abstraction_lvl)))
    ccObject <- list(
      data_features = data_features() %>% dplyr::filter(.data$CONCEPT_ID %in% as.integer(rownames(target_matrix())),
                                                        .data$ABSTRACTION_LEVEL == parsed_abstraction_value),
      filtered_target = temp_filtered_target,
      config = config
    )
    CohortContrast:::save_object(object = ccObject, path = fileName)
    combiningWaiter$hide()
  })

  # Mapping table related stuff
  shiny::observe({
    shiny::req(data_features())
    target_mod(data_features())
  })

  filtered_data <- shiny::reactive({
    # Filter the data based on the user's selected abstraction level
    shiny::req(input$abstraction_lvl) # Ensure the input is available before proceeding
    shiny::req(input$studyName)
    parsed_abstraction_value <- ifelse(input$abstraction_lvl == "original", -1, ifelse(input$abstraction_lvl == "source", -2, as.numeric(input$abstraction_lvl)))

    filtered_data <- target_mod()[
      ABSTRACTION_LEVEL == parsed_abstraction_value
    ]
  })

  output$concept_table <- DT::renderDT({
    shiny::req(input$studyName)
    DT::datatable(
      filtered_data(),
      selection = 'multiple',
      filter = 'top',
      options = list(
        columnDefs = list(
          list(targets = which(names(filtered_data()) %in% c("TIME_TO_EVENT", "ABSTRACTION_LEVEL")), visible = FALSE)
        )
      )
    )
  }, server = TRUE)

  output$mapping_history_table <- DT::renderDT({
    shiny::req(input$studyName)
    DT::datatable(
      complementaryMappingTable(),
      selection = 'multiple',
      filter = 'top',
      # options = list(
      #   columnDefs = list(
      #     list(targets = which(names(filtered_data()) %in% c("TIME_TO_EVENT", "ABSTRACTION_LEVEL")), visible = FALSE)
      #   )
      # )
    )
  }, server = TRUE)

  # Create a proxy for the DT
  dt_proxy <- DT::dataTableProxy("concept_table")

  # Observe changes in the checkbox
  observeEvent(input$dt_select_all, {
    if (isTRUE(input$dt_select_all)) {
      # Select all rows if checkbox is checked
      DT::selectRows(dt_proxy, input$concept_table_rows_all)
    } else {
      # Deselect all rows if checkbox is unchecked
      DT::selectRows(dt_proxy, NULL)
    }
  })

  # Combine concepts
  shiny::observeEvent(input$accept_btn, {
    combiningWaiter$show()
    shiny::removeModal()
    new_concept_name <- input$new_concept_name
    combineSelectedConcepts(new_concept_name, isManualCombine = T)
    target_mod(data_features())

    loaded_data(list(
      data_initial = data_initial(),
      data_patients = data_patients(),
      data_features = data_features(),
      data_person = loaded_data()$data_person,
      target_matrix = loaded_data()$target_matrix,
      target_row_annotation = loaded_data()$target_row_annotation,
      target_col_annotation = loaded_data()$target_col_annotation,
      config = loaded_data()$config
    ))
    shiny::updateCheckboxInput(session, "dt_select_all", "Select all", value = F)
    combiningWaiter$hide()
  })


  shiny::observeEvent(input$accept_corr_btn, {
    combiningWaiter$show()
    shiny::removeModal()
    new_concept_name <- input$new_concept_name
    combineSelectedConcepts(new_concept_name, isManualCombine = FALSE)
    target_mod(data_features())

    loaded_data(list(
      data_initial = data_initial(),
      data_patients = data_patients(),
      data_features = data_features(),
      data_person = loaded_data()$data_person,
      target_matrix = loaded_data()$target_matrix,
      target_row_annotation = loaded_data()$target_row_annotation,
      target_col_annotation = loaded_data()$target_col_annotation,
      config = loaded_data()$config
    ))
    shiny::updateCheckboxInput(session, "dt_select_all", "Select all", value = F)
    combiningWaiter$hide()
  })

  shiny::observeEvent(input$combine_btn, {
    if (length(input$concept_table_rows_selected) > 1) {
      shiny::showModal(shiny::modalDialog(
        title = "Combine Concepts",
        textInput("new_concept_name", "Enter New Concept Name:", ""),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("accept_btn", "Accept")
        )
      ))
    } else {
      shiny::showNotification("Select at least two rows to combine", type = "warning")
    }
  })


  shiny::observeEvent(input$combine_corr_btn, {
    if (!is.null(selected_correlation_group())) {
      shiny::showModal(shiny::modalDialog(
        title = "Combine Concepts",
        textInput("new_concept_name", "Enter New Concept Name:", ""),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("accept_corr_btn", "Accept")
        )
      ))
    } else {
      shiny::showNotification("Select a correlation group to combine", type = "warning")
    }
  })


  output$dynamic_selectize_trajectory_inputs <- renderUI({
    if (length(selected_correlation_group_labels()$labels) == 0 | isFALSE(input$correlationView)) {
      return(NULL)
    }

    state_names <- selected_correlation_group_labels()
    state_names <- state_names$labelsTrimmed
    # Split state names into two roughly equal groups
    n <- length(state_names)
    half <- ceiling(n / 2)
    col1_states <- state_names[1:half]
    col2_states <- state_names[(half + 1):n]

    # Create the UI layout with a separate row for the title
    tagList(
      fluidRow(
        column(12, h4("Filter trajectory states", style = "margin-top: 10px;"))
      ),
      fluidRow(
        column(
          width = 6, # First column (half width)
          lapply(col1_states, function(state) {
            selectizeInput(
              inputId = paste0("select_", state),
              label = state,
              choices = c("First occurrence", "All events", "Last occurrence"),
              selected = "All events"
            )
          })
        ),
        column(
          width = 6, # Second column (half width)
          lapply(col2_states, function(state) {
            selectizeInput(
              inputId = paste0("select_", state),
              label = state,
              choices = c("First occurrence", "All events", "Last occurrence"),
              selected = "All events"
            )
          })
        )
      )
    )
  })

  selected_trajectory_filtering_values <- reactive({
    state_names <- selected_correlation_group_labels()

    state_names_trimmed <- state_names$labelsTrimmed
    state_names_original <- state_names$labels

    # Get the selected value for each state
    selections <- lapply(state_names_trimmed, function(state) {
      input[[paste0("select_", state)]] # Access input value dynamically
    })

    # Return a named list
    names(selections) <- state_names_original
    selections
  })

  # Reset data to original
  shiny::observeEvent(input$reset_btn_mappings, {
    initial_data <- list(
      data_initial = original_data()$data_initial,
      data_patients = original_data()$data_patients,
      data_features = original_data()$data_features,
      data_person = original_data()$data_person,
      target_matrix = original_data()$data_matrix,
      target_row_annotation = original_data()$target_row_annotation,
      target_col_annotation = original_data()$target_col_annotation,
      config = original_data()$config
    )

    loaded_data(initial_data)
    data_features(original_data()$data_features)
    data_patients(original_data()$data_patients)
    data_initial(original_data()$data_initial)
    target_mod(original_data()$data_features)

    data_features(data_features())
    complementaryMappingTable(if (!is.null(original_data()$complementaryMappingTable)) complementaryMappingTable(original_data()$complementaryMappingTable) else data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), NEW_CONCEPT_ID = integer(), NEW_CONCEPT_NAME = character(), ABSTRACTION_LEVE = integer(), stringsAsFactors = FALSE))
    })

  # Save data to file on button press
  shiny::observeEvent(input$save_btn, {
    # Create the base file path
    CohortContrast:::createPathToResults(paste0(pathToResults, "/snapshots"))
    file_base <- stringr::str_c(pathToResults, "/snapshots/", studyName(), "_Snapshot")
    file_path <- stringr::str_c(file_base, ".rds")
    counter <- 1

    # Check if file exists and append increasing numbers if necessary
    while (file.exists(file_path)) {
      file_path <- stringr::str_c(file_base, "_", counter, ".rds")
      counter <- counter + 1
    }

    temp_filtered_target = list(target_matrix = target_matrix(),
                                target_row_annotation = target_row_annotation(),
                                target_col_annotation = target_col_annotation(),
                                target_time_annotation = target_time_annotation()
    )
    correlation_threshold = if(is.null(input$correlation_threshold)) 0.95 else input$correlation_threshold
    temp_filtered_target = prepare_filtered_target(temp_filtered_target, correlation_threshold)
    # Extract the data from the reactive
    ccObject <- list(
      data_initial = data_initial(),
      data_patients = data_patients(),
      data_features = data_features(),
      data_person = loaded_data()$data_person,
      filtered_target = temp_filtered_target,
      complementaryMappingTable = complementaryMappingTable(),
      config = loaded_data()$config
    )
    # Extract trajectory generation info from session
    selectedFeatureIds = as.integer(ccObject$filtered_target$target_row_annotation %>% rownames())
    parsed_abstraction_value <- ifelse(input$abstraction_lvl == "original", -1, ifelse(input$abstraction_lvl == "source", -2, as.numeric(input$abstraction_lvl)))
    ccObject$config$abstractionLevel = parsed_abstraction_value
    selectedFeatureNames = ccObject$data_features %>%
      dplyr::filter(.data$ABSTRACTION_LEVEL == ccObject$config$abstractionLevel,
                    .data$CONCEPT_ID %in% selectedFeatureIds) %>%
      dplyr::select(.data$CONCEPT_NAME) %>% dplyr::pull(.data$CONCEPT_NAME)

    trajectoryDataList = list(
      selectedFeatureIds = selectedFeatureIds,
      selectedFeatureNames = selectedFeatureNames
    )
    ccObject$trajectoryDataList = trajectoryDataList
    # Save the actual data to the file
    saveRDS(ccObject, file = file_path)
    snapshotWaiter$hide()
    # Notify the user
    shiny::showNotification(paste("Data saved to", file_path))
  })

  #################################################################### FILTERING (REMOVE CONCEPTS)
  # # Reactive expression to prepare choices with both CONCEPT_ID and CONCEPT_NAME for display

  output$dynamic_correlation_widgets <- renderUI({
    if (isTRUE(input$correlationView)) {
      fluidRow(
        column(
          width = 4,
          sliderInput(
            "correlation_threshold",
            label = "Correlation cutoff",
            min = 0,
            max = 1,
            value = 1,
            step = 0.01
          )
        ),
        column(
          width = 4,
          uiOutput("correlation_group_selection"),
          actionButton("combine_corr_btn", "Combine State Group Concepts")
        ),
        column(
          width = 4,
          sliderInput(
            "edge_prevalence_threshold",
            label = "Edge prevalence cutoff",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.01
          )
        )
      )
    }
  })


  # Server-side rendering of the pickerInput
  output$correlation_group_selection <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "filter_correlation_group",
      label = "Select correlation group",
      choices = NULL, # Start with no choices, they will be loaded server-side
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE, # Enable live search in the dropdown
        title = "Search & select a group",
        header = "Select a group",
        style = "btn-primary"
      )
    )
  })

  # Monitor correlation_groups() for changes
  shiny::observe({
    # Fetch the updated correlation groups
    groups <- correlation_groups()
    # Create a named list for the pickerInput choices
    data_groups <- list()

    # Iterate through the groups to create group-level choices with formatted names
    for (i in seq_along(groups)) {
      group <- groups[[i]]
      concept_ids <- names(group)
      concept_names <- as.character(group)

      # Create a formatted label for the group, adding line breaks for each concept
      group_label <- paste(
        sprintf("<b>State Group %d</b>", i), # Bold group name
        paste(concept_names, collapse = "<br>"), # Join concept names with line breaks
        sep = "<br>"
      )

      # Add to the data_groups list
      data_groups[[paste("State Group", i)]] <- setNames(
        paste(concept_ids, collapse = ";"), # Concatenate all IDs for this group
        group_label # Use the formatted label
      )
    }

        # Update the pickerInput choices with group-level options
    shinyWidgets::updatePickerInput(
      session,
      "filter_correlation_group",
      choices = unlist(data_groups), # Flatten the list for pickerInput
      choicesOpt = list(
        content = unname(unlist(lapply(data_groups, names))) # Add HTML labels
      )
    )
  })

  # selected ids
  shiny::observeEvent(input$filter_correlation_group, {
    # Retrieve the selected group
    selected <- input$filter_correlation_group
    # Initialize a variable to store selected IDs
    selected_ids <- NULL

    if (!is.null(selected)) {
      # Split the selected group value into individual IDs
      selected_ids <- unlist(strsplit(selected, ";"))
    }
    # Update the reactive value with the selected IDs
    selected_correlation_group(selected_ids)
    parsed_abstraction_value <- ifelse(input$abstraction_lvl == "original", -1, ifelse(input$abstraction_lvl == "source", -2, as.numeric(input$abstraction_lvl)))

    selected_correlation_group_labels(getTrimmedConceptIDLabels(ids = selected_ids, data = data_features(), absLvl = parsed_abstraction_value))
  })


  # TODO sometimes does not load, cannot always recreate
  # Server-side selectize input for filtering by CONCEPT_ID and CONCEPT_NAME
  output$dynamic_concepts_removal <- shiny::renderUI({
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::selectizeInput(
          inputId = "filter_concept_id",
          label = "Select concepts to remove",
          choices = NULL, # Start with no choices, they will be loaded server-side
          selected = NULL,
          multiple = FALSE,
          options = list(
            placeholder = "Search & select a CONCEPT_ID",
            server = TRUE, # Enable server-side processing
            loadThrottle = 100, # Time delay before querying the server, in milliseconds
            load = I('function(query_filtering, callback) {
              if (!query_filtering.length) return callback();
              Shiny.setInputValue("search_query_filtering", query_filtering);
            }')
          )
        )
      ),
      shiny::column(
        width = 6,
        shiny::actionButton("add_filter", "Remove concept", icon = shiny::icon("trash"))
      )
    )
  })

  # Server-side search for selectizeInput
  shiny::observeEvent(input$search_query_filtering, {
    query_filtering <- input$search_query_filtering
    data_filtering <- data_features()

    # Filter the choices based on the search query
    matched_choices_filtering <- stats::setNames(
      data_filtering$CONCEPT_ID,
      paste0(data_filtering$CONCEPT_NAME, " (", data_filtering$CONCEPT_ID, ")")
    )

    # Send the matched choices to the selectizeInput
    shiny::updateSelectizeInput(session, "filter_concept_id", choices = matched_choices_filtering, server = TRUE)
  })

  # Observe when the "Add Filter" button is clicked
  shiny::observeEvent(input$add_filter, {
    # Retrieve current filters
    filters <- selected_filters()
    # Get the selected CONCEPT_NAME based on the selected CONCEPT_ID
    concept_name <- data_features()$CONCEPT_NAME[
      data_features()$CONCEPT_ID == input$filter_concept_id
    ][1]

    # Check if the pair is unique and add it to the list
    if (!(input$filter_concept_id %in% sapply(filters, `[[`, "id"))) {
      filters <- append(filters, list(list(id = input$filter_concept_id, name = concept_name)))
      selected_filters(filters)
    }
  })

  output$selected_filters_list <- shiny::renderUI({
    filters <- selected_filters()
    if (length(filters) == 0) {
      return(NULL)
    }
    htmltools::tags$ul(
      lapply(seq_along(filters), function(i) {
        # Increment the counter within an isolated context
        shiny::isolate({
          counter <- removeButtonCounter() + 1
          removeButtonCounter(counter) # Update the reactive value, but isolated
        })
        # Map the counter to the filter ID
        removeButtonCounterMap[[as.character(counter)]] <- filters[[i]]$id
        # Create the UI element for the filter with the remove button
        filterTagItem <- htmltools::tags$li(
          paste(filters[[i]]$id, "-", filters[[i]]$name),
          shiny::actionButton(inputId = paste0("remove_filter_", counter), label = "Restore", icon = icon("arrow-up"))
        )
        return(filterTagItem)
      })
    )
  })


  # Manage observers for removal buttons
  shiny::observe({
    filters <- selected_filters()
    current_ids <- names(removeButtonCounterMap)

    # Loop through the counter map to create or update observers for each remove button
    lapply(current_ids, function(counter) {
      if (!is.null(removeButtonCounterMap[[counter]])) {
        shiny::observeEvent(input[[paste0("remove_filter_", counter)]],
          {
            filter_id_to_remove <- removeButtonCounterMap[[counter]]
            # Remove the filter from the list
            updated_filters <- filters[!sapply(filters, function(f) f$id == filter_id_to_remove)]
            selected_filters(updated_filters)


            data_features <- data_features()
            data_patients <- data_patients()

            data_features_original <- original_data()$data_features
            data_patients_original <- original_data()$data_patients

            data_features_restore <- data_features_original[data_features_original$CONCEPT_ID == removeButtonCounterMap[[counter]], ]
            data_patients_restore <- data_patients_original[data_patients_original$CONCEPT_ID == removeButtonCounterMap[[counter]], ]

            data_features <- rbind(data_features, data_features_restore)
            data_patients <- rbind(data_patients, data_patients_restore)

            # Update the reactive values with the restored data
            data_patients(data_patients)
            data_features(data_features)

            # Update the loaded_data reactive with the restored data
            loaded_data(list(
              data_initial = data_initial(),
              data_patients = data_patients(), # Note the use of data_patients()
              data_features = data_features(), # Note the use of data_features()
              data_person = loaded_data()$data_person,
              target_matrix = loaded_data()$target_matrix,
              target_row_annotation = loaded_data()$target_row_annotation,
              target_col_annotation = loaded_data()$target_col_annotation,
              complementaryMappingTable = loaded_data()$complementaryMappingTable,
              config = loaded_data()$config
            ))


            # Remove the mapping for the button after it's used
            removeButtonCounterMap[[counter]] <- NULL

            # Destroy the observer for this button after removal to prevent looping
            shiny::isolate(removeButtonCounterMap[[counter]] <- NULL)
          },
          once = TRUE
        ) # Ensure the observer is only triggered once
      }
    })
  })

  shiny::observe({
    filters <- selected_filters()

    # Retrieve the current values of data_features and data_patients
    data_filtering <- data_features()
    data_patients <- data_patients()

    if (length(filters) > 0) {
      for (filter in filters) {
        # Filter data based on the selected filters
        data_filtering <- data_filtering[data_filtering$CONCEPT_ID != filter$id, ]
        data_patients <- data_patients[data_patients$CONCEPT_ID != filter$id, ]
      }
    }

    # Update the reactive values with the filtered data
    data_patients(data_patients)
    data_features(data_filtering)

    # Update the loaded_data reactive with the new filtered data
    loaded_data(list(
      data_initial = data_initial(),
      data_patients = data_patients(), # Note the use of data_patients()
      data_features = data_features(), # Note the use of data_features()
      data_person = loaded_data()$data_person,
      target_matrix = loaded_data()$target_matrix,
      target_row_annotation = loaded_data()$target_row_annotation,
      target_col_annotation = loaded_data()$target_col_annotation,
      complementaryMappingTable = loaded_data()$complementaryMappingTable,
      config = loaded_data()$config
    ))
  })


  #################################################################### FILTERING (SELECT ONLY WITH CONCEPTS)
  # Reactive expression to prepare choices with both CONCEPT_ID and CONCEPT_NAME for display

  # Server-side selectize input for filtering by CONCEPT_ID and CONCEPT_NAME
  output$dynamic_concepts_selection <- shiny::renderUI({
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::selectizeInput(
          inputId = "filter_patient_concept_id_selection",
          label = "Select concepts for subject filtering",
          choices = NULL, # Start with no choices, they will be loaded server-side
          selected = NULL,
          multiple = FALSE,
          options = list(
            placeholder = "Search & select a CONCEPT_ID",
            server = TRUE, # Enable server-side processing
            loadThrottle = 100, # Time delay before querying the server, in milliseconds
            load = I('function(query_filtering, callback) {
              if (!query_filtering.length) return callback();
              Shiny.setInputValue("search_patient_query_filtering", query_filtering);
            }')
          )
        )
      ),
      shiny::column(
        width = 6,
        shiny::actionButton("add_patients_filter", "Filter with concept", icon = shiny::icon("user-plus")),
        shiny::actionButton("disregard_patients_filter", "Filter without concept", icon = shiny::icon("user-minus"))
      )
    )
  })

  # Server-side search for selectizeInput
  shiny::observeEvent(input$search_patient_query_filtering, {
    query_filtering <- input$search_patient_query_filtering
    data_filtering <- data_features()

    # Filter the choices based on the search query
    matched_choices_filtering <- stats::setNames(
      data_filtering$CONCEPT_ID,
      paste0(data_filtering$CONCEPT_NAME, " (", data_filtering$CONCEPT_ID, ")")
    )

    # Send the matched choices to the selectizeInput
    shiny::updateSelectizeInput(session, "filter_patient_concept_id_selection", choices = matched_choices_filtering, server = TRUE)
  })

  # Observe when the "Add Filter" button is clicked
  shiny::observeEvent(input$add_patients_filter, {
    # If the value has changed since the last observation
    if (input$add_patients_filter > last_add_filter_value()) {
      # Update last seen value
      last_add_filter_value(input$add_patients_filter)

      # Perform the add filter logic
      add_filter_logic("with")
    }
  })

  # Observe when the "Disregard Filter" button is clicked
  shiny::observeEvent(input$disregard_patients_filter, {
    # If the value has changed since the last observation
    if (input$disregard_patients_filter > last_disregard_filter_value()) {
      # Update last seen value
      last_disregard_filter_value(input$disregard_patients_filter)

      # Perform the disregard filter logic
      add_filter_logic("without")
    }
  })

  # Function to handle the addition of filters
  add_filter_logic <- function(filter_type) {
    # Retrieve current filters
    filters <- selected_patient_filters()

    # Get the selected CONCEPT_NAME based on the selected CONCEPT_ID
    concept_name <- data_features()$CONCEPT_NAME[
      data_features()$CONCEPT_ID == input$filter_patient_concept_id_selection
    ][1]

    # Check if the pair is unique and add it to the list
    if (!(input$filter_patient_concept_id_selection %in% sapply(filters, `[[`, "id"))) {
      filters <- append(filters, list(list(
        id = input$filter_patient_concept_id_selection,
        name = concept_name,
        type = filter_type
      )))
      selected_patient_filters(filters)
    }
  }
  output$selected_patient_filters_list <- shiny::renderUI({
    filters <- selected_patient_filters()

    if (length(filters) == 0) {
      return(NULL)
    }

    htmltools::tags$ul(
      lapply(seq_along(filters), function(i) {
        # Increment the counter within an isolated context
        shiny::isolate({
          counter <- filterButtonCounter() + 1
          filterButtonCounter(counter) # Update the reactive value, but isolated
        })

        # Map the counter to the filter ID
        filterButtonCounterMap[[as.character(counter)]] <- filters[[i]]$id

        # Determine icon based on filter type
        filter_icon <- if (filters[[i]]$type == "without") {
          icon("user-minus")
        } else {
          icon("user-plus")
        }

        # Create the UI element for the filter with the appropriate icon
        filterTagItem <- htmltools::tags$li(
          paste(filters[[i]]$id, "-", filters[[i]]$name),
          shiny::actionButton(inputId = paste0("add_filter_", counter), label = "Remove", icon = filter_icon)
        )

        return(filterTagItem)
      })
    )
  })


  # Manage observers for removal buttons
  shiny::observe({
    filters <- selected_patient_filters()
    current_ids <- names(filterButtonCounterMap)

    # Loop through the counter map to create or update observers for each remove button
    lapply(current_ids, function(counter) {
      if (!is.null(filterButtonCounterMap[[counter]])) {
        shiny::observeEvent(input[[paste0("add_filter_", counter)]],
          {
            filter_id_to_remove <- filterButtonCounterMap[[counter]]
            # Remove the filter from the list
            updated_filters <- filters[!sapply(filters, function(f) f$id == filter_id_to_remove)]
            selected_patient_filters(updated_filters)

            # Update the reactive values with the restored data
            data_patients(original_data()$data_patients)
            data_features(original_data()$data_features)
            data_initial(original_data()$data_initial)

            # Update the loaded_data reactive with the restored data
            keepUsersWithConcepts(updated_filters)
            # Remove the mapping for the button after it's used
            filterButtonCounterMap[[counter]] <- NULL

            # Destroy the observer for this button after removal to prevent looping
            shiny::isolate(filterButtonCounterMap[[counter]] <- NULL)
          },
          once = TRUE
        ) # Ensure the observer is only triggered once
      }
    })
  })


  shiny::observe({
    shiny::req(data_patients())
    filters <- selected_patient_filters()
    keepUsersWithConcepts(filters)
  })

  observeEvent(input$correlationView, {
    # Only trigger when input$correlationView is TRUE
    if (isTRUE(input$correlationView)) {
      shiny::observe({
        shiny::req(studyName())
        later::later(function() {
          isolate({
            correlation_groups(NULL)
          })
        }, delay = 0.5)
      })
    }
  })

  # Function to combine selected concepts
  combineSelectedConcepts <- function(new_concept_name, isManualCombine = TRUE) {
    selected_rows <- input$concept_table_rows_selected
    selected_ids <- selected_correlation_group()
    parsed_abstraction_value <- ifelse(input$abstraction_lvl == "original", -1, ifelse(input$abstraction_lvl == "source", -2, as.numeric(input$abstraction_lvl)))

    abstraction_level <- parsed_abstraction_value
    data_features <- data_features()
    data_patients <- data_patients()
    data_initial <- data_initial()
    complementary_mapping <- complementaryMappingTable()

    # Assuming data_initial, data_features, and processed_table are data.tables
    # Step 1: Grouping and summarizing, followed by reshaping
    n_patients_temp <- data_initial[, .N, by = COHORT_DEFINITION_ID]
    n_patients <- reshape2::dcast(n_patients_temp, formula = 1 ~ COHORT_DEFINITION_ID, value.var = "N", fill = 0)
    # Step 2: Extracting count values
    count_target <- n_patients$target
    count_control <- n_patients$control

    processed_table <- target_mod()[ABSTRACTION_LEVEL == abstraction_level]

    selected_concept_ids = NA
    selected_concept_names = NA
    representingConceptId = NA
    representingHeritage = NA
    representingZTest = NA
    representingLogitTest = NA
    representingKSTest = NA

    if(isManualCombine) {
    # Step 3: Filtering processed_table and selecting concept IDs
    selected_concept_ids <- as.numeric(processed_table$CONCEPT_ID[selected_rows])
    selected_concept_names <- processed_table$CONCEPT_NAME[selected_rows]

    representingConceptId <- selected_concept_ids[
      which.max(as.numeric(processed_table$PREVALENCE_DIFFERENCE_RATIO[selected_rows]))
    ]
    representingHeritage <- processed_table$HERITAGE[selected_rows][
      which.max(as.numeric(processed_table$PREVALENCE_DIFFERENCE_RATIO[selected_rows]))]

    # Step 4: Determining ZTEST, KSTEST and LOGITTEST values
    representingZTest <- any(processed_table$ZTEST[selected_rows])
    representingLogitTest <- any(processed_table$LOGITTEST[selected_rows])
    representingKSTest <- any(processed_table$KSTEST[selected_rows])
    }
    else {
      # Step 3: Filtering processed_table and selecting concept IDs
      selected_concept_ids <- as.numeric(selected_ids)
      selected_concept_names <- processed_table[CONCEPT_ID %in% selected_concept_ids, CONCEPT_NAME]
      representingConceptId <- selected_concept_ids[
        which.max(as.numeric(processed_table[CONCEPT_ID %in% selected_concept_ids, PREVALENCE_DIFFERENCE_RATIO]))
      ]
      representingHeritage <- processed_table[CONCEPT_ID %in% selected_concept_ids, HERITAGE][
        which.max(as.numeric(processed_table[CONCEPT_ID %in% selected_concept_ids, PREVALENCE_DIFFERENCE_RATIO]))]

      # Step 4: Determining ZTEST, KSTEST and LOGITTEST values
      representingZTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, ZTEST])
      representingLogitTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, LOGITTEST])
      representingKSTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, KSTEST])
    }

    # Step 5: Updating data_features with new values
    data_features[
      CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstraction_level,
      `:=`(
        ZTEST = representingZTest,
        LOGITTEST = representingLogitTest,
        KSTEST = representingKSTest,
        HERITAGE = representingHeritage
      )
    ]

    rows_to_update <- data_patients[
      , CONCEPT_ID %in% selected_concept_ids & ABSTRACTION_LEVEL == abstraction_level
    ]

    # Step 4: Update rows and group by relevant columns, then summarize
    data_patients[
      rows_to_update,
      `:=`(
        CONCEPT_ID = representingConceptId,
        CONCEPT_NAME = new_concept_name,
        HERITAGE = representingHeritage
      )
    ]

    # Step 5: Summarize by grouping and calculating prevalence
    data_patients <- data_patients[
      , .(PREVALENCE = sum(PREVALENCE),
          TIME_TO_EVENT = list(unlist(TIME_TO_EVENT))),
      by = .(COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, CONCEPT_NAME, HERITAGE, ABSTRACTION_LEVEL)
    ]
    # Assuming data_features and data_patients are data.tables
    # Step 1: Select specific columns from data_features
    data_features_temp <- data_features[
      , .(CONCEPT_ID, ABSTRACTION_LEVEL, ZTEST,ZTEST_P_VALUE, LOGITTEST,LOGITTEST_P_VALUE, KSTEST, KSTEST_P_VALUE, HERITAGE)
    ]
    # Step 2: Summarize data_patients by CONCEPT_ID, CONCEPT_NAME, and ABSTRACTION_LEVEL

    data_features <- data_patients[
      , .(
        TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "target" & PREVALENCE > 0),
        CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "control" & PREVALENCE > 0),
        TIME_TO_EVENT = list(unlist(TIME_TO_EVENT))
      ),
      by = .(CONCEPT_ID, CONCEPT_NAME, ABSTRACTION_LEVEL)
    ]

    # Step 3: Add new columns using mutate-like operations
    data_features <- data_features[
      , `:=`(
        TARGET_SUBJECT_PREVALENCE = TARGET_SUBJECT_COUNT / count_target,
        CONTROL_SUBJECT_PREVALENCE = CONTROL_SUBJECT_COUNT / count_control
      )
    ]

    data_features <- data_features[
      , `:=`(
        PREVALENCE_DIFFERENCE_RATIO = data.table::fifelse(
          is.na(TARGET_SUBJECT_PREVALENCE) | TARGET_SUBJECT_PREVALENCE == 0, 0,
          data.table::fifelse(
            is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0,
            data.table::fifelse(TARGET_SUBJECT_PREVALENCE == 0, -1, 100),
            TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
          )
        )
      )
    ]

    # Step 4: Join with data_features_temp
    data_features <- data_features[
      data_features_temp,
      on = .(CONCEPT_ID, ABSTRACTION_LEVEL)
    ]

    data_features <- data_features[!is.na(CONCEPT_NAME)]


    # Step 5: Determining ZTEST, KSTEST and LOGITTEST values
    total_concepts = data_features %>% dplyr::filter(.data$ABSTRACTION_LEVEL == abstraction_level) %>% nrow()
    # ZTEST
    concept_row <- data_features[CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstraction_level, .(TARGET_SUBJECT_COUNT, CONTROL_SUBJECT_COUNT)]
    target_subject_count <- concept_row$TARGET_SUBJECT_COUNT
    control_subject_count <- concept_row$CONTROL_SUBJECT_COUNT

    representingZTest = FALSE
    representingZTestPValue = 1

    ztest_result <- stats::prop.test(
      c(target_subject_count, control_subject_count),
      c(count_target, count_control),
      conf.level = 0.95
    )
    if (!(is.na(ztest_result$p.value))) {
      representingZTestPValue = ztest_result$p.value
      representingZTest = if(representingZTestPValue < 0.05/total_concepts) TRUE else FALSE
    }

    # Logit Test
    representingLogitTest = FALSE
    representingLogitTestPValue = 1
    # Create the dataset for logistic regression
    concept_data <- data_patients %>%
      dplyr::filter(.data$ABSTRACTION_LEVEL == abstraction_level, .data$CONCEPT_ID == representingConceptId) %>%
      dplyr::mutate(
        PREVALENCE = 1,
        TARGET = dplyr::if_else(.data$COHORT_DEFINITION_ID == "target", 1, 0),
        CONTROL = dplyr::if_else(.data$COHORT_DEFINITION_ID != "target", 0, 1)
      ) %>% dplyr::select(.data$COHORT_DEFINITION_ID, .data$PREVALENCE, .data$TARGET, .data$CONTROL)

    no_match_target = data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID == "target") %>% nrow() - concept_data %>% dplyr::filter(.data$COHORT_DEFINITION_ID == "target") %>% nrow()
    no_match_control = data_initial %>% dplyr::filter(.data$COHORT_DEFINITION_ID != "target") %>% nrow() - concept_data %>% dplyr::filter(.data$COHORT_DEFINITION_ID != "target") %>% nrow()

    no_match_target_df = NULL
    no_match_control_df = NULL
    if(no_match_target != 0){
      no_match_target_df = data.frame(COHORT_DEFINITION_ID = "target", PREVALENCE = 0, TARGET = rep(1, no_match_target), CONTROL = 0)
    }
    if(no_match_control != 0){
      no_match_control_df = data.frame(COHORT_DEFINITION_ID = "control", PREVALENCE = 0, TARGET = 0, CONTROL = rep(1, no_match_control))
    }

    concept_data = rbind(rbind(no_match_target_df, no_match_control_df), concept_data)

    prevalence_cohort_2 <- ifelse(is.na(sum(concept_data$PREVALENCE[concept_data$TARGET == 1])), 0, sum(concept_data$PREVALENCE[concept_data$TARGET == 1]))
    prevalence_cohort_1 <- ifelse(is.na(sum(concept_data$PREVALENCE[concept_data$CONTROL == 1])), 0, sum(concept_data$PREVALENCE[concept_data$CONTROL == 1]))

   if (prevalence_cohort_1 == 0 | prevalence_cohort_2 == 0) {
     representingLogitTestPValue = NA
     representingLogitTest = TRUE
   } else {
    # Perform logistic regression
    model <- stats::glm(TARGET ~ PREVALENCE, data = concept_data, family = stats::binomial)
    summary_model <- summary(model)

    # Check if the presence of the concept is significant
    p_value <- tryCatch({
      # Attempt to access the p-value
      p_value <- summary_model$coefficients[2, 4]
      p_value  # Return the value if successful
    }, error = function(e) {
      # Return NA or another indicator if there is an error
      NA
    })

    if (!(is.na(p_value))) {
      representingLogitTestPValue = p_value
      representingLogitTest = if(representingLogitTestPValue < 0.05/total_concepts) TRUE else FALSE
    }
   }
    # KS Test
    agg_data <- data_features %>% dplyr::select(.data$CONCEPT_ID, .data$ABSTRACTION_LEVEL, .data$TIME_TO_EVENT)
    concept_date_array =  unlist(dplyr::filter(agg_data, .data$CONCEPT_ID == representingConceptId, .data$ABSTRACTION_LEVEL == abstraction_level)$TIME_TO_EVENT)
    concept_date_array_min = min(concept_date_array) # 0
    concept_date_array_max = max(concept_date_array)
    ks_result <- stats::ks.test(jitter(concept_date_array, amount=0.1), "punif", min=concept_date_array_min, max=concept_date_array_max)


    representingKSTest = ks_result$p.value < 0.05/total_concepts
    representingKSTestPValue = ks_result$p.value

    # Update a specific row with new values for example columns
    data_features[CONCEPT_ID == representingConceptId & ABSTRACTION_LEVEL == abstraction_level, `:=`(
      ZTEST = representingZTest,
      ZTEST_P_VALUE = representingZTestPValue,
      LOGITTEST = representingLogitTest,
      LOGITTEST_P_VALUE = representingLogitTestPValue,
      KSTEST = representingKSTest,
      KSTEST_P_VALUE = representingKSTestPValue,
      HERITAGE = representingHeritage
    )]

    # Tests end

    data_features(data_features)
    data_patients(data_patients)
    # Update complementaryMappingTable
    new_rows <- data.frame(CONCEPT_ID = selected_concept_ids, CONCEPT_NAME = selected_concept_names,NEW_CONCEPT_ID = representingConceptId, NEW_CONCEPT_NAME = new_concept_name, ABSTRACTION_LEVEL = abstraction_level, stringsAsFactors = FALSE)
    complementary_mapping <- rbind(complementary_mapping, new_rows)
    # Update all related concept names in complementaryMappingTable
    # Step 1: Identify related concepts
    related_concepts <- complementary_mapping[
      complementary_mapping$CONCEPT_ID %in% selected_concept_ids & complementary_mapping$ABSTRACTION_LEVEL == abstraction_level,
      "NEW_CONCEPT_NAME"
    ]
    # Step 2: Update CONCEPT_NAME for related concepts
    complementary_mapping = data.table::as.data.table(complementary_mapping)
    complementary_mapping[
      complementary_mapping$NEW_CONCEPT_NAME %in% related_concepts & complementary_mapping$ABSTRACTION_LEVEL == abstraction_level,
      NEW_CONCEPT_NAME := new_concept_name
    ]
    # Step 3: Remove duplicates
    complementary_mapping <- unique(complementary_mapping)

    complementaryMappingTable(complementary_mapping)

    loaded_data(list(
      data_initial = data_initial,
      data_patients = data_patients,
      data_features = data_features,
      data_person = loaded_data()$data_person,
      target_matrix = loaded_data()$target_matrix,
      target_row_annotation = loaded_data()$target_row_annotation,
      target_col_annotation = loaded_data()$target_col_annotation,
      complementaryMappingTable = complementary_mapping,
      config = loaded_data()$config

    ))
  }
  # Function to combine selected concepts
  keepUsersWithConcepts <- function(filters) {
    if (length(filters) == 0) {
      return()
    }
    parsed_abstraction_value <- ifelse(input$abstraction_lvl == "original", -1, ifelse(input$abstraction_lvl == "source", -2, as.numeric(input$abstraction_lvl)))


    abstraction_level <- parsed_abstraction_value
    data_features <- data_features()
    data_patients_filtering <- data_patients()
    data_initial_filtering <- data_initial()
    complementary_mapping <- complementaryMappingTable()

    # Convert from int64
    data_initial_filtering$SUBJECT_ID <- as.integer(data_initial_filtering$SUBJECT_ID)
    data_patients_filtering$PERSON_ID <- as.integer(data_patients_filtering$PERSON_ID)
    # Step 1: Filter rows where COHORT_DEFINITION_ID is 'control'
    data_patients_control <- data_patients_filtering[
      COHORT_DEFINITION_ID == "control"
    ]

    # Step 2: Filter rows where COHORT_DEFINITION_ID is 'target'
    data_patients_target <- data_patients_filtering[
      COHORT_DEFINITION_ID == "target"
    ]
    # Initialize personIdsToKeep with all PERSON_IDs initially
    personIdsToKeep <- data_patients_target$PERSON_ID

    for (filter in filters) {
      # Find PERSON_IDs matching the current filter
      # Step 1: Filter rows based on CONCEPT_ID
      currentPersonIds <- data_patients_target[
        CONCEPT_ID == as.integer(filter$id),
        .(PERSON_ID)
      ]

      # Step 2: Select distinct PERSON_IDs
      currentPersonIds <- unique(currentPersonIds)
      # Step 3: Pull the PERSON_ID column and convert to integer
      currentPersonIds <- as.integer(currentPersonIds$PERSON_ID)

      if(filter$type == "without"){
      currentPersonIds <- setdiff(unique(data_patients_target$PERSON_ID), currentPersonIds)
      }

      # Intersect with the previous PERSON_IDs
      personIdsToKeep <- intersect(personIdsToKeep, currentPersonIds)
    }

    # Step 1: Filter the target data based on personIdsToKeep
    data_patients_target <- data_patients_target[
      PERSON_ID %in% personIdsToKeep
    ]

    # Step 2: Combine the filtered target data with the control data
    data_patients_filtering <- data.table::rbindlist(list(data_patients_target, data_patients_control))


    # Step 1: Filter for 'target' cohort
    target_ids <- unique(data_patients_target$PERSON_ID)
    target_filter <- data_initial_filtering[
      SUBJECT_ID %in% target_ids & COHORT_DEFINITION_ID == "target"
    ]

    # Step 2: Filter for 'control' cohort
    control_ids <- unique(data_patients_control$PERSON_ID)
    control_filter <- data_initial_filtering[
      SUBJECT_ID %in% control_ids & COHORT_DEFINITION_ID == "control"
    ]

    # Step 3: Combine the filtered data tables
    data_initial_filtering <- data.table::rbindlist(list(target_filter, control_filter))


    count_target <- data_initial_filtering[
      COHORT_DEFINITION_ID == "target",
      .N
    ]

    count_control <- data_initial_filtering[
      COHORT_DEFINITION_ID == "control",
      .N
    ]

    # Step 2: Select specific columns
    data_features_temp <- data_features[
      , .(CONCEPT_ID, ABSTRACTION_LEVEL, ZTEST,ZTEST_P_VALUE, LOGITTEST,LOGITTEST_P_VALUE, KSTEST, KSTEST_P_VALUE, HERITAGE)
    ]
    if (nrow(data_patients_filtering) == 0) {
      data_features_filtering <- data_features[0]
    } else {
      data_features_filtering <- data_patients_filtering[
        , .(
          TARGET_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "target" & PREVALENCE > 0),
          CONTROL_SUBJECT_COUNT = sum(COHORT_DEFINITION_ID == "control" & PREVALENCE > 0),
          TIME_TO_EVENT = list(unlist(TIME_TO_EVENT))
        ),
        by = .(CONCEPT_ID, CONCEPT_NAME, ABSTRACTION_LEVEL)
      ][
        , `:=`(
          TARGET_SUBJECT_PREVALENCE = data.table::fifelse(count_target == 0, 0, TARGET_SUBJECT_COUNT / count_target),
          CONTROL_SUBJECT_PREVALENCE = data.table::fifelse(count_control == 0, 0, CONTROL_SUBJECT_COUNT / count_control)
        ),
        by = .(CONCEPT_ID, ABSTRACTION_LEVEL)
      ]

      data_features_filtering <- data_features_filtering[
        , `:=`(
          PREVALENCE_DIFFERENCE_RATIO = data.table::fifelse(
            is.na(TARGET_SUBJECT_PREVALENCE) | TARGET_SUBJECT_PREVALENCE == 0, 0,
            data.table::fifelse(
              is.na(CONTROL_SUBJECT_PREVALENCE) | CONTROL_SUBJECT_PREVALENCE == 0,
              data.table::fifelse(TARGET_SUBJECT_PREVALENCE == 0, -1, 100),
              TARGET_SUBJECT_PREVALENCE / CONTROL_SUBJECT_PREVALENCE
            )
          )
        ),
        by = .(CONCEPT_ID, ABSTRACTION_LEVEL)
      ]

      # Step 5: Joining with data_features_temp
      data_features_filtering <- data_features_filtering[
        data_features_temp,
        on = .(CONCEPT_ID, ABSTRACTION_LEVEL)
      ]
    }
    data_features_filtering <- data_features_filtering[!is.na(CONCEPT_NAME)]

    data_features(data_features_filtering)
    data_patients(data_patients_filtering)
    data_initial(data_initial_filtering)


    loaded_data(list(
      data_initial = data_initial_filtering,
      data_patients = data_patients_filtering,
      data_features = data_features_filtering,
      data_person = loaded_data()$data_person,
      target_matrix = loaded_data()$target_matrix,
      target_row_annotation = loaded_data()$target_row_annotation,
      target_col_annotation = loaded_data()$target_col_annotation,
      complementaryMappingTable = complementary_mapping,
      config = loaded_data()$config
    ))
  }

  getTrimmedConceptIDLabels = function(ids, data, absLvl) {
    if(length(ids) > 1) {
      # Fetch original labels
      labels = data[(CONCEPT_ID %in% ids) & (ABSTRACTION_LEVEL == absLvl), CONCEPT_NAME]
      # Trim the labels
      labelsTrimmed = sub("/C:.*", "", labels)
      # Return a list with both labels and trimmed labels
      return(list(labels = labels, labelsTrimmed = labelsTrimmed))
    } else {
      # Return an empty list if `ids` is not valid
      return(list(labels = character(0), labelsTrimmed = character(0)))
    }
  }
  }

