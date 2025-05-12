
################################################################################
#
# Server content
#
################################################################################

server <- function(input, output, session) {
  library(dplyr)
  # Reactive values
  ## Information regarding the study
  study_info <- shiny::reactiveVal(NULL)
  studyName <- shiny::reactiveVal()

  ### Data from the study
  originalData <- shiny::reactiveVal(NULL)
  cachedData <- shiny::reactiveVal(NULL)

  # User inputs
  ## Direct user inputs
  abstractionLevelReactive <- shiny::reactiveVal("-1")
  autoScaleRate <- shiny::reactiveVal(FALSE)
  applyInverseTarget <- shiny::reactiveVal(FALSE)
  applyZTest <- shiny::reactiveVal(FALSE)
  applyLogitTest <- shiny::reactiveVal(FALSE)
  correlationCutoff <- shiny::reactiveVal(1)
  isCorrelationView <- shiny::reactiveVal(FALSE)
  isRemoveUntreated <- shiny::reactiveVal(FALSE)
  activeDomains <- shiny::reactiveVal(NULL)
  patientPrevalence <- shiny::reactiveVal(0.2)
  conceptPrevalenceRatio <- shiny::reactiveVal(1.25)
  correlationGroupChoices <- shiny::reactiveVal(NULL)
  correlationGroupChoicesLabels <- shiny::reactiveVal(NULL)
  selectedCorrelationGroup <- shiny::reactiveVal(NULL)
  selectedFirstDemographicGroup <- shiny::reactiveVal(NULL)
  selectedSecondDemographicGroup <- shiny::reactiveVal(NULL)
  selectedCorrelationGroupLabels <- shiny::reactiveVal(list(labels = character(0), labelsTrimmed = character(0)))
  selectedTrajectoryFilters <- shiny::reactiveValues(data = list())
  selectedFilters <- shiny::reactiveVal(list())
  selectedPatientFilters <- shiny::reactiveVal(list())


  ## Indirect user inputs
  correlationGroups <- shiny::reactiveVal(NULL)
  complementaryMappingTable <- shiny::reactiveVal(data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), NEW_CONCEPT_ID = integer(), NEW_CONCEPT_NAME = character() , ABSTRACTION_LEVEL = integer(), TYPE = character(), stringsAsFactors = FALSE))
  isPatientLevelDataPresent <- shiny::reactiveVal(TRUE)
  lookbackDays <- shiny::reactiveVal(NULL)
  removeButtonCounter <- shiny::reactiveVal(0)
  removeButtonCounterMap <- shiny::reactiveValues()
  filterButtonCounter <- shiny::reactiveVal(0)
  filterButtonCounterMap <- shiny::reactiveValues()
  lastAddFilterValue <- shiny::reactiveVal(0)
  lastDisregardFilterValue <- shiny::reactiveVal(0)

  # Reactive data transformations

  target_mod <- shiny::reactiveVal()
  data_features <- shiny::reactiveVal(NULL)
  data_patients <- shiny::reactiveVal(NULL)
  data_initial <- shiny::reactiveVal(NULL)
  data_person <- shiny::reactiveVal(NULL)
  target_row_annotation <- shiny::reactiveVal(NULL)
  target_col_annotation <- shiny::reactiveVal(NULL)
  target_time_annotation <- shiny::reactiveVal(NULL)
  target_matrix <- shiny::reactiveVal(NULL)

  # No patient data study
  prevalence_plot_data <- shiny::reactiveVal(NULL)
  time_plot_data = shiny::reactiveVal(NULL)
  heatmap_plot_data = shiny::reactiveVal(NULL)
  prevalence_plot_data_correlation <- shiny::reactiveVal(NULL)
  time_plot_data_correlation = shiny::reactiveVal(NULL)
  heatmap_plot_data_correlation = shiny::reactiveVal(NULL)

  # Proxies
  dt_proxy <- DT::dataTableProxy("concept_table")
  target <- shiny::reactiveVal(NULL)
  target_filtered <- shiny::reactiveVal(NULL)
  hierarchySuggestionsTable <- shiny::reactiveVal(NULL)
  correlationSuggestionsTable <- shiny::reactiveVal(NULL)
  last_row_annotation <- shiny::reactiveVal(NULL)

  # Plots' height coefficient
  nrOfConcepts <- shiny::reactive({
    activeConcepts = NULL
    if (isPatientLevelDataPresent()){
      if (is.data.frame(target_row_annotation())) {
        activeConcepts = nrow(target_row_annotation())
      } else {
        0  # Return 0 if data_features() is not yet a dataframe
      }
    } else {
      activeConcepts = nrow(prevalence_plot_data())
    }
    if (is.null(activeConcepts)) 1 else activeConcepts
  })

  # Waiters
  readingStudiesWaiter <- CohortContrast:::createFullScreenWaiter("Loading studies from path, please wait ...")
  readingStudyWaiter <- CohortContrast:::createFullScreenWaiter("Reading in the selected study data, please wait ...")
  loadingStudyWaiter <- CohortContrast:::createFullScreenWaiter("Loading the selected study data, please wait ...")
  filteringStudyWaiter <- CohortContrast:::createFullScreenWaiter("Filtering the data, please wait ...")
  prevalencePlotWaiter <- CohortContrast:::createFullScreenWaiter("Creating the prevalence plot, please wait ...")
  heatmapPlotWaiter <- CohortContrast:::createFullScreenWaiter("Creating the heatmap plot, please wait ...")
  timepanelPlotWaiter <- CohortContrast:::createFullScreenWaiter("Creating the time panel plot, please wait ...")
  trajectoryPlotWaiter <- CohortContrast:::createFullScreenWaiter("Creating the trajectory plot, please wait ...")
  demographicPlotWaiter <- CohortContrast:::createFullScreenWaiter("Creating the demographic plot, please wait ...")
  combineConceptsWaiter <- CohortContrast:::createFullScreenWaiter("Mapping concepts, please wait ...")
  createSnapshotWaiter <- CohortContrast:::createFullScreenWaiter("Creating a snapshot, please wait ...")
  createVisualSnapshotWaiter <- CohortContrast:::createFullScreenWaiter("Creating a visual snapshot, please wait ...")
  createMappingWaiter <- CohortContrast:::createFullScreenWaiter("Creating mapping suggestions, please wait ...")

  # Toggles not available if no patient data present
  shiny::observeEvent(input$autoScaleRate, {
    if (!isPatientLevelDataPresent()) {
      CohortContrast:::showNoPatientDataAllowedWarning()
      shiny.fluent::updateToggle.shinyInput(session, "autoScaleRate", value = FALSE)  # Force toggle back
    }
  })
  shiny::observeEvent(input$applyInverseTarget, {
    if (!isPatientLevelDataPresent()) {
      CohortContrast:::showNoPatientDataAllowedWarning()
      shiny.fluent::updateToggle.shinyInput(session, "applyInverseTarget", value = FALSE)  # Force toggle back
    }
  })
  shiny::observeEvent(input$removeUntreated, {
    if (!isPatientLevelDataPresent()) {
      CohortContrast:::showNoPatientDataAllowedWarning()
      shiny.fluent::updateToggle.shinyInput(session, "removeUntreated", value = FALSE)  # Force toggle back
    }
  })

  ########################################################### LOAD SELECTED DATA
  # Function to load data and update study_info
  load_study_data <- function() {
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
    CohortContrast:::printCustomMessage("COMPLETED: Loading studies from working directory")
  }

  # Initialize data on app start (load once when booting)
  shiny::observe({
    readingStudiesWaiter$show()
    load_study_data()
    readingStudiesWaiter$hide()
  })

  # Study info dataframe
  study_df <- shiny::reactive({
    study_info()
  })
  # Study info dataframe output
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

  # Observe the (study) row selection and update the hidden text input
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
    readingStudyWaiter$show()
    correct_study_name <- input$studyName
    CohortContrast:::printCustomMessage(paste("EXECUTION: Loading study", correct_study_name, "from working directory ...", sep = " "))
    file_path <- stringr::str_c(pathToResults, "/", correct_study_name, ".rds")
    studyName(correct_study_name)
    if (file.exists(file_path)) {
      object <- readRDS(file_path)
      # Fix problems related to column classes
      object$data_patients$PERSON_ID <- CohortContrast:::convert_int64(object$data_patients$PERSON_ID)
      # Ensure that all relevant components are converted to data.table
      object$data_initial <- data.table::as.data.table(object$data_initial)
      object$data_patients <- data.table::as.data.table(object$data_patients)
      object$data_features <- data.table::as.data.table(object$data_features)
      object$data_person <- data.table::as.data.table(object$data_person)
      object$target_matrix <- data.table::as.data.table(object$target_matrix)
      object$target_row_annotation <- data.table::as.data.table(object$target_row_annotation)
      object$target_col_annotation <- data.table::as.data.table(object$target_col_annotation)

      data.table::setDT(object$conceptsData$concept)
      data.table::setDT(object$conceptsData$concept_ancestor)

      # Forward the converted data.tables
      shiny::isolate({
        originalData(list(
          data_initial = object$data_initial,
          data_patients = object$data_patients,
          data_features = object$data_features,
          data_person = object$data_person,
          target_matrix = object$target_matrix,
          target_row_annotation = object$target_row_annotation,
          target_col_annotation = object$target_col_annotation,
          config = object$config,
          compressedDatas = object$compressedDatas,
          prevalence_plot_data = object$compressedDatas[[abstractionLevelReactive()]]$prevalencePlotData,
          time_plot_data = object$compressedDatas[[abstractionLevelReactive()]]$timePlotData,
          heatmap_plot_data = object$compressedDatas[[abstractionLevelReactive()]]$heatmapPlotData,
          conceptsData = object$conceptsData,
          prevalence_plot_data_correlation = NULL,
          time_plot_data_correlation = NULL,
          heatmap_plot_data_correlation = NULL
        ))

        # Set cached data as originalData at start
        cachedData(deepCopyList(originalData()))

        # Update reactive values with the converted data.tables
        data_features(data.table::copy(object$data_features))
        data_patients(data.table::copy(object$data_patients))
        data_initial(data.table::copy(object$data_initial))
        data_person(data.table::copy(object$data_person))
        target_mod(data.table::copy(object$data_features))

        prevalence_plot_data(data.table::copy(object$compressedDatas[[abstractionLevelReactive()]]$prevalencePlotData))
        time_plot_data(data.table::copy(object$compressedDatas[[abstractionLevelReactive()]]$timePlotData))
        heatmap_plot_data(data.table::copy(object$compressedDatas[[abstractionLevelReactive()]]$heatmapPlotData))


        prevalence_plot_data_correlation(NULL)
        time_plot_data_correlation(NULL)
        heatmap_plot_data_correlation(NULL)

        lookbackDays(cachedData()$config$lookbackDays)
        isPatientLevelDataPresent(!isFALSE(cachedData()$config$patientLevelData))

        removeButtonCounter(0)
        removeButtonCounterMap = NULL
        filterButtonCounter(0)
        filterButtonCounterMap = NULL
        lastAddFilterValue(0)
        lastDisregardFilterValue(0)
      })
      if (!(is.null(object$complementaryMappingTable) | isFALSE(object$complementaryMappingTable))) complementaryMappingTable(data.table::copy(object$complementaryMappingTable))
      CohortContrast:::printCustomMessage(paste("COMPLETED: Loading study", correct_study_name, "from working directory", sep = " "))
    } else {
      print("File not found")
    }
    readingStudyWaiter$hide()
  })

  #################################################################### RENDER UI
  # Render UI for abstraction level selection dynamically
  output$reactive_abstraction_lvl <- shiny::renderUI({
    shinyWidgets::sliderTextInput(
      inputId = "abstraction_lvl",
      label = h3("Abstraction level"),
      choices = availableAbstractionLevelsReactive(),  # Use dynamically retrieved levels
      grid = TRUE
    )
  })

  # Extract unique abstraction levels from your dataset
  availableAbstractionLevelsReactive <- shiny::reactive({
    if(is.null(studyName())){
      return(c("original","0","1","2","3","4","5","6","7","8","9","10","source"))
    }
    levels = NULL
    if (isPatientLevelDataPresent()){
      levels = unique(isolate(cachedData()$data_patients$ABSTRACTION_LEVEL))
    } else {
      levels = names(isolate(cachedData()$compressedDatas))
    }
    result <- CohortContrast:::convert_abstraction_levels(levels)
    return(result)
  })

  # Render UI for lookback period dynamically
  output$lookBackSlider <- renderUI({
    shiny::req(lookbackDays()) # Ensure lookbackDays is available
    if (is.numeric(lookbackDays())) {
      sliderInput(
        inputId = "lookback_slider",
        h3("Lookback days"),
        min = -lookbackDays(),
        max = 0,
        value = 0,
        step = ceiling(lookbackDays() / 10)
      )
    } else {
      NULL
    }
  })

  # Observe lookback for reactivity
  shiny::observe({
    shiny::req(studyName())
    if (!is.null(input$lookback_slider)) {
      # Use isolate to prevent unnecessary reactivity
      threshold <- abs(isolate(input$lookback_slider))
      # Filter data based on the slider value
      updated_data <- data.table::copy(originalData()$data_patients) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          TIME_TO_EVENT = list(TIME_TO_EVENT[TIME_TO_EVENT >= lookbackDays() - threshold])
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(lengths(TIME_TO_EVENT) > 0) # Remove rows where TIME_TO_EVENT is empty
      # Update the reactive value without triggering unnecessary reactivity
      isolate({
        data_patients(data.table::as.data.table(updated_data))
      })
    }
  })


  ############################################################## INPUT OBSERVERS

  # Inputs that need a recalculation for cache
  observeEvent(list(input$abstraction_lvl,
                    input$autoScaleRate,
                    input$applyInverseTarget,
                    input$applyZTest,
                    input$applyLogitTest), {
    shiny::req(cachedData())
    abstractionLevelReactive(as.character(ifelse(input$abstraction_lvl == "original" || is.null(input$abstraction_lvl), -1, ifelse(input$abstraction_lvl == "source", -2, as.numeric(input$abstraction_lvl)))))
    autoScaleRate(if (!is.null(input$autoScaleRate) && input$autoScaleRate) TRUE else FALSE)
    applyInverseTarget(if (!is.null(input$applyInverseTarget) && input$applyInverseTarget) TRUE else FALSE)
    applyZTest(if (!is.null(input$applyZTest) && input$applyZTest) TRUE else FALSE)
    applyLogitTest(if (!is.null(input$applyLogitTest) && input$applyLogitTest) TRUE else FALSE)

    # Reset data
    cachedData(deepCopyList(originalData()))
    # Implement active complementary mapping table if PDV
    if (isPatientLevelDataPresent() && is.data.frame(complementaryMappingTable()) && !is.null(cachedData())){
    combineConceptsWaiter$show()
    tempCachedData <- CohortContrast:::implementComplementaryMappingTable(data = cachedData(),
                                                         complementaryMappingTable = complementaryMappingTable(),
                                                         abstractionLevel = abstractionLevelReactive())
    tempCachedData$data_patients$ABSTRACTION_LEVEL = as.numeric(tempCachedData$data_patients$ABSTRACTION_LEVEL)
    cachedData(list(
      data_initial = tempCachedData$data_initial,
      data_patients = tempCachedData$data_patients,
      data_features = tempCachedData$data_features,
      data_person = tempCachedData$data_person,
      target_matrix = tempCachedData$target_matrix,
      target_row_annotation = tempCachedData$target_row_annotation,
      target_col_annotation = tempCachedData$target_col_annotation,
      config = tempCachedData$config
    ))
    combineConceptsWaiter$hide()
    }
    target()
  })

  # Inputs that change cached data only
  observeEvent(list(input$correlationView,
                    input$correlation_threshold,
                    input$removeUntreated,
                    input$domain,
                    input$prevalence,
                    input$prevalence_ratio), {
    conceptPrevalenceRatio(input$prevalence_ratio)
    patientPrevalence(input$prevalence)
    activeDomains(input$domain)
    isRemoveUntreated(input$removeUntreated)
    isCorrelationView(input$correlationView)
    selectedCorrelationGroup(NULL)
    correlationCutoff(if (!is.null(input$correlation_threshold)) input$correlation_threshold else 0.95)
                    })

  ################################################# REACTIVE DATA TRANSFORMATION
  target <- shiny::reactive({
    shiny::req(studyName(), pathToResults)
    loadingStudyWaiter$show()
    result <- NULL
    if (isPatientLevelDataPresent()) {
      result <- CohortContrast:::format_results(
        data = cachedData(),
        autoScaleRate = autoScaleRate(),
        applyInverseTarget = applyInverseTarget(),
        applyZTest = applyZTest(),
        applyLogitTest = applyLogitTest(),
        abstractionLevel = as.numeric(abstractionLevelReactive())
      )
    } else {
      result <- list()
      cachedData(list(
        data_initial = cachedData()$data_initial,
        data_patients = cachedData()$data_patients,
        data_features = cachedData()$data_features,
        data_person = cachedData()$data_person,
        complementaryMappingTable = cachedData()$complementaryMappingTable,
        target_matrix = cachedData()$target_matrix,
        target_row_annotation = cachedData()$target_row_annotation,
        target_col_annotation = cachedData()$target_col_annotation,
        config = cachedData()$config,
        compressedDatas = cachedData()$compressedDatas,
        prevalence_plot_data = cachedData()$compressedDatas[[abstractionLevelReactive()]]$prevalencePlotData,
        time_plot_data = cachedData()$compressedDatas[[abstractionLevelReactive()]]$timePlotData,
        heatmap_plot_data =  cachedData()$compressedDatas[[abstractionLevelReactive()]]$heatmapPlotData,
        prevalence_plot_data_correlation = NULL,
        time_plot_data_correlation = NULL,
        heatmap_plot_data_correlation = NULL
      ))
      printCustomMessage("NOTE: This study has all patient data removed, most features are omitted")
    }
    loadingStudyWaiter$hide()
    result
  })

  target_filtered <- shiny::reactive({
    shiny::req(studyName())
    filteringStudyWaiter$show()
    correlation_cutoff <- correlationCutoff()
    result <- NULL

    if (isPatientLevelDataPresent()) {
      result <- CohortContrast:::filter_target(
        target(),
        prevalence_threshold = patientPrevalence(),
        prevalence_ratio_threshold = conceptPrevalenceRatio(),
        domains = activeDomains(),
        removeUntreated = isRemoveUntreated()
      )
      result <- CohortContrast:::prepare_filtered_target(result, correlation_cutoff)
      target_row_annotation(result$target_row_annotation)
      target_col_annotation(result$target_col_annotation)
      target_time_annotation(result$target_time_annotation)
      target_matrix(result$target_matrix)
      correlationGroups(result$correlation_analysis$groups)

    } else {
      result <- list()
      prevalence_plot_data(if (is.null(cachedData()$prevalence_plot_data)) NULL else cachedData()$prevalence_plot_data %>%
                             dplyr::filter(HERITAGE %in% activeDomains(),
                                           PREVALENCE >= patientPrevalence(),
                                           PREVALENCE_DIFFERENCE_RATIO >= conceptPrevalenceRatio(),
                                           ZTEST >= applyZTest(),
                                           LOGITTEST >= applyLogitTest()))


      time_plot_data(if (is.null(cachedData()$time_plot_data)) NULL else cachedData()$time_plot_data %>%
                       dplyr::filter(HERITAGE %in% activeDomains(),
                                     CONCEPT_NAME %in% unique(prevalence_plot_data()$CONCEPT_NAME))
      )
      heatmap_plot_data(list(
        map = cachedData()$heatmap_plot_data$map,
        correlation_analysis = cachedData()$heatmap_plot_data$correlation_analysis,
        correlation_matrix = cachedData()$heatmap_plot_data$correlation_matrix[as.character(unique(prevalence_plot_data()$CONCEPT_ID)),
                                                                              as.character(unique(prevalence_plot_data()$CONCEPT_ID))]
      ))

      result$prevalence_plot_data = prevalence_plot_data()
      result$time_plot_data = time_plot_data()
      result$heatmap_plot_data = heatmap_plot_data()

      if(isCorrelationView()){
        # Initiate heatmap data transformation

        heatmap_plot_data_correlation(prepare_heatmap_correlation_groups(heatmapData = heatmap_plot_data(), correlation_threshold =  correlationCutoff()))

        groups_list <- heatmap_plot_data_correlation()$correlation_analysis$groups
        conceptToGroup <- do.call(rbind, lapply(seq_along(groups_list), function(i) {
          data.frame(CONCEPT_ID = unlist(groups_list[[i]]), Group = i)
        }))

        prevalence_plot_data_correlation(prevalence_plot_data() %>% dplyr::select(-Group) %>% dplyr::left_join(conceptToGroup, by = "CONCEPT_ID"))
        time_plot_data_correlation(time_plot_data() %>% dplyr::select(-Group) %>% dplyr::left_join(conceptToGroup, by = "CONCEPT_ID"))

        result$heatmap_plot_data_correlation = heatmap_plot_data_correlation()
        result$prevalence_plot_data_correlation = prevalence_plot_data_correlation()
        result$time_plot_data_correlation = time_plot_data_correlation()
      }
    }
    filteringStudyWaiter$hide()
    result
  })

  ################################################################# RENDER PLOTS
  output$prevalencePlot <- shiny::renderPlot({
    prevalencePlotWaiter$show()
    result <- tryCatch({
      if (is.null(studyName()) || is.null(target_filtered())) {
        CohortContrast:::getErrorPlot("Please select a study from the dashboard")
      } else {
        if (isPatientLevelDataPresent()) {
          CohortContrast:::createPrevalencePlot(
            filtered_target = target_filtered(),
            isCorrelationView = isCorrelationView()
          )
        } else {
          if (isCorrelationView()) {
            CohortContrast:::getPrevalencePlotCorrelation(prevalence_plot_data_correlation())
          } else {
            CohortContrast:::getPrevalencePlotRegular(prevalence_plot_data())
          }
        }
      }
    }, error = function(e) {
      print(e)
      CohortContrast:::createPrevalencePlot(filtered_target = NULL, isCorrelationView = FALSE)
    })

    prevalencePlotWaiter$hide()
    result
  }, height = function() min(160 + nrOfConcepts() * 24, 10000))

  output$heatmapPlot <- shiny::renderPlot({
    heatmapPlotWaiter$show()

    result <- tryCatch({
      if (is.null(studyName()) || is.null(target_filtered())) {
        CohortContrast:::getErrorPlot("Please select a study from the dashboard")
      } else {
        if (isPatientLevelDataPresent()) {
          CohortContrast:::createHeatmapPlot(
            filtered_target = target_filtered(),
            isCorrelationView = isCorrelationView()
          )
        } else {
          if (isCorrelationView()) {
            CohortContrast:::getHeatmapPlotCorrelationNoPatientDataAllowed(heatmap_plot_data_correlation())
          } else {
            CohortContrast:::getHeatmapPlotRegularNoPatientDataAllowed(heatmap_plot_data())
          }
        }
      }
    }, error = function(e) {
      print(e)
      CohortContrast:::createHeatmapPlot(filtered_target = NULL, isCorrelationView = FALSE)
    })

    heatmapPlotWaiter$hide()
    result
  }, height = function() min(160 + nrOfConcepts() * 24, 10000))

  output$time_panelPlot <- shiny::renderPlot({
    timepanelPlotWaiter$show()

    result <- tryCatch({
      if (is.null(studyName()) || is.null(target_filtered())) {
        CohortContrast:::getErrorPlot("Please select a study from the dashboard")
      } else {
        if (isPatientLevelDataPresent()) {
          CohortContrast:::createTimePlot(
            target_filtered(),
            isCorrelationView = isCorrelationView()
          )
        } else {
          if (isCorrelationView()) {
            CohortContrast:::getTimePlotCorrelation(time_plot_data_correlation())
          } else {
            CohortContrast:::getTimePlotRegular(time_plot_data())
          }
        }
      }
    }, error = function(e) {
      print(e)
      CohortContrast:::createTimePlot(NULL)
    })

    timepanelPlotWaiter$hide()
    result
  }, height = function() min(160 + nrOfConcepts() * 24, 10000))


  output$dynamicTrajectoryGraphUI <- shiny::renderUI({
    trajectoryPlotWaiter$show()
    result <- NULL

    if (is.null(studyName()) || is.null(target_filtered())) {
      output$errorPlotTrajectory <- shiny::renderPlot({ CohortContrast:::getErrorPlot("Please select a study from the dashboard") })
      result <- shiny::plotOutput("errorPlotTrajectory")
    } else if (!isPatientLevelDataPresent()) {
      output$errorPlotTrajectory <- shiny::renderPlot({ CohortContrast:::getErrorPlot("This feature is not supported without patient level data") })
      result <- shiny::plotOutput("errorPlotTrajectory")
    } else if ((is.null(selectedCorrelationGroup()) || identical(selectedCorrelationGroup(), character(0))) && isCorrelationView()) {
      output$errorPlotTrajectory <- shiny::renderPlot({ CohortContrast:::getErrorPlot("A correlation group must be selected") })
      result <- shiny::plotOutput("errorPlotTrajectory")
    } else {
      result <- generateBPMNTrajectoryViewer(
        filtered_target = target_filtered(),
        edge_prevalence_threshold = input$edge_prevalence_threshold,
        selection_list = selectedTrajectoryFilters$data,
        selected_ids = selectedCorrelationGroup(),
        is_patient_level_data_present = isPatientLevelDataPresent(),
        is_corr_initiated = isCorrelationView()
      )
    }
    trajectoryPlotWaiter$hide()
    result
  })


  ################################################################# DEMOGRPAHICS

  output$dynamicDemographicAgeGraphUI <- shiny::renderUI({
    demographicPlotWaiter$show()
    result <- NULL

    if (is.null(studyName()) || is.null(target_filtered())) {
      output$dynamicDemographicAgePlots <- shiny::renderPlot({ CohortContrast:::getErrorPlot("Please select a study from the dashboard") })
      result <- shiny::plotOutput("dynamicDemographicAgePlots")
    } else if (!isPatientLevelDataPresent()) {
      output$dynamicDemographicAgePlots <- shiny::renderPlot({ CohortContrast:::getErrorPlot("This feature is not supported without patient level data") })
      result <- shiny::plotOutput("dynamicDemographicAgePlots")
    } else {

      plot_data <- CohortContrast:::getDemographyPlotData(data_patients = data_patients(), data_person = data_person(), data_initial = data_initial())
      plot <- CohortContrast:::getDemographyAgeAtIndexPlot(plot_data = plot_data)

      #combined_plot <- plot_target + plot_control + patchwork::plot_layout(ncol = 1)
      output$dynamicDemographicAgePlots <- shiny::renderPlot({ plot })

      result <- shiny::plotOutput("dynamicDemographicAgePlots", height = "700px", width = "100%")
    }
    demographicPlotWaiter$hide()
    result
  })

  output$dynamicDemographicYearOfBirthGraphUI <- shiny::renderUI({
    demographicPlotWaiter$show()
    result <- NULL
    if (is.null(studyName()) || is.null(target_filtered())) {
      output$demographicYearOfBirthPlots <- shiny::renderPlot({ CohortContrast:::getErrorPlot("Please select a study from the dashboard") })
      result <- shiny::plotOutput("demographicYearOfBirthPlots")
    } else if (!isPatientLevelDataPresent()) {
      output$demographicYearOfBirthPlots <- shiny::renderPlot({ CohortContrast:::getErrorPlot("This feature is not supported without patient level data") })
      result <- shiny::plotOutput("demographicYearOfBirthPlots")
    } else {

    plot_data <- CohortContrast:::getDemographyPlotData(data_patients = data_patients(), data_person = data_person(), data_initial = data_initial())
    plot <- CohortContrast:::getDemographyYearOfBirthPlot(plot_data = plot_data)

   # combined_plot <- plot_target + plot_control + patchwork::plot_layout(ncol = 1)
      output$demographicYearOfBirthPlots <- shiny::renderPlot({ plot })

      result <- shiny::plotOutput("demographicYearOfBirthPlots", height = "700px", width = "100%")
    }
    demographicPlotWaiter$hide()
    result
  })

  output$dynamicDemographicCompareGraphUI <- shiny::renderUI({
    demographicPlotWaiter$show()
    result <- NULL

    if (is.null(studyName()) || is.null(target_filtered())) {
      output$demographicComparePlots <- shiny::renderPlot({
        CohortContrast:::getErrorPlot("Please select a study from the dashboard")
      })
      result <- shiny::plotOutput("demographicComparePlots")

    } else if (!isPatientLevelDataPresent()) {
      output$demographicComparePlots <- shiny::renderPlot({
        CohortContrast:::getErrorPlot("This feature is not supported without patient level data")
      })
      result <- shiny::plotOutput("demographicComparePlots")

    }
    else if (isCorrelationView()) {

      firstGroupPersonIds = NULL
      secondGroupPersonIds = NULL
      if (is.null(selectedFirstDemographicGroup()) || identical(selectedFirstDemographicGroup(), character(0))){
        # firstGroupPersonIds = target_col_annotation()$SUBJECT_ID
      } else {
        firstGroupPersonIds = data_patients() %>% dplyr::filter(.data$CONCEPT_ID %in% selectedFirstDemographicGroup(),
                                                                .data$COHORT_DEFINITION_ID == "target") %>% dplyr::pull(.data$PERSON_ID) %>% unique()
      }

      if (is.null(selectedSecondDemographicGroup()) || identical(selectedFirstDemographicGroup(), character(0))){
        # secondGroupPersonIds = target_col_annotation()$SUBJECT_ID
      } else {
        secondGroupPersonIds = data_patients() %>% dplyr::filter(.data$CONCEPT_ID %in% selectedSecondDemographicGroup(),
                                                                .data$COHORT_DEFINITION_ID == "target") %>% dplyr::pull(.data$PERSON_ID) %>% unique()
      }
      plot_data <- CohortContrast:::getDemographyPlotData(
        data_patients = data_patients(),
        data_person = data_person(),
        data_initial = data_initial(),
        groupOneIds = firstGroupPersonIds,
        groupTwoIds = secondGroupPersonIds
      )

      plot <- CohortContrast:::getDemographyAgeAtIndexPlot(plot_data = plot_data)

      output$demographicComparePlots <- shiny::renderPlot({ plot })

      result <- shiny::plotOutput("demographicComparePlots", height = "700px", width = "100%")

    } else {
      firstGroupPersonIds <- data_patients() %>% dplyr::filter(.data$CONCEPT_ID %in% rownames(target_row_annotation()),
                                                               .data$COHORT_DEFINITION_ID == "target") %>% dplyr::pull(.data$PERSON_ID) %>% unique()

      plot_data <- CohortContrast:::getDemographyPlotData(
        data_patients = data_patients(),
        data_person = data_person(),
        data_initial = data_initial(),
        groupOneIds = firstGroupPersonIds
      )
      plot <- CohortContrast:::getDemographyAgeAtIndexPlot(plot_data = plot_data,
                                                           title = "Relative Population Distribution by Gender and Age at Index. Active concepts vs Target")

      output$demographicComparePlots <- shiny::renderPlot({ plot })

      result <- shiny::plotOutput("demographicComparePlots", height = "700px", width = "100%")
    }
    demographicPlotWaiter$hide()
    result
  })

  output$dynamic_demographic_widgets <- renderUI({
    if (isTRUE(isCorrelationView())) {
      # Pickers have to me moved out because they will be always reinitialized
      result <- shiny::tagList(shiny::fluidRow(
        shiny::column(6,
                      shinyWidgets::pickerInput(
                        inputId = "filter_demogrpahic_group_1",
                        label = "Select first correlation group",
                        choices = correlationGroupChoices(),  # Dynamically updated
                        choicesOpt = correlationGroupChoicesLabels(), # Dynamically updated
                        multiple = FALSE,
                        options = shinyWidgets::pickerOptions(
                          liveSearch = TRUE,
                          title = "Search & select a group",
                          header = "Select a group",
                          style = "btn-primary"
                        )
                      )
        ),
        shiny::column(6,
                      shinyWidgets::pickerInput(
                        inputId = "filter_demogrpahic_group_2",
                        label = "Select second correlation group",
                        choices = correlationGroupChoices(),  # Dynamically updated
                        choicesOpt = correlationGroupChoicesLabels(), # Dynamically updated
                        multiple = FALSE,
                        options = shinyWidgets::pickerOptions(
                          liveSearch = TRUE,
                          title = "Search & select a group",
                          header = "Select a group",
                          style = "btn-primary"
                        )
                      )
        )
      )
      )
    }
  })

  shiny::observeEvent(input$filter_demogrpahic_group_1, {
    # Retrieve the selected group
    if(isCorrelationView()){
      selected <- input$filter_demogrpahic_group_1
      # Initialize a variable to store selected IDs
      selected_ids <- NULL

      if (!is.null(selected)) {
        # Split the selected group value into individual IDs
        selected_ids <- unlist(strsplit(selected, ";"))
      }
      # Update the reactive value with the selected IDs
      selectedFirstDemographicGroup(selected_ids)
    }
  })

  shiny::observeEvent(input$filter_demogrpahic_group_2, {
    # Retrieve the selected group
    if(isCorrelationView()){
      selected <- input$filter_demogrpahic_group_2
      # Initialize a variable to store selected IDs
      selected_ids <- NULL

      if (!is.null(selected)) {
        # Split the selected group value into individual IDs
        selected_ids <- unlist(strsplit(selected, ";"))
      }
      # Update the reactive value with the selected IDs
      selectedSecondDemographicGroup(selected_ids)
    }
  })

  ############################################################## VISUAL SNAPSHOT
  shiny::observeEvent(input$visual_snapshot, {
    if(isPatientLevelDataPresent()){
      shiny::showModal(shiny::modalDialog(
        title = "Save snapshot",
        textInput("new_visual_snapshot_name", "Enter New Snapshot Name:", ""),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("accept_btn_visual_snaphot", "Accept")
        )
      ))
    } else {
      CohortContrast:::showNoPatientDataAllowedWarning()
    }
  })

  shiny::observeEvent(input$accept_btn_visual_snaphot, {
    createVisualSnapshotWaiter$show()
    shiny::removeModal()
    CohortContrast:::createPathToResults(paste0(pathToResults, "/visual_snapshots"))
    fileName <- paste(pathToResults, "/visual_snapshots/",CohortContrast:::sanitize_single(input$new_visual_snapshot_name),".rds", sep = "")
    config <- cachedData()$config
    config$prevalenceCutOff <- conceptPrevalenceRatio()
    config$presenceFilter <- patientPrevalence()
    config$domainsIncluded <- activeDomains()
    temp_filtered_target = list(target_matrix = target_matrix(),
                                target_row_annotation = target_row_annotation(),
                                target_col_annotation = target_col_annotation(),
                                target_time_annotation = target_time_annotation()
    )
    temp_filtered_target = CohortContrast:::prepare_filtered_target(temp_filtered_target, correlationCutoff())
    ccObject <- list(
      data_features = data_features() %>% dplyr::filter(.data$CONCEPT_ID %in% as.integer(rownames(target_matrix())),
                                                        .data$ABSTRACTION_LEVEL == as.numeric(abstractionLevelReactive())),
      filtered_target = temp_filtered_target,
      config = config
    )
    CohortContrast:::save_object(object = ccObject, path = fileName)
    createVisualSnapshotWaiter$hide()
  })

  ################################################################ MAPPING TABLE
  shiny::observe({
    shiny::req(data_features())
    target_mod(data_features())
  })

  filtered_data <- shiny::reactive({
    # Filter the data based on the user's selected abstraction level
    shiny::req(abstractionLevelReactive(), studyName()) # Ensure the input is available before proceeding

     filtered_data <- target_mod()[
       ABSTRACTION_LEVEL == as.numeric(abstractionLevelReactive())
    ]
     return(filtered_data)
  })

  output$concept_table <- DT::renderDT({
    shiny::validate(
      shiny::need(studyName(), "")
    )

    col_order <- c(
      "CONCEPT_ID", "CONCEPT_NAME", "ABSTRACTION_LEVEL", "HERITAGE",
      "TARGET_SUBJECT_COUNT", "CONTROL_SUBJECT_COUNT", "TIME_TO_EVENT",
      "TARGET_SUBJECT_PREVALENCE", "CONTROL_SUBJECT_PREVALENCE",
      "PREVALENCE_DIFFERENCE_RATIO", "ZTEST", "ZTEST_P_VALUE",
      "LOGITTEST", "LOGITTEST_P_VALUE", "KSTEST", "KSTEST_P_VALUE"
    )
    filtered_data_reordered <- filtered_data()[, ..col_order, drop = FALSE]

    DT::datatable(
      filtered_data_reordered,
      selection = 'multiple',
      filter = 'top',
      options = list(
        columnDefs = list(
          list(targets = which(names(filtered_data_reordered) %in% c("TIME_TO_EVENT", "ABSTRACTION_LEVEL")), visible = FALSE)
        )
      )
    )
  }, server = TRUE)

  output$mapping_history_table <- DT::renderDT({
    shiny::validate(
      shiny::need(studyName(), "Please select a study from the Dashboard.")
    )

    DT::datatable(
      complementaryMappingTable(),
      selection = 'none',
      filter = 'top',
    )
  }, server = TRUE)

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
    combineConceptsWaiter$show()
    shiny::removeModal()
    new_concept_name <- input$new_concept_name

    processed_table <- target_mod()[ABSTRACTION_LEVEL == as.numeric(abstractionLevelReactive())]
    selected_concept_ids <- as.numeric(processed_table$CONCEPT_ID[input$concept_table_rows_selected])
    combineSelectedConcepts(new_concept_name = new_concept_name, selected_ids = selected_concept_ids, type = "custom")
    target_mod(data_features())

    shiny::updateCheckboxInput(session, "dt_select_all", "Select all", value = F)
    combineConceptsWaiter$hide()
    target()
  })
  # Rename concept
  shiny::observeEvent(input$accept_renamed_btn, {
    shiny::removeModal()
    new_concept_name <- input$new_renamed_concept_name
    processed_table <- target_mod()[ABSTRACTION_LEVEL == as.numeric(abstractionLevelReactive())]
    selected_concept_id <- as.numeric(processed_table$CONCEPT_ID[input$concept_table_rows_selected])
    renameSelectedConcept(new_concept_name = new_concept_name, selected_concept_id = selected_concept_id)
    target_mod(data_features())
    target()
  })

  # Reset data to original
  shiny::observeEvent(input$reset_btn_mappings, {
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else {
      # Restore original state
      cachedData(deepCopyList(originalData()))
      data_features(data.table::copy(originalData()$data_features))
      data_patients(data.table::copy(originalData()$data_patients))
      data_initial(data.table::copy(originalData()$data_initial))
      target_mod(data.table::copy(originalData()$data_features))
      target_row_annotation(data.table::copy(originalData()$target_row_annotation))
      target_col_annotation(data.table::copy(originalData()$target_col_annotation))
      target_time_annotation(data.table::copy(originalData()$target_time_annotation))
      target_matrix(data.table::copy(originalData()$target_matrix))
      complementaryMappingTable(if (!(is.null(originalData()$complementaryMappingTable) | isFALSE(originalData()$complementaryMappingTable))) data.table::copy(originalData()$complementaryMappingTable) else data.frame(CONCEPT_ID = integer(), CONCEPT_NAME = character(), NEW_CONCEPT_ID = integer(), NEW_CONCEPT_NAME = character(), ABSTRACTION_LEVEL = integer(), TYPE = character(), stringsAsFactors = FALSE))
      }
  })
  ########################################################## MAPPING SUGGESTIONS

  observeEvent(list(input$mappingControlPanel), {
      # Example: Call a function to update data
    if (input$mappingControlPanel %in% c("hierarchy_suggestions_tab", "correlation_suggestions_tab") && isPatientLevelDataPresent()) {
      # Get current row names
      current_row_names <- rownames(target_row_annotation())
      # Check if rownames have changed
      if (!identical(current_row_names, last_row_annotation())) {
        createMappingWaiter$show()
        last_row_annotation(current_row_names)
        updateHierarchySuggestions(allowOnlyActiveChildren = input$allowOnlyActiveChildrenHierarchyMappingToggle,
                                   isOnlyActiveParentsAllowed = input$allowOnlyActiveParentsHierarchyMappingToggle)
        updateCorrelationSuggestions()
        createMappingWaiter$hide()
      }
    }
  })

  observeEvent(list(input$allowOnlyActiveParentsHierarchyMappingToggle, input$allowOnlyActiveChildrenHierarchyMappingToggle), {
    shiny::req(hierarchySuggestionsTable())
    createMappingWaiter$show()
    updateHierarchySuggestions(allowOnlyActiveChildren = input$allowOnlyActiveChildrenHierarchyMappingToggle,
                               isOnlyActiveParentsAllowed = input$allowOnlyActiveParentsHierarchyMappingToggle)
    createMappingWaiter$hide()
  })

  output$mapping_hierarchy_suggestions_concept_table <- DT::renderDT({
    shiny::req(studyName(), hierarchySuggestionsTable())

    mapping_data = hierarchySuggestionsTable()

    col_order <- c(
      'PARENT_NAME', 'COUNT', 'CONCEPT_NAMES'
    )
    mapping_data_reordered <- mapping_data[, ..col_order, drop = FALSE]

    DT::datatable(
      mapping_data_reordered,
      selection = 'multiple',
      filter = 'top',
    )
  }, server = TRUE)


  shiny::observeEvent(input$combine_hierarchy_suggestion_btn, {
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else if (length(input$mapping_hierarchy_suggestions_concept_table_rows_selected) > 0) {
      combineConceptsWaiter$show()
      for (rowId in input$mapping_hierarchy_suggestions_concept_table_rows_selected) {
        selected_concept_ids <- as.numeric(unlist(hierarchySuggestionsTable()$CONCEPT_IDS[rowId]))
        selected_parent_id <- as.numeric(hierarchySuggestionsTable()$PARENT_ID[rowId])
        selected_parent_name <- hierarchySuggestionsTable()$PARENT_NAME[rowId]
        combineSelectedConcepts(new_concept_name = selected_parent_name,
                                new_concept_id =  selected_parent_id,
                                selected_ids = selected_concept_ids,
                                type = "hierarchy")
      }
      target_mod(data_features())
      target_filtered()
      updateHierarchySuggestions(allowOnlyActiveChildren = input$allowOnlyActiveChildrenHierarchyMappingToggle,
                                 isOnlyActiveParentsAllowed = input$allowOnlyActiveParentsHierarchyMappingToggle)
      combineConceptsWaiter$hide()
    } else {
      shiny::showNotification("Select at least one row to combine", type = "warning")
    }
  })

  shiny::observeEvent(input$combine_hierarchy_suggestion_automatic_btn, {
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else {
      combineConceptsWaiter$show()
      mappingsToExecute <- CohortContrast:::filterHeritagePriorityMappings(hierarchySuggestionsTable())
      apply(mappingsToExecute, 1, function(row) {

        selected_concept_ids <- as.numeric(unlist(row['CONCEPT_IDS']))
        if(!any(is.na(selected_concept_ids))){
        selected_parent_id <- as.numeric(row['PARENT_ID'])
        selected_parent_name <- as.character(row['PARENT_NAME'])
        combineSelectedConcepts(new_concept_name = selected_parent_name,
                                new_concept_id =  selected_parent_id,
                                selected_ids = selected_concept_ids,
                                type = "hierarchy")
      }
        })
      target_mod(data_features())
      target_filtered()
      updateHierarchySuggestions(allowOnlyActiveChildren = input$allowOnlyActiveChildrenHierarchyMappingToggle,
                                 isOnlyActiveParentsAllowed = input$allowOnlyActiveParentsHierarchyMappingToggle)
      combineConceptsWaiter$hide()
    }
  })

  output$mapping_correlation_suggestions_concept_table <- DT::renderDT({
    shiny::req(studyName(), correlationSuggestionsTable())

    mapping_data = correlationSuggestionsTable()
    mapping_data <- mapping_data[order(-CORRELATION)]
    correlationSuggestionsTable(mapping_data)
    col_order <- c(
      'CONCEPT_NAMES', 'CORRELATION', 'P_VALUE', 'MEDIAN_DAYS_INBETWEEN'
    )
    mapping_data_reordered <- mapping_data[, ..col_order, drop = FALSE]

    DT::datatable(
      mapping_data_reordered,
      selection = 'single',
      filter = 'top',
    )
  }, server = TRUE)

  shiny::observeEvent(input$combine_correlation_suggestion_btn, {
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else if (length(input$mapping_correlation_suggestions_concept_table_rows_selected) > 0) {
      combineConceptsWaiter$show()
      rowId = input$mapping_correlation_suggestions_concept_table_rows_selected
        selected_concept_ids <- as.numeric(unlist(correlationSuggestionsTable()$CONCEPT_IDS[rowId]))
        selected_concept_names <- unlist(correlationSuggestionsTable()$CONCEPT_NAMES[rowId])
        selected_parent_id <-  selected_concept_ids[1]
        selected_parent_name <- selected_concept_names[1]
        combineSelectedConcepts(new_concept_name = selected_parent_name,
                                new_concept_id =  selected_parent_id,
                                selected_ids = selected_concept_ids,
                                type = "correlation")
        target_mod(data_features())
        target_filtered()
        updateCorrelationSuggestions()

        combineConceptsWaiter$hide()

    } else {
      shiny::showNotification("Select at least one row to combine", type = "warning")
    }
  })

  shiny::observeEvent(input$combine_correlation_suggestion_automatic_btn, {
    combineConceptsWaiter$show()
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else {
      mappingsToExecute <- CohortContrast:::filterCorrelationMappings(correlationSuggestionsTable(), data_features = data_features(), minCorrelation = input$combine_correlation_suggestion_automatic_correlation_threshold, maxDaysInbetween = input$combine_correlation_suggestion_automatic_days_threshold)
      apply(mappingsToExecute, 1, function(row) {

        selected_concept_ids <- as.numeric(unlist(row['CONCEPT_IDS']))
        if(!any(is.na(selected_concept_ids))){
          selected_concept_names <- unlist(row['CONCEPT_NAMES'])
          selected_parent_id <-  selected_concept_ids[1]
          selected_parent_name <- selected_concept_names[1]
          combineSelectedConcepts(new_concept_name = selected_parent_name,
                                  new_concept_id =  selected_parent_id,
                                  selected_ids = selected_concept_ids,
                                  type = "correlation")
        }
      })

      target_filtered()
      target_mod(data_features())
      updateCorrelationSuggestions()
    }
    combineConceptsWaiter$hide()
  })
  ############################################################# CORRELATION VIEW
  shiny::observeEvent(input$accept_corr_btn, {
      shiny::removeModal()
      new_concept_name <- input$new_concept_name
      combineSelectedConcepts(new_concept_name = new_concept_name, selected_ids = selectedCorrelationGroup(), type = "correlation")
      target_mod(data_features())

      cachedData(list(
        data_initial = data_initial(),
        data_patients = data_patients(),
        data_features = data_features(),
        data_person = data_person(),
        complementaryMappingTable = cachedData()$complementaryMappingTable,
        target_matrix = cachedData()$target_matrix,
        target_row_annotation = cachedData()$target_row_annotation,
        target_col_annotation = cachedData()$target_col_annotation,
        config = cachedData()$config,
        compressedDatas = cachedData()$compressedDatas,
        prevalence_plot_data = cachedData()$prevalence_plot_data,
        time_plot_data = cachedData()$time_plot_data,
        heatmap_plot_data = cachedData()$heatmap_plot_data,
        prevalence_plot_data_correlation = cachedData()$prevalence_plot_data_correlation,
        time_plot_data_correlation = cachedData()$time_plot_data_correlation,
        heatmap_plot_data_correlation = cachedData()$heatmap_plot_data_correlation
      ))

      shiny::updateCheckboxInput(session, "dt_select_all", "Select all", value = F)
    })

  output$dynamic_correlation_widgets <- renderUI({
      if (isTRUE(isCorrelationView())) {
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
            actionButton("combine_corr_btn", "Combine Group Concepts")
          )
        )
      }
    })


  # Server-side rendering of the pickerInput
  output$correlation_group_selection <- shiny::renderUI({
      shinyWidgets::pickerInput(
        inputId = "filter_correlation_group",
        label = "Select correlation group",
        choices = correlationGroupChoices(),  # Dynamically updated
        choicesOpt = correlationGroupChoicesLabels(), # Dynamically updated
        multiple = FALSE,
        options = shinyWidgets::pickerOptions(
          liveSearch = TRUE, # Enable live search in the dropdown
          title = "Search & select a group",
          header = "Select a group",
          style = "btn-primary"
        )
      )
    })

  # Monitor correlationGroups() for changes
  shiny::observe({
      shiny::req(studyName(), isCorrelationView() == TRUE)
      # Fetch the updated correlation groups
      groups <- correlationGroups()
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

      correlationGroupChoices(unlist(data_groups))
      correlationGroupChoicesLabels(list(
        content = unname(unlist(lapply(data_groups, names))) # Add HTML labels
      ))
    })

  # selected ids
  shiny::observeEvent(input$filter_correlation_group, {
      # Retrieve the selected group
    if(isCorrelationView()){
      selected <- input$filter_correlation_group
      # Initialize a variable to store selected IDs
      selected_ids <- NULL

      if (!is.null(selected)) {
        # Split the selected group value into individual IDs
        selected_ids <- unlist(strsplit(selected, ";"))
      }
      # Update the reactive value with the selected IDs
      selectedCorrelationGroup(selected_ids)
      selectedCorrelationGroupLabels(getTrimmedConceptIDLabels(ids = selected_ids, data = data_features(), absLvl = as.numeric(abstractionLevelReactive())))
    }
      })


  shiny::observeEvent(input$combine_btn, {
   if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else if (length(input$concept_table_rows_selected) > 1) {
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

  shiny::observeEvent(input$rename_btn, {
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else if (length(input$concept_table_rows_selected) > 1) {
      shiny::showNotification("Select only one row", type = "warning")
    } else if (length(input$concept_table_rows_selected) < 1) {
      shiny::showNotification("Select one row from the concept table for renaming", type = "warning")
    } else {
      shiny::showModal(shiny::modalDialog(
        title = "Combine Concepts",
        textInput("new_renamed_concept_name", "Enter New Concept Name:", ""),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("accept_renamed_btn", "Accept")
        )
      ))
    }
  })


  shiny::observeEvent(input$combine_corr_btn, {
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else if (!is.null(selectedCorrelationGroup())) {
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


######################################################### TRAJECTORY FILTERING
  shiny::observe({
    state_names <- selectedCorrelationGroupLabels()$labelsTrimmed
    if (length(state_names) > 0 && length(selectedTrajectoryFilters$data) == 0) {
      for (state in state_names) {
        selectedTrajectoryFilters$data[[state]] <- "All events"
      }
    }
  })

  # Apply "Select All" when button is clicked
  shiny::observeEvent(input$apply_select_all, {
    state_names <- selectedCorrelationGroupLabels()$labelsTrimmed
    selected_value <- input$select_all

    for (state in state_names) {
      selectedTrajectoryFilters$data[[state]] <- selected_value
    }
  })

  # Handle individual selection updates
  shiny::observeEvent(input$radio_selection, {
    state_name <- input$radio_selection$state
    selected_value <- input$radio_selection$value
    # Update the specific state without resetting everything
    selectedTrajectoryFilters$data[[state_name]] <- selected_value
  })


  output$trajectory_view_table <- DT::renderDT({
    state_names <- selectedCorrelationGroupLabels()$labelsTrimmed
    if (length(state_names) == 0) return(NULL)

    choices <- c("First occurrence", "All events", "Last occurrence")

    # Function to generate radio button HTML
    generateRadioButtons <- function(inputId, selected_value) {
      paste0(
        '<input type="radio" name="', inputId, '" value="First occurrence" ',
        ifelse(selected_value == "First occurrence", "checked", ""),
        ' onclick="updateRadioValue(\'', inputId, '\', \'First occurrence\')"> First occurrence ',

        '<input type="radio" name="', inputId, '" value="All events" ',
        ifelse(selected_value == "All events", "checked", ""),
        ' onclick="updateRadioValue(\'', inputId, '\', \'All events\')"> All events ',

        '<input type="radio" name="', inputId, '" value="Last occurrence" ',
        ifelse(selected_value == "Last occurrence", "checked", ""),
        ' onclick="updateRadioValue(\'', inputId, '\', \'Last occurrence\')"> Last occurrence '
      )
    }
    df <- data.frame(
      State = state_names,
      Selection = sapply(state_names, function(state) {
        inputId <- state

        # Always prioritize manual selections
        selected_value <- if (!is.null(selectedTrajectoryFilters$data[[state]])) {
          selectedTrajectoryFilters$data[[state]]  # Use the manually selected value
        } else {
          "All events"  # Default value for newly added states
        }

        generateRadioButtons(inputId, selected_value)
      }),
      stringsAsFactors = FALSE
    )

    DT::datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        paging = FALSE,
        ordering = FALSE
      )
    )
  })


  output$dynamic_selectize_trajectory_inputs <- shiny::renderUI({
    if (is.null(studyName()) || is.null(target_filtered()) || isFALSE(isPatientLevelDataPresent())) {
      return(NULL)
    }
    if (!isCorrelationView()) {
      selected_ids = as.character(rownames(target_row_annotation()))
      selectedCorrelationGroup(selected_ids)
      selectedCorrelationGroupLabels(getTrimmedConceptIDLabels(ids = selected_ids, data = data_features(), absLvl = as.numeric(abstractionLevelReactive())))
    }
    state_names <- selectedCorrelationGroupLabels()$labelsTrimmed
    n <- length(state_names)
    half <- ceiling(n / 2)
    col1_states <- state_names[1:half]
    col2_states <- state_names[(half + 1):n]

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(12, shiny::h4("Filter trajectory states", style = "margin-top: 10px; margin-left: 20px;")),
        shiny::column(
          width = 4,
          style = "margin-left: 20px;",
          shiny::sliderInput(
            "edge_prevalence_threshold",
            label = "Edge prevalence cutoff",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.01
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          style = "margin-left: 20px;",
          shiny::radioButtons(
            "select_all", "Apply to all:",
            choices = c("First occurrence", "All events", "Last occurrence"),
            selected = "All events",
            inline = TRUE
          ),
          shiny::actionButton("apply_select_all", "Apply Selection to All")
        )
      ),
      DT::DTOutput("trajectory_view_table")
    )
  })

  ################################################################ SAVE SNAPSHOT
  # Save data snapshot to file on button press
  shiny::observeEvent(input$save_btn, {
    createSnapshotWaiter$show()
    if(isPatientLevelDataPresent()){
      # Create the base file path
      CohortContrast:::createPathToResults(paste0(pathToResults, "/snapshots"))
      file_base <- stringr::str_c(pathToResults, "/snapshots/")
      file_study_name <- stringr::str_c(studyName(), "_Snapshot")
      file_path <- stringr::str_c(file_base, file_study_name,".rds")
      counter <- 1

      # Check if file exists and append increasing numbers if necessary
      while (file.exists(file_path)) {
        file_study_name <- stringr::str_c(file_study_name, "_", counter)
        file_path <- stringr::str_c(file_base, file_study_name,".rds")
        counter <- counter + 1
      }

      temp_filtered_target = list(target_matrix = target_matrix(),
                                  target_row_annotation = target_row_annotation(),
                                  target_col_annotation = target_col_annotation(),
                                  target_time_annotation = target_time_annotation()
      )
      temp_filtered_target = CohortContrast:::prepare_filtered_target(temp_filtered_target, correlationCutoff())
      # Extract the data from the reactive
      ccObject <- list(
        data_initial = data_initial(),
        data_patients = data_patients(),
        data_features = data_features(),
        data_person = data_person(),
        conceptsData = originalData()$conceptsData,
        filtered_target = temp_filtered_target,
        complementaryMappingTable = complementaryMappingTable(),
        config = cachedData()$config
      )
      # Extract trajectory generation info from session
      selectedFeatureIds = as.integer(ccObject$filtered_target$target_row_annotation %>% rownames())
      ccObject$config$abstractionLevel = as.numeric(abstractionLevelReactive())
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
      CohortContrast:::save_object_metadata(object = ccObject, path = file_path, studyName = file_study_name)
      # Notify the user
      shiny::showNotification(paste("Data saved to", file_path))
    } else {
      CohortContrast:::showNoPatientDataAllowedWarning()
    }
    createSnapshotWaiter$hide()
  })

  ################################################## FILTERING (REMOVE CONCEPTS)

  # Server-side selectize input for filtering by CONCEPT_ID and CONCEPT_NAME
  output$dynamic_concepts_removal <- shiny::renderUI({
    shiny::validate(
      shiny::need(studyName(), "Please select a study from the Dashboard.")
    )
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
  # Bug fix if going to "Mapping" before
  shiny::outputOptions(output, "dynamic_concepts_removal", suspendWhenHidden = FALSE)

  # Server-side search for selectizeInput
  shiny::observeEvent(list(input$search_query_filtering,
                           abstractionLevelReactive()), {
    shiny::req(studyName())
    query_filtering <- input$search_query_filtering
    data_query_filtering <- data_features()[
      ABSTRACTION_LEVEL == as.numeric(abstractionLevelReactive())
    ]

    # Filter the choices based on the search query
    matched_choices_filtering <- stats::setNames(
      data_query_filtering$CONCEPT_ID,
      paste0(data_query_filtering$CONCEPT_NAME, " (", data_query_filtering$CONCEPT_ID, ")")
    )

    # Send the matched choices to the selectizeInput
    shiny::updateSelectizeInput(session, "filter_concept_id", choices = matched_choices_filtering, server = TRUE)
  })

  # Observe when the "Add Filter" button is clicked
  shiny::observeEvent(input$add_filter, {
    # Retrieve current filters
    filters <- selectedFilters()
    # Get the selected CONCEPT_NAME based on the selected CONCEPT_ID
    concept_name <- data_features()$CONCEPT_NAME[
      data_features()$CONCEPT_ID == input$filter_concept_id
    ][1]

    # Check if the pair is unique and add it to the list
    if (!(input$filter_concept_id %in% sapply(filters, `[[`, "id"))) {
      filters <- append(filters, list(list(id = input$filter_concept_id, name = concept_name)))
      selectedFilters(filters)
    }
    # }
  })

  output$selected_filters_list <- shiny::renderUI({
    filters <- selectedFilters()
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
    shiny::req(selectedFilters())
    filters <- selectedFilters()
    current_ids <- names(removeButtonCounterMap)
    # Loop through the counter map to create or update observers for each remove button
    lapply(current_ids, function(counter) {
      if (!is.null(removeButtonCounterMap[[counter]])) {
        shiny::observeEvent(input[[paste0("remove_filter_", counter)]],
                            {
                              filter_id_to_remove <- removeButtonCounterMap[[counter]]
                              # Remove the filter from the list
                              updated_filters <- filters[!sapply(filters, function(f) f$id == filter_id_to_remove)]
                              selectedFilters(updated_filters)


                              data_features <- data_features()
                              data_patients <- data_patients()

                              data_features_original <- data.table::copy(originalData()$data_features)
                              data_patients_original <- data.table::copy(originalData()$data_patients)

                              data_features_restore <- data_features_original[data_features_original$CONCEPT_ID == removeButtonCounterMap[[counter]], ]
                              data_patients_restore <- data_patients_original[data_patients_original$CONCEPT_ID == removeButtonCounterMap[[counter]], ]

                              data_features <- rbind(data_features, data_features_restore)
                              data_patients <- rbind(data_patients, data_patients_restore)

                              # Update the reactive values with the restored data
                              data_patients(data_patients)
                              data_features(data_features)

                              # Update the loaded_data reactive with the restored data
                              if(isPatientLevelDataPresent()) {
                                cachedData(list(
                                  data_initial = data_initial(),
                                  data_patients = data_patients(),
                                  data_features = data_features(),
                                  data_person = cachedData()$data_person,
                                  complementaryMappingTable = cachedData()$complementaryMappingTable,
                                  target_matrix = cachedData()$target_matrix,
                                  target_row_annotation = cachedData()$target_row_annotation,
                                  target_col_annotation = cachedData()$target_col_annotation,
                                  config = cachedData()$config,
                                  compressedDatas = cachedData()$compressedDatas,
                                  prevalence_plot_data = cachedData()$prevalence_plot_data,
                                  time_plot_data = cachedData()$time_plot_data,
                                  heatmap_plot_data = cachedData()$heatmap_plot_data,
                                  prevalence_plot_data_correlation = cachedData()$prevalence_plot_data_correlation,
                                  time_plot_data_correlation = cachedData()$time_plot_data_correlation,
                                  heatmap_plot_data_correlation = cachedData()$heatmap_plot_data_correlation
                                ))

                              } else {

                                prevalence_plot_data(originalData()$compressedDatas[[abstractionLevelReactive()]]$prevalencePlotData %>% dplyr::filter(CONCEPT_ID %in% data_features$CONCEPT_ID))

                                time_plot_data(originalData()$compressedDatas[[abstractionLevelReactive()]]$timePlotData %>%
                                                 dplyr::filter(CONCEPT_NAME %in% unique(prevalence_plot_data()$CONCEPT_NAME))
                                )

                                heatmap_plot_data(list(
                                  map = originalData()$compressedDatas[[abstractionLevelReactive()]]$heatmapPlotData$heatmap_plot_data$map,
                                  correlation_analysis = originalData()$compressedDatas[[abstractionLevelReactive()]]$heatmapPlotData$heatmap_plot_data$correlation_analysis,
                                  correlation_matrix = originalData()$compressedDatas[[abstractionLevelReactive()]]$heatmapPlotData$correlation_matrix[as.character(unique(prevalence_plot_data()$CONCEPT_ID)),
                                                                                                                                                        as.character(unique(prevalence_plot_data()$CONCEPT_ID))]
                                ))

                                cachedData(list(
                                  data_initial = data_initial(),
                                  data_patients = data_patients(),
                                  data_features = data_features(),
                                  data_person = cachedData()$data_person,
                                  complementaryMappingTable = cachedData()$complementaryMappingTable,
                                  target_matrix = cachedData()$target_matrix,
                                  target_row_annotation = cachedData()$target_row_annotation,
                                  target_col_annotation = cachedData()$target_col_annotation,
                                  config = cachedData()$config,
                                  compressedDatas = cachedData()$compressedDatas,
                                  prevalence_plot_data = prevalence_plot_data(),
                                  time_plot_data = time_plot_data(),
                                  heatmap_plot_data =  heatmap_plot_data(),
                                  prevalence_plot_data_correlation = NULL,
                                  time_plot_data_correlation = NULL,
                                  heatmap_plot_data_correlation = NULL
                                ))
                              }

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
    shiny::req(selectedFilters())
    shiny::validate(
      shiny::need(length(selectedFilters()) > 0, "No filters selected.")
    )
    filters <- selectedFilters()
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


      if(!isPatientLevelDataPresent()) {
        prevalence_plot_data(prevalence_plot_data() %>% dplyr::filter(CONCEPT_ID %in% data_filtering$CONCEPT_ID))

        time_plot_data(if (is.null(cachedData()$time_plot_data)) NULL else cachedData()$time_plot_data %>%
                         dplyr::filter(CONCEPT_NAME %in% unique(prevalence_plot_data()$CONCEPT_NAME))
        )
        heatmap_plot_data(list(
          map = cachedData()$heatmap_plot_data$map,
          correlation_analysis = cachedData()$heatmap_plot_data$correlation_analysis,
          correlation_matrix = cachedData()$heatmap_plot_data$correlation_matrix[as.character(unique(prevalence_plot_data()$CONCEPT_ID)),
                                                                                as.character(unique(prevalence_plot_data()$CONCEPT_ID))]
        ))
      }

    isolate({
      cachedData(list(
        data_initial = data_initial(),
        data_patients = data_patients(),
        data_features = data_features(),
        data_person = cachedData()$data_person,
        complementaryMappingTable = cachedData()$complementaryMappingTable,
        target_matrix = cachedData()$target_matrix,
        target_row_annotation = cachedData()$target_row_annotation,
        target_col_annotation = cachedData()$target_col_annotation,
        config = cachedData()$config,
        compressedDatas = cachedData()$compressedDatas,
        prevalence_plot_data = prevalence_plot_data(),
        time_plot_data = time_plot_data(),
        heatmap_plot_data =  heatmap_plot_data(),
        prevalence_plot_data_correlation = NULL,
        time_plot_data_correlation = NULL,
        heatmap_plot_data_correlation = NULL
      ))
    })
  })


########################################## FILTERING (SELECT ONLY WITH CONCEPTS)
# Server-side selectize input for filtering by CONCEPT_ID and CONCEPT_NAME
  output$dynamic_concepts_selection <- shiny::renderUI({
    shiny::req(studyName())
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

  # Bug fix if going to "Mapping" before
  shiny::outputOptions(output, "dynamic_concepts_selection", suspendWhenHidden = FALSE)

  # Server-side search for selectizeInput
  shiny::observeEvent(list(input$search_patient_query_filtering,
                           abstractionLevelReactive()), {
    shiny::req(studyName())
    query_filtering <- input$search_patient_query_filtering

    data_patient_query_filtering <- data_features()[
      ABSTRACTION_LEVEL == as.numeric(abstractionLevelReactive())
    ]

    # Filter the choices based on the search query
    matched_choices_filtering <- stats::setNames(
      data_patient_query_filtering$CONCEPT_ID,
      paste0(data_patient_query_filtering$CONCEPT_NAME, " (", data_patient_query_filtering$CONCEPT_ID, ")")
    )

    # Send the matched choices to the selectizeInput
    shiny::updateSelectizeInput(session, "filter_patient_concept_id_selection", choices = matched_choices_filtering, server = TRUE)
  })
  # Observe when the "Add Filter" button is clicked
  shiny::observeEvent(input$add_patients_filter, {
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else {
      # If the value has changed since the last observation
      if (input$add_patients_filter > lastAddFilterValue()) {
        # Update last seen value
        lastAddFilterValue(input$add_patients_filter)

        # Perform the add filter logic
        add_filter_logic("with")
      }
    }
  })

  # Observe when the "Disregard Filter" button is clicked
  shiny::observeEvent(input$disregard_patients_filter, {
    if(!isPatientLevelDataPresent()){
      CohortContrast:::showNoPatientDataAllowedWarning()
    } else {
      # If the value has changed since the last observation
      if (input$disregard_patients_filter > lastDisregardFilterValue()) {
        # Update last seen value
        lastDisregardFilterValue(input$disregard_patients_filter)

        # Perform the disregard filter logic
        add_filter_logic("without")
      }
    }
  })

  # Function to handle the addition of filters
  add_filter_logic <- function(filter_type) {
    # Retrieve current filters
    filters <- selectedPatientFilters()

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
      selectedPatientFilters(filters)
    }
  }

  output$selected_patient_filters_list <- shiny::renderUI({
    filters <- selectedPatientFilters()

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
    filters <- selectedPatientFilters()
    current_ids <- names(filterButtonCounterMap)

    # Loop through the counter map to create or update observers for each remove button
    lapply(current_ids, function(counter) {
      if (!is.null(filterButtonCounterMap[[counter]])) {
        shiny::observeEvent(input[[paste0("add_filter_", counter)]],
                            {
                              filter_id_to_remove <- filterButtonCounterMap[[counter]]
                              # Remove the filter from the list
                              updated_filters <- filters[!sapply(filters, function(f) f$id == filter_id_to_remove)]
                              selectedPatientFilters(updated_filters)

                              # Update the reactive values with the restored data
                              data_patients(data.table::copy(originalData()$data_patients))
                              data_features(data.table::copy(originalData()$data_features))
                              data_initial(data.table::copy(originalData()$data_initial))

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
    shiny::req(studyName())
    filters <- selectedPatientFilters()
    keepUsersWithConcepts(filters)
  })

  observeEvent(isCorrelationView(), {
    shiny::req(studyName())
    # Only trigger when input$correlationView is changed to TRUE
    if (isTRUE(isCorrelationView())) {
      shiny::observe({
        shiny::req(studyName())
        later::later(function() {
          isolate({
            correlationGroups(NULL)
            correlationGroupChoices(NULL)
            correlationGroupChoicesLabels(NULL)
          })
        }, delay = 0.5)
      })
    }
  })

  #################################################################### FUNCTIONS
  # Function to rename selected concept
  renameSelectedConcept <- function(new_concept_name, selected_concept_id) {
    abstraction_level <- as.numeric(abstractionLevelReactive())
    data_features <- data_features()
    data_patients <- data_patients()
    complementary_mapping <- complementaryMappingTable()


    processed_table <- target_mod()[ABSTRACTION_LEVEL == abstraction_level]
    selected_concept_name <- processed_table[CONCEPT_ID == selected_concept_id, CONCEPT_NAME]
    # check if new_concept_name can be used
    new_concept_name = ensureUniqueConceptName(data_patients, selected_concept_id, new_concept_name)

    # Update complementaryMappingTable
    new_row <- data.frame(CONCEPT_ID = selected_concept_id, CONCEPT_NAME = selected_concept_name, NEW_CONCEPT_ID = selected_concept_id, NEW_CONCEPT_NAME = new_concept_name, ABSTRACTION_LEVEL = abstraction_level, TYPE = "rename", stringsAsFactors = FALSE)
    complementary_mapping <- rbind(complementary_mapping, new_row)

    # Update tables
    rows_to_update <- data_patients[
      , CONCEPT_ID == selected_concept_id & ABSTRACTION_LEVEL == abstraction_level
    ]

    data_patients[
      rows_to_update,
      `:=`(
        CONCEPT_NAME = new_concept_name
      )
    ]

    rows_to_update <- data_features[
      , CONCEPT_ID == selected_concept_id & ABSTRACTION_LEVEL == abstraction_level
    ]

    data_features[
      rows_to_update,
      `:=`(
        CONCEPT_NAME = new_concept_name
      )
    ]

    data_features(data_features)
    data_patients(data_patients)
    complementaryMappingTable(complementary_mapping)
    cachedData(list(
      data_initial = data_initial(),
      data_patients = data_patients,
      data_features = data_features,
      data_person = data_person(),
      complementaryMappingTable = complementary_mapping,
      # Preserve existing values if needed
      target_matrix = cachedData()$target_matrix,
      target_row_annotation = cachedData()$target_row_annotation,
      target_col_annotation = cachedData()$target_col_annotation,
      config = cachedData()$config,
      compressedDatas = cachedData()$compressedDatas,
      prevalence_plot_data = cachedData()$prevalence_plot_data,
      time_plot_data = cachedData()$time_plot_data,
      heatmap_plot_data = cachedData()$heatmap_plot_data,
      prevalence_plot_data_correlation = cachedData()$prevalence_plot_data_correlation,
      time_plot_data_correlation = cachedData()$time_plot_data_correlation,
      heatmap_plot_data_correlation = cachedData()$heatmap_plot_data_correlation
    ))
    }

  # Function to combine selected concepts
  combineSelectedConcepts <- function(new_concept_name, new_concept_id = NULL, selected_ids = NULL, type = "custom") {
    # selected_rows <- input$concept_table_rows_selected
    # selected_ids <- selectedCorrelationGroup()
    abstraction_level <- as.numeric(abstractionLevelReactive())
    data_features <- data_features()
    data_patients <- data_patients()
    data_initial <- data_initial()
    data_person <- data_person()
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
    representingConceptId = new_concept_id
    representingHeritage = NA
    representingZTest = NA
    representingLogitTest = NA
    representingKSTest = NA

      # Step 3: Filtering processed_table and selecting concept IDs
      selected_concept_ids <- as.numeric(selected_ids)
      selected_concept_names <- processed_table[CONCEPT_ID %in% selected_concept_ids, CONCEPT_NAME]

      if(is.null(representingConceptId)){
      representingConceptId <- selected_concept_ids[
        which.max(as.numeric(processed_table[CONCEPT_ID %in% selected_concept_ids, PREVALENCE_DIFFERENCE_RATIO]))
      ]
      }
      representingHeritage <- processed_table[CONCEPT_ID %in% selected_concept_ids, HERITAGE][
        which.max(as.numeric(processed_table[CONCEPT_ID %in% selected_concept_ids, PREVALENCE_DIFFERENCE_RATIO]))]

      # Step 4: Determining ZTEST, KSTEST and LOGITTEST values
      representingZTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, ZTEST])
      representingLogitTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, LOGITTEST])
      representingKSTest <- any(processed_table[CONCEPT_ID %in% selected_concept_ids, KSTEST])


    # Remove row if there was for representingConceptId
    data_features <- data_features[!(CONCEPT_ID %in% c(selected_concept_ids, representingConceptId) & ABSTRACTION_LEVEL == abstraction_level)]

    # check if new_concept_name can be used
    new_concept_name = ensureUniqueConceptName(data_features, c(selected_concept_ids, representingConceptId), new_concept_name)

    new_row <- data.table::data.table(
      CONCEPT_ID = representingConceptId,
      CONCEPT_NAME = new_concept_name,
      ABSTRACTION_LEVEL = abstraction_level,
      ZTEST = representingZTest,
      LOGITTEST = representingLogitTest,
      KSTEST = representingKSTest,
      HERITAGE = representingHeritage
    )

    data_features <- rbind(data_features, new_row, fill = TRUE)

    rows_to_update <- data_patients[
      , CONCEPT_ID %in% c(selected_concept_ids, representingConceptId) & ABSTRACTION_LEVEL == abstraction_level
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
    concept_row <- data_features[CONCEPT_ID == representingConceptId & CONCEPT_NAME == new_concept_name & ABSTRACTION_LEVEL == abstraction_level, .(TARGET_SUBJECT_COUNT, CONTROL_SUBJECT_COUNT)]
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
    new_rows <- data.frame(CONCEPT_ID = selected_concept_ids, CONCEPT_NAME = selected_concept_names, NEW_CONCEPT_ID = representingConceptId, NEW_CONCEPT_NAME = new_concept_name, ABSTRACTION_LEVEL = abstraction_level, TYPE = type, stringsAsFactors = FALSE)
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
    cachedData(list(
      data_initial = data_initial,
      data_patients = data_patients,
      data_features = data_features,
      data_person = data_person,
      complementaryMappingTable = complementary_mapping,
      # Preserve existing values if needed
      target_matrix = cachedData()$target_matrix,
      target_row_annotation = cachedData()$target_row_annotation,
      target_col_annotation = cachedData()$target_col_annotation,
      config = cachedData()$config,
      compressedDatas = cachedData()$compressedDatas,
      prevalence_plot_data = cachedData()$prevalence_plot_data,
      time_plot_data = cachedData()$time_plot_data,
      heatmap_plot_data = cachedData()$heatmap_plot_data,
      prevalence_plot_data_correlation = cachedData()$prevalence_plot_data_correlation,
      time_plot_data_correlation = cachedData()$time_plot_data_correlation,
      heatmap_plot_data_correlation = cachedData()$heatmap_plot_data_correlation
    ))
  }
#   # Function to combine selected concepts
  keepUsersWithConcepts <- function(filters) {
    if (length(filters) == 0) {
      return()
    }
    abstraction_level <- as.numeric(abstractionLevelReactive())
    data_features <- data_features()
    data_patients_filtering <- data_patients()
    data_initial_filtering <- data_initial()
    data_person_filtering <- data_person()
    complementary_mapping <- complementaryMappingTable()

    # Convert from int64
    data_initial_filtering$SUBJECT_ID <- as.integer(data_initial_filtering$SUBJECT_ID)
    data_person_filtering$PERSON_ID <- as.integer(data_person_filtering$PERSON_ID)
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

    data_person_filtering <- data_person_filtering[PERSON_ID %in% c(control_ids, target_ids)]


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
    data_person(data_person_filtering)

    cachedData(list(
      data_initial = data_initial_filtering,
      data_patients = data_patients_filtering,
      data_features = data_features_filtering,
      data_person = cachedData()$data_person,
      complementaryMappingTable = complementary_mapping,
      # Preserve existing values if needed
      target_matrix = cachedData()$target_matrix,
      target_row_annotation = cachedData()$target_row_annotation,
      target_col_annotation = cachedData()$target_col_annotation,
      config = cachedData()$config,
      compressedDatas = cachedData()$compressedDatas,
      prevalence_plot_data = cachedData()$prevalence_plot_data,
      time_plot_data = cachedData()$time_plot_data,
      heatmap_plot_data = cachedData()$heatmap_plot_data,
      prevalence_plot_data_correlation = cachedData()$prevalence_plot_data_correlation,
      time_plot_data_correlation = cachedData()$time_plot_data_correlation,
      heatmap_plot_data_correlation = cachedData()$heatmap_plot_data_correlation
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

  deepCopyList <- function(lst) {
    lapply(lst, function(x) {
      if (data.table::is.data.table(x)) {
        return(data.table::copy(x))  # Deep copy data.table objects
      } else if (is.data.frame(x)) {
        return(as.data.frame(x))  # Deep copy data.frame objects
      } else if (is.list(x)) {
        return(deepCopyList(x))  # Recursively copy nested lists
      } else {
        return(x)  # Copy primitives (numeric, character, logical, NULL)
      }
    })
  }

  generateBPMNTrajectoryViewer <- function(filtered_target = NULL, edge_prevalence_threshold = 0.5, selection_list = NULL, selected_ids = NULL, is_patient_level_data_present = NULL, is_corr_initiated = FALSE) {
    # Ensure edgePrevalence has a default value
    edge_prevalence <- if (is.null(edge_prevalence_threshold)) 0.5 else edge_prevalence_threshold

    # Generate BPMN JSON data
    bpmn_json <- CohortContrast:::createBPMNTrajectoryData(
      filtered_target = filtered_target,
      edgePrevalence = edge_prevalence,
      selectionList = selection_list,
      selectedIds = selected_ids,
      isPatientLevelDataPresent = is_patient_level_data_present
    )

    # Convert the BPMN data to JSON format
    json_data <- jsonlite::toJSON(bpmn_json, auto_unbox = TRUE)

    # Generate the BPMN Viewer iframe and send JSON data
    result = htmltools::tagList(
      # ✅ Load the BPMN Viewer
      tags$iframe(
        id = "bpmnFrame",
        src = "public/index.html",
        width = "100%",
        height = "650px"
      ),

      # Send JSON Data to the iFrame after it loads
      tags$script(HTML(paste0("
      document.getElementById('bpmnFrame').onload = function() {
        console.log('✅ Sending BPMN Data to iframe...');
        document.getElementById('bpmnFrame').contentWindow.postMessage(", json_data, ", '*');
      };
    ")))
    )
  }

  updateHierarchySuggestions <- function(customisedTable = NULL, allowOnlyActiveChildren = TRUE, isOnlyActiveParentsAllowed = TRUE) {
      shiny::req(studyName(), pathToResults)
      result <- NULL
      if(is.null(customisedTable)) {
        if(allowOnlyActiveChildren){
      result <- CohortContrast:::getAncestorMappings(
        active_concept_ids = rownames(target_row_annotation()),
        concept_table = originalData()$conceptsData$concept,
        concept_ancestor = originalData()$conceptsData$concept_ancestor,
        allowed_parents = rownames(target_row_annotation()),
        isOnlyActiveParentsAllowed = isOnlyActiveParentsAllowed
      )
        } else{
          result <- CohortContrast:::getAncestorMappings(
            active_concept_ids = data_patients() %>% dplyr::filter(ABSTRACTION_LEVEL == abstractionLevelReactive()) %>% dplyr::pull(CONCEPT_ID),
            concept_table = originalData()$conceptsData$concept,
            concept_ancestor = originalData()$conceptsData$concept_ancestor,
            allowed_parents = rownames(target_row_annotation()),
            isOnlyActiveParentsAllowed = isOnlyActiveParentsAllowed
          )
}
      } else {
        result <- customisedTable
      }
      hierarchySuggestionsTable(result)
  }

  updateCorrelationSuggestions <- function(customisedTable = NULL) {
    shiny::req(studyName(), pathToResults)
    result <- NULL
    if(is.null(customisedTable)) {
      result_corr <- CohortContrast:::computePairwiseCorrelations(target_filtered()$correlation_analysis$ordered_matrix, target_row_annotation())
      result_time <- CohortContrast:::calculateMedianTransitions(target_time_annotation())
      result <- CohortContrast:::mergeCorrelationWithTransitions(correlation_data = result_corr, transition_data = result_time)
    } else {
      result <- customisedTable
    }
    correlationSuggestionsTable(result)
  }

  ensureUniqueConceptName <- function(data_patients, selected_ids, new_concept_name) {
    # Get concept names that are not associated with the selected IDs
    used_concept_names <- unique(
      data_patients %>%
        dplyr::filter(!(.data$CONCEPT_ID %in% selected_ids)) %>%
        dplyr::pull(CONCEPT_NAME)
    )

    counter <- 2
    maybe_new_concept_name <- new_concept_name

    # Check if the new concept name is already used and make it unique
    while (maybe_new_concept_name %in% used_concept_names) {
      maybe_new_concept_name <- paste(new_concept_name, counter)
      counter <- counter + 1
      cli::cli_alert_warning(
        paste0(
          "Mapped '", gsub("\\{", "{{", gsub("\\}", "}}", new_concept_name)),
          "' to '", gsub("\\{", "{{", gsub("\\}", "}}", maybe_new_concept_name)),
          "' because of duplicate concept names for differing ids!"
        )
      )
    }

    return(maybe_new_concept_name)
  }
  }

