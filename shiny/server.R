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
  study_info <- reactiveVal(list())
  studyName <- reactiveVal(studyName)

  # Function to load data and update study_info
  load_study_data <- function() {
    names_and_rows <- list()
    study_names <- get_study_names(pathToResults)
    for (study_name in study_names) {
      file_path <- str_c(pathToResults, "/tmp/datasets/", study_name, "_CC_medData.rdata")
      if (file.exists(file_path)) {
        load(file_path)  # Assuming data_features is loaded
        rows <- nrow(dplyr::distinct(dplyr::select(dplyr::filter(object$data_patients, COHORT_DEFINITION_ID == 2), PERSON_ID)))  # Adjust this if the data frame has a different name
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

  # Existing observeEvent for loading data based on selection
  observeEvent(input$studyName, {
    req(input$studyName)
    split_name <- unlist(strsplit(input$studyName, " ", fixed = TRUE))
    correct_study_name <- split_name[1]
    file_path <- str_c(pathToResults, "/tmp/datasets/", correct_study_name, "_CC_medData.rdata")
    studyName(correct_study_name)  # Update studyName reactive value
    if (file.exists(file_path)) {
      print("Data loaded")
    } else {
      print("File not found")
    }
  })

  # Reactive data transform
  target = reactive({
    print(studyName())
    format_results(
      pathToResults = pathToResults,
      studyName = studyName(),
      autoScaleTime =  if (input$scaleTime)
        TRUE
      else
        FALSE,
      applyInverseTarget =  if (input$applyInverseTarget)
        TRUE
      else
        FALSE
    )
  })

  target_filtered = reactive({
    filter_target(
      target(),
      input$prevalence,
      input$prevalence_ratio,
      input$domain,
      removeUntreated = if (input$removeUntreated)
        TRUE
      else
        FALSE
    )
  })

  # output$prevalence <- renderPlot({
  #   plot_prevalence(target_filtered())
  # }, height = 950)  # Specify width and height in pixels
  output$prevalence <- renderPlot({
    # Attempt to plot and handle errors if they occur
    tryCatch({
      # Code that might throw an error
      plot_prevalence(target_filtered())
    }, error = function(e) {
      # Error handling code
      # Here you log the error if needed and return an alternative representation
      print(e)  # Print the error message to the R console (optional)
      plot_prevalence(NULL)    # Return NULL to ensure no plot output
    })
  }, height = 950)


  # output$heatmap <- renderPlot({
  #   plot_heatmap(target_filtered())
  # }, height = 950)

  output$heatmap <- renderPlot({
    # Attempt to plot and handle errors if they occur
    tryCatch({
      # Code that might throw an error
      plot_heatmap(target_filtered())
    }, error = function(e) {
      # Error handling code
      # Here you log the error if needed and return an alternative representation
      print(e)  # Print the error message to the R console (optional)
      plot_prevalence(NULL)     # Return NULL to ensure no plot output
    })
  }, height = 950)
}
