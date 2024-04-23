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

  #load(stringr::str_c(pathToResults, "/tmp/datasets/", input$studyName, "_CC_medData.rdata"))

  # Reactive for loading data
  # observeEvent(input$studyName,{
  #   req(input$studyName)  # Ensure that studyName is selected
  #   file_path <- stringr::str_c(pathToResults, "/tmp/datasets/", input$studyName, "_CC_medData.rdata")
  #   if (file.exists(file_path)) {
  #     load(file_path)
  #     return(paste("Data loaded from", file_path))
  #   } else {
  #     return(paste("File not found:", file_path))
  #   }
  # })
  study_info <- reactiveVal(list())

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
    file_path <- str_c(pathToResults, "/tmp/datasets/", input$studyName, "_CC_medData.rdata")
    if (file.exists(file_path)) {
      load(file_path)  # Assuming data_features is loaded
      return(paste("Data loaded from", file_path))
    } else {
      return(paste("File not found:", file_path))
    }
  })

  target = reactive({

    format_results(pathToResults = pathToResults, studyName = studyName)
  })

  target_filtered = reactive({
    filter_target(target(), input$prevalence, input$prevalence_ratio, input$domain)
  })

  output$prevalence <- renderPlot({
    plot_prevalence(target_filtered())
  }, height = 950)  # Specify width and height in pixels

  output$heatmap <- renderPlot({
    plot_heatmap(target_filtered())
  }, height = 950)
}
