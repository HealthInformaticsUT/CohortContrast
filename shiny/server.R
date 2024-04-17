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

server = function(input, output) {

  load(stringr::str_c(pathToResults, "/tmp/datasets/", studyName, "_CC_medData.rdata"))
  cohort_table = data.frame(cohort_name = c(studyName, "Test"), number_subjects = c(nrow(dplyr::distinct(
    dplyr::select(
      dplyr::filter(object$data_patients,
                    COHORT_DEFINITION_ID == 2),
    PERSON_ID))),0))
  output$cohorts = shiny::renderDataTable({
      DT::formatStyle(DT::datatable(
        cohort_table,
        # colnames = c(Cohort = cohort_name, "#Persons" = number_subjects),
        selection = list(mode = "single", target = "row")
      ),
        "number_subjects",
        # background = styleColorBar(c(0, max(cohort_table$number_subjects)), "lightgreen")
        background = DT::styleColorBar(c(0, 50000), "lightgreen")
      )
  })
 ## TODO: Get person
  target = reactive({
    # if(is.null(input$cohorts_rows_selected)){
    #   return(NULL)
    # }
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
