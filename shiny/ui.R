# Define the dashboard header
library(shiny)
header <- shinydashboard::dashboardHeader(
  title = "Data Dashboard"
)

# Define the dashboard sidebar
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    shinydashboard::menuItem("Settings", tabName = "settings", icon = icon("sliders"))
  )
)

# Define the dashboard body
body <- shinydashboard::dashboardBody(
  shinydashboard::tabItems(
    # Dashboard tab
    shinydashboard::tabItem(
      tabName = "dashboard",
      fluidRow(
        h1("Cohorts"),
        selectInput("studyName", "Choose a study:", choices = NULL)
        # selectInput("studyName", "Choose a study:",
        #             choices = get_study_names(pathToResults))
      ),
      hr(),
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput(
            "domain",
            h3("Select domain"),
            choices = list(
              "Drug" = "drug_exposure",
              "Measurement" = "measurement",
              "Procedure" = "procedure_occurrence",
              "Observation" = "observation"
            ),
            selected = c("drug_exposure", "measurement", "procedure_occurrence", "observation")
          )
        ),
        column(
          width = 4,
          sliderInput(
            "prevalence_ratio",
            h3("Enrichment cutoff"),
            min = 0,
            max = 10,
            value = 2
          )
        ),
        column(
          width = 4,
          sliderInput(
            "prevalence",
            h3("Prevalence cutoff"),
            min = 0,
            max = 1,
            value = 0.05
          )
        )
      ),
      hr(),
      tabsetPanel(
        tabPanel(
          "Prevalence plot",
          plotOutput("prevalence")
        ),
        tabPanel(
          "Heatmap",
          plotOutput("heatmap")
        )
      )
    ),
    # Settings tab
    shinydashboard::tabItem(
      tabName = "settings",
      h3("Settings Panel"),
      p("Settings related content could be placed here.")
    )
  )
)

# Combine them into the dashboardPage function call
ui <- shinydashboard::dashboardPage(
  skin = "black",
  header = header,
  sidebar = sidebar,
  body = body
)
