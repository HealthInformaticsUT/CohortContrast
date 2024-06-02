# Define the dashboard header
library(shiny)
header <- shinydashboard::dashboardHeader(title = "Data Dashboard")

# Define the dashboard sidebar
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    shinydashboard::menuItem("Mapping", tabName = "mapping", icon = icon("sliders")),
    shinydashboard::menuItem("Overview", tabName = "overview", icon = icon("receipt"))
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
              "Observation" = "observation",
              "Condition" = "condition_occurrence",
              "Visit" = "visit_occurrence",
              "Visit detail" = "visit_detail"
            ),
            selected = c(
              "drug_exposure",
              "measurement",
              "procedure_occurrence",
              "observation",
              "condition_occurrence",
              "visit_occurrence",
              "visit_detail"
            )
          )
        ),
        column(
          width = 4,
          sliderInput(
            "prevalence_ratio",
            h3("Enrichment cutoff"),
            min = 1,
            max = 10,
            value = 1,
            step = 0.05
          )
        ),
        column(
          width = 4,
          sliderInput(
            "prevalence",
            h3("Prevalence cutoff"),
            min = 0,
            max = 1,
            value = 0.05,
            step = 0.01
          )
        )
      ),
      shiny::hr(),
      # Add Fluent UI toggles here
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::div(
            shiny.fluent::Toggle.shinyInput(
              "scaleTime",
              label = "Scale for time",
              value = FALSE,
              onText = "On",
              offText = "Off"
            ),
            shiny::textOutput("scaleTimeValue")
          )
        ),
        shiny::column(
          width = 4,
          shiny::div(
            shiny.fluent::Toggle.shinyInput(
              "applyInverseTarget",
              label = "Inverse target",
              value = FALSE,
              onText = "On",
              offText = "Off"
            ),
            shiny::textOutput("applyInverseTargetValue")
          )
        ),
        shiny::column(
          width = 4,
          shiny::div(
            shiny.fluent::Toggle.shinyInput(
              "removeUntreated",
              label = "Remove untreated patients",
              value = FALSE,
              onText = "On",
              offText = "Off"
            ),
            shiny::textOutput("removeUntreatedValue")
          )
        )
      ),
      hr(),
      tabsetPanel(
        tabPanel("Prevalence plot",
                 plotOutput("prevalence")),
        tabPanel("Heatmap",
                 plotOutput("heatmap"))
      )
    ),
    # Mapping tab
    shinydashboard::tabItem(
      tabName = "mapping",
      h3("Mapping Panel"),
      fluidRow(DT::DTOutput("concept_table")),
      fluidRow(
        actionButton("combine_btn", "Combine Selected"),
        actionButton("reset_btn", "Reset")
      )
    ),
    # Settings tab
    shinydashboard::tabItem(
      tabName = "overview",
      h3("Overview panel"),
      p("What data, where data, why data?")
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
