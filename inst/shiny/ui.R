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
    shinydashboard::menuItem("Help", tabName = "help", icon = icon("receipt"))
  )
)

# Define the dashboard body
body <- shinydashboard::dashboardBody(
  waiter::use_waiter(),
  shinydashboard::tabItems(
    # Dashboard tab
    shinydashboard::tabItem(
      tabName = "dashboard",
      fluidRow(
        h3("Dashboard panel"),
        div(id = "studyName_container",  # Wrap selectInput in a div with an ID
            selectInput("studyName", "Choose a study:", choices = NULL)
        )
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
            value = 3,
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
        ),
        column(
          width = 4,
          sliderInput(
            "abstraction_lvl",
            h3("Abstraction level"),
            min = -1,
            max = 10,
            value = -1,
            step = 1
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
              "scaleRate",
              label = "Scale for rate",
              value = FALSE,
              onText = "On",
              offText = "Off"
            ),
            shiny::textOutput("scaleRateValue")
          ),
          shiny::div(
            shiny.fluent::Toggle.shinyInput(
              "applyZTest",
              label = "Apply Z-test",
              value = FALSE,
              onText = "On",
              offText = "Off"
            ),
            shiny::textOutput("applyZTestValue")
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
          ),
          shiny::div(
            shiny.fluent::Toggle.shinyInput(
              "applyLogitTest",
              label = "Apply Logit-test",
              value = FALSE,
              onText = "On",
              offText = "Off"
            ),
            shiny::textOutput("applyLogitTestValue")
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
      fluidRow(div(style = 'overflow-x: auto;', DT::DTOutput("concept_table"))),
      fluidRow(
        actionButton("combine_btn", "Combine Selected"),
        actionButton("reset_btn", "Reset"),
        actionButton("save_btn", "Snapshot")
      )
    ),
    # Settings tab
    shinydashboard::tabItem(
      tabName = "help",
      h3("Help panel"),
      shiny::tags$div(
        style = "background-color: #f8f8f8; padding: 20px; border-radius: 5px;",
        shiny::tags$h4("Enrichment Cutoff"),
        shiny::tags$p("The enrichment cutoff is the value of the relative risk ratio. It compares the prevalence of a concept between target and control cohorts. Concepts with a relative risk ratio below this cutoff will be filtered out."),
        shiny::tags$h4("Prevalence Cutoff"),
        shiny::tags$p("The prevalence cutoff is the minimum percentage of people who should have the concept in the target cohort. Concepts with a prevalence below this cutoff will be filtered out."),
        shiny::tags$h4("Abstraction level"),
        shiny::tags$p("The abstraction level slider helps you to automatically combine or expand concepts. The levels are calculated based on concept ancestor table in the CDM. Level -1 stands for default imported concept ids, the levels from 0 to 10 represent (0 - highest level, 10 - lowest level) the distance in concept hierarchy from the ancestor of highest level."),
        shiny::tags$h4("Scale for Rate"),
        shiny::tags$p("When enabled, the prevalence will be calculated on a yearly basis for each person. This takes into account the total number of occurrences of the concept in the observation period."),
        shiny::tags$h4("Inverse Target"),
        shiny::tags$p("When enabled, the control cohort will become the target cohort and vice versa."),
        shiny::tags$h4("Apply Z-test"),
        shiny::tags$p("Apply Z-test for filtering concepts based on the difference in concept prevalence. This test is performed using a 2x2 table."),
        shiny::tags$h4("Apply Logit-test"),
        shiny::tags$p("Apply Logit-test for filtering concepts based on the significance inside a logit model. This test is performed using a logistic regression model."),
        shiny::tags$h4("Remove Untreated Patients"),
        shiny::tags$p("Remove all patients who have no concept in the target cohort.")
      )
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
