# Define the dashboard header
library(shiny)
header <- shinydashboard::dashboardHeader(title = "CohortContrast Dashboard")

# Define the dashboard sidebar
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    shinydashboard::menuItem("Concept search", tabName = "mapping", icon = icon("sliders")),
    shinydashboard::menuItem("Filtering", tabName = "filtering", icon = icon("filter")),
    shinydashboard::menuItem("Help", tabName = "help", icon = icon("receipt"))
  )
)

# Define the dashboard body
body <- shinydashboard::dashboardBody(
  waiter::use_waiter(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  shinydashboard::tabItems(
    # Dashboard tab
    shinydashboard::tabItem(
      tabName = "dashboard",
      shinyjs::useShinyjs(),  # Initialize shinyjs
      fluidRow(
        h3("Dashboard Panel"),
        div(
          id = "studyName_container",
          DT::dataTableOutput("study_table") # New: Table to display studies
        ),
        shinyjs::hidden(  # Hidden input field to store the selected study
          textInput("studyName", "Selected Study", value = "")
        )
      ),
      verbatimTextOutput("selected_study"),
      hr(),
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput(
            "domain",
            h3("Select domain"),
            choices = list(
              "Condition" = "condition_occurrence",
              "Drug" = "drug_exposure",
              "Measurement" = "measurement",
              "Observation" = "observation",
              "Procedure" = "procedure_occurrence",
              "Visit" = "visit_occurrence",
              "Visit detail" = "visit_detail"
            ),
            selected = c(
              "drug_exposure",
              "procedure_occurrence",
              "condition_occurrence"
            )
          )
        ),
        column(
          width = 4,
          sliderInput(
            "prevalence_ratio",
            h3("Risk Ratio cutoff"),
            min = 1,
            max = 10,
            value = 2,
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
            value = 0.2,
            step = 0.01
          )
        ),
        tags$style(HTML("
    .irs-grid-text {
      transform: rotate(-45deg); /* Rotates the text 45 degrees counterclockwise */
      transform-origin: center; /* Keeps the text aligned */
      white-space: nowrap; /* Prevents text from wrapping */
    }
  ")),
        column(
          width = 4,
          shinyWidgets::sliderTextInput(
            inputId = "abstraction_lvl",
            label = h3("Abstraction level"),
            choices = c(
              "original",
              "0",
              "1",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7",
              "8",
              "9",
              "10",
              "source"
            ),
            selected = "original",
            grid = TRUE
          )
        ),
        column(
          width = 4,
          uiOutput("lookBackSlider"),
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
          ),
          shiny::div(
            shiny.fluent::Toggle.shinyInput(
              "correlationView",
              label = "Apply correlation view",
              value = FALSE,
              onText = "On",
              offText = "Off"
            ),
            shiny::textOutput("correlationViewValue")
          )
        )
      ),
      hr(),
      # Add correlation view values here
        uiOutput("dynamic_correlation_widgets"),
      hr(),
      shiny::actionButton("visual_snapshot", "Create visual snapshot!"),
      fluidRow(tabsetPanel(
        tabPanel("Prevalence plot",
                 plotOutput("prevalencePlot")),
        tabPanel("Time panel",
                 plotOutput("time_panelPlot")),
        tabPanel("Heatmap",
                 plotOutput("heatmapPlot")),
        tabPanel("Trajectories",
                 uiOutput("dynamicGraphUI"),
                 uiOutput("dynamic_selectize_trajectory_inputs")
                 )
      ))
    ),
    shinydashboard::tabItem(
      tabName = "filtering",
      h3("Concept Filtering Panel"),

      # First fluidRow with two columns
      fluidRow(
        column(
          width = 6,  # Adjust width as needed (out of 12)
          tags$div(
            style = "width: 100%; padding: 0 20px;",  # Full width with some padding
            uiOutput("dynamic_concepts_removal"),
            uiOutput("selected_filters_list")
            # actionButton("reset_btn_filter", "Reset Filters")
          )
        ),
        column(
          width = 6,  # Adjust width as needed (out of 12)
          tags$div(
            style = "width: 100%; padding: 0 20px;",  # Full width with some padding
            uiOutput("dynamic_concepts_selection"),
            uiOutput("selected_patient_filters_list")
            # actionButton("reset_btn_filter", "Reset Filters")
          )
        )
      ),

      tags$style(HTML("
    #dynamic_concepts_removal .form-group {
      width: 100%;
    }
    #dynamic_concepts_removal {
      display: flex;
      flex-wrap: wrap;
      width: 100%;
    }

    #dynamic_concepts_selection .form-group {
      width: 100%;
    }
    #dynamic_concepts_selection {
      display: flex;
      flex-wrap: wrap;
      width: 100%;
    }
  "))
    ),
    # Mapping tab
    shinydashboard::tabItem(
      tabName = "mapping",
      h3("Mapping Panel"),
      fluidRow(
        shinyjs::useShinyjs(),
        checkboxInput("dt_select_all", "Select all"),
        div(style = 'overflow-x: auto;', DT::DTOutput("concept_table"))
      ),
      fluidRow(
        actionButton("combine_btn", "Combine Selected"),
        actionButton("reset_btn_mappings", "Reset"),
        actionButton("save_btn", "Snapshot")
      ),
      h3("Mapping History"),
      fluidRow(
        div(style = 'overflow-x: auto;', DT::DTOutput("mapping_history_table"))
      ),
    ),
    # Settings tab
    shinydashboard::tabItem(
      tabName = "help",
      h3("Help panel"),
      shiny::tags$div(
        style = "background-color: #f8f8f8; padding: 20px; border-radius: 5px;",
        shiny::tags$h4("Risk Ratio Cutoff"),
        shiny::tags$p("The risk ratio cutoff is the value of the relative risk ratio. It compares the prevalence of a concept between target and control cohorts. Concepts with a relative risk ratio below this cutoff will be filtered out."),
        shiny::tags$h4("Prevalence Cutoff"),
        shiny::tags$p("The prevalence cutoff is the minimum percentage of people who should have the concept in the target cohort. Concepts with a prevalence below this cutoff will be filtered out."),
        shiny::tags$h4("Abstraction level"),
        shiny::tags$p("The abstraction level slider helps you to automatically combine or expand concepts. The levels are calculated based on concept ancestor table in the CDM. Level -1 stands for default imported concept ids and level -2 for source data (if it was set TRUE). The levels from 0 to 10 represent (0 - highest level, 10 - lowest level) the distance in concept hierarchy from the ancestor of highest level."),
        shiny::tags$h4("Scale for Rate"),
        shiny::tags$p("When enabled, the prevalence will be calculated on a yearly basis for each person. This takes into account the total number of occurrences of the concept in the observation period."),
        shiny::tags$h4("Inverse Target"),
        shiny::tags$p("When enabled, the control cohort will become the target cohort and vice versa."),
        shiny::tags$h4("Apply Z-test"),
        shiny::tags$p("Apply Z-test for filtering concepts based on the difference in concept prevalence. This test is performed using a 2x2 table."),
        shiny::tags$h4("Apply Logit-test"),
        shiny::tags$p("Apply Logit-test for filtering concepts based on the significance inside a logit model. This test is performed using a logistic regression model."),
        shiny::tags$h4("Remove Untreated Patients"),
        shiny::tags$p("Remove all patients who have no concept in the target cohort."),
        shiny::tags$h4("Correlation cutoff"),
        shiny::tags$p("Under the correlation tab, this cutoff references the correlation value which will be used for forming concept groups.
                      When there are groups forming with extremely high values it might indicate that those concepts have the same meaning and they should be mapped together.
                      When there are groups forming with moderately high values it might indicate that this combination is a treatment arm."),
        shiny::tags$h4("Edge prevalence cutoff"),
        shiny::tags$p("When a correlation group has been selected it can be shown on a trajectory graph. For filtering which edges are shown play around with the value.
                      If the cutoff point is set to 50% this means that all the edges (transitions) which are shown are present for at least 50% of the people
                      who have had any of the concepts (in the selected group) occurring to them.")
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
