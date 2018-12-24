library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinydashboard)
library(shinycssloaders)
library(rclipboard)

ui <- dashboardPage(
  dashboardHeader(title = "CDM Metadata", titleWidth = "300px",
                  tags$li(a(href = 'http://www.ohdsi.org',
                            img(src = 'ohdsi_logo_mini.png',
                                title = "OHDSI", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  dashboardSidebar(width = "300px",
                   useShinyjs(),
                   selectInput(inputId = "cdmSource", label = "CDM Source", choices = lapply(cdmSources, function(c) c$name), width = "250px"),
                   conditionalPanel(condition = "input.cdmSource != 'All Sources'",
                     selectInput(inputId = "selectAgent", label = "Select Agent", choices = c(), width = "250px"),
                     
                     div(style = "display:inline-block;text-align: left;",
                         actionButton("btnAddNewAgent", label = "Add", icon = icon("plus"))),
                     div(style = "display:inline-block;text-align: left;",
                         actionButton("btnDeleteAgent", label = "Delete", icon = icon("minus"))),
                     div(style = "display:inline-block;text-align: left;",
                         actionButton("btnEditAgent", label = "Edit", icon = icon("edit")))
                     ),
    sidebarMenu(
      id = "tabs",
      menuItem("Sources Overview", tabName = "overview", icon = icon("play"), selected = TRUE),
      menuItem("Source Provenance", tabName = "provenance", icon = icon("database")),
      menuItem("Heel Results", tabName = "heelResults", icon = icon("table")),
      menuItem("Concept Knowledge Base", tabName = "conceptKb", icon = icon("line-chart")),
      menuItem("Concept Set Knowledge Base", tabName = "conceptSetKb", icon = icon("list")),
      menuItem("Cohort Knowledge Base", tabName = "cohortDefKb", icon = icon("globe"))
    ),
    conditionalPanel(condition = "input.tabs == 'provenance'",
                     textAreaInput(inputId = "sourceDescription", label = "Source Description"),
                     actionButton(inputId = "btnSubmitDescription", label = "Submit", icon = icon("check"))
                     ),
    conditionalPanel(condition = "input.tabs == 'heelResults'", 
      selectInput(inputId = "heelStatus", label = "Heel Status", choices = c("", 
                                                                             "Needs Review", #= 900000519, 
                                                                             "Non-issue", # = 900000518, 
                                                                             "Issue"), # = 900000517), 
                                                                             width = "250px"),
      textAreaInput(inputId = "heelAnnotation", label = "Heel Annotation",
                    rows = 4, resize = "none", width = "250px"),
      div(style = "display:inline-block;text-align: left;",
          actionButton(inputId = "btnSubmitHeel", label = "Submit", icon = icon("check"))),
      div(style = "display:inline-block;text-align: left;",
          actionButton(inputId = "btnDeleteHeel", label = "Remove", icon = icon("minus")))
    ),
    conditionalPanel(condition = "input.tabs == 'conceptKb'",
                     #checkboxInput(inputId = "toggleConcepts", label = "See only concepts with metadata", value = FALSE),
                     selectInput(inputId = "domainId", label = "Domain",
                                 choices = c("Condition" = 402, 
                                             "Procedure" = 602, "Device" = 2101, "Drug" = 702, "Measurement" = 1801, "Observation" = 802)), 
                     
                     selectInput(inputId = "conceptId", label = "Pick a concept", width = "400px",
                                 choices = c())
                     )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem("overview",
              h3("Sources Overview"),
              helpText("View metadata about all sources from your site"),
              
              ),
      tabItem("provenance",
              h3("Source Provenance: ", textOutput(outputId = "sourceName")),
              helpText("Create and view metadata about the source's provenance"),
              div(
                h4("Source Description"),
                uiOutput("clip"),
                textOutput(outputId = "sourceDescriptionOutput")  
              ),
              div(
                h4("CDM Version"),
                textOutput(outputId = "cdmVersion")
              )
              ),
      tabItem("conceptKb", 
              h3("Concept"),
              helpText("Create and view metadata about a concept"),
              plotlyOutput(outputId = "conceptPlot") %>% withSpinner(color="#0dc5c1"),
              verbatimTextOutput(outputId = "conceptName", placeholder = TRUE),
              dateInput(inputId = "conceptStartDate", label = "Temporal Event Start Date", value = "1900-01-01"),
              textAreaInput(inputId = "temporalEventValue", label = "Metadata Value", width = "800px"),
              actionButton(inputId = "btnAddTemporalAnnotation", label = "Add Temporal Event Annotation")
              ),
      tabItem("heelResults",
              h3("Achilles Heel Results"),
              helpText("Annotate and view Data Quality results from Achilles Heel"),
              DT::dataTableOutput(outputId = "dtHeelResults") %>% withSpinner(color="#0dc5c1")
              ),
      tabItem("conceptSetKb",
              h3("Concept Set Knowledge Base"),
              fluidRow(
                column(4,
                       DT::dataTableOutput(outputId = "dtConceptSetPicker") %>% withSpinner(color="#0dc5c1"))
              ),
              verbatimTextOutput(outputId = "includedConcepts")
              ),
      tabItem("cohortDefKb",
              h3("Cohort Knowledge Base"),
              helpText("Explore known metadata about a cohort definition"),
              DT::dataTableOutput(outputId = "dtCohortPicker") %>% withSpinner(color="#0dc5c1"),
              tabsetPanel(type = "tabs",
                          tabPanel("Knowledge Base", verbatimTextOutput(outputId = "cohortConcepts"))
                          #tabPanel("JSON", verbatimTextOutput(outputId = "cohortJson")))
              )
          )
      )
  )
)