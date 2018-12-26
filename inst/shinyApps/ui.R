library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinydashboard)
library(shinycssloaders)

ui <- dashboardPage(
  dashboardHeader(title = "CDM Metadata", titleWidth = "300px",
                  dropdownMenuOutput(outputId = "tasksDropdown"),
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
      menuItem("Sources Overview", tabName = "overview", icon = icon("sitemap"), selected = TRUE),
      menuItem("Source Provenance", tabName = "provenance", icon = icon("database")),
      menuItem("Heel Results", tabName = "heelResults", icon = icon("table")),
      menuItem("Concept Knowledge Base", tabName = "conceptKb", icon = icon("line-chart")),
      menuItem("Concept Set Knowledge Base", tabName = "conceptSetKb", icon = icon("list")),
      menuItem("Cohort Knowledge Base", tabName = "cohortDefKb", icon = icon("globe"))
    )
    # conditionalPanel(condition = "input.tabs == 'provenance'",
    #                  textAreaInput(inputId = "sourceDescription", label = "Source Description", height = "300px"),
    #                  actionButton(inputId = "btnSubmitDescription", label = "Submit", icon = icon("check"))
    #                  ),
    # conditionalPanel(condition = "input.tabs == 'heelResults'", 
    # ),
    # conditionalPanel(condition = "input.tabs == 'conceptKb'",
    #                  #checkboxInput(inputId = "toggleConcepts", label = "See only concepts with metadata", value = FALSE),
    #                  selectInput(inputId = "domainId", label = "Domain", selectize = TRUE,
    #                              choices = domainConceptIds), 
    #                  
    #                  selectInput(inputId = "conceptId", label = "Pick a concept", width = "400px", selectize = TRUE,
    #                              choices = c())
    #                  )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem("overview",
              h3("Sources Overview"),
              helpText("Overview metadata about all sources from your site"),
              #div(id = "overviewBoxes"),
              fluidRow(
                infoBox("CDM Sources", length(cdmSources[sapply(cdmSources, function(c) c$name != "All Sources")]), 
                        icon = icon("sitemap"), fill = TRUE),
                infoBoxOutput(outputId = "numPersons"),
                infoBoxOutput(outputId = "numHumanAgents"),
                infoBoxOutput(outputId = "numAlgorithmAgents"),
                infoBoxOutput(outputId = "propTagged")
              )
              ),
      tabItem("provenance",
              h3("Source Provenance"),
              helpText("Create and view metadata about the source's provenance"),
              div(id = "SourceDescCrud"),
              div(id = "overviewBox")
              ),
      tabItem("conceptKb", 
              h3("Concept"),
              helpText("Create and view metadata about a concept"),
              box(width = 2, selectInput(inputId = "domainId", label = "Domain", selectize = TRUE,
                              choices = domainConceptIds),
                  selectInput(inputId = "conceptId", label = "Pick a concept", width = "400px", selectize = TRUE,
                              choices = c())),
              box(width = 7, 
                plotlyOutput(outputId = "conceptKbPlot") %>% withSpinner(color = spinnerColor) 
              ),
              box(width = 3,
                verbatimTextOutput(outputId = "conceptName", placeholder = TRUE),
                dateInput(inputId = "conceptStartDate", label = "Temporal Event Start Date", value = "1900-01-01"),
                textAreaInput(inputId = "temporalEventValue", label = "Metadata Value"),
                actionButton(inputId = "btnAddTemporalAnnotation", label = "Add Temporal Event Annotation"))
              ),
      tabItem("heelResults",
              h3("Achilles Heel Results"),
              helpText("Annotate and view Data Quality results from Achilles Heel"),
              box(width = 9, DT::dataTableOutput(outputId = "dtHeelResults") %>% withSpinner(color = spinnerColor)),
              box(width = 3, selectInput(inputId = "heelStatus", label = "Heel Status", choices = c("",
                                                                                         "Needs Review", #= 900000519,
                                                                                         "Non-issue", # = 900000518,
                                                                                         "Issue"), # = 900000517),
                              width = "250px"),
                  textAreaInput(inputId = "heelAnnotation", label = "Heel Annotation",
                                rows = 4, resize = "none", width = "250px"),
                  div(style = "display:inline-block;text-align: left;",
                      actionButton(inputId = "btnSubmitHeel", label = "Submit", icon = icon("check"))),
                  div(style = "display:inline-block;text-align: left;",
                      actionButton(inputId = "btnDeleteHeel", label = "Remove", icon = icon("minus"))))
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