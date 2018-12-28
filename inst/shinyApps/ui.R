library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinydashboard)
library(shinycssloaders)

ui <- dashboardPage(
  dashboardHeader(title = "CDM METADATA", titleWidth = "300px",
                  dropdownMenuOutput(outputId = "tasksDropdown"),
                  tags$li(a(href = 'http://www.ohdsi.org', target = "_blank",
                            img(src = 'ohdsi_logo_mini.png',
                                title = "OHDSI", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown"),
                  tags$li(a(href = atlasUrl, target = "_blank",
                            img(src = 'atlas_logo.png',
                                title = "Atlas", height = "30px"),
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
      menuItem("Site Overview", tabName = "overview", icon = icon("sitemap"), selected = TRUE),
      menuItem("Source Provenance", tabName = "provenance", icon = icon("database")),
      menuItem("Heel Results", tabName = "heelResults", icon = icon("table")),
      menuItem("Concept Knowledge Base", tabName = "conceptKb", icon = icon("line-chart")),
      menuItem("Concept Set Knowledge Base", tabName = "conceptSetKb", icon = icon("list")),
      menuItem("Cohort Knowledge Base", tabName = "cohortDefKb", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem("overview",
              h3("Site Overview"),
              helpText("View metadata about all CDM sources in the site"),
              fluidRow(
                infoBox("Number of CDM Sources", length(cdmSources[sapply(cdmSources, function(c) c$name != "All Sources")]), 
                        icon = icon("sitemap"), fill = TRUE),
                infoBoxOutput(outputId = "numPersons"),
                infoBoxOutput(outputId = "numHumanAgents"),
                infoBoxOutput(outputId = "numAlgorithmAgents"),
                infoBoxOutput(outputId = "propTagged")
              )
              ),
      tabItem("provenance",
              h3("Source Provenance"),
              helpText("Create and view metadata about the selected CDM source's provenance"),
              fluidRow(
                div(id = "SourceDescCrud"),
                div(id = "overviewBox")  
              )
              ),
      tabItem("conceptKb", 
              h3("Concept Knowledge Base"),
              helpText("Create and view known temporal events about a concept"),
              fluidRow(
                box(width = 3, selectInput(inputId = "domainId", label = "Domain", selectize = TRUE,
                                           choices = c()),
                    selectInput(inputId = "conceptId", label = "Pick a concept", width = "400px", selectize = TRUE,
                                choices = c()),
                    selectInput(inputId = "conceptStartDate", label = "Select Date", choices = c()),
                    textAreaInput(inputId = "temporalEventValue", label = "Temporal Event Description", 
                                  placeholder = "Enter a description of the temporal event connected to this concept at the above date"),
                    actionButton(inputId = "btnAddTemporalEvent", label = "Add", icon = icon("check")),
                    actionButton(inputId = "btnEditTemporalEvent", label = "Edit", icon = icon("edit")),
                    actionButton(inputId = "btnDeleteTemporalEvent", label = "Delete", icon = icon("minus"))),
                box(width = 9, 
                    plotlyOutput(outputId = "conceptKbPlot") %>% withSpinner(color = spinnerColor) 
                )    
              ),
              fluidRow(
                box(width = 12,
                    DT::dataTableOutput(outputId = "dtTemporalEvent") %>% withSpinner(color="#0dc5c1")) 
              )
              ),
      tabItem("heelResults",
              h3("Achilles Heel Results"),
              helpText("Annotate and view Data Quality results from Achilles Heel"),
              fluidRow(
                box(width = 9, DT::dataTableOutput(outputId = "dtHeelResults") %>% withSpinner(color = spinnerColor)),
                box(width = 3, 
                    div(strong("(1) Download Current Heel Results"), downloadButton(outputId = "downloadHeelResults", label = "Download CSV")),
                    fileInput(inputId = "uploadHeelAnnotations", label = "(2) Annotate the CSV and re-upload",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    selectInput(inputId = "heelStatus", label = "Heel Status", selectize = TRUE,
                                           choices = c("",
                                                       "Needs Review", #= 900000519,
                                                       "Non-issue", # = 900000518,
                                                       "Issue"), # = 900000517),
                                width = "250px"),
                    textAreaInput(inputId = "heelAnnotation", label = "Heel Annotation", placeholder = "None selected",
                                  rows = 4, resize = "none", width = "250px"),
                    div(style = "display:inline-block;text-align: left;",
                        actionButton(inputId = "btnSubmitHeel", label = "Submit", icon = icon("check"))),
                    div(style = "display:inline-block;text-align: left;",
                        actionButton(inputId = "btnDeleteHeel", label = "Remove", icon = icon("minus"))))
              )
              ),
      tabItem("conceptSetKb",
              h3("Concept Set Knowledge Base"),
              helpText("Explore known metadata about a concept set"),
              fluidRow(
                column(4,
                       DT::dataTableOutput(outputId = "dtConceptSetPicker") %>% withSpinner(color="#0dc5c1")),
                column(8,
                       DT::dataTableOutput(outputId = "dtConceptSetMeta") %>% withSpinner(color="#0dc5c1"))
              )
              ),
      tabItem("cohortDefKb",
              h3("Cohort Knowledge Base"),
              helpText("Explore known metadata about a cohort definition"),
              fluidRow(
                column(5,
                       DT::dataTableOutput(outputId = "dtCohortPicker") %>% withSpinner(color="#0dc5c1")),
                column(7,
                       actionButton(inputId = "btnGetCohortMeta", label = "Fetch Known Metadata", icon = icon("database")),
                       br(),
                       uiOutput(outputId = "knownCohortMeta")
                )
              )
          )
      )
  )
)