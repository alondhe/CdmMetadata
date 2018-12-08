library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)

ui <- fluidPage(
  
  # setBackgroundColor(color = "#003142", gradient = c("linear",
  #                                                       "radial"), direction = c("bottom", "top", "right", "left")),
  
  # App title ----
  titlePanel(title = div(img(src = "ohdsi_logo.png"), strong(sprintf("CDM Metadata: %s", Sys.getenv("sourceName")))), 
             windowTitle = "CDM Metadata"),
  
  tabsetPanel(type = "pills",
              tabPanel(title = "Manage Metadata", icon = icon("wrench"),
                       tabsetPanel(type = "tabs",
                                   tabPanel(title = "Agents", 
                                            h3("Agent Information"),
                                            helpText("Manage information about a metadata creator (can be a human or an algorithm)"),
                                            sidebarLayout(
                                              sidebarPanel(
                                                selectInput(inputId = "agentConceptId", label = "Agent Type", 
                                                            choices = c("Human" = 1000, "Algorithm" = 2000), width = "200px"),
                                                conditionalPanel(condition = "input.agentConceptId == 1000",
                                                                 textInput(inputId = "agentFirstName", label = "First Name", width = "200px"),
                                                                 textInput(inputId = "agentLastName", label = "Last Name", width = "200px"),
                                                                 textInput(inputId = "agentSuffix", label = "Suffix", width = "200px")
                                                                 ),
                                                conditionalPanel(condition = "input.agentConceptId == 2000",
                                                                 textInput(inputId = "agentAlgorithm", label = "Algorithm Name", width = "200px"),
                                                                 textAreaInput(inputId = "agentDescription", label = "Description", 
                                                                               rows = 4, resize = "none", width = "200px")
                                                                 ),
                                                actionButton(inputId = "btnClearAgent", label = "Clear Selected", icon = icon("edit")),
                                                actionButton(inputId = "btnSubmitAgent", label = "Add New", icon = icon("check")),
                                                actionButton(inputId = "btnDeleteAgent", label = "Delete Selected", icon = icon("minus")), width = 3
                                                ),
                                              mainPanel(DT::dataTableOutput("dtAgent"))
                                              )
                                            ),
                                   tabPanel("Entities and Activities", 
                                            
                                            wellPanel(style = "background: #003142",
                                                      h4("Selected Agent", style = "color: white"),
                                                      span(htmlOutput(outputId = "selectedAgentEA"), style = "color: white")
                                            ),
                                            
                                            h3("Entities and Activities"), 
                                            helpText("Enter new metadata information. Metadata consists of an entity (the artifact of the CDM database that you wish to tag with metadata) 
                                                     and an activity (the phenomena you wish to describe about the entity)."),
                                            fluidRow(
                                              column(3,
                                                     wellPanel(
                                                       textInput(inputId = "entityConceptId", label = "Entity Concept Id", width = "200px", value = 0),
                                                       textInput(inputId = "entityAsString", label = "Entity As String", width = "200px"), 
                                                       textInput(inputId = "entityIdentifier", label = "Entity Identifer", width = "200px"),
                                                       textInput(inputId = "activityConceptId", label = "Activity Concept Id", width = "200px", value = 0), 
                                                       # selectInput(inputId = "activityConceptClass", label = "Activity Concept Class", width = "200px",
                                                       #             choices = c("Data Quality",
                                                       #                         "Provenance",
                                                       #                         "ETL/Design")),
                                                       # selectInput(inputId = "activityConceptId2", label = "Activity", width = "200px", 
                                                       #             choices = c("Conformance" = 900000112,
                                                       #                         "Completeness" = 900000113,
                                                       #                         "Plausibility" = 900000114	,
                                                       #                         "Temporal Event" = 900000115,
                                                       #                         "Value" = 900000116)),
                                                       textInput(inputId = "activityTypeConceptId", label = "Activity Type Concept Id", width = "200px", value = 0),
                                                       textInput(inputId = "activityAsString", label = "Activity As String", width = "200px"),
                                                       dateRangeInput(inputId = "activityDates", label = "Activity Date Start and End", width = "200px"),
                                                       selectInput(inputId = "securityConceptIdEA", label = "Security Concept Id", choices = c("License Not Required" = 0), 
                                                                   width = "200px"),
                                                       
                                                       actionButton(inputId = "btnClearEA", label = "Clear Selected", icon = icon("edit")),
                                                       actionButton(inputId = "btnSubmitEA", label = "Add New", icon = icon("check")),
                                                       actionButton(inputId = "btnDeleteEA", label = "Delete Selected", icon = icon("minus"))
                                                       )
                                                     ),
                                              column(9,
                                                     mainPanel(DT::dataTableOutput("dtEntityActivity"))
                                              )
                                            ),
                                            h3("Associated Values"),
                                            helpText("Enter any associated value with the selected Entity/Activity record. You can insert multiple values."),
                                            fluidRow(
                                              column(3,
                                                     wellPanel(
                                                       numericInput(inputId = "valueOrdinalEA", value = 1, label = "Ordinal", width = "200px"),
                                                       textInput(inputId = "valueConceptIdEA", label = "Value Concept Id", width = "200px", value = 0),
                                                       textInput(inputId = "valueTypeConceptIdEA", label = "Value Type Concept Id", width = "200px", value = 0),
                                                       textInput(inputId = "valueAsStringEA", label = "Value As String", width = "200px"),
                                                       numericInput(inputId = "valueAsNumberEA", label = "Value As Number", value = 0, width = "200px"),
                                                       textInput(inputId = "operatorConceptIdEA", label = "Operator Concept Id", value = 0, width = "200px"), 
                                                       actionButton(inputId = "btnClearValueEA", label = "Clear Selected", icon = icon("edit")),
                                                       actionButton(inputId = "btnSubmitValueEA", label = "Add New", icon = icon("check")),
                                                       actionButton(inputId = "btnDeleteValueEA", label = "Delete Selected", icon = icon("minus"))  
                                                     )
                                              ),
                                              column(9,
                                                     DT::dataTableOutput("dtValueEA")
                                              )
                                            )
                                          ),
                                   tabPanel("Annotations", 
                                            wellPanel(style = "background: #003142",
                                                      h4("Selected Agent", style = "color: white"),
                                                      span(htmlOutput(outputId = "selectedAgentAnn"), style = "color: white"),
                                                      span(htmlOutput(outputId = "selectedEAAnn"), style = "color: white")
                                            ),
                                            h3("Annotations"),
                                            helpText("Enter 1 or more annotations for an existing entity/activity record."),
                                            fluidRow(
                                              column(3, 
                                                     wellPanel(
                                                       textInput(inputId = "annotationConceptId", label = "Annotation Concept Id", width = "200px", value = 0),
                                                       textInput(inputId = "annotationTypeConceptId", label = "Annotation Type Concept Id", width = "200px", value = 0),
                                                       selectInput(inputId = "securityConceptIdAnnotation", label = "Security Concept Id", choices = c("License Not Required" = 0), 
                                                                   width = "200px"),
                                                       actionButton(inputId = "btnClearAnnotation", label = "Clear Selected", icon = icon("edit")),
                                                       actionButton(inputId = "btnSubmitAnnotation", label = "Add New", icon = icon("check")),
                                                       actionButton(inputId = "btnDeleteAnnotation", label = "Delete Selected", icon = icon("minus")), width = 3 
                                                     )
                                              ),
                                              column(5, 
                                                     DT::dataTableOutput("dtAnnotation")
                                              )
                                            ),
                                            h3("Associated Values"),
                                            helpText("Enter any associated value with the selected annotation record. You can insert multiple values."),
                                            fluidRow(
                                              column(3,
                                                     wellPanel(
                                                       numericInput(inputId = "valueOrdinalAnn", value = 1, label = "Ordinal", width = "200px"),
                                                       textInput(inputId = "valueConceptIdAnn", label = "Value Concept Id", width = "200px", value = 0),
                                                       textInput(inputId = "valueTypeConceptIdAnn", label = "Value Type Concept Id", width = "200px", value = 0),
                                                       textInput(inputId = "valueAsStringAnn", label = "Value As String", width = "200px"),
                                                       numericInput(inputId = "valueAsNumberAnn", label = "Value As Number", value = 0, width = "200px"),
                                                       textInput(inputId = "operatorConceptIdAnn", label = "Operator Concept Id", width = "200px", value = 0), 
                                                       actionButton(inputId = "btnClearValueAnn", label = "Clear Selected", icon = icon("edit")),
                                                       actionButton(inputId = "btnSubmitValueAnn", label = "Add New", icon = icon("check")),
                                                       actionButton(inputId = "btnDeleteValueAnn", label = "Delete Selected", icon = icon("minus"))  
                                                       )
                                                     ),
                                              column(5,
                                                     DT::dataTableOutput("dtValueAnnotation")
                                                     )
                                              )
                                            )
                                   )
                       ),
              tabPanel(title = "Explore Metadata", icon = icon("play"),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Concept", 
                                            h3("Concept"),
                                            helpText("View metadata about a concept"),
                                            
                                            checkboxInput(inputId = "toggleConcepts", label = "See only concepts with metadata", value = FALSE),
                                            selectInput(inputId = "domainId", label = "Domain",
                                                        choices = c("Condition" = 402, 
                                                                    "Procedure" = 602, "Device" = 2101, "Drug" = 702, "Measurement" = 1801, "Observation" = 802)), 
                                            
                                            selectInput(inputId = "conceptId", label = "Pick a concept", width = "400px",
                                                        choices = c()), #c("Biopsy of lymphatic structure" = 2002290)),
                                            verbatimTextOutput(outputId = "selectedConcept", placeholder = TRUE),
                                            plotlyOutput(outputId = "conceptPlot")
                                            
                                            ),
                                   tabPanel("Heel Results", 
                                            h3("Heel Results"),
                                            helpText("View metadata about elements of a cohort definition"))
                       )
              )
  )
)