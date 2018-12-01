library(shiny)
library(DT)

ui <- fluidPage(
  
  # App title ----
  titlePanel(title = div(img(src = "ohdsi_logo.png"), strong(sprintf("CDM Metadata: %s", Sys.getenv("sourceName")))), 
             windowTitle = "CDM Metadata"),
  
  tabsetPanel(type = "pills",
              tabPanel("Manage Metadata",
                       tabsetPanel(type = "tabs",
                                   tabPanel("Agents", 
                                            h3("Agent Information"),
                                            helpText("Manage information about a metadata creator (can be a human or an algorithm)"),
                                            sidebarLayout(
                                              sidebarPanel(
                                                selectInput(inputId = "agentConcept", label = "Agent Type", choices = c("Human" = "hum", "Algorithm" = "alg")),
                                                conditionalPanel(condition = "input.agentConcept == 'hum'",
                                                                 textInput(inputId = "agentFirstName", label = "First Name"),
                                                                 textInput(inputId = "agentLastName", label = "Last Name"),
                                                                 textInput(inputId = "agentSuffix", label = "Suffix")
                                                                 ),
                                                conditionalPanel(condition = "input.agentConcept == 'alg'",
                                                                 textInput(inputId = "agentAlgorithm", label = "Algorithm Name"),
                                                                 textAreaInput(inputId = "agentDescription", label = "Description", 
                                                                               rows = 4, resize = "none")
                                                                 ),
                                                actionButton(inputId = "btnClearAgent", label = "Clear Selected Agent", icon = icon("user-edit")),
                                                actionButton(inputId = "btnSubmitAgent", label = "Add New Agent"),
                                                actionButton(inputId = "btnDeleteAgent", label = "Delete Selected Agent", icon = icon("user-minus"))
                                                ),
                                              mainPanel(DT::dataTableOutput("dtAgent"))
                                              )
                                            ),
                                   tabPanel("Entities and Activities", 
                                            h3("Entities and Activities"), 
                                            helpText("Enter the new metadata information. Metadata consists of an entity (the artifact of the CDM database that you wish to tag with metadata) 
                                                     and an activity (the phenomena you wish to describe about the entity)."),
                                            selectInput(inputId = "entityConceptId", label = "Entity Concept Id", choices = c("None" = 0)),
                                            textInput(inputId = "entityAsString", label = "Entity As String"), textInput(inputId = "entityIdentifier", label = "Entity Identifer"),
                                            selectInput(inputId = "activityConceptId", label = "Activity Concept Id", choices = c("None" = 0)), 
                                            selectInput(inputId = "activityTypeConceptId",
                                                        label = "Activity Type Concept Id", choices = c("None" = 0)),
                                            textInput(inputId = "activityAsString",
                                                      label = "Activity As String"),
                                            dateRangeInput(inputId = "activityDates", label = "Activity Date Start and End"),
                                            #actionButton(inputId = "addAnnotation", label = "Add Annotation(s)"),
                                            actionButton(inputId = "addValue", label = "Add Associated Values")
                                            ),
                                   tabPanel("Annotations", 
                                            h3("Annotations"),
                                            helpText("Enter new annotation information for an existing entity/activity record.")
                                            ),
                                   tabPanel("Associated Values", 
                                            h3("Associated Values"),
                                            helpText("Enter any associated value with the new metadata or annotation records. You can insert multiple values."),
                                            numericInput(inputId = "valueOrdinal", value = 1, label = "Ordinal"),
                                            selectInput(inputId = "valueConceptId",
                                                        label = "Value Concept Id", choices = c("None" = 0))
                                            )
                                   )
                       ),
              tabPanel("Explore Metadata")
              )
  )