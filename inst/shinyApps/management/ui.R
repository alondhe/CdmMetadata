library(shiny)
library(DT)

ui <- fluidPage(
  
  # App title ----
  titlePanel(title = div(img(src = "ohdsi_logo.png"), strong(sprintf("CDM Metadata Management: %s", Sys.getenv("sourceName")))), 
             windowTitle = "CDM Metadata Management"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      tabsetPanel(type = "pills",
                  tabPanel("Identify Agent", 
                           h3("Agent Information"),
                           helpText("Enter information about a metadata creator (can be a human or an algorithm)"),
                           
                           selectInput(inputId = "loadOrAddAgent", label = "Load Existing or Add New Agent", 
                                       choices = c("Load Existing" = "load", "Add New" = "add")),
                           
                           conditionalPanel(condition = "input.loadOrAddAgent == 'load'",
                                            DT::dataTableOutput("dtAgent")
                                            ),
                           
                           conditionalPanel(condition = "input.loadOrAddAgent == 'add'",
                             
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
                             actionButton(inputId = "addAgent", label = "Add New Agent")
                           )
                           ),
                  tabPanel("Add New Metadata", 
                           h3("Add New Metadata"),
                           helpText("Enter the new metadata information. Metadata consists of an entity (the artifact of the CDM database that you wish to tag with metadata)
                                    and an activity (the phenomena you wish to describe about the entity)."),
                           
                           selectInput(inputId = "entityConceptId", 
                                       label = "Entity Concept Id", choices = c("None" = 0)),
                           textInput(inputId = "entityAsString",
                                     label = "Entity As String"),
                           textInput(inputId = "entityIdentifier",
                                     label = "Entity Identifer"),
                   
                           selectInput(inputId = "activityConceptId", 
                                       label = "Activity Concept Id", choices = c("None" = 0)),
                           selectInput(inputId = "activityTypeConceptId", 
                                       label = "Activity Type Concept Id", choices = c("None" = 0)),
                           textInput(inputId = "activityAsString",
                                     label = "Activity As String"),
                           
                           #actionButton(inputId = "addAnnotation", label = "Add Annotation(s)"),
                           actionButton(inputId = "addValue", label = "Add Associated Values")
                  ),
                  tabPanel("Add New Annotation", 
                           h3("Add New Annotation"),
                           helpText("Enter new annotation information for an existing entity/activity record.")
                           
                           
                  ),
                  tabPanel("Add Associated Values", 
                           h3("Add Associated Values"),
                           helpText("Enter any associated value with the new metadata or annotation records. You can insert multiple values."),
                           numericInput(inputId = "valueOrdinal", value = 1, label = "Ordinal"),
                           selectInput(inputId = "valueConceptId",
                                       label = "Value Concept Id", choices = c("None" = 0))
                  )
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      
    )
  )
)