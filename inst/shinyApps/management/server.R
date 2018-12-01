library(shiny)
library(DT)
library(magrittr)
library(tidyr)

shinyServer(function(input, output, session) {

  
  agentTextInputs <- c("agentFirstName",
                       "agentLastName",
                       "agentSuffix",
                       "agentAlgorithm",
                       "agentDescription")
  
  agentChoices <- c("Human" = "hum", "Algorithm" = "alg")
    
  .clearTextInputs <- function(textInputNames) {
    for (t in textInputNames) {
      updateTextInput(session, t, value = "")
    }  
    updateActionButton(session = session, inputId = "btnSubmitAgent", label = "Add New Agent")
  }
  
  .getAgents <- function() {
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
    
    sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_agent
                                order by meta_agent_id;",
                                resultsDatabaseSchema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    DatabaseConnector::disconnect(connection = connection)  
    
    df
  }
  
  output$dtAgent <- renderDataTable({
    input$btnSubmitAgent
    input$btnDeleteAgent
    df <- .getAgents()
    
    options <- list(pageLength = 10000,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = FALSE,
                    scrollY = '75vh')
    selection <- list(mode = "single", target = "row")

    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE,
                       class = "stripe nowrap compact", extensions = c("Responsive"))
    table <- formatStyle(table = table,
                         columns = ncol(df),
                         target = "row")
    table
  }, server = FALSE)
  
  
  .getMaxId <- function(tableName, fieldName) {
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
    sql <- SqlRender::renderSql(sql = "select max(@fieldName) as MAX_ID from @resultsDatabaseSchema.@tableName;",
                                fieldName = fieldName,
                                tableName = tableName,
                                resultsDatabaseSchema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    
    result <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    if (is.na(result)) {
      0
    } else {
      as.integer(result$MAX_ID)
    }
  }
  
  .addAgent <- function() {
    resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    
    df <- data.frame(
      meta_agent_id = .getMaxId(tableName = "meta_agent", fieldName = "meta_agent_id") + 1,
      meta_agent_concept_id = ifelse(input$agentConcept == "hum", 1000, 2000),
      agent_first_name = input$agentFirstName,
      agent_last_name = input$agentLastName,
      agent_suffix = input$agentSuffix,
      agent_algorithm = input$agentAlgorithm,
      agent_description = input$agentDescription
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_agent", resultsDatabaseSchema), 
                                   data = df, 
                                   dropTableIfExists = FALSE, createTable = FALSE, useMppBulkLoad = FALSE) 
    DatabaseConnector::disconnect(connection = connection)
    
    showNotification(sprintf("New agent added"))
    
    .clearTextInputs(textInputNames = agentTextInputs)
  }
  
  .updateAgent <- function() {
    
    resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    
    row_count <- input$dtAgent_rows_selected
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "management/updateAgent.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema,
                                             agentFirstName = input$agentFirstName,
                                             agentLastName = input$agentLastName,
                                             agentSuffix = input$agentSuffix,
                                             agentAlgorithm = input$agentAlgorithm,
                                             agentDescription = input$agentDescription,
                                             metaAgentId = .getAgents()[row_count, ]$META_AGENT_ID)

    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    DatabaseConnector::disconnect(connection = connection)
    
    showNotification(sprintf("Agent updated"))
  }
  
  .deleteAgent <- function() {
    resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    
    row_count <- input$dtAgent_rows_selected
    metaAgentId <- .getAgents()[row_count, ]$META_AGENT_ID
    
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_agent where meta_agent_id = '@metaAgentId';",
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                metaAgentId = metaAgentId)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    DatabaseConnector::disconnect(connection = connection)
    
    showNotification(sprintf("Agent deleted"))
  }
  
  observeEvent(input$btnDeleteAgent, handlerExpr = {
    .deleteAgent()
    .clearTextInputs(agentTextInputs)
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnClearAgent, handlerExpr = {
    .clearTextInputs(textInputNames = agentTextInputs)
  })
  
  observeEvent(eventExpr = input$dtAgent_rows_selected, handlerExpr = {
    updateActionButton(session = session, inputId = "btnSubmitAgent", label = "Update Agent")
    row_count <- input$dtAgent_rows_selected

    if (.getAgents()[row_count,]$META_AGENT_CONCEPT_ID == 1000) {
      updateSelectInput(session = session, inputId = "agentConcept", selected = agentChoices[1])
    } else {
      updateSelectInput(session = session, inputId = "agentConcept", selected = agentChoices[2])
    }
    
    for (t in agentTextInputs) {
      updateTextInput(session = session,
                      inputId = t,
                      value = .getAgents()[row_count,][toupper(SqlRender::camelCaseToSnakeCase(t))][[1]])
    }
  })
  
  observeEvent(eventExpr = input$btnSubmitAgent, handlerExpr = {
    agentSelected <- !is.null(input$dtAgent_rows_selected)
    if (agentSelected) {
      .updateAgent()
    } else {
      .addAgent()
    }
  }, priority = 1)
  
  EntityActivities <- reactive({
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_entity_activity;",
                                resultsDatabaseSchema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    DatabaseConnector::disconnect(connection = connection)
    
    df
  })
  
  
  Annotations <- reactive({
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_annotation;",
                                resultsDatabaseSchema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    DatabaseConnector::disconnect(connection = connection)
    
    df
  })
 
  
})