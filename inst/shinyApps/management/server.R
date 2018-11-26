library(shiny)
library(DT)
library(magrittr)
library(tidyr)

shinyServer(function(input, output, session) {
  
  Agents <- eventReactive(input$addAgent, {
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
    
    sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_agent;",
                                resultsDatabaseSchema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    df
  }, ignoreNULL = FALSE)
  
  
  getMaxId <- function(tableName, fieldName) {
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
  
  observeEvent(input$addAgent, {
    resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"), 
                                                                    server = Sys.getenv("server"), 
                                                                    port = Sys.getenv("port"))
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    df <- data.frame(
      meta_agent_id = getMaxId(tableName = "meta_agent", fieldName = "meta_agent_id") + 1,
      meta_agent_concept_id = 1000,
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
    
    showNotification(sprintf("New agent added"))
    
    choices <- c("Load Existing" = "load", "Add New" = "add")
    updateSelectInput(session, "loadOrAddAgent",
                      label = "Load Existing or Add New Agent", 
                      choices = choices,
                      selected = head(choices, 1)
    )
    
    input$loadOrAddAgent
  })
  
  output$dtAgent <- renderDataTable({
    Agents()
  })
  
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