library(shiny)
library(DT)
library(magrittr)
library(tidyr)

shinyServer(function(input, output, session) {
  
  dtAgentProxy <- dataTableProxy("dtAgent")
  dtEAProxy <- dataTableProxy("dtEntityActivity")
  dtValueEAProxy <- dataTableProxy("dtValueEA")
  dtValueAnnProxy <- dataTableProxy("dtValueAnnotation")
  dtAnnotationProxy <- dataTableProxy("dtAnnotation")
  
  resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
  cdmDatabaseSchema <- Sys.getenv("cdmDatabaseSchema")
  vocabDatabaseSchema <- Sys.getenv("vocabDatabaseSchema")
  
  if (Sys.getenv("user") == "") {
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"),
                                                                    server = Sys.getenv("server"),
                                                                    port = Sys.getenv("port"))
  } else {
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"),
                                                                    user = Sys.getenv("user"),
                                                                    password = Sys.getenv("password"),
                                                                    server = Sys.getenv("server"),
                                                                    port = Sys.getenv("port"))
  }
  

  # Text Inputs ------------------------------------------------------
  
  agentTextInputs <- c("agentFirstName",
                       "agentLastName",
                       "agentSuffix",
                       "agentAlgorithm",
                       "agentDescription")
  
  entityActivityTextInputs <- c(
    "entityConceptId",
    "entityAsString", 
    "entityIdentifier",
    "activityConceptId",
    "activityTypeConceptId",
    "activityAsString"
  )
  
  annotationTextInputs <- c(
    "annotationConceptId",
    "annotationTypeConceptId",
    "securityConceptIdAnnotation"
  )
  
  valueTextInputs <- function(suffix) {
    lapply(c(
      "valueOrdinal",
      "valueConceptId",
      "valueTypeConceptId",
      "valueAsString",
      "valueAsNumber",
      "operatorConceptId"),  function(v) {
        paste0(v, suffix)
      })
  }
  
  agentChoices <- c("Human" = "1000", "Algorithm" = "2000")

  .clearTextInputs <- function(textInputNames, submitButtonId) {
    for (t in textInputNames) {
      updateTextInput(session, t, value = "")
    }  
    if (!is.null(submitButtonId)) {
      updateActionButton(session = session, inputId = submitButtonId, label = "Add New")
    }
  }
  
  .getMaxId <- function(tableName, fieldName) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
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
  
  .getSelectedAgentText <- function() {
    if (is.null(input$dtAgent_rows_selected)) {
      "No Agent Selected"
    } else {
      if (input$agentConceptId == 1000) {
        sprintf("Human: %1s %2s (%3s)",
                input$agentFirstName,
                input$agentLastName,
                input$agentSuffix)  
      } else {
        sprintf("Algorithm: %1s (%2s)",
                input$agentAlgorithm,
                input$agentDescription)  
      }  
    }
  }
  
  .getSelectedEAText <- function() {
    if (is.null(input$dtEntityActivity_rows_selected)) {
      "No Entity/Activity Selected"
    } else {
      sprintf("<b>Entity</b>: %1s<br/>
              <b>Activity</b>: %2s<br/>",
              input$entityConceptId,
              input$activityConceptId)  
    }
  }
  
  # exists functions ---------------------------------------------------------------
  
  .agentRecordExists <- function() {
    df <- .getAgents() %>%
      dplyr::filter(META_AGENT_CONCEPT_ID == input$agentConceptId &
                    AGENT_FIRST_NAME == input$agentFirstName & 
                    AGENT_LAST_NAME == input$agentLastName & 
                    AGENT_SUFFIX == input$agentSuffix &
                    AGENT_ALGORITHM == input$agentAlgorithm &
                    AGENT_DESCRIPTION == input$agentDescription)

      nrow(df) > 0
  }
  
  .entityActivityRecordExists <- function() {
    df <- .getEntityActivities() %>%
      dplyr::filter(ACTIVITY_CONCEPT_ID == input$activityConceptId,
                    ACTIVITY_TYPE_CONCEPT_ID == input$activityTypeConceptId,
                    ACTIVITY_AS_STRING == input$activityAsString,
                    ENTITY_CONCEPT_ID == input$entityConceptId,
                    ENTITY_AS_STRING == input$entityAsString,
                    ENTITY_IDENTIFIER == input$entityIdentifier,
                    ACTIVITY_START_DATE == input$activityDates[1],
                    ACTIVITY_END_DATE == input$activityDates[2])
    
    nrow(df) > 0
  }
  
  .annotationRecordExists <- function() {
    
    row_count <- input$dtAgent_rows_selected
    metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    
    df <- .getAnnotations() %>%
      dplyr::filter(META_ENTITY_ACTIVITY_ID == metaEntityActivityId,
                    ANNOTATION_CONCEPT_ID == input$annotationConceptId,
                    ANNOTATION_TYPE_CONCEPT_ID == input$annotationTypeConceptId
                    )
    
    nrow(df) > 0
  }
  
  # query tables -----------------------------------------------------------------
  
  .getConceptPrevalance <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "conceptExplore/getPrevalence.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema,
                                             conceptId = input$conceptId,
                                             analysisId = input$domainId)
    
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    if (nrow(df) > 0) {
      df$STRATUM_2 <- as.Date(paste0(df$STRATUM_2, '01'), format='%Y%m%d')  
    }
    
    df
  }
  
  .getChartMeta <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "conceptExplore/getChartEntityActivity.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema,
                                             entityConceptId = input$conceptId)
    
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .getAgents <- function() {

    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    metaAgentConceptId <- input$agentConceptId
    
    sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_agent
                                where meta_agent_concept_id = @metaAgentConceptId
                                order by meta_agent_id;",
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                metaAgentConceptId = metaAgentConceptId)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .getEntityActivities <- function(subset = TRUE) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (subset) {
      row_count <- input$dtAgent_rows_selected
      metaAgentId <- .getAgents()[row_count, ]$META_AGENT_ID
    } else {
      metaAgentId <- NA
    }
    
    df <- tryCatch({
      sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_entity_activity
                                where 1=1 {@subset}?{and meta_agent_id = @metaAgentId}
                                order by meta_entity_activity_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema,
                                  subset = subset,
                                  metaAgentId = metaAgentId)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      df <- NULL 
    })
    
    df
  }
  
  .getAnnotations <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    
    df <- tryCatch({
      sql <- SqlRender::renderSql(sql = "select * from @resultsDatabaseSchema.meta_annotation 
                                  where meta_entity_activity_id = @metaEntityActivityId
                                  order by meta_annotation_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema,
                                  metaEntityActivityId = metaEntityActivityId)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      df <- NULL 
    })
    
    df
  }
  
  .getValues <- function(metaAnnotationId = NULL) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    
    df <- tryCatch({
      sql <- SqlRender::renderSql(sql = "select * from @resultsDatabaseSchema.meta_value 
                                  where meta_entity_activity_id = @metaEntityActivityId
                                  {@isAnnotation}?{and meta_annotation_id = @metaAnnotationId}
                                  order by meta_annotation_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema,
                                  metaEntityActivityId = metaEntityActivityId,
                                  metaAnnotationId = metaAnnotationId,
                                  isAnnotation = !is.null(metaAnnotationId))$sql
      
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      df <- NULL 
    })
    
    df
  }
  
  
  # output rendering -----------------------------------------------------------
  
  observeEvent(input$toggleConcepts, {
    
    if (input$toggleConcepts) {
      selected <- achillesConcepts[achillesConcepts$ANALYSIS_ID == input$domainId,]
      
      meta <- .getEntityActivities(FALSE)
      
      meta <- meta$ENTITY_CONCEPT_ID[meta$ACTIVITY_AS_STRING == "Temporal Event"]
      selected <- selected[selected$CONCEPT_ID %in% meta,]
      
      choices <- setNames(as.integer(selected$CONCEPT_ID), paste(selected$CONCEPT_ID, as.character(selected$CONCEPT_NAME), sep = " - "))
      
      updateSelectInput(session = session, inputId = "conceptId", choices = choices)
    }
  })
  
  observeEvent(input$domainId, {

    selected <- achillesConcepts[achillesConcepts$ANALYSIS_ID == input$domainId,]

    choices <- setNames(as.integer(selected$CONCEPT_ID), paste(selected$CONCEPT_ID, as.character(selected$CONCEPT_NAME), sep = " - "))
    updateSelectInput(session = session, inputId = "conceptId", choices = choices)
  })
  
  output$selectedConcept <- renderText({
    input$conceptId
  })
  
  output$conceptPlot <- renderPlotly({
    
    df <- .getConceptPrevalance()
    
    meta <- .getChartMeta()
    
    if (nrow(meta) > 0) {
      chartDate <- lubridate::floor_date(meta$ACTIVITY_START_DATE, "month")
      a <- list(
        x = chartDate,
        y = df$COUNT_VALUE[df$STRATUM_2 == chartDate],
        text = meta$VALUE_AS_STRING,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 7
      )
      
      plot_ly(df, x = ~STRATUM_2, y = ~COUNT_VALUE, type = "scatter", mode = "lines", hoverinfo = 'text') %>%
        layout(xaxis = list(title = "Date"), yaxis = list(title = "# of Events")) %>%
        layout(annotations = a)  
    } else {
      plot_ly(df, x = ~STRATUM_2, y = ~COUNT_VALUE, type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Date"), yaxis = list(title = "# of Events"))
    }
    
  })
  
  output$selectedAgentEA <- renderText({
    .getSelectedAgentText()  
  })
  
  output$selectedAgentAnn <- renderText({
    .getSelectedAgentText()
  })
  
  output$selectedEAAnn <- renderText({
    .getSelectedEAText()
  })
  
  output$dtAgent <- renderDataTable({
    input$btnSubmitAgent
    input$btnDeleteAgent
    
    if (input$agentConceptId == 1000) {
      df <- .getAgents() %>%
        dplyr::select(AGENT_FIRST_NAME,
                      AGENT_LAST_NAME,
                      AGENT_SUFFIX)
      colnames <- c("First Name", "Last Name", "Suffix")
    } else {
      df <- .getAgents() %>%
        dplyr::select(AGENT_ALGORITHM,
                      AGENT_DESCRIPTION)
      colnames <- c("Algorithm Name", "Algorithm Description")
    }
    
    
    
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
                       rownames = FALSE, colnames = colnames,
                       class = "stripe nowrap compact", extensions = c("Responsive"))
    table <- formatStyle(table = table,
                         columns = ncol(df),
                         target = "row")
    table
  }, server = FALSE)
  
  
  output$dtEntityActivity <- renderDataTable({
    input$btnClearAgent
    input$btnSubmitEA
    input$btnDeleteEA
    df <- .getEntityActivities()
    
    if (!is.null(df)) {
      df <- dplyr::select(df, 
                          ENTITY_CONCEPT_ID,
                          ENTITY_AS_STRING,
                          ENTITY_IDENTIFIER,
                          ACTIVITY_CONCEPT_ID,
                          ACTIVITY_TYPE_CONCEPT_ID,
                          ACTIVITY_AS_STRING,
                          ACTIVITY_START_DATE,
                          ACTIVITY_END_DATE,
                          SECURITY_CONCEPT_ID)
      
      colnames <- c("Entity Concept Id", 
                    "Entity As String",
                    "Entity Identifier",
                    "Activity Concept Id",
                    "Activity Type Concept Id",
                    "Activity As String",
                    "Activity Start Date",
                    "Activity End Date",
                    "Security Concept Id")
      
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
                         rownames = FALSE, colnames = colnames,
                         class = "stripe nowrap compact", extensions = c("Responsive"))
      table <- formatStyle(table = table,
                           columns = ncol(df),
                           target = "row")
      table  
    }
  }, server = FALSE)
  
  output$dtAnnotation <- renderDataTable({
    input$btnClearAnnotation
    input$btnSubmitAnnotation
    input$btnDeleteAnnotation
    df <- .getAnnotations()
    
    if (!is.null(df)) {
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
    }
  }, server = FALSE)
  
  output$dtValueEA <- renderDataTable({
    input$btnClearValueEA
    input$btnSubmitValueEA
    input$btnDeleteValueEA
    df <- .getValues()
    
    if (!is.null(df)) {
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
    }
  }, server = FALSE)
  
  
  # crud operations -----------------------------------------------------------
  
  .addAgent <- function() {

    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))

    if (.agentRecordExists()) {
      showNotification(ui = "Agent already exists", type = "error")
    } else {
      df <- data.frame(
        meta_agent_id = .getMaxId(tableName = "meta_agent", fieldName = "meta_agent_id") + 1,
        meta_agent_concept_id = as.integer(input$agentConceptId),
        agent_first_name = input$agentFirstName,
        agent_last_name = input$agentLastName,
        agent_suffix = input$agentSuffix,
        agent_algorithm = input$agentAlgorithm,
        agent_description = input$agentDescription
      )
      
      df[df == ""] <- NA
      DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_agent", resultsDatabaseSchema), 
                                     data = df, 
                                     dropTableIfExists = FALSE, createTable = FALSE, useMppBulkLoad = FALSE) 
      
      showNotification(sprintf("New agent added"))
      
      .clearTextInputs(textInputNames = agentTextInputs, "btnSubmitAgent")
    }
  }
  
  .addEntityActivity <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (.entityActivityRecordExists()) {
      showNotification(ui = "Entity/Activity already exists", type = "error")
    } else {
      
      row_count <- input$dtAgent_rows_selected
      metaAgentId <- .getAgents()[row_count, ]$META_AGENT_ID

        df <- data.frame(
        meta_entity_activity_Id = .getMaxId("meta_entity_activity", "meta_entity_activity_id") + 1,
        meta_agent_id = as.integer(metaAgentId),
        entity_Concept_Id = as.integer(input$entityConceptId),
        entity_As_String = input$entityAsString,
        entity_Identifier = ifelse(input$entityIdentifier == "", NA, as.integer(input$entityIdentifier)),
        activity_Concept_Id = as.integer(input$activityConceptId),
        activity_Type_Concept_Id = as.integer(input$activityTypeConceptId),
        activity_As_String = input$activityAsString,
        activity_Start_Date = input$activityDates[1],
        activity_End_Date = input$activityDates[2],
        security_Concept_Id = as.integer(input$securityConceptIdEA)
      )
      
      DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema),
                                     data = df, dropTableIfExists = F, createTable = F, useMppBulkLoad = F)
      
      showNotification(sprintf("New entity/activity added"))
      
      .clearTextInputs(entityActivityTextInputs, "btnSubmitEA")
    }
  }
  
  .addValue <- function(metaEntityActivityId, metaAnnotationId = NA) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (!is.na(metaAnnotationId)) {
      suffix <- "Ann"
      df <- data.frame(
        meta_value_id = .getMaxId("meta_value", "meta_value_id") + 1,
        value_ordinal = as.integer(input$valueOrdinalAnn),
        meta_entity_activity_id = as.integer(metaEntityActivityId),
        meta_annotation_id = as.integer(metaAnnotationId),
        value_concept_id = as.integer(input$valueConceptIdAnn),
        value_type_concept_id = as.integer(input$valueTypeConceptIdAnn),
        value_as_string = input$valueAsStringAnn,
        value_as_number = as.numeric(input$valueAsNumberAnn),
        operator_concept_id = as.integer(input$operatorConceptIdAnn)
      )
    } else {
      suffix <- "EA"
      df <- data.frame(
        meta_value_id = .getMaxId("meta_value", "meta_value_id") + 1,
        value_ordinal = as.integer(input$valueOrdinalEA),
        meta_entity_activity_id = as.integer(metaEntityActivityId),
        meta_annotation_id = as.integer(metaAnnotationId),
        value_concept_id = as.integer(input$valueConceptIdEA),
        value_type_concept_id = as.integer(input$valueTypeConceptIdEA),
        value_as_string = input$valueAsStringEA,
        value_as_number = as.numeric(input$valueAsNumberEA),
        operator_concept_id = as.integer(input$operatorConceptIdEA)
      )
    }
      
    DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_value", resultsDatabaseSchema),
                                     data = df, dropTableIfExists = F, createTable = F, useMppBulkLoad = F)
      
    showNotification(sprintf("New value added"))
      
    .clearTextInputs(valueTextInputs(suffix), sprintf("btnClearValue%s", suffix))
  }
  
  .updateAgent <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
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
    
    showNotification(sprintf("Agent updated"))
  }
  
  .updateEntityActivity <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "management/updateEntityActivity.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema,
                                             entityConceptId = input$entityConceptId,
                                             entityAsString = input$entityAsString,
                                             entityIdentifier = ifelse(input$entityIdentifier == "", "NULL", as.integer(input$entityIdentifier)),
                                             #activityConceptId = input$activityConceptId,
                                             activityTypeConceptId = input$activityTypeConceptId,
                                             activityAsString = input$activityAsString,
                                             activityStartDate = input$activityDates[1],
                                             activityEndDate = input$activityDates[2],
                                             securityConceptId = input$securityConceptIdEA,
                                             metaEntityActivityId = .getEntityActivities()[row_count,]$META_ENTITY_ACTIVITY_ID)
    cat(sql)
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Entity/Activity record updated"))
  }
  
  .updateValue <- function(metaValueId, isAnnotation = FALSE) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (isAnnotation) {
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "management/updateValue.sql", 
                                               packageName = "CdmMetadata", 
                                               dbms = connectionDetails$dbms,
                                               resultsDatabaseSchema = resultsDatabaseSchema,
                                               metaValueId = metaValueId,
                                               valueOrdinal = input$valueOrdinalAnn,
                                               valueConceptId = input$valueConceptIdAnn,
                                               valueTypeConceptId = input$valueTypeConceptIdAnn,
                                               valueAsString = input$valueAsStringAnn,
                                               valueAsNumber = input$valueAsNumberAnn,
                                               operatorConceptId = input$operatorConceptIdAnn)
      
    } else {
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "management/updateValue.sql", 
                                               packageName = "CdmMetadata", 
                                               dbms = connectionDetails$dbms,
                                               resultsDatabaseSchema = resultsDatabaseSchema,
                                               metaValueId = metaValueId,
                                               valueOrdinal = input$valueOrdinalEA,
                                               valueConceptId = input$valueConceptIdEA,
                                               valueTypeConceptId = input$valueTypeConceptIdEA,
                                               valueAsString = input$valueAsStringEA,
                                               valueAsNumber = input$valueAsNumberEA,
                                               operatorConceptId = input$operatorConceptIdEA)
      
    }
    
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Value record updated"))
  }
  
  .deleteAgent <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtAgent_rows_selected
    metaAgentId <- .getAgents()[row_count, ]$META_AGENT_ID
    
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_agent where meta_agent_id = @metaAgentId;",
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                metaAgentId = metaAgentId)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Agent deleted"))
  }
  
  .deleteEntityActivity <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_entity_activity 
                                where meta_entity_activity_id = @metaEntityActivityId;",
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                metaEntityActivityId = metaEntityActivityId)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_value
                                where meta_entity_activity_id = @metaEntityActivityId;",
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                metaEntityActivityId = metaEntityActivityId)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Entity/Activity record deleted"))
  }
  
  .deleteValue <- function(metaValueId) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_value
                                where meta_value_id = @metaValueId;",
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                metaValueId = metaValueId)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Value record deleted"))
  }
  
  # Agent Events -----------------------------------------------
  
  observeEvent(input$agentConceptId, handlerExpr = {
    .clearTextInputs(agentTextInputs, "btnSubmitAgent")
  })
  
  observeEvent(input$btnDeleteAgent, handlerExpr = {
    .deleteAgent()
    .clearTextInputs(agentTextInputs, "btnSubmitAgent")
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnClearAgent, handlerExpr = {
    .clearTextInputs(agentTextInputs, "btnSubmitAgent")
    selectRows(proxy = dtAgentProxy, selected = NULL)
    selectRows(proxy = dtEAProxy, selected = NULL)
    
    showNotification("Agent de-selected")
  })
  
  observeEvent(eventExpr = input$btnSubmitAgent, handlerExpr = {
    agentSelected <- !is.null(input$dtAgent_rows_selected)
    if (agentSelected) {
      .updateAgent()
    } else {
      .addAgent()
    }
    .clearTextInputs(agentTextInputs, "btnSubmitAgent")
  }, priority = 1)
  
  observeEvent(eventExpr = input$dtAgent_rows_selected, handlerExpr = {
    updateActionButton(session = session, inputId = "btnSubmitAgent", label = "Update Selected")
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
  
  # Entity Activity Events ---------------------------------------
  
  observeEvent(eventExpr = input$btnClearEA, handlerExpr = {
    .clearTextInputs(entityActivityTextInputs, "btnSubmitEA")
    selectRows(proxy = dtEAProxy, selected = NULL)
    selectRows(proxy = dtAnnotationProxy, selected = NULL)
    selectRows(proxy = dtValueAnnProxy, selected = NULL)
    selectRows(proxy = dtValueEAProxy, selected = NULL)
    showNotification("Entity/Activity record de-selected")
  })
  
  observeEvent(input$btnDeleteEA, handlerExpr = {
    .deleteEntityActivity()
    .clearTextInputs(agentTextInputs, "btnSubmitEA")
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnSubmitEA, handlerExpr = {
    selected <- !is.null(input$dtEntityActivity_rows_selected)
    if (selected) {
      .updateEntityActivity()
    } else {
      .addEntityActivity()
    }
    .clearTextInputs(entityActivityTextInputs, "btnSubmitEA")
  }, priority = 1)
  
  
  observeEvent(eventExpr = input$dtEntityActivity_rows_selected, handlerExpr = {
    updateActionButton(session = session, inputId = "btnSubmitEA", label = "Update Selected")
    row_count <- input$dtEntityActivity_rows_selected
    
    for (t in entityActivityTextInputs) {
      updateTextInput(session = session,
                      inputId = t,
                      value = .getEntityActivities()[row_count,][toupper(SqlRender::camelCaseToSnakeCase(t))][[1]])
    }
  })
  

  # Annotation Events -----------------------------------

  observeEvent(eventExpr = input$btnClearAnnotation, handlerExpr = {
    .clearTextInputs(annotationTextInputs, "btnSubmitAnnotation")
    showNotification("Annotation record de-selected")
  })
  
  observeEvent(eventExpr = input$btnSubmitAnnotation, handlerExpr = {
    selected <- !is.null(input$dtAnnotation_rows_selected)
    if (selected) {
      .updateAnnotation()
    } else {
      .addAnnotation()
    }
    .clearTextInputs(annotationTextInputs, "btnSubmitAnnotation")
  }, priority = 1)
  
  observeEvent(input$btnDeleteAnnotation, handlerExpr = {
    .deleteAnnotation()
    .clearTextInputs(agentTextInputs, "btnSubmitEA")
  }, priority = 1)
  
  observeEvent(eventExpr = input$dtAnnotation_rows_selected, handlerExpr = {
    updateActionButton(session = session, inputId = "btnSubmitAnnotation", label = "Update Selected")
    row_count <- input$dtAnnotation_rows_selected
    
    for (t in annotationTextInputs) {
      updateTextInput(session = session,
                      inputId = t,
                      value = .getAnnotations()[row_count,][toupper(SqlRender::camelCaseToSnakeCase(t))][[1]])
    }
  })
  
  # EA Value Events --------------------------------------------------
  
  observeEvent(eventExpr = input$btnClearValueEA, handlerExpr = {
    .clearTextInputs(entityActivityTextInputs, "btnSubmitValueEA")
    showNotification("Value record de-selected")
  })
  
  observeEvent(eventExpr = input$btnSubmitValueEA, handlerExpr = {
    row_count <- input$dtEntityActivity_rows_selected
    metaEntityActivityId <- .getEntityActivities()[row_count,]$META_ENTITY_ACTIVITY_ID
    
    selected <- !is.null(input$dtValueEA_rows_selected)
    if (selected) {
      row_count <- input$dtValueEA_rows_selected
      metaValueId <- .getValues()[row_count,]$META_VALUE_ID
      .updateValue(metaValueId)
    } else {
      .addValue(metaEntityActivityId)
    }
    .clearTextInputs(valueTextInputs("EA"), "btnSubmitValueEA")
  }, priority = 1)
  
  observeEvent(input$btnDeleteValueEA, handlerExpr = {
    row_count <- input$dtValueEA_rows_selected
    metaValueId <- .getValues()[row_count,]$META_VALUE_ID
    
    .deleteValue(metaValueId)
    .clearTextInputs(agentTextInputs, "btnSubmitValueEA")
  }, priority = 1)
  
  observeEvent(eventExpr = input$dtValueEA_rows_selected, handlerExpr = {
    updateActionButton(session = session, inputId = "btnSubmitValueEA", label = "Update Selected")
    row_count <- input$dtValueEA_rows_selected
    
    for (t in valueTextInputs("EA")) {
      t <- gsub(pattern = "EA", replacement = "", x = t)
      updateTextInput(session = session,
                      inputId = t,
                      value = .getValues()[row_count,][toupper(SqlRender::camelCaseToSnakeCase(t))][[1]])
    }
  })
  
  # Annotation Value Events --------------------------------------------------
  
  observeEvent(eventExpr = input$btnClearValueAnn, handlerExpr = {
    .clearTextInputs(entityActivityTextInputs, "btnSubmitValueAnn")
    showNotification("Value record de-selected")
  })
  
  observeEvent(eventExpr = input$btnSubmitValueAnn, handlerExpr = {
    row_count <- input$dtEntityActivity_rows_selected
    metaEntityActivityId <- .getEntityActivities()[row_count,]$META_ENTITY_ACTIVITY_ID
    
    selected <- !is.null(input$dtValueAnnotation_rows_selected)
    if (selected) {
      row_count <- input$dtValueEA_rows_selected
      metaValueId <- .getValues()[row_count,]$META_VALUE_ID
      .updateValue(metaValueId, TRUE)
    } else {
      row_count <- input$dtAnnotation_rows_selected
      metaAnnotationId <- .getAnnotations()[row_count,]$META_ANNOTATION_ID
      
      .addValue(metaEntityActivityId, metaAnnotationId)
    }
    .clearTextInputs(valueTextInputs("Ann"), "btnSubmitValueAnn")
  }, priority = 1)
  
  observeEvent(input$btnDeleteValueAnn, handlerExpr = {
    row_count <- input$dtValueAnnotation_rows_selected
    metaValueId <- .getValues()[row_count,]$META_VALUE_ID
    
    .deleteValue(metaValueId)
    .clearTextInputs(agentTextInputs, "btnSubmitValueAnn")
  }, priority = 1)
  
  
  observeEvent(eventExpr = input$dtValueAnnotation_rows_selected, handlerExpr = {
    updateActionButton(session = session, inputId = "btnSubmitValueAnn", label = "Update Selected")
    row_count <- input$dtValueAnnotation_rows_selected
    
    for (t in valueTextInputs("Ann")) {
      t <- gsub(pattern = "Ann", replacement = "", x = t)
      updateTextInput(session = session,
                      inputId = t,
                      value = .getValues()[row_count,][toupper(SqlRender::camelCaseToSnakeCase(t))][[1]])
    }
  })
  
})