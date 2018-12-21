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
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql(sql = "select max(@fieldName) as MAX_ID from @resultsDatabaseSchema.@tableName;",
                                fieldName = fieldName,
                                tableName = tableName,
                                resultsDatabaseSchema = resultsDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
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
  
  .getHeelResults <- function() {
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "heelResults/getHeels.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema())
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  #.getConceptPrevalance <- function() {
  
  conceptPrevalence <- reactive({
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "conceptExplore/getPrevalence.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema(),
                                             conceptId = input$conceptId,
                                             analysisId = input$domainId)
    
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    if (nrow(df) > 0) {
      df$STRATUM_2 <- as.Date(paste0(df$STRATUM_2, '01'), format='%Y%m%d')  
    }
    
    df
  })
  
  chartMeta <- reactive({
  #.getChartMeta <- function() {
    if (input$conceptId != "") {
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
      on.exit(DatabaseConnector::disconnect(connection = connection))
      
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "conceptExplore/getChartEntityActivity.sql", 
                                               packageName = "CdmMetadata", 
                                               dbms = connectionDetails()$dbms,
                                               resultsDatabaseSchema = resultsDatabaseSchema(),
                                               entityConceptId = input$conceptId)
      
      DatabaseConnector::querySql(connection = connection, sql = sql)
    } else {
      data.frame()
    }
  })
  
  .getAgents <- function() {

    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    # if (is.null(metaAgentConceptId)) {
    #   metaAgentConceptId <- input$agentConceptId
    # }
    
    sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_agent
                                order by meta_agent_id;",
                                resultsDatabaseSchema = resultsDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .getEntityActivities <- function(subsetByAgent = TRUE) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (subsetByAgent) {
      row_count <- input$dtAgent_rows_selected
      metaAgentId <- .getAgents()[row_count, ]$META_AGENT_ID
    } else {
      metaAgentId <- NA
    }
    
    df <- tryCatch({
      sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_entity_activity
                                where 1=1 {@subsetByAgent}?{and meta_agent_id = @metaAgentId}
                                order by meta_entity_activity_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                  subsetByAgent = subsetByAgent,
                                  metaAgentId = metaAgentId)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      df <- NULL 
    })
    
    df
  }
  
  .getAnnotations <- function(metaEntityActivityId = NULL) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    if (is.null(metaEntityActivityId)) {
      metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    }
    
    df <- tryCatch({
      sql <- SqlRender::renderSql(sql = "select * from @resultsDatabaseSchema.meta_annotation 
                                  where meta_entity_activity_id = @metaEntityActivityId
                                  order by meta_annotation_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                  metaEntityActivityId = metaEntityActivityId)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      data.frame() 
    })
    
    df
  }
  
  .getValues <- function(metaEntityActivityId = NULL, metaAnnotationId = NULL) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (is.null(metaEntityActivityId)) {
      row_count <- input$dtEntityActivity_rows_selected
      metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    }
    
    df <- tryCatch({
      sql <- SqlRender::renderSql(sql = "select * from @resultsDatabaseSchema.meta_value 
                                  where meta_entity_activity_id = @metaEntityActivityId
                                  {@isAnnotation}?{and meta_annotation_id = @metaAnnotationId}
                                  order by meta_annotation_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                  metaEntityActivityId = metaEntityActivityId,
                                  metaAnnotationId = metaAnnotationId,
                                  isAnnotation = !is.null(metaAnnotationId))$sql
      
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      df <- NULL 
    })
    
    df
  }
  
  
  # reactives -----------------------------------------------------------------
  
  connectionDetails <- reactive({
    index <- which(sapply(cdmSources, function(c) c$name == input$cdmSource))
    cdmSource <- cdmSources[[index]]
    
    if (is.null(cdmSource$user)) {
      connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = cdmSource$dbms,
                                                                      server = cdmSource$server,
                                                                      port = cdmSource$port,
                                                                      extraSettings = cdmSource$extraSettings)
    } else {
      connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = cdmSource$dbms,
                                                                      server = cdmSource$server,
                                                                      port = cdmSource$port,
                                                                      user = cdmSource$user,
                                                                      password = cdmSource$password,
                                                                      extraSettings = cdmSource$extraSettings)
    }
    connectionDetails
  })
  
  resultsDatabaseSchema <- reactive({
    index <- which(sapply(cdmSources, function(c) c$name == input$cdmSource))
    cdmSource <- cdmSources[[index]]
    cdmSource$resultsDatabaseSchema
  })
  
  cdmDatabaseSchema <- reactive({
    index <- which(sapply(cdmSources, function(c) c$name == input$cdmSource))
    cdmSource <- cdmSources[[index]]
    cdmSource$cdmDatabaseSchema
  })
  
  vocabDatabaseSchema <- reactive({
    index <- which(sapply(cdmSources, function(c) c$name == input$cdmSource))
    cdmSource <- cdmSources[[index]]
    cdmSource$vocabDatabaseSchema
  })
  
  achillesConcepts <- reactive({
    sql <- SqlRender::renderSql("select distinct A.analysis_id, A.stratum_1, B.concept_name, B.concept_id from @resultsDatabaseSchema.achilles_results A
                                join @vocabDatabaseSchema.concept B on cast(stratum_1 as integer) = B.concept_id
                                  and B.concept_id <> 0
                                where A.analysis_id in (402,602,2101,702,1801,802);",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                vocabDatabaseSchema = vocabDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    DatabaseConnector::querySql(connection = connection, sql = sql)
  })
  
  # input rendering ------------------------------------------------------------
  
  # output rendering -----------------------------------------------------------
  
  output$conceptName <- renderText({
    
    df <- achillesConcepts()
    paste(input$conceptId,
          df$CONCEPT_NAME[df$CONCEPT_ID == input$conceptId], sep = " - ")
  })
  
  output$dtHeelResults <- renderDataTable(expr = {
    
    input$btnSubmitHeel
    input$btnDeleteHeel
    
    df <- .getHeelResults()
    
    df <- dplyr::arrange(df, ANALYSIS_ID) %>%
      dplyr::select(`Analysis Id` = ANALYSIS_ID,
                    `Rule Id` = RULE_ID,
                    `Message` = ACHILLES_HEEL_WARNING,
                    `Record Count` = RECORD_COUNT,
                    `Heel Status` = ANNOTATION_AS_STRING,
                    `Heel Annotation` = VALUE_AS_STRING,
                    `Agent` = AGENT)
    
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
                       class = "stripe nowrap compact", extensions = c("Responsive")) %>%
      formatStyle("Heel Status", #"Warning Type",
                  target = "row",
                  backgroundColor = styleEqual(c("Non-issue", "Needs Review", "Issue"),
                                               c("#e8f0ff", "#fffedb", "#ffdbdb")))
    
    table
  })

  observeEvent(input$toggleConcepts, {
    
    df <- achillesConcepts()
    selected <- df[df$DOMAIN_ID == input$domainId,]
    
    if (input$toggleConcepts) {
      meta <- .getEntityActivities(FALSE)
      
      tempEvents <- meta$ENTITY_CONCEPT_ID[meta$ACTIVITY_AS_STRING == "Temporal Event"]
      selected <- selected[selected$CONCEPT_ID %in% tempEvents,]
      
      choices <- setNames(as.integer(selected$CONCEPT_ID), paste(selected$CONCEPT_ID, as.character(selected$CONCEPT_NAME), sep = " - "))
      
    } else {
      choices <- setNames(as.integer(selected$CONCEPT_ID), paste(selected$CONCEPT_ID, 
                                                                 as.character(selected$CONCEPT_NAME), sep = " - "))
    }
    
    updateSelectInput(session = session, inputId = "conceptId", choices = choices)
  }, priority = 1)
  
  observeEvent(input$domainId, {

    selected <- achillesConcepts()[achillesConcepts()$ANALYSIS_ID == input$domainId,]

    choices <- setNames(as.integer(selected$CONCEPT_ID), paste(selected$CONCEPT_ID, as.character(selected$CONCEPT_NAME), sep = " - "))
    updateSelectInput(session = session, inputId = "conceptId", choices = choices)
  })
  
  output$selectedConcept <- renderText({
    input$conceptId
  })
  
  output$conceptPlot <- renderPlotly({
    input$btnAddTemporalAnnotation
    .refreshConceptPlot()
  })
  
  output$dtAgent <- renderDataTable({
    input$selectAgent
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
  
  .refreshConceptPlot <- function() {
    df <- conceptPrevalence() # .getConceptPrevalance()
    
    meta <- chartMeta() #.getChartMeta()
    
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
      
      plot_ly(df, x = ~STRATUM_2, y = ~COUNT_VALUE, type = "scatter", mode = "lines+markers", hoverinfo = 'text', source = "C") %>%
        layout(xaxis = list(title = "Date"), yaxis = list(title = "# of Events")) %>%
        layout(annotations = a)  
    } else {
      plot_ly(df, x = ~STRATUM_2, y = ~COUNT_VALUE, type = "scatter", mode = "lines+markers", source = "C") %>%
        layout(xaxis = list(title = "Date"), yaxis = list(title = "# of Events"))
    }
  }
  
  .addConceptAnnotation <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    
    metaEntityActivityId <- .getMaxId(tableName = "meta_entity_activity",
                                      fieldName = "meta_entity_activity_id") + 1
    
    entityActivity <- data.frame(
      meta_entity_activity_id = metaEntityActivityId,
      meta_agent_id = as.integer(input$selectAgent),
      entity_concept_id = as.integer(input$conceptId),
      entity_as_string = "",
      entity_identifier = NA,
      activity_concept_id = 0,
      activity_type_concept_id = 0,
      activity_as_string = "Temporal Event",
      activity_start_date = input$conceptStartDate,
      activity_end_date = NA,
      security_concept_id = 0
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema()), 
                                   data = entityActivity, 
                                   dropTableIfExists = F, createTable = F)
    
    metaAnnotationId <- .getMaxId(tableName = "meta_annotation",
                                  fieldName = "meta_annotation_id") + 1
    
    metaValueId <- .getMaxId(tableName = "meta_value",
                             fieldName = "meta_value_id") + 1
    
    value <- data.frame(
      meta_value_id = metaValueId,
      value_ordinal = 1, 
      meta_entity_activity_id = metaEntityActivityId,
      meta_annotation_id = NA,
      value_concept_id = 0,
      value_type_concept_id = 0,
      value_as_string = input$conceptAnnotation,
      value_as_number = NA,
      operator_concept_id = 0
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_value", resultsDatabaseSchema()), 
                                   data = value, 
                                   dropTableIfExists = F, createTable = F)
    
    showNotification(sprintf("New Concept Annotation added"))
  }
  
  .addHeelAnnotation <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtHeelResults_rows_selected
    annotationAsString <- .getHeelResults()[row_count,]$ANNOTATION_AS_STRING


    metaEntityActivityId <- .getMaxId(tableName = "meta_entity_activity",
                                      fieldName = "meta_entity_activity_id") + 1
    
    entityActivity <- data.frame(
      meta_entity_activity_id = metaEntityActivityId,
      meta_agent_id = as.integer(input$selectAgent),
      entity_concept_id = 0,
      entity_as_string = "",
      entity_identifier = NA,
      activity_concept_id = 0,
      activity_type_concept_id = 0,
      activity_as_string = .getHeelResults()[row_count,]$ACHILLES_HEEL_WARNING,
      activity_start_date = NA,
      activity_end_date = NA,
      security_concept_id = 0
    )
    
    # matches <- .getEntityActivities(FALSE) %>%
    #   dplyr::filter(ACTIVITY_AS_STRING == .getHeelResults()[row_count,]$ACHILLES_HEEL_WARNING)
    # 
    # if (nrow(matches) == 0) {
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema()), 
                                   data = entityActivity, 
                                   dropTableIfExists = F, createTable = F)
    
    metaAnnotationId <- .getMaxId(tableName = "meta_annotation",
                                    fieldName = "meta_annotation_id") + 1
    
    annotation <- data.frame(
      meta_annotation_id = metaAnnotationId,
      meta_agent_id = as.integer(input$selectAgent),
      meta_entity_activity_id = metaEntityActivityId,
      annotation_concept_id = 0,
      annotation_as_string = input$heelStatus,
      annotation_type_concept_id = 0,
      security_concept_id = 0
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_annotation", resultsDatabaseSchema()), 
                                   data = annotation, 
                                   dropTableIfExists = F, createTable = F)
    
    metaValueId <- .getMaxId(tableName = "meta_value",
                             fieldName = "meta_value_id") + 1

    value <- data.frame(
      meta_value_id = metaValueId,
      value_ordinal = 1, 
      meta_entity_activity_id = metaEntityActivityId,
      meta_annotation_id = metaAnnotationId,
      value_concept_id = 0,
      value_type_concept_id = 0,
      value_as_string = input$heelAnnotation,
      value_as_number = NA,
      operator_concept_id = 0
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_value", resultsDatabaseSchema()), 
                                   data = value, 
                                   dropTableIfExists = F, createTable = F)
    
    showNotification(sprintf("New Heel Annotation added"))
      #}
    #}
  }
  
  .updateHeelAnnotation <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtHeelResults_rows_selected
    activityAsString <- .getHeelResults()[row_count,]$ACHILLES_HEEL_WARNING
    
    metaEntityActivityId <- (.getEntityActivities(subsetByAgent = FALSE) %>%
      dplyr::filter(ACTIVITY_AS_STRING == activityAsString))$META_ENTITY_ACTIVITY_ID
    
    metaAnnotationId <- .getAnnotations(metaEntityActivityId = metaEntityActivityId)$META_ANNOTATION_ID
    
    metaValueId <- .getValues(metaEntityActivityId = metaEntityActivityId, 
                              metaAnnotationId = metaAnnotationId)$META_VALUE_ID
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "heelResults/updateHeel.sql",
                                             dbms = connectionDetails()$dbms, 
                                             packageName = "CdmMetadata",
                                             resultsDatabaseSchema = resultsDatabaseSchema(),
                                             metaAgentId = input$selectAgent,
                                             annotationAsString = input$heelStatus,
                                             metaAnnotationId = metaAnnotationId,
                                             metaValueId = metaValueId,
                                             valueAsString = input$heelAnnotation)
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification("Heel Annotation Updated")
  }
  
  .deleteHeelAnnotation <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtHeelResults_rows_selected
    activityAsString <- .getHeelResults()[row_count,]$ACHILLES_HEEL_WARNING
    
    metaEntityActivityId <- (.getEntityActivities(subsetByAgent = FALSE) %>%
      dplyr::filter(ACTIVITY_AS_STRING == activityAsString))$META_ENTITY_ACTIVITY_ID
    
    metaAnnotationId <- (.getAnnotations(metaEntityActivityId = metaEntityActivityId))$META_ANNOTATION_ID
    
    metaValueId <- (.getValues(metaEntityActivityId = metaEntityActivityId,
                               metaAnnotationId = metaAnnotationId))$META_VALUE_ID
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "heelResults/deleteHeel.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema(),
                                             metaEntityActivityId = metaEntityActivityId,
                                             metaAnnotationId = metaAnnotationId,
                                             metaValueId = metaValueId)
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification("Heel Annotation Removed")
  }
  
  .addAgent <- function() {

    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
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
                                     tableName = sprintf("%s.meta_agent", resultsDatabaseSchema()), 
                                     data = df, 
                                     dropTableIfExists = FALSE, createTable = FALSE, useMppBulkLoad = FALSE) 
      
      showNotification(sprintf("New agent added"))
      
      .clearTextInputs(textInputNames = agentTextInputs, "btnSubmitAgent")
    }
  }
  
  .addEntityActivity <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
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
                                     tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema()),
                                     data = df, dropTableIfExists = F, createTable = F, useMppBulkLoad = F)
      
      showNotification(sprintf("New entity/activity added"))
      
      .clearTextInputs(entityActivityTextInputs, "btnSubmitEA")
    }
  }
  
  .addValue <- function(metaEntityActivityId, metaAnnotationId = NA) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
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
                                     tableName = sprintf("%s.meta_value", resultsDatabaseSchema()),
                                     data = df, dropTableIfExists = F, createTable = F, useMppBulkLoad = F)
      
    showNotification(sprintf("New value added"))
      
    .clearTextInputs(valueTextInputs(suffix), sprintf("btnClearValue%s", suffix))
  }
  
  .updateAgent <- function(agentFirstName, 
                           agentLastName,
                           agentSuffix,
                           agentAlgorithm,
                           agentDescription,
                           metaAgentId) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "management/updateAgent.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema(),
                                             agentFirstName = agentFirstName,
                                             agentLastName = agentLastName,
                                             agentSuffix = agentSuffix,
                                             agentAlgorithm = agentAlgorithm,
                                             agentDescription = agentDescription,
                                             metaAgentId = metaAgentId)
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    df <- .getAgents()
    humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
    algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
    choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                               humans$AGENT_FIRST_NAME, sep = ", ")),
                    `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
    
    updateSelectInput(session = session, inputId = "selectAgent", choices = choices)
    
    showNotification(sprintf("Agent updated"))
    removeModal(session = session)
  }
  
  .updateEntityActivity <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "management/updateEntityActivity.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema(),
                                             entityConceptId = input$entityConceptId,
                                             entityAsString = input$entityAsString,
                                             entityIdentifier = ifelse(input$entityIdentifier == "", "NULL", as.integer(input$entityIdentifier)),
                                             activityConceptId = input$activityConceptId,
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
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (isAnnotation) {
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "management/updateValue.sql", 
                                               packageName = "CdmMetadata", 
                                               dbms = connectionDetails()$dbms,
                                               resultsDatabaseSchema = resultsDatabaseSchema(),
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
                                               dbms = connectionDetails()$dbms,
                                               resultsDatabaseSchema = resultsDatabaseSchema(),
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
  
  .deleteAgent <- function(metaAgentId) {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_agent where meta_agent_id = @metaAgentId;",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                metaAgentId = metaAgentId)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Agent deleted"))
  }
  
  .deleteEntityActivity <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_entity_activity 
                                where meta_entity_activity_id = @metaEntityActivityId;",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                metaEntityActivityId = metaEntityActivityId)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_value
                                where meta_entity_activity_id = @metaEntityActivityId;",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                metaEntityActivityId = metaEntityActivityId)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Entity/Activity record deleted"))
  }
  
  .deleteValue <- function(metaValueId) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_value
                                where meta_value_id = @metaValueId;",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                metaValueId = metaValueId)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Value record deleted"))
  }
  
  # Add New Agent Events ---------------------------------------
  
  observeEvent(input$btnAddNewAgent, handlerExpr = {
    showModal(modalDialog(
      title = "Add New Agent",
      selectInput(inputId = "agentConceptId", label = "Agent Type", 
                  choices = c("Human" = 1000, "Algorithm" = 2000), width = "250px"),
      conditionalPanel(condition = "input.agentConceptId == 1000",
                       textInput(inputId = "agentFirstName", label = "First Name", width = "250px"),
                       textInput(inputId = "agentLastName", label = "Last Name", width = "250px"),
                       textInput(inputId = "agentSuffix", label = "Suffix", width = "250px")
      ),
      conditionalPanel(condition = "input.agentConceptId == 2000",
                       textInput(inputId = "agentAlgorithm", label = "Algorithm Name", width = "250px"),
                       textAreaInput(inputId = "agentDescription", label = "Description", 
                                     rows = 4, resize = "none", width = "250px")
      ),
      actionButton(inputId = "btnSubmitAgent", label = "Add New", icon = icon("check")),
      width = 3)
    )
  }, priority = 1)
  
  observeEvent(input$btnDeleteAgent, handlerExpr = {
    
    showModal(
      modalDialog(
        title = "Delete Agent",
        "Are you sure you want to delete this agent?",
        actionButton(inputId = "btnConfirmDeleteAgent", label = "Yes, delete this agent")
      )
    )
  }, priority = 1)
  
  observeEvent(input$btnConfirmDeleteAgent, handlerExpr = {
    .deleteAgent(input$selectAgent)
    df <- .getAgents()
    humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
    algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
    choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                               humans$AGENT_FIRST_NAME, sep = ", ")),
                    `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
    updateSelectInput(session = session, inputId = "selectAgent", choices = choices)
    removeModal(session = session)
  }, priority = 1)
  
  observeEvent(input$btnEditAgent, handlerExpr = {
    
    df <- .getAgents()
    thisAgent <- df[df$META_AGENT_ID == input$selectAgent, ]
    
    if (thisAgent$META_AGENT_CONCEPT_ID == 1000) {
      showModal(modalDialog(
        title = "Edit Agent",
        textInput(inputId = "agentFirstName", label = "First Name", width = "250px"),
        textInput(inputId = "agentLastName", label = "Last Name", width = "250px"),
        textInput(inputId = "agentSuffix", label = "Suffix", width = "250px"),
        actionButton(inputId = "btnUpdateAgent", label = "Submit Changes", icon = icon("check"))
        ))
      updateTextInput(session = session, inputId = "agentFirstName", value = thisAgent$AGENT_FIRST_NAME)
      updateTextInput(session = session, inputId = "agentLastName", value = thisAgent$AGENT_LAST_NAME)
      updateTextInput(session = session, inputId = "agentSuffix", value = thisAgent$AGENT_SUFFIX)
    } else {
      showModal(modalDialog(
        title = "Edit Agent",
        textInput(inputId = "agentAlgorithm", label = "Algorithm Name", width = "250px"),
        textAreaInput(inputId = "agentDescription", label = "Description", 
                      rows = 4, resize = "none", width = "250px"),
        actionButton(inputId = "btnUpdateAgent", label = "Submit Changes", icon = icon("check"))
      ))
      updateTextInput(session = session, inputId = "agentAlgorithm", value = thisAgent$AGENT_ALGORITHM)
      updateTextInput(session = session, inputId = "agentDescription", value = thisAgent$AGENT_DESCRIPTION)
    }
  }, priority = 1)
  
  observeEvent(input$btnUpdateAgent, handlerExpr = {
    df <- .getAgents()
    metaAgentConceptId <- df$META_AGENT_CONCEPT_ID[df$META_AGENT_ID == input$selectAgent]
    
    agentFirstName <- ""
    agentLastName <- ""
    agentSuffix <- ""
    agentAlgorithm <- ""
    agentDescription <- ""
    metaAgentId <- input$selectAgent
    
    if (metaAgentConceptId == 1000) {
      agentFirstName <- input$agentFirstName
      agentLastName <- input$agentLastName
      agentSuffix <- input$agentSuffix
    } else {
      agentAlgorithm <- input$agentAlgorithm
      agentDescription <- input$agentDescription
    }
    
    .updateAgent(agentFirstName = agentFirstName,
                 agentLastName = agentLastName,
                 agentSuffix = agentSuffix,
                 agentAlgorithm = agentAlgorithm,
                 agentDescription = agentDescription,
                 metaAgentId = metaAgentId
                 )
  }, priority = 1)
  
  # Annotate Temporal Events -----------------------------------
  
  observe({
    clicked <- event_data(event = "plotly_click", source = "C", session = session)
    updateDateInput(session = session, inputId = "conceptStartDate", value = clicked$x)
  })
  
  observeEvent(input$btnAddTemporalAnnotation, handlerExpr = {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    metaEntityActivityId <- .getMaxId("meta_entity_activity", "meta_entity_activity_id") + 1
    
    df <- data.frame(
      meta_entity_activity_Id = metaEntityActivityId,
      meta_agent_id = as.integer(input$selectAgent),
      entity_concept_Id = as.integer(input$conceptId),
      entity_as_string = NA,
      entity_identifier = NA,
      activity_concept_id = 0,
      activity_type_concept_id = 0,
      activity_as_string = "Temporal Event",
      activity_start_date = input$conceptStartDate,
      activity_end_date = input$conceptStartDate,
      security_concept_id = 0
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema()),
                                   data = df, dropTableIfExists = F, createTable = F, useMppBulkLoad = F)
    
    metaValueId <- .getMaxId(tableName = "meta_value",
                             fieldName = "meta_value_id") + 1
    
    value <- data.frame(
      meta_value_id = metaValueId,
      value_ordinal = 1, 
      meta_entity_activity_id = metaEntityActivityId,
      meta_annotation_id = NA,
      value_concept_id = 0,
      value_type_concept_id = 0,
      value_as_string = input$temporalEventValue,
      value_as_number = NA,
      operator_concept_id = 0
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_value", resultsDatabaseSchema()), 
                                   data = value, 
                                   dropTableIfExists = F, createTable = F)
    
    
    #.refreshConceptPlot()
    
    showNotification(sprintf("New Temporal Event added"))
    
  }, priority = 1)
  
  # Agent Events -----------------------------------------------
  
  
  observeEvent(eventExpr = input$btnSubmitAgent, handlerExpr = {
    .addAgent()
    df <- .getAgents()
    humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
    algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
    choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                               humans$AGENT_FIRST_NAME, sep = ", ")),
                    `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
    updateSelectInput(session = session, inputId = "selectAgent", choices = choices)
    removeModal(session = session)
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
  
  observe({
    df <- .getAgents()
    
    humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
    algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
    
    choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                               humans$AGENT_FIRST_NAME, sep = ", ")),
                    `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
    updateSelectInput(session = session, inputId = "selectAgent", choices = choices)
  })
  
  # Heel Events ------------------------------------------------------------
  
  observeEvent(eventExpr = input$dtHeelResults_rows_selected, handlerExpr = {
    row_count <- input$dtHeelResults_rows_selected
    annotationAsString <- .getHeelResults()[row_count,]$ANNOTATION_AS_STRING
    
    valueAsString <- .getHeelResults()[row_count,]$VALUE_AS_STRING
    if (!is.na(annotationAsString)) {
      updateSelectInput(session = session, inputId = "heelStatus", selected = annotationAsString)
    }
    if (!is.na(valueAsString)) {
      updateTextInput(session = session, inputId = "heelAnnotation", value = valueAsString)
    }
  })
  
  observeEvent(eventExpr = input$btnDeleteHeel, handlerExpr = {
    .deleteHeelAnnotation()
    updateTextInput(session = session, inputId = "heelAnnotation", value = "")
    updateSelectInput(session = session, inputId = "heelStatus", selected = "")
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnSubmitHeel, handlerExpr = {
    if (input$heelStatus == "Needs Review" | input$heelAnnotation == "") {
      showNotification(ui = "Please change Heel Status and add Annotation", type = "error")
    } else {
      row_count <- input$dtHeelResults_rows_selected
      newAnnotation <- .getHeelResults()[row_count,]$ANNOTATION_AS_STRING == "Needs Review"
      
      if (!newAnnotation) {
        .updateHeelAnnotation()
      } else {
        .addHeelAnnotation()
      }
      updateTextInput(session = session, inputId = "heelAnnotation", value = "")
      updateSelectInput(session = session, inputId = "heelStatus", selected = "")
    }
  }, priority = 1)
  
})


.getSourceName <- function(connectionDetails,
                           cdmDatabaseSchema) {
  sql <- SqlRender::renderSql(sql = "select cdm_source_name from @cdmDatabaseSchema.cdm_source",
                              cdmDatabaseSchema = cdmDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  sourceName <- tryCatch({
    s <- DatabaseConnector::querySql(connection = connection, sql = sql)
    s[1,]
  }, error = function (e) {
    ""
  }, finally = {
    DatabaseConnector::disconnect(connection = connection)
    rm(connection)
  })
  sourceName
}