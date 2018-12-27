library(shiny)
library(DT)
library(magrittr)
library(tidyr)
library(httr)
library(shinyjs)

shinyServer(function(input, output, session) {
  
  # CRUD buttons -----------------------------------
  
  .createCrudButtons <- function(parentDiv, crudTypes = c("Submit", "Edit", "Delete")) {
    
    suffix <- gsub(pattern = "Crud", replacement = "", x = parentDiv)
    
    divs <- lapply(crudTypes, function(crudType) {
      if (crudType == "Submit") {
        iconName <- "plus"
      } else if (crudType == "Edit") {
        iconName <- "edit"
      } else {
        iconName <- "minus"
      }
      div(style = "display:inline-block;text-align: left;padding-bottom: 20px",
          actionButton(inputId = sprintf("btnModal%1s%2s", crudType, suffix),
                       label = crudType, icon = icon(iconName)))
    })
    
    insertUI(selector = sprintf("#%s", parentDiv), session = session,
             ui = {
               divs
             })
  }
  
  .createCrudButtons(parentDiv = "SourceDescCrud", crudTypes = c("Edit"))
  #.createCrudButtons(parentDiv = "TempEventCrud", crudTypes = c("Edit", "Delete"))
  #.createCrudButtons(parentDiv = "HeelResultsCrud", crudTypes = c("Edit", "Delete"))
  
  # Reactives ---------------------------------------------------------------------
  
  conceptsMeta <- reactive({
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "conceptExplore/getConceptsAndMetadata.sql",
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema(),
                                             conceptId = input$conceptId,
                                             analysisId = input$domainId)
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    if (nrow(df) > 0) {
      df$DATE <- as.Date(paste0(df$STRATUM_2, "01"), format = "%Y%m%d")
    }
    df
  })
  
  associatedTempEvents <- reactive({
    if (nrow(conceptsMeta()) > 0) {
      conceptsMeta()[!is.na(conceptsMeta()$VALUE_AS_STRING),]  
    } else {
      data.frame()
    }
  })
  
  currentSource <- reactive({
    index <- which(sapply(cdmSources, function(c) c$name == input$cdmSource))
    cdmSources[[index]]
  })
  
  connectionDetails <- reactive({
    if (input$cdmSource != "All Sources") {
      .getConnectionDetails(currentSource())
    } else {
      NULL
    }
  })
  
  resultsDatabaseSchema <- reactive({
    if (input$cdmSource != "All Sources") {
      currentSource()$resultsDatabaseSchema
    } else {
      NULL
    }
  })
  
  cdmDatabaseSchema <- reactive({
    if (input$cdmSource != "All Sources") {
      currentSource()$cdmDatabaseSchema
    } else {
      NULL
    }
  })
  
  vocabDatabaseSchema <- reactive({
    if (input$cdmSource != "All Sources") {
      currentSource()$vocabDatabaseSchema
    } else {
      NULL
    }
  })
  
  achillesConcepts <- reactive({
    if (input$cdmSource != "All Sources") {
      rdsFile <- file.path("data", "achilles_concepts", sprintf("%s.rds", currentSource()$name))
      readRDS(file = rdsFile)
    } else {
      NULL
    }
  })
  
  cohortDefinitions <- reactive({
    if (input$cdmSource != "All Sources") {
      url <- sprintf("%1s/cohortdefinition", baseUrl)
      cohorts <- httr::content(httr::GET(url))
      cohorts <- lapply(cohorts, function(c) {
        data.frame(ID = c$id, Name = c$name)
      })
      do.call("rbind", cohorts)  
    } else {
      NULL
    }
  })
  
  conceptSets <- reactive({
    if (input$cdmSource != "All Sources") {
      url <- sprintf("%1s/conceptset", baseUrl)
      sets <- httr::content(httr::GET(url))
      sets <- lapply(sets, function(c) {
        data.frame(ID = c$id, Name = c$name)
      })
      do.call("rbind", sets)  
    } else {
      NULL
    }
  })
  
  
  # Source Queries ------------------------------------------------------
  
  .getConceptPrevalence <- function() {
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
  }
  
  .getChartMeta <- function() {
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
  }
  
  .getObservationPeriodDates <- function(connectionDetails,
                                         resultsDatabaseSchema) {
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "source/getObservationPeriods.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema)
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .getSourceDescription <- function(connectionDetails,
                                    resultsDatabaseSchema) {
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "source/getDescription.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema)
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .deleteTemporalEvent <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql(sql = "select A.meta_entity_activity_id, B.meta_value_id from @resultsDatabaseSchema.meta_entity_activity A
                                      join @resultsDatabaseSchema.meta_value B on A.meta_entity_activity_id = B.meta_entity_activity_id
                                      where A.entity_concept_id = @entityConceptId and A.activity_start_date = '@activityStartDate' 
                                      and B.value_as_string = '@valueAsString';",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                entityConceptId = input$conceptId,
                                activityStartDate = input$conceptStartDate,
                                valueAsString = input$temporalEventValue)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "conceptExplore/deleteTemporalEvent.sql",
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema(),
                                             metaValueId = df$META_VALUE_ID,
                                             metaEntityActivityId = df$META_ENTITY_ACTIVITY_ID)  
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification("Temporal Event Deleted")
    removeModal(session = session)
  }
  
  .addTemporalEvent <- function() {
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
      activity_start_date = as.Date(input$conceptStartDate),
      activity_end_date = as.Date(input$conceptStartDate),
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
    
    # output$conceptKbPlot <- renderPlotly({
    #   .refreshConceptPlot()
    # })
    
    showNotification(sprintf("New Temporal Event added"))
  }
  
  .editTemporalEvent <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql(sql = "select B.meta_value_id from @resultsDatabaseSchema.meta_entity_activity A
                                      join @resultsDatabaseSchema.meta_value B on A.meta_entity_activity_id = B.meta_entity_activity_id
                                      where A.entity_concept_id = @entityConceptId and A.activity_start_date = '@activityStartDate' 
                                      and B.value_as_string = '@valueAsString';",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                entityConceptId = input$conceptId,
                                activityStartDate = input$conceptStartDate,
                                valueAsString = input$temporalEventValue)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    metaValueId <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "conceptExplore/updateTemporalEvent.sql",
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema(),
                                             metaValueId = metaValueId,
                                             valueAsString = input$tempEventEdit)  
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification("Temporal Event Changes Submitted")
  }
  
  .submitSourceDescription <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql("select A.meta_entity_activity_id, B.meta_value_id
                                  from @resultsDatabaseSchema.meta_entity_activity A
                                  join @resultsDatabaseSchema.meta_value B on A.meta_entity_activity_id = B.meta_entity_activity_id
                                  where A.entity_as_string = 'Source' and A.activity_as_string = 'Source Provenance';",
                                resultsDatabaseSchema = resultsDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    if (nrow(df) == 0) {
      metaEntityActivityId <- .getMaxId(tableName = "meta_entity_activity",
                                        fieldName = "meta_entity_activity_id") + 1
      
      entityActivity <- data.frame(
        meta_entity_activity_id = metaEntityActivityId,
        meta_agent_id = as.integer(input$selectAgent),
        entity_concept_id = 0,
        entity_as_string = "Source",
        entity_identifier = NA,
        activity_concept_id = 0,
        activity_type_concept_id = 0,
        activity_as_string = "Source Provenance",
        activity_start_date = NA,
        activity_end_date = NA,
        security_concept_id = 0
      )
      
      DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_entity_activity", 
                                                         resultsDatabaseSchema()), 
                                     data = entityActivity, 
                                     dropTableIfExists = F, createTable = F)
      
      metaValueId <- .getMaxId(tableName = "meta_value",
                               fieldName = "meta_value_id") + 1
      
      value <- data.frame(
        meta_value_id = metaValueId,
        value_ordinal = 1, 
        meta_entity_activity_id = metaEntityActivityId,
        meta_annotation_id = NA,
        value_concept_id = 0,
        value_type_concept_id = 0,
        value_as_string = input$sourceDescEdit,
        value_as_number = NA,
        operator_concept_id = 0
      )
      
      DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_value", resultsDatabaseSchema()), 
                                     data = value, 
                                     dropTableIfExists = F, createTable = F)
      
    } else {
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "source/updateDescription.sql",
                                               packageName = "CdmMetadata", 
                                               dbms = connectionDetails()$dbms,
                                               resultsDatabaseSchema = resultsDatabaseSchema(),
                                               metaValueId = df$META_VALUE_ID,
                                               metaEntityActivityId = df$META_ENTITY_ACTIVITY_ID,
                                               metaAgentId = input$selectAgent,
                                               valueAsString = input$sourceDescEdit)  
      
      DatabaseConnector::executeSql(connection = connection, sql = sql)
    }
    
    .createSourceOverview(cdmSource = currentSource(), parentDiv = "overviewBox", width = 12)
    showNotification("Source Description Submitted")
    
    removeModal(session = session)
  }
  
  .getSourcePopulation <- function() {
    df <- readRDS("data/totalPop.rds")
    prettyNum(df$COUNT_VALUE[df$CDM_SOURCE == currentSource()$name], big.mark = ",")
  }
  
  .createSourceOverview <- function(cdmSource, parentDiv, width = 12) {
    connectionDetails <- .getConnectionDetails(cdmSource)
    
    df <- .getObservationPeriodDates(connectionDetails,
                                     cdmSource$resultsDatabaseSchema)
    
    df$DATE <- as.Date(paste0(df$STRATUM_1, '01'), format='%Y%m%d') 
    
    plot <- plot_ly(df, x = ~DATE, y = ~COUNT_VALUE, type = "scatter", mode = "lines+markers", 
                    source = "C") %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "# of Persons"))
    
    removeUI(selector = sprintf("#%s div:has(> .box)", parentDiv), session = session)
    
    insertUI(selector = sprintf("#%s", parentDiv), 
             ui = {
               shinydashboard::box(title = cdmSource$name, collapsible = TRUE, width = width,
                                   div(.getSourceDescription(connectionDetails,
                                                             cdmSource$resultsDatabaseSchema)),
                                   div(h4("Start Date"), min(df$DATE)),
                                   div(h4("End Date"), max(df$DATE)),
                                   div(h4("Population Count", .getSourcePopulation())),
                                   plot
               )
             }, session = session)
  }
  
  # Observes ----------------------------------------------------------
  
  observe({
    if (input$tabs == "conceptKb" & currentSource()$name != "All Sources") {
      selected <- achillesConcepts()[achillesConcepts()$ANALYSIS_ID == input$domainId,]
      
      choices <- setNames(as.integer(selected$CONCEPT_ID), 
                          paste(selected$CONCEPT_ID, as.character(selected$CONCEPT_NAME), sep = " - "))
      updateSelectInput(session = session, inputId = "conceptId", choices = choices) 
    }
  })
  
  observe({
    input$btnSubmitHeel
    input$btnDeleteHeel
    if (currentSource()$name != "All Sources") {
      show(id = "tasksDropdown")
      
      sourceDesc <- .getSourceDescription(connectionDetails = connectionDetails(), 
                                          resultsDatabaseSchema = resultsDatabaseSchema())
      
      sourceDescItem <- taskItem(text = "Source Description Available", 
                                         value = 100 * as.integer(sourceDesc$VALUE_AS_STRING != ""), 
                                 color = "orange")  
      df <- .getHeelResults()
      ratio <- nrow(df[!is.na(df$VALUE_AS_STRING),])/nrow(df)
        
      heelItem <- taskItem(value = round(ratio * 100.00, digits = 2), color = "blue", 
                           text = "Heel Results Reviewed")
      
      # items <- c()
      # if (sourceDesc$VALUE_AS_STRING == "") {
      #   items <- list(items, sourceDesc)
      # }
      # if (ratio < 1) {
      #   items <- list(items, heelItem)
      # }
      output$tasksDropdown <- renderMenu({
        dropdownMenu(
          type = "tasks", badgeStatus = "danger",
          .list = list(sourceDescItem, heelItem)
        )
      }) 
    } else {
      hide(id = "tasksDropdown")
    }
  })
  
  observe({
    clicked <- event_data(event = "plotly_click", source = "C", session = session)
    if (!is.null(clicked)) {
    #  updateDateInput(session = session, inputId = "conceptStartDate", value = clicked$x)  
      updateTextInput(session = session, inputId = "conceptStartDate", value = clicked$x)
    }
    
  })
  
  observe({
    if (input$cdmSource != "All Sources") {
      df <- .getAgents()  
      humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
      algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
      
      choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                                 humans$AGENT_FIRST_NAME, sep = ", ")),
                      `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
      updateSelectInput(session = session, inputId = "selectAgent", choices = choices)
    }
  })
  
  
  observe({
    if (input$cdmSource == "All Sources") {
      hide(selector = "#sidebarCollapsed li a[data-value=provenance]")
      hide(selector = "#sidebarCollapsed li a[data-value=heelResults]")  
      hide(selector = "#sidebarCollapsed li a[data-value=conceptKb]")  
      hide(selector = "#sidebarCollapsed li a[data-value=conceptSetKb]")  
      hide(selector = "#sidebarCollapsed li a[data-value=cohortDefKb]")  
      
      updateTabItems(session = session, inputId = "tabs", selected = "overview")
      
    } else {
      show(selector = "#sidebarCollapsed li a[data-value=provenance]")
      show(selector = "#sidebarCollapsed li a[data-value=heelResults]")  
      show(selector = "#sidebarCollapsed li a[data-value=conceptKb]")  
      show(selector = "#sidebarCollapsed li a[data-value=conceptSetKb]")  
      show(selector = "#sidebarCollapsed li a[data-value=cohortDefKb]")  
      updateTabItems(session = session, inputId = "tabs", selected = "provenance")
    }
  })
  
  observe({
    if (input$tabs == "overview") {
      hide(selector = "#sidebarCollapsed li a[data-value=provenance]")
      hide(selector = "#sidebarCollapsed li a[data-value=heelResults]")  
      hide(selector = "#sidebarCollapsed li a[data-value=conceptKb]")  
      hide(selector = "#sidebarCollapsed li a[data-value=conceptSetKb]")  
      hide(selector = "#sidebarCollapsed li a[data-value=cohortDefKb]")  
      updateSelectInput(session = session, inputId = "cdmSource", selected = "All Sources")
      
    } else if (input$tabs == "provenance" & input$cdmSource != "All Sources") {
      index <- which(sapply(cdmSources, function(c) c$name == input$cdmSource))
      cdmSource <- cdmSources[[index]]
      .createSourceOverview(cdmSource = cdmSource, parentDiv = "overviewBox", width = 12)
    }
  }) 

  
  # Output DataTable renders ------------------------------------------
  
  output$dtTemporalEvent <- renderDataTable(expr = {
    input$btnAddTemporalEvent
    input$btnEditTemporalEvent
    input$btnConfirmDeleteTempEvent
    
    if (nrow(associatedTempEvents()) > 0) {
      metaDataTable <- dplyr::select(associatedTempEvents(), 
                                     `Date` = DATE,
                                     `Temporal Event` = VALUE_AS_STRING)
    } else {
      metaDataTable <- data.frame()
    }
    
    options <- list(pageLength = 10,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '15vh')
    selection <- list(mode = "single", target = "row")
    
    table <- datatable(metaDataTable,
                       options = options,
                       selection = "single",
                       rownames = FALSE, 
                       class = "stripe nowrap compact", extensions = c("Responsive"))
    
    table  
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
    
    options <- list(pageLength = 50,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '50vh')
    selection <- list(mode = "single", target = "row")
    
    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE, 
                       class = "stripe wrap compact", extensions = c("Responsive")) %>%
      formatStyle("Heel Status", #"Warning Type",
                  target = "row",
                  backgroundColor = styleEqual(c("Non-issue", "Needs Review", "Issue"),
                                               c("#deffc9", "#fffedb", "#ffdbdb")))
    
    table
  })

  output$dtCohortPicker <- renderDataTable({
    df <- cohortDefinitions()

    options <- list(pageLength = 50,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '35vh')
    selection <- list(mode = "single", target = "row")
    
    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE, 
                       class = "stripe nowrap compact", extensions = c("Responsive"))
    
    table
  })
  
  output$dtConceptSetPicker <- renderDataTable({
    df <- conceptSets()
    
    options <- list(pageLength = 50,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '35vh')
    selection <- list(mode = "single", target = "row")
    
    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE, 
                       class = "stripe nowrap compact", extensions = c("Responsive"))
    
    table
  })
  
  observeEvent(eventExpr = input$dtCohortPicker_rows_selected, handlerExpr = {
    row_count <- input$dtCohortPicker_rows_selected
    cohortId <- cohortDefinitions()[row_count, ]$ID
    
    url <- sprintf("%s/cohortdefinition/%d", baseUrl, as.integer(cohortId))
    
    json <- httr::content(x = httr::GET(url), encoding = "json")

  })
  
  observeEvent(eventExpr = input$dtConceptSetPicker_rows_selected, handlerExpr = {
    row_count <- input$dtConceptSetPicker_rows_selected
    conceptSetId <- conceptSets()[row_count, ]$ID
    concepts <- OhdsiRTools::getConceptSetConceptIds(baseUrl = baseUrl, 
                                                     setId = conceptSetId)
    
    output$includedConcepts <- renderText({ paste(concepts, collapse = ",") })
  })
  
  # Helpers ---------------------------------------------------
  
  agentTextInputs <- c("agentFirstName",
                       "agentLastName",
                       "agentSuffix",
                       "agentAlgorithm",
                       "agentDescription")
  
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

  # Exists functions ---------------------------------------------------------------
  
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
  
  # Query Metadata tables -----------------------------------------------------------------
  
  .refreshConceptPlot <- function() {

    df <- conceptsMeta()
    meta <- associatedTempEvents()

    if (nrow(df) > 0) {
      if (nrow(meta) > 0) {
        plot_ly(data = df, x = ~DATE, y = ~COUNT_VALUE, name = "Concept Prevalance",
                type = "scatter", mode = "lines", source = "C") %>%
          add_trace(data = meta, x = ~DATE, y = ~COUNT_VALUE, text = ~VALUE_AS_STRING, 
                    name = "Temporal Event", mode = "markers") %>%
          layout(xaxis = list(title = "Date"), yaxis = list(title = "# of Events"))
      } else {
        plot_ly(data = df, x = ~DATE, y = ~COUNT_VALUE, name = "Concept Prevalance",
                type = "scatter", mode = "lines", source = "C") %>%
          layout(xaxis = list(title = "Date"), yaxis = list(title = "# of Events"))
      }
      
    } else {
      NULL
    }
  }
  
  .getHeelResults <- function() {
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "heelResults/getHeels.sql", 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails()$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema())
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .getAgents <- function() {

    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
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
  
  # Output Plotly Renders ---------------------------------------------------
  
  output$conceptKbPlot <- renderPlotly({
    .refreshConceptPlot()
  })
  
  # InfoBox Renders -----------------------------------------------------
  
 
  output$numPersons <- renderInfoBox({
    df <- readRDS("data/totalPop.rds")
    totalPop <- sum(df$COUNT_VALUE)
    infoBox(
      "Total Persons", prettyNum(totalPop, big.mark = ","), icon = icon("users"),
      color = "purple", fill = TRUE
    )
  })
  
  output$numHumanAgents <- renderInfoBox({
    agents <- lapply(cdmSources[sapply(cdmSources, function(c) c$name != "All Sources")], function(cdmSource) {
      connectionDetails <- .getConnectionDetails(cdmSource)
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      sql <- SqlRender::renderSql(sql = "select agent_first_name, agent_last_name 
                                  from @resultsDatabaseSchema.meta_agent where meta_agent_concept_id = 1000;",
                                  resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      agents <- DatabaseConnector::querySql(connection = connection, sql = sql)
      DatabaseConnector::disconnect(connection = connection)
      agents
    })
    
    uniques <- dplyr::distinct(do.call("rbind", agents))
    
    infoBox(
      "Number of Distinct Human Agents", nrow(uniques), icon = icon("user-tag"),
      color = "red", fill = TRUE
    )
  })
  
  output$numAlgorithmAgents <- renderInfoBox({
    agents <- lapply(cdmSources[sapply(cdmSources, function(c) c$name != "All Sources")], function(cdmSource) {
      connectionDetails <- .getConnectionDetails(cdmSource)
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      sql <- SqlRender::renderSql(sql = "select agent_algorithm 
                                  from @resultsDatabaseSchema.meta_agent where meta_agent_concept_id = 2000;",
                                  resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      agents <- DatabaseConnector::querySql(connection = connection, sql = sql)
      DatabaseConnector::disconnect(connection = connection)
      agents
    })
    
    uniques <- dplyr::distinct(do.call("rbind", agents))
    infoBox(
      "Number of Distinct Algorithm Agents", nrow(uniques), icon = icon("code"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$propTagged <- renderInfoBox({
    counts <- lapply(cdmSources[sapply(cdmSources, function(c) c$name != "All Sources")], function(cdmSource) {
      connectionDetails <- .getConnectionDetails(cdmSource)
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      sql <- SqlRender::renderSql(sql = "select count(*) from @resultsDatabaseSchema.meta_entity_activity;",
                                  resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      count <- DatabaseConnector::querySql(connection = connection, sql = sql)
      DatabaseConnector::disconnect(connection = connection)
      as.integer(count > 0)
    })
    
    prop <- sum(unlist(counts)) / length(cdmSources[sapply(cdmSources, function(c) c$name != "All Sources")])
    
    infoBox(
      "Proportion of Sources with Metadata", prop * 100.00, icon = icon("percent"),
      color = "green", fill = TRUE
    )
  })
  
  # Observe Events ------------------------------------------------------
  
  observeEvent(input$btnEditTemporalEvent, handlerExpr = {
    .editTemporalEvent()
    updateTextAreaInput(session = session, inputId = "temporalEventValue", value = "")
    updateTextInput(session = session, inputId = "conceptStartDate", value = "")
  }, priority = 1)
  
  observeEvent(input$btnDeleteTemporalEvent, handlerExpr = {
    showModal(
      modalDialog(
        title = "Delete Temporal Event",
        "Are you sure you want to delete this temporal event?",
        actionButton(inputId = "btnConfirmDeleteTempEvent", label = "Yes, delete this temporal event")
      )
    )
  })
  
  observeEvent(input$btnConfirmDeleteTempEvent, handlerExpr = {
    .deleteTemporalEvent()
    updateTextAreaInput(session = session, inputId = "temporalEventValue", value = "")
    updateTextInput(session = session, inputId = "conceptStartDate", value = "")
  }, priority = 1)
  
  observeEvent(input$btnAddTemporalEvent, handlerExpr = {
    .addTemporalEvent()
    updateTextInput(session = session, inputId = "conceptStartDate", value = "")
    updateTextAreaInput(session = session, inputId = "temporalEventValue", value = "")
  }, priority = 1)
  
  observeEvent(eventExpr = input$dtTemporalEvent_rows_selected, handlerExpr = {
    row_count <- input$dtTemporalEvent_rows_selected
    activityStartDate <- associatedTempEvents()[row_count,]$DATE
    valueAsString <- associatedTempEvents()[row_count,]$VALUE_AS_STRING
    updateTextInput(session = session, inputId = "conceptStartDate", value = activityStartDate)
    updateTextAreaInput(session = session, inputId = "temporalEventValue", value = valueAsString)
  })
  

  observeEvent(eventExpr = input$btnModalEditSourceDesc, handlerExpr = {
    showModal(modalDialog(
      title = "Edit Source Description",
      textAreaInput(inputId = "sourceDescEdit", label = "Source Description", 
                    placeholder = "Needs a source description", width = "500px", height = "300px",
                    value = .getSourceDescription(connectionDetails = connectionDetails(),
                                                  resultsDatabaseSchema = resultsDatabaseSchema())),
      actionButton(inputId = "btnEditSourceDesc", label = "Submit Changes")
    )
    )
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnEditSourceDesc, handlerExpr = {
    .submitSourceDescription()
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnEditHeelResults, handlerExpr = {
    showModal(modalDialog(
      title = "Edit Heel Result Annotation"
    )
    )
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnDeleteHeelResults, handlerExpr = {
    showModal(modalDialog(
      title = "Delete Heel Result Annotation"
    )
    )
  })
  
  observeEvent(eventExpr = input$dtHeelResults_rows_selected, handlerExpr = {
    row_count <- input$dtHeelResults_rows_selected
    annotationAsString <- .getHeelResults()[row_count,]$ANNOTATION_AS_STRING
    
    valueAsString <- .getHeelResults()[row_count,]$VALUE_AS_STRING
    if (!is.na(annotationAsString)) {
      updateSelectInput(session = session, inputId = "heelStatus", selected = annotationAsString)
    }
    if (!is.na(valueAsString)) {
      updateTextInput(session = session, inputId = "heelAnnotation", value = valueAsString)
    } else {
      updateTextInput(session = session, inputId = "heelAnnotation", value = "")
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
  

  # Crud operations -----------------------------------------------------------
  
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
      activity_start_date = as.Date(input$conceptStartDate),
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
  

})

