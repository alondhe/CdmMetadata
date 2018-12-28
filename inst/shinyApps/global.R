.getConnectionDetails <- function(cdmSource) {
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
}

baseUrl <- jsonlite::read_json(path = Sys.getenv("jsonPath"))$baseUrl
atlasUrl <- jsonlite::read_json(path = Sys.getenv("jsonPath"))$atlasUrl
if (atlasUrl == "") {
  atlasUrl <- "http://www.ohdsi.org/web/atlas"
}

cdmSources <- jsonlite::read_json(path = Sys.getenv("jsonPath"))$sources

rdsRoot <- file.path("data", "achilles_concepts")

if (!dir.exists(rdsRoot)) {
  dir.create(rdsRoot, recursive = TRUE)
}

for (cdmSource in cdmSources) {
  rdsFile <- file.path(rdsRoot, sprintf("%s.rds", cdmSource$name))
  connectionDetails <- .getConnectionDetails(cdmSource)
  
  if (!file.exists(rdsFile)) {
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "conceptExplore/getAchillesConcepts.sql",
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails$dbms,
                                             resultsDatabaseSchema = cdmSource$resultsDatabaseSchema,
                                             vocabDatabaseSchema = cdmSource$vocabDatabaseSchema)
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    saveRDS(object = df, file = rdsFile) 
    DatabaseConnector::disconnect(connection = connection)
  }
}

popRds <- file.path("data", "totalPop.rds")
if (!file.exists(popRds)) {
  results <- lapply(cdmSources, function(cdmSources) {
    connectionDetails <- .getConnectionDetails(cdmSource)
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    sql <- SqlRender::renderSql(sql = "select '@cdmSource' as cdm_source, count_value 
                                from @resultsDatabaseSchema.achilles_results where analysis_id = 1;",
                                cdmSource = cdmSource$name,
                                resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    pop <- DatabaseConnector::querySql(connection = connection, sql = sql)
    DatabaseConnector::disconnect(connection = connection)
    pop
  })
  
  totalPop <- do.call("rbind", results)
  saveRDS(object = totalPop, file = popRds)
}



siteSource <- list(
  list(name = "All Sources")
)
  
cdmSources <- c(siteSource, cdmSources)


domainConceptIds <- c("Condition" = 402,
                      "Procedure" = 602, "Drug" = 702, "Measurement" = 1801, "Observation" = 802)

# domainConceptIds <- c("Condition", "Drug", "Observation", "Measurement", "Procedure")

spinnerColor <- "#0dc5c1"

heelIssueTypes <- c("Non-issue", "Needs Review", "Issue")

