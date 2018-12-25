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

cdmSources <- jsonlite::read_json(path = Sys.getenv("jsonPath"))$sources


if (!dir.exists("data")) {
  dir.create("data")
}

for (cdmSource in cdmSources) {
  rdsFile <- file.path("data", sprintf("%s.rds", cdmSource$name))
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

siteSource <- list(
  list(name = "All Sources")
)
  
cdmSources <- c(siteSource, cdmSources)