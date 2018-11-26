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


.checkShinyDeps <- function() {
  dependencies <- c("shiny",
                    "DT",
                    "shinydashboard",
                    "magrittr",
                    "tidyr")
  
  for (d in dependencies) {
    if (!requireNamespace(d, quietly = TRUE)) {
      message <- sprintf(
        "You must install %1s first. You may install it using devtools with the following code: 
        \n    install.packages('%2s')
        \n\nAlternately, you might want to install ALL suggested packages using:
        \n    devtools::install_github('OHDSI/Achilles', dependencies = TRUE)", d, d)
      stop(message, call. = FALSE)
    }
  }
}


#' Launches the Maintenance Shiny App
#' 
#' @param connectionDetails
#' @param cdmDatabaseSchema
#' @param resultsDatabaseSchema
#' @param vocabDatabaseSchema
#' 
#' @export
launchMaintenanceApp <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 resultsDatabaseSchema,
                                 vocabDatabaseSchema = cdmDatabaseSchema) {

  .checkShinyDeps()
 
  Sys.setenv(sourceName = .getSourceName(connectionDetails, cdmDatabaseSchema),
             dbms = connectionDetails$dbms,
             server = connectionDetails$server,
             user = connectionDetails$user, 
             password = connectionDetails$password,
             port = connectionDetails$port,
             cdmDatabaseSchema = cdmDatabaseSchema,
             resultsDatabaseSchema = resultsDatabaseSchema,
             vocabDatabaseSchema = vocabDatabaseSchema)
  
  appDir <- system.file("shinyApps", "management", package = "CdmMetadata")
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
