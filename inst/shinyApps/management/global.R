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

connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

sql <- SqlRender::renderSql("select distinct A.analysis_id, A.stratum_1, B.concept_name, B.concept_id from @resultsDatabaseSchema.achilles_results A
                                join @vocabDatabaseSchema.concept B on cast(stratum_1 as integer) = B.concept_id
                                  and B.concept_id <> 0
                                where A.analysis_id in (402,602,2101,702,1801,802);",
                            resultsDatabaseSchema = resultsDatabaseSchema,
                            vocabDatabaseSchema = vocabDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
achillesConcepts <- DatabaseConnector::querySql(connection = connection, sql = sql)