# resultsDatabaseSchema <- Sys.getenv("resultsDatabaseSchema")
# cdmDatabaseSchema <- Sys.getenv("cdmDatabaseSchema")
# vocabDatabaseSchema <- Sys.getenv("vocabDatabaseSchema")
# 
# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = Sys.getenv("dbms"), 
#                                                                 user = Sys.getenv("user"),
#                                                                 password = Sys.getenv("password"), 
#                                                                 server = Sys.getenv("server"), 
#                                                                 port = Sys.getenv("port"))
# 
# connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
#   
# sql <- SqlRender::renderSql("select * from @vocabDatabaseSchema.concept
#                               where standard_concept = 'S' and invalid_reason is null;",
#                             vocabDatabaseSchema = vocabDatabaseSchema)$sql
# sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
# standardConcepts <- DatabaseConnector::querySql(connection = connection, sql = sql)
# DatabaseConnector::disconnect(connection = connection)