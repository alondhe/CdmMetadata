baseUrl <- jsonlite::read_json(path = Sys.getenv("jsonPath"))$baseUrl

cdmSources <- jsonlite::read_json(path = Sys.getenv("jsonPath"))$sources

siteSource <- list(
  list(name = "All Sources")
)
  
cdmSources <- c(siteSource, cdmSources)
