dataPath <- file.path(getwd(), "data")
rdsRoot <- file.path(dataPath, "achilles_concepts")
sqlRoot <- file.path(getwd(), "sql")

if (!dir.exists(dataPath)) {
  dir.create(path = dataPath, recursive = TRUE)
}

if (!dir.exists(rdsRoot)) {
  dir.create(rdsRoot, recursive = TRUE)
}

jsonPath <- file.path(dataPath, "sources.rds")

domainConceptIds <- c("Condition" = 402,
                      "Procedure" = 602, "Drug" = 702, "Measurement" = 1801, "Observation" = 802)

spinnerColor <- "#0dc5c1"
heelIssueTypes <- c("Non-issue", "Needs Review", "Issue")
