# ==============================================================================
# DaSHDiagnostic — Study Execution Script
# Run this file to execute the full analysis pipeline at a single site.
# Adjust all parameters in Sections 1–2 before running.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Environment
# ------------------------------------------------------------------------------
library(DaSHDiagnostic)
source("../credentials.r")

# ------------------------------------------------------------------------------
# 1. Site-specific parameters  (edit these for every site)
# ------------------------------------------------------------------------------

JDBC <- "/home/a_kiselev/Jdbc"
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms        = DBMS,
  user        = USER,
  password    = PASSWORD,
  server      = SERVER,
  port        = DB_PORT,
  pathToDriver = JDBC
)

## -- Schema names -------------------------------------------------------------
cdmDatabaseSchema        <- "marketscan_ccaemdcr_aug2025"# read-only CDM
vocabularyDatabaseSchema <- cdmDatabaseSchema     # usually same as CDM
cohortDatabaseSchema     <- "marketscan_ccaemdcr_aug2025_results"        # write-enabled; needs read/write/delete
cohortTable              <- "cohort_DaSH"


## -- Dataset metadata ---------------------------------------------------------
# All results are stratified by dataset; never pooled across sites.
databaseId          <- "MarketScan"           # short identifier, no spaces

## -- Output -------------------------------------------------------------------
outputFolder <- file.path("/home/a_kiselev/output/DaSH")
options(andromedaTempFolder = file.path(outputFolder, "andromedaTemp"))
## -- Pipeline parameters ------------------------------------------------------
runCreateCohorts = TRUE
runBaselineCharacteristics = TRUE
runIRandTTEAnalysis = TRUE
incremental = TRUE
includeCohortStats = TRUE


## -- Launch pipeline --- ------------------------------------------------------
execute(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    outputFolder = outputFolder,
    incremental = incremental,
    includeCohortStats = includeCohortStats,
    minCellCount = 5,
    runCreateCohorts = runCreateCohorts,
    runBaselineCharacteristics = runBaselineCharacteristics,
    runIRandTTEAnalysis = runIRandTTEAnalysis,
    databaseId = databaseId
)
