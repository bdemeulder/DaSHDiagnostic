#' Execute the DaSHDiagnostics study pipeline
#'
#' @param connectionDetails       DatabaseConnector connection details object.
#' @param cdmDatabaseSchema       Schema containing the OMOP CDM tables (read-only).
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables (usually same as CDM).
#' @param cohortDatabaseSchema    Schema where cohort tables will be written (needs read/write/delete).
#' @param cohortTable             Name of the cohort table to create.
#' @param databaseId              Short site identifier used to label all outputs.
#' @param outputFolder            Path where all result files will be written.
#' @param incrementalCohorts         Logical. Passed to \code{generateStudyCohorts}: if \code{TRUE},
#'   skip cohorts whose SQL checksum has not changed since the last run.
#' @param includeCohortStats         Logical. Passed to \code{generateStudyCohorts}: if \code{TRUE},
#'   cohort SQL is rebuilt with inclusion rule statistics enabled and the stats are
#'   exported to \code{outputFolder/cohortStatistics/}.
#' @export

execute <- function(
    connectionDetails,
    cdmDatabaseSchema,
    vocabularyDatabaseSchema,
    cohortDatabaseSchema,
    cohortTable,
    outputFolder,
    incremental,
    includeCohortStats,
    minCellCount = 5,
    runCreateCohorts = TRUE,
    runBaselineCharacteristics = TRUE,
    runIRandTTEAnalysis = TRUE
    ) {

  if (runCreateCohorts){
    generateStudyCohorts(
      connectionDetails        = connectionDetails,
      cdmDatabaseSchema        = cdmDatabaseSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      cohortDatabaseSchema     = cohortDatabaseSchema,
      cohortTable              = cohortTable,
      outputFolder             = outputFolder,
      incremental              = incremental,
      includeCohortStats       = TRUE
    )}

  if (runBaselineCharacteristics){
    runBaselineCharacteristics(
      connectionDetails        = connectionDetails,
      cdmDatabaseSchema        = cdmDatabaseSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      cohortDatabaseSchema     = cohortDatabaseSchema,
      cohortTable              = cohortTable,
      databaseId               = databaseId,
      minCellCount             = 5,
      outputFolder             = outputFolder
    )}

  if (runIRandTTEAnalysis){
    runIRandTTEAnalysis(connectionDetails,
                        cdmDatabaseSchema,
                        cohortDatabaseSchema,
                        cohortTable,
                        databaseId,
                        targetCohortIds = NULL,
                        outcomeCohortIds = NULL,
                        minCellCount = 5,
                        outputFolder
    )}

}
