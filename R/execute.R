#' Execute the DaSHDiagnostic study pipeline
#'
#' Runs the study end to end: cohort generation, baseline characteristics, and the
#' incidence-rate / time-to-event safety analysis. Each stage can be toggled independently.
#'
#' @param connectionDetails        DatabaseConnector connection details object.
#' @param cdmDatabaseSchema        Schema containing the OMOP CDM tables (read-only).
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables (usually same as CDM).
#' @param cohortDatabaseSchema     Schema where cohort tables are written (needs read/write/delete).
#' @param cohortTable              Name of the cohort table to create.
#' @param databaseId               Short site identifier used to label all outputs.
#' @param outputFolder             Path where all result files are written.
#' @param incremental              Logical. Passed to \code{generateStudyCohorts}: if
#'   \code{TRUE}, skip cohorts whose SQL checksum is unchanged since the last run.
#' @param includeCohortStats       Logical. Passed to \code{generateStudyCohorts}: if
#'   \code{TRUE}, rebuild cohort SQL with inclusion-rule statistics and export them.
#' @param minCellCount             Minimum number of events/subjects reported; smaller cells
#'   are suppressed. Default 5.
#' @param runCreateCohorts           Logical. Run cohort generation. Default \code{TRUE}.
#' @param runBaselineCharacteristics Logical. Run baseline characterization. Default \code{TRUE}.
#' @param runIRandTTEAnalysis        Logical. Run the IR/TTE safety analysis. Default \code{TRUE}.
#'
#' @return Invisibly \code{NULL}; called for its side effects (files written to
#'   \code{outputFolder}).
#' @export

execute <- function(
    connectionDetails,
    cdmDatabaseSchema,
    vocabularyDatabaseSchema,
    cohortDatabaseSchema,
    cohortTable,
    databaseId,
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
      includeCohortStats       = includeCohortStats
    )}

  if (runBaselineCharacteristics){
    runBaselineCharacteristics(
      connectionDetails        = connectionDetails,
      cdmDatabaseSchema        = cdmDatabaseSchema,
      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
      cohortDatabaseSchema     = cohortDatabaseSchema,
      cohortTable              = cohortTable,
      databaseId               = databaseId,
      minCellCount             = minCellCount,
      outputFolder             = outputFolder
    )}

  if (runIRandTTEAnalysis){
    runIRandTTEAnalysis(connectionDetails        = connectionDetails,
                        cdmDatabaseSchema        = cdmDatabaseSchema,
                        cohortDatabaseSchema     = cohortDatabaseSchema,
                        cohortTable              = cohortTable,
                        databaseId               = databaseId,
                        targetCohortIds = NULL,
                        outcomeCohortIds = NULL,
                        minCellCount = minCellCount,
                        outputFolder = outputFolder
    )}

}
