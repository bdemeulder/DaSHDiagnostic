# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of DaSHDiagnostic
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Run baseline cohort diagnostics for the study's target cohorts
#'
#' Runs \code{CohortDiagnostics::executeDiagnostics()} across the target cohorts listed in
#' \code{inst/settings/IRsettings.csv} (temporal characterization, index-event breakdown,
#' inclusion statistics, visit context, cohort relationship), then a second pass restricted to a
#' curated set of drug concepts. Results are exported to
#' \code{outputFolder/baselineCharacteristics} (and \code{..._drug_exposure}) and can be viewed
#' with \code{CohortDiagnostics::launchDiagnosticsExplorer()}.
#'
#' @param connectionDetails        DatabaseConnector connection details object.
#' @param cdmDatabaseSchema        Schema containing the OMOP CDM tables (read-only).
#' @param vocabularyDatabaseSchema Schema containing the vocabulary tables.
#' @param cohortDatabaseSchema     Schema where cohort tables are written.
#' @param cohortTable              Name of the cohort table.
#' @param databaseId               Short site identifier used to label outputs.
#' @param minCellCount             Minimum cell count for suppression. Default 5.
#' @param outputFolder             Path where result files are written.
#'
#' @return Invisibly \code{NULL}; called for its side effects.
#' @export

runBaselineCharacteristics <- function(connectionDetails,
                                       cdmDatabaseSchema,
                                       vocabularyDatabaseSchema = cdmDatabaseSchema,
                                       cohortDatabaseSchema,
                                       cohortTable,
                                       databaseId,
                                       minCellCount = 5,
                                       outputFolder) {

  exportFolder <- file.path(outputFolder, "baselineCharacteristics")
  if (!dir.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  # --------------------------------------------------------------------------
  # Collect all unique cohort IDs used in the study
  # --------------------------------------------------------------------------
  targetIds <- unique(
    read.csv(system.file("settings", "IRsettings.csv", package = "DaSHDiagnostic"))$target_id
  )
  allIds <- unique(targetIds)

  # --------------------------------------------------------------------------
  # Build cohort definition set for all study cohorts
  # --------------------------------------------------------------------------
  pathToCsv       <- system.file("settings", "CohortsToCreate.csv", package = "DaSHDiagnostic")
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  cohortsToCreate <- dplyr::rename(cohortsToCreate, cohortName = cohort_name)

  jsonFolder <- system.file("cohorts",          package = "DaSHDiagnostic")
  sqlFolder  <- system.file("sql", "sql_server", package = "DaSHDiagnostic")

  rows <- cohortsToCreate[cohortsToCreate$cohortId %in% allIds, ]
  if (nrow(rows) == 0) {
    stop("None of the supplied cohort IDs were found in CohortsToCreate.csv")
  }

  missing <- setdiff(allIds, rows$cohortId)
  if (length(missing) > 0) {
    ParallelLogger::logWarn(
      "The following cohort IDs were not found in CohortsToCreate.csv and will be skipped: ",
      paste(missing, collapse = ", ")
    )
  }

  cohortDefinitionSet <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    data.frame(
      cohortId   = rows$cohortId[i],
      cohortName = rows$cohortName[i],
      json       = paste(readLines(file.path(jsonFolder, paste0(rows$cohortId[i], ".json")), warn = FALSE), collapse = "\n"),
      sql        = paste(readLines(file.path(sqlFolder,  paste0(rows$cohortId[i], ".sql")),  warn = FALSE), collapse = "\n"),
      stringsAsFactors = FALSE
    )
  }))

  # --------------------------------------------------------------------------
  # Temporal covariate settings
  # --------------------------------------------------------------------------
  temporalCovariateSettings <- FeatureExtraction::createTemporalCovariateSettings(
    useDemographicsGender               = TRUE,
    useDemographicsAge                  = TRUE,
    useDemographicsAgeGroup             = TRUE,
    useDemographicsRace                 = TRUE,
    useDemographicsEthnicity            = TRUE,
    useDemographicsIndexYear            = TRUE,
    useDemographicsPriorObservationTime = TRUE,
    useDemographicsPostObservationTime  = TRUE,
    useDemographicsTimeInCohort         = TRUE,
    useConditionOccurrence              = TRUE,
    useConditionOccurrencePrimaryInpatient = TRUE,
    useProcedureOccurrence              = TRUE,
    useDrugEraStart                     = TRUE,
    useMeasurement                      = TRUE,
    useMeasurementValue                 = TRUE,
    useMeasurementRangeGroup            = TRUE,
    useMeasurementValueAsConcept        = TRUE,
    useConditionEraStart                = TRUE,
    useConditionEraOverlap              = TRUE,
    useConditionEraGroupStart           = FALSE, # https://github.com/OHDSI/FeatureExtraction/issues/144
    useConditionEraGroupOverlap         = TRUE,
    useDrugExposure                     = FALSE, # too many concept IDs
    useDrugEraOverlap                   = FALSE,
    useDrugEraGroupStart                = FALSE, # https://github.com/OHDSI/FeatureExtraction/issues/144
    useDrugEraGroupOverlap              = TRUE,
    useObservation                      = TRUE,
    useVisitConceptCount                = TRUE,
    useVisitCount                       = TRUE,
    useDeviceExposure                   = TRUE,
    useCharlsonIndex                    = TRUE,
    useDcsi                             = TRUE,
    useChads2                           = TRUE,
    useChads2Vasc                       = TRUE,
    useHfrs                             = FALSE,
    temporalStartDays = c(
        -30,
        -30,
        -30,
        -30,
        -30,
        -30,
        180,
        365,
        1095
    ),
    temporalEndDays = c(
      30,
      90,
      180,
      365,
      1095,
      1825,
      365,
      1095,
      1825
    )
  )

  # --------------------------------------------------------------------------
  # Run diagnostics across all study cohorts
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "Running cohort diagnostics for ", nrow(cohortDefinitionSet), " cohorts: ",
    paste(cohortDefinitionSet$cohortId, collapse = ", ")
  )

  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet               = cohortDefinitionSet,
    exportFolder                      = exportFolder,
    databaseId                        = databaseId,
    connectionDetails                 = connectionDetails,
    cdmDatabaseSchema                 = cdmDatabaseSchema,
    cohortDatabaseSchema              = cohortDatabaseSchema,
    cohortTable                       = cohortTable,
    vocabularyDatabaseSchema          = vocabularyDatabaseSchema,
    temporalCovariateSettings         = temporalCovariateSettings,
    runTemporalCohortCharacterization = TRUE,
    runBreakdownIndexEvents           = TRUE,
    runInclusionStatistics            = TRUE,
    runVisitContext                   = TRUE,
    runCohortRelationship             = TRUE,
    runIncidenceRate                  = FALSE,
    runOrphanConcepts                 = FALSE,
    runTimeSeries                     = FALSE,
    minCellCount                      = minCellCount
  )

  ParallelLogger::logInfo(
    "Cohort diagnostics complete. Results written to ", exportFolder,
    ". Load with CohortDiagnostics::launchDiagnosticsExplorer()."
  )
  invisible(NULL)

  # --------------------------------------------------------------------------
  # Temporal covariate settings for drugs exposure using concept sets
  # --------------------------------------------------------------------------

  temporalCovariateSettings.drugs <- FeatureExtraction::createTemporalCovariateSettings(
    useDrugExposure                     = TRUE,
    useDrugEraStart                     = TRUE,
    useDrugEraOverlap                   = TRUE,
    useDrugEraGroupStart                = TRUE,
    useDrugEraGroupOverlap              = TRUE,
    includedCovariateConceptIds         = c(
      902729,
      1378382,
      1315942,
      40222431,
      1343039,
      739471,
      35834903,
      1351541,
      1366773,
      1366310,
      35807349,
      35807385,
      19058410,
      19033280,
      19010868,
      42900250,
      1361291,
      963987,
      40239056
    ),
    temporalStartDays = c(
      -30,
      -30,
      -30,
      -30,
      -30,
      -30,
      180,
      365,
      1095
    ),
    temporalEndDays = c(
      30,
      90,
      180,
      365,
      1095,
      1825,
      365,
      1095,
      1825
    )
  )
  # --------------------------------------------------------------------------
  # Run diagnostics across all study cohorts for drug exposure
  # --------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "Running cohort diagnostics for ", nrow(cohortDefinitionSet), " cohorts: ",
    paste(cohortDefinitionSet$cohortId, collapse = ", ")
  )

  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet               = cohortDefinitionSet,
    exportFolder                      = paste0(exportFolder, "_drug_exposure"),
    databaseId                        = databaseId,
    connectionDetails                 = connectionDetails,
    cdmDatabaseSchema                 = cdmDatabaseSchema,
    cohortDatabaseSchema              = cohortDatabaseSchema,
    cohortTable                       = cohortTable,
    vocabularyDatabaseSchema          = vocabularyDatabaseSchema,
    temporalCovariateSettings         = temporalCovariateSettings.drugs,
    runTemporalCohortCharacterization = TRUE,
    runBreakdownIndexEvents           = FALSE,
    runInclusionStatistics            = FALSE,
    runVisitContext                   = FALSE,
    runCohortRelationship             = FALSE,
    runIncidenceRate                  = FALSE,
    runOrphanConcepts                 = FALSE,
    runTimeSeries                     = FALSE,
    minCellCount                      = minCellCount
  )

  ParallelLogger::logInfo(
    "Cohort diagnostics drug exposure complete. Results written to ", exportFolder,
    ". Load with CohortDiagnostics::launchDiagnosticsExplorer()."
  )
  invisible(NULL)

}
