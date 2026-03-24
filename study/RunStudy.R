results_folder <- here("Results", cdmName(cdm))
if (!file.exists(results_folder)) {
  dir.create(results_folder, recursive = TRUE)
}
results <- list()
createLogFile(logFile = file.path(results_folder, "log_{date}_{time}"))

maxObsEnd <- cdm$observation_period |>
  summarise(maxObsEnd = max(observation_period_end_date, na.rm = TRUE)) |>
  dplyr::pull()

study_period <- c(as.Date(study_start), as.Date(maxObsEnd))

cdm$person <- cdm$person |>
  filter(
    !is.na(gender_concept_id),
    !is.na(year_of_birth),
    gender_concept_id %in% c(8507,8532)
  )

# Load functions
source(here("Analyses","functions.R"))

if (db_name == "DataLoch") {
  cdm$drug_exposure <- cdm$drug_exposure |>
    filter(drug_type_concept_id == 32829)
}

# create and export snapshot
logMessage("RETRIEVING SNAPSHOT")
results[["snap"]] <- summariseOmopSnapshot(cdm)
logMessage("SNAPSHOT COMPLETED")

# summarise observation periods
logMessage("RETRIEVING OBSERVATION PERIOD SUMMARY")
results[["observation_period"]] <- summariseObservationPeriod(cdm$observation_period)
logMessage("OBSERVATION PERIOD SUMMARY COMPLETED")

logMessage("INSTANTIATING OUTCOME COHORTS")
source(here("Cohorts","InstantiateOutcomeCohorts.R"))
logMessage("INSTANTIATED OUTCOME COHORTS")

logMessage("INSTANTIATING HOSPITAL COHORTS")
source(here("Cohorts", "Hospital", "InstantiatingHospitalCohorts.R"))
logMessage("HOSPITAL COHORTS INSTANTIATED")

logMessage("RUN SUMMARISE CHARACTERISTICS")
source(here("Analyses", "hospitalCharacteristics.R"))
logMessage("SUMMARISE CHARACTERISTICS FINISHED")

# export results ----
result <- omopgenerics::bind(results)

omopgenerics::exportSummarisedResult(
  result,
  minCellCount = min_cell_count,
  path = results_folder,
  fileName = "hospital_results_{cdm_name}_{date}.csv"
)
