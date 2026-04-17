results_folder <- here("Results", cdmName(cdm))
if (!file.exists(results_folder)) {
  dir.create(results_folder, recursive = TRUE)
}
results <- list()
createLogFile(logFile = file.path(results_folder, "log_{date}_{time}"))

maxObsEnd <- cdm$observation_period |>
  summarise(max_obs_end = max(observation_period_end_date, na.rm = TRUE)) |>
  dplyr::pull()

study_period <- c(as.Date(study_start), as.Date(maxObsEnd))

# parametrisation
drugs <- importCodelist(here("Cohorts", "drugs"), type = "csv")
drugs_tromb <- drugs[grepl("thrombolytics", names(drugs))]
drugs_rest <- drugs[!grepl("thrombolytics", names(drugs))]
mi_proc <- importCodelist(here("Cohorts", "miProcedures"), type = "csv")
stroke_proc <- importCodelist(here("Cohorts", "strokeProcedures"), type = "csv")
conditions <- importCodelist(here("Cohorts", "comorbidities"), type = "csv")
miTypes <- importCodelist(here("Cohorts", "miTypes"), type = "csv")

ageGroupStrata <- list("age_range" = list(c(18, 64), c(65, 84), c(85, Inf)))
ageGroupChar <- list(c(18, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, Inf))

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

logMessage("Build observation period")
cdm <- buildObservationPeriod(cdm = cdm, recordsFrom = "visit_occurrence")

# create and export snapshot
logMessage("RETRIEVING SNAPSHOT")
results[["snap"]] <- summariseOmopSnapshot(cdm)

# summarise observation periods
logMessage("RETRIEVING OBSERVATION PERIOD SUMMARY")
results[["observation_period"]] <- summariseObservationPeriod(cdm$observation_period)

logMessage("INSTANTIATING COHORTS")
source(here("Cohorts","InstantiateCohorts.R"))

logMessage("SUMMARISE CHARACTERISTICS")
source(here("Analyses", "hospitalCharacteristics.R"))

# export results ----
result <- omopgenerics::bind(results)

omopgenerics::exportSummarisedResult(
  result,
  minCellCount = min_cell_count,
  path = results_folder,
  fileName = "hospital_results_{cdm_name}_{date}.csv"
)
