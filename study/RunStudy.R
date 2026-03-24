results_folder <- here("Results", cdmName(cdm))
if (!file.exists(results_folder)) {
  dir.create(results_folder, recursive = TRUE)
}
results <- list()
logger_name <- gsub(":| |-", "_", paste0("log_01_001_", Sys.time(), ".txt"))
logger <- create.logger()
logfile(logger) <- here(results_folder, logger_name)
level(logger) <- "INFO"
info(logger, "LOG CREATED")

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

if(db_name == "DataLoch" & isTRUE(primary_care) & isFALSE(hospital_care)){
  cdm$drug_exposure <- cdm$drug_exposure |>
    filter(drug_type_concept_id == 32839) |>
    compute(name = "drug_exposure", temporary = FALSE)
} else if(db_name == "DataLoch" & isTRUE(hospital_care) & isFALSE(primary_care)){
  cdm$drug_exposure <- cdm$drug_exposure |>
    filter(drug_type_concept_id == 32829) |>
    compute(name = "drug_exposure", temporary = FALSE)
} else if(isTRUE(primary_care) & isTRUE(hospital_care)){
  cli::cli_abort("Can't have both primary_care and hospital_care set to TRUE. Can only do one at a time.")
} else if(isFALSE(primary_care) & isFALSE(hospital_care)){
  cli::cli_abort("Can't have both primary_care and hospital_care set to FALSE. Must have one set to TRUE.")
}

# create and export snapshot
info(logger, "RETRIEVING SNAPSHOT")
cli::cli_text("- GETTING CDM SNAPSHOT ({Sys.time()})")

snap <- summariseOmopSnapshot(cdm)

results[["snap"]] <- snap

info(logger, "SNAPSHOT COMPLETED")

# summarise observation periods
info(logger, "RETRIEVING OBSERVATION PERIOD SUMMARY")
cli::cli_text("- GETTING OBSERVATION PERIOD SUMMARY ({Sys.time()})")

obs <- summariseObservationPeriod(cdm$observation_period)

results[["observation_period"]] <- obs

info(logger, "OBSERVATION PERIOD SUMMARY COMPLETED")

info(logger, "INSTANTIATING OUTCOME COHORTS")
source(here("Cohorts","InstantiateOutcomeCohorts.R"))
info(logger, "INSTANTIATED OUTCOME COHORTS")

if(isTRUE(hospital_care)){
  info(logger, "INSTANTIATING HOSPITAL COHORTS")
  source(here("Cohorts", "Hospital", "InstantiatingHospitalCohorts.R"))
  info(logger, "HOSPITAL COHORTS INSTANTIATED")
} 

if(isTRUE(primary_care)){
info(logger, "INSTANTIATING PRIMARY CARE COHORTS")
source(here("Cohorts","Primary", "InstantiateMIDrugCohorts.R"))
source(here("Cohorts","Primary", "InstantiateStrokeDrugCohorts.R"))
info(logger, "PRIMARY CARE COHORTS INSTANTIATED")
}

if(isTRUE(run_drug_adherence) & isTRUE(primary_care)){
info(logger, "RUN DRUG ADHERENCE")
source(here("Analyses", "competingRisk.R"))
source(here("Analyses", "multistate.R"))
info(logger, "DRUG ADHERENCE FINISHED")
}

if(isTRUE(run_characteristics) & isTRUE(primary_care)){
info(logger, "RUN SUMMARISE CHARACTERISTICS")
source(here("Analyses", "characteristics.R"))
info(logger, "SUMMARISE CHARACTERISTICS FINISHED")
}

if(isTRUE(run_characteristics) & isTRUE(hospital_care)){
  info(logger, "RUN SUMMARISE CHARACTERISTICS")
  source(here("Analyses", "hospitalCharacteristics.R"))
  info(logger, "SUMMARISE CHARACTERISTICS FINISHED")
}

# export results ----
info(logger, "EXPORTING RESULTS")

result <- omopgenerics::bind(results)

if(isTRUE(primary_care)){

omopgenerics::exportSummarisedResult(result,
                                     minCellCount = min_cell_count,
                                     path = results_folder,
                                     fileName = "primary_results_{cdm_name}_{date}.csv"
)
}

if(isTRUE(hospital_care)){
  
  omopgenerics::exportSummarisedResult(result,
                                       minCellCount = min_cell_count,
                                       path = results_folder,
                                       fileName = "hospital_results_{cdm_name}_{date}.csv"
  )
}



info(logger, "RESULTS EXPORTED")

info(logger, "STUDY CODE FINISHED")

