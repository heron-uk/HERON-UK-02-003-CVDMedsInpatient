# parametrisation
drugs_cl <- importCodelist(here("Cohorts", "drugs"), type = "csv")
drugs_tromb <- drugs_cl[grepl("thrombolytics", names(drugs_cl))]
drugs_rest <- drugs_cl[!grepl("thrombolytics", names(drugs_cl))]
mi_proc <- importCodelist(here("Cohorts", "miProcedures"), type = "csv")
stroke_proc <- importCodelist(here("Cohorts", "strokeProcedures"), type = "csv")
comorb <- importCodelist(here("Cohorts", "comorbidities"), type = "csv")
miTypes <- importCodelist(here("Cohorts", "miTypes"), type = "csv")

ageGroupStrata <- list("age_range" = list(c(18, 64), c(65, 84), c(85, Inf)))
ageGroupChar <- list(c(18, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, Inf))

# Cohort Counts + Attrition

logMessage("Extract counts")

results[["cohort_count_mi"]] <- cdm$mi_inpatient_first |>
  summariseCohortCount()

results[["cohort_count_stroke"]] <- cdm$stroke_inpatient_first|>
  summariseCohortCount()

logMessage("Cohort code use")

results[["cohort_code_use_mi"]] <- summariseCohortCodeUse(
  cohortTable = "mi_inpatient_first",
  cdm = cdm,
  timing = "entry", 
  x = acute_mi_cl
)

results[["cohort_code_use_stroke_broad"]] <-summariseCohortCodeUse(
  cohortTable = "stroke_inpatient_first",
  cdm = cdm,
  timing = "entry",
  x = stroke_broad, 
  cohortId = "stroke_broad"
)

logMessage("Extract attrition")

results[["cohort_attrition_mi"]] <- cdm$mi_inpatient_first |>
  summariseCohortAttrition()

results[["cohort_attrition_stroke"]] <- cdm$stroke_inpatient_first |>
  summariseCohortAttrition()
  
logMessage("Characterise MI")

cdm$mi_inpatient_chars <- cdm$mi_inpatient_first |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    ageGroup = ageGroupStrata,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "mi_inpatient_chars"
  ) |>
  addEthnicity() |>
  addSES() |>
  addConceptIntersectFlag(
    conceptSet = miTypes["nstemi"],
    window = c(0, 0),
    name = "mi_inpatient_chars",
    nameStyle = "not_stemi"
  ) |>
  addConceptIntersectFlag(
    conceptSet = miTypes["stemi"],
    window = c(0, 0),
    name = "mi_inpatient_chars",
    nameStyle = "stemi"
  ) |>
  mutate(mi_type = case_when(
    stemi == 1 & not_stemi == 1 ~ "Both",
    stemi == 1 & not_stemi == 0 ~ "STEMI",
    stemi == 0 & not_stemi == 1 ~ "not STEMI",
    stemi == 0 & not_stemi == 0 ~ "None"
  ))

char_mi <- cdm$mi_inpatient_chars |>
  summariseCharacteristics(
    ageGroup = ageGroupChar,
    conceptIntersectFlag = list(
      "Drugs [0, 14]" = list(
        conceptSet = drugs_rest,
        window = c(0, 14)
      ),
      "Drugs [-28, 28]" = list(
        conceptSet = drugs_tromb,
        window = c(-28, 28)
      ),
      "Procedures [-28, 28]" = list(
        conceptSet = mi_proc,
        window = c(-28, 28)
      ),
      "Prior Stroke [-30 to 0]" = list(
        conceptSet = stroke_cl,
        window = c(-30, 0)
      ),
      "Prior Comorbidities (-Inf to 0]" = list(
        conceptSet = comorb,
        window = c(-Inf, 0)
      )
    ),
    
    cohortIntersectFlag = list(
      "Prior Comorbidities (-Inf to 0]" = list(
        targetCohortTable = "ckd_any",
        window = c(-Inf, 0)
      ),
      "Prior Comorbidities (-Inf to 0]" = list(
        targetCohortTable = "obesity",
        window = c(-Inf, 0)
      )
    ),
    
    tableIntersectFlag = list(
      "28-day mortality" = list(
        tableName = "death",
        window = c(0, 28)
      )
    ),
    
    strata = list("mi_type", c("age_range"), c("sex"), c("ses")),
    
    otherVariables = c("ses", "ethnicity", "mi_type")
  )

results[["summmarise_characteristics_mi"]] <- char_mi

logMessage("Characterise Stroke")

cdm$stroke_inpatient_chars <- cdm$stroke_inpatient_first |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    ageGroup = ageGroupStrata,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "stroke_inpatient_chars"
  ) |>
  addEthnicity() |>
  addSES()

char_stroke <- cdm$stroke_inpatient_chars |>
  summariseCharacteristics(
    ageGroup = ageGroupChar,
    conceptIntersectFlag = list(
      "Drugs [0, 14]" = list(
        conceptSet = drugs_rest,
        window = c(0, 14)
      ),
      "Drugs [-28, 28]" = list(
        conceptSet = drugs_tromb,
        window = c(-28, 28)
      ),
      "Procedures [-28, 28]" = list(
        conceptSet = stroke_proc,
        window = c(-28, 28)
      ),
      "Prior MI (-30 to 0]" = list(
        conceptSet = acute_mi_cl,
        window = c(-30, 0)
      ),
      "Prior Comorbidities (-Inf to 0]" = list(
        conceptSet = comorb,
        window = c(-Inf, 0)
      )
    ),
    
    cohortIntersectFlag = list(
      "Prior Comorbidities (-Inf to 0]" = list(
        targetCohortTable = "ckd_any",
        window = c(-Inf, 0)
      ),
      "Prior Comorbidities (-Inf to 0]" = list(
        targetCohortTable = "obesity",
        window = c(-Inf, 0)
      )
    ),
    
    tableIntersectFlag = list(
      "28-day mortality" = list(
        tableName = "death",
        window = c(0, 28)
      )
    ),
    
    strata = list(c("age_range"), c("sex"), c("ses")),
    
    otherVariables = c("ses", "ethnicity")
  )

results[["summmarise_characteristics_stroke"]] <- char_stroke

logMessage("Summarise timming using datetime")

if (omopgenerics::cdmVersion(cdm) == "5.3") {
  colsAD <- c("admit" = "admitting_source_concept_id", "discharge" = "discharge_to_concept_id")
} else {
  colsAD <- c("admit" = "admitted_from_concept_id", "discharge" = "discharged_to_concept_id")
}

# visits
persons <- cdm$stroke_inpatient_first |>
  dplyr::select("subject_id") |>
  dplyr::union_all(
    cdm$mi_inpatient_first |>
      dplyr::select("subject_id")
  ) |>
  dplyr::distinct(subject_id) |>
  dplyr::compute(name = omopgenerics::uniqueTableName())

visits <- cdm$visit_occurrence |>
  dplyr::rename("subject_id" = "person_id") |>
  dplyr::filter(.data$visit_concept_id %in% !!inpatientCodes) |>
  dplyr::inner_join(persons, by = "subject_id") |>
  dplyr::select("subject_id", "visit_start_date", "visit_start_datetime", "visit_end_date", dplyr::all_of(colsAD)) |>
  dplyr::compute(name = omopgenerics::uniqueTableName())

# timing between admission fro Stroke and medication/procedures
nm <- omopgenerics::uniqueTableName()
strokeAdmission <- visits |>
  dplyr::inner_join(
    cdm$stroke_inpatient_first |>
      PatientProfiles::addCohortName() |>
      dplyr::select(
        "subject_id", "stroke_date" = "cohort_start_date", "cohort_name"
      ),
    by = c("subject_id")
  ) |>
  dplyr::filter(
    .data$visit_start_date <= .data$stroke_date &
      .data$visit_end_date >= .data$stroke_date
  ) |>
  dplyr::select(
    "subject_id", "visit_start_datetime", "admit", "discharge", "visit_end_date",
    "cohort_name"
  ) |>
  dplyr::compute(name = nm)
  
codes <- drugs_cl[c("thrombolytics_alteplase", "thrombolytics_tenecteplase")] |>
  dplyr::as_tibble() |>
  dplyr::rename("drug" = "codelist_name", "drug_concept_id" = "concept_id")

nm <- omopgenerics::uniqueTableName()
cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = codes)

drugOfInterest <- cdm$drug_exposure |>
  dplyr::select("subject_id" = "person_id", "drug_concept_id", "drug_exposure_start_date", "drug_exposure_start_datetime") |>
  dplyr::inner_join(cdm[[nm]], by = "drug_concept_id") |>
  dplyr::inner_join(
    strokeAdmission |>
      dplyr::select("cohort_name", "subject_id", "visit_start_datetime", "visit_end_date"),
    by = "subject_id"
  ) |>
  dplyr::filter(
    drug_exposure_start_date <= visit_end_date &
      visit_start_datetime <= drug_exposure_start_datetime
  ) |>
  dplyr::group_by(cohort_name, subject_id, drug) |>
  dplyr::summarise(drug_exposure_start_datetime = min(drug_exposure_start_datetime)) |>
  dplyr::collect() |>
  tidyr::pivot_wider(
    names_from = "drug",
    values_from = "drug_exposure_start_datetime"
  )

codes <- stroke_proc |>
  dplyr::as_tibble() |>
  dplyr::rename("procedure" = "codelist_name", "procedure_concept_id" = "concept_id")
nm <- omopgenerics::uniqueTableName()
cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = codes)

proceduresOfInterest <- cdm$procedure_occurrence |>
  dplyr::select(subject_id = "person_id", "procedure_concept_id", "procedure_date",
                "procedure_datetime") |>
  dplyr::inner_join(cdm[[nm]], by = "procedure_concept_id") |>
  dplyr::inner_join(
    strokeAdmission |>
      dplyr::select("cohort_name", "subject_id", "visit_start_datetime", "visit_end_date"),
    by = "subject_id"
  ) |>
  dplyr::filter(
    procedure_date <= visit_end_date &
      visit_start_datetime <= procedure_datetime
  ) |>
  dplyr::group_by(cohort_name, subject_id, procedure) |>
  dplyr::summarise(procedure_datetime = min(procedure_datetime)) |>
  dplyr::collect() |>
  tidyr::pivot_wider(
    names_from = "procedure",
    values_from = "procedure_datetime"
  )

x <- strokeAdmission |>
  dplyr::select("cohort_name","subject_id", "visit_start_datetime", "admit", "discharge") |>
  dplyr::collect() |>
  dplyr::left_join(drugOfInterest, by = c("cohort_name", "subject_id")) |>
  dplyr::left_join(proceduresOfInterest, by = c("cohort_name", "subject_id"))

cols <- c(names(stroke_proc), "thrombolytics_alteplase", "thrombolytics_tenecteplase")
for (col in cols) {
  if (col %in% colnames(x)) {
    x[[col]] <- as.numeric(difftime(time1 = x[[col]], time2 = x$visit_start_datetime, units = "mins"))
  } else {
    x[[col]] <- as.numeric(NA)
  }
}

x$admit <- sprintf("%i", dplyr::coalesce(x$admit, 0))
x$discharge <- sprintf("%i", dplyr::coalesce(x$discharge, 0))

results$extra_stroke <- PatientProfiles::summariseResult(
  table = x,
  group = "cohort_name", 
  variables = list(
    c("discharge", "admit"),
    cols
  ),
  estimates = list(
    c("count", "percentage"),
    c("percentage_missing", "count_missing", "min", "max", "q05", "q25", 
      "median", "q75", "q95")
  )
)

# timing between admission from MI and medication/procedures
nm <- omopgenerics::uniqueTableName()
miAdmission <- visits |>
  dplyr::inner_join(
    cdm$mi_inpatient_chars |>
      dplyr::select(
        "subject_id", "mi_date" = "cohort_start_date", "mi_type"
      ),
    by = c("subject_id")
  ) |>
  dplyr::filter(
    .data$visit_start_date <= .data$mi_date &
      .data$visit_end_date >= .data$mi_date
  ) |>
  dplyr::select(
    "subject_id", "visit_start_datetime", "admit", "discharge", "visit_end_date",
    "mi_type"
  ) |>
  dplyr::compute(name = nm)

codes <- drugs_cl[c("thrombolytics_alteplase", "thrombolytics_tenecteplase")] |>
  dplyr::as_tibble() |>
  dplyr::rename("drug" = "codelist_name", "drug_concept_id" = "concept_id")
nm <- omopgenerics::uniqueTableName()
cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = codes)

drugOfInterest <- cdm$drug_exposure |>
  dplyr::select(subject_id = "person_id", "drug_concept_id", "drug_exposure_start_date",
                "drug_exposure_start_datetime") |>
  dplyr::inner_join(cdm[[nm]], by = "drug_concept_id") |>
  dplyr::inner_join(
    miAdmission |>
      dplyr::select("subject_id", "visit_start_datetime", "visit_end_date"),
    by = "subject_id"
  ) |>
  dplyr::filter(
    drug_exposure_start_date <= visit_end_date &
      visit_start_datetime <= drug_exposure_start_datetime
  ) |>
  dplyr::group_by(subject_id, drug) |>
  dplyr::summarise(drug_exposure_start_datetime = min(drug_exposure_start_datetime)) |>
  dplyr::collect() |>
  tidyr::pivot_wider(
    names_from = "drug",
    values_from = "drug_exposure_start_datetime"
  )

codes <- mi_proc |>
  dplyr::as_tibble() |>
  dplyr::rename("procedure" = "codelist_name", "procedure_concept_id" = "concept_id")
nm <- omopgenerics::uniqueTableName()
cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = codes)

proceduresOfInterest <- cdm$procedure_occurrence |>
  dplyr::select(subject_id = "person_id", "procedure_concept_id", "procedure_date",
                "procedure_datetime") |>
  dplyr::inner_join(cdm[[nm]], by = "procedure_concept_id") |>
  dplyr::inner_join(
    miAdmission |>
      dplyr::select("subject_id", "visit_start_datetime", "visit_end_date"),
    by = "subject_id"
  ) |>
  dplyr::filter(
    procedure_date <= visit_end_date &
      visit_start_datetime <= procedure_datetime
  ) |>
  dplyr::group_by(subject_id, procedure) |>
  dplyr::summarise(procedure_datetime = min(procedure_datetime)) |>
  dplyr::collect() |>
  tidyr::pivot_wider(
    names_from = "procedure",
    values_from = "procedure_datetime"
  )

x <- miAdmission |>
  dplyr::select("subject_id", "visit_start_datetime", "admit", "discharge", "mi_type") |>
  dplyr::collect() |>
  dplyr::left_join(drugOfInterest, by = "subject_id") |>
  dplyr::left_join(proceduresOfInterest, by = "subject_id")

cols <- c(names(mi_proc), "thrombolytics_alteplase", "thrombolytics_tenecteplase")
for (col in cols) {
  if (col %in% colnames(x)) {
    x[[col]] <- as.numeric(difftime(time1 = x[[col]], time2 = x$visit_start_datetime, units = "mins"))
  } else {
    x[[col]] <- as.numeric(NA)
  }
}

x$admit <- sprintf("%i", dplyr::coalesce(x$admit, 0))
x$discharge <- sprintf("%i", dplyr::coalesce(x$discharge, 0))
x$cohort_name <- "acute_mi"

results$extra_mi <- PatientProfiles::summariseResult(
  table = x,
  group = "cohort_name",
  strata = "mi_type",
  variables = list(
    c("discharge", "admit"),
    cols
  ),
  estimates = list(
    c("count", "percentage"),
    c("percentage_missing", "count_missing", "min", "max", "q05", "q25", 
      "median", "q75", "q95")
  )
)
