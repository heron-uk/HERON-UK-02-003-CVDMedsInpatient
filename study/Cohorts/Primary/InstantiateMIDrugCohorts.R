### cardio drugs (excl. bb)
info(logger, "INSTANTIATING CARDIOVASCULAR DRUGS COHORT")

mi_drugs_cl <- importCodelist(here("Cohorts", "Primary", "drugs"), type = "csv")

names(mi_drugs_cl) <- paste0(names(mi_drugs_cl), "_mi")

cdm$mi_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = mi_drugs_cl,
  table = "drug_exposure",
  name = "mi_drugs"
)

# collapse records that are within 14 days of each other
cdm$mi_drugs <- cdm$mi_drugs |>
  collapseCohorts(gap = 28,
                  name = "mi_drugs") 

cdm$mi_drugs_first <- cdm$mi_drugs |>
  requireCohortIntersect(
    targetCohortTable = "acute_mi_first",
    window = c(-28,0),
    name = "mi_drugs_first"
  ) |>
  requireIsFirstEntry() |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150))

cdm$mi_drugs_after_event <- cdm$mi_drugs |>
  inner_join(cdm$mi_drugs_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "mi_drugs_after_event", temporary = FALSE)


info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT")

########
info(logger, "INSTANTIATE BETA BLOCKERS AND HF COHORTS - MI")
### beta blockers

cdm$beta_blockers_after_event <- cdm$mi_drugs_after_event |>
  subsetCohorts(
    cohortId = "beta_blockers_mi",
    name = "beta_blockers_after_event"
  )

cdm$beta_blockers_hf <- cdm$beta_blockers_after_event |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "beta_blockers_hf"
  ) |>
  renameCohort(
    newCohortName = "beta_blockers_hf"
  )

cdm$beta_blockers_no_hf <- cdm$beta_blockers_after_event |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf, -1),
    intersections = 0,
    atFirst = TRUE,
    name = "beta_blockers_no_hf"
  ) |>
  renameCohort(
    newCohortName = "beta_blockers_no_hf"
  )

cdm$beta_blockers_hf_first <- cdm$beta_blockers_hf |>
  requireIsFirstEntry(name = "beta_blockers_hf_first")

cdm$beta_blockers_no_hf_first <- cdm$beta_blockers_no_hf |>
  requireIsFirstEntry(name = "beta_blockers_no_hf_first")

info(logger, "INSTANTIATED BETA BLOCKERS AND HF COHORTS - MI")

########
info(logger, "INSTANTIATE ANTIPLATELETS COHORT - MI")
### antiplatelets

cdm$antiplatelets_mi <- unionCohorts(
  cohort = cdm$mi_drugs_after_event,
  cohortId = c("aspirin_mi", "p2y12_inhibitors_mi", "dipyridamole_mi"),
  gap = 28,
  keepOriginalCohorts = FALSE,
  name = "antiplatelets_mi"
  ) |>
  renameCohort(
    newCohortName = "antiplatelets_mi"
  )

cdm$antiplatelets_mi_first <- cdm$antiplatelets_mi |>
  requireIsFirstEntry(name = "antiplatelets_mi_first")

info(logger, "INSTANTIATED ANTIPLATELETS COHORTS - MI")

########
info(logger, "INSTANTIATE DUAL ANTIPLATELETS COHORT - MI")
### antiplatelets

cdm$dual_antiplatelet_mi_1 <- cdm$mi_drugs_after_event |>
  intersectCohorts(
    cohortId = c("aspirin_mi", "p2y12_inhibitors_mi"),
    gap = 28,
    name = "dual_antiplatelet_mi_1"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_mi_1"
  )

cdm$dual_antiplatelet_mi_2 <- cdm$mi_drugs_after_event |>
  intersectCohorts(
    cohortId = c("aspirin_mi", "dipyridamole_mi"),
    gap = 28,
    name = "dual_antiplatelet_mi_2"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_mi_2"
  )

cdm <- omopgenerics::bind(
  cdm$dual_antiplatelet_mi_1,
  cdm$dual_antiplatelet_mi_2,
  name = "dual_antiplatelet_mi"
)
cdm$dual_antiplatelet_mi <- unionCohorts(
  cohort = cdm$dual_antiplatelet_mi,
  gap = 28,
  cohortName = "dual_antiplatelet_mi",
  name = "dual_antiplatelet_mi"
)

cdm$dual_antiplatelet_mi_first <- cdm$dual_antiplatelet_mi |>
  requireIsFirstEntry(name = "dual_antiplatelet_mi_first")

info(logger, "INSTANTIATED DUAL ANTIPLATELETS COHORTS - MI")

info(logger, "INSTANTIATE ANTICOAGULANT COHORT - MI")
### anticoagulents

cdm$anticoagulants_mi <- unionCohorts(
    cohort = cdm$mi_drugs_after_event,
    cohortId = c("warfarin_mi", "doacs_mi"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    name = "anticoagulants_mi"
  ) |>
  renameCohort(
    newCohortName = "anticoagulants_mi"
  )

cdm$anticoagulants_mi_first <- cdm$anticoagulants_mi |>
  requireIsFirstEntry(name = "anticoagulants_mi_first")

info(logger, "INSTANTIATED ANTICOAGULANT COHORT - MI")

info(logger, "INSTANTIATE ANTICOAGULANT WITH AF COHORT - MI")

cdm$anticoagulants_mi_af <- cdm$anticoagulants_mi |>
  requireCohortIntersect(
    targetCohortTable = "atrial_fibrillation",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "anticoagulants_mi_af"
  ) |>
  renameCohort(
    newCohortName = "anticoagulants_mi_af"
  )

cdm$anticoagulants_mi_af_first <- cdm$anticoagulants_mi_af |>
  requireIsFirstEntry(name = "anticoagulants_mi_af_first")

info(logger, "INSTANTIATED ANTICOAGULANT WITH AF COHORT - MI")

info(logger, "INSTANTIATE LIPID-LOWERING COHORT - MI")
### antiplatelets
  
  cdm$lipid_lowering_mi <- unionCohorts(
    cohort = cdm$mi_drugs_after_event,
    cohortId = c("statin_mi", "pcsk9_inhibitors_mi", "ezetimibe_mi"),
    gap = 28,
    keepOriginalCohorts = FALSE,
    name = "lipid_lowering_mi"
  ) |>
  renameCohort(
    newCohortName = "lipid_lowering_mi"
  )
  
cdm$lipid_lowering_mi_first <- cdm$lipid_lowering_mi |>
  requireIsFirstEntry(name = "lipid_lowering_mi_first")

info(logger, "INSTANTIATED LIPID-LOWERING COHORT - MI")

info(logger, "INSTANTIATE ANTIHYPERTENSIVE COHORT - MI")
### antiplatelets

cdm$antihypertensive_mi <- unionCohorts(
  cohort = cdm$mi_drugs_after_event,
  cohortId = c("acei_arbs_mi", "calcium_channel_blockers_mi", "thiazide_diuretics_mi"),
  gap = 28,
  keepOriginalCohorts = FALSE,
  name = "antihypertensive_mi"
) |>
  renameCohort(
    newCohortName = "antihypertensive_mi"
  )

cdm$antihypertensive_mi_first <- cdm$antihypertensive_mi |>
  requireIsFirstEntry(name = "antihypertensive_mi_first")

info(logger, "INSTANTIATED ANTIHYPERTENSIVE COHORT - MI")


### combine into one table
  
cdm <- omopgenerics::bind(
  cdm$mi_drugs_after_event,
  cdm$beta_blockers_hf,
  cdm$beta_blockers_no_hf,
  cdm$antiplatelets_mi,
  cdm$dual_antiplatelet_mi,
  cdm$anticoagulants_mi,
  cdm$anticoagulants_mi_af,
  cdm$lipid_lowering_mi,
  cdm$antihypertensive_mi,
  name = "mi_drugs_final"
)

cdm <- omopgenerics::bind(
  cdm$mi_drugs_first,
  cdm$beta_blockers_hf_first,
  cdm$beta_blockers_no_hf_first,
  cdm$antiplatelets_mi_first,
  cdm$dual_antiplatelet_mi_first,
  cdm$anticoagulants_mi_first,
  cdm$anticoagulants_mi_af_first,
  cdm$lipid_lowering_mi_first,
  cdm$antihypertensive_mi_first,
  name = "mi_drugs_first"
)

info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT")
