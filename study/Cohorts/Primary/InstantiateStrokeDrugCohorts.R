info(logger, "INSTANTIATING CARDIOVASCULAR DRUGS COHORT - STROKE")

stroke_drugs_cl <- importCodelist(here("Cohorts", "Primary", "drugs"), type = "csv")

names(stroke_drugs_cl) <- paste0(names(stroke_drugs_cl), "_stroke")

cdm$stroke_drugs <- conceptCohort(
  cdm = cdm,
  table = "drug_exposure",
  conceptSet = stroke_drugs_cl,
  name = "stroke_drugs"
)

# collapse records that are within 14 days of each other
cdm$stroke_drugs <- cdm$stroke_drugs |>
  collapseCohorts(gap = 28,
                  name = "stroke_drugs") 

cdm$stroke_drugs_first <- cdm$stroke_drugs |>
  requireCohortIntersect(
    targetCohortTable = "stroke_first",
    window = c(-28,0),
    name = "stroke_drugs_first"
  ) |>
  requireIsFirstEntry() |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150))

cdm$stroke_drugs_after_event <- cdm$stroke_drugs |>
  inner_join(cdm$stroke_drugs_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "stroke_drugs_after_event", temporary = FALSE)

########
info(logger, "INSTANTIATE ANTIPLATELETS COHORT - STROKE")
### antiplatelets

cdm$antiplatelets_stroke <- unionCohorts(
  cohort = cdm$stroke_drugs_after_event,
  cohortId = c("aspirin_stroke", "p2y12_inhibitors_stroke", "dipyridamole_stroke"),
  gap = 28,
  keepOriginalCohorts = FALSE,
  name = "antiplatelets_stroke"
) |>
  renameCohort(
    newCohortName = "antiplatelets_stroke"
  )

cdm$antiplatelets_stroke_first <- cdm$antiplatelets_stroke |>
  requireIsFirstEntry(name = "antiplatelets_stroke_first")

info(logger, "INSTANTIATED ANTIPLATELETS COHORTS - STROKE")

########
info(logger, "INSTANTIATE DUAL ANTIPLATELETS COHORT - STROKE")
### antiplatelets

cdm$dual_antiplatelet_stroke_1 <- cdm$stroke_drugs_after_event |>
  intersectCohorts(
    cohortId = c("aspirin_stroke", "p2y12_inhibitors_stroke"),
    gap = 28,
    name = "dual_antiplatelet_stroke_1"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_stroke_1"
  )

cdm$dual_antiplatelet_stroke_2 <- cdm$stroke_drugs_after_event |>
  intersectCohorts(
    cohortId = c("aspirin_stroke", "dipyridamole_stroke"),
    gap = 28,
    name = "dual_antiplatelet_stroke_2"
  ) |>
  renameCohort(
    newCohortName = "dual_antiplatelet_stroke_2"
  )

cdm <- omopgenerics::bind(
  cdm$dual_antiplatelet_stroke_1,
  cdm$dual_antiplatelet_stroke_2,
  name = "dual_antiplatelet_stroke"
)
cdm$dual_antiplatelet_stroke <- unionCohorts(
  cohort = cdm$dual_antiplatelet_stroke,
  gap = 28,
  cohortName = "dual_antiplatelet_stroke",
  name = "dual_antiplatelet_stroke"
)

cdm$dual_antiplatelet_stroke_first <- cdm$dual_antiplatelet_stroke |>
  requireIsFirstEntry(name = "dual_antiplatelet_stroke_first")

info(logger, "INSTANTIATED DUAL ANTIPLATELETS COHORTS - STROKE")


info(logger, "INSTANTIATE ANTICOAGULANT COHORT - STROKE")
### anticoagulents

cdm$anticoagulants_stroke <- unionCohorts(
  cohort = cdm$stroke_drugs_after_event,
  cohortId = c("warfarin_stroke", "doacs_stroke"),
  gap = 28,
  keepOriginalCohorts = FALSE,
  name = "anticoagulants_stroke"
) |>
  renameCohort(
    newCohortName = "anticoagulants_stroke"
  )

cdm$anticoagulants_stroke_first <- cdm$anticoagulants_stroke |>
  requireIsFirstEntry(name = "anticoagulants_stroke_first")

info(logger, "INSTANTIATED ANTICOAGULANT COHORT - STROKE")

info(logger, "INSTANTIATE ANTICOAGULANT WITH AF COHORT - STROKE")

cdm$anticoagulants_stroke_af <- cdm$anticoagulants_stroke |>
  requireCohortIntersect(
    targetCohortTable = "atrial_fibrillation",
    window = c(-Inf, -1),
    atFirst = TRUE,
    name = "anticoagulants_stroke_af"
  ) |>
  renameCohort(
    newCohortName = "anticoagulants_stroke_af"
  )

cdm$anticoagulants_stroke_af_first <- cdm$anticoagulants_stroke_af |>
  requireIsFirstEntry(name = "anticoagulants_stroke_af_first")

info(logger, "INSTANTIATED ANTICOAGULANT WITH AF COHORT - STROKE")

info(logger, "INSTANTIATE LIPID-LOWERING COHORT - STROKE")
### antiplatelets

cdm$lipid_lowering_stroke <- unionCohorts(
  cohort = cdm$stroke_drugs_after_event,
  cohortId = c("statin_stroke", "pcsk9_inhibitors_stroke", "ezetimibe_stroke"),
  gap = 28,
  keepOriginalCohorts = FALSE,
  name = "lipid_lowering_stroke"
) |>
  renameCohort(
    newCohortName = "lipid_lowering_stroke"
  )

cdm$lipid_lowering_stroke_first <- cdm$lipid_lowering_stroke |>
  requireIsFirstEntry(name = "lipid_lowering_stroke_first")

info(logger, "INSTANTIATED LIPID-LOWERING COHORT - STROKE")

info(logger, "INSTANTIATE ANTIHYPERTENSIVE COHORT - STROKE")
### antiplatelets

cdm$antihypertensive_stroke <- unionCohorts(
  cohort = cdm$stroke_drugs_after_event,
  cohortId = c("acei_arbs_stroke", "calcium_channel_blockers_stroke", "thiazide_diuretics_stroke"),
  gap = 28,
  keepOriginalCohorts = FALSE,
  name = "antihypertensive_stroke"
) |>
  renameCohort(
    newCohortName = "antihypertensive_stroke"
  )

cdm$antihypertensive_stroke_first <- cdm$antihypertensive_stroke |>
  requireIsFirstEntry(name = "antihypertensive_stroke_first")

info(logger, "INSTANTIATED ANTIHYPERTENSIVE COHORT - STROKE")


### combine into one table

cdm <- omopgenerics::bind(
  cdm$stroke_drugs_after_event,
  cdm$antiplatelets_stroke,
  cdm$dual_antiplatelet_stroke,
  cdm$anticoagulants_stroke,
  cdm$anticoagulants_stroke_af,
  cdm$lipid_lowering_stroke,
  cdm$antihypertensive_stroke,
  name = "stroke_drugs_final"
)

cdm <- omopgenerics::bind(
  cdm$stroke_drugs_first,
  cdm$antiplatelets_stroke_first,
  cdm$dual_antiplatelet_stroke_first,
  cdm$anticoagulants_stroke_first,
  cdm$anticoagulants_stroke_af_first,
  cdm$lipid_lowering_stroke_first,
  cdm$antihypertensive_stroke_first,
  name = "stroke_drugs_first"
)

info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT - STROKE")
