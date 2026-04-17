logMessage("GET INPATIENT COHORT")
inpatientCodes <- c(9201L, 262L, 9203L)
cdm$inpatient_visit <- conceptCohort(
  cdm = cdm,
  conceptSet = list(inpatient = inpatientCodes),
  name = "inpatient_visit"
) 

logMessage("INSTANTIATING HOSPITAL MI COHORT")
cdm$acute_mi <- conceptCohort(
  cdm = cdm,
  conceptSet = conditions["acute_mi"],
  name = "acute_mi"
) |> 
  requireCohortIntersect(
    cohortId = "acute_mi",
    targetCohortTable = "acute_mi", 
    targetCohortId = "acute_mi",
    window = c(-28, -1), 
    intersections = 0
  ) |>
  requireInDateRange(dateRange = study_period) |>
  requireAge(ageRange = c(18, 150)) |>
  requireCohortIntersect(
    cohortId = "acute_mi",
    targetCohortTable = "inpatient_visit",
    targetCohortId = "inpatient",
    window = c(0, 0),
    intersections = c(1, Inf)
  ) |>
  requireIsFirstEntry() |>
  addConceptIntersectFlag(
    conceptSet = miTypes[c("nstemi", "stemi")],
    window = c(0, 0),
    name = "acute_mi",
    nameStyle = "{concept_name}"
  ) |>
  mutate(mi_type = case_when(
    stemi == 1 & nstemi == 1 ~ "Both",
    stemi == 1 & nstemi == 0 ~ "STEMI",
    stemi == 0 & nstemi == 1 ~ "not STEMI",
    stemi == 0 & nstemi == 0 ~ "None"
  )) |>
  select(!c("stemi", "nstemi")) |>
  compute(name = "acute_mi") |>
  copyCohorts(name = "acute_mi", n = 3) |>
  renameCohort(
    newCohortName = c("acute_mi_stemi", "acute_mi_not_stemi"),
    cohortId = c(2, 3)
  ) |>
  filter(
    cohort_definition_id != 2 | mi_type == "STEMI",
    cohort_definition_id != 3 | mi_type == "not STEMI"
  ) |>
  compute(name = "acute_mi") |>
  recordCohortAttrition(reason = "Only STEMI records", cohortId = 2) |>
  recordCohortAttrition(reason = "Only not STEMI records", cohortId = 3)

logMessage("INSTANTIATING HOSPITAL STROKE COHORT")
cdm$stroke <- conceptCohort(
  cdm = cdm,
  conceptSet = conditions["stroke"],
  name = "stroke"
) |> 
  requireCohortIntersect(
    cohortId = "stroke",
    targetCohortTable = "stroke", 
    targetCohortId = "stroke",
    window = c(-28, -1), 
    intersections = 0
  ) |>
  requireInDateRange(dateRange = study_period) |>
  requireAge(ageRange = c(18, 150)) |>
  requireCohortIntersect(
    cohortId = "stroke",
    targetCohortTable = "inpatient_visit",
    targetCohortId = "inpatient",
    window = c(0, 0),
    intersections = c(1, Inf)
  ) |>
  requireIsFirstEntry() |>
  mutate(mi_type = "None") |>
  compute(name = "stroke") |>
  copyCohorts(name = "stroke", n = 2) |>
  renameCohort(newCohortName = "stroke_no_valve", cohortId = "stroke_1") |>
  requireConceptIntersect(
    cohortId = "stroke_no_valve",
    conceptSet = conditions["valve_disorders_broad"], 
    window = c(-Inf, 0),
    intersections = 0
  )

logMessage("BIND STUDY COHORTS")
cdm <- bind(cdm$stroke, cdm$acute_mi, name = "index_cohorts")

logMessage("INSTANTIATE DRUG INITIATORS COHORTS")

nms <- c("antihypertensives", "beta_blockers", "anticoagulants", "antiplatelets", "lipid_lowering_drugs")
all <- c(nms, "thrombolytics")
cdm$drug_initiators <- conceptCohort(
  cdm = cdm,
  conceptSet = drugs[all],
  name = "drug_initiators", 
  exit = "event_end_date"
)

cohorts <- settings(cdm$index_cohorts)$cohort_name
n <- length(cohorts)
if (n > 1) {
  defaultNames <- all
  newNames <- character()
  for (k in 1:(n - 1)) {
    defaultNames <- c(defaultNames, paste0(all, "_", k))
  }
  for (k in seq_len(n)) {
    newNames <- c(newNames, paste0(all, "_", cohorts[k]))
  } 
  cdm$drug_initiators <- cdm$drug_initiators |>
    copyCohorts(name = "drug_initiators", n = n) |>
    renameCohort(newCohortName = newNames, cohortId = defaultNames) 
}

# apply inclusion criteria
for (cohort in cohorts) {
  cdm$drug_initiators <- cdm$drug_initiators |>
    requireCohortIntersect(
      cohortId = paste0(nms, "_", cohort),
      targetCohortTable = "index_cohorts",
      targetCohortId = cohort,
      window = c(-28, 0),
      intersections = c(1, Inf)
    ) |>
    requireCohortIntersect(
      cohortId = paste0("thrombolytics_", cohort),
      targetCohortTable = "index_cohorts",
      targetCohortId = cohort,
      window = c(-28, 7),
      intersections = c(1, Inf)
    )
}

cdm$drug_initiators <- cdm$drug_initiators |>
  requireIsFirstEntry()

logMessage("INSTANTIATE OBESITY COHORTS")
obesity_diag <- list(obesity = c(
  604591, 4271317, 4171972,  4270189, 4079899,  4235799,
  4087487,  40481140, 36713437,  36678790,  45763687,  4097929,  4097996,  4182506,
  4100857,  4160821,  4029277,  4029276,  37166819,  4029900,  36717154,  4005991,
  4163032,  4185912,  4171147,  4177337,  4220527,  4203289,  35622038,  36674490,
  36674893,  4171317,  438731,  37208175,  37164247,  42872398,  4216214,  36716144,
  37110069,  434005,  37395980,  433736,  4212443,  4215969,  4189665,  36716555,
  36717199,  37204685,  37206117,  37397209,
  37162364,  36716151,  37204815,  37311904,  45757112,  4183240,
  4093860,  37163354, 36674827,  3199162,
  45771307,  36676689,  37204691,  37018860,  42539192,  37164244,
  4217557,  37166818,  4211019,  36714072, 36714548,  37165655
))
cdm$obesity <- conceptCohort(
  cdm = cdm, 
  conceptSet = obesity_diag,
  exit = "event_start_date",
  name = "obesity"
)
cdm$obesity_bmi <- measurementCohort(
  cdm = cdm, 
  conceptSet = list("bmi_measurement" = c(3038553, 36304833)), 
  valueAsNumber = list("bmi_measurement" = list(c(30, 60))),   
  name = "obesity_bmi"
)
cdm$obesity_body_weight <- measurementCohort(
  cdm = cdm, 
  conceptSet = list(
    "body_weight"= c(3025315, 4099154, 3013762, 3023166, 3027492)
  ), 
  valueAsNumber = list(
    "body_weight"= list(
      "9529" = c(120, 200), 
      "3195625" = c(265, 440)
    )
  ),
  name = "obesity_body_weight"
)

cdm <- bind(cdm$obesity, cdm$obesity_bmi, cdm$obesity_body_weight, name = "obesity")

cdm$obesity <- cdm$obesity |>
  requireIsFirstEntry() |>
  unionCohorts(cohortName = "obesity") |> 
  exitAtObservationEnd()
