logMessage("GET INPATIENT COHORT")

stroke_broad <- importCodelist(path = here("Cohorts", "Hospital"), type = "csv")
cdm$stroke_broad <- conceptCohort(
  cdm = cdm,
  conceptSet = stroke_broad,
  name = "stroke_broad"
) |>
  collapseCohorts(gap = 28)

cdm <- bind(cdm$stroke, cdm$stroke_broad, name = "stroke")

inpatientCodes <- c(9201, 262, 9203)

cdm$inpatient_visit <- conceptCohort(
  cdm = cdm,
  conceptSet = list(inpatient = inpatientCodes),
  name = "inpatient_visit"
) 

logMessage("GOT INPATIENT COHORT")
logMessage("INSTANTIATING HOSPITAL MI COHORT")

cdm$hospital_mi_first <- cdm$acute_mi |>
  requireInDateRange(
    dateRange = study_period,
    name = "hospital_mi_first"
  ) |>
  requireAge(
    ageRange = c(18, 150)
  ) |>
  requireIsFirstEntry()

cdm$mi_inpatient_first <- cdm$hospital_mi_first |>
  requireCohortIntersect(
    targetCohortTable = "inpatient_visit",
    window = c(0, 0),
    name = "mi_inpatient_first"
  )

logMessage("INSTANTIATED HOSPITAL MI COHORT")

logMessage("INSTANTIATING HOSPITAL STROKE COHORT")

cdm$hospital_stroke_first <- cdm$stroke |>
  requireInDateRange(
    dateRange = study_period,
    name = "hospital_stroke_first"
  ) |>
  requireAge(
    ageRange = c(18, 150)
  ) |>
  requireIsFirstEntry()

cdm$stroke_inpatient_first <- cdm$hospital_stroke_first |>
  requireCohortIntersect(
    cohortId = "ischemic_stroke",
    targetCohortTable = "inpatient_visit",
    window = c(0, 0),
    name = "stroke_inpatient_first"
  ) |>
  requireCohortIntersect(
    cohortId = "stroke_broad",
    targetCohortTable = "inpatient_visit",
    window = c(0, 0)
  )

logMessage("INSTANTIATED HOSPITAL STROKE COHORT")
