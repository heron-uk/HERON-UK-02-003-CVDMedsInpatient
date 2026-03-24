mi_drugs_count <- cdm$mi_drugs_final |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100) |>
  pull(cohort_definition_id)

if(length(mi_drugs_count) > 0) {
cdm$mi_drugs_ad <- cdm$mi_drugs_final |>
  subsetCohorts(
    cohortId = mi_drugs_count,
    name = "mi_drugs_ad"
  ) |>
  addDemographics(
    ageGroup = list(c(18, 39), 
                    c(40, 49),
                    c(50, 59),
                    c(60, 69),
                    c(70, 79),
                    c(80, 89),
                    c(90, 150)),
    priorObservation = FALSE,
    futureObservation = FALSE
  ) |>
  addSES()

results[["ppc_mi"]] <- cdm$mi_drugs_ad |>
  summariseProportionOfPatientsCovered(followUpDays = 1825,
                                       strata = list(c("age_group"), c("sex"), c("ses")
                                                     ))
} else {
  cli::cli_alert_info("Insufficient sample size to perform PPC for MI. No drug cohorts with 100 or more patients were found.")
}

stroke_drugs_count <- cdm$stroke_drugs_final |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100) |>
  pull(cohort_definition_id)


if(length(stroke_drugs_count) > 0) {
cdm$stroke_drugs_ad <- cdm$stroke_drugs_final |>
  subsetCohorts(
    cohortId = stroke_drugs_count,
    name = "stroke_drugs_ad"
  ) |>
  addDemographics(
    ageGroup = list(c(18, 39), 
                    c(40, 49),
                    c(50, 59),
                    c(60, 69),
                    c(70, 79),
                    c(80, 89),
                    c(90, 150)),
    priorObservation = FALSE,
    futureObservation = FALSE
  ) |>
  addSES()

results[["ppc_stroke"]] <- cdm$stroke_drugs_ad |>
  summariseProportionOfPatientsCovered(followUpDays = 1825,
                                       strata = list(c("age_group"), c("sex"), c("ses")
                                       ))
} else {
  cli::cli_alert_info("Insufficient sample size to perform PPC for stroke. No drug cohorts with 100 or more patients were found.")
}




