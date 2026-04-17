
logMessage("Extract counts")
results[["cohort_count"]] <- cdm$index_cohorts |>
  summariseCohortCount()

logMessage("Cohort code use")
results[["cohort_code_use_mi"]] <- summariseCohortCodeUse(
  cohortTable = "index_cohorts",
  cohortId = "acute_mi",
  cdm = cdm,
  timing = "entry", 
  x = conditions["acute_mi"]
)
results[["cohort_code_use_stroke"]] <-summariseCohortCodeUse(
  cohortTable = "index_cohorts",
  cdm = cdm,
  timing = "entry",
  x = conditions["stroke"]
)

logMessage("Extract attrition")
results[["cohort_attrition"]] <- cdm$index_cohorts |>
  summariseCohortAttrition()
  
logMessage("Characterise cohorts")
cdm$index_cohorts <- cdm$index_cohorts |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    ageGroup = ageGroupStrata,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "index_cohorts"
  ) |>
  addEthnicity() |>
  addSES()

results[["characterisation"]] <- cdm$index_cohorts |>
  summariseCharacteristics(
    ageGroup = ageGroupChar,
    conceptIntersectFlag = list(
      "Drugs [0, 28]" = list(
        conceptSet = drugs_rest,
        window = c(0, 28)
      ),
      "Drugs [-7, 28]" = list(
        conceptSet = drugs_tromb,
        window = c(-7, 28)
      ),
      "Procedures [-7, 28]" = list(
        conceptSet = mi_proc,
        window = c(-7, 28)
      ),
      "Prior Comorbidities (-Inf to 0]" = list(
        conceptSet = conditions,
        window = c(-Inf, 0)
      ),
      "Prior MI/Stroke (-Inf to -1]" = list(
        conceptSet = conditions[c("stroke", "acute_mi")],
        window = c(-Inf, -1)
      )
    ),
    
    cohortIntersectFlag = list(
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
    
    otherVariables = c("ses", "ethnicity", "mi_type")
  )

logMessage("Characterise drug initiatiors")

results[["characterisation_drug_initiators"]] <- cdm$drug_initiators |>
  addEthnicity() |>
  addSES() |>
  summariseCharacteristics(
    ageGroup = ageGroupChar,
    otherVariables = c("ses", "ethnicity")
  )

logMessage("Drug initiators MI")
x <- cdm$index_cohorts |>
  addConceptIntersectFlag(
    conceptSet = drugs[nms], 
    indexDate = "cohort_start_date",
    window = c(0, 28),
    nameStyle = "drug_{concept_name}"
  ) |>
  addConceptIntersectFlag(
    conceptSet = drugs["thrombolytics"], 
    indexDate = "cohort_start_date",
    window = c(-7, 28),
    nameStyle = "drug_{concept_name}"
  ) |>
  addCohortName() |>
  select(
    "cohort_name", "age_range", "sex", "ses", "ethnicity", "mi_type", 
    starts_with("drug_")
  ) |>
  collect()

x <- x |>
  mutate(
    age_range = factor(age_range, levels = c(
      "60 to 69", "18 to 39", "40 to 49", "50 to 59", "70 to 79", "80 to 89", 
      "90 or above"
    )),
    sex = factor(sex, levels = c("Female", "Male")),
    ethnicity = factor(ethnicity, levels = unique(c("White", x$ethnicity))),
    ses = factor(ses, c("1", "2", "3", "4", "5", "Missing")),
    mi_type = factor(mi_type, c("not STEMI", "STEMI", "Both", "None"))
  )
drugs <- colnames(x)[startsWith(colnames(x), "drug_")]
cohorts <- unique(x$cohort_name)

models <- list()
for (drug in drugs) {
  for (cohort in cohorts) {
    xx <- x |>
      filter(cohort_name == cohort) |>
      select("age_range", "sex", "ses", "ethnicity", "mi_type", initiate = all_of(drug))
    vars <- xx |>
      select("age_range", "sex", "ses", "ethnicity", "mi_type") |>
      as.list() |>
      keep(\(x) length(unique(x)) > 1) |>
      names()
    if (vars > 0 & length(unique(xx$initiate)) > 1) {
      formula <- paste0("initiate ~ ", paste0(vars, collapse = " + ")) |>
        as.formula()
      models[[paste0(drug, "_", cohort)]] <- tryCatch({
        reg <- glm(formula = formula, data = xx, family = binomial)
        tidy(reg) |>
          filter(term != "(Intercept)") |>
          select(variable_name = "term", coef = "estimate", se_coef = "std.error") |>
          mutate(index_condition = cohort, drug = drug)
      },
      error = function(e) {
        logMessage(paste0("error in ", cohort, " - ", drug, ": ", as.character(e)))
        NULL
      })
    }
  }
}

results[["logistic_regression"]] <- bind_rows(models) |>
  mutate(
    cdm_name = cdmName(cdm),
    result_type = "drug_initiate",
    variable_level = NA_character_
  ) |>
  transformToSummarisedResult(
    group = "index_condition",
    strata = "drug",
    estimates = c("coef", "se_coef"),
    settings = "result_type"
  )
