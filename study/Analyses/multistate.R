# prepare data drug episodes - MI
mi_drugs_count <- cdm$mi_drugs_final |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100) |>
  pull(cohort_definition_id)

cdm$mi_drugs_msm <- cdm$mi_drugs_final |>
  subsetCohorts(
    cohortId = mi_drugs_count,
    name = "mi_drugs_msm"
  )  |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "mi_drugs_msm",
    ageGroup = list(
      "18 to 39" = c(18, 39),
      "40 to 49" = c(40, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150))
  ) |>
  addSES()

stroke_drugs_count <- cdm$stroke_drugs_final |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100) |>
  pull(cohort_definition_id)

cdm$stroke_drugs_msm <- cdm$stroke_drugs_final |>
  subsetCohorts(
    cohortId = stroke_drugs_count,
    name = "stroke_drugs_msm"
  ) |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "stroke_drugs_msm",
    ageGroup = list(
      "18 to 39" = c(18, 39),
      "40 to 49" = c(40, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150))
  ) |>
  addSES()

if(length(mi_drugs_count) > 0){
nm_1 <- omopgenerics::uniqueTableName()
xd_1 <- cdm$mi_drugs_msm |>
  addCohortName() |>
  group_by(cohort_name, subject_id, age_group, sex, ses) |>
  mutate(t0 = min(cohort_start_date, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    start_discontinuation = date_count_between(t0, cohort_end_date),
    start_drug = date_count_between(t0, cohort_start_date)
  ) |>
  compute(name = nm_1) |>
  addDeathDays(indexDate = "t0", name = nm_1) |>
  addFutureObservation(indexDate = "t0", futureObservationType = "days", name = nm_1) |>
  addCohortIntersectDays(indexDate = "t0", targetCohortTable = "acute_mi_second", window = c(-Inf,Inf)) |>
  rename(second_event = acute_mi_minf_to_inf) |>
  select("cohort_name", "subject_id", "age_group", "sex", "ses", "start_drug", "start_discontinuation", "days_to_death", "future_observation", "second_event") |>
  collect() |>
  mutate(
    days_to_death = coalesce(days_to_death, 9999L),
    second_event = coalesce(second_event, 9999L),
    future_observation = pmin(days_to_death, future_observation, second_event),
    start_discontinuation = start_discontinuation + 1
  ) |>
  arrange(cohort_name, subject_id, start_drug) |>
  filter(second_event > 0)

  omopgenerics::dropSourceTable(cdm = cdm, name = nm_1)
} else {
  cli::cli_alert_info("Insufficient cohort counts for MI treatment - skipping multistate model")
}

if(length(stroke_drugs_count) > 0){
nm_2 <- omopgenerics::uniqueTableName()
xd_2 <- cdm$stroke_drugs_msm |>
  addCohortName() |>
  group_by(cohort_name, subject_id, age_group, sex, ses) |>
  mutate(t0 = min(cohort_start_date, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    start_discontinuation = date_count_between(t0, cohort_end_date),
    start_drug = date_count_between(t0, cohort_start_date)
  ) |>
  compute(name = nm_2) |>
  addDeathDays(indexDate = "t0", name = nm_2) |>
  addFutureObservation(indexDate = "t0", futureObservationType = "days", name = nm_2) |>
  addCohortIntersectDays(indexDate = "t0", targetCohortTable = "stroke_second", window = c(-Inf,Inf)) |>
  rename(second_event = ischemic_stroke_minf_to_inf) |>
  select("cohort_name", "subject_id", "age_group", "sex", "ses", "start_drug", "start_discontinuation", "days_to_death", "future_observation", "second_event") |>
  collect() |>
  mutate(
    days_to_death = coalesce(days_to_death, 9999L),
    second_event = coalesce(second_event, 9999L),
    future_observation = pmin(days_to_death, future_observation, second_event),
    start_discontinuation = start_discontinuation + 1
  ) |>
  arrange(cohort_name, subject_id, start_drug) |>
  filter(second_event > 0)

  omopgenerics::dropSourceTable(cdm = cdm, name = nm_2)
} else {
  cli::cli_alert_info("Insufficient cohort counts for stroke treatment - skipping multistate model")
}

if(length(mi_drugs_count) > 0 & length(stroke_drugs_count) > 0) {
  xd <- bind_rows(xd_1,xd_2)
} else if(length(mi_drugs_count) > 0 & length(stroke_drugs_count) == 0) {
  xd <- xd_1
} else if(length(mi_drugs_count) == 0 & length(stroke_drugs_count) > 0) {
  xd <- xd_2
}

# transitions
tmat <- matrix(NA, 3, 3)
tmat[1, 2] <- 1
tmat[2, 1] <- 2
tmat[1, 3] <- 3
tmat[2, 3] <- 4
states <- c("treated", "untreated", "death")
dimnames(tmat) <- list(from = states, to = states)

# prepare transitions from treated
transitionsTreated <- xd |>
  filter(start_drug < future_observation) |>
  mutate(
    transition = case_when(
      days_to_death <= future_observation & days_to_death <= start_discontinuation ~ "death",
      future_observation <= start_discontinuation ~ "censor",
      .default = "discontinue"
    ),
    Tstart = start_drug, 
    Tstop = case_when(
      transition == "death" ~ days_to_death,
      transition == "censor" ~ future_observation,
      transition == "discontinue" ~ start_discontinuation
    )
  ) |>
  select("cohort_name", "subject_id", "age_group", "sex", "ses",  "Tstart", "Tstop", "transition")

# prepare transitions from untreated
transitionsUntreated <- xd |>
  filter(start_discontinuation < future_observation) |>
  group_by(cohort_name, subject_id) |>
  mutate(start_drug = coalesce(lead(start_drug), 9999L)) |>
  ungroup() |>
  mutate(
    transition = case_when(
      days_to_death <= future_observation & days_to_death <= start_drug ~ "death",
      future_observation <= start_drug ~ "censor",
      .default = "restart"
    ),
    Tstart = start_discontinuation, 
    Tstop = case_when(
      transition == "death" ~ days_to_death,
      transition == "censor" ~ future_observation,
      transition == "restart" ~ start_drug
    )
  ) |>
  select("cohort_name", "subject_id", "age_group", "sex", "ses", "Tstart", "Tstop", "transition")

# treated to untreated
x <- transitionsTreated |>
  mutate(
    from = 1L, 
    to = 2L, 
    trans = 1L,
    status = if_else(transition == "discontinue", 1, 0)
  ) |>
  select("cohort_name", "subject_id", "age_group", "sex", "ses", "from", "to", "trans", "Tstart", "Tstop", "status") |>
  # treated to death
  union_all(
    transitionsTreated |>
      mutate(
        from = 1L, 
        to = 3L, 
        trans = 3L,
        status = if_else(transition == "death", 1, 0)
      ) |>
      select("cohort_name", "subject_id","age_group", "sex", "ses", "from", "to", "trans", "Tstart", "Tstop", "status")
  ) |>
  # untreated to treated
  union_all(
    transitionsUntreated |>
      mutate(
        from = 2L, 
        to = 1L, 
        trans = 2L,
        status = if_else(transition == "restart", 1, 0)
      ) |>
      select("cohort_name", "subject_id","age_group", "sex", "ses", "from", "to", "trans", "Tstart", "Tstop", "status")
  ) |>
  # untreated to death
  union_all(
    transitionsUntreated |>
      mutate(
        from = 2L, 
        to = 3L, 
        trans = 4L,
        status = if_else(transition == "death", 1, 0)
      ) |>
      select("cohort_name", "subject_id","age_group", "sex", "ses", "from", "to", "trans", "Tstart", "Tstop", "status")
  )

cohorts <- unique(x$cohort_name)

msm_results <- list()

for (coh in cohorts) {
  msdata <- x |>
    filter(cohort_name == coh) |>
    mutate(
      sex = relevel(factor(sex), ref = "Female"),
      age_group = relevel(factor(age_group), ref = "50 to 59"),
      ses = relevel(factor(ses), ref = "5")
    )
  
  cli::cli_inform(c(i = "Fitting MS model for {.pkg {coh}}"))
  
  # fit probabilities over time (unadjusted model)
  cox_mod <- coxph(
    Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
    data = msdata
  )
  
  msf <- msfit(cox_mod, trans = tmat) 
  pt_list <- probtrans(msf, predt = 0)
  
  xp <- pt_list[[1]] |>
    as_tibble() |>
    select(time, pstate1, pstate2, pstate3) |>
    pivot_longer(starts_with("pstate"), names_to = "state", values_to = "probability") |>
    mutate(state = recode(state, pstate1 = "Treated", pstate2 = "Discontinued", pstate3 = "Death"))|>
    arrange(time, state) |>
    mutate(cohort_name = coh,
           result_type = "mms_probabilities") |>
    filter(time <= 1830)
  
  sum_xp <- omopgenerics::transformToSummarisedResult(
    x = xp,
    group = c("cohort_name"),
    estimates = c("probability"),
    additional = c("time", "state"),
    settings = c("result_type")
  ) |>
    mutate(cdm_name = omopgenerics::cdmName(cdm))
  
  
  msm_results[[paste0("msm_prob_",coh)]] <- sum_xp
  
  # fit adjusted model
  
  sex_count <- length(unique(msdata$sex))
  ses_count <- length(unique(msdata$ses))
  age_group_count <- length(unique(msdata$age_group))
  
  if (sex_count < 2 & ses_count < 2 & age_group_count < 2) {
    cli::cli_alert_info("Insufficient levels in strata for cohort {.pkg {coh}}. Skipping multistate model.")
  } else {
    
    form <- "Surv(Tstart, Tstop, status) ~ "
    if (sex_count > 1) {
      form <- paste0(form, "sex:strata(trans) + ")
    }
    if (ses_count > 1) {
      form <- paste0(form, "ses:strata(trans) + ")
    }
    if (age_group_count > 1) {
      form <- paste0(form, "age_group:strata(trans) + ")
    }
    form <- paste0(form, "strata(trans) + cluster(subject_id)") |>
      as.formula()
    
    # fit adjusted model
    res_coef <- tryCatch({
      cox_mod <- coxph(formula = form, data = msdata)
      
      # extract coefficients
      tidy(cox_mod) |>
        select("variable_level" = "term", "coef" = "estimate", "se" = "std.error", "se_robust" = "robust.se") |>
        mutate(
          cohort_name = coh,
          cdm_name = omopgenerics::cdmName(cdm),
          variable_name = "Cox regression coefficients",
          result_type = "cox_coefficients"
        ) |>
        omopgenerics::transformToSummarisedResult(
          group = c("cohort_name"),
          estimates = c("coef", "se", "se_robust"),
          settings = c("result_type")
        )
    }, error = function(e) {
      cli::cli_inform(c("x" = "Model failed to fit: {.var {e}}"))
      NULL
    })
    
    msm_results[[paste0("msm_coef_",coh)]] <- res_coef
    
  }
}

all_msm_results <- msm_results |>
  purrr::compact() |>
  omopgenerics::bind() |>
  omopgenerics::newSummarisedResult()

results[["msm"]] <- all_msm_results
