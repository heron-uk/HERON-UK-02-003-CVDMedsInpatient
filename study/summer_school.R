
library(CohortConstructor)
library(DrugUtilisation)
library(omopgenerics)
library(here)

codes <- importCodelist(here("Cohorts"), recursive = TRUE)

cdm$acute_mi <- conceptCohort(
  cdm = cdm,
  name = "acute_mi",
  conceptSet = codes["acute_mi_broad"]
)

cdm$death_cohort <- deathCohort(cdm = cdm, name = "death_cohort", subsetCohort = "acute_mi")

cdm$drugs <- conceptCohort(
  cdm = cdm,
  name = "drugs",
  conceptSet = codes["beta_blockers"], 
  subsetCohort = "death_cohort"
) |>
  requireCohortIntersect(
    targetCohortTable = "acute_mi", 
    window = c(-28, 0),
    atFirst = TRUE
  )

# survival
result1 <- summariseDiscontinuationAsSurvival(
  cohort = cdm$drugs,
  followUpDays = 730
)

# competing risk
result2 <- summariseDiscontinuationAsSurvival(
  cohort = cdm$drugs,
  followUpDays = 730,
  competingOutcomeCohortTable = "death_cohort"
)

# ppc
result3 <- summariseProportionOfPatientsCovered(
  cohort = cdm$drugs,
  followUpDays = 730
)

# multistate
nm <- uniqueTableName()
x <- cdm$drugs |>
  addCohortName() |>
  group_by(cohort_name, subject_id) |>
  mutate(t0 = min(cohort_start_date, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    start_discontinuation = date_count_between(t0, cohort_end_date),
    start_drug = date_count_between(t0, cohort_start_date)
  ) |>
  compute(name = nm) |>
  addDeathDays(indexDate = "t0", name = nm) |>
  addFutureObservation(indexDate = "t0", futureObservationType = "days", name = nm) |>
  addCohortIntersectDays(indexDate = "t0", targetCohortTable = "acute_mi", window = c(0, Inf), nameStyle = "second_event") |>
  select("cohort_name", "subject_id", "start_drug", "start_discontinuation", "days_to_death", "future_observation", "second_event") |>
  collect() |>
  mutate(
    days_to_death = coalesce(days_to_death, 9999L),
    second_event = coalesce(second_event, 9999L),
    future_observation = pmin(days_to_death, future_observation, second_event),
    start_discontinuation = start_discontinuation + 1
  ) |>
  arrange(cohort_name, subject_id, start_drug)

tmat <- matrix(NA, 3, 3)
tmat[1, 2] <- 1
tmat[2, 1] <- 2
tmat[1, 3] <- 3
tmat[2, 3] <- 4
states <- c("treated", "untreated", "death")
dimnames(tmat) <- list(from = states, to = states)

transitionsTreated <- x |>
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
  select("cohort_name", "subject_id", "Tstart", "Tstop", "transition")

# prepare transitions from untreated
transitionsUntreated <- x |>
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
  select("cohort_name", "subject_id", "Tstart", "Tstop", "transition")

# treated to untreated
x <- transitionsTreated |>
  mutate(
    from = 1L, 
    to = 2L, 
    trans = 1L,
    status = if_else(transition == "discontinue", 1, 0)
  ) |>
  select("cohort_name", "subject_id", "from", "to", "trans", "Tstart", "Tstop", "status") |>
  # treated to death
  union_all(
    transitionsTreated |>
      mutate(
        from = 1L, 
        to = 3L, 
        trans = 3L,
        status = if_else(transition == "death", 1, 0)
      ) |>
      select("cohort_name", "subject_id", "from", "to", "trans", "Tstart", "Tstop", "status")
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
      select("cohort_name", "subject_id", "from", "to", "trans", "Tstart", "Tstop", "status")
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
      select("cohort_name", "subject_id", "from", "to", "trans", "Tstart", "Tstop", "status")
  )

# fit probabilities over time (unadjusted model)
cox_mod <- coxph(
  Surv(Tstart, Tstop, status) ~ strata(trans) + cluster(subject_id),
  data = x
)

msf <- msfit(cox_mod, trans = tmat) 
pt_list <- probtrans(msf, predt = 0)
