
library(CohortConstructor)
library(DrugUtilisation)
library(omopgenerics)
library(here)
library(DBI)
library(CDMConnector)
library(dplyr)
library(PatientProfiles)
library(clock)
library(survival)
library(mstate)
library(tidyr)
library(cmprsk)
library(tidycmprsk)
library(ggsurvfit)

# Create cdm object ----
dbName <- "CPRD GOLD"

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "ODBC Driver 18 for SQL Server",
                      Server = "163.1.64.198",
                      Database = "cdm_gold_202512",
                      UID = "martics",
                      PWD = "Pass1word",
                      TrustServerCertificate = "yes",
                      Port = 1433)

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- c(catalog = "public_100k", schema = "dbo")

# A prefix for all permanent tables in the database
writePrefix <- "mc_"

# The name of the schema where results tables will be created
writeSchema <- "results"

# minimum counts that can be displayed according to data governance
minCellCount <- 5

cdm <- cdmFromCon(
  con = con,
  cdmSchema = c("cdm_gold_202512", "dbo"),
  writeSchema = c("cdm_gold_202512", "results"),
  writePrefix = writePrefix,
  cdmName = dbName
)

# Create cohorts ----
codes <- importCodelist(here("Cohorts"), recursive = TRUE)

cdm$acute_mi <- conceptCohort(
  cdm = cdm,
  name = "acute_mi",
  conceptSet = codes["acute_mi"]
)

cdm$death_cohort <- deathCohort(cdm = cdm, name = "death_cohort", subsetCohort = "acute_mi")

cdm$drugs <- conceptCohort(
  cdm = cdm,
  name = "drugs",
  conceptSet = codes["beta_blockers"], 
  subsetCohort = "acute_mi"
) |>
  requireCohortIntersect(
    targetCohortTable = "acute_mi", 
    window = c(-28, 0),
    atFirst = TRUE
  ) |>
  collapseCohorts(gap = 1)

cdm$drugs_30 <- cdm$drugs |>
  collapseCohorts(gap = 30, name = "drugs_30")

cdm$drugs_first <- cdm$drugs |>
  requireIsFirstEntry(name = "drugs_first")

cdm$drugs_30_first <- cdm$drugs_30 |>
  requireIsFirstEntry(name = "drugs_30_first")

# analyses functions ----
# survival
runSurvival <- function(cohort) {
  summariseDiscontinuationAsSurvival(
    cohort = cohort,
    followUpDays = 730
  ) |>
    filter(variable_name == "Survival probability of discontinuation_of_beta_blockers (Outcome)") |>
    tidy() |>
    mutate(
      time = as.numeric(variable_level) / 365,
      estimate = 100 * estimate,
      estimate_lower = 100 * estimate_95CI_lower,
      estimate_upper = 100 * estimate_95CI_upper,
      event = "discontinuation",
      type = "survival"
    ) |>
    select(cohort_name, time, estimate, estimate_lower, estimate_upper, event, type) |>
    filter(time <= 2)
}

# competing risk
runCompeting <- function(cohort) {
  x <- cohort |>
    addDeathDays() |>
    addFutureObservationQuery() |>
    collect() |>
    mutate(
      days_to_death = coalesce(days_to_death, 9999L),
      discontinuation = as.integer(cohort_end_date - cohort_start_date) + 1,
      time = pmin(days_to_death, discontinuation, future_observation),
      status = case_when(
        time == days_to_death ~ 2,
        time == future_observation ~ 0,
        time == discontinuation ~ 1
      )
    )
  cuminc(Surv(time, as.factor(status)) ~ 1, data = x) |>
    tidy() |>
    mutate(
      time = as.numeric(time) / 365,
      estimate = 100 * (1 - estimate),
      estimate_lower = 100 * (1 - conf.low),
      estimate_upper = 100 * (1 - conf.high),
      event = if_else(outcome == "1", "discontinuation", "death"),
      type = "competing_risk",
      cohort_name = "beta_blockers"
    ) |>
    select(cohort_name, time, estimate, estimate_lower, estimate_upper, event, type) |>
    filter(time <= 2)
}

# ppc
runPpc <- function(cohort) {
  summariseProportionOfPatientsCovered(
    cohort = cohort,
    followUpDays = 730
  ) |>
    tidy() |>
    mutate(
      time = as.numeric(time) / 365,
      estimate = ppc,
      estimate_lower = ppc_lower,
      estimate_upper = ppc_upper,
      event = "discontinuation",
      type = "ppc"
    ) |>
    select(cohort_name, time, estimate, estimate_lower, estimate_upper, event, type) |>
    filter(time <= 2)
}

# multistate
runMultistate <- function(cohort) {
  nm <- uniqueTableName()
  x <- cohort |>
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
  
  pt_list[[1]] |>
    mutate(time = time / 365, pstate1 = 100 * (1-pstate1), pstate2 = 100 * (1-pstate2), pstate3 = 100 * (1-pstate3)) |>
    select(time, Treated = pstate1, Untreated = pstate2, Death = pstate3) |>
    filter(time <= 2) |>
    pivot_longer(!"time") |>
    mutate(
      estimate = value,
      estimate_lower = NA,
      estimate_upper = NA,
      event = name,
      type = "multi_state",
      cohort_name = "beta_blockers"
    ) |>
    select(cohort_name, time, estimate, estimate_lower, estimate_upper, event, type)
}

# run studies ----
results <- list()
results$survival_0 <- runSurvival(cdm$drugs_first) |>
  mutate(gap = "0")
results$survival_30 <- runSurvival(cdm$drugs_30_first) |>
  mutate(gap = "30")
results$comp_0 <- runCompeting(cdm$drugs_first) |>
  mutate(gap = "0")
results$comp_30 <- runCompeting(cdm$drugs_30_first) |>
  mutate(gap = "30")
results$ppc_0 <- runPpc(cdm$drugs) |>
  mutate(gap = "0")
results$ppc_30 <- runPpc(cdm$drugs_30) |>
  mutate(gap = "30")
results$ms_0 <- runMultistate(cdm$drugs) |>
  mutate(gap = "0")
results$ms_30 <- runMultistate(cdm$drugs_30) |>
  mutate(gap = "30")
result <- bind_rows(results)

## plots ----
library(ggplot2)

plotDisc <- function(x, title) {
  x <- x |>
    mutate(group = paste0(gap, "_", event))
  ggplot(data = x, aes(x = time, y = estimate, ymax = estimate_upper, ymin = estimate_lower, colour = gap, fill = gap, group = group)) +
    geom_step() +
    geom_ribbon(alpha = 0.5, colour = NA) +
    facet_grid(~cohort_name) +
    labs(y = title, x = "Time (years)") +
    theme_bw() +
    theme(legend.position = "top") +
    coord_cartesian(ylim = c(0, 100))
}

# survival just gap 0
p1 <- result |>
  filter(type == "survival", gap == "0") |>
  plotDisc("Survival probability (%)")

# survival gap comparison
p11 <- result |>
  filter(type == "survival") |>
  plotDisc("Survival probability (%)")

# proportion of patients covered
p2 <- result |>
  filter(type == "ppc") |>
  plotDisc("Proportion of Patients Covered (%)")

p22 <- result |>
  filter(type == "ppc") |>
  plotDisc("Proportion of Patients Covered (%)")

# competing risk
p3 <- result |>
  filter(type == "competing_risk", gap == "0") |>
  plotDisc("Survival probability (%)")

# competing risk gap comparison
p33 <- result |>
  filter(type == "competing_risk") |>
  plotDisc("Survival probability (%)")

# multi state 0
p4 <- result |>
  filter(type == "multi_state", gap == "30") |>
  ggplot(aes(x = time, y = estimate, fill = event)) +
  geom_area(colour = "black") +
  theme(legend.position = "top") +
  labs(x = "Time (years)", y = "Probability (%)", fill = "State")

# multi state 30
p44 <- result |>
  filter(type == "multi_state", gap == "30") |>
  ggplot(aes(x = time, y = estimate, fill = event)) +
  geom_area(colour = "black") +
  theme(legend.position = "top") +
  labs(x = "Time (years)", y = "Probability (%)", fill = "State")
