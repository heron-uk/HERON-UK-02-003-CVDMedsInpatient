# prepare ses
n_imd <- cdm$observation |> 
  dplyr::filter(.data$observation_source_concept_id == 35812882L) |> 
  dplyr::tally() |>
  dplyr::pull()
n_townsend <- cdm$measurement |> 
  dplyr::filter(.data$measurement_concept_id == 715996L) |> 
  dplyr::tally() |>
  dplyr::pull() 

if (n_imd > 0) {
  
  logMessage("Extracting SES from IMD")
  cdm$ses_table <- cdm$observation |> 
    dplyr::filter(.data$observation_source_concept_id == 35812882L) |>
    dplyr::select("person_id", "observation_date", "ses" = "value_as_number") |>
    dplyr::compute(name = "ses_table") |>
    dplyr::filter(!is.na(.data$ses)) |>
    dplyr::compute(name = "ses_table") |>
    dplyr::group_by(.data$person_id) |>
    dplyr::filter(.data$observation_date == max(.data$observation_date, na.rm = TRUE)) |>
    dplyr::select("person_id", "ses") |>
    dplyr::distinct() |>
    dplyr::compute(name = "ses_table")
  
} else if (n_townsend > 0) {
  
  logMessage("Extracting SES from townsend")
  cdm$ses_table <- cdm$measurement |> 
    dplyr::filter(.data$measurement_concept_id == 715996L) |>
    dplyr::select("person_id", "measurement_date", "ses" = "value_as_number") |>
    dplyr::compute(name = "ses_table") |>
    dplyr::filter(!is.na(.data$ses)) |>
    dplyr::compute(name = "ses_table") |>
    dplyr::group_by(.data$person_id) |>
    dplyr::filter(.data$measurement_date == max(.data$measurement_date, na.rm = TRUE)) |>
    dplyr::select("person_id", "ses") |>
    dplyr::mutate(ses = dplyr::case_when(
      .data$ses %in% c(1, 2)  ~ 1L,
      .data$ses %in% c(3, 4)  ~ 2L,
      .data$ses %in% c(5, 6)  ~ 3L,
      .data$ses %in% c(7, 8)  ~ 4L,
      .data$ses %in% c(9, 10) ~ 5L,
      TRUE ~ NA_real_
    )) |>
    dplyr::distinct() |>
    dplyr::compute(name = "ses_table")
  
} else {
  
  logMessage("No SES found")
  cdm$ses_table <- cdm$person |>
    dplyr::select("person_id") |>
    dplyr::mutate(ses = "Missing") |>
    dplyr::compute(name = "ses_table")
}

# search duplicated
exclude <- cdm$ses_table |>
  dplyr::group_by(.data$person_id) |>
  dplyr::filter(dplyr::n() > 1) |>
  dplyr::ungroup() |>
  dplyr::select("person_id") |>
  dplyr::compute()
n <- exclude |>
  dplyr::tally() |>
  dplyr::pull()
if (n > 0) {
  logMessage(paste0(n, " persons have duplicated SES records and have been assigned to Missing"))
  cdm$ses_table <- cdm$ses_table |>
    dplyr::anti_join(exclude, by = "person_id") |>
    dplyr::compute(name = "ses_table")
}

# add to person
cdm$ses_table <- cdm$ses_table |>
  dplyr::left_join(
    cdm$person |>
      dplyr::select("person_id"), 
    by = "person_id"
  ) |>
  dplyr::mutate(ses = dplyr::coalesce(as.character(ses), "Missing")) |>
  dplyr::rename("subject_id" = "person_id") |>
  dplyr::compute(name = "ses_table")

## Add socio-economic status
addSES <- function(cohort) {
  cdm <- omopgenerics::cdmReference(cohort)
  cohort |>
    dplyr::left_join(cdm$ses_table, by = "subject_id") |>
    dplyr::compute(name = omopgenerics::tableName(cohort))
}

# prepare ethnicity
logMessage("prepare ethnicity table")
ethConcepts <- tibble(
  race_source_concept_id = c(
    700385L, 700386L, 700390L, 700369L, 700364L, 700362L, 700363L, 700367L,
    700366L, 700391L, 700389L, 700388L, 700387L, 700365L, 700368L
  ),
  ethnicity_concept = c(
    "White", "White", "Mix", "Other", "Asian", "Asian", "Asian", "Black", 
    "Black", "Mix", "Mix", "Mix", "White", "Asian", "Black"
  )
)
cdm <- insertTable(cdm = cdm, name = "eth_con", table = ethConcepts)
ethValue <- read_csv(here::here("Analyses", "ethnicity.csv"), show_col_types = FALSE) |>
  mutate(
    race_source_value = tolower(.data$variable_level),
    ethnicity_value = .data$broad
  ) |>
  select("race_source_value", "ethnicity_value") |>
  distinct()
cdm <- insertTable(cdm = cdm, name = "eth_val", table = ethValue)
cdm$ethnicity_table <- cdm$person |>
  select("person_id", "race_source_concept_id", "race_source_value") |>
  mutate(race_source_value = tolower(.data$race_source_value)) |>
  left_join(cdm$eth_val, by = "race_source_value") |>
  left_join(cdm$eth_con, by = "race_source_concept_id") |>
  compute(name = "ethnicity_table") |>
  mutate(ethnicity = coalesce(
    .data$ethnicity_concept, .data$ethnicity_value, "Unknown"
  )) |>
  select(subject_id = "person_id", "ethnicity") |>
  compute(name = "ethnicity_table")

omopgenerics::dropSourceTable(cdm = cdm, name = c("eth_val", "eth_con"))

## Add ethnicity
addEthnicity <- function(cohort) {
  cdm <- omopgenerics::cdmReference(cohort)
  cohort |>
    dplyr::left_join(cdm$ethnicity_table, by = "subject_id") |>
    dplyr::compute(name = omopgenerics::tableName(cohort))
}

# summarise values
results[["summary"]] <- cdm$person |>
  group_by(variable_level = race_source_concept_id) |>
  tally(name = "count") |>
  collect() |>
  mutate(
    variable_name = "race_source_concept_id",
    variable_level = as.character(variable_level)
  ) |>
  union_all(
    cdm$person |>
      group_by(variable_level = race_source_value) |>
      tally(name = "count") |>
      collect() |>
      mutate(
        variable_name = "race_source_value",
        variable_level = as.character(variable_level)
      )
  ) |>
  bind_rows(
    cdm$ethnicity_table |>
      group_by(variable_level = ethnicity) |>
      tally(name = "count") |>
      collect() |>
      mutate(variable_name = "ethnicity")
  ) |>
  bind_rows(
    cdm$ses_table |>
      group_by(variable_level = ses) |>
      tally(name = "count") |>
      collect() |>
      mutate(variable_name = "ses")
  ) |>
  mutate(
    result_type = "summary",
    cdm_name = cdmName(cdm)
  ) |>
  transformToSummarisedResult(
    estimates = c("count"),
    settings = "result_type"
  )
