# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_observation_period = list(result_type = "summarise_observation_period", observation_period_ordinal = "all"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_demographics = list(result_type = "summarise_characteristics", variable_name = c("Number records", "Number subjects", "Cohort start date", "Cohort end date", "Ses", "Ethnicity", "Age", "Age group", "Sex", "Prior observation", "Future observation", "Days in cohort", "Days to next record", "Mi type", "28-day mortality", "Prior comorbidities (-inf to 0)")),
  summarise_treatments = list(result_type = "summarise_characteristics", variable_name = c("Drugs [0, 14]", "Drugs [-28, 28]")),
  summarise_procedures = list(result_type = "summarise_characteristics", variable_name = "Procedures [-28, 28]")
)

source(file.path(getwd(), "functions.R"))

# correct cdm_name
list.files(file.path(getwd(), "rawData"), pattern = ".csv$", full.names = TRUE) |>
  purrr::map(\(x) {
    res <- readr::read_csv(x, show_col_types = FALSE)
    cn <- unique(res$cdm_name)
    cn <- cn[!is.na(cn) & cn != "unknown"]
    res |>
      dplyr::mutate(cdm_name = dplyr::if_else(
        .data$cdm_name == "unknown", .env$cn, .data$cdm_name
      )) |>
      readr::write_csv(file = x)
  }) |>
  invisible()

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))

resultChar <- result |>
  omopgenerics::filterSettings(result_type == "summarise_characteristics") |>
  dplyr::filter(!variable_name %in% c(
    "Cohort end date", "Days in cohort", "Days to next record"
  )) |>
  dplyr::mutate(
    variable_name = stringr::str_replace_all(
      string = variable_name,
      pattern = "Ses",
      replacement = "SES"
    ),
    variable_name = stringr::str_replace_all(
      string = variable_name,
      pattern = "Mi type",
      replacement = "MI type"
    )
  ) |>
  omopgenerics::splitStrata() |>
  dplyr::mutate(
    strata = dplyr::case_when(
      age_range != "overall" ~ paste0("Age group: ", age_range),
      ses != "overall" ~ paste0("SES: ", ses),
      sex != "overall" ~ paste0("Sex: ", sex),
      mi_type != "overall" ~ paste0("MI type: ", mi_type),
      .default = "overall"
    ),
    strata_id = dplyr::case_when(
      age_range != "overall" ~ 2L,
      ses != "overall" ~ 3L,
      sex != "overall" ~ 4L,
      mi_type != "overall" ~ 5L,
      .default = 1L
    )
  ) |>
  dplyr::arrange(cdm_name, group_level, strata_id, strata) |>
  dplyr::select(!c("strata_id")) |>
  omopgenerics::uniteStrata(c("strata", "age_range", "ses", "sex", "mi_type"))

result <- omopgenerics::bind(
  result |>
    omopgenerics::filterSettings(result_type != "summarise_characteristics"),
  resultChar
)

result$group_level <- stringr::str_replace_all(result$group_level, "ischemic_stroke", "AIS_narrow")
result$group_level <- stringr::str_replace_all(result$group_level, "stroke_broad", "AIS_broad")
result$group_level <- stringr::str_replace_all(result$group_level, "acute_mi", "AMI")

data <- prepareResult(result, resultList)

values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
