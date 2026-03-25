# pak::pkg_install("oxford-pharmacoepi/AthenaR")
# vocabPath <- file.path(omopgenerics::omopDataFolder(), "AthenaR")
# dir.create(vocabPath, showWarnings = FALSE)
# AthenaR::downloadVocabulary("v20260227", path = vocabPath)
# zip::unzip(zipfile = "/Users/martics/Documents//AthenaR/v20260227/raw.zip", exdir = file.path(vocabPath, "v20260227"))
concepts <- readr::read_delim(file.path(vocabPath, "v20260227", "raw", "CONCEPT.csv"), delim = "\t") |>
  dplyr::select("concept_id", "concept_name", "concept_code", "vocabulary_id")

# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_observation_period = list(result_type = "summarise_observation_period", observation_period_ordinal = "all"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_demographics = list(result_type = "summarise_characteristics", variable_name = c("Number records", "Number subjects", "Cohort start date", "Socio-economic status", "Ethnicity", "Ethnicity group", "Age", "Age group", "Sex", "Prior observation", "Future observation", "Mi type", "Prior comorbidities (-inf to 0)")),
  summarise_death = list(result_type = "summarise_characteristics", variable_name = c("28-day mortality")),
  summarise_treatments = list(result_type = "summarise_characteristics", variable_name = c("Drugs [0, 14]", "Drugs [-28, 28]")),
  summarise_procedures = list(result_type = "summarise_characteristics", variable_name = "Procedures [-28, 28]"),
  admit_discharge = list(result_type = "summarise_table", variable_name = c("number records", "number subjects", "admit", "discharge")),
  timings = list(result_type = "summarise_table", variable_name = c("number records", "number subjects", "Days to treatment", "Days to procedure"))
)

source(file.path(getwd(), "functions.R"))

# correct cdm_name
list.files(file.path(getwd(), "rawData"), pattern = ".csv$", full.names = TRUE) |>
  purrr::map(\(x) {
    res <- readr::read_csv(x, show_col_types = FALSE)
    cn <- unique(res$cdm_name)
    cn <- cn[!is.na(cn) & cn != "unknown"]
    res <- res |>
      dplyr::mutate(cdm_name = dplyr::if_else(
        .data$cdm_name == "unknown", .env$cn, .data$cdm_name
      )) |>
      dplyr::filter(variable_name != "settings" | .data$estimate_name != "table_name")
    # same settings
    set <- res |>
      dplyr::filter(.data$variable_name == "settings") |>
      dplyr::select("result_id", "estimate_name", "estimate_value") |>
      dplyr::distinct() |>
      tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
      dplyr::group_by(dplyr::across(!"result_id")) |>
      dplyr::mutate(new_id = dplyr::cur_group_id()) |>
      dplyr::ungroup() |>
      dplyr::select("result_id", "new_id") |>
      dplyr::distinct()
    res |>
      dplyr::left_join(set, by = "result_id") |>
      dplyr::select(!"result_id") |>
      dplyr::rename(result_id = new_id) |>
      dplyr::distinct() |>
      readr::write_csv(file = x)
  }) |>
  invisible()

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))

result <- result |>
  omopgenerics::filterGroup(cohort_name != "ischemic_stroke")

result$group_level <- stringr::str_replace_all(result$group_level, "stroke_broad", "Acute Ischemic Stroke")
result$group_level <- stringr::str_replace_all(result$group_level, "acute_mi", "Acute Myocardial Infarction")

resultChar <- result |>
  omopgenerics::filterSettings(result_type == "summarise_characteristics") |>
  dplyr::filter(!variable_name %in% c(
    "Cohort end date", "Days in cohort", "Days to next record"
  ))

resultEth <- resultChar |>
  dplyr::filter(.data$variable_name == "Ethnicity") |>
  dplyr::left_join(
    concepts |>
      dplyr::filter(.data$vocabulary_id == "NHS Ethnic Category") |>
      dplyr::select("variable_level" = "concept_code", "new" = "concept_name"),
    by = "variable_level"
  ) |>
  dplyr::mutate(variable_level = dplyr::coalesce(.data$new, .data$variable_level)) |>
  dplyr::select(!"new") |>
  dplyr::left_join(
    concepts |>
      dplyr::mutate(concept_id = sprintf("%i", concept_id)) |>
      dplyr::select("variable_level" = "concept_id", "new" = "concept_name"),
    by = "variable_level"
  ) |>
  dplyr::mutate(
    variable_level = dplyr::coalesce(.data$new, .data$variable_level)
  ) |>
  dplyr::select(!"new")

ethnicity <- readr::read_csv(file = here::here("rawData", "ethnicity", "ethnicity.csv"), show_col_types = FALSE) |>
  dplyr::mutate(correct = dplyr::coalesce(.data$correct, .data$variable_level))

resultEth <- resultEth |>
  dplyr::left_join(ethnicity, by = "variable_level")

if (any(is.na(resultEth$correct))) {
  cli::cli_abort(c(x = "New missing ethnicities"))
}

resultEth <- resultEth |>
  dplyr::select(!c("variable_level", "broad")) |>
  dplyr::rename("variable_level" = "correct") |>
  dplyr::union_all(
    resultEth |>
      dplyr::mutate(variable_name = "Ethnicity group") |>
      dplyr::select(!c("variable_level", "correct")) |>
      dplyr::rename("variable_level" = "broad")
  ) |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::select(!"estimate_type") |>
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::group_by(dplyr::across(!c("count", "percentage", "id"))) |>
  dplyr::summarise(
    count = sum(as.numeric(.data$count), na.rm = TRUE),
    percentage = sum(as.numeric(.data$percentage), na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    c("count", "percentage"),
    names_to = "estimate_name",
    values_to = "estimate_value"
  ) |>
  dplyr::mutate(
    estimate_type = dplyr::if_else(.data$estimate_name == "count", "integer", "percentage"),
    estimate_value = dplyr::if_else(
      .data$estimate_name == "count",
      sprintf("%.0f", .data$estimate_value),
      sprintf("%.2f", .data$estimate_value)
    )
  )

resultChar <- resultChar |>
  dplyr::filter(.data$variable_name != "Ethnicity") |>
  dplyr::union_all(resultEth) |>
  dplyr::mutate(
    variable_name = stringr::str_replace_all(
      string = variable_name,
      pattern = "Ses",
      replacement = "Socio-economic status"
    ),
    variable_name = stringr::str_replace_all(
      string = variable_name,
      pattern = "Mi type",
      replacement = "MI type"
    )
  ) |>
  omopgenerics::splitGroup() |>
  omopgenerics::splitStrata() |>
  dplyr::filter(mi_type %in% c("overall", "STEMI", "not STEMI")) |>
  dplyr::mutate(
    cohort_name = dplyr::if_else(
      mi_type != "overall",
      paste0(cohort_name, " (", mi_type, ")"),
      cohort_name
    ),
    strata = dplyr::case_when(
      age_range != "overall" ~ paste0("Age group: ", age_range),
      ses != "overall" ~ paste0("SES: ", ses),
      sex != "overall" ~ paste0("Sex: ", sex),
      .default = "overall"
    ),
    strata_id = dplyr::case_when(
      age_range != "overall" ~ 2L,
      ses != "overall" ~ 3L,
      sex != "overall" ~ 4L,
      .default = 1L
    ),
    variable_id = match(variable_name, c("Number records", "Number subjects", "Cohort start date", "Socio-economic status", "Ethnicity group", "Ethnicity", "Age", "Age group", "Sex", "Prior observation", "Future observation", "MI type", "Prior comorbidities (-inf to 0)"))
  ) |>
  dplyr::arrange(cdm_name, cohort_name, strata_id, strata, variable_id) |>
  dplyr::select(!c("strata_id", "sex", "age_range", "ses", "mi_type", "variable_id")) |>
  omopgenerics::uniteGroup(c("cohort_name")) |>
  omopgenerics::uniteStrata(c("strata"))

resTiming <- result |>
  omopgenerics::filterSettings(result_type == "summarise_table") |>
  dplyr::filter(variable_name %in% c(
    "number records", "number subjects",
    "thrombolytics_alteplase", "thrombolytics_tenecteplase",
    "stroke_rx_procedures", "thromboendarterectomy",
    "coronary_artery_bypass_graft", "percutaneous_coronary_intervention"
  )) |>
  omopgenerics::splitGroup() |>
  omopgenerics::splitStrata() |>
  dplyr::filter(mi_type %in% c("overall", "STEMI", "not STEMI")) |>
  dplyr::mutate(
    cohort_name = dplyr::if_else(
      mi_type != "overall",
      paste0(cohort_name, " (", mi_type, ")"),
      cohort_name
    ),
    variable_level = dplyr::if_else(
      variable_name %in% c("thrombolytics_alteplase", "thrombolytics_tenecteplase", "stroke_rx_procedures", "thromboendarterectomy", "coronary_artery_bypass_graft", "percutaneous_coronary_intervention"),
      variable_name,
      variable_level
    ),
    variable_name = dplyr::case_when(
      variable_name %in% c("thrombolytics_alteplase", "thrombolytics_tenecteplase") ~ "Days to treatment",
      variable_name %in% c("stroke_rx_procedures", "thromboendarterectomy", "coronary_artery_bypass_graft", "percutaneous_coronary_intervention") ~ "Days to procedure",
      .default = variable_name
    )
  ) |>
  omopgenerics::uniteGroup("cohort_name") |>
  omopgenerics::uniteStrata() |>
  dplyr::select(!"mi_type")

resAdDis <- result |>
  omopgenerics::filterSettings(result_type == "summarise_table") |>
  dplyr::filter(variable_name %in% c("admit", "discharge")) |>
  omopgenerics::splitGroup() |>
  omopgenerics::splitStrata() |>
  dplyr::filter(mi_type %in% c("overall", "STEMI", "not STEMI")) |>
  dplyr::mutate(
    cohort_name = dplyr::if_else(
      mi_type != "overall",
      paste0(cohort_name, " (", mi_type, ")"),
      cohort_name
    )
  ) |>
  omopgenerics::uniteGroup("cohort_name") |>
  omopgenerics::uniteStrata() |>
  dplyr::select(!"mi_type") |>
  dplyr::left_join(
    concepts |>
      dplyr::mutate(concept_id = sprintf("%i", concept_id)) |>
      dplyr::select("variable_level" = "concept_id", "new_variable_level" = "concept_name"),
    by = "variable_level"
  ) |>
  dplyr::mutate(variable_level = dplyr::coalesce(.data$new_variable_level, .data$variable_level)) |>
  dplyr::select(!"new_variable_level")

result <- omopgenerics::bind(
  result |>
    omopgenerics::filterSettings(!result_type %in% c("summarise_characteristics", "summarise_table")),
  resultChar,
  resTiming,
  resAdDis
)

data <- prepareResult(result, resultList)

values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

selected$summarise_demographics_variable_name <- selected$summarise_demographics_variable_name[selected$summarise_demographics_variable_name != "Ethnicity"]
selected$summarise_demographics_strata <- "overall"
selected$summarise_death_strata <- "overall"
selected$summarise_treatments_strata <- "overall"
selected$summarise_procedures_strata <- "overall"

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
