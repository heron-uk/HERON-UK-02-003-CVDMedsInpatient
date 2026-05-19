
# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_observation_period = list(result_type = "summarise_observation_period", observation_period_ordinal = "all"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_demographics = list(result_type = "summarise_characteristics", index_condition = "overall", variable_name = c("Number subjects", "Cohort start date", "Socio-economic status", "Ethnicity", "Age", "Age group", "Sex", "MI type", "Prior comorbidities (-inf to 0]", "Prior mi/stroke (-inf to -1]")),
  summarise_death = list(result_type = "summarise_characteristics", variable_name = c("28-day mortality")),
  summarise_treatments = list(result_type = "summarise_characteristics", variable_name = c("Drugs [-7, 28]", "Drugs [0, 28]")),
  summarise_procedures = list(result_type = "summarise_characteristics", variable_name = "Procedures [-7, 28]"),
  summarise_drug_initiators = list(result_type = "summarise_characteristics", cohort_name = "overall", variable_name = c("Number subjects", "Socio-economic status", "Ethnicity", "Age", "Age group", "Sex")),
  drug_initiate = list(result_type = "drug_initiate")
)

source(file.path(getwd(), "functions.R"))

result <- file.path(getwd(), "rawData") |>
  list.files(pattern = "\\.csv$", full.names = TRUE) |>
  purrr::map(\(x) {
    readr::read_csv(file = x, na = "", show_col_types = FALSE) |>
      omopgenerics::newSummarisedResult()
  }) |>
  omopgenerics::bind()

cohorts <- c("stroke", "stroke_no_valve", "stroke_with_af", "acute_mi", "acute_mi_stemi", "acute_mi_not_stemi")

resultChar <- result |>
  omopgenerics::filterSettings(result_type == "summarise_characteristics") |>
  omopgenerics::splitGroup() |>
  omopgenerics::splitStrata() |>
  dplyr::mutate(
    drug = dplyr::if_else(
      cohort_name %in% cohorts,
      NA_character_,
      dplyr::case_when(
        stringr::str_ends(.data$cohort_name, "stroke") ~ stringr::str_replace(.data$cohort_name, "_stroke", ""),
        stringr::str_ends(.data$cohort_name, "stroke_no_valve") ~ stringr::str_replace(.data$cohort_name, "_stroke_no_valve", ""),
        stringr::str_ends(.data$cohort_name, "stroke_with_af") ~ stringr::str_replace(.data$cohort_name, "_stroke_with_af", ""),
        stringr::str_ends(.data$cohort_name, "acute_mi") ~ stringr::str_replace(.data$cohort_name, "_acute_mi", ""),
        stringr::str_ends(.data$cohort_name, "acute_mi_stemi") ~ stringr::str_replace(.data$cohort_name, "_acute_mi_stemi", ""),
        stringr::str_ends(.data$cohort_name, "acute_mi_not_stemi") ~ stringr::str_replace(.data$cohort_name, "_acute_mi_not_stemi", "")
      )
    ),
    index_condition = dplyr::if_else(
      cohort_name %in% cohorts,
      NA_character_,
      dplyr::case_when(
        stringr::str_ends(.data$cohort_name, "stroke") ~ "stroke",
        stringr::str_ends(.data$cohort_name, "stroke_no_valve") ~ "stroke_no_valve",
        stringr::str_ends(.data$cohort_name, "stroke_with_af") ~ "stroke_with_af",
        stringr::str_ends(.data$cohort_name, "acute_mi") ~ "acute_mi",
        stringr::str_ends(.data$cohort_name, "acute_mi_stemi") ~ "acute_mi_stemi",
        stringr::str_ends(.data$cohort_name, "acute_mi_not_stemi") ~ "acute_mi_not_stemi"
      )
    ),
    cohort_name = dplyr::if_else(
      cohort_name %in% cohorts,
      cohort_name,
      NA_character_
    ),
    ses = dplyr::case_when(
      .data$ses == "1" ~ "Q1 (Least deprived)",
      .data$ses == "2" ~ "Q2",
      .data$ses == "3" ~ "Q3",
      .data$ses == "4" ~ "Q4",
      .data$ses == "5" ~ "Q5 (Most deprived)",
      .data$ses == "NA" ~ "Missing",
      .default = .data$ses
    ),
    strata = dplyr::case_when(
      .data$age_range != "overall" ~ paste0("Age group: ", age_range),
      .data$sex != "overall" ~ paste0("Sex: ", sex),
      .data$ses != "overall" ~ paste0("SES: ", ses),
      .data$ethnicity != "overall" ~ paste0("Ethnicity: ", ethnicity),
      .default = "overall"
    ),
    variable_level = dplyr::case_when(
      .data$variable_level == "1" ~ "Q1 (Least deprived)",
      .data$variable_level == "2" ~ "Q2",
      .data$variable_level == "3" ~ "Q3",
      .data$variable_level == "4" ~ "Q4",
      .data$variable_level == "5" ~ "Q5 (Most deprived)",
      .data$variable_level == "NA" & .data$variable_name == "Ses" ~ "Missing",
      .data$variable_level == "NA" ~ NA_character_,
      .default = .data$variable_level
    ),
    variable_name = dplyr::case_when(
      .data$variable_name == "Ses" ~ "Socio-economic status",
      .data$variable_name == "Mi type" ~ "MI type",
      .default = .data$variable_name
    )
  ) |>
  dplyr::filter(
    variable_name != "Procedures [-7, 28]" |
      (startsWith(cohort_name, "stroke") & variable_level %in% c("Thromboendarterectomy", "Stroke rx procedures")) |
      (startsWith(cohort_name, "acute_mi") & variable_level %in% c("Percutaneous coronary intervention", "Coronary artery bypass graft"))
  ) |>
  omopgenerics::uniteStrata("strata") |>
  omopgenerics::uniteGroup(c("cohort_name", "index_condition", "drug")) |>
  dplyr::select(!c("age_range", "sex", "ses", "ethnicity")) |>
  dplyr::filter(
    .data$variable_name != "MI type" |
      group_level %in% cohorts
  )

init <- result |>
  omopgenerics::filterSettings(result_type == "drug_initiate") |>
  omopgenerics::tidy() |>
  dplyr::filter(index_condition != "acute_mi" | variable_name != "model6") |>
  dplyr::mutate(
    model = dplyr::case_when(
      variable_name == "model1" ~ "Age model",
      variable_name == "model2" ~ "Age+Sex model",
      variable_name == "model3" ~ "SES + Age + Sex model",
      variable_name == "model4" ~ "Eth + Age + Sex model",
      variable_name == "model5" ~ "MI type + Age + Sex model",
      variable_name %in% c("model6", "model7") ~ "SES + Eth + (MI type) + Age + Sex model"
    ),
    model_type = dplyr::if_else(
      variable_name %in% c("model6", "model7"), "Combined model", "Individual model"
    ),
    variable_name = dplyr::case_when(
      stringr::str_starts(variable_level, "age_group") ~ "Age group",
      stringr::str_starts(variable_level, "sex") ~ "Sex",
      stringr::str_starts(variable_level, "ses") ~ "Socio-economic Status",
      stringr::str_starts(variable_level, "ethnicity") ~ "Ethnicity",
      stringr::str_starts(variable_level, "mi_type") ~ "MI type",
    ),
    variable_level = stringr::str_replace(.data$variable_level, "age_group|sex|ses|ethnicity|mi_type", ""),
    rr = exp(.data$coef),
    rr_lower = exp(.data$coef - 1.96 * .data$se_coef),
    rr_upper = exp(.data$coef + 1.96 * .data$se_coef),
    dplyr::across(dplyr::starts_with("rr"), \(x) dplyr::if_else(x>100, 100, x)),
    result_type = "drug_initiate"
  ) |>
  dplyr::filter(
    model %in% c("Age model", "SES + Eth + (MI type) + Age + Sex model") |
      (model == "Age+Sex model" & variable_name != "Age group") |
      !variable_name %in% c("Sex", "Age group")
  ) |>
  omopgenerics::transformToSummarisedResult(
    group = "index_condition",
    strata = "drug",
    additional = c("model", "model_type"),
    estimates = c("rr", "rr_lower", "rr_upper"),
    settings = "result_type"
  )

result <- result |>
  omopgenerics::filterSettings(
    !result_type %in% c("summarise_characteristics", "drug_initiate")
  ) |>
  omopgenerics::bind(resultChar, init)

data <- prepareResult(result, resultList)

values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

selected$summarise_demographics_strata <- "overall"
selected$summarise_death_strata <- "overall"
selected$summarise_treatments_strata <- "overall"
selected$summarise_procedures_strata <- "overall"

# prepare data radial plots
count1 <- data$summarise_drug_initiators |>
  dplyr::filter(
    .data$variable_name %in% c("Socio-economic status", "Ethnicity", "Age group", "Sex"),
    estimate_name == "count"
  ) |>
  omopgenerics::tidy() |>
  dplyr::select("cdm_name", "index_condition", "drug", "variable_name", "variable_level", count1 = "count")
den1 <- data$summarise_drug_initiators |>
  dplyr::filter(
    .data$variable_name == "Number subjects",
    estimate_name == "count"
  ) |>
  omopgenerics::tidy() |>
  dplyr::select("cdm_name", "index_condition", "drug", den1 = "count")
count2 <- data$summarise_demographics |>
  dplyr::filter(
    .data$variable_name %in% c("Socio-economic status", "Ethnicity", "Age group", "Sex"),
    estimate_name == "count"
  ) |>
  omopgenerics::tidy() |>
  dplyr::filter(strata == "overall") |>
  dplyr::select("cdm_name", "index_condition" = "cohort_name", "variable_name", "variable_level", count2 = "count")
den2 <- data$summarise_demographics |>
  dplyr::filter(
    .data$variable_name == "Number subjects",
    estimate_name == "count"
  ) |>
  omopgenerics::tidy() |>
  dplyr::filter(strata == "overall") |>
  dplyr::select("cdm_name", "index_condition" = "cohort_name", den2 = "count")
data$radial <- count1 |>
  dplyr::full_join(den1, by = c("cdm_name", "index_condition", "drug")) |>
  dplyr::full_join(count2, by = c("cdm_name", "index_condition", "variable_name", "variable_level")) |>
  dplyr::full_join(den2, by = c("cdm_name", "index_condition")) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.numeric)) |>
  dplyr::mutate(
    rr = (count1 / den1) / (count2 / den2),
    rr_lower = exp(log(rr) - 1.96 * sqrt(1/count1 + 1/count2 - 1/den1 - 1/den2)),
    rr_upper = exp(log(rr) + 1.96 * sqrt(1/count1 + 1/count2 - 1/den1 - 1/den2)),
    dplyr::across(dplyr::starts_with("rr"), \(x) dplyr::if_else(x>100, 100, x))
  )
id <- data$radial |>
  dplyr::distinct(.data$variable_name, .data$variable_level) |>
  dplyr::arrange(.data$variable_name, .data$variable_level) |>
  dplyr::mutate(y = dplyr::row_number())
data$radial <- data$radial |>
  dplyr::left_join(id, by = c("variable_name", "variable_level"))

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
