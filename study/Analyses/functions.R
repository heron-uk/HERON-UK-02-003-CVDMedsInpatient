## Add socio-economic status
addSES <- function(cohort){
  n_imd <- cdm$observation |> dplyr::filter(.data$observation_source_concept_id == 35812882L) |> dplyr::tally() |> dplyr::pull()
  n_townsend <- cdm$measurement |> dplyr::filter(.data$measurement_concept_id == 715996L) |> dplyr::tally() |> dplyr::pull() 
  if (n_imd > 0){
    cohort |> dplyr::left_join(cdm$observation |> 
                                 dplyr::filter(.data$observation_source_concept_id == 35812882L) |> 
                                 dplyr::select("person_id", "ses" = "value_as_number"),
                               by = c("subject_id" = "person_id")) |>
      dplyr::mutate(ses = as.character(.data$ses),
                    ses = coalesce(.data$ses, "Missing"))
    
  }else if (n_townsend>0) {
    cohort |> PatientProfiles::addConceptIntersectField(conceptSet = list(townsend = 715996L), 
                                                        indexDate = "cohort_start_date",
                                                        field = "value_as_number", 
                                                        window = list(c(-Inf, Inf)), 
                                                        order = "last", 
                                                        nameStyle = "ses", 
                                                        inObservation = FALSE) |>
      dplyr::mutate(
        ses = dplyr::case_when(
          ses %in% c(1, 2)  ~ 1L,
          ses %in% c(3, 4)  ~ 2L,
          ses %in% c(5, 6)  ~ 3L,
          ses %in% c(7, 8)  ~ 4L,
          ses %in% c(9, 10) ~ 5L,
          TRUE ~ NA_real_
        ),
        ses = as.character(.data$ses),
        ses = dplyr::coalesce(ses, "Missing")
      )
  }else {
    cohort
  }
}

## Add ethnicity

addEthnicity <- function(cohort) {
  cohort |> dplyr::left_join(cdm$person |> 
                               dplyr::select("person_id", "race_source_value"),
                             by = c("subject_id" = "person_id") ) |>
    dplyr::mutate(ethnicity_group = dplyr::case_when(
      .data$race_source_value %in% c("9", "10", "11", "12", "13") ~ "Asian",
      .data$race_source_value %in% c("14", "15", "16")          ~ "Black",
      .data$race_source_value %in% c("1", "2", "3", "4", "20")      ~ "White",
      .data$race_source_value %in% c("5", "6", "7", "8")          ~ "Mix",
      .data$race_source_value %in% c("17", "18")              ~ "Other",
      is.na(race_source_value)                      ~ "Unknown",
      TRUE                                          ~ "Unknown"
    ),
    ethnicity = dplyr::case_when(
      race_source_value == "1"  ~ "WHITE: British/N. Irish",
      race_source_value == "2"  ~ "WHITE: Irish",
      race_source_value == "3"  ~ "WHITE: Gypsy/Irish Traveller",
      race_source_value == "4"  ~ "WHITE: Other white",
      race_source_value == "5"  ~ "MIXED/MULTIPLE: White/Black Caribbean",
      race_source_value == "6"  ~ "MIXED/MULTIPLE: White/Black African",
      race_source_value == "7"  ~ "MIXED/MULTIPLE: White/Asian",
      race_source_value == "8"  ~ "MIXED/MULTIPLE: Other mixed/multiple",
      race_source_value == "9"  ~ "ASIAN: Indian",
      race_source_value == "10" ~ "ASIAN: Pakistani",
      race_source_value == "11" ~ "ASIAN: Bangladeshi",
      race_source_value == "12" ~ "ASIAN: Chinese",
      race_source_value == "13" ~ "ASIAN: Other Asian",
      race_source_value == "14" ~ "BLACK: African",
      race_source_value == "15" ~ "BLACK: Caribbean",
      race_source_value == "16" ~ "BLACK: Other black",
      race_source_value == "17" ~ "OTHER: Arab",
      race_source_value == "18" ~ "OTHER: Other ethnicity",
      race_source_value == "19" ~ "UNKNOWN",
      race_source_value == "20" ~ "WHITE: Roma",
      is.na(race_source_value) ~ "Unknown",
      TRUE ~ race_source_value
    )) |> 
    dplyr::select(-"race_source_value")
}

### Get CKD

addCKDStage <- function(cohort) {
  name <- tableName(cohort)
  cdm[[name]] |>
    addCohortIntersectDays(
      targetCohortTable = "ckd_stage",
      order = "last",
      window = c(-Inf, -1),
      nameStyle = "{cohort_name}",
      name = name
    ) |>
    mutate(
      ckd_stage_1 = abs(coalesce(ckd_stage_1, 999)),
      ckd_stage_2 = abs(coalesce(ckd_stage_2, 999)),
      ckd_stage_3 = abs(coalesce(ckd_stage_3, 999)),
      ckd_stage_4 = abs(coalesce(ckd_stage_4, 999)),
      ckd_stage_5 = abs(coalesce(ckd_stage_5, 999))
    ) |>
    mutate(
      ckd_stage = case_when(
        ckd_stage_1 == 999 & ckd_stage_2 == 999 & ckd_stage_3 == 999 & ckd_stage_4 == 999 & ckd_stage_5 == 999 ~ "Missing",
        ckd_stage_1 <= ckd_stage_2 & ckd_stage_1 <= ckd_stage_3 & ckd_stage_1 <= ckd_stage_4 & ckd_stage_1 <= ckd_stage_5 ~ "Stage 1",
        ckd_stage_2 <= ckd_stage_3 & ckd_stage_2 <= ckd_stage_4 & ckd_stage_2 <= ckd_stage_5 ~ "Stage 2",
        ckd_stage_3 <= ckd_stage_4 & ckd_stage_3 <= ckd_stage_5 ~ "Stage 3",
        ckd_stage_4 <= ckd_stage_5 ~ "Stage 4",
        .default = "Stage 5"
      )
    ) |>
    select(!all_of(c(paste0("ckd_stage_", 1:5)))) |>
    compute(name = name, temporary = FALSE)
}
