## Add socio-economic status
addSES <- function(cohort) {
  
  cdm <- omopgenerics::cdmReference(cohort)
  
  n_imd <- cdm$observation |> 
    dplyr::filter(.data$observation_source_concept_id == 35812882L) |> 
    dplyr::tally() |>
    dplyr::pull()
  n_townsend <- cdm$measurement |> 
    dplyr::filter(.data$measurement_concept_id == 715996L) |> 
    dplyr::tally() |>
    dplyr::pull() 
  
  nm <- omopgenerics::uniqueTableName()
  
  if (n_imd > 0) {
    
    logMessage("Extracting SES from IMD")
    sesRecords <- cdm$observation |> 
      dplyr::filter(.data$observation_source_concept_id == 35812882L) |>
      dplyr::select("subject_id" = "person_id", "observation_date", "ses" = "value_as_number") |>
      dplyr::compute(name = nm) |>
      dplyr::group_by(.data$person_id) |>
      dplyr::filter(.data$observation_date == max(.data$observation_date, na.rm = TRUE)) |>
      dplyr::select("person_id", "ses") |>
      dplyr::distinct() |>
      dplyr::compute(name = nm) |>
      dplyr::inner_join(
        cohort |>
          dplyr::distinct(.data$subject_id),
        by = "subject_id"
      ) |>
      dplyr::compute(name = nm)
    
  } else if (n_townsend > 0) {
    
    logMessage("Extracting SES from townsend")
    sesRecords <- cdm$measurement |> 
      dplyr::filter(.data$measurement_concept_id == 715996L) |>
      dplyr::select(
        "subject_id" = "person_id", 
        "townsend_date" = "measurement_date",
        "ses" = "value_as_number"
      ) |>
      dplyr::compute(name = nm) |>
      dplyr::group_by(.data$subject_id) |>
      dplyr::filter(.data$townsend_date == max(.data$townsend_date, na.rm = TRUE)) |>
      dplyr::select("subject_id", "ses") |>
      dplyr::mutate(ses = dplyr::case_when(
        .data$ses %in% c(1, 2)  ~ 1L,
        .data$ses %in% c(3, 4)  ~ 2L,
        .data$ses %in% c(5, 6)  ~ 3L,
        .data$ses %in% c(7, 8)  ~ 4L,
        .data$ses %in% c(9, 10) ~ 5L,
        TRUE ~ NA_real_
      )) |>
      dplyr::distinct() |>
      dplyr::compute(name = nm) |>
      dplyr::inner_join(
        cohort |>
          dplyr::distinct(.data$subject_id),
        by = "subject_id"
      ) |>
      dplyr::compute(name = nm)
    
  } else {
    
    logMessage("No SES found")
    sesRecords <- cdm$person |>
      dplyr::select("subject_id" = "person_id") |>
      dplyr::mutate(ses = "Missing") |>
      dplyr::compute(name = nm)
  }
  
  # search duplicated
  exclude <- sesRecords |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup() |>
    dplyr::select("subject_id")
  n <- exclude |>
    dplyr::tally() |>
    dplyr::pull()
  
  if (n > 0) {
    logMessage(paste0(n, " persons have duplicated SES records and have been assigned to Missing"))
    sesRecords <- sesRecords |>
      dplyr::anti_join(exclude, by = "subject_id") |>
      dplyr::compute(name = nm)
  }
  
  cohort <- cohort |>
    dplyr::left_join(sesRecords, by = "subject_id") |>
    dplyr::mutate(ses = dplyr::coalesce(as.character(ses), "Missing")) |>
    dplyr::compute(name = omopgenerics::tableName(cohort))
  
  omopgenerics::dropSourceTable(cdm = cdm, name = nm)
  
  return(cohort)
}

## Add ethnicity
addEthnicity <- function(cohort) {
  
  cdm <- omopgenerics::cdmReference(cohort)
  
  # check if they have the race_source_concept_id mapped
  nConcepts <- cdm$person |>
    dplyr::distinct(.data$race_source_concept_id) |>
    dplyr::pull() |>
    purrr::keep(\(x) !is.na(x)) |>
    purrr::keep(\(x) x != 0) |>
    length()
  
  if (nConcepts > 0) {
    logMessage("Adding the ehnicity from race_source_concept_id")
    eth <- cdm$person |>
      dplyr::select(subject_id = "person_id", concept_id = "race_source_concept_id") |>
      dplyr::inner_join(
        cdm$concept |>
          dplyr::select("concept_id", ethnicity = "concept_name"),
        by = "concept_id"
      ) |>
      dplyr::select("subject_id", "ethnicity")
  } else {
    logMessage("Adding the ehnicity from race_source_value")
    eth <- cdm$person |>
      dplyr::select(subject_id = "person_id", ethnicity = "race_source_value")
  }
  
  cohort |>
    dplyr::left_join(eth, by = "subject_id") |>
    dplyr::mutate(ethnicity = dplyr::coalesce(as.character(ethnicity), "Unknown")) |>
    dplyr::compute(name = omopgenerics::tableName(cohort))
}
