
# install packages if not installed
library(CDMConnector)
library(dplyr)
library(DBI)
library(readr)

con <- dbConnect("...")

cdmName <- "..."
cdmSchema <- "..."
writeSchema <- "..."
writePrefix <- "..."

cdm <- cdmFromCon(
  con = con,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  writePrefix = writePrefix,
  cdmName = cdmName,
  .softValidation = TRUE
)

ethnicityConcept <- cdm$person |>
  distinct(value = race_source_concept_id) |>
  collect() |>
  mutate(value = as.character(value), type = "source_ethnicity_concept")

ethnicityValue <- cdm$person |>
  distinct(value = race_source_value) |>
  collect() |>
  mutate(value = as.character(value), type = "source_ethnicity_value")

imdValue <- cdm$observation |> 
  filter(observation_source_concept_id == 35812882L) |>
  distinct(value = value_as_number) |>
  collect() |>
  mutate(value = as.character(value), type = "imd_value")

townsendValue <- cdm$measurement |> 
  filter(measurement_concept_id == 715996L) |>
  distinct(value = value_as_number) |>
  collect() |>
  mutate(value = as.character(value), type = "townsend_value")

values <- ethnicityConcept |>
  union_all(ethnicityValue) |>
  union_all(imdValue) |>
  union_all(townsendValue)

write_csv(values, "values_ses_ethnicity.csv")
