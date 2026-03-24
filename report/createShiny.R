
library(OmopViewer)
result <- omopgenerics::importSummarisedResult("rawData")

pd <- panelDetailsFromResult(result = result)

# demographics
pd$summarise_demographics <- getPanel("summarise_characteristics")
pd$summarise_demographics$title <- "Summarise demographics"
pd$summarise_demographics$data$variable_name <- c(
  "Number records", "Number subjects", "Cohort start date", "Cohort end date",
  "Ses", "Ethnicity", "Age", "Age group", "Sex", "Prior observation",
  "Future observation", "Days in cohort", "Days to next record",
   "Prior ischemic stroke (-30 to -1)", "Prior mi (-30 to -1)"
)

# treatments
pd$summarise_treatments <- getPanel("summarise_characteristics")
pd$summarise_treatments$title <- "Summarise treatments"
pd$summarise_treatments$data$variable_name <- c("Drug treatment (0, 14)")

# procedures
pd$summarise_procedures <- getPanel("summarise_characteristics")
pd$summarise_procedures$title <- "Summarise procedures"
pd$summarise_procedures$data$variable_name <- c("Procedures (0, 14)")

pd$summarise_characteristics <- NULL

exportStaticApp(
  result = result,
  directory = here::here(),
  logo = NULL,
  background = TRUE,
  summary = FALSE,
  updateButtons = FALSE,
  panelDetails = pd,
  report = TRUE,
  panelStructure = list(
    "Diagnostics" = c(
      "summarise_omop_snapshot", "summarise_observation_period",
      "cohort_code_use"
    ),
    "Characteristics" = c(
      "summarise_cohort_count", "summarise_cohort_attrition",
      "summarise_demographics", "summarise_treatments", "summarise_procedures"
    )
  )
)

