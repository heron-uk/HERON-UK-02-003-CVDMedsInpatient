# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
# install.packages("renv")
# renv::activate()
renv::restore()

library(CDMConnector)
library(DBI)
library(log4r)
library(readr)
library(DrugUtilisation)
library(OmopSketch)
library(dplyr)
library(here)
library(tidyr)
library(CodelistGenerator)
library(CohortConstructor)
library(CohortCharacteristics)
library(DrugExposureDiagnostics)
library(omopgenerics)
library(stringr)
library(RPostgres)
library(odbc)
library(OmopConstructor)
library(CohortSurvival)
library(PatientProfiles)
library(clock)
library(survival)
library(mstate)
library(broom)

#database metadata and connection details
#The name/ acronym for the database

db_name <- "..."

# Database connection details
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html
# for more details.
# you may need to install another package for this
# eg for postgres

db <- dbConnect("...",
                dbname = "...",
                port = "...",
                host = "...",
                user = "...",
                password = "...",
                bigint = c("integer")
)

# Set database details -----

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_schema <- "..."

# The name of the schema where results tables will be created
write_schema <- "..."

# Table prefix -----
# any tables created in the database during the analysis will start with this prefix
study_prefix <- "..."

# create cdm reference -----
cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdm_schema,
  writeSchema = write_schema,
  cdmName = db_name,
  writePrefix = study_prefix
)


# Hospital databases should set the start date as "2022-01-01". 
study_start <- "2012-01-01"

min_cell_count <- 5

hospital_care <- FALSE
primary_care <- TRUE

run_characteristics <- TRUE
run_drug_adherence <- TRUE

# Run the study
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your output folder to share
cli::cli_alert_success("Study finished")