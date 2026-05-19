# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
# install.packages("renv")
# renv::activate()
renv::restore()

library(CDMConnector)
library(DBI)
library(readr)
library(OmopSketch)
library(dplyr)
library(here)
library(tidyr)
library(CodelistGenerator)
library(CohortConstructor)
library(CohortCharacteristics)
library(omopgenerics)
library(stringr)
library(RPostgres)
library(odbc)
library(PatientProfiles)
library(clock)
library(OmopConstructor)
library(purrr)
library(broom)

#database metadata and connection details
#The name/ acronym for the database

dbName <- "..."

# Database connection details
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html
# for more details.
# you may need to install another package for this
# eg for postgres

con <- dbConnect("...")

# Set database details -----

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- "..."

# The name of the schema where results tables will be created
writeSchema <- "..."

# Table prefix -----
# any tables created in the database during the analysis will start with this prefix
writePrefix <- "..."

# create cdm reference -----
cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  cdmName = dbName,
  writePrefix = writePrefix
)

min_cell_count <- 5

# Run the study
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your output folder to share
cli::cli_alert_success("Study finished")