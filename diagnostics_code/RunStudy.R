
omopgenerics::assertNumeric(minCellCount, length = 1)

# create logger ----
omopgenerics::createLogFile(logFile = here::here("Results", "log_{date}_{time}"))

# instantiate necessary cohorts ----
omopgenerics::logMessage("INSTANTIATING STUDY COHORTS")
source(here("Cohorts", "InstantiateCohorts.R"))
omopgenerics::logMessage("STUDY COHORTS INSTANTIATED")

# run diagnostics ----
omopgenerics::logMessage("RUN PHENOTYPER")
source(here("PhenotypeR.R"))
omopgenerics::logMessage("PHENOTYPER FINISHED")
