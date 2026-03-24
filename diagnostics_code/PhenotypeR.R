
result <- phenotypeDiagnostics(cdm[["study_cohorts"]], 
                               clinicalTableSample = 0,
                               measurementSample = 0,
                               drugExposureSample = 0,
                               populationDateRange = as.Date(c("2012-01-01", 
                                                               NA)))

# export the results
exportSummarisedResult(result, 
                      fileName = "results_drugs_{cdm_name}_{date}.csv",
                      path = here::here("Results"), 
                      minCellCount = minCellCount)
