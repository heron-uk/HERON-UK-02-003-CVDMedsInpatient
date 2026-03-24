# Drugs -----
omopgenerics::logMessage("-- creating drug cohorts")
drug_codes <- omopgenerics::importCodelist(here::here("Cohorts", "drugs"), 
                                           type = "csv")
cdm[["drugs"]] <- conceptCohort(cdm,
                                conceptSet = drug_codes,
                                table = "drug_exposure",
                                exit = "event_end_date",
                                name = "drugs") 

# Conditions (concept based) -----
omopgenerics::logMessage("-- creating condition cohorts (concept based)")
condition_codes <- omopgenerics::importCodelist(here::here("Cohorts", "conditions"), 
                                           type = "csv")
cdm[["conditions"]] <- conceptCohort(cdm,
                                conceptSet = condition_codes,
                                exit = "event_start_date",
                                name = "conditions") 

cdm[["conditions"]] <- cdm[["conditions"]] |> 
  exitAtObservationEnd()

# Obesity (based on measurement or diagnosis) -----
omopgenerics::logMessage("-- creating obesity cohort (measurement or diagnosis)")
obesity_diag <- list(obesity = c(
  604591, 4271317, 4171972,  4270189, 4079899,  4235799,
  4087487,  40481140, 36713437,  36678790,  45763687,  4097929,  4097996,  4182506,
  4100857,  4160821,  4029277,  4029276,  37166819,  4029900,  36717154,  4005991,
  4163032,  4185912,  4171147,  4177337,  4220527,  4203289,  35622038,  36674490,
  36674893,  4171317,  438731,  37208175,  37164247,  42872398,  4216214,  36716144,
  37110069,  434005,  37395980,  433736,  4212443,  4215969,  4189665,  36716555,
  36717199,  37204685,  37206117,  37397209,
  37162364,  36716151,  37204815,  37311904,  45757112,  4183240,
  4093860,  37163354, 36674827,  3199162,
  45771307,  36676689,  37204691,  37018860,  42539192,  37164244,
  4217557,  37166818,  4211019,  36714072, 36714548,  37165655
))
cdm$obesity <- conceptCohort(
  cdm = cdm, conceptSet = obesity_diag, exit = "event_start_date", name = "obesity"
)

cdm$obesity_bmi <- measurementCohort(
  cdm = cdm, 
  conceptSet = list("bmi_measurement" = c(3038553, 36304833)), 
  valueAsNumber = list("bmi_measurement" = list(c(30, 60))),   
  name = "obesity_bmi"
)
# body weight cohort
cdm$obesity_body_weight <- measurementCohort(
  cdm = cdm, conceptSet = list("body_weight"= c(3025315, 4099154, 3013762,
                                                3023166, 3027492)), 
  valueAsNumber = list("body_weight"= list("9529" = c(120, 200), 
                                           "3195625" = c(265, 440))),
  name = "obesity_body_weight"
)
# bind and union
cdm <- omopgenerics::bind(cdm$obesity, 
                          cdm$obesity_bmi, 
                          cdm$obesity_body_weight, 
                          name = "obesity")
cdm$obesity <- cdm$obesity |>
  unionCohorts(cohortName = "obesity") |> 
  exitAtObservationEnd()


omopgenerics::logMessage("-- creating ckd cohorts (measurement or diagnosis)")
## CKD stage from measurements
egfr_codes <- c(
  1619025,  1619026, 3029829,  3029859, 3030104,  3045262,
  3049187,  3053283, 3964988,  3965919, 4213477,  36031320,
  36031846,  36303797, 36304157,  36306178, 36307905,  36660257,
  37393690,  37399046,  40764999,  40769275,
  40771922,  42869913,  46236952,  4338520, 36303653,  37208635,
  37393011,  37393012,  40478895,  40478963, 40483219,  40485075,
  40490315,  44788275,  44790060,  44790183,  44806420,  44808279, 45766361
)
cdm$ckd_stage_meausurement <- measurementCohort(
  cdm = cdm,
  conceptSet = list("egfr" = egfr_codes),
  valueAsNumber = list("ckd_stage_1_meas" = list("8795" = c(90, 9999999),
                                                 "720870" = c(90, 9999999)),
                       "ckd_stage_2_meas" = list("8795" = c(60, 89.99999),
                                                 "720870" = c(60, 89.99999)),
                       "ckd_stage_3_meas" = list("8795" = c(30, 59.99999),
                                                 "720870" = c(30, 59.99999)),
                       "ckd_stage_4_meas" = list("8795" = c(15, 29.99999),
                                                 "720870" = c(15, 29.99999)),
                       "ckd_stage_5_meas" = list("8795" = c(0, 14.99999),
                                                 "720870" = c(0, 14.99999))
                       ),
  name = "ckd_stage_meausurement"
) 
## CKD stage from diagnoses
ckd_diag_codes <- list(ckd_stage_1_diag = c(765535, 46284566, 46284567, 46284570, 443614, 46270354,
                                 601161, 44782703, 45773576, 43531559, 44792226, 44792227, 
                                 44784640, 43021853),
                 ckd_stage_2_diag = c(762000,	46284572, 46287169,46284575,
                                 443601,	46270355,	601162,	44782692,	45769901,	43531566,
                                 44792228,	44792229,	45757447,	43021836,	43021854),
                 ckd_stage_3_diag = c(37019193,	762001,	46284587,	46286992,
                                 46284588,	46284591,	46284592,	46284593,	443597,	46273636,
                                 601163,	44782691,	45771075,	43531653,
                                 44792230,	44792231,	45763854,
                                 44792232,	44792249,	45763855,	44792250,
                                 44792251,	45757446,	43021835,	43020456,	762033),
                 ckd_stage_4_diag = c(765536,	46284597,46284598,	46284599,
                                 443612,	46273514,	601164,	44782689,
                                 45769902,	43531577,	44792252,	44792253,
                                 45757445,	44784639,	43020457,	762034),
                 ckd_stage_5_diag = 	c(45768813,	760850,	46284600,	46284602,
                                  46284603,	443611, 46270356,
                                  601165,	44782690,	45769903,	43531562,
                                  37017813,	44792254,	37018761,	44792255,
                                  46273164,	37018886,	601166,	44782717,
                                  45769904,	45769906,	4030520,	4128200,
                                  4125970, 193782, 45772751,
                                  45757393,	45757392,	45757444,	762973,
                                  44784638,	43020437,	43020455,	43021864))

cdm$ckd_stage_diagnosis <- conceptCohort(cdm = cdm,
                                         ckd_diag_codes, 
                                         name = "ckd_stage_diagnosis",
                                         exit = "event_start_date")
## combine
cdm <- bind(cdm$ckd_stage_meausurement, 
            cdm$ckd_stage_diagnosis,  
            name = "ckd_stage")

cdm$ckd_stage <- cdm$ckd_stage |>
  unionCohorts(cohortId = c("ckd_stage_1_meas", "ckd_stage_1_diag"), 
               cohortName = "ckd_stage_1", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_2_meas", "ckd_stage_2_diag"), 
               cohortName = "ckd_stage_2", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_3_meas", "ckd_stage_3_diag"), 
               cohortName = "ckd_stage_3", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_4_meas", "ckd_stage_4_diag"), 
               cohortName = "ckd_stage_4", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_5_meas", "ckd_stage_5_diag"), 
               cohortName = "ckd_stage_5", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |> 
  subsetCohorts(cohortId = c("ckd_stage_1","ckd_stage_2","ckd_stage_3",
                             "ckd_stage_4","ckd_stage_5"), 
                name = "ckd_stage")



# Procedures -----
omopgenerics::logMessage("-- creating procedure cohorts")
procedure_codes <- omopgenerics::importCodelist(here::here("Cohorts", "procedures"), 
                                                type = "csv")
cdm[["procedures"]] <- conceptCohort(cdm,
                                     conceptSet = procedure_codes,
                                     exit = "event_start_date",
                                     name = "procedures") 

cdm[["procedures"]] <- cdm[["procedures"]]

# Bind  -----
cdm <- bind(cdm[["drugs"]],
            cdm[["conditions"]],
            cdm[["obesity"]],
            cdm[["ckd_stage"]],
            cdm[["procedures"]],
            name = "study_cohorts")
