
# aspirin -------
aspirin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("aspirin"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(aspirin, path = here::here("Cohorts", "drugs"),
               type = "csv")

# P2Y12 inhibitors -----
p2y12_inhibitors <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("clopidogrel", "ticagrelor",
           "prasugrel", "cangrelor"),
  nameStyle = "{concept_name}",
  type = "codelist")  |> 
  unionCodelists()
names(p2y12_inhibitors) <- "p2y12_inhibitors"
exportCodelist(p2y12_inhibitors, path = here::here("Cohorts", "drugs"),
               type = "csv")

# dipyridamole -------
dipyridamole <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("dipyridamole"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(dipyridamole, path = here::here("Cohorts", "drugs"),
               type = "csv")

# ACEi and ARBs -------
acei_arbs <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c(# acei
    "captopril", "enalapril", "fosinopril", 
    "imidapril", "lisinopril", "perindopril", "quinapril", 
    "ramipril", "trandolapril",
    # arbs
    "azilsartan", "candesartan", "eprosartan", "irbesartan",        
    "losartan", "olmesartan", "telmisartan", "valsartan"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(acei_arbs) <- "acei_arbs"
exportCodelist(acei_arbs, path = here::here("Cohorts", "drugs"),
               type = "csv")


# diuretic (thiazide) ----
thiazide_diuretic <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("indapamide", "chlorthalidone", "metolazone",
           "xipamide", "bendroflumethiazide", 
           "hydrochlorothiazide"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(thiazide_diuretic) <- "thiazide_diuretics"
exportCodelist(thiazide_diuretic, path = here::here("Cohorts", "drugs"),
               type = "csv")

# calcium-channel blocker ----
calcium_channel_blocker <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("amlodipine", "felodipine", "lacidipine", "lercanidipine", 
           "nicardipine", "nifedipine", "verapamil", "diltiazem"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(calcium_channel_blocker) <- "calcium_channel_blockers"
exportCodelist(calcium_channel_blocker, path = here::here("Cohorts", "drugs"),
               type = "csv")

# beta blockers -------
beta_blockers <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("acebutolol", "alprenolol", "atenolol", 
           "bisoprolol", "carvedilol", 
           "metoprolol", "nadolol",
           "oxprenolol", "pindolol", 
           "propranolol", "timolol"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(beta_blockers) <- "beta_blockers"
exportCodelist(beta_blockers, path = here::here("Cohorts", "drugs"),
               type = "csv")

# statin -------
statin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("atorvastatin", "rosuvastatin", "simvastatin", 
           "pravastatin", "fluvastatin"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(statin) <- "statin"
exportCodelist(statin, path = here::here("Cohorts", "drugs"),
               type = "csv")

# ezetimibe  -------
ezetimibe <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("ezetimibe"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(ezetimibe, path = here::here("Cohorts", "drugs"),
               type = "csv")

# PCSK9 inhibitors ----
pcsk9_inhibitors <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("alirocumab", "evolocumab", 
           "inclisiran"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(pcsk9_inhibitors) <- "pcsk9_inhibitors"
exportCodelist(pcsk9_inhibitors, path = here::here("Cohorts", "drugs"),
               type = "csv")

# thrombolytics -------
thrombolytics <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("alteplase", "tenecteplase", 
           "reteplase", "streptokinase"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(thrombolytics) <- "thrombolytics"
exportCodelist(thrombolytics, path = here::here("Cohorts", "drugs"),
               type = "csv")

# warfarin -------
warfarin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("warfarin"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(warfarin, path = here::here("Cohorts", "drugs"),
               type = "csv")
# direct oral anticoagulants (DOACs) ----
doacs <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("apixaban", "dabigatran", 
           "edoxaban", "rivaroxaban"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(doacs) <- "doacs"
exportCodelist(doacs, path = here::here("Cohorts", "drugs"),
               type = "csv")


# GP IIb/ IIIa  -------
gp_iib_iiia <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("eptifibatide", "tirofiban", "abciximab"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(gp_iib_iiia) <- "gp_iib_iiia"
exportCodelist(gp_iib_iiia, path = here::here("Cohorts", "drugs"),
               type = "csv")

# thrombin inhibitors ----
thrombin_inhibitors <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("heparin", "enoxaparin", 
           "fondaparinux", "bivalirudin"),
  nameStyle = "{concept_name}",
  type = "codelist") |> 
  unionCodelists()
names(thrombin_inhibitors) <- "thrombin_inhibitors"
exportCodelist(thrombin_inhibitors, path = here::here("Cohorts", "drugs"),
               type = "csv")

# anticoagulants ----
anticoagulants <- c(warfarin, doacs_all, thrombin_inhibitors_all) |> 
  unionCodelists()
names(anticoagulants) <- "anticoagulants"
exportCodelist(anticoagulants, path = here::here("Cohorts", "drugs"),
               type = "csv")

# anti-dyslipidaemia  -----
anti_dyslipidaemia <- c(statin_all, pcsk9_inhibitors_all,
                        ezetimibe) |> 
  unionCodelists()
names(anti_dyslipidaemia) <- "anti_dyslipidaemia"
exportCodelist(anti_dyslipidaemia, path = here::here("Cohorts", "drugs"),
               type = "csv")

# antihypertensive -----
antihypertensive <- c(acei_arbs_all, beta_blockers_all,
                        calcium_channel_blocker_all, thiazide_diuretic_all) |> 
  unionCodelists()
names(antihypertensive) <- "antihypertensive"
exportCodelist(antihypertensive, path = here::here("Cohorts", "drugs"),
               type = "csv")

# antiplatelet -----
antiplatelet <- c(aspirin, dipyridamole,
                  p2y12_inhibitors) |> 
  unionCodelists()
names(antiplatelet) <- "antiplatelet"
exportCodelist(antiplatelet, path = here::here("Cohorts", "drugs"),
               type = "csv")
