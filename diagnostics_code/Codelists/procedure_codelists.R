
opcs4_to_standard <- function(opcs4_codes){
  cdm_vocab_2025_08$concept |>
    dplyr::filter(concept_code %in% !!opcs4_codes,
                  vocabulary_id == "OPCS4") |>
    dplyr::select("concept_id_1" = "concept_id") |>
    dplyr::left_join(cdm_vocab_2025_08$concept_relationship, by = "concept_id_1") |>
    dplyr::filter(relationship_id == "Maps to") |>
    dplyr::select("concept_id" = "concept_id_2") |>
    dplyr::left_join(cdm_vocab_2025_08$concept, by = "concept_id") |>
    dplyr::pull("concept_id")
}

# coronary artery bypass graft -----
cabg_opcs4 <- c("K40.1","K40.2","K40.3","K40.4","K40.8","K40.9",
                "K41.1","K41.2","K41.3","K41.4","K41.8","K41.9",
                "K42.1","K42.2","K42.3","K42.4","K42.8","K42.9",
                "K43.1","K43.2","K43.3","K43.4","K43.8","K43.9",
                "K44.1","K44.2","K44.3","K44.4","K44.8","K44.9",
                "K45.1","K45.2","K45.3","K45.4","K45.8","K45.9",
                "K46.1","K46.2","K46.3","K46.4","K46.8","K46.9",
                "K45.5","K45.6", "K46.5")
cabg_from_opcs4 <- opcs4_to_standard(cabg_opcs4)
cabg_snomed <- c(4336464L)

coronary_artery_bypass_graft <- list(
  coronary_artery_bypass_graft = c(cabg_snomed,cabg_from_opcs4) |> unique()) |>
  omopgenerics::newCodelist()
exportCodelist(coronary_artery_bypass_graft, here("Cohorts", "procedures"), "csv")


# percutaneous coronary intervention -----
pci_opcs4 <- c(
  "K49.1",  "K49.2", "K49.3", "K49.4", "K49.8", "K49.9",
  "K50.1", "K50.4", "K50.8", "K50.9",
  "K75.1", "K75.2", "K75.3", "K75.4", "K75.8", "K75.9")
pci_from_opcs4 <- opcs4_to_standard(pci_opcs4)

pci_snomed <- c("3655773", "3655774", "3655775", "3655776", "4020653",  "4106556", 
        "4150819",  "4172515", "4181025",  "4184832", "4212973",  "4216130",  
        "4225903", "4287108",  "37153378", "37153385",  "37153597",
        "37154069",  "37157437",  "37157438", "37158379",
        "43531439",  "43531440",  "44511133",  "44511138",
        "44511139",  "44511268",  "44511269",  "44511270", "44511271",
        "44511272",  "45769224","4006788","4020074", "4050590",
        "4051012",  "4051039", "4139362",  "4167564", "4170285",  "4171077",
        "4178148",  "4181955", "4184298", "4197882",  "4207625",
        "4216356",  "4217445", "4238755", "4244719", "4264285",
        "4264286", "4265293",  "4283892",  "4286646",
        "4303797","4328103",  "4329263",  "4330920","4336469", "4337739",
        "4337740", "35607959",  "37017357", "37111313",  "42537597",
        "42538258",  "42538259",  "44789455",  "44811692",
        "45770795","46271002") |> 
  as.integer()

percutaneous_coronary_intervention <- list(
  percutaneous_coronary_intervention = c(pci_snomed, pci_from_opcs4) |> unique()) |>
  omopgenerics::newCodelist()
exportCodelist(percutaneous_coronary_intervention, 
               here("Cohorts", "procedures"), "csv")

# carotid thromboendarterectomy -----
tea_opcs4 <- c("L29.4", "L29.5")
tea_from_opcs4 <- opcs4_to_standard(tea_opcs4)
tea_snomed <- c(4283095, 4234947, 4231552,
                4225217, 4021878, 4049551,
                4052115, 4226833, 4172230,
                4174591, 4216131, 4315669,
                4251929, 4035152)
thromboendarterectomy <- list(
  thromboendarterectomy = c(tea_snomed, tea_from_opcs4) |> unique()) |>
  omopgenerics::newCodelist()
exportCodelist(thromboendarterectomy, here("Cohorts", "procedures"), "csv")

# cerebral endovascular thrombectomy -----
evt_opcs4 <- c("L71.2")
evt_from_opcs4 <- opcs4_to_standard(evt_opcs4)
evt_snomed <- c(45760289, 45762301, 45770943, 45762364,
                45762133, 45768111, 4330779, 4231647, 4161134,
                4254255, 4019660, 4185284, 4069779,
                4315669,  4277450, 4033394,
                4287324, 4278676, 4144624,
                3549547, 4220354, 45764612)
evt_loinc <- c(36305550, 36305520, 1470059)
  
endovascular_thrombectomy <- list(
  endovascular_thrombectomy = c(evt_snomed, evt_loinc, evt_from_opcs4) |> unique()) |>
  omopgenerics::newCodelist()
exportCodelist(endovascular_thrombectomy, here("Cohorts", "procedures"), "csv")

# stenting extracranial  -----
se_opcs4 <- c("L31.4")
se_from_opcs4 <- opcs4_to_standard(se_opcs4) 
se_snomed <- c(4137380, 43531444, 4305223, 
               4185151, 4050288, 45764331,
               4161383, 4162771, 4197186,
               4197514, 4205194, 4330384,
               4197514, 4161383, 4205194,
               43531444, 4050288, 37018285,
               37016985, 4137380
               )
stenting_extracranial <- list(
  stenting_extracranial = c(se_snomed, se_from_opcs4) |> unique()) |>
  omopgenerics::newCodelist()

exportCodelist(stenting_extracranial, here("Cohorts", "procedures"), "csv")


# stenting intracranial -----
si_opcs4 <- c("L35.3")
si_from_opcs4 <- opcs4_to_standard(si_opcs4) 
si_snomed <- c(4326511, 3654287, 4232252, 4283095, 
               4305223, 45764018, 46272781,
               45764018, 45764019, 45771517)
stenting_intracranial <- list(
  stenting_intracranial = c(si_snomed, si_from_opcs4) |> unique()) |>
  omopgenerics::newCodelist()

exportCodelist(stenting_intracranial, here("Cohorts", "procedures"), "csv")


# codelist with details -----
cd <- c(coronary_artery_bypass_graft, 
     percutaneous_coronary_intervention, 
     thromboendarterectomy, 
     endovascular_thrombectomy, 
     stenting_extracranial,
     stenting_intracranial) |> 
     asCodelistWithDetails(cdm_vocab_2025_08)

writexl::write_xlsx(cd,
                    here::here("Codelists", "procedures_with_details.xlsx"))
