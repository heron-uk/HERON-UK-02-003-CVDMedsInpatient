#afib
afib <- readxl::read_excel("Codelists/xlsx_files/AFib_reviewd.xlsx") |>
  pull(concept_id)

afib_cl <- newCodelist(list("atrial_fibrillation" = afib))

exportCodelist(afib_cl, path = "Cohorts/conditions", type = "csv")

# atherosclerosis

arth <- readxl::read_excel("Codelists/xlsx_files/arterioscl_aorta_reviewed.xlsx",
                           col_names = FALSE) |>
  pull(`...1`)

arth_cl <- newCodelist(list("aortic_atherosclerosis" = arth))

exportCodelist(arth_cl, path = "Cohorts/conditions", type = "csv")

# Endovascular Treatment

evt <- readxl::read_excel("Codelists/xlsx_files/codelist_EVT_reviewed.xlsx", 
                         sheet = "codes") |>
  pull(concept_id)

evt_cl <- newCodelist(list("endovascular_treatment" = evt))

exportCodelist(evt_cl, path = "Cohorts/procedures", type = "csv")

# Patent foramen ovale

pfo <- readxl::read_excel("Codelists/xlsx_files/codelist_PFO_reviewed.xlsx", 
                          sheet = "codes") |>
  pull(concept_id)

pfo_cl <- newCodelist(list("patent_foramen_ovale" = pfo))

exportCodelist(pfo_cl, path = "Cohorts/conditions", type = "csv")

# Coronary arteriosclerosis

cor_art <- readxl::read_excel("Codelists/xlsx_files/coronary_arteriosclerosis_reviewed.xlsx") |>
  pull(concept_id)

cor_art_cl <- newCodelist(list("coronary_arteriosclerosis" = cor_art))

exportCodelist(cor_art_cl, path = "Cohorts/conditions", type = "csv")

# Heart Failure

hf <- readxl::read_excel("Codelists/xlsx_files/Heart_Failure_reviewed.xlsx") |>
  select(concept.CONCEPT_ID, concept.CONCEPT_NAME) |>
  filter(!is.na(concept.CONCEPT_ID)) |>
  pull(concept.CONCEPT_ID)

hf_cl <- newCodelist(list("heart_failure" = hf))

exportCodelist(hf_cl, path = "Cohorts/conditions", type = "csv")

# intracranial stent

int_stent <- readxl::read_excel("Codelists/xlsx_files/stent_intracranial_reviewed.xlsx",
                                col_names = FALSE) |>
  pull(`...1`)

int_stent_cl <- newCodelist(list("intracranial_stent" = int_stent))

exportCodelist(int_stent_cl, path = "Cohorts/procedures", type = "csv")

# intracranial stent

ext_stent <- readxl::read_excel("Codelists/xlsx_files/stenting_extracranial_reviewed.xlsx",
                                sheet = "codes") |>
  pull(concept_id)

ext_stent_cl <- newCodelist(list("extracranial_stent" = ext_stent))

exportCodelist(ext_stent_cl, path = "Cohorts/procedures", type = "csv")

# TEA

tea <- readxl::read_excel("Codelists/xlsx_files/TEA_reviewed.xlsx",
                                col_names = FALSE) |>
  pull(`...1`)

tea_cl <- newCodelist(list("TEA" = tea))

exportCodelist(tea_cl, path = "Cohorts/procedures", type = "csv")


# Hypertension

ht <- readxl::read_excel("Codelists/xlsx_files/codelist_hypertension_primary_reviewed.xlsx", 
              sheet = "codes") |>
  pull(concept_id)

ht_cl <- newCodelist(list("hypertension" = ht))

exportCodelist(ht_cl, path = "Cohorts/conditions", type = "csv")

# Craniectomy

cran <- readxl::read_excel("Codelists/xlsx_files/codelist_craniectomy_reviewed.xlsx", 
                           sheet = "codes") |>
  filter(`...7` == "T") |>
  pull(concept_id)

cran_cl <- newCodelist(list("craniectomy" = cran))

exportCodelist(cran_cl, path = "Cohorts/procedures", type = "csv")

# Carotid Disease

car <- readxl::read_excel("Codelists/xlsx_files/DARWINEUcodelist_Carotid_disease.xlsx", 
                          sheet = "codes") |>
  filter(CS_prev == "T") |>
  pull(concept_id)

car_cl <- newCodelist(list("carotid_disease" = car))

exportCodelist(car_cl, path = "Cohorts/conditions", type = "csv")

# Ischemic Stroke

isch <- readxl::read_excel("Codelists/xlsx_files/ischstroke_reviewed.xlsx") |>
  filter(Isch_incident == "T") |>
  pull(concept_id)

isch_cl <- newCodelist(list("ischemic_stroke" = isch))

exportCodelist(isch_cl, path = "Cohorts/conditions", type = "csv")

# Hemorrhagic Stroke

hem <- readxl::read_excel("Codelists/xlsx_files/hem_stroke_reviewed.xlsx") |>
  pull(concept_id)

hem_cl <- newCodelist(list("hemorrhagic_stroke" = hem))

exportCodelist(hem_cl, path = "Cohorts/conditions", type = "csv")

# Alcohol use

alc <- readxl::read_excel("Codelists/xlsx_files/OH_use.xlsx") |>
  pull(concept_id)

alc_cl <- newCodelist(list("alcohol_use" = alc))

exportCodelist(alc_cl, path = "Cohorts/conditions", type = "csv")

# Dyslipidemia

dys <- readxl::read_excel("Codelists/xlsx_files/DARWINEUcodelist_dyslipidemia).xlsx",
                          sheet = "codes") |>
  filter(dlp_broad == "T",
                dlp_highlevels == "T") |>
  pull(concept_id)

dys_cl <- newCodelist(list("dyslipidemia" = dys))

exportCodelist(dys_cl, path = "Cohorts/conditions", type = "csv")

# Valve heart disease (broad)

val <- readxl::read_excel("Codelists/xlsx_files/DARWINEUcodelist_valve_disease).xlsx",
                          sheet = "codes") |>
  filter(Valve_broad == "T") |>
  pull(concept_id)

val_cl <- newCodelist(list("valve_disease_broad" = val))

exportCodelist(val_cl, path = "Cohorts/conditions", type = "csv")

# Valve heart disease (inf endoc)

val <- readxl::read_excel("Codelists/xlsx_files/DARWINEUcodelist_valve_disease).xlsx",
                          sheet = "codes") |>
  filter(`Inf endoc` == "T") |>
  pull(concept_id)

val_cl <- newCodelist(list("valve_disease_inf_endoc" = val))

exportCodelist(val_cl, path = "Cohorts/conditions", type = "csv")
