-----
# this block contain the metadata of the .md document, you can add here:
#   - background keys: header, footer.
#   - bslib::card arguments.
# you can have more information how to use this section on the background
# vingette
header: "HERON-UK-02-003-CVDMedsInpatient Acute Myocardial Infarction Study"
-----

### **Variation in Acute Myocardial Infarction Management: a Cohort Study Using Real-World Data From UK Hospitals**

*Alejandro Ballve*<sup>1,\*</sup>, *Elin Rowlands*<sup>1,\*</sup>, *Anna Camps-Vilaró*<sup>1,2</sup>, *Cecilia Campanile*<sup>1</sup>, *Stelios Theophanous*<sup>3</sup>, *Alexander Coles*<sup>3</sup>, *Geoff Hall*<sup>3</sup>, *Ian Farr*<sup>4</sup>, *Tim Howcroft*<sup>4</sup>, *Rafael Henkin*<sup>5,6</sup>, *Jennifer C. E. Lane*<sup>5,6</sup>, *Usama Rahman*<sup>5,6</sup>, *Hiba Junaid*<sup>5,6</sup>, *Nicola Symmers*<sup>7</sup>, *Mahéva Vallet*<sup>7</sup>, *Peter Hall*<sup>7</sup>, *Colin Mclean*<sup>7</sup>, *Marta Alcalde-Herraiz*<sup>1</sup>, *Joan Guzman*<sup>8</sup>, *Anna Saura-Lázaro*<sup>1</sup>, *Daniel Prieto-Alhambra*<sup>1,9</sup>, *Edward Burn*<sup>1</sup>, *Danielle Newby*<sup>1,+</sup>, *Martí Català*<sup>1</sup>

<sup>1</sup> Health Data Sciences (HDS), Translational Sciences, Botnar Research Centre, University of Oxford, Oxford
<sup>2</sup> CIBER de Enfermedades Cardiovasculares, Instituto de Salud Carlos III, Madrid, Spain
<sup>3</sup> Leeds Teaching Hospitals NHS Trust, UK
<sup>4</sup> Lancashire Teaching Hospitals NHS Trust, UK
<sup>5</sup> Bone and Joint Health, Queen Mary University of London, London, UK
<sup>6</sup> Barts Health NHS Trust, London, UK
<sup>7</sup> DataLoch, University of Edinburgh, UK
<sup>8</sup> Cardiology Department, Hospital Clínic de Barcelona, Barcelona, Spain
<sup>9</sup> Department of Medical Informatics, Erasmus MC University, Rotterdam, Netherlands

<sup>\*</sup> Shared first authorship

<sup>+</sup> Corresponding author: [danielle.newby@ndorms.ox.ac.uk](mailto:danielle.newby@ndorms.ox.ac.uk)

#### Abstract

**Background**

Although the management of acute myocardial infarction (AMI) is well-established in clinical guidelines (1), variations have been reported in Europe and the UK not only across centres but also across sociodemographic subgroups, including differences related to age, sex, ethnicity, and socioeconomic status (SES) (2–6). However, a comprehensive picture of how these variations are distributed across such demographic subgroups and different healthcare centres in the UK is lacking.

**Methods**

We conducted a network cohort study across the UK using routinely collected electronic health records mapped to the Observational Medical Outcomes Partnership Common Data Model (OMOP CDM). The study included three datasets within the Health Data Research UK OMOP Network (HERON UK) (7): Barts Health NHS Trusts (Barts), Lancashire Teaching Hospitals (IDRIL), and Leeds Teaching Hospitals NHS Trust (LTHT). The study period was from 2022 to end of database capture. Individuals were required to be aged 18 years or older, to be admitted as inpatient, and to have a hospital record of AMI, which was required to be the first in that centre. Such record of AMI was established as the index event. The primary outcomes of interest were in-hospital drug treatment and/or procedures implemented in the 28 days after the AMI. Drug groups of interest were thrombolytics, antiplatelets, anticoagulants, betablockers, antihypertensives and lipid-lowering drugs. Procedural interventions included percutaneous coronary intervention (PCI) and coronary artery bypass graft (CABG). Individuals’ characteristics at the time of the AMI were described, and included age, sex, SES, ethnicity, main comorbidities, and 28-days mortality. Additionally, the odds ratio (OR) with 95% confidence interval (CI) to initiate each specific drug based on the sociodemographic characteristics was calculated, taking as a reference group the one with the highest proportion of individuals.

**Results**

17,165 individuals with a diagnosis of AMI were identified, 9,110 at Barts, 2,839 at IDRIL and 5,216 at LTHT. Of those, 3,929 were STEMI (22.8%) and 9,812 (57.2%) were NSTEMI. 

Individuals at Barts were younger than individuals at IDRIL and LTHT (median age 65 years [55 - 75] vs 74 years [62 - 83] and 71 years [60-81], respectively). A male predominance was observed across all centres (63.5-70.5%). The most frequent ethnic group was White, although Barts had a greater diversity (38.6% Asian, 9% Black). IDRIL had the highest proportion of individuals from the two most deprived quintiles (Q4 and Q5) (20.4% and 22.4% respectively). Females and white individuals were older. Individuals with STEMI were younger (median age 62 years [52 – 71] at Barts, 69 years [58-80] at IDRIL and 66 years [57-77] at LTHT) than individuals with NSTEMI (median age of 66 years [56-76] years at Barts, 73 years [61-82] years old at IDRIL, and 72 years [61 - 81] at LTHT), and had a higher proportion of males (69.2-75.9% vs 64.3-68.8%). Individuals with NSTEMI had a higher proportion of comorbidities. The most frequent co-pathologies were hypertension (62.8-74.3%), dyslipidaemia (32.3-64%), diabetes mellitus type 2 (30.4-45.9%) and obesity (17.5-35%). 8.7 -14.5% had had a prior MI. 

28 days mortality was 6.8% at Barts, 15.4% at IDRIL and 11.4% at LTHT. Mortality was higher for STEMI (9.4-20.5%) than for NSTEMI (3.8-9.9%).  Mortality increased with increasing age, from 2.9-6.3% (<65 years) to 20.4-27.9% (>85 years). Mortality was higher in females than in males across centres (9.3-17.2% vs 5.8-14.5%). 

Antiplatelets and anticoagulants were prescribed in the majority of individuals (86.4-90.6% and 76-89.9% respectively). When considering age, sex, ethnicity, SES and MI type, the 70-79 years and the 80-89 years groups had lower odds to initiate antiplatelets than the 60-69 years group across the three centres (e.g. at Barts, 70-79 years OR [95%CI] 0.78 [0.64-0.94]; 80-89 years OR [95% CI] 0.69 [0.56-0.85]). 

Lipid-lowering drugs (mostly statins) were prescribed to 64.2-83.7% of the study population. Statins prescription declined with increasing age, from 62.1-90.8% in <65 years old to 57-67% in >85 years old. Statin prescription was lower in females (61.3-77.9%) than in males (65.7-87%). 

Betablockers were prescribed in 65.8-79.5% of the cases. At Barts and LTHT, the prescription of betablockers declined with increasing age, from 78.3-86.4% in <65 years old to 63.2-67.4% in ≥85 years old. The prescription of betablockers in Black individuals (55.6-78.2%) was lower than in White and Asian individuals (65.7-78.8% and 68.6-83.7% respectively). 

Antihypertensives (mostly angiotensin converting enzyme inhibitors (ACEIs) or angiotensin receptor blockers (ARBs)) were prescribed in 53.9-76.9% of individuals, and their prescription declined with increasing age, from 54.4-69.2% in <65 years old to 45-58.9% in >85 years old. 

Thrombolytics counts were negligible in all data sources. 

We only had data regarding procedures and surgical interventions for Barts and LTHT, where PCI was conducted in 59.8-68.8% of the cases and CABG in the 1.9-9.5%. The percentage of PCI conducted was higher for STEMI (81.2%) than for NSTEMI (71.3%), whereas the contrary was observed for CABG (5.6% in STEMI vs 12.3% in NSTEMI). There was a steep decline in PCI with increasing age, from 79.3% in <65 years old to 25.8% in ≥85 years old. from 78.2-79.3% in <65 years old to 22.8-25.8% in ≥85 years old. Both PCI and CABG were less conducted in females (47.5-58.5% and 0.6-5.2%, respectively) than in males (66.9-73.2% and 0.6-11.3%, respectively). Asian had a higher percentage of PCI performed (68.4-72.4%), as compared to white individuals (59.8-66.7%) and black individuals (49.1-62.7%). 

**Discussion and conclusion**

Overall, the proportion of individuals receiving the analysed treatments would have been expected to be higher. This might be related to the limitations inherent to the use of RWD. Differences in the distribution of baseline clinical features, including median age and comorbidities, may partly explain the variation observed in the treatments prescribed in the setting of AMI across centres and sociodemographic subgroups. However, there were variations between population that raise the possibility of inequities in healthcare provision. Particularly, there were lower prescription proportions of several treatments to older individuals, females, and black individuals. These potential inequities should be further specifically explored in future studies. 

<img src="https://cdn.simpleicons.org/github" width="20"/> <https://github.com/heron-uk/HERON-UK-02-003-CVDMedsInpatient>
