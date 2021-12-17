# COMORBIDITIES --> CHARLESTON COMORBIDITY INDEX (CCI)

#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated: 2021-11-19
# @DateModified: 2021-11-19 
# @Description - This file ........
# @SourceData - `covid diagnoses 11.08.21.csv` 
# @Variables - 

#-------------------------------------------------------------------------------

# Background ----
# Comorbidities (`covid diagnoses 11.08.2021.csv`)
# This file contains data on patients' comorbid diagnoses (inc. ICD-10 codes, 
# severity of DX, DX group) and whether they were present on admission.

#-------------------------------------------------------------------------------

#####################################################################################

# Section 1: Load packages and import data ----
# Load required packages 
library(here)
library(readr)
library(dplyr)
library(tidyverse)

# Read in data
comorbid <- read_delim(here("data", "raw", "covid diagnoses 11.08.21.csv"))

# Filter where `present_on_admit` == "Yes" or "NULL"
comorbid <- comorbid %>% filter(present_on_admit %in% c("Yes", "NULL"))

# Select only `deid_enc_id` & `current_icd10_list` columns 
icd_only <- comorbid %>% select(deid_enc_id, current_icd10_list)

# Create list of comorbidities based on ICD-10 codes
# Citation: https://www.researchgate.net/publication/317237270_Developing_an_ICD-10-CM_version_of_Charlson_comorbidities_for_United_States_real-world_healthcare_data
CCI_long <- c("Myocardial Infarction", "Congestive Heart Failure", "Periphral Vascular Disease","Cerebrovascular Disease", 
  "Dementia", "Chronic Pulmonary Disease", "Rheumatic Disease", "Peptic Ulcer Disease", "Mild Liver Disease", 
  "Diabetes without complications", "Diabetes with complications","Paraplegia and Hemiplegia", 
  "Renal Disease", "Cancer","Moderate or Severe Liver Disease", "Metastatic Carcinoma", "AIDS/HIV")

cci_mi <- c('I21.02','I21.09','I21.3', 'I21.4', 'I21.A1')
cci_chf <- c('I09.9','I11.0', 'I13.0', 'I13.2', 'I25.5', 'I42.0', 'I42.8', 'I42.9', 
             'I50.1','I50.20', 'I50.21', 'I50.22', 'I50.23', '150.30', 'I50.32', 
             'I50.33', 'I50.42', 'I50.43', 'I50.811', 'I50.813','I50.82','I50.84','I50.9')
cci_pvd <- c('I70.0','I70.1','I70.201','I70.202','I70.208','I70.222','I70.245',
             'I70.261','I70.8','I71.00','I71.01','I71.02','I71.03','I71.2','I71.4', 
             'I73.9', 'I77.71', 'I77.72', 'I77.79', 'K55.9')
cci_cvd <- c('G45.3', 'G46.0', 'I60.32','I60.9','I61.1','I61.4','I61.5','I61.9',
             'I62.01','I62.02','I62.03','I63.032','I63.312','I63.322','I63.40',
             'I63.411','I63.412','I63.422','I63.432','I63.9','I65.1','I65.21',
             'I65.22','I65.23','I65.29','I66.22','I67.1','I67.2','I67.5','I67.6',
             'I67.7','I67.82','I67.848','I67.89','I67.9','I68.0')
cci_dem <- c('F01.50','F01.51','F02.80','F02.81','F03.90','F03.91', 'G30.1', 
             'G30.9', 'G31.09', 'G31.83')
cci_cpd <- c('I27.20','I27.21','I27.22','I27.23','I27.29','J42','J43.1','J43.9',
             'J44.0','J44.1','J44.9','J45.20','J45.21','J45.30','J45.40','J45.50',
             'J45.901','J45.902','J45.909','J47.0','J47.9', 'J67.2','J67.9')
cci_rheuma <- c('M05.00','M05.10','M06.041','M06.9', 'M32.14','M32.9','M33.13',
                'M33.20','M34.1','M34.9', 'M35.00', 'M35.1', 'M35.9')
cci_pud <- c('K25.4','K25.9','K26.4','K26.6','K27.9','K28.4')
cci_mld <- c('B18.1','B18.2','B18.9', 'K70.30', 'K70.31', 'K70.9', 'K74.0','K74.60',
             'K74.69', 'K75.4', 'K75.81', 'K75.9', 'K76.0', 'K76.1', 'K76.89', 'K76.9')
cci_dmWITHOUT <- c('E10.11', 'E10.641', 'E10.649', 'E10.65', 'E10.69', 'E10.9', 
                   'E11.649','E11.65',  'E11.9') 
cci_dmWITH <- c('E10.21','E10.22','E10.319','E10.3599','E10.36','E10.39','E10.40',
                'E10.42','E10.43','E10.51','E10.52','E11.21','E11.22','E11.319',
                'E11.3299','E11.3599','E11.36','E11.39','E11.40','E11.41','E11.42',
                'E11.43','E11.51','E11.52','E11.59','E10.622','E11.21','E11.22',
                'E11.319','E11.3299','E11.3599','E11.36','E11.39','E11.40','E11.41',
                'E11.42','E11.43','E11.51','E11.52','E11.59','E11.610','E11.621',
                'E11.622')
cci_hemipara <- c('G81.90','G81.91','G81.94', 'G82.20', 'G82.50', 'G83.9')
cci_renal <- c('E10.21', 'E10.22', 'E11.21', 'E11.22', 'I12.0', 'I12.9', 'I13.0', 
               'I13.10', 'N05.2', 'NO5.9', 'NO8', 'N18.1','N18.2','N18.3',
               'N18.30','N18.31','N18.32','N18.4','N18.5','N18.6','N18.9')
cci_cancer <- c('C02.1','C04.9','C06.9','C10.9','C15.4','C16.2','C16.9','C17.0',
                'C17.9','C18.4','C18.7','C18.9','C20','C22.0','C22.1','C23',
                'C25.9','C34.11','C34.31','C34.32','C34.90','C34.91','7C34.92',
                'C40.02','C41.0','C41.9','C43.9','C45.0','C48.0','C48.2','C49.12',
                'C49.9','C50.919','C51.9','C53.0','C53.9','C54.1','C55','C56.9',
                'C61','C62.91','C64.1','C67.9','C69.90','C71.1','C71.2','C71.3',
                'C71.6','C72.0','C73','C74.01','C85.10','C85.19','C85.90','C90.00',
                'C90.01','C90.02','C90.31','C91.00','C91.01','C91.02','C91.10',
                'C91.11','C92.00','C92.01','C92.02','C92.10','C92.11','C92.30',
                'C95.00', 'C7A.8', 'D45')
cci_msld <- c('I85.10', 'I86.4', 'K70.40','K72.90', 'K76.6', 'K76.7')
cci_tumor <- c('C77.0','C77.1','C77.2','C77.3','C77.5','C77.9','C78.00','C78.01',
               'C78.02','C78.1','C78.2','C78.6','C78.7','C78.89','C79.11','C79.2',
               'C79.31','C79.49','C79.51','C79.71','C79.72','C79.89','C79.9',
               'C80.1', 'C7B.8')
cci_hiv <- c('B20')


icd_counts <- icd_only %>% 
  distinct(deid_enc_id, current_icd10_list, dx_group_id) %>%
  mutate(cci_cancer = if_else(current_icd10_list %in% cci_cancer, 1, 0),
         cci_chf = if_else(current_icd10_list %in% cci_chf, 1, 0),
         cci_cvd = if_else(current_icd10_list %in% cci_cvd, 1, 0),
         cci_dem = if_else(current_icd10_list %in% cci_dem, 1, 0),
         cci_dmWITH = if_else(current_icd10_list %in% cci_dmWITH, 1, 0),
         cci_dmWITHOUT = if_else(current_icd10_list %in% cci_dmWITHOUT, 1, 0),
         cci_hemipara = if_else(current_icd10_list %in% cci_hemipara, 1, 0),
         cci_hiv = if_else(current_icd10_list %in% cci_hiv, 1, 0),
         cci_mi = if_else(current_icd10_list %in% cci_mi, 1, 0),
         cci_mld = if_else(current_icd10_list %in% cci_mld, 1, 0),
         cci_msld = if_else(current_icd10_list %in% cci_msld, 1, 0),
         cci_pud = if_else(current_icd10_list %in% cci_pud, 1, 0),
         cci_pvd = if_else(current_icd10_list %in% cci_pvd, 1, 0),
         cci_renal = if_else(current_icd10_list %in% cci_renal, 1, 0),
         cci_rheuma = if_else(current_icd10_list %in% cci_rheuma, 1, 0),
         cci_tumor = if_else(current_icd10_list %in% cci_tumor, 1, 0))

# Calculate weighted Charlston Comorbidity Index (CCI) score 
# All conditions are weighted on a scale from 1 to 6
# Source: https://doi.org/10.1016/0021-9681(87)90171-8
# Source: https://doi.org/10.1016/S0895-4356(01)00521-2

cci_final <-icd_counts %>% 
  distinct(deid_enc_id, current_icd10_list, dx_group_id) %>%
  group_by(deid_enc_id) %>% 
  summarize(CCI_total = sum((2*cci_cancer) + cci_chf + cci_cvd + cci_dem + (2*cci_dmWITH) + 
                        cci_dmWITHOUT + (2*cci_hemipara) + (6*cci_hiv) + cci_mi + 
                        cci_mld + (3*cci_msld) + cci_pud + cci_pvd + (2*cci_renal) + 
                        cci_rheuma + (6*cci_tumor))) %>% 
  distinct(deid_enc_id, .keep_all = T)


cci_final2 <-icd_counts %>% 
  group_by(deid_enc_id) %>% 
  distinct_at(vars(-current_icd10_list)) %>%
  mutate(CCI_total = sum((2*cci_cancer) + cci_chf + cci_cvd + cci_dem + (2*cci_dmWITH) + 
                              cci_dmWITHOUT + (2*cci_hemipara) + (6*cci_hiv) + cci_mi + 
                              cci_mld + (3*cci_msld) + cci_pud + cci_pvd + (2*cci_renal) + 
                              cci_rheuma + (6*cci_tumor)))

#GETTING SOMWHERE WITH THIS... COUNTS.. SUM COUNTS... IF > 0, then include in EQ CCI
blah <- icd_counts %>% 
  group_by(deid_enc_id) %>%
  mutate_if(is.numeric, sum) %>% 
  case_when(cci_renal > 1 ~ "1")

## TEST TEST 
test <- icd_counts %>% 
  group_by(deid_enc_id) %>%
  mutate(count = n())


#COMORBIDITY PACKAGE 
install.packages("comorbidity")
library(comorbidity)  

icd_only$current_icd10_list <- gsub("\\.", "", icd_only$current_icd10_list)

cci_res <- comorbidity(icd_only, id ="deid_enc_id", code = "current_icd10_list",
            score = "charlson", assign0 = T, icd = "icd10")

