#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated: 2021-12-01
# @DateModified: 2021-12-02 at 10:30AM
# @Description - This file reads in necessary data and identifies variables of interest for building a 
# multivariate logistic regression model to evaluate C diff risk among hospitalized patients. 
#-------------------------------------------------------------------------------

# Section 1: Load packages, import data, and initial cleaning ----

# Load required packages 
library(here)
library(readr)
library(tidyverse)

# Read in data
med_admin <- read_delim(here("data", "raw", "med admin table 11.08.21.csv"), delim = "|")
dm <- read_delim(here("data", "raw", "demographics and event table 11.08.21.csv"), delim = "|")
lab <- read_delim(here("data", "raw", "lab results 11.08.21.csv"), delim = "|")
dx <- read_delim(here("data", "raw", "covid diagnoses 11.08.21.csv"), delim = "|")

# Initial cleaning of datasets 
# Clean `dm` data
dm <- dm %>% rename(ID = deid_enc_id,
                    race = pat_race) %>% 
  select(ID, age, sex, race, ethnicity, language, readmit, ED_dispo, present_source)

# Clean `lab` data
lab <- lab %>% rename(ID = deid_enc_id) %>% 
  select(ID, lab_concept)

# Clean `dx` data
dx <- dx %>% rename(ID = deid_enc_id) %>% 
  select(ID, dx_group, DX_NAME, present_on_admit, severity_of_dx)

# Clean `med_admin` table 
med_admin <- med_admin %>% rename(ID = deid_enc_id) %>% 
  select(ID, thera_class, pharm_class, name)

######################################################################################################

# Section 2: Identify variables of interest in datasets to include in logistic reg model ----
    # COMMON PARAMETERS / RISK FACTORS FOR C DIFF RISK ASSESSMENT MODEL 

# Age >= 65 --> dm$age

# Prior hospital admission in 30 days --> dm$readmit == "Yes"

# Admission from ED --> dm$ED_dispo == "Admit" OR dm$present_source == ??

# Diabetes --> lab$lab_concept == "diabetes" OR dx$dx_group == "DIABETES MELLITUS"

# Receipt of antibiotics in last 30 days --> med_admin$thera_class %in% c("ANTIBIOTICS", "ANTIFUNGALS", "ANTIINFECTIVES/MISCELLANEOUS", "ANTIPARASITICS", "ANTIVIRALS")
    # NOTE: Doesn't include outpatient abx, only those during hospitalization

# Receipt of proton pump inhibitor (PPI) --> med_admin$pharm_class == "PROTON-PUMP INHIBITORS"

# Receipt of gastric acid suppressants, laxatives, etc. --> med_admin$pharm_class %in% c("ANTACIDS", "LAXATIVES AND CATHARTICS", "LAXATIVES, LOCAL/RECTAL")

levels(as.factor(med_admin$thera_class))

med_admin %>% filter(thera_class == "GASTROINTESTINAL") %>% select(pharm_class) %>% unique()


