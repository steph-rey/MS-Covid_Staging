#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated: 2021-11-15
# @DateModified: Sys.time()
# @Description - This file parses the admit and discharge date from the `dm` dataset and returns an expanded dataset with one row per patient per day of hospitalization. 
# @SourceData - `dm_covid.csv`
# @Variables - 
    # @admit_time: Date/time that patient was admitted (YYYY-MM-DD HH:MM:SS)
    # @discharge_time: Date/time that patient was discharged (YYYY-MM-DD HH:MM:SS)
    # @admit_date: Date that patient was admitted (YYYY-MM-DD)
    # @discharge_date: Date that patient was discharged (YYYY-MM-DD)
    # @day_date: Specific day of patient encounter in hospital (YYYY-MM-DD)
#-------------------------------------------------------------------------------

# Background ----
# Demographics and Events
    # This file contains data on patients' age, sex, race, ethnicity, time of
    # admission, time of discharge or death, comorbidities, and other demographics.

#####################################################################################

# Section 1: Load packages and import data ----
# Load required packages 
library(here)
library(tidyverse)

# Read in data
dm <- read_csv(here("data", "dm_covid.csv"))

# Look at structure of `dm` and identify variable types 
str(dm)

#####################################################################################

# Section 2: Create new var `day_date` that spans from `admit_date` to `discharge_date` to create an expanded patient-each-day table ----
# Create vector of variables to include in final dataset, assign to `vars`
vars <- c("deid_enc_id", "age", "sex", "zip_code", "pat_race", "ethnicity",
          "smoking", "BMI", "RR_encounter", "NIV_encounter", "new_intubation_encounter",
          "admit_or_transfer_ICU", "ECMO_encounter", "HD_UF_CRRT_encounter",
          "end_in_death", "admit_time", "discharge_time", "hospital_LOS")

dm <- dm %>% 
  select(vars) %>% 
    # Filter to only include variables in `vars` vector
  mutate(admit_date = as.Date(admit_time),
         discharge_date = as.Date(discharge_time)) %>% 
    # Parse date from `admit_time` and `discharge_time`
  group_by(deid_enc_id) %>%     # Group by `deid_enc_id` (unique patient identifier)
  mutate(day_date = list(seq(min(admit_date), max(discharge_date), by = "day"))) %>% 
  unnest(day_date)    # Create new var `day_date` that spans from `admit_date` to `discharge_date` for each patient
