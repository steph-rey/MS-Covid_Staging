
#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated: 2021-12-01
# @DateModified: 2021-12-03
# @Description - This file imports `bin_clin_scen_df_11.08.2021.csv` which contains all binary clinical scenarios and O2 devices for all patients, and runs a function to generate WHO Clinical Stages of Severity (4-10).
# @SourceData - `bin_clin_scen_df_11.08.2021.csv`

#-------------------------------------------------------------------------------

# Load packages, import data, and initial cleaning ----

# Load required packages 
library(here)
library(readxl)
library(dplyr)
library(tidyverse)

# Read in data
bin_clin_scen_df <- read_csv(here("data", "binary_clin_scen_df_11.08.21.csv"))
str(bin_clin_scen_df)

# Create new variable `STAGE` and set values to NA 
bin_clin_scen_df$STAGE <- NA

# Get_stages function ----

# Run function to get stages based on patients' binary clinical scenarios and usage of O2 devices
stages_df <- bin_clin_scen_df %>% 
  mutate(STAGE = case_when(DEATH==1 ~ 10,
                           ECMO==1 | 
                             ((INTUB==1 | IVDEV==1) & SF_LT_200==1 & (VP==1 | CRRT==1)) ~ 9,      # SHOULD I JUST USE 'IVDEV' OR ('INTUB' OR 'IVDEV')???
                           ((INTUB==1 | IVDEV==1) & SF_LT_200==1) |    # SHOULD I JUST USE 'IVDEV' OR ('INTUB' OR 'IVDEV')???
                             VP==1 | 
                             CRRT==1 ~ 8,
                           (INTUB==1 | IVDEV==1) & 
                             SF_LT_200==0 ~ 7,
                          (NIVDEV==1 & CPAPDEV==0) & 
                            IVDEV==0 & 
                            ((NIV_per_day > NC_PER_DAY) | NC_GT_12==1 | (NC_PER_DAY>0 & NC_GT_12==1)) ~ 6,
                          SIMPLEDEV==1 & (LowO2==1 | HighO2 == 1 | SIMPLE_PER_DAY > 1) ~ 5,
                          NC_PER_DAY>0 & NC_GT_12==0 ~ 5, 
                          (NIVDEV==0 & CPAPDEV==0 & INTUB==0 & IVDEV==0 & SIMPLEDEV==0) & LowO2==1 ~ 5,
                          TRUE ~ 4))

# Save as `stages_df` as .csv file
write_csv(stages_df, here("data", "stages_df_11.08.21.csv"))

#################################################################################
# END OF DOCUMENT ---- 
#################################################################################

