#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated: 2021-11-16
# @DateModified: 2021-12-10 
# @Description - This file imports flowsheet and med admin data and returns a dataset containing all binary clinical scenarios and O2 devices per patient for each day of stay in hospital. 
# @SourceData - `flowsheet data table 11.08.2021.csv` & `med admin table 11.08.2021.csv`

#-------------------------------------------------------------------------------

# Background ----
# Flowsheet Vitals and Device Data (`flowsheet data table 11.08.2021.csv`)
    # This file contains data on SpO2, FiO2, O2 Flow Rate, O2 Device, 
    # Ventilation Modes and Settings, Dialysis Settings, ECMO Settings 

# Medical Administration Record (`med admin table 11.08.2021`)
    # This file contains data on Vasopressor (VP) Name, VP Rate, and VP Action.

#################################################################################

# Load packages, import data, and initial cleaning ----

# Load required packages 
library(here)
library(readr)
library(dplyr)
library(tidyverse)
library(Hmisc)   # this package is only used for "%nin%"

# Read in data
med_raw <- read_delim(here("data", "raw", "med admin table 11.08.21.csv"))
fs_raw <- read_delim(here("data", "raw", "flowsheet data table 11.08.21.csv"))
dm_raw <- read_delim(here("data", "dm_covid.csv"))

# Clean `med` table 
med <- med_raw %>%
  rename(ID = deid_enc_id,     # rename variable names
         time = taken_time) %>% 
  mutate(date = as.Date(time, format = "%Y-%m-%d"),    # create new column `date` from `taken_time`
         infusion_rate = as.numeric(infusion_rate)) %>%       # transform infusion rate to numeric 
  select(ID, time, date, vasopressor_flag, infusion_rate)    # keep only these variables 

# Clean `fs` table (where `meas_value` is numeric type, and `meas_val_chr` is character) 
fs <- fs_raw %>% 
  rename(ID = deid_enc_id,     # rename variable names
         meas_name = flo_meas_name, 
         time = recorded_time) %>% 
  filter(clinical_concept %nin% c("Respirations", "Urine_output", "Blood_pressure", "Pulse", "Temperature")) %>%  # filter out rows where `clinical_concept` == Respirations, Urine_output, Blood_pressure, Pulse, and Temperature
  filter(!is.na(meas_value)) %>%     # filter out missing/NA meas_value
  mutate(date = as.Date(time, format = "%Y-%m-%d"), # create new col `date` from `recorded_time`
         meas_val_chr = meas_value,                 # create new var `meas_val_chr` to keep meas_value as character type
         meas_value = as.numeric(meas_value)) %>%   # transform `meas_value` to numeric type
  select(ID, meas_name, meas_value, meas_val_chr, time, date, clinical_concept)   # keep only these variables

# Clean `dm` table 
dm <- dm_raw %>%
  rename(ID = deid_enc_id) %>%      # rename `deid_enc_ID` var 
  mutate(death_date = as.Date(as.POSIXct(death_time, format = "%Y-%m-%d %H:%M:%S"))) %>%       # create new var `death_date` from `death_time`
  select(ID, age, sex, zip_code, pat_race, ethnicity, BMI, end_in_death, death_date, hospital_LOS)
  # can edit select() to include/exclude variables from `dm_raw`

#################################################################################

# VASOPRESSOR ----
    # VP=1 if vasopressor_flag = 'Yes' & infusion_rate > 0

# Create table to indicate whether each patient received vasopressor each day
vp.tab <- med %>% 
  mutate(vp_yn = if_else((vasopressor_flag == "Yes" & infusion_rate > 0), 1, 0)) %>%  # create new variable `vp_yn` to indicate whether patient received VP at timestamp
  group_by(ID, date) %>%          # group by ID and date 
  mutate(VP = max(vp_yn)) %>%     # if pt was on VP at any timestamp during day, then VP = 1 (yes)
  distinct(ID, date, VP)          # remove duplicates

    # Summary stats: n=54,955 unique patients; n=751 unique dates 

#################################################################################

# ECMO (Flowsheet Vitals and Device Table) ----
    # ECMO=1 if clinical_concept == ECMO_settings

# Create table to indicate whether each patient received ECMO each day 
ecmo.tab <- fs %>% 
  mutate(ecmo_yn = if_else(clinical_concept == "ECMO_settings", 1, 0)) %>%  # create new variable `ecmo_yn` to indicate whether pt received ECMO at timestamp
  group_by(ID, date) %>%                  # group by ID and date
  mutate(ECMO = max(ecmo_yn)) %>%         # if pt was on ECMO at any time during day, then ECMO = 1 (yes)
  distinct(ID, date, ECMO)                # remove duplicates 

    # Summary stats: n=55,000 unique patients; n=742 unique dates 

#################################################################################

# CRRT (Flowsheet Vitals and Device Table) ----
    # CRRT=1 if clinical_concept == HD_UF_CRRT_settings and meas_name == ...

# Create table to indicate whether patient received CRRT each day 
crrt.tab <- fs %>% 
  mutate(crrt_yn = if_else((clinical_concept == "HD_UF_CRRT_settings" &       # create new var `crrt_yn` to indicate whether pt received CRRT at any timestamp 
                              meas_name %in% c("R UFR TRANSCRIBED CRRT IP_CD_UCSF", 
                                               "R CRRT BLOOD FLOW RATE")), 1, 0)) %>%  
  group_by(ID, date) %>%               # group by ID and date
  mutate(CRRT = max(crrt_yn)) %>%      # if pt received CRRT at any time during day, then CRRT = 1 (yes)
  distinct(ID, date, CRRT)             # remove dups 

    # Summary stats: n=55,000 unique patients; n=742 unique dates 

#################################################################################

# NIV Settings (Flowsheet Vitals and Device Table) ----
    # NIV=1 if received support from NIV device
    # NIV_per_day 

# Create table to indicate whether patient received support from NIV device each day (`NIV`)
niv.tab <- fs %>% 
  mutate(niv_yn = if_else(clinical_concept == "NIV_settings", 1, 0)) %>% 
  group_by(ID, date) %>% 
  mutate(NIV = max(niv_yn),
         NIV_per_day = sum(clinical_concept == "NIV_settings")) %>% 
  distinct(ID, date, NIV, NIV_per_day)

    # Summary stats: n=55,000 unique patients; n=742 unique dates 

#################################################################################

# HD Settings (Flowsheet Vitals and Device Table) ----
    # HD=1 if received HD device / settings

# Create table to indicate whether patient received HD each day
hd.tab <- fs %>% 
  mutate(hd_yn = if_else((clinical_concept == "HD_UF_CRRT_settings" & 
           meas_name %in% c("R HD BLOOD FLOW RATE", "R HD ULTRAFILTRATION RATE")), 1, 0)) %>%
  group_by(ID, date) %>% 
  mutate(HD = max(hd_yn)) %>% 
  distinct(ID, date, HD)

#################################################################################

# IV Settings (Flowsheet Vitals and Device Table) ----
# IV=1 if received IV device / settings

# Create table to indicate whether patient received IV each day
#iv.tab <- fs %>% 
#  mutate(iv_yn = if_else(clinical_concept == "IV_settings", 1, 0)) %>%
#  group_by(ID, date) %>% 
 # mutate(IV = max(iv_yn)) %>% 
 # distinct(ID, date, IV) %>% 
 # arrange(desc(IV))

#################################################################################

# Intubation Settings (Flowsheet Vitals and Device Table) ----
    # INTUB=1 if received intubation

# Create table to indicate whether patient received intubation each day
intub.tab <- fs %>% 
  mutate(intub_yn = if_else(clinical_concept == "Intubation_settings", 1, 0)) %>% 
  group_by(ID, date) %>% 
  mutate(INTUB = max(intub_yn)) %>% 
  distinct(ID, date, INTUB)

#################################################################################

# SpO2 / FiO2 < 200 ---- 
    # SF_LT_200=1 if SpO2 / FiO2 < 200 AT SAME TIMESTAMP

# Filter data for SpO2 values and assign to table `spo2.tab`
spo2.tab <- fs %>% 
  filter(clinical_concept == "SpO2") 

# Filter data for FiO2 values and assign to table `fio2.tab`
fio2.tab <- fs %>% 
  filter(meas_name == "R FIO2")

# Merge SpO2 and FiO2 tables and calculate SF 
SF.tab <- fio2.tab %>% 
  inner_join(spo2.tab, by = c("ID", "time"), suffix = c(".fi", ".sp")) %>% 
  mutate(SF = (meas_value.sp / meas_value.fi * 100)) %>% 
  select(ID, time, date.fi, SF) %>% 
  rename(date = date.fi)

# Create table to indicate whether patient experienced SpO2/FiO2<200 at same timestamp (`SF_LT_200`)
SF200.tab <- SF.tab %>% 
  mutate(sf_lt_200_yn = if_else(SF < 200, 1, 0)) %>% 
  group_by(ID, date) %>% 
  mutate(SF_LT_200 = max(sf_lt_200_yn)) %>% 
  distinct(ID, date, SF_LT_200)

#################################################################################

# O2 Devices Table ---- MAY DELELETE LATER ATTEMPT #1 ---------

# Assign list of O2 devices to respiratory support categories: NONE, SIMPLE, NIV, 
# IV, CPAP, and NC (inc high-flow nasal cannula)

    # NONE
    no_dev_names <- c("None (Room air)", "Cool mist", "Other (Comment);Cool mist", 
                      "Other (Comment);None (Room air)")
    # SIMPLE
    simple_dev_names <- c("Aerosol", "Blow-by", "Face tent", "Nasal cannula", 
                          "Simple mask", "Trach mask", "Other (Comment);Trach mask", 
                          "Other (Comment);Simple mask", "Other (Comment);Blow-by",
                          "Other (Comment);Nasal cannula", "Other (Comment);
                          Face tent", "Other (Comment);Aerosol mask")
    
    # Non-Invasive Ventilation (NIV) 
    niv_dev_names <- c("BiPAP", "BVM", "High flow nasal cannula", "Venturi mask",
                       "T-piece", "Non-rebreather mask", "Transtracheal catheter",
                       "Other (Comment);Non-rebreather mask", "Other (Comment);BiPAP",
                       "Other (Comment);High flow nasal cannula")
    
    # Invasive (IV)
    iv_dev_names <- c("Vent", "Ventilator", "Other (Comment);Ventilator")
    
    # CPAP
    cpap_dev_names <- c("CPAP", "Other (Comment);CPAP")
    
    # Nasal cannula or high-flow nasal cannula (HFNC)
    nc_dev_names <- c("Nasal cannula", "Other (Comment);Nasal cannula", 
                        "High flow nasal cannula", "Other (Comment);High flow nasal cannula")

# Create table of O2 devices with column assigned to each category of respiratory support
device.tab <- fs %>%
  filter(clinical_concept == "O2_device") %>%
  mutate(NOdev_yn = if_else(meas_val_chr %in% no_dev_names, 1, 0),
         SIMPLEdev_yn = if_else(meas_val_chr %in% simple_dev_names, 1, 0),
         NIVdev_yn = if_else(meas_val_chr %in% niv_dev_names, 1, 0),
         IVdev_yn = if_else(meas_val_chr %in% iv_dev_names, 1, 0),
         CPAPdev_yn = if_else(meas_val_chr %in% cpap_dev_names, 1, 0),
         NCdev_yn = if_else(meas_val_chr %in% nc_dev_names, 1, 0)) %>% 
  group_by(ID, date) %>% 
  mutate(NODEV = max(NOdev_yn),
         SIMPLEDEV = max(SIMPLEdev_yn),
         SIMPLE_PER_DAY = sum(meas_val_chr %in% simple_dev_names),
         NIVDEV = max(NIVdev_yn),
         IVDEV = max(IVdev_yn),
         CPAPDEV = max(CPAPdev_yn),
         NCDEV = max(NCdev_yn),
         NC_PER_DAY = sum(meas_val_chr %in% nc_dev_names)) %>% 
  distinct(ID, date, NODEV, SIMPLEDEV, SIMPLE_PER_DAY, NIVDEV, IVDEV, CPAPDEV, NCDEV, NC_PER_DAY)

  # Summary stats: n=54,959 unique patients; n=738 unique dates 

#################################################################################

# Assign list of O2 devices to respiratory support categories: NONE, SIMPLE, NIV, 
# IV, CPAP, and NC (inc high-flow nasal cannula)

# Read in `o2_dev_names.csv` which lists all O2 devices and their respective categories 
o2_dev_names <- read_csv(here("data", "o2_dev_names.csv"))

# NONE -- filter all rows where dev_cat == "no_dev"
no_dev_names <- filter(o2_dev_names, dev_cat == "no_dev")[[1]]

# SIMPLE -- filter all rows where dev_cat == "simple_dev"
simple_dev_names <- filter(o2_dev_names, dev_cat == "simple_dev")[[1]]

# Non-Invasive Ventilation (NIV) -- filter all rows where dev_cat == "NIV_dev"
niv_dev_names <- filter(o2_dev_names, dev_cat == "NIV_dev")[[1]]

# Invasive (IV) -- filter all rows where dev_cat == "iv_dev"
iv_dev_names <- filter(o2_dev_names, dev_cat == "iv_dev")[[1]]

# CPAP -- filter all rows where dev_cat == "cpap_dev"
cpap_dev_names <- filter(o2_dev_names, dev_cat == "cpap_dev")[[1]]

# Nasal cannula or high-flow nasal cannula (HFNC) -- filter all rows where nc_dev == 1
nc_dev_names <- filter(o2_dev_names, nc_dev == 1)[[1]]

# O2 Device Classification Table ---- 
# Create table of O2 devices with column assigned to each category of respiratory support
device.tab2 <- fs %>%
  filter(clinical_concept == "O2_device") %>%
  mutate(NOdev_yn = if_else(meas_val_chr %in% no_dev_names, 1, 0),
         SIMPLEdev_yn = if_else(meas_val_chr %in% simple_dev_names, 1, 0),
         NIVdev_yn = if_else(meas_val_chr %in% niv_dev_names, 1, 0),
         IVdev_yn = if_else(meas_val_chr %in% iv_dev_names, 1, 0),
         CPAPdev_yn = if_else(meas_val_chr %in% cpap_dev_names, 1, 0),
         NCdev_yn = if_else(meas_val_chr %in% nc_dev_names, 1, 0)) %>% 
  group_by(ID, date) %>% 
  mutate(NODEV = max(NOdev_yn),
         SIMPLEDEV = max(SIMPLEdev_yn),
         SIMPLE_PER_DAY = sum(meas_val_chr %in% simple_dev_names),
         NIVDEV = max(NIVdev_yn),
         IVDEV = max(IVdev_yn),
         CPAPDEV = max(CPAPdev_yn),
         NCDEV = max(NCdev_yn),
         NC_PER_DAY = sum(meas_val_chr %in% nc_dev_names)) %>% 
  distinct(ID, date, NODEV, SIMPLEDEV, SIMPLE_PER_DAY, NIVDEV, IVDEV, CPAPDEV, NCDEV, NC_PER_DAY)

  # Summary stats: n=54,959 unique patients; n=738 unique dates 

#################################################################################

# Low vs. High O2 Flow Rate Table ----

# Classify whether patients had O2 use and low / high O2 flow rate
    # O2: meas_value > 0 then O2_use = 1 (yes)
    # LowO2: meas_value <= 12
    # HighO2: meas_value > 12

# Create vector for O2 flow rate names
O2_names <- c("R OXYGEN FLOW RATE", "R OXYGEN FLOW RATE 2 IP_CD_UCSF")

O2.tab <- fs %>% 
  mutate(O2 = if_else(meas_name %in% O2_names & meas_value > 0, 1, 0),
         LowO2 = if_else(meas_name %in% O2_names & meas_value <= 12, 1, 0),
         HighO2 = if_else(meas_name %in% O2_names & meas_value > 12, 1, 0)) %>% 
  group_by(ID, date) %>% 
  mutate(O2 = max(O2),
         LowO2 = max(LowO2), 
         HighO2 = max(HighO2)) %>% 
  distinct(ID, date, O2, LowO2, HighO2)

# Merge `device.tab` and `O2.tab` (left_join on ID and date columns)
O2_DEV.tab <- O2.tab %>%
  left_join(device.tab2, by = c("ID", "date")) %>%          #left join????
  mutate(NC_GT_12 = if_else(NCDEV == 1 & HighO2 == 1, 1, 0))
  
#################################################################################

# Join all tables ----

    # List of tables to join
    # vp.tab, ecmo.tab, crrt.tab, niv.tab, hd.tab, intub.tab, SF.tab, SF200.tab,  O2_DEV.tab

# Merge all tables to aggregate binary clinical scenario and O2 device variables 
bin_clin_scen_df <- fs %>% 
  distinct(ID, date) %>% 
  left_join(dm, by = "ID", suffix = c("", ".dupcol")) %>% 
  select(ID, date, death_date, end_in_death) %>% 
  left_join(vp.tab, by = c("ID", "date"), suffix = c("", ".dupcol")) %>%
  left_join(ecmo.tab, by = c("ID", "date"), suffix = c("", ".dupcol")) %>%
  left_join(crrt.tab, by = c("ID", "date"), suffix = c("", ".dupcol")) %>%
  left_join(niv.tab, by = c("ID", "date"), suffix = c("", ".dupcol")) %>%
  left_join(hd.tab, by = c("ID", "date"), suffix = c("", ".dupcol")) %>%
  left_join(intub.tab, by = c("ID", "date"), suffix = c("", ".dupcol")) %>%
  left_join(SF200.tab, by = c("ID", "date"), suffix = c("", ".dupcol")) %>%
  left_join(O2_DEV.tab, by = c("ID", "date"), suffix = c("", ".dupcol")) %>%
  select(-ends_with(".dupcol")) %>%
  mutate(DEATH = 0,
         DEATH = if_else(date == death_date, 1, 0))

#################################################################################

# Export df as .csv and .Rdata ----
write_csv(bin_clin_scen_df, here("data", "binary_clin_scen_df_11.08.21.csv"))
save(bin_clin_scen_df, file = here("data", "binary_clin_scen_df_11.08.21.Rdata"))

#################################################################################
# END OF DOCUMENT ---- 
#################################################################################
