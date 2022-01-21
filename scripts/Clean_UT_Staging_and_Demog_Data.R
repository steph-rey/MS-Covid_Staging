# Load required packages
library(tidyverse)
library(forcats)

# Read in daily staging data 
stage_raw <- read_csv("/Users/sreynolds2/Downloads/UTSW_Ordinal_Scale_data_unt_12312021_FINAL.csv")

# Clean up
stage <- stage_raw %>% 
  filter(!is.na(ordinal_scale_final)) %>% 
  select(PAT_ID, event_time, ordinal_scale_final, HSP_INITIAL_DATE) %>% 
  mutate(date = as.Date(event_time)) %>% 
  rename(ID = PAT_ID,
         stage = ordinal_scale_final) %>% 
  group_by(ID, date) %>% 
  mutate(stage = max(stage)) %>% 
  distinct(ID, date, stage)

# Read in demographics data
demog_raw <- read_csv("/Users/sreynolds2/Downloads/COVID_Reg_Hosp_Demographics_03012020_123121_FINAL.csv")

# Clean up 
demog <- demog_raw %>%  
  distinct(PAT_ID, BIRTH_DATE, .keep_all = T) %>% 
  rename(ID = PAT_ID,
         BMI = Initial_BMI) %>% 
  mutate(age = as.numeric((as.Date(ADM_DATE) - as.Date(BIRTH_DATE)) / 365.25),
         death = as.factor(ifelse(Patient_Status=="Deceased", "Yes", "No")),
         sex = as.factor(Sex),
         race = as.factor(Race),
         ethnicity = as.factor(Ethnic_Group),
         zip = as.character(Zip_Code)) %>% 
  select(ID, age, sex, race, ethnicity, zip, BMI, death)

# Collapse levels for ethnicity and race variables (aligning with UCSF dataset)
demog$ethnicity <- fct_collapse(demog$ethnicity,
                           "Not Hispanic or Latino" = c("Non-Hispanic/Latino", "White"),
                           "Unknown" = c("Unknown", "Declined"))

demog$race <- fct_collapse(demog$race,
                           "Other" = c("Some other race", "American Indian or Alaska Native"),
                           "Unknown" = c("Unavailable/Unknown", "Declined"))

# Merge demographics and daily staging datasets 
all_pt_days <- inner_join(stage, demog, by = "ID") %>% 
  relocate(date, .before=age) %>% 
  relocate(stage, .before=age)

# Group by ID then create variables for LOS, max_stage, date_adm, date_disc, stage_adm, and stage_disc 
all_pt_days <- all_pt_days %>% 
  group_by(ID) %>% 
  mutate(LOS = as.integer(round(max(date) - min(date), 1)),
         max_stage = as.factor(max(stage)), 
         date_adm = min(date),
         date_disc = max(date),
         stage_adm = as.factor(stage[date==min(date)]),
         stage_disc = as.factor(stage[date==max(date)]))

# Save resulting df as csv
write_csv(all_pt_days, "/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/UTSW/dm_stg_all_pt_days.csv")
saveRDS(unique_pts, file="/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/UTSW/dm_stg_all_pt_days.rds")

# Create df that contains only unique pts 
unique_pts <- all_pt_days %>% 
  distinct(ID, .keep_all = T) %>% 
  select(-c(date, stage))

# Save resulting df as csv
write_csv(unique_pts, "/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/UTSW/dm_stg_unique_pts.csv")
saveRDS(unique_pts, file="/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/UTSW/dm_stg_unique_pts.rds")

