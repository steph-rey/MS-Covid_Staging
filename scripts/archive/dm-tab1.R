#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated: 2021-11-15
# @DateModified: Sys.time()
# @Description - This file summarizes demographics of COVID (+) inpatients admitted to UCSF between Feb 3, 2020 - May 29, 2021.
# @SourceData - `demographics and event table_11.08.21.csv`
# @Variables - 
    # @deid_enc_id: Deidentified encounter ID, unique to each patient
    # @age: Age (in years)
    # @sex: Sex (Female, Male)
    # @pat_race: Race (Other, Asian, Black or African American, Unknown, Native Hawaiian or Other Pacific Islander, White or Caucasian)
    # @ethnicity: Ethnicity (Hispanic of Latino, Not Hispanic or Latino, Unknown)
    # @BMI: Body Mass Index
    # @covid_pos: Whether patient tested positive for CoV2-SARS-2 (Yes, No)
    # @hospital_LOS: Length of stay (in days)
#-------------------------------------------------------------------------------

# Background ----
# Demographics and Events
    # This file contains data on patients' age, sex, race, ethnicity, time of
    # admission, time of discharge or death, comorbidities, and other demographics.

#####################################################################################

# Section 1: Read in data and filter for covid(+) inpatients ----
# Load packages 
library(here)
library(readr)
library(dplyr)
library(tidyverse)
library(forcats)
library(tableone)
library(labelled)

# Read in `demographics and event table 11.08.21.csv` and assign to tibble `dm`
dm <- read.csv(here("data", "raw", "demographics and event table 11.08.21.csv"), sep = "|")

# Replace NULL values with NA for `hospital_LOS`
dm$hospital_LOS[dm$hospital_LOS == "NULL"] <- NA

# Filter dataset where `covid_pos` == 'Yes' & `hospital_LOS` > 0
dm <- dm %>% 
  filter(covid_pos == "Yes" & LOS > 0)

#####################################################################################

# Section 2: Transform and collapse variables ---- 
# Transform `hospital_LOS` to numeric
dm$hospital_LOS <- as.numeric(dm$hospital_LOS)

# Transform `BMI` to numeric
dm$BMI <- as.numeric(dm$BMI)

# Transform `age` to numeric
dm$age <- as.numeric(dm$age)

# Transform `sex` to factor
dm$sex <- as.factor(dm$sex)

# Transform `race` to factor, and collapse categories
dm$pat_race <- as.factor(dm$pat_race)

dm$pat_race <- fct_collapse(dm$pat_race, 
                                 "Other" = c("Other", "American Indian or Alaska Native"),
                                 "Unknown" = c("Unknown", "Declined", "Unknown/Declined"),
                                 "Native Hawaiian or Other Pacific Islander" = c("Native Hawaiian or Other Pacific Islander", "Other Pacific Islander"))

# Transform 'ethnicity' to factor, and collapse categories
dm$ethnicity <- as.factor(dm$ethnicity)

dm$ethnicity <- fct_collapse(dm$ethnicity, 
                             "Unknown" = c("Declined", "Unknown", "Unknown/Declined"))

# Save `dm` dataset as CSV file
write.csv(dm, file = here("data", "dm_covid.csv"), row.names = FALSE)

# Section 3 ---- Create Table One of demographics and LOS 
# Assign necessary variables in which to include in table 1 to `vars` vector 
dput(names(dm))
vars <- c("age", "sex", "pat_race", "ethnicity", "hospital_LOS", "BMI")

# Assign variable labels using labelled package 
var_label_list <- list(n = "Number of COVID+ inpatient admissions",
                       age = "Age (in years)",
                       sex = "Biological Sex",
                       race = "Race",
                       smoking = "Smoker",
                       ethnicity = "Ethnicity",
                       LOS = "Length of Stay (in days)",
                       BMI = "BMI")

labelled::var_label(dm) <- var_label_list

# Create a table 1 object
tab1 <- CreateTableOne(vars = vars, data = dm, includeNA = T)
tab1

# Print table 1 
tab1_res <- print(tab1, showAllLevels = T, varLabels = T, nonnormal = "hospital_LOS", noSpaces = T)

# Save as CSV file
write.csv(tab1_res, file = here("Table 1: Demographics of COVID-19 Inpatients.csv"))


# Run summary statistics on `stages_df`
```{r Summary stats}
summary(stages_df)

# CreateCatTable from tableone package 
cont_vars <- c("age", "BMI", "end_in_death", "LOS", "STAGE")
cat_vars <- c("sex", "race", "ethnicity", "smoking", "end_in_death", "STAGE")

tableone::CreateCatTable(data=stages_df, cat_vars)
tableone::CreateContTable(data=stages_df, cont_vars)

```
