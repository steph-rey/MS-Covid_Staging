#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated - 2021-11-19
# @DateModified - 2021-04-20 at 4:00PM
# @Description - This file uses the comorbidity package to compute the (weighted)
# Charlson score and (weighted) Charlson index for each patient. It also calculates
# the (weighted) Elixhauser score.
# @SourceData - `all encounter diagnoses 4.14.22.csv` 
# @Input Variables - 
    # @deid_enc_id: Deidentified encounter ID (unique patient identifier)
    # @current_icd10_list: ICD-10 code that classifies dx, sx, and procedures
# @Output Variables - 
    # @score: Charlson score (aggregate measure of comorbidity)
    # @index: Charlson index 
    # @wscore: Weighted Charlson score
    # @windex: Weighted Charlson index 
#-------------------------------------------------------------------------------

# Background ----

# Comorbidities (`all encounter diagnoses 4.14.22.csv`)

# This file contains data on patients' comorbid diagnoses (inc. ICD-10 codes, 
# severity of DX, DX group) and whether they were present on admission.

#-------------------------------------------------------------------------------

#####################################################################################

# Load packages and import data ----

# Install comorbidities package
# install.packages("comorbidity")

# Load required packages 
library(tidyverse)
library(comorbidity)

# Read in data
dx <- read_delim(here("data", "raw", "all encounter diagnoses 4.14.22.csv"), delim = "|")

# Remove last 2 rows 
dx <- dx[-c(nrow(dx)-1, nrow(dx)), ]

# View data 
head(dx)

# Filter where `present_on_admit` == "Yes" or "NULL" and select only ID and ICD code columns
comorbid <- dx %>% 
  filter(present_on_admit %in% c("Yes", "NULL")) %>% 
  select(deid_enc_id, current_icd10_list)

cat("Number of unique encounters:", n_distinct(comorbid$deid_enc_id))

#####################################################################################

# Run comorbidity() to compute weighted Charlson score and Charlson index ----

# Run comorbidity function to compute weighted Charlson score and weighted Charlson index
CCI <- comorbidity(comorbid, id ="deid_enc_id", code = "current_icd10_list",
                       score = "charlson", assign0 = T, icd = "icd10")

# Save `CCI' as CSV and RDS
write_csv(CCI, here("data", "CCI.csv"))
saveRDS(CCI, here("data", "CCI.rds"))

#####################################################################################

# Run comorbidity() to compute Elixhauser score ----
  # Same as above, but set score = "elixhauser"

# Run comorbidity function to compute Elixhauser score 
elixhauser <- comorbidity(comorbid, id ="deid_enc_id", code = "current_icd10_list",
                   score = "elixhauser", assign0 = T, icd = "icd10")

# Save `elixhauser' as CSV and RDS
write_csv(elixhauser, here("data", "elixhauser.csv"))
saveRDS(elixhauser, here("data", "elixhauser.rds"))

#####################################################################################

# R documentation for comorbidity package can be found here:
# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf

#####################################################################################

# Remove middle "." in  `current_icd10_list` variable 
# comorbid$current_icd10_list <- gsub("\\.", "", icd_only$current_icd10_list)
# Don't have to do this, since comorbidity::comorbidity() has an argument called 
# `tidy.codes` in which all ICD-10 codes are converted to upper
# case and all non-alphanumeric characters are removed using the expression
# [^[:alnum:]]. Defaults to TRUE.

#####################################################################################
