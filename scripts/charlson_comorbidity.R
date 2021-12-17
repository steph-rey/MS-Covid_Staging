#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated - 2021-11-19
# @DateModified - 2021-11-19 at 5:07PM
# @Description - This file uses the comorbidity package to compute the (weighted)
# Charlson score and (weighted) Charlson index for each patient. 
# @SourceData - `covid diagnoses 11.08.21.csv` 
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

# Comorbidities (`covid diagnoses 11.08.2021.csv`)

# This file contains data on patients' comorbid diagnoses (inc. ICD-10 codes, 
# severity of DX, DX group) and whether they were present on admission.

#-------------------------------------------------------------------------------

#####################################################################################

# Section 1: Load packages and import data ----

# Install comorbidities package
# install.packages("comorbidity")

# Load required packages 
library(here)
library(tidyverse)
library(comorbidity)

# Read in data
comorbid <- read_delim(here("data", "raw", "covid diagnoses 11.08.21.csv"))

# Filter where `present_on_admit` == "Yes" or "NULL" and select only ID and ICD code columns
comorbid <- comorbid %>% 
  filter(present_on_admit %in% c("Yes", "NULL")) %>% 
  select(deid_enc_id, current_icd10_list)

#####################################################################################

# Section 2: Run comorbidity() to compute weighted Charlson score and Charlson index ----

# Remove middle "." in  `current_icd10_list` variable 
# comorbid$current_icd10_list <- gsub("\\.", "", icd_only$current_icd10_list)
# Don't have to do this, since comorbidity::comorbidity() has an argument called 
# `tidy.codes` in which all ICD-10 codes are converted to upper
# case and all non-alphanumeric characters are removed using the expression
# [^[:alnum:]]. Defaults to TRUE.

# Run comorbidity function to compute weighted Charlson score and weighted Charlson index
charlson_score <- comorbidity(comorbid, id ="deid_enc_id", code = "current_icd10_list",
                       score = "charlson", assign0 = T, icd = "icd10")

# Save `charlson_score' as .csv file 
write_csv(charlson_score, here("data", "charlson_wscore_windex.csv"))

#####################################################################################

# R documentation for comorbidity package can be found here:
# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf

#####################################################################################
