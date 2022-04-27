# Linear Regression (LOS ~ stage_adm) by Location (UCSF vs. UTSW)

# Import utsw data
ut <- readRDS("/Users/sreynolds2/Documents/GitHub/MS-UTSW_Covid_Staging/data/ut_df_unique_pts.rds")

# Import ucsf data and remove unnecessary variables
uc <- readRDS("/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/covid_dm_stages_unique_FINAL_4.14.22.rds") %>%
  select(-c(marital_status, language, smoking, date_adm, date_disc)) 

# For UTSW, change max_stage, stage_adm, and stage_disc variables to ORDERED factor type
ut$max_stage <- factor(ut$max_stage, ordered = T, levels = c("4", "5", "6", "7", "8", "9", "10"))
ut$stage_adm <- factor(ut$stage_adm, ordered = T, levels = c("4", "5", "6", "7", "8", "9"))
ut$stage_disc <- factor(ut$stage_disc, ordered = T, levels = c("4", "5", "6", "7", "8", "9", "10"))

# Merge datasets using location (loc) as identifier 
combined <- bind_rows("UTSW" = ut, "UCSF" = uc, .id = "loc") %>% 
  relocate(loc, .after = "ID")

# Load packages required for multiple linear regression 
library(tidymodels)
library(tidyquant)
library(broom)
library(purrr)
library(tidyverse)

# Group split by location `loc` to create iterables for UCSF and UTSW, then generate linear regression for each iterable
# LOS ~ stage_adm
combined %>% 
  mutate(loc = as.factor(loc)) %>% 
  group_by(loc) %>% 
  group_split() %>% 
  
  # Iterate with map() to generate linear regression and summarize regression output 
  map_dfr(.f = function(df) {
    lm(LOS ~ stage_adm, data = df) %>% 
      glance() %>% 
      add_column(loc = unique(df$loc), .before = 1)
  })

####
# GROUP SPLIT BY STAGE_ADM
combined %>% 
  group_by(stage_adm) %>% 
  group_split() %>% 
  
  # ITERATE W/ MAP()
  map_dfr(.f = function(df) {
    lm(LOS ~ age, data = df) %>% 
      glance() %>% 
      add_column(stage_adm = unique(df$stage_adm), .before = 1)
  })
  
