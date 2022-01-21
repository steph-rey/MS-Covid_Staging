# Linear Regression (LOS ~ stage_adm) by Location (UCSF vs. UTSW)

# Import utsw dataset
ut <- readRDS("/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/UTSW/dm_stg_unique_pts.rds")

# Import ucsf data; remove DOD and smoking vars
uc <- readRDS("/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/dm_stg_unique_pt_11.08.21.rds") %>%
  select(-c(DOD, smoking)) 

# Merge datasets using location (loc) as identifier 
combined <- bind_rows("UTSW" = ut, "UCSF" = uc, .id = "loc") %>% 
  relocate(loc, .after = "ID")

# Load packages required for multiple linear regressions 
library(tidymodels)
library(tidyquant)
library(broom)
library(purrr)
library(tidyverse)

# Group split by location `loc` to create iterables for UCSF and UTSW, then generate linear regression for each iterable
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
  





