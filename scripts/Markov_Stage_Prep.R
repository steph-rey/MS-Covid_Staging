# Load packages
library(here)
library(tidyverse)
library(readr)

# Read in data
df <- read_csv(here("data", "dm_stg_clean_11.08.21.csv")) 

# Collapse levels of `stage` to Moderate (M), Severe (S), and Dead (D) --> `stagecat`
  # where "1" = Moderate (4-5), "2" = Severe (6-9), and "3" = Dead (10)
df$stagecat <- fct_collapse(as.character(df$stage),
                            "1" = c("4", "5"),
                            "2" = c("6", "7", "8", "9"),
                            "3" = "10")

# Reorder levels of stage var and convert to numeric type 
df$stagecat <- factor(df$stagecat, levels=c("1", "2", "3")) %>% as.numeric(df$stagecat)

# Create dataset for Markov Model 
df <- df %>%  
  select(ID, date, stagecat) %>%    #add `DOD` ??
  nest_by(ID) %>% 
  mutate(diff = stagecat - lag(stagecat)) %>% 
  unnest()

df$date[i] + 1

df2 <- nest_by(df, ID)






