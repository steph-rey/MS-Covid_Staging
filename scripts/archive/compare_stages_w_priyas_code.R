
#-------------------------------------------------------------------------------
# @Project - MINDSCAPE: Modeling of infectious network dynamics for surveillance, control and prevention enhancement
# @Author - Steph Reynolds (Stephanie.Reynolds@ucsf.edu)
# @DateCreated: 2021-12-03
# @DateModified: 2021-12-03 at 
# @Description - This file imports `stages_df_11.08.2021.csv` which contains WHO Clinical Stages of Severity (4-10) for each patient on each day of stay, and compares it with Priya R.'s version with staging. 
# @SourceData - `stages_df_11.08.2021.csv`

#-------------------------------------------------------------------------------

# Load packages, import data, and initial cleaning ----

# Load required packages 
library(here)
library(tidyverse)
library(readxl)
library(data.table)

# Read in data
stages_df <- read_csv(here("data", "stages_df_11.08.21.csv"))
priya <- read_excel(here("data", "dm_covid_LOS_stg.xlsx")) 
priya$...1 <- NULL

#################################################################################

# Run anti_join on Priya and my stages dataset ----
stages_df <- as.data.table(stages_df)
priya <- as.data.table(priya)

fintersect(stages_df, priya)

# Compare Priya's staging file with mine 
diff <- anti_join(priya, stages_df, by = c("deid_enc_id" = "ID", "day_date" = "date"))

    # There are 27 rows that don't perfectly align

# Inspect `diff`
compare_diff <- left_join(diff, stages_df, by = c("deid_enc_id" = "ID", "day_date" = "date")) 

#################################################################################

# Create separate datasets that filter by stage ----

# Filter stages_df for STAGE==10 and check to make sure only those with DEATH==1 are included
stg10 <- filter(stages_df, STAGE==10)

# Filter stages_df for STAGE==9 and check to make sure only those with INTUB or IVDEV==1 AND SF_LT_200==1 AND VP or CRRT==1 are included
stg9 <- filter(stages_df, STAGE==9)

# Filter stages_df for STAGE==8
stg8 <- filter(stages_df, STAGE==8) %>% 
  arrange(VP, CRRT, SF_LT_200, IVDEV)

# Filter stages_df for STAGE==7
stg7 <- filter(stages_df, STAGE==7)

# Filter stages_df for STAGE==6
stg6 <- filter(stages_df, STAGE==6)

# Filter stages_df for STAGE==5
stg5 <- filter(stages_df, STAGE==5)

#################################################################################

# Compare summary statistics on STAGE variable in both datasets ----
summary(priya$Stage)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 4.000   4.000   5.000   5.741   7.000  10.000 
summary(stages_df$STAGE)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 4.000   4.000   4.000   4.688   5.000  10.000 

#################################################################################
# END OF DOCUMENT ---- 
#################################################################################
