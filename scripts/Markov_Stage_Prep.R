# @Title: Prep Covid Staging Data for Markov Model
# @Project: Mindscape
# @DateCreated: 01-06-2022
# @DateModified: 01-07-2022

# Load packages
library(tidyverse)
library(msm)

# Import data
df <- read_csv("/Users/stephanie.reynolds/Downloads/dm_stg_clean_11.08.21.csv")

# Transform stages to stage categories where:
# stages 4-5 = 1 (moderate)
# stages 6-9 = 2 (severe)
# stage 10 = 3 (dead)
# discharged = 4 ???
df <- df %>%
  mutate(stagecat = case_when(stage %in% c(4, 5) ~ 1,
                              stage %in% 6:9 ~ 2,
                              stage == 10 ~ 3)) %>%
  select(ID, date, stagecat)

# Prep dataset for Markov Model
# For each 'ID', create new row to represent discharge state
# Bind to dataset and arrange by 'ID' and 'date'
# Set stagecat to '4' or 'discharged'
markov <- df %>%
  group_by(ID) %>%
  summarize(date = max(date) + 1) %>%
  mutate(stagecat = NA) %>%
  rbind(df) %>%
  arrange(ID, date) %>%
  mutate(stagecat = case_when(is.na(stagecat) ~ 4,
                              TRUE ~ stagecat))

# Run statetable.msm fxn
statetable.msm(state = stagecat, subject = ID, data = markov)

# Create matrix to show allowed transitions where:
# 1 = allowed transitions
# 0 = forbidden transitions
qmat0 <- matrix(c(1,1,1,1,
                  1,1,1,1,
                  0,0,0,1,
                  0,0,0,0),
                nrow = 4, ncol = 4,
                byrow=TRUE,
                dimnames=list(from=1:4, to=1:4))
# Print qmatrix
qmat0

# Determine crude estimate of transition rates
crudeinits.msm(formula = stagecat ~ date, subject = ID, data = markov, qmatrix = qmat0)

# Run msm fxn
# Set obstype = 2
msm(stagecat ~ date, ID, data = markov, qmatrix = qmat0, gen.inits = TRUE, obstype = 2)
# ERROR




########### DISCARD - PRIOR ATTEMPTS ###########
#switch ID to integer format? then try statetable.msm again
df$ID <- as.factor(df$ID)
levels(df$ID) <- 1:n_distinct(df$ID) #same results as when ID was character type




#df_tmw <- df %>%                            # Create copy of 'df'
mutate(date = date - 1) %>%               # Change date so it represents day before
  rename(tmwstage = stagecat)               # Rename column to 'tmwstage'

# Left join 'df' table to 'df_tmw' on 'ID' and 'date'
#markov <- left_join(df, df_tmw, by = c("ID", "date")) %>%
mutate(tmwstage = case_when(is.na(tmwstage) ~ "Discharged",
                            TRUE ~ tmwstage))
