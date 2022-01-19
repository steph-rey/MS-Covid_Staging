# Linear Regression with tidymodels, vip, and performance packages
# Model: Can LOS be explained by max stage, age, race, and BMI? 
# Formula: LOS ~ max_stage + age + race + BMI
# Data: Demographics and Daily Covid Staging Data

# Load required packages ---- 
library(tidyverse)
library(tidymodels)
library(performance) # check model assumptions and performance
library(vip) # calculate and visualize feature importance 

# Import `df_CDI_risk_model.csv` and assign to `df` ----
df <- read_csv("/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/dm_stg_unique_pt_11.08.21.csv")

# Assign list of variables to be converted to factor type 
vars_fct <- c("sex", "race", "ethnicity", "smoking", "death", "max_stage", "stage_adm", "stage_disc")

# Convert these variables to factor type
df <- df %>% 
  mutate_at(vars_fct, factor)

# Create dataset with only relevant parameters and assign to `regdf`
regdf <- df %>% 
  select(-c(ID, zip, DOD, date_adm, date_disc, stage_disc))

# Exploratory data analysis (EDA) with dlookr ---- 
dlookr::diagnose_web_report(regdf)

# Fit the model and assign to `m1` ----
m1 <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(LOS ~ max_stage + age + race + BMI, data = regdf)

m1

# Need help with testing interactions in diff models and comparing their performance 

# Check model assumptions ----
check_model(m1)

# Check model performance ---- 
model_performance(m1)

# Calculate and visualize feature importance using vip package ----
  # Visualize most important features 
  m1 %>% 
    vip(num_features = 15,
        geom = "point",
        aes = list(size = 3, color = '18bc9c')) + 
    theme_minimal(base_size = 14) + 
    labs(title = "Logistic Regression: Feature Importance")
  # Features with highest importance are: max_stage 6-10

  # Visualize top features (max_stage and stage_adm)
  regdf %>% ggplot(aes(LOS, max_stage)) +
    geom_boxplot() + 
    geom_jitter(alpha = 0.3) + 
    theme_minimal(base_size = 16) +
    #scale_color_viridis_d(end = 0.9) + 
    labs(title = "Comparison of Predicted LOS\nusing Max Stage as Key Predictor")

# End of Document ----

# More code, but didn't work - KEEP FOR LATER IN CASE EDIT ----
  
  # Create train and test splits ----
  # Set seed to ensure reproducibility 
  set.seed(123)
  
  # Create initial splits 
  splits <- regdf %>% initial_split(prop = 0.8)
  
  # View splits - can also view the training and testing splits individualy
  splits
  training(splits)
  testing(splits)
  
  # Fit the model and assign to `m1` ----
  m1 <- linear_reg() %>% 
    set_engine("lm") %>% 
    fit(LOS ~ max_stage + age + race + BMI, data = training(splits))
  
  m1
  
  # Predict class and numeric probabilities on testing data ---- 
  # Predict class (whether or not pt predicted to have cdiff)
  prediction_class_test <- predict(m1, new_data = testing(splits), type = 'class')
  prediction_class_test
  
  # Predict numeric probabilities for each class (Y/N cdiff)
  prediction_prob_test <- predict(m1, new_data = testing(splits), type = 'prob')
  prediction_prob_test
  
  # Create table that binds predicted class and probs
  # Will be used to evaluate model in next step 
  results_tbl <- bind_cols(prediction_class_test, prediction_prob_test, testing(splits))
  results_tbl
  
  # Evaluate model performance: AUC and ROC ----
  # Plot AUC 
  results_tbl %>% roc_auc(cdif, .pred_0)
  # AUC = 0.845
  
  # Visualize ROC 
  results_tbl %>% roc_curve(cdif, .pred_0) %>% 
    autoplot()

