library(sf)
library(tidyverse)
library(tidymodels)
library(keras)
library(randomForest)
library(nnet)
library(missForest)

setwd("~/Desktop/DATA\ 440/Deliverables")

#Bringing in the shell data
load("swz_shell_data.RData")

#Bringing in and pivoting out the DHS data (to individuals)
load("DHS_desired_vars.RData")

#  code is copied/adapted from "modeling_households.R"
sex_ind <- dhs_desired_vars %>% 
  dplyr::select(hhid, hv005, hv009, hv024, starts_with("hv104_")) %>% #selecting out only the necessary vars
  pivot_longer(starts_with("hv104_"), names_to = "household_member", values_to = "sex") %>% #pivoting to long format... individuals for each obs
  mutate(household_member = household_member %>% str_replace("hv104_", "") %>% as.numeric(), #making member simply a num
         hhid = hhid %>% str_trim()) #trimming trailing/leading whitespace

age_ind <- dhs_desired_vars %>% #same process for age
  dplyr::select(hhid, hv005, hv009, hv024, starts_with("hv105_")) %>%
  pivot_longer(starts_with("hv105_"), names_to = "household_member", values_to = "age") %>%
  mutate(household_member = household_member %>% str_replace("hv105_", "") %>% as.numeric(),
         hhid = hhid %>% str_trim())

edu_ind <- dhs_desired_vars %>% #same process for education
  dplyr::select(hhid, hv005, hv009, hv024, starts_with("hv106_")) %>%
  pivot_longer(starts_with("hv106_"), names_to = "household_member", values_to = "educ_lvl") %>%
  mutate(household_member = household_member %>% str_replace("hv106_", "") %>% as.numeric(),
         hhid = hhid %>% str_trim())

dhs_ind <- sex_ind %>% 
  select(-hv024) %>% #taking out the region identifier
  bind_cols(dplyr::select(age_ind, age), dplyr::select(edu_ind, educ_lvl)) %>%
  filter(!(is.na(sex) & is.na(age) & is.na(educ_lvl))) #removing out the people that don't exist

#  imputing missing data
ind_data_imputed <- dhs_ind %>% #imputing missing data w/random forest method
  dplyr::select(sex, age, educ_lvl) %>% 
  as.matrix() %>%
  missForest(maxiter = 5, ntree = 100)

ind_data_final <- dhs_ind %>% #binding back onto original
  dplyr::select(-c(age, sex, educ_lvl)) %>% 
  bind_cols(ind_data_imputed$ximp %>% as.data.frame()) %>% 
  mutate(educ_lvl = round(educ_lvl), age = round(age))
save(ind_data_final, file = "dhs_pivoted_to_ind.RData")

#Train/test split
ind_data_split <- ind_data_final %>% 
  mutate(educ_lvl = as.factor(educ_lvl)) %>% #making sure the outcome var is a factor
  initial_split(prop = 0.7)

training <- ind_data_split %>% training()
testing <- ind_data_split %>% testing()

#Building a recipe for preprocessing
#  hv009 is household size
ind_recipe <- recipe(educ_lvl ~ sex + age + hv009, data = training) %>% #initiating the recipe
  step_corr(all_predictors()) %>% #removing vars w/large absolute correlation
  step_center(all_predictors(), -all_outcomes()) %>% #center vars about 0
  step_scale(all_predictors(), -all_outcomes()) %>% #scaling to std dev of 1
  prep()

ind_testing <- ind_recipe %>% bake(testing) #applying the recipe to the testing data
ind_training <- juice(ind_recipe) #extracting out the preprocessed training data

#Specifying a few diff models
#  these all take a hot sec to run
ind_mnlr <- multinom_reg(mode = "classification") %>%
  set_engine("nnet") %>% 
  fit(educ_lvl ~ sex + age + hv009, data = ind_training) #fitting the model
  
ind_rf <- rand_forest(mode = "classification", trees = 75) %>% #reducing num. of trees... my computer prob can't handle 100
  set_engine("randomForest") %>% 
  fit(educ_lvl ~ sex + age + hv009, data = ind_training) #fitting the model

#Validating the models
ind_mnlr %>% #for the multinomial log reg
  predict(ind_testing) %>% #predict on the preprocessed training set
  bind_cols(ind_testing) %>% #bind to training set
  metrics(truth = educ_lvl, estimate = .pred_class) #estimate accuracy and kappa stat

ind_rf %>% #for the random forest... much better accuracy than mnlr
  predict(ind_testing) %>% 
  bind_cols(ind_testing) %>% 
  metrics(truth = educ_lvl, estimate = .pred_class) 

#Visualizing metrics
ind_mnlr_probs <- ind_mnlr %>% #extracting class probs
  predict(ind_testing, type = "prob") %>%
  bind_cols(ind_testing)

ind_rf_probs <- ind_rf %>% 
  predict(ind_testing, type = "prob") %>%
  bind_cols(ind_testing)

#  plotting some of the metrics
ind_mnlr_probs %>% #gain curve
  gain_curve(educ_lvl, .pred_0, .pred_1, .pred_2, .pred_3, .pred_8) %>% 
  autoplot()

ind_rf_probs %>% 
  gain_curve(educ_lvl, .pred_0, .pred_1, .pred_2, .pred_3, .pred_8) %>% 
  autoplot()

ind_mnlr_probs %>% #gain curve
  roc_curve(educ_lvl, .pred_0, .pred_1, .pred_2, .pred_3, .pred_8) %>% 
  autoplot()

ind_rf_probs %>% 
  roc_curve(educ_lvl, .pred_0, .pred_1, .pred_2, .pred_3, .pred_8) %>% 
  autoplot()

#Predicting for the shell data
ind_shell_preproc <- ind_shell_data_final %>% 
  select(-educ_lvl) %>%
  bake(ind_recipe, .)

shell_pred <- ind_rf %>% #applying the random forest model to the shell data 
  predict(ind_shell_preproc) %>% 
  bind_cols(ind_shell_data_final) %>% 
  select(-.pred_class, .pred_class) %>%
  rename(educ_lvl_pred = .pred_class)
st_geometry(shell_pred) <- shell_pred$geometry #mending the geometry
save(shell_pred, file = "shell_pred.RData") #saving the final synth pop

#Prof Frazier includes a neural net... going to leave out, but could bring to see how it compares!