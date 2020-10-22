library(sf)
library(tidyverse)
library(tidymodels)
library(keras)
library(randomForest)
library(nnet)

setwd("~/Desktop/DATA\ 440/Deliverables")

#TODO: use SURVEY data for training + testing (needs to be pivoted), THEN apply to shell data

#Bringing in the shell data
load("swz_shell_data.RData")

#Train/test split
shell_data_split <- ind_shell_data_final %>% 
  mutate(educ_lvl = as.factor(educ_lvl)) %>% #making sure the outcome var is a factor
  st_set_geometry(NULL) %>% #removing the geometry... can be added back on after the fact, but causes problems!
  initial_split(prop = 0.7)

training <- shell_data_split %>% training()
testing <- shell_data_split %>% testing()

#Building a recipe for preprocessing
#  hv009 is household size
shell_recipe <- recipe(educ_lvl ~ sex + age + hv009, data = training) %>% #initiating the recipe
  step_corr(all_predictors()) %>% #removing vars w/large absolute correlation
  step_center(all_predictors(), -all_outcomes()) %>% #center vars about 0
  step_scale(all_predictors(), -all_outcomes()) %>% #scaling to std dev of 1
  prep()

shell_testing <- shell_recipe %>% bake(testing) #applying the recipe to the testing data
shell_training <- juice(shell_recipe) #extracting out the preprocessed training data

#Specifying a few diff models
#  these all take a hot sec to run
shell_mnlr <- multinom_reg(mode = "classification") %>%
  set_engine("nnet") %>% 
  fit(educ_lvl ~ sex + age + hv009, data = shell_training) #fitting the model
  
shell_rf <- rand_forest(mode = "classification", trees = 50) %>% #reducing num. of trees... my computer prob can't handle 100
  set_engine("randomForest") %>% 
  fit(educ_lvl ~ sex + age + hv009, data = shell_training) #fitting the model

#Validating the models
shell_mnlr %>% #for the multinomial log reg
  predict(shell_testing) %>% #predict on the preprocessed training set
  bind_cols(shell_testing) %>% #bind to training set
  metrics(truth = educ_lvl, estimate = .pred_class) #estimate accuracy and kappa stat

shell_rf %>% #for the random forest... much better accuracy than mnlr
  predict(shell_testing) %>% 
  bind_cols(shell_testing) %>% 
  metrics(truth = educ_lvl, estimate = .pred_class) 

#Visualizing metrics
shell_mnlr_probs <- shell_mnlr %>% #extracting class probs
  predict(shell_testing, type = "prob") %>%
  bind_cols(shell_testing)

shell_rf_probs <- shell_rf %>% 
  predict(shell_testing, type = "prob") %>%
  bind_cols(shell_testing)

#  plotting some of the metrics
shell_mnlr_probs %>% #gain curve
  gain_curve(educ_lvl, .pred_0:.pred_1:.pred_2:.pred_3:.pred_8) %>% 
  filter(.level %in% c(0, 1, 2, 3, 8)) %>% #not sure if this is valid... but makes it much neater
  autoplot()

shell_rf_probs %>% 
  gain_curve(educ_lvl, .pred_0:.pred_1:.pred_2:.pred_3:.pred_8) %>% 
  filter(.level %in% c(0, 1, 2, 3, 8)) %>% 
  autoplot()

shell_mnlr_probs %>% #gain curve
  roc_curve(educ_lvl, .pred_0:.pred_1:.pred_2:.pred_3:.pred_8) %>% 
  filter(.level %in% c(0, 1, 2, 3, 8)) %>% #not sure if this is valid... but makes it much neater
  autoplot()

shell_rf_probs %>% 
  roc_curve(educ_lvl, .pred_0:.pred_1:.pred_2:.pred_3:.pred_8) %>% 
  filter(.level %in% c(0, 1, 2, 3, 8)) %>% 
  autoplot()

#Prof Frazier includes a neural net... going to leave out, but could bring to see how it compares!