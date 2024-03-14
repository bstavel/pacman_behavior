### batch files for joint modeling ###

## libraries ##
library(tidyverse)
library(ggplot2)
library(magrittr)
library(ggthemr)
library(grid)
library(gtable)
library(gridExtra)
library(wesanderson)
library(ggsci)
library(zoo)
library(kableExtra)
library(lme4)
library(RColorBrewer)
library(doParallel)
library(parallel)
library(foreach)
library(here)
library(fs)
library(ggcorrplot)
library(viridis)
library(lmtest)
library(gt)
library(survminer)
library(survival)
library(effectsize)
library(scales)
library(JMbayes2)
library(caret)

## hand written functions ##
source(path(here(), "R", 'mutate_cond.R'))
source(path(here(), "R", "clean_behavioral_data.R"))
source(path(here(), "R", "create_distance_df.R"))
source(path(here(), "R", "joint_modeling_functions.R"))
## plotting helpers ##
ggthemr("light")
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)


## Load Data ##
hc_theta_data <- read_csv( path(here(), "munge", "theta_ieeg_hc_all_subs_logged_iti_onset.csv"))
game_data_distance <-  read_csv(path(here(), "munge", "all_subs_complete_distance_df.csv"))

## Only Bob data #
game_data_distance <- game_data_distance %>%
  filter(attack_chase_bob == 'Bob') 

## Clean ieeg data ##
game_data_distance <- game_data_distance %>%
  filter(subject != "BJH026") %>% # no hc data
  filter(trial_time <= 5.10) 


## Permuted? ##
permuted <- FALSE
seed <- 78
final_sub_list <-  unique(game_data_distance$subject)

for(current_subject in final_sub_list){
  
  print(current_subject)
  
  ## Prep DF for Joint Model Fitting and select predictor variables ##
  joint_dist_df <- prep_joint_df(game_data_distance, current_subject, permuted, ieeg = FALSE)
  
  hc_sub_trials <- hc_theta_data %>%
    filter(subject == current_subject) %>%
    pull(trial_numeric) %>%
    unique()
  
  joint_dist_df <- joint_dist_df %>%
    filter(trial_numeric %in% hc_sub_trials)
  
  ## Create Test/Train ##
  split_df <- joint_dist_df %>% select(trial_numeric, turnaround_time) %>% distinct()
  split_index <- create_test_train(split_df, seed)
  train_trials <- split_df[split_index, 'trial_numeric'] %>% pull(trial_numeric)
  test_trials <- split_df[-split_index, 'trial_numeric'] %>% pull(trial_numeric)
  
  ## Merge with the neural predictors ##
  joint_theta_dist_df <- prep_for_nerual_predictors(joint_dist_df, hc_theta_data)
    
  ## Prep Train/Test DFs ##
  # longitudinal dfs
  train_long_data <- joint_theta_dist_df %>%
    filter(trial_numeric %in% train_trials)
  test_long_data <- joint_theta_dist_df %>%
    filter(trial_numeric %in% test_trials)
  
  # survival dfs
  cox_df <- create_survival_df(joint_theta_dist_df)
  train_cox_df <- cox_df %>%
    filter(trial_numeric %in% train_trials)
  test_cox_df <- cox_df %>%
    filter(trial_numeric %in% test_trials)

  ## Fit Joint Model ##
  # file_name <- paste0(current_subject, "_hc_theta-basic_", seed, "_", permuted)
  # fit_joint_models(train_long_data, train_cox_df, file_name)
  file_name <- paste0(current_subject, "_hc_theta_", seed, "_", permuted)
  fit_joint_theta_models(train_long_data, train_cox_df, file_name)

    
}

