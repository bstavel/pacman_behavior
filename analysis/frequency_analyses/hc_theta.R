
## libraries ##
library(tidyverse)
library(ggplot2)
library(lmerTest)
library(doParallel)
library(parallel)
library(foreach)
library(here)
library(fs)
library(lmtest)
library(scales)
library(ggthemr)
library(RColorBrewer)

## hand written functions ##
source(path(here(), "R", 'mutate_cond.R'))
source(path(here(), "R", 'create_distance_df.R'))
source(path(here(), "R", 'clean_behavioral_data.R'))
source(path(here(), "R", 'compile_ieeg_csv_files.R'))
source(path(here(), "R", 'run_and_plot_lme_models.R'))

## plotting helpers ##
ggthemr("light")
ghost_colors <- c("#E48DB7","#55BBC8", "#DE0D16")
getPalette = colorRampPalette(brewer.pal(17, "Set1"))

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

# ## parallelization ##
nCores <- 16
registerDoParallel(nCores)


# ieeg data #
hc_theta_data <- read_csv( path(here(), "munge", "theta_ieeg_hc_all_subs_logged_iti_onset.csv"))

# behavioral data #
all_subs_g_dist <- read_csv(path(here(), "munge", "all_subs_complete_distance_df.csv"))
all_subs_ng_dist <- read_csv(path(here(), "munge", "all_subs_noghost_complete_distance_df.csv"))

# combine ghost and no ghost #
all_subs_dist <- all_subs_g_dist %>% 
  select(-GhostLocation, -distance_to_ghost, -min_distance, -cdf_distance)
all_subs_dist <- rbind(all_subs_dist, all_subs_ng_dist)

# add ITI to behavioral df
behavior_iti_df <- all_subs_dist %>% 
  select(subject, trial_numeric, trial_time) %>%
  filter(trial_time < 1) %>%
  mutate(trial_time = round(trial_time - 1, 2))

all_subs_dist <- full_join(all_subs_dist, behavior_iti_df)


# ## Combined Model ##
# # Onset
# individual_and_overall_robust_lme_onset_before_turn_model_and_plot("hc", "theta", 
#                                 all_subs_g_dist, hc_theta_data, 
#                                 y_low = -.5, y_high = .5,
#                                 plot_title = "Theta ... hc... Trial Onset",
#                                 rerun_model = TRUE)

# # Turnaround
# individual_and_overall_robust_lme_turnaround_model_and_plot("hc", "theta", 
#                                 all_subs_g_dist, hc_theta_data, 
#                                 y_low = -.5, y_high = .5,
#                                 plot_title = "Theta ... hc... Turnaround",
#                                 rerun_model = TRUE)

# ## Indiviudal Predictors ##
# # Distance to Ghost
# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("hc", "theta", 
#                                 all_subs_g_dist, hc_theta_data, 
#                                 predictor = "distance_to_ghost",
#                                 y_low = -.5, y_high = .5,
#                                 plot_title = "Theta ... hc... Turnaround",
#                                 rerun_model = TRUE)

# # Points Remaining
# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("hc", "theta", 
#                                 all_subs_g_dist, hc_theta_data, 
#                                 predictor = "points_remaining",
#                                 y_low = -.5, y_high = .5,
#                                 plot_title = "Theta ... hc... Turnaround",
#                                 rerun_model = TRUE)

# Distance to Ghost - Turnaround
individual_and_overall_lme_onset_combval_turnaround_model_and_plot("hc", "theta", 
                                all_subs_g_dist, hc_theta_data, 
                                predictor = "distance_to_ghost",
                                y_low = -.5, y_high = .5,
                                plot_title = "Theta ... hc... Turnaround",
                                rerun_model = TRUE)

# Points Remaining  - Turnaround
individual_and_overall_lme_onset_combval_turnaround_model_and_plot("hc", "theta", 
                                all_subs_g_dist, hc_theta_data, 
                                predictor = "points_remaining",
                                y_low = -.5, y_high = .5,
                                plot_title = "Theta ... hc... Turnaround",
                                rerun_model = TRUE)

# ## Split Ghost Close/Far Model
# individual_and_overall_robust_split_lme_onset_before_turn_model_and_plot("hc", "theta", 
#                                 all_subs_g_dist, hc_theta_data, 
#                                 y_low = -.5, y_high = .5,
#                                 plot_title = "Theta ... hc... Trial Onset",
#                                 rerun_model = TRUE)

# ## subject specific
# for (sub in unique(hc_theta_data$subject)[12:14]) {

#   print(sub)
#   idx = which(sub == unique(hc_theta_data$subject))

#   if(sub == "LL13"){
#     continue
#   }


#   individual_subject_robust_lme_onset_before_turn_model_and_plot("hc", "theta", sub, c25[idx],
#                                 all_subs_g_dist, hc_theta_data, 
#                                 y_low = -3, y_high = 3,
#                                 plot_title = paste0("Theta, HC, ", sub),
#                                 rerun_model = FALSE)
# }                                


# ## Interaction Model

# individual_and_overall_robust_interact_lme_onset_before_turn_model_and_plot("hc", "theta", 
#                                 all_subs_g_dist, hc_theta_data, 
#                                 reward_pred = "points_remaining", 
#                                 threat_pred = "distance_to_ghost",                                     
#                                 y_low = -12, y_high = 12,
#                                 plot_title = "Interaction Model: Points Remaining * Distance",
#                                 rerun_model = TRUE)


