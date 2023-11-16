
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

## hand written functions ##
source(path(here(), "R", 'mutate_cond.R'))
source(path(here(), "R", 'create_distance_df.R'))
source(path(here(), "R", 'clean_behavioral_data.R'))
source(path(here(), "R", 'compile_ieeg_csv_files.R'))
source(path(here(), "R", 'run_and_plot_lme_models.R'))

## plotting helpers ##
ggthemr("light")
ghost_colors <- c("#E48DB7","#55BBC8", "#DE0D16")

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
ofc_theta_data <- read_csv( path(here(), "munge", "theta_ieeg_ofc_all_subs_logged_iti_onset.csv"))

# behavioral data #
all_subs_g_dist <- read_csv(path(here(), "munge", "all_subs_distance_df.csv"))
all_subs_ng_dist <- read_csv(path(here(), "munge", "all_subs_noghost_distance_df.csv"))

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



#OFC and Theta Onset Before Turnaround plot
individual_and_overall_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", 
                                all_subs_g_dist, ofc_theta_data, 
                                y_low = -2.5, y_high = 3,
                                plot_title = "Theta correlates with reward and threat values in the OFC \nat trial onset",
                                rerun_model = FALSE)

# subject specific
# individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "BJH016", "#E48DB7",
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -6, y_high = 6,
#                                 plot_title = "Theta, OFC, BJH016",
#                                 rerun_model = TRUE)

# individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "BJH021", "#FCC673",
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -6, y_high = 6,
#                                 plot_title = "Theta, OFC, BJH021",
#                                 rerun_model = TRUE)

# individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "BJH025", "#55BBC8",
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -6, y_high = 6,
#                                 plot_title = "Theta, OFC, BJH025",
#                                 rerun_model = TRUE)
# individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "LL10", "#palegreen2",
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -6, y_high = 6,
#                                 plot_title = "Theta, OFC, LL10",
#                                 rerun_model = TRUE)
# individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "LL12", "#deeppink1",
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -6, y_high = 6,
#                                 plot_title = "Theta, OFC, LL12",
#                                 rerun_model = TRUE)
# individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "LL13", "#brown",
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -6, y_high = 6,
#                                 plot_title = "Theta, OFC, LL13",
#                                 rerun_model = TRUE)

# individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "SLCH002", "#6A3D9A",
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -6, y_high = 6,
#                                 plot_title = "Theta, OFC, SLCH002",
#                                 rerun_model = TRUE)


# #OFC and Theta Onset at Turnaround plot
# individual_and_overall_robust_lme_onset_turnaround_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -5, y_high = 5,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = TRUE)


# #OFC and Theta Onset Discounted value
# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "discounted_value", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = FALSE)

# #OFC and Theta Onset Combined value
# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "combined_value", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = FALSE)

# #OFC and Theta Onset Combined value
# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "UserLocation", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = TRUE)

# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "GhostLocation", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = TRUE)

# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "discounted_reward", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = FALSE)

# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "discounted_4reward", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = TRUE)

# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "discounted_goal", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = TRUE)

# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "points_remaining_goal", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = TRUE)

# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "discounted_value_goal", 
#                                 y_low = -7, y_high = 7,
#                                 plot_title = "Theta does not encode threat or reward values in the OFC after Turnaround",
#                                 rerun_model = TRUE)


# individual_and_overall_robust_interact_lme_onset_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -15, y_high = 15,
#                                 plot_title = "Interaction Model",
#                                 rerun_model = TRUE)

# individual_and_overall_robust_interact_lme_onset_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 reward_pred = "points_remaining", 
#                                 threat_pred = "distance_to_ghost",                                     
#                                 y_low = -12, y_high = 12,
#                                 plot_title = "Interaction Model: Points Remaining * Distance",
#                                 rerun_model = FALSE)


# individual_and_overall_robust_interact_lme_onset_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 reward_pred = "points_remaining_goal", 
#                                 threat_pred = "distance_to_ghost",                                     
#                                 y_low = -12, y_high = 12,
#                                 plot_title = "Interaction Model: Goal Points Remaining * Distance",
#                                 rerun_model = TRUE)


# individual_and_overall_lme_onset_combval_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 predictor = "reward_threat_diff", 
#                                 y_low = -4, y_high = 4,
#                                 plot_title = "Points Remaining - Distance to Ghost",
#                                 rerun_model = FALSE)

## equal time bins

# individual_and_overall_robust_equal_time_lme_onset_before_turn_model_and_plot("OFC", "theta", 
#                                 all_subs_g_dist, ofc_theta_data, 
#                                 y_low = -10, y_high = 10,
#                                 plot_title = "Theta encodes reward and threat values in the OFC at trial onset",
#                                 rerun_model = TRUE)