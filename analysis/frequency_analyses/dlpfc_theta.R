
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
dlpfc_theta_data <- read_csv( path(here(), "munge", "theta_ieeg_dlpfc_all_subs_logged_iti_onset.csv"))

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


# dlpfc and Theta Onset Before Turnaround plot
individual_and_overall_robust_lme_onset_before_turn_model_and_plot("dlpfc", "theta", 
                                all_subs_g_dist, dlpfc_theta_data, 
                                y_low = -6, y_high = 6,
                                plot_title = "Theta encodes some threat values in the dlpfc at trial onset",
                                rerun_model = FALSE)

## subject specific
individual_subject_robust_lme_onset_before_turn_model_and_plot("dlpfc", "theta", "BJH016", "#E48DB7",
                                all_subs_g_dist, dlpfc_theta_data, 
                                y_low = -6, y_high = 6,
                                plot_title = "Theta, dlPFC, BJH016",
                                rerun_model = TRUE)

individual_subject_robust_lme_onset_before_turn_model_and_plot("dlpfc", "theta", "BJH021", "#FCC673",
                                all_subs_g_dist, dlpfc_theta_data, 
                                y_low = -6, y_high = 6,
                                plot_title = "Theta, dlPFC, BJH021",
                                rerun_model = TRUE)

individual_subject_robust_lme_onset_before_turn_model_and_plot("dlpfc", "theta", "BJH025", "#55BBC8",
                                all_subs_g_dist, dlpfc_theta_data, 
                                y_low = -6, y_high = 6,
                                plot_title = "Theta, dlPFC, BJH025",
                                rerun_model = TRUE)

individual_subject_robust_lme_onset_before_turn_model_and_plot("dlpfc", "theta", "LL10", "#palegreen2",
                                all_subs_g_dist, dlpfc_theta_data, 
                                y_low = -6, y_high = 6,
                                plot_title = "Theta, dlPFC, LL10",
                                rerun_model = TRUE)

individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "LL12", "#deeppink1",
                                all_subs_g_dist, dlpfc_theta_data, 
                                y_low = -6, y_high = 6,
                                plot_title = "Theta, dlPFC, LL12",
                                rerun_model = TRUE)

individual_subject_robust_lme_onset_before_turn_model_and_plot("OFC", "theta", "LL13", "#brown",
                                all_subs_g_dist, dlpfc_theta_data, 
                                y_low = -6, y_high = 6,
                                plot_title = "Theta, dlPFC, LL13",
                                rerun_model = TRUE)

individual_subject_robust_lme_onset_before_turn_model_and_plot("dlpfc", "theta", "SLCH002", "#6A3D9A",
                                all_subs_g_dist, dlpfc_theta_data, 
                                y_low = -6, y_high = 6,
                                plot_title = "Theta, dlpfc, SLCH002",
                                rerun_model = TRUE)


# individual_and_overall_robust_lme_onset_turnaround_model_and_plot("dlpfc", "theta", 
#                                 all_subs_g_dist, dlpfc_theta_data, 
#                                 y_low = -5, y_high = 5,
#                                 plot_title = "Theta encodes threat values adn some reward in the dlpfc after Turnaround",
#                                 rerun_model = TRUE)