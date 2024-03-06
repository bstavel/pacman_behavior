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
dlpfc_hfa_data <- read_csv( path(here(), "munge", "hfa_ieeg_dlpfc_all_subs_logged_iti_onset.csv"))

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


## dlpfc and hfa Onset Before Turnaround plot
individual_and_overall_robust_lme_onset_before_turn_model_and_plot("dlpfc", "hfa", 
                                all_subs_g_dist, dlpfc_hfa_data, 
                                y_low = -3, y_high = 3,
                                plot_title = "hfa ... dlPFC... Trial Onset",
                                rerun_model = TRUE)

individual_and_overall_robust_lme_onset_turnaround_model_and_plot("dlpfc", "hfa", 
                                all_subs_g_dist, dlpfc_hfa_data, 
                                y_low = -3, y_high = 3,
                                plot_title = "hfa ... dlPFC... Turnaround",
                                rerun_model = TRUE)

## subject specific
for (sub in unique(dlpfc_hfa_data$subject)) {

  print(sub)
  idx = which(sub == unique(dlpfc_hfa_data$subject))

  if(sub %in% c("BJH039", "BJH041", "LL13")){
    next
  }


  individual_subject_robust_lme_onset_before_turn_model_and_plot("dlpfc", "hfa", sub, c25[idx],
                                all_subs_g_dist, dlpfc_hfa_data, 
                                y_low = -3, y_high = 3,
                                plot_title = paste0("hfa, dlpfc, ", sub),
                                rerun_model = TRUE)
}                                


## Interaction Model

individual_and_overall_robust_interact_lme_onset_before_turn_model_and_plot("dlpfc", "hfa", 
                                all_subs_g_dist, dlpfc_hfa_data, 
                                reward_pred = "points_remaining", 
                                threat_pred = "distance_to_ghost",                                     
                                y_low = -12, y_high = 12,
                                plot_title = "Interaction Model: Points Remaining * Distance",
                                rerun_model = TRUE)

