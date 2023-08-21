### Batch Script for dlPFC Trial Onset Correlations ###

## libraries ##
library(tidyverse)
library(ggplot2)
library(magrittr)
library(grid)
library(gtable)
library(RColorBrewer)
library(doParallel)
library(parallel)
library(foreach)
library(here)
library(fs)
library(scales)

## hand written functions ##
source(path(here(), "R", 'mutate_cond.R'))
source(path(here(), "R", 'compute_lfp_correlation.R'))


# ## parallelization ##
nCores <- 16
registerDoParallel(nCores)


## Load Region specific csvs ## 
hc_onset_df <- read_csv(path(here(), "munge", "theta_behavior_hc_all_subs_onset.csv"))
amyg_onset_df <- read_csv(path(here(), "munge", "theta_behavior_amyg_all_subs_onset.csv"))
ofc_onset_df <- read_csv(path(here(), "munge", "theta_behavior_ofc_all_subs_onset.csv"))
cing_onset_df <- read_csv(path(here(), "munge", "theta_behavior_cing_all_subs_onset.csv"))
dlpfc_onset_df <- read_csv(path(here(), "munge", "theta_behavior_dlpfc_all_subs_onset.csv"))
insula_onset_df <- read_csv(path(here(), "munge", "theta_behavior_insula_all_subs_onset.csv"))

## Bind together ##
theta_onset_df <- rbind(hc_onset_df %>% mutate(region = "hc"),
                             amyg_onset_df %>% mutate(region = "amyg"),
                             ofc_onset_df %>% mutate(region = "ofc"),
                             cing_onset_df %>% mutate(region = "cing"),
                             dlpfc_onset_df %>% mutate(region = "dlpfc"),
                             insula_onset_df %>% mutate(region = "insula"))

# Only First Second
theta_onset_df <- theta_onset_df %>%
  filter(trial_time >= 0 & trial_time < 1) %>%
  mutate(elec_short = gsub("_.*", "", electrode)) %>%
  filter(!(subject == "LL13" & elec_short == "LH1-LH2"))

## Subject List ##
sub_list <- c("LL10", "LL13", "LL12", "BJH021", "BJH025", "SLCH002", "BJH016")
sub_list_short <- c("LL13", "LL12", "BJH021", "BJH025", "SLCH002", "BJH016")


## Compute Correlation ##
# compute_lfp_correlation(sub_list_short, "dlpfc", "hc", theta_onset_df)
compute_lfp_correlation(sub_list, "hc", "amyg", theta_onset_df)
compute_lfp_correlation(sub_list, "hc", "ofc", theta_onset_df)
compute_lfp_correlation(sub_list, "hc", "cing", theta_onset_df)
compute_lfp_correlation(sub_list, "hc", "insula", theta_onset_df)




