## libraries ##
library(tidyverse)
library(here)
library(fs)


## hand written functions ##
source(path(here(), "R", 'ccf_functions.R'))


# load sig pairs by coherence analysis #
all_sig_pairs <- read_csv(path(here(), "results", "sig_theta_pairs.csv"))

# read turn-locked hfa data
onset_lme_df <- read_csv(path(here(), "munge", "ccf_hfa_onset_lme_df.csv"))

# load data for permutations
all_subs_hfa_data <- read_csv(path(here(), "munge", "ccf_all_subs_hfa_df.csv"))


# MFG ~ HC


sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "mfg_hc") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))

mfg_hc_df <- onset_lme_df %>%
  filter(region %in% c("mfg", "hc"))


ccf_mfg_hc_results <- calculate_overall_ccf(sig_pairs, mfg_hc_df)
ccf_mfg_hc_null_results <- run_and_save_perms(mfg_hc_df, sig_pairs, all_subs_hfa_data, "null_ccf_mfg_hc_hfa_results.csv")

# save true version
write_csv(ccf_mfg_hc_results, path(here(), "results", "ccf_mfg_hc_hfa_results.csv"))


