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


# HC ~ Amygdala


sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "hc_amyg") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))

hc_amyg_df <- onset_lme_df %>%
  filter(region %in% c("hc", "amyg"))


ccf_hc_amyg_results <- calculate_overall_ccf(sig_pairs, hc_amyg_df)
ccf_hc_amyg_null_results <- run_and_save_perms(hc_amyg_df, sig_pairs, all_subs_hfa_data, "null_ccf_hc_amyg_hfa_results.csv")

# save true version
write_csv(ccf_hc_amyg_results, path(here(), "results", "ccf_hc_amyg_hfa_results.csv"))



# HC ~ ACC


sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "hc_cing") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))


hc_cing_df <- onset_lme_df %>%
  filter(region %in% c("hc", "cing"))

ccf_hc_cing_results <- calculate_overall_ccf(sig_pairs, hc_cing_df)
ccf_hc_cing_null_results <- run_and_save_perms(hc_cing_df, sig_pairs, all_subs_hfa_data, "null_ccf_hc_cing_hfa_results.csv")

# save true version
write_csv(ccf_hc_cing_results, path(here(), "results", "ccf_hc_cing_hfa_results.csv"))




# OFC ~ Amyg


sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "ofc_amyg") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))

ofc_amyg_df <- onset_lme_df %>%
  filter(region %in% c("ofc", "amyg"))

ccf_ofc_amyg_results <- calculate_overall_ccf(sig_pairs, ofc_amyg_df)
ccf_ofc_amyg_null_results <- run_and_save_perms(ofc_amyg_df, sig_pairs, all_subs_hfa_data, "null_ccf_ofc_amyg_hfa_results.csv")

# save true version
write_csv(ccf_ofc_amyg_results, path(here(), "results", "ccf_ofc_amyg_hfa_results.csv"))



# OFC ~ HC


sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "ofc_hc") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))

ofc_hc_df <- onset_lme_df %>%
  filter(region %in% c("ofc", "hc"))


ccf_ofc_hc_results <- calculate_overall_ccf(sig_pairs, ofc_hc_df)
ccf_ofc_hc_null_results <- run_and_save_perms(ofc_hc_df, sig_pairs, all_subs_hfa_data, "null_ccf_ofc_hc_results.csv", perms_run = 50)

# save true version
write_csv(ccf_ofc_hc_results, path(here(), "results", "ccf_ofc_hc_hfa_results.csv"))


# OFC ~ ACC


sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "ofc_cing") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))

ofc_acc_df <- onset_lme_df %>%
  filter(region %in% c("ofc", "cing"))


ccf_ofc_acc_results <- calculate_overall_ccf(sig_pairs, ofc_acc_df)
ccf_ofc_acc_null_results <- run_and_save_perms(ofc_acc_df, sig_pairs, all_subs_hfa_data, "null_ccf_ofc_acc_results.csv", perms_run = 36)

# save true version
write_csv(ccf_ofc_acc_results, path(here(), "results", "ccf_ofc_acc_hfa_results.csv"))





# OFC ~ MFG


sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "ofc_mfg") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))

ofc_mfg_df <- onset_lme_df %>%
  filter(region %in% c("ofc", "mfg"))


ccf_ofc_mfg_results <- calculate_overall_ccf(sig_pairs, ofc_mfg_df)
ccf_ofc_mfg_null_results <- run_and_save_perms(ofc_mfg_df, sig_pairs, all_subs_hfa_data, "null_ccf_ofc_mfg_hfa_results.csv")

# save true version
write_csv(ccf_ofc_mfg_results, path(here(), "results", "ccf_ofc_mfg_hfa_results.csv"))






# MFG ~ ACC


sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "mfg_cing") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))

mfg_cing_df <- onset_lme_df %>%
  filter(region %in% c("mfg", "cing"))


ccf_mfg_cing_results <- calculate_overall_ccf(sig_pairs, mfg_cing_df)
ccf_mfg_cing_null_results <- run_and_save_perms(mfg_cing_df, sig_pairs, all_subs_hfa_data, "null_ccf_mfg_cing_hfa_results.csv")

# save true version
write_csv(ccf_mfg_cing_results, path(here(), "results", "ccf_mfg_cing_hfa_results.csv"))




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



# MFG ~ Amyg

sig_pairs <- all_sig_pairs %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  filter(metric == "Imaginary Coherence") %>%
  filter(roi_pair == "mfg_amyg") %>%
  mutate(pair2 = paste0(subject, "_", gsub(".*_to_", "", pairs))) %>%
  mutate(pair1 = paste0(subject, "_", gsub("_to_.*", "", pairs)))

mfg_amyg_df <- onset_lme_df %>%
  filter(region %in% c("mfg", "amyg"))


ccf_mfg_amyg_results <- calculate_overall_ccf(sig_pairs, mfg_amyg_df)
ccf_mfg_amyg_null_results <- run_and_save_perms(mfg_amyg_df, sig_pairs, "null_ccf_mfg_amyg_hfa_results.csv")

# save true version
write_csv(ccf_mfg_amyg_results, path(here(), "results", "ccf_mfg_amyg_hfa_results.csv"))


