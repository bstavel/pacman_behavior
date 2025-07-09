### Approach Theta Coherence ~ Time ###

## libraries ##
library(tidyverse)
library(ggplot2)
library(doParallel)
library(parallel)
library(foreach)
library(here)
library(fs)
library(scales)
library(brms)

## load data ##
conn_df <- read_csv(path(here(), "munge", "combined_ghost_connectivity_newsubs.csv"))

## split the dlpfc electrodes into MFG and SFG ##
regions_df <- read_csv(path(here(), "munge", "mni_coordinates_all_subs_with_detailed_regions.csv"))

mfg_df <- regions_df %>%
  filter(region == "mfg") %>%
  mutate(elec_id = paste0(subject, "_", Electrode))

sfg_df <- regions_df %>%
  filter(region == "sfg") %>%
  mutate(elec_id = paste0(subject, "_", Electrode))

conn_detailed_df <- conn_df %>%
  mutate(
    first_pair = gsub("_to_.*", "", pairs),
    second_pair = gsub(".*_to_", "", pairs),
    first_pair_1 = gsub("-.*", "", first_pair),
    first_pair_2 = gsub(".*-", "", first_pair),
    second_pair_1 = gsub("-.*", "", second_pair),
    second_pair_2 = gsub(".*-", "", second_pair),
  ) %>%
  mutate(
    detailed_first_region = if_else(
      first_region == "dlpfc" &
        paste0(subject, "_", first_pair_1) %in% mfg_df$elec_id |
        paste0(subject, "_", first_pair_2) %in%  mfg_df$elec_id,
      "mfg",
      if_else(
        first_region == "dlpfc" &
          paste0(subject, "_", first_pair_1) %in% sfg_df$elec_id |
          paste0(subject, "_", first_pair_2) %in%  sfg_df$elec_id,
        "sfg",
        first_region
      )),
    detailed_second_region = if_else(
      second_region == "dlpfc" &
        paste0(subject, "_", second_pair_1) %in% mfg_df$elec_id |
        paste0(subject, "_", second_pair_2) %in%  mfg_df$elec_id,
      "mfg",
      if_else(
        second_region == "dlpfc" &
          paste0(subject, "_", second_pair_1) %in% sfg_df$elec_id |
          paste0(subject, "_", second_pair_2) %in%  sfg_df$elec_id,
        "sfg",
        second_region
      )
    )
  )


## Check that everything worked correctly
table(conn_detailed_df$detailed_first_region)
table(conn_detailed_df$detailed_second_region)

# final rename
conn_detailed_df <- conn_detailed_df %>%
  mutate(first_region = detailed_first_region, 
         second_region = detailed_second_region)


# pull the sig electrode key ##
# load threshold csv 
sig_thresh_df <- read_csv(path(here(), "results", "sig_theta_threshold_pairs.csv"))

# separate significance lists
sub_elec_lists <- sig_thresh_df %>%
  # get rid of unused regions
  filter(!grepl("sfg", roi_pair)) %>%
  filter(!grepl("insula", roi_pair)) %>%
  filter(roi_pair != "mfg_mfg") %>%
  mutate(pair_id = paste0(subject, "_", pairs)) %>%
  mutate(sig = "True") %>%
  select(pair_id, roi_pair, threshold, sig) 

sig_50_list <- sub_elec_lists %>% filter(threshold == "50") %>% distinct()
sig_100_list <- sub_elec_lists %>% filter(threshold == "100") %>% distinct()
sig_10a_list <- sub_elec_lists %>% filter(threshold == "10a") %>% distinct()


avd_time_window <- 2
app_time_window <- -2

# ## All Electrodes ##
# 
# ## create the approach df ##
# app_sig_elec_df <- conn_detailed_df %>%
#   mutate(roi_pair = paste0(first_region, "_", second_region)) %>%
#   filter(time >= app_time_window & time <= 0) %>%
#   mutate(key = paste0(subject, "_", pairs)) %>%
#   mutate(time = round(time, 1)) %>%
#   group_by(key, time) %>%
#   mutate(connectivity = mean(abs(connectivity))) %>%
#   select(-percent_sig) %>%
#   distinct() %>%
#   # get rid of unused regions
#   filter(!grepl("sfg", roi_pair)) %>%
#   filter(!grepl("insula", roi_pair)) %>%
#   filter(first_region != second_region) %>%
#   ## unify roi names
#   mutate(
#     roi_pair = case_when(
#       grepl("ofc", roi_pair) & grepl("hc", roi_pair) ~ "ofc_hc",
#       grepl("ofc", roi_pair) & grepl("amyg", roi_pair) ~ "ofc_amyg",
#       grepl("ofc", roi_pair) & grepl("mfg", roi_pair) ~ "ofc_mfg",
#       grepl("ofc", roi_pair) & grepl("cing", roi_pair) ~ "ofc_cing",
#       grepl("hc", roi_pair) & grepl("amyg", roi_pair) ~ "hc_amyg",
#       grepl("hc", roi_pair) & grepl("mfg", roi_pair) ~ "hc_mfg",
#       grepl("hc", roi_pair) & grepl("cing", roi_pair) ~ "hc_cing",
#       grepl("amyg", roi_pair) & grepl("mfg", roi_pair) ~ "amyg_mfg",
#       grepl("amyg", roi_pair) & grepl("cing", roi_pair) ~ "amyg_cing",
#       grepl("mfg", roi_pair) & grepl("cing", roi_pair) ~ "mfg_cing"
#     ))
# 
# 
# imcoh_app_sig_df <- app_sig_elec_df %>%
#   ungroup() %>%
#   filter(metric == "Imaginary Coherence") %>%
#   mutate(scaled_log_connectivity = scale(log(connectivity))[,1]) %>%
#   select(subject, time, scaled_log_connectivity, connectivity, pairs, first_pair, second_pair, roi_pair, first_region, second_region, key) %>%
#   distinct()
# 
# 
# 
# # Set the number of cores for parallel processing
# options(mc.cores = parallel::detectCores(), brms.backend = "cmdstanr")
# 
# # set the priors #
# priors <- c(
#   prior(normal(0, 5), class = "Intercept"),            # Prior for the intercept
#   prior(normal(0, 2), class = "b"),                    # Prior for fixed effects
#   prior(exponential(1), class = "sd"),                 # Prior for random effects standard deviations
#   prior(lkj(2), class = "cor")                         # Prior for random effects correlations
# )
# 
# 
# # Fit the model
# rising_model <- brm(
#   formula = scaled_log_connectivity ~ time+ (1 + time | subject/key),
#   data = imcoh_app_sig_df,
#   prior = priors,
#   family = gaussian(),
#   iter = 5000,
#   warmup = 2000,
#   chains = 4,
#   control = list(adapt_delta = 0.99),
#   seed = 1234
# )
# 
# 
# # save rising model #
# save(rising_model, file = path(here(), "results", paste0(avd_time_window, "_rising_model_all_elecs_brms.RData")))
# 
# 
# # summary #
# summary(rising_model)


# ### 500ms Threshold ###
# 
# ## create the approach df ##
# app_sig_elec_df <- conn_detailed_df %>%
#   mutate(roi_pair = paste0(first_region, "_", second_region)) %>%
#   filter(time >= app_time_window & time <= 0) %>%
#   mutate(key = paste0(subject, "_", pairs)) %>%
#   filter(key %in% sig_50_list$pair_id) %>%
#   mutate(time = round(time, 1)) %>%
#   group_by(key, time) %>%
#   mutate(connectivity = mean(abs(connectivity))) %>%
#   select(-percent_sig) %>%
#   distinct() %>%
#   # get rid of unused regions
#   filter(!grepl("sfg", roi_pair)) %>%
#   filter(!grepl("insula", roi_pair)) %>%
#   filter(first_region != second_region) %>%
#   ## unify roi names
#   mutate(
#     roi_pair = case_when(
#       grepl("ofc", roi_pair) & grepl("hc", roi_pair) ~ "ofc_hc",
#       grepl("ofc", roi_pair) & grepl("amyg", roi_pair) ~ "ofc_amyg",
#       grepl("ofc", roi_pair) & grepl("mfg", roi_pair) ~ "ofc_mfg",
#       grepl("ofc", roi_pair) & grepl("cing", roi_pair) ~ "ofc_cing",
#       grepl("hc", roi_pair) & grepl("amyg", roi_pair) ~ "hc_amyg",
#       grepl("hc", roi_pair) & grepl("mfg", roi_pair) ~ "hc_mfg",
#       grepl("hc", roi_pair) & grepl("cing", roi_pair) ~ "hc_cing",
#       grepl("amyg", roi_pair) & grepl("mfg", roi_pair) ~ "amyg_mfg",
#       grepl("amyg", roi_pair) & grepl("cing", roi_pair) ~ "amyg_cing",
#       grepl("mfg", roi_pair) & grepl("cing", roi_pair) ~ "mfg_cing"
#     ))
# 
# 
# imcoh_app_sig_df <- app_sig_elec_df %>%
#   ungroup() %>%
#   filter(metric == "Imaginary Coherence") %>%
#   mutate(scaled_log_connectivity = scale(log(connectivity))[,1]) %>%
#   select(subject, time, scaled_log_connectivity, connectivity, pairs, first_pair, second_pair, roi_pair, first_region, second_region, key) %>%
#   distinct()
# 
# # Set the number of cores for parallel processing
# options(mc.cores = parallel::detectCores(), brms.backend = "cmdstanr")
# 
# # set the priors #
# priors <- c(
#   prior(normal(0, 5), class = "Intercept"),            # Prior for the intercept
#   prior(normal(0, 2), class = "b"),                    # Prior for fixed effects
#   prior(exponential(1), class = "sd"),                 # Prior for random effects standard deviations
#   prior(lkj(2), class = "cor")                         # Prior for random effects correlations
# )
# 
# 
# # Fit the model
# rising_model <- brm(
#   formula = scaled_log_connectivity ~ time+ (1 + time | subject/key),
#   data = imcoh_app_sig_df,
#   prior = priors,
#   family = gaussian(),
#   iter = 5000,
#   warmup = 2000,
#   chains = 4,
#   control = list(adapt_delta = 0.99),
#   seed = 1234
# )
# 
# 
# # save rising model #
# save(rising_model, file = path(here(), "results", paste0(avd_time_window, "_rising_model_50_elecs_brms.RData")))
# 
# 
# # summary #
# summary(rising_model)


### 1000ms Threshold ###

## create the approach df ##
app_sig_elec_df <- conn_detailed_df %>%
  mutate(roi_pair = paste0(first_region, "_", second_region)) %>%
  filter(time >= app_time_window & time <= 0) %>%
  mutate(key = paste0(subject, "_", pairs)) %>%
  filter(key %in% sig_100_list$pair_id) %>%
  mutate(time = round(time, 1)) %>%
  group_by(key, time) %>%
  mutate(connectivity = mean(abs(connectivity))) %>%
  select(-percent_sig) %>%
  distinct() %>%
  # get rid of unused regions
  filter(!grepl("sfg", roi_pair)) %>%
  filter(!grepl("insula", roi_pair)) %>%
  filter(first_region != second_region) %>%
  ## unify roi names
  mutate(
    roi_pair = case_when(
      grepl("ofc", roi_pair) & grepl("hc", roi_pair) ~ "ofc_hc",
      grepl("ofc", roi_pair) & grepl("amyg", roi_pair) ~ "ofc_amyg",
      grepl("ofc", roi_pair) & grepl("mfg", roi_pair) ~ "ofc_mfg",
      grepl("ofc", roi_pair) & grepl("cing", roi_pair) ~ "ofc_cing",
      grepl("hc", roi_pair) & grepl("amyg", roi_pair) ~ "hc_amyg",
      grepl("hc", roi_pair) & grepl("mfg", roi_pair) ~ "hc_mfg",
      grepl("hc", roi_pair) & grepl("cing", roi_pair) ~ "hc_cing",
      grepl("amyg", roi_pair) & grepl("mfg", roi_pair) ~ "amyg_mfg",
      grepl("amyg", roi_pair) & grepl("cing", roi_pair) ~ "amyg_cing",
      grepl("mfg", roi_pair) & grepl("cing", roi_pair) ~ "mfg_cing"
    ))


imcoh_app_sig_df <- app_sig_elec_df %>%
  ungroup() %>%
  filter(metric == "Imaginary Coherence") %>%
  mutate(scaled_log_connectivity = scale(log(connectivity))[,1]) %>%
  select(subject, time, scaled_log_connectivity, connectivity, pairs, first_pair, second_pair, roi_pair, first_region, second_region, key) %>%
  distinct()

# Set the number of cores for parallel processing
options(mc.cores = parallel::detectCores(), brms.backend = "cmdstanr")

# set the priors #
priors <- c(
  prior(normal(0, 5), class = "Intercept"),            # Prior for the intercept
  prior(normal(0, 2), class = "b"),                    # Prior for fixed effects
  prior(exponential(1), class = "sd"),                 # Prior for random effects standard deviations
  prior(lkj(2), class = "cor")                         # Prior for random effects correlations
)


# Fit the model
rising_model <- brm(
  formula = scaled_log_connectivity ~ time+ (1 + time | subject/key),
  data = imcoh_app_sig_df,
  prior = priors,
  family = gaussian(),
  iter = 5000,
  warmup = 2000,
  chains = 4,
  control = list(adapt_delta = 0.99),
  seed = 1234
)


# save rising model #
save(rising_model, file = path(here(), "results", paste0(avd_time_window, "_rising_model_100_elecs_brms.RData")))


# summary #
summary(rising_model)



# ### 100ms, Approach Only Threshold ###
# 
# ## create the approach df ##
# app_sig_elec_df <- conn_detailed_df %>%
#   mutate(roi_pair = paste0(first_region, "_", second_region)) %>%
#   filter(time >= app_time_window & time <= 0) %>%
#   mutate(key = paste0(subject, "_", pairs)) %>%
#   filter(key %in% sig_10a_list$pair_id) %>%
#   mutate(time = round(time, 1)) %>%
#   group_by(key, time) %>%
#   mutate(connectivity = mean(abs(connectivity))) %>%
#   select(-percent_sig) %>%
#   distinct() %>%
#   # get rid of unused regions
#   filter(!grepl("sfg", roi_pair)) %>%
#   filter(!grepl("insula", roi_pair)) %>%
#   filter(first_region != second_region) %>%
#   ## unify roi names
#   mutate(
#     roi_pair = case_when(
#       grepl("ofc", roi_pair) & grepl("hc", roi_pair) ~ "ofc_hc",
#       grepl("ofc", roi_pair) & grepl("amyg", roi_pair) ~ "ofc_amyg",
#       grepl("ofc", roi_pair) & grepl("mfg", roi_pair) ~ "ofc_mfg",
#       grepl("ofc", roi_pair) & grepl("cing", roi_pair) ~ "ofc_cing",
#       grepl("hc", roi_pair) & grepl("amyg", roi_pair) ~ "hc_amyg",
#       grepl("hc", roi_pair) & grepl("mfg", roi_pair) ~ "hc_mfg",
#       grepl("hc", roi_pair) & grepl("cing", roi_pair) ~ "hc_cing",
#       grepl("amyg", roi_pair) & grepl("mfg", roi_pair) ~ "amyg_mfg",
#       grepl("amyg", roi_pair) & grepl("cing", roi_pair) ~ "amyg_cing",
#       grepl("mfg", roi_pair) & grepl("cing", roi_pair) ~ "mfg_cing"
#     ))
# 
# 
# imcoh_app_sig_df <- app_sig_elec_df %>%
#   ungroup() %>%
#   filter(metric == "Imaginary Coherence") %>%
#   mutate(scaled_log_connectivity = scale(log(connectivity))[,1]) %>%
#   select(subject, time, scaled_log_connectivity, connectivity, pairs, first_pair, second_pair, roi_pair, first_region, second_region, key) %>%
#   distinct()
# 
# # Set the number of cores for parallel processing
# options(mc.cores = parallel::detectCores(), brms.backend = "cmdstanr")
# 
# # set the priors #
# priors <- c(
#   prior(normal(0, 5), class = "Intercept"),            # Prior for the intercept
#   prior(normal(0, 2), class = "b"),                    # Prior for fixed effects
#   prior(exponential(1), class = "sd"),                 # Prior for random effects standard deviations
#   prior(lkj(2), class = "cor")                         # Prior for random effects correlations
# )
# 
# 
# # Fit the model
# rising_model <- brm(
#   formula = scaled_log_connectivity ~ time+ (1 + time | subject/key),
#   data = imcoh_app_sig_df,
#   prior = priors,
#   family = gaussian(),
#   iter = 8000,
#   warmup = 4000,
#   chains = 4,
#   control = list(adapt_delta = 0.99),
#   seed = 1234
# )
# 
# 
# # save rising model #
# save(rising_model, file = path(here(), "results", paste0(avd_time_window, "_rising_model_10a_elecs_brms.RData")))
# 
# 
# # summary #
# summary(rising_model)
