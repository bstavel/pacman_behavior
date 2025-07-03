
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

## hand written functions ##
source(path(here(), "R", 'mutate_cond.R'))
source(path(here(), "R", "bayesian_helpers.R"))

### Load and Prep DF ###

# load correlations 
correlation_df <- read_csv(path(here(), "results", "theta_correlations_newsubs.csv"))

## separate dlpfc data into mfg and sfg data ##

# load localization data
regions_df <- read_csv(path(here(), "munge", "mni_coordinates_all_subs_with_detailed_regions.csv"))

mfg_df <- regions_df %>%
  filter(region == "mfg") %>%
  mutate(elec_id = paste0(subject, "_", Electrode))

sfg_df <- regions_df %>%
  filter(region == "sfg") %>%
  mutate(elec_id = paste0(subject, "_", Electrode))

correlation_detailed_df <- correlation_df %>%
  rowwise() %>%
  mutate(
    first_pair = gsub(paste0(subject, "_"), "", elec1),
    second_pair = gsub(paste0(subject, "_"), "", elec2),
    first_pair_1 = gsub("-.*", "", first_pair),
    first_pair_2 = gsub(".*-", "", first_pair),
    second_pair_1 = gsub("-.*", "", second_pair),
    second_pair_2 = gsub(".*-", "", second_pair),
  ) %>%
  ungroup() %>%
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


# final rename
correlation_detailed_df <- correlation_detailed_df %>%
  mutate(first_region = detailed_first_region, 
         second_region = detailed_second_region) %>%
  select(-detailed_first_region, -detailed_second_region)

# fix pair id to fit with sig pairs
correlation_df <- correlation_detailed_df %>%
  rowwise() %>%
  mutate(roi_pair1 = paste0(subject, gsub(subject, "", elec1), "_to", gsub(subject, "", elec2)),
         roi_pair2 = paste0(subject, gsub(subject, "", elec2), "_to", gsub(subject, "", elec1))) %>%
  ungroup()


## get list of sig theta pairs ##

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

sig_50_list <- sub_elec_lists %>% filter(threshold == 50) %>% distinct()


#### Run 50 Threshold Model ####

## prep brms ##

# Set the number of cores for parallel processing
options(mc.cores = parallel::detectCores())

# set the priors #
priors <- c(
  prior(normal(0, 5), class = "Intercept"),            # Prior for the intercept
  prior(normal(0, 2), class = "b"),                    # Prior for fixed effects
  prior(exponential(1), class = "sd"),                 # Prior for random effects standard deviations
  prior(lkj(2), class = "cor")                         # Prior for random effects correlations
)


## Fit the model
model <- brm(
  formula = scale_correlation ~ scale_logged_times * region_pair + (1 + scale_logged_times | subject/roi_pair1),
  data = correlation_sig_df,
  prior = priors,
  family = gaussian(),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  control = list(adapt_delta = 0.99),
  seed = 1234
)

## save full model ##
save(model, file = path(here(), "results", "full_hfa_all_roi_50_elecs_model_brms.RData"))





