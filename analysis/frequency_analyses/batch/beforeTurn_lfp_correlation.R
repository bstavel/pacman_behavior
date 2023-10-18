### Batch Script for After Turnaround Correlations ###

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
hc_turnaround_df <- read_csv(path(here(), "munge", "theta_hc_all_subs_turnaround.csv"))
amyg_turnaround_df <- read_csv(path(here(), "munge", "theta_amyg_all_subs_turnaround.csv"))
ofc_turnaround_df <- read_csv(path(here(), "munge", "theta_ofc_all_subs_turnaround.csv"))
cing_turnaround_df <- read_csv(path(here(), "munge", "theta_cing_all_subs_turnaround.csv"))
dlpfc_turnaround_df <- read_csv(path(here(), "munge", "theta_dlpfc_all_subs_turnaround.csv"))
insula_turnaround_df <- read_csv(path(here(), "munge", "theta_insula_all_subs_turnaround.csv"))

## Bind together ##
theta_turnaround_df <- rbind(hc_turnaround_df %>% mutate(region = "hc"),
                        amyg_turnaround_df %>% mutate(region = "amyg"),
                        ofc_turnaround_df %>% mutate(region = "ofc"),
                        cing_turnaround_df %>% mutate(region = "cing"),
                        dlpfc_turnaround_df %>% mutate(region = "dlpfc"),
                        insula_turnaround_df %>% mutate(region = "insula"))

# Only First Second
theta_turnaround_df <- theta_turnaround_df %>%
  mutate(trial_time = trial_time - 1) %>%
  filter(trial_time >= -1 & trial_time < 0) %>%
  mutate(elec_short = gsub("_.*", "", electrode)) %>%
  filter(!grepl("noghost", electrode)) %>%
  filter(!(subject == "LL13" & elec_short == "LH1-LH2"))

## Subject List ##
sub_list <- c("LL10", "LL13", "LL12", "BJH021", "BJH025", "SLCH002", "BJH016")


## Compute Correlation ##

# dlPFC #
try({
    compute_lfp_correlation(sub_list, "dlpfc", "hc", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "dlpfc", "ofc", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "dlpfc", "cing", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "dlpfc", "amyg", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "dlpfc", "insula", theta_turnaround_df, "before_only")
    }, TRUE)

# HC #
try({
    compute_lfp_correlation(sub_list, "hc", "ofc", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "hc", "cing", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "hc", "amyg", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "hc", "insula", theta_turnaround_df, "before_only")
    }, TRUE)
# OFC #
try({
    compute_lfp_correlation(sub_list, "ofc", "cing", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "ofc", "amyg", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "ofc", "insula", theta_turnaround_df, "before_only")
    }, TRUE)
# Cingulate #
try({
    compute_lfp_correlation(sub_list, "cing", "amyg", theta_turnaround_df, "before_only")
    }, TRUE)
try({
    compute_lfp_correlation(sub_list, "cing", "insula", theta_turnaround_df, "before_only")
    }, TRUE)

# Amygdala #
try({
    compute_lfp_correlation(sub_list, "amyg", "insula", theta_turnaround_df, "before_only")
    }, TRUE)
