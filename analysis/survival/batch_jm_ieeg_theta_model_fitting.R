### iEEG Batch Survival Script

## libraries ##
library(tidyverse)
library(ggplot2)
library(caret)
library(magrittr)
library(ggthemr)
library(grid)
library(gtable)
library(kableExtra)
library(lme4)
library(RColorBrewer)
library(doParallel)
library(parallel)
library(foreach)
library(here)
library(fs)
library(viridis)
library(lmtest)
library(survival)
library(effectsize)
library(scales)
library(JMbayes2)
library(rmarkdown)

## hand written functions ##
source(path(here(), "R", 'mutate_cond.R'))
source(path(here(), "R", "clean_behavioral_data.R"))
source(path(here(), "R", "create_distance_df.R"))
source(path(here(), "R", "joint_modeling_functions.R"))
source(path(here(), "R", "jm_visualization_functions.R"))
source(path(here(), "R", 'separate_mfg_sfg.R'))

## plotting helpers ##
ggthemr("light")
# getPalette = colorRampPalette(brewer.pal(17, "Set1"))

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
nCores <- 8
registerDoParallel(nCores)


## Load iEEG Data ##
hc_theta_data <- read_csv( path(here(), "munge", "theta_ieeg_hc_all_subs_logged_iti_onset.csv"))

# # separate dlpfc into sfg and mfg -- hfa
# dlpfc_hfa_data <- dlpfc_hfa_data %>%
#   mutate(electrode = gsub("_.*", "", electrode))
# dlpfc_hfa_data <- separate_mfg_sfg(dlpfc_hfa_data)
# sfg_hfa_data <- dlpfc_hfa_data %>% filter(sfg == 1) %>% select(-sfg, -mfg)
# mfg_hfa_data <- dlpfc_hfa_data %>% filter(mfg == 1) %>% select(-sfg, -mfg)

sub_list <- "BJH016"

### mfg hfa iEEG Model

failed_subjects <- c()
for(current_subject in sub_list){
  

  tryCatch({

    output_file_name <- paste("jm_time_theta_power_permuted_modeling_for_", current_subject, "_2025.html", sep = "")
    
    # Render the R Markdown document, passing the current subject as a parameter
    render(input = path(here(), "R", "jm_fit_ieeg_theta_plot_template.Rmd"),
           output_file = output_file_name,
           params = list(subject = current_subject, permuted = T),
           output_dir = path(here(), "analysis", "survival")
    )
    
   }, error = function(e) {
   # If an error occurs, print the error message and add the subject to the failed list
   message("Failed to render report for ", current_subject, ": ", e$message)
   failed_subjects <- c(failed_subjects, current_subject)
   })
 }


 print("failed subjects:")
 print(failed_subjects)






