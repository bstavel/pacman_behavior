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

## plotting helpers ##
ggthemr("light")
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
nCores <- 8
registerDoParallel(nCores)


## Load Data ##
hc_theta_data <- read_csv( path(here(), "munge", "theta_ieeg_hc_all_subs_logged_iti_onset.csv"))
game_data_distance <-  read_csv(path(here(), "munge", "all_subs_complete_distance_df.csv"))

## Clean ieeg data ##
game_data_distance <- game_data_distance %>%
  filter(subject != "BJH026") %>% # no hc data
  filter(trial_time <= 5.10) 


### Base Model, iEEG Sample

failed_subjects <- c()
for(current_subject in unique(game_data_distance$subject)){
  
  tryCatch({
    
    output_file_name <- paste("jm_base_dir_modeling_for_", current_subject, ".html", sep = "")
    
    # Render the R Markdown document, passing the current subject as a parameter
    render(input = path(here(), "R", "jm_fit_plot_template.Rmd"),
           output_file = output_file_name,
           params = list(subject = current_subject),
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






