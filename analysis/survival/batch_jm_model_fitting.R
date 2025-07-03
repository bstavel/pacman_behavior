

## libraries ##
library(tidyverse)
library(ggplot2)
library(magrittr)
library(ggthemr)
library(grid)
library(gtable)
library(gridExtra)
library(wesanderson)
library(ggsci)
library(zoo)
library(kableExtra)
library(lme4)
library(RColorBrewer)
library(doParallel)
library(parallel)
library(foreach)
library(here)
library(fs)
library(ggcorrplot)
library(viridis)
library(lmtest)
library(gt)
library(survminer)
library(survival)
library(effectsize)
library(scales)
library(JMbayes2)
library(caret)
library(rmarkdown)

## hand written functions ##
source(path(here(), "R", 'mutate_cond.R'))
source(path(here(), "R", "clean_behavioral_data.R"))
source(path(here(), "R", "create_distance_df.R"))
source(path(here(), "R", "joint_modeling_functions.R"))
source(path(here(), "R", "jm_visualization_functions.R"))

## plotting helpers ##
ggthemr("light")

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
pilot_game_data_distance <- read_csv(path(here(), "munge", "prolific", "cleaned_pilot_distance_data.csv"))
ns_game_data_distance <- read_csv(path(here(), "munge", "prolific", "cleaned_pilot_distance_data_newsample.csv"))

## Bind Rows ##
# game_data_distance <- bind_rows(pilot_game_data_distance, ns_game_data_distance)
game_data_distance <- pilot_game_data_distance

no_converge_subs <- c("Subject_5")

failed_subjects <- c()
# for(current_subject in unique(game_data_distance$subject)){
for(current_subject in no_converge_subs){
  
  tryCatch({
    
    output_file_name <- paste("jm_base_true_modeling_for_", current_subject, ".html", sep = "")
    
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






