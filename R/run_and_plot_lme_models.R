individual_and_overall_robust_lme_onset_before_turn_model_and_plot <- function(region, freq, distance_df, brain_df, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
  if(rerun_model == TRUE){
    
    ## Run LME Multiple Regression Models with two predictors: Distance to Ghost, Points Remaining
    
    # merge behavior data with ieeg data
    brain_behave_df <- left_join(distance_df %>% 
                                     select(-move_step) %>% mutate(trial_time = round(trial_time, 2)) %>% 
                                     filter(!(subject == "SLCH002" & trial_numeric == 220)), 
                                 brain_df %>% mutate(trial_time = round(trial_time, 2)))
    
    
    # prep for linear mixed effects models #
    brain_behave_lme_df <- brain_behave_df %>%
      # round time so that we can loop over time later
      mutate(trial_time = round(trial_time, 2)) %>%
      # exclude time before they started moving, may be good to look at not doing this as well
      filter(Direction != "Still") %>%
      # filiter out ITI
      filter(reward_groups != 99 & !is.na(electrode)) %>%
      # prep electrode variables
      mutate(elec_id = paste0(subject, "_", gsub("_.*", "", electrode))) %>%
      mutate(electrode = gsub("_.*", "", electrode)) %>%
      # shift the time so that time 0 is the start of movement onset
      group_by(elec_id, trial_numeric, subject) %>%
      mutate(move_time = round(trial_time - first(trial_time), 2)) %>%
      # filter time so that trials end when the person turnaround for the final time
      mutate(event = if_else(last_away == away_choice & away_choice != 0, 1, 0)) %>%
      mutate(event = replace(event, is.na(event), 0)) %>%
      mutate(turnaround_time = if_else(event == 1, move_time, 0)) %>%
      filter(move_time < max(turnaround_time)) %>%
      ungroup() %>%
      # filter to max time 3 since after 3s we start losing a lot of trials
      filter(move_time < 3) %>%
      # select and rescale the necessary variables for the model
      select(subject, elec_id, theta, trial_numeric, move_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = rescale(points_remaining)) %>%
      mutate(distance_to_ghost = rescale(distance_to_ghost)) 

    
    # Parallelized loop over steps
    results <- foreach(step=sort(unique(brain_behave_lme_df$move_time)), .combine=rbind, .packages=c("dplyr", "robustlmm", "lmerTest", "tibble")) %dopar% {
      
      # prep model result dfs - distance to ghost
      dist_to_g_results <- tibble(
        case = character(0),
        time = numeric(0),
        Estimate = numeric(0),
        `Std. Error` = numeric(0),
        `t value` = numeric(0),
        `Pr(>|t|)` = numeric(0)
      )
      
      # prep model result dfs - points remaining
      points_r_results <- tibble(
        case = character(0),
        time = numeric(0),
        Estimate = numeric(0),
        `Std. Error` = numeric(0),
        `t value` = numeric(0),
        `Pr(>|t|)` = numeric(0)
      )
      
      
      print(step)
      
      # filter to only the necessary timepoint
      time_step_df <- brain_behave_lme_df %>%
        filter(move_time == step)
      
      # fit the model with random effects of subject and electrode
      model_all <- summary(robustlmm::rlmer(theta ~ distance_to_ghost + points_remaining + (1|subject/elec_id), data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject/elec_id), data = time_step_df))
      coefs <- data.frame(coef(model_dfs))
      coefs.robust <- coef(model_all)
      p.values <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      # add to results - distance
      dist_to_g_results <- rbind(dist_to_g_results, 
                                 tibble(
                                   case = "all_subs",
                                   time = step, 
                                   Estimate = model_all$coefficients[2, "Estimate"],
                                   `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                   `t value` = model_all$coefficients[2, "t value"],
                                   `Pr(>|t|)` = p.values[2]
                                 ))
      
      # add to results - points remaining
      points_r_results <- rbind(points_r_results, 
                                tibble(
                                  case = "all_subs",
                                  time = step, 
                                  Estimate = model_all$coefficients[3, "Estimate"],
                                  `Std. Error` = model_all$coefficients[3, "Std. Error"],
                                  `t value` = model_all$coefficients[3, "t value"],
                                  `Pr(>|t|)` = p.values[3]
                                ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          
          model_sub <- summary(MASS::rlm(theta ~ distance_to_ghost + points_remaining, data = time_step_df %>% filter(elec_id == elec)))
          
          # calculate p value by getting dfs from satteraite approximated dfs
          coefs.robust <- coef(model_sub)
          p.values <- 2*pt(abs(coefs.robust[, "t value"]), model_sub$df[2], lower=FALSE)
          
          dist_to_g_results <- rbind(dist_to_g_results, 
                                     tibble(
                                       case = elec,
                                       time = step, 
                                       Estimate = model_sub$coefficients[2, "Value"],
                                       `Std. Error` = model_sub$coefficients[2, "Std. Error"],
                                       `t value` = model_sub$coefficients[2, "t value"],
                                       `Pr(>|t|)` = p.values[2]
                                     ))
          
          # add to results
          points_r_results <- rbind(points_r_results, 
                                    tibble(
                                      case = elec,
                                      time = step, 
                                      Estimate = model_sub$coefficients[3, "Value"],
                                      `Std. Error` = model_sub$coefficients[3, "Std. Error"],
                                      `t value` = model_sub$coefficients[3, "t value"],
                                      `Pr(>|t|)` = p.values[3]
                                    )) 
        }, TRUE)
      }
      
      return(list(dist_to_g_results_step = dist_to_g_results, points_r_results_step = points_r_results))
    }
    
    
    points_r_results <- bind_rows(results[, "points_r_results_step"])
    dist_to_g_results <- bind_rows(results[, "dist_to_g_results_step"]) 
    
    # consolidate different model results #
    model_results_onset_before_turn_all_subs <- rbind(dist_to_g_results %>% mutate(pred = "dist"),
                                                      points_r_results %>% mutate(pred = "points"))
    
    # save for later #
    write_csv(model_results_onset_before_turn_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_before_turn_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- model_results_onset_before_turn_all_subs %>%
    # FDR correct across time points
    group_by(case) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # mutate(pval_fdr = `Pr(>|t|)`) %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = if_else(grepl("dist", pred), "Distance to Ghost", "Points Remaining")) %>%
    mutate(main = if_else(case == "all_subs", "All Subjects", gsub("_.*", "", case))) %>%
    mutate(large_line = if_else(case == "all_subs", "all_subs", "indv_lines")) %>%
    # plot results
    ggplot(., aes(x = time, y = Estimate)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = main, alpha = large_line), size = 3) +
    geom_line(aes(color = main, group = case, size = large_line, alpha = large_line)) +
    geom_label(aes(x = sig), label = "*", color = "black", y = (y_high -.3), label.size = 0.5) +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(family = "Arial", color = '#2D2327', size = 16, face = "bold"),
          legend.title = element_text(family = "Arial", color = '#2D2327', size = 16),
          legend.text = element_text(family = "Arial", color = '#2D2327', size = 16),
          axis.title = element_text(family = "Arial", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Arial", color = '#2D2327', size = 14), 
          plot.title = element_text(family = "Arial", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 1)) +
    scale_color_manual(values = c("black", "#E48DB7", "#FCC673", "#55BBC8", "palegreen2", "deeppink1", "brown", "#6A3D9A")) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .5), guide = "none") +
    xlim(0, 2.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "trial_onset_before_turn_indv_subs_robust.png", sep = "_")),
         plot = plot,
         width = 12,
         height = 10,
         units = "in",
         dpi = 300)
  
}


individual_and_overall_robust_lme_onset_turnaround_model_and_plot <- function(region, freq, distance_df, brain_df, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
  if(rerun_model == TRUE){
    
    ## Run LME Multiple Regression Models with two predictors: Distance to Ghost, Points Remaining
    
    # merge behavior data with ieeg data
   brain_behave_df <- left_join(distance_df %>% 
                                 select(-move_step) %>% mutate(trial_time = round(trial_time, 2)) %>% 
                                 filter(!(subject == "SLCH002" & trial_numeric == 220)), 
                             brain_df %>% mutate(trial_time = round(trial_time, 2)))
    
    
    # prep for linear mixed effects models #
    brain_behave_lme_df <- brain_behave_df %>%
      # round time so that we can loop over time later
      mutate(trial_time = round(trial_time, 2)) %>%
      # exclude time before they started moving, may be good to look at not doing this as well
      filter(Direction != "Still") %>%
      # filiter out ITI
      filter(reward_groups != 99 & !is.na(electrode)) %>%
      # prep electrode variables
      mutate(elec_id = paste0(subject, "_", gsub("_.*", "", electrode))) %>%
      mutate(electrode = gsub("_.*", "", electrode)) %>%
      # shift the time so that time 0 is the start of movement onset
      group_by(elec_id, trial_numeric, subject) %>%
      # shift trial time so that 0 is turnaround
      mutate(event = if_else(last_away == away_choice & away_choice != 0, 1, 0)) %>%
      mutate(event = replace(event, is.na(event), 0)) %>%
      mutate(turnaround_time = if_else(event == 1, trial_time, 0)) %>%
      mutate(turn_time = round(trial_time - max(turnaround_time), 2)) %>%
      ungroup() %>%
      filter(turn_time > -2 & turn_time < 2) %>%
      # select and rescale the necessary variables for the model
      select(subject, elec_id, theta, trial_numeric, turn_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = rescale(points_remaining)) %>%
      mutate(distance_to_ghost = rescale(distance_to_ghost))
    
        # Parallelized loop over steps
    results <- foreach(step=sort(unique(brain_behave_lme_df$turn_time)), .combine=rbind, .packages=c("dplyr", "robustlmm", "lmerTest", "tibble")) %dopar% {
      
      # prep model result dfs - distance to ghost
      dist_to_g_results <- tibble(
        case = character(0),
        time = numeric(0),
        Estimate = numeric(0),
        `Std. Error` = numeric(0),
        `t value` = numeric(0),
        `Pr(>|t|)` = numeric(0)
      )
      
      # prep model result dfs - points remaining
      points_r_results <- tibble(
        case = character(0),
        time = numeric(0),
        Estimate = numeric(0),
        `Std. Error` = numeric(0),
        `t value` = numeric(0),
        `Pr(>|t|)` = numeric(0)
      )
      
      
      print(step)
      
      # filter to only the necessary timepoint
      time_step_df <- brain_behave_lme_df %>%
        filter(turn_time == step)
      
      # fit the model with random effects of subject and electrode
      model_all <- summary(robustlmm::rlmer(theta ~ distance_to_ghost + points_remaining + (1|subject/elec_id), data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject/elec_id), data = time_step_df))
      coefs <- data.frame(coef(model_dfs))
      coefs.robust <- coef(model_all)
      p.values <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      # add to results - distance
      dist_to_g_results <- rbind(dist_to_g_results, 
                                 tibble(
                                   case = "all_subs",
                                   time = step, 
                                   Estimate = model_all$coefficients[2, "Estimate"],
                                   `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                   `t value` = model_all$coefficients[2, "t value"],
                                   `Pr(>|t|)` = p.values[2]
                                 ))
      
      # add to results - points remaining
      points_r_results <- rbind(points_r_results, 
                                tibble(
                                  case = "all_subs",
                                  time = step, 
                                  Estimate = model_all$coefficients[3, "Estimate"],
                                  `Std. Error` = model_all$coefficients[3, "Std. Error"],
                                  `t value` = model_all$coefficients[3, "t value"],
                                  `Pr(>|t|)` = p.values[3]
                                ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          
          model_sub <- summary(MASS::rlm(theta ~ distance_to_ghost + points_remaining, data = time_step_df %>% filter(elec_id == elec)))
          
          # calculate p value by getting dfs from satteraite approximated dfs
          coefs.robust <- coef(model_sub)
          p.values <- 2*pt(abs(coefs.robust[, "t value"]), model_sub$df[2], lower=FALSE)
          
          dist_to_g_results <- rbind(dist_to_g_results, 
                                     tibble(
                                       case = elec,
                                       time = step, 
                                       Estimate = model_sub$coefficients[2, "Value"],
                                       `Std. Error` = model_sub$coefficients[2, "Std. Error"],
                                       `t value` = model_sub$coefficients[2, "t value"],
                                       `Pr(>|t|)` = p.values[2]
                                     ))
          
          # add to results
          points_r_results <- rbind(points_r_results, 
                                    tibble(
                                      case = elec,
                                      time = step, 
                                      Estimate = model_sub$coefficients[3, "Value"],
                                      `Std. Error` = model_sub$coefficients[3, "Std. Error"],
                                      `t value` = model_sub$coefficients[3, "t value"],
                                      `Pr(>|t|)` = p.values[3]
                                    )) 
        }, TRUE)
      }
      
      return(list(dist_to_g_results_step = dist_to_g_results, points_r_results_step = points_r_results))
    }
        
    points_r_results <- bind_rows(results[, "points_r_results_step"])
    dist_to_g_results <- bind_rows(results[, "dist_to_g_results_step"]) 
    
    # consolidate different model results #
    model_results_onset_turnaround_all_subs <- rbind(dist_to_g_results %>% mutate(pred = "dist"),
                                                      points_r_results %>% mutate(pred = "points"))
    
    # save for later #
    write_csv(model_results_onset_turnaround_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_turnaround_all_subs_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_turnaround_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_turnaround_all_subs_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- model_results_onset_turnaround_all_subs %>%
    # FDR correct across time points
    group_by(case) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = if_else(grepl("dist", pred), "Distance to Ghost", "Points Remaining")) %>%
    mutate(main = if_else(case == "all_subs", "All Subjects", gsub("_.*", "", case))) %>%
    mutate(large_line = if_else(case == "all_subs", "all_subs", "indv_lines")) %>%
    # plot results
    ggplot(., aes(x = time, y = Estimate)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = main, alpha = large_line), size = 3) +
    geom_line(aes(color = main, group = case, size = large_line, alpha = large_line)) +
    geom_label(aes(x = sig), label = "*", color = "black", y = (y_high -.3), label.size = 0.5) +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(family = "Arial", color = '#2D2327', size = 16, face = "bold"),
          legend.title = element_text(family = "Arial", color = '#2D2327', size = 16),
          legend.text = element_text(family = "Arial", color = '#2D2327', size = 16),
          axis.title = element_text(family = "Arial", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Arial", color = '#2D2327', size = 14), 
          plot.title = element_text(family = "Arial", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 1)) +
    scale_color_manual(values = c("black", "#E48DB7", "#FCC673", "#55BBC8", "palegreen2", "deeppink1", "brown", "#6A3D9A")) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .5), guide = "none") +
    xlim(-2, 2)  + ylim(y_low, y_high) + labs(color = "", x = "Time (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "trial_onset_turnaround_indv_subs_robust.png", sep = "_")),
         plot = plot,
         width = 12,
         height = 10,
         units = "in",
         dpi = 300)
  
}



individual_and_overall_lme_onset_combval_before_turn_model_and_plot <- function(region, freq, distance_df, brain_df, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
  if(rerun_model == TRUE){
    
    ## Run LME Multiple Regression Models with one predictor
    
    # merge behavior data with ieeg data
    brain_behave_df <- left_join(distance_df %>% select(-move_step) %>% mutate(trial_time = round(trial_time, 2)), 
                                 brain_df %>% mutate(trial_time = round(trial_time, 2)))
    
    
    # prep for linear mixed effects models #
    brain_behave_lme_df <- brain_behave_df %>%
      # round time so that we can loop over time later
      mutate(trial_time = round(trial_time, 2)) %>%
      # exclude time before they started moving, may be good to look at not doing this as well
      filter(Direction != "Still") %>%
      # filiter out ITI
      filter(reward_groups != 99 & !is.na(electrode)) %>%
      # prep electrode variables
      mutate(elec_id = paste0(subject, "_", gsub("_.*", "", electrode))) %>%
      mutate(electrode = gsub("_.*", "", electrode)) %>%
      # shift the time so that time 0 is the start of movement onset
      group_by(elec_id, trial_numeric, subject) %>%
      mutate(move_time = round(trial_time - first(trial_time), 2)) %>%
      # filter time so that trials end when the person turnaround for the final time
      mutate(event = if_else(last_away == away_choice & away_choice != 0, 1, 0)) %>%
      mutate(event = replace(event, is.na(event), 0)) %>%
      mutate(turnaround_time = if_else(event == 1, move_time, 0)) %>%
      filter(move_time < max(turnaround_time)) %>%
      ungroup() %>%
      # filter to max time 3 since after 3s we start losing a lot of trials
      filter(move_time < 3) %>%
      # select and rescale the necessary variables for the model
      rowwise() %>%
      mutate(combined_value = points_remaining * distance_to_ghost) %>%
      ungroup() %>%
      mutate(combined_value = rescale(combined_value)) %>%
      select(subject, elec_id, theta, trial_numeric, move_time, combined_value)  %>%
      mutate(combined_value = rescale(combined_value))
    
    # prep model result dfs - combined value
      value_results_only <- tibble(
      case = character(0),
      time = numeric(0),
      Estimate = numeric(0),
      `Std. Error` = numeric(0),
      `t value` = numeric(0),
      `Pr(>|t|)` = numeric(0)
    )
    
    
    # loop over time
    for(step in sort(unique(brain_behave_lme_df$move_time))){
      
      print(step)
      
      # filter to only the necessary timepoint
      time_step_df <- brain_behave_lme_df %>%
        filter(move_time == step)
      
      # fit the model with random effects of subject and electrode
      model_all <- summary(lmer(theta ~ combined_value + (1|subject/elec_id), data = time_step_df))
      
      # add to results - combined value
      value_results_only <- rbind(value_results_only, 
                                 tibble(
                                   case = "all_subs",
                                   time = step, 
                                   Estimate = model_all$coefficients[2, "Estimate"],
                                   `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                   `t value` = model_all$coefficients[2, "t value"],
                                   `Pr(>|t|)` = model_all$coefficients[2, "Pr(>|t|)"]
                                 ))
    
      
      # now loop by subject to get the subjet specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that urn out of time points
        try( {
          
          model_sub <- summary(lm(theta ~ combined_value, data = time_step_df %>% filter(elec_id == elec)))
          
          
          value_results_only <- rbind(value_results_only, 
                                     tibble(
                                       case = elec,
                                       time = step, 
                                       Estimate = model_sub$coefficients[2, "Estimate"],
                                       `Std. Error` = model_sub$coefficients[2, "Std. Error"],
                                       `t value` = model_sub$coefficients[2, "t value"],
                                       `Pr(>|t|)` = model_sub$coefficients[2, "Pr(>|t|)"]
                                     ))

        }, TRUE)
      }
      
      
      
    }
    
    # save for later #
    write_csv(value_results_only, path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_combval_onset_before_turn_all_subs.csv", sep = "_")))
    
  } else{
    
    # load previous model
    value_results_only <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_combval_onset_before_turn_all_subs.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- value_results_only %>%
    # FDR correct across time points
    group_by(case) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = "Combined Value") %>%
    mutate(main = if_else(case == "all_subs", "All Subjects", gsub("_.*", "", case))) %>%
    mutate(large_line = if_else(case == "all_subs", "all_subs", "indv_lines")) %>%
    # plot results
    ggplot(., aes(x = time, y = Estimate)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = main, alpha = large_line), size = 3) +
    geom_line(aes(color = main, group = case, size = large_line, alpha = large_line)) +
    geom_label(aes(x = sig), label = "*", color = "black", y = (y_high -.3), label.size = 0.5) +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(family = "Arial", color = '#2D2327', size = 16, face = "bold"),
          legend.title = element_text(family = "Arial", color = '#2D2327', size = 16),
          legend.text = element_text(family = "Arial", color = '#2D2327', size = 16),
          axis.title = element_text(family = "Arial", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Arial", color = '#2D2327', size = 14), 
          plot.title = element_text(family = "Arial", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 1)) +
    scale_color_manual(values = c("black", "#E48DB7", "#FCC673", "#55BBC8", "palegreen2", "deeppink1", "brown", "#6A3D9A")) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .5), guide = "none") +
    xlim(0, 2.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "trial_combval_onset_before_turn_indv_subs.png", sep = "_")),
         plot = plot,
         width = 12,
         height = 10,
         units = "in",
         dpi = 300)
  
}
