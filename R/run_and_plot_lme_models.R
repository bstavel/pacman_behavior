
## Single Predictors for Linear Mixed Effects Models ##

individual_and_overall_lme_onset_combval_before_turn_model_and_plot <- function(region, freq, distance_df, brain_df, predictor, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
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
      filter(move_time < 2) %>%
      # select and scale the necessary variables for the model
      rowwise() %>%
      mutate(combined_value = points_remaining * distance_to_ghost) %>%
      mutate(discounted_value = points_remaining * 1/distance_to_ghost) %>%
      mutate(discounted_4reward = if_else(Eaten == 5, 0, points_remaining * 1/abs(UserLocation - Biscuit5))) %>%
      mutate(points_remaining_goal = if_else(dots_eaten == 5, points_remaining,
                                             if_else(dots_eaten == 4 & reward_groups %in% c(3, 4), points_remaining - 20, # ate 4 dots, big dot remaining
                                                     if_else(dots_eaten == 4 & reward_groups %in% c(1, 2), points_remaining - 10,  # ate 4 dots, small dot remaining
                                                             if_else(dots_eaten == 3 & reward_groups %in% c(1, 2), points_remaining - 20 - 10,  # ate 3 dots, 1 big dot, 1 small dot remaining
                                                                     if_else(dots_eaten == 3 & reward_groups == 4, points_remaining - 20 - 20,  # ate 3 dots, 2 big dots remaining
                                                                             if_else(dots_eaten == 3 & reward_groups == 3, points_remaining - 10 - 10, # ate 3 dots, 2 small dots remaining
                                                                                     if_else(dots_eaten == 2 & reward_groups %in% c(1, 3), points_remaining - 10 - 10 -20, # ate 2 dots, 2 small dots remaining, 1 big
                                                                                             if_else(dots_eaten == 2 & reward_groups %in% c(2, 4), points_remaining - 20 - 20 - 10,  # ate 2 dots, 2 small big remaining, 1 small
                                                                                                     if_else(dots_eaten == 1, points_remaining - 20 - 20 - 10 -10, # always same amount of reward after eating 1 dot
                                                                                                             0)))))))))) %>%
      mutate(discounted_goal = if_else(dots_eaten == 5, points_remaining_goal * 1/(abs(UserLocation - Biscuit5)),
                                       if_else(dots_eaten == 4, points_remaining_goal * 1/(abs(UserLocation - Biscuit4)),
                                               if_else(dots_eaten == 3, points_remaining_goal * 1/(abs(UserLocation - Biscuit3)),
                                                       if_else(dots_eaten == 2, points_remaining_goal * 1/(abs(UserLocation - Biscuit2)),
                                                               if_else(dots_eaten == 1, points_remaining_goal * 1/(abs(UserLocation - Biscuit1)),
                                                                       0)))))) %>%
      mutate(discounted_value_goal = points_remaining_goal * 1/distance_to_ghost) %>%                                      
      ungroup() %>%
      select(subject, elec_id, theta, trial_numeric, move_time, distance_to_ghost, points_remaining, discounted_value_goal, points_remaining_goal, Eaten, points_remaining_goal,
             combined_value, discounted_value, GhostLocation, UserLocation, discounted_reward, discounted_4reward, discounted_goal)  %>%
      mutate(combined_value = scale(combined_value)) %>%
      mutate(Eaten = scale(Eaten)) %>%
      mutate(discounted_value = scale(discounted_value)) %>%
      mutate(UserLocation = scale(UserLocation)) %>%
      mutate(GhostLocation = scale(GhostLocation))  %>%
      mutate(discounted_reward = scale(discounted_reward)) %>%
      mutate(discounted_4reward = scale(discounted_4reward)) %>%
      mutate(discounted_goal = scale(discounted_goal)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(points_remaining_goal = scale(points_remaining_goal)) %>%
      mutate(reward_threat_diff = scale(points_remaining - distance_to_ghost)) %>%
      mutate(eaten_threat_diff = scale(Eaten - distance_to_ghost)) %>%
      mutate(nreward_threat_add = scale(distance_to_ghost + (-1*points_remaining))) %>%
      mutate(reward_threat_sum = scale(points_remaining + distance_to_ghost))
    
    
    # Parallelized loop over steps
    results <- foreach(step=sort(unique(brain_behave_lme_df$move_time)), .combine=rbind, .packages=c("dplyr", "robustlmm", "lmerTest", "tibble")) %dopar% {
      
      
      # prep model result dfs - points remaining
      value_results_only <- tibble(
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      formula_pred <- as.formula(paste0("theta ~ ", predictor, " + (1|subject)"))
      # model_all <- summary(robustlmm::rlmer(formula_pred, data = time_step_df))
      model_all <- summary(lmerTest::lmer(formula_pred, data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      # model_dfs <- summary(lmerTest::lmer(formula_pred, data = time_step_df))
      # coefs <- data.frame(coef(model_dfs))
      # coefs.robust <- coef(model_all)
      # p.values <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      
      # add to results - value
      value_results_only <- rbind(value_results_only, 
                                  tibble(
                                    case = "all_subs",
                                    time = step, 
                                    Estimate = model_all$coefficients[2, "Estimate"],
                                    `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                    `t value` = model_all$coefficients[2, "t value"],
                                    `Pr(>|t|)` = model_all$coefficients[2, "Pr(>|t|)"]
                                  ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          lm_formula_pred <- as.formula(paste0("theta ~ ", predictor))
          model_sub <- summary(MASS::rlm(lm_formula_pred, data = time_step_df %>% filter(elec_id == elec)))
          
          # calculate p value by getting dfs from satteraite approximated dfs
          coefs.robust <- coef(model_sub)
          p.values <- 2*pt(abs(coefs.robust[, "t value"]), model_sub$df[2], lower=FALSE)
          
          
          # add to results
          value_results_only <- rbind(value_results_only, 
                                      tibble(
                                        case = elec,
                                        time = step, 
                                        Estimate = model_sub$coefficients[2, "Value"],
                                        `Std. Error` = model_sub$coefficients[2, "Std. Error"],
                                        `t value` = model_sub$coefficients[2, "t value"],
                                        `Pr(>|t|)` = p.values[2]
                                      )) 
        }, TRUE)
      }
      
      return(value_results_only)
    }
    
    value_results_only <- results
    
    # save for later #
    write_csv(value_results_only, path(here(), "munge", "lme_model_results", paste(region, freq, "model_results", predictor, "onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    value_results_only <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_results", predictor, "onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- value_results_only %>%
    # FDR correct across time points
    group_by(case) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = predictor) %>%
    mutate(main = if_else(case == "all_subs", "All Subjects", gsub("_.*", "", case))) %>%
    mutate(large_line = if_else(case == "all_subs", "all_subs", "indv_lines")) %>%
    # plot results
    ggplot(., aes(x = time, y = Estimate)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = main, alpha = large_line), size = 3) +
    geom_line(aes(color = main, group = case, size = large_line, alpha = large_line)) +
    geom_label(aes(x = sig), label = "*", color = "black", y = (y_high -.1), label.size = 0.5) +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(family = "Arial", color = '#2D2327', size = 16, face = "bold"),
          legend.title = element_text(family = "Arial", color = '#2D2327', size = 16),
          legend.text = element_text(family = "Arial", color = '#2D2327', size = 16),
          axis.title = element_text(family = "Arial", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Arial", color = '#2D2327', size = 14), 
          plot.title = element_text(family = "Arial", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(16))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .18), guide = "none") +
    xlim(0, 1.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "trial", predictor, "onset_before_turn_indv_subs.png", sep = "_")),
         plot = plot,
         width = 12,
         height = 10,
         units = "in",
         dpi = 300)
  
}
individual_and_overall_lme_onset_combval_turnaround_model_and_plot <- function(region, freq, distance_df, brain_df, predictor, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
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
      # shift trial time so that 0 is turnaround
      mutate(event = if_else(last_away == away_choice & away_choice != 0, 1, 0)) %>%
      mutate(event = replace(event, is.na(event), 0)) %>%
      mutate(turnaround_time = if_else(event == 1, trial_time, 0)) %>%
      mutate(turn_time = round(trial_time - max(turnaround_time), 2)) %>%
      ungroup() %>%
      filter(turn_time > -2 & turn_time < 2) %>%
      # select and scale the necessary variables for the model
      rowwise() %>%
      mutate(combined_value = points_remaining * distance_to_ghost) %>%
      mutate(discounted_value = points_remaining * 1/distance_to_ghost) %>%
      mutate(discounted_4reward = if_else(Eaten == 5, 0, points_remaining * 1/abs(UserLocation - Biscuit5))) %>%
      mutate(points_remaining_goal = if_else(dots_eaten == 5, points_remaining,
                                             if_else(dots_eaten == 4 & reward_groups %in% c(3, 4), points_remaining - 20, # ate 4 dots, big dot remaining
                                                     if_else(dots_eaten == 4 & reward_groups %in% c(1, 2), points_remaining - 10,  # ate 4 dots, small dot remaining
                                                             if_else(dots_eaten == 3 & reward_groups %in% c(1, 2), points_remaining - 20 - 10,  # ate 3 dots, 1 big dot, 1 small dot remaining
                                                                     if_else(dots_eaten == 3 & reward_groups == 4, points_remaining - 20 - 20,  # ate 3 dots, 2 big dots remaining
                                                                             if_else(dots_eaten == 3 & reward_groups == 3, points_remaining - 10 - 10, # ate 3 dots, 2 small dots remaining
                                                                                     if_else(dots_eaten == 2 & reward_groups %in% c(1, 3), points_remaining - 10 - 10 -20, # ate 2 dots, 2 small dots remaining, 1 big
                                                                                             if_else(dots_eaten == 2 & reward_groups %in% c(2, 4), points_remaining - 20 - 20 - 10,  # ate 2 dots, 2 small big remaining, 1 small
                                                                                                     if_else(dots_eaten == 1, points_remaining - 20 - 20 - 10 -10, # always same amount of reward after eating 1 dot
                                                                                                             0)))))))))) %>%
      mutate(discounted_goal = if_else(dots_eaten == 5, points_remaining_goal * 1/(abs(UserLocation - Biscuit5)),
                                       if_else(dots_eaten == 4, points_remaining_goal * 1/(abs(UserLocation - Biscuit4)),
                                               if_else(dots_eaten == 3, points_remaining_goal * 1/(abs(UserLocation - Biscuit3)),
                                                       if_else(dots_eaten == 2, points_remaining_goal * 1/(abs(UserLocation - Biscuit2)),
                                                               if_else(dots_eaten == 1, points_remaining_goal * 1/(abs(UserLocation - Biscuit1)),
                                                                       0)))))) %>%
      mutate(discounted_value_goal = points_remaining_goal * 1/distance_to_ghost) %>%                                      
      ungroup() %>%
      select(subject, elec_id, theta, trial_numeric, turn_time, distance_to_ghost, points_remaining, discounted_value_goal, points_remaining_goal, Eaten, points_remaining_goal,
             combined_value, discounted_value, GhostLocation, UserLocation, discounted_reward, discounted_4reward, discounted_goal)  %>%
      mutate(combined_value = scale(combined_value)) %>%
      mutate(Eaten = scale(Eaten)) %>%
      mutate(discounted_value = scale(discounted_value)) %>%
      mutate(UserLocation = scale(UserLocation)) %>%
      mutate(GhostLocation = scale(GhostLocation))  %>%
      mutate(discounted_reward = scale(discounted_reward)) %>%
      mutate(discounted_4reward = scale(discounted_4reward)) %>%
      mutate(discounted_goal = scale(discounted_goal)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(points_remaining_goal = scale(points_remaining_goal)) %>%
      mutate(reward_threat_diff = scale(points_remaining - distance_to_ghost)) %>%
      mutate(eaten_threat_diff = scale(Eaten - distance_to_ghost)) %>%
      mutate(nreward_threat_add = scale(distance_to_ghost + (-1*points_remaining))) %>%
      mutate(reward_threat_sum = scale(points_remaining + distance_to_ghost))
    
    
    # Parallelized loop over steps
    results <- foreach(step=sort(unique(brain_behave_lme_df$turn_time)), .combine=rbind, .packages=c("dplyr", "robustlmm", "lmerTest", "tibble")) %dopar% {
      
      
      # prep model result dfs - points remaining
      value_results_only <- tibble(
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
        filter(turn_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      formula_pred <- as.formula(paste0("theta ~ ", predictor, " + (1|subject)"))
      # model_all <- summary(robustlmm::rlmer(formula_pred, data = time_step_df))
      model_all <- summary(lmerTest::lmer(formula_pred, data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      # model_dfs <- summary(lmerTest::lmer(formula_pred, data = time_step_df))
      # coefs <- data.frame(coef(model_dfs))
      # coefs.robust <- coef(model_all)
      # p.values <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      
      # add to results - value
      value_results_only <- rbind(value_results_only, 
                                  tibble(
                                    case = "all_subs",
                                    time = step, 
                                    Estimate = model_all$coefficients[2, "Estimate"],
                                    `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                    `t value` = model_all$coefficients[2, "t value"],
                                    `Pr(>|t|)` = model_all$coefficients[2, "Pr(>|t|)"]
                                  ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          lm_formula_pred <- as.formula(paste0("theta ~ ", predictor))
          model_sub <- summary(MASS::rlm(lm_formula_pred, data = time_step_df %>% filter(elec_id == elec)))
          
          # calculate p value by getting dfs from satteraite approximated dfs
          coefs.robust <- coef(model_sub)
          p.values <- 2*pt(abs(coefs.robust[, "t value"]), model_sub$df[2], lower=FALSE)
          
          
          # add to results
          value_results_only <- rbind(value_results_only, 
                                      tibble(
                                        case = elec,
                                        time = step, 
                                        Estimate = model_sub$coefficients[2, "Value"],
                                        `Std. Error` = model_sub$coefficients[2, "Std. Error"],
                                        `t value` = model_sub$coefficients[2, "t value"],
                                        `Pr(>|t|)` = p.values[2]
                                      )) 
        }, TRUE)
      }
      
      return(value_results_only)
    }
    
    value_results_only <- results
    
    # save for later #
    write_csv(value_results_only, path(here(), "munge", "lme_model_results", paste(region, freq, "model_results", predictor, "turnaround_all_subs_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    value_results_only <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_results", predictor, "turnaround_all_subs_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- value_results_only %>%
    # FDR correct across time points
    group_by(case) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = predictor) %>%
    mutate(main = if_else(case == "all_subs", "All Subjects", gsub("_.*", "", case))) %>%
    mutate(large_line = if_else(case == "all_subs", "all_subs", "indv_lines")) %>%
    # plot results
    ggplot(., aes(x = time, y = Estimate)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = main, alpha = large_line), size = 3) +
    geom_line(aes(color = main, group = case, size = large_line, alpha = large_line)) +
    geom_label(aes(x = sig), label = "*", color = "black", y = (y_high -.1), label.size = 0.5) +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(family = "Arial", color = '#2D2327', size = 16, face = "bold"),
          legend.title = element_text(family = "Arial", color = '#2D2327', size = 16),
          legend.text = element_text(family = "Arial", color = '#2D2327', size = 16),
          axis.title = element_text(family = "Arial", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Arial", color = '#2D2327', size = 14), 
          plot.title = element_text(family = "Arial", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(16))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .18), guide = "none") +
    xlim(-2, 2)  + ylim(y_low, y_high) + labs(color = "", x = "Time (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "trial", predictor, "turnaround_indv_subs.png", sep = "_")),
         plot = plot,
         width = 12,
         height = 10,
         units = "in",
         dpi = 300)
  
}

## Multiple Predictors for Linear Mixed Effects Models ##

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
      filter(move_time < 2) %>%
      # select and scale the necessary variables for the model
      select(subject, elec_id, theta, trial_numeric, move_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) 
    
    
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      # model_all <- summary(robustlmm::rlmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
      model_all <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
      
      # # calculate p value by getting dfs from satteraite approximated dfs
      # model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
      # coefs <- data.frame(coef(model_dfs))
      # coefs.robust <- coef(model_all)
      # p.values <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      # add to results - distance
      dist_to_g_results <- rbind(dist_to_g_results, 
                                 tibble(
                                   case = "all_subs",
                                   time = step, 
                                   Estimate = model_all$coefficients[2, "Estimate"],
                                   `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                   `t value` = model_all$coefficients[2, "t value"],
                                   `Pr(>|t|)` = model_all$coefficients[2, "Pr(>|t|)"]
                                 ))
      
      # add to results - points remaining
      points_r_results <- rbind(points_r_results, 
                                tibble(
                                  case = "all_subs",
                                  time = step, 
                                  Estimate = model_all$coefficients[3, "Estimate"],
                                  `Std. Error` = model_all$coefficients[3, "Std. Error"],
                                  `t value` = model_all$coefficients[3, "t value"],
                                  `Pr(>|t|)` = model_all$coefficients[3, "Pr(>|t|)"]
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
    write_csv(model_results_onset_before_turn_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_onset_before_turn_all_subs_log_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_before_turn_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_onset_before_turn_all_subs_log_robust.csv", sep = "_")))
    
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
          legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18, face = "bold"),
          legend.title = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          legend.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          axis.title = element_text(family = "Gill Sans", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          plot.subtitle =  element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          plot.title = element_text(family = "Gill Sans", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(15))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .1), guide = "none") +
    xlim(0, 1.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time from movement onset (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title, subtitle = "Rolling robust regression with random effect of subject (black line), individual\nelectrode traces are color-coded by subject, FDR-corrected for time")
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "trial_onset_before_turn_indv_subs_log_robust.png", sep = "_")),
         plot = plot,
         width = 10.6,
         height = 9.5,
         units = "in",
         dpi = 300)
  
}


individual_and_overall_robust_lme_turnaround_model_and_plot <- function(region, freq, distance_df, brain_df, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
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
      # select and scale the necessary variables for the model
      select(subject, elec_id, theta, trial_numeric, turn_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost))
    
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
        filter(turn_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      # model_all <- summary(robustlmm::rlmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
      model_all <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      # model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
      # coefs <- data.frame(coef(model_dfs))
      # coefs.robust <- coef(model_all)
      # p.values <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      # add to results - distance
      dist_to_g_results <- rbind(dist_to_g_results, 
                                 tibble(
                                   case = "all_subs",
                                   time = step, 
                                   Estimate = model_all$coefficients[2, "Estimate"],
                                   `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                   `t value` = model_all$coefficients[2, "t value"],
                                   `Pr(>|t|)` = model_all$coefficients[2, "Pr(>|t|)"]
                                 ))
      
      # add to results - points remaining
      points_r_results <- rbind(points_r_results, 
                                tibble(
                                  case = "all_subs",
                                  time = step, 
                                  Estimate = model_all$coefficients[3, "Estimate"],
                                  `Std. Error` = model_all$coefficients[3, "Std. Error"],
                                  `t value` = model_all$coefficients[3, "t value"],
                                  `Pr(>|t|)` = model_all$coefficients[3, "Pr(>|t|)"]
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
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(15))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .1), guide = "none") +
    xlim(-2, 2)  + ylim(y_low, y_high) + labs(color = "", x = "Time at turnaround (in seconds)", y = "Beta Coefficient") +
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


## By Subject ##
individual_subject_robust_lme_onset_before_turn_model_and_plot <- function(region, freq, sub, color_sub, distance_df, brain_df, plot_title, y_low, y_high, rerun_model = TRUE){
### Only single subject at a time, no overall model  
  
  if(rerun_model == TRUE){
    
    ## Run LME Multiple Regression Models with two predictors: Distance to Ghost, Points Remaining
    
    # merge behavior data with ieeg data
    brain_behave_df <- left_join(distance_df %>% 
                                   select(-move_step) %>% mutate(trial_time = round(trial_time, 2)) %>% 
                                   filter(!(subject == "SLCH002" & trial_numeric == 220)), 
                                 brain_df %>% mutate(trial_time = round(trial_time, 2)))
    
    
    # prep for linear mixed effects models #
    brain_behave_lme_df <- brain_behave_df %>%
      filter(subject == sub) %>%
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
      filter(move_time < 1.5) %>%
      # get probe ID rather than elec id
      mutate(probe_id = gsub("[123456789]", "", gsub("-.*", "", electrode))) %>%
      # select and scale the necessary variables for the model
      select(subject, elec_id, probe_id, theta, trial_numeric, move_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) 
    
    
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      model_all <- summary(robustlmm::rlmer(theta ~ distance_to_ghost + points_remaining + (1|elec_id), data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|elec_id), data = time_step_df))
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
    write_csv(model_results_onset_before_turn_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, sub, "model_results_onset_before_turn_all_subs_log_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_before_turn_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, sub,  "model_results_onset_before_turn_all_subs_log_robust.csv", sep = "_")))
    
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
    scale_color_manual(values = c("black", color_sub)) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .18), guide = "none") +
    xlim(0, 1.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time from movement onset (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, sub, "trial_onset_before_turn_indv_subs_log_robust.png", sep = "_")),
         plot = plot,
         width = 10.5,
         height = 10.5,
         units = "in",
         dpi = 300)
  
}



### INTERACTION MODELS ###

individual_and_overall_robust_interact_lme_onset_before_turn_model_and_plot <- function(region, freq, distance_df, brain_df, threat_pred, reward_pred, plot_title, y_low, y_high, rerun_model = TRUE){
## interaction model  
  
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
      filter(move_time < 2) %>%
    # select and scale the necessary variables for the model
      rowwise() %>%
      mutate(combined_value = points_remaining * distance_to_ghost) %>%
      mutate(discounted_value = points_remaining * 1/distance_to_ghost) %>%
      mutate(discounted_4reward = if_else(Eaten == 5, 0, points_remaining * 1/abs(UserLocation - Biscuit5))) %>%
      mutate(points_remaining_goal = if_else(dots_eaten == 5, points_remaining,
                                             if_else(dots_eaten == 4 & reward_groups %in% c(3, 4), points_remaining - 20, # ate 4 dots, big dot remaining
                                                     if_else(dots_eaten == 4 & reward_groups %in% c(1, 2), points_remaining - 10,  # ate 4 dots, small dot remaining
                                                             if_else(dots_eaten == 3 & reward_groups %in% c(1, 2), points_remaining - 20 - 10,  # ate 3 dots, 1 big dot, 1 small dot remaining
                                                                     if_else(dots_eaten == 3 & reward_groups == 4, points_remaining - 20 - 20,  # ate 3 dots, 2 big dots remaining
                                                                             if_else(dots_eaten == 3 & reward_groups == 3, points_remaining - 10 - 10, # ate 3 dots, 2 small dots remaining
                                                                                     if_else(dots_eaten == 2 & reward_groups %in% c(1, 3), points_remaining - 10 - 10 -20, # ate 2 dots, 2 small dots remaining, 1 big
                                                                                             if_else(dots_eaten == 2 & reward_groups %in% c(2, 4), points_remaining - 20 - 20 - 10,  # ate 2 dots, 2 small big remaining, 1 small
                                                                                                     if_else(dots_eaten == 1, points_remaining - 20 - 20 - 10 -10, # always same amount of reward after eating 1 dot
                                                                                                             0)))))))))) %>%
      mutate(discounted_goal = if_else(dots_eaten == 5, points_remaining_goal * 1/(abs(UserLocation - Biscuit5)),
                                       if_else(dots_eaten == 4, points_remaining_goal * 1/(abs(UserLocation - Biscuit4)),
                                               if_else(dots_eaten == 3, points_remaining_goal * 1/(abs(UserLocation - Biscuit3)),
                                                       if_else(dots_eaten == 2, points_remaining_goal * 1/(abs(UserLocation - Biscuit2)),
                                                               if_else(dots_eaten == 1, points_remaining_goal * 1/(abs(UserLocation - Biscuit1)),
                                                                       0)))))) %>%
      mutate(discounted_value_goal = points_remaining_goal * 1/distance_to_ghost) %>%                                      
      ungroup() %>%
      mutate(distance_inverse = 1/distance_to_ghost) %>%
      select(subject, elec_id, theta, trial_numeric, move_time, points_remaining, distance_to_ghost, distance_inverse, Eaten, points_remaining_goal)  %>%
      # group_by(move_time) %>%
      mutate(points_remaining_goal = scale(points_remaining_goal)) %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(Eaten = scale(Eaten)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) %>%
      mutate(distance_to_ghost_neg = distance_to_ghost * -1) %>%
      mutate(points_remaining_neg = points_remaining * -1) %>%
      mutate(distance_inverse = scale(distance_inverse)) 
    
    
    
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
      
      # prep model result dfs - interaction
      interaction_results <- tibble(
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      formula_pred <- as.formula(paste0("theta ~ ", threat_pred, " + ", reward_pred, " + ", threat_pred, "*", reward_pred, " + (1|subject)"))
      model_all <- summary(robustlmm::rlmer(formula_pred, data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
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
      
      # add to results - points remaining
      interaction_results <- rbind(interaction_results, 
                                   tibble(
                                     case = "all_subs",
                                     time = step, 
                                     Estimate = model_all$coefficients[4, "Estimate"],
                                     `Std. Error` = model_all$coefficients[4, "Std. Error"],
                                     `t value` = model_all$coefficients[4, "t value"],
                                     `Pr(>|t|)` = p.values[4]
                                   ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          formula_sub_pred <- as.formula(paste0("theta ~ ", threat_pred, " + ", reward_pred, " + ", threat_pred, "*", reward_pred))
          model_sub <- summary(MASS::rlm(formula_sub_pred, data = time_step_df %>% filter(elec_id == elec)))
          
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
          
          # add to results - points remaining
          interaction_results <- rbind(interaction_results, 
                                       tibble(
                                         case = elec,
                                         time = step, 
                                         Estimate = model_sub$coefficients[4, "Value"],
                                         `Std. Error` = model_sub$coefficients[4, "Std. Error"],
                                         `t value` = model_sub$coefficients[4, "t value"],
                                         `Pr(>|t|)` = p.values[4]
                                       ))   
        }, TRUE)
      }
      
      return(list(dist_to_g_results_step = dist_to_g_results, points_r_results_step = points_r_results, interaction_results_step = interaction_results))
    }
    
    
    points_r_results <- bind_rows(results[, "points_r_results_step"])
    dist_to_g_results <- bind_rows(results[, "dist_to_g_results_step"]) 
    interaction_results <- bind_rows(results[, "interaction_results_step"]) 
    
    # consolidate different model results #
    model_results_onset_before_turn_all_subs <- rbind(dist_to_g_results %>% mutate(pred = "dist"),
                                                      points_r_results %>% mutate(pred = "points"),
                                                      interaction_results %>% mutate(pred = "interaction"))
    
    # save for later #
    write_csv(model_results_onset_before_turn_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, threat_pred, reward_pred, 
                                                                                                         "model_interaction_results_onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_before_turn_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, threat_pred, reward_pred, 
                                                                                                          "model_interaction_results_onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- model_results_onset_before_turn_all_subs %>%
    # FDR correct across time points
    group_by(case, pred) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # mutate(pval_fdr = `Pr(>|t|)`) %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = if_else(grepl("dist", pred), threat_pred, 
                               if_else(grepl("points", pred), reward_pred, "Interaction"))) %>%
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
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(15))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .1), guide = "none") +
    xlim(0, 1.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time from movement onset (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, threat_pred, reward_pred,  "interaction_trial_onset_before_turn_indv_subs_robust.png", sep = "_")),
         plot = plot,
         width = 10.5,
         height = 10.5,
         units = "in",
         dpi = 300)
  
}

individual_and_overall_robust_interact_lme_turnaround_model_and_plot <- function(region, freq, distance_df, brain_df, threat_pred, reward_pred, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
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
      # select and scale the necessary variables for the model
      select(subject, elec_id, theta, trial_numeric, turn_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost))
    
    # Parallelized loop over steps
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
      
      # prep model result dfs - interaction
      interaction_results <- tibble(
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
        filter(turn_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      formula_pred <- as.formula(paste0("theta ~ ", threat_pred, " + ", reward_pred, " + ", threat_pred, "*", reward_pred, " + (1|subject)"))
      model_all <- summary(robustlmm::rlmer(formula_pred, data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
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
      
      # add to results - points remaining
      interaction_results <- rbind(interaction_results, 
                                   tibble(
                                     case = "all_subs",
                                     time = step, 
                                     Estimate = model_all$coefficients[4, "Estimate"],
                                     `Std. Error` = model_all$coefficients[4, "Std. Error"],
                                     `t value` = model_all$coefficients[4, "t value"],
                                     `Pr(>|t|)` = p.values[4]
                                   ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          formula_sub_pred <- as.formula(paste0("theta ~ ", threat_pred, " + ", reward_pred, " + ", threat_pred, "*", reward_pred))
          model_sub <- summary(MASS::rlm(formula_sub_pred, data = time_step_df %>% filter(elec_id == elec)))
          
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
          
          # add to results - points remaining
          interaction_results <- rbind(interaction_results, 
                                       tibble(
                                         case = elec,
                                         time = step, 
                                         Estimate = model_sub$coefficients[4, "Value"],
                                         `Std. Error` = model_sub$coefficients[4, "Std. Error"],
                                         `t value` = model_sub$coefficients[4, "t value"],
                                         `Pr(>|t|)` = p.values[4]
                                       ))   
        }, TRUE)
      }
      
      return(list(dist_to_g_results_step = dist_to_g_results, points_r_results_step = points_r_results, interaction_results_step = interaction_results))
    }
    
    
    points_r_results <- bind_rows(results[, "points_r_results_step"])
    dist_to_g_results <- bind_rows(results[, "dist_to_g_results_step"]) 
    interaction_results <- bind_rows(results[, "interaction_results_step"]) 
    
    # consolidate different model results #
    model_results_onset_before_turn_all_subs <- rbind(dist_to_g_results %>% mutate(pred = "dist"),
                                                      points_r_results %>% mutate(pred = "points"),
                                                      interaction_results %>% mutate(pred = "interaction"))
    
    # save for later #
    write_csv(model_results_onset_before_turn_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, threat_pred, reward_pred, 
                                                                                                         "model_interaction_results_turnaround_all_subs_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_before_turn_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, threat_pred, reward_pred, 
                                                                                                          "model_interaction_results_turnaround_all_subs_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- model_results_onset_before_turn_all_subs %>%
    # FDR correct across time points
    group_by(case, pred) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # mutate(pval_fdr = `Pr(>|t|)`) %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = if_else(grepl("dist", pred), threat_pred, 
                               if_else(grepl("points", pred), reward_pred, "Interaction"))) %>%
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
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(15))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .1), guide = "none") +
    xlim(-2, 2)  + ylim(y_low, y_high) + labs(color = "", x = "Time from movement onset (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, threat_pred, reward_pred,  "interaction_trial_turnaround_indv_subs_robust.png", sep = "_")),
         plot = plot,
         width = 10.5,
         height = 10.5,
         units = "in",
         dpi = 300)
  
}

## By Subject ##
individual_subject_robust_interact_lme_onset_before_turn_model_and_plot <- function(region, freq, sub, color_sub, distance_df, brain_df, threat_pred, reward_pred, plot_title, y_low, y_high, rerun_model = TRUE){
### Only single subject at a time, no overall model  
  
  if(rerun_model == TRUE){
    
    ## Run LME Multiple Regression Models with two predictors: Distance to Ghost, Points Remaining
    
    # merge behavior data with ieeg data
    brain_behave_df <- left_join(distance_df %>% 
                                   select(-move_step) %>% mutate(trial_time = round(trial_time, 2)) %>% 
                                   filter(!(subject == "SLCH002" & trial_numeric == 220)), 
                                 brain_df %>% mutate(trial_time = round(trial_time, 2)))
    
    
    # prep for linear mixed effects models #
    brain_behave_lme_df <- brain_behave_df %>%
      filter(subject == sub) %>%
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
      filter(move_time < 1.5) %>%
      # get probe ID rather than elec id
      mutate(probe_id = gsub("[123456789]", "", gsub("-.*", "", electrode))) %>%
      # select and scale the necessary variables for the model
      select(subject, elec_id, probe_id, theta, trial_numeric, move_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) 
    
    
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
      
      # prep model result dfs - interaction
      interaction_results <- tibble(
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      formula_pred <- as.formula(paste0("theta ~ ", threat_pred, " + ", reward_pred, " + ", threat_pred, "*", reward_pred, " + (1|elec_id)"))
      model_all <- summary(robustlmm::rlmer(formula_pred, data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|elec_id), data = time_step_df))
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
      
      # add to results - points remaining
      interaction_results <- rbind(interaction_results, 
                                   tibble(
                                     case = "all_subs",
                                     time = step, 
                                     Estimate = model_all$coefficients[4, "Estimate"],
                                     `Std. Error` = model_all$coefficients[4, "Std. Error"],
                                     `t value` = model_all$coefficients[4, "t value"],
                                     `Pr(>|t|)` = p.values[4]
                                   ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          formula_sub_pred <- as.formula(paste0("theta ~ ", threat_pred, " + ", reward_pred, " + ", threat_pred, "*", reward_pred))
          model_sub <- summary(MASS::rlm(formula_sub_pred, data = time_step_df %>% filter(elec_id == elec)))
          
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
          
          # add to results - points remaining
          interaction_results <- rbind(interaction_results, 
                                       tibble(
                                         case = elec,
                                         time = step, 
                                         Estimate = model_sub$coefficients[4, "Value"],
                                         `Std. Error` = model_sub$coefficients[4, "Std. Error"],
                                         `t value` = model_sub$coefficients[4, "t value"],
                                         `Pr(>|t|)` = p.values[4]
                                       ))   
        }, TRUE)
      }
      
      return(list(dist_to_g_results_step = dist_to_g_results, points_r_results_step = points_r_results, interaction_results_step = interaction_results))
    }
    
    
    points_r_results <- bind_rows(results[, "points_r_results_step"])
    dist_to_g_results <- bind_rows(results[, "dist_to_g_results_step"]) 
    interaction_results <- bind_rows(results[, "interaction_results_step"]) 
    
    # consolidate different model results #
    model_results_onset_before_turn_all_subs <- rbind(dist_to_g_results %>% mutate(pred = "dist"),
                                                      points_r_results %>% mutate(pred = "points"),
                                                      interaction_results %>% mutate(pred = "interaction"))
    
    # save for later #
    write_csv(model_results_onset_before_turn_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, sub, threat_pred, reward_pred, 
                                                                                                         "model_interaction_results_onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_before_turn_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, sub, threat_pred, reward_pred, 
                                                                                                          "model_interaction_results_onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- model_results_onset_before_turn_all_subs %>%
    # FDR correct across time points
    group_by(case, pred) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # mutate(pval_fdr = `Pr(>|t|)`) %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = if_else(grepl("dist", pred), threat_pred, 
                               if_else(grepl("points", pred), reward_pred, "Interaction"))) %>%
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
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", color_sub)) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .1), guide = "none") +
    xlim(0, 1.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time from movement onset (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, sub, threat_pred, reward_pred,  "interaction_trial_onset_before_turn_indv_subs_robust.png", sep = "_")),
         plot = plot,
         width = 10.5,
         height = 10.5,
         units = "in",
         dpi = 300)
  
}


### Alternatives ###

individual_and_overall_lme_onset_combval_before_turn_model_and_plot <- function(region, freq, distance_df, brain_df, predictor, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
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
      filter(move_time < 2) %>%
      # select and scale the necessary variables for the model
      rowwise() %>%
      mutate(combined_value = points_remaining * distance_to_ghost) %>%
      mutate(discounted_value = points_remaining * 1/distance_to_ghost) %>%
      mutate(discounted_4reward = if_else(Eaten == 5, 0, points_remaining * 1/abs(UserLocation - Biscuit5))) %>%
      mutate(points_remaining_goal = if_else(dots_eaten == 5, points_remaining,
                                             if_else(dots_eaten == 4 & reward_groups %in% c(3, 4), points_remaining - 20, # ate 4 dots, big dot remaining
                                                     if_else(dots_eaten == 4 & reward_groups %in% c(1, 2), points_remaining - 10,  # ate 4 dots, small dot remaining
                                                             if_else(dots_eaten == 3 & reward_groups %in% c(1, 2), points_remaining - 20 - 10,  # ate 3 dots, 1 big dot, 1 small dot remaining
                                                                     if_else(dots_eaten == 3 & reward_groups == 4, points_remaining - 20 - 20,  # ate 3 dots, 2 big dots remaining
                                                                             if_else(dots_eaten == 3 & reward_groups == 3, points_remaining - 10 - 10, # ate 3 dots, 2 small dots remaining
                                                                                     if_else(dots_eaten == 2 & reward_groups %in% c(1, 3), points_remaining - 10 - 10 -20, # ate 2 dots, 2 small dots remaining, 1 big
                                                                                             if_else(dots_eaten == 2 & reward_groups %in% c(2, 4), points_remaining - 20 - 20 - 10,  # ate 2 dots, 2 small big remaining, 1 small
                                                                                                     if_else(dots_eaten == 1, points_remaining - 20 - 20 - 10 -10, # always same amount of reward after eating 1 dot
                                                                                                             0)))))))))) %>%
      mutate(discounted_goal = if_else(dots_eaten == 5, points_remaining_goal * 1/(abs(UserLocation - Biscuit5)),
                                       if_else(dots_eaten == 4, points_remaining_goal * 1/(abs(UserLocation - Biscuit4)),
                                               if_else(dots_eaten == 3, points_remaining_goal * 1/(abs(UserLocation - Biscuit3)),
                                                       if_else(dots_eaten == 2, points_remaining_goal * 1/(abs(UserLocation - Biscuit2)),
                                                               if_else(dots_eaten == 1, points_remaining_goal * 1/(abs(UserLocation - Biscuit1)),
                                                                       0)))))) %>%
      mutate(discounted_value_goal = points_remaining_goal * 1/distance_to_ghost) %>%                                      
      ungroup() %>%
      select(subject, elec_id, theta, trial_numeric, move_time, distance_to_ghost, points_remaining, discounted_value_goal, points_remaining_goal, Eaten, points_remaining_goal,
             combined_value, discounted_value, GhostLocation, UserLocation, discounted_reward, discounted_4reward, discounted_goal)  %>%
      mutate(combined_value = scale(combined_value)) %>%
      mutate(Eaten = scale(Eaten)) %>%
      mutate(discounted_value = scale(discounted_value)) %>%
      mutate(UserLocation = scale(UserLocation)) %>%
      mutate(GhostLocation = scale(GhostLocation))  %>%
      mutate(discounted_reward = scale(discounted_reward)) %>%
      mutate(discounted_4reward = scale(discounted_4reward)) %>%
      mutate(discounted_goal = scale(discounted_goal)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(points_remaining_goal = scale(points_remaining_goal)) %>%
      mutate(reward_threat_diff = scale(points_remaining - distance_to_ghost)) %>%
      mutate(eaten_threat_diff = scale(Eaten - distance_to_ghost)) %>%
      mutate(nreward_threat_add = scale(distance_to_ghost + (-1*points_remaining))) %>%
      mutate(reward_threat_sum = scale(points_remaining + distance_to_ghost))
    
    
    # Parallelized loop over steps
    results <- foreach(step=sort(unique(brain_behave_lme_df$move_time)), .combine=rbind, .packages=c("dplyr", "robustlmm", "lmerTest", "tibble")) %dopar% {
      
      
      # prep model result dfs - points remaining
      value_results_only <- tibble(
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      formula_pred <- as.formula(paste0("theta ~ ", predictor, " + (1|subject)"))
      # model_all <- summary(robustlmm::rlmer(formula_pred, data = time_step_df))
      model_all <- summary(lmerTest::lmer(formula_pred, data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      # model_dfs <- summary(lmerTest::lmer(formula_pred, data = time_step_df))
      # coefs <- data.frame(coef(model_dfs))
      # coefs.robust <- coef(model_all)
      # p.values <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      
      # add to results - value
      value_results_only <- rbind(value_results_only, 
                                  tibble(
                                    case = "all_subs",
                                    time = step, 
                                    Estimate = model_all$coefficients[2, "Estimate"],
                                    `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                    `t value` = model_all$coefficients[2, "t value"],
                                    `Pr(>|t|)` = model_all$coefficients[2, "Pr(>|t|)"]
                                  ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          lm_formula_pred <- as.formula(paste0("theta ~ ", predictor))
          model_sub <- summary(MASS::rlm(lm_formula_pred, data = time_step_df %>% filter(elec_id == elec)))
          
          # calculate p value by getting dfs from satteraite approximated dfs
          coefs.robust <- coef(model_sub)
          p.values <- 2*pt(abs(coefs.robust[, "t value"]), model_sub$df[2], lower=FALSE)
          
          
          # add to results
          value_results_only <- rbind(value_results_only, 
                                      tibble(
                                        case = elec,
                                        time = step, 
                                        Estimate = model_sub$coefficients[2, "Value"],
                                        `Std. Error` = model_sub$coefficients[2, "Std. Error"],
                                        `t value` = model_sub$coefficients[2, "t value"],
                                        `Pr(>|t|)` = p.values[2]
                                      )) 
        }, TRUE)
      }
      
      return(value_results_only)
    }
    
    value_results_only <- results
    
    # save for later #
    write_csv(value_results_only, path(here(), "munge", "lme_model_results", paste(region, freq, "model_results", predictor, "onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    value_results_only <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_results", predictor, "onset_before_turn_all_subs_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- value_results_only %>%
    # FDR correct across time points
    group_by(case) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = predictor) %>%
    mutate(main = if_else(case == "all_subs", "All Subjects", gsub("_.*", "", case))) %>%
    mutate(large_line = if_else(case == "all_subs", "all_subs", "indv_lines")) %>%
    # plot results
    ggplot(., aes(x = time, y = Estimate)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = main, alpha = large_line), size = 3) +
    geom_line(aes(color = main, group = case, size = large_line, alpha = large_line)) +
    geom_label(aes(x = sig), label = "*", color = "black", y = (y_high -.1), label.size = 0.5) +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(family = "Arial", color = '#2D2327', size = 16, face = "bold"),
          legend.title = element_text(family = "Arial", color = '#2D2327', size = 16),
          legend.text = element_text(family = "Arial", color = '#2D2327', size = 16),
          axis.title = element_text(family = "Arial", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Arial", color = '#2D2327', size = 14), 
          plot.title = element_text(family = "Arial", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(16))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .18), guide = "none") +
    xlim(0, 1.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "trial", predictor, "onset_before_turn_indv_subs.png", sep = "_")),
         plot = plot,
         width = 12,
         height = 10,
         units = "in",
         dpi = 300)
  
}

individual_and_overall_robust_equal_time_lme_onset_before_turn_model_and_plot <- function(region, freq, distance_df, brain_df, plot_title, y_low, y_high, rerun_model = TRUE){
  
  
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
      filter(max(turnaround_time) >= 1.2) %>%
      filter(move_time < max(turnaround_time)) %>%
      ungroup() %>%
      # filter to max time 3 since after 3s we start losing a lot of trials
      filter(move_time < 1.2) %>%
      # select and scale the necessary variables for the model
      select(subject, elec_id, theta, trial_numeric, move_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) 
    
    
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      model_all <- summary(robustlmm::rlmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      model_dfs <- summary(lmerTest::lmer(theta ~ distance_to_ghost + points_remaining + (1|subject), data = time_step_df))
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
    write_csv(model_results_onset_before_turn_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_onset_before_turn_all_subs_equal_time_log_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_before_turn_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_results_onset_before_turn_all_subs_equal_time_log_robust.csv", sep = "_")))
    
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
    scale_alpha_manual(values = c(1, .18), guide = "none") +
    xlim(0, 1.2)  + ylim(y_low, y_high) + labs(color = "", x = "Time from movement onset (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title)
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "trial_onset_before_turn_indv_subs_equal_time_log_robust.png", sep = "_")),
         plot = plot,
         width = 10.5,
         height = 10.5,
         units = "in",
         dpi = 300)
  
}

reward_prediction_robust_lme_onset_before_turn_model_and_plot <- function(region, freq, distance_df, brain_df, plot_title, y_low, y_high, rerun_model = TRUE){

  
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
      filter(move_time < 2) %>%
      # select and scale the necessary variables for the model
      select(subject, elec_id, theta, trial_numeric, move_time, points_remaining, distance_to_ghost)  %>%
      group_by(subject, move_time) %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) %>%
      mutate(reward_prediction  = points_remaining + distance_to_ghost + (points_remaining * distance_to_ghost))


    # Parallelized loop over steps
    pe_results <- foreach(step=sort(unique(brain_behave_lme_df$move_time)), .combine=rbind, .packages=c("dplyr", "robustlmm", "lmerTest", "tibble")) %dopar% {
      
      # prep model result dfs - distance to ghost
      pe_results <- tibble(
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      model_all <- summary(robustlmm::rlmer(theta ~ reward_prediction + (1|subject), data = time_step_df))
      
      # calculate p value by getting dfs from satteraite approximated dfs
      model_dfs <- summary(lmerTest::lmer(theta ~ reward_prediction + (1|subject), data = time_step_df))
      coefs <- data.frame(coef(model_dfs))
      coefs.robust <- coef(model_all)
      p.values <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      # add to results - reward prediction
      pe_results <- rbind(pe_results, 
                                tibble(
                                  case = "all_subs",
                                  time = step, 
                                  Estimate = model_all$coefficients[2, "Estimate"],
                                  `Std. Error` = model_all$coefficients[2, "Std. Error"],
                                  `t value` = model_all$coefficients[2, "t value"],
                                  `Pr(>|t|)` = p.values[2]
                                ))
      

      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          
          model_sub <- summary(MASS::rlm(theta ~ reward_prediction, data = time_step_df %>% filter(elec_id == elec)))
          
          # calculate p value by getting dfs from satteraite approximated dfs
          coefs.robust <- coef(model_sub)
          p.values <- 2*pt(abs(coefs.robust[, "t value"]), model_sub$df[2], lower=FALSE)
          
          pe_results <- rbind(pe_results, 
                                    tibble(
                                      case = elec,
                                      time = step, 
                                      Estimate = model_sub$coefficients[2, "Value"],
                                      `Std. Error` = model_sub$coefficients[2, "Std. Error"],
                                      `t value` = model_sub$coefficients[2, "t value"],
                                      `Pr(>|t|)` = p.values[2]
                                    ))
          
        }, TRUE)
      }
      
      return(pe_results)
    }

    
    # save for later #
    write_csv(pe_results, path(here(), "munge", "lme_model_results", paste(region, freq, "reward_prediction_model_onset_before_turn_all_subs_log_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    pe_results <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "reward_prediction_model_onset_before_turn_all_subs_log_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- pe_results %>%
    # FDR correct across time points
    group_by(case) %>%
    mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    # mutate(pval_fdr = `Pr(>|t|)`) %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(main = if_else(case == "all_subs", "All Subjects", gsub("_.*", "", case))) %>%
    mutate(large_line = if_else(case == "all_subs", "all_subs", "indv_lines")) %>%
    # plot results
    ggplot(., aes(x = time, y = Estimate)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(aes(color = main, alpha = large_line), size = 3) +
    geom_line(aes(color = main, group = case, size = large_line, alpha = large_line)) +
    geom_label(aes(x = sig), label = "*", color = "black", y = (y_high -.03), label.size = 0.5) +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18, face = "bold"),
          legend.title = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          legend.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          axis.title = element_text(family = "Gill Sans", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          plot.subtitle =  element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          plot.title = element_text(family = "Gill Sans", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(15))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .1), guide = "none") +
    xlim(0, 1.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time from movement onset (in seconds)", y = "Beta Coefficient") +
    ggtitle(plot_title, subtitle = "")
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "prediction_onset_before_turn_indv_subs_log_robust.png", sep = "_")),
         plot = plot,
         width = 10.6,
         height = 9.5,
         units = "in",
         dpi = 300)
  
}

individual_and_overall_robust_split_lme_onset_before_turn_model_and_plot <- function(region, freq, distance_df, brain_df, plot_title, y_low, y_high, rerun_model = TRUE){

  
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
      filter(move_time < 2) %>%
      # select and scale the necessary variables for the model
      select(subject, elec_id, theta, trial_numeric, move_time, points_remaining, distance_to_ghost)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      group_by(subject) %>%
      mutate(ghost_close = if_else(distance_to_ghost > median(distance_to_ghost), "far", "close")) %>%
      ungroup()
    
    
    # Parallelized loop over steps
    results <- foreach(step=sort(unique(brain_behave_lme_df$move_time)), .combine=rbind, .packages=c("dplyr", "robustlmm", "lmerTest", "tibble")) %dopar% {
      
      # prep model result dfs - distance to ghost
      close_results <- tibble(
        case = character(0),
        time = numeric(0),
        Estimate = numeric(0),
        `Std. Error` = numeric(0),
        `t value` = numeric(0),
        `Pr(>|t|)` = numeric(0)
      )
      
      # prep model result dfs - points remaining
      far_results <- tibble(
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
        filter(move_time == step) %>%
        mutate(theta = scale(theta))
      
      # fit the model with random effects of subject and electrode
      # model_close <- summary(robustlmm::rlmer(theta ~ points_remaining + (1|subject), data = time_step_df %>% filter(ghost_close == "close")))
      # model_far <- summary(robustlmm::rlmer(theta ~ points_remaining + (1|subject), data = time_step_df %>% filter(ghost_close == "far")))
      
      ## calculate p value by getting dfs from satteraite approximated dfs
      # Close
      model_close <- summary(lmerTest::lmer(theta ~  points_remaining + (1|subject), data = time_step_df %>% filter(ghost_close == "close")))
      # coefs <- data.frame(coef(model_dfs))
      # coefs.robust <- coef(model_close)
      # p.values_close <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)

      # Far
      model_far <- summary(lmerTest::lmer(theta ~  points_remaining + (1|subject), data = time_step_df %>% filter(ghost_close == "far")))
      # coefs <- data.frame(coef(model_dfs))
      # coefs.robust <- coef(model_far)
      # p.values_far <- 2*pt(abs(coefs.robust[, "t value"]), coefs$df, lower=FALSE)
      
      # add to results - distance
      close_results <- rbind(close_results, 
                                 tibble(
                                   case = "all_subs",
                                   time = step, 
                                   Estimate = model_close$coefficients[2, "Estimate"],
                                   `Std. Error` = model_close$coefficients[2, "Std. Error"],
                                   `t value` = model_close$coefficients[2, "t value"],
                                   `Pr(>|t|)` = model_close$coefficients[2, "Pr(>|t|)"]
                                 ))
      
      # add to results - points remaining
      far_results <- rbind(far_results, 
                                tibble(
                                  case = "all_subs",
                                  time = step, 
                                  Estimate = model_far$coefficients[2, "Estimate"],
                                  `Std. Error` = model_far$coefficients[2, "Std. Error"],
                                  `t value` = model_far$coefficients[2, "t value"],
                                  `Pr(>|t|)` = model_far$coefficients[2, "Pr(>|t|)"]
                                ))
      
      # now loop by subject to get the subject specific estimates by subject
      for(elec in unique(time_step_df$elec_id)){
        
        # put into a try statement for subjects that run out of time points
        try({
          
          model_sub_close <- summary(MASS::rlm(theta ~ points_remaining, data = time_step_df %>% filter(elec_id == elec) %>% filter(ghost_close == "close")))
          model_sub_far <- summary(MASS::rlm(theta ~ points_remaining, data = time_step_df %>% filter(elec_id == elec) %>% filter(ghost_close == "far")))
          
          ## calculate p value by getting dfs from satteraite approximated dfs
          # close
          coefs.robust <- coef(model_sub_close)
          p.values_close <- 2*pt(abs(coefs.robust[, "t value"]), model_sub_close$df[2], lower=FALSE)
          # far
          coefs.robust <- coef(model_sub_far)
          p.values_far <- 2*pt(abs(coefs.robust[, "t value"]), model_sub_far$df[2], lower=FALSE)          
          
          close_results <- rbind(close_results, 
                                     tibble(
                                       case = elec,
                                       time = step, 
                                       Estimate = model_sub_close$coefficients[2, "Value"],
                                       `Std. Error` = model_sub_close$coefficients[2, "Std. Error"],
                                       `t value` = model_sub_close$coefficients[2, "t value"],
                                       `Pr(>|t|)` = p.values_close[2]
                                     ))
          
          # add to results
          far_results <- rbind(far_results, 
                                    tibble(
                                      case = elec,
                                      time = step, 
                                      Estimate = model_sub_far$coefficients[2, "Value"],
                                      `Std. Error` = model_sub_far$coefficients[2, "Std. Error"],
                                      `t value` = model_sub_far$coefficients[2, "t value"],
                                      `Pr(>|t|)` = p.values_far[2]
                                    )) 
        }, TRUE)
      }
      
      return(list(close_results_step = close_results, far_results_step = far_results))
    }
    
    
    close_results <- bind_rows(results[, "close_results_step"])
    far_results <- bind_rows(results[, "far_results_step"]) 
    
    # consolidate different model results #
    model_results_onset_before_turn_all_subs <- rbind(close_results %>% mutate(ghost_close = "close"),
                                                      far_results %>% mutate(ghost_close = "far"))
    
    # save for later #
    write_csv(model_results_onset_before_turn_all_subs, path(here(), "munge", "lme_model_results", paste(region, freq, "model_split_results_onset_before_turn_all_subs_log_robust.csv", sep = "_")))
    
  } else{
    
    # load previous model
    model_results_onset_before_turn_all_subs <- read_csv(path(here(), "munge", "lme_model_results", paste(region, freq, "model_split_results_onset_before_turn_all_subs_log_robust.csv", sep = "_")))
    
  }
  
  
  ## plot model results ##
  plot <- model_results_onset_before_turn_all_subs %>%
    # FDR correct across time points
    group_by(case) %>%
    # mutate(pval_fdr = p.adjust(`Pr(>|t|)`, "fdr")) %>%
    ungroup() %>%
    mutate(pval_fdr = `Pr(>|t|)`) %>%
    # prep df for mapping to significance, all subs vs individual subs, different regressors
    mutate(sig = if_else(pval_fdr < .05 & case == "all_subs", time, -10)) %>%
    mutate(regressor = if_else(grepl("close", ghost_close), "Ghost was close", "Ghost was far")) %>%
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
          legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18, face = "bold"),
          legend.title = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          legend.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          axis.title = element_text(family = "Gill Sans", color = '#2D2327', size = 18), 
          axis.text = element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          plot.subtitle =  element_text(family = "Gill Sans", color = '#2D2327', size = 18),
          plot.title = element_text(family = "Gill Sans", color = '#2D2327', size = 20)) +
    guides(colour = guide_legend(direction = "horizontal", title.position = "top", label.position = "right", nrow = 2)) +
    scale_color_manual(values = c("black", getPalette(15))) + #"white", "#FCC673"
    scale_size_manual(values = c(1, .5), guide  = "none") +
    scale_alpha_manual(values = c(1, .1), guide = "none") +
    xlim(0, 1.5)  + ylim(y_low, y_high) + labs(color = "", x = "Time from movement onset (in seconds)", y = "Beta Coefficient") +
    facet_wrap(~regressor, ncol = 1) +
    ggtitle(plot_title, subtitle = "Rolling robust regression with random effect of subject (black line), individual\nelectrode traces are color-coded by subject, FDR-corrected for time")
  
  # save plot #
  ggsave(path(here(), "figures", "linear_mixed_effect_models", paste(region, freq, "split_trial_onset_before_turn_indv_subs_log_robust.png", sep = "_")),
         plot = plot,
         width = 10.6,
         height = 9.5,
         units = "in",
         dpi = 300)
  
}