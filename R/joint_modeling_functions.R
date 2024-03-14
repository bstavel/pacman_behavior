random_time_series <- function(n, low_val, high_val) {
  timeseries = numeric(n)
  timeseries[1] = sample(low_val:high_val, 1)
  
  for (i in 2:n) {
    change = sample(c(-2, 2), 1)
    
    # Check boundaries and adjust if necessary
    if (timeseries[i-1] + change < 0) {
      change = 2
    } else if (timeseries[i-1] + change > 100) {
      change = -2
    }
    
    timeseries[i] = timeseries[i-1] + change
  }
  return(timeseries)
  
}


prep_joint_orig_df <- function(distance_df, sub, permute = FALSE, ieeg = FALSE) {
  
    if(permute){
    
      joint_dist_df <- distance_df %>%
        filter(subject == sub) %>%
        filter(Direction != "Still") %>%
        filter(dots_eaten != 0) %>%
        group_by(trial_numeric) %>%
        # prep time variables
        mutate(trial_time = trial_time - first(trial_time)) %>%
        mutate(event = if_else(last_away == away_choice & away_choice != 0, 1, 0)) %>%
        mutate(turnaround_time_tmp = if_else(event == 1, trial_time, 0)) %>%
        mutate(turnaround_time = max(turnaround_time_tmp)) %>%
        ungroup() %>%
        filter(turnaround_time != 0) %>% # exclude trials where they just ran into the ghost
        filter(trial_time <= turnaround_time) %>%
        group_by(trial_numeric) %>%
        # prep permuted predictors 
        mutate(distance_to_ghost = random_time_series(length(distance_to_ghost), 0, 100)) %>%
        mutate(points_remaining = sample(points_remaining, length(points_remaining))) %>%
        ungroup() %>%
        select(subject, trial_numeric, distance_to_ghost, last_away, event, turnaround_time,
               away_choice, trial_time, TrialType, points_remaining, reward_groups) %>%
        mutate(EVENT = 1)  %>%
        mutate(points_remaining = scale(points_remaining)) %>%
        mutate(distance_to_ghost = scale(distance_to_ghost))
      
      return(joint_dist_df)
    
    } else {
      
      joint_dist_df <- distance_df %>%
        filter(subject == sub) %>%
        filter(Direction != "Still") %>%
        filter(dots_eaten != 0) %>%
        group_by(trial_numeric) %>%
        # prep time variables
        mutate(trial_time = trial_time - first(trial_time)) %>%
        mutate(event = if_else(last_away == away_choice & away_choice != 0, 1, 0)) %>%
        mutate(turnaround_time_tmp = if_else(event == 1, trial_time, 0)) %>%
        mutate(turnaround_time = max(turnaround_time_tmp)) %>%
        ungroup() %>%
        filter(turnaround_time != 0) %>% # exclude trials where they just ran into the ghost
        filter(trial_time <= turnaround_time) %>%
        select(subject, trial_numeric, distance_to_ghost, last_away, points_remaining, event, turnaround_time,
               away_choice, trial_time, TrialType, reward_groups) %>%
        mutate(EVENT = 1) %>%
        mutate(points_remaining = scale(points_remaining)) %>%
        mutate(distance_to_ghost = scale(distance_to_ghost))
      
      return(joint_dist_df)    
      
    }

}


prep_joint_df <- function(distance_df, sub, permute = FALSE, ieeg = FALSE) {
  
  if(permute){
    
    joint_dist_df <- distance_df %>%
      filter(subject == sub) %>%
      filter(Direction != "Still") %>%
      filter(dots_eaten != 0) %>%
      filter(died == 0) %>%
      group_by(trial_numeric) %>%
      # prep time variables
      mutate(jm_time = trial_time - first(trial_time)) %>%
      mutate(event = if_else(last_away == away_choice & away_choice != 0, 1, 0)) %>%
      mutate(turnaround_time_tmp = if_else(event == 1, jm_time, 0)) %>%
      mutate(turnaround_time = max(turnaround_time_tmp)) %>%
      ungroup() %>%
      filter(turnaround_time != 0) %>% # exclude trials where they just ran into the ghost
      filter(jm_time <= turnaround_time) %>%
      group_by(trial_numeric) %>%
      # prep permuted predictors 
      mutate(distance_to_ghost = random_time_series(length(distance_to_ghost), 0, 100)) %>%
      mutate(points_remaining = sample(points_remaining, length(points_remaining))) %>%
      ungroup() %>%
      select(subject, trial_numeric, distance_to_ghost, last_away, event, turnaround_time,
             away_choice, trial_time, jm_time, TrialType, points_remaining, reward_groups) %>%
      mutate(EVENT = 1)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) %>%
      mutate(trial_time = round(trial_time, 2)) %>%
      mutate(jm_time = round(jm_time, 2))
    
    return(joint_dist_df)
    
  } else {
    
    joint_dist_df <- distance_df %>%
      filter(subject == sub) %>%
      filter(Direction != "Still") %>%
      filter(dots_eaten != 0) %>%
      filter(died == 0) %>%
      group_by(trial_numeric) %>%
      # prep time variables
      mutate(jm_time = trial_time - first(trial_time)) %>%
      mutate(event = if_else(last_away == away_choice & away_choice != 0, 1, 0)) %>%
      mutate(turnaround_time_tmp = if_else(event == 1, jm_time, 0)) %>%
      mutate(turnaround_time = max(turnaround_time_tmp)) %>%
      ungroup() %>%
      filter(turnaround_time != 0) %>% # exclude trials where they just ran into the ghost
      filter(jm_time <= turnaround_time) %>%
      select(subject, trial_numeric, distance_to_ghost, Direction, last_away, points_remaining, event, turnaround_time,
             away_choice, trial_time, jm_time, TrialType, reward_groups) %>%
      mutate(EVENT = 1) %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost)) %>%
      mutate(trial_time = round(trial_time, 2)) %>%
      mutate(jm_time = round(jm_time, 2))
    
    return(joint_dist_df)    
    
  }
  
}


prep_for_nerual_predictors <- function(joint_dist_df, theta_df){
  
  
  ## Merge iEEG data with behavioral data ##
  joint_theta_dist_df <- left_join(joint_dist_df %>% mutate(trial_time = round(trial_time, 2)), 
                                   theta_df %>% mutate(trial_time = round(trial_time, 2)))
  
  ## Create hc theta average ##
  joint_theta_dist_df <- joint_theta_dist_df %>%
    filter(!is.na(electrode)) %>%
    mutate(trial_time = round(trial_time, 2)) %>%
    group_by(trial_numeric, trial_time) %>%
    mutate(mean_elec_theta = mean(theta)) %>%
    select(-electrode, -theta) %>%
    distinct() %>%
    ungroup()  %>%
    mutate(mean_elec_theta = scale(mean_elec_theta)) %>%
    mutate(sin_term = sin(2 * pi * trial_time / 5)) %>%
    mutate(cos_term = cos(2 * pi * trial_time / 5)) %>%
    mutate(trial_time = round(trial_time, 2)) %>%
    mutate(turnaround_time = round(turnaround_time, 2))
  
  
  return(joint_theta_dist_df)
  
}

create_test_train <- function(df, seed){
  ## create test and train data sets with similar distributions of `turnaround_time` ##
  
  # prep df, create stratification variable on trial length #
  df <- df %>%
    mutate(turnaround_time_cat = 
             cut(turnaround_time, breaks = quantile(turnaround_time, 
                                                    probs = seq(0, 1, by = 0.2)), include.lowest = TRUE)) %>%
    mutate(strata = paste(reward_groups, turnaround_time_cat, sep="_"))
  
  set.seed(seed)  # for reproducibility
  # split_index <- createDataPartition(df$strata, 
  #                                    p = 0.8, 
  #                                    list = FALSE, 
  #                                    times = 1)
  split_index <- createFolds(df$strata, k = 5, list = TRUE, returnTrain = TRUE)
  
  # return
  return(split_index)
  
}


create_survival_df <- function(joint_dist_df){
  # create df with only trial-level variables, for now just time of turnaround
  
  cox_df <- joint_dist_df %>%
    mutate(event = 1) %>%
    select(trial_numeric, turnaround_time, event, EVENT) %>%
    distinct()
  
  return(cox_df)
  
}


fit_joint_reward_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 100000, niterEM = 100000, msMaxIter = 100000)
  lm_reward <- lme(points_remaining ~ trial_time, data = train_long_data, 
                   random = ~trial_time | trial_numeric, 
                   control = control)

  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_reward), time_var = "trial_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 8)
  
  # print summary #
  print(summary(lm_reward))
  print(summary(jm_fit))
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, ".rds")))
  
  return(jm_fit)
}


fit_joint_threat_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_threat <- lme(distance_to_ghost ~ trial_time, data = train_long_data, random = ~trial_time | trial_numeric, control = control)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_threat), time_var = "trial_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 8)
  
  # print summary #
  print(summary(lm_threat))
  print(summary(jm_fit))
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, ".rds")))
}

fit_null_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_null <- lme(trial_time ~ 1, data = train_long_data, random = ~1 | trial_numeric, control = control)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_null), time_var = "trial_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 8)
  
  # print summary #
  print(summary(lm_null))
  print(summary(jm_fit))
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, ".rds")))
}


fit_joint_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_threat <- lme(distance_to_ghost ~ jm_time, data = train_long_data, random = ~jm_time | trial_numeric, control = control)
  lm_reward <- lme(points_remaining ~ jm_time, data = train_long_data, random = ~jm_time | trial_numeric, control = control)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_threat, lm_reward), time_var = "jm_time", data_Surv = train_cox_df, 
               n_burnin = 2000, n_iter = 40000, n_chains =8, cores = 6)
  
  # print summary #
  print(kable(summary(lm_threat)$tTable, caption = "Threat Model") %>% kable_styling())
  print(kable(summary(lm_reward)$tTable, caption = "Reward Model") %>% kable_styling())
  # print(summary(jm_fit))
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, ".rds")))
  
  return(jm_fit)
}

fit_joint_models_all_trials <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_threat <- lme(distance_to_ghost ~ trial_time:ghost_trial, data = train_long_data, random = ~trial_time | trial_numeric, control = control)
  lm_reward <- lme(points_remaining ~ trial_time, data = train_long_data, random = ~trial_time | trial_numeric, control = control)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_threat, lm_reward), time_var = "trial_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 6)
  
  # print summary #
  kable(summary(lm_threat)$tTable, caption = "Threat Model") %>% kable_styling()
  kable(summary(lm_reward)$tTable, caption = "Reward Model") %>% kable_styling()
  # print(summary(jm_fit))
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, ".rds")))
  
  return(jm_fit)
}

fit_joint_time_theta_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_theta <- lme(mean_theta ~ jm_time, data = train_long_data, random = ~jm_time | trial_numeric)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_theta), time_var = "jm_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 6)
  
  # print summary #
  print(kable(summary(lm_theta)$tTable, caption = "Threat Model") %>% kable_styling())
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, "_theta_time.rds")))
  
  return(jm_fit)
}


fit_joint_time_theta_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_theta <- lme(mean_theta ~ jm_time, data = train_long_data, random = ~jm_time | trial_numeric)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_theta), 
               time_var = "jm_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 6)
  
  # print summary #
  print(kable(summary(lm_theta)$tTable, caption = "Threat Model") %>% kable_styling())
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, "_theta_time.rds")))
  
  return(jm_fit)
}

fit_joint_time_slope_theta_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_theta <- lme(mean_theta ~ jm_time, data = train_long_data, random = ~jm_time | trial_numeric)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_theta), functional_forms = ~value(mean_theta) + slope(mean_theta),
               time_var = "jm_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 6)
  
  # print summary #
  print(kable(summary(lm_theta)$tTable, caption = "Threat Model") %>% kable_styling())
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, "_theta_time_slope.rds")))
  
  return(jm_fit)
}


fit_joint_spline_theta_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_theta <- lme(mean_theta ~  ns(jm_time, knots = c(.5)), 
                  data = train_long_data, 
                  random = ~jm_time | trial_numeric, control = control)
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_theta), time_var = "jm_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 6)
  
  # print summary #
  print(kable(summary(lm_theta)$tTable, caption = "Threat Model") %>% kable_styling())
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, "_theta_spline.rds")))
  
  return(jm_fit)
}

fit_joint_theta_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_threat <- lme(distance_to_ghost ~ trial_time, data = train_long_data, random = ~trial_time | trial_numeric, control = control)
  lm_reward <- lme(points_remaining ~ trial_time, data = train_long_data, random = ~trial_time | trial_numeric, control = control)
  lm_theta <- lme(mean_elec_theta ~ sin_term + cos_term, data = train_long_data, random = ~trial_time | trial_numeric, control = control)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_threat, lm_reward, lm_theta), time_var = "trial_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 6)
  
  # print summary #
  print(kable(summary(lm_threat)) %>% kable_styling())
  print(kable(summary(lm_reward)) %>% kable_styling())
  # print(summary(jm_fit))
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, ".rds")))
}

fit_hc_theta_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_theta <- lme(mean_elec_theta ~ sin_term + cos_term, data = train_long_data, random = ~trial_time | trial_numeric, control = control)
  
  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_threat, lm_reward, lm_theta), time_var = "trial_time", data_Surv = train_cox_df, 
               n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 6)
  
  # print summary #
  print(kable(summary(lm_threat)) %>% kable_styling())
  print(kable(summary(lm_reward)) %>% kable_styling())
  # print(summary(jm_fit))
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, ".rds")))
}




