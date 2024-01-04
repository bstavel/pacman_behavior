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


prep_joint_df <- function(distance_df, sub, permute = FALSE) {
  
  if(permute){
  
    joint_dist_df <- pilot_game_data_distance %>%
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
      select(subject, trial_numeric, distance_to_ghost, last_away, trial_length, event, turnaround_time,
             away_choice, trial_time, TrialType, points_remaining) %>%
      mutate(EVENT = 1)  %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost))
    
    return(joint_dist_df)
  
  } else {
    
    joint_dist_df <- pilot_game_data_distance %>%
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
             away_choice, trial_time, TrialType) %>%
      mutate(EVENT = 1) %>%
      mutate(points_remaining = scale(points_remaining)) %>%
      mutate(distance_to_ghost = scale(distance_to_ghost))
    
    return(joint_dist_df)    
    
  }

}

create_test_train <- function(df, seed){
  ## create test and train data sets with similar distributions of `turnaround_time` ##
  
  # prep df, create stratification variable on trial length #
  df <- df %>%
    mutate(turnaround_time_cat = cut(turnaround_time, breaks = quantile(turnaround_time, probs = seq(0, 1, by = 0.1)), include.lowest = TRUE))
    
  # use caret to create partition #
  set.seed(seed)  # for reproducibility
  splitIndex <- createDataPartition(df$turnaround_time_cat, p = 0.8, list = FALSE)
  
  # return
  return(splitIndex)
  
}


create_survival_df <- function(joint_dist_df){
  # create df with only trial-level variables, for now just time of turnaround
  
  cox_df <- joint_dist_df %>%
    group_by(trial_numeric) %>%
    filter(trial_time == turnaround_time) %>%
    ungroup() %>%
    mutate(event = 1) %>%
    select(trial_numeric, turnaround_time, event, EVENT) 
  
  return(cox_df)
  
}


fit_joint_models <- function(train_long_data, train_cox_df, name){
  
  # longitudinal model #
  control = lmeControl(maxIter = 200000, niterEM = 200000, msMaxIter = 200000)
  lm_threat <- lme(distance_to_ghost ~ trial_time, data = train_long_data, random = ~trial_time | trial_numeric, control = control)
  lm_reward <- lme(points_remaining ~ trial_time, data = train_long_data, random = ~trial_time | trial_numeric, control = control)

  # survival model #
  cox_fit <- coxph(Surv(turnaround_time, EVENT) ~ 1, data = train_cox_df)
  
  # joint model #
  jm_fit <- jm(cox_fit, list(lm_threat, lm_reward), time_var = "trial_time", data_Surv = train_cox_df, 
                        n_burnin = 1000, n_iter = 30000, n_chains =8, cores = 8)
  
  # print summary #
  print(summary(lm_threat))
  print(summary(lm_reward))
  print(summary(jm_fit))
  
  # save joint model #
  saveRDS(jm_fit, path(here(), "data", "joint_models", paste0(name, ".rds")))
}




