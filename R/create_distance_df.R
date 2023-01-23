# set threat function
threat_function <- distributions3::Beta(2, 5.5)

create_distance_df_bci <- function(df, ghost = TRUE){

  if(ghost){  
    distance_df <- df %>%
      filter(Trial != "ITI") %>%
      filter(TrialType <= 16) %>% #  ghost trials only
      filter(died == 0) %>%
      group_by(trial_numeric) %>%
      # user movement and distance measures
      mutate(distance_to_ghost = abs(GhostLocation - UserLocation)) %>%
      mutate(min_distance = min(distance_to_ghost)) %>%
      mutate(towards_ghost = if_else(Direction == 2 & GhostLocation < UserLocation, "Towards",  # Left:2 Right :11
                                     if_else(Direction == 11 & GhostLocation > UserLocation, "Towards",
                                             if_else(Direction == 4, "Still", "Away")))) %>%
      mutate(away_choice_tmp = c(0, diff(factor(towards_ghost)))) %>%
      mutate(away_choice = if_else( (away_choice_tmp == -2 & towards_ghost == "Away") | 
                                      (away_choice_tmp == -1 & towards_ghost == "Away"), distance_to_ghost, 0)) %>%
      select(-away_choice_tmp) %>%
      mutate(number_of_runs = sum(away_choice > 0, na.rm = T)) %>%
      mutate(last_away = if_else(number_of_runs == 0, 0, last(away_choice[away_choice > 0]) )) %>%
      # distance to rewards
      mutate(points_remaining = if_else(reward_groups == 1 & Eaten == 0, 70, #  small	large	large	small	small, total 70
                                        if_else(reward_groups == 1 & Eaten == 1, 70 - 10,
                                                if_else(reward_groups == 1 & Eaten == 2, 70 - 10 - 20,
                                                        if_else(reward_groups == 1 & Eaten == 3, 70 - 10 - 20 - 20,
                                                                if_else(reward_groups == 1 & Eaten == 4, 70 - 10 - 20 -20 - 10,
                                                                        if_else(reward_groups == 1 & Eaten == 5, 70 - 10 - 20 -20 -10 - 10,
                                                                                if_else(reward_groups == 2 & Eaten == 0, 70, #  small	large	small	large	small, total 70
                                                                                        if_else(reward_groups == 2 & Eaten == 1, 70 - 10,
                                                                                                if_else(reward_groups == 2 & Eaten == 2, 70 - 10 -20,
                                                                                                        if_else(reward_groups == 2 & Eaten == 3, 70 - 10 -20 -10,
                                                                                                                if_else(reward_groups == 2 & Eaten == 4, 70 - 10 -20 -10 -20,
                                                                                                                        if_else(reward_groups == 2 & Eaten == 5, 70 - 10 -20 -10 -20 -10,
                                                                                                                                if_else(reward_groups == 3 & Eaten == 0, 80, #  large	small	large	small	large, total 80
                                                                                                                                        if_else(reward_groups == 3 & Eaten == 1, 80 - 20,
                                                                                                                                                if_else(reward_groups == 3 & Eaten == 2, 80 - 20 - 10,
                                                                                                                                                        if_else(reward_groups == 3 & Eaten == 3, 80 - 20 - 10 - 20,
                                                                                                                                                                if_else(reward_groups == 3 & Eaten == 4, 80 - 20 - 10 - 20 - 10,
                                                                                                                                                                        if_else(reward_groups == 3 & Eaten == 5, 80 - 20 - 10 - 20 - 10 - 20,
                                                                                                                                                                                if_else(reward_groups == 4 & Eaten == 0, 70, #  small	small	small	large	large, total 70
                                                                                                                                                                                        if_else(reward_groups == 4 & Eaten == 1, 70 - 10,
                                                                                                                                                                                                if_else(reward_groups == 4 & Eaten == 2, 70 - 10 - 10,
                                                                                                                                                                                                        if_else(reward_groups == 4 & Eaten == 3, 70 - 10 - 10 - 10,
                                                                                                                                                                                                                if_else(reward_groups == 4 & Eaten == 4, 70 - 10 - 10 - 10 - 20,
                                                                                                                                                                                                                        if_else(reward_groups == 4 & Eaten == 5, 70 - 10 - 10 - 10 - 20 - 20, 999
                                                                                                                                                                                                                        ))))))))))))))))))))))))) %>%
      mutate(points_aquired = if_else(reward_groups == 3, 80 - points_remaining, 70 - points_remaining)) %>%
      mutate(distance_to_next_reward = if_else(Eaten == 0, abs(UserLocation - Biscuit1), 
                                               if_else(Eaten == 1, abs(UserLocation - Biscuit2),
                                                       if_else(Eaten == 2, abs(UserLocation - Biscuit3),
                                                               if_else(Eaten == 3, abs(UserLocation - Biscuit4), 
                                                                       if_else(Eaten == 4, abs(UserLocation - Biscuit5), 0)))))) %>%
      mutate(cdf_distance = distributions3::cdf(threat_function, distance_to_ghost/100)) %>%
      mutate(Direction = if_else(Direction == 4, "Still", if_else(Direction == 11, "Left", "Right"))) %>%
      mutate(Direction = factor(Direction)) %>%
      mutate(discounted_reward = if_else(Eaten == 5, 0, points_remaining * 1/distance_to_next_reward)) %>%
      ungroup()
    
    
    return(distance_df)
    
  } else {
    distance_df <- df %>%
      filter(Trial != "ITI") %>%
      filter(TrialType > 16) %>% #  ghost trials only
      group_by(trial_numeric) %>%
      # user movement and distance measures
      mutate(towards_ghost = if_else(Direction == 2 & starting_side == "Right", "Towards",  # Left:2 Right :11
                                     if_else(Direction == 11 & starting_side == "Left", "Towards",
                                             if_else(Direction == 4, "Still", "Away")))) %>%
      mutate(away_choice_tmp = c(0, diff(factor(towards_ghost)))) %>%
      mutate(away_choice = if_else( (away_choice_tmp == -2 & towards_ghost == "Away") | 
                                      (away_choice_tmp == -1 & towards_ghost == "Away"), trial_time, 0)) %>%
      select(-away_choice_tmp) %>%
      mutate(number_of_runs = sum(away_choice > 0, na.rm = T)) %>%
      mutate(last_away = if_else(number_of_runs == 0, 0, last(away_choice[away_choice > 0]) )) %>%
      # distance to rewards
      mutate(points_remaining = if_else(reward_groups == 1 & Eaten == 0, 70, #  small	large	large	small	small, total 70
                                        if_else(reward_groups == 1 & Eaten == 1, 70 - 10,
                                                if_else(reward_groups == 1 & Eaten == 2, 70 - 10 - 20,
                                                        if_else(reward_groups == 1 & Eaten == 3, 70 - 10 - 20 - 20,
                                                                if_else(reward_groups == 1 & Eaten == 4, 70 - 10 - 20 -20 - 10,
                                                                        if_else(reward_groups == 1 & Eaten == 5, 70 - 10 - 20 -20 -10 - 10,
                                                                                if_else(reward_groups == 2 & Eaten == 0, 70, #  small	large	small	large	small, total 70
                                                                                        if_else(reward_groups == 2 & Eaten == 1, 70 - 10,
                                                                                                if_else(reward_groups == 2 & Eaten == 2, 70 - 10 -20,
                                                                                                        if_else(reward_groups == 2 & Eaten == 3, 70 - 10 -20 -10,
                                                                                                                if_else(reward_groups == 2 & Eaten == 4, 70 - 10 -20 -10 -20,
                                                                                                                        if_else(reward_groups == 2 & Eaten == 5, 70 - 10 -20 -10 -20 -10,
                                                                                                                                if_else(reward_groups == 3 & Eaten == 0, 80, #  large	small	large	small	large, total 80
                                                                                                                                        if_else(reward_groups == 3 & Eaten == 1, 80 - 20,
                                                                                                                                                if_else(reward_groups == 3 & Eaten == 2, 80 - 20 - 10,
                                                                                                                                                        if_else(reward_groups == 3 & Eaten == 3, 80 - 20 - 10 - 20,
                                                                                                                                                                if_else(reward_groups == 3 & Eaten == 4, 80 - 20 - 10 - 20 - 10,
                                                                                                                                                                        if_else(reward_groups == 3 & Eaten == 5, 80 - 20 - 10 - 20 - 10 - 20,
                                                                                                                                                                                if_else(reward_groups == 4 & Eaten == 0, 70, #  small	small	small	large	large, total 70
                                                                                                                                                                                        if_else(reward_groups == 4 & Eaten == 1, 70 - 10,
                                                                                                                                                                                                if_else(reward_groups == 4 & Eaten == 2, 70 - 10 - 10,
                                                                                                                                                                                                        if_else(reward_groups == 4 & Eaten == 3, 70 - 10 - 10 - 10,
                                                                                                                                                                                                                if_else(reward_groups == 4 & Eaten == 4, 70 - 10 - 10 - 10 - 20,
                                                                                                                                                                                                                        if_else(reward_groups == 4 & Eaten == 5, 70 - 10 - 10 - 10 - 20 - 20, 999
                                                                                                                                                                                                                        ))))))))))))))))))))))))) %>%
      mutate(points_aquired = if_else(reward_groups == 3, 80 - points_remaining, 70 - points_remaining)) %>%
      mutate(distance_to_next_reward = if_else(Eaten == 0, abs(UserLocation - Biscuit1), 
                                               if_else(Eaten == 1, abs(UserLocation - Biscuit2),
                                                       if_else(Eaten == 2, abs(UserLocation - Biscuit3),
                                                               if_else(Eaten == 3, abs(UserLocation - Biscuit4), 
                                                                       if_else(Eaten == 4, abs(UserLocation - Biscuit5), 0)))))) %>%
      mutate(Direction = if_else(Direction == 4, "Still", if_else(Direction == 11, "Left", "Right"))) %>%
      mutate(Direction = factor(Direction)) %>%
      mutate(discounted_reward = if_else(Eaten == 5, 0, points_remaining * 1/distance_to_next_reward)) %>%
      select(-GhostLocation) %>%
      ungroup()
    
    
    return(distance_df)
  }
  
}


create_distance_prolific <- function(df){
  
  # only difference with above function is that is also groups by subject
  
  df_distance <- df %>%
    filter(Trial != "ITI") %>%
    filter(TrialType <= 16) %>%
    mutate(trial_numeric = factor(trial_numeric)) %>%
    filter(died == 0) %>%
    group_by(subject, Trial) %>%
    # user movement and distance measures
    mutate(distance_to_ghost = abs(GhostLocation - UserLocation)) %>%
    mutate(min_distance = min(distance_to_ghost)) %>%
    mutate(towards_ghost = if_else(Direction == 2 & GhostLocation < UserLocation, "Towards", 
                                   if_else(Direction == 11 & GhostLocation > UserLocation, "Towards",
                                           if_else(Direction == 4, "Still", "Away")))) %>%
    mutate(away_choice_tmp = c(0, diff(factor(towards_ghost)))) %>%
    mutate(away_choice = if_else( (away_choice_tmp == -2 & towards_ghost == "Away") | 
                                    (away_choice_tmp == -1 & towards_ghost == "Away"), distance_to_ghost, 0)) %>%
    select(-away_choice_tmp) %>%
    mutate(number_of_runs = sum(away_choice > 0, na.rm = T)) %>%
    mutate(last_away = if_else(number_of_runs == 0, 0, last(away_choice[away_choice > 0]) )) %>%
    # distance to rewards
    mutate(points_remaining = if_else(reward_groups == 1 & Eaten == 0, 70, #  small	large	large	small	small, total 70
                                      if_else(reward_groups == 1 & Eaten == 1, 70 - 10,
                                              if_else(reward_groups == 1 & Eaten == 2, 70 - 10 - 20,
                                                      if_else(reward_groups == 1 & Eaten == 3, 70 - 10 - 20 - 20,
                                                              if_else(reward_groups == 1 & Eaten == 4, 70 - 10 - 20 -20 - 10,
                                                                      if_else(reward_groups == 1 & Eaten == 5, 70 - 10 - 20 -20 -10 - 10,
                                                                              if_else(reward_groups == 2 & Eaten == 0, 70, #  small	large	small	large	small, total 70
                                                                                      if_else(reward_groups == 2 & Eaten == 1, 70 - 10,
                                                                                              if_else(reward_groups == 2 & Eaten == 2, 70 - 10 -20,
                                                                                                      if_else(reward_groups == 2 & Eaten == 3, 70 - 10 -20 -10,
                                                                                                              if_else(reward_groups == 2 & Eaten == 4, 70 - 10 -20 -10 -20,
                                                                                                                      if_else(reward_groups == 2 & Eaten == 5, 70 - 10 -20 -10 -20 -10,
                                                                                                                              if_else(reward_groups == 3 & Eaten == 0, 80, #  large	small	large	small	large, total 80
                                                                                                                                      if_else(reward_groups == 3 & Eaten == 1, 80 - 20,
                                                                                                                                              if_else(reward_groups == 3 & Eaten == 2, 80 - 20 - 10,
                                                                                                                                                      if_else(reward_groups == 3 & Eaten == 3, 80 - 20 - 10 - 20,
                                                                                                                                                              if_else(reward_groups == 3 & Eaten == 4, 80 - 20 - 10 - 20 - 10,
                                                                                                                                                                      if_else(reward_groups == 3 & Eaten == 5, 80 - 20 - 10 - 20 - 10 - 20,
                                                                                                                                                                              if_else(reward_groups == 4 & Eaten == 0, 70, #  small	small	small	large	large, total 70
                                                                                                                                                                                      if_else(reward_groups == 4 & Eaten == 1, 70 - 10,
                                                                                                                                                                                              if_else(reward_groups == 4 & Eaten == 2, 70 - 10 - 10,
                                                                                                                                                                                                      if_else(reward_groups == 4 & Eaten == 3, 70 - 10 - 10 - 10,
                                                                                                                                                                                                              if_else(reward_groups == 4 & Eaten == 4, 70 - 10 - 10 - 10 - 20,
                                                                                                                                                                                                                      if_else(reward_groups == 4 & Eaten == 5, 70 - 10 - 10 - 10 - 20 - 20, 999
                                                                                                                                                                                                                      ))))))))))))))))))))))))) %>%
    mutate(points_aquired = if_else(reward_groups == 3, 80 - points_remaining, 70 - points_remaining)) %>%
    mutate(distance_to_next_reward = if_else(Eaten == 0, abs(UserLocation - Biscuit1), 
                                             if_else(Eaten == 1, abs(UserLocation - Biscuit2),
                                                     if_else(Eaten == 2, abs(UserLocation - Biscuit3),
                                                             if_else(Eaten == 3, abs(UserLocation - Biscuit4), 
                                                                     if_else(Eaten == 4, abs(UserLocation - Biscuit5), 0)))))) %>%
    mutate(cdf_distance = distributions3::cdf(threat_function, distance_to_ghost/100)) %>%
    mutate(Direction = if_else(Direction == 4, "Still", if_else(Direction == 11, "Left", "Right"))) %>%
    mutate(Direction = factor(Direction)) %>%
    mutate(discounted_reward = if_else(Eaten == 5, 0, points_remaining * 1/distance_to_next_reward)) %>%
    ungroup()
  
  
}



create_subject_survival_aalen <- function(df, sub){
  
  survival_df <- df %>%
    filter(last_away != 0) %>% # filter out times person ran into the ghost
    filter(subject == sub) %>%
    group_by(trial_numeric) %>%
    filter(as.character(Direction) != "Still") %>%
    mutate(Direction = factor(Direction)) %>%
    filter(dots_eaten > 0) %>%
    mutate(trial_time = trial_time - first(trial_time)) %>%
    filter(trial_time <= trial_time[away_choice == last_away]) %>%
    mutate(trial_time = round(trial_time/.05)*.05) %>%
    mutate(survival_time = max(trial_time)) %>%
    mutate(time1 = round(trial_time, digits = 2)) %>%
    mutate(tmp = n()) %>%
    filter(tmp > 3) %>%
    mutate(time2 = c(time1[2:n()], time1[n()] + .05)) %>%
    # mutate(time1_r = round(time1/.05)*.05) %>%
    # mutate(time2_r = round(time2/.05)*.05) %>%
    mutate(dif = time2 - time1) %>%
    mutate(status = if_else(survival_time ==trial_time, 1, 0 )) %>%
    mutate(censor = 1)  %>%
    mutate(dif_prob = if_else(dif < .049 | dif > .051, 1, 0)) %>%
    mutate(dif_prob_sum = sum(dif_prob)) %>%
    filter(dif_prob_sum == 0) %>%
    select(subject, trial_numeric, survival_time, min_distance, status, reward_groups, censor, UserLocation, GhostLocation, starts_with("Biscuit"),
           distance_to_ghost, jittered_start_location, Eaten, Score, Lives, Direction, time1, time2, dif, number_of_runs, 
           discounted_reward, cdf_distance, distance_to_next_reward, points_aquired, points_remaining) %>%
    distinct() 
  
  return(survival_df)
  
}


create_survival_aalen <- function(df){
  
  survival_df <- df %>%
    filter(last_away != 0) %>% # filter out times person ran into the ghost
    group_by(trial_numeric, subject) %>%
    filter(as.character(Direction) != "Still") %>%
    mutate(Direction = factor(Direction)) %>%
    filter(dots_eaten > 0) %>%
    mutate(trial_time = trial_time - first(trial_time)) %>%
    filter(trial_time <= trial_time[away_choice == last_away]) %>%
    mutate(trial_time = round(trial_time/.05)*.05) %>%
    mutate(survival_time = max(trial_time)) %>%
    mutate(time1 = round(trial_time, digits = 2)) %>%
    mutate(tmp = n()) %>%
    filter(tmp > 3) %>%
    mutate(time2 = c(time1[2:n()], time1[n()] + .05)) %>%
    # mutate(time1_r = round(time1/.05)*.05) %>%
    # mutate(time2_r = round(time2/.05)*.05) %>%
    mutate(dif = time2 - time1) %>%
    mutate(status = if_else(survival_time ==trial_time, 1, 0 )) %>%
    mutate(censor = 1)  %>%
    mutate(dif_prob = if_else(dif < .049 | dif > .051, 1, 0)) %>%
    mutate(dif_prob_sum = sum(dif_prob)) %>%
    filter(dif_prob_sum == 0) %>%
    select(subject, trial_numeric, survival_time, min_distance, status, reward_groups, censor, UserLocation, GhostLocation, starts_with("Biscuit"),
           distance_to_ghost, jittered_start_location, Eaten, Score, Lives, Direction, time1, time2, dif, number_of_runs, 
           discounted_reward, cdf_distance, distance_to_next_reward, points_aquired, points_remaining) %>%
    distinct() 
  
  return(survival_df)
  
}
