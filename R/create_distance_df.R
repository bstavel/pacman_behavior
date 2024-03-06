# set threat function
threat_function <- distributions3::Beta(2, 5.5)

create_distance_df_bci <- function(df, ghost = TRUE){

  if(ghost){  
    distance_df <- df %>%
      filter(Trial != "ITI") %>%
      filter(TrialType <= 16) %>% #  ghost trials only
      # filter(attack_chase_bob == 'Bob') %>%
      group_by(trial_numeric) %>%
      # user movement and distance measures
      mutate(distance_to_ghost = abs(GhostLocation - UserLocation)) %>%
      mutate(min_distance = min(distance_to_ghost)) %>%
      mutate(towards_ghost = if_else(Direction == "Left" & starting_side == "Right", "Towards",  # Left:2 Right :11
                                     if_else(Direction == "Right" & starting_side == "Left", "Towards",
                                             if_else(Direction == "Left" & starting_side == "Left", "Away",  # Left:2 Right :11
                                                     if_else(Direction == "Right" & starting_side == "Right", "Away",
                                                             if_else(Direction == "Still", "Still", "Unsure")))))) %>%
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
      mutate(discounted_reward = if_else(Eaten == 5, 0, points_remaining * 1/distance_to_next_reward)) %>%
      # filter(Direction != "Unsure") %>%
      mutate(Direction = factor(Direction)) %>%
      ungroup()
    
    
    return(distance_df)
    
  } else {
    distance_df <- df %>%
      filter(Trial != "ITI") %>%
      filter(TrialType > 16) %>% #  ghost trials only
      group_by(trial_numeric) %>%
      # user movement and distance measures
      mutate(towards_ghost = if_else(Direction == "Left" & starting_side == "Right", "Towards",  # Left:2 Right :11
                                     if_else(Direction == "Right" & starting_side == "Left", "Towards",
                                             if_else(Direction == "Left" & starting_side == "Left", "Away",  # Left:2 Right :11
                                                     if_else(Direction == "Right" & starting_side == "Right", "Away",
                                             if_else(Direction == "Still", "Still", "Unsure")))))) %>%
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
      # filter(Direction != "Unsure") %>%
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
    # filter(attack_chase_bob == 'Bob') %>%
    group_by(subject, Trial) %>%
    # user movement and distance measures
    mutate(distance_to_ghost = abs(GhostLocation - UserLocation)) %>%
    mutate(min_distance = min(distance_to_ghost)) %>%
    mutate(towards_ghost = if_else(Direction == "Left" & starting_side == "Right", "Towards",  # Left:2 Right :11
                                   if_else(Direction == "Right" & starting_side == "Left", "Towards",
                                           if_else(Direction == "Left" & starting_side == "Left", "Away",  # Left:2 Right :11
                                                   if_else(Direction == "Right" & starting_side == "Right", "Away",
                                                           if_else(Direction == "Still", "Still", "Unsure")))))) %>%
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
    # filter(Direction != "Unsure") %>%
    mutate(Direction = factor(Direction)) %>%
    mutate(discounted_reward = if_else(Eaten == 5, 0, points_remaining * 1/distance_to_next_reward)) %>%
    ungroup()
  
  return(df_distance)
  
}






