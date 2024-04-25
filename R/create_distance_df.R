# set threat function
threat_function <- distributions3::Beta(2, 5.5)

create_distance_df_bci <- function(df, ghost = TRUE){

  if(ghost){  
    distance_df <- df %>%
      filter(Trial != "ITI") %>%
      filter(TrialType <= 16) %>% #  ghost trials only
      group_by(trial_numeric) %>%
      # user movement and distance measures
      mutate(distance_to_ghost = abs(GhostLocation - UserLocation)) %>%
      mutate(min_distance = min(distance_to_ghost)) %>%
      mutate(towards_ghost = case_when(Direction == "Left" & starting_side == "Right" ~ "Towards",  # Left:2 Right :11
                                       Direction == "Right" & starting_side == "Left" ~ "Towards",
                                       Direction == "Left" & starting_side == "Left" ~ "Away",  # Left:2 Right :11
                                       Direction == "Right" & starting_side == "Right" ~ "Away",
                                       Direction == "Still" ~ "Still")) %>%
      mutate(away_choice_tmp = c(0, diff(factor(towards_ghost)))) %>%
      mutate(away_choice = if_else( (away_choice_tmp == -2 & towards_ghost == "Away") | 
                                    (away_choice_tmp == -1 & towards_ghost == "Away"), distance_to_ghost, 0)) %>%
      select(-away_choice_tmp) %>%
      mutate(number_of_runs = sum(away_choice > 0, na.rm = T)) %>%
      mutate(last_away = if_else(number_of_runs == 0, 0, last(away_choice[away_choice > 0]) )) %>%
      mutate(distance_to_next_reward = case_when(Eaten == 0 ~ abs(UserLocation - Biscuit1), 
                                                 Eaten == 1 ~ abs(UserLocation - Biscuit2),
                                                 Eaten == 2 ~ abs(UserLocation - Biscuit3),
                                                 Eaten == 3 ~ abs(UserLocation - Biscuit4), 
                                                 Eaten == 4 ~ abs(UserLocation - Biscuit5), .default =  0)) %>%
      mutate(cdf_distance = distributions3::cdf(threat_function, distance_to_ghost/100)) %>%
      mutate(discounted_reward = if_else(Eaten == 5, 0, points_remaining * 1/distance_to_next_reward)) %>%
      mutate(Direction = factor(Direction)) %>%
      ungroup()
    
    
    return(distance_df)
    
  } else {
    distance_df <- df %>%
      filter(Trial != "ITI") %>%
      filter(TrialType > 16) %>% #  ghost trials only
      group_by(trial_numeric) %>%
      # user movement and distance measures
      mutate(towards_ghost = case_when(Direction == "Left" & starting_side == "Right" ~ "Towards",  # Left:2 Right :11
                                       Direction == "Right" & starting_side == "Left" ~ "Towards",
                                       Direction == "Left" & starting_side == "Left" ~ "Away",  # Left:2 Right :11
                                       Direction == "Right" & starting_side == "Right" ~ "Away",
                                       Direction == "Still" ~ "Still")) %>%
      mutate(away_choice_tmp = c(0, diff(factor(towards_ghost)))) %>%
      mutate(away_choice = if_else( (away_choice_tmp == -2 & towards_ghost == "Away") | 
                                      (away_choice_tmp == -1 & towards_ghost == "Away"), trial_time, 0)) %>%
      select(-away_choice_tmp) %>%
      mutate(number_of_runs = sum(away_choice > 0, na.rm = T)) %>%
      mutate(last_away = if_else(number_of_runs == 0, 0, last(away_choice[away_choice > 0]) )) %>%        
      mutate(distance_to_next_reward = case_when(Eaten == 0 ~ abs(UserLocation - Biscuit1), 
                                                 Eaten == 1 ~ abs(UserLocation - Biscuit2),
                                                 Eaten == 2 ~ abs(UserLocation - Biscuit3),
                                                 Eaten == 3 ~ abs(UserLocation - Biscuit4), 
                                                 Eaten == 4 ~ abs(UserLocation - Biscuit5), .default = 0)) %>%
      mutate(Direction = factor(Direction)) %>%
      mutate(discounted_reward = if_else(Eaten == 5, 0, points_remaining * 1/distance_to_next_reward)) %>%
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
    group_by(subject, Trial) %>%
    # user movement and distance measures
    mutate(distance_to_ghost = abs(GhostLocation - UserLocation)) %>%
    mutate(min_distance = min(distance_to_ghost)) %>%
    mutate(towards_ghost = case_when(Direction == "Left" & starting_side == "Right" ~ "Towards",  # Left:2 Right :11
                                     Direction == "Right" & starting_side == "Left" ~ "Towards",
                                     Direction == "Left" & starting_side == "Left" ~ "Away",  # Left:2 Right :11
                                     Direction == "Right" & starting_side == "Right" ~ "Away",
                                     Direction == "Still" ~ "Still")) %>%
    mutate(away_choice_tmp = c(0, diff(factor(towards_ghost)))) %>%
    mutate(away_choice = if_else( (away_choice_tmp == -2 & towards_ghost == "Away") | 
                                  (away_choice_tmp == -1 & towards_ghost == "Away"), distance_to_ghost, 0)) %>%
    select(-away_choice_tmp) %>%
    mutate(number_of_runs = sum(away_choice > 0, na.rm = T)) %>%
    mutate(last_away = if_else(number_of_runs == 0, 0, last(away_choice[away_choice > 0]) )) %>%
    mutate(distance_to_next_reward = case_when(Eaten == 0, abs(UserLocation - Biscuit1), 
                                               Eaten == 1, abs(UserLocation - Biscuit2),
                                               Eaten == 2, abs(UserLocation - Biscuit3),
                                               Eaten == 3, abs(UserLocation - Biscuit4), 
                                               Eaten == 4, abs(UserLocation - Biscuit5), .default = 0)) %>%
    mutate(cdf_distance = distributions3::cdf(threat_function, distance_to_ghost/100)) %>%
    mutate(Direction = factor(Direction)) %>%
    mutate(discounted_reward = if_else(Eaten == 5, 0, points_remaining * 1/distance_to_next_reward)) %>%
    ungroup()
  
  return(df_distance)
  
}






