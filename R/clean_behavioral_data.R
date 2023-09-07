clean_bci_data <- function(df, sample_rate){
  df_clean <- df %>%
    select(-any_of(c("...1", "X1"))) %>%
    rename(subject = Subject) %>%
    # ungrouped timing variables
    rename(sample = Time) %>%
    mutate(Time = sample /sample_rate) %>%
    mutate(time_step = c(FALSE, diff(Time))) %>%
    mutate(trial_numeric = as.numeric(gsub("Trial_", "", Trial))) %>%
    # record sampling rate #
    mutate(sfreq = sample_rate) %>%
    ## grouped by trial variables
    group_by(subject, Trial) %>%
    mutate(jittered_start_location = first(UserLocation)) %>%
    mutate(base_start_location = if_else(TrialType %in% c(16, 8), 30,
                                         if_else(TrialType %in% c(19, 15, 14, 7, 6), 50,
                                                 if_else(TrialType %in% c(20, 13, 5), 70,
                                                         if_else(TrialType %in% c(17, 12, 4), 110,
                                                                 if_else(TrialType %in% c(18, 11, 10, 3, 2), 130,
                                                                         if_else(TrialType %in% c(9, 1), 150, 999))))))) %>%
    # get correct trial type info (sometimes trial type switches early/ lags)
    mutate(TrialType = as.numeric(names(sort(table(TrialType),decreasing=TRUE))[1])) %>%
    # # same deal with lives
    mutate(Lives = as.numeric(names(sort(table(Lives),decreasing=TRUE))[1])) %>%
    # biscuit location
    mutate(Biscuit1 = if_else(Biscuit1 == FALSE & base_start_location <= 80,  base_start_location + 12, 
                              if_else(Biscuit1 == FALSE & base_start_location > 80, base_start_location -12, 1111)))  %>%
    mutate(Biscuit2 = if_else(Biscuit2 == FALSE & base_start_location <= 80,  base_start_location + 22, 
                              if_else(Biscuit2 == FALSE & base_start_location > 80, base_start_location -22, 1111)))  %>%
    mutate(Biscuit3 = if_else(Biscuit3 == FALSE & base_start_location <= 80,  base_start_location + 32, 
                              if_else(Biscuit3 == FALSE & base_start_location > 80, base_start_location -32, 1111)))  %>%
    mutate(Biscuit4 = if_else(Biscuit4 == FALSE & base_start_location <= 80,  base_start_location + 42, 
                              if_else(Biscuit4 == FALSE & base_start_location > 80, base_start_location -42, 1111)))  %>%
    mutate(Biscuit5 = if_else(Biscuit5 == FALSE & base_start_location <= 80,  base_start_location + 52, 
                              if_else(Biscuit5 == FALSE & base_start_location > 80, base_start_location -52, 1111)))  %>%
    mutate(dots_eaten = max(Eaten)) %>%
    # Fix Direction
    mutate(move_step = c(0, diff(UserLocation))) %>%
    mutate(move_direction = if_else(move_step %in% c(-2, -4), "Left",
                                        if_else(move_step %in% c(2, 4), "Right", 
                                                if_else(move_step == 0, "Still", "Unsure")))) %>%
    mutate(Direction = if_else(Direction == 2, "Left", 
                                if_else(Direction == 11, "Right", 
                                        if_else(Direction == 4, "Still", "Unsure")))) %>%
    # get starting side
    mutate(starting_side = if_else(base_start_location < 95, "Left", "Right")) %>%
    # trial timing information
    mutate(Trial = if_else(Trial_on_off == 0, "ITI", Trial)) %>%
    mutate_cond(Trial == "ITI", 
                GhostLocation = NA, UserLocation = NA, Lives = NA, starting_side = NA, Direction = NA,
                Biscuit1 = NA, Biscuit2 = NA, Biscuit3 = NA, Biscuit4 = NA, Biscuit5 = NA, 
                Attack = NA, Chase = NA, Eaten = NA, TrialType = NA) %>%
    mutate(trial_time = if_else(Trial == "ITI", 999, Time - first(Time))) %>%
    mutate(trial_flip = 1:n()) %>%
    mutate(time_lag = c(FALSE, diff(trial_time))) %>%
    mutate(trial_end = as.numeric(c(diff(trial_numeric) > 0, FALSE))) %>%
    mutate(trial_length = max(trial_time)) %>%
    # trial grouping variables
    mutate(reward_groups = if_else(TrialType %in% c(1, 5, 9, 13, 20), 2, 
                                   if_else(TrialType %in% c(2, 6, 10, 14, 19), 3, 
                                           if_else(TrialType %in% c(3, 7, 11, 15, 18), 1, 
                                                   if_else(TrialType %in% c(4, 8, 12, 16, 17), 4, 99))))) %>%
    mutate(ghost_start_dir = if_else(TrialType > 16, "no_ghost", 
                                     if_else(TrialType <= 4 | TrialType >= 13, "away", "towards"))) %>%
    mutate(rewards_direction_groups = paste0(reward_groups, "_", ghost_start_dir)) %>%
    mutate(attack_chase_bob = if_else(Attack == T, "Attack", if_else(Chase == T, "Chase", "Bob"))) %>%
    ungroup() 
  
  died_trials <- df_clean %>%
    filter(Trial != "ITI") %>%
    # filter out trials where they didn't get any points, because it gets confusing it they didn't lose points
    group_by(trial_numeric) %>%
    mutate(total_eaten = max(Eaten)) %>%
    mutate(score_orig = Score) %>%
    mutate(Score = if_else(trial_numeric %% 20 == 1 & Eaten == 0, Score - first(Score), Score)) %>%
    mutate(Lives = if_else(trial_numeric %% 20 == 1, 3, Lives)) %>%
    mutate(escape = if_else(UserLocation < 10, 1,
                            if_else(UserLocation > 170, 1, 0))) %>%
    mutate(escaped = if_else(sum(escape) > 0, 1, 0)) %>% 
    ungroup() %>%
    select(trial_numeric, Lives, Score, total_eaten, TrialType, score_orig, escaped) %>%
    distinct() %>%
    mutate(died_lives = abs(as.numeric(c(diff(Lives), 0)))) %>%
    mutate(died_lives = if_else(died_lives == 0, 0, 1)) %>%
    mutate(died_score = c(diff(Score), 0)) %>%
    mutate(died_score = if_else(died_score < 0 & died_score > -90, 1, 0)) %>%
    group_by(trial_numeric) %>%
    mutate(died_score = sum(died_score)) %>%
    mutate(died_lives = sum(died_lives)) %>%
    ungroup() %>%
    mutate(died = if_else(escaped == 0 & (died_score == 1 | died_lives == 1 | trial_numeric == max(df_clean$trial_numeric)), 1, 0)) %>%
    select(trial_numeric, died) %>%
    distinct()
  

  df_clean <- left_join(df_clean, died_trials)
    
  return(df_clean)
}


clean_data_for_aalen <- function(df){
  
  df_clean <- df %>%
    # ungrouped timing variables
    mutate(time_step = c(FALSE, diff(Time))) %>%
    mutate(trial_numeric = as.numeric(gsub("Trial_", "", Trial))) %>%
    ## grouped by trial variables
    group_by(subject, Trial) %>%
    mutate(jittered_start_location = first(UserLocation)) %>%
    mutate(base_start_location = if_else(TrialType %in% c(16, 8), 30,
                                         if_else(TrialType %in% c(19, 15, 14, 7, 6), 50,
                                                 if_else(TrialType %in% c(20, 13, 5), 70,
                                                         if_else(TrialType %in% c(17, 12, 4), 110,
                                                                 if_else(TrialType %in% c(18, 11, 10, 3, 2), 130,
                                                                         if_else(TrialType %in% c(9, 1), 150, 999))))))) %>%
    # biscuit location
    mutate(Biscuit1 = if_else(Biscuit1 == FALSE & base_start_location <= 80,  base_start_location + 12, 
                              if_else(Biscuit1 == FALSE & base_start_location > 80, base_start_location -12, 1111)))  %>%
    mutate(Biscuit2 = if_else(Biscuit2 == FALSE & base_start_location <= 80,  base_start_location + 22, 
                              if_else(Biscuit2 == FALSE & base_start_location > 80, base_start_location -22, 1111)))  %>%
    mutate(Biscuit3 = if_else(Biscuit3 == FALSE & base_start_location <= 80,  base_start_location + 32, 
                              if_else(Biscuit3 == FALSE & base_start_location > 80, base_start_location -32, 1111)))  %>%
    mutate(Biscuit4 = if_else(Biscuit4 == FALSE & base_start_location <= 80,  base_start_location + 42, 
                              if_else(Biscuit4 == FALSE & base_start_location > 80, base_start_location -42, 1111)))  %>%
    mutate(Biscuit5 = if_else(Biscuit5 == FALSE & base_start_location <= 80,  base_start_location + 52, 
                              if_else(Biscuit5 == FALSE & base_start_location > 80, base_start_location -52, 1111)))  %>%
    # Fix Direction
    mutate(move = c(0, diff(UserLocation))) %>%
    mutate(Direction = if_else(move %in% c(-2, -4), "Left",
                               if_else(move %in% c(2, 4), "Right", 
                                       if_else(move == 0, "Still", "Unsure")))) %>%
    select(-move) %>%
    # trial timing information
    mutate(Trial = if_else(Trial == "Trial_1", Trial, if_else(Time == first(Time), "ITI", Trial))) %>%
    mutate_cond(Trial == "ITI", 
                GhostLocation = NA, UserLocation = NA, 
                Biscuit1 = NA, Biscuit2 = NA, Biscuit3 = NA, Biscuit4 = NA, Biscuit5 = NA, Direction = NA,
                Attack = NA, Chase = NA, Eaten = NA, TrialType = NA) %>%
    mutate(trial_time = if_else(Trial == "ITI", 999, Time - first(Time))) %>%
    mutate(trial_flip = 1:n()) %>%
    mutate(time_lag = c(FALSE, diff(trial_time))) %>%
    mutate(trial_end = as.numeric(c(diff(trial_numeric) > 0, FALSE))) %>%
    mutate(trial_length = max(trial_time)) %>%
    # trial grouping variables
    mutate(reward_groups = if_else(TrialType %in% c(1, 5, 9, 13), 2, 
                                   if_else(TrialType %in% c(2, 6, 10, 14), 3, 
                                           if_else(TrialType %in% c(3, 7, 11, 15), 1, 
                                                   if_else(TrialType %in% c(4, 8, 12, 16), 4, 99))))) %>%
    mutate(dots_eaten = max(Eaten)) %>%
    mutate(ghost_start_dir = if_else(TrialType > 16, "no_ghost", 
                                     if_else(TrialType <= 4 | TrialType >= 13, "away", "towards"))) %>%
    mutate(rewards_direction_groups = paste0(reward_groups, "_", ghost_start_dir)) %>%
    mutate(attack_chase_bob = if_else(Attack == T, "Attack", if_else(Chase == T, "Chase", "Bob"))) %>%
    mutate(ghost_objective_direction_tmp = c(FALSE, diff(GhostLocation))) %>%
    mutate(ghost_objective_direction = if_else(ghost_objective_direction_tmp >= 2, "Right", 
                                               if_else(ghost_objective_direction_tmp <= -2, "Left", "still"))) %>%
    select(-ghost_objective_direction_tmp) %>%
    mutate(ghost_direction = if_else((base_start_location < 100 & ghost_objective_direction == "Left") |
                                       (base_start_location > 100 & ghost_objective_direction == "Right") , "Towards",
                                     if_else(ghost_objective_direction == "still", "still", "Away"))) %>%
    ungroup() %>%
    group_by(subject) %>%
    arrange(subject, trial_numeric) %>%
    mutate(life_change = as.numeric(c(diff(Score) < 0, FALSE))) %>%
    group_by(subject, Trial) %>%
    mutate(died = sum(life_change)) %>%
    select(-life_change) %>%
    ungroup()
  
  # get rid of paused trials #
  paused_trials <- df_clean %>%
    filter(time_step > 1 & trial_flip != 1 & Trial != "ITI") %>%
    ungroup() %>%
    mutate(trial_ids = paste0(subject, "_", trial_numeric)) %>%
    pull(trial_ids) %>%
    unique()
  
  df_clean <- df_clean %>%
    ungroup() %>%
    mutate(trial_ids = paste0(subject, "_", trial_numeric)) %>%
    filter(!trial_ids %in% paused_trials)
  
  
  return(df_clean)
  
  
}


get_across_task_variables_bci <- function(clean_df){
  
  all_vars_df <- clean_df %>%
    group_by(subject) %>%
    filter(!is.na(Score)) %>%
    filter(Trial != "ITI") %>%
    filter(Trial != 0) %>%
    # calculate number of deaths
    mutate(death_check = as.numeric(c(diff(Score) < 0, FALSE))) %>%
    mutate(total_deaths = sum(death_check)) %>%
    mutate(max_trial = max(trial_numeric)) %>%
    mutate(trial_in_block = trial_numeric %% 20) %>%
    mutate(trial_in_block = if_else(trial_in_block == 0, 20, trial_in_block)) %>%
    # new minigame
    mutate(lives_check = as.numeric(c(diff(Lives) > 0, FALSE))) %>%
    mutate(total_games = sum(lives_check) + 1) %>%
    group_by(subject, Trial) %>%
    mutate(dots_eaten = max(Eaten)) %>%
    mutate(max_score = max(Score, na.rm = T)) %>%
    mutate(trial_died = sum(death_check)) %>%
    mutate(last_trial_in_minigame = sum(lives_check)) %>% # if lose all lives, mark as last trial
    mutate(last_trial_in_minigame = if_else(trial_in_block == 20, 1, last_trial_in_minigame)) %>% # 20 is always last trial 
    group_by(subject) %>%
    mutate(average_score = mean(max_score)) %>%
    mutate(max_time = max(Time)) %>%
    select(subject, Trial, trial_numeric, trial_in_block, TrialType, trial_length,
           trial_died, last_trial_in_minigame, Lives, dots_eaten,
           max_trial, total_deaths, average_score, max_time) %>%
    distinct()
  
  # get trials in minigame
  round <- 1
  game <- 1
  all_vars_df$trial_in_minigame <- 0
  all_vars_df$minigame <- 0
  for(idx in 1:nrow(all_vars_df)){
    # add to df
    all_vars_df$trial_in_minigame[idx] <- round
    all_vars_df$minigame[idx] <- game
    if(all_vars_df$last_trial_in_minigame[idx] == 1){
      round <- 1
      game <- game + 1
    } else {
      round <- round + 1
    }
    
    if(all_vars_df$subject[idx + 1] != all_vars_df$subject[idx] & idx != nrow(all_vars_df)) {
      round <- 1
      game <- 1
    }
    
  }
  
  # max trials in minigame and such
  all_vars_df <- all_vars_df %>%
    group_by(subject) %>%
    mutate(longest_minigame = max(trial_in_minigame)) %>%
    mutate(longest_minigame_under20 = max(trial_in_minigame[trial_in_minigame < 20])) %>%
    mutate(number_of_minigames = max(minigame)) %>%
    mutate(block = ceiling(trial_numeric/20)) %>%
    group_by(subject, block) %>%
    mutate(block_deaths = sum(trial_died)) %>%
    mutate(average_dots_per_block = mean(dots_eaten)) %>%
    ungroup()
  
  all_vars_df <- all_vars_df %>%
    mutate(block = factor(block, levels = 0:23, labels = 0:23))
  
  return(all_vars_df)
  
  
}


### Prolific

clean_prolific_data <- function(df){
  
  
  ## IMPORTANT NOTES ##
  # subject 84 and trial 48 somehow managed to press before trial began-- manually need to change startlocation to 63 and starting direction
  # subject 70 on trial 127 also pressed, true start was 57
  ## 11 is to the right
  ## 2 is to the left
  
  # clean Biscuits #
  clean_df <- df %>%
    # ungrouped timing variables
    mutate(time_step = c(FALSE, diff(Time))) %>%
    mutate(trial_numeric = as.numeric(gsub("Trial_", "", Trial))) %>%
    ## grouped by trial variables
    group_by(subject, Trial) %>%
    mutate(jittered_start_location = first(UserLocation)) %>%
    mutate(base_start_location = if_else(TrialType %in% c(16, 8), 30,
                                         if_else(TrialType %in% c(19, 15, 14, 7, 6), 50,
                                                 if_else(TrialType %in% c(20, 13, 5), 70,
                                                         if_else(TrialType %in% c(17, 12, 4), 110,
                                                                 if_else(TrialType %in% c(18, 11, 10, 3, 2), 130,
                                                                         if_else(TrialType %in% c(9, 1), 150, 999))))))) %>%
    # get correct trial type info (sometimes trial type switches early/ lags)
    mutate(TrialType = as.numeric(names(sort(table(TrialType),decreasing=TRUE))[1])) %>%
    # same deal with lives
    mutate(Lives = as.numeric(names(sort(table(Lives),decreasing=TRUE))[1])) %>%    
    # biscuit location
    mutate(Biscuit1 = if_else(Biscuit1 == FALSE & base_start_location <= 80,  base_start_location + 12, 
                              if_else(Biscuit1 == FALSE & base_start_location > 80, base_start_location -12, 1111)))  %>%
    mutate(Biscuit2 = if_else(Biscuit2 == FALSE & base_start_location <= 80,  base_start_location + 22, 
                              if_else(Biscuit2 == FALSE & base_start_location > 80, base_start_location -22, 1111)))  %>%
    mutate(Biscuit3 = if_else(Biscuit3 == FALSE & base_start_location <= 80,  base_start_location + 32, 
                              if_else(Biscuit3 == FALSE & base_start_location > 80, base_start_location -32, 1111)))  %>%
    mutate(Biscuit4 = if_else(Biscuit4 == FALSE & base_start_location <= 80,  base_start_location + 42, 
                              if_else(Biscuit4 == FALSE & base_start_location > 80, base_start_location -42, 1111)))  %>%
    mutate(Biscuit5 = if_else(Biscuit5 == FALSE & base_start_location <= 80,  base_start_location + 52, 
                              if_else(Biscuit5 == FALSE & base_start_location > 80, base_start_location -52, 1111)))  %>%
    # Fix Direction
    mutate(move_step = c(0, diff(UserLocation))) %>%
    mutate(move_direction = if_else(move_step %in% c(-2, -4), "Left",
                                    if_else(move_step %in% c(2, 4), "Right", 
                                            if_else(move_step == 0, "Still", "Unsure")))) %>%
    mutate(Direction = if_else(Direction == 2, "Left", 
                               if_else(Direction == 11, "Right", 
                                       if_else(Direction == 4, "Still", "Unsure")))) %>%
    # get starting side
    mutate(starting_side = if_else(base_start_location < 95, "Left", "Right")) %>%
    # trial timing information
    mutate(Trial = if_else(Trial == "Trial_1", Trial, if_else(Time == first(Time), "ITI", Trial))) %>%
    mutate_cond(Trial == "ITI", 
                GhostLocation = NA, UserLocation = NA, 
                Biscuit1 = NA, Biscuit2 = NA, Biscuit3 = NA, Biscuit4 = NA, Biscuit5 = NA, 
                Attack = NA, Chase = NA, Eaten = NA, TrialType = NA) %>%
    mutate(trial_time = if_else(Trial == "ITI", 999, Time - first(Time))) %>%
    mutate(trial_flip = 1:n()) %>%
    mutate(time_lag = c(FALSE, diff(trial_time))) %>%
    mutate(trial_end = as.numeric(c(diff(trial_numeric) > 0, FALSE))) %>%
    mutate(trial_length = max(trial_time)) %>%
    # trial grouping variables
    mutate(reward_groups = if_else(TrialType %in% c(1, 5, 9, 13), 2, 
                                   if_else(TrialType %in% c(2, 6, 10, 14), 3, 
                                           if_else(TrialType %in% c(3, 7, 11, 15), 1, 
                                                   if_else(TrialType %in% c(4, 8, 12, 16), 4, 99))))) %>%
    mutate(dots_eaten = max(Eaten)) %>%
    mutate(ghost_start_dir = if_else(TrialType > 16, "no_ghost", 
                                     if_else(TrialType <= 4 | TrialType >= 13, "away", "towards"))) %>%
    mutate(rewards_direction_groups = paste0(reward_groups, "_", ghost_start_dir)) %>%
    mutate(attack_chase_bob = if_else(Attack == T, "Attack", if_else(Chase == T, "Chase", "Bob"))) %>%
    mutate(ghost_objective_direction_tmp = c(FALSE, diff(GhostLocation))) %>%
    mutate(ghost_objective_direction = if_else(ghost_objective_direction_tmp >= 2, "Right", 
                                               if_else(ghost_objective_direction_tmp <= -2, "Left", "still"))) %>%
    select(-ghost_objective_direction_tmp) %>%
    mutate(ghost_direction = if_else((base_start_location < 100 & ghost_objective_direction == "Left") |
                                       (base_start_location > 100 & ghost_objective_direction == "Right") , "Towards",
                                     if_else(ghost_objective_direction == "still", "still", "Away"))) %>%
    ungroup() %>%
    group_by(subject) %>%
    arrange(subject, trial_numeric) %>%
    mutate(life_change = as.numeric(c(diff(Score) < 0, FALSE))) %>%
    group_by(subject, Trial) %>%
    mutate(died = sum(life_change)) %>%
    select(-life_change) %>%
    ungroup()
  
  died_trials <- clean_df %>%
    filter(Trial != "ITI") %>%
    # filter out trials where they didn't get any points, because it gets confusing it they didn't lose points
    group_by(trial_numeric) %>%
    mutate(total_eaten = max(Eaten)) %>%
    mutate(Score = if_else(trial_numeric %% 20 == 1 & Eaten == 0, Score - first(Score), Score)) %>%
    ungroup() %>%
    select(trial_numeric, Lives, Score, total_eaten, TrialType) %>%
    distinct() %>%
    mutate(died_lives = abs(as.numeric(c(diff(Lives), 0)))) %>%
    mutate(died_lives = if_else(died_lives == 0, 0, 1)) %>%
    mutate(died_score = c(diff(Score), 0)) %>%
    mutate(died_score = if_else(died_score < 0 & died_score > -90, 1, 0)) %>%
    group_by(trial_numeric) %>%
    mutate(died_score = sum(died_score)) %>%
    mutate(died_lives = sum(died_lives)) %>%
    ungroup() %>%
    mutate(died = if_else(died_score == 1 | died_lives == 1, 1, 0)) %>%
    select(trial_numeric, died) %>%
    distinct()
  
  
  clean_df <- left_join(clean_df, died_trials)  
  
  # get rid of paused trials #
  paused_trials <- clean_df %>%
    filter(time_step > 1 & trial_flip != 1 & Trial != "ITI") %>%
    ungroup() %>%
    mutate(trial_ids = paste0(subject, "_", trial_numeric)) %>%
    pull(trial_ids) %>%
    unique()
  
  clean_df <- clean_df %>%
    ungroup() %>%
    mutate(trial_ids = paste0(subject, "_", trial_numeric)) %>%
    filter(!trial_ids %in% paused_trials)
  
  return(clean_df)
  
}

