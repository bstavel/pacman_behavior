create_last_away_events <- function(df, fname) {
  
  last_away_df <- df %>%
    filter(Trial != "ITI") %>%
    filter(trial_numeric != 0) %>%
    filter(died == 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    group_by(neural_trial_numeric) %>%
    mutate(towards_ghost = if_else(Direction == "Left" & starting_side == "Right", "Towards",  # Left:2 Right :11
                                   if_else(Direction == "Right" & starting_side == "Left", "Towards",
                                           if_else(Direction == "Left" & starting_side == "Left", "Away",  # Left:2 Right :11
                                                   if_else(Direction == "Right" & starting_side == "Right", "Away",
                                                           if_else(Direction == "Still", "Still", "Unsure")))))) %>%
    mutate(away_choice_tmp = c(0, diff(factor(towards_ghost)))) %>%
    mutate(away_choice = if_else( (away_choice_tmp == -2 & towards_ghost == "Away") | 
                                    (away_choice_tmp == -1 & towards_ghost == "Away"), sample, 0)) %>%
    select(-away_choice_tmp) %>%
    mutate(number_of_runs = sum(away_choice > 0, na.rm = T)) %>%
    mutate(last_away = if_else(number_of_runs == 0, 0, last(away_choice[away_choice > 0]) )) %>%
    ungroup() %>%
    filter(sample == last_away)
  
  print(anyDuplicated(last_away_df$neural_trial_numeric))
  
  
  last_away_clean_df <- last_away_df %>%
    ungroup() %>%
    mutate(sample_before = 0) %>%
    mutate(event = 1) %>%
    select(neural_trial_numeric, TrialType, sample, sample_before, event)
  
  
  write_csv(last_away_clean_df, paste0(path(here(), './data/ieeg_behave/', fname)))
  
  # diagnostic plots to check it works
  test <- left_join(df, last_away_clean_df %>% select(sample, event))
  
  plot1 <- test %>%
    mutate(event = if_else(event ==1, trial_time, event)) %>%
    filter(Trial != "ITI") %>%
    filter(trial_length <= 10) %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    filter(!(neural_trial_numeric %in% bad_df$neural_trial_numeric)) %>%
    filter(trial_numeric < 100) %>%
    pivot_longer(cols = c(GhostLocation, UserLocation), values_to = "location", names_to = "unit") %>%
    ggplot(., aes(x = trial_time, y = location, color = unit)) +
    geom_hline(yintercept = 170, color = "black") +
    geom_hline(yintercept = 10, color = "black") +
    geom_vline(aes(xintercept = event), color = 'black') +
    geom_point() +
    geom_line() +
    theme(panel.background = element_rect(fill = "white")) +
    facet_wrap(~neural_trial_numeric)
  
  plot2 <- test %>%
    mutate(event = if_else(event ==1, trial_time, event)) %>%
    filter(Trial != "ITI") %>%
    filter(trial_length <= 10) %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    filter(!(neural_trial_numeric %in% bad_df$neural_trial_numeric)) %>%
    filter(trial_numeric > 100 & trial_numeric <=200) %>%
    pivot_longer(cols = c(GhostLocation, UserLocation), values_to = "location", names_to = "unit") %>%
    ggplot(., aes(x = trial_time, y = location, color = unit)) +
    geom_hline(yintercept = 170, color = "black") +
    geom_hline(yintercept = 10, color = "black") +
    geom_vline(aes(xintercept = event), color = 'black') +
    geom_point() +
    geom_line() +
    theme(panel.background = element_rect(fill = "white")) +
    facet_wrap(~neural_trial_numeric)
  
  plot3 <- test %>%
    mutate(event = if_else(event ==1, trial_time, event)) %>%
    filter(Trial != "ITI") %>%
    filter(trial_length <= 10) %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    filter(!(neural_trial_numeric %in% bad_df$neural_trial_numeric)) %>%
    filter(trial_numeric > 200) %>%
    pivot_longer(cols = c(GhostLocation, UserLocation), values_to = "location", names_to = "unit") %>%
    ggplot(., aes(x = trial_time, y = location, color = unit)) +
    geom_hline(yintercept = 170, color = "black") +
    geom_hline(yintercept = 10, color = "black") +
    geom_vline(aes(xintercept = event), color = 'black') +
    geom_point() +
    geom_line() +
    theme(panel.background = element_rect(fill = "white")) +
    facet_wrap(~neural_trial_numeric)
  
  print(plot1)
  print(plot2)
  print(plot3)
}


create_first_dot_events <- function(df, fname) {
  
  first_dot_df <- df %>%
    filter(Trial != "ITI") %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    group_by(neural_trial_numeric) %>%
    filter(Biscuit1 == 1111) %>%
    filter(trial_time == min(trial_time)) %>%
    ungroup() %>%
    mutate(sample_before = 0) %>%
    mutate(event = 1) %>%
    select(neural_trial_numeric, TrialType, sample, sample_before, event)
 
  
  print(anyDuplicated(first_dot_df$neural_trial_numeric))
  
  
  first_dot_clean_df <- first_dot_df %>%
    ungroup() %>%
    mutate(sample_before = 0) %>%
    mutate(event = 1) %>%
    select(neural_trial_numeric, TrialType, sample, sample_before, event)
  
  
  write_csv(first_dot_clean_df, paste0(path(here(), './data/ieeg_behave/', fname)))
  
}


create_first_move_events <- function(df, fname) {
  
  first_move_df <- df %>%
    filter(Trial != "ITI") %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    group_by(neural_trial_numeric) %>%
    filter(Direction != "Still") %>%
    filter(trial_time == min(trial_time)) %>%
    ungroup() %>%
    mutate(sample_before = 0) %>%
    mutate(event = 1) %>%
    select(neural_trial_numeric, TrialType, sample, sample_before, event)
  
  
  print(anyDuplicated(first_move_df$neural_trial_numeric))
  
  
  first_move_clean_df <- first_move_df %>%
    ungroup() %>%
    mutate(sample_before = 0) %>%
    mutate(event = 1) %>%
    select(neural_trial_numeric, TrialType, sample, sample_before, event)
  
  
  write_csv(first_move_clean_df, paste0(path(here(), './data/ieeg_behave/', fname)))
  
}


create_died_events <- function(df, fname) {
  
  # for Died events, we will time lock to end trial, so we do not need to get event samples. 
  # Instead, we will create a metadata df that we can easily stratify trials where they died or not
  # if ghost last position if across midline of starting side, count as chase, this way we exclude chases that only just began
  # as the person exited the trial
  
 
  died_df <- df %>%
    filter(Trial != "ITI") %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    group_by(neural_trial_numeric) %>%
    mutate(Attack = any(Attack))%>%
    mutate(ghost_cross_tmp = if_else(starting_side == "Right" & as.numeric(GhostLocation > 100), 1,
                                     if_else(starting_side == "Left" & as.numeric(GhostLocation < 100), 1, 0))) %>%
    mutate(ghost_cross = sum(ghost_cross_tmp)) %>%
    mutate(Chase = any(Chase)) %>%
    mutate(chase_trial = if_else(TrialType <= 16 & (Chase == TRUE | Attack == TRUE) & died == 0 & ghost_cross > 3, 1, 0)) %>%
    select(neural_trial_numeric, TrialType, died, Attack, chase_trial, trial_length) %>%
    ungroup() %>%
    distinct()
  
  print(anyDuplicated(died_df$neural_trial_numeric))
  
  
  write_csv(died_df, paste0(path(here(), './data/ieeg_behave/', fname)))
  
  # diagnostic plots
  chase_trials <- died_df %>%
    filter(chase_trial == 1) %>%
    pull(neural_trial_numeric)
  attack_trials <- died_df %>%
    filter(died == 1) %>%
    pull(neural_trial_numeric)
  
  plot1 <- df %>%
    filter(Trial != "ITI") %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    filter(neural_trial_numeric %in% chase_trials) %>%
    filter(!(neural_trial_numeric %in% bad_df$neural_trial_numeric)) %>%
    pivot_longer(cols = c(GhostLocation, UserLocation), values_to = "location", names_to = "unit") %>%
    ggplot(., aes(x = trial_time, y = location, color = unit)) +
    geom_hline(yintercept = 170, color = "black") +
    geom_hline(yintercept = 10, color = "black") +
    geom_point() +
    geom_line() +
    theme(panel.background = element_rect(fill = "white")) +
    facet_wrap(~neural_trial_numeric) +
    ggtitle("chase")
  
  plot2 <- df %>%
    filter(Trial != "ITI") %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    filter(neural_trial_numeric %in% attack_trials) %>%
    filter(!(neural_trial_numeric %in% bad_df$neural_trial_numeric)) %>%
    pivot_longer(cols = c(GhostLocation, UserLocation), values_to = "location", names_to = "unit") %>%
    ggplot(., aes(x = trial_time, y = location, color = unit)) +
    geom_hline(yintercept = 170, color = "black") +
    geom_hline(yintercept = 10, color = "black") +
    geom_point() +
    geom_line() +
    theme(panel.background = element_rect(fill = "white")) +
    facet_wrap(~neural_trial_numeric)  +
    ggtitle("attack")
  
  print(plot1)
  print(plot2)
  
  
}



create_attack_events <- function(df, fname) {
  
  # moment of attack when the person was still approaching.
  # includes diagnostic plots to ensure it is working as expected
  
  
  attack_df <- df %>%
    filter(Trial != "ITI") %>%
    filter(trial_numeric != 0) %>%
    filter(TrialType <= 16) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    group_by(neural_trial_numeric) %>%
    # mutate(towards_ghost = if_else(Direction == "Left" & starting_side == "Right", "Towards",  # Left:2 Right :11
    #                                if_else(Direction == "Right" & starting_side == "Left", "Towards",
    #                                        if_else(Direction == "Left" & starting_side == "Left", "Away",  # Left:2 Right :11
    #                                                if_else(Direction == "Right" & starting_side == "Right", "Away",
    #                                                        if_else(Direction == "Still", "Still", "Unsure")))))) %>%
    mutate(dist_to_exit = if_else(starting_side == "Left", UserLocation - 10, 170 - UserLocation)) %>%
    mutate(distance_to_ghost = abs(UserLocation - GhostLocation)) %>%
    filter(Chase == TRUE | Attack == TRUE) %>%
    filter(sample == first(sample)) %>%
    filter(distance_to_ghost <= dist_to_exit) %>%
    ungroup()
  
  print(anyDuplicated(attack_df$neural_trial_numeric))
  
  
  attack_clean_df <- attack_df %>%
    ungroup() %>%
    mutate(sample_before = 0) %>%
    mutate(event = 1) %>%
    select(neural_trial_numeric, TrialType, sample, sample_before, event)
  
  
  write_csv(attack_clean_df, paste0(path(here(), './data/ieeg_behave/', fname)))
  
  # diagnostic plots to check it works
  test <- left_join(df, attack_clean_df %>% select(sample, event))
  
  plot1 <- test %>%
    mutate(event = if_else(event ==1, trial_time, event)) %>%
    filter(Trial != "ITI") %>%
    filter(trial_length <= 10) %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    filter(!(neural_trial_numeric %in% bad_df$neural_trial_numeric)) %>%
    filter(trial_numeric < 100) %>%
    pivot_longer(cols = c(GhostLocation, UserLocation), values_to = "location", names_to = "unit") %>%
    ggplot(., aes(x = trial_time, y = location, color = unit)) +
    geom_hline(yintercept = 170, color = "black") +
    geom_hline(yintercept = 10, color = "black") +
    geom_vline(aes(xintercept = event), color = 'black') +
    geom_point() +
    geom_line() +
    theme(panel.background = element_rect(fill = "white")) +
    facet_wrap(~neural_trial_numeric)
  
  plot2 <- test %>%
    mutate(event = if_else(event ==1, trial_time, event)) %>%
    filter(Trial != "ITI") %>%
    filter(trial_length <= 10) %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    filter(!(neural_trial_numeric %in% bad_df$neural_trial_numeric)) %>%
    filter(trial_numeric > 100 & trial_numeric <=200) %>%
    pivot_longer(cols = c(GhostLocation, UserLocation), values_to = "location", names_to = "unit") %>%
    ggplot(., aes(x = trial_time, y = location, color = unit)) +
    geom_hline(yintercept = 170, color = "black") +
    geom_hline(yintercept = 10, color = "black") +
    geom_vline(aes(xintercept = event), color = 'black') +
    geom_point() +
    geom_line() +
    theme(panel.background = element_rect(fill = "white")) +
    facet_wrap(~neural_trial_numeric)
  
  plot3 <- test %>%
    mutate(event = if_else(event ==1, trial_time, event)) %>%
    filter(Trial != "ITI") %>%
    filter(trial_length <= 10) %>%
    filter(trial_numeric != 0) %>%
    mutate(neural_trial_numeric = trial_numeric - 1) %>%
    filter(!(neural_trial_numeric %in% bad_df$neural_trial_numeric)) %>%
    filter(trial_numeric > 200) %>%
    pivot_longer(cols = c(GhostLocation, UserLocation), values_to = "location", names_to = "unit") %>%
    ggplot(., aes(x = trial_time, y = location, color = unit)) +
    geom_hline(yintercept = 170, color = "black") +
    geom_hline(yintercept = 10, color = "black") +
    geom_vline(aes(xintercept = event), color = 'black') +
    geom_point() +
    geom_line() +
    theme(panel.background = element_rect(fill = "white")) +
    facet_wrap(~neural_trial_numeric)
  
  print(plot1)
  print(plot2)
  print(plot3)
  
  
}
   
