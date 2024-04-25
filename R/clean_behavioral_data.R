adjust_flip_data <- function(df){
  # Function: adjust_flip_data
  # Purpose: Prepares and cleans data for analysis by adjusting trial data based on flip events.
  # Input:
  #   - df: A dataframe containing trial data, which includes variables like Trial, GhostLocation, trial_flip, etc.
  # Process:
  #   - Filters out rows where Trial is "ITI" (inter-trial intervals) to focus on relevant trials.
  #   - Groups data by 'subject' and 'trial_numeric' to handle data on a per-trial basis for each subject.
  #   - Calculates changes in ghost object location and applies a temporary variable 'ghost_objective_direction_tmp'.
  #   - Handles trial flips: resets certain metrics if trial_flip is flagged (1),
  #     identifies jump points based on significant changes in ghost position or movement,
  #     and marks these jumps for potential exclusion.
  #   - Calculates the 'jump_flip' as the minimum flip index where a jump occurs,
  #     adjusts 'disc' as the difference between the total flips and the earliest significant jump flip.
  #   - Determines jump locations as 'low' or 'high' based on 'jump_flip' values.
  #   - Filters out data based on 'remove' flags to exclude marked jumps, ensuring the first significant jump is not removed if it's in a 'low' location.
  #   - Adjusts 'trial_flip' to a relative scale starting from the first flip in each trial and recalculates the max and min flips.
  #   - Resets initial trial flip metrics to ensure a standardized starting point for further analysis.
  # Output:
  #   - Returns a cleaned and adjusted dataframe, ready for subsequent analysis steps, with flipped trial data normalized and non-relevant data points removed.
  
  
  clean_ghost_df <- df %>%
    filter(Trial != "ITI") %>%
    filter(TrialType <= 16) %>%
    group_by(subject, trial_numeric) %>%
    mutate(ghost_objective_direction_tmp = c(FALSE, diff(GhostLocation))) %>%
    mutate(
      ghost_objective_direction_tmp = ifelse(trial_flip == 1, 0, ghost_objective_direction_tmp),
      move_step = ifelse(trial_flip == 1, 0, move_step),
      jump = ifelse(ghost_objective_direction_tmp > 10 | ghost_objective_direction_tmp < -10 | move_step > 10 | move_step < -10, 1, 0),
      jump_trial = max(jump),
      jump_flip = ifelse(jump == 1, trial_flip, 9999),
      jump_flip = min(jump_flip),
      jump_flip = ifelse(jump_flip == 9999, NA, jump_flip),
      total_flips = max(trial_flip),
      disc = (total_flips - jump_flip),
      jump_location = ifelse(jump_flip < 5, "low",
                             ifelse(jump_flip > 5 & jump_flip < 9999, "high", NA)),
      remove = jump,
      remove = ifelse(jump_location == "low", rev(cummax(rev(remove))), cummax(remove)),
      remove = ifelse(jump_location == "low" & trial_flip == jump_flip, 0, remove) # don’t want to remove first jump flip for low location
    ) %>%
    filter(remove == 0 | is.na(remove)) %>%
    mutate(
      first_flip = min(trial_flip),
      trial_flip = (trial_flip - first_flip + 1),
      max_flip = max(trial_flip),
      ghost_objective_direction_tmp = ifelse(trial_flip == 1, 0, ghost_objective_direction_tmp),
      move_step = ifelse(trial_flip == 1, 0, move_step)) %>%
    select(-ghost_objective_direction_tmp, -jump, -jump_flip, -total_flips, -jump_location,  -remove, -first_flip, -max_flip, -disc)
  
  clean_noghost_df <- df %>% 
    filter(Trial != "0") %>%
    filter(TrialType > 16) %>%
    group_by(subject, trial_numeric) %>%
    mutate(
      move_step = ifelse(trial_flip == 1, 0, move_step), #repeat for noghost trials
      jump = ifelse(move_step > 10 | move_step < -10, 1, 0),
      jump_trial = max(jump),
      jump_flip = ifelse(jump == 1, trial_flip, -99),
      jump_flip = max(jump_flip),
      jump_flip = ifelse(jump_flip < 0, NA, jump_flip),
      total_flips = max(trial_flip),
      disc = (total_flips - jump_flip),
      jump_location = ifelse(jump_flip < 5 & jump_flip > 0, "low",
                             ifelse(jump_flip > 5, "high", NA)),
      remove = jump,
      remove = ifelse(jump_location == "low", rev(cummax(rev(remove))), cummax(remove)),
      remove = ifelse(jump_location == "low" & trial_flip == jump_flip, 0, remove) #dont want to remove low jump flip 3
    ) %>%
    filter(remove == 0 | is.na(remove)) %>%
    mutate(
      first_flip = min(trial_flip),
      trial_flip = (trial_flip - first_flip + 1),
      max_flip = max(trial_flip),
      move_step = ifelse(trial_flip == 1, 0, move_step)) %>%
    ungroup() %>%
    select(-jump, -jump_flip, -total_flips, -jump_location,  -remove, -first_flip, -max_flip, -disc)
  
  # recombine #
  clean_df <- bind_rows(clean_ghost_df, clean_noghost_df)
  
  # resort #
  clean_df <- clean_df %>%
    arrange(Time)
  

}

cumsum_na_ignore <- function(x) {
  # Replace NA with 0 temporarily for cumsum calculation
  cumsum_values <- cumsum(replace(x, is.na(x), 0))
  
  # Put NA back where they originally were
  cumsum_values[is.na(x)] <- NA
  
  return(cumsum_values)
}


create_died_var <- function(df) {
  # `create_died_var` is a function that processes a dataframe `df` to determine and annotate whether a subject 'died' in each trial of a gaming dataset.
  # This function performs several transformations and calculations:
  # 1. Initializes a column `died_new` with a default value of 2.
  # 2. Filters out trials labeled "ITI" or "0".
  # 3. Calculates changes in the `Lives` and `Score` columns to detect losses within each subject's trials.
  # 4. Classifies trials into types based on escape outcomes and proximity to hazards.
  # 5. Determines if a death occurred based on specific criteria across the trials:
  #    - Life loss detection: Checks if a life was lost from one trial to the next.
  #    - Made a successful exit far from the ghost
  #    - Trial ended close to the ghost and far from the exit
  #    - Score changes: Examines if the score did not increase despite potential points being collected, indicating a death might have occurred due to game mechanics.
  #    - Inter-Trial Interval (ITI) times: Analyzes the duration between trials to decide if it's long enough for a death animation, suggesting a death.
  # 6. Adjusts the `died_new` status based on trial types and additional game-related conditions such as being attacked or involved in a chase.
  # 7. Renames the final `died_new` column to `died` and returns a dataframe with `subject`, `trial_numeric`, and `died` columns, ensuring each row is unique.
  
  death_df <- df %>%
    mutate(died_new = 2) %>%
    filter(Trial != "ITI" & Trial != "0") %>%
    group_by(subject) %>%
    arrange(trial_numeric) %>%
    mutate(
      life_lost = as.numeric(c(diff(Lives) < 0, FALSE)), #does not catch trials where lives = 1 and dies, then lives back to 3
      score_lost_all = as.numeric(c(diff(Score) < 0, FALSE))
    ) %>%
    ungroup() %>%
    group_by(trial_numeric) %>%
    mutate(
      # calculate if they lost a life across trials. If so, they always died
      life_lost_trial = max(life_lost, na.rm = T),
      # prep score change. if the score did not change from the beginning to the end of the trial BUT they collected points, they always died (this avoids the block updating issue)
      first_score = first(Score),
      last_score = last(Score),
      got_dots = if_else(dots_eaten > 0, 1, 0),
      points_added = last_score - first_score,
      score_died = if_else(got_dots == 1 & points_added == 0, 1, 0),
      score_died_trial = max(score_died, na.rm = T),
      # look at the ITI - if it was under 3.15 then they did not die, because it was not long enough for the death animation
      first_time = first(Time),
      last_time = last(Time)
    ) %>%
    ungroup() %>%
    mutate(next_first_time = lead(first_time, order_by = trial_numeric)) %>%
    mutate(iti_time_tmp = next_first_time - last_time, na.rm = T) %>%
    group_by(trial_numeric) %>%
    mutate(
      iti_time = max(iti_time_tmp, na.rm = T),
      # were they very close to the ghost at the end of the trial?
      ghost_catch = if_else(is.na(GhostLocation), 0,
                            if_else(abs(last(GhostLocation) - last(UserLocation)) <= 10, 1, 0)), # change to last moment of trial rather than any moment
      ghost_catch_trial = max(ghost_catch),
      # did they exit the corridor?
      corridor_exit = ifelse(UserLocation <= 11 | UserLocation >= 169, 1, 0),
      corridor_exit_expanded = ifelse(UserLocation < 13 | UserLocation > 165, 1, 0),
      exit_trial = max(corridor_exit, na.rm = T),
      exit_trial_expanded = max(corridor_exit_expanded, na.rm = T),
      # create types by whether or not they exited the corridor and/or were close the ghost
      escaped_trialtypes = case_when(
        ghost_catch_trial == 0 & exit_trial == 1 ~ 4, # true escape
        ghost_catch_trial == 0 & exit_trial == 0 ~ 3, # not caught but not left - often paused trials
        ghost_catch_trial == 1 & exit_trial == 1 ~ 2, # close escape (caught and left)
        ghost_catch_trial == 1 & exit_trial == 0 ~ 1  # true death
      ),
      # defining death - clear cases
      died_new = if_else(life_lost_trial == 1, 1, died_new), # using 0, 1, 2 to check later, 2 = death
      died_new = if_else(escaped_trialtypes == 1, 1, died_new),
      died_new = if_else(escaped_trialtypes == 4, 0, died_new),
      # defining death - complicated
      died_new = if_else(escaped_trialtypes == 2 & score_died_trial == 1, 1, died_new),
      died_new = if_else(escaped_trialtypes == 2 & iti_time < 3, 0, died_new),
      died_new = if_else(escaped_trialtypes == 3 & iti_time < 3, 0, died_new),
      # still unsure?
      died_new = if_else(died_new == 2 & last(Attack) == T, 1, died_new),
      died_new = if_else(died_new == 2 & (last(UserLocation) >= 25 & last(UserLocation) <= 155), 1, died_new),
      died_new = if_else(died_new == 2 & last(Chase) == T, 0, died_new),
      # last
      died_new = if_else(TrialType >16, 0, died_new)
    ) %>%
    ungroup() %>%
    rename(died = died_new) %>%
    select(subject, trial_numeric, died) %>%
    distinct()
  
  return(death_df)
}

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
    # remove Trial 0 - not a real trial
    filter(Trial != "0") %>%
    ## grouped by trial variables
    group_by(subject, Trial) %>%
    mutate(jittered_start_location = first(UserLocation)) %>%
    # get correct trial type info (sometimes trial type switches early/ lags)
    mutate(TrialType = as.numeric(names(sort(table(TrialType),decreasing=TRUE))[1])) %>%
    # # same deal with lives
    mutate(Lives = as.numeric(names(sort(table(Lives),decreasing=TRUE))[1])) %>%
    mutate(base_start_location = case_when(
                                        TrialType %in% c(16, 8)            ~ 30,
                                        TrialType %in% c(19, 15, 14, 7, 6) ~ 50,
                                        TrialType %in% c(20, 13, 5)        ~ 70,
                                        TrialType %in% c(17, 12, 4)        ~ 110,
                                        TrialType %in% c(18, 11, 10, 3, 2) ~ 130,
                                        TrialType %in% c(9, 1)             ~ 150 
                                        ))%>%
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
    mutate(move_direction = case_when(move_step %in% c(-2, -4) ~ "Left",
                                      move_step %in% c(2, 4) ~ "Right", 
                                      move_step == 0 ~ "Still", .default  = "Unsure")) %>%
    mutate(Direction = case_when(Direction == 2 ~ "Left", 
                                 Direction == 11 ~ "Right", 
                                 Direction == 4 ~ "Still", .default  = "Unsure")) %>%
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
    mutate(reward_groups = case_when(TrialType %in% c(1, 5, 9, 13, 20)  ~ 2, 
                                     TrialType %in% c(2, 6, 10, 14, 19) ~ 3, 
                                     TrialType %in% c(3, 7, 11, 15, 18) ~ 1, 
                                     TrialType %in% c(4, 8, 12, 16, 17) ~ 4)) %>%
    mutate(ghost_start_dir = if_else(TrialType > 16, "no_ghost", 
                                     if_else(TrialType <= 4 | TrialType >= 13, "away", "towards"))) %>%
    mutate(rewards_direction_groups = paste0(reward_groups, "_", ghost_start_dir)) %>%
    mutate(attack_chase_bob = if_else(Attack == T, "Attack", if_else(Chase == T, "Chase", "Bob"))) %>%
    ungroup() 
  
  
  ## now adjust flips 
  clean_flip_data <- adjust_flip_data(df_clean)
  
  ## identify death trials 
  death_df <- create_died_var(clean_flip_data)
  
  # Bind with death
  clean_flip_death_data <- left_join(clean_flip_data, death_df)
  
  # Now recalcualte lives and score
  clean_flip_death_data <- clean_flip_death_data %>%
                            mutate(points_remaining = case_when(
                              # Reward Group 1
                              reward_groups == 1 & Biscuit5 == 1111 ~ 70 - 10 - 20 -20 -10 - 10, #  small	large	large	small	small, total 70
                              reward_groups == 1 & Biscuit4 == 1111 ~ 70 - 10 - 20 -20 - 10,
                              reward_groups == 1 & Biscuit3 == 1111 ~ 70 - 10 - 20 - 20,
                              reward_groups == 1 & Biscuit2 == 1111 ~ 70 - 10 - 20, 
                              reward_groups == 1 & Biscuit1 == 1111 ~ 70 - 10, 
                              reward_groups == 1 ~ 70,
                              # Reward Group 2
                              reward_groups == 2 & Biscuit5 == 1111 ~ 70 - 10 -20 -10 -20 -10, #  small	large	small	large	small, total 70
                              reward_groups == 2 & Biscuit4 == 1111 ~ 70 - 10 -20 -10 -20,
                              reward_groups == 2 & Biscuit3 == 1111 ~ 70 - 10 -20 -10, 
                              reward_groups == 2 & Biscuit2 == 1111 ~ 70 - 10 -20,
                              reward_groups == 2 & Biscuit1 == 1111 ~ 70 - 10, 
                              reward_groups == 2 ~ 70, 
                              # Reward Group 3
                              reward_groups == 3 & Biscuit5 == 1111 ~ 80 - 20 - 10 - 20 - 10 - 20, #  large	small	large	small	large, total 80
                              reward_groups == 3 & Biscuit4 == 1111 ~ 80 - 20 - 10 - 20 - 10, 
                              reward_groups == 3 & Biscuit3 == 1111 ~ 80 - 20 - 10 - 20, 
                              reward_groups == 3 & Biscuit2 == 1111 ~ 80 - 20 - 10,
                              reward_groups == 3 & Biscuit1 == 1111 ~ 80 - 20,
                              reward_groups == 3 ~ 80,
                              # Reward Group 4
                              reward_groups == 4 & Biscuit5 == 1111 ~ 70 - 10 - 10 - 10 - 20 - 20, #  small	small	small	large	large, total 70
                              reward_groups == 4 & Biscuit4 == 1111 ~ 70 - 10 - 10 - 10 - 20,
                              reward_groups == 4 & Biscuit3 == 1111 ~ 70 - 10 - 10 - 10,
                              reward_groups == 4 & Biscuit2 == 1111 ~ 70 - 10 - 10,
                              reward_groups == 4 & Biscuit1 == 1111 ~ 70 - 10,
                              reward_groups == 4 ~ 70,
                            )) %>%
                            mutate(points_aquired = if_else(reward_groups == 3, 80 - points_remaining, 70 - points_remaining)) %>%
                            group_by(Trial) %>%
                            mutate(total_trial_points = last(points_aquired)) %>%
                            # select(Trial, trial_numeric, starts_with("Biscuit"), Eaten, Score, points_aquired, points_remaining, total_trial_points, died) %>%
                            ungroup()
  
  lives_score_df <- clean_flip_death_data %>%
    filter(Trial != "ITI") %>%
    select(trial_numeric, total_trial_points, died) %>%
    distinct() %>%
    mutate(block = ceiling(trial_numeric/20) ) %>%
    group_by(block) %>%
    mutate(
      game_number = floor(cumsum(died)/3),
      game_number = c(0, game_number[1:n()-1]),
      lives_at_end_of_trial_tmp = 3 - (cumsum(died) %% 3),
      lives_at_end_of_trial = if_else(lives_at_end_of_trial_tmp == 3 & died == 1, 0, lives_at_end_of_trial_tmp),
      lives_new = if_else(died == 0, lives_at_end_of_trial, lives_at_end_of_trial + 1)
    ) %>%
    group_by(block, game_number) %>%
    mutate(
      total_trial_points = if_else(died == 1, 0,  total_trial_points),
      points_from_last_trial = lag(total_trial_points, default = 0),
      points_up_to_last_trial = cumsum(points_from_last_trial),
      score_at_trial_end = points_up_to_last_trial + total_trial_points
    ) %>%
    select(trial_numeric, points_up_to_last_trial, score_at_trial_end, lives_new, game_number)
  
  final_clean_data <- left_join(clean_flip_death_data , lives_score_df)
  
  final_clean_data <- final_clean_data %>%
    mutate(new_score = points_aquired + points_up_to_last_trial) %>%
    rename(old_score = Score, old_lives = Lives, Score = new_score, Lives = lives_new)
    
  return(final_clean_data)
}

get_across_task_variables_bci <- function(clean_df){
  
  all_vars_df <- clean_df %>%
    group_by(subject) %>%
    filter(!is.na(Score)) %>%
    filter(Trial != "ITI") %>%
    filter(Trial != 0) %>%
    mutate(max_trial = max(trial_numeric)) %>%
    mutate(trial_in_block = trial_numeric %% 20) %>%
    mutate(trial_in_block = if_else(trial_in_block == 0, 20, trial_in_block)) %>%
    # new minigame
    mutate(lives_check = as.numeric(c(diff(Lives) > 0, FALSE))) %>%
    mutate(total_games = sum(lives_check) + 1) %>%
    group_by(subject, Trial) %>%
    mutate(dots_eaten = max(Eaten)) %>%
    mutate(max_score = max(Score, na.rm = T)) %>%
    mutate(last_trial_in_minigame = sum(lives_check)) %>% # if lose all lives, mark as last trial
    mutate(last_trial_in_minigame = if_else(trial_in_block == 20, 1, last_trial_in_minigame)) %>% # 20 is always last trial 
    group_by(subject) %>%
    mutate(average_score = mean(max_score)) %>%
    mutate(max_time = max(Time)) %>%
    select(subject, Trial, trial_numeric, trial_in_block, TrialType, trial_length,
           died, last_trial_in_minigame, Lives, dots_eaten,
           max_trial, average_score, max_time) %>%
    distinct() %>%
    # calculate number of deaths
    mutate(total_deaths = sum(died)) 
  
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
    mutate(block_deaths = sum(died)) %>%
    mutate(average_dots_per_block = mean(dots_eaten)) %>%
    ungroup()
  
  all_vars_df <- all_vars_df %>%
    mutate(block = factor(block, levels = 1:max(block), labels = 1:max(block)))
  
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
    # get correct trial type info (sometimes trial type switches early/ lags)
    mutate(TrialType = as.numeric(names(sort(table(TrialType),decreasing=TRUE))[1])) %>%
    # same deal with lives
    mutate(Lives = as.numeric(names(sort(table(Lives),decreasing=TRUE))[1])) %>%  
    # get jittered and base starting locations
    mutate(jittered_start_location = first(UserLocation)) %>%
    mutate(base_start_location = case_when(
                                        TrialType %in% c(16, 8)            ~ 30,
                                        TrialType %in% c(19, 15, 14, 7, 6) ~ 50,
                                        TrialType %in% c(20, 13, 5)        ~ 70,
                                        TrialType %in% c(17, 12, 4)        ~ 110,
                                        TrialType %in% c(18, 11, 10, 3, 2) ~ 130,
                                        TrialType %in% c(9, 1)             ~ 150 
    ))%>%
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
    mutate(move_direction = case_when(move_step %in% c(-2, -4) ~ "Left",
                                      move_step %in% c(2, 4) ~ "Right", 
                                      move_step == 0 ~ "Still", .default  = "Unsure")) %>%
    mutate(Direction = case_when(Direction == 2 ~ "Left", 
                                 Direction == 11 ~ "Right", 
                                 Direction == 4 ~ "Still", .default  = "Unsure")) %>%
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
    mutate(reward_groups = case_when(TrialType %in% c(1, 5, 9, 13, 20)  ~ 2, 
                                     TrialType %in% c(2, 6, 10, 14, 19) ~ 3, 
                                     TrialType %in% c(3, 7, 11, 15, 18) ~ 1, 
                                     TrialType %in% c(4, 8, 12, 16, 17) ~ 4)) %>%
    mutate(dots_eaten = max(Eaten)) %>%
    mutate(ghost_start_dir = if_else(TrialType > 16, "no_ghost", 
                             if_else(TrialType <= 4 | TrialType >= 13, "away", "towards"))) %>%
    mutate(rewards_direction_groups = paste0(reward_groups, "_", ghost_start_dir)) %>%
    mutate(attack_chase_bob = if_else(Attack == T, "Attack", 
                              if_else(Chase == T, "Chase", "Bob"))) %>%
    mutate(ghost_objective_direction_tmp = c(FALSE, diff(GhostLocation))) %>%
    mutate(ghost_objective_direction = if_else(ghost_objective_direction_tmp >= 2, "Right", 
                                       if_else(ghost_objective_direction_tmp <= -2, "Left", "still"))) %>%
    select(-ghost_objective_direction_tmp) %>%
    mutate(ghost_direction = if_else((base_start_location < 100 & ghost_objective_direction == "Left") |
                                      (base_start_location > 100 & ghost_objective_direction == "Right") , "Towards",
                             if_else(ghost_objective_direction == "still", "still", "Away"))) %>%
    mutate(points_remaining = case_when(
      # Reward Group 1
      reward_groups == 1 & Biscuit5 == 1111 ~ 70 - 10 - 20 -20 -10 - 10, #  small	large	large	small	small, total 70
      reward_groups == 1 & Biscuit4 == 1111 ~ 70 - 10 - 20 -20 - 10,
      reward_groups == 1 & Biscuit3 == 1111 ~ 70 - 10 - 20 - 20,
      reward_groups == 1 & Biscuit2 == 1111 ~ 70 - 10 - 20, 
      reward_groups == 1 & Biscuit1 == 1111 ~ 70 - 10, 
      reward_groups == 1 ~ 70,
      # Reward Group 2
      reward_groups == 2 & Biscuit5 == 1111 ~ 70 - 10 -20 -10 -20 -10, #  small	large	small	large	small, total 70
      reward_groups == 2 & Biscuit4 == 1111 ~ 70 - 10 -20 -10 -20,
      reward_groups == 2 & Biscuit3 == 1111 ~ 70 - 10 -20 -10, 
      reward_groups == 2 & Biscuit2 == 1111 ~ 70 - 10 -20,
      reward_groups == 2 & Biscuit1 == 1111 ~ 70 - 10, 
      reward_groups == 2 ~ 70, 
      # Reward Group 3
      reward_groups == 3 & Biscuit5 == 1111 ~ 80 - 20 - 10 - 20 - 10 - 20, #  large	small	large	small	large, total 80
      reward_groups == 3 & Biscuit4 == 1111 ~ 80 - 20 - 10 - 20 - 10, 
      reward_groups == 3 & Biscuit3 == 1111 ~ 80 - 20 - 10 - 20, 
      reward_groups == 3 & Biscuit2 == 1111 ~ 80 - 20 - 10,
      reward_groups == 3 & Biscuit1 == 1111 ~ 80 - 20,
      reward_groups == 3 ~ 80,
      # Reward Group 4
      reward_groups == 4 & Biscuit5 == 1111 ~ 70 - 10 - 10 - 10 - 20 - 20, #  small	small	small	large	large, total 70
      reward_groups == 4 & Biscuit4 == 1111 ~ 70 - 10 - 10 - 10 - 20,
      reward_groups == 4 & Biscuit3 == 1111 ~ 70 - 10 - 10 - 10,
      reward_groups == 4 & Biscuit2 == 1111 ~ 70 - 10 - 10,
      reward_groups == 4 & Biscuit1 == 1111 ~ 70 - 10,
      reward_groups == 4 ~ 70,
    )) %>%
    mutate(points_aquired = if_else(reward_groups == 3, 80 - points_remaining, 70 - points_remaining)) %>%
    ungroup() %>%
    group_by(subject) %>%
    arrange(subject, trial_numeric) %>%
    mutate(life_change = as.numeric(c(diff(Score) < 0, FALSE))) %>%
    group_by(subject, Trial) %>%
    mutate(died = sum(life_change)) %>%
    select(-life_change) %>%
    ungroup()
  
  ## identify death trials 
  death_df <- create_died_var(clean_df)
  
  # Bind with death
  clean_df <- left_join(clean_df, death_df)
  
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

