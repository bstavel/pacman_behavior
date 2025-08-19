detrend_signal <- function(signal) {
  time <- 1:length(signal) # Create a time index
  lm_fit <- lm(signal ~ time) # Fit a linear model
  detrended_signal <- signal - predict(lm_fit) # Subtract the trend
  return(detrended_signal)
}


calculate_overall_ccf <- function(sig_pairs, theta_df){
  
  results_df <- tibble("subject" = character(),
                       "elec1" = character(),
                       "elec2" = character(),
                       "best_lag" = numeric(),
                       "best_cor" = numeric(),
                       "best_time" = numeric())
  
  
  for(sub in unique(sig_pairs$subject)){
    
    sub_sig_pairs <- sig_pairs %>% filter(subject == sub)
    
    for(nrow in 1:nrow(sub_sig_pairs)){
      
      pair1 <- sub_sig_pairs %>% slice(nrow) %>% pull(pair1) #amyg
      pair2 <- sub_sig_pairs %>% slice(nrow) %>% pull(pair2) #hc
      
      if((pair1 %in% theta_df$elec_id) & (pair2 %in% theta_df$elec_id)){
        
        signal1_df <- theta_df %>% filter(subject == sub & elec_id == pair1) 
        signal2_df <- theta_df %>% filter(subject == sub & elec_id == pair2) 
        
        
        signal1_full <- NULL
        signal2_full <- NULL
        for(trial in unique(signal1_df$trial_numeric)){
          
          app_length <- signal1_df %>% filter(trial_numeric == trial) %>% select(move_time) %>% max()
          
          signal1 <- signal1_df %>% filter(trial_numeric == trial) %>% pull(theta) # ofc
          signal2 <- signal2_df %>% filter(trial_numeric == trial) %>% pull(theta) # hc
          
          # # detrend
          signal1_d <- detrend_signal(signal1)
          signal2_d <- detrend_signal(signal2)
          
          # bind
          signal1_full <- c(signal1_full, signal1_d)
          signal2_full <- c(signal2_full, signal2_d)
          
        }
        
        # comptue correlation
        ccf_res <- ccf(signal1_full, signal2_full, lag.max = 25, plot = F)
        
        # Identify which index of acf_vals has the largest *absolute* correlation
        acf_vals <- ccf_res$acf
        idx_max <- which.max(acf_vals)
        
        b_lag   <- ccf_res$lag[idx_max]
        b_cor   <- acf_vals[idx_max]
        b_time <- .01 * b_lag
        
        # store results
        results_df <- results_df %>% add_row(subject = sub, elec1 = pair1, elec2 = pair2, 
                                             best_lag = b_lag, best_cor = b_cor, best_time = b_time)
        
      }
      
      
    }
    
  }  
  
  return(results_df)
  
}

create_permuted_data <- function(df, all_time_df){
  
  # We'll store final results as list elements, then combine once at the end:
  df_null_list <- vector("list", length = 0)
  list_counter <- 1
  
  trial_lengths <- df %>%
    group_by(subject, elec_id, trial_numeric) %>%
    summarise(trial_length = n()) %>%
    ungroup() %>%
    distinct()
  
  df_null <- df %>%
    mutate(theta = NA)
  
  df_final_null <- tibble()
  
  
  unique_subjects <- unique(trial_lengths$subject)
  
  for (sub in unique_subjects) {
    
    # Filter once for this subject
    sub_trial_length <- trial_lengths %>% 
      filter(subject == sub)
    
    # The "null template" rows for this subject
    sub_df_null <- df_null %>% 
      filter(subject == sub)
    
    # The full data (with real theta) for this subject
    # "electrode" here must match your column in all_time_df
    full_sub_data <- all_time_df %>%
      filter(subject == sub)
    
    # For each electrode in this subject:
    for (elec in unique(sub_trial_length$elec_id)) {
      
      # Filter once for this subject/electrode
      sub_elec_trial_length <- sub_trial_length %>%
        filter(elec_id == elec)
      
      # Filter the null template for subject/electrode
      sub_elec_df_null <- sub_df_null %>%
        filter(elec_id == elec)
      
      # Filter the real data for subject/electrode
      #   You used gsub(...) to remove subject name from the electrode column,
      #   so replicate that logic:
      elec_name_in_full <- gsub(paste0(sub, "_"), "", elec)
      elec_sub_data <- full_sub_data %>%
        filter(electrode == elec_name_in_full)
      
      # Split data by trial_numeric in advance 
      # so we can grab the relevant rows without repeated filtering
      elec_sub_data_split <- split(elec_sub_data, elec_sub_data$trial_numeric)
      
      # Now iterate over each trial i
      for (i in unique(sub_elec_trial_length$trial_numeric)) {
        
        # length of trial i
        i_length <- sub_elec_trial_length$trial_length[
          sub_elec_trial_length$trial_numeric == i
        ]
        
        # Randomly pick a different trial j
        possible_j <- sub_elec_trial_length$trial_numeric[
          sub_elec_trial_length$trial_numeric != i
        ]
        chosen_j <- sample(possible_j, 1)
        
        # Grab chosen_j's full data (already split)
        j_data <- elec_sub_data_split[[as.character(chosen_j)]]
        
        # Randomly pick a start point
        sample_start <- sample(1:(nrow(j_data) - i_length), 1)
        
        
        # Subset j_data rows from sample_start to sample_start + i_length - 1
        # (Ensure j_data is in the correct row order if needed)
        j_data <- j_data[sample_start:(sample_start + i_length - 1), , drop = FALSE]
        
        # Grab the corresponding rows from the null template
        df_tmp <- sub_elec_df_null %>%
          filter(trial_numeric == i)
        
        # Fill theta from j_data
        df_tmp$theta <- j_data$theta
        
        # Store in the list
        df_null_list[[list_counter]] <- df_tmp
        list_counter <- list_counter + 1
      }
    }
  }
  
  # Combine all pieces at once
  df_final_null <- bind_rows(df_null_list)
  
  return(df_final_null)
  
}


run_and_save_perms <- function(df, sig_pairs_df, all_time_df, file_name, perms_run = NA){
  
  if(is.na(perms_run)){
    
    perms <- 1:200
    null_ccf_results <- tibble()
  } else{
    perms <- perms_run:200
    null_ccf_results <- read_csv(path(here(), "results", "ccf", file_name))
  } 
  
  # parallel plan
  old_plan <- future::plan(multisession, workers = 2)
  on.exit(future::plan(old_plan), add = TRUE)
  set.seed(123)  # reproducible seeds across workers
  
  batch_size <- 5
  
  for (i in seq(1, length(perms), by = batch_size)) {
    batch <- perms[i:min(i + batch_size - 1, length(perms))]
    
    message("Running perms: ", paste(batch, collapse = ", "))
    
    # run this batch in parallel
    batch_results <- future_lapply(
      batch,
      function(perm) {
        # light progress from workers (emits after completion)
        message("  perm ", perm)
        
        df_final_null <- create_permuted_data(df, all_time_df)
        null_ccf_tmp  <- calculate_overall_ccf(sig_pairs_df, df_final_null)
        null_ccf_tmp$perm <- perm
        null_ccf_tmp
      },
      future.seed = TRUE
    ) %>% bind_rows()
    
    # append and (optionally) save
    null_ccf_results <- bind_rows(null_ccf_results, batch_results)
    
    if (max(batch) %% 5 == 0) {
      write_csv(null_ccf_results, path(here(), "results", "ccf", file_name))
    }
  }
  
  return(null_ccf_results)
  
}

get_p_values <- function(df, df_null){
  
  df$prop_greater <- NA
  for(i in 1:nrow(df)){
    
    row <- df[i, ]
    
    null_vals <- df_null %>%
      filter(elec1 == row$elec1 & elec2 == row$elec2) %>%
      pull(best_cor)
    
    prop_greater <- sum(null_vals > row$best_cor) / length(null_vals)
    
    df[i, "prop_greater"] <- prop_greater
    
  }
  return(df)
}