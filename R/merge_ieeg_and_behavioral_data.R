merge_ieeg_and_behavioral_data <- function(roi, sub, distance_df, timelock_folder, keyword_include = "*", keyword_exclude = "9999"){
  
  shift <- 0
  hc_elecs <- list.files(path(here(), "data_mount", "remote", "preprocessing", sub, "ieeg", timelock_folder))
  hc_elecs <- hc_elecs[grepl(keyword_include, hc_elecs) & !grepl(keyword_exclude, hc_elecs)]
  hc_elecs <- hc_elecs[grepl(roi, hc_elecs)]
  
  if(class(distance_df$trial_time) == 'numeric' & unique(distance_df$sfreq) == 512){
    distance_df <- distance_df %>%
      mutate(trial_time = as.character(round(trial_time, 2)))
  } else if(class(distance_df$trial_time) == 'numeric') {
    distance_df <- distance_df %>%
      mutate(trial_time = as.character(round(trial_time, 3)))
  }
  
  # get the time step #
  if(grepl("delta", keyword_include)){
    sample_step <- floor(unique(distance_df$sfreq)/2*4)
    time_step <- 1/sample_step
  } else if(grepl("theta", keyword_include)) {
    sample_step <- floor(unique(distance_df$sfreq)/5*4)
    time_step <- 1/sample_step
    
  } else if(grepl("alpha", keyword_include)) {
    sample_step <- floor(unique(distance_df$sfreq)/11*4)
    time_step <- 1/sample_step
    
  } else if(grepl("beta", keyword_include)) {
    sample_step <- floor(unique(distance_df$sfreq)/22*4)
    time_step <- 1/sample_step
    
  } else if(grepl("gamma", keyword_include)) {
    sample_step <- floor(unique(distance_df$sfreq)/50*4)
    time_step <- 1/sample_step
    
  } else if(grepl("hfa", keyword_include)) {  
    sample_step <- floor(unique(distance_df$sfreq)/110*4)
    time_step <- 1/sample_step
    
  }
  
  
  elec_full_data_all_elecs <- NULL
  for(file in hc_elecs){
    
    print(file)
    
    elec_theta_data <- read_csv(path(here(), "data_mount", "remote", "preprocessing", sub, "ieeg", timelock_folder, file))
    
    # fix col names
    elec_theta_data <- elec_theta_data %>%
      select(-any_of(c("X1", "...1"))) %>%
      rename(trial_numeric = trial)
  
    timepoints <- ncol(elec_theta_data) - 1
    colnames(elec_theta_data) <- c(paste0("bin_", colnames(elec_theta_data)[1:timepoints]), 'trial_numeric')
    
    # pivot
    elec_theta_data <- elec_theta_data %>%
      pivot_longer(names_to = 'bin', values_to = 'theta', cols = starts_with("bin")) %>%
      # mutate(bin = as.numeric(gsub("bin_", "", bin))) %>%
      # mutate(bin = paste0("bin_", if_else(bin - shift > -1, bin - shift, 999))) %>%
      # filter(bin != "bin_999") %>%
      mutate(trial_numeric = trial_numeric + 1) %>%
      mutate(trial_time = time_step * as.numeric(gsub("bin_", "", bin))) %>% 
      # mutate(trial_time = trial_time - 1) %>%
      mutate(trial_time = as.character(round(trial_time, 3)))  %>% 
      select(-bin)
    
    
    # merge 
    elec_full_data <- inner_join(elec_theta_data, distance_df)
    
    elec_full_data <- elec_full_data %>%
      mutate(trial_numeric = factor(trial_numeric)) %>%
      mutate(electrode = file)
    
    # rbind
    elec_full_data_all_elecs <- rbind(elec_full_data_all_elecs, elec_full_data)
    
  }
  
  
  elec_full_data_all_elecs <- elec_full_data_all_elecs %>%
    mutate(trial_time = as.numeric(trial_time))
  
  return(elec_full_data_all_elecs)
  
}