compile_ieeg_csv_files <- function(roi, sub, sample_freq, timelock_folder, keyword_include = "*", keyword_exclude = "9999"){
  
  shift <- 0
  hc_elecs <- list.files(path(here(), "data_mount", "remote", "pacman", "preprocessing", sub, "ieeg", timelock_folder))
  hc_elecs <- hc_elecs[grepl(keyword_include, hc_elecs) & !grepl(keyword_exclude, hc_elecs)]
  hc_elecs <- hc_elecs[grepl(roi, hc_elecs)]

  
  # get the time step #
    sample_step <- sample_freq/20
    time_step <- sample_step/sample_freq
  
  elec_full_data_all_elecs <- NULL
  for(file in hc_elecs){
    
    print(file)
    
    elec_theta_data <- read_csv(path(here(), "data_mount", "remote", "pacman", "preprocessing", sub, "ieeg", timelock_folder, file))
    
    # fix col names
    elec_theta_data <- elec_theta_data %>%
      select(-any_of(c("X1", "...1"))) %>%
      rename(trial_numeric = trial)
    
    timepoints <- ncol(elec_theta_data) - 1
    colnames(elec_theta_data) <- c(paste0("bin_", colnames(elec_theta_data)[1:timepoints]), 'trial_numeric')
    
    # pivot
    elec_clean_data <- elec_theta_data %>%
      pivot_longer(names_to = 'bin', values_to = 'theta', cols = starts_with("bin")) %>%
      # mutate(bin = as.numeric(gsub("bin_", "", bin))) %>%
      # mutate(bin = paste0("bin_", if_else(bin - shift > -1, bin - shift, 999))) %>%
      # filter(bin != "bin_999") %>%
      mutate(trial_numeric = trial_numeric + 1) %>%
      mutate(trial_time = time_step * as.numeric(gsub("bin_", "", bin))) %>% 
      mutate(trial_time = trial_time - 1) %>%
      mutate(trial_time = as.character(round(trial_time, 3)))  %>% 
      select(-bin) %>%
      mutate(trial_numeric = factor(trial_numeric)) %>%
      mutate(electrode = file) %>%
      mutate(subject = sub)
    
    # rbind
    elec_full_data_all_elecs <- rbind(elec_full_data_all_elecs, elec_clean_data)
    
  }
  
  
  elec_full_data_all_elecs <- elec_full_data_all_elecs %>%
    mutate(trial_time = as.numeric(trial_time))
  
  return(elec_full_data_all_elecs)
  
}