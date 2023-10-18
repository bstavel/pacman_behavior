merge_theta_and_behavioral_data <- function(roi, sub, distance_df, timepoints, keyword_include = "*", keyword_exclude = "choice"){
  
  shift <- 0
  hc_elecs <- list.files(path(here(), "data", "theta_csvs", roi, sub))
  hc_elecs <- hc_elecs[grepl(keyword_include, hc_elecs) & !grepl(keyword_exclude, hc_elecs)]
  
  elec_full_data_all_elecs <- NULL
  for(file in hc_elecs){
    
    print(file)
    
    elec_theta_data <- read_csv(path(here(), "data", "theta_csvs", roi, sub, file))
    
    # fix col names
    elec_theta_data <- elec_theta_data %>%
      select(-any_of(c("X1", "...1")))
    if(!"trial_numeric" %in% colnames(elec_theta_data)){
      elec_theta_data <- elec_theta_data %>%
        rename(trial_numeric = trial)
    }
    colnames(elec_theta_data) <- c(paste0("bin_", colnames(elec_theta_data)[1:timepoints]), 'trial_numeric')
    
    # pivot
    elec_theta_data <- elec_theta_data %>%
      pivot_longer(names_to = 'bin', values_to = 'theta', cols = starts_with("bin")) %>%
      mutate(bin = as.numeric(gsub("bin_", "", bin))) %>%
      mutate(bin = paste0("bin_", if_else(bin - shift > -1, bin - shift, 999))) %>%
      filter(bin != "bin_999") %>%
      mutate(trial_numeric = trial_numeric + 1) %>%
      mutate(trial_time = .05 * as.numeric(gsub("bin_", "", bin))) %>% 
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