compute_lfp_correlation <- function(sub_list, roi1, roi2, freq_df){

  # Loop over subjects #
  for(sub in sub_list){
    
    # filter to sub df
    sub_df <- freq_df %>% filter(subject == sub)
    
    # pull the first set of regions
    roi1_elecs <- sub_df %>%
      filter(region == roi1) %>%
      pull(elec_short) %>%
      unique()
    
    # pull the second set of regions
    roi2_elecs <- sub_df %>%
      filter(region == roi2) %>%
      pull(elec_short) %>%
      unique()
    
    # prep df to hold the estimated difference in correlation distributions and the pvalue
    tests <- tibble(subject = character(), elec1 = character(), elec2 = character(), estimate = numeric(), statistics = numeric(), pval = numeric())
    
    # loop over roi1 elecs
    for(elec1 in roi1_elecs){
      
      # filter to elec1 of roi1 elecs
      elec1_df <- sub_df %>% filter(elec_short == elec1)
      
      # loop over all elecs in roi2
      for(elec2 in roi2_elecs){
        print(paste("Correlation Between", elec1, "and", elec2))
        
        # filter to elec2 of roi2 elecs
        elec2_df <- sub_df %>% filter(elec_short == elec2) %>% filter(region == roi2)
        
        # prep permuted null
        true_cor <- NULL
        null_cor <- matrix(nrow = length(unique(sub_df$trial_numeric)), ncol = length(unique(sub_df$trial_numeric)))
        
        # loop over trials
        for(t in 1:length(unique(sub_df$trial_numeric))){
          
          # compute true correlations during trial t
          trial <- unique(sub_df$trial_numeric)[t]
          t1 <- elec1_df  %>% filter(trial_numeric == trial)  %>% pull(theta)
          t2 <- elec2_df  %>% filter(trial_numeric == trial) %>% pull(theta)
          true_cor[t] <- cor(t1, t2)
          
          # compute correlation over all other trials
          null_cor[t,] <- foreach(h = 1:length(unique(sub_df$trial_numeric)), .inorder=FALSE, .combine = 'c') %dopar% {
            null_trial <- unique(sub_df$trial_numeric)[h]
            
            # we have already computed correlation from permuted trials where null_trial < trial
            if(null_trial > trial){
              t1 <- elec1_df  %>% filter(trial_numeric == trial)  %>% pull(theta)
              t2 <- elec2_df  %>% filter(trial_numeric == null_trial) %>%  pull(theta)
              return(cor(t1, t2))
            } else {
              return(NA)
            }
          }
        }  
        
        # compute difference
        null_cor <- null_cor[!is.na(null_cor)]
        test_result <- wilcox.test(true_cor, null_cor, conf.int = TRUE)
        tests <- rbind(tests, c(sub, elec1, elec2, test_result$estimate, test_result$statistic, test_result$p.value))
        
        # save out files
        write.csv(true_cor, path(here(), "results", "cor_analysis", paste(sub, elec1, "to", elec2, "true_cor.csv", sep = "_")))
        write.csv(null_cor, path(here(), "results", "cor_analysis", paste(sub, elec1, "to", elec2, "null_cor.csv", sep = "_")))
        write.csv(tests, path(here(), "results", "cor_analysis", paste0(sub, "_correlation_wilcox_test_", roi1, "_", roi2, ".csv")))
        
        
      }
      
    }
    
  }
  
}