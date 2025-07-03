### Bayesian Helpers

## Random Effects

get_subject_elec_res <- function(model, fixed_effect, random_effect){
  # model: model of interest, from loaded brms object
  # fixed_effect: fixed effect, as a character, e.g. "time"
  # random_effect: random effect, as a character, e.g. "key", "electrode" (assumes there is also always "subject)

  # pull random efftcs
  subject_random_effects <- ranef(model, groups = "subject")
  
  # Extract posterior samples of random effects
  posterior_ranef_subject <- as_draws_df(model) %>%
    select(starts_with("r_subject["))
  
  posterior_ranef_electrode <- as_draws_df(model) %>%
    select(starts_with(paste0("r_subject:", random_effect, "[")))
  
  # Extract posterior samples of the fixed effect
  posterior_fixed <- as_draws_df(model, variable = paste0("b_", fixed_effect))
  
  ## Pull Subject Level Effects
  # Get the list of subjects
  subjects <- unique(model$data$subject)
  
  # Initialize a list to store subject-specific effect samples
  subject_effects <- list()
  
  # Loop over each subject to create subject-level effects
  for (subject in subjects) {
    # Create the variable name for the random effect
    ranef_var <- paste0("r_subject[", subject, paste0(",", fixed_effect, "]"))
    
    # Check if the variable exists in the posterior samples
    if (ranef_var %in% names(posterior_ranef_subject)) {
      ranef_samples <- posterior_ranef_subject[[ranef_var]]
    } else {
      stop(paste("Random effect variable", ranef_var, "not found in posterior samples."))
    }
    
    # Combine fixed and random effects
    subject_specific_samples <- posterior_fixed[[paste0("b_", fixed_effect)]] + ranef_samples
    
    # Store the samples
    subject_effects[[as.character(subject)]] <- subject_specific_samples
  }
  
  # Initialize a data frame to store the results
  subject_ci <- data.frame(
    subject = character(),
    mean = numeric(),
    lower = numeric(),
    upper = numeric(),
    significant = logical(),
    stringsAsFactors = FALSE
  )
  
  ## calculate CI around Subject-level effects
  for (subject in names(subject_effects)) {
    # Get the samples
    samples <- subject_effects[[subject]]
    
    # Calculate mean and credible intervals
    mean_effect <- mean(samples)
    ci_lower <- quantile(samples, probs = 0.025)
    ci_upper <- quantile(samples, probs = 0.975)
    
    # Determine if CI includes zero
    significant <- ci_lower > 0 | ci_upper < 0
    
    # Add to the data frame
    subject_ci <- rbind(subject_ci, data.frame(
      subject = subject,
      mean_sub = mean_effect,
      lower_sub = ci_lower,
      upper_sub = ci_upper,
      significant_sub = significant,
      stringsAsFactors = FALSE
    ))
  }
  
  ## Electrode Effects
  # Extract unique combinations of subject and key from the model data
  electrode_info <- unique(model$data[, c("subject", random_effect)])
  
  # Create a unique identifier for electrodes (e.g., subject_key)
  electrode_info$subject_key <- paste0(electrode_info$subject, "_", electrode_info[[random_effect]])
  
  # Initialize a list to store electrode-specific effect samples
  electrode_effects <- list()
  
  # Loop over each electrode
  for (i in 1:nrow(electrode_info)) {
    subject <- electrode_info$subject[i]
    key <- electrode_info[[random_effect]][i]
    subject_key <- electrode_info$subject_key[i]
    
    # Create variable names for random effects
    ranef_subject_var <- paste0("r_subject[", subject, paste0(",", fixed_effect, "]"))
    ranef_electrode_var <- gsub(" ", ".", paste0("r_subject:", random_effect, "[", subject_key, paste0(",", fixed_effect, "]")))
    
    # Check if variables exist in the posterior samples
    if (ranef_subject_var %in% names(posterior_ranef_subject)) {
      ranef_subject_samples <- posterior_ranef_subject[[ranef_subject_var]]
    } else {
      stop(paste("Random effect variable", ranef_subject_var, "not found in posterior samples."))
    }
    
    if (ranef_electrode_var %in% names(posterior_ranef_electrode)) {
      ranef_electrode_samples <- posterior_ranef_electrode[[ranef_electrode_var]]
    } else {
      stop(paste("Random effect variable", ranef_electrode_var, "not found in posterior samples."))
    }
    
    # Combine fixed effect, subject random effect, and electrode random effect
    electrode_specific_samples <- posterior_fixed[[paste0("b_", fixed_effect)]] + ranef_subject_samples + ranef_electrode_samples
    
    # Store the samples
    electrode_effects[[subject_key]] <- electrode_specific_samples
  }
  
  
  # Initialize a data frame to store the results
  electrode_ci <-  data.frame(
    subject_key = character(),
    subject = character(),
    mean_key = numeric(),
    lower_key = numeric(),
    upper_key = numeric(),
    significant_key = logical(),
    stringsAsFactors = FALSE
  )
  
  for (subject_key in names(electrode_effects)) {
    # Get the samples
    samples <- electrode_effects[[subject_key]]
    
    # Calculate mean and credible intervals
    mean_effect <- mean(samples)
    ci_lower <- quantile(samples, probs = 0.025)
    ci_upper <- quantile(samples, probs = 0.975)
    
    # Determine if CI includes zero
    significant <- ci_lower > 0 | ci_upper < 0
    
    # Extract subject and key from subject_key
    parts <- strsplit(subject_key, "_")[[1]]
    subject <- parts[1]
    
    # Add to data frame
    electrode_ci <- rbind(electrode_ci, data.frame(
      subject_key = subject_key,
      subject = subject,
      mean_key = mean_effect,
      lower_key = ci_lower,
      upper_key = ci_upper,
      significant_key = significant,
      stringsAsFactors = FALSE
    ))
  }
  
  # bind electrode ci and subject_ci
  subject_electrode_ci <- full_join(subject_ci, electrode_ci)
  
  
   return(subject_electrode_ci)

}



get_subject_elec_res_with_intercept <- function(model,
                                                time_var   = "scale_logged_times",
                                                region_var = "region_pair",
                                                random_var = "roi_pair1") {
  # 1) pull data + draws
  dat       <- model$data %>%
    mutate(
      across(all_of(c(region_var, random_var)),
             ~ as.character(.x))
    )
  draws_df  <- as_draws_df(model)
  N_draws   <- nrow(draws_df)
  
  # 2) get your unique region levels as CHARACTER
  region_lvls <- unique(dat[[region_var]])
  
  # 3) extract fixed‐effect posteriors
  fixed_main <- draws_df[[paste0("b_", time_var)]]
  fixed_int  <- map(region_lvls, function(lvl) {
    # look for the exact column name in draws_df
    pat <- paste0("^b_", time_var, ":", region_var, lvl, "$")
    nm  <- str_subset(names(draws_df), pat)
    if (length(nm) == 1) {
      draws_df[[nm]]
    } else {
      # reference‐level or not found → zeros
      rep(0, N_draws)
    }
  }) %>% set_names(region_lvls)
  
  # 4) pull the two random‐slope blocks once
  ranef_sub_df <- draws_df %>% select(starts_with("r_subject["))
  ranef_roi_df <- draws_df %>% select(starts_with(paste0("r_subject:", random_var, "[")))
  
  # 5) build subject‐by‐region summaries
  subject_ci <- expand_grid(
    subject = unique(dat$subject),
    region  = region_lvls
  ) %>%
    pmap_dfr(function(subject, region) {
      # locate their random slope
      sub_var <- paste0("r_subject[", subject, ",", time_var, "]")
      sub_samps <- ranef_sub_df[[sub_var]]
      # combine: main + interaction + subject‐slope
      all_samps <- fixed_main + fixed_int[[region]] + sub_samps
      
      ci <- quantile(all_samps, probs = c(0.025, 0.975))
      tibble(
        subject         = subject,
        region          = region,
        mean_sub        = mean(all_samps),
        lower_sub       = ci[1],
        upper_sub       = ci[2],
        significant_sub = (ci[1] > 0) | (ci[2] < 0)
      )
    })
  
    # 6) electrode (subject:roi_pair1) by region
    electrode_info <- dat %>%
      distinct(
        subject,
        !!sym(random_var),
        !!sym(region_var)
      ) %>%
      transmute(
        subject     = subject,
        region      = .data[[region_var]],
        roi         = .data[[random_var]],
        subject_key = paste0(subject, "_", roi)
      )
    
    electrode_ci <- pmap_dfr(electrode_info, function(subject, region, roi, subject_key) {
      sub_var <- paste0("r_subject[", subject, ",", time_var, "]")
      roi_var <- paste0("r_subject:", random_var, "[", subject_key, ",", time_var, "]") %>%
        str_replace_all(" ", ".")
      
      sub_samps <- ranef_sub_df[[sub_var]]
      roi_samps <- ranef_roi_df[[roi_var]]
      all_samps <- fixed_main + fixed_int[[region]] + sub_samps + roi_samps
      
      ci <- quantile(all_samps, probs = c(0.025, 0.975))
      tibble(
        subject_key      = subject_key,
        subject          = subject,
        region           = region,
        mean_key         = mean(all_samps),
        lower_key        = ci[1],
        upper_key        = ci[2],
      significant_key  = (ci[1] > 0) | (ci[2] < 0)
      )
    })
  
  # 7) join and return
  left_join(subject_ci, electrode_ci, by = c("subject", "region"))
  }


# values to find CI intervals in the density function, helper for get_region_effects_for_turnaround_models function
closest_value <- function(x, v) {
  # Calculate the absolute differences between each element in v and x
  differences <- abs(v - x)
  
  # Find the index of the minimum difference
  index_of_min <- which.min(differences)
  
  # Return the value in v at the index of the minimum difference
  closest <- v[index_of_min]
  return(closest)
}

# Function to summarize posterior samples, helper for get_region_effects_for_turnaround_models function
summarize_posterior <- function(samples) {
  mean_val <- mean(samples)
  sd_val <- sd(samples)
  ci_lower <- quantile(samples, 0.025)
  ci_upper <- quantile(samples, 0.975)
  max_density = max(density(samples)$y)
  ci_lower_dens = density(samples)$y[density(samples)$x == closest_value(ci_lower, density(samples)$x)]
  ci_upper_dens = density(samples)$y[density(samples)$x == closest_value(ci_upper, density(samples)$x)]
  prob_positive <- mean(samples > 0)
  tibble(
    Mean = mean_val,
    SD = sd_val,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    Max_Density = max_density,
    CI_Lower_Density = ci_lower_dens,
    CI_Upper_Density = ci_upper_dens,
    Prob_Positive = prob_positive
  )
}

get_region_effects_for_turnaround_models <- function(model, region_pairs){
  # model: brms turnaround model
  # region_pairs: list of pairs, unique(correlation_sig_df$region_pair)

  # Function to extract sample for each region pair
  get_samples_for_region <- function(region) {
    if (region == "amyg_cing") {
      slope <- as_draws_df(model)$b_scale_logged_times
    } else {
      interaction_term <- paste0("b_scale_logged_times:region_pair", region)
      slope <- as_draws_df(model)$b_scale_logged_times + as_draws_df(model)[[interaction_term]]
    }
  }
  
  # Function to extract slopes for each region pair
  get_slope_for_region <- function(region) {
    if (region == "amyg_cing") {
      slope <- as_draws_df(model)$b_scale_logged_times
    } else {
      interaction_term <- paste0("b_scale_logged_times:region_pair", region)
      slope <- as_draws_df(model)$b_scale_logged_times + as_draws_df(model)[[interaction_term]]
    }
    summarize_posterior(slope)
  }
  
  # get all samples #
  full_sample <-  lapply(region_pairs, get_samples_for_region)
  names(full_sample) <- region_pairs
  full_sample_df <- as_tibble(full_sample)
  
  # Summarize slopes for all region pairs
  slopes_summary <- lapply(region_pairs, get_slope_for_region)
  names(slopes_summary) <- region_pairs
  
  # Combine summaries into a data frame
  slopes_summary_df <- bind_rows(slopes_summary, .id = "Region_Pair")
  
  return(slopes_summary_df)

}
