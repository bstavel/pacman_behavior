### Bayesian Helpers


pull_bayesian_model_summaries <- function(model, coef_name){
  
  post <- as_draws_df(model)[[coef_name]]
  
  # Replace mean=0, sd=2 with your declared prior for this coefficient
  bf <- bayesfactor_parameters(
    posterior = post,
    prior     = distribution_normal(n = 50000, mean = 0, sd = 2),
    null      = 0
  )
  
  P_plus   <- mean(post > 0)
  
  # Grab the model summary
  model_summary <- summary(model)
  
  # Extract row for this coefficient
  row <- model_summary$fixed[gsub("b_", "", coef_name), ]
  
  result <- tibble(
    mean     = round(row["Estimate"], 2),
    CrI_2.5  = round(row["l-95% CI"], 2),
    CrI_97.5 = round(row["u-95% CI"], 2),
    P_plus   = round(P_plus, 2),
    BF10     = round(exp(bf$log_BF), 2),
    ESS_bulk = round(row["Bulk_ESS"], 1),
    ESS_tail = round(row["Tail_ESS"], 1)
  )
  
  return(result)
  
}


extract_model_summaries_factor_analysis <- function(cur_model, cur_regions){
  
  # Extract posterior samples of fixed effects coefficients
  posterior_samples <- as_draws_df(cur_model) %>% select(starts_with("b_"))
  
  # Create all possible pairs of regions
  region_pairs <- expand.grid(Region1 = cur_regions, Region2 = cur_regions) %>%
    filter(Region1 != Region2)
  
  # compute intercept
  intercept <- mean(posterior_samples$b_Intercept)
  
  results <- data.frame(
    Region1 = character(),
    Region2 = character(),
    Intercept = numeric(),
    Mean = numeric(),
    max_density = numeric(),
    ci_upper = numeric(),
    ci_lower = numeric(),
    P_plus = numeric(),
    BF10 =numeric(),
    ESS = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(region_pairs)) {
    region1 <- region_pairs$Region1[i]
    region2 <- region_pairs$Region2[i]
    
    # Compute difference samples
    difference_samples <- compute_difference_samples(region1, region2, cur_regions, posterior_samples)
    
    # Compute mean
    mean_diff <- mean(difference_samples)
    
    
    # Compute mode (max density)
    density_estimate <- density(difference_samples)
    mode_diff <- density_estimate$x[which.max(density_estimate$y)]
    max_dens = max(density_estimate$y)
    
    # Compute credible intervals
    ci_low <- quantile(difference_samples, probs = 0.025)
    ci_up <- quantile(difference_samples, probs = 0.975)
    
    # compute ESS for contrast
    ess <- ess_bulk(difference_samples)
    
    # Compute P+
    p_plus   <- mean(difference_samples > 0)
    
    # Compute density at credible interval bounds
    density_lower <- approx(density_estimate$x, density_estimate$y, xout = ci_low)$y
    density_upper <- approx(density_estimate$x, density_estimate$y, xout = ci_up)$y
    
    # Add to results
    results <- rbind(results, data.frame(
      Region1 = region1,
      Region2 = region2,
      Mean = exp(mean_diff),
      Intercept = exp(intercept),
      ci_lower = exp(ci_low),
      ci_upper = exp(ci_up),
      P_plus = p_plus,
      ESS = ess,
      stringsAsFactors = FALSE
    ))
  }
  return(results)
}



## Random Effects

get_subject_elec_te <- function(model, fixed_effect, random_effect){
  # model: brmsfit
  # fixed_effect: e.g., "time"
  # random_effect: e.g., "key" or "electrode" (assumes there is also always "subject")
  
  ## --- Pull subject random effects ---
  subject_random_effects <- ranef(model, groups = "subject")
  
  # Extract posterior samples of random effects for subjects
  posterior_ranef_subject <- as_draws_df(model) %>%
    select(starts_with("r_subject["))
  
  # Extract posterior samples of random effects for electrodes
  posterior_ranef_electrode <- as_draws_df(model) %>%
    select(starts_with(paste0("r_subject:", random_effect, "[")))
  
  # Extract posterior samples of the fixed effect
  posterior_fixed <- as_draws_df(model, variable = paste0("b_", fixed_effect))
  
  ## --- SUBJECT-LEVEL totals: fixed + subject deviation ---
  subjects <- unique(model$data$subject)
  subject_effects <- list()
  
  for (subject in subjects) {
    ranef_var <- paste0("r_subject[", subject, ",", fixed_effect, "]")
    if (ranef_var %in% names(posterior_ranef_subject)) {
      ranef_samples <- posterior_ranef_subject[[ranef_var]]
    } else {
      stop(paste("Random effect variable", ranef_var, "not found in posterior samples."))
    }
    subject_specific_samples <- posterior_fixed[[paste0("b_", fixed_effect)]] + ranef_samples
    subject_effects[[as.character(subject)]] <- subject_specific_samples
  }
  
  subject_ci <- data.frame(
    subject = character(),
    mean_sub = numeric(),
    lower_sub = numeric(),
    upper_sub = numeric(),
    Pplus_sub = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (subject in names(subject_effects)) {
    samples <- subject_effects[[subject]]
    mean_effect <- mean(samples)
    ci_lower <- quantile(samples, probs = 0.025)
    ci_upper <- quantile(samples, probs = 0.975)
    Pplus <- mean(samples > 0)      
    
    subject_ci <- rbind(
      subject_ci,
      data.frame(
        subject = subject,
        mean_sub = mean_effect,
        lower_sub = ci_lower,
        upper_sub = ci_upper,
        Pplus_sub = Pplus,                
        stringsAsFactors = FALSE
      )
    )
  }
  
  ## --- ELECTRODE-LEVEL totals: fixed + subject deviation + subject:electrode deviation ---
  # Use the already-extracted posterior_ranef_electrode and posterior_ranef_subject
  # Build the list of (subject, electrode) pairs present in the model data
  pairs <- unique(model$data[, c("subject", random_effect)])
  names(pairs) <- c("subject", "electrode")
  
  electrode_ci <- data.frame(
    subject   = character(),
    electrode = character(),
    mean_ele  = numeric(),
    lower_ele = numeric(),
    upper_ele = numeric(),
    Pplus_ele = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(pairs))) {
    s <- pairs$subject[i]
    e <- pairs$electrode[i]
    
    # names in draws: r_subject[<s>,<fixed>] and r_subject:<re>[<s>:<e>,<fixed>]
    re_sub_name <- paste0("r_subject[", s, ",", fixed_effect, "]")
    re_ele_name <- paste0("r_subject:", random_effect, "[", s, ":", e, ",", fixed_effect, "]")
    
    if (!re_sub_name %in% names(posterior_ranef_subject)) {
      stop("Random effect column not found: ", re_sub_name)
    }
    # If the subject:electrode slope isn’t present, treat as 0 deviation
    re_sub_draws <- posterior_ranef_subject[[re_sub_name]]
    re_ele_draws <- if (re_ele_name %in% names(posterior_ranef_electrode)) {
      posterior_ranef_electrode[[re_ele_name]]
    } else {
      rep(0, length(re_sub_draws))
    }
    
    total_draws <- posterior_fixed[[paste0("b_", fixed_effect)]] + re_sub_draws + re_ele_draws
    
    mean_effect <- mean(total_draws)
    ci_lower <- quantile(total_draws, probs = 0.025)
    ci_upper <- quantile(total_draws, probs = 0.975)
    Pplus <- mean(total_draws > 0)                 
    
    electrode_ci <- rbind(
      electrode_ci,
      data.frame(
        subject   = s,
        electrode = e,
        mean_ele  = mean_effect,
        lower_ele = ci_lower,
        upper_ele = ci_upper,
        Pplus_ele = Pplus,              
        stringsAsFactors = FALSE
      )
    )
  }
  
  list(subject = subject_ci, electrode = electrode_ci)
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
