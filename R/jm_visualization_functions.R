predict_on_test_set <- function(jm_fit, test_long_data, test_cox_df){
# This function generates predictions on a test set for a fitted joint model.
#
# The function takes three arguments:
# - jm_fit: A fitted joint model object. This model should be previously fitted using longitudinal and survival data.
# - test_long_data: A dataframe containing the test longitudinal data. This dataframe should include the same
#   variables used for fitting the joint model.
# - test_cox_df: A dataframe containing the test survival data.
#
# The function performs the following operations:
# 1. Filters the test_long_data to include only observations where 'trial_time' is less than 0.5. The model uses these 500ms 
#    to predict the turning time in a given heald out trial
# 2. Selects relevant columns from test_long_data, excluding 'turnaround_time' and 'EVENT', so there is no way it could use 
#    that info to make its predictions
# 3. Uses the 'predict' function on the 'jm_fit' model, passing the preprocessed test longitudinal data. It
#    specifies to process events, return the modified test data with predictions, identifies the unique ID variable,
#    and sets the time points for prediction as a sequence from 0.5 to 2.5 in steps of 0.05.
# 4. Renames the 'turnaround_time' column to 'turntime_real' in the test_cox_df for clarity and selects only
#    the necessary columns ('trial_numeric' and 'turntime_real') for joining with the predictions.
# 5. Performs a left join of the test predictions with the survival data (test_cox_df) based on 'trial_numeric' to
#    combine predictions with actual survival times.
# 6. Arranges the resulting dataframe by 'turntime_real' to ensure predictions are sorted by the actual survival times.
# 7. Converts the 'trial_numeric' variable to a factor, with levels specified as the unique values of 'trial_numeric',
#    for better handling in subsequent analyses or plots.
#
# Finally, the function returns the dataframe 'test_predictions', which contains the survival predictions alongside
# the actual survival times, ready for evaluation or further analysis.
  
  test_pred_df <- test_long_data %>% 
    filter(turnaround_time > 0.5) %>%
    filter(jm_time < .5) %>%
    select(-turnaround_time, -EVENT)
  
  test_predictions <- predict(jm_fit, 
                        newdata = test_pred_df, process = "event", return_newdata = TRUE, 
                        idVar = "trial_numeric", times = seq(.5, 2.5, .05))
  
  test_cox_df <- test_cox_df %>% rename(turntime_real = turnaround_time) %>% select(trial_numeric, turntime_real)
  test_predictions <- left_join(test_predictions, test_cox_df, by = "trial_numeric")
  
  test_predictions <- test_predictions %>%
    arrange(turntime_real) %>%
    mutate(trial_numeric = factor(trial_numeric, levels = unique(trial_numeric)))
  
  return(test_predictions)
  
}

plot_survival_predictions <- function(test_predictions, current_subject){
# This function creates and plots survival predictions for a given subject from a test predictions dataframe.
#
# Arguments:
# - test_predictions: A dataframe containing survival predictions. This dataframe should include columns for
#   'trial_time' (time points of the prediction), 'pred_CIF' (predicted cumulative incidence function values),
#   'trial_numeric' (identifier for the trial), and 'turntime_real' (actual time to event).
# - current_subject: A character string or numeric value representing the subject for whom the plot is generated.
#   This is used to customize the plot title to indicate the subject's predictions being displayed.
#
# The function performs the following steps to create the plot:
# 1. Initializes a ggplot object using the 'test_predictions' dataframe. It sets 'trial_time' as the x-axis,
#    'pred_CIF' as the y-axis, and uses 'trial_numeric' to color-code the points, providing a visual distinction
#    between trials.
# 2. Adds a shaded rectangle from 0 to 0.5 on the x-axis and 0 to 1 on the y-axis using 'geom_rect'. This rectangle
#    represents the 500ms window during which the model made its predictions, highlighting the prediction interval.
# 3. Uses 'geom_vline' to draw vertical lines at the 'turntime_real' for each trial, indicating the true turnaround
#    time. The lines are color-coded by 'trial_numeric' and set to a transparency of 0.7.
# 4. Plots the predicted probabilities as points on the graph with 'geom_point', allowing for a direct visual
#    comparison between the predicted and actual turnaround times.
# 5. Applies the Viridis color scale to the plot, enhancing visual clarity and accessibility.
# 6. Customizes the plot's appearance (background, text color, text size, legend positioning, title) for better
#    readability and professional presentation.
# 7. Sets the y-axis limits to 0-1 and x-axis limits to 0-2.5, framing the plot within the relevant range of
#    probabilities and time.
# 8. Adds a title to the plot using 'ggtitle', including the subject identifier to specify whose predictions are
#    being visualized.
# 9. Labels the x-axis as "Time" and the y-axis as "Probability of Turning Around", clearly indicating what the
#    plot represents.
#
# The function concludes by printing the plot, making it visible for review or further analysis. This plot can be
# used to assess the accuracy of survival predictions against actual outcomes, particularly in the context of
# longitudinal and survival data analysis.
  
  
  prediction_plot <- ggplot(test_predictions, aes(x = jm_time, y = pred_CIF, color = trial_numeric)) +
    # representing the 500ms window the model was given in order to make its predictions
    geom_rect(aes(xmin=0, xmax=.5, ymin=0, ymax=1), fill = "lightgrey", alpha = .5, color = 'lightgrey') +
    # true turnaround time
    geom_vline(aes(xintercept = turntime_real, color = trial_numeric, alpha = .7)) +
    # predicted probabilities on turnaround time
    geom_point() +
    scale_color_viridis_d() +  # Using Viridis color scale
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 12),
          legend.position =  'none',
          plot.title  = element_text(color = "black", size = 16, face = "plain")) +
    ylim(0, 1) + xlim(0, 2.5) + labs(color = "Test Trial") +
    ggtitle(paste0("Prediction on held out trials: ", current_subject)) +
    labs(x = "Time", y = "Probability of Turning Around")
  
  print(prediction_plot)
  
  
}

plot_correlation_plot <- function(test_predictions, current_subject){
# This function creates and displays a scatter plot comparing actual and predicted turnaround times
# for a specific subject given their test predictions dataset.
#
# Arguments:
# - test_predictions: A dataframe containing the test predictions. It must include at least 'trial_numeric'
#   as a trial identifier, 'turntime_real' as the actual turnaround time, 'pred_CIF' as the predicted cumulative
#   incidence function values, and 'trial_time' as the predicted time points.
# - current_subject: A character string or numeric value representing the subject for whom the plot is generated,
#   only used in the plot title.
#
# The function performs the following operations:
# 1. Prepares the data by calculating the absolute difference between 'pred_CIF' and 0.5 to find predictions
#    closest to a 50% cumulative incidence function, indicating a critical threshold for decision-making in
#    survival analysis. This step identifies the predicted turnaround time closest to this critical value for
#    each trial.
# 2. Filters the dataset to retain only the rows with the predicted turnaround time closest to the critical
#    threshold of 50% for each trial.
# 3. Selects distinct rows based on 'trial_numeric', 'turntime_real', and the calculated 'turntime_pred' to ensure
#    a clean dataset for plotting and correlation analysis.
# 4. Calculates the Pearson correlation coefficient ('cor_score') between the actual and predicted turnaround times
#    to quantify the linear relationship between these variables.
# 5. Generates a scatter plot using 'ggplot2' with actual turnaround times on the x-axis and predicted turnaround
#    times on the y-axis. Points are sized for visibility, and the plot background is set to white for clarity.
# 6. The plot is further customized with labels for both axes, set limits for x and y axes to focus on relevant
#    ranges, and a title that includes the current subject and the calculated correlation score.
#
# The function concludes by printing the scatter plot, allowing for visual inspection of the relationship between
# actual and predicted turnaround times. This visualization aids in evaluating the predictive performance of the
# model, specifically how well it can predict the turnaround time for the given subject.
  
  
  scatter_pred_df <- test_predictions %>%
    mutate(near_50 = abs(pred_CIF - .5)) %>%
    group_by(trial_numeric) %>%
    mutate(closest_to_50 = min(near_50)) %>%
    filter(closest_to_50 == near_50) %>%
    mutate(turntime_pred = jm_time) %>%
    ungroup() %>%
    select(trial_numeric, turntime_real, turntime_pred) %>%
    distinct()
  
  cor_score <- cor(scatter_pred_df$turntime_real, scatter_pred_df$turntime_pred)

  scatter_pred_plot <- scatter_pred_df %>%
    ggplot(., aes(x = turntime_real, y = turntime_pred)) +
    geom_point(size = 4) +
    geom_smooth(method = "lm", formula = "y ~x", color = "black", se = F) +
    theme(panel.background = element_rect(fill = "white")) +
    labs(x = "True Turnaround Time", y = "Predicted Turnaround Time") +
    ylim(.5, 5) + xlim(.5, 5) +
    ggtitle(paste0("Prediction Scatterplot: ", current_subject, "; Correlation: ", round(cor_score, 2)))
  
  print(scatter_pred_plot)
  
  return(cor_score)
  
}
  
  