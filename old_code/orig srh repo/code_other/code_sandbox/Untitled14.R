# Load necessary libraries
library(dplyr)      # For data manipulation
library(tidyr)      # For data tidying
library(ggplot2)    # For plotting
library(survival)   # For Cox proportional hazards models
library(survey)     # For survey weighted analysis

# -----------------------------------------------------------------------------
# Function to prepare the data for a given sliding window.
# -----------------------------------------------------------------------------
prepare_window <- function(data, start_year, window_length = 15) {
  # data: Original dataset.
  # start_year: The first year of the sliding window.
  # window_length: Duration of the window (default is 15 years).
  # This function subsets the data to include only those surveys conducted
  # between start_year and start_year+window_length and computes new outcome variables.
  
  # Filter participants whose survey_year falls within the window.
  df_window <- data %>%
    filter(survey_year >= start_year, survey_year < start_year + window_length) %>%
    # Create new variables for the 15-year follow-up.
    mutate(
      # Calculate difference between death year and survey year.
      # For those who did not die (or missing death_year), assign a value greater than window_length.
      diff_years = if_else(!is.na(death_year), death_year - survey_year, window_length + 1),
      
      # New event indicator: if death occurred within 15 years, event15 = 1, else 0.
      event15 = if_else(diff_years <= window_length, 1, 0),
      
      # New exit time: if event occurred within window,
      # calculate age at death as (death_year - survey_year + age_at_survey),
      # otherwise, set to age_at_survey + window_length (administrative censoring).
      age_at_death_new = if_else(event15 == 1,
                                 (death_year - survey_year) + age_at_survey,
                                 age_at_survey + window_length)
    )
  
  return(df_window)
}

# -----------------------------------------------------------------------------
# Function to run a Cox proportional hazards model on a given window,
# using the two-time Surv() format.
# -----------------------------------------------------------------------------
run_cox <- function(df, weighted = FALSE, weight_var = "weight") {
  # df: Data frame that has been prepared by prepare_window() with new outcomes.
  # weighted: Logical flag; if TRUE, perform survey weighted analysis.
  # weight_var: Column name containing the survey weights.
  
  if (weighted) {
    # Create a survey design object.
    design <- svydesign(ids = ~1, weights = as.formula(paste0("~", weight_var)), data = df)
    
    # Fit the weighted Cox model using svycoxph with the two-time format.
    fit <- svycoxph(Surv(time = age_at_survey,
                         time2 = age_at_death_new,
                         event = event15) ~ self_rated_health, 
                    design = design)
  } else {
    # Fit the standard Cox model using the two-time format.
    fit <- coxph(Surv(time = age_at_survey,
                      time2 = age_at_death_new,
                      event = event15) ~ self_rated_health, 
                 data = df)
  }
  
  return(fit)
}

# -----------------------------------------------------------------------------
# Main function to run sliding window analyses over a range of survey years.
# -----------------------------------------------------------------------------
run_sliding_windows <- function(data, start_years, window_length = 15, 
                                weighted = FALSE, weight_var = "weight") {
  # Initialize an empty tibble with the necessary columns
  results <- tibble(
    start_year = numeric(),
    coef       = numeric(),
    hr         = numeric(),
    conf_low   = numeric(),
    conf_high  = numeric(),
    p_value    = numeric(),
    n          = numeric()
  )
  
  # Loop over each start_year.
  for (sy in start_years) {
    # Prepare the dataset for this window.
    df_win <- prepare_window(data, start_year = sy, window_length = window_length)
    
    # Sanity check: Skip window if no observations.
    if(nrow(df_win) == 0) next
    
    # Run the Cox model.
    fit <- run_cox(df_win, weighted = weighted, weight_var = weight_var)
    
    # Extract summary statistics from the model.
    # Extract summary statistics from the model.
    fit_summary <- summary(fit)
    # Directly extract the hazard ratio and its 95% CI.
    hr       <- fit_summary$conf.int["self_rated_health", "exp(coef)"]
    conf_low <- fit_summary$conf.int["self_rated_health", "lower .95"]
    conf_high<- fit_summary$conf.int["self_rated_health", "upper .95"]
    coef_val <- fit_summary$coefficients["self_rated_health", "coef"]
    p_value  <- fit_summary$coefficients["self_rated_health", "Pr(>|z|)"]
    
    
    # Append the results using add_row.
    results <- results %>% 
      add_row(
        start_year = sy,
        coef       = coef_val,
        hr         = hr,
        conf_low   = conf_low,
        conf_high  = conf_high,
        p_value    = p_value,
        n          = nrow(df_win)
      )
  }
  
  return(results)
}


# -----------------------------------------------------------------------------
# Function to plot the evolution of hazard ratios over sliding windows.
# -----------------------------------------------------------------------------
plot_hr <- function(results, title = "Hazard Ratios over Sliding Windows") {
  # results: Data frame produced by run_sliding_windows().
  ggplot(results, aes(x = start_year, y = hr)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymax = conf_high, ymin = conf_low), width = 0.5) +
    labs(x = "Survey Start Year",
         y = "Hazard Ratio (self-rated health)",
         title = title) +
    theme_minimal()
}

# -----------------------------------------------------------------------------
# Example Usage
# -----------------------------------------------------------------------------
# Assume `data` is your data frame with the necessary columns.
# Variables required include:
# - survey_year: Year of the survey.
# - age_at_survey: Age at survey (entry time).
# - death_year: Year of death (if applicable).
# - self_rated_health: Predictor of interest.
# - weight: Survey weight (if available).
#
# Define the range of starting survey years for the sliding windows.
window_length <- 15
min_year <- min(data$survey_year, na.rm = TRUE)
max_year <- max(data$survey_year, na.rm = TRUE)
start_years <- seq(min_year, max_year - window_length, by = 1)

# Run analyses WITHOUT survey weights.
results_unweighted <- run_sliding_windows(data = data_nhis_mort_filter_more, start_years, window_length, weighted = FALSE)
# Plot unweighted hazard ratios.
plot_hr(results_unweighted, title = "Unweighted Hazard Ratios over Sliding Windows")

# Run analyses WITH survey weights.
results_weighted <- run_sliding_windows(data, start_years, window_length, weighted = TRUE, weight_var = "weight")
# Plot weighted hazard ratios.
plot_hr(results_weighted, title = "Weighted Hazard Ratios over Sliding Windows")

# -----------------------------------------------------------------------------
# Sensitivity, Sanity, and Fit Checks
# -----------------------------------------------------------------------------
# Sanity Check: Check the number of observations and events in each window.
print(results_unweighted)

# Optional: Check the proportional hazards assumption for one window model.
# For example, check for the window starting in 1990.
df_1990 <- prepare_window(data, start_year = 1990, window_length = window_length)
cox_fit_1990 <- coxph(Surv(time = age_at_survey,
                           time2 = age_at_death_new,
                           event = event15) ~ self_rated_health, data = df_1990)
ph_test <- cox.zph(cox_fit_1990)
print(ph_test)

# -----------------------------------------------------------------------------
# Caveats, Assumptions, and Limitations
# -----------------------------------------------------------------------------
# - Administrative Censoring: We censor follow-up at exactly 15 years post-survey.
#   This means any events after 15 years are not considered, which might impact long-term predictions.
#
# - Window Overlap: Sliding windows will contain overlapping data. While this is by design to study
#   evolving predictive power over time, the non-independence between windows should be considered
#   when interpreting trends.
#
# - Survey Weights: When using survey weights, ensure that the weight variable is correctly specified.
#   The code uses a simple survey design (ids = ~1); if your design is more complex (e.g., clustering,
#   stratification), adjust the svydesign() call accordingly.
#
# - Proportional Hazards Assumption: It is advisable to check the PH assumption (using, for example, cox.zph)
#   in each window or a representative subset.
#
# - Data Quality and Missingness: This script assumes complete information on survey_year, age_at_survey, 
#   and death_year. Handle missing values appropriately before running the analysis.
