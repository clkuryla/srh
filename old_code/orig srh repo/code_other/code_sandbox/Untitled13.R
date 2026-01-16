library(tidyverse)
library(survival)
library(survey)
library(broom)
library(ggplot2)

# Function to create sliding windows of data
create_sliding_windows <- function(data, 
                                   window_size = 15,
                                   step_size = 1,
                                   survey_year_col = "year",
                                   death_year_col = "MORTDODY",
                                   age_col = "age",
                                   vital_status_col = "mortstat_recoded") {
  
  # Get range of survey years
  min_year <- min(data[[survey_year_col]])
  max_year <- max(data[[survey_year_col]])
  
  # Create sequence of start years for windows
  start_years <- seq(min_year, max_year - window_size, by = step_size)
  
  # Initialize list to store window datasets
  window_data <- list()
  
  for(start_year in start_years) {
    end_year <- start_year + window_size
    
    # Filter data for current window
    window_df <- data %>%
      filter(!!sym(survey_year_col) >= start_year,
             !!sym(survey_year_col) < end_year) %>%
      mutate(
        # Calculate years until death or censoring
        years_to_event = case_when(
          !!sym(vital_status_col) == 1 ~ 
            pmin(!!sym(death_year_col) - !!sym(survey_year_col), window_size),
          TRUE ~ window_size
        ),
        # Calculate age at death or censoring
        age_at_event = !!sym(age_col) + years_to_event,
        # New vital status (1 if died within window, 0 otherwise)
        window_vital_status = case_when(
          !!sym(vital_status_col) == 1 & 
            (!!sym(death_year_col) - !!sym(survey_year_col)) <= window_size ~ 1,
          TRUE ~ 0
        ),
        # Add window identifiers
        window_start = start_year,
        window_end = end_year
      )
    
    window_data[[as.character(start_year)]] <- window_df
  }
  
  return(window_data)
}

# Function to run Cox models without survey weights
run_cox_models <- function(window_data, 
                           formula = Surv(years_to_event, window_vital_status) ~ srh) {
  
  # Initialize results dataframe
  results <- tibble()
  
  for(window in names(window_data)) {
    # Fit Cox model
    cox_fit <- coxph(formula, data = window_data[[window]])
    
    # Extract coefficients and confidence intervals
    coef_data <- tidy(cox_fit, conf.int = TRUE) %>%
      mutate(
        window_start = as.numeric(window),
        window_end = window_start + 15
      )
    
    results <- bind_rows(results, coef_data)
  }
  
  return(results)
}

# Function to run Cox models with survey weights
run_weighted_cox_models <- function(window_data,
                                    formula = Surv(years_to_event, window_vital_status) ~ srh,
                                    weight_var = "MORTWT",
                                    design_vars = NULL) {
  
  # Initialize results dataframe
  results <- tibble()
  
  for(window in names(window_data)) {
    # Create survey design
    survey_design <- svydesign(
      ids = ~1,
      weights = as.formula(paste0("~", weight_var)),
      data = window_data[[window]]
    )
    
    # Fit weighted Cox model
    weighted_cox <- svycoxph(formula, design = survey_design)
    
    # Extract coefficients and confidence intervals
    coef_data <- tidy(weighted_cox, conf.int = TRUE) %>%
      mutate(
        window_start = as.numeric(window),
        window_end = window_start + 15
      )
    
    results <- bind_rows(results, coef_data)
  }
  
  return(results)
}

# Function to plot hazard ratios over time
plot_hazard_ratios <- function(model_results, 
                               term_of_interest = "srh",
                               title = "Hazard Ratios Over Time") {
  
  model_results %>%
    filter(term == term_of_interest) %>%
    ggplot(aes(x = window_start, y = estimate)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_point() +
    theme_minimal() +
    labs(
      title = title,
      x = "Window Start Year",
      y = "Hazard Ratio (95% CI)",
      caption = "Shaded area represents 95% confidence interval"
    )
}

# Example usage:
# Assuming your data frame is called 'mortality_data'
windows <- create_sliding_windows(data_nhis_mort_filter)
# 
# Unweighted analysis
unweighted_results <- run_cox_models(windows)
plot_hazard_ratios(unweighted_results,
                   title = "Unweighted Hazard Ratios for Self-Rated Health")
# 
# # Weighted analysis
weighted_results <- run_weighted_cox_models(windows)
plot_hazard_ratios(weighted_results,
                   title = "Weighted Hazard Ratios for Self-Rated Health")