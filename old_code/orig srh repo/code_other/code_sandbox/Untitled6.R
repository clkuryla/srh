library(tidyverse)
library(survival)
library(survey)
library(broom)

# Define time periods
time_periods <- list(
  period1 = 1981:1995,
  period2 = 1988:2002,
  period3 = 1995:2009,
  period4 = 2002:2016,
  period5 = 2009:2018  # Updated to match actual data availability
)

# Function to run weighted Cox model for a specific time period
run_weighted_cox_model <- function(data, years) {
  # Print diagnostic information
  cat(sprintf("\nProcessing years %d-%d\n", min(years), max(years)))
  
  # Subset data for the specific period
  model_data <- data %>%
    filter(YEAR %in% years)
  
  # Create survey design object
  survey_design <- svydesign(
    ids = ~1,
    # strata = ~STRATA,
    weights = ~MORTWT,
    data = model_data %>% filter(!(is.na(MORTWT)))#,
    # nest = TRUE
  )
  
  # Fit the weighted Cox model
  model <- svycoxph(
    Surv(time = YEAR, time2 = MORTDODY, event = MORTSTAT) ~ srh,
    design = survey_design
  )
  
  # Extract coefficients and statistics
  results <- tidy(model) %>%
    mutate(
      period_start = min(years),
      period_end = max(years),
      exp_coef = exp(estimate),
      exp_coef_se = exp(estimate) * std.error
    )
  
  return(results)
}

# Run models for all time periods
results_df <- map_df(names(time_periods), function(period_name) {
  years <- time_periods[[period_name]]
  run_weighted_cox_model(data_nhis_mort_filter, years)
}) %>%
  mutate(period = paste(period_start, "-", period_end))

# Create coefficient plot
coef_plot <- ggplot(results_df, aes(x = period, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, 
                    ymax = estimate + std.error),
                width = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Weighted Cox PH Coefficients Across Time Periods",
       x = "Time Period",
       y = "Coefficient (with SE)")

# Create hazard ratio plot
exp_coef_plot <- ggplot(results_df, aes(x = period, y = exp_coef)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = exp_coef - exp_coef_se, 
                    ymax = exp_coef + exp_coef_se),
                width = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Weighted Hazard Ratios Across Time Periods",
       x = "Time Period",
       y = "Hazard Ratio (with SE)")

# Create summary table
results_table <- results_df %>%
  select(period,
         coefficient = estimate, 
         se_coef = std.error,
         hazard_ratio = exp_coef,
         hr_se = exp_coef_se) %>%
  arrange(period)

# Display results
print("Numerical Results:")
print(results_table)

# Display plots
print(coef_plot)
print(exp_coef_plot)

# Save results if needed
# write_csv(results_table, "weighted_cox_results.csv")
# ggsave("weighted_cox_coefficients.png", coef_plot)
# ggsave("weighted_cox_hazard_ratios.png", exp_coef_plot)