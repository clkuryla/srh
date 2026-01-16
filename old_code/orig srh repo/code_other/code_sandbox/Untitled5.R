library(tidyverse)
library(survival)
library(survey)
library(broom)

# Define time periods and age groups
time_periods <- list(
  period1 = 1981:1995,
  period2 = 1988:2002,
  period3 = 1995:2009,
  period4 = 2002:2016,
  period5 = 2009:2018  # Updated to match actual data availability
)

age_groups <- list(
  "18-29" = c(18, 29),
  "30-39" = c(30, 39),
  "40-49" = c(40, 49),
  "50-59" = c(50, 59),
  "60-69" = c(60, 69),
  "70+" = c(70, Inf)
)

# Modified function with better error handling
run_weighted_cox_model <- function(data, years, age_range, age_group_name) {
  # Subset data
  model_data <- data %>%
    filter(YEAR %in% years,
           AGE >= age_range[1],
           AGE <= age_range[2])
  
  # Only proceed if we have sufficient data
  if(nrow(model_data) < 10 || sum(model_data$MORTSTAT) < 5) {
    warning(sprintf("Insufficient data for age group %s in years %d-%d", 
                    age_group_name, min(years), max(years)))
    return(tibble(
      term = "srh",
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      period_start = min(years),
      period_end = max(years),
      exp_coef = NA_real_,
      exp_coef_se = NA_real_,
      age_group = age_group_name
    ))
  }
  
  # Create survey design object
  survey_design <- svydesign(
    ids = ~1,
   # strata = ~STRATA,
    weights = ~MORTWT,
    data = model_data %>% filter(!(is.na(MORTWT)))#,
   # nest = TRUE
  )
  
  # Fit the weighted Cox model
  tryCatch({
    model <- svycoxph(
      Surv(time = YEAR, time2 = MORTDODY, event = MORTSTAT) ~ srh,
      design = survey_design
    )
    
    # Extract results
    results <- tidy(model) %>%
      mutate(
        period_start = min(years),
        period_end = max(years),
        exp_coef = exp(estimate),
        exp_coef_se = exp(estimate) * std.error,
        age_group = age_group_name
      )
    
    return(results)
  }, error = function(e) {
    warning(sprintf("Error in model for age group %s in years %d-%d: %s", 
                    age_group_name, min(years), max(years), e$message))
    return(tibble(
      term = "srh",
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      period_start = min(years),
      period_end = max(years),
      exp_coef = NA_real_,
      exp_coef_se = NA_real_,
      age_group = age_group_name
    ))
  })
}

# Run models for all time periods and age groups
results_df <- map_df(names(time_periods), function(period_name) {
  years <- time_periods[[period_name]]
  map_df(names(age_groups), function(age_group) {
    run_weighted_cox_model(data_nhis_mort_filter, years, age_groups[[age_group]], age_group)
  })
}) %>%
  mutate(period = paste(period_start, "-", period_end))

# Create plots with different colors for age groups
coef_plot <- ggplot(results_df %>% filter(!is.na(estimate)), 
                    aes(x = period, y = estimate, color = age_group, group = age_group)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_line() +
  geom_errorbar(aes(ymin = estimate - std.error, 
                    ymax = estimate + std.error),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        legend.position = "right") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Weighted Cox PH Coefficients Across Time Periods by Age Group",
       subtitle = "Missing values indicate insufficient data or model convergence issues",
       x = "Time Period",
       y = "Coefficient (with SE)",
       color = "Age Group")

# Create plot for hazard ratios
exp_coef_plot <- ggplot(results_df %>% filter(!is.na(exp_coef)), 
                        aes(x = period, y = exp_coef, color = age_group, group = age_group)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_line() +
  geom_errorbar(aes(ymin = exp_coef - exp_coef_se, 
                    ymax = exp_coef + exp_coef_se),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        legend.position = "right") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Weighted Hazard Ratios Across Time Periods by Age Group",
       subtitle = "Missing values indicate insufficient data or model convergence issues",
       x = "Time Period",
       y = "Hazard Ratio (with SE)",
       color = "Age Group")

# Create summary table
results_table <- results_df %>%
  select(period,
         age_group,
         coefficient = estimate, 
         se_coef = std.error,
         hazard_ratio = exp_coef,
         hr_se = exp_coef_se) %>%
  arrange(period, age_group)

# Display results
print("Numerical Results:")
print(results_table)

# Display plots
print(coef_plot)
print(exp_coef_plot)

