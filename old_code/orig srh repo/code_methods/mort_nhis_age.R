# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(survey)

#------------------------------------------------------------------------------
# 1) Prepare your data with a new column for age groups
#    (If you don't already have an age_group variable)
#------------------------------------------------------------------------------
data_nhis_mort_filter_more <- data_nhis_mort_filter_more %>%
  mutate(age_group = cut(
    age_at_survey,
    breaks = c(18, 30, 40, 50, 60, 70, Inf),
    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
    right = FALSE
  ))

#------------------------------------------------------------------------------
# 2) prepare_window function (unchanged from earlier, but repeated here)
#------------------------------------------------------------------------------
prepare_window <- function(data, start_year, window_length = 15) {
  data %>%
    filter(survey_year >= start_year, survey_year < start_year + window_length) %>%
    mutate(
      diff_years = if_else(!is.na(death_year), death_year - survey_year, window_length + 1),
      event15 = if_else(diff_years <= window_length, 1, 0),
      age_at_death_new = if_else(
        event15 == 1,
        (death_year - survey_year) + age_at_survey,
        age_at_survey + window_length
      )
    )
}

#------------------------------------------------------------------------------
# 3) run_cox function (unchanged, but uses two-time Surv format)
#------------------------------------------------------------------------------
run_cox <- function(df, weighted = FALSE, weight_var = "weight") {
  if (weighted) {
    design <- svydesign(ids = ~1, weights = as.formula(paste0("~", weight_var)), data = df %>% filter(!(is.na(MORTWT))))
    fit <- svycoxph(
      Surv(time = age_at_survey, time2 = age_at_death_new, event = event15) ~ self_rated_health,
      design = design
    )
  } else {
    fit <- coxph(
      Surv(time = age_at_survey, time2 = age_at_death_new, event = event15) ~ self_rated_health,
      data = df
    )
  }
  return(fit)
}

#------------------------------------------------------------------------------
# 4) Sliding window analysis by age group
#------------------------------------------------------------------------------
run_sliding_windows_by_age <- function(data,
                                       start_years,
                                       window_length = 15,
                                       weighted = FALSE,
                                       weight_var = "weight") {
  
  # Initialize results tibble with columns for window start year, age group, etc.
  results <- tibble(
    start_year = numeric(),
    age_group  = character(),
    coef       = numeric(),
    hr         = numeric(),
    conf_low   = numeric(),
    conf_high  = numeric(),
    p_value    = numeric(),
    n          = numeric()
  )
  
  # Loop over each sliding window start_year
  for (sy in start_years) {
    # Prepare the dataset for this window
    df_win <- prepare_window(data, start_year = sy, window_length = window_length)
    
    # If there are no observations, skip
    if (nrow(df_win) == 0) next
    
    # Now group by age_group and run separate models
    # (Only for age groups with data in this window)
    df_groups <- df_win %>%
      group_split(age_group, .drop = TRUE)
    

    
    # For each group, fit the Cox model and store results
    for (group_df in df_groups) {
      current_age_group <- unique(group_df$age_group)
      
      #####################
      # For each group, check if there are enough non-missing weight observations (if weighted)
      if (weighted && sum(!is.na(group_df[[weight_var]])) < 5) {
        next
      }
      ####################
      
      # If there's not enough data to fit a model, skip
    #  if (nrow(group_df) < 5) next
      if (nrow(group_df) < 5 || sum(group_df$event15, na.rm = TRUE) == 0) next
      
      
      fit <- run_cox(group_df, weighted = weighted, weight_var = weight_var)
      fit_summary <- summary(fit)
      
      # Safely extract the estimates for self_rated_health
      # Make sure that "self_rated_health" is actually in the model
      if (!"self_rated_health" %in% rownames(fit_summary$coefficients)) next
      
      coef_val <- fit_summary$coefficients["self_rated_health", "coef"]
      
      # Extract hazard ratio and confidence intervals directly
      hr_val       <- fit_summary$conf.int["self_rated_health", "exp(coef)"]
      conf_low_val <- fit_summary$conf.int["self_rated_health", "lower .95"]
      conf_high_val<- fit_summary$conf.int["self_rated_health", "upper .95"]
      
      p_val        <- fit_summary$coefficients["self_rated_health", "Pr(>|z|)"]
      
      # Add row to results
      results <- results %>%
        add_row(
          start_year = sy,
          age_group  = as.character(current_age_group),
          coef       = coef_val,
          hr         = hr_val,
          conf_low   = conf_low_val,
          conf_high  = conf_high_val,
          p_value    = p_val,
          n          = nrow(group_df)
        )
    }
  }
  
  return(results)
}

#------------------------------------------------------------------------------
# 5) Plot function for age-grouped hazard ratios
#------------------------------------------------------------------------------
plot_hr_by_age <- function(results, title = "Weighted Cox PH Coefficients\nAcross Time Periods by Age Group") {
  # Here we plot the coefficient or the hazard ratio.
  # The user’s example plot uses coefficients on the y-axis. Adjust as desired.
  ggplot(results, aes(x = factor(start_year), y = coef, color = age_group, group = age_group)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = coef - (coef - log(conf_low)), ymax = coef + (log(conf_high) - coef)),
                  width = 0.2) +
    labs(
      x = "Time Period",
      y = "Coefficient (with SE)",  # or "Coefficient"
      color = "Age Group",
      title = title,
      subtitle = paste0(window_length, "-Year Mortality Window")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45)) +
    theme(legend.position = "right")
}

#------------------------------------------------------------------------------
# Example Usage
#------------------------------------------------------------------------------
# 1) Define your sliding windows
window_length <- 1
min_year <- min(data_nhis_mort_filter_more$survey_year, na.rm = TRUE)
max_year <- max(data_nhis_mort_filter_more$survey_year, na.rm = TRUE)
start_years <- seq(min_year, max_year - window_length, by = 1)

# 2) Run analyses by age group WITHOUT survey weights
results_unweighted_age <- run_sliding_windows_by_age(
  data = data_nhis_mort_filter_more,
  start_years = start_years,
  window_length = window_length,
  weighted = FALSE
)

# 3) Plot the results (coefficients) by age group
plot_hr_by_age(results_unweighted_age, 
               title = "Unweighted Cox PH Coefficients\nby Age Group over Time Windows")

# 4) Run analyses by age group WITH survey weights
results_weighted_age <- run_sliding_windows_by_age(
  data = data_nhis_mort_filter_more,
  start_years = start_years,
  window_length = window_length,
  weighted = TRUE,
  weight_var = "MORTWT"
)

# 5) Plot the weighted results
plot_hr_by_age(results_weighted_age, 
               title = "Weighted Cox PH Coefficients\nby Age Group over Time Windows")

#------------------------------------------------------------------------------
# Notes and Tips
#------------------------------------------------------------------------------
# 1. The function `plot_hr_by_age()` above uses the coefficient (log-HR) on the Y-axis.
#    - If you want to plot the hazard ratio directly, change y = hr in ggplot() and
#      adjust the error bar logic (you would use conf_low and conf_high directly).
#    - Alternatively, if you want the hazard ratio on a log scale, you can do:
#         scale_y_log10()
#      after specifying y = hr.

# 2. The confidence intervals in the coefficient scale:
#    - If you store only the conf_low and conf_high for HR, you can transform them to log scale with
#      log(conf_low) and log(conf_high). Then your error bars become:
#         aes(ymin = log(conf_low), ymax = log(conf_high)).
#    - Or keep everything in the HR scale from the start and plot hazard ratios with error bars:
#         aes(ymin = conf_low, ymax = conf_high).

# 3. Because of smaller sample sizes in some age groups and windows, you might find that
#    some windows or groups have too few events to fit the model reliably. In the example code,
#    we skip groups with fewer than 5 records. Adjust this threshold as needed.

# 4. Make sure the “self_rated_health” variable is indeed in your data frame, or rename it
#    to whatever variable you’re analyzing.

# 5. Check your design assumptions if you have a more complex survey design (clustering, s
