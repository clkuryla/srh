library(dplyr)

data_nhanes <- data_nhanes %>%
  mutate(age_group = cut(
    age_visit,
    breaks = c(18, 30, 40, 50, 60, 70, Inf),
    labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
    right = FALSE
  ))

prepare_window_nhanes <- function(data, start_year, window_length = 10) {
  # Subset data to the selected window
  df_window <- data %>%
    filter(year >= start_year, year < start_year + window_length) %>%
    mutate(
      diff_age = age_last - age_visit,
      event_window = if_else(deceased == 1 & diff_age <= window_length, 1, 0),
      age_last_window = if_else(
        event_window == 1,
        age_last,              # Died within window
        age_visit + window_length  # Administratively censored
      )
    )
  return(df_window)
}

library(survival)
library(survey)

run_cox_nhanes <- function(df, weighted = FALSE, weight_var = "weight") {
  if (weighted) {
    # Example: minimal design (no PSU/strata). Adapt for actual NHANES design:
    design <- svydesign(ids = ~1, weights = as.formula(paste0("~", weight_var)), data = df)
    
    fit <- svycoxph(
      Surv(time = age_visit, time2 = age_last_window, event = event_window) ~ srh,
      design = design
    )
  } else {
    fit <- coxph(
      Surv(time = age_visit, time2 = age_last_window, event = event_window) ~ srh,
      data = df
    )
  }
  return(fit)
}

run_sliding_windows_nhanes <- function(data, 
                                       start_years, 
                                       window_length = 10,
                                       weighted = FALSE, 
                                       weight_var = "weight") {
  
  # Initialize an empty tibble with known columns
  results <- tibble::tibble(
    start_year = numeric(),
    coef       = numeric(),
    hr         = numeric(),
    conf_low   = numeric(),
    conf_high  = numeric(),
    p_value    = numeric(),
    n          = numeric()
  )
  
  for (sy in start_years) {
    # Subset and define event/censoring
    df_win <- prepare_window_nhanes(data, start_year = sy, window_length = window_length)
    if (nrow(df_win) == 0) next
    
    # Fit Cox model
    fit <- run_cox_nhanes(df_win, weighted = weighted, weight_var = weight_var)
    fit_summary <- summary(fit)
    
    # If srh isn't in the model, skip
    if (!"srh" %in% rownames(fit_summary$coefficients)) next
    
    # Extract estimates
    coef_val <- fit_summary$coefficients["srh", "coef"]
    hr_val   <- fit_summary$conf.int["srh", "exp(coef)"]
    c_low    <- fit_summary$conf.int["srh", "lower .95"]
    c_high   <- fit_summary$conf.int["srh", "upper .95"]
    p_val    <- fit_summary$coefficients["srh", "Pr(>|z|)"]
    
    # Append row
    results <- results %>%
      add_row(
        start_year = sy,
        coef       = coef_val,
        hr         = hr_val,
        conf_low   = c_low,
        conf_high  = c_high,
        p_value    = p_val,
        n          = nrow(df_win)
      )
  }
  
  return(results)
}

run_sliding_windows_by_age_nhanes <- function(data,
                                              start_years,
                                              window_length = 10,
                                              weighted = FALSE,
                                              weight_var = "weight") {
  
  results <- tibble::tibble(
    start_year = numeric(),
    age_group  = character(),
    coef       = numeric(),
    hr         = numeric(),
    conf_low   = numeric(),
    conf_high  = numeric(),
    p_value    = numeric(),
    n          = numeric()
  )
  
  for (sy in start_years) {
    # Prepare data for this window
    df_win <- prepare_window_nhanes(data, sy, window_length)
    if (nrow(df_win) == 0) next
    
    # Split by age_group (assuming it exists in df_win)
    df_groups <- df_win %>%
      group_split(age_group, .drop = TRUE)
    
    for (group_df in df_groups) {
      current_age_group <- unique(group_df$age_group)
      if (length(current_age_group) != 1) next  # sanity check
      if (nrow(group_df) < 5) next             # skip if too few data
      
      fit <- run_cox_nhanes(group_df, weighted, weight_var)
      fit_summary <- summary(fit)
      
      if (!"srh" %in% rownames(fit_summary$coefficients)) next
      
      coef_val <- fit_summary$coefficients["srh", "coef"]
      hr_val   <- fit_summary$conf.int["srh", "exp(coef)"]
      c_low    <- fit_summary$conf.int["srh", "lower .95"]
      c_high   <- fit_summary$conf.int["srh", "upper .95"]
      p_val    <- fit_summary$coefficients["srh", "Pr(>|z|)"]
      
      results <- results %>%
        add_row(
          start_year = sy,
          age_group  = as.character(current_age_group),
          coef       = coef_val,
          hr         = hr_val,
          conf_low   = c_low,
          conf_high  = c_high,
          p_value    = p_val,
          n          = nrow(group_df)
        )
    }
  }
  
  return(results)
}

plot_nhanes_sliding <- function(results, 
                                y_var = "coef",
                                title = "Cox PH Coefficients by Start Year") {
  # y_var can be "coef" or "hr", depending on what you want on the y-axis.
  
  # We'll plot with confidence intervals in the same scale (log-coef or HR).
  # If plotting HR, we can do a log scale or direct scale. Example here is direct scale for HR.
  
  if (y_var == "coef") {
    ggplot(results, aes(x = start_year, y = coef)) +
      geom_point() +
      geom_line() +
      # error bars in coefficient scale:
      geom_errorbar(aes(ymin = coef - (coef - log(conf_low)), 
                        ymax = coef + (log(conf_high) - coef)), width = 0.3) +
      labs(x = "Start Year", y = "Coefficient (log-HR)", title = title, subtitle = "NHANES") +
      theme_minimal()
    
  } else {
    ggplot(results, aes(x = start_year, y = hr)) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.3) +
      labs(x = "Start Year", y = "Hazard Ratio", title = title) +
      theme_minimal()
  }
}

plot_nhanes_sliding_by_age <- function(results, 
                                       y_var = "coef",
                                       title = "Cox PH Coefficients by Start Year & Age Group") {
  
  if (y_var == "coef") {
    ggplot(results, aes(x = start_year, y = coef, color = age_group, group = age_group)) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(
        ymin = coef - (coef - log(conf_low)),
        ymax = coef + (log(conf_high) - coef)
      ), width = 0.3) +
      labs(x = "Start Year", y = "Coefficient (log-HR)", color = "Age Group", title = title) +
      theme_minimal()
    
  } else {
    ggplot(results, aes(x = start_year, y = hr, color = age_group, group = age_group)) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.3) +
      labs(x = "Start Year", y = "Hazard Ratio", color = "Age Group", title = title, subtitle = "NHANES") +
      theme_minimal()
  }
}




######### Apply
# 1. Define your sliding window range
#    Suppose you have data from 2001 to 2017, you might choose a 10-year window
library(dplyr)

min_year <- min(data_nhanes$year, na.rm = TRUE) # e.g. 2001
max_year <- max(data_nhanes$year, na.rm = TRUE) # e.g. 2017
window_length <- 3

# We'll define start years from 2001 to 2007, for instance (so that each window
# has up to 10 years after baseline).
start_years <- seq(min_year, max_year - window_length, by = 2) 
# by=2 if you only want to jump every 2 years (since NHANES is 2-year cycles).
# Or by=1 if you want every single year.

# 2. Run unweighted sliding window analysis (all ages together)
results_nhanes_unweighted <- run_sliding_windows_nhanes(
  data = data_nhanes,
  start_years = start_years,
  window_length = window_length,
  weighted = FALSE
)

# 3. Plot the coefficient or hazard ratio
plot_nhanes_sliding(results_nhanes_unweighted, y_var = "coef",
                    title = "Unweighted Cox PH HR by Start Year (NHANES)")

# 4. Run weighted sliding window analysis (all ages)
#    Suppose your NHANES weight variable is named "wt_mec".
results_nhanes_weighted <- run_sliding_windows_nhanes(
  data = data_nhanes,
  start_years = start_years,
  window_length = window_length,
  weighted = TRUE,
  weight_var = "wt_mec"   # or your chosen weight var
)

plot_nhanes_sliding(results_nhanes_weighted, y_var = "hr",
                    title = "Weighted Cox PH HR by Start Year (NHANES)")

# 5. If you want separate age groups, make sure data_nhanes has 'age_group' column
#    (as shown above). Then run:
results_nhanes_unweighted_age <- run_sliding_windows_by_age_nhanes(
  data = data_nhanes,
  start_years = start_years,
  window_length = window_length,
  weighted = FALSE
)

# 6. Plot by age group (hazard ratio)
plot_nhanes_sliding_by_age(results_nhanes_unweighted_age, y_var = "coef",
                           title = "Unweighted Cox PH HR by Start Year & Age Group (NHANES)")

# 7. Weighted by age group
results_nhanes_weighted_age <- run_sliding_windows_by_age_nhanes(
  data = data_nhanes,
  start_years = start_years,
  window_length = window_length,
  weighted = TRUE,
  weight_var = "wt_mec"
)

plot_nhanes_sliding_by_age(results_nhanes_weighted_age, y_var = "hr",
                           title = "Weighted Cox PH HR by Start Year & Age Group (NHANES)")


