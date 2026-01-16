library(dplyr)

data_nhis <- data_nhis_mort_filter_more %>%
  mutate(srh_factor = factor(
    srh, 
    levels = c(1, 2, 3, 4, 5), 
    labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")
  ))

library(survival)
library(survey)

prepare_window_nhis <- function(data, start_year, window_length = 15) {
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

run_cox_nhis <- function(df, weighted = FALSE, weight_var = "weight") {
  if (weighted) {
    design <- svydesign(ids = ~1, weights = as.formula(paste0("~", weight_var)), data = df)
    fit <- svycoxph(
      Surv(time = age_at_survey, time2 = age_at_death_new, event = event15) ~ srh_factor,
      design = design
    )
  } else {
    fit <- coxph(
      Surv(time = age_at_survey, time2 = age_at_death_new, event = event15) ~ srh_factor,
      data = df
    )
  }
  return(fit)
}

run_sliding_windows_nhis_srhCat <- function(data, start_years, window_length = 15, 
                                            weighted = FALSE, weight_var = "weight") {
  # Initialize a tibble with columns for window, srh category, etc.
  results <- tibble::tibble(
    start_year   = numeric(),
    srh_category = character(),
    coef         = numeric(),
    hr           = numeric(),
    conf_low     = numeric(),
    conf_high    = numeric(),
    p_value      = numeric(),
    n            = numeric()
  )
  
  for (sy in start_years) {
    df_win <- prepare_window_nhis(data, start_year = sy, window_length = window_length)
    if (nrow(df_win) == 0) next
    
    fit <- run_cox_nhis(df_win, weighted, weight_var)
    fit_summary <- summary(fit)
    
    # If the model didn't converge or doesn't have srh_factor, skip
    all_vars <- rownames(fit_summary$coefficients)
    # Typically the reference category won't appear in the summary (it’s the intercept).
    # So the rows will be "srh_factorVeryGood", "srh_factorGood", etc.
    srh_rows <- grep("srh_factor", all_vars)
    if (length(srh_rows) == 0) next
    
    # Extract total N
    n_obs <- nrow(df_win)
    
    # For each SRH level, store the estimates
    for (row_idx in srh_rows) {
      row_name <- all_vars[row_idx]
      coef_val <- fit_summary$coefficients[row_name, "coef"]
      hr_val   <- fit_summary$conf.int[row_name, "exp(coef)"]
      c_low    <- fit_summary$conf.int[row_name, "lower .95"]
      c_high   <- fit_summary$conf.int[row_name, "upper .95"]
      p_val    <- fit_summary$coefficients[row_name, "Pr(>|z|)"]
      
      # Parse out the SRH category from row_name, e.g. "srh_factorGood" => "Good"
      # One way: sub("srh_factor", "", row_name)
      srh_cat_label <- sub("srh_factor", "", row_name)
      
      results <- results %>%
        add_row(
          start_year   = sy,
          srh_category = srh_cat_label,
          coef         = coef_val,
          hr           = hr_val,
          conf_low     = c_low,
          conf_high    = c_high,
          p_value      = p_val,
          n            = n_obs
        )
    }
  }
  
  return(results)
}

library(ggplot2)

plot_srh_categories_nhis <- function(results,
                                     title = "NHIS: Hazard Ratios by SRH Category over Sliding Windows") {
  ggplot(results, aes(x = start_year, y = hr, color = srh_category, group = srh_category)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.3) +
    labs(x = "Start Year",
         y = "Hazard Ratio (SRH Category vs. Reference)",
         color = "SRH Category",
         title = title) +
    theme_minimal()
}

###### Apply

# Suppose your data frame is called data_nhis
# 1) Define your sliding windows
window_length <- 8
min_year <- min(data_nhis$survey_year, na.rm = TRUE)
max_year <- max(data_nhis$survey_year, na.rm = TRUE)
start_years <- seq(min_year, max_year - window_length, by = 1)

# 2) Run unweighted analysis
results_nhis_unweighted_srh <- run_sliding_windows_nhis_srhCat(
  data = data_nhis,
  start_years = start_years,
  window_length = window_length,
  weighted = FALSE
)

# 3) Plot
plot_srh_categories_nhis(results_nhis_unweighted_srh,
                         title = "NHIS (Unweighted): HR by SRH Category")

# 4) Weighted analysis (if you have a weight column, e.g. "weight")
results_nhis_weighted_srh <- run_sliding_windows_nhis_srhCat(
  data = data_nhis,
  start_years = start_years,
  window_length = window_length,
  weighted = TRUE,
  weight_var = "MORTWT"
)

plot_srh_categories_nhis(results_nhis_weighted_srh,
                         title = "NHIS (Weighted): HR by SRH Category")






######### NHANES

data_nhanes <- data_nhanes %>%
  mutate(srh_factor = factor(
    srh, 
    levels = c(1, 2, 3, 4, 5),
    labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")
  ))

prepare_window_nhanes <- function(data, start_year, window_length = 10) {
  data %>%
    filter(year >= start_year, year < start_year + window_length) %>%
    mutate(
      diff_age = age_last - age_visit,
      event_window = if_else(deceased == 1 & diff_age <= window_length, 1, 0),
      age_last_window = if_else(
        event_window == 1,
        age_last,
        age_visit + window_length
      )
    )
}

run_cox_nhanes <- function(df, weighted = FALSE, weight_var = "weight") {
  if (weighted) {
    # For real NHANES, specify PSU, strata, etc. Here we show a simplified example:
    design <- svydesign(ids = ~1, weights = as.formula(paste0("~", weight_var)), data = df)
    
    fit <- svycoxph(
      Surv(time = age_visit, time2 = age_last_window, event = event_window) ~ srh_factor,
      design = design
    )
  } else {
    fit <- coxph(
      Surv(time = age_visit, time2 = age_last_window, event = event_window) ~ srh_factor,
      data = df
    )
  }
  return(fit)
}

run_sliding_windows_nhanes_srhCat <- function(data, start_years, window_length = 10,
                                              weighted = FALSE, weight_var = "weight") {
  
  results <- tibble::tibble(
    start_year   = numeric(),
    srh_category = character(),
    coef         = numeric(),
    hr           = numeric(),
    conf_low     = numeric(),
    conf_high    = numeric(),
    p_value      = numeric(),
    n            = numeric()
  )
  
  for (sy in start_years) {
    df_win <- prepare_window_nhanes(data, sy, window_length)
    if (nrow(df_win) == 0) next
    
    fit <- run_cox_nhanes(df_win, weighted, weight_var)
    fit_summary <- summary(fit)
    
    # Identify row names for srh_factor categories
    all_vars <- rownames(fit_summary$coefficients)
    srh_rows <- grep("srh_factor", all_vars)
    if (length(srh_rows) == 0) next
    
    n_obs <- nrow(df_win)
    
    for (row_idx in srh_rows) {
      row_name <- all_vars[row_idx]
      coef_val <- fit_summary$coefficients[row_name, "coef"]
      hr_val   <- fit_summary$conf.int[row_name, "exp(coef)"]
      c_low    <- fit_summary$conf.int[row_name, "lower .95"]
      c_high   <- fit_summary$conf.int[row_name, "upper .95"]
      p_val    <- fit_summary$coefficients[row_name, "Pr(>|z|)"]
      
      srh_cat_label <- sub("srh_factor", "", row_name)
      
      results <- results %>%
        add_row(
          start_year   = sy,
          srh_category = srh_cat_label,
          coef         = coef_val,
          hr           = hr_val,
          conf_low     = c_low,
          conf_high    = c_high,
          p_value      = p_val,
          n            = n_obs
        )
    }
  }
  
  return(results)
}

plot_srh_categories_nhanes <- function(results,
                                       title = "NHANES: Hazard Ratios by SRH Category over Sliding Windows") {
  ggplot(results, aes(x = start_year, y = hr, color = srh_category, group = srh_category)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.3) +
    labs(x = "Start Year",
         y = "Hazard Ratio (SRH Category vs. Reference)",
         color = "SRH Category",
         title = title) +
    theme_minimal()
}


##### Apply

# Suppose your data frame is data_nhanes
library(dplyr)

min_year <- min(data_nhanes$year, na.rm = TRUE)  # e.g. 2001
max_year <- max(data_nhanes$year, na.rm = TRUE)  # e.g. 2017
window_length <- 8

# For NHANES, each wave is 2 years, so let's step by 2
start_years <- seq(min_year, max_year - window_length, by = 2)

# Unweighted
results_nhanes_unweighted_srh <- run_sliding_windows_nhanes_srhCat(
  data = data_nhanes,
  start_years = start_years,
  window_length = window_length,
  weighted = FALSE
)

plot_srh_categories_nhanes(results_nhanes_unweighted_srh,
                           title = "NHANES (Unweighted): HR by SRH Category")

# Weighted (simple design, adapt as needed for PSU, strata)
results_nhanes_weighted_srh <- run_sliding_windows_nhanes_srhCat(
  data = data_nhanes,
  start_years = start_years,
  window_length = window_length,
  weighted = TRUE,
  weight_var = "wt_mec"  # or your chosen weight
)

plot_srh_categories_nhanes(results_nhanes_weighted_srh,
                           title = "NHANES (Weighted): HR by SRH Category")

