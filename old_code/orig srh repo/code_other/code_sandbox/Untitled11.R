weighted_km_analysis <- function(
    survey_design,
    age_groups_list       = NULL,  # Named list of (min_age, max_age)
    selected_age_group    = NULL,  # Character key to pick age range (may be NULL)
    time_period_list,
    selected_time_period,
    year_col              = "year",
    time_col              = "survtime",
    event_col             = "mortstat_recoded",
    group_col             = "srh_binary",
    se                    = FALSE,
    subset_age            = TRUE
) {
  # Make a copy of the original design
  design_copy <- survey_design
  
  # 1) Create a proper 0/1 event indicator in the design copy
  design_copy$variables$event_indicator <- ifelse(
    design_copy$variables[[event_col]] == 1, 
    1, 
    0
  )
  
  # 2) Subset by age if desired (subset_age=TRUE)
  if (subset_age && !is.null(selected_age_group)) {
    age_range <- age_groups_list[[selected_age_group]]
    design_copy <- subset(
      design_copy,
      design_copy$variables[["age"]] >= age_range[1] &
        design_copy$variables[["age"]] <= age_range[2]
    )
  }
  
  # 3) Subset by the selected time period
  time_range <- time_period_list[[selected_time_period]]
  design_copy <- subset(
    design_copy,
    design_copy$variables[[year_col]] >= min(time_range) &
      design_copy$variables[[year_col]] <= max(time_range)
  )
  
  # 4) Fit the survey-weighted Kaplan-Meier Model
  # Build Surv(...) from time_col and event_indicator
  km_formula <- as.formula(
    paste0("Surv(", time_col, ", event_indicator) ~ ", group_col)
  )
  
  km_fit <- svykm(
    formula = km_formula,
    design  = design_copy,
    se      = se
  )
  
  # 5) Convert km_fit to a tidy data frame
  library(purrr)  # ensure this is loaded
  library(tibble)
  
  km_df <- map2_df(
    .x = km_fit,
    .y = names(km_fit),
    ~ tibble(
      time         = .x$time,
      surv         = .x$surv,
      surv_lower   = .x$lower,  # if se=TRUE, else NA
      surv_upper   = .x$upper,  # if se=TRUE, else NA
      group_value  = .y         # factor level from group_col
    )
  )
  
  return(km_df)
}

##############

library(purrr)
library(dplyr)

time_period_vec <- names(time_periods)

# For each time period, run the function
km_list <- map(time_period_vec, function(tp) {
  df <- weighted_km_analysis(
    survey_design        = survey_design,
    age_groups_list      = age_groups,
    selected_age_group   = NULL,
    time_period_list     = time_periods,
    selected_time_period = tp,
    subset_age           = FALSE, 
    year_col             = "year",
    time_col             = "survtime",
    event_col            = "mortstat_recoded",
    group_col            = "srh_binary"
  )
  # Tag the time_period
  df$time_period <- tp
  df
})

# Combine into a single data frame
km_all_ages_time_df <- bind_rows(km_list)

# Same plotting code as before:
plot_all_ages_time <- ggplot(km_all_ages_time_df, aes(x = time, y = surv, color = group_value)) +
  geom_step(size = 1) +
  facet_wrap(~ time_period) +
  labs(
    x     = "Time (years)",
    y     = "Survival Probability",
    color = "SRH Group",
    title = "Survey-Weighted KM: All Ages Across Different Time Periods"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_all_ages_time)
