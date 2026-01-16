library(survey)      # For complex survey design & survival analysis
library(survival)    # For Surv objects
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(purrr)       # For functional programming
library(tibble)      # For tibbles

############################################################################
# 1. Example Lists of Age Groups and Time Periods
############################################################################

age_groups <- list(
  "18-29" = c(18, 29),
  "30-39" = c(30, 39),
  "40-49" = c(40, 49),
  "50-59" = c(50, 59),
  "60-69" = c(60, 69),
  "70+"   = c(70, Inf)
)

time_periods <- list(
  period2 = 1986:2001,
  period3 = 1991:2006,
  period4 = 1996:2011,
  period5 = 2001:2016,
  period6 = 2006:2018
)

############################################################################
# 2. Enhanced Weighted KM Function (Handling Censoring More Explicitly)
############################################################################

weighted_km_analysis <- function(
    survey_design,
    age_groups_list       = NULL,  # Named list of (min_age, max_age)
    selected_age_group    = NULL,  # Character key to pick age range
    time_period_list,
    selected_time_period,
    year_col              = "year",
    time_col              = "survtime",
    event_col             = "mortstat_recoded",
    group_col             = "srh_binary",
    se                    = FALSE,
    subset_age            = TRUE    # If FALSE, we do NOT subset by age
) {
  
  # Copy the design so we can modify its variables if needed
  # (especially to ensure event coding is correct)
  design_copy <- survey_design
  
  # --------------------------------------------------------------------
  # A) Force a proper 0/1 event indicator in the design object
  #    (Sometimes mortstat_recoded might have missing or
  #     other codes, so let's ensure we have 0 or 1 only.)
  # --------------------------------------------------------------------
  design_copy$variables <- design_copy$variables %>%
    mutate(
      # If your mortstat_recoded is already 0/1, this should be fine.
      # If it's something else, adapt accordingly:
      event_indicator = if_else(.data[[event_col]] == 1, 1, 0)
    )
  
  # --------------------------------------------------------------------
  # B) Subset by age if subset_age=TRUE
  # --------------------------------------------------------------------
  if (subset_age) {
    age_range <- age_groups_list[[selected_age_group]]
    design_copy <- subset(
      design_copy,
      AGE >= age_range[1] & AGE <= age_range[2]
    )
  }
  
  # --------------------------------------------------------------------
  # C) Subset by the selected time period
  # --------------------------------------------------------------------
  time_range <- time_period_list[[selected_time_period]]
  design_copy <- subset(
    design_copy,
    .data[[year_col]] >= min(time_range) & .data[[year_col]] <= max(time_range)
  )
  
  # --------------------------------------------------------------------
  # D) Fit the survey-weighted Kaplan-Meier Model
  #    Use our newly created 'event_indicator' instead of the old column.
  # --------------------------------------------------------------------
  km_formula <- as.formula(
    paste0("Surv(", time_col, ", event_indicator) ~ ", group_col)
  )
  
  km_fit <- svykm(
    formula = km_formula,
    design  = design_copy,
    se      = se
  )
  
  # --------------------------------------------------------------------
  # E) Convert km_fit to a Tidy Data Frame
  # --------------------------------------------------------------------
  km_df <- map2_df(
    .x = km_fit,
    .y = names(km_fit),
    ~ tibble(
      time         = .x$time,
      surv         = .x$surv,
      surv_lower   = .x$lower,  # If se=TRUE, else NA
      surv_upper   = .x$upper,  # If se=TRUE, else NA
      group_value  = .y         # The group label
    )
  )
  
  return(km_df)
}

############################################################################
# 3. Plot (A): Show All Ages for Each Time Period
############################################################################
#  - This loop does NOT subset by age, so we pass subset_age=FALSE.
#  - We'll facet by time period to see all period subsets in a single figure.

# Create a data frame of just the time period names
time_only <- tibble(time_period = names(time_periods))

# For each time_period, compute KM data
km_all_ages_time_df <- time_only %>%
  mutate(
    km_data = map(
      .x = time_period,
      .f = ~ weighted_km_analysis(
        survey_design        = survey_design,
        age_groups_list      = age_groups,
        selected_age_group   = NULL,    # not used
        time_period_list     = time_periods,
        selected_time_period = .x,
        subset_age           = FALSE,   # key: don't subset by age
        year_col             = "year",
        time_col             = "survtime",
        event_col            = "mortstat_recoded",
        group_col            = "srh_binary",
        se                   = FALSE
      )
    )
  ) %>%
  # Unnest the km_data into a single tibblea
  unnest(cols = km_data)

# Now we have one data frame with columns: time, surv, group_value, time_period
# Plot it
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

# Print or save
print(plot_all_ages_time)

############################################################################
# 4. Plot (B): Grid of Age Groups (Rows) vs. Time Periods (Columns)
############################################################################
#  - We'll create every combination of age_group x time_period
#  - Then call the KM function (subsetting by both age + period)
#  - Then plot with facet_grid(rows=vars(age_group), cols=vars(time_period))

all_combinations <- expand.grid(
  age_group   = names(age_groups),
  time_period = names(time_periods),
  stringsAsFactors = FALSE
)

km_grid_df <- pmap_dfr(
  list(all_combinations$age_group, all_combinations$time_period),
  function(ag, tp) {
    km_data <- weighted_km_analysis(
      survey_design        = survey_design,
      age_groups_list      = age_groups,
      selected_age_group   = ag,
      time_period_list     = time_periods,
      selected_time_period = tp,
      subset_age           = TRUE,    # we do want to subset by age here
      year_col             = "year",
      time_col             = "survtime",
      event_col            = "mortstat_recoded",
      group_col            = "srh_binary",
      se                   = FALSE
    )
    
    km_data %>%
      mutate(
        age_group   = ag,
        time_period = tp
      )
  }
)

# Now we can plot the grid
plot_age_period_grid <- ggplot(km_grid_df, aes(x = time, y = surv, color = group_value)) +
  geom_step(size = 1) +
  facet_grid(rows = vars(age_group), cols = vars(time_period)) +
  labs(
    x     = "Time (years)",
    y     = "Survival Probability",
    color = "SRH Group",
    title = "Survey-Weighted KM by Age Group (Rows) and Time Period (Columns)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_age_period_grid)
