library(survey)      # For complex survey design & survival analysis
library(survival)    # For Surv objects
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(purrr)       # For functional programming (map2_df, etc.)
library(tibble)      # For tibbles

############################################################################
# 1. Example lists of age groups and time periods
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
# 2. Example Weighted KM Function (Same as Before)
#    This function subsets by age group and time period, then computes
#    survey-weighted Kaplan-Meier estimates. 
############################################################################

weighted_km_analysis <- function(
    survey_design,             # A 'survey' package design object
    age_groups_list,           # A named list of age ranges
    selected_age_group,        # A string (e.g., "18-29")
    time_period_list,          # A named list of year sequences
    selected_time_period,      # e.g., "period2"
    year_col          = "year",
    time_col          = "survtime",
    event_col         = "mortstat_recoded",
    group_col         = "srh_binary",
    se                = FALSE
) {
  
  # ---- Subset by age ----
  age_range <- age_groups_list[[selected_age_group]]
  subset_expr_age <- quote(AGE >= age_range[1] & AGE <= age_range[2])
  
  # ---- Subset by time period ----
  time_range <- time_period_list[[selected_time_period]]
  # We'll use min and max of the vector
  subset_expr_time <- bquote(.(as.name(year_col)) >= .(min(time_range)) &
                               .(as.name(year_col)) <= .(max(time_range)))
  
  # Combine the subset expressions
  subset_design <- subset(
    survey_design,
    eval(subset_expr_age) & eval(subset_expr_time)
  )
  
  # ---- Fit the survey-weighted Kaplan-Meier model ----
  km_formula <- as.formula(
    paste0("Surv(", time_col, ", ", event_col, ") ~ ", group_col)
  )
  
  km_fit <- svykm(
    formula = km_formula,
    design  = subset_design,
    se      = se
  )
  
  # ---- Convert svykm result into a tidy data frame ----
  km_df <- map2_df(
    .x = km_fit,
    .y = names(km_fit),
    ~ {
      tibble(
        time         = .x$time,
        surv         = .x$surv,
        surv_lower   = .x$lower,  # If se=TRUE, else NA
        surv_upper   = .x$upper,  # If se=TRUE, else NA
        group_value  = .y         # The group label, e.g. "Fair-Poor"
      )
    }
  )
  
  return(km_df)
}

############################################################################
# 3. Generate a data frame of all (age_group, time_period) combinations
############################################################################

# We'll create a data frame that has columns: age_group, time_period
# that represent every combination. 
all_combinations <- expand.grid(
  age_group     = names(age_groups),
  time_period   = names(time_periods),
  stringsAsFactors = FALSE
)

############################################################################
# 4. Loop (map) through each combination, compute the KM, store the results
############################################################################

# We'll use map2_df to iterate over the rows in 'all_combinations', 
# calling weighted_km_analysis for each pair. We combine the outputs into
# a single tibble. 
km_all_df <- purrr::pmap_dfr(
  .l = list(all_combinations$age_group, all_combinations$time_period),
  .f = function(a, p) {
    # Compute weighted KM data for (age_group = a, time_period = p)
    km_df <- weighted_km_analysis(
      survey_design        = survey_design,
      age_groups_list      = age_groups,
      selected_age_group   = a,
      time_period_list     = time_periods,
      selected_time_period = p,
      year_col             = "YEAR",
      time_col             = "survtime",
      event_col            = "mortstat_recoded",
      group_col            = "srh_binary",
      se                   = FALSE
    )
    
    # Tag these results with the age_group and time_period
    km_df <- km_df %>%
      mutate(
        age_group     = a,
        time_period   = p
      )
    return(km_df)
  }
)

# Let's peek at the structure:
# head(km_all_df)
# This should have columns: time, surv, surv_lower, surv_upper, group_value,
# plus age_group, time_period

############################################################################
# 5. Create a single ggplot with facet_grid(rows=age_group, cols=time_period)
############################################################################

km_all_plot <- ggplot(km_all_df, aes(x = time, y = surv, color = group_value)) +
  geom_step(size = 1) +
  # If you'd like confidence intervals, you could do something like:
  # geom_ribbon(
  #   aes(ymin = surv_lower, ymax = surv_upper, fill = group_value),
  #   alpha = 0.2,
  #   color = NA
  # ) +
  facet_grid(rows = vars(age_group), cols = vars(time_period)) +
  labs(
    x     = "Time (years)",
    y     = "Survival Probability",
    color = "SRH Group",
    title = "Survey-Weighted KM by Age Group (Rows) and Time Period (Columns)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print or save the final multi-facet figure
print(km_all_plot)
