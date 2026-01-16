# bad because of the bug where everyone was dead


library(survey)      # For complex survey design & survival analysis
library(survival)    # For Surv objects
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(purrr)       # For functional programming (map2_df)
library(tibble)      # For tibbles

# Example age groups:
age_groups <- list(
  "18-29" = c(18, 29),
  "30-39" = c(30, 39),
  "40-49" = c(40, 49),
  "50-59" = c(50, 59),
  "60-69" = c(60, 69),
  "70+"   = c(70, Inf)
)

############################################################################
# Define a function that subsets data by a chosen age group and 
# produces survey-weighted Kaplan-Meier estimates and a ggplot2 survival curve.
############################################################################

weighted_km_analysis <- function(
    survey_design,            # A 'survey' package design object
    age_groups_list,          # A named list of age ranges (like the one above)
    selected_age_group,       # A string key to pick an age range (e.g., "18-29")
    time_col         = "survtime",         # The column name for time-to-event
    event_col        = "mortstat_recoded", # The column name for event indicator
    group_col        = "srh_binary",       # The column used to group curves
    se               = FALSE               # Whether to compute standard errors
) {
  
  # ----------------------------------------------------------------------
  # 1. Subset the design by the selected age group
  # ----------------------------------------------------------------------
  # Extract the numeric range (e.g., c(18, 29)) from the named list
  age_range <- age_groups_list[[selected_age_group]]
  
  # Subset the design object for individuals within the chosen age range
  # Assumes your underlying data has an 'age' variable in the design
  subset_design <- subset(
    survey_design, 
    AGE >= age_range[1] & AGE <= age_range[2]
  )
  
  # ----------------------------------------------------------------------
  # 2. Fit the survey-weighted Kaplan-Meier model
  # ----------------------------------------------------------------------
  # The Surv() function takes time and event indicator (1=event, 0=censored)
  # 'group_col' is the grouping variable (e.g., "srh_binary")
  # 'svykm()' applies a Kaplan-Meier estimator that respects survey weights
  
  # Dynamically build the formula: Surv(time_col, event_col) ~ group_col
  km_formula <- as.formula(
    paste0("Surv(", time_col, ", ", event_col, ") ~ ", group_col)
  )
  
  km_fit <- svykm(
    formula = km_formula,
    design  = subset_design,
    se      = se
  )
  
  # ----------------------------------------------------------------------
  # 3. Convert the svykm result into a tidy data frame
  # ----------------------------------------------------------------------
  # km_fit is a list of survival curve objects, each named by level of 'group_col'
  km_df <- map2_df(
    .x = km_fit,
    .y = names(km_fit),
    ~ {
      tibble(
        time         = .x$time,
        surv         = .x$surv,
        surv_lower   = .x$lower,  # Available if se=TRUE, else NA
        surv_upper   = .x$upper,  # Available if se=TRUE, else NA
        group_value  = .y         # The group label, e.g. "Fair-Poor"
      )
    }
  )
  
  # ----------------------------------------------------------------------
  # 4. Plot the survival curves using ggplot2
  # ----------------------------------------------------------------------
  # We plot a step function for the survival probability,
  # optionally adding confidence bands if se=TRUE.
  
  km_plot <- ggplot(km_df, aes(x = time, y = surv, color = group_value)) +
    geom_step(size = 1) +
    # If confidence intervals are present, you could add geom_ribbon or
    # geom_line(...) to depict the lower and upper bounds:
    # geom_ribbon(aes(ymin = surv_lower, ymax = surv_upper, fill = group_value),
    #             alpha = 0.2, color = NA) +
    labs(
      x     = "Time (years)",
      y     = "Survival Probability",
      color = group_col,
    #  title = paste0("Survey-Weighted KM Curves by ", group_col, 
                   #  " (Age Group: ", selected_age_group, ")")
      title = "KM Curves by SRH",
      subtitle = paste0("Age Group: ", selected_age_group)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # ----------------------------------------------------------------------
  # 5. Return the tidy data frame and the plot
  # ----------------------------------------------------------------------
  return(list(km_data = km_df, km_plot = km_plot))
}

############################################################################
# Example Usage
############################################################################

# Suppose your existing survey design object is called 'survey_design' 
# and it contains variables:
# - 'age'                  = respondent's age
# - 'survtime'             = time variable
# - 'mortstat_recoded'     = event indicator (1=death, 0=alive)
# - 'srh_binary'           = "Good-Excellent" vs "Fair-Poor"

# Let's run the function for the 30-39 age group:
results_30_39 <- weighted_km_analysis(
  survey_design       = survey_design,
  age_groups_list     = age_groups,
  selected_age_group  = "30-39",
  time_col            = "survtime",
  event_col           = "mortstat_recoded",
  group_col           = "srh_binary",
  se                  = FALSE
)

# Extract the resulting data frame:
km_df_30_39   <- results_30_39$km_data
# Plot object:
km_plot_30_39 <- results_30_39$km_plot
# Display the plot:
print(km_plot_30_39)

results_18_29 <- weighted_km_analysis(
  survey_design       = survey_design,
  age_groups_list     = age_groups,
  selected_age_group  = "18-29"#,
 # time_col            = "survtime",
 # event_col           = "mortstat_recoded",
 # group_col           = "srh_binary",
 # se                  = FALSE
)
results_18_29$km_plot

results_40_49 <- weighted_km_analysis(
  survey_design       = survey_design,
  age_groups_list     = age_groups,
  selected_age_group  = "40-49"#,
  # time_col            = "survtime",
  # event_col           = "mortstat_recoded",
  # group_col           = "srh_binary",
  # se                  = FALSE
)
results_40_49$km_plot

results_50_59 <- weighted_km_analysis(
  survey_design       = survey_design,
  age_groups_list     = age_groups,
  selected_age_group  = "50-59"#,
  # time_col            = "survtime",
  # event_col           = "mortstat_recoded",
  # group_col           = "srh_binary",
  # se                  = FALSE
)
results_50_59$km_plot

results_60_69 <- weighted_km_analysis(
  survey_design       = survey_design,
  age_groups_list     = age_groups,
  selected_age_group  = "60-69"#,
  # time_col            = "survtime",
  # event_col           = "mortstat_recoded",
  # group_col           = "srh_binary",
  # se                  = FALSE
)
results_60_69$km_plot

results_70 <- weighted_km_analysis(
  survey_design       = survey_design,
  age_groups_list     = age_groups,
  selected_age_group  = "70+"#,
  # time_col            = "survtime",
  # event_col           = "mortstat_recoded",
  # group_col           = "srh_binary",
  # se                  = FALSE
)
results_70$km_plot
