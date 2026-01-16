# Load required packages
library(tidyverse)
library(survey)
library(broom)

# Function to compute and plot age coefficient trends by year
lm_coef_age_by_year <- function(data_object,
                                dataset_title,        # e.g., "BRFSS 1993-2023 Dataset"
                                is_weighted = FALSE,   # Set TRUE if data_object is a survey design object
                                year_var = "year",     # Name of the year variable
                                srh_var = "srh",       # Name of the self-rated health variable
                                age_var = "age_group"  # Name of the age variable (numeric; e.g., age in 5-year units)
) {
  # If weighted, assume data_object is a survey design; otherwise, it's a data frame
  
  if (is_weighted) {
    # Extract underlying data from the survey design object
    design_data <- data_object$variables
    unique_years <- sort(unique(design_data[[year_var]]))
    
    results <- map_dfr(unique_years, function(yr) {
      # Subset the survey design to the current year
      design_year <- data_object[data_object$variables[[year_var]] == yr, ]
      # Skip if no observations
      if(nrow(design_year$variables) == 0) return(NULL)
      
      # Run weighted regression
      model <- svyglm(as.formula(paste(srh_var, "~", age_var)), design = design_year)
      tidy_model <- broom::tidy(model, conf.int = TRUE)
      # Keep only the row corresponding to the age variable
      tidy_model <- tidy_model %>% filter(term == age_var)
      tidy_model[[year_var]] <- yr
      tidy_model
    })
    
  } else {
    # For unweighted data, group by year and run the regression within each group
    results <- data_object %>%
      group_by(across(all_of(year_var))) %>%
      group_modify(~ {
        model <- lm(as.formula(paste(srh_var, "~", age_var)), data = .x)
        tidy_model <- broom::tidy(model, conf.int = TRUE)
        tidy_model %>% filter(term == age_var)
      }) %>%
      ungroup()
  }
  
  # Rename and select columns for clarity
  results <- results %>%
    rename(coef = estimate,
           se = std.error,
           t_statistic = statistic,
           p_value = p.value) %>%
    select(!!year_var, coef, conf.low, conf.high, se, t_statistic, p_value)
  
  # Create a plot of the age coefficient over time with error bars
  p <- ggplot(results, aes_string(x = year_var, y = "coef")) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,
                  position = position_dodge(0.05)) +
    labs(title = paste("Change in Age Coefficient Over Years"),
         subtitle = dataset_title,
         x = "Year",
         y = paste("Coefficient of Age")) +
    theme_minimal()
  
  # Return a list containing the results table and the plot
  
  return(list(table = results, plot = p))
}

# Example usage:
# For unweighted data:
age_coef_over_time_brfss <- lm_coef_age_by_year(data_object = data_brfss,
                              dataset_title = "BRFSS 1993-2023 Dataset",
                              is_weighted = FALSE,
                              age_var = "age_5yr_num")
knitr::kable(age_coef_over_time_brfss$table, caption = "BRFSS 1993-2023 Dataset")
print(age_coef_over_time_brfss$plot)
#
# For weighted survey data:
age_coef_over_time_cps <- lm_coef_age_by_year(data_object = svy_cps,
                              dataset_title = "CPS Dataset",
                              is_weighted = TRUE,
                              year_var = "year",     # Name of the year variable
                              srh_var = "srh",       # Name of the self-rated health variable
                              age_var = "age_group"  # Name of the age variable (numeric; e.g., age in 5-year units)
)
knitr::kable(age_coef_over_time_cps$table, caption = "CPS Dataset")
print(age_coef_over_time_cps$plot)

# Weighted data
age_coef_over_time_cps <- lm_coef_age_by_year(data_object = svy_cps,
                              dataset_title = "CPS Dataset",
                              is_weighted = TRUE,
                              age_var = "age")
knitr::kable(age_coef_over_time_cps$table, caption = "CPS Dataset")
print(age_coef_over_time_cps$plot)











# Load required packages
library(tidyverse)
library(survey)
library(broom)

lm_coef_age_by_year <- function(data_object,
                                dataset_title,         # e.g., "BRFSS 1993-2023 Dataset"
                                is_weighted = FALSE,    # TRUE if data_object is a survey design object
                                year_var = "year",      # Name of the year variable
                                srh_var = "srh",        # Name of the self-rated health variable
                                age_var = "age",        # Name of the continuous age variable
                                weight_var = "ASECWT"   # Name of the weight variable (used only if is_weighted = TRUE)
) {
  # Check that the age variable is numeric
  if (is_weighted) {
    if (!is.numeric(data_object$variables[[age_var]])) {
      stop("For regression analysis, the age variable must be numeric. Please supply a numeric variable (e.g., age).")
    }
  } else {
    if (!is.numeric(data_object[[age_var]])) {
      stop("For regression analysis, the age variable must be numeric. Please supply a numeric variable (e.g., age).")
    }
  }
  
  # Extract unique years
  if (is_weighted) {
    unique_years <- sort(unique(data_object$variables[[year_var]]))
  } else {
    unique_years <- sort(unique(data_object[[year_var]]))
  }
  
  # Compute regression for each year and extract the coefficient for the age variable.
  if (is_weighted) {
    results <- map_dfr(unique_years, function(yr) {
      # Subset the survey design object to observations from the current year,
      # and filter out those with zero (or non-positive) weight.
      design_year <- data_object[
        data_object$variables[[year_var]] == yr &
          data_object$variables[[weight_var]] > 0,
      ]
      # Skip if no observations remain
      if(nrow(design_year$variables) == 0) return(NULL)
      
      # Run the weighted regression of srh on age
      model <- svyglm(as.formula(paste(srh_var, "~", age_var)), design = design_year)
      tidy_model <- broom::tidy(model, conf.int = TRUE)
      # Retain only the row corresponding to the age variable
      tidy_model <- tidy_model %>% filter(term == age_var)
      tidy_model[[year_var]] <- yr
      tidy_model
    })
  } else {
    results <- data_object %>%
      group_by(across(all_of(year_var))) %>%
      group_modify(~ {
        model <- lm(as.formula(paste(srh_var, "~", age_var)), data = .x)
        tidy_model <- broom::tidy(model, conf.int = TRUE)
        tidy_model %>% filter(term == age_var)
      }) %>%
      ungroup()
  }
  
  # Rename and select columns for clarity.
  results <- results %>%
    rename(coef = estimate,
           se = std.error,
           t_statistic = statistic,
           p_value = p.value) %>%
    select(!!year_var, coef, conf.low, conf.high, se, t_statistic, p_value)
  
  # Create a plot of the age coefficient over time with error bars.
  p <- ggplot(results, aes_string(x = year_var, y = "coef")) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,
                  position = position_dodge(0.05)) +
    labs(title = paste("Change in Age Coefficient Over Years", sep = ""),
         subtitle = dataset_title,
         x = "Year",
         y = paste("Coefficient of Age")) +
    theme_minimal()
  
  # Return both the results table and the plot.
  return(list(table = results, plot = p))
}

# Example usage:
# For unweighted data:
# result <- lm_coef_age_by_year(data_object = data_brfss,
#                               dataset_title = "BRFSS 1993-2023 Dataset",
#                               is_weighted = FALSE,
#                               age_var = "age")
# knitr::kable(result$table, caption = "BRFSS 1993-2023 Dataset")
# print(result$plot)
#
# For weighted survey data:
# result <- lm_coef_age_by_year(data_object = svy_brfss,
#                               dataset_title = "BRFSS 1993-2023 Dataset",
#                               is_weighted = TRUE,
#                               age_var = "age",
#                               weight_var = "ASECWT")
# knitr::kable(result$table, caption = "BRFSS 1993-2023 Dataset")
# print(result$plot)

