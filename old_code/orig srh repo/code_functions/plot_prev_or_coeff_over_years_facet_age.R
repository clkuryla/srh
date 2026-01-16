##### This flexible function complements the SRH analysis. Its purpose is to explore additional variables in the dataset. It facets by age group and then plots over years (time): either the prevalence or the association, as measured by a coefficient, on self-rated health, with the possibility of adjustment for variables. 

library(tidyverse)
library(survey)
library(broom)


# Master function for creating survey-weighted trend plots
# 
# For coefficient plots:
# - outcome: regress on this. usually srh
# - predictors: variables whose coefficients you want to PLOT
# - adjust_for: additional covariates to include in the model (for adjustment) but NOT plot
# - The regression will include: outcome ~ predictors + adjust_for
# - Only coefficients for 'predictors' will be shown in the plot
# - The output will include both plotted coefficients and full model results

fctn_plot_prev_or_coeff_over_yrs_facet_age <- function(svy_data,
                                 plot_type = c("coefficient", "prevalence"),
                                 variables = NULL,        # For prevalence plots
                                 predictors = NULL,       # For coefficient plots - variables to plot
                                 adjust_for = NULL,       # Additional covariates to adjust for (not plotted)
                                 outcome = NULL,          # For coefficient plots
                                 age_var = "age_decade_cat",
                                 year_var = "year",
                                 colors = NULL,
                                 legend_labels = NULL,    # Custom legend labels
                                 overall_title = NULL,
                                 y_label = NULL,
                                 x_label = "Year",
                                 legend_title = NULL,
                                 legend_position = c(0.76, 0.18),
                                 legend_justification = c(0, 0),
                                 title_bold = FALSE,
                                 strip_text_size = 11,
                                 strip_text_bold = FALSE,
                                 title_size = 14,
                                 facet_rows = 1,
                                 confidence_level = 0.95,
                                 min_n = 30) {
  
  # Validate plot type
  plot_type <- match.arg(plot_type)
  
  # Default colors if not provided
  if(is.null(colors)) {
    # colors <- c("#E91E63", "#8BC34A", "#2196F3", "#FF9800", "#9C27B0", 
    #             "lightpink", "gold", "mediumblue", "gray")
    colors <- c("cornflowerblue", "mediumvioletred", "chartreuse3", "darkgoldenrod1",
                "mediumpurple1", "darkorange2", 
                "lightpink", "aquamarine", "gray", "gold")    
  }
  
  # Set default titles based on plot type
  if(is.null(overall_title)) {
    overall_title <- ifelse(plot_type == "coefficient", 
                            "Coefficient Trends by Age Group",
                            "Health Measure Trends by Age Group")
  }
  
  if(is.null(y_label)) {
    y_label <- ifelse(plot_type == "coefficient", 
                      "Coefficient", 
                      "Average Value")
  }
  
  if(is.null(legend_title)) {
    legend_title <- ifelse(plot_type == "coefficient", 
                           "Predictor", 
                           "Variable")
  }
  
  # Get unique age groups and years
  age_groups <- unique(svy_data$variables[[age_var]])
  age_groups <- age_groups[!is.na(age_groups)]
  
  years <- unique(svy_data$variables[[year_var]])
  years <- sort(years[!is.na(years)])
  
  # Initialize results data frame
  results_df <- data.frame()
  
  # Branch based on plot type
  if(plot_type == "coefficient") {
    # Coefficient plot logic
    if(is.null(predictors) || is.null(outcome)) {
      stop("For coefficient plots, both 'predictors' and 'outcome' must be specified")
    }
    
    # Combine predictors and adjustment covariates for the model
    all_model_vars <- c(predictors, adjust_for)
    
    # Initialize results for plotting and full model results
    plot_results_df <- data.frame()
    full_results_df <- data.frame()
    
    # Loop through age groups and years
    for(age in age_groups) {
      for(yr in years) {
        # Subset survey data
        svy_subset <- subset(svy_data, 
                             get(age_var) == age & get(year_var) == yr)
        
        # Check if subset has enough data
        if(nrow(svy_subset$variables) > min_n) {
          # Build formula with all variables
          formula_str <- paste(outcome, "~", paste(all_model_vars, collapse = " + "))
          formula_obj <- as.formula(formula_str)
          
          # Try to fit model
          tryCatch({
            model <- svyglm(formula_obj, design = svy_subset)
            
            # Extract coefficients with confidence intervals
            coef_summary <- tidy(model, conf.int = TRUE, conf.level = confidence_level)
            
            # Add metadata
            coef_summary$age_group <- as.character(age)
            coef_summary$year <- yr
            
            # Separate results for plotting (only predictors) and full results
            plot_coef <- coef_summary[coef_summary$term %in% predictors, ]
            all_coef <- coef_summary[coef_summary$term %in% all_model_vars, ]
            
            # Mark which variables are adjustment variables
            all_coef$variable_type <- ifelse(all_coef$term %in% predictors, 
                                             "Plotted", "Adjustment")
            
            # Bind to results
            plot_results_df <- rbind(plot_results_df, plot_coef)
            full_results_df <- rbind(full_results_df, all_coef)
          }, error = function(e) {
            # Skip if model fails
            NULL
          })
        }
      }
    }
    
    # Use plot_results_df for plotting
    results_df <- plot_results_df %>%
      rename(
        variable = term,
        value = estimate,
        se = std.error,
        ci_lower = conf.low,
        ci_upper = conf.high
      )
    
    # Also rename full results table for consistency
    if(nrow(full_results_df) > 0) {
      full_results_df <- full_results_df %>%
        rename(
          variable = term,
          coefficient = estimate,
          se = std.error,
          ci_lower = conf.low,
          ci_upper = conf.high
        )
    }
    
    # Variables to use for plotting
    plot_vars <- predictors
    
  } else {
    # Prevalence/average plot logic
    if(is.null(variables)) {
      stop("For prevalence plots, 'variables' must be specified")
    }
    
    # Loop through variables, age groups, and years
    for(var in variables) {
      # Check variable type
      var_values <- svy_data$variables[[var]]
      is_binary <- all(var_values %in% c(0, 1, NA))
      
      for(age in age_groups) {
        for(yr in years) {
          # Subset survey data
          svy_subset <- subset(svy_data, 
                               get(age_var) == age & get(year_var) == yr)
          
          # Check if subset has enough data
          if(nrow(svy_subset$variables) > min_n) {
            # Add check for non-NA values
            non_na_count <- sum(!is.na(svy_subset$variables[[var]]))
            
            # Only proceed if there are actual non-NA values
            if(non_na_count > 0) {
              tryCatch({
                # Calculate weighted mean (works for both binary and continuous)
                result <- svymean(as.formula(paste("~", var)), 
                                  design = svy_subset, 
                                  na.rm = TRUE)
                estimate <- as.numeric(result)[1]
                se <- as.numeric(sqrt(attr(result, "var")))[1]
                
                # Calculate confidence intervals
                z_score <- qnorm(1 - (1 - confidence_level) / 2)
                ci_lower <- estimate - z_score * se
                ci_upper <- estimate + z_score * se
                
                # If binary, convert to percentage
                if(is_binary) {
                  estimate <- estimate * 100
                  ci_lower <- ci_lower * 100
                  ci_upper <- ci_upper * 100
                  se <- se * 100
                }
                
                # Add to results
                new_row <- data.frame(
                  variable = var,
                  age_group = as.character(age),
                  year = yr,
                  value = estimate,
                  se = se,
                  ci_lower = ci_lower,
                  ci_upper = ci_upper,
                  type = ifelse(is_binary, "prevalence", "mean")
                )
                
                results_df <- rbind(results_df, new_row)
                
              }, error = function(e) {
                # Skip if calculation fails
                NULL
              })
            }
          }
        }
      }
    }
    
    # Check if any results were generated
    if(nrow(results_df) == 0) {
      stop("No valid data found. This could mean:\n",
           "  - Variables don't exist in the dataset\n",
           "  - All values are NA for the selected variables\n",
           "  - No age-year combinations meet the min_n threshold (", min_n, ")\n",
           "  Try: checking variable names, lowering min_n, or filtering years first")
    }
    
    # Variables to use for plotting
    plot_vars <- variables
  }
  
  # Create named color vector for proper mapping
  color_mapping <- setNames(colors[1:length(plot_vars)], plot_vars)
  
  # Create legend labels - use custom if provided, otherwise auto-generate
  if(!is.null(legend_labels)) {
    # Check if the right number of labels were provided
    if(length(legend_labels) != length(plot_vars)) {
      stop(paste("Number of legend_labels (", length(legend_labels), 
                 ") must match number of variables/predictors (", 
                 length(plot_vars), ")", sep = ""))
    }
    label_mapping <- setNames(legend_labels, plot_vars)
  } else {
    # Auto-generate nice labels
    label_mapping <- setNames(
      stringr::str_to_title(gsub("_", " ", plot_vars)),
      plot_vars
    )
  }
  
  # Create the plot
  p <- ggplot(results_df, aes(x = year, y = value, color = variable)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 0.8, alpha = 0.6) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = variable), 
                alpha = 0.4, linetype = 0) +
    # facet_wrap(~ age_group, nrow = facet_rows) +
    facet_wrap(~ age_group, nrow = 1) +
    scale_color_manual(values = color_mapping, labels = label_mapping) +
    scale_fill_manual(values = color_mapping, labels = label_mapping) +
    labs(
      title = overall_title,
      subtitle = "Age Group (Years)",
      x = x_label,
      y = y_label,
      color = legend_title,
      fill = legend_title
    ) +
    theme_minimal() +
    theme(
      # legend.position = "inside",  # First specify that legend should be inside
      # legend.position.inside = legend_position,  # Then specify where inside
      # legend.justification = legend_justification,
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.8, "lines"),
      axis.text.x = element_text(angle = 45), #, hjust = 1, vjust = 1),
      strip.text = element_text(
        size = strip_text_size, 
        face = ifelse(strip_text_bold, "bold", "plain")
      ),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        hjust = 0.5, 
        size = title_size, 
        face = ifelse(title_bold, "bold", "plain")
      ),
      plot.subtitle = element_text(  
        hjust = 0,  # Left align
        size = strip_text_size,
        face = ifelse(strip_text_bold, "bold", "plain"),
        margin = margin(b = 5)
      )
    )
  
  # Create summary table
  if(plot_type == "coefficient") {
    # For coefficient plots, show full model results if adjustment variables were used
    if(!is.null(adjust_for) && length(adjust_for) > 0 && nrow(full_results_df) > 0) {
      summary_table <- full_results_df %>%
        select(age_group, year, variable, variable_type, coefficient, se, p.value, ci_lower, ci_upper) %>%
        arrange(age_group, year, variable_type, variable) %>%
        mutate(across(where(is.numeric), ~round(., 4)))
      
      # Also create a separate table for just the plotted coefficients
      plot_summary <- summary_table %>%
        filter(variable_type == "Plotted") %>%
        select(-variable_type)
      
      cat("\nSummary of PLOTTED coefficients (first 20 rows):\n")
      print(head(plot_summary, 20))
      
      cat("\nFull model results including adjustment variables (first 30 rows):\n")
      print(head(summary_table, 30))
    } else {
      # No adjustment variables, just show the regular summary
      summary_table <- results_df %>%
        select(age_group, year, variable, value, se, p.value, ci_lower, ci_upper) %>%
        rename(coefficient = value) %>%
        mutate(across(where(is.numeric), ~round(., 4)))
      
      cat("\nSummary of results (first 20 rows):\n")
      print(head(summary_table, 20))
    }
  } else {
    summary_table <- results_df %>%
      select(age_group, year, variable, value, se, ci_lower, ci_upper, type) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
    
    cat("\nSummary of results (first 20 rows):\n")
    print(head(summary_table, 20))
  }
  
  # Return both plot and data
  if(plot_type == "coefficient" && !is.null(adjust_for) && length(adjust_for) > 0 && nrow(full_results_df) > 0) {
    return(list(
      plot = p, 
      plotted_data = plot_summary,
      full_model_data = summary_table
    ))
  } else {
    return(list(plot = p, data = summary_table))
  }
}

# How to use
# Helper function to combine multiple plots if needed
combine_plots <- function(plot_list, 
                          ncol = 1, 
                          main_title = "Combined Analysis") {
  
  library(patchwork)
  
  # Combine plots using patchwork
  combined <- wrap_plots(plot_list, ncol = ncol) +
    plot_annotation(
      title = main_title,
      theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    )
  
  return(combined)
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================
# NOTE 1: You can now specify custom legend labels!
# Use legend_labels = c("Label1", "Label2", "Label3") to manually set legend names
# The labels must be in the same order as your predictors/variables

# NOTE 2: For coefficient plots, you can adjust for covariates without plotting them!
# Use predictors = variables you want to plot
# Use adjust_for = additional covariates to include in the model (but not plot)
# The function will return both the plotted coefficients and the full model results

# Example 1: Coefficient plot with custom legend labels AND adjustment variables
# coef_results <- plot_survey_trends(
#   svy_data = svy_brfss,
#   plot_type = "coefficient",
#   predictors = c("physical_health", "mental_health", "usual_activities_health"),
#   adjust_for = c("diabetes_dx", "htn_dx", "sex"),  # Adjust for these but don't plot
#   outcome = "srh",
#   age_var = "age_decade_cat",
#   year_var = "year",
#   legend_labels = c("Physical", "Mental", "Functional"),  # Custom legend labels
#   overall_title = "Adjusted Health Coefficients on Self-Rated Health",
#   y_label = "Adjusted Coefficient on SRH",
#   legend_position = c(0.76, 0.18)
# )
# Access results:
# coef_results$plot           # The plot
# coef_results$plotted_data   # Coefficients that are plotted
# coef_results$full_model_data # All coefficients including adjustment variables

# Example 2: Prevalence/Average plot with custom legend labels
# prev_results <- plot_survey_trends(
#   svy_data = svy_brfss,
#   plot_type = "prevalence",
#   variables = c("physical_health_good_days", "mental_health_good_days", 
#                 "usual_activities_health_good_days"),
#   age_var = "age_decade_cat",
#   year_var = "year",
#   legend_labels = c("Physical Health Days", "Mental Health Days", "Functional Days"),
#   overall_title = "Trends in Health Measures",
#   y_label = "Average Good Days",
#   legend_position = c(0.76, 0.18)
# )

# Example 3: Prevalence plot for binary variables with custom labels
# binary_results <- plot_survey_trends(
#   svy_data = svy_brfss,
#   plot_type = "prevalence",
#   variables = c("diabetes_dx", "htn_dx", "dep_dx"),
#   age_var = "age_decade_cat",
#   year_var = "year",
#   legend_labels = c("Diabetes", "Hypertension", "Depression"),  # Custom clean labels
#   colors = c("#FF5722", "#9C27B0", "#00BCD4"),  # Custom colors
#   overall_title = "Disease Prevalence Trends",
#   y_label = "Prevalence (%)",
#   legend_title = "Condition",
#   legend_position = c(0.76, 0.18)
# )

# Example 4: Single predictor with multiple adjustment covariates
# adjusted_results <- plot_survey_trends(
#   svy_data = svy_brfss,
#   plot_type = "coefficient",
#   predictors = "diabetes_dx",  # Just plot diabetes coefficient
#   adjust_for = c("sex", "htn_dx", "physical_health", "mental_health"),  # Adjust for these
#   outcome = "srh",
#   legend_labels = c("Diabetes (adjusted)"),
#   overall_title = "Association of Diabetes with SRH (Fully Adjusted Model)",
#   y_label = "Adjusted Coefficient"
# )

# Example 5: With custom formatting and legend labels
# custom_results <- plot_survey_trends(
#   svy_data = svy_brfss,
#   plot_type = "coefficient",
#   predictors = c("physical_health", "mental_health"),
#   outcome = "srh",
#   legend_labels = c("Physical Health\n(Good Days)", "Mental Health\n(Good Days)"),
#   legend_position = c(0.80, 0.15),      # Adjust legend position
#   title_bold = FALSE,                   # Unbold title
#   strip_text_bold = FALSE,              # Unbold facet labels
#   strip_text_size = 10,                 # Smaller facet labels
#   title_size = 12,                      # Smaller title
#   facet_rows = 3,                       # 3 rows instead of 2
#   confidence_level = 0.99,              # 99% confidence intervals
#   min_n = 50                            # Require at least 50 observations
# )
