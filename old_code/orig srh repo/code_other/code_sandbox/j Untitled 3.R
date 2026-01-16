library(tidyverse)
library(survey)
library(broom)

fctn_plot_prev_or_coeff_over_yrs_facet_age <- function(
    svy_data,
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
    min_n = 30
) {
  # Validate plot type
  plot_type <- match.arg(plot_type)
  
  # Default colors if not provided
  if (is.null(colors)) {
    colors <- c(
      "cornflowerblue", "mediumvioletred", "chartreuse3", "darkgoldenrod1",
      "mediumpurple1", "darkorange2", 
      "lightpink", "aquamarine", "gray", "gold"
    )
  }
  
  # Set default titles based on plot type
  if (is.null(overall_title)) {
    overall_title <- ifelse(
      plot_type == "coefficient", 
      "Coefficient Trends by Age Group",
      "Response Trends by Age Group"
    )
  }
  
  if (is.null(y_label)) {
    y_label <- ifelse(
      plot_type == "coefficient", 
      "Coefficient", 
      "Average Value"
    )
  }
  
  if (is.null(legend_title)) {
    legend_title <- ifelse(
      plot_type == "coefficient", 
      "Predictor", 
      "Survey Response"
    )
  }
  
  # Get unique age groups and years
  age_groups <- unique(svy_data$variables[[age_var]])
  age_groups <- age_groups[!is.na(age_groups)]
  
  years <- unique(svy_data$variables[[year_var]])
  years <- sort(years[!is.na(years)])
  
  # Initialize results data frame
  results_df <- data.frame()
  
  # ---------------------------------------------------------------------------
  # COEFFICIENT PLOTS
  # ---------------------------------------------------------------------------
  if (plot_type == "coefficient") {
    if (is.null(predictors) || is.null(outcome)) {
      stop("For coefficient plots, both 'predictors' and 'outcome' must be specified")
    }
    
    # Combine predictors and adjustment covariates for the model
    all_model_vars <- c(predictors, adjust_for)
    
    # Initialize results for plotting and full model results
    plot_results_df <- data.frame()
    full_results_df <- data.frame()
    
    # Loop through age groups and years
    for (age in age_groups) {
      for (yr in years) {
        # Explicit logical index from design variables
        vars <- svy_data$variables
        idx  <- vars[[age_var]] == age & vars[[year_var]] == yr
        
        if (!any(idx, na.rm = TRUE)) next
        
        # Subset survey design
        svy_subset <- subset(svy_data, idx)
        
        # Check if subset has enough data
        if (nrow(svy_subset$variables) > min_n) {
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
            coef_summary$year      <- yr
            
            # Separate results for plotting (only predictors) and full results
            plot_coef <- coef_summary[coef_summary$term %in% predictors, ]
            all_coef  <- coef_summary[coef_summary$term %in% all_model_vars, ]
            
            # Mark which variables are adjustment variables
            all_coef$variable_type <- ifelse(
              all_coef$term %in% predictors, 
              "Plotted", 
              "Adjustment"
            )
            
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
        value    = estimate,
        se       = std.error,
        ci_lower = conf.low,
        ci_upper = conf.high
      )
    
    # Also rename full results table for consistency
    if (nrow(full_results_df) > 0) {
      full_results_df <- full_results_df %>%
        rename(
          variable    = term,
          coefficient = estimate,
          se          = std.error,
          ci_lower    = conf.low,
          ci_upper    = conf.high
        )
    }
    
    # Variables to use for plotting
    plot_vars <- predictors
    
    # ---------------------------------------------------------------------------
    # PREVALENCE / MEAN PLOTS
    # ---------------------------------------------------------------------------
  } else {
    # Prevalence/average plot logic
    if (is.null(variables)) {
      stop("For prevalence plots, 'variables' must be specified")
    }
    
    # Loop through variables, age groups, and years
    for (var in variables) {
      # Check variable type
      var_values <- svy_data$variables[[var]]
      is_binary  <- all(var_values %in% c(0, 1, NA))
      
      for (age in age_groups) {
        for (yr in years) {
          # Build logical index for this age–year cell
          vars <- svy_data$variables
          idx  <- vars[[age_var]] == age & vars[[year_var]] == yr
          
          # If no rows match, skip
          if (!any(idx, na.rm = TRUE)) next
          
          # Subset the survey design with the logical index
          svy_subset <- subset(svy_data, idx)
          
          # Check if subset has enough data
          if (nrow(svy_subset$variables) > min_n) {
            # Add check for non-NA values
            non_na_count <- sum(!is.na(svy_subset$variables[[var]]))
            
            # Only proceed if there are actual non-NA values
            if (non_na_count > 0) {
              tryCatch({
                # Calculate weighted mean (works for both binary and continuous)
                result   <- svymean(as.formula(paste("~", var)), 
                                    design = svy_subset, 
                                    na.rm  = TRUE)
                estimate <- as.numeric(result)[1]
                se       <- as.numeric(sqrt(attr(result, "var")))[1]
                
                # Calculate confidence intervals
                z_score  <- qnorm(1 - (1 - confidence_level) / 2)
                ci_lower <- estimate - z_score * se
                ci_upper <- estimate + z_score * se
                
                # If binary, convert to percentage
                if (is_binary) {
                  estimate <- estimate * 100
                  ci_lower <- ci_lower * 100
                  ci_upper <- ci_upper * 100
                  se       <- se * 100
                }
                
                # Add to results
                new_row <- data.frame(
                  variable  = var,
                  age_group = as.character(age),
                  year      = yr,
                  value     = estimate,
                  se        = se,
                  ci_lower  = ci_lower,
                  ci_upper  = ci_upper,
                  type      = ifelse(is_binary, "prevalence", "mean")
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
    if (nrow(results_df) == 0) {
      stop(
        "No valid data found. This could mean:\n",
        "  - Variables don't exist in the dataset\n",
        "  - All values are NA for the selected variables\n",
        "  - No age-year combinations meet the min_n threshold (", min_n, ")\n",
        "  Try: checking variable names, lowering min_n, or filtering years first"
      )
    }
    
    # Variables to use for plotting
    plot_vars <- variables
  }
  
  # ---------------------------------------------------------------------------
  # PLOT SETUP
  # ---------------------------------------------------------------------------
  # Create named color vector for proper mapping
  color_mapping <- setNames(colors[1:length(plot_vars)], plot_vars)
  
  # Create legend labels - use custom if provided, otherwise auto-generate
  if (!is.null(legend_labels)) {
    if (length(legend_labels) != length(plot_vars)) {
      stop(
        "Number of legend_labels (", length(legend_labels), 
        ") must match number of variables/predictors (", length(plot_vars), ")"
      )
    }
    label_mapping <- setNames(legend_labels, plot_vars)
  } else {
    label_mapping <- setNames(
      stringr::str_to_title(gsub("_", " ", plot_vars)),
      plot_vars
    )
  }
  
  # Create the plot
  p <- ggplot(results_df, aes(x = year, y = value, color = variable)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 0.8, alpha = 0.6) +
    geom_ribbon(
      aes(ymin = ci_lower, ymax = ci_upper, fill = variable), 
      alpha   = 0.4, 
      linetype = 0
    ) +
    facet_wrap(~ age_group, nrow = facet_rows) +
    scale_color_manual(values = color_mapping, labels = label_mapping) +
    scale_fill_manual(values  = color_mapping, labels = label_mapping) +
    labs(
      title    = overall_title,
      subtitle = "Age Group (Years)",
      x        = x_label,
      y        = y_label,
      color    = legend_title,
      fill     = legend_title
    ) +
    theme_minimal() +
    theme(
      legend.position   = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.key.size   = unit(0.8, "lines"),
      axis.text.x       = element_text(angle = 45),
      strip.text        = element_text(
        size = strip_text_size, 
        face = ifelse(strip_text_bold, "bold", "plain")
      ),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(
        hjust = 0.5, 
        size  = title_size, 
        face  = ifelse(title_bold, "bold", "plain")
      ),
      plot.subtitle     = element_text(  
        hjust  = 0, 
        size   = strip_text_size,
        face   = ifelse(strip_text_bold, "bold", "plain"),
        margin = margin(b = 5)
      )
    )
  
  # ---------------------------------------------------------------------------
  # SUMMARY TABLE
  # ---------------------------------------------------------------------------
  if (plot_type == "coefficient") {
    if (!is.null(adjust_for) && length(adjust_for) > 0 && nrow(full_results_df) > 0) {
      summary_table <- full_results_df %>%
        select(
          age_group, year, variable, variable_type,
          coefficient, se, p.value, ci_lower, ci_upper
        ) %>%
        arrange(age_group, year, variable_type, variable) %>%
        mutate(across(where(is.numeric), ~ round(., 4)))
      
      plot_summary <- summary_table %>%
        filter(variable_type == "Plotted") %>%
        select(-variable_type)
      
      cat("\nSummary of PLOTTED coefficients (first 20 rows):\n")
      print(head(plot_summary, 20))
      
      cat("\nFull model results including adjustment variables (first 30 rows):\n")
      print(head(summary_table, 30))
      
      return(list(
        plot           = p, 
        plotted_data   = plot_summary,
        full_model_data = summary_table
      ))
      
    } else {
      summary_table <- results_df %>%
        select(age_group, year, variable, value, se, p.value, ci_lower, ci_upper) %>%
        rename(coefficient = value) %>%
        mutate(across(where(is.numeric), ~ round(., 4)))
      
      cat("\nSummary of results (first 20 rows):\n")
      print(head(summary_table, 20))
      
      return(list(
        plot = p,
        data = summary_table
      ))
    }
    
  } else {
    # PREVALENCE / MEAN BRANCH
    if (nrow(results_df) == 0L) {
      stop(
        "No valid data found. This could mean:\n",
        "  - Variables don't exist in the dataset\n",
        "  - All values are NA for the selected variables\n",
        "  - No age-year combinations meet the min_n threshold (", min_n, ")\n",
        "Try: checking variable names, lowering min_n, or filtering years first"
      )
    }
    
    summary_table <- results_df %>%
      select(age_group, year, variable, value, se, ci_lower, ci_upper, type) %>%
      mutate(across(where(is.numeric), ~ round(., 2)))
    
    cat("\nSummary of results (first 20 rows):\n")
    print(head(summary_table, 20))
    
    return(list(
      plot = p,
      data = summary_table
    ))
  }
}
