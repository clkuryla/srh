# Analysis of Self-Rated Health Determinants Across Age Groups Over Time
# This code implements a systematic analysis of how different factors affect 
# self-rated health (SRH) across different age groups and time periods

library(tidyverse)
library(survey)
library(gridExtra)

###########################################
# 1. VARIABLE SELECTION AND PREPARATION
###########################################

# Function to prepare variables for analysis
prepare_variables <- function(data) {
  # Create selected derived variables
  data <- data %>%
    # BMI variable
    mutate(bmi = weight / ((height/100)^2)) %>%
    # Cardiovascular risk composite
    mutate(cv_risk = rowSums(cbind(heart_dis, stroke, angina, mi) * 1, na.rm = TRUE) > 0) %>%
    # Age groups (make sure it exists)
    mutate(age_group = factor(age_group, 
                              levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
                              ordered = TRUE))
  
  # Convert health status to ordered factor
  data <- data %>%
    mutate(
      srh_factor = factor(srh, levels = 1:5, 
                          labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
                          ordered = TRUE)
    )
  
  # Standardize continuous variables
  continuous_vars <- c(
    "bmi", "body_fat_p", "sbp", "dbp", "phq9", "ldl", "hdl",
    "heart_rate", "glu", "hba1c", "chol", "crp"
  )
  
  # Check which variables actually exist in the data
  available_vars <- continuous_vars[continuous_vars %in% names(data)]
  
  # Apply standardization
  for (var in available_vars) {
    # Standardize (center and scale)
    z_var_name <- paste0(var, "_z")
    data[[z_var_name]] <- scale(data[[var]])[,1]
    
    # Also create deviation from optimal for relevant variables
    if (var %in% c("bmi", "sbp", "dbp", "glu", "ldl")) {
      dev_var_name <- paste0(var, "_dev")
      # Define optimal values
      optimal_values <- list(
        bmi = 22,        # Midpoint of normal range
        sbp = 115,       # Optimal systolic BP
        dbp = 75,        # Optimal diastolic BP
        glu = 85,        # Optimal fasting glucose
        ldl = 100        # Optimal LDL
      )
      
      # Calculate deviation from optimal
      data[[dev_var_name]] <- abs(data[[var]] - optimal_values[[var]])
    }
  }
  
  # Create clinical categories for key variables
  if ("bmi" %in% names(data)) {
    data <- data %>%
      mutate(bmi_cat = case_when(
        bmi < 18.5 ~ "Underweight",
        bmi < 25 ~ "Normal",
        bmi < 30 ~ "Overweight",
        TRUE ~ "Obese"
      ))
    
    # Make it a factor with ordered levels
    data$bmi_cat <- factor(data$bmi_cat,
                           levels = c("Underweight", "Normal", "Overweight", "Obese"),
                           ordered = TRUE)
  }
  
  if (all(c("sbp", "dbp") %in% names(data))) {
    data <- data %>%
      mutate(bp_cat = case_when(
        sbp < 120 & dbp < 80 ~ "Normal",
        (sbp >= 120 & sbp < 130) & dbp < 80 ~ "Elevated",
        (sbp >= 130 & sbp < 140) | (dbp >= 80 & dbp < 90) ~ "Stage 1",
        sbp >= 140 | dbp >= 90 ~ "Stage 2",
        sbp > 180 | dbp > 120 ~ "Crisis",
        TRUE ~ NA_character_
      ))
    
    # Make it a factor with ordered levels
    data$bp_cat <- factor(data$bp_cat,
                          levels = c("Normal", "Elevated", "Stage 1", "Stage 2", "Crisis"),
                          ordered = TRUE)
  }
  
  # Create clinical categories for depression if PHQ9 exists
  if ("phq9" %in% names(data)) {
    data <- data %>%
      mutate(phq9_cat = case_when(
        phq9 < 5 ~ "Minimal",
        phq9 < 10 ~ "Mild",
        phq9 < 15 ~ "Moderate",
        phq9 < 20 ~ "Moderately severe",
        TRUE ~ "Severe"
      ))
    
    # Make it a factor with ordered levels
    data$phq9_cat <- factor(data$phq9_cat,
                            levels = c("Minimal", "Mild", "Moderate", "Moderately severe", "Severe"),
                            ordered = TRUE)
  }
  
  # Create a composite comorbidity score
  comorbidity_vars <- c(
    "heart_dis", "stroke", "diabetes", "asthma", "arthritis", 
    "lung_dis", "cancer_e", "chf", "angina", "mi"
  )
  
  # Check which variables actually exist
  available_comorbidity_vars <- comorbidity_vars[comorbidity_vars %in% names(data)]
  
  # Create comorbidity score if we have at least some of the variables
  if (length(available_comorbidity_vars) > 0) {
    data <- data %>%
      mutate(comorbidity_score = rowSums(select(., all_of(available_comorbidity_vars)), na.rm = TRUE))
  }
  
  # Age-normalize functional measures
  if ("grip_str_max" %in% names(data)) {
    # Group by age_group and sex, then calculate z-scores within groups
    data <- data %>%
      group_by(age_group, sex) %>%
      mutate(grip_str_z = scale(grip_str_max)[,1]) %>%
      ungroup()
  }
  
  return(data)
}

###########################################
# 2. SURVEY DESIGN AND ANALYSIS
###########################################

# Create survey design with prepared variables
create_survey_design <- function(data, use_mec_weights = TRUE) {
  # Prepare variables first
  data_prep <- prepare_variables(data)
  
  # Determine weights to use
  weights_var <- if(use_mec_weights) "WTMEC2YR" else "WTINT2YR"
  
  # Check if weights variable exists, otherwise use dem_wghts
  if (!weights_var %in% names(data_prep) && "dem_wghts" %in% names(data_prep)) {
    weights_var <- "dem_wghts"
  }
  
  # Check if PSU and strata variables exist
  has_psu <- "SDMVPSU" %in% names(data_prep)
  has_strata <- "SDMVSTRA" %in% names(data_prep)
  
  # Create design based on available variables
  if (has_psu && has_strata) {
    svy_design <- data_prep %>%
      filter(!(is.na(SDMVPSU))) %>% 
      as_survey_design(
        ids = SDMVPSU,
        weights = !!sym(weights_var),
        strata = SDMVSTRA,
        nest = TRUE
      )
  } else if (has_psu) {
    svy_design <- data_prep %>%
      filter(!(is.na(SDMVPSU))) %>% 
      as_survey_design(
        ids = SDMVPSU,
        weights = !!sym(weights_var),
        nest = TRUE
      )
  } else {
    # If no PSU/strata, use simple design
    svy_design <- data_prep %>%
      as_survey_design(
        ids = 1,
        weights = !!sym(weights_var),
        nest = TRUE
      )
  }
  
  return(svy_design)
}

###########################################
# 3. KEY VARIABLE SELECTION
###########################################

# Function to select key predictor variables based on available data
select_key_predictors <- function(data) {
  # Define groups of variables by domain
  domains <- list(
    # Binary health conditions
    conditions = c("heart_dis", "stroke", "diabetes", "asthma", "arthritis", "lung_dis", "cancer_e"),
    
    # Clinical biomarkers
    biomarkers = c("bmi", "body_fat_p", "sbp", "dbp", "glu", "hba1c", "chol", "hdl", "ldl", "crp"),
    
    # Mental health
    mental = c("phq9", "ds_score"),
    
    # Functional status
    functional = c("grip_str_max", "fev1"),
    
    # Lifestyle/behaviors
    lifestyle = c("smoking", "met_month"),
    
    # Socioeconomic 
    socioeconomic = c("educ", "pir", "income_h")
  )
  
  # For each domain, find available variables
  available_predictors <- list()
  
  for (domain_name in names(domains)) {
    domain_vars <- domains[[domain_name]]
    available <- domain_vars[domain_vars %in% names(data)]
    
    if (length(available) > 0) {
      available_predictors[[domain_name]] <- available
    }
  }
  
  # Flatten list to get final variable selection
  selected_variables <- unlist(available_predictors)
  
  # Add derived variables that we created
  derived_vars <- c(
    # Standardized versions
    paste0(unlist(domains$biomarkers), "_z"),
    
    # Deviation versions
    paste0(c("bmi", "sbp", "dbp", "glu", "ldl"), "_dev"),
    
    # Categorical versions
    "bmi_cat", "bp_cat", "phq9_cat",
    
    # Composite scores
    "comorbidity_score", "cv_risk"
  )
  
  # Check which derived variables actually exist
  available_derived <- derived_vars[derived_vars %in% names(data)]
  
  # Combine original and derived variables
  all_selected_vars <- c(selected_variables, available_derived)
  
  return(all_selected_vars)
}

###########################################
# 4. ANALYSIS FUNCTIONS
###########################################

# Function to analyze relationship between SRH and selected predictors
analyze_srh_predictors <- function(survey_design, predictors, 
                                   by_year = TRUE, by_age = TRUE,
                                   control_vars = NULL) {
  
  # Initialize results list
  results_list <- list()
  
  # Determine grouping variables
  group_vars <- c()
  if (by_year) group_vars <- c(group_vars, "year")
  if (by_age) group_vars <- c(group_vars, "age_group")
  
  # Loop through predictors and analyze
  for (predictor in predictors) {
    # Create base formula
    if (is.null(control_vars)) {
      formula_str <- paste("srh ~", predictor)
    } else {
      formula_str <- paste("srh ~", predictor, "+", 
                           paste(control_vars, collapse = " + "))
    }
    
    # Convert to formula
    f <- as.formula(formula_str)
    
    # Run analysis
    tryCatch({
      # Check if predictor is categorical
      is_categorical <- is.factor(survey_design$variables[[predictor]]) || 
        is.character(survey_design$variables[[predictor]])
      
      # For categorical predictors, we need a different approach
      if (is_categorical) {
        # Run model overall to get reference for categorical variables
        overall_model <- svyglm(formula = f, design = survey_design, family = gaussian())
        predictor_coefs <- coef(summary(overall_model))
        
        # Extract all coefficients related to the predictor
        predictor_terms <- grep(paste0("^", predictor), rownames(predictor_coefs), value = TRUE)
        
        # Store results for each level
        level_results <- data.frame()
        
        for (term in predictor_terms) {
          level_results <- rbind(level_results, data.frame(
            predictor = predictor,
            level = gsub(paste0("^", predictor), "", term),
            coefficient = predictor_coefs[term, "Estimate"],
            se = predictor_coefs[term, "Std. Error"],
            p_value = predictor_coefs[term, "Pr(>|t|)"]
          ))
        }
        
        # Add to results list
        results_list[[predictor]] <- level_results
        
      } else {
        # For continuous predictors, analyze by groups
        if (length(group_vars) > 0) {
          # If grouping variables specified, analyze by groups
          grouped_results <- survey_design %>%
            group_by(!!!syms(group_vars)) %>%
            summarize(
              model = list(svyglm(f, design = .)),
              .groups = "drop"
            )
          
          # Extract coefficients from models
          model_results <- grouped_results %>%
            mutate(
              coef_data = map(model, function(m) {
                if (is.null(m)) return(NULL)
                
                summ <- summary(m)
                coefs <- coef(summ)
                
                # Check if predictor is in coefficients
                if (!predictor %in% rownames(coefs)) {
                  return(NULL)
                }
                
                data.frame(
                  predictor = predictor,
                  coefficient = coefs[predictor, "Estimate"],
                  se = coefs[predictor, "Std. Error"],
                  t_value = coefs[predictor, "t value"],
                  p_value = coefs[predictor, "Pr(>|t|)"]
                )
              })
            ) %>%
            select(-model) %>%
            unnest(coef_data)
          
          # Add to results list
          results_list[[predictor]] <- model_results
          
        } else {
          # If no grouping, just analyze overall
          overall_model <- svyglm(formula = f, design = survey_design, family = gaussian())
          summ <- summary(overall_model)
          coefs <- coef(summ)
          
          # Check if predictor is in coefficients
          if (predictor %in% rownames(coefs)) {
            results_list[[predictor]] <- data.frame(
              predictor = predictor,
              coefficient = coefs[predictor, "Estimate"],
              se = coefs[predictor, "Std. Error"],
              t_value = coefs[predictor, "t value"],
              p_value = coefs[predictor, "Pr(>|t|)"]
            )
          }
        }
      }
    }, error = function(e) {
      # If error, just skip this predictor
      message(paste("Error analyzing predictor:", predictor, "-", e$message))
    })
  }
  
  # Combine all results
  all_results <- bind_rows(results_list, .id = "variable")
  
  return(all_results)
}

###########################################
# 5. VISUALIZATION FUNCTIONS
###########################################

# Function to plot predictor importance by age group over time
plot_predictor_importance <- function(results, 
                                      compare_type = "predictors",
                                      standardize = TRUE,
                                      facet_scales = "free_y") {
  
  # Check if required grouping variables are present
  has_year <- "year" %in% names(results)
  has_age <- "age_group" %in% names(results)
  
  if (!has_year || !has_age) {
    stop("Results must contain both 'year' and 'age_group' variables")
  }
  
  # Prepare data for plotting
  plot_data <- results %>%
    filter(!is.na(coefficient))
  
  # Standardize coefficients if requested
  if (standardize) {
    plot_data <- plot_data %>%
      group_by(variable) %>%
      mutate(
        scaled_coef = coefficient / max(abs(coefficient), na.rm = TRUE),
        scaled_se = se / max(abs(coefficient), na.rm = TRUE)
      ) %>%
      ungroup()
    
    y_var <- "scaled_coef"
    error_min <- "scaled_coef - scaled_se"
    error_max <- "scaled_coef + scaled_se"
    y_label <- "Standardized Coefficient"
  } else {
    y_var <- "coefficient"
    error_min <- "coefficient - se"
    error_max <- "coefficient + se"
    y_label <- "Coefficient (Association with SRH)"
  }
  
  # Add significance indicators
  plot_data <- plot_data %>%
    mutate(sig_level = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ))
  
  # Create appropriate plot based on comparison type
  if (compare_type == "predictors") {
    # Compare different predictors within each age group
    p <- ggplot(plot_data, 
                aes(x = year, y = !!sym(y_var), 
                    color = variable, group = variable)) +
      geom_line() +
      geom_point(aes(shape = sig_level != "")) +
      geom_errorbar(
        aes(ymin = !!sym(error_min), ymax = !!sym(error_max)),
        width = 0.2, alpha = 0.5
      ) +
      facet_wrap(~ age_group, scales = facet_scales) +
      labs(
        title = "Changes in Determinants of Self-Rated Health Over Time",
        subtitle = "By Age Group",
        x = "Year",
        y = y_label,
        color = "Predictor",
        shape = "Significant"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "lightgrey"),
        strip.text = element_text(face = "bold")
      )
  } else if (compare_type == "age_groups") {
    # Compare different age groups for each predictor
    p <- ggplot(plot_data, 
                aes(x = year, y = !!sym(y_var), 
                    color = age_group, group = age_group)) +
      geom_line() +
      geom_point(aes(shape = sig_level != "")) +
      geom_errorbar(
        aes(ymin = !!sym(error_min), ymax = !!sym(error_max)),
        width = 0.2, alpha = 0.5
      ) +
      facet_wrap(~ variable, scales = facet_scales) +
      labs(
        title = "Changes in Determinants of Self-Rated Health Over Time",
        subtitle = "By Predictor",
        x = "Year",
        y = y_label,
        color = "Age Group",
        shape = "Significant"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "lightgrey"),
        strip.text = element_text(face = "bold")
      )
  }
  
  return(p)
}

# Function to test if temporal changes in predictor importance are significant
test_temporal_changes <- function(results) {
  
  # Check if required variables are present
  required_vars <- c("variable", "year", "age_group", "coefficient", "se")
  missing_vars <- setdiff(required_vars, names(results))
  
  if (length(missing_vars) > 0) {
    stop(paste("Results missing required variables:", 
               paste(missing_vars, collapse = ", ")))
  }
  
  # Prepare results data frame
  trend_results <- results %>%
    group_by(variable, age_group) %>%
    filter(n() >= 3) %>%  # Need at least 3 time points
    group_map(~ {
      # Calculate weights (inverse variance)
      weights <- 1 / (.x$se^2)
      
      # Fit weighted linear regression
      trend_model <- tryCatch({
        lm(coefficient ~ year, data = .x, weights = weights)
      }, error = function(e) {
        return(NULL)
      })
      
      # Extract results if model successful
      if (!is.null(trend_model)) {
        mod_summary <- summary(trend_model)
        
        # Create result row
        data.frame(
          variable = unique(.x$variable),
          age_group = unique(.x$age_group),
          trend_slope = coef(mod_summary)[2, 1],
          trend_se = coef(mod_summary)[2, 2],
          trend_p = coef(mod_summary)[2, 4],
          is_significant = coef(mod_summary)[2, 4] < 0.05
        )
      } else {
        NULL
      }
    }) %>%
    bind_rows()
  
  return(trend_results)
}

# Function to compare the relative importance of different domains
compare_domain_importance <- function(results) {
  # Define domains and their variables
  domains <- list(
    "Health Conditions" = c("heart_dis", "stroke", "diabetes", "asthma", 
                            "arthritis", "lung_dis", "cancer_e", "comorbidity_score",
                            "cv_risk"),
    "Biomarkers" = c("bmi", "body_fat_p", "sbp", "dbp", "glu", "hba1c", 
                     "chol", "hdl", "ldl", "crp", 
                     "bmi_z", "sbp_z", "dbp_z", "bmi_dev", "sbp_dev", "dbp_dev"),
    "Mental Health" = c("phq9", "ds_score", "phq9_z"),
    "Functional Status" = c("grip_str_max", "fev1", "grip_str_z"),
    "Lifestyle" = c("smoking", "met_month"),
    "Socioeconomic" = c("educ", "pir", "income_h")
  )
  
  # Assign domain to each variable
  results_with_domain <- results %>%
    mutate(domain = case_when(
      variable %in% domains[["Health Conditions"]] ~ "Health Conditions",
      variable %in% domains[["Biomarkers"]] ~ "Biomarkers",
      variable %in% domains[["Mental Health"]] ~ "Mental Health",
      variable %in% domains[["Functional Status"]] ~ "Functional Status",
      variable %in% domains[["Lifestyle"]] ~ "Lifestyle",
      variable %in% domains[["Socioeconomic"]] ~ "Socioeconomic",
      TRUE ~ "Other"
    ))
  
  # Calculate average absolute coefficient by domain, age group, and year
  domain_importance <- results_with_domain %>%
    filter(!is.na(coefficient)) %>%
    group_by(domain, age_group, year) %>%
    summarize(
      mean_abs_coef = mean(abs(coefficient), na.rm = TRUE),
      count = n(),
      significant_count = sum(p_value < 0.05, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Only include domains with at least 2 variables
    filter(count >= 2)
  
  # Calculate proportion of significant predictors
  domain_importance <- domain_importance %>%
    mutate(prop_significant = significant_count / count)
  
  # Plot domain importance by age group over time
  p <- ggplot(domain_importance, 
              aes(x = year, y = mean_abs_coef, color = domain, group = domain)) +
    geom_line(size = 1) +
    geom_point(aes(size = prop_significant)) +
    facet_wrap(~ age_group) +
    labs(
      title = "Relative Importance of Different Domains for Self-Rated Health",
      subtitle = "Size of point indicates proportion of significant predictors",
      x = "Year",
      y = "Mean Absolute Coefficient",
      color = "Domain",
      size = "Proportion Significant"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "lightgrey"),
      strip.text = element_text(face = "bold")
    )
  
  return(p)
}

###########################################
# 6. SAMPLE ANALYSIS WORKFLOW
###########################################

# Example workflow
run_srh_analysis <- function(data_nhanes) {
  # 1. Create survey design with prepared variables
  svy_design <- create_survey_design(data_nhanes)
  
  # 2. Select key predictors based on available data
  predictors <- select_key_predictors(svy_design$variables)
  
  # 3. Run analysis of SRH predictors by age and year
  results <- analyze_srh_predictors(svy_design, predictors)
  
  # 4. Visualize changes in predictor importance over time
  # Compare predictors within each age group
  p1 <- plot_predictor_importance(results, compare_type = "predictors")
  
  # Compare age groups for each predictor
  p2 <- plot_predictor_importance(results, compare_type = "age_groups")
  
  # 5. Test if temporal changes are significant
  trend_results <- test_temporal_changes(results)
  
  # 6. Compare domain importance
  p3 <- compare_domain_importance(results)
  
  # Return results
  return(list(
    survey_design = svy_design,
    predictors = predictors,
    results = results,
    trend_results = trend_results,
    plots = list(
      predictors_by_age = p1,
      age_groups_by_predictor = p2,
      domain_importance = p3
    )
  ))
}

# Example usage:
analysis_results <- run_srh_analysis(data_nhanes)
# 
# # View significant temporal trends
significant_trends <- analysis_results$trend_results %>%
  filter(is_significant) %>%
  arrange(variable, age_group)
# 
# # Display plots
analysis_results$plots$predictors_by_age
analysis_results$plots$age_groups_by_predictor
analysis_results$plots$domain_importance




analysis_results <- run_srh_analysis(data_nhanes)
# 
# # View significant temporal trends
significant_trends <- analysis_results$trend_results %>%
  filter(is_significant) %>%
  arrange(variable, age_group)
# 
# # Display plots
analysis_results$plots$predictors_by_age
analysis_results$plots$age_groups_by_predictor
analysis_results$plots$domain_importance




run_srh_analysis <- function(data_nhanes) {
  # 1. Create survey design with prepared variables
  svy_design <- create_survey_design(data_nhanes)
  
  # 2. Select key predictors based on available data
  predictors <- select_key_predictors(svy_design$variables)
  
  # 3. Run analysis of SRH predictors by age and year
  results <- analyze_srh_predictors(svy_design, predictors)
  
  # 4. Visualize changes in predictor importance over time
  # Compare predictors within each age group
  p1 <- plot_predictor_importance(results, compare_type = "predictors")
  
  # Compare age groups for each predictor
  p2 <- plot_predictor_importance(results, compare_type = "age_groups")
  
  # 5. Test if temporal changes are significant
  trend_results <- test_temporal_changes(results)
  
  # 6. Compare domain importance
  p3 <- compare_domain_importance(results)
  
  # Return results
  return(list(
    survey_design = svy_design,
    predictors = predictors,
    results = results,
    trend_results = trend_results,
    plots = list(
      predictors_by_age = p1,
      age_groups_by_predictor = p2,
      domain_importance = p3
    )
  ))
}