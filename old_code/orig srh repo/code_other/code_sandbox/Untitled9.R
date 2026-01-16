###############################################################################
# 0. Load Required Packages
###############################################################################

library(tidyverse)
library(survey)       # For complex survey design objects and survey GLMs
library(broom)        # For tidying model output (if desired)
library(rlang)        # For tidy evaluation within functions

###############################################################################
# 1. Example Data and Survey Design Setup
###############################################################################

# Below, we assume you already have a data frame called `data_nhanes` 
# which includes relevant columns like `SDMVPSU`, `WTINT2YR`, `SDMVSTRA`, 
# `heart_dis`, `stroke`, `srh`, `year`, etc.
# We'll demonstrate with placeholders to show the structure:

# Example minimal data generation (COMMENT OUT in real usage):
# set.seed(123)
# data_nhanes <- tibble(
#   SDMVPSU   = sample(1:30, 300, replace = TRUE),
#   WTINT2YR  = runif(300, 0.5, 2),
#   SDMVSTRA  = sample(1:10, 300, replace = TRUE),
#   release_nb = sample(2011:2018, 300, replace = TRUE),
#   heart_dis = rbinom(300, 1, 0.15),
#   stroke    = rbinom(300, 1, 0.05),
#   srh       = rnorm(300, 3, 1),  # Self-rated health, e.g. 1=Poor, 5=Excellent
#   age_group = sample(c("18-29", "30-44", "45-64", "65+"), 300, replace = TRUE)
# )

# Create the main survey design object:
svy_nhanes <- data_nhanes %>%
  filter(!is.na(SDMVPSU)) %>% 
  as_survey_design(
    ids     = SDMVPSU,    # PSU identifiers
    weights = WTINT2YR,   # Interview weights (choose WTMEC2YR if you need MEC weights)
    strata  = SDMVSTRA,
    nest    = TRUE
  )

# For demonstration, we show a different weighting design (if you need it):
# (You mentioned `dem_wghts` in your original script; adapt as needed)
# svy_nhanes_k <- data_nhanes %>% 
#   as_survey_design(
#     ids     = 1,
#     weights = dem_wghts
#   )

# Also add or rename your `year` variable if needed:
# data_nhanes <- data_nhanes %>% mutate(year = release_nb)

###############################################################################
# 2. Simple Survey GLM Examples
###############################################################################

# Example 2a: Quasibinomial for a binary outcome:
svyglm(
  formula = heart_dis ~ srh,
  design  = svy_nhanes,
  family  = quasibinomial()
) %>%
  summary()

# Example 2b: Gaussian for a continuous outcome:
svyglm(
  formula = srh ~ heart_dis,
  design  = svy_nhanes,
  family  = gaussian()
) %>%
  summary()

###############################################################################
# 3. General Function to Summarize "srh ~ X" by a Single Grouping Variable
###############################################################################

# This function:
#   - Takes a formula like srh ~ heart_dis
#   - Restricts to non-missing observations in those variables
#   - Groups by one variable (e.g., year, age_group)
#   - For each group, fits a survey GLM using the designated family
#   - Extracts coefficient, standard error, t-statistic, and p-value 
#     for the predictor of interest
#   - Returns a tibble of results

srh_vs_X_results <- function(formula,
                             design        = svy_nhanes,
                             family        = stats::gaussian(),
                             group_by_1) {
  
  # Convert string or formula input to a formula (if character)
  f <- as.formula(formula)
  
  # Extract variable names from the formula for filtering
  vars_in_formula <- all.vars(f)
  var_1 <- vars_in_formula[1]  # Response
  var_2 <- vars_in_formula[2]  # Predictor
  
  var_1_sym <- sym(var_1)
  var_2_sym <- sym(var_2)
  
  # 1. Filter out missing values for the main variables
  # 2. Group by the specified grouping variable
  # 3. Within each group, fit the model and extract the relevant coefficient
  model_results <- design %>%
    filter(!is.na(!!var_1_sym), !is.na(!!var_2_sym)) %>%
    group_by({{group_by_1}}) %>% 
    group_map_dfr(~ {
      
      # If there's no variation in the response, return NA
      if (length(unique(.x$variables[[var_1]])) < 2) {
        return(tibble(
          group_1     = unique(.x$variables[[as_name(enquo(group_by_1))]]),
          formula     = as.character(formula),
          coefficient = NA_real_,
          se          = NA_real_,
          t_value     = NA_real_,
          p_value     = NA_real_
        ))
      }
      
      # Fit the model for the subset
      model_var <- svyglm(formula = f, design = .x, family = family)
      coefs <- summary(model_var)$coefficients
      
      # If the predictor row doesn't exist in the summary, return NA
      if (!var_2 %in% rownames(coefs)) {
        return(tibble(
          group_1     = unique(.x$variables[[as_name(enquo(group_by_1))]]),
          formula     = as.character(formula),
          coefficient = NA_real_,
          se          = NA_real_,
          t_value     = NA_real_,
          p_value     = NA_real_
        ))
      }
      
      # Otherwise, return the coefficient row for var_2
      tibble(
        group_1     = unique(.x$variables[[as_name(enquo(group_by_1))]]),
        formula     = as.character(formula),
        coefficient = coefs[var_2, "Estimate"],
        se          = coefs[var_2, "Std. Error"],
        t_value     = coefs[var_2, "t value"],
        p_value     = coefs[var_2, "Pr(>|t|)"]
      )
      
    }) %>%
    ungroup()
  
  model_results
}

###############################################################################
# 4. Corresponding Plot Function (One Grouping Variable)
###############################################################################

# This function:
#   - Takes the output of srh_vs_X_results()
#   - Creates a simple line+point plot of coefficient by group
#   - Adds error bars for +/- 1 SE

srh_vs_X_plot <- function(model_results, title = NULL) {
  group_by_1    <- colnames(model_results)[1]   # e.g. "year" or "age_group"
  group_by_1_sym <- sym(group_by_1)
  if (is.null(title)) title <- unique(model_results$formula)
  
  ggplot(model_results, aes(x = !!group_by_1_sym, y = coefficient)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = coefficient - se, ymax = coefficient + se),
                  width = 0.2) +
    labs(x = group_by_1, y = "Coefficient", title = title) +
    theme_minimal()
}

###############################################################################
# 5. Test the Single-Group Functions
###############################################################################

# Example: Relationship between srh and stroke, grouped by year
srh_vs_X_results_data <- srh_vs_X_results(
  formula = srh ~ stroke,
  design  = svy_nhanes,
  family  = gaussian(),
  group_by_1 = year
)

# Now plot
srh_vs_X_plot(srh_vs_X_results_data)

###############################################################################
# 6. General Function for Two Grouping Variables
###############################################################################

# Similar concept as srh_vs_X_results(), but we allow grouping by two variables 
# (e.g. year and age_group).

srh_vs_X_per_Y_results <- function(formula,
                                   design   = svy_nhanes,
                                   family   = gaussian(),
                                   group_by_1,
                                   group_by_2) {
  
  f <- as.formula(formula)
  vars_in_formula <- all.vars(f)
  var_1 <- vars_in_formula[1]  # response
  var_2 <- vars_in_formula[2]  # predictor
  
  var_1_sym <- sym(var_1)
  var_2_sym <- sym(var_2)
  
  model_results <- design %>%
    filter(!is.na(!!var_1_sym), !is.na(!!var_2_sym)) %>%
    group_by({{group_by_1}}, {{group_by_2}}) %>%
    group_map_dfr(~ {
      
      # If there's no variation in the response for that subset, return NA
      if (length(unique(.x$variables[[var_1]])) < 2) {
        return(tibble(
          group_1     = unique(.x$variables[[as_name(enquo(group_by_1))]]),
          group_2     = unique(.x$variables[[as_name(enquo(group_by_2))]]),
          formula     = as.character(formula),
          coefficient = NA_real_,
          se          = NA_real_,
          t_value     = NA_real_,
          p_value     = NA_real_
        ))
      }
      
      # Fit the model
      model_var <- svyglm(formula = f, design = .x, family = family)
      coefs <- summary(model_var)$coefficients
      
      if (!var_2 %in% rownames(coefs)) {
        return(tibble(
          group_1     = unique(.x$variables[[as_name(enquo(group_by_1))]]),
          group_2     = unique(.x$variables[[as_name(enquo(group_by_2))]]),
          formula     = as.character(formula),
          coefficient = NA_real_,
          se          = NA_real_,
          t_value     = NA_real_,
          p_value     = NA_real_
        ))
      }
      
      tibble(
        group_1     = unique(.x$variables[[as_name(enquo(group_by_1))]]),
        group_2     = unique(.x$variables[[as_name(enquo(group_by_2))]]),
        formula     = as.character(formula),
        coefficient = coefs[var_2, "Estimate"],
        se          = coefs[var_2, "Std. Error"],
        t_value     = coefs[var_2, "t value"],
        p_value     = coefs[var_2, "Pr(>|t|)"]
      )
    }) %>%
    ungroup()
  
  model_results
}

###############################################################################
# 7. Corresponding Plot Function (Two Grouping Variables)
###############################################################################

srh_vs_X_per_Y_plot <- function(model_results, title = NULL) {
  # The first column is group_1, the second is group_2
  group_by_1    <- colnames(model_results)[1]
  group_by_2    <- colnames(model_results)[2]
  group_by_1_sym <- sym(group_by_1)
  group_by_2_sym <- sym(group_by_2)
  
  if (is.null(title)) title <- unique(model_results$formula)
  
  ggplot(model_results, 
         aes(x = !!group_by_1_sym, y = coefficient, color = !!group_by_2_sym)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = coefficient - se, ymax = coefficient + se),
                  width = 0.2) +
    labs(
      x     = group_by_1,
      y     = "Coefficient",
      color = group_by_2,
      title = title
    ) +
    theme_minimal()
}

###############################################################################
# 8. Test the Two-Group Functions
###############################################################################

srh_vs_X_per_Y_results_data <- srh_vs_X_per_Y_results(
  formula     = srh ~ lung_dis,
  design      = svy_nhanes,
 # family      = gaussian(),
#  family      = quasibinomial(),
  group_by_1  = year,
  group_by_2  = age_group
)

srh_vs_X_per_Y_plot(srh_vs_X_per_Y_results_data)

###############################################################################
# 9. Utility "Plot-Only" Helper (Single Grouping)
###############################################################################

# If you only want a quick function for "srh ~ X" by some grouping variable
# and to directly get the plot:

plot_var_vs_srh_per_something <- function(X, group_by_1, 
                                          design = svy_nhanes, 
                                          family = gaussian()) {
  group_by_1_sym <- sym(group_by_1)
  
  # Build formula as srh ~ X
  frm <- as.formula(paste0("srh ~ ", X))
  
  # Get the model results, then plot
  model_results <- srh_vs_X_results(frm, design, family, !!group_by_1_sym)
  srh_vs_X_plot(model_results)
}

# Quick test:
plot_var_vs_srh_per_something("stroke", "age_group")

###############################################################################
# 10. Notes on Fit Checks, Sensitivity, and Limitations
###############################################################################

# A. MODEL FIT AND DIAGNOSTICS
#    - For GLM diagnostics, consider using residual plots, 
#      e.g., `residuals(model_var, type="pearson")` or 
#      `residuals(model_var, type="deviance")`, 
#      though these are less straightforward with survey designs.
#    - The 'survey' package also provides svyglm diagnostics, 
#      but residual-based checks can be trickier for complex designs.
#    - Consider checking the distribution (binomial, Poisson, Gaussian) 
#      assumptions or quasi-likelihood if data are overdispersed.

# B. SENSITIVITY ANALYSES
#    - Try different functional forms (e.g., polynomial terms for age).
#    - Include/exclude certain subgroups to see if results are robust.
#    - Switch between interview weight (WTINT2YR) and MEC weight (WTMEC2YR) 
#      if that changes the sample or adjusts for different aspects of design.

# C. SANITY CHECKS
#    - Ensure enough variation in both the predictor (X) and outcome (srh)
#      across groups (which the code partially checks).
#    - Confirm the group variable(s) are not too granular, causing groups
#      with very small sample sizes or no variation.

# D. LIMITATIONS & ASSUMPTIONS
#    - Survey weighting relies on correct specification of strata, PSU, 
#      and weights. Using the correct NHANES weights for your specific 
#      analyses is crucial for unbiased estimates.
#    - The code shown is for unadjusted models (srh ~ X). If you want 
#      covariate adjustment, modify the formula accordingly 
#      (e.g., srh ~ X + age + sex + ...).
#    - The grouping approach (group_by_1, group_by_2) separately fits 
#      a model per group. Interpret differences with caution: they are 
#      not from a single model with interactions, but rather 
#      separate sub-sample fits.

###############################################################################
# 11. Conclusion
###############################################################################

# The above functions demonstrate a flexible and extensible way to:
#    - set up a survey design,
#    - run small formula-based analyses across subgroups,
#    - gather tidy outputs,
#    - and visualize coefficients with error bars.
# You can adapt these functions further for more complex models, 
# e.g. adding multiple covariates, using different families, etc.



analyze_and_plot_by_group(
  formula = srh ~ heart_dis,
  group_by_var1 = "year",
  group_by_var2 = "age_group"
)

# Example usage:
# Compare multiple health conditions' effects on SRH by age group over time
health_predictors <- c("heart_dis", "stroke", "diabetes", "phq9", "asthma", "educ")

results <- compare_srh_predictors(
  covariates = health_predictors
)
# 
# # Visualize temporal trends with age groups as facets
plot_srh_predictors_over_time(results, plot_type = "facet")
# 
# # Visualize with health conditions as facets
plot_srh_predictors_over_time(results, plot_type = "covariate")
# 
# # Test if changes over time are significant
trend_tests <- test_temporal_change(results)
# 
# # Show which predictors have significantly changed over time for each age group
trend_tests %>%
  filter(significant == TRUE) %>%
  arrange(covariate, age_group)
