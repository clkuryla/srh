################################################################################
#                                                                              #
#   AGE-PERIOD-COHORT ANALYSIS OF SELF-RATED HEALTH                           #
#   Comprehensive Analysis Script for NHIS (and adaptable to other surveys)   #
#                                                                              #
#   PRIMARY OUTCOME: Continuous SRH (1-5, where 5 = Excellent)                #
#   SENSITIVITY: Binary Fair/Poor (0/1)                                        #
#                                                                              #
#   Author: [Your Name]                                                        #
#   Date: [Date]                                                               #
#                                                                              #
#   This script implements:                                                    #
#   1. Descriptive visualizations (Lexis surfaces, trajectories)              #
#   2. Survey-weighted descriptive trends                                      #
#   3. HAPC-CCREM (frequentist and Bayesian)                                  #
#   4. Intrinsic Estimator                                                     #
#   5. Sensitivity analyses (including binary outcome)                         #
#   6. Publication-ready tables and figures                                    #
#                                                                              #
################################################################################

# =============================================================================
# SECTION 0: SETUP AND PACKAGES
# =============================================================================

# Install packages if needed (uncomment as necessary)
# install.packages(c("tidyverse", "survey", "lme4", "rstanarm", "broom.mixed",
#                    "ggplot2", "viridis", "patchwork", "gt", "gtsummary",
#                    "Epi", "splines", "scales", "here", "performance"))

library(tidyverse)
library(survey)
library(lme4)
library(rstanarm)
library(broom.mixed)
library(ggplot2)
library(viridis)
library(patchwork)
library(gt)
library(gtsummary)
library(splines)
library(scales)

# Set seed for reproducibility (Bayesian models)
set.seed(271828)

# Set ggplot theme for publication
# theme_set(theme_minimal(base_size = 12) +
#             theme(panel.grid.minor = element_blank(),
#                   legend.position = "bottom",
#                   plot.title = element_text(face = "bold")))

# =============================================================================
# SECTION 1: DATA PREPARATION
# =============================================================================

#' Prepare data for APC analysis
#' 
#' @param df Data frame with columns: age, year, srh, psu, wt, strata
#' @param age_width Width of age groups (default 5 years)
#' @param period_width Width of period groups (default 5 years)
#' @param min_age Minimum age to include (default 25)
#' @param max_age Maximum age to include (default 84)
#' @param srh_already_reversed If TRUE, assumes SRH is already coded 5=Excellent
#' @return Data frame with additional APC variables

prepare_apc_data <- function(df, 
                             age_width = 5, 
                             period_width = 5,
                             min_age = 18, 
                             max_age = 89,
                             srh_already_reversed = FALSE) {
  
  df_apc <- df %>%
    # Filter to analysis sample
    filter(age >= min_age & age <= max_age) %>%
    filter(!is.na(srh) & !is.na(age) & !is.na(year) & !is.na(wt)) %>%
    
    # Create birth cohort (year - age)
    mutate(
      cohort = year - age,
      
      # PRIMARY OUTCOME: Continuous SRH (1-5, 5 = Excellent)
      # If original coding is 1=Excellent, 5=Poor, reverse it
      srh_continuous = if (srh_already_reversed) {
        as.numeric(srh)
      } else {
        6 - as.numeric(srh)  # Reverse: 1->5, 2->4, 3->3, 4->2, 5->1
      },
      
      # SENSITIVITY OUTCOME: Binary Fair/Poor
      # After reversing: 1 and 2 are Fair/Poor (originally 4 and 5)
      srh_fairpoor = as.numeric(srh_continuous <= 2),
      
      # Create grouped variables for modeling
      # Age groups (e.g., 25-29, 30-34, ...)
      age_group = cut(age, 
                      breaks = seq(min_age, max_age + age_width, by = age_width),
                      right = FALSE,
                      labels = paste0(seq(min_age, max_age, by = age_width), "-",
                                      seq(min_age + age_width - 1, max_age + age_width - 1, by = age_width))),
      
      # Period groups (based on actual years in data)
      period_group = cut(year,
                         breaks = seq(floor(min(year)/period_width)*period_width,
                                      ceiling(max(year)/period_width)*period_width + period_width,
                                      by = period_width),
                         right = FALSE),
      
      # Cohort groups (5-year birth cohorts)
      cohort_group = cut(cohort,
                         breaks = seq(floor(min(cohort)/period_width)*period_width,
                                      ceiling(max(cohort)/period_width)*period_width + period_width,
                                      by = period_width),
                         right = FALSE),
      
      # Numeric versions for modeling
      age_centered = age - mean(age),
      age_squared = age_centered^2,
      
      # Create factors for random effects
      period_factor = as.factor(year),
      cohort_factor = as.factor(cohort_group),
      age_factor = as.factor(age_group),
      
      # Strata and PSU as factors
      strata_factor = as.factor(strata),
      psu_factor = as.factor(psu),
      
      # Log weight for Bayesian models (following your colleague's approach)
      ln_wt = log(wt)
    )
  
  # Print summary of data structure
  cat("\n========== DATA SUMMARY ==========\n")
  cat("N observations:", nrow(df_apc), "\n")
  cat("Year range:", min(df_apc$year), "-", max(df_apc$year), "\n")
  cat("Age range:", min(df_apc$age), "-", max(df_apc$age), "\n")
  cat("Cohort range:", min(df_apc$cohort), "-", max(df_apc$cohort), "\n")
  cat("Number of periods (years):", n_distinct(df_apc$year), "\n")
  cat("Number of period groups:", n_distinct(df_apc$period_group), "\n")
  cat("Number of cohort groups:", n_distinct(df_apc$cohort_group), "\n")
  cat("Number of age groups:", n_distinct(df_apc$age_group), "\n")
  cat("\nSRH CODING (after preparation):\n")
  cat("  Continuous SRH: 1 (Poor) to 5 (Excellent)\n")
  cat("  Mean SRH:", round(mean(df_apc$srh_continuous), 2), "\n")
  cat("  SD SRH:", round(sd(df_apc$srh_continuous), 2), "\n")
  cat("  % Fair/Poor:", round(mean(df_apc$srh_fairpoor) * 100, 1), "%\n")
  cat("==================================\n\n")
  
  return(df_apc)
}

# -----------------------------------------------------------------------------
# Apply data preparation
# -----------------------------------------------------------------------------

# Example with simulated data structure (REMOVE THIS AND USE YOUR ACTUAL DATA)
example_structure <- tribble(
  ~age, ~year, ~srh, ~psu, ~wt, ~strata,
  45,   2010,  2,    101,  1.5, 1,
  32,   2015,  3,    102,  2.1, 2
)
cat("Expected data structure:\n")
print(example_structure)
cat("\nNote: If your SRH is coded 1=Excellent to 5=Poor, set srh_already_reversed=FALSE\n")
cat("      If your SRH is coded 1=Poor to 5=Excellent, set srh_already_reversed=TRUE\n")

# =============================================================================
# SECTION 2: CREATE SURVEY DESIGN OBJECT
# =============================================================================

#' Create survey design object
#' 
#' @param df_apc Prepared APC data frame
#' @return svydesign object

create_survey_design <- function(df_apc) {
  
  # Check for single-PSU strata and set options
  options(survey.lonely.psu = "adjust")  # or "certainty" or "remove"
  
  svy_design <- svydesign(
    id = ~psu,           # Primary sampling unit
    strata = ~strata,    # Stratification variable
    weights = ~wt,       # Survey weights
    data = df_apc,
    nest = TRUE          # PSUs nested within strata
  )
  
  cat("Survey design created successfully.\n")
  cat("Sum of weights:", sum(weights(svy_design)), "\n")
  
  return(svy_design)
}

# =============================================================================
# SECTION 3: DESCRIPTIVE VISUALIZATIONS
# =============================================================================

# -----------------------------------------------------------------------------
# 3.1 Lexis Surface (Heatmap of SRH by Age and Period)
# -----------------------------------------------------------------------------

#' Create Lexis surface heatmap
#' 
#' RATIONALE: Lexis surfaces show the outcome across the age-period plane,
#' with diagonal patterns suggesting cohort effects. This is the essential
#' first step before any APC modeling.
#' 
#' WHAT IT SHOWS:
#' - Horizontal patterns = period effects (all ages affected similarly in a year)
#' - Vertical patterns = age effects (consistent age gradient across years)
#' - Diagonal patterns = cohort effects (birth cohorts carry effects through time)
#'
#' INTERPRETATION FOR CONTINUOUS SRH:
#' - Higher values (yellower) = better health
#' - Lower values (darker) = worse health

create_lexis_surface <- function(df_apc, svy_design, outcome = "srh_continuous") {
  
  # Calculate survey-weighted means by age group and year
  formula_str <- as.formula(paste0("~", outcome))
  
  lexis_data <- svyby(formula_str, 
                      ~age_group + year, 
                      svy_design, 
                      svymean, 
                      na.rm = TRUE) %>%
    as_tibble() %>%
    rename(mean_outcome = all_of(outcome),
           se_outcome = paste0("se.", outcome))
  
  # Extract numeric age midpoint for plotting
  lexis_data <- lexis_data %>%
    mutate(age_mid = as.numeric(gsub("-.*", "", as.character(age_group))) + 2.5)
  
  # Create Lexis surface plot
  p_lexis <- ggplot(lexis_data, aes(x = year, y = age_mid, fill = mean_outcome)) +
    geom_tile() +
    scale_fill_viridis(option = "plasma", 
                       name = "Mean SRH\n(5=Excellent)",
                       limits = c(1, 5)) +
    # Add cohort diagonals
    geom_abline(intercept = seq(-2000, -1900, by = 10), 
                slope = 1, 
                color = "white", 
                alpha = 0.3, 
                linetype = "dashed") +
    labs(
      title = "Lexis Surface: Self-Rated Health by Age and Period",
      subtitle = "Diagonal lines indicate birth cohorts; higher values = better health",
      x = "Survey Year (Period)",
      y = "Age",
      caption = "Source: NHIS. Survey-weighted estimates. SRH: 1=Poor to 5=Excellent."
    ) +
    theme(legend.position = "right") +
    coord_fixed(ratio = 0.5)
  
  return(list(plot = p_lexis, data = lexis_data))
}

# -----------------------------------------------------------------------------
# 3.2 Age-Specific Trends Over Time
# -----------------------------------------------------------------------------

#' Create age-specific trend plots
#' 
#' RATIONALE: Shows how the outcome changes over time (periods) within
#' each age group. Parallel trends suggest pure period effects;
#' converging/diverging trends suggest age-period interactions.
#' 
#' WHAT IT SHOWS FOR CONTINUOUS SRH:
#' - Lines moving up = health improving
#' - Converging lines = age gradient flattening (your hypothesis!)
#' - Specifically: if older age groups improve more, lines converge

create_age_specific_trends <- function(df_apc, svy_design, outcome = "srh_continuous") {
  
  formula_str <- as.formula(paste0("~", outcome))
  
  trend_data <- svyby(formula_str, 
                      ~age_group + year, 
                      svy_design, 
                      svymean, 
                      na.rm = TRUE) %>%
    as_tibble() %>%
    rename(mean_outcome = all_of(outcome),
           se_outcome = paste0("se.", outcome)) %>%
    mutate(
      ci_lower = mean_outcome - 1.96 * se_outcome,
      ci_upper = mean_outcome + 1.96 * se_outcome
    )
  
  # Create line plot
  p_trends <- ggplot(trend_data, aes(x = year, y = mean_outcome, 
                                     color = age_group, group = age_group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = age_group), 
                alpha = 0.1, color = NA) +
    scale_color_viridis_d(option = "turbo", name = "Age Group") +
    scale_fill_viridis_d(option = "turbo", name = "Age Group") +
    scale_y_continuous(limits = c(1, 5)) +
    labs(
      title = "Age-Specific Trends in Self-Rated Health",
      subtitle = "Converging lines indicate flattening of age gradient over time",
      x = "Survey Year",
      y = "Mean Self-Rated Health (1=Poor, 5=Excellent)",
      caption = "Source: NHIS. Survey-weighted estimates with 95% CIs."
    )
  
  return(list(plot = p_trends, data = trend_data))
}

# -----------------------------------------------------------------------------
# 3.3 Cohort Trajectories (Age Profiles by Birth Cohort)
# -----------------------------------------------------------------------------

#' Create cohort trajectory plots
#' 
#' RATIONALE: Shows how each birth cohort's health changes as they age.
#' Different intercepts suggest cohort effects; different slopes suggest
#' cohort-by-age interactions.
#' 
#' WHAT IT SHOWS FOR CONTINUOUS SRH:
#' - Downward slopes = health declines with age (expected)
#' - Higher lines = healthier cohorts
#' - Flatter slopes for younger cohorts = less age-related decline

create_cohort_trajectories <- function(df_apc, svy_design, outcome = "srh_continuous") {
  
  formula_str <- as.formula(paste0("~", outcome))
  
  cohort_data <- svyby(formula_str, 
                       ~cohort_group + age_group, 
                       svy_design, 
                       svymean, 
                       na.rm = TRUE) %>%
    as_tibble() %>%
    rename(mean_outcome = all_of(outcome),
           se_outcome = paste0("se.", outcome)) %>%
    mutate(
      age_mid = as.numeric(gsub("-.*", "", as.character(age_group))) + 2.5,
      cohort_mid = as.numeric(gsub("\\[|,.*", "", as.character(cohort_group))) + 2.5
    ) %>%
    # Filter to cohorts with enough data points
    group_by(cohort_group) %>%
    filter(n() >= 3) %>%
    ungroup()
  
  # Create trajectory plot
  p_cohort <- ggplot(cohort_data, aes(x = age_mid, y = mean_outcome, 
                                      color = cohort_group, group = cohort_group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d(option = "turbo", name = "Birth Cohort") +
    scale_y_continuous(limits = c(1, 5)) +
    labs(
      title = "Cohort Trajectories: Self-Rated Health by Age",
      subtitle = "Each line represents a birth cohort aging through the data",
      x = "Age",
      y = "Mean Self-Rated Health (1=Poor, 5=Excellent)",
      caption = "Source: NHIS. Survey-weighted estimates. Cohorts with ≥3 observations shown."
    )
  
  return(list(plot = p_cohort, data = cohort_data))
}

# -----------------------------------------------------------------------------
# 3.4 Period Trajectories (Age Profiles by Period)
# -----------------------------------------------------------------------------

#' Create period-specific age profiles
#' 
#' RATIONALE: Shows the age gradient in each survey year/period.
#' Changes in the age gradient over time suggest age-period interactions.
#' 
#' WHAT IT SHOWS FOR CONTINUOUS SRH:
#' - Downward slopes = older people report worse health
#' - Flattening slopes over time = convergence (your hypothesis!)

create_period_profiles <- function(df_apc, svy_design, outcome = "srh_continuous") {
  
  formula_str <- as.formula(paste0("~", outcome))
  
  period_data <- svyby(formula_str, 
                       ~year + age_group, 
                       svy_design, 
                       svymean, 
                       na.rm = TRUE) %>%
    as_tibble() %>%
    rename(mean_outcome = all_of(outcome),
           se_outcome = paste0("se.", outcome)) %>%
    mutate(
      age_mid = as.numeric(gsub("-.*", "", as.character(age_group))) + 2.5
    )
  
  # Select subset of years for clarity (e.g., every 5 years)
  years_to_show <- period_data %>%
    distinct(year) %>%
    arrange(year) %>%
    filter(row_number() %% 5 == 1 | row_number() == n()) %>%
    pull(year)
  
  p_period <- period_data %>%
    filter(year %in% years_to_show) %>%
    ggplot(aes(x = age_mid, y = mean_outcome, 
               color = factor(year), group = factor(year))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d(option = "turbo", name = "Survey Year") +
    scale_y_continuous(limits = c(1, 5)) +
    labs(
      title = "Age Profiles by Survey Period",
      subtitle = "Flattening slopes over time indicate convergence across age groups",
      x = "Age",
      y = "Mean Self-Rated Health (1=Poor, 5=Excellent)",
      caption = "Source: NHIS. Survey-weighted estimates. Selected years shown."
    )
  
  return(list(plot = p_period, data = period_data))
}

# -----------------------------------------------------------------------------
# 3.5 Combined Descriptive Figure
# -----------------------------------------------------------------------------

create_combined_descriptive_figure <- function(lexis_result, trends_result, 
                                               cohort_result, period_result) {
  
  combined_plot <- (lexis_result$plot + trends_result$plot) /
    (cohort_result$plot + period_result$plot) +
    plot_annotation(
      title = "Descriptive Age-Period-Cohort Patterns in Self-Rated Health",
      subtitle = "NHIS Data | SRH Scale: 1 (Poor) to 5 (Excellent)",
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )
  
  return(combined_plot)
}

# =============================================================================
# SECTION 4: HAPC-CCREM MODELS (FREQUENTIST) - LINEAR MIXED MODELS
# =============================================================================

#' Fit frequentist HAPC models for continuous outcome
#' 
#' RATIONALE: The HAPC-CCREM treats age as a fixed effect and period/cohort
#' as random effects drawn from normal distributions. For continuous SRH,
#' we use linear mixed models (lmer) instead of logistic.
#' 
#' WHAT THE OUTPUT SHOWS:
#' - Fixed effects: The age-health relationship (linear + quadratic)
#'   - Negative age coefficient = health declines with age
#' - Random effects variances: How much variation is attributable to 
#'   period vs. cohort contexts
#' - BLUPs: Estimated deviations for each specific period/cohort
#' 
#' INTERPRETATION FOR CONTINUOUS SRH:
#' - Positive period effects = that period has better-than-average health
#' - Positive cohort effects = that cohort has better-than-average health

fit_hapc_frequentist <- function(df_apc, outcome = "srh_continuous") {
  
  cat("\n========== FITTING FREQUENTIST HAPC MODELS (LINEAR) ==========\n\n")
  
  # Model 1: Basic HAPC - continuous age
  cat("Fitting Model 1: Basic HAPC (continuous age, linear model)...\n")
  
  formula_basic <- as.formula(paste0(
    outcome, " ~ age_centered + age_squared + (1|period_factor) + (1|cohort_factor)"
  ))
  
  model_basic <- lmer(formula_basic,
                      data = df_apc,
                      REML = TRUE,
                      control = lmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 100000)))
  
  # Model 2: HAPC with age groups as fixed effects
  cat("Fitting Model 2: HAPC with age groups...\n")
  
  formula_agegroup <- as.formula(paste0(
    outcome, " ~ age_factor + (1|period_factor) + (1|cohort_factor)"
  ))
  
  model_agegroup <- lmer(formula_agegroup,
                         data = df_apc,
                         REML = TRUE,
                         control = lmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 100000)))
  
  # Model 3: Age-period interaction (to test for convergence)
  cat("Fitting Model 3: HAPC with age-period interaction...\n")
  
  # Use a linear year term for interaction
  df_apc$year_centered <- df_apc$year - mean(df_apc$year)
  
  formula_interaction <- as.formula(paste0(
    outcome, " ~ age_centered * year_centered + age_squared + ",
    "(1|period_factor) + (1|cohort_factor)"
  ))
  
  model_interaction <- lmer(formula_interaction,
                            data = df_apc,
                            REML = TRUE,
                            control = lmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
  
  cat("\nAll frequentist models fitted successfully.\n")
  
  return(list(
    basic = model_basic,
    agegroup = model_agegroup,
    interaction = model_interaction
  ))
}

# -----------------------------------------------------------------------------
# 4.2 Extract and Format HAPC Results
# -----------------------------------------------------------------------------

extract_hapc_results <- function(model, model_name = "HAPC Model") {
  
  cat("\n==========", model_name, "==========\n\n")
  
  # Fixed effects
  cat("FIXED EFFECTS:\n")
  fe <- tidy(model, effects = "fixed", conf.int = TRUE)
  print(fe)
  
  # Random effects (variance components)
  cat("\nVARIANCE COMPONENTS:\n")
  vc <- as.data.frame(VarCorr(model))
  print(vc)
  
  # Calculate variance proportions
  total_var <- sum(vc$vcov)
  cat("\nPROPORTION OF VARIANCE:\n")
  
  period_var <- vc$vcov[vc$grp == "period_factor"]
  cohort_var <- vc$vcov[vc$grp == "cohort_factor"]
  residual_var <- vc$vcov[vc$grp == "Residual"]
  
  cat("Period variance / Total:", round(period_var / total_var * 100, 1), "%\n")
  cat("Cohort variance / Total:", round(cohort_var / total_var * 100, 1), "%\n")
  cat("Residual variance / Total:", round(residual_var / total_var * 100, 1), "%\n")
  
  # Ratio of cohort to period variance
  cat("\nCohort/Period variance ratio:", round(cohort_var / period_var, 2), "\n")
  
  # Interpretation guide
  cat("\nINTERPRETATION:\n")
  cat("Age coefficient:", round(fixef(model)["age_centered"], 4), "\n")
  cat("  (Negative = health declines with age, as expected)\n")
  cat("  Per 10-year age increase: ", round(fixef(model)["age_centered"] * 10, 3), " points on SRH scale\n")
  
  # Random effects (BLUPs) for plotting
  re <- ranef(model)
  
  # Model fit
  cat("\nMODEL FIT:\n")
  cat("AIC:", AIC(model), "\n")
  cat("BIC:", BIC(model), "\n")
  cat("Log-likelihood:", as.numeric(logLik(model)), "\n")
  
  # R-squared approximations
  cat("\nR-SQUARED (approximate):\n")
  r2_marginal <- performance::r2_nakagawa(model)$R2_marginal
  r2_conditional <- performance::r2_nakagawa(model)$R2_conditional
  cat("Marginal R² (fixed effects):", round(r2_marginal, 3), "\n")
  cat("Conditional R² (fixed + random):", round(r2_conditional, 3), "\n")
  
  return(list(
    fixed_effects = fe,
    variance_components = vc,
    random_effects = re,
    model = model
  ))
}

# -----------------------------------------------------------------------------
# 4.3 Plot Random Effects (Period and Cohort Deviations)
# -----------------------------------------------------------------------------

plot_random_effects <- function(model, df_apc) {
  
  re <- ranef(model, condVar = TRUE)
  
  # Period effects
  period_re <- as.data.frame(re$period_factor) %>%
    rownames_to_column("period") %>%
    rename(estimate = `(Intercept)`) %>%
    mutate(
      period = as.numeric(as.character(period)),
      se = sqrt(attr(re$period_factor, "postVar")[1,1,])
    ) %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )
  
  p_period <- ggplot(period_re, aes(x = period, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.5, alpha = 0.5) +
    geom_point(size = 2, color = "steelblue") +
    geom_line(color = "steelblue", alpha = 0.5) +
    labs(
      title = "Period Effects (Random Intercepts)",
      subtitle = "Deviations from grand mean; positive = better health than average",
      x = "Survey Year",
      y = "Deviation in SRH (points)",
      caption = "95% CIs shown. Effects centered at zero."
    )
  
  # Cohort effects
  cohort_re <- as.data.frame(re$cohort_factor) %>%
    rownames_to_column("cohort_group") %>%
    rename(estimate = `(Intercept)`) %>%
    mutate(
      cohort_mid = as.numeric(gsub("\\[|,.*", "", cohort_group)) + 2.5,
      se = sqrt(attr(re$cohort_factor, "postVar")[1,1,])
    ) %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    ) %>%
    arrange(cohort_mid)
  
  p_cohort <- ggplot(cohort_re, aes(x = cohort_mid, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 1, alpha = 0.5) +
    geom_point(size = 2, color = "darkred") +
    geom_line(color = "darkred", alpha = 0.5) +
    labs(
      title = "Cohort Effects (Random Intercepts)",
      subtitle = "Deviations from grand mean; positive = better health than average",
      x = "Birth Cohort (midpoint)",
      y = "Deviation in SRH (points)",
      caption = "95% CIs shown. Effects centered at zero."
    )
  
  # Combined plot
  combined <- p_period + p_cohort +
    plot_annotation(
      title = "HAPC Random Effects: Period and Cohort Deviations",
      subtitle = "Linear Mixed Model | Continuous SRH (1-5)",
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )
  
  return(list(
    period_plot = p_period,
    cohort_plot = p_cohort,
    combined = combined,
    period_data = period_re,
    cohort_data = cohort_re
  ))
}

# =============================================================================
# SECTION 5: BAYESIAN HAPC MODEL (Following Your Colleague's Approach)
# =============================================================================

#' Fit Bayesian HAPC model using rstanarm for continuous outcome
#' 
#' RATIONALE: Bayesian estimation provides:
#' - Better handling of variance components near boundary (zero)
#' - Full posterior distributions for uncertainty quantification
#' - Natural incorporation of survey design via nested random effects
#' - No reliance on asymptotic approximations
#' 
#' For continuous outcome, use stan_lmer instead of stan_glmer

fit_hapc_bayesian <- function(df_apc, 
                              outcome = "srh_continuous",
                              iter = 4000,
                              adapt_delta = 0.99,
                              cores = 4) {
  
  cat("\n========== FITTING BAYESIAN HAPC MODEL (LINEAR) ==========\n\n")
  cat("This may take 10-30 minutes depending on data size and iterations.\n\n")
  
  # Ensure all grouping variables are factors
  df_apc <- df_apc %>%
    mutate(
      period_factor = as.factor(year),
      cohort_factor = as.factor(cohort_group),
      strata_factor = as.factor(strata),
      psu_factor = as.factor(paste(strata, psu, sep = "_"))
    )
  
  # Check for valid log weights
  if (any(df_apc$wt <= 0, na.rm = TRUE)) {
    warning("Some weights are zero or negative. Replacing with minimum positive weight.")
    min_pos_wt <- min(df_apc$wt[df_apc$wt > 0], na.rm = TRUE)
    df_apc$wt[df_apc$wt <= 0] <- min_pos_wt
  }
  df_apc$ln_wt <- log(df_apc$wt)
  
  # Prepare formula following your colleague's approach
  # Using stan_lmer for continuous outcome
  formula_bayesian <- as.formula(paste0(
    outcome, " ~ age_centered + scale(age_squared) + ln_wt + ",
    "(1|period_factor) + (1|cohort_factor) + (1|strata_factor/psu_factor)"
  ))
  
  # Fit model - using stan_lmer for continuous outcome
  model_bayes <- stan_lmer(
    formula_bayesian,
    data = df_apc,
    adapt_delta = adapt_delta,
    iter = iter,
    cores = cores,
    seed = 20240107,
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_intercept = normal(3, 1),  # Prior centered around middle of SRH scale
    prior_aux = exponential(1)  # Prior on residual SD
  )
  
  cat("\nBayesian model fitted successfully.\n")
  cat("Check diagnostics with: summary(model), pp_check(model)\n\n")
  
  return(model_bayes)
}

# -----------------------------------------------------------------------------
# 5.2 Extract Bayesian Results
# -----------------------------------------------------------------------------

extract_bayesian_results <- function(model_bayes) {
  
  cat("\n========== BAYESIAN HAPC RESULTS ==========\n\n")
  
  # Summary statistics
  cat("POSTERIOR SUMMARY:\n")
  summ <- summary(model_bayes, 
                  pars = c("(Intercept)", "age_centered", "scale(age_squared)", "ln_wt", "sigma"),
                  probs = c(0.025, 0.50, 0.975))
  print(summ)
  
  # Variance components
  cat("\nVARIANCE COMPONENTS (Posterior Medians):\n")
  vc <- VarCorr(model_bayes)
  print(vc)
  
  # Interpretation
  cat("\nINTERPRETATION:\n")
  age_coef <- fixef(model_bayes)["age_centered"]
  cat("Age coefficient (posterior mean):", round(age_coef, 4), "\n")
  cat("Per 10-year age increase:", round(age_coef * 10, 3), "points on SRH scale\n")
  
  # R-hat and effective sample size
  cat("\nDIAGNOSTICS:\n")
  cat("Check Rhat < 1.01 and n_eff > 400 for all parameters\n")
  
  return(list(
    model = model_bayes,
    summary = summ,
    variance_components = vc
  ))
}

# -----------------------------------------------------------------------------
# 5.3 Bayesian Model Diagnostics
# -----------------------------------------------------------------------------

bayesian_diagnostics <- function(model_bayes) {
  
  # Trace plots for key parameters
  p_trace <- plot(model_bayes, plotfun = "trace", 
                  pars = c("(Intercept)", "age_centered", "sigma"))
  
  # Posterior predictive check
  p_ppcheck <- pp_check(model_bayes, nreps = 50) +
    labs(title = "Posterior Predictive Check",
         subtitle = "Dark line = observed; light lines = simulated from posterior")
  
  # Rhat check
  summ <- summary(model_bayes)
  rhats <- summ[, "Rhat"]
  cat("All Rhat < 1.01:", all(rhats < 1.01, na.rm = TRUE), "\n")
  cat("Max Rhat:", max(rhats, na.rm = TRUE), "\n")
  
  return(list(trace = p_trace, ppcheck = p_ppcheck))
}

# -----------------------------------------------------------------------------
# 5.4 Plot Bayesian Random Effects
# -----------------------------------------------------------------------------

plot_bayesian_random_effects <- function(model_bayes, df_apc) {
  
  # Extract random effects with uncertainty
  posterior <- as.data.frame(model_bayes)
  
  # Period effects
  period_cols <- grep("b\\[\\(Intercept\\) period_factor:", names(posterior), value = TRUE)
  
  if (length(period_cols) > 0) {
    period_draws <- posterior[, period_cols, drop = FALSE]
    
    period_summary <- tibble(
      period = gsub(".*period_factor:(\\d+)\\]", "\\1", period_cols),
      mean = colMeans(period_draws),
      median = apply(period_draws, 2, median),
      lower = apply(period_draws, 2, quantile, 0.025),
      upper = apply(period_draws, 2, quantile, 0.975),
      prob_positive = colMeans(period_draws > 0)
    ) %>%
      mutate(period = as.numeric(period)) %>%
      arrange(period)
    
    p_period <- ggplot(period_summary, aes(x = period, y = median)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "steelblue") +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(
        title = "Period Effects (Bayesian)",
        subtitle = "Posterior medians with 95% credible intervals",
        x = "Survey Year",
        y = "Deviation in SRH (points)"
      )
  } else {
    period_summary <- NULL
    p_period <- NULL
  }
  
  # Cohort effects
  cohort_cols <- grep("b\\[\\(Intercept\\) cohort_factor:", names(posterior), value = TRUE)
  
  if (length(cohort_cols) > 0) {
    cohort_draws <- posterior[, cohort_cols, drop = FALSE]
    
    cohort_summary <- tibble(
      cohort = gsub(".*cohort_factor:(.+)\\]", "\\1", cohort_cols),
      mean = colMeans(cohort_draws),
      median = apply(cohort_draws, 2, median),
      lower = apply(cohort_draws, 2, quantile, 0.025),
      upper = apply(cohort_draws, 2, quantile, 0.975)
    ) %>%
      mutate(
        cohort_start = as.numeric(gsub("\\[|,.*", "", cohort)),
        cohort_mid = cohort_start + 2.5
      ) %>%
      arrange(cohort_mid)
    
    p_cohort <- ggplot(cohort_summary, aes(x = cohort_mid, y = median)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkred") +
      geom_line(color = "darkred", linewidth = 1) +
      geom_point(color = "darkred", size = 2) +
      labs(
        title = "Cohort Effects (Bayesian)",
        subtitle = "Posterior medians with 95% credible intervals",
        x = "Birth Cohort (midpoint year)",
        y = "Deviation in SRH (points)"
      )
  } else {
    cohort_summary <- NULL
    p_cohort <- NULL
  }
  
  # Combined
  if (!is.null(p_period) & !is.null(p_cohort)) {
    combined <- p_period + p_cohort
  } else {
    combined <- p_period
  }
  
  return(list(
    period_plot = p_period,
    cohort_plot = p_cohort,
    combined = combined,
    period_summary = period_summary,
    cohort_summary = cohort_summary
  ))
}

# =============================================================================
# SECTION 6: INTRINSIC ESTIMATOR
# =============================================================================

#' Prepare and fit Intrinsic Estimator for continuous outcome

prepare_ie_data <- function(df_apc, svy_design, outcome = "srh_continuous") {
  
  formula_str <- as.formula(paste0("~", outcome))
  
  apc_table <- svyby(formula_str,
                     ~age_group + year,
                     svy_design,
                     svymean,
                     na.rm = TRUE) %>%
    as_tibble() %>%
    rename(mean_srh = all_of(outcome),
           se = paste0("se.", outcome)) %>%
    mutate(
      age_index = as.numeric(age_group),
      period_index = as.numeric(factor(year)),
      cohort_index = period_index - age_index + max(as.numeric(age_group))
    )
  
  # Get sample sizes
  n_table <- df_apc %>%
    group_by(age_group, year) %>%
    summarize(n = n(), .groups = "drop")
  
  apc_table <- apc_table %>%
    left_join(n_table, by = c("age_group", "year"))
  
  return(apc_table)
}

fit_ie_manual <- function(apc_table) {
  
  cat("\n========== INTRINSIC ESTIMATOR (Continuous Outcome) ==========\n\n")
  
  n_age <- n_distinct(apc_table$age_group)
  n_period <- n_distinct(apc_table$year)
  n_cohort <- n_age + n_period - 1
  
  cat("Number of age groups:", n_age, "\n")
  cat("Number of periods:", n_period, "\n")
  cat("Number of cohorts:", n_cohort, "\n\n")
  
  # Try apc package first
  tryCatch({
    library(apc)
    
    rate_matrix <- apc_table %>%
      select(age_group, year, mean_srh) %>%
      pivot_wider(names_from = year, values_from = mean_srh) %>%
      select(-age_group) %>%
      as.matrix()
    
    rownames(rate_matrix) <- levels(apc_table$age_group)
    
    apc_fit <- apc.fit(rate_matrix, model.family = "gaussian.rates")
    
    cat("IE model fitted using apc package.\n")
    
    return(list(fit = apc_fit, rate_matrix = rate_matrix))
    
  }, error = function(e) {
    cat("apc package not available. Using constrained regression approach.\n\n")
    
    apc_table <- apc_table %>%
      mutate(
        age_f = factor(age_group),
        period_f = factor(year),
        cohort_f = factor(cohort_index)
      )
    
    options(contrasts = c("contr.sum", "contr.sum"))
    
    # Weighted linear regression
    model_apc <- lm(mean_srh ~ age_f + period_f + cohort_f, 
                    data = apc_table,
                    weights = n)
    
    cat("Note: This is a constrained GLM approximation, not true IE.\n\n")
    print(summary(model_apc))
    
    return(list(fit = model_apc, data = apc_table))
  })
}

# =============================================================================
# SECTION 7: SENSITIVITY ANALYSES
# =============================================================================

# -----------------------------------------------------------------------------
# 7.1 SENSITIVITY: Binary Fair/Poor Outcome
# -----------------------------------------------------------------------------

#' Run HAPC with binary outcome as sensitivity analysis
#' 
#' This matches common approaches in the literature and checks
#' whether results are robust to outcome specification.

fit_hapc_binary_sensitivity <- function(df_apc) {
  
  cat("\n========== SENSITIVITY: BINARY FAIR/POOR OUTCOME ==========\n\n")
  
  # Frequentist logistic HAPC
  formula_binary <- srh_fairpoor ~ age_centered + age_squared + 
    (1|period_factor) + (1|cohort_factor)
  
  model_binary <- glmer(formula_binary,
                        data = df_apc,
                        family = binomial(link = "logit"),
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 100000)))
  
  cat("BINARY OUTCOME MODEL (Fair/Poor = 1):\n\n")
  cat("FIXED EFFECTS (log-odds):\n")
  print(tidy(model_binary, effects = "fixed", conf.int = TRUE, exponentiate = FALSE))
  
  cat("\nFIXED EFFECTS (odds ratios):\n")
  print(tidy(model_binary, effects = "fixed", conf.int = TRUE, exponentiate = TRUE))
  
  cat("\nVARIANCE COMPONENTS:\n")
  print(VarCorr(model_binary))
  
  # Compare variance ratios
  vc <- as.data.frame(VarCorr(model_binary))
  cohort_var <- vc$vcov[vc$grp == "cohort_factor"]
  period_var <- vc$vcov[vc$grp == "period_factor"]
  
  cat("\nCohort/Period variance ratio:", round(cohort_var / period_var, 2), "\n")
  
  return(model_binary)
}

# -----------------------------------------------------------------------------
# 7.2 Sensitivity: Vary Grouping Schemes
# -----------------------------------------------------------------------------

sensitivity_grouping <- function(df_apc, outcome = "srh_continuous") {
  
  cat("\n========== SENSITIVITY: GROUPING SCHEMES ==========\n\n")
  
  # 5-year cohort groups (default)
  df_5yr <- df_apc %>%
    mutate(cohort_5 = cut(cohort, breaks = seq(1900, 2010, 5), right = FALSE))
  
  # 10-year cohort groups
  df_10yr <- df_apc %>%
    mutate(cohort_10 = cut(cohort, breaks = seq(1900, 2010, 10), right = FALSE))
  
  cat("Fitting model with 5-year cohort groups...\n")
  model_5yr <- lmer(
    as.formula(paste0(outcome, " ~ age_centered + age_squared + ",
                      "(1|factor(year)) + (1|cohort_5)")),
    data = df_5yr,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )
  
  cat("Fitting model with 10-year cohort groups...\n")
  model_10yr <- lmer(
    as.formula(paste0(outcome, " ~ age_centered + age_squared + ",
                      "(1|factor(year)) + (1|cohort_10)")),
    data = df_10yr,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )
  
  cat("\nVARIANCE COMPONENTS COMPARISON:\n")
  cat("\n5-year grouping:\n")
  print(VarCorr(model_5yr))
  cat("\n10-year grouping:\n")
  print(VarCorr(model_10yr))
  
  # Compare age coefficients
  cat("\nAGE COEFFICIENTS:\n")
  cat("5-year grouping:", round(fixef(model_5yr)["age_centered"], 5), "\n")
  cat("10-year grouping:", round(fixef(model_10yr)["age_centered"], 5), "\n")
  
  return(list(model_5yr = model_5yr, model_10yr = model_10yr))
}

# -----------------------------------------------------------------------------
# 7.3 Sensitivity: Vary Age Functional Form
# -----------------------------------------------------------------------------

sensitivity_age_form <- function(df_apc, outcome = "srh_continuous") {
  
  cat("\n========== SENSITIVITY: AGE FUNCTIONAL FORM ==========\n\n")
  
  # Linear + quadratic (baseline)
  cat("Model 1: Linear + Quadratic age\n")
  m1 <- lmer(
    as.formula(paste0(outcome, " ~ age_centered + age_squared + ",
                      "(1|period_factor) + (1|cohort_factor)")),
    data = df_apc,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )
  
  # Cubic
  cat("Model 2: Cubic age\n")
  df_apc$age_cubed <- df_apc$age_centered^3
  m2 <- lmer(
    as.formula(paste0(outcome, " ~ age_centered + age_squared + age_cubed + ",
                      "(1|period_factor) + (1|cohort_factor)")),
    data = df_apc,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )
  
  # Natural spline
  cat("Model 3: Natural spline (df=4)\n")
  m3 <- lmer(
    as.formula(paste0(outcome, " ~ ns(age, df=4) + ",
                      "(1|period_factor) + (1|cohort_factor)")),
    data = df_apc,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )
  
  cat("\nMODEL COMPARISON (AIC):\n")
  cat("Linear + Quadratic:", AIC(m1), "\n")
  cat("Cubic:", AIC(m2), "\n")
  cat("Natural spline (df=4):", AIC(m3), "\n")
  
  cat("\nCOHORT VARIANCE across specifications:\n")
  cat("Linear + Quadratic:", 
      as.data.frame(VarCorr(m1))$vcov[as.data.frame(VarCorr(m1))$grp == "cohort_factor"], "\n")
  cat("Cubic:", 
      as.data.frame(VarCorr(m2))$vcov[as.data.frame(VarCorr(m2))$grp == "cohort_factor"], "\n")
  cat("Spline:", 
      as.data.frame(VarCorr(m3))$vcov[as.data.frame(VarCorr(m3))$grp == "cohort_factor"], "\n")
  
  return(list(quadratic = m1, cubic = m2, spline = m3))
}

# =============================================================================
# SECTION 8: PUBLICATION-READY TABLES
# =============================================================================

create_table1 <- function(df_apc, svy_design) {
  
  tbl1 <- svy_design %>%
    tbl_svysummary(
      include = c(age, year, srh_continuous, srh_fairpoor),
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      label = list(
        age ~ "Age (years)",
        year ~ "Survey Year",
        srh_continuous ~ "Self-Rated Health (1-5, 5=Excellent)",
        srh_fairpoor ~ "Fair/Poor Health"
      ),
      digits = list(
        all_continuous() ~ 2,
        all_categorical() ~ c(0, 1)
      )
    ) %>%
    modify_header(label = "**Characteristic**") %>%
    modify_caption("**Table 1. Sample Characteristics (Survey-Weighted)**")
  
  return(tbl1)
}

create_hapc_table <- function(model_freq, model_bayes = NULL, model_binary = NULL) {
  
  # Frequentist continuous results
  fe_freq <- tidy(model_freq, effects = "fixed", conf.int = TRUE) %>%
    mutate(method = "Frequentist (Continuous)")
  
  vc_freq <- as.data.frame(VarCorr(model_freq)) %>%
    select(grp, vcov, sdcor) %>%
    mutate(method = "Frequentist (Continuous)")
  
  # Combine
  fe_combined <- fe_freq
  
  if (!is.null(model_bayes)) {
    fe_bayes <- tidy(model_bayes, effects = "fixed", conf.int = TRUE) %>%
      mutate(method = "Bayesian (Continuous)")
    fe_combined <- bind_rows(fe_combined, fe_bayes)
  }
  
  if (!is.null(model_binary)) {
    fe_binary <- tidy(model_binary, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
      mutate(method = "Frequentist (Binary)")
    # Note: binary uses OR, continuous uses coefficients
  }
  
  # Create gt table
  tbl_fixed <- fe_combined %>%
    select(method, term, estimate, conf.low, conf.high) %>%
    mutate(
      estimate = round(estimate, 4),
      ci = paste0("(", round(conf.low, 4), ", ", round(conf.high, 4), ")")
    ) %>%
    select(method, term, estimate, ci) %>%
    gt() %>%
    tab_header(
      title = "Table 2. HAPC Model Results: Fixed Effects",
      subtitle = "Continuous SRH (1-5) | Coefficients with 95% CIs"
    ) %>%
    cols_label(
      method = "Method",
      term = "Parameter",
      estimate = "Estimate",
      ci = "95% CI"
    )
  
  return(tbl_fixed)
}

# =============================================================================
# SECTION 9: MAIN ANALYSIS WORKFLOW
# =============================================================================

run_full_analysis <- function(df, 
                              outcome = "srh", #outcome = "srh_continuous",
                              run_bayesian = TRUE,
                              run_binary_sensitivity = TRUE,
                              save_plots = TRUE,
                              output_dir = "apc_results",
                              srh_already_reversed = TRUE) {
  
  cat("\n")
  cat("================================================================\n")
  cat("   APC ANALYSIS OF SELF-RATED HEALTH (CONTINUOUS)              \n")
  cat("   Primary: SRH 1-5 (5 = Excellent)                            \n")
  cat("   Sensitivity: Binary Fair/Poor                               \n")
  cat("================================================================\n\n")
  
  if (save_plots) {
    dir.create(output_dir, showWarnings = FALSE)
  }
  
  # -------------------------------------------------------------------------
  # Step 1: Data Preparation
  # -------------------------------------------------------------------------
  cat("STEP 1: Preparing data...\n")
  df_apc <- prepare_apc_data(df, srh_already_reversed = srh_already_reversed)
  
  # -------------------------------------------------------------------------
  # Step 2: Create Survey Design
  # -------------------------------------------------------------------------
  cat("STEP 2: Creating survey design object...\n")
  svy_design <- create_survey_design(df_apc)
  
  # -------------------------------------------------------------------------
  # Step 3: Descriptive Visualizations
  # -------------------------------------------------------------------------
  cat("STEP 3: Creating descriptive visualizations...\n")
  
  lexis_result <- create_lexis_surface(df_apc, svy_design, outcome)
  trends_result <- create_age_specific_trends(df_apc, svy_design, outcome)
  cohort_result <- create_cohort_trajectories(df_apc, svy_design, outcome)
  period_result <- create_period_profiles(df_apc, svy_design, outcome)
  
  descriptive_figure <- create_combined_descriptive_figure(
    lexis_result, trends_result, cohort_result, period_result
  )
  
  if (save_plots) {
    ggsave(file.path(output_dir, "figure1_descriptive.png"), 
           descriptive_figure, width = 14, height = 12, dpi = 300)
    ggsave(file.path(output_dir, "figure1_descriptive.pdf"), 
           descriptive_figure, width = 14, height = 12)
  }
  
  # -------------------------------------------------------------------------
  # Step 4: Frequentist HAPC Models (Linear)
  # -------------------------------------------------------------------------
  cat("STEP 4: Fitting frequentist HAPC models (linear)...\n")
  
  hapc_models <- fit_hapc_frequentist(df_apc, outcome)
  hapc_results <- extract_hapc_results(hapc_models$basic, "Basic HAPC (Linear)")
  
  re_plots <- plot_random_effects(hapc_models$basic, df_apc)
  
  if (save_plots) {
    ggsave(file.path(output_dir, "figure2_random_effects.png"),
           re_plots$combined, width = 12, height = 5, dpi = 300)
  }
  
  # -------------------------------------------------------------------------
  # Step 5: Bayesian HAPC Model (Linear)
  # -------------------------------------------------------------------------
  if (run_bayesian) {
    cat("STEP 5: Fitting Bayesian HAPC model (linear)...\n")
    
    model_bayes <- fit_hapc_bayesian(df_apc, outcome, iter = 4000)
    bayes_results <- extract_bayesian_results(model_bayes)
    bayes_diagnostics <- bayesian_diagnostics(model_bayes)
    bayes_re_plots <- plot_bayesian_random_effects(model_bayes, df_apc)
    
    if (save_plots) {
      ggsave(file.path(output_dir, "figure3_bayesian_ppcheck.png"),
             bayes_diagnostics$ppcheck, width = 8, height = 6, dpi = 300)
      if (!is.null(bayes_re_plots$combined)) {
        ggsave(file.path(output_dir, "figure4_bayesian_random_effects.png"),
               bayes_re_plots$combined, width = 12, height = 5, dpi = 300)
      }
    }
  } else {
    model_bayes <- NULL
    bayes_results <- NULL
    bayes_re_plots <- NULL
  }
  
  # -------------------------------------------------------------------------
  # Step 6: Intrinsic Estimator
  # -------------------------------------------------------------------------
  cat("STEP 6: Fitting Intrinsic Estimator...\n")
  
  ie_data <- prepare_ie_data(df_apc, svy_design, outcome)
  ie_results <- fit_ie_manual(ie_data)
  
  # -------------------------------------------------------------------------
  # Step 7: Sensitivity Analyses
  # -------------------------------------------------------------------------
  cat("STEP 7: Running sensitivity analyses...\n")
  
  # Binary outcome sensitivity
  model_binary <- NULL
  if (run_binary_sensitivity) {
    cat("\n--- Binary Fair/Poor Sensitivity ---\n")
    model_binary <- fit_hapc_binary_sensitivity(df_apc)
  }
  
  # Grouping sensitivity
  sensitivity_group <- sensitivity_grouping(df_apc, outcome)
  
  # Age functional form sensitivity
  sensitivity_age <- sensitivity_age_form(df_apc, outcome)
  
  # -------------------------------------------------------------------------
  # Step 8: Create Tables
  # -------------------------------------------------------------------------
  cat("STEP 8: Creating publication tables...\n")
  
  table1 <- create_table1(df_apc, svy_design)
  hapc_table <- create_hapc_table(hapc_models$basic, model_bayes, model_binary)
  
  # -------------------------------------------------------------------------
  # Return all results
  # -------------------------------------------------------------------------
  cat("\n")
  cat("================================================================\n")
  cat("   ANALYSIS COMPLETE                                           \n")
  cat("================================================================\n\n")
  
  results <- list(
    data = df_apc,
    survey_design = svy_design,
    descriptive = list(
      lexis = lexis_result,
      trends = trends_result,
      cohort = cohort_result,
      period = period_result,
      combined_figure = descriptive_figure
    ),
    hapc_frequentist = list(
      models = hapc_models,
      results = hapc_results,
      re_plots = re_plots
    ),
    hapc_bayesian = list(
      model = model_bayes,
      results = bayes_results,
      re_plots = bayes_re_plots
    ),
    intrinsic_estimator = ie_results,
    sensitivity = list(
      binary = model_binary,
      grouping = sensitivity_group,
      age_form = sensitivity_age
    ),
    tables = list(
      table1 = table1,
      hapc = hapc_table
    )
  )
  
  return(results)
}

# =============================================================================
# SECTION 10: EXAMPLE USAGE
# =============================================================================

# To run the analysis with your NHIS data:
#
# 1. Load your data
#    df <- read_csv("nhis_data.csv")
    df <- df_claude
#
# 2. Check your SRH coding:
#    table(df$srh)
#    
#    If 1=Excellent to 5=Poor (standard NHIS):
#      Set srh_already_reversed = FALSE (default)
#      The code will reverse it to 1=Poor, 5=Excellent
#    
#    If already 1=Poor to 5=Excellent:
#      Set srh_already_reversed = TRUE
#
# 3. Run the analysis:
   results <- run_full_analysis(
     df = df,
     outcome = "srh_continuous",      # Primary: continuous
     run_bayesian = FALSE,
     run_binary_sensitivity = FALSE,   # Also run binary as sensitivity
     save_plots = TRUE,
     output_dir = "apc_results_nhis",
     srh_already_reversed = TRUE     # Set based on your coding
   )
#
# 4. Access results:
#    results$descriptive$combined_figure
#    results$hapc_frequentist$results
#    results$sensitivity$binary  # Binary sensitivity results
#    results$tables$table1

# =============================================================================
# END OF SCRIPT
# =============================================================================