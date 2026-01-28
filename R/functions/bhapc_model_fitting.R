# ==============================================================================
# bhapc_model_fitting.R
# Bayesian Hierarchical APC model fitting using rstanarm
# Based on Gloria Graf's methodology
# Author: Christine Lucille Kuryla
# ==============================================================================

library(rstanarm)
library(broom.mixed)
library(dplyr)

# ==============================================================================
# Model Fitting
# ==============================================================================

#' Fit Bayesian HAPC model
#'
#' Fits a Bayesian Hierarchical Age-Period-Cohort model using rstanarm::stan_lmer.
#' Follows Gloria's specification with crossed random effects for period and cohort.
#'
#' @param bhapc_df Data frame from prepare_bhapc_data()
#' @param outcome Character: column name of outcome variable (default "srh")
#' @param include_strata Logical: include strata/PSU random effects (default TRUE if available)
#' @param adapt_delta Numeric: Stan adapt_delta parameter (default 0.998, Gloria's value)
#' @param iter Integer: number of iterations (default 6000, Gloria's value)
#' @param chains Integer: number of chains (default 4)
#' @param cores Integer: number of cores for parallel (default all available)
#' @param seed Integer: random seed for reproducibility
#' @return List with model object, summary, and diagnostics
#'
#' @details
#' Model specification:
#' outcome ~ age + scale(age_squared) + lnWt + (1|period_4yr) + (1|cohort_4yr) + (1|strata/psu)
#'
#' The log weight (lnWt) is included as a covariate to partially account for
#' survey design, following Gloria's approach.
#'
#' @examples
#' result <- fit_bhapc_model(bhapc_df)
fit_bhapc_model <- function(bhapc_df,
                             outcome = "srh",
                             include_strata = TRUE,
                             adapt_delta = 0.998,
                             iter = 6000,
                             chains = 4,
                             cores = parallel::detectCores(),
                             seed = 20260128) {

  # Check outcome column exists
  stopifnot(outcome %in% names(bhapc_df))

  message("Fitting BHAPC model...")
  message("  Outcome: ", outcome)
  message("  N observations: ", format(nrow(bhapc_df), big.mark = ","))
  message("  N periods: ", length(unique(bhapc_df$period_4yr)))
  message("  N cohorts: ", length(unique(bhapc_df$cohort_4yr)))
  message("  adapt_delta: ", adapt_delta)
  message("  iter: ", iter)
  message("  chains: ", chains)
  message("  cores: ", cores)

  # Check for strata/PSU columns
  has_strata <- "strata" %in% names(bhapc_df) && !all(is.na(bhapc_df$strata))
  has_psu <- "psu" %in% names(bhapc_df) && !all(is.na(bhapc_df$psu))

  # Build formula
  if (include_strata && has_strata && has_psu) {
    message("  Including strata/PSU random effects")
    formula_str <- paste0(outcome, " ~ age + scale(age_squared) + lnWt + ",
                          "(1|period_4yr) + (1|cohort_4yr) + (1|strata/psu)")
  } else if (include_strata && has_strata) {
    message("  Including strata random effects only")
    formula_str <- paste0(outcome, " ~ age + scale(age_squared) + lnWt + ",
                          "(1|period_4yr) + (1|cohort_4yr) + (1|strata)")
  } else {
    message("  No strata/PSU random effects (not available)")
    formula_str <- paste0(outcome, " ~ age + scale(age_squared) + lnWt + ",
                          "(1|period_4yr) + (1|cohort_4yr)")
  }

  formula <- as.formula(formula_str)
  message("  Formula: ", formula_str)

  # Set seed for reproducibility
  set.seed(seed)

  # Fit model
  start_time <- Sys.time()

  model <- stan_lmer(
    formula = formula,
    data = bhapc_df,
    adapt_delta = adapt_delta,
    iter = iter,
    chains = chains,
    cores = cores
  )

  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  message("  Model fitting completed in ", round(elapsed, 1), " minutes")

  # Extract diagnostics
  diagnostics <- extract_bhapc_diagnostics(model)

  # Check convergence
  if (any(diagnostics$rhat > 1.01, na.rm = TRUE)) {
    warning("Some Rhat values > 1.01, indicating potential convergence issues")
  }

  if (any(diagnostics$n_eff < 400, na.rm = TRUE)) {
    warning("Some n_eff values < 400, consider increasing iterations")
  }

  # Return results
  list(
    model = model,
    formula = formula_str,
    outcome = outcome,
    n_obs = nrow(bhapc_df),
    n_periods = length(unique(bhapc_df$period_4yr)),
    n_cohorts = length(unique(bhapc_df$cohort_4yr)),
    diagnostics = diagnostics,
    elapsed_minutes = as.numeric(elapsed),
    seed = seed,
    timestamp = Sys.time()
  )
}


#' Extract model diagnostics
#'
#' Extracts Rhat and effective sample size diagnostics from fitted model
#'
#' @param model rstanarm model object
#' @return Data frame with parameter diagnostics
extract_bhapc_diagnostics <- function(model) {
  summary_df <- as.data.frame(summary(model))

  # Get parameter names from rownames
  summary_df$parameter <- rownames(summary_df)

  # Extract key diagnostics
  diagnostics <- summary_df %>%
    select(parameter, mean, sd, n_eff, Rhat) %>%
    filter(!grepl("^b\\[", parameter))  # Exclude individual random effects

  diagnostics
}


#' Extract variance components from BHAPC model
#'
#' Extracts and formats variance components (period, cohort, residual)
#'
#' @param model rstanarm model object
#' @return Data frame with variance decomposition
extract_variance_components <- function(model) {
  # Get variance-covariance components
  vc <- VarCorr(model)

  # Extract variances for each grouping factor
  var_list <- list()

  for (grp in names(vc)) {
    var_list[[grp]] <- as.numeric(vc[[grp]])
  }

  # Get residual variance
  sigma <- sigma(model)
  var_list[["Residual"]] <- sigma^2

  # Create data frame
  var_df <- tibble(
    component = names(var_list),
    variance = unlist(var_list)
  )

  # Calculate total and percentages
  total_var <- sum(var_df$variance)

  var_df <- var_df %>%
    mutate(
      pct_of_total = variance / total_var * 100,
      sd = sqrt(variance)
    )

  # Add total row
  var_df <- bind_rows(
    var_df,
    tibble(
      component = "Total",
      variance = total_var,
      pct_of_total = 100,
      sd = sqrt(total_var)
    )
  )

  var_df
}


#' Extract fixed effects from BHAPC model
#'
#' Extracts fixed effects with 90% credible intervals
#'
#' @param model rstanarm model object
#' @return Data frame with fixed effects estimates
extract_fixed_effects <- function(model) {
  # Get summary with 90% intervals
  summary_df <- as.data.frame(summary(model, probs = c(0.05, 0.95)))
  summary_df$term <- rownames(summary_df)

  # Filter to fixed effects only (exclude random effects and sigma)
  fixed_terms <- c("(Intercept)", "age", "scale(age_squared)", "lnWt")

  fixed_df <- summary_df %>%
    filter(term %in% fixed_terms | grepl("^[a-z]", term)) %>%
    filter(!grepl("^Sigma|^b\\[", term)) %>%
    select(term, mean, sd, `5%`, `95%`, n_eff, Rhat) %>%
    rename(
      estimate = mean,
      std_error = sd,
      ci_lower_90 = `5%`,
      ci_upper_90 = `95%`
    )

  fixed_df
}


#' Extract random effects from BHAPC model
#'
#' Extracts period and cohort random effects with 90% credible intervals
#'
#' @param model rstanarm model object
#' @param bhapc_df Data frame used to fit model (for SD of age_squared)
#' @return List with period_effects and cohort_effects data frames
extract_random_effects <- function(model, bhapc_df = NULL) {
  # Get all random effects
  ranef_summary <- as.data.frame(summary(model, probs = c(0.05, 0.95)))
  ranef_summary$term <- rownames(ranef_summary)

  # Extract period effects
  period_effects <- ranef_summary %>%
    filter(grepl("period_4yr", term)) %>%
    mutate(
      period = gsub("b\\[\\(Intercept\\) period_4yr:(.*)\\]", "\\1", term)
    ) %>%
    select(period, mean, sd, `5%`, `95%`) %>%
    rename(
      estimate = mean,
      std_error = sd,
      ci_lower_90 = `5%`,
      ci_upper_90 = `95%`
    ) %>%
    mutate(period = as.numeric(period)) %>%
    arrange(period)

  # Extract cohort effects
  cohort_effects <- ranef_summary %>%
    filter(grepl("cohort_4yr", term)) %>%
    mutate(
      cohort = gsub("b\\[\\(Intercept\\) cohort_4yr:(.*)\\]", "\\1", term)
    ) %>%
    select(cohort, mean, sd, `5%`, `95%`) %>%
    rename(
      estimate = mean,
      std_error = sd,
      ci_lower_90 = `5%`,
      ci_upper_90 = `95%`
    ) %>%
    mutate(cohort = as.numeric(cohort)) %>%
    arrange(cohort)

  list(
    period_effects = period_effects,
    cohort_effects = cohort_effects
  )
}


#' Compute age effect curve from BHAPC model
#'
#' Computes the quadratic age effect with 90% CI for plotting
#'
#' @param model rstanarm model object
#' @param bhapc_df Data frame used to fit model (for SD of age_squared)
#' @param age_range Numeric vector of length 2 for age range (default c(18, 85))
#' @param n_points Number of points for curve (default 100)
#' @return Data frame with age effect curve
compute_age_effect <- function(model,
                                bhapc_df,
                                age_range = c(18, 85),
                                n_points = 100) {

  # Get fixed effects summary with 5% and 95% quantiles
  summary_df <- as.data.frame(summary(model, probs = c(0.05, 0.95)))
  summary_df$term <- rownames(summary_df)

  # Extract age coefficients
  age_coef <- summary_df %>% filter(term == "age") %>% pull(mean)
  age_coef_lower <- summary_df %>% filter(term == "age") %>% pull(`5%`)
  age_coef_upper <- summary_df %>% filter(term == "age") %>% pull(`95%`)

  # Extract age-squared coefficients (need to unscale)
  age_sq_scaled_coef <- summary_df %>% filter(term == "scale(age_squared)") %>% pull(mean)
  age_sq_scaled_lower <- summary_df %>% filter(term == "scale(age_squared)") %>% pull(`5%`)
  age_sq_scaled_upper <- summary_df %>% filter(term == "scale(age_squared)") %>% pull(`95%`)

  # Unscale the age_squared coefficient
  age_sq_sd <- sd(bhapc_df$age_squared)
  age_sq_coef <- age_sq_scaled_coef / age_sq_sd
  age_sq_coef_lower <- age_sq_scaled_lower / age_sq_sd
  age_sq_coef_upper <- age_sq_scaled_upper / age_sq_sd

  # Create age grid
  ages <- seq(age_range[1], age_range[2], length.out = n_points)

  # Compute effect at each age (relative to age 0 for scaling)
  # Effect = age_coef * age + age_sq_coef * age^2
  effect_df <- tibble(
    age = ages,
    estimate = (age_coef * ages) + (age_sq_coef * ages^2),
    ci_lower_90 = (age_coef_lower * ages) + (age_sq_coef_lower * ages^2),
    ci_upper_90 = (age_coef_upper * ages) + (age_sq_coef_upper * ages^2)
  )

  # Center the effect at the minimum age for interpretability
  # This shows the relative effect as age increases from 18
  min_effect <- effect_df$estimate[1]
  effect_df <- effect_df %>%
    mutate(
      estimate_centered = estimate - min_effect,
      ci_lower_centered = ci_lower_90 - effect_df$ci_lower_90[1],
      ci_upper_centered = ci_upper_90 - effect_df$ci_upper_90[1]
    )

  effect_df
}


# ==============================================================================
# Model Comparison
# ==============================================================================

#' Compare BHAPC models using LOOIC
#'
#' Computes and compares LOOIC for model selection
#'
#' @param models Named list of rstanarm model objects
#' @return Data frame with model comparison
compare_bhapc_models <- function(models) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("Package 'loo' is required for model comparison")
  }

  # Compute LOO for each model
  loo_list <- lapply(models, function(m) {
    tryCatch(
      loo::loo(m),
      error = function(e) {
        warning("LOO computation failed: ", e$message)
        NULL
      }
    )
  })

  # Extract key metrics
  results <- lapply(names(loo_list), function(nm) {
    l <- loo_list[[nm]]
    if (!is.null(l)) {
      tibble(
        model = nm,
        elpd_loo = l$estimates["elpd_loo", "Estimate"],
        se_elpd_loo = l$estimates["elpd_loo", "SE"],
        looic = -2 * l$estimates["elpd_loo", "Estimate"],
        p_loo = l$estimates["p_loo", "Estimate"]
      )
    }
  })

  bind_rows(results)
}
