# ==============================================================================
# bhapc_table_generation.R
# Table generation for BHAPC analysis results
# Based on Gloria Graf's Table 2 format
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(tidyr)

# ==============================================================================
# Table 2: APC Decomposition Results
# ==============================================================================

#' Create Table 2: APC decomposition from BHAPC model
#'
#' Generates a formatted table with fixed effects and variance components
#' analogous to Gloria's Table 2.
#'
#' @param model rstanarm model object from fit_bhapc_model()
#' @param bhapc_df Data frame used to fit model (for SD of age_squared)
#' @param survey Character: survey name for labeling
#' @param output_path Optional path to save CSV
#' @return Data frame with Table 2 contents
#'
#' @details
#' Table includes:
#' - Fixed effects: Age (linear), Age (quadratic)
#' - Variance components: Period, Cohort, Residual
#' - 90% credible intervals for all estimates
#'
#' @examples
#' table2 <- create_table2(model, bhapc_df, survey = "nhanes")
create_table2 <- function(model,
                           bhapc_df,
                           survey = "nhanes",
                           output_path = NULL) {

  # Get fixed effects summary
  summary_df <- as.data.frame(summary(model, probs = c(0.05, 0.95)))
  summary_df$term <- rownames(summary_df)

  # Extract age coefficient
  age_row <- summary_df %>%
    filter(term == "age") %>%
    mutate(
      effect = "Age (linear)",
      estimate = round(mean, 4),
      ci_90 = paste0("[", round(`5%`, 4), ", ", round(`95%`, 4), "]")
    ) %>%
    select(effect, estimate, ci_90)

  # Extract age-squared coefficient (unscaled)
  age_sq_sd <- sd(bhapc_df$age_squared)
  age_sq_row <- summary_df %>%
    filter(term == "scale(age_squared)") %>%
    mutate(
      effect = "Age (quadratic)",
      # Unscale coefficient
      estimate = round(mean / age_sq_sd, 6),
      ci_90 = paste0("[", round(`5%` / age_sq_sd, 6), ", ",
                     round(`95%` / age_sq_sd, 6), "]")
    ) %>%
    select(effect, estimate, ci_90)

  # Extract lnWt coefficient
  lnwt_row <- summary_df %>%
    filter(term == "lnWt") %>%
    mutate(
      effect = "Log weight",
      estimate = round(mean, 4),
      ci_90 = paste0("[", round(`5%`, 4), ", ", round(`95%`, 4), "]")
    ) %>%
    select(effect, estimate, ci_90)

  # Combine fixed effects
  fixed_effects <- bind_rows(age_row, age_sq_row, lnwt_row)

  # Extract variance components
  vc <- VarCorr(model)
  sigma_resid <- sigma(model)

  # Period variance
  period_var <- if ("period_4yr" %in% names(vc)) {
    as.numeric(vc[["period_4yr"]])
  } else {
    NA_real_
  }

  # Cohort variance
  cohort_var <- if ("cohort_4yr" %in% names(vc)) {
    as.numeric(vc[["cohort_4yr"]])
  } else {
    NA_real_
  }

  # Residual variance
  resid_var <- sigma_resid^2

  # Calculate total variance (period + cohort + residual)
  total_var <- sum(c(period_var, cohort_var, resid_var), na.rm = TRUE)

  # Create variance component rows
  variance_rows <- tibble(
    effect = c("Period variance", "Cohort variance", "Residual variance", "Total variance"),
    estimate = round(c(period_var, cohort_var, resid_var, total_var), 4),
    ci_90 = c(
      paste0("(", round(period_var / total_var * 100, 1), "%)"),
      paste0("(", round(cohort_var / total_var * 100, 1), "%)"),
      paste0("(", round(resid_var / total_var * 100, 1), "%)"),
      "(100%)"
    )
  )

  # Add section headers
  table2 <- bind_rows(
    tibble(effect = "Fixed Effects", estimate = NA_real_, ci_90 = NA_character_),
    fixed_effects,
    tibble(effect = "", estimate = NA_real_, ci_90 = NA_character_),
    tibble(effect = "Variance Components", estimate = NA_real_, ci_90 = NA_character_),
    variance_rows
  )

  # Add survey identifier
  table2 <- table2 %>%
    mutate(survey = toupper(survey)) %>%
    select(survey, everything())

  # Save if path provided
  if (!is.null(output_path)) {
    write.csv(table2, output_path, row.names = FALSE)
    message("Saved Table 2 to: ", output_path)
  }

  table2
}


#' Create variance decomposition summary
#'
#' Creates a clean variance decomposition table for multiple surveys
#'
#' @param models Named list of model results from fit_bhapc_model()
#' @param bhapc_dfs Named list of data frames used to fit models
#' @return Data frame with variance decomposition across surveys
create_variance_summary <- function(models, bhapc_dfs = NULL) {

  results <- lapply(names(models), function(survey) {
    model <- models[[survey]]

    if (is.null(model) || is.null(model$model)) {
      return(NULL)
    }

    # Extract variance components
    vc <- VarCorr(model$model)
    sigma_resid <- sigma(model$model)

    period_var <- if ("period_4yr" %in% names(vc)) {
      as.numeric(vc[["period_4yr"]])
    } else {
      NA_real_
    }

    cohort_var <- if ("cohort_4yr" %in% names(vc)) {
      as.numeric(vc[["cohort_4yr"]])
    } else {
      NA_real_
    }

    resid_var <- sigma_resid^2
    total_var <- sum(c(period_var, cohort_var, resid_var), na.rm = TRUE)

    tibble(
      survey = toupper(survey),
      n_obs = model$n_obs,
      period_var = round(period_var, 4),
      cohort_var = round(cohort_var, 4),
      residual_var = round(resid_var, 4),
      total_var = round(total_var, 4),
      period_pct = round(period_var / total_var * 100, 1),
      cohort_pct = round(cohort_var / total_var * 100, 1),
      residual_pct = round(resid_var / total_var * 100, 1)
    )
  })

  bind_rows(results)
}


#' Create model diagnostics table
#'
#' Creates a table summarizing model diagnostics (Rhat, n_eff)
#'
#' @param result Result list from fit_bhapc_model()
#' @return Data frame with diagnostic summary
create_diagnostics_table <- function(result) {
  diag <- result$diagnostics

  # Summary statistics
  tibble(
    survey = toupper(result$outcome),
    n_obs = result$n_obs,
    n_periods = result$n_periods,
    n_cohorts = result$n_cohorts,
    elapsed_min = round(result$elapsed_minutes, 1),
    max_rhat = round(max(diag$Rhat, na.rm = TRUE), 3),
    min_neff = round(min(diag$n_eff, na.rm = TRUE), 0),
    converged = max(diag$Rhat, na.rm = TRUE) < 1.01 && min(diag$n_eff, na.rm = TRUE) > 400
  )
}


#' Create fixed effects comparison table
#'
#' Creates a table comparing fixed effects across multiple surveys
#'
#' @param models Named list of model results from fit_bhapc_model()
#' @param bhapc_dfs Named list of data frames used to fit models
#' @return Data frame with fixed effects across surveys
create_fixed_effects_comparison <- function(models, bhapc_dfs) {

  results <- lapply(names(models), function(survey) {
    model <- models[[survey]]
    bhapc_df <- bhapc_dfs[[survey]]

    if (is.null(model) || is.null(model$model)) {
      return(NULL)
    }

    # Get summary
    summary_df <- as.data.frame(summary(model$model, probs = c(0.05, 0.95)))
    summary_df$term <- rownames(summary_df)

    # Age coefficient
    age_est <- summary_df %>%
      filter(term == "age") %>%
      pull(mean)
    age_ci <- summary_df %>%
      filter(term == "age") %>%
      select(`5%`, `95%`) %>%
      as.numeric()

    # Age-squared coefficient (unscaled)
    age_sq_sd <- sd(bhapc_df$age_squared)
    age_sq_est <- (summary_df %>%
      filter(term == "scale(age_squared)") %>%
      pull(mean)) / age_sq_sd
    age_sq_ci <- (summary_df %>%
      filter(term == "scale(age_squared)") %>%
      select(`5%`, `95%`) %>%
      as.numeric()) / age_sq_sd

    tibble(
      survey = toupper(survey),
      age_coef = round(age_est, 4),
      age_ci_lower = round(age_ci[1], 4),
      age_ci_upper = round(age_ci[2], 4),
      age_sq_coef = round(age_sq_est, 6),
      age_sq_ci_lower = round(age_sq_ci[1], 6),
      age_sq_ci_upper = round(age_sq_ci[2], 6)
    )
  })

  bind_rows(results)
}


#' Format table for publication
#'
#' Applies formatting for publication-ready output
#'
#' @param table Data frame to format
#' @param format Character: "latex", "html", or "markdown"
#' @return Formatted table (kable or gt object)
format_table <- function(table, format = "markdown") {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::kable(table, format = format, digits = 4)
  } else {
    print(table)
  }
}
