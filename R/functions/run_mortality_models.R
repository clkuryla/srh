# ==============================================================================
# run_mortality_models.R
# Survey-weighted Cox proportional hazards models for SRH-mortality analysis
# ==============================================================================

library(dplyr)
library(tidyr)
library(survival)
library(survey)
library(rlang)

# ------------------------------------------------------------------------------
# SINGLE MODEL FITTING
# ------------------------------------------------------------------------------

#' Fit survey-weighted Cox PH model
#'
#' @description Fits age-indexed Cox proportional hazards model with
#'   SRH as the predictor. Uses survey weights via svycoxph().
#'
#' @param df Data frame with columns: age_at_survey, age_at_end, event, srh, weight
#' @param srh_var Name of SRH variable (default "srh")
#' @param weight_var Name of weight variable (default "weight")
#' @param weighted Whether to use survey weights (default TRUE)
#'
#' @return List with model results:
#'   - coef: Coefficient (log-hazard ratio)
#'   - hr: Hazard ratio
#'   - conf_low, conf_high: 95% CI for HR
#'   - p_value: P-value for coefficient
#'   - n: Sample size
#'   - n_events: Number of events (deaths)
#'
#' @export
run_cox_weighted <- function(df,
                             srh_var = "srh",
                             weight_var = "weight",
                             weighted = TRUE) {

  # Check required columns
  required <- c("age_at_survey", "age_at_end", "event", srh_var)
  if (weighted) required <- c(required, weight_var)

  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    rlang::abort(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Filter to complete cases for model variables
  df_model <- df %>%
    filter(
      !is.na(.data[[srh_var]]),
      !is.na(age_at_survey),
      !is.na(age_at_end),
      !is.na(event)
    )

  if (weighted) {
    df_model <- df_model %>% filter(!is.na(.data[[weight_var]]), .data[[weight_var]] > 0)
  }

  n <- nrow(df_model)
  n_events <- sum(df_model$event, na.rm = TRUE)

  # Return NA results if insufficient data
  if (n < 10 || n_events < 5) {
    return(list(
      coef = NA_real_,
      hr = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      p_value = NA_real_,
      n = n,
      n_events = n_events,
      convergence = FALSE
    ))
  }

  # Build model formula
  formula <- as.formula(paste0(
    "Surv(age_at_survey, age_at_end, event) ~ ", srh_var
  ))

  # Fit model
  tryCatch({
    if (weighted) {
      # Survey-weighted Cox PH
      design <- svydesign(
        ids = ~1,
        weights = as.formula(paste0("~", weight_var)),
        data = df_model
      )
      fit <- svycoxph(formula, design = design)
    } else {
      # Unweighted Cox PH
      fit <- coxph(formula, data = df_model)
    }

    # Extract results
    fit_summary <- summary(fit)

    # Check that SRH is in the model
    if (!srh_var %in% rownames(fit_summary$coefficients)) {
      return(list(
        coef = NA_real_,
        hr = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        p_value = NA_real_,
        n = n,
        n_events = n_events,
        convergence = FALSE
      ))
    }

    list(
      coef = fit_summary$coefficients[srh_var, "coef"],
      hr = fit_summary$conf.int[srh_var, "exp(coef)"],
      conf_low = fit_summary$conf.int[srh_var, "lower .95"],
      conf_high = fit_summary$conf.int[srh_var, "upper .95"],
      p_value = fit_summary$coefficients[srh_var, "Pr(>|z|)"],
      n = n,
      n_events = n_events,
      convergence = TRUE
    )

  }, error = function(e) {
    list(
      coef = NA_real_,
      hr = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      p_value = NA_real_,
      n = n,
      n_events = n_events,
      convergence = FALSE,
      error_msg = conditionMessage(e)
    )
  })
}

# ------------------------------------------------------------------------------
# SLIDING WINDOW ANALYSIS
# ------------------------------------------------------------------------------

#' Run sliding window analysis by age group
#'
#' @description Fits Cox models for each combination of time window and
#'   age group, returning a tidy tibble of results.
#'
#' @param data Prepared mortality data from prepare_mortality_data()
#' @param start_years Vector of window start years
#' @param window_length Length of each window in years
#' @param follow_up_years Length of mortality follow-up (default same as window)
#' @param weighted Whether to use survey weights (default TRUE)
#' @param weight_var Name of weight variable (default "weight")
#' @param min_events Minimum deaths required to run model (default 10)
#' @param min_n Minimum sample size required (default 50)
#' @param verbose Print progress messages (default TRUE)
#'
#' @return Tibble with columns:
#'   - start_year, end_year, window_length
#'   - age_group
#'   - coef, hr, conf_low, conf_high, p_value
#'   - n, n_events
#'
#' @export
run_sliding_windows_by_age <- function(data,
                                       start_years,
                                       window_length,
                                       follow_up_years = NULL,
                                       weighted = TRUE,
                                       weight_var = "weight",
                                       min_events = 10,
                                       min_n = 50,
                                       verbose = TRUE) {

  if (is.null(follow_up_years)) {
    follow_up_years <- window_length
  }

  # Get unique age groups
  age_groups <- levels(data$age_group)
  if (is.null(age_groups)) {
    age_groups <- unique(data$age_group)
  }

  # Initialize results
  results <- tibble(
    start_year = integer(),
    end_year = integer(),
    window_length = integer(),
    follow_up_years = integer(),
    age_group = character(),
    coef = numeric(),
    hr = numeric(),
    conf_low = numeric(),
    conf_high = numeric(),
    p_value = numeric(),
    n = integer(),
    n_events = integer(),
    converged = logical()
  )

  n_windows <- length(start_years)
  n_groups <- length(age_groups)

  if (verbose) {
    message(sprintf(
      "Running %d windows x %d age groups = %d models (window_length=%d, follow_up=%d)",
      n_windows, n_groups, n_windows * n_groups, window_length, follow_up_years
    ))
  }

  # Loop over windows
  for (i in seq_along(start_years)) {
    sy <- start_years[i]

    if (verbose && i %% 5 == 1) {
      message(sprintf("  Processing window starting %d (%d of %d)...", sy, i, n_windows))
    }

    # Prepare window data
    df_window <- prepare_window(
      data,
      start_year = sy,
      window_length = window_length,
      follow_up_years = follow_up_years
    )

    if (nrow(df_window) == 0) next

    # Loop over age groups
    for (ag in age_groups) {
      df_age <- df_window %>% filter(age_group == ag)

      n <- nrow(df_age)
      n_events <- sum(df_age$event, na.rm = TRUE)

      # Skip if insufficient data
      if (n < min_n || n_events < min_events) {
        results <- results %>%
          add_row(
            start_year = sy,
            end_year = sy + window_length - 1L,
            window_length = as.integer(window_length),
            follow_up_years = as.integer(follow_up_years),
            age_group = as.character(ag),
            coef = NA_real_,
            hr = NA_real_,
            conf_low = NA_real_,
            conf_high = NA_real_,
            p_value = NA_real_,
            n = as.integer(n),
            n_events = as.integer(n_events),
            converged = FALSE
          )
        next
      }

      # Fit model
      model_result <- run_cox_weighted(
        df_age,
        srh_var = "srh",
        weight_var = weight_var,
        weighted = weighted
      )

      results <- results %>%
        add_row(
          start_year = sy,
          end_year = sy + window_length - 1L,
          window_length = as.integer(window_length),
          follow_up_years = as.integer(follow_up_years),
          age_group = as.character(ag),
          coef = model_result$coef,
          hr = model_result$hr,
          conf_low = model_result$conf_low,
          conf_high = model_result$conf_high,
          p_value = model_result$p_value,
          n = as.integer(model_result$n),
          n_events = as.integer(model_result$n_events),
          converged = model_result$convergence
        )
    }
  }

  # Convert age_group back to factor with original levels
  results <- results %>%
    mutate(age_group = factor(age_group, levels = age_groups))

  if (verbose) {
    n_converged <- sum(results$converged, na.rm = TRUE)
    message(sprintf(
      "Completed: %d models converged out of %d attempted",
      n_converged, nrow(results)
    ))
  }

  return(results)
}

# ------------------------------------------------------------------------------
# MULTIPLE WINDOW LENGTHS
# ------------------------------------------------------------------------------

#' Run analysis for multiple window lengths
#'
#' @description Convenience function to run sliding window analysis for
#'   multiple window lengths (e.g., 1, 2, 3, 5, 10, 15 years).
#'
#' @param data Prepared mortality data
#' @param window_lengths Vector of window lengths to analyze
#' @param ... Additional arguments passed to run_sliding_windows_by_age()
#'
#' @return Tibble combining results from all window lengths
#' @export
run_multiple_windows <- function(data,
                                 window_lengths = c(1, 2, 3, 5, 10, 15),
                                 ...) {

  min_year <- min(data$survey_year, na.rm = TRUE)
  max_year <- max(data$survey_year, na.rm = TRUE)

  results_list <- lapply(window_lengths, function(wl) {
    message(sprintf("\n=== Window length: %d years ===", wl))
    start_years <- get_window_starts(min_year, max_year, wl)

    run_sliding_windows_by_age(
      data,
      start_years = start_years,
      window_length = wl,
      ...
    )
  })

  bind_rows(results_list)
}

# ------------------------------------------------------------------------------
# SUMMARY AND DIAGNOSTICS
# ------------------------------------------------------------------------------

#' Summarize mortality model results
#'
#' @param results Results from run_sliding_windows_by_age()
#' @return Summary tibble by age group
#' @export
summarize_mortality_results <- function(results) {
  results %>%
    filter(converged) %>%
    group_by(age_group, window_length) %>%
    summarise(
      n_windows = n(),
      mean_coef = mean(coef, na.rm = TRUE),
      sd_coef = sd(coef, na.rm = TRUE),
      mean_hr = mean(hr, na.rm = TRUE),
      median_n = median(n, na.rm = TRUE),
      median_events = median(n_events, na.rm = TRUE),
      .groups = "drop"
    )
}
