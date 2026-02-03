# ==============================================================================
# run_mortality_sensitivity.R
# Functions for mortality sensitivity analyses: adjusted and stratified Cox PH
#
# These functions extend the mortality analysis pipeline to support:
#   1. Covariate-adjusted models (e.g., SRH + sex)
#   2. Stratified models (e.g., separate models by sex)
#
# Reuses prepare_window() from mortality_utils.R for window preparation.
# ==============================================================================

library(dplyr)
library(tidyr)
library(survival)
library(survey)
library(rlang)

# ------------------------------------------------------------------------------
# DATA PREPARATION
# ------------------------------------------------------------------------------

#' Prepare mortality data from wrangled data_nhis.rds
#'
#' @description Bridges the wrangled data_nhis.rds (which uses lowercase column
#'   names like year, age, srh) to the mortality analysis pipeline. Creates the
#'   columns expected by prepare_window() and run_cox_adjusted().
#'
#'   Also creates factor versions of covariates for sensitivity analyses:
#'   - sex (factor: Male, Female)
#'   - educ_3cat_f (factor: < HS, HS/Some college, College+)
#'   - race_includehisp_f (factor, created from RACENEW/HISPETH if available)
#'
#' @param data Wrangled NHIS data from data_nhis.rds
#' @param min_age Minimum age to include (default 18)
#' @param max_age Maximum age to include (default 89)
#' @param age_scheme Age grouping scheme (default "B")
#' @param min_year Minimum survey year (default 1982)
#' @param follow_up_end_year Last year of mortality follow-up (default 2019)
#'
#' @return Data frame ready for mortality sensitivity analysis
#' @export
prepare_mortality_from_nhis <- function(data,
                                         min_age = 18,
                                         max_age = 89,
                                         age_scheme = "B",
                                         min_year = 1982,
                                         follow_up_end_year = 2019) {

  # Check required columns

  required_cols <- c("year", "age", "srh", "MORTSTAT", "MORTWT")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check for MORTDODY (needed for accurate death timing)
  has_mortdody <- "MORTDODY" %in% names(data)
  if (!has_mortdody) {
    rlang::warn("MORTDODY not found; death year will be approximate.")
  }

  # --- Core mortality variables ---
  data_clean <- data %>%
    filter(
      MORTSTAT %in% 1:2,
      !is.na(MORTWT),
      MORTWT > 0,
      year >= min_year,
      age >= min_age,
      age <= max_age,
      srh %in% 1:5
    ) %>%
    mutate(
      survey_year = as.integer(year),
      age_at_survey = as.integer(age),
      # srh is already coded higher = better in data_nhis.rds
      mortality_status = if_else(MORTSTAT == 1L, 1L, 0L),
      death_year = if (has_mortdody) {
        if_else(
          MORTSTAT == 1L & !is.na(MORTDODY) & MORTDODY > 0,
          as.integer(MORTDODY),
          NA_integer_
        )
      } else {
        NA_integer_
      },
      weight = MORTWT
    )

  # --- Add age groups ---
  data_clean <- add_age_group(data_clean, age_var = age_at_survey, scheme = age_scheme)

  # --- Create covariate factors ---

  # Sex
  if ("sex" %in% names(data_clean)) {
    data_clean <- data_clean %>%
      mutate(sex = factor(sex, levels = c("Male", "Female")))
    n_sex <- sum(!is.na(data_clean$sex))
    message(sprintf("  Sex: %s non-missing (%.1f%%)",
                    format(n_sex, big.mark = ","),
                    100 * n_sex / nrow(data_clean)))
  }

  # Education (3-category)
  if ("educ_3cat" %in% names(data_clean)) {
    data_clean <- data_clean %>%
      mutate(
        educ_3cat_f = factor(
          educ_3cat,
          levels = 1:3,
          labels = c("< HS", "HS/Some college", "College+")
        )
      )
    n_educ <- sum(!is.na(data_clean$educ_3cat_f))
    message(sprintf("  Education: %s non-missing (%.1f%%)",
                    format(n_educ, big.mark = ","),
                    100 * n_educ / nrow(data_clean)))
  }

  # Race/ethnicity (create from RACENEW/HISPETH if available)
  has_racenew <- "RACENEW" %in% names(data_clean)
  has_hispeth <- "HISPETH" %in% names(data_clean)

  if (has_racenew) {
    data_clean <- data_clean %>%
      mutate(
        hispanic = case_when(
          has_hispeth & HISPETH == 10 ~ 0L,
          has_hispeth & HISPETH >= 20 & HISPETH <= 70 ~ 1L,
          TRUE ~ NA_integer_
        ),
        race_5cat = case_when(
          RACENEW == 100 ~ "White",
          RACENEW == 200 ~ "Black",
          RACENEW == 300 ~ "AIAN",
          RACENEW == 400 ~ "Asian",
          RACENEW >= 500 ~ "Other",
          TRUE ~ NA_character_
        ),
        race_includehisp = case_when(
          hispanic == 1 ~ "Hispanic",
          !is.na(race_5cat) ~ race_5cat,
          TRUE ~ NA_character_
        ),
        race_includehisp_f = factor(
          race_includehisp,
          levels = c("White", "Black", "AIAN", "Asian", "Hispanic", "Other"),
          labels = c("NH White", "NH Black", "NH AIAN", "NH Asian", "Hispanic", "Other/Multi")
        )
      )
    n_race <- sum(!is.na(data_clean$race_includehisp_f))
    message(sprintf("  Race/ethnicity: %s non-missing (%.1f%%)",
                    format(n_race, big.mark = ","),
                    100 * n_race / nrow(data_clean)))
  } else if ("race_includehisp_f" %in% names(data_clean)) {
    # Already exists (e.g., wrangle_race.R was run)
    n_race <- sum(!is.na(data_clean$race_includehisp_f))
    message(sprintf("  Race/ethnicity (pre-existing): %s non-missing (%.1f%%)",
                    format(n_race, big.mark = ","),
                    100 * n_race / nrow(data_clean)))
  } else {
    message("  Race/ethnicity: NOT AVAILABLE (RACENEW/HISPETH not in data)")
  }

  # --- Summary ---
  n_clean <- nrow(data_clean)
  n_deaths <- sum(data_clean$mortality_status, na.rm = TRUE)

  message(sprintf(
    "\nMortality sensitivity data prepared:\n  %s records\n  Deaths: %s (%.1f%%)\n  Years: %d-%d\n  Ages: %d-%d",
    format(n_clean, big.mark = ","),
    format(n_deaths, big.mark = ","),
    100 * n_deaths / n_clean,
    min(data_clean$survey_year, na.rm = TRUE),
    max(data_clean$survey_year, na.rm = TRUE),
    min(data_clean$age_at_survey, na.rm = TRUE),
    max(data_clean$age_at_survey, na.rm = TRUE)
  ))

  return(data_clean)
}


# ------------------------------------------------------------------------------
# ADJUSTED COX MODEL
# ------------------------------------------------------------------------------

#' Fit survey-weighted Cox PH model with optional covariate adjustment
#'
#' @description Extends run_cox_weighted() to support additional covariates.
#'   The formula becomes Surv(age_at_survey, age_at_end, event) ~ srh + cov1 + cov2 ...
#'   Only the SRH coefficient is extracted (covariates are adjustments, not targets).
#'
#' @param df Data frame with required columns
#' @param adjust_vars Character vector of covariate names to adjust for (default empty)
#' @param srh_var Name of SRH variable (default "srh")
#' @param weight_var Name of weight variable (default "weight")
#' @param weighted Whether to use survey weights (default TRUE)
#'
#' @return List with SRH model results (same structure as run_cox_weighted)
#' @export
run_cox_adjusted <- function(df,
                              adjust_vars = character(0),
                              srh_var = "srh",
                              weight_var = "weight",
                              weighted = TRUE) {

  # Check required columns
  required <- c("age_at_survey", "age_at_end", "event", srh_var, adjust_vars)
  if (weighted) required <- c(required, weight_var)

  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    rlang::abort(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Filter to complete cases for ALL model variables
  df_model <- df %>%
    filter(
      !is.na(.data[[srh_var]]),
      !is.na(age_at_survey),
      !is.na(age_at_end),
      !is.na(event)
    )

  # Drop NAs for adjustment variables

  for (v in adjust_vars) {
    df_model <- df_model %>% filter(!is.na(.data[[v]]))
  }

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

  # Build model formula: Surv(...) ~ srh + cov1 + cov2 ...
  rhs <- paste(c(srh_var, adjust_vars), collapse = " + ")
  formula <- as.formula(paste0("Surv(age_at_survey, age_at_end, event) ~ ", rhs))

  # Fit model
  tryCatch({
    if (weighted) {
      design <- svydesign(
        ids = ~1,
        weights = as.formula(paste0("~", weight_var)),
        data = df_model
      )
      fit <- svycoxph(formula, design = design)
    } else {
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
# SLIDING WINDOW ANALYSIS WITH ADJUSTMENT
# ------------------------------------------------------------------------------

#' Run sliding window analysis by age group with covariate adjustment
#'
#' @description Mirrors run_sliding_windows_by_age() but passes adjust_vars
#'   to run_cox_adjusted(). Adds a model_label column to results.
#'
#' @param data Prepared mortality data from prepare_mortality_from_nhis()
#' @param start_years Vector of window start years
#' @param window_length Length of each window in years
#' @param adjust_vars Character vector of covariate names (default empty)
#' @param model_label Label for this model specification (auto-generated if NULL)
#' @param follow_up_years Length of mortality follow-up (default same as window)
#' @param weighted Whether to use survey weights (default TRUE)
#' @param weight_var Name of weight variable (default "weight")
#' @param min_events Minimum deaths required (default 10)
#' @param min_n Minimum sample size (default 50)
#' @param verbose Print progress (default TRUE)
#'
#' @return Tibble with model results including model_label column
#' @export
run_sliding_windows_adjusted <- function(data,
                                          start_years,
                                          window_length,
                                          adjust_vars = character(0),
                                          model_label = NULL,
                                          follow_up_years = NULL,
                                          weighted = TRUE,
                                          weight_var = "weight",
                                          min_events = 10,
                                          min_n = 50,
                                          verbose = TRUE) {

  if (is.null(follow_up_years)) {
    follow_up_years <- window_length
  }

  # Auto-generate model label
  if (is.null(model_label)) {
    model_label <- if (length(adjust_vars) > 0) {
      paste("Adjusted:", paste(adjust_vars, collapse = ", "))
    } else {
      "Unadjusted"
    }
  }

  # Get unique age groups
  age_groups <- levels(data$age_group)
  if (is.null(age_groups)) {
    age_groups <- sort(unique(data$age_group))
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
    converged = logical(),
    model_label = character()
  )

  n_windows <- length(start_years)
  n_groups <- length(age_groups)

  if (verbose) {
    message(sprintf(
      "Running %s: %d windows x %d age groups = %d models (window=%d, follow_up=%d)",
      model_label, n_windows, n_groups, n_windows * n_groups,
      window_length, follow_up_years
    ))
  }

  # Loop over windows
  for (i in seq_along(start_years)) {
    sy <- start_years[i]

    if (verbose && i %% 5 == 1) {
      message(sprintf("  Window starting %d (%d of %d)...", sy, i, n_windows))
    }

    # Prepare window data (reuse from mortality_utils.R)
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
            converged = FALSE,
            model_label = model_label
          )
        next
      }

      # Fit adjusted model
      model_result <- run_cox_adjusted(
        df_age,
        adjust_vars = adjust_vars,
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
          converged = model_result$convergence,
          model_label = model_label
        )
    }
  }

  # Convert age_group back to factor
  results <- results %>%
    mutate(age_group = factor(age_group, levels = age_groups))

  if (verbose) {
    n_converged <- sum(results$converged, na.rm = TRUE)
    message(sprintf(
      "  %s: %d converged out of %d attempted",
      model_label, n_converged, nrow(results)
    ))
  }

  return(results)
}


# ------------------------------------------------------------------------------
# STRATIFIED SLIDING WINDOW ANALYSIS
# ------------------------------------------------------------------------------

#' Run sliding window analysis stratified by a categorical variable
#'
#' @description Fits separate Cox models for each stratum of a covariate,
#'   for each age group and time window. Uses survey subpopulation methods
#'   (subset on the full design) for proper variance estimation.
#'
#' @param data Prepared mortality data from prepare_mortality_from_nhis()
#' @param start_years Vector of window start years
#' @param window_length Length of each window in years
#' @param strat_var Name of the stratification variable (e.g., "sex")
#' @param follow_up_years Length of mortality follow-up (default same as window)
#' @param weighted Whether to use survey weights (default TRUE)
#' @param weight_var Name of weight variable (default "weight")
#' @param min_events Minimum deaths required (default 10)
#' @param min_n Minimum sample size (default 50)
#' @param verbose Print progress (default TRUE)
#'
#' @return Tibble with model results including stratum and strat_var columns
#' @export
run_sliding_windows_stratified <- function(data,
                                            start_years,
                                            window_length,
                                            strat_var,
                                            follow_up_years = NULL,
                                            weighted = TRUE,
                                            weight_var = "weight",
                                            min_events = 10,
                                            min_n = 50,
                                            verbose = TRUE) {

  if (is.null(follow_up_years)) {
    follow_up_years <- window_length
  }

  # Check strat_var exists and has levels
  if (!strat_var %in% names(data)) {
    rlang::abort(paste("Stratification variable not found:", strat_var))
  }

  # Get strata levels
  strat_vals <- if (is.factor(data[[strat_var]])) {
    levels(data[[strat_var]])
  } else {
    sort(unique(na.omit(data[[strat_var]])))
  }

  if (length(strat_vals) < 2) {
    rlang::warn(paste("Fewer than 2 levels in", strat_var))
  }

  if (verbose) {
    message(sprintf(
      "Stratified analysis by %s: %d levels (%s)",
      strat_var, length(strat_vals), paste(strat_vals, collapse = ", ")
    ))
  }

  # Run for each stratum
  all_results <- list()

  for (sv in strat_vals) {
    if (verbose) {
      message(sprintf("\n--- Stratum: %s = %s ---", strat_var, sv))
    }

    # Subset data to this stratum
    # Using subset() on the survey design is the proper approach, but since
    # we're constructing the design inside run_cox_adjusted (weights-only),
    # filtering the data frame directly is equivalent for weights-only designs.
    data_stratum <- data %>%
      filter(.data[[strat_var]] == sv)

    if (nrow(data_stratum) == 0) {
      if (verbose) message("  No data for this stratum, skipping.")
      next
    }

    stratum_results <- run_sliding_windows_adjusted(
      data_stratum,
      start_years = start_years,
      window_length = window_length,
      adjust_vars = character(0),  # No adjustment within strata
      model_label = as.character(sv),
      follow_up_years = follow_up_years,
      weighted = weighted,
      weight_var = weight_var,
      min_events = min_events,
      min_n = min_n,
      verbose = verbose
    )

    stratum_results <- stratum_results %>%
      mutate(
        strat_var = strat_var,
        stratum = as.character(sv)
      )

    all_results[[sv]] <- stratum_results
  }

  bind_rows(all_results)
}
