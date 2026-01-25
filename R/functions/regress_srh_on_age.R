# ==============================================================================
# regress_srh_on_age.R
# Figure 1 Panel B: Age Coefficient on SRH Over Time
#
# Purpose: Run survey-weighted linear regressions of SRH ~ age for each year
#          within a survey, extracting the age coefficient to show how the
#          relationship between age and self-rated health changes over time.
#
# Key insight: If the age coefficient trends toward zero, age is becoming
#              less predictive of SRH (the convergence phenomenon).
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(survey)
library(broom)

# ------------------------------------------------------------------------------
# YEARLY REGRESSION FUNCTION
# ------------------------------------------------------------------------------

#' Run survey-weighted regression of SRH on age for each year
#'
#' @description
#' For each year in the dataset, fits a survey-weighted linear model:
#'   SRH ~ age
#' and extracts the age coefficient with standard error and confidence interval.
#'
#' @param data Data frame with columns: srh, age, year, wt (and optional psu, strata)
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_var Name of age variable (default "age")
#' @param year_var Name of year variable (default "year")
#' @param psu_var Name of PSU variable (default "psu", NULL if not available)
#' @param strata_var Name of strata variable (default "strata", NULL if not available)
#' @param wt_var Name of weight variable (default "wt")
#' @param ci_level Confidence level for intervals (default 0.95)
#' @param lonely_psu How to handle single-PSU strata (default "adjust")
#'
#' @return Data frame with columns:
#'   - survey: Survey name
#'   - year: Survey year
#'   - coefficient: Age coefficient (effect of 1-year increase in age on SRH)
#'   - se: Standard error of coefficient
#'   - t_value: t-statistic
#'   - p_value: p-value for coefficient
#'   - ci_lower, ci_upper: Confidence interval bounds
#'   - n_unweighted: Unweighted sample size for that year
#'
#' @details
#' The model is: SRH ~ age (simple linear regression, no covariates)
#'
#' Interpretation:
#' - Negative coefficient: Older people report worse health (typical finding)
#' - Coefficient trending toward zero: Age becoming less predictive over time
#'
#' Survey design elements (PSU, strata) are used when available. If not,
#' a weights-only design is used with a warning.
#'
#' @examples
#' # coefficients <- regress_age_coefficient_by_year(data_nhis, "NHIS")
#'
regress_age_coefficient_by_year <- function(
    data,
    survey_name,
    srh_var = "srh",
    age_var = "age",
    year_var = "year",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    ci_level = 0.95,
    lonely_psu = "adjust"
) {

  # --- Input validation ---
  stopifnot(is.data.frame(data))
  stopifnot(srh_var %in% names(data))
  stopifnot(age_var %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))

  # --- Set survey options ---
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # --- Check which design elements are available ---
  has_psu <- !is.null(psu_var) && psu_var %in% names(data)
  has_strata <- !is.null(strata_var) && strata_var %in% names(data)

  if (!has_psu || !has_strata) {
    message("Note: ", survey_name, " - Using weights-only design. ",
            "PSU: ", has_psu, ", Strata: ", has_strata)
  }

  # --- Get unique years ---
  years <- sort(unique(data[[year_var]]))
  message("Processing ", survey_name, ": ", length(years), " years (",
          min(years), "-", max(years), ")")

  # --- Run regression for each year ---
  results_list <- vector("list", length(years))

  for (i in seq_along(years)) {
    yr <- years[i]

    # Subset to this year
    data_year <- data[data[[year_var]] == yr, , drop = FALSE]

    # Filter to valid observations
    valid_mask <- !is.na(data_year[[srh_var]]) &
      !is.na(data_year[[age_var]]) &
      !is.na(data_year[[wt_var]]) &
      data_year[[wt_var]] > 0

    if (has_psu) valid_mask <- valid_mask & !is.na(data_year[[psu_var]])
    if (has_strata) valid_mask <- valid_mask & !is.na(data_year[[strata_var]])

    data_year <- data_year[valid_mask, , drop = FALSE]
    n_unweighted <- nrow(data_year)

    # Skip if too few observations
    if (n_unweighted < 30) {
      warning("Year ", yr, " has only ", n_unweighted, " observations. Skipping.")
      next
    }

    # --- Create survey design ---
    tryCatch({
      if (has_psu && has_strata) {
        svy_design <- svydesign(
          ids = as.formula(paste0("~", psu_var)),
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_year,
          nest = TRUE
        )
      } else if (has_psu) {
        svy_design <- svydesign(
          ids = as.formula(paste0("~", psu_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_year
        )
      } else if (has_strata) {
        svy_design <- svydesign(
          ids = ~1,
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_year
        )
      } else {
        svy_design <- svydesign(
          ids = ~1,
          weights = as.formula(paste0("~", wt_var)),
          data = data_year
        )
      }

      # --- Fit the model: SRH ~ age ---
      formula <- as.formula(paste0(srh_var, " ~ ", age_var))
      model <- svyglm(formula, design = svy_design)

      # --- Extract age coefficient ---
      coef_summary <- summary(model)$coefficients
      age_row <- coef_summary[age_var, ]

      coefficient <- age_row["Estimate"]
      se <- age_row["Std. Error"]
      t_value <- age_row["t value"]
      p_value <- age_row["Pr(>|t|)"]

      # Confidence interval
      ci <- confint(model, level = ci_level)[age_var, ]
      ci_lower <- ci[1]
      ci_upper <- ci[2]

      results_list[[i]] <- data.frame(
        survey = survey_name,
        year = yr,
        coefficient = coefficient,
        se = se,
        t_value = t_value,
        p_value = p_value,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        n_unweighted = n_unweighted,
        stringsAsFactors = FALSE
      )

    }, error = function(e) {
      warning("Year ", yr, " failed: ", e$message)
    })
  }

  # --- Combine results ---
  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    stop("No valid results for ", survey_name)
  }

  message("  Completed: ", nrow(coefficients), " years with valid coefficients")

  return(coefficients)
}


# ------------------------------------------------------------------------------
# METAREGRESSION FUNCTION
# ------------------------------------------------------------------------------

#' Run metaregression on age coefficients over time
#'
#' @description
#' Regresses the age coefficients on year using inverse-variance weighting
#' to quantify the trend in the age-SRH relationship over time.
#'
#' @param coefficients Data frame output from regress_age_coefficient_by_year()
#' @param survey_name Character string for labeling (uses first survey if NULL)
#'
#' @return Data frame with metaregression results:
#'   - survey: Survey name
#'   - slope: Change in age coefficient per year
#'   - slope_se: Standard error of slope
#'   - slope_p_value: p-value for slope
#'   - intercept: Intercept (coefficient at year 0, not meaningful)
#'   - r_squared: R-squared of the metaregression
#'   - n_years: Number of years included
#'   - year_range: Range of years
#'   - interpretation: Plain-language interpretation
#'
#' @details
#' Uses inverse-variance weighting: weight = 1 / SE^2
#'
#' A positive slope means the (typically negative) age coefficient is
#' becoming less negative over time, i.e., trending toward zero.
#'
#' @examples
#' # meta <- run_metaregression(coefficients_nhis, "NHIS")
#'
run_metaregression <- function(coefficients, survey_name = NULL) {

  # --- Input validation ---
  stopifnot(is.data.frame(coefficients))
  required_cols <- c("year", "coefficient", "se")
  stopifnot(all(required_cols %in% names(coefficients)))

  # Use survey name from data if not provided

  if (is.null(survey_name) && "survey" %in% names(coefficients)) {
    survey_name <- unique(coefficients$survey)[1]
  }
  if (is.null(survey_name)) {
    survey_name <- "Unknown"
  }

  # --- Compute inverse-variance weights ---
  # Weight = 1 / SE^2
  coefficients <- coefficients %>%
    filter(!is.na(se), se > 0) %>%
    mutate(iv_weight = 1 / (se^2))

  if (nrow(coefficients) < 3) {
    warning("Fewer than 3 valid years for metaregression in ", survey_name)
    return(NULL)
  }

  # --- Run weighted linear regression: coefficient ~ year ---
  meta_model <- lm(coefficient ~ year, data = coefficients, weights = iv_weight)
  meta_summary <- summary(meta_model)

  # --- Extract results ---
  slope <- coef(meta_model)["year"]
  slope_se <- meta_summary$coefficients["year", "Std. Error"]
  slope_p <- meta_summary$coefficients["year", "Pr(>|t|)"]
  intercept <- coef(meta_model)["(Intercept)"]
  r_squared <- meta_summary$r.squared

  # --- Interpretation ---
  # If original coefficient is negative (older = worse health):
  #   - Positive slope = coefficient becoming less negative = trending toward 0
  #   - Negative slope = coefficient becoming more negative = diverging from 0
  if (slope > 0) {
    interpretation <- "Age coefficient trending toward zero (convergence)"
  } else if (slope < 0) {
    interpretation <- "Age coefficient trending away from zero (divergence)"
  } else {
    interpretation <- "No significant trend"
  }

  result <- data.frame(
    survey = survey_name,
    slope = slope,
    slope_se = slope_se,
    slope_p_value = slope_p,
    intercept = intercept,
    r_squared = r_squared,
    n_years = nrow(coefficients),
    year_min = min(coefficients$year),
    year_max = max(coefficients$year),
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL

  return(result)
}


#' Run pooled metaregression across all surveys
#'
#' @description
#' Pools coefficients from all surveys and runs a single metaregression
#' to estimate the overall trend in the age-SRH relationship.
#'
#' @param coefficients_list Named list of coefficient data frames (one per survey)
#'
#' @return Data frame with pooled metaregression results
#'
#' @details
#' Coefficients from all surveys are combined and regressed on year
#' using inverse-variance weighting. This gives an overall estimate
#' of the trend, assuming the trend is similar across surveys.
#'
run_pooled_metaregression <- function(coefficients_list) {

  # Combine all coefficients
  all_coefficients <- dplyr::bind_rows(coefficients_list, .id = "survey")

  # Run metaregression on pooled data
  result <- run_metaregression(all_coefficients, survey_name = "Pooled (all surveys)")

  return(result)
}


# ------------------------------------------------------------------------------
# TABLE EXPORT HELPER
# ------------------------------------------------------------------------------

#' Save coefficient table to CSV and RDS
#'
#' @param coefficients Data frame of coefficients
#' @param survey_name Character. Survey name for filename
#' @param output_dir Character. Directory to save files.
#' @param date_suffix Logical. Add date suffix to filename?
#'
save_coefficients_table <- function(
    coefficients,
    survey_name,
    output_dir = here::here("output", "tables"),
    date_suffix = TRUE
) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_name <- paste0("fig1b_coefficients_", tolower(survey_name))
  if (date_suffix) {
    base_name <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"))
  }

  # Round for CSV
  coefficients_rounded <- coefficients %>%
    mutate(across(where(is.numeric), ~ round(.x, 6)))

  csv_path <- file.path(output_dir, paste0(base_name, ".csv"))
  readr::write_csv(coefficients_rounded, csv_path)
  message("Saved: ", csv_path)

  rds_path <- file.path(output_dir, paste0(base_name, ".rds"))
  readr::write_rds(coefficients, rds_path)
  message("Saved: ", rds_path)

  invisible(NULL)
}


#' Save all survey coefficient tables
#'
#' @param coefficients_list Named list of coefficient data frames
#' @param output_dir Character. Directory to save files.
#' @param date_suffix Logical. Add date suffix to filenames?
#'
save_all_coefficients_tables <- function(
    coefficients_list,
    output_dir = here::here("output", "tables"),
    date_suffix = TRUE
) {

  for (svy in names(coefficients_list)) {
    save_coefficients_table(
      coefficients = coefficients_list[[svy]],
      survey_name = svy,
      output_dir = output_dir,
      date_suffix = date_suffix
    )
  }

  message("\nAll ", length(coefficients_list), " coefficient tables saved")
  invisible(NULL)
}


# ------------------------------------------------------------------------------
# LOGISTIC REGRESSION FUNCTION (for dichotomized SRH)
# ------------------------------------------------------------------------------

#' Run survey-weighted logistic regression of dichotomized SRH on age for each year
#'
#' @description
#' For each year in the dataset, fits a survey-weighted logistic model:
#'   SRH_dichot ~ age
#' and extracts the age coefficient with standard error and confidence interval.
#' Optionally computes marginal effects on the probability scale.
#'
#' @param data Data frame with columns: dichotomized SRH, age, year, wt (and optional psu, strata)
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param srh_dichot_var Name of dichotomized SRH variable (default "srh_dichot")
#' @param age_var Name of age variable (default "age")
#' @param year_var Name of year variable (default "year")
#' @param psu_var Name of PSU variable (default "psu", NULL if not available)
#' @param strata_var Name of strata variable (default "strata", NULL if not available)
#' @param wt_var Name of weight variable (default "wt")
#' @param ci_level Confidence level for intervals (default 0.95)
#' @param lonely_psu How to handle single-PSU strata (default "adjust")
#' @param compute_marginal Compute average marginal effect? (default TRUE)
#' @param age_increment Age increment for marginal effect (default 10 years)
#'
#' @return Data frame with columns:
#'   - survey: Survey name
#'   - year: Survey year
#'   - coefficient: Age coefficient on log-odds scale
#'   - se: Standard error of coefficient
#'   - ci_lower, ci_upper: Confidence interval bounds (log-odds)
#'   - marginal_effect: Average marginal effect (change in probability per age_increment years)
#'   - marginal_se: Standard error of marginal effect
#'   - n_unweighted: Unweighted sample size for that year
#'
#' @details
#' The model is: SRH_dichot ~ age (simple logistic regression, no covariates)
#'
#' Interpretation:
#' - With "higher = better" coding (1 = good health):
#'   - Negative coefficient: Older people have lower probability of good health (typical)
#'   - Coefficient trending toward zero: Age becoming less predictive (convergence)
#' - Marginal effect: Average change in predicted probability per `age_increment` year increase
#'
regress_age_coefficient_by_year_logistic <- function(
    data,
    survey_name,
    srh_dichot_var = "srh_dichot",
    age_var = "age",
    year_var = "year",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    ci_level = 0.95,
    lonely_psu = "adjust",
    compute_marginal = TRUE,
    age_increment = 10
) {

  # --- Input validation ---
  stopifnot(is.data.frame(data))
  stopifnot(srh_dichot_var %in% names(data))
  stopifnot(age_var %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))

  # --- Set survey options ---
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # --- Check which design elements are available ---
  has_psu <- !is.null(psu_var) && psu_var %in% names(data)
  has_strata <- !is.null(strata_var) && strata_var %in% names(data)

  if (!has_psu || !has_strata) {
    message("Note: ", survey_name, " - Using weights-only design. ",
            "PSU: ", has_psu, ", Strata: ", has_strata)
  }

  # --- Get unique years ---
  years <- sort(unique(data[[year_var]]))
  message("Processing ", survey_name, " (logistic): ", length(years), " years (",
          min(years), "-", max(years), ")")

  # --- Run regression for each year ---
  results_list <- vector("list", length(years))

  for (i in seq_along(years)) {
    yr <- years[i]

    # Subset to this year
    data_year <- data[data[[year_var]] == yr, , drop = FALSE]

    # Filter to valid observations
    valid_mask <- !is.na(data_year[[srh_dichot_var]]) &
      !is.na(data_year[[age_var]]) &
      !is.na(data_year[[wt_var]]) &
      data_year[[wt_var]] > 0

    if (has_psu) valid_mask <- valid_mask & !is.na(data_year[[psu_var]])
    if (has_strata) valid_mask <- valid_mask & !is.na(data_year[[strata_var]])

    data_year <- data_year[valid_mask, , drop = FALSE]
    n_unweighted <- nrow(data_year)

    # Skip if too few observations
    if (n_unweighted < 30) {
      warning("Year ", yr, " has only ", n_unweighted, " observations. Skipping.")
      next
    }

    # --- Create survey design ---
    tryCatch({
      if (has_psu && has_strata) {
        svy_design <- svydesign(
          ids = as.formula(paste0("~", psu_var)),
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_year,
          nest = TRUE
        )
      } else if (has_psu) {
        svy_design <- svydesign(
          ids = as.formula(paste0("~", psu_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_year
        )
      } else if (has_strata) {
        svy_design <- svydesign(
          ids = ~1,
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_year
        )
      } else {
        svy_design <- svydesign(
          ids = ~1,
          weights = as.formula(paste0("~", wt_var)),
          data = data_year
        )
      }

      # --- Fit the logistic model: SRH_dichot ~ age ---
      formula <- as.formula(paste0(srh_dichot_var, " ~ ", age_var))
      model <- svyglm(formula, design = svy_design, family = quasibinomial())

      # --- Extract age coefficient (log-odds scale) ---
      coef_summary <- summary(model)$coefficients
      age_row <- coef_summary[age_var, ]

      coefficient <- age_row["Estimate"]
      se <- age_row["Std. Error"]
      t_value <- age_row["t value"]
      p_value <- age_row["Pr(>|t|)"]

      # Confidence interval (log-odds scale)
      ci <- confint(model, level = ci_level)[age_var, ]
      ci_lower <- ci[1]
      ci_upper <- ci[2]

      # --- Compute average marginal effect (probability scale) ---
      marginal_effect <- NA_real_
      marginal_se <- NA_real_

      if (compute_marginal) {
        # Average marginal effect: mean of dP/d(age) across all observations
        # dP/d(age) = beta * p * (1 - p) where p = predicted probability
        # For age_increment years: multiply by age_increment

        beta <- coefficient
        pred_probs <- predict(model, type = "response")

        # Marginal effect for each observation
        me_individual <- beta * pred_probs * (1 - pred_probs) * age_increment

        # Average marginal effect (weighted)
        weights_vec <- weights(svy_design)
        marginal_effect <- weighted.mean(me_individual, w = weights_vec, na.rm = TRUE)

        # Approximate SE using delta method
        # Var(AME) approx = (AME/beta)^2 * Var(beta) for large samples
        if (!is.na(marginal_effect) && !is.na(se) && abs(coefficient) > 1e-10) {
          marginal_se <- abs(marginal_effect / coefficient) * se
        }
      }

      results_list[[i]] <- data.frame(
        survey = survey_name,
        year = yr,
        coefficient = coefficient,
        se = se,
        t_value = t_value,
        p_value = p_value,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        marginal_effect = marginal_effect,
        marginal_se = marginal_se,
        n_unweighted = n_unweighted,
        stringsAsFactors = FALSE
      )

    }, error = function(e) {
      warning("Year ", yr, " failed: ", e$message)
    })
  }

  # --- Combine results ---
  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    stop("No valid results for ", survey_name)
  }

  message("  Completed: ", nrow(coefficients), " years with valid coefficients")

  return(coefficients)
}


#' Run metaregression on logistic age coefficients over time
#'
#' @description
#' Wrapper for run_metaregression() that works with logistic regression output.
#' Can run on either log-odds coefficients or marginal effects.
#'
#' @param coefficients Data frame output from regress_age_coefficient_by_year_logistic()
#' @param survey_name Character string for labeling
#' @param use_marginal Use marginal_effect instead of coefficient? Default FALSE (log-odds)
#'
#' @return Data frame with metaregression results
#'
run_metaregression_logistic <- function(coefficients, survey_name = NULL, use_marginal = FALSE) {

  if (use_marginal) {
    # Use marginal effects
    if (!"marginal_effect" %in% names(coefficients) ||
        all(is.na(coefficients$marginal_effect))) {
      stop("Marginal effects not available in coefficients data frame")
    }

    coefficients_tmp <- coefficients %>%
      transmute(
        survey = survey,
        year = year,
        coefficient = marginal_effect,
        se = marginal_se
      ) %>%
      filter(!is.na(coefficient), !is.na(se))

  } else {
    # Use log-odds coefficients
    coefficients_tmp <- coefficients
  }

  run_metaregression(coefficients_tmp, survey_name = survey_name)
}


#' Save metaregression results table
#'
#' @param meta_results Data frame of metaregression results (one row per survey)
#' @param output_dir Character. Directory to save files.
#' @param date_suffix Logical. Add date suffix to filename?
#'
save_metaregression_table <- function(
    meta_results,
    output_dir = here::here("output", "tables"),
    date_suffix = TRUE
) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_name <- "fig1b_metaregression"
  if (date_suffix) {
    base_name <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"))
  }

  # Round for CSV
  meta_rounded <- meta_results %>%
    mutate(across(where(is.numeric), ~ round(.x, 6)))

  csv_path <- file.path(output_dir, paste0(base_name, ".csv"))
  readr::write_csv(meta_rounded, csv_path)
  message("Saved: ", csv_path)

  rds_path <- file.path(output_dir, paste0(base_name, ".rds"))
  readr::write_rds(meta_results, rds_path)
  message("Saved: ", rds_path)

  invisible(NULL)
}
