# ==============================================================================
# regress_covariate_by_year.R
# Figure 3: Coefficient Stability Analysis
#
# Purpose: Run survey-weighted regressions of SRH ~ covariate for each year
#          to show that the relationship between health covariates and SRH
#          remains stable over time (i.e., "what SRH measures" hasn't changed).
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(survey)
library(broom)

# ------------------------------------------------------------------------------
# MAIN REGRESSION FUNCTION
# ------------------------------------------------------------------------------

#' Run survey-weighted regression of SRH on a covariate for each year
#'
#' @description
#' For each year in the dataset, fits a survey-weighted linear model:
#'   SRH ~ covariate
#' and extracts the covariate coefficient with standard error and CI.
#'
#' @param data Data frame with columns: srh, covariate, year, wt (and optional psu, strata)
#' @param covariate_var Name of covariate variable (character string)
#' @param covariate_label Human-readable label for the covariate (for output)
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param srh_var Name of SRH variable (default "srh")
#' @param year_var Name of year variable (default "year")
#' @param psu_var Name of PSU variable (default "psu", NULL if not available)
#' @param strata_var Name of strata variable (default "strata", NULL if not available)
#' @param wt_var Name of weight variable (default "wt")
#' @param ci_level Confidence level for intervals (default 0.95)
#' @param lonely_psu How to handle single-PSU strata (default "adjust")
#' @param min_n Minimum observations per year to include (default 100)
#'
#' @return Data frame with columns:
#'   - survey: Survey name
#'   - covariate: Covariate name
#'   - covariate_label: Human-readable label
#'   - year: Survey year
#'   - coefficient: Covariate coefficient
#'   - se: Standard error of coefficient
#'   - ci_lower, ci_upper: Confidence interval bounds
#'   - n_unweighted: Unweighted sample size for that year
#'   - n_covariate_1: Count of covariate == 1 (for binary covariates)
#'
#' @details
#' The model is: SRH ~ covariate (simple linear regression, no additional covariates)
#'
#' Interpretation for binary covariates (0/1):
#' - Negative coefficient: Having the condition associated with worse SRH
#' - Stable coefficient over time: The meaning of SRH is constant
#'
regress_covariate_by_year <- function(
    data,
    covariate_var,
    covariate_label = NULL,
    survey_name,
    srh_var = "srh",
    year_var = "year",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    ci_level = 0.95,
    lonely_psu = "adjust",
    min_n = 100
) {

  # --- Input validation ---
  stopifnot(is.data.frame(data))
  stopifnot(srh_var %in% names(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))

  if (is.null(covariate_label)) {
    covariate_label <- covariate_var
  }

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
  message("Processing ", survey_name, " - ", covariate_label, ": ",
          length(years), " years (", min(years), "-", max(years), ")")

  # --- Run regression for each year ---
  results_list <- vector("list", length(years))

  for (i in seq_along(years)) {
    yr <- years[i]

    # Subset to this year
    data_year <- data[data[[year_var]] == yr, , drop = FALSE]

    # Filter to valid observations
    valid_mask <- !is.na(data_year[[srh_var]]) &
      !is.na(data_year[[covariate_var]]) &
      !is.na(data_year[[wt_var]]) &
      data_year[[wt_var]] > 0

    if (has_psu) valid_mask <- valid_mask & !is.na(data_year[[psu_var]])
    if (has_strata) valid_mask <- valid_mask & !is.na(data_year[[strata_var]])

    data_year <- data_year[valid_mask, , drop = FALSE]
    n_unweighted <- nrow(data_year)

    # Count covariate == 1 (for binary variables)
    n_covariate_1 <- sum(data_year[[covariate_var]] == 1, na.rm = TRUE)

    # Skip if too few observations
    if (n_unweighted < min_n) {
      message("  Year ", yr, " has only ", n_unweighted, " observations. Skipping.")
      next
    }

    # Skip if no variation in covariate
    if (length(unique(data_year[[covariate_var]])) < 2) {
      message("  Year ", yr, " has no variation in covariate. Skipping.")
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

      # --- Fit the model: SRH ~ covariate ---
      formula <- as.formula(paste0(srh_var, " ~ ", covariate_var))
      model <- svyglm(formula, design = svy_design)

      # --- Extract covariate coefficient ---
      coef_summary <- summary(model)$coefficients
      covar_row <- coef_summary[covariate_var, ]

      coefficient <- covar_row["Estimate"]
      se <- covar_row["Std. Error"]

      # Confidence interval
      ci <- confint(model, level = ci_level)[covariate_var, ]
      ci_lower <- ci[1]
      ci_upper <- ci[2]

      results_list[[i]] <- data.frame(
        survey = survey_name,
        covariate = covariate_var,
        covariate_label = covariate_label,
        year = yr,
        coefficient = coefficient,
        se = se,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        n_unweighted = n_unweighted,
        n_covariate_1 = n_covariate_1,
        stringsAsFactors = FALSE
      )

    }, error = function(e) {
      warning("  Year ", yr, " failed: ", e$message)
    })
  }

  # --- Combine results ---
  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    warning("No valid results for ", survey_name, " - ", covariate_label)
    return(NULL)
  }

  message("  Completed: ", nrow(coefficients), " years with valid coefficients")

  return(coefficients)
}


# ------------------------------------------------------------------------------
# BATCH REGRESSION FUNCTION
# ------------------------------------------------------------------------------

#' Run regressions for multiple covariates
#'
#' @description
#' Convenience function to run regress_covariate_by_year for multiple covariates
#' and combine results.
#'
#' @param data Data frame with survey data
#' @param covariate_list Named vector where names are variable names and values
#'   are human-readable labels
#' @param survey_name Survey name for output
#' @param ... Additional arguments passed to regress_covariate_by_year
#'
#' @return Combined data frame of all covariate regressions
#'
regress_covariates_by_year <- function(
    data,
    covariate_list,
    survey_name,
    ...
) {

  results_list <- vector("list", length(covariate_list))

  for (i in seq_along(covariate_list)) {
    covar_var <- names(covariate_list)[i]
    covar_label <- covariate_list[i]

    # Check if variable exists in data
    if (!covar_var %in% names(data)) {
      message("Variable ", covar_var, " not found in data. Skipping.")
      next
    }

    result <- regress_covariate_by_year(
      data = data,
      covariate_var = covar_var,
      covariate_label = covar_label,
      survey_name = survey_name,
      ...
    )

    results_list[[i]] <- result
  }

  # Combine all results
  all_results <- dplyr::bind_rows(results_list)

  return(all_results)
}


# ------------------------------------------------------------------------------
# RESCALING HELPER
# ------------------------------------------------------------------------------

#' Rescale a continuous covariate to 0-1 range
#'
#' @description
#' For mental health scales that differ across surveys, rescale to 0-1 range
#' so coefficients are comparable (effect of moving from min to max).
#'
#' @param x Numeric vector
#' @param min_val Minimum value of scale (default: observed min)
#' @param max_val Maximum value of scale (default: observed max)
#'
#' @return Rescaled vector in 0-1 range
#'
rescale_01 <- function(x, min_val = NULL, max_val = NULL) {
  if (is.null(min_val)) min_val <- min(x, na.rm = TRUE)
  if (is.null(max_val)) max_val <- max(x, na.rm = TRUE)

  (x - min_val) / (max_val - min_val)
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
save_fig3_coefficients_table <- function(
    coefficients,
    survey_name,
    output_dir = here::here("output", "tables"),
    date_suffix = TRUE
) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_name <- paste0("fig3_coefficients_", tolower(survey_name))
  if (date_suffix) {
    base_name <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"))
  }

  # Round for CSV
  coefficients_rounded <- coefficients %>%
    mutate(across(where(is.numeric) & !matches("year|n_"), ~ round(.x, 6)))

  csv_path <- file.path(output_dir, paste0(base_name, ".csv"))
  readr::write_csv(coefficients_rounded, csv_path)
  message("Saved: ", csv_path)

  rds_path <- file.path(output_dir, paste0(base_name, ".rds"))
  readr::write_rds(coefficients, rds_path)
  message("Saved: ", rds_path)

  invisible(NULL)
}
