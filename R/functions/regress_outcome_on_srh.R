# ==============================================================================
# regress_outcome_on_srh.R
# Hospital Utilization Validity Analysis
#
# Purpose: Run survey-weighted regressions of outcome ~ SRH for each year
#          to show that SRH predicts healthcare utilization (validity test).
#
# Key difference from regress_covariate_by_year.R:
#   - That function fits: SRH ~ covariate (does SRH capture health?)
#   - This function fits: outcome ~ SRH (does SRH predict behavior?)
#
# Expected interpretation:
#   - Negative coefficient = better SRH (higher value) → less utilization
#   - Stable coefficient over time = SRH meaning is consistent
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(survey)
library(broom)

# ------------------------------------------------------------------------------
# MAIN REGRESSION FUNCTION (OUTCOME ~ SRH)
# ------------------------------------------------------------------------------

#' Run survey-weighted regression of an outcome on SRH, stratified by age group
#'
#' @description
#' For each age group × year combination, fits a survey-weighted linear model:
#'   outcome ~ SRH
#' and extracts the SRH coefficient with standard error and CI.
#'
#' This is a validity test: does self-rated health predict actual healthcare
#' utilization? A negative coefficient indicates that better SRH (higher value)
#' is associated with less healthcare use.
#'
#' @param data Data frame with columns: outcome, srh, year, age_group, wt
#'   (and optional psu, strata)
#' @param outcome_var Name of outcome variable (character string)
#' @param outcome_label Human-readable label for the outcome (for output)
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param age_group_var Name of age group variable (default "age_group")
#' @param age_groups Character vector of age groups to include
#'   (default: scheme B from add_age_group)
#' @param srh_var Name of SRH variable (default "srh")
#' @param year_var Name of year variable (default "year")
#' @param psu_var Name of PSU variable (default "psu", NULL if not available)
#' @param strata_var Name of strata variable (default "strata", NULL if not available)
#' @param wt_var Name of weight variable (default "wt")
#' @param ci_level Confidence level for intervals (default 0.95)
#' @param lonely_psu How to handle single-PSU strata (default "adjust")
#' @param min_n Minimum observations per age-year cell to include (default 50)
#'
#' @return Data frame with columns:
#'   - survey: Survey name
#'   - outcome: Outcome variable name
#'   - outcome_label: Human-readable label
#'   - age_group: Age group
#'   - year: Survey year
#'   - coefficient: SRH coefficient (effect of SRH on outcome)
#'   - se: Standard error of coefficient
#'   - ci_lower, ci_upper: Confidence interval bounds
#'   - n_unweighted: Unweighted sample size for that age-year cell
#'
#' @details
#' The model is: outcome ~ srh (simple linear regression, no additional covariates)
#'
#' Interpretation:
#' - Negative coefficient: Better SRH → less utilization (expected pattern)
#' - Stable coefficient over time: The predictive meaning of SRH is constant
#'
regress_outcome_on_srh_by_age_year <- function(
    data,
    outcome_var,
    outcome_label = NULL,
    survey_name,
    age_group_var = "age_group",
    age_groups = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
    srh_var = "srh",
    year_var = "year",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    ci_level = 0.95,
    lonely_psu = "adjust",
    min_n = 50
) {

  # --- Input validation ---
  stopifnot(is.data.frame(data))
  stopifnot(srh_var %in% names(data))
  stopifnot(outcome_var %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))
  stopifnot(age_group_var %in% names(data))

  if (is.null(outcome_label)) {
    outcome_label <- outcome_var
  }

  # --- Set survey options ---
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # --- Check which design elements are available ---
  has_psu <- !is.null(psu_var) && psu_var %in% names(data)
  has_strata <- !is.null(strata_var) && strata_var %in% names(data)

  # --- Get unique years and filter age groups ---
  years <- sort(unique(data[[year_var]]))

  # Filter to specified age groups that exist in data
  existing_age_groups <- unique(data[[age_group_var]])
  age_groups <- intersect(age_groups, existing_age_groups)

  if (length(age_groups) == 0) {
    warning("No matching age groups found in data.")
    return(NULL)
  }

  message("Processing ", survey_name, " - ", outcome_label, " (outcome ~ SRH): ",
          length(years), " years x ", length(age_groups), " age groups")

  # --- Run regression for each age group × year ---
  results_list <- list()

  for (ag in age_groups) {
    for (yr in years) {

      # Subset to this age group and year
      data_subset <- data[
        data[[age_group_var]] == ag & data[[year_var]] == yr, ,
        drop = FALSE
      ]

      # Filter to valid observations
      valid_mask <- !is.na(data_subset[[srh_var]]) &
        !is.na(data_subset[[outcome_var]]) &
        !is.na(data_subset[[wt_var]]) &
        data_subset[[wt_var]] > 0

      if (has_psu) valid_mask <- valid_mask & !is.na(data_subset[[psu_var]])
      if (has_strata) valid_mask <- valid_mask & !is.na(data_subset[[strata_var]])

      data_subset <- data_subset[valid_mask, , drop = FALSE]
      n_unweighted <- nrow(data_subset)

      # Skip if too few observations
      if (n_unweighted < min_n) {
        next
      }

      # Skip if no variation in outcome
      if (length(unique(data_subset[[outcome_var]])) < 2) {
        next
      }

      # --- Create survey design ---
      tryCatch({
        if (has_psu && has_strata) {
          svy_design <- svydesign(
            ids = as.formula(paste0("~", psu_var)),
            strata = as.formula(paste0("~", strata_var)),
            weights = as.formula(paste0("~", wt_var)),
            data = data_subset,
            nest = TRUE
          )
        } else if (has_psu) {
          svy_design <- svydesign(
            ids = as.formula(paste0("~", psu_var)),
            weights = as.formula(paste0("~", wt_var)),
            data = data_subset
          )
        } else if (has_strata) {
          svy_design <- svydesign(
            ids = ~1,
            strata = as.formula(paste0("~", strata_var)),
            weights = as.formula(paste0("~", wt_var)),
            data = data_subset
          )
        } else {
          svy_design <- svydesign(
            ids = ~1,
            weights = as.formula(paste0("~", wt_var)),
            data = data_subset
          )
        }

        # --- Fit the model: outcome ~ SRH ---
        formula <- as.formula(paste0(outcome_var, " ~ ", srh_var))
        model <- svyglm(formula, design = svy_design)

        # --- Extract SRH coefficient ---
        coef_summary <- summary(model)$coefficients
        srh_row <- coef_summary[srh_var, ]

        coefficient <- srh_row["Estimate"]
        se <- srh_row["Std. Error"]

        # Confidence interval
        ci <- confint(model, level = ci_level)[srh_var, ]
        ci_lower <- ci[1]
        ci_upper <- ci[2]

        results_list[[length(results_list) + 1]] <- data.frame(
          survey = survey_name,
          outcome = outcome_var,
          outcome_label = outcome_label,
          age_group = ag,
          year = yr,
          coefficient = coefficient,
          se = se,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          n_unweighted = n_unweighted,
          stringsAsFactors = FALSE
        )

      }, error = function(e) {
        # Silently skip failed cells
      })
    }
  }

  # --- Combine results ---
  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    warning("No valid results for ", survey_name, " - ", outcome_label)
    return(NULL)
  }

  # Ensure age_group is a factor with correct ordering
  coefficients$age_group <- factor(
    coefficients$age_group,
    levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
  )

  message("  Completed: ", nrow(coefficients), " age-year cells with valid coefficients")

  return(coefficients)
}


# ------------------------------------------------------------------------------
# BATCH FUNCTION FOR MULTIPLE OUTCOMES
# ------------------------------------------------------------------------------

#' Run outcome ~ SRH regressions for multiple outcomes
#'
#' @description
#' Convenience function to run regress_outcome_on_srh_by_age_year for multiple
#' outcome variables and combine results.
#'
#' @param data Data frame with survey data
#' @param outcome_list Named vector where names are variable names and values
#'   are human-readable labels
#' @param survey_name Survey name for output
#' @param ... Additional arguments passed to regress_outcome_on_srh_by_age_year
#'
#' @return Combined data frame of all outcome regressions
#'
regress_outcomes_on_srh_by_age_year <- function(
    data,
    outcome_list,
    survey_name,
    ...
) {

  results_list <- vector("list", length(outcome_list))

  for (i in seq_along(outcome_list)) {
    outcome_var <- names(outcome_list)[i]
    outcome_label <- outcome_list[i]

    # Check if variable exists in data
    if (!outcome_var %in% names(data)) {
      message("Variable ", outcome_var, " not found in data. Skipping.")
      next
    }

    result <- regress_outcome_on_srh_by_age_year(
      data = data,
      outcome_var = outcome_var,
      outcome_label = outcome_label,
      survey_name = survey_name,
      ...
    )

    results_list[[i]] <- result
  }

  # Combine all results
  all_results <- dplyr::bind_rows(results_list)

  return(all_results)
}
