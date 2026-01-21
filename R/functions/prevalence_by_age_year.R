# ==============================================================================
# prevalence_by_age_year.R
# Figure 3B: Prevalence/Mean Trends by Age Group Over Time
#
# Purpose: Compute survey-weighted means of covariates by age group and year
#          to show what is actually changing (rising comorbidities, mental health
#          burden in younger groups, etc.)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(survey)

# ------------------------------------------------------------------------------
# WEIGHTED MEAN BY AGE GROUP AND YEAR
# ------------------------------------------------------------------------------

#' Compute survey-weighted mean of a variable by age group and year
#'
#' @description
#' For each age group × year combination, computes the survey-weighted mean
#' (and standard error) of a continuous or binary variable.
#'
#' This function is designed for Figure 3B: showing prevalence/mean trends
#' of health covariates by age group over time.
#'
#' @param data Data frame with columns: variable, year, age_group, wt
#'   (and optional psu, strata)
#' @param var_name Name of the variable to compute mean for (character string)
#' @param var_label Human-readable label for the variable (for output)
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param age_group_var Name of age group variable (default "age_group")
#' @param age_groups Character vector of age groups to include
#'   (default: scheme B from add_age_group)
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
#'   - variable: Variable name
#'   - variable_label: Human-readable label
#'   - age_group: Age group
#'   - year: Survey year
#'   - mean: Weighted mean
#'   - se: Standard error of mean
#'   - ci_lower, ci_upper: Confidence interval bounds
#'   - n_unweighted: Unweighted sample size for that age-year cell
#'
mean_by_age_year <- function(
    data,
    var_name,
    var_label = NULL,
    survey_name,
    age_group_var = "age_group",
    age_groups = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
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
  stopifnot(var_name %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))
  stopifnot(age_group_var %in% names(data))

  if (is.null(var_label)) {
    var_label <- var_name
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

  message("Processing ", survey_name, " - ", var_label, " (prevalence): ",
          length(years), " years x ", length(age_groups), " age groups")

  # --- Compute mean for each age group x year ---
  results_list <- list()

  for (ag in age_groups) {
    for (yr in years) {

      # Subset to this age group and year
      data_subset <- data[
        data[[age_group_var]] == ag & data[[year_var]] == yr, ,
        drop = FALSE
      ]

      # Filter to valid observations
      valid_mask <- !is.na(data_subset[[var_name]]) &
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

        # --- Compute weighted mean ---
        formula <- as.formula(paste0("~", var_name))
        mean_result <- svymean(formula, design = svy_design, na.rm = TRUE)

        mean_val <- coef(mean_result)[1]
        se_val <- SE(mean_result)[1]

        # Confidence interval
        z <- qnorm(1 - (1 - ci_level) / 2)
        ci_lower <- mean_val - z * se_val
        ci_upper <- mean_val + z * se_val

        results_list[[length(results_list) + 1]] <- data.frame(
          survey = survey_name,
          variable = var_name,
          variable_label = var_label,
          age_group = ag,
          year = yr,
          mean = mean_val,
          se = se_val,
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
  results <- dplyr::bind_rows(results_list)
  rownames(results) <- NULL

  if (nrow(results) == 0) {
    warning("No valid results for ", survey_name, " - ", var_label)
    return(NULL)
  }

  # Ensure age_group is a factor with correct ordering
  results$age_group <- factor(
    results$age_group,
    levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
  )

  message("  Completed: ", nrow(results), " age-year cells with valid means")

  return(results)
}


# ------------------------------------------------------------------------------
# BATCH FUNCTION FOR MULTIPLE VARIABLES
# ------------------------------------------------------------------------------

#' Compute means for multiple variables by age group and year
#'
#' @description
#' Convenience function to run mean_by_age_year for multiple variables
#' and combine results.
#'
#' @param data Data frame with survey data
#' @param var_list Named vector where names are variable names and values
#'   are human-readable labels
#' @param survey_name Survey name for output
#' @param ... Additional arguments passed to mean_by_age_year
#'
#' @return Combined data frame of all variable means
#'
means_by_age_year <- function(
    data,
    var_list,
    survey_name,
    ...
) {

  results_list <- vector("list", length(var_list))

  for (i in seq_along(var_list)) {
    var_nm <- names(var_list)[i]
    var_lbl <- var_list[i]

    # Check if variable exists in data
    if (!var_nm %in% names(data)) {
      message("Variable ", var_nm, " not found in data. Skipping.")
      next
    }

    result <- mean_by_age_year(
      data = data,
      var_name = var_nm,
      var_label = var_lbl,
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
# TABLE EXPORT HELPER
# ------------------------------------------------------------------------------

#' Save prevalence table to CSV and RDS
#'
#' @param data Data frame of prevalence/means
#' @param prefix Character. Filename prefix (e.g., "fig3b_prevalence")
#' @param survey_name Character. Survey name for filename
#' @param output_dir Character. Directory to save files.
#' @param date_suffix Logical. Add date suffix to filename?
#'
save_prevalence_table <- function(
    data,
    prefix = "fig3b_prevalence",
    survey_name,
    output_dir = here::here("output", "tables"),
    date_suffix = TRUE
) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_name <- paste0(prefix, "_", tolower(survey_name))
  if (date_suffix) {
    base_name <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"))
  }

  # Round for CSV
  data_rounded <- data %>%
    mutate(across(where(is.numeric) & !matches("year|n_"), ~ round(.x, 6)))

  csv_path <- file.path(output_dir, paste0(base_name, ".csv"))
  readr::write_csv(data_rounded, csv_path)
  message("Saved: ", csv_path)

  rds_path <- file.path(output_dir, paste0(base_name, ".rds"))
  readr::write_rds(data, rds_path)
  message("Saved: ", rds_path)

  invisible(NULL)
}
