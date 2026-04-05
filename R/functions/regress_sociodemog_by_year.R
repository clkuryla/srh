# ==============================================================================
# regress_sociodemog_by_year.R
# Figure S1 & S2: Sociodemographic Covariate Sensitivity Analyses
#
# Purpose: Run survey-weighted regressions examining how sociodemographic
#          covariates (sex, race, education) relate to the SRH convergence.
#
# Functions:
#   1. regress_age_adjusted_by_year() - Age coefficient adjusted for covariate
#   2. regress_covariate_overall_by_year() - Covariate coefficient (all ages pooled)
#   3. regress_covariate_by_age_year_categorical() - Covariate by age group (rainbow lines)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(survey)
library(broom)

# ------------------------------------------------------------------------------
# FUNCTION 1: AGE COEFFICIENT ADJUSTED FOR COVARIATE
# ------------------------------------------------------------------------------

#' Run survey-weighted regression of SRH on age, adjusted for a covariate
#'
#' @description
#' For each year in the dataset, fits a survey-weighted linear model:
#'   SRH ~ age + covariate
#' and extracts the age coefficient with standard error and CI.
#'
#' This shows whether the convergence phenomenon persists after adjusting
#' for sociodemographic covariates.
#'
#' @param data Data frame with columns: srh, age, covariate, year, wt
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param covariate_var Name of covariate variable to adjust for
#' @param covariate_label Human-readable label for the covariate
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_var Name of age variable (default "age")
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
#'   - covariate_adjusted: Covariate adjusted for
#'   - year: Survey year
#'   - coefficient: Age coefficient (adjusted)
#'   - se: Standard error of coefficient
#'   - ci_lower, ci_upper: Confidence interval bounds
#'   - n_unweighted: Unweighted sample size for that year
#'
regress_age_adjusted_by_year <- function(
    data,
    survey_name,
    covariate_var,
    covariate_label = NULL,
    srh_var = "srh",
    age_var = "age",
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
  stopifnot(age_var %in% names(data))
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
  message("Processing ", survey_name, " (SRH ~ age + ", covariate_label, "): ",
          length(years), " years (", min(years), "-", max(years), ")")

  # --- Run regression for each year ---
  results_list <- vector("list", length(years))

  for (i in seq_along(years)) {
    yr <- years[i]

    # Subset to this year
    data_year <- data[data[[year_var]] == yr, , drop = FALSE]

    # Filter to valid observations
    valid_mask <- !is.na(data_year[[srh_var]]) &
      !is.na(data_year[[age_var]]) &
      !is.na(data_year[[covariate_var]]) &
      !is.na(data_year[[wt_var]]) &
      data_year[[wt_var]] > 0

    if (has_psu) valid_mask <- valid_mask & !is.na(data_year[[psu_var]])
    if (has_strata) valid_mask <- valid_mask & !is.na(data_year[[strata_var]])

    data_year <- data_year[valid_mask, , drop = FALSE]
    n_unweighted <- nrow(data_year)

    # Skip if too few observations
    if (n_unweighted < min_n) {
      message("  Year ", yr, " has only ", n_unweighted, " observations. Skipping.")
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

      # --- Fit the model: SRH ~ age + covariate ---
      formula <- as.formula(paste0(srh_var, " ~ ", age_var, " + ", covariate_var))
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
        covariate_adjusted = covariate_label,
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
      warning("  Year ", yr, " failed: ", e$message)
    })
  }

  # --- Combine results ---
  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    warning("No valid results for ", survey_name, " adjusted for ", covariate_label)
    return(NULL)
  }

  message("  Completed: ", nrow(coefficients), " years with valid coefficients")

  return(coefficients)
}


# ------------------------------------------------------------------------------
# FUNCTION 2: COVARIATE COEFFICIENT (ALL AGES POOLED)
# ------------------------------------------------------------------------------

#' Run survey-weighted regression of SRH on a categorical covariate (pooled)
#'
#' @description
#' For each year in the dataset, fits a survey-weighted linear model:
#'   SRH ~ covariate
#' and extracts all covariate level coefficients (vs reference) with SE and CI.
#'
#' For categorical covariates, this returns one row per non-reference level.
#'
#' @param data Data frame with columns: srh, covariate, year, wt
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param covariate_var Name of covariate variable (character or factor)
#' @param covariate_label Human-readable label for the covariate
#' @param reference_level Reference level for categorical covariate (first level if NULL)
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
#'   - level: Covariate level (for categorical)
#'   - reference: Reference level
#'   - year: Survey year
#'   - coefficient: Covariate coefficient
#'   - se: Standard error
#'   - ci_lower, ci_upper: Confidence interval bounds
#'   - n_unweighted: Sample size
#'
regress_covariate_overall_by_year <- function(
    data,
    survey_name,
    covariate_var,
    covariate_label = NULL,
    reference_level = NULL,
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

  # --- Handle factor/character covariate ---
  covar_vec <- data[[covariate_var]]
  if (is.character(covar_vec)) {
    covar_vec <- factor(covar_vec)
  }
  if (is.factor(covar_vec)) {
    all_levels <- levels(covar_vec)
    if (!is.null(reference_level)) {
      if (!reference_level %in% all_levels) {
        stop("Reference level '", reference_level, "' not found in covariate levels")
      }
      data[[covariate_var]] <- relevel(factor(data[[covariate_var]]), ref = reference_level)
    } else {
      reference_level <- all_levels[1]
      data[[covariate_var]] <- factor(data[[covariate_var]], levels = all_levels)
    }
    covar_levels <- setdiff(levels(data[[covariate_var]]), reference_level)
  } else {
    # Numeric/continuous covariate
    covar_levels <- covariate_var
    reference_level <- NA_character_
  }

  # --- Set survey options ---
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # --- Check which design elements are available ---
  has_psu <- !is.null(psu_var) && psu_var %in% names(data)
  has_strata <- !is.null(strata_var) && strata_var %in% names(data)

  # --- Get unique years ---
  years <- sort(unique(data[[year_var]]))
  message("Processing ", survey_name, " (SRH ~ ", covariate_label, "): ",
          length(years), " years (", min(years), "-", max(years), ")")

  # --- Run regression for each year ---
  results_list <- list()

  for (yr in years) {
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

      # --- Extract coefficients for each level ---
      coef_summary <- summary(model)$coefficients
      ci_all <- confint(model, level = ci_level)

      # For categorical covariates, extract each level's coefficient
      for (lvl in covar_levels) {
        # Build coefficient name as R would create it
        if (is.factor(data_year[[covariate_var]])) {
          coef_name <- paste0(covariate_var, lvl)
        } else {
          coef_name <- covariate_var
          lvl <- covariate_var
        }

        if (!coef_name %in% rownames(coef_summary)) {
          next
        }

        coef_row <- coef_summary[coef_name, ]
        coefficient <- coef_row["Estimate"]
        se <- coef_row["Std. Error"]

        ci <- ci_all[coef_name, ]
        ci_lower <- ci[1]
        ci_upper <- ci[2]

        results_list[[length(results_list) + 1]] <- data.frame(
          survey = survey_name,
          covariate = covariate_var,
          covariate_label = covariate_label,
          level = lvl,
          reference = reference_level,
          year = yr,
          coefficient = coefficient,
          se = se,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          n_unweighted = n_unweighted,
          stringsAsFactors = FALSE
        )
      }

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

  message("  Completed: ", nrow(coefficients), " year-level combinations")

  return(coefficients)
}


# ------------------------------------------------------------------------------
# FUNCTION 3: COVARIATE COEFFICIENT BY AGE GROUP (RAINBOW LINES)
# ------------------------------------------------------------------------------

#' Run survey-weighted regression of SRH on categorical covariate, by age group
#'
#' @description
#' For each age group × year combination, fits a survey-weighted linear model:
#'   SRH ~ covariate
#' and extracts all covariate level coefficients (vs reference).
#'
#' This is for Figure S2 Panel B: rainbow lines showing covariate effect
#' by age group over time.
#'
#' @param data Data frame with columns: srh, covariate, year, age_group, wt
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param covariate_var Name of covariate variable
#' @param covariate_label Human-readable label for the covariate
#' @param reference_level Reference level for categorical covariate
#' @param age_group_var Name of age group variable (default "age_group")
#' @param age_groups Character vector of age groups to include
#' @param srh_var Name of SRH variable (default "srh")
#' @param year_var Name of year variable (default "year")
#' @param psu_var Name of PSU variable (default "psu", NULL if not available)
#' @param strata_var Name of strata variable (default "strata", NULL if not available)
#' @param wt_var Name of weight variable (default "wt")
#' @param ci_level Confidence level for intervals (default 0.95)
#' @param lonely_psu How to handle single-PSU strata (default "adjust")
#' @param min_n Minimum observations per age-year cell (default 50)
#'
#' @return Data frame with columns including:
#'   - survey, covariate, covariate_label, level, reference
#'   - age_group, year, coefficient, se, ci_lower, ci_upper, n_unweighted
#'
regress_covariate_by_age_year_categorical <- function(
    data,
    survey_name,
    covariate_var,
    covariate_label = NULL,
    reference_level = NULL,
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
  stopifnot(covariate_var %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))
  stopifnot(age_group_var %in% names(data))

  if (is.null(covariate_label)) {
    covariate_label <- covariate_var
  }

  # --- Handle factor/character covariate ---
  covar_vec <- data[[covariate_var]]
  if (is.character(covar_vec)) {
    covar_vec <- factor(covar_vec)
  }
  if (is.factor(covar_vec)) {
    all_levels <- levels(covar_vec)
    if (!is.null(reference_level)) {
      if (!reference_level %in% all_levels) {
        stop("Reference level '", reference_level, "' not found in covariate levels")
      }
      data[[covariate_var]] <- relevel(factor(data[[covariate_var]]), ref = reference_level)
    } else {
      reference_level <- all_levels[1]
      data[[covariate_var]] <- factor(data[[covariate_var]], levels = all_levels)
    }
    covar_levels <- setdiff(levels(data[[covariate_var]]), reference_level)
  } else {
    # Numeric covariate
    covar_levels <- covariate_var
    reference_level <- NA_character_
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
  existing_age_groups <- unique(data[[age_group_var]])
  age_groups <- intersect(age_groups, existing_age_groups)

  if (length(age_groups) == 0) {
    warning("No matching age groups found in data.")
    return(NULL)
  }

  message("Processing ", survey_name, " (SRH ~ ", covariate_label, " by age): ",
          length(years), " years x ", length(age_groups), " age groups")

  # --- Run regression for each age group x year ---
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
        !is.na(data_subset[[covariate_var]]) &
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

      # Skip if no variation in covariate
      if (length(unique(data_subset[[covariate_var]])) < 2) {
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

        # --- Fit the model: SRH ~ covariate ---
        formula <- as.formula(paste0(srh_var, " ~ ", covariate_var))
        model <- svyglm(formula, design = svy_design)

        # --- Extract coefficients for each level ---
        coef_summary <- summary(model)$coefficients
        ci_all <- confint(model, level = ci_level)

        for (lvl in covar_levels) {
          # Build coefficient name
          if (is.factor(data_subset[[covariate_var]])) {
            coef_name <- paste0(covariate_var, lvl)
          } else {
            coef_name <- covariate_var
            lvl <- covariate_var
          }

          if (!coef_name %in% rownames(coef_summary)) {
            next
          }

          coef_row <- coef_summary[coef_name, ]
          coefficient <- coef_row["Estimate"]
          se <- coef_row["Std. Error"]

          ci <- ci_all[coef_name, ]
          ci_lower <- ci[1]
          ci_upper <- ci[2]

          results_list[[length(results_list) + 1]] <- data.frame(
            survey = survey_name,
            covariate = covariate_var,
            covariate_label = covariate_label,
            level = lvl,
            reference = reference_level,
            age_group = ag,
            year = yr,
            coefficient = coefficient,
            se = se,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            n_unweighted = n_unweighted,
            stringsAsFactors = FALSE
          )
        }

      }, error = function(e) {
        # Silently skip failed cells
      })
    }
  }

  # --- Combine results ---
  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    warning("No valid results for ", survey_name, " - ", covariate_label)
    return(NULL)
  }

  # Ensure age_group is a factor with correct ordering
  coefficients$age_group <- factor(
    coefficients$age_group,
    levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
  )

  message("  Completed: ", nrow(coefficients), " age-year-level cells")

  return(coefficients)
}


# ------------------------------------------------------------------------------
# FUNCTION 4: AGE COEFFICIENT BY STRATUM (STRATIFIED REGRESSIONS)
# ------------------------------------------------------------------------------

#' Run survey-weighted regression of SRH on age, stratified by a covariate
#'
#' @description
#' For each year × stratum level, fits a survey-weighted linear model:
#'   SRH ~ age
#' This differs from adjusted coefficients - here we run separate regressions
#' for each stratum (e.g., Males only, Females only) rather than including the
#' covariate as a control variable.
#'
#' IMPORTANT: Uses subset() on survey design objects (not filter on data) to
#' get correct standard errors for subpopulations.
#'
#' @param data Data frame with columns: srh, age, stratum_var, year, wt
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param stratum_var Name of stratifying variable (e.g., "sex", "race_includehisp")
#' @param stratum_label Human-readable label for the stratum variable
#' @param stratum_levels Optional character vector of levels to include. If NULL,
#'        uses all non-NA levels found in the data.
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_var Name of age variable (default "age")
#' @param year_var Name of year variable (default "year")
#' @param psu_var Name of PSU variable (default "psu", NULL if not available)
#' @param strata_var Name of strata variable (default "strata", NULL if not available)
#' @param wt_var Name of weight variable (default "wt")
#' @param ci_level Confidence level for intervals (default 0.95)
#' @param lonely_psu How to handle single-PSU strata (default "adjust")
#' @param min_n Minimum observations per stratum-year cell (default 100)
#'
#' @return Data frame with columns:
#'   - survey: Survey name
#'   - stratum_var: Name of stratifying variable
#'   - stratum_label: Human-readable label
#'   - stratum_level: Level of the stratum (e.g., "Male", "Female")
#'   - year: Survey year
#'   - coefficient: Age coefficient (from SRH ~ age within stratum)
#'   - se: Standard error of coefficient
#'   - ci_lower, ci_upper: Confidence interval bounds
#'   - n_unweighted: Unweighted sample size for that stratum-year cell
#'
regress_age_by_stratum_year <- function(
    data,
    survey_name,
    stratum_var,
    stratum_label = NULL,
    stratum_levels = NULL,
    srh_var = "srh",
    age_var = "age",
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
  stopifnot(age_var %in% names(data))
  stopifnot(stratum_var %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))

  if (is.null(stratum_label)) {
    stratum_label <- stratum_var
  }

  # --- Determine stratum levels ---
  stratum_vec <- data[[stratum_var]]
  if (is.character(stratum_vec)) {
    stratum_vec <- factor(stratum_vec)
  }
  if (is.factor(stratum_vec)) {
    all_levels <- levels(stratum_vec)
  } else {
    all_levels <- unique(stratum_vec[!is.na(stratum_vec)])
  }

  if (!is.null(stratum_levels)) {
    # Use only specified levels
    all_levels <- intersect(stratum_levels, all_levels)
  }

  if (length(all_levels) == 0) {
    warning("No valid stratum levels found for ", stratum_var)
    return(NULL)
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
  message("Processing ", survey_name, " (SRH ~ age | ", stratum_label, "): ",
          length(years), " years x ", length(all_levels), " strata")

  # --- Run regression for each year x stratum ---
  results_list <- list()

  for (yr in years) {
    # Subset to this year
    data_year <- data[data[[year_var]] == yr, , drop = FALSE]

    # Filter to valid observations for creating survey design
    valid_mask <- !is.na(data_year[[srh_var]]) &
      !is.na(data_year[[age_var]]) &
      !is.na(data_year[[stratum_var]]) &
      !is.na(data_year[[wt_var]]) &
      data_year[[wt_var]] > 0

    if (has_psu) valid_mask <- valid_mask & !is.na(data_year[[psu_var]])
    if (has_strata) valid_mask <- valid_mask & !is.na(data_year[[strata_var]])

    data_year <- data_year[valid_mask, , drop = FALSE]

    if (nrow(data_year) < min_n) {
      next
    }

    # --- Create survey design for this year ---
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

      # --- For each stratum level, subset and fit ---
      for (lvl in all_levels) {

        # Use subset() on survey design - CRITICAL for correct SE
        # Build the subset expression dynamically
        svy_subset <- tryCatch({
          subset(svy_design, data_year[[stratum_var]] == lvl)
        }, error = function(e) NULL)

        if (is.null(svy_subset)) next

        n_unweighted <- nrow(svy_subset$variables)

        # Skip if too few observations in this stratum
        if (n_unweighted < min_n) {
          next
        }

        # --- Fit the model: SRH ~ age ---
        formula <- as.formula(paste0(srh_var, " ~ ", age_var))
        model <- tryCatch({
          svyglm(formula, design = svy_subset)
        }, error = function(e) NULL)

        if (is.null(model)) next

        # --- Extract age coefficient ---
        coef_summary <- summary(model)$coefficients

        if (!age_var %in% rownames(coef_summary)) next

        age_row <- coef_summary[age_var, ]

        coefficient <- age_row["Estimate"]
        se <- age_row["Std. Error"]
        t_value <- age_row["t value"]
        p_value <- age_row["Pr(>|t|)"]

        # Confidence interval
        ci <- tryCatch({
          confint(model, level = ci_level)[age_var, ]
        }, error = function(e) c(NA, NA))
        ci_lower <- ci[1]
        ci_upper <- ci[2]

        results_list[[length(results_list) + 1]] <- data.frame(
          survey = survey_name,
          stratum_var = stratum_var,
          stratum_label = stratum_label,
          stratum_level = as.character(lvl),
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
      }

    }, error = function(e) {
      warning("  Year ", yr, " failed: ", e$message)
    })
  }

  # --- Combine results ---
  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    warning("No valid results for ", survey_name, " stratified by ", stratum_label)
    return(NULL)
  }

  message("  Completed: ", nrow(coefficients), " stratum-year cells")

  return(coefficients)
}


# ------------------------------------------------------------------------------
# HELPER: RUN FOR ALL COVARIATES
# ------------------------------------------------------------------------------

#' Run adjusted age coefficient regressions for multiple covariates
#'
#' @param data Data frame with survey data
#' @param survey_name Survey name
#' @param covariate_list Named list: names = variable names, values = labels
#' @param ... Additional arguments passed to regress_age_adjusted_by_year
#'
#' @return Combined data frame
#'
run_all_adjusted_age_coefficients <- function(
    data,
    survey_name,
    covariate_list,
    ...
) {
  results <- lapply(names(covariate_list), function(cv) {
    if (!cv %in% names(data)) {
      message("Variable ", cv, " not found in data. Skipping.")
      return(NULL)
    }
    regress_age_adjusted_by_year(
      data = data,
      survey_name = survey_name,
      covariate_var = cv,
      covariate_label = covariate_list[[cv]],
      ...
    )
  })
  dplyr::bind_rows(results)
}


#' Run covariate coefficient regressions for multiple covariates
#'
#' @param data Data frame with survey data
#' @param survey_name Survey name
#' @param covariate_list Named list: list(var_name = list(label = "Label", ref = "Reference"))
#' @param by_age_group Logical. If TRUE, run by age group. If FALSE, pool all ages.
#' @param ... Additional arguments
#'
#' @return Combined data frame
#'
run_all_covariate_coefficients <- function(
    data,
    survey_name,
    covariate_list,
    by_age_group = FALSE,
    ...
) {
  results <- lapply(names(covariate_list), function(cv) {
    if (!cv %in% names(data)) {
      message("Variable ", cv, " not found in data. Skipping.")
      return(NULL)
    }

    cv_info <- covariate_list[[cv]]
    label <- if (is.list(cv_info)) cv_info$label else cv_info
    ref <- if (is.list(cv_info) && !is.null(cv_info$ref)) cv_info$ref else NULL

    if (by_age_group) {
      regress_covariate_by_age_year_categorical(
        data = data,
        survey_name = survey_name,
        covariate_var = cv,
        covariate_label = label,
        reference_level = ref,
        ...
      )
    } else {
      regress_covariate_overall_by_year(
        data = data,
        survey_name = survey_name,
        covariate_var = cv,
        covariate_label = label,
        reference_level = ref,
        ...
      )
    }
  })
  dplyr::bind_rows(results)
}


# ------------------------------------------------------------------------------
# FUNCTION 6: FULL MODEL — ALL COVARIATES SIMULTANEOUSLY
# ------------------------------------------------------------------------------

#' Fit SRH ~ age + sex + race + education (all together) for each year
#'
#' @description
#' For each year, fits a survey-weighted linear model with ALL covariates:
#'   SRH ~ age + sex + race_includehisp + educ_3cat
#' Returns ALL coefficients (age + each covariate level) in tidy format.
#'
#' Analogous to regress_age_adjusted_by_year() + regress_covariate_overall_by_year()
#' but from a single joint model rather than separate one-at-a-time models.
#'
#' @param data Data frame with columns: srh, age, sex, race_includehisp, educ_3cat, year, wt
#' @param survey_name Character string for labeling output
#' @param covariate_vars Named list of covariate specs:
#'   list(sex = list(label = "Sex", ref = "Male"), ...)
#' @param srh_var,age_var,year_var,psu_var,strata_var,wt_var Column names
#' @param ci_level Confidence level (default 0.95)
#' @param lonely_psu How to handle single-PSU strata (default "adjust")
#' @param min_n Minimum observations per year (default 100)
#'
#' @return Data frame with columns:
#'   survey, term, covariate, covariate_label, level, reference,
#'   year, coefficient, se, ci_lower, ci_upper, n_unweighted
#'
regress_all_covariates_by_year <- function(
    data,
    survey_name,
    covariate_vars = list(
      sex = list(label = "Sex", ref = "Male"),
      race_includehisp = list(label = "Race/Ethnicity", ref = "White"),
      educ_3cat = list(label = "Education", ref = "LT_HS")
    ),
    srh_var = "srh",
    age_var = "age",
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
  stopifnot(age_var %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))

  # Check which covariates are actually available
  available_covars <- list()
  for (cv in names(covariate_vars)) {
    if (cv %in% names(data) && !all(is.na(data[[cv]]))) {
      available_covars[[cv]] <- covariate_vars[[cv]]
    } else {
      message("  Skipping ", cv, " - not available in ", survey_name)
    }
  }

  if (length(available_covars) == 0) {
    warning("No covariates available for ", survey_name)
    return(NULL)
  }

  # --- Set up factor levels with correct references ---
  for (cv in names(available_covars)) {
    ref <- available_covars[[cv]]$ref
    if (is.character(data[[cv]]) || is.factor(data[[cv]])) {
      data[[cv]] <- relevel(factor(data[[cv]]), ref = ref)
    }
  }

  # --- Set survey options ---
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # --- Check design elements ---
  has_psu <- !is.null(psu_var) && psu_var %in% names(data)
  has_strata <- !is.null(strata_var) && strata_var %in% names(data)

  message("Processing ", survey_name,
          " (SRH ~ age + ", paste(names(available_covars), collapse = " + "), ")")

  # --- Get unique years ---
  years <- sort(unique(data[[year_var]]))

  # --- Run regression for each year ---
  results_list <- list()

  for (yr in years) {
    data_year <- data[data[[year_var]] == yr, , drop = FALSE]

    # Base valid mask: srh, age, weights
    valid_mask <- !is.na(data_year[[srh_var]]) &
      !is.na(data_year[[age_var]]) &
      !is.na(data_year[[wt_var]]) &
      data_year[[wt_var]] > 0

    if (has_psu) valid_mask <- valid_mask & !is.na(data_year[[psu_var]])
    if (has_strata) valid_mask <- valid_mask & !is.na(data_year[[strata_var]])

    # Determine which covariates are usable this year:
    # require non-NA AND 2+ levels after NA removal
    year_covars <- list()
    year_valid_mask <- valid_mask
    for (cv in names(available_covars)) {
      cv_mask <- !is.na(data_year[[cv]])
      cv_vals <- data_year[[cv]][valid_mask & cv_mask]
      if (length(unique(cv_vals)) >= 2) {
        year_covars[[cv]] <- available_covars[[cv]]
        year_valid_mask <- year_valid_mask & cv_mask
      } else {
        message("  Year ", yr, ": dropping ", cv,
                " (insufficient levels)")
      }
    }

    data_year <- data_year[year_valid_mask, , drop = FALSE]
    n_unweighted <- nrow(data_year)

    if (n_unweighted < min_n) {
      message("  Year ", yr, " has only ", n_unweighted, " obs. Skipping.")
      next
    }

    # Build per-year formula
    rhs <- paste(c(age_var, names(year_covars)), collapse = " + ")
    yr_formula <- as.formula(paste0(srh_var, " ~ ", rhs))

    tryCatch({
      # --- Create survey design ---
      if (has_psu && has_strata) {
        svy_design <- svydesign(
          ids = as.formula(paste0("~", psu_var)),
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_year, nest = TRUE
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

      # --- Fit the model (covariates may vary by year) ---
      model <- svyglm(yr_formula, design = svy_design)

      coef_summary <- summary(model)$coefficients
      ci_all <- confint(model, level = ci_level)

      # --- Extract age coefficient ---
      if (age_var %in% rownames(coef_summary)) {
        age_row <- coef_summary[age_var, ]
        ci_age <- ci_all[age_var, ]
        results_list[[length(results_list) + 1]] <- data.frame(
          survey = survey_name,
          term = age_var,
          covariate = "age",
          covariate_label = "Age",
          level = "age",
          reference = NA_character_,
          year = yr,
          coefficient = age_row["Estimate"],
          se = age_row["Std. Error"],
          ci_lower = ci_age[1],
          ci_upper = ci_age[2],
          n_unweighted = n_unweighted,
          stringsAsFactors = FALSE
        )
      }

      # --- Extract covariate coefficients (only for this year's covars) ---
      for (cv in names(year_covars)) {
        label <- year_covars[[cv]]$label
        ref <- year_covars[[cv]]$ref
        cv_levels <- setdiff(levels(data_year[[cv]]), ref)

        for (lvl in cv_levels) {
          coef_name <- paste0(cv, lvl)
          if (!coef_name %in% rownames(coef_summary)) next

          coef_row <- coef_summary[coef_name, ]
          ci_row <- ci_all[coef_name, ]

          results_list[[length(results_list) + 1]] <- data.frame(
            survey = survey_name,
            term = coef_name,
            covariate = cv,
            covariate_label = label,
            level = lvl,
            reference = ref,
            year = yr,
            coefficient = coef_row["Estimate"],
            se = coef_row["Std. Error"],
            ci_lower = ci_row[1],
            ci_upper = ci_row[2],
            n_unweighted = n_unweighted,
            stringsAsFactors = FALSE
          )
        }
      }

    }, error = function(e) {
      warning("  Year ", yr, " failed: ", e$message)
    })
  }

  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    warning("No valid results for ", survey_name)
    return(NULL)
  }

  message("  Completed: ", nrow(coefficients), " coefficient-year rows")
  return(coefficients)
}


# ------------------------------------------------------------------------------
# FUNCTION 7: FULL MODEL — ALL COVARIATES BY AGE GROUP
# ------------------------------------------------------------------------------

#' Fit SRH ~ sex + race + education (all together) by age group x year
#'
#' @description
#' For each age group x year, fits:
#'   SRH ~ sex + race_includehisp + educ_3cat
#' Returns all covariate coefficients. Analogous to
#' regress_covariate_by_age_year_categorical() but with all covariates in
#' a single model.
#'
#' @inheritParams regress_all_covariates_by_year
#' @param age_group_var Name of age group variable (default "age_group")
#' @param age_groups Character vector of age groups to include
#' @param min_n Minimum observations per age-year cell (default 50)
#'
#' @return Data frame with columns:
#'   survey, term, covariate, covariate_label, level, reference,
#'   age_group, year, coefficient, se, ci_lower, ci_upper, n_unweighted
#'
regress_all_covariates_by_age_year <- function(
    data,
    survey_name,
    covariate_vars = list(
      sex = list(label = "Sex", ref = "Male"),
      race_includehisp = list(label = "Race/Ethnicity", ref = "White"),
      educ_3cat = list(label = "Education", ref = "LT_HS")
    ),
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

  stopifnot(is.data.frame(data))
  stopifnot(age_group_var %in% names(data))

  # Check available covariates
  available_covars <- list()
  for (cv in names(covariate_vars)) {
    if (cv %in% names(data) && !all(is.na(data[[cv]]))) {
      available_covars[[cv]] <- covariate_vars[[cv]]
    } else {
      message("  Skipping ", cv, " - not available in ", survey_name)
    }
  }

  if (length(available_covars) == 0) {
    warning("No covariates available for ", survey_name)
    return(NULL)
  }

  # Set up factor levels
  for (cv in names(available_covars)) {
    ref <- available_covars[[cv]]$ref
    if (is.character(data[[cv]]) || is.factor(data[[cv]])) {
      data[[cv]] <- relevel(factor(data[[cv]]), ref = ref)
    }
  }

  # Survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  has_psu <- !is.null(psu_var) && psu_var %in% names(data)
  has_strata <- !is.null(strata_var) && strata_var %in% names(data)

  # Filter to valid age groups
  existing_ag <- unique(data[[age_group_var]])
  age_groups <- intersect(age_groups, existing_ag)

  years <- sort(unique(data[[year_var]]))
  message("Processing ", survey_name,
          " (SRH ~ ", paste(names(available_covars), collapse = " + "),
          " by age): ",
          length(years), " years x ", length(age_groups), " age groups")

  results_list <- list()

  for (ag in age_groups) {
    for (yr in years) {
      data_subset <- data[
        data[[age_group_var]] == ag & data[[year_var]] == yr, ,
        drop = FALSE
      ]

      # Base valid mask
      valid_mask <- !is.na(data_subset[[srh_var]]) &
        !is.na(data_subset[[wt_var]]) &
        data_subset[[wt_var]] > 0

      if (has_psu) valid_mask <- valid_mask & !is.na(data_subset[[psu_var]])
      if (has_strata) valid_mask <- valid_mask & !is.na(data_subset[[strata_var]])

      # Determine which covariates are usable in this cell
      cell_covars <- list()
      cell_valid_mask <- valid_mask
      for (cv in names(available_covars)) {
        cv_mask <- !is.na(data_subset[[cv]])
        cv_vals <- data_subset[[cv]][valid_mask & cv_mask]
        if (length(unique(cv_vals)) >= 2) {
          cell_covars[[cv]] <- available_covars[[cv]]
          cell_valid_mask <- cell_valid_mask & cv_mask
        }
      }

      if (length(cell_covars) == 0) next

      data_subset <- data_subset[cell_valid_mask, , drop = FALSE]
      n_unweighted <- nrow(data_subset)

      if (n_unweighted < min_n) next

      # Build per-cell formula
      cell_rhs <- paste(names(cell_covars), collapse = " + ")
      cell_formula <- as.formula(paste0(srh_var, " ~ ", cell_rhs))

      tryCatch({
        if (has_psu && has_strata) {
          svy_design <- svydesign(
            ids = as.formula(paste0("~", psu_var)),
            strata = as.formula(paste0("~", strata_var)),
            weights = as.formula(paste0("~", wt_var)),
            data = data_subset, nest = TRUE
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

        model <- svyglm(cell_formula, design = svy_design)

        coef_summary <- summary(model)$coefficients
        ci_all <- confint(model, level = ci_level)

        # Extract covariate coefficients
        for (cv in names(cell_covars)) {
          label <- cell_covars[[cv]]$label
          ref <- cell_covars[[cv]]$ref
          cv_levels <- setdiff(levels(data_subset[[cv]]), ref)

          for (lvl in cv_levels) {
            coef_name <- paste0(cv, lvl)
            if (!coef_name %in% rownames(coef_summary)) next

            coef_row <- coef_summary[coef_name, ]
            ci_row <- ci_all[coef_name, ]

            results_list[[length(results_list) + 1]] <- data.frame(
              survey = survey_name,
              term = coef_name,
              covariate = cv,
              covariate_label = label,
              level = lvl,
              reference = ref,
              age_group = ag,
              year = yr,
              coefficient = coef_row["Estimate"],
              se = coef_row["Std. Error"],
              ci_lower = ci_row[1],
              ci_upper = ci_row[2],
              n_unweighted = n_unweighted,
              stringsAsFactors = FALSE
            )
          }
        }

      }, error = function(e) {
        # Silently skip failed cells
      })
    }
  }

  coefficients <- dplyr::bind_rows(results_list)
  rownames(coefficients) <- NULL

  if (nrow(coefficients) == 0) {
    warning("No valid results for ", survey_name)
    return(NULL)
  }

  coefficients$age_group <- factor(
    coefficients$age_group,
    levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
  )

  message("  Completed: ", nrow(coefficients), " age-year-level cells")
  return(coefficients)
}
