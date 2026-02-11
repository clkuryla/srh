# ==============================================================================
# covariate_sensitivity_functions.R
# Core computation functions for covariate sensitivity analyses
#
# Purpose: Provide reusable functions for stratified analyses of SRH by
#          sociodemographic covariates (sex, education, race). These functions
#          compute statistics but do NOT plot.
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(tidyr)
library(survey)
library(here)

source(here::here("R/paths.R"))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Safely fit a survey-weighted GLM with error handling
#'
#' @description
#' Wraps svyglm in tryCatch to handle model fitting failures gracefully.
#' Returns NULL on error instead of stopping execution.
#'
#' @param formula Model formula
#' @param design Survey design object
#' @param family GLM family (default: gaussian())
#'
#' @return Model object on success, NULL on failure
#'
safe_svyglm <- function(formula, design, family = gaussian()) {
  tryCatch({
    svyglm(formula, design = design, family = family)
  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    return(NULL)
  })
}


#' Create a survey design object
#'
#' @description
#' Creates a survey design object based on available design elements.
#' Uses weights-only design if PSU/strata not available.
#'
#' @param data Data frame
#' @param weight_var Name of weight variable
#' @param psu_var Name of PSU variable (NULL if not available)
#' @param strata_var Name of strata variable (NULL if not available)
#' @param nest Logical, whether to use nested PSUs (default TRUE)
#'
#' @return Survey design object
#'
create_survey_design <- function(data, weight_var = "wt",
                                  psu_var = NULL, strata_var = NULL,
                                  nest = TRUE) {

  has_psu <- !is.null(psu_var) && psu_var %in% names(data)
  has_strata <- !is.null(strata_var) && strata_var %in% names(data)

  # Set lonely.psu option

  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  if (has_psu && has_strata) {
    svy_design <- svydesign(
      ids = as.formula(paste0("~", psu_var)),
      strata = as.formula(paste0("~", strata_var)),
      weights = as.formula(paste0("~", weight_var)),
      data = data,
      nest = nest
    )
  } else if (has_psu) {
    svy_design <- svydesign(
      ids = as.formula(paste0("~", psu_var)),
      weights = as.formula(paste0("~", weight_var)),
      data = data
    )
  } else if (has_strata) {
    svy_design <- svydesign(
      ids = ~1,
      strata = as.formula(paste0("~", strata_var)),
      weights = as.formula(paste0("~", weight_var)),
      data = data
    )
  } else {
    svy_design <- svydesign(
      ids = ~1,
      weights = as.formula(paste0("~", weight_var)),
      data = data
    )
  }

  return(svy_design)
}


# ==============================================================================
# DATA LOADING
# ==============================================================================

#' Load all 6 datasets
#'
#' @description
#' Loads all 6 survey datasets in canonical order: BRFSS, MEPS, NHIS, CPS,
#' NHANES, GSS. Uses paths from R/paths.R.
#'
#' @return Named list of data frames in order: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
#'
#' @examples
#' # datasets <- load_all_datasets()
#' # data_brfss <- datasets$BRFSS
#'
load_all_datasets <- function() {

  ensure_dirs()

  datasets <- list(
    BRFSS  = readr::read_rds(derived_path("data_brfss.rds")),
    MEPS   = readr::read_rds(derived_path("data_meps.rds")),
    NHIS   = readr::read_rds(derived_path("data_nhis.rds")),
    CPS    = readr::read_rds(derived_path("data_cps.rds")),
    NHANES = readr::read_rds(derived_path("data_nhanes.rds")),
    GSS    = readr::read_rds(derived_path("data_gss.rds"))
  )

  # Basic validation
  for (name in names(datasets)) {
    if (!is.data.frame(datasets[[name]])) {
      stop("Failed to load ", name, " dataset")
    }
    message("Loaded ", name, ": ", nrow(datasets[[name]]), " rows")
  }

  return(datasets)
}


# ==============================================================================
# FUNCTION 2: STRATIFIED AGE COEFFICIENTS
# ==============================================================================

#' Compute age coefficients on SRH within covariate strata
#'
#' @description
#' For each year × covariate stratum, runs survey-weighted regression
#' SRH ~ age and returns the age coefficient. Used for Page 1 Row 1 of
#' sensitivity analysis.
#'
#' @param data Data frame with srh, age, year, weight_var, and covariate_var
#' @param covariate_var Name of the covariate variable (character)
#' @param covariate_levels Character vector of covariate levels to analyze
#' @param weight_var Name of weight variable (default "wt")
#' @param min_n Minimum sample size to compute coefficient (default 30)
#' @param psu_var Name of PSU variable (NULL if not available)
#' @param strata_var Name of strata variable (NULL if not available)
#'
#' @return Data frame with columns: year, stratum, beta_age, se, p_value, n
#'
#' @examples
#' # coefs <- compute_stratified_age_coefficients(
#' #   data_nhis, "sex", c("Male", "Female")
#' # )
#'
compute_stratified_age_coefficients <- function(data,
                                                 covariate_var,
                                                 covariate_levels,
                                                 weight_var = "wt",
                                                 min_n = 30,
                                                 psu_var = NULL,
                                                 strata_var = NULL) {


  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot("srh" %in% names(data))
  stopifnot("age" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  years <- sort(unique(data$year))
  results_list <- list()

  for (yr in years) {
    for (stratum in covariate_levels) {

      # Subset data
      data_subset <- data %>%
        filter(
          year == yr,
          .data[[covariate_var]] == stratum,
          !is.na(srh),
          !is.na(age),
          !is.na(.data[[weight_var]]),
          .data[[weight_var]] > 0
        )

      n <- nrow(data_subset)

      # Skip if insufficient sample size
      if (n < min_n) {
        results_list[[length(results_list) + 1]] <- data.frame(
          year = yr,
          stratum = stratum,
          beta_age = NA_real_,
          se = NA_real_,
          p_value = NA_real_,
          n = n,
          stringsAsFactors = FALSE
        )
        next
      }

      # Create survey design and fit model
      tryCatch({
        svy_design <- create_survey_design(
          data_subset, weight_var, psu_var, strata_var
        )

        model <- safe_svyglm(srh ~ age, design = svy_design)

        if (is.null(model)) {
          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            stratum = stratum,
            beta_age = NA_real_,
            se = NA_real_,
            p_value = NA_real_,
            n = n,
            stringsAsFactors = FALSE
          )
          next
        }

        coef_summary <- summary(model)$coefficients

        results_list[[length(results_list) + 1]] <- data.frame(
          year = yr,
          stratum = stratum,
          beta_age = coef_summary["age", "Estimate"],
          se = coef_summary["age", "Std. Error"],
          p_value = coef_summary["age", "Pr(>|t|)"],
          n = n,
          stringsAsFactors = FALSE
        )

      }, error = function(e) {
        results_list[[length(results_list) + 1]] <<- data.frame(
          year = yr,
          stratum = stratum,
          beta_age = NA_real_,
          se = NA_real_,
          p_value = NA_real_,
          n = n,
          stringsAsFactors = FALSE
        )
      })
    }
  }

  results <- bind_rows(results_list)
  return(results)
}


# ==============================================================================
# FUNCTION 3: META-REGRESSION TREND
# ==============================================================================

#' Compute meta-regression trend of age coefficients over time
#'
#' @description
#' Fits linear regression of beta_age ~ year using inverse-variance weighting
#' to assess whether the age-SRH relationship is changing over time within
#' a stratum.
#'
#' @param coef_data Data frame from compute_stratified_age_coefficients
#'   (filtered to one stratum). Must have columns: year, beta_age, se
#'
#' @return Data frame with: slope, se, p_value, is_significant (p < 0.05)
#'
#' @examples
#' # trend <- compute_meta_regression_trend(coef_data_males)
#'
compute_meta_regression_trend <- function(coef_data) {

  # Input validation
  stopifnot(is.data.frame(coef_data))
  stopifnot(all(c("year", "beta_age", "se") %in% names(coef_data)))

  # Filter to valid rows
  valid_data <- coef_data %>%
    filter(!is.na(beta_age), !is.na(se), se > 0)

  if (nrow(valid_data) < 3) {
    return(data.frame(
      slope = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      is_significant = NA,
      stringsAsFactors = FALSE
    ))
  }

  # Compute inverse-variance weights
  valid_data <- valid_data %>%
    mutate(iv_weight = 1 / (se^2))

  # Fit weighted linear regression
  tryCatch({
    meta_model <- lm(beta_age ~ year, data = valid_data, weights = iv_weight)
    meta_summary <- summary(meta_model)

    slope <- coef(meta_model)["year"]
    slope_se <- meta_summary$coefficients["year", "Std. Error"]
    slope_p <- meta_summary$coefficients["year", "Pr(>|t|)"]

    return(data.frame(
      slope = slope,
      se = slope_se,
      p_value = slope_p,
      is_significant = slope_p < 0.05,
      stringsAsFactors = FALSE
    ))

  }, error = function(e) {
    return(data.frame(
      slope = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      is_significant = NA,
      stringsAsFactors = FALSE
    ))
  })
}


# ==============================================================================
# FUNCTION 4: COVARIATE COEFFICIENTS
# ==============================================================================

#' Compute covariate coefficients on SRH over time
#'
#' @description
#' For each year, runs survey-weighted regression SRH ~ covariate and returns
#' coefficients for non-reference levels. Used for Page 2 SC1.
#'
#' @param data Data frame with srh, covariate, year, weight_var
#' @param covariate_var Name of the covariate variable (character)
#' @param covariate_levels Character vector of covariate levels
#' @param reference_level The reference level for the covariate
#' @param weight_var Name of weight variable (default "wt")
#' @param min_n Minimum sample size (default 30)
#' @param psu_var Name of PSU variable (NULL if not available)
#' @param strata_var Name of strata variable (NULL if not available)
#'
#' @return Data frame with: year, level, beta, se, ci_lower, ci_upper,
#'   p_value, n, is_significant
#'
#' @examples
#' # coefs <- compute_covariate_coefficients(
#' #   data_nhis, "sex", c("Male", "Female"), "Male"
#' # )
#'
compute_covariate_coefficients <- function(data,
                                            covariate_var,
                                            covariate_levels,
                                            reference_level,
                                            weight_var = "wt",
                                            min_n = 30,
                                            psu_var = NULL,
                                            strata_var = NULL) {

  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot("srh" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))
  stopifnot(reference_level %in% covariate_levels)

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Set reference level
  non_ref_levels <- setdiff(covariate_levels, reference_level)

  years <- sort(unique(data$year))
  results_list <- list()

  for (yr in years) {

    # Subset data
    data_year <- data %>%
      filter(
        year == yr,
        !is.na(srh),
        !is.na(.data[[covariate_var]]),
        .data[[covariate_var]] %in% covariate_levels,
        !is.na(.data[[weight_var]]),
        .data[[weight_var]] > 0
      )

    # Set factor with reference level
    data_year[[covariate_var]] <- factor(
      data_year[[covariate_var]],
      levels = c(reference_level, non_ref_levels)
    )

    n <- nrow(data_year)

    if (n < min_n) {
      for (lvl in non_ref_levels) {
        results_list[[length(results_list) + 1]] <- data.frame(
          year = yr,
          level = lvl,
          beta = NA_real_,
          se = NA_real_,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          p_value = NA_real_,
          n = n,
          is_significant = NA,
          stringsAsFactors = FALSE
        )
      }
      next
    }

    # Check for variation in covariate
    if (length(unique(data_year[[covariate_var]])) < 2) {
      for (lvl in non_ref_levels) {
        results_list[[length(results_list) + 1]] <- data.frame(
          year = yr,
          level = lvl,
          beta = NA_real_,
          se = NA_real_,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          p_value = NA_real_,
          n = n,
          is_significant = NA,
          stringsAsFactors = FALSE
        )
      }
      next
    }

    tryCatch({
      svy_design <- create_survey_design(
        data_year, weight_var, psu_var, strata_var
      )

      formula <- as.formula(paste0("srh ~ ", covariate_var))
      model <- safe_svyglm(formula, design = svy_design)

      if (is.null(model)) {
        for (lvl in non_ref_levels) {
          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            level = lvl,
            beta = NA_real_,
            se = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            p_value = NA_real_,
            n = n,
            is_significant = NA,
            stringsAsFactors = FALSE
          )
        }
        next
      }

      coef_summary <- summary(model)$coefficients
      ci <- confint(model, level = 0.95)

      for (lvl in non_ref_levels) {
        coef_name <- paste0(covariate_var, lvl)

        if (coef_name %in% rownames(coef_summary)) {
          beta <- coef_summary[coef_name, "Estimate"]
          se <- coef_summary[coef_name, "Std. Error"]
          p_val <- coef_summary[coef_name, "Pr(>|t|)"]
          ci_low <- ci[coef_name, 1]
          ci_up <- ci[coef_name, 2]

          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            level = lvl,
            beta = beta,
            se = se,
            ci_lower = ci_low,
            ci_upper = ci_up,
            p_value = p_val,
            n = n,
            is_significant = p_val < 0.05,
            stringsAsFactors = FALSE
          )
        } else {
          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            level = lvl,
            beta = NA_real_,
            se = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            p_value = NA_real_,
            n = n,
            is_significant = NA,
            stringsAsFactors = FALSE
          )
        }
      }

    }, error = function(e) {
      for (lvl in non_ref_levels) {
        results_list[[length(results_list) + 1]] <<- data.frame(
          year = yr,
          level = lvl,
          beta = NA_real_,
          se = NA_real_,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          p_value = NA_real_,
          n = n,
          is_significant = NA,
          stringsAsFactors = FALSE
        )
      }
    })
  }

  results <- bind_rows(results_list)
  return(results)
}


# ==============================================================================
# FUNCTION 5: COVARIATE COEFFICIENTS BY AGE
# ==============================================================================

#' Compute covariate coefficients by age group over time
#'
#' @description
#' For each year × age group, runs survey-weighted regression SRH ~ covariate
#' and returns coefficients for non-reference levels. Used for Page 2 SC2.
#'
#' @param data Data frame with srh, covariate, year, age_var, weight_var
#' @param covariate_var Name of the covariate variable (character)
#' @param covariate_levels Character vector of covariate levels
#' @param reference_level The reference level for the covariate
#' @param age_var Name of age group variable (default "age_group")
#' @param weight_var Name of weight variable (default "wt")
#' @param min_n Minimum sample size (default 30)
#' @param psu_var Name of PSU variable (NULL if not available)
#' @param strata_var Name of strata variable (NULL if not available)
#'
#' @return Data frame with: year, age_group, level, beta, se, ci_lower,
#'   ci_upper, p_value, n, is_significant
#'
compute_covariate_coefficients_by_age <- function(data,
                                                   covariate_var,
                                                   covariate_levels,
                                                   reference_level,
                                                   age_var = "age_group",
                                                   weight_var = "wt",
                                                   min_n = 30,
                                                   psu_var = NULL,
                                                   strata_var = NULL) {

  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot(age_var %in% names(data))
  stopifnot("srh" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))
  stopifnot(reference_level %in% covariate_levels)

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  non_ref_levels <- setdiff(covariate_levels, reference_level)

  years <- sort(unique(data$year))
  age_groups <- sort(unique(data[[age_var]]))
  results_list <- list()

  for (yr in years) {
    for (ag in age_groups) {

      # Subset data
      data_subset <- data %>%
        filter(
          year == yr,
          .data[[age_var]] == ag,
          !is.na(srh),
          !is.na(.data[[covariate_var]]),
          .data[[covariate_var]] %in% covariate_levels,
          !is.na(.data[[weight_var]]),
          .data[[weight_var]] > 0
        )

      # Set factor with reference level
      data_subset[[covariate_var]] <- factor(
        data_subset[[covariate_var]],
        levels = c(reference_level, non_ref_levels)
      )

      n <- nrow(data_subset)

      if (n < min_n || length(unique(data_subset[[covariate_var]])) < 2) {
        for (lvl in non_ref_levels) {
          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            age_group = ag,
            level = lvl,
            beta = NA_real_,
            se = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            p_value = NA_real_,
            n = n,
            is_significant = NA,
            stringsAsFactors = FALSE
          )
        }
        next
      }

      tryCatch({
        svy_design <- create_survey_design(
          data_subset, weight_var, psu_var, strata_var
        )

        formula <- as.formula(paste0("srh ~ ", covariate_var))
        model <- safe_svyglm(formula, design = svy_design)

        if (is.null(model)) {
          for (lvl in non_ref_levels) {
            results_list[[length(results_list) + 1]] <- data.frame(
              year = yr,
              age_group = ag,
              level = lvl,
              beta = NA_real_,
              se = NA_real_,
              ci_lower = NA_real_,
              ci_upper = NA_real_,
              p_value = NA_real_,
              n = n,
              is_significant = NA,
              stringsAsFactors = FALSE
            )
          }
          next
        }

        coef_summary <- summary(model)$coefficients
        ci <- confint(model, level = 0.95)

        for (lvl in non_ref_levels) {
          coef_name <- paste0(covariate_var, lvl)

          if (coef_name %in% rownames(coef_summary)) {
            results_list[[length(results_list) + 1]] <- data.frame(
              year = yr,
              age_group = ag,
              level = lvl,
              beta = coef_summary[coef_name, "Estimate"],
              se = coef_summary[coef_name, "Std. Error"],
              ci_lower = ci[coef_name, 1],
              ci_upper = ci[coef_name, 2],
              p_value = coef_summary[coef_name, "Pr(>|t|)"],
              n = n,
              is_significant = coef_summary[coef_name, "Pr(>|t|)"] < 0.05,
              stringsAsFactors = FALSE
            )
          } else {
            results_list[[length(results_list) + 1]] <- data.frame(
              year = yr,
              age_group = ag,
              level = lvl,
              beta = NA_real_,
              se = NA_real_,
              ci_lower = NA_real_,
              ci_upper = NA_real_,
              p_value = NA_real_,
              n = n,
              is_significant = NA,
              stringsAsFactors = FALSE
            )
          }
        }

      }, error = function(e) {
        for (lvl in non_ref_levels) {
          results_list[[length(results_list) + 1]] <<- data.frame(
            year = yr,
            age_group = ag,
            level = lvl,
            beta = NA_real_,
            se = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            p_value = NA_real_,
            n = n,
            is_significant = NA,
            stringsAsFactors = FALSE
          )
        }
      })
    }
  }

  results <- bind_rows(results_list)
  return(results)
}


# ==============================================================================
# FUNCTION 6: R-SQUARED
# ==============================================================================

#' Compute survey-weighted R² for a formula by year
#'
#' @description
#' For each year, computes survey-weighted R² for the given formula.
#' Uses the survey package's pseudo R² (1 - residual variance / total variance).
#'
#' @param data Data frame
#' @param formula Model formula as character string (e.g., "srh ~ age")
#' @param weight_var Name of weight variable (default "wt")
#' @param min_n Minimum sample size (default 30)
#' @param psu_var Name of PSU variable (NULL if not available)
#' @param strata_var Name of strata variable (NULL if not available)
#'
#' @return Data frame with: year, r_squared, n
#'
#' @examples
#' # r2 <- compute_r_squared(data_nhis, "srh ~ age")
#'
compute_r_squared <- function(data,
                               formula,
                               weight_var = "wt",
                               min_n = 30,
                               psu_var = NULL,
                               strata_var = NULL) {

  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))

  formula_obj <- as.formula(formula)
  response_var <- all.vars(formula_obj)[1]

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  years <- sort(unique(data$year))
  results_list <- list()

  for (yr in years) {

    # Subset data for this year
    data_year <- data %>%
      filter(
        year == yr,
        !is.na(.data[[weight_var]]),
        .data[[weight_var]] > 0
      )

    # Remove rows with NA in any variable in the formula
    all_vars <- all.vars(formula_obj)
    complete_mask <- complete.cases(data_year[, all_vars, drop = FALSE])
    data_year <- data_year[complete_mask, , drop = FALSE]

    n <- nrow(data_year)

    if (n < min_n) {
      results_list[[length(results_list) + 1]] <- data.frame(
        year = yr,
        r_squared = NA_real_,
        n = n,
        stringsAsFactors = FALSE
      )
      next
    }

    tryCatch({
      svy_design <- create_survey_design(
        data_year, weight_var, psu_var, strata_var
      )

      model <- safe_svyglm(formula_obj, design = svy_design)

      if (is.null(model)) {
        results_list[[length(results_list) + 1]] <- data.frame(
          year = yr,
          r_squared = NA_real_,
          n = n,
          stringsAsFactors = FALSE
        )
        next
      }

      # Compute pseudo R² = 1 - (residual variance / total variance)
      # Using weighted variance of response and residuals
      weights_vec <- weights(svy_design)
      y <- data_year[[response_var]]
      fitted_vals <- fitted(model)
      residuals <- y - fitted_vals

      # Weighted total variance
      total_var <- sum(weights_vec * (y - weighted.mean(y, weights_vec))^2) /
                   sum(weights_vec)

      # Weighted residual variance
      resid_var <- sum(weights_vec * residuals^2) / sum(weights_vec)

      r_squared <- 1 - (resid_var / total_var)

      # Ensure R² is in valid range [0, 1]
      r_squared <- max(0, min(1, r_squared))

      results_list[[length(results_list) + 1]] <- data.frame(
        year = yr,
        r_squared = r_squared,
        n = n,
        stringsAsFactors = FALSE
      )

    }, error = function(e) {
      results_list[[length(results_list) + 1]] <<- data.frame(
        year = yr,
        r_squared = NA_real_,
        n = n,
        stringsAsFactors = FALSE
      )
    })
  }

  results <- bind_rows(results_list)
  return(results)
}


# ==============================================================================
# FUNCTION 7: MODEL COMPARISON R²
# ==============================================================================

#' Compute R² for 4 models comparing age and covariate effects
#'
#' @description
#' For each year, computes R² for 4 nested models:
#'   1. SRH ~ age
#'   2. SRH ~ covariate
#'   3. SRH ~ age + covariate
#'   4. SRH ~ age + covariate + age:covariate
#' Used for Page 3 SV7.
#'
#' @param data Data frame
#' @param covariate_var Name of the covariate variable (character)
#' @param weight_var Name of weight variable (default "wt")
#' @param min_n Minimum sample size (default 30)
#' @param psu_var Name of PSU variable (NULL if not available)
#' @param strata_var Name of strata variable (NULL if not available)
#'
#' @return Data frame with: year, model (1-4), r_squared, n
#'
compute_model_comparison_r2 <- function(data,
                                         covariate_var,
                                         weight_var = "wt",
                                         min_n = 30,
                                         psu_var = NULL,
                                         strata_var = NULL) {

  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot("srh" %in% names(data))
  stopifnot("age" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))

  # Define the 4 models
  formulas <- list(
    "1" = "srh ~ age",
    "2" = paste0("srh ~ ", covariate_var),
    "3" = paste0("srh ~ age + ", covariate_var),
    "4" = paste0("srh ~ age * ", covariate_var)
  )

  results_list <- list()

  for (model_num in names(formulas)) {
    formula_str <- formulas[[model_num]]

    r2_results <- compute_r_squared(
      data = data,
      formula = formula_str,
      weight_var = weight_var,
      min_n = min_n,
      psu_var = psu_var,
      strata_var = strata_var
    )

    r2_results$model <- as.integer(model_num)
    results_list[[model_num]] <- r2_results
  }

  results <- bind_rows(results_list)
  results <- results %>% select(year, model, r_squared, n)

  return(results)
}


# ==============================================================================
# FUNCTION 8: COVARIATE COMPOSITION
# ==============================================================================

#' Compute covariate composition by age group over time
#'
#' @description
#' For each year × age group, computes survey-weighted proportion in each
#' covariate category. Used for Page 4.
#'
#' @param data Data frame
#' @param covariate_var Name of the covariate variable (character)
#' @param covariate_levels Character vector of covariate levels
#' @param age_var Name of age group variable (default "age_group")
#' @param weight_var Name of weight variable (default "wt")
#'
#' @return Data frame with: year, age_group, level, proportion, n
#'
compute_composition <- function(data,
                                 covariate_var,
                                 covariate_levels,
                                 age_var = "age_group",
                                 weight_var = "wt") {

  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot(age_var %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))

  results <- data %>%
    filter(
      !is.na(.data[[covariate_var]]),
      .data[[covariate_var]] %in% covariate_levels,
      !is.na(.data[[weight_var]]),
      .data[[weight_var]] > 0,
      !is.na(.data[[age_var]])
    ) %>%
    group_by(year, .data[[age_var]]) %>%
    mutate(total_weight = sum(.data[[weight_var]])) %>%
    group_by(year, .data[[age_var]], .data[[covariate_var]]) %>%
    summarize(
      proportion = sum(.data[[weight_var]]) / first(total_weight),
      n = n(),
      .groups = "drop"
    ) %>%
    rename(
      age_group = !!sym(age_var),
      level = !!sym(covariate_var)
    )

  return(results)
}


# ==============================================================================
# FUNCTION 9: SRH COMPOSITION BY COVARIATE
# ==============================================================================

#' Compute SRH distribution within covariate strata over time
#'
#' @description
#' For each year × covariate stratum, computes survey-weighted proportion
#' in each SRH category (1-5). Used for Page 5.
#'
#' @param data Data frame
#' @param covariate_var Name of the covariate variable (character)
#' @param covariate_levels Character vector of covariate levels
#' @param weight_var Name of weight variable (default "wt")
#'
#' @return Data frame with: year, stratum, srh_level, proportion, n
#'
compute_srh_composition <- function(data,
                                     covariate_var,
                                     covariate_levels,
                                     weight_var = "wt") {

  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot("srh" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))

  results <- data %>%
    filter(
      !is.na(.data[[covariate_var]]),
      .data[[covariate_var]] %in% covariate_levels,
      !is.na(srh),
      !is.na(.data[[weight_var]]),
      .data[[weight_var]] > 0
    ) %>%
    group_by(year, .data[[covariate_var]]) %>%
    mutate(total_weight = sum(.data[[weight_var]])) %>%
    group_by(year, .data[[covariate_var]], srh) %>%
    summarize(
      proportion = sum(.data[[weight_var]]) / first(total_weight),
      n = n(),
      .groups = "drop"
    ) %>%
    rename(
      stratum = !!sym(covariate_var),
      srh_level = srh
    )

  return(results)
}


# ==============================================================================
# FUNCTION 10: FOREST PLOT COEFFICIENTS
# ==============================================================================

#' Compute covariate coefficients controlling for age (for forest plots)
#'
#' @description
#' For each year, runs survey-weighted regression SRH ~ age + covariate
#' and returns coefficients for non-reference covariate levels.
#' Used for Page 6 forest plots.
#'
#' @param data Data frame
#' @param covariate_var Name of the covariate variable (character)
#' @param covariate_levels Character vector of covariate levels
#' @param reference_level The reference level for the covariate
#' @param weight_var Name of weight variable (default "wt")
#' @param min_n Minimum sample size (default 30)
#' @param psu_var Name of PSU variable (NULL if not available)
#' @param strata_var Name of strata variable (NULL if not available)
#'
#' @return Data frame with: year, level, beta, se, ci_lower, ci_upper,
#'   p_value, is_significant, n
#'
compute_forest_coefficients <- function(data,
                                         covariate_var,
                                         covariate_levels,
                                         reference_level,
                                         weight_var = "wt",
                                         min_n = 30,
                                         psu_var = NULL,
                                         strata_var = NULL) {

  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot("srh" %in% names(data))
  stopifnot("age" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))
  stopifnot(reference_level %in% covariate_levels)

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  non_ref_levels <- setdiff(covariate_levels, reference_level)

  years <- sort(unique(data$year))
  results_list <- list()

  for (yr in years) {

    # Subset data
    data_year <- data %>%
      filter(
        year == yr,
        !is.na(srh),
        !is.na(age),
        !is.na(.data[[covariate_var]]),
        .data[[covariate_var]] %in% covariate_levels,
        !is.na(.data[[weight_var]]),
        .data[[weight_var]] > 0
      )

    # Set factor with reference level
    data_year[[covariate_var]] <- factor(
      data_year[[covariate_var]],
      levels = c(reference_level, non_ref_levels)
    )

    n <- nrow(data_year)

    if (n < min_n || length(unique(data_year[[covariate_var]])) < 2) {
      for (lvl in non_ref_levels) {
        results_list[[length(results_list) + 1]] <- data.frame(
          year = yr,
          level = lvl,
          beta = NA_real_,
          se = NA_real_,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          p_value = NA_real_,
          is_significant = NA,
          n = n,
          stringsAsFactors = FALSE
        )
      }
      next
    }

    tryCatch({
      svy_design <- create_survey_design(
        data_year, weight_var, psu_var, strata_var
      )

      formula <- as.formula(paste0("srh ~ age + ", covariate_var))
      model <- safe_svyglm(formula, design = svy_design)

      if (is.null(model)) {
        for (lvl in non_ref_levels) {
          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            level = lvl,
            beta = NA_real_,
            se = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            p_value = NA_real_,
            is_significant = NA,
            n = n,
            stringsAsFactors = FALSE
          )
        }
        next
      }

      coef_summary <- summary(model)$coefficients
      ci <- confint(model, level = 0.95)

      for (lvl in non_ref_levels) {
        coef_name <- paste0(covariate_var, lvl)

        if (coef_name %in% rownames(coef_summary)) {
          p_val <- coef_summary[coef_name, "Pr(>|t|)"]

          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            level = lvl,
            beta = coef_summary[coef_name, "Estimate"],
            se = coef_summary[coef_name, "Std. Error"],
            ci_lower = ci[coef_name, 1],
            ci_upper = ci[coef_name, 2],
            p_value = p_val,
            is_significant = p_val < 0.05,
            n = n,
            stringsAsFactors = FALSE
          )
        } else {
          results_list[[length(results_list) + 1]] <- data.frame(
            year = yr,
            level = lvl,
            beta = NA_real_,
            se = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            p_value = NA_real_,
            is_significant = NA,
            n = n,
            stringsAsFactors = FALSE
          )
        }
      }

    }, error = function(e) {
      for (lvl in non_ref_levels) {
        results_list[[length(results_list) + 1]] <<- data.frame(
          year = yr,
          level = lvl,
          beta = NA_real_,
          se = NA_real_,
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          p_value = NA_real_,
          is_significant = NA,
          n = n,
          stringsAsFactors = FALSE
        )
      }
    })
  }

  results <- bind_rows(results_list)
  return(results)
}


# ==============================================================================
# TEST EXAMPLE (commented out)
# ==============================================================================

# # Simple test with sex as covariate on one dataset
# if (FALSE) {
#   library(here)
#   source(here::here("R/paths.R"))
#
#   # Load one dataset
#   data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) %>%
#     drop_na(srh, age, year, wt)
#
#   # Check if sex variable exists and its levels
#   print(table(data_nhis$sex, useNA = "ifany"))
#
#   # Test compute_stratified_age_coefficients
#   strat_coefs <- compute_stratified_age_coefficients(
#     data = data_nhis,
#     covariate_var = "sex",
#     covariate_levels = c("Male", "Female"),
#     weight_var = "wt"
#   )
#   print(head(strat_coefs))
#
#   # Test compute_meta_regression_trend
#   male_coefs <- strat_coefs %>% filter(stratum == "Male")
#   trend <- compute_meta_regression_trend(male_coefs)
#   print(trend)
#
#   # Test compute_covariate_coefficients
#   cov_coefs <- compute_covariate_coefficients(
#     data = data_nhis,
#     covariate_var = "sex",
#     covariate_levels = c("Male", "Female"),
#     reference_level = "Male",
#     weight_var = "wt"
#   )
#   print(head(cov_coefs))
#
#   # Test compute_r_squared
#   r2 <- compute_r_squared(data_nhis, "srh ~ age", weight_var = "wt")
#   print(head(r2))
#
#   # Test compute_model_comparison_r2
#   model_r2 <- compute_model_comparison_r2(
#     data = data_nhis,
#     covariate_var = "sex",
#     weight_var = "wt"
#   )
#   print(head(model_r2))
#
#   # Test compute_composition
#   comp <- compute_composition(
#     data = data_nhis,
#     covariate_var = "sex",
#     covariate_levels = c("Male", "Female"),
#     age_var = "age_group",
#     weight_var = "wt"
#   )
#   print(head(comp))
#
#   # Test compute_srh_composition
#   srh_comp <- compute_srh_composition(
#     data = data_nhis,
#     covariate_var = "sex",
#     covariate_levels = c("Male", "Female"),
#     weight_var = "wt"
#   )
#   print(head(srh_comp))
#
#   # Test compute_forest_coefficients
#   forest <- compute_forest_coefficients(
#     data = data_nhis,
#     covariate_var = "sex",
#     covariate_levels = c("Male", "Female"),
#     reference_level = "Male",
#     weight_var = "wt"
#   )
#   print(head(forest))
# }
