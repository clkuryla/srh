# ==============================================================================
# covariate_analysis_functions.R
# Core Statistical Functions for SRH Covariate Sensitivity Analysis
#
# Purpose: Multi-dataset functions to analyze how covariates relate to
#          self-rated health trends and age gradients across surveys.
#
# Key Design:
#   - All functions take a named list of datasets and loop through them
#   - All return tibbles with a `dataset` column for combined analysis
#   - BRFSS always uses weights-only design (no PSU/strata)
#   - Ages >= 90 are filtered out
#   - SRH coding: higher = better (1-5 for most, 1-4 for GSS)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(tidyr)
library(survey)
library(broom)

# ==============================================================================
# INTERNAL HELPER FUNCTIONS
# ==============================================================================

#' Create survey design with BRFSS exception
#'
#' @param data Data frame to create design from
#' @param dataset_name Name of the dataset (used to detect BRFSS)
#' @param wt_var Weight variable name
#' @param psu_var PSU variable name (ignored for BRFSS)
#' @param strata_var Strata variable name (ignored for BRFSS)
#'
#' @return survey.design object
#' @keywords internal
.create_survey_design <- function(
    data,
    dataset_name,
    wt_var = "wt",
    psu_var = "psu",
    strata_var = "strata"
) {
  # BRFSS exception: always weights-only due to computational constraints
  if (toupper(dataset_name) == "BRFSS") {
    return(svydesign(
      ids = ~1,
      weights = as.formula(paste0("~", wt_var)),
      data = data
    ))
  }

  # Check which design elements are available
  has_psu <- !is.null(psu_var) && psu_var %in% names(data) &&
    !all(is.na(data[[psu_var]]))
  has_strata <- !is.null(strata_var) && strata_var %in% names(data) &&
    !all(is.na(data[[strata_var]]))

  # Create design based on available elements
  if (has_psu && has_strata) {
    svydesign(
      ids = as.formula(paste0("~", psu_var)),
      strata = as.formula(paste0("~", strata_var)),
      weights = as.formula(paste0("~", wt_var)),
      data = data,
      nest = TRUE
    )
  } else if (has_psu) {
    svydesign(
      ids = as.formula(paste0("~", psu_var)),
      weights = as.formula(paste0("~", wt_var)),
      data = data
    )
  } else if (has_strata) {
    svydesign(
      ids = ~1,
      strata = as.formula(paste0("~", strata_var)),
      weights = as.formula(paste0("~", wt_var)),
      data = data
    )
  } else {
    svydesign(
      ids = ~1,
      weights = as.formula(paste0("~", wt_var)),
      data = data
    )
  }
}


#' Validate covariate data
#'
#' @param data Data frame to validate
#' @param covariate Covariate variable name
#' @param required_vars Vector of required variable names
#'
#' @return TRUE if valid, stops with error otherwise
#' @keywords internal
.validate_covariate_data <- function(
    data,
    covariate,
    required_vars = c("srh", "age", "year", "wt")
) {
  stopifnot(is.data.frame(data))

  # Check required variables
  missing_vars <- setdiff(c(required_vars, covariate), names(data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }

  # Check SRH range (1-5 for most surveys, 1-4 for GSS)
  srh_range <- range(data$srh, na.rm = TRUE)
  if (srh_range[1] < 1 || srh_range[2] > 5) {
    warning("SRH values outside expected range [1,5]: [",
            srh_range[1], ", ", srh_range[2], "]")
  }

  TRUE
}


#' Filter out ages >= 90
#'
#' @param data Data frame with age column
#' @param age_var Name of age variable
#' @param max_age Maximum age to include (default 89)
#'
#' @return Filtered data frame
#' @keywords internal
.filter_age_range <- function(data, age_var = "age", max_age = 89) {
  data %>%
    filter(.data[[age_var]] <= max_age)
}


#' Prepare data for analysis
#'
#' @param data Data frame
#' @param covariate Covariate variable name
#' @param max_age Maximum age to include
#'
#' @return Cleaned data frame
#' @keywords internal
.prepare_data <- function(data, covariate, max_age = 89) {
  data %>%
    drop_na(srh, age, year, wt, !!sym(covariate)) %>%
    filter(age <= max_age, wt > 0)
}


#' Run analysis across all datasets
#'
#' @param datasets Named list of data frames
#' @param fn Function to apply to each dataset
#' @param ... Additional arguments passed to fn
#'
#' @return Combined tibble with dataset column
#' @keywords internal
.run_across_datasets <- function(datasets, fn, ...) {
  results <- list()

  for (i in seq_along(datasets)) {
    dataset_name <- names(datasets)[i]
    data <- datasets[[i]]

    result <- tryCatch({
      fn(data = data, dataset_name = dataset_name, ...)
    }, error = function(e) {
      warning("Failed for ", dataset_name, ": ", e$message)
      NULL
    })

    if (!is.null(result) && nrow(result) > 0) {
      result$dataset <- dataset_name
      results[[dataset_name]] <- result
    }
  }

  if (length(results) == 0) {
    warning("No valid results from any dataset")
    return(tibble())
  }

  bind_rows(results) %>%
    select(dataset, everything())
}


# ==============================================================================
# MAIN ANALYSIS FUNCTIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. compute_stratified_betas
# ------------------------------------------------------------------------------

#' Compute age coefficients stratified by covariate levels
#'
#' @description
#' For each dataset × year × covariate_level combination, runs:
#'   SRH ~ age
#' and extracts the age coefficient. This shows how the age-SRH relationship
#' varies by covariate level and whether it changes over time.
#'
#' @param datasets Named list of data frames (one per survey)
#' @param covariate Character string: name of covariate variable
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_var Name of age variable (default "age")
#' @param year_var Name of year variable (default "year")
#' @param wt_var Name of weight variable (default "wt")
#' @param psu_var Name of PSU variable (default "psu")
#' @param strata_var Name of strata variable (default "strata")
#' @param min_n Minimum observations per cell (default 100)
#' @param ci_level Confidence level (default 0.95)
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return Tibble with columns:
#'   dataset, year, covariate_level, beta_age, se, ci_low, ci_high, p_value, n
#'
#' @export
compute_stratified_betas <- function(
    datasets,
    covariate,
    srh_var = "srh",
    age_var = "age",
    year_var = "year",
    wt_var = "wt",
    psu_var = "psu",
    strata_var = "strata",
    min_n = 100,
    ci_level = 0.95,
    lonely_psu = "adjust"
) {

  # Set survey options
 old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Inner function for single dataset
  .compute_stratified_betas_single <- function(data, dataset_name, ...) {

    # Validate and prepare
    .validate_covariate_data(data, covariate)
    data <- .prepare_data(data, covariate)

    # Get unique years and covariate levels
    years <- sort(unique(data[[year_var]]))
    covariate_levels <- sort(unique(data[[covariate]]))

    message("Processing ", dataset_name, ": ", length(years), " years × ",
            length(covariate_levels), " levels of ", covariate)

    results_list <- list()

    for (yr in years) {
      for (lev in covariate_levels) {

        # Subset to this year and covariate level
        data_subset <- data %>%
          filter(.data[[year_var]] == yr, .data[[covariate]] == lev)

        n <- nrow(data_subset)

        # Skip if too few observations
        if (n < min_n) next

        # Skip if no variation in SRH or age
        if (length(unique(data_subset[[srh_var]])) < 2) next
        if (length(unique(data_subset[[age_var]])) < 2) next

        tryCatch({
          # Create survey design
          svy_design <- .create_survey_design(
            data_subset, dataset_name, wt_var, psu_var, strata_var
          )

          # Fit model: SRH ~ age
          formula <- as.formula(paste0(srh_var, " ~ ", age_var))
          model <- svyglm(formula, design = svy_design)

          # Extract age coefficient
          coef_summary <- summary(model)$coefficients
          age_row <- coef_summary[age_var, ]

          beta_age <- age_row["Estimate"]
          se <- age_row["Std. Error"]
          p_value <- age_row["Pr(>|t|)"]

          # Confidence interval
          ci <- confint(model, level = ci_level)[age_var, ]

          results_list[[length(results_list) + 1]] <- tibble(
            year = yr,
            covariate_level = as.character(lev),
            beta_age = beta_age,
            se = se,
            ci_low = ci[1],
            ci_high = ci[2],
            p_value = p_value,
            n = n
          )

        }, error = function(e) {
          # Silently skip failed cells
        })
      }
    }

    if (length(results_list) == 0) {
      return(tibble())
    }

    bind_rows(results_list)
  }

  # Run across all datasets
  .run_across_datasets(
    datasets,
    .compute_stratified_betas_single,
    covariate = covariate,
    srh_var = srh_var,
    age_var = age_var,
    year_var = year_var,
    wt_var = wt_var,
    psu_var = psu_var,
    strata_var = strata_var,
    min_n = min_n,
    ci_level = ci_level
  )
}


# ------------------------------------------------------------------------------
# 2. compute_beta_trends
# ------------------------------------------------------------------------------

#' Compute trends in age coefficients over time
#'
#' @description
#' For each dataset × covariate_level, fits:
#'   beta_age ~ year
#' using inverse-variance weighting (1/se²) to quantify whether the age-SRH
#' relationship is changing over time within each covariate stratum.
#'
#' @param stratified_betas Output from compute_stratified_betas()
#' @param covariate Character string: covariate name (for labeling)
#'
#' @return Tibble with columns:
#'   dataset, covariate_level, slope, slope_se, p_value, r_squared, n_years, interpretation
#'
#' @export
compute_beta_trends <- function(stratified_betas, covariate = NULL) {

  if (nrow(stratified_betas) == 0) {
    warning("No data in stratified_betas")
    return(tibble())
  }

  # Group by dataset and covariate level
  results <- stratified_betas %>%
    filter(!is.na(se), se > 0) %>%
    group_by(dataset, covariate_level) %>%
    filter(n() >= 3) %>%  # Need at least 3 years for trend
    group_modify(function(df, keys) {

      # Compute inverse-variance weights
      df <- df %>%
        mutate(iv_weight = 1 / (se^2))

      # Fit weighted linear regression
      tryCatch({
        model <- lm(beta_age ~ year, data = df, weights = iv_weight)
        model_summary <- summary(model)

        slope <- coef(model)["year"]
        slope_se <- model_summary$coefficients["year", "Std. Error"]
        p_value <- model_summary$coefficients["year", "Pr(>|t|)"]
        r_squared <- model_summary$r.squared

        # Interpretation: positive slope means coefficient trending toward zero
        # (assuming original coefficient is negative, which is typical)
        interpretation <- case_when(
          p_value > 0.05 ~ "No significant trend",
          slope > 0 ~ "Age coefficient trending toward zero (convergence)",
          slope < 0 ~ "Age coefficient trending away from zero (divergence)",
          TRUE ~ "No significant trend"
        )

        tibble(
          slope = slope,
          slope_se = slope_se,
          p_value = p_value,
          r_squared = r_squared,
          n_years = nrow(df),
          interpretation = interpretation
        )

      }, error = function(e) {
        tibble(
          slope = NA_real_,
          slope_se = NA_real_,
          p_value = NA_real_,
          r_squared = NA_real_,
          n_years = nrow(df),
          interpretation = "Model failed"
        )
      })
    }) %>%
    ungroup()

  results
}


# ------------------------------------------------------------------------------
# 3. compute_predicted_srh
# ------------------------------------------------------------------------------

#' Compute predicted SRH at representative ages
#'
#' @description
#' For each dataset × year, fits:
#'   SRH ~ age * covariate
#' and predicts SRH at representative ages for each covariate level.
#' Shows how the age gradient manifests across covariate groups.
#'
#' @param datasets Named list of data frames
#' @param covariate Character string: covariate variable name
#' @param representative_ages Ages at which to predict (default c(25, 45, 65, 85))
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_var Name of age variable (default "age")
#' @param year_var Name of year variable (default "year")
#' @param wt_var Name of weight variable (default "wt")
#' @param psu_var Name of PSU variable (default "psu")
#' @param strata_var Name of strata variable (default "strata")
#' @param min_n Minimum observations per year (default 500)
#' @param ci_level Confidence level (default 0.95)
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return Tibble with columns:
#'   dataset, year, age, covariate_level, predicted_srh, se, ci_low, ci_high
#'
#' @export
compute_predicted_srh <- function(
    datasets,
    covariate,
    representative_ages = c(25, 45, 65, 85),
    srh_var = "srh",
    age_var = "age",
    year_var = "year",
    wt_var = "wt",
    psu_var = "psu",
    strata_var = "strata",
    min_n = 500,
    ci_level = 0.95,
    lonely_psu = "adjust"
) {

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Inner function for single dataset
  .compute_predicted_single <- function(data, dataset_name, ...) {

    # Validate and prepare
    .validate_covariate_data(data, covariate)
    data <- .prepare_data(data, covariate)

    # Get unique years and covariate levels
    years <- sort(unique(data[[year_var]]))
    covariate_levels <- sort(unique(data[[covariate]]))

    message("Processing ", dataset_name, ": ", length(years), " years, predicting at ",
            length(representative_ages), " ages × ", length(covariate_levels), " levels")

    results_list <- list()

    for (yr in years) {

      # Subset to this year
      data_year <- data %>%
        filter(.data[[year_var]] == yr)

      n <- nrow(data_year)

      # Skip if too few observations
      if (n < min_n) next

      # Skip if insufficient variation
      if (length(unique(data_year[[srh_var]])) < 2) next
      if (length(unique(data_year[[age_var]])) < 3) next
      if (length(unique(data_year[[covariate]])) < 2) next

      tryCatch({
        # Create survey design
        svy_design <- .create_survey_design(
          data_year, dataset_name, wt_var, psu_var, strata_var
        )

        # Fit interaction model: SRH ~ age * covariate
        formula <- as.formula(paste0(srh_var, " ~ ", age_var, " * factor(", covariate, ")"))
        model <- svyglm(formula, design = svy_design)

        # Create prediction grid
        pred_grid <- expand.grid(
          age = representative_ages,
          covariate_level = covariate_levels,
          stringsAsFactors = FALSE
        )
        names(pred_grid)[1] <- age_var
        names(pred_grid)[2] <- covariate

        # Convert covariate to factor with same levels as in model
        pred_grid[[covariate]] <- factor(pred_grid[[covariate]], levels = covariate_levels)

        # Get predictions with standard errors
        pred <- predict(model, newdata = pred_grid, se.fit = TRUE)

        # Calculate CI
        z <- qnorm(1 - (1 - ci_level) / 2)

        pred_results <- tibble(
          year = yr,
          age = pred_grid[[age_var]],
          covariate_level = as.character(pred_grid[[covariate]]),
          predicted_srh = as.numeric(pred),
          se = as.numeric(sqrt(attr(pred, "var")))
        ) %>%
          mutate(
            ci_low = predicted_srh - z * se,
            ci_high = predicted_srh + z * se
          )

        results_list[[length(results_list) + 1]] <- pred_results

      }, error = function(e) {
        # Silently skip failed years
      })
    }

    if (length(results_list) == 0) {
      return(tibble())
    }

    bind_rows(results_list)
  }

  # Run across all datasets
  .run_across_datasets(
    datasets,
    .compute_predicted_single,
    covariate = covariate,
    representative_ages = representative_ages,
    srh_var = srh_var,
    age_var = age_var,
    year_var = year_var,
    wt_var = wt_var,
    psu_var = psu_var,
    strata_var = strata_var,
    min_n = min_n,
    ci_level = ci_level
  )
}


# ------------------------------------------------------------------------------
# 4. compute_beta_covariate
# ------------------------------------------------------------------------------

#' Compute covariate coefficients over time
#'
#' @description
#' For each dataset × year, fits:
#'   SRH ~ age + covariate
#' and extracts the covariate coefficient(s). Shows how strongly the covariate
#' predicts SRH, controlling for age, and whether this changes over time.
#'
#' @param datasets Named list of data frames
#' @param covariate Character string: covariate variable name
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_var Name of age variable (default "age")
#' @param year_var Name of year variable (default "year")
#' @param wt_var Name of weight variable (default "wt")
#' @param psu_var Name of PSU variable (default "psu")
#' @param strata_var Name of strata variable (default "strata")
#' @param min_n Minimum observations per year (default 500)
#' @param ci_level Confidence level (default 0.95)
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return Tibble with columns:
#'   dataset, year, term, contrast, beta, se, ci_low, ci_high, p_value, n
#'
#' @details
#' For categorical covariates, returns one row per contrast level (vs reference).
#' For continuous covariates, returns one row per year.
#'
#' @export
compute_beta_covariate <- function(
    datasets,
    covariate,
    srh_var = "srh",
    age_var = "age",
    year_var = "year",
    wt_var = "wt",
    psu_var = "psu",
    strata_var = "strata",
    min_n = 500,
    ci_level = 0.95,
    lonely_psu = "adjust"
) {

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Inner function for single dataset
  .compute_beta_cov_single <- function(data, dataset_name, ...) {

    # Validate and prepare
    .validate_covariate_data(data, covariate)
    data <- .prepare_data(data, covariate)

    # Determine if covariate is categorical
    is_categorical <- is.factor(data[[covariate]]) ||
      is.character(data[[covariate]]) ||
      length(unique(data[[covariate]])) <= 10

    # Get unique years
    years <- sort(unique(data[[year_var]]))

    message("Processing ", dataset_name, ": ", length(years), " years, covariate = ",
            covariate, " (", ifelse(is_categorical, "categorical", "continuous"), ")")

    results_list <- list()

    for (yr in years) {

      # Subset to this year
      data_year <- data %>%
        filter(.data[[year_var]] == yr)

      n <- nrow(data_year)

      # Skip if too few observations
      if (n < min_n) next

      tryCatch({
        # Create survey design
        svy_design <- .create_survey_design(
          data_year, dataset_name, wt_var, psu_var, strata_var
        )

        # Fit model: SRH ~ age + covariate
        if (is_categorical) {
          formula <- as.formula(paste0(srh_var, " ~ ", age_var, " + factor(", covariate, ")"))
        } else {
          formula <- as.formula(paste0(srh_var, " ~ ", age_var, " + ", covariate))
        }

        model <- svyglm(formula, design = svy_design)

        # Extract coefficients using broom
        tidy_results <- broom::tidy(model, conf.int = TRUE, conf.level = ci_level)

        # Filter to covariate terms (exclude intercept and age)
        covar_results <- tidy_results %>%
          filter(grepl(covariate, term, fixed = TRUE) |
                   grepl(paste0("factor\\(", covariate, "\\)"), term)) %>%
          mutate(
            year = yr,
            covariate_name = covariate,
            # Extract contrast level from term name
            contrast = gsub(paste0("factor\\(", covariate, "\\)"), "", term),
            contrast = ifelse(contrast == covariate, "continuous", contrast),
            n = n
          ) %>%
          select(
            year,
            term,
            contrast,
            beta = estimate,
            se = std.error,
            ci_low = conf.low,
            ci_high = conf.high,
            p_value = p.value,
            n
          )

        results_list[[length(results_list) + 1]] <- covar_results

      }, error = function(e) {
        # Silently skip failed years
      })
    }

    if (length(results_list) == 0) {
      return(tibble())
    }

    bind_rows(results_list)
  }

  # Run across all datasets
  .run_across_datasets(
    datasets,
    .compute_beta_cov_single,
    covariate = covariate,
    srh_var = srh_var,
    age_var = age_var,
    year_var = year_var,
    wt_var = wt_var,
    psu_var = psu_var,
    strata_var = strata_var,
    min_n = min_n,
    ci_level = ci_level
  )
}


# ------------------------------------------------------------------------------
# 5. compute_interaction
# ------------------------------------------------------------------------------

#' Compute age × covariate interaction coefficients
#'
#' @description
#' For each dataset × year, fits main effects and interaction models:
#'   Main: SRH ~ age + covariate
#'   Interaction: SRH ~ age * covariate
#' Performs Wald F-test for interaction and extracts interaction coefficients.
#'
#' @param datasets Named list of data frames
#' @param covariate Character string: covariate variable name
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_var Name of age variable (default "age")
#' @param year_var Name of year variable (default "year")
#' @param wt_var Name of weight variable (default "wt")
#' @param psu_var Name of PSU variable (default "psu")
#' @param strata_var Name of strata variable (default "strata")
#' @param min_n Minimum observations per year (default 500)
#' @param ci_level Confidence level (default 0.95)
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return Tibble with columns:
#'   dataset, year, term, beta_interaction, se, ci_low, ci_high,
#'   wald_fstat, wald_df, wald_ddf, wald_p_value, n
#'
#' @export
compute_interaction <- function(
    datasets,
    covariate,
    srh_var = "srh",
    age_var = "age",
    year_var = "year",
    wt_var = "wt",
    psu_var = "psu",
    strata_var = "strata",
    min_n = 500,
    ci_level = 0.95,
    lonely_psu = "adjust"
) {

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Inner function for single dataset
  .compute_interaction_single <- function(data, dataset_name, ...) {

    # Validate and prepare
    .validate_covariate_data(data, covariate)
    data <- .prepare_data(data, covariate)

    # Determine if covariate is categorical
    is_categorical <- is.factor(data[[covariate]]) ||
      is.character(data[[covariate]]) ||
      length(unique(data[[covariate]])) <= 10

    # Get unique years
    years <- sort(unique(data[[year_var]]))

    message("Processing ", dataset_name, ": ", length(years),
            " years, testing age × ", covariate, " interaction")

    results_list <- list()

    for (yr in years) {

      # Subset to this year
      data_year <- data %>%
        filter(.data[[year_var]] == yr)

      n <- nrow(data_year)

      # Skip if too few observations
      if (n < min_n) next

      tryCatch({
        # Create survey design
        svy_design <- .create_survey_design(
          data_year, dataset_name, wt_var, psu_var, strata_var
        )

        # Fit main effects model
        if (is_categorical) {
          formula_main <- as.formula(paste0(srh_var, " ~ ", age_var, " + factor(", covariate, ")"))
          formula_int <- as.formula(paste0(srh_var, " ~ ", age_var, " * factor(", covariate, ")"))
        } else {
          formula_main <- as.formula(paste0(srh_var, " ~ ", age_var, " + ", covariate))
          formula_int <- as.formula(paste0(srh_var, " ~ ", age_var, " * ", covariate))
        }

        model_main <- svyglm(formula_main, design = svy_design)
        model_int <- svyglm(formula_int, design = svy_design)

        # Wald test for interaction (survey package returns F-test)
        wald_test <- tryCatch({
          anova(model_main, model_int, method = "Wald")
        }, error = function(e) {
          NULL
        })

        # Extract F-test results from regTermTest object
        if (!is.null(wald_test)) {
          test_fstat <- wald_test$Ftest
          test_df <- wald_test$df
          test_ddf <- wald_test$ddf
          test_p_value <- wald_test$p
        } else {
          test_fstat <- NA_real_
          test_df <- NA_real_
          test_ddf <- NA_real_
          test_p_value <- NA_real_
        }

        # Extract interaction coefficients
        tidy_results <- broom::tidy(model_int, conf.int = TRUE, conf.level = ci_level)

        # Filter to interaction terms
        interaction_terms <- tidy_results %>%
          filter(grepl(":", term))

        # Build result tibble explicitly to avoid column name conflicts
        if (nrow(interaction_terms) > 0) {
          int_results <- tibble(
            year = yr,
            term = interaction_terms$term,
            beta_interaction = interaction_terms$estimate,
            se = interaction_terms$std.error,
            ci_low = interaction_terms$conf.low,
            ci_high = interaction_terms$conf.high,
            wald_fstat = test_fstat,
            wald_df = test_df,
            wald_ddf = test_ddf,
            wald_p_value = test_p_value,
            n = n
          )
        } else {
          # If no interaction terms found (shouldn't happen), create a placeholder
          int_results <- tibble(
            year = yr,
            term = paste0(age_var, ":", covariate),
            beta_interaction = NA_real_,
            se = NA_real_,
            ci_low = NA_real_,
            ci_high = NA_real_,
            wald_fstat = test_fstat,
            wald_df = test_df,
            wald_ddf = test_ddf,
            wald_p_value = test_p_value,
            n = n
          )
        }

        results_list[[length(results_list) + 1]] <- int_results

      }, error = function(e) {
        # Silently skip failed years
      })
    }

    if (length(results_list) == 0) {
      return(tibble())
    }

    bind_rows(results_list)
  }

  # Run across all datasets
  .run_across_datasets(
    datasets,
    .compute_interaction_single,
    covariate = covariate,
    srh_var = srh_var,
    age_var = age_var,
    year_var = year_var,
    wt_var = wt_var,
    psu_var = psu_var,
    strata_var = strata_var,
    min_n = min_n,
    ci_level = ci_level
  )
}


# ==============================================================================
# EXPORT HELPERS (optional)
# ==============================================================================

#' Save covariate analysis results
#'
#' @param results List of analysis results (stratified_betas, beta_trends, etc.)
#' @param covariate Covariate name (for filename)
#' @param output_dir Output directory
#' @param date_suffix Add date to filename?
#'
#' @export
save_covariate_results <- function(
    results,
    covariate,
    output_dir = here::here("output", "sensitivity", "tables"),
    date_suffix = TRUE
) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_name <- paste0("covariate_analysis_", tolower(covariate))
  if (date_suffix) {
    base_name <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"))
  }

  # Save as RDS (full precision)
  rds_path <- file.path(output_dir, paste0(base_name, ".rds"))
  readr::write_rds(results, rds_path)
  message("Saved: ", rds_path)

  # Optionally save individual components as CSV
  for (name in names(results)) {
    if (is.data.frame(results[[name]]) && nrow(results[[name]]) > 0) {
      csv_path <- file.path(output_dir, paste0(base_name, "_", name, ".csv"))

      # Round numeric columns
      df_rounded <- results[[name]] %>%
        mutate(across(where(is.numeric) & !matches("year|n"), ~ round(.x, 6)))

      readr::write_csv(df_rounded, csv_path)
      message("Saved: ", csv_path)
    }
  }

  invisible(NULL)
}


# ==============================================================================
# DIAGNOSTIC COMPUTATION FUNCTIONS
# ==============================================================================

#' Create simple weighted survey design
#'
#' @description
#' Creates a weights-only survey design (no PSU/strata).
#' Per specification, use this for all diagnostic functions.
#'
#' @param data Data frame
#' @param wt_var Name of weight variable
#'
#' @return survey.design object
#' @keywords internal
.create_simple_design <- function(data, wt_var = "wt") {
  svydesign(
    ids = ~1,
    weights = as.formula(paste0("~", wt_var)),
    data = data
  )
}


# ------------------------------------------------------------------------------
# 6. compute_variance_decomposition
# ------------------------------------------------------------------------------

#' Compute variance decomposition (R²) for nested models
#'
#' @description
#' For each dataset × year, fits three nested models:
#'   Model 1: SRH ~ age
#'   Model 2: SRH ~ age + covariate
#'   Model 3: SRH ~ age * covariate
#' Extracts pseudo-R² from each to show how much variance is explained by
#' age alone, adding the covariate, and adding the interaction.
#'
#' @param datasets Named list of data frames (one per survey)
#' @param covariate Character string: name of covariate variable
#' @param wt_var Name of weight variable (default "wt")
#' @param year_var Name of year variable (default "year")
#' @param age_var Name of age variable (default "age")
#' @param srh_var Name of SRH variable (default "srh")
#' @param min_n Minimum observations per year (default 500)
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return Tibble with columns:
#'   dataset, year, r2_age, r2_age_cov, r2_full, delta_r2_covariate,
#'   delta_r2_interaction, n
#'
#' @details
#' Uses pseudo-R² calculated as 1 - (residual deviance / null deviance) for
#' survey-weighted GLMs. This is analogous to R² for OLS.
#'
#' @export
compute_variance_decomposition <- function(
    datasets,
    covariate,
    wt_var = "wt",
    year_var = "year",
    age_var = "age",
    srh_var = "srh",
    min_n = 500,
    lonely_psu = "adjust"
) {

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Inner function for single dataset
  .compute_var_decomp_single <- function(data, dataset_name, ...) {

    # Validate and prepare
    .validate_covariate_data(data, covariate)
    data <- .prepare_data(data, covariate)

    # Get unique years
    years <- sort(unique(data[[year_var]]))

    message("Processing ", dataset_name, ": ", length(years),
            " years for variance decomposition")

    results_list <- list()

    for (yr in years) {

      # Subset to this year
      data_year <- data %>%
        filter(.data[[year_var]] == yr)

      n <- nrow(data_year)

      # Skip if too few observations
      if (n < min_n) next

      # Skip if insufficient variation
      if (length(unique(data_year[[srh_var]])) < 2) next
      if (length(unique(data_year[[age_var]])) < 3) next
      if (length(unique(data_year[[covariate]])) < 2) next

      tryCatch({
        # Create simple weighted design
        svy_design <- .create_simple_design(data_year, wt_var)

        # Model 1: SRH ~ age
        formula_age <- as.formula(paste0(srh_var, " ~ ", age_var))
        model_age <- svyglm(formula_age, design = svy_design)

        # Model 2: SRH ~ age + covariate
        formula_age_cov <- as.formula(paste0(srh_var, " ~ ", age_var,
                                              " + factor(", covariate, ")"))
        model_age_cov <- svyglm(formula_age_cov, design = svy_design)

        # Model 3: SRH ~ age * covariate
        formula_full <- as.formula(paste0(srh_var, " ~ ", age_var,
                                           " * factor(", covariate, ")"))
        model_full <- svyglm(formula_full, design = svy_design)

        # Calculate pseudo-R² = 1 - (residual deviance / null deviance)
        # For Gaussian GLM, deviance = sum of squared residuals
        r2_age <- 1 - (model_age$deviance / model_age$null.deviance)
        r2_age_cov <- 1 - (model_age_cov$deviance / model_age_cov$null.deviance)
        r2_full <- 1 - (model_full$deviance / model_full$null.deviance)

        # Calculate deltas
        delta_r2_covariate <- r2_age_cov - r2_age
        delta_r2_interaction <- r2_full - r2_age_cov

        results_list[[length(results_list) + 1]] <- tibble(
          year = yr,
          r2_age = r2_age,
          r2_age_cov = r2_age_cov,
          r2_full = r2_full,
          delta_r2_covariate = delta_r2_covariate,
          delta_r2_interaction = delta_r2_interaction,
          n = n
        )

      }, error = function(e) {
        # Silently skip failed years
      })
    }

    if (length(results_list) == 0) {
      return(tibble())
    }

    bind_rows(results_list)
  }

  # Run across all datasets
  .run_across_datasets(
    datasets,
    .compute_var_decomp_single,
    covariate = covariate,
    wt_var = wt_var,
    year_var = year_var,
    age_var = age_var,
    srh_var = srh_var,
    min_n = min_n
  )
}


# ------------------------------------------------------------------------------
# 7. compute_srh_variability
# ------------------------------------------------------------------------------

#' Compute weighted SD and IQR of SRH
#'
#' @description
#' For each dataset × year × covariate_level, computes weighted standard
#' deviation and interquartile range of SRH. Useful for assessing whether
#' SRH variability differs across groups or has changed over time.
#'
#' @param datasets Named list of data frames (one per survey)
#' @param covariate Character string: name of covariate variable
#' @param wt_var Name of weight variable (default "wt")
#' @param year_var Name of year variable (default "year")
#' @param srh_var Name of SRH variable (default "srh")
#' @param min_n Minimum observations per cell (default 30)
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return Tibble with columns:
#'   dataset, year, covariate_level, sd_srh, iqr_srh, n
#'
#' @export
compute_srh_variability <- function(
    datasets,
    covariate,
    wt_var = "wt",
    year_var = "year",
    srh_var = "srh",
    min_n = 30,
    lonely_psu = "adjust"
) {

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Inner function for single dataset
  .compute_var_single <- function(data, dataset_name, ...) {

    # Validate and prepare
    .validate_covariate_data(data, covariate)
    data <- .prepare_data(data, covariate)

    # Get unique years and covariate levels
    years <- sort(unique(data[[year_var]]))
    covariate_levels <- sort(unique(data[[covariate]]))

    message("Processing ", dataset_name, ": ", length(years), " years × ",
            length(covariate_levels), " levels for SRH variability")

    results_list <- list()

    for (yr in years) {
      for (lev in covariate_levels) {

        # Subset to this year and covariate level
        data_subset <- data %>%
          filter(.data[[year_var]] == yr, .data[[covariate]] == lev)

        n <- nrow(data_subset)

        # Skip if too few observations
        if (n < min_n) next

        # Skip if no variation in SRH
        if (length(unique(data_subset[[srh_var]])) < 2) next

        tryCatch({
          # Create simple weighted design
          svy_design <- .create_simple_design(data_subset, wt_var)

          # Compute weighted variance/SD
          var_result <- svyvar(as.formula(paste0("~", srh_var)), design = svy_design)
          sd_srh <- sqrt(as.numeric(var_result))

          # Compute weighted IQR
          quantile_result <- svyquantile(
            as.formula(paste0("~", srh_var)),
            design = svy_design,
            quantiles = c(0.25, 0.75),
            ci = FALSE
          )
          # Extract quantiles - svyquantile returns a list
          q25 <- coef(quantile_result)[[1]][1]
          q75 <- coef(quantile_result)[[1]][2]
          iqr_srh <- q75 - q25

          results_list[[length(results_list) + 1]] <- tibble(
            year = yr,
            covariate_level = as.character(lev),
            sd_srh = sd_srh,
            iqr_srh = iqr_srh,
            n = n
          )

        }, error = function(e) {
          # Silently skip failed cells
        })
      }
    }

    if (length(results_list) == 0) {
      return(tibble())
    }

    bind_rows(results_list)
  }

  # Run across all datasets
  .run_across_datasets(
    datasets,
    .compute_var_single,
    covariate = covariate,
    wt_var = wt_var,
    year_var = year_var,
    srh_var = srh_var,
    min_n = min_n
  )
}


# ------------------------------------------------------------------------------
# 8. compute_srh_distribution
# ------------------------------------------------------------------------------

#' Compute weighted SRH category distribution
#'
#' @description
#' For each dataset × year × covariate_level, computes the weighted proportion
#' of respondents in each SRH category. Handles both 5-point (most surveys)
#' and 4-point (GSS) scales automatically.
#'
#' @param datasets Named list of data frames (one per survey)
#' @param covariate Character string: name of covariate variable
#' @param wt_var Name of weight variable (default "wt")
#' @param year_var Name of year variable (default "year")
#' @param srh_var Name of SRH variable (default "srh")
#' @param srh_labels_5pt Labels for 5-point scale (default: Poor to Excellent)
#' @param srh_labels_4pt Labels for 4-point scale (default: Poor to Excellent, no Very Good)
#' @param min_n Minimum observations per cell (default 30)
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return Tibble with columns:
#'   dataset, year, covariate_level, srh_category, srh_numeric, proportion, proportion_se
#'
#' @export
compute_srh_distribution <- function(
    datasets,
    covariate,
    wt_var = "wt",
    year_var = "year",
    srh_var = "srh",
    srh_labels_5pt = c("Poor", "Fair", "Good", "Very Good", "Excellent"),
    srh_labels_4pt = c("Poor", "Fair", "Good", "Excellent"),
    min_n = 30,
    lonely_psu = "adjust"
) {

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Inner function for single dataset
  .compute_dist_single <- function(data, dataset_name, ...) {

    # Validate and prepare
    .validate_covariate_data(data, covariate)
    data <- .prepare_data(data, covariate)

    # Detect scale: GSS uses 4-point (max SRH = 4), others use 5-point
    max_srh <- max(data[[srh_var]], na.rm = TRUE)
    is_4pt <- max_srh <= 4
    srh_labels <- if (is_4pt) srh_labels_4pt else srh_labels_5pt
    srh_values <- if (is_4pt) 1:4 else 1:5

    # Get unique years and covariate levels
    years <- sort(unique(data[[year_var]]))
    covariate_levels <- sort(unique(data[[covariate]]))

    message("Processing ", dataset_name, ": ", length(years), " years × ",
            length(covariate_levels), " levels for SRH distribution (",
            if (is_4pt) "4-point" else "5-point", " scale)")

    results_list <- list()

    for (yr in years) {
      for (lev in covariate_levels) {

        # Subset to this year and covariate level
        data_subset <- data %>%
          filter(.data[[year_var]] == yr, .data[[covariate]] == lev)

        n <- nrow(data_subset)

        # Skip if too few observations
        if (n < min_n) next

        tryCatch({
          # Convert SRH to factor with all possible levels
          data_subset <- data_subset %>%
            mutate(srh_factor = factor(.data[[srh_var]], levels = srh_values))

          # Create simple weighted design
          svy_design <- .create_simple_design(data_subset, wt_var)

          # Compute weighted proportions
          prop_result <- svymean(~srh_factor, design = svy_design, na.rm = TRUE)

          # Extract proportions and SEs
          props <- as.numeric(coef(prop_result))
          ses <- as.numeric(SE(prop_result))

          # Build result for each SRH category
          for (i in seq_along(srh_values)) {
            results_list[[length(results_list) + 1]] <- tibble(
              year = yr,
              covariate_level = as.character(lev),
              srh_category = srh_labels[i],
              srh_numeric = srh_values[i],
              proportion = props[i],
              proportion_se = ses[i]
            )
          }

        }, error = function(e) {
          # Silently skip failed cells
        })
      }
    }

    if (length(results_list) == 0) {
      return(tibble())
    }

    bind_rows(results_list)
  }

  # Run across all datasets
  .run_across_datasets(
    datasets,
    .compute_dist_single,
    covariate = covariate,
    wt_var = wt_var,
    year_var = year_var,
    srh_var = srh_var,
    srh_labels_5pt = srh_labels_5pt,
    srh_labels_4pt = srh_labels_4pt,
    min_n = min_n
  )
}


# ------------------------------------------------------------------------------
# 9. compute_sample_diagnostics
# ------------------------------------------------------------------------------

#' Compute sample diagnostics: sizes, composition, missingness
#'
#' @description
#' Computes sample sizes, weighted composition, and missingness rates.
#' Returns two tibbles:
#'   - sample_sizes: n and composition by dataset × year × covariate_level
#'   - missingness: missing rates by dataset × year (before dropping NAs)
#'
#' @param datasets Named list of data frames (one per survey)
#' @param covariate Character string: name of covariate variable
#' @param wt_var Name of weight variable (default "wt")
#' @param year_var Name of year variable (default "year")
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_var Name of age variable (default "age")
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return List with two tibbles:
#'   - sample_sizes: dataset, year, covariate_level, n_unweighted, n_weighted,
#'                   pct_of_total, mean_age, mean_srh
#'   - missingness: dataset, year, pct_missing_covariate, pct_missing_srh,
#'                  pct_missing_age, pct_complete
#'
#' @export
compute_sample_diagnostics <- function(
    datasets,
    covariate,
    wt_var = "wt",
    year_var = "year",
    srh_var = "srh",
    age_var = "age",
    lonely_psu = "adjust"
) {

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  sample_sizes_all <- list()
  missingness_all <- list()

  for (i in seq_along(datasets)) {
    dataset_name <- names(datasets)[i]
    data <- datasets[[i]]

    message("Processing ", dataset_name, " for sample diagnostics")

    tryCatch({

      # Check required variables exist
      required_vars <- c(srh_var, age_var, year_var, wt_var, covariate)
      missing_vars <- setdiff(required_vars, names(data))
      if (length(missing_vars) > 0) {
        warning("Skipping ", dataset_name, ": missing variables ",
                paste(missing_vars, collapse = ", "))
        next
      }

      # ---------------------------------------------------------------------
      # 1. Missingness (before dropping NAs)
      # ---------------------------------------------------------------------
      years <- sort(unique(data[[year_var]]))

      for (yr in years) {
        data_year <- data %>%
          filter(.data[[year_var]] == yr, .data[[age_var]] <= 89)

        n_total <- nrow(data_year)
        if (n_total == 0) next

        pct_missing_cov <- 100 * sum(is.na(data_year[[covariate]])) / n_total
        pct_missing_srh <- 100 * sum(is.na(data_year[[srh_var]])) / n_total
        pct_missing_age <- 100 * sum(is.na(data_year[[age_var]])) / n_total
        pct_missing_wt <- 100 * sum(is.na(data_year[[wt_var]]) |
                                       data_year[[wt_var]] <= 0) / n_total

        # Complete cases: non-missing on all key vars and wt > 0
        complete <- !is.na(data_year[[covariate]]) &
          !is.na(data_year[[srh_var]]) &
          !is.na(data_year[[age_var]]) &
          !is.na(data_year[[wt_var]]) &
          data_year[[wt_var]] > 0
        pct_complete <- 100 * sum(complete) / n_total

        missingness_all[[length(missingness_all) + 1]] <- tibble(
          dataset = dataset_name,
          year = yr,
          n_total = n_total,
          pct_missing_covariate = pct_missing_cov,
          pct_missing_srh = pct_missing_srh,
          pct_missing_age = pct_missing_age,
          pct_missing_wt = pct_missing_wt,
          pct_complete = pct_complete
        )
      }

      # ---------------------------------------------------------------------
      # 2. Sample sizes (after dropping NAs, by covariate level)
      # ---------------------------------------------------------------------
      data_clean <- .prepare_data(data, covariate)
      years <- sort(unique(data_clean[[year_var]]))
      covariate_levels <- sort(unique(data_clean[[covariate]]))

      for (yr in years) {
        data_year <- data_clean %>%
          filter(.data[[year_var]] == yr)

        n_year_total <- nrow(data_year)
        wt_year_total <- sum(data_year[[wt_var]])

        if (n_year_total == 0) next

        # Create survey design for this year
        svy_year <- .create_simple_design(data_year, wt_var)

        for (lev in covariate_levels) {
          data_subset <- data_year %>%
            filter(.data[[covariate]] == lev)

          n_unweighted <- nrow(data_subset)
          if (n_unweighted == 0) next

          n_weighted <- sum(data_subset[[wt_var]])
          pct_of_total <- 100 * n_weighted / wt_year_total

          # Compute weighted mean age and SRH for this subset
          svy_subset <- .create_simple_design(data_subset, wt_var)
          mean_age <- as.numeric(coef(svymean(as.formula(paste0("~", age_var)),
                                               design = svy_subset)))
          mean_srh <- as.numeric(coef(svymean(as.formula(paste0("~", srh_var)),
                                               design = svy_subset)))

          sample_sizes_all[[length(sample_sizes_all) + 1]] <- tibble(
            dataset = dataset_name,
            year = yr,
            covariate_level = as.character(lev),
            n_unweighted = n_unweighted,
            n_weighted = n_weighted,
            pct_of_total = pct_of_total,
            mean_age = mean_age,
            mean_srh = mean_srh
          )
        }
      }

    }, error = function(e) {
      warning("Failed for ", dataset_name, ": ", e$message)
    })
  }

  # Combine results
  sample_sizes <- if (length(sample_sizes_all) > 0) {
    bind_rows(sample_sizes_all)
  } else {
    tibble()
  }

  missingness <- if (length(missingness_all) > 0) {
    bind_rows(missingness_all)
  } else {
    tibble()
  }

  list(
    sample_sizes = sample_sizes,
    missingness = missingness
  )
}


# ------------------------------------------------------------------------------
# 10. compute_age_distribution_by_stratum
# ------------------------------------------------------------------------------

#' Compute mean SRH by age group, covariate, and year
#'
#' @description
#' For each dataset × year × covariate_level × age_group, computes weighted
#' mean SRH with standard errors and confidence intervals. This is the
#' primary data for Page 1 figures showing SRH trends by age.
#'
#' @param datasets Named list of data frames (one per survey)
#' @param covariate Character string: name of covariate variable
#' @param wt_var Name of weight variable (default "wt")
#' @param year_var Name of year variable (default "year")
#' @param age_var Name of age variable (default "age")
#' @param srh_var Name of SRH variable (default "srh")
#' @param age_breaks Breakpoints for age groups (default c(18, 30, 40, 50, 60, 70, 80, 90))
#' @param age_labels Labels for age groups (default c("18-29", "30-39", ...))
#' @param min_n Minimum observations per cell (default 30)
#' @param ci_level Confidence level (default 0.95)
#' @param lonely_psu How to handle lonely PSUs (default "adjust")
#'
#' @return Tibble with columns:
#'   dataset, year, covariate_level, age_group, mean_srh, se_srh, ci_low, ci_high, n
#'
#' @export
compute_age_distribution_by_stratum <- function(
    datasets,
    covariate,
    wt_var = "wt",
    year_var = "year",
    age_var = "age",
    srh_var = "srh",
    age_breaks = c(18, 30, 40, 50, 60, 70, 80, 90),
    age_labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
    min_n = 30,
    ci_level = 0.95,
    lonely_psu = "adjust"
) {

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # CI multiplier
  z <- qnorm(1 - (1 - ci_level) / 2)

  # Inner function for single dataset
  .compute_age_dist_single <- function(data, dataset_name, ...) {

    # Validate and prepare
    .validate_covariate_data(data, covariate)
    data <- .prepare_data(data, covariate)

    # Create age groups
    data <- data %>%
      mutate(age_group = cut(
        .data[[age_var]],
        breaks = age_breaks,
        labels = age_labels,
        right = FALSE,
        include.lowest = TRUE
      )) %>%
      filter(!is.na(age_group))

    # Get unique years and covariate levels
    years <- sort(unique(data[[year_var]]))
    covariate_levels <- sort(unique(data[[covariate]]))

    message("Processing ", dataset_name, ": ", length(years), " years × ",
            length(covariate_levels), " levels × ", length(age_labels),
            " age groups")

    results_list <- list()

    for (yr in years) {
      for (lev in covariate_levels) {
        for (ag in age_labels) {

          # Subset to this year, covariate level, and age group
          data_subset <- data %>%
            filter(
              .data[[year_var]] == yr,
              .data[[covariate]] == lev,
              age_group == ag
            )

          n <- nrow(data_subset)

          # Skip if too few observations
          if (n < min_n) next

          # Skip if no variation in SRH
          if (length(unique(data_subset[[srh_var]])) < 2) {
            # Still compute mean, but SE will be 0
            tryCatch({
              svy_design <- .create_simple_design(data_subset, wt_var)
              mean_result <- svymean(as.formula(paste0("~", srh_var)),
                                      design = svy_design)
              mean_srh <- as.numeric(coef(mean_result))
              se_srh <- 0

              results_list[[length(results_list) + 1]] <- tibble(
                year = yr,
                covariate_level = as.character(lev),
                age_group = ag,
                mean_srh = mean_srh,
                se_srh = se_srh,
                ci_low = mean_srh,
                ci_high = mean_srh,
                n = n
              )
            }, error = function(e) {
              # Skip
            })
            next
          }

          tryCatch({
            # Create simple weighted design
            svy_design <- .create_simple_design(data_subset, wt_var)

            # Compute weighted mean and SE
            mean_result <- svymean(as.formula(paste0("~", srh_var)),
                                    design = svy_design)
            mean_srh <- as.numeric(coef(mean_result))
            se_srh <- as.numeric(SE(mean_result))

            # Confidence interval
            ci_low <- mean_srh - z * se_srh
            ci_high <- mean_srh + z * se_srh

            results_list[[length(results_list) + 1]] <- tibble(
              year = yr,
              covariate_level = as.character(lev),
              age_group = ag,
              mean_srh = mean_srh,
              se_srh = se_srh,
              ci_low = ci_low,
              ci_high = ci_high,
              n = n
            )

          }, error = function(e) {
            # Silently skip failed cells
          })
        }
      }
    }

    if (length(results_list) == 0) {
      return(tibble())
    }

    bind_rows(results_list)
  }

  # Run across all datasets
  .run_across_datasets(
    datasets,
    .compute_age_dist_single,
    covariate = covariate,
    wt_var = wt_var,
    year_var = year_var,
    age_var = age_var,
    srh_var = srh_var,
    age_breaks = age_breaks,
    age_labels = age_labels,
    min_n = min_n,
    ci_level = ci_level
  )
}
