# ==============================================================================
# 03_apc_analysis.R
# Age-Period-Cohort (APC) Analysis Pipeline
# Author: Christine Lucille Kuryla
#
# Purpose: Run APC analyses across 6 US surveys to test whether observed SRH
# convergence reflects period effects vs cohort effects.
#
# Models per survey:
#   1. Age + Period (AP) - weighted where possible
#   2. Age + Cohort (AC) - weighted where possible
#   3. Age x Period interaction - weighted where possible
#   4. HAPC with crossed random effects - unweighted (subsampled for large surveys)
#
# Outputs per survey:
#   - output/apc/{survey}_results_continuous.rds
#   - output/apc/{survey}_results_binary.rds
# ==============================================================================

library(tidyverse)
library(here)
library(survey)
library(lme4)
library(broom)
library(broom.mixed)

source(here::here("R", "paths.R"))
source(here::here("R", "srh_common_functions.R"))

# ==============================================================================
# Configuration
# ==============================================================================

# Survey processing order (smallest to largest for testing)
SURVEY_ORDER <- c("gss", "nhanes", "meps", "nhis", "cps", "brfss")

# Survey metadata
SURVEY_META <- list(
  gss = list(
    srh_scale = 4,
    has_design = FALSE,
    hapc_fraction = 1.0,
    hapc_seed = NA
  ),
  nhanes = list(
    srh_scale = 5,
    has_design = TRUE,
    hapc_fraction = 1.0,
    hapc_seed = NA
  ),
  meps = list(
    srh_scale = 5,
    has_design = TRUE,
    hapc_fraction = 1.0,
    hapc_seed = NA
  ),
  nhis = list(
    srh_scale = 5,
    has_design = TRUE,
    hapc_fraction = 0.10,
    hapc_seed = 12347
  ),
  cps = list(
    srh_scale = 5,
    has_design = FALSE,
    hapc_fraction = 0.10,
    hapc_seed = 12346
  ),
  brfss = list(
    srh_scale = 5,
    has_design = TRUE,
    hapc_fraction = 0.05,
    hapc_seed = 12345
  )
)

# Survey display order for tables/figures
SURVEY_DISPLAY_ORDER <- c("brfss", "nhis", "meps", "nhanes", "gss", "cps")

# APC colors (from plan, consistent with srh_common_functions.R)
APC_COLORS <- c(
  "Age" = "#0072B2",      # blue
  "Period" = "#009E73",   # bluish green
  "Cohort" = "#CC79A7"    # reddish purple
)

# ==============================================================================
# Data Loading and Preparation Functions
# ==============================================================================

#' Load survey data from derived path
#'
#' @param survey_name Character string: one of gss, nhanes, meps, nhis, cps, brfss
#' @return Data frame with survey data
load_survey_data <- function(survey_name) {
  survey_name <- tolower(survey_name)
  stopifnot(survey_name %in% names(SURVEY_META))

  file_path <- derived_path(paste0("data_", survey_name, ".rds"))

  if (!file.exists(file_path)) {
    stop("Data file not found: ", file_path)
  }

  message("Loading ", toupper(survey_name), " data from: ", file_path)
  df <- readRDS(file_path)

  # Verify required columns exist
  required_cols <- c("year", "age", "srh")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Add dataset identifier if not present
  if (!"dataset" %in% names(df)) {
    df$dataset <- toupper(survey_name)
  }

  message("  Loaded ", format(nrow(df), big.mark = ","), " rows")

  df
}


#' Structure data for APC analysis
#'
#' Creates age groups, cohorts, period, and fair/poor indicator.
#' Filters to ages 18-88.
#'
#' @param df Data frame with year, age, srh columns
#' @param srh_scale Integer: maximum SRH value (4 for GSS, 5 for others)
#' @return Data frame with APC variables added
structure_apc_data <- function(df, srh_scale = 5) {
  stopifnot(srh_scale %in% c(4, 5))

  message("  Structuring data for APC analysis...")


  df_apc <- df %>%
    # Filter to valid ages
    filter(!is.na(age), age >= 18, age <= 88) %>%
    # Filter to valid SRH
    filter(!is.na(srh), srh >= 1, srh <= srh_scale) %>%
    mutate(
      # Period is just year
      period = year,

      # Create 5-year age groups
      age_group = cut(
        age,
        breaks = seq(15, 90, by = 5),
        labels = c("15-19", "20-24", "25-29", "30-34", "35-39",
                   "40-44", "45-49", "50-54", "55-59", "60-64",
                   "65-69", "70-74", "75-79", "80-84", "85-89"),
        right = FALSE
      ),
      # Age group midpoint for cohort calculation
      age_midpoint = case_when(
        age >= 18 & age < 20 ~ 17.5,  # Partial group
        age >= 20 & age < 25 ~ 22.5,
        age >= 25 & age < 30 ~ 27.5,
        age >= 30 & age < 35 ~ 32.5,
        age >= 35 & age < 40 ~ 37.5,
        age >= 40 & age < 45 ~ 42.5,
        age >= 45 & age < 50 ~ 47.5,
        age >= 50 & age < 55 ~ 52.5,
        age >= 55 & age < 60 ~ 57.5,
        age >= 60 & age < 65 ~ 62.5,
        age >= 65 & age < 70 ~ 67.5,
        age >= 70 & age < 75 ~ 72.5,
        age >= 75 & age < 80 ~ 77.5,
        age >= 80 & age < 85 ~ 82.5,
        age >= 85 & age <= 88 ~ 86.5,
        TRUE ~ NA_real_
      ),

      # Birth cohort = year - age (using midpoint for groups)
      cohort = year - age_midpoint,

      # Create 5-year cohort groups
      cohort_group = cut(
        cohort,
        breaks = seq(1880, 2010, by = 5),
        labels = paste0(seq(1880, 2005, by = 5), "-", seq(1884, 2009, by = 5)),
        right = FALSE
      ),

      # Binary fair/poor outcome
      # Per INVARIANTS.md: srh_fairpoor = (srh_num <= 2) for all datasets
      srh_fairpoor = as.integer(srh <= 2)
    ) %>%
    # Remove rows with missing APC variables
    filter(!is.na(age_group), !is.na(cohort_group))

  # Verify SRH coding
  srh_range <- range(df_apc$srh, na.rm = TRUE)
  if (srh_range[1] < 1 || srh_range[2] > srh_scale) {
    warning("SRH values outside expected range [1, ", srh_scale, "]: [",
            srh_range[1], ", ", srh_range[2], "]")
  }

  message("  After filtering: ", format(nrow(df_apc), big.mark = ","), " rows")
  message("  Years: ", min(df_apc$year), "-", max(df_apc$year))
  message("  Age groups: ", length(unique(df_apc$age_group)))
  message("  Cohort groups: ", length(unique(df_apc$cohort_group)))
  message("  Fair/Poor rate: ", round(mean(df_apc$srh_fairpoor) * 100, 1), "%")

  df_apc
}


#' Create subsample for HAPC analysis
#'
#' Stratified sampling by age_group x year to maintain cell coverage
#'
#' @param df Data frame with age_group and year columns
#' @param fraction Fraction to sample (0 < fraction <= 1)
#' @param seed Random seed for reproducibility
#' @return Subsampled data frame
create_hapc_subsample <- function(df, fraction = 1.0, seed = NULL) {
  if (fraction >= 1.0) {
    return(df)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  message("  Creating ", fraction * 100, "% stratified subsample for HAPC...")

  df_sample <- df %>%
    group_by(age_group, year) %>%
    slice_sample(prop = fraction) %>%
    ungroup()

  message("  Subsample size: ", format(nrow(df_sample), big.mark = ","), " rows")

  df_sample
}


# ==============================================================================
# Survey Design Functions
# ==============================================================================

#' Create survey design object
#'
#' Uses full design (strata, PSU, weights) when available, otherwise weights only
#'
#' @param df Data frame with survey design columns
#' @param has_design Logical: does survey have strata/PSU?
#' @return svydesign object
create_survey_design <- function(df, has_design = TRUE) {
  # Check for weight column
  wt_col <- intersect(c("wt", "weight", "wtfa", "finalwt"), names(df))

  if (length(wt_col) == 0) {
    message("  No weight column found, using unweighted design")
    return(svydesign(ids = ~1, data = df))
  }

  wt_col <- wt_col[1]

  # Rename weight column to wt for consistency
  if (wt_col != "wt") {
    df$wt <- df[[wt_col]]
  }

  # Check for design columns
  has_strata <- "strata" %in% names(df) && !all(is.na(df$strata))
  has_psu <- "psu" %in% names(df) && !all(is.na(df$psu))

  if (has_design && has_strata && has_psu) {
    message("  Using full survey design (strata + PSU + weights)")

    # Handle single-PSU strata
    options(survey.lonely.psu = "adjust")

    design <- svydesign(
      ids = ~psu,
      strata = ~strata,
      weights = ~wt,
      data = df,
      nest = TRUE
    )
  } else {
    message("  Using weights-only design")
    design <- svydesign(
      ids = ~1,
      weights = ~wt,
      data = df
    )
  }

  design
}


# ==============================================================================
# Model Fitting Functions
# ==============================================================================

#' Fit Age + Period model (weighted)
#'
#' @param design Survey design object
#' @param outcome Character: "continuous" or "binary"
#' @return List with model object and summary
fit_ap_model <- function(design, outcome = "continuous") {
  message("  Fitting Age + Period model...")

  tryCatch({
    if (outcome == "continuous") {
      model <- svyglm(srh ~ age_group + factor(period), design = design)
    } else {
      model <- svyglm(
        srh_fairpoor ~ age_group + factor(period),
        design = design,
        family = quasibinomial()
      )
    }

    summary_df <- broom::tidy(model, conf.int = TRUE)

    list(
      model = model,
      summary = summary_df,
      converged = TRUE,
      error = NULL
    )
  }, error = function(e) {
    warning("AP model failed: ", e$message)
    list(model = NULL, summary = NULL, converged = FALSE, error = e$message)
  })
}


#' Fit Age + Cohort model (weighted)
#'
#' @param design Survey design object
#' @param outcome Character: "continuous" or "binary"
#' @return List with model object and summary
fit_ac_model <- function(design, outcome = "continuous") {
  message("  Fitting Age + Cohort model...")

  tryCatch({
    if (outcome == "continuous") {
      model <- svyglm(srh ~ age_group + cohort_group, design = design)
    } else {
      model <- svyglm(
        srh_fairpoor ~ age_group + cohort_group,
        design = design,
        family = quasibinomial()
      )
    }

    summary_df <- broom::tidy(model, conf.int = TRUE)

    list(
      model = model,
      summary = summary_df,
      converged = TRUE,
      error = NULL
    )
  }, error = function(e) {
    warning("AC model failed: ", e$message)
    list(model = NULL, summary = NULL, converged = FALSE, error = e$message)
  })
}


#' Fit Age x Period interaction model (weighted)
#'
#' Tests whether the age effect changes over time (key for convergence)
#'
#' @param design Survey design object
#' @param outcome Character: "continuous" or "binary"
#' @return List with model object, summary, and interaction test
fit_interaction_model <- function(design, outcome = "continuous") {
  message("  Fitting Age x Period interaction model...")

  tryCatch({
    # First fit main effects model
    if (outcome == "continuous") {
      model_main <- svyglm(srh ~ age_group + factor(period), design = design)
      model_int <- svyglm(srh ~ age_group * factor(period), design = design)
    } else {
      model_main <- svyglm(
        srh_fairpoor ~ age_group + factor(period),
        design = design,
        family = quasibinomial()
      )
      model_int <- svyglm(
        srh_fairpoor ~ age_group * factor(period),
        design = design,
        family = quasibinomial()
      )
    }

    # Test interaction significance (Wald test)
    # Get interaction terms
    int_terms <- grep("age_group.*period|period.*age_group",
                      names(coef(model_int)), value = TRUE)

    if (length(int_terms) > 0) {
      # Use regTermTest for joint test of interaction terms
      int_test <- regTermTest(model_int, ~ age_group:factor(period))
      int_fstat <- int_test$Ftest
      int_pvalue <- int_test$p
      int_df <- c(int_test$df, int_test$ddf)
    } else {
      int_fstat <- NA
      int_pvalue <- NA
      int_df <- c(NA, NA)
    }

    summary_df <- broom::tidy(model_int, conf.int = TRUE)

    list(
      model = model_int,
      model_main = model_main,
      summary = summary_df,
      interaction_test = list(
        F_statistic = int_fstat,
        p_value = int_pvalue,
        df = int_df
      ),
      converged = TRUE,
      error = NULL
    )
  }, error = function(e) {
    warning("Interaction model failed: ", e$message)
    list(
      model = NULL,
      model_main = NULL,
      summary = NULL,
      interaction_test = NULL,
      converged = FALSE,
      error = e$message
    )
  })
}


#' Fit HAPC (Hierarchical Age-Period-Cohort) model
#'
#' Uses crossed random effects for period and cohort (unweighted)
#'
#' @param df Data frame (not design object)
#' @param outcome Character: "continuous" or "binary"
#' @return List with model object, summary, and variance components
fit_hapc_model <- function(df, outcome = "continuous") {
  message("  Fitting HAPC model (crossed random effects)...")

  tryCatch({
    if (outcome == "continuous") {
      model <- lmer(
        srh ~ age_group + (1 | period) + (1 | cohort_group),
        data = df,
        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
      )
    } else {
      model <- glmer(
        srh_fairpoor ~ age_group + (1 | period) + (1 | cohort_group),
        data = df,
        family = binomial(),
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
      )
    }

    # Extract variance components
    vc <- as.data.frame(VarCorr(model))

    period_var <- vc$vcov[vc$grp == "period"]
    cohort_var <- vc$vcov[vc$grp == "cohort_group"]

    # For continuous: also get residual variance
    if (outcome == "continuous") {
      resid_var <- vc$vcov[vc$grp == "Residual"]
      total_var <- period_var + cohort_var + resid_var
    } else {
      # For binary: use pi^2/3 for residual variance (logistic)
      resid_var <- pi^2 / 3
      total_var <- period_var + cohort_var + resid_var
    }

    variance_decomp <- tibble(
      component = c("Period", "Cohort", "Residual", "Total"),
      variance = c(period_var, cohort_var, resid_var, total_var),
      pct_of_total = c(period_var, cohort_var, resid_var, total_var) / total_var * 100
    )

    # Get fixed effects summary
    summary_df <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)

    list(
      model = model,
      summary = summary_df,
      variance_components = variance_decomp,
      converged = TRUE,
      error = NULL
    )
  }, error = function(e) {
    warning("HAPC model failed: ", e$message)
    list(
      model = NULL,
      summary = NULL,
      variance_components = NULL,
      converged = FALSE,
      error = e$message
    )
  })
}


# ==============================================================================
# Main Pipeline Functions
# ==============================================================================

#' Fit all models for a single survey
#'
#' @param df Structured APC data frame
#' @param survey_name Character: survey name
#' @param meta Survey metadata list
#' @return List with results for continuous and binary outcomes
fit_models_for_survey <- function(df, survey_name, meta) {
  message("\n", strrep("=", 60))
  message("Fitting models for ", toupper(survey_name))
  message(strrep("=", 60))

  results <- list(
    survey = survey_name,
    n_obs = nrow(df),
    year_range = range(df$year),
    srh_scale = meta$srh_scale,
    continuous = list(),
    binary = list()
  )

  # Create survey design for weighted models (full design for AP/AC)
  design <- create_survey_design(df, has_design = meta$has_design)

  # Create weights-only design for interaction model (much faster)
  # Full design with interaction terms is computationally prohibitive
  design_wt_only <- create_survey_design(df, has_design = FALSE)

  # Create HAPC subsample
  df_hapc <- create_hapc_subsample(
    df,
    fraction = meta$hapc_fraction,
    seed = meta$hapc_seed
  )

  results$hapc_n <- nrow(df_hapc)

  # Fit models for continuous outcome
  message("\n--- Continuous SRH outcome ---")
  results$continuous$ap <- fit_ap_model(design, "continuous")
  results$continuous$ac <- fit_ac_model(design, "continuous")
  results$continuous$interaction <- fit_interaction_model(design_wt_only, "continuous")
  results$continuous$hapc <- fit_hapc_model(df_hapc, "continuous")

  # Fit models for binary outcome
  message("\n--- Binary (Fair/Poor) outcome ---")
  results$binary$ap <- fit_ap_model(design, "binary")
  results$binary$ac <- fit_ac_model(design, "binary")
  results$binary$interaction <- fit_interaction_model(design_wt_only, "binary")
  results$binary$hapc <- fit_hapc_model(df_hapc, "binary")

  results
}


#' Run full APC pipeline for one survey
#'
#' @param survey_name Character: survey name
#' @return List with all results
run_apc_pipeline <- function(survey_name) {
  survey_name <- tolower(survey_name)
  meta <- SURVEY_META[[survey_name]]

  if (is.null(meta)) {
    stop("Unknown survey: ", survey_name)
  }

  # Load data
  df <- load_survey_data(survey_name)

  # Structure for APC

  df_apc <- structure_apc_data(df, srh_scale = meta$srh_scale)

  # Fit all models
  results <- fit_models_for_survey(df_apc, survey_name, meta)

  # Add timestamp
  results$timestamp <- Sys.time()

  results
}


#' Save results to disk
#'
#' @param results Results list from run_apc_pipeline
#' @param survey_name Survey name
save_apc_results <- function(results, survey_name) {
  output_dir <- here::here("output", "apc")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Save continuous results
  continuous_path <- file.path(output_dir, paste0(survey_name, "_results_continuous.rds"))
  saveRDS(results$continuous, continuous_path)
  message("Saved: ", continuous_path)

  # Save binary results
  binary_path <- file.path(output_dir, paste0(survey_name, "_results_binary.rds"))
  saveRDS(results$binary, binary_path)
  message("Saved: ", binary_path)

  # Save full results object
  full_path <- file.path(output_dir, paste0(survey_name, "_results_full.rds"))
  saveRDS(results, full_path)
  message("Saved: ", full_path)
}


# ==============================================================================
# Run Pipeline
# ==============================================================================

run_all_surveys <- function(surveys = SURVEY_ORDER) {
  message("\n", strrep("=", 70))
  message("APC ANALYSIS PIPELINE")
  message("Starting at: ", Sys.time())
  message(strrep("=", 70))

  all_results <- list()

  for (survey in surveys) {
    message("\n\n", strrep("#", 70))
    message("# Processing: ", toupper(survey))
    message(strrep("#", 70))

    start_time <- Sys.time()

    tryCatch({
      results <- run_apc_pipeline(survey)
      save_apc_results(results, survey)
      all_results[[survey]] <- results

      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      message("\n", toupper(survey), " completed in ", round(elapsed, 1), " minutes")

    }, error = function(e) {
      message("\nERROR processing ", toupper(survey), ": ", e$message)
      all_results[[survey]] <- list(error = e$message)
    })
  }

  message("\n\n", strrep("=", 70))
  message("PIPELINE COMPLETE")
  message("Finished at: ", Sys.time())
  message(strrep("=", 70))

  invisible(all_results)
}

# Uncomment to run:
# results <- run_all_surveys()

# Or run individual surveys for testing:
# results_gss <- run_apc_pipeline("gss")
# save_apc_results(results_gss, "gss")
