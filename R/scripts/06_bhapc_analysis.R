# ==============================================================================
# 06_bhapc_analysis.R
# Bayesian Hierarchical APC (BHAPC) Analysis Pipeline
# Based on Gloria Graf's methodology
# Author: Christine Lucille Kuryla
#
# Purpose: Run BHAPC analyses to decompose SRH variation into Age, Period,
# and Cohort components using Bayesian estimation.
#
# Outputs per survey:
#   - output/apc/bhapc/{survey}_bhapc_model.rds    (saved model object)
#   - output/apc/bhapc/{survey}_bhapc_summary.csv  (model summary)
#   - output/apc/bhapc/table2_{survey}.csv         (Table 2 analog)
#   - output/apc/bhapc/figure2_{survey}.png        (Descriptive means)
#   - output/apc/bhapc/figure3_{survey}.png        (APC effects)
#
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(rstanarm)
library(broom.mixed)
library(survey)
library(srvyr)
library(patchwork)

# Source paths and common functions
source(here::here("R", "paths.R"))
source(here::here("R", "srh_common_functions.R"))

# Source BHAPC functions
source(here::here("R", "functions", "bhapc_data_prep.R"))
source(here::here("R", "functions", "bhapc_model_fitting.R"))
source(here::here("R", "functions", "bhapc_table_generation.R"))
source(here::here("R", "functions", "bhapc_figure_generation.R"))

# Create output directory
bhapc_output_dir <- here::here("output", "apc", "bhapc")
dir.create(bhapc_output_dir, recursive = TRUE, showWarnings = FALSE)

# Set seed for reproducibility
set.seed(20260128)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Survey metadata for BHAPC analysis
BHAPC_META <- list(
  nhanes = list(
    srh_scale = 5,
    age_min = 18,
    age_max = 80,  # NHANES top-codes at 80
    iter = 6000,
    adapt_delta = 0.998
  ),
  gss = list(
    srh_scale = 4,
    age_min = 18,
    age_max = 89,
    iter = 6000,
    adapt_delta = 0.998
  ),
  meps = list(
    srh_scale = 5,
    age_min = 18,
    age_max = 85,
    iter = 6000,
    adapt_delta = 0.998
  ),
  nhis = list(
    srh_scale = 5,
    age_min = 18,
    age_max = 85,
    iter = 6000,
    adapt_delta = 0.998
  ),
  brfss = list(
    srh_scale = 5,
    age_min = 18,
    age_max = 89,  # Uses age groups, with 80+ as top
    iter = 6000,
    adapt_delta = 0.998
  ),
  cps = list(
    srh_scale = 5,
    age_min = 18,
    age_max = 85,
    iter = 6000,
    adapt_delta = 0.998
  )
)

# Processing order (start with smallest for testing)
SURVEY_ORDER <- c("nhanes", "gss", "meps", "nhis", "cps", "brfss")

# ------------------------------------------------------------------------------
# Main Pipeline Functions
# ------------------------------------------------------------------------------

#' Run BHAPC analysis for a single survey
#'
#' @param survey_name Character: survey name
#' @param run_model Logical: whether to fit the model (FALSE to just prep data)
#' @param save_outputs Logical: whether to save outputs to disk
#' @return List with all results
run_bhapc_survey <- function(survey_name,
                              run_model = TRUE,
                              save_outputs = TRUE) {

  survey_name <- tolower(survey_name)
  meta <- BHAPC_META[[survey_name]]

  if (is.null(meta)) {
    stop("Unknown survey: ", survey_name)
  }

  message("\n", strrep("=", 70))
  message("BHAPC ANALYSIS: ", toupper(survey_name))
  message(strrep("=", 70))

  # Load data
  file_path <- derived_path(paste0("data_", survey_name, ".rds"))
  message("\nLoading data from: ", file_path)

  if (!file.exists(file_path)) {
    stop("Data file not found: ", file_path)
  }

  df <- readRDS(file_path)
  message("  Loaded ", format(nrow(df), big.mark = ","), " rows")

  # Prepare BHAPC data
  message("\n--- Data Preparation ---")
  bhapc_df <- prepare_bhapc_data(
    df,
    survey = survey_name,
    age_min = meta$age_min,
    age_max = meta$age_max,
    srh_scale = meta$srh_scale
  )

  # Create cell coverage diagnostics
  coverage <- create_cell_coverage(bhapc_df)
  message("\n  Cell coverage (age x period):")
  print(coverage, n = Inf)

  # Create cohort coverage diagnostics
  cohort_coverage <- create_cohort_coverage(bhapc_df)
  message("\n  Cohort coverage:")
  print(cohort_coverage, n = Inf)

  # Initialize results list
  results <- list(
    survey = survey_name,
    data_summary = summarize_bhapc_data(bhapc_df),
    bhapc_df = bhapc_df,
    coverage = coverage,
    cohort_coverage = cohort_coverage
  )

  # Fit model if requested
  if (run_model) {
    message("\n--- Model Fitting ---")

    model_result <- fit_bhapc_model(
      bhapc_df,
      outcome = "srh",
      adapt_delta = meta$adapt_delta,
      iter = meta$iter
    )

    results$model_result <- model_result

    # Extract variance components
    results$variance_components <- extract_variance_components(model_result$model)
    message("\n  Variance decomposition:")
    print(results$variance_components)

    # Check diagnostics
    message("\n  Model diagnostics:")
    message("    Max Rhat: ", round(max(model_result$diagnostics$Rhat, na.rm = TRUE), 3))
    message("    Min n_eff: ", round(min(model_result$diagnostics$n_eff, na.rm = TRUE), 0))
    message("    Converged: ", max(model_result$diagnostics$Rhat, na.rm = TRUE) < 1.01)
  }

  # Generate outputs if requested
  if (save_outputs) {
    message("\n--- Saving Outputs ---")

    # Save prepared data
    data_path <- file.path(bhapc_output_dir, paste0(survey_name, "_bhapc_data.rds"))
    saveRDS(bhapc_df, data_path)
    message("  Saved: ", data_path)

    if (run_model) {
      # Save model object
      model_path <- file.path(bhapc_output_dir, paste0(survey_name, "_bhapc_model.rds"))
      saveRDS(model_result, model_path)
      message("  Saved: ", model_path)

      # Create and save Table 2
      table2 <- create_table2(
        model_result$model,
        bhapc_df,
        survey = survey_name,
        output_path = file.path(bhapc_output_dir, paste0("table2_", survey_name, ".csv"))
      )
      results$table2 <- table2

      # Create and save Figure 2 (descriptive)
      fig2 <- create_figure2_descriptive(
        bhapc_df,
        survey = survey_name,
        output_path = file.path(bhapc_output_dir, paste0("figure2_", survey_name, ".png"))
      )
      results$figure2 <- fig2

      # Create and save Figure 3 (APC effects)
      fig3 <- create_figure3_apc_effects(
        model_result$model,
        bhapc_df,
        survey = survey_name,
        output_path = file.path(bhapc_output_dir, paste0("figure3_", survey_name, ".png"))
      )
      results$figure3 <- fig3

      # Save variance components
      vc_path <- file.path(bhapc_output_dir, paste0(survey_name, "_variance_decomposition.csv"))
      write.csv(results$variance_components, vc_path, row.names = FALSE)
      message("  Saved: ", vc_path)
    }
  }

  results$timestamp <- Sys.time()
  message("\n", toupper(survey_name), " BHAPC analysis complete at: ", Sys.time())

  results
}


#' Run BHAPC analysis for multiple surveys
#'
#' @param surveys Character vector of survey names (default: all)
#' @param run_model Logical: whether to fit models
#' @param save_outputs Logical: whether to save outputs
#' @return Named list with results for each survey
run_bhapc_all <- function(surveys = SURVEY_ORDER,
                           run_model = TRUE,
                           save_outputs = TRUE) {

  message("\n", strrep("=", 70))
  message("BHAPC ANALYSIS PIPELINE")
  message("Starting at: ", Sys.time())
  message("Surveys: ", paste(toupper(surveys), collapse = ", "))
  message(strrep("=", 70))

  all_results <- list()

  for (survey in surveys) {
    start_time <- Sys.time()

    tryCatch({
      results <- run_bhapc_survey(survey, run_model, save_outputs)
      all_results[[survey]] <- results

      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      message("\n", toupper(survey), " completed in ", round(elapsed, 1), " minutes")

    }, error = function(e) {
      message("\nERROR processing ", toupper(survey), ": ", e$message)
      all_results[[survey]] <- list(error = e$message)
    })
  }

  # Create cross-survey summary if multiple surveys
  if (length(surveys) > 1 && run_model) {
    message("\n--- Creating Cross-Survey Summary ---")

    # Combine variance decomposition
    variance_summary <- create_variance_summary(
      lapply(all_results, function(x) x$model_result)
    )

    summary_path <- file.path(bhapc_output_dir, "variance_summary_all_surveys.csv")
    write.csv(variance_summary, summary_path, row.names = FALSE)
    message("  Saved: ", summary_path)

    all_results$variance_summary <- variance_summary
  }

  message("\n\n", strrep("=", 70))
  message("BHAPC PIPELINE COMPLETE")
  message("Finished at: ", Sys.time())
  message(strrep("=", 70))

  invisible(all_results)
}


# ------------------------------------------------------------------------------
# Quick Analysis Functions (for development/testing)
# ------------------------------------------------------------------------------

#' Quick BHAPC analysis with reduced iterations (for testing)
#'
#' @param survey_name Survey name
#' @param iter Number of iterations (default 500 for quick testing)
#' @return Results list
quick_bhapc <- function(survey_name = "nhanes", iter = 500) {

  survey_name <- tolower(survey_name)
  meta <- BHAPC_META[[survey_name]]

  # Load and prep data
  df <- readRDS(derived_path(paste0("data_", survey_name, ".rds")))
  bhapc_df <- prepare_bhapc_data(
    df,
    survey = survey_name,
    age_min = meta$age_min,
    age_max = meta$age_max,
    srh_scale = meta$srh_scale
  )

  # Fit model with reduced iterations
  model_result <- fit_bhapc_model(
    bhapc_df,
    outcome = "srh",
    adapt_delta = 0.99,  # Lower for faster testing
    iter = iter,
    chains = 2  # Fewer chains for testing
  )

  list(
    bhapc_df = bhapc_df,
    model_result = model_result,
    variance = extract_variance_components(model_result$model)
  )
}


#' Data prep only (no model fitting)
#'
#' @param survey_name Survey name
#' @return Prepared BHAPC data frame
prep_bhapc_only <- function(survey_name = "nhanes") {

  survey_name <- tolower(survey_name)
  meta <- BHAPC_META[[survey_name]]

  df <- readRDS(derived_path(paste0("data_", survey_name, ".rds")))

  prepare_bhapc_data(
    df,
    survey = survey_name,
    age_min = meta$age_min,
    age_max = meta$age_max,
    srh_scale = meta$srh_scale
  )
}


# ------------------------------------------------------------------------------
# Run Pipeline (uncomment as needed)
# ------------------------------------------------------------------------------

# Test with NHANES first (smallest dataset)
# nhanes_result <- run_bhapc_survey("nhanes")

# Quick test with reduced iterations
# quick_result <- quick_bhapc("nhanes", iter = 500)

# Run all surveys
# all_results <- run_bhapc_all()

# Run subset of surveys
# subset_results <- run_bhapc_all(c("nhanes", "gss"))

# Data prep only (for checking data structure before model fitting)
# nhanes_bhapc <- prep_bhapc_only("nhanes")

message("\n06_bhapc_analysis.R loaded successfully")
message("Run run_bhapc_survey('nhanes') to start analysis")
