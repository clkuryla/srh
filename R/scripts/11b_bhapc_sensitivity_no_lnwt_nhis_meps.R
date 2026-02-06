# ==============================================================================
# 11b_bhapc_sensitivity_no_lnwt_nhis_meps.R
# Sensitivity Analysis: BHAPC without lnWt covariate (NHIS and MEPS)
# Author: Christine Lucille Kuryla
#
# Purpose: Test robustness of BHAPC results by removing log-weight covariate
# Uses 100K subsampling for speed
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BHAPC SENSITIVITY ANALYSIS - NO lnWt (NHIS & MEPS)\n")
cat("Started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(future)
  library(furrr)
})

# Source paths and functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "subsample_survey.R"))

# Create output directory
output_dir <- here("output", "bhapc_sensitivity_no_lnwt")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

# ==============================================================================
# Configuration
# ==============================================================================

SUBSAMPLE_TARGET <- 100000  # 100K for speed
N_CORES <- parallel::detectCores()
ITER <- 6000
ADAPT_DELTA <- 0.998

SURVEY_CONFIG <- list(
  nhis = list(srh_scale = 5, age_min = 18, age_max = 85),
  meps = list(srh_scale = 5, age_min = 18, age_max = 85)
)

cat("Configuration:\n")
cat("  Subsample target:", format(SUBSAMPLE_TARGET, big.mark = ","), "\n")
cat("  Iterations:", ITER, "\n")
cat("  Total cores:", N_CORES, "\n")
cat("  Cores per survey:", floor(N_CORES / 2), "\n\n")

# ==============================================================================
# Run single survey analysis
# ==============================================================================

run_sensitivity_analysis <- function(survey_name, config, subsample_target,
                                      iter, adapt_delta, cores_per_survey, output_dir) {

  # Load packages in worker

suppressPackageStartupMessages({
    library(tidyverse)
    library(here)
    library(rstanarm)
    library(broom.mixed)
  })

  source(here("R", "paths.R"))
  source(here("R", "functions", "bhapc_data_prep.R"))
  source(here("R", "functions", "subsample_survey.R"))

  cat("\n", strrep("=", 70), "\n")
  cat("SENSITIVITY ANALYSIS: ", toupper(survey_name), " (NO lnWt)\n")
  cat("Started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat(strrep("=", 70), "\n")

  tryCatch({
    # Load data
    data_path <- file.path(
      Sys.getenv("DATA_DEPOT", "/home/ubuntu/data_depot"),
      "_derived", "srh_project", "essential_datasets",
      paste0("data_essential_", survey_name, ".rds")
    )

    df <- readRDS(data_path) %>% drop_na(srh, age, year, wt)
    original_n <- nrow(df)
    cat("  Original N:", format(original_n, big.mark = ","), "\n")

    # Subsample
    if (nrow(df) > subsample_target) {
      cat("  Subsampling to", format(subsample_target, big.mark = ","), "...\n")
      sub_result <- subsample_survey(df, target_n = subsample_target, seed = 20260205)
      df <- sub_result$data %>% select(-wt) %>% rename(wt = wt_sub)
    }
    cat("  Final N:", format(nrow(df), big.mark = ","), "\n")

    # Prepare BHAPC data
    bhapc_df <- prepare_bhapc_data(
      df,
      survey = survey_name,
      age_min = config$age_min,
      age_max = config$age_max,
      srh_scale = config$srh_scale
    )

    cat("  BHAPC N:", format(nrow(bhapc_df), big.mark = ","), "\n")

    # Fit model WITHOUT lnWt
    cat("\n  Fitting model (NO lnWt)...\n")
    cat("  Formula: srh ~ age + scale(age_squared) + (1|period_4yr) + (1|cohort_4yr)\n")

    model_start <- Sys.time()

    model_no_lnwt <- stan_lmer(
      srh ~ age + scale(age_squared) + (1|period_4yr) + (1|cohort_4yr),
      data = bhapc_df,
      adapt_delta = adapt_delta,
      iter = iter,
      chains = 4,
      cores = cores_per_survey,
      seed = 20260205
    )

    model_elapsed <- difftime(Sys.time(), model_start, units = "mins")
    cat("  Model completed in", round(model_elapsed, 1), "minutes\n")

    # Extract variance components
    vc <- VarCorr(model_no_lnwt)
    sigma_resid <- sigma(model_no_lnwt)

    var_period <- as.numeric(vc$period_4yr)
    var_cohort <- as.numeric(vc$cohort_4yr)
    var_resid <- sigma_resid^2
    var_total <- var_period + var_cohort + var_resid

    variance_df <- tibble(
      component = c("period_4yr", "cohort_4yr", "Residual", "Total"),
      variance = c(var_period, var_cohort, var_resid, var_total),
      pct_of_total = c(var_period, var_cohort, var_resid, var_total) / var_total * 100
    )

    cat("\n  Variance Decomposition (NO lnWt):\n")
    cat("    Period:", round(var_period / var_total * 100, 2), "%\n")
    cat("    Cohort:", round(var_cohort / var_total * 100, 2), "%\n")
    cat("    Residual:", round(var_resid / var_total * 100, 2), "%\n")

    # Extract fixed effects
    summary_df <- as.data.frame(summary(model_no_lnwt, probs = c(0.05, 0.95)))
    summary_df$term <- rownames(summary_df)

    fixed_effects <- summary_df %>%
      filter(term %in% c("(Intercept)", "age", "scale(age_squared)")) %>%
      select(term, mean, sd, `5%`, `95%`)

    age_lin <- fixed_effects$mean[fixed_effects$term == "age"]
    age_quad_scaled <- fixed_effects$mean[fixed_effects$term == "scale(age_squared)"]

    cat("\n  Fixed Effects:\n")
    cat("    Age (linear):", round(age_lin, 6), "\n")
    cat("    Age (quadratic, scaled):", round(age_quad_scaled, 4), "\n")

    # Check convergence
    summary_full <- as.data.frame(summary(model_no_lnwt))
    max_rhat <- max(summary_full$Rhat, na.rm = TRUE)
    min_neff <- min(summary_full$n_eff, na.rm = TRUE)

    cat("\n  Diagnostics: Rhat =", round(max_rhat, 3), ", n_eff =", round(min_neff, 0), "\n")

    # Save results
    survey_upper <- toupper(survey_name)

    # Table 2 format
    table2_no_lnwt <- tibble(
      survey = survey_upper,
      effect = c("Fixed Effects", "Age (linear)", "Age (quadratic)", "",
                 "Variance Components", "Period variance", "Cohort variance",
                 "Residual variance", "Total variance"),
      estimate = c(NA, age_lin, age_quad_scaled, NA, NA,
                   var_period, var_cohort, var_resid, var_total),
      ci_90 = c(NA,
                paste0("[", round(fixed_effects$`5%`[fixed_effects$term == "age"], 4),
                       ", ", round(fixed_effects$`95%`[fixed_effects$term == "age"], 4), "]"),
                paste0("[", round(fixed_effects$`5%`[fixed_effects$term == "scale(age_squared)"], 4),
                       ", ", round(fixed_effects$`95%`[fixed_effects$term == "scale(age_squared)"], 4), "]"),
                NA, NA,
                paste0("(", round(var_period / var_total * 100, 1), "%)"),
                paste0("(", round(var_cohort / var_total * 100, 1), "%)"),
                paste0("(", round(var_resid / var_total * 100, 1), "%)"),
                "(100%)")
    )

    write.csv(table2_no_lnwt,
              file.path(output_dir, paste0("table2_", survey_name, "_no_lnwt.csv")),
              row.names = FALSE)

    write.csv(variance_df,
              file.path(output_dir, paste0(survey_name, "_variance_decomposition_no_lnwt.csv")),
              row.names = FALSE)

    saveRDS(model_no_lnwt,
            file.path(output_dir, paste0(survey_name, "_bhapc_model_no_lnwt.rds")))

    # Compare with original
    original_path <- here("output", "bhapc_parallel", survey_name,
                          paste0("table2_", survey_name, ".csv"))

    if (file.exists(original_path)) {
      original_table2 <- read.csv(original_path)

      orig_age_lin <- as.numeric(original_table2$estimate[original_table2$effect == "Age (linear)"])
      orig_period_var <- as.numeric(original_table2$estimate[original_table2$effect == "Period variance"])
      orig_cohort_var <- as.numeric(original_table2$estimate[original_table2$effect == "Cohort variance"])
      orig_resid_var <- as.numeric(original_table2$estimate[original_table2$effect == "Residual variance"])
      orig_total_var <- orig_period_var + orig_cohort_var + orig_resid_var
      orig_lnwt <- as.numeric(original_table2$estimate[original_table2$effect == "Log weight"])

      comparison_df <- tibble(
        metric = c("Age (linear)", "Period var %", "Cohort var %", "Residual var %", "Original lnWt coef"),
        original_with_lnWt = c(orig_age_lin,
                               orig_period_var / orig_total_var * 100,
                               orig_cohort_var / orig_total_var * 100,
                               orig_resid_var / orig_total_var * 100,
                               orig_lnwt),
        sensitivity_no_lnWt = c(age_lin,
                                var_period / var_total * 100,
                                var_cohort / var_total * 100,
                                var_resid / var_total * 100,
                                NA),
        difference = c(age_lin - orig_age_lin,
                       var_period / var_total * 100 - orig_period_var / orig_total_var * 100,
                       var_cohort / var_total * 100 - orig_cohort_var / orig_total_var * 100,
                       var_resid / var_total * 100 - orig_resid_var / orig_total_var * 100,
                       NA)
      )

      write.csv(comparison_df,
                file.path(output_dir, paste0(survey_name, "_comparison_lnwt_sensitivity.csv")),
                row.names = FALSE)

      cat("\n  Comparison with original:\n")
      cat("    Age (linear): ", sprintf("%.5f", orig_age_lin), " vs ", sprintf("%.5f", age_lin),
          " (diff: ", sprintf("%+.5f", age_lin - orig_age_lin), ")\n", sep = "")
      cat("    Period var %: ", sprintf("%.2f%%", orig_period_var / orig_total_var * 100),
          " vs ", sprintf("%.2f%%", var_period / var_total * 100), "\n", sep = "")
      cat("    Cohort var %: ", sprintf("%.2f%%", orig_cohort_var / orig_total_var * 100),
          " vs ", sprintf("%.2f%%", var_cohort / var_total * 100), "\n", sep = "")
      cat("    Original lnWt coefficient:", sprintf("%.4f", orig_lnwt), "\n")
    }

    cat("\n  Files saved to:", output_dir, "\n")
    cat(strrep("=", 70), "\n")

    list(
      status = "success",
      survey = survey_name,
      n_obs = nrow(bhapc_df),
      elapsed_min = as.numeric(model_elapsed),
      max_rhat = max_rhat,
      converged = max_rhat < 1.01,
      period_var_pct = var_period / var_total * 100,
      cohort_var_pct = var_cohort / var_total * 100
    )

  }, error = function(e) {
    cat("\nERROR:", e$message, "\n")
    list(status = "error", survey = survey_name, error = e$message)
  })
}

# ==============================================================================
# Run both surveys in parallel
# ==============================================================================

cat("Running NHIS and MEPS in parallel...\n\n")

plan(multisession, workers = 2)

start_time <- Sys.time()

results <- future_map(
  names(SURVEY_CONFIG),
  function(survey_name) {
    run_sensitivity_analysis(
      survey_name = survey_name,
      config = SURVEY_CONFIG[[survey_name]],
      subsample_target = SUBSAMPLE_TARGET,
      iter = ITER,
      adapt_delta = ADAPT_DELTA,
      cores_per_survey = floor(N_CORES / 2),
      output_dir = output_dir
    )
  },
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

names(results) <- names(SURVEY_CONFIG)

total_elapsed <- difftime(Sys.time(), start_time, units = "mins")

plan(sequential)

# ==============================================================================
# Summary
# ==============================================================================

cat("\n\n", strrep("=", 80), "\n")
cat("ALL SENSITIVITY ANALYSES COMPLETE\n")
cat("Total time:", round(total_elapsed, 1), "minutes\n")
cat(strrep("=", 80), "\n\n")

# Print summary
summary_df <- map_dfr(results, function(r) {
  tibble(
    survey = r$survey,
    status = r$status,
    n_obs = r$n_obs %||% NA,
    elapsed_min = round(r$elapsed_min %||% NA, 1),
    converged = r$converged %||% NA,
    period_var_pct = round(r$period_var_pct %||% NA, 2),
    cohort_var_pct = round(r$cohort_var_pct %||% NA, 2)
  )
})

print(summary_df)

write.csv(summary_df, file.path(output_dir, "nhis_meps_summary.csv"), row.names = FALSE)

cat("\nOutputs saved to:", output_dir, "\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
