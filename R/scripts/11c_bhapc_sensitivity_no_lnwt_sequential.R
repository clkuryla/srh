# ==============================================================================
# 11c_bhapc_sensitivity_no_lnwt_sequential.R
# Sensitivity Analysis: BHAPC without lnWt covariate (NHIS and MEPS - sequential)
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BHAPC SENSITIVITY ANALYSIS - NO lnWt (NHIS & MEPS SEQUENTIAL)\n")
cat("Started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
})

source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "subsample_survey.R"))

output_dir <- here("output", "bhapc_sensitivity_no_lnwt")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

SUBSAMPLE_TARGET <- 100000
ITER <- 4000  # Reduced for speed
ADAPT_DELTA <- 0.99
N_CORES <- parallel::detectCores()

# ==============================================================================
# Function to run one survey
# ==============================================================================

run_one_survey <- function(survey_name, srh_scale, age_min, age_max) {

  cat("\n", strrep("=", 70), "\n")
  cat("SENSITIVITY ANALYSIS:", toupper(survey_name), "(NO lnWt)\n")
  cat("Started at:", format(Sys.time(), "%H:%M:%S"), "\n")
  cat(strrep("=", 70), "\n")

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
  if (nrow(df) > SUBSAMPLE_TARGET) {
    cat("  Subsampling to", format(SUBSAMPLE_TARGET, big.mark = ","), "...\n")
    sub_result <- subsample_survey(df, target_n = SUBSAMPLE_TARGET, seed = 20260205)
    df <- sub_result$data %>% select(-wt) %>% rename(wt = wt_sub)
  }
  cat("  Final N:", format(nrow(df), big.mark = ","), "\n")

  # Prepare BHAPC data
  bhapc_df <- prepare_bhapc_data(df, survey = survey_name, age_min = age_min,
                                  age_max = age_max, srh_scale = srh_scale)

  cat("  BHAPC N:", format(nrow(bhapc_df), big.mark = ","), "\n")

  # Fit model WITHOUT lnWt
  cat("\n  Fitting model (NO lnWt)...\n")
  cat("  Formula: srh ~ age + scale(age_squared) + (1|period_4yr) + (1|cohort_4yr)\n")
  cat("  iter =", ITER, ", cores =", N_CORES, "\n")

  model_start <- Sys.time()

  model_no_lnwt <- stan_lmer(
    srh ~ age + scale(age_squared) + (1|period_4yr) + (1|cohort_4yr),
    data = bhapc_df,
    adapt_delta = ADAPT_DELTA,
    iter = ITER,
    chains = 4,
    cores = N_CORES,
    seed = 20260205
  )

  model_elapsed <- difftime(Sys.time(), model_start, units = "mins")
  cat("  Model completed in", round(model_elapsed, 1), "minutes\n")

  # Extract variance components
  vc <- VarCorr(model_no_lnwt)
  var_period <- as.numeric(vc$period_4yr)
  var_cohort <- as.numeric(vc$cohort_4yr)
  var_resid <- sigma(model_no_lnwt)^2
  var_total <- var_period + var_cohort + var_resid

  cat("\n  Variance Decomposition:\n")
  cat("    Period:", round(var_period / var_total * 100, 2), "%\n")
  cat("    Cohort:", round(var_cohort / var_total * 100, 2), "%\n")
  cat("    Residual:", round(var_resid / var_total * 100, 2), "%\n")

  # Extract fixed effects
  summary_df <- as.data.frame(summary(model_no_lnwt, probs = c(0.05, 0.95)))
  summary_df$term <- rownames(summary_df)

  age_lin <- summary_df$mean[summary_df$term == "age"]
  age_quad <- summary_df$mean[summary_df$term == "scale(age_squared)"]

  cat("\n  Fixed Effects:\n")
  cat("    Age (linear):", round(age_lin, 6), "\n")
  cat("    Age (quadratic, scaled):", round(age_quad, 4), "\n")

  # Check convergence
  max_rhat <- max(summary_df$Rhat, na.rm = TRUE)
  cat("\n  Diagnostics: Max Rhat =", round(max_rhat, 3), "\n")

  # Save results
  survey_upper <- toupper(survey_name)

  table2 <- tibble(
    survey = survey_upper,
    effect = c("Fixed Effects", "Age (linear)", "Age (quadratic)", "",
               "Variance Components", "Period variance", "Cohort variance",
               "Residual variance", "Total variance"),
    estimate = c(NA, age_lin, age_quad, NA, NA, var_period, var_cohort, var_resid, var_total),
    ci_90 = c(NA,
              paste0("[", round(summary_df$`5%`[summary_df$term == "age"], 4),
                     ", ", round(summary_df$`95%`[summary_df$term == "age"], 4), "]"),
              paste0("[", round(summary_df$`5%`[summary_df$term == "scale(age_squared)"], 4),
                     ", ", round(summary_df$`95%`[summary_df$term == "scale(age_squared)"], 4), "]"),
              NA, NA,
              paste0("(", round(var_period / var_total * 100, 1), "%)"),
              paste0("(", round(var_cohort / var_total * 100, 1), "%)"),
              paste0("(", round(var_resid / var_total * 100, 1), "%)"),
              "(100%)")
  )

  write.csv(table2, file.path(output_dir, paste0("table2_", survey_name, "_no_lnwt.csv")), row.names = FALSE)

  variance_df <- tibble(
    component = c("period_4yr", "cohort_4yr", "Residual", "Total"),
    variance = c(var_period, var_cohort, var_resid, var_total),
    pct_of_total = c(var_period, var_cohort, var_resid, var_total) / var_total * 100
  )
  write.csv(variance_df, file.path(output_dir, paste0(survey_name, "_variance_decomposition_no_lnwt.csv")), row.names = FALSE)

  saveRDS(model_no_lnwt, file.path(output_dir, paste0(survey_name, "_bhapc_model_no_lnwt.rds")))

  # Compare with original
  original_path <- here("output", "bhapc_parallel", survey_name, paste0("table2_", survey_name, ".csv"))

  if (file.exists(original_path)) {
    orig <- read.csv(original_path)
    orig_age_lin <- as.numeric(orig$estimate[orig$effect == "Age (linear)"])
    orig_period <- as.numeric(orig$estimate[orig$effect == "Period variance"])
    orig_cohort <- as.numeric(orig$estimate[orig$effect == "Cohort variance"])
    orig_resid <- as.numeric(orig$estimate[orig$effect == "Residual variance"])
    orig_total <- orig_period + orig_cohort + orig_resid
    orig_lnwt <- as.numeric(orig$estimate[orig$effect == "Log weight"])

    comparison <- tibble(
      metric = c("Age (linear)", "Period var %", "Cohort var %", "Residual var %", "Original lnWt coef"),
      original_with_lnWt = c(orig_age_lin, orig_period/orig_total*100, orig_cohort/orig_total*100, orig_resid/orig_total*100, orig_lnwt),
      sensitivity_no_lnWt = c(age_lin, var_period/var_total*100, var_cohort/var_total*100, var_resid/var_total*100, NA),
      difference = c(age_lin - orig_age_lin,
                     var_period/var_total*100 - orig_period/orig_total*100,
                     var_cohort/var_total*100 - orig_cohort/orig_total*100,
                     var_resid/var_total*100 - orig_resid/orig_total*100, NA)
    )

    write.csv(comparison, file.path(output_dir, paste0(survey_name, "_comparison_lnwt_sensitivity.csv")), row.names = FALSE)

    cat("\n  Comparison with original:\n")
    cat("    Age (linear): ", sprintf("%.5f", orig_age_lin), " vs ", sprintf("%.5f", age_lin),
        " (diff: ", sprintf("%+.5f", age_lin - orig_age_lin), ")\n", sep = "")
    cat("    Period var %: ", sprintf("%.2f%%", orig_period/orig_total*100),
        " vs ", sprintf("%.2f%%", var_period/var_total*100), "\n", sep = "")
    cat("    Cohort var %: ", sprintf("%.2f%%", orig_cohort/orig_total*100),
        " vs ", sprintf("%.2f%%", var_cohort/var_total*100), "\n", sep = "")
    cat("    Original lnWt coefficient:", sprintf("%.4f", orig_lnwt), "\n")
  }

  cat("\n  Files saved to:", output_dir, "\n")

  list(
    survey = survey_name,
    status = "success",
    n_obs = nrow(bhapc_df),
    elapsed_min = as.numeric(model_elapsed),
    period_var_pct = var_period / var_total * 100,
    cohort_var_pct = var_cohort / var_total * 100
  )
}

# ==============================================================================
# Run NHIS then MEPS
# ==============================================================================

start_time <- Sys.time()

results <- list()

# NHIS
results$nhis <- run_one_survey("nhis", srh_scale = 5, age_min = 18, age_max = 85)

# MEPS
results$meps <- run_one_survey("meps", srh_scale = 5, age_min = 18, age_max = 85)

total_elapsed <- difftime(Sys.time(), start_time, units = "mins")

# ==============================================================================
# Summary
# ==============================================================================

cat("\n\n", strrep("=", 80), "\n")
cat("ALL COMPLETE\n")
cat("Total time:", round(total_elapsed, 1), "minutes\n")
cat(strrep("=", 80), "\n\n")

summary_df <- map_dfr(results, as_tibble)
print(summary_df)

write.csv(summary_df, file.path(output_dir, "nhis_meps_summary.csv"), row.names = FALSE)

cat("\nOutputs saved to:", output_dir, "\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
