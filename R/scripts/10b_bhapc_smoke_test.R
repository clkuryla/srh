# ==============================================================================
# 10b_bhapc_smoke_test.R
# Smoke test for BHAPC pipeline - 10k random sample from each dataset
# Quick test to verify code runs on all 6 datasets before overnight run
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("BHAPC SMOKE TEST - 10k samples per dataset\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n\n")

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(patchwork)
})

# Source functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_table_generation.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

# Set DATA_DEPOT if not set
if (Sys.getenv("DATA_DEPOT") == "") {
  Sys.setenv(DATA_DEPOT = "/home/ubuntu/data_depot")
}

# Output directory
output_dir <- here("output", "bhapc_smoke_test")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260129)

# ==============================================================================
# Configuration
# ==============================================================================

SAMPLE_SIZE <- 10000  # 10k per dataset
MODEL_ITER <- 500     # Minimal iterations for smoke test
CHAINS <- 2           # Fewer chains for speed
CORES <- 4            # Cores per model

ALL_SURVEYS <- c("nhanes", "gss", "meps", "nhis", "cps", "brfss")

SURVEY_CONFIG <- list(
  nhanes = list(srh_scale = 5, age_min = 18, age_max = 80),
  gss    = list(srh_scale = 4, age_min = 18, age_max = 89),
  meps   = list(srh_scale = 5, age_min = 18, age_max = 85),
  nhis   = list(srh_scale = 5, age_min = 18, age_max = 85),
  brfss  = list(srh_scale = 5, age_min = 18, age_max = 89),
  cps    = list(srh_scale = 5, age_min = 18, age_max = 85)
)

# ==============================================================================
# Run smoke test for each survey
# ==============================================================================

results <- list()

for (survey_name in ALL_SURVEYS) {

  cat("\n", strrep("-", 60), "\n")
  cat("TESTING:", toupper(survey_name), "\n")
  cat(strrep("-", 60), "\n")

  config <- SURVEY_CONFIG[[survey_name]]
  start_time <- Sys.time()

  result <- tryCatch({

    # 1. Load data
    data_path <- file.path(
      Sys.getenv("DATA_DEPOT"),
      "_derived", "srh_project", "essential_datasets",
      paste0("data_essential_", survey_name, ".rds")
    )

    df <- readRDS(data_path)
    df <- df %>% drop_na(srh, age, year, wt)
    cat("  Original N:", format(nrow(df), big.mark = ","), "\n")

    # 2. Simple random sample (no stratification, no weight adjustment)
    if (nrow(df) > SAMPLE_SIZE) {
      df <- df %>% slice_sample(n = SAMPLE_SIZE)
    }
    cat("  Sampled N:", format(nrow(df), big.mark = ","), "\n")

    # 3. Prepare BHAPC data
    bhapc_df <- prepare_bhapc_data(
      df,
      survey = survey_name,
      age_min = config$age_min,
      age_max = config$age_max,
      srh_scale = config$srh_scale
    )
    cat("  BHAPC N:", format(nrow(bhapc_df), big.mark = ","), "\n")
    cat("  Periods:", paste(unique(bhapc_df$period_4yr), collapse = ", "), "\n")

    # 4. Fit main BHAPC model (minimal iterations)
    cat("  Fitting main model (iter =", MODEL_ITER, ")...\n")

    model_result <- fit_bhapc_model(
      bhapc_df,
      outcome = "srh",
      adapt_delta = 0.95,
      iter = MODEL_ITER,
      chains = CHAINS,
      cores = CORES,
      seed = 20260129
    )

    # 5. Extract variance decomposition
    variance_df <- extract_variance_components(model_result$model)

    period_pct <- variance_df$pct_of_total[variance_df$component == "period_4yr"]
    cohort_pct <- variance_df$pct_of_total[variance_df$component == "cohort_4yr"]

    cat("  Variance - Period:", round(period_pct, 1), "%, Cohort:", round(cohort_pct, 1), "%\n")

    # 6. Check diagnostics
    max_rhat <- max(model_result$diagnostics$Rhat, na.rm = TRUE)
    min_neff <- min(model_result$diagnostics$n_eff, na.rm = TRUE)
    cat("  Max Rhat:", round(max_rhat, 3), ", Min n_eff:", round(min_neff, 0), "\n")

    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    cat("  Completed in:", round(elapsed, 1), "minutes\n")
    cat("  STATUS: SUCCESS\n")

    list(
      status = "success",
      survey = survey_name,
      n_obs = nrow(bhapc_df),
      elapsed_min = as.numeric(elapsed),
      max_rhat = max_rhat,
      min_neff = min_neff,
      period_pct = period_pct,
      cohort_pct = cohort_pct
    )

  }, error = function(e) {
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    cat("  ERROR:", e$message, "\n")
    cat("  STATUS: FAILED\n")

    list(
      status = "error",
      survey = survey_name,
      error = e$message,
      elapsed_min = as.numeric(elapsed)
    )
  })

  results[[survey_name]] <- result
}

# ==============================================================================
# Summary
# ==============================================================================

cat("\n\n", strrep("=", 70), "\n")
cat("SMOKE TEST SUMMARY\n")
cat(strrep("=", 70), "\n\n")

# Define %||% operator for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

summary_df <- map_dfr(results, function(r) {
  tibble(
    Survey = toupper(r$survey),
    Status = r$status,
    N = r$n_obs %||% NA,
    `Time (min)` = round(r$elapsed_min %||% NA, 1),
    `Max Rhat` = round(r$max_rhat %||% NA, 3),
    `Period %` = round(r$period_pct %||% NA, 1),
    `Cohort %` = round(r$cohort_pct %||% NA, 1)
  )
})

print(summary_df, n = Inf)

# Count successes/failures
n_success <- sum(sapply(results, function(r) r$status == "success"))
n_failed <- length(results) - n_success

cat("\n")
cat("Passed:", n_success, "/", length(results), "\n")
cat("Failed:", n_failed, "/", length(results), "\n")

if (n_failed > 0) {
  cat("\nFailed surveys:\n")
  for (r in results) {
    if (r$status == "error") {
      cat("  ", toupper(r$survey), ":", r$error, "\n")
    }
  }
}

# Save summary
write.csv(summary_df, file.path(output_dir, "smoke_test_summary.csv"), row.names = FALSE)
saveRDS(results, file.path(output_dir, "smoke_test_results.rds"))

cat("\n", strrep("=", 70), "\n")
if (n_failed == 0) {
  cat("ALL SMOKE TESTS PASSED - Ready for full pipeline run\n")
} else {
  cat("SMOKE TESTS FAILED - Fix issues before running full pipeline\n")
}
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
