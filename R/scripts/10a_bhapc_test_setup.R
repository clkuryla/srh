# ==============================================================================
# 10a_bhapc_test_setup.R
# Quick test to verify BHAPC parallel pipeline setup
# Run this BEFORE running the full overnight script
# ==============================================================================

cat("\n=== BHAPC PARALLEL PIPELINE - SETUP TEST ===\n\n")

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(future)
  library(furrr)
})

# Test 1: Check R environment
cat("1. R Environment:\n")
cat("   R version:", R.version$version.string, "\n")
cat("   Cores available:", parallel::detectCores(), "\n")

# Test 2: Check DATA_DEPOT
cat("\n2. Data Depot:\n")
if (Sys.getenv("DATA_DEPOT") == "") {
  Sys.setenv(DATA_DEPOT = "/home/ubuntu/data_depot")
  cat("   DATA_DEPOT set to:", Sys.getenv("DATA_DEPOT"), "\n")
} else {
  cat("   DATA_DEPOT:", Sys.getenv("DATA_DEPOT"), "\n")
}

# Test 3: Check data files exist
cat("\n3. Data Files:\n")
data_path <- file.path(Sys.getenv("DATA_DEPOT"), "_derived", "srh_project", "essential_datasets")
surveys <- c("nhanes", "gss", "meps", "nhis", "cps", "brfss")

for (s in surveys) {
  fp <- file.path(data_path, paste0("data_essential_", s, ".rds"))
  if (file.exists(fp)) {
    df <- readRDS(fp)
    cat(sprintf("   %-8s: %12s rows [OK]\n", toupper(s), format(nrow(df), big.mark = ",")))
  } else {
    cat(sprintf("   %-8s: NOT FOUND [ERROR]\n", toupper(s)))
  }
}

# Test 4: Source all required functions
cat("\n4. Function Files:\n")
funcs <- c(
  "R/paths.R",
  "R/functions/subsample_survey.R",
  "R/functions/bhapc_data_prep.R",
  "R/functions/bhapc_model_fitting.R",
  "R/functions/bhapc_table_generation.R",
  "R/functions/bhapc_figure_generation.R"
)

for (f in funcs) {
  tryCatch({
    source(here(f))
    cat(sprintf("   %-50s [OK]\n", f))
  }, error = function(e) {
    cat(sprintf("   %-50s [ERROR: %s]\n", f, e$message))
  })
}

# Test 5: Quick data prep test
cat("\n5. Data Preparation Test (NHANES):\n")
tryCatch({
  df <- readRDS(file.path(data_path, "data_essential_nhanes.rds"))
  df_clean <- df %>% drop_na(srh, age, year, wt)
  bhapc_df <- prepare_bhapc_data(df_clean, survey = "nhanes", age_max = 80)
  cat("   Prep successful: N =", nrow(bhapc_df), "periods =",
      paste(unique(bhapc_df$period_4yr), collapse = ", "), "\n")
}, error = function(e) {
  cat("   ERROR:", e$message, "\n")
})

# Test 6: Quick subsampling test
cat("\n6. Subsampling Test (BRFSS → 200k):\n")
tryCatch({
  df <- readRDS(file.path(data_path, "data_essential_brfss.rds"))
  df_clean <- df %>% drop_na(srh, age, year, wt)
  cat("   Original N:", format(nrow(df_clean), big.mark = ","), "\n")

  sub_result <- subsample_survey(df_clean, target_n = 200000, seed = 1248)
  cat("   Subsampled N:", format(nrow(sub_result$data), big.mark = ","), "\n")

  # Check weighted mean SRH preservation
  orig_mean <- sub_result$validation %>% filter(variable == "srh_mean") %>% pull(original)
  sub_mean <- sub_result$validation %>% filter(variable == "srh_mean") %>% pull(subsampled)
  cat("   SRH mean - Original:", round(orig_mean, 4), "Subsampled:", round(sub_mean, 4), "\n")
}, error = function(e) {
  cat("   ERROR:", e$message, "\n")
})

# Test 7: rstanarm availability
cat("\n7. rstanarm Test:\n")
tryCatch({
  library(rstanarm)
  cat("   rstanarm version:", as.character(packageVersion("rstanarm")), "[OK]\n")
}, error = function(e) {
  cat("   ERROR:", e$message, "\n")
})

# Test 8: Output directory
cat("\n8. Output Directory:\n")
output_dir <- here("output", "bhapc_parallel")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("   Created:", output_dir, "\n")
} else {
  cat("   Exists:", output_dir, "\n")
}

# Summary
cat("\n", strrep("=", 50), "\n")
cat("SETUP TEST COMPLETE\n")
cat("If all tests passed, run the full pipeline:\n")
cat("  screen -S bhapc\n")
cat("  Rscript R/scripts/10_bhapc_parallel_all_surveys.R 2>&1 | tee output/bhapc_parallel/bhapc.log\n")
cat("  # Ctrl+A, D to detach\n")
cat(strrep("=", 50), "\n")
