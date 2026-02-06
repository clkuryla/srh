# ==============================================================================
# 11_bhapc_sensitivity_no_lnwt_gss.R
# Sensitivity Analysis: BHAPC without lnWt covariate (GSS only)
# Author: Christine Lucille Kuryla
#
# Purpose: Test robustness of BHAPC results by removing log-weight covariate
#
# Usage:
#   Rscript R/scripts/11_bhapc_sensitivity_no_lnwt_gss.R
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BHAPC SENSITIVITY ANALYSIS - NO lnWt (GSS ONLY)\n")
cat("Started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
})

# Source paths and data prep functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))

# Create output directory
output_dir <- here("output", "bhapc_sensitivity_no_lnwt")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

# ==============================================================================
# Configuration for GSS
# ==============================================================================

SURVEY_NAME <- "gss"
SRH_SCALE <- 4  # GSS uses 4-point scale
AGE_MIN <- 18
AGE_MAX <- 89
ITER <- 6000
ADAPT_DELTA <- 0.998
N_CORES <- parallel::detectCores()

cat("Configuration:\n")
cat("  Survey:", toupper(SURVEY_NAME), "\n")
cat("  SRH scale:", SRH_SCALE, "\n")
cat("  Age range:", AGE_MIN, "-", AGE_MAX, "\n")
cat("  Iterations:", ITER, "\n")
cat("  Cores:", N_CORES, "\n\n")

# ==============================================================================
# Load and Prepare Data
# ==============================================================================

cat("--- STEP 1: DATA LOADING & PREPARATION ---\n")

# Load GSS data
data_path <- file.path(
  Sys.getenv("DATA_DEPOT", "/home/ubuntu/data_depot"),
  "_derived", "srh_project", "essential_datasets",
  "data_essential_gss.rds"
)

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}

df <- readRDS(data_path)
df <- df %>% drop_na(srh, age, year, wt)

cat("  Loaded N:", format(nrow(df), big.mark = ","), "\n")

# Prepare BHAPC data
bhapc_df <- prepare_bhapc_data(
  df,
  survey = SURVEY_NAME,
  age_min = AGE_MIN,
  age_max = AGE_MAX,
  srh_scale = SRH_SCALE
)

cat("  BHAPC N:", format(nrow(bhapc_df), big.mark = ","), "\n")
cat("  Periods:", paste(unique(bhapc_df$period_4yr), collapse = ", "), "\n")
cat("  Cohorts:", length(unique(bhapc_df$cohort_4yr)), "\n\n")

# ==============================================================================
# Fit BHAPC Model WITHOUT lnWt
# ==============================================================================

cat("--- STEP 2: FITTING BHAPC MODEL WITHOUT lnWt ---\n")

# Formula WITHOUT lnWt (this is the key difference from the original)
formula_no_lnwt <- srh ~ age + scale(age_squared) + (1|period_4yr) + (1|cohort_4yr)

cat("  Formula: srh ~ age + scale(age_squared) + (1|period_4yr) + (1|cohort_4yr)\n")
cat("  NOTE: lnWt EXCLUDED from model\n")
cat("  iter =", ITER, ", adapt_delta =", ADAPT_DELTA, "\n")
cat("  cores =", N_CORES, "\n")

model_start <- Sys.time()

model_no_lnwt <- stan_lmer(
  formula = formula_no_lnwt,
  data = bhapc_df,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = 20260205
)

model_elapsed <- difftime(Sys.time(), model_start, units = "mins")
cat("  Model completed in", round(model_elapsed, 1), "minutes\n\n")

# ==============================================================================
# Extract Results
# ==============================================================================

cat("--- STEP 3: EXTRACTING RESULTS ---\n")

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
  pct_of_total = c(
    var_period / var_total * 100,
    var_cohort / var_total * 100,
    var_resid / var_total * 100,
    100
  )
)

cat("\nVariance Decomposition (NO lnWt):\n")
for (i in 1:nrow(variance_df)) {
  cat("  ", variance_df$component[i], ": ",
      round(variance_df$variance[i], 4), " (",
      round(variance_df$pct_of_total[i], 1), "%)\n", sep = "")
}

# Extract fixed effects with 90% CIs
summary_df <- as.data.frame(summary(model_no_lnwt, probs = c(0.05, 0.95)))
summary_df$term <- rownames(summary_df)

fixed_effects <- summary_df %>%
  filter(term %in% c("(Intercept)", "age", "scale(age_squared)")) %>%
  select(term, mean, sd, `5%`, `95%`) %>%
  rename(
    estimate = mean,
    std_error = sd,
    ci_lower_90 = `5%`,
    ci_upper_90 = `95%`
  )

cat("\nFixed Effects (NO lnWt):\n")
for (i in 1:nrow(fixed_effects)) {
  cat("  ", fixed_effects$term[i], ": ",
      round(fixed_effects$estimate[i], 6), " [",
      round(fixed_effects$ci_lower_90[i], 6), ", ",
      round(fixed_effects$ci_upper_90[i], 6), "]\n", sep = "")
}

# Check convergence
summary_full <- as.data.frame(summary(model_no_lnwt))
summary_full$parameter <- rownames(summary_full)
max_rhat <- max(summary_full$Rhat, na.rm = TRUE)
min_neff <- min(summary_full$n_eff, na.rm = TRUE)

cat("\nDiagnostics:\n")
cat("  Max Rhat:", round(max_rhat, 3), "\n")
cat("  Min n_eff:", round(min_neff, 0), "\n")
cat("  Converged:", max_rhat < 1.01, "\n")

# ==============================================================================
# Save Results
# ==============================================================================

cat("\n--- STEP 4: SAVING RESULTS ---\n")

# Create Table 2 format (similar to original)
table2_no_lnwt <- tibble(
  survey = "GSS",
  effect = c(
    "Fixed Effects",
    "Age (linear)",
    "Age (quadratic)",
    "",
    "Variance Components",
    "Period variance",
    "Cohort variance",
    "Residual variance",
    "Total variance"
  ),
  estimate = c(
    NA,
    fixed_effects$estimate[fixed_effects$term == "age"],
    fixed_effects$estimate[fixed_effects$term == "scale(age_squared)"],
    NA,
    NA,
    var_period,
    var_cohort,
    var_resid,
    var_total
  ),
  ci_90 = c(
    NA,
    paste0("[", round(fixed_effects$ci_lower_90[fixed_effects$term == "age"], 6),
           ", ", round(fixed_effects$ci_upper_90[fixed_effects$term == "age"], 6), "]"),
    paste0("[", round(fixed_effects$ci_lower_90[fixed_effects$term == "scale(age_squared)"], 6),
           ", ", round(fixed_effects$ci_upper_90[fixed_effects$term == "scale(age_squared)"], 6), "]"),
    NA,
    NA,
    paste0("(", round(var_period / var_total * 100, 1), "%)"),
    paste0("(", round(var_cohort / var_total * 100, 1), "%)"),
    paste0("(", round(var_resid / var_total * 100, 1), "%)"),
    "(100%)"
  )
)

# Save table
write.csv(table2_no_lnwt,
          file.path(output_dir, "table2_gss_no_lnwt.csv"),
          row.names = FALSE)
cat("  Saved: table2_gss_no_lnwt.csv\n")

# Save variance decomposition
write.csv(variance_df,
          file.path(output_dir, "gss_variance_decomposition_no_lnwt.csv"),
          row.names = FALSE)
cat("  Saved: gss_variance_decomposition_no_lnwt.csv\n")

# Save model object
saveRDS(model_no_lnwt, file.path(output_dir, "gss_bhapc_model_no_lnwt.rds"))
cat("  Saved: gss_bhapc_model_no_lnwt.rds\n")

# ==============================================================================
# Compare with Original Results
# ==============================================================================

cat("\n--- STEP 5: COMPARISON WITH ORIGINAL ---\n")

# Load original table2
original_path <- here("output", "bhapc_parallel", "gss", "table2_gss.csv")
if (file.exists(original_path)) {
  original_table2 <- read.csv(original_path)

  # Extract key values for comparison
  orig_age_lin <- as.numeric(original_table2$estimate[original_table2$effect == "Age (linear)"])
  orig_age_quad <- as.numeric(original_table2$estimate[original_table2$effect == "Age (quadratic)"])
  orig_period_var <- as.numeric(original_table2$estimate[original_table2$effect == "Period variance"])
  orig_cohort_var <- as.numeric(original_table2$estimate[original_table2$effect == "Cohort variance"])
  orig_resid_var <- as.numeric(original_table2$estimate[original_table2$effect == "Residual variance"])
  orig_total_var <- orig_period_var + orig_cohort_var + orig_resid_var

  new_age_lin <- fixed_effects$estimate[fixed_effects$term == "age"]
  new_age_quad <- fixed_effects$estimate[fixed_effects$term == "scale(age_squared)"]

  cat("\n  Comparison: Original (with lnWt) vs Sensitivity (no lnWt)\n")
  cat("  ", strrep("-", 60), "\n")
  cat("  Age (linear):    ", sprintf("%8.5f", orig_age_lin), " vs ", sprintf("%8.5f", new_age_lin),
      " (diff: ", sprintf("%+.5f", new_age_lin - orig_age_lin), ")\n", sep = "")
  cat("  Age (quadratic): ", sprintf("%8.6f", orig_age_quad), " vs ", sprintf("%8.6f", new_age_quad),
      " (diff: ", sprintf("%+.6f", new_age_quad - orig_age_quad), ")\n", sep = "")
  cat("  ", strrep("-", 60), "\n")
  cat("  Period var %:    ", sprintf("%6.2f%%", orig_period_var / orig_total_var * 100),
      " vs ", sprintf("%6.2f%%", var_period / var_total * 100), "\n", sep = "")
  cat("  Cohort var %:    ", sprintf("%6.2f%%", orig_cohort_var / orig_total_var * 100),
      " vs ", sprintf("%6.2f%%", var_cohort / var_total * 100), "\n", sep = "")
  cat("  Residual var %:  ", sprintf("%6.2f%%", orig_resid_var / orig_total_var * 100),
      " vs ", sprintf("%6.2f%%", var_resid / var_total * 100), "\n", sep = "")

  # Save comparison
  comparison_df <- tibble(
    metric = c("Age (linear)", "Age (quadratic)", "Period var %", "Cohort var %", "Residual var %"),
    original_with_lnWt = c(orig_age_lin, orig_age_quad,
                           orig_period_var / orig_total_var * 100,
                           orig_cohort_var / orig_total_var * 100,
                           orig_resid_var / orig_total_var * 100),
    sensitivity_no_lnWt = c(new_age_lin, new_age_quad,
                            var_period / var_total * 100,
                            var_cohort / var_total * 100,
                            var_resid / var_total * 100),
    difference = c(new_age_lin - orig_age_lin,
                   new_age_quad - orig_age_quad,
                   var_period / var_total * 100 - orig_period_var / orig_total_var * 100,
                   var_cohort / var_total * 100 - orig_cohort_var / orig_total_var * 100,
                   var_resid / var_total * 100 - orig_resid_var / orig_total_var * 100)
  )

  write.csv(comparison_df,
            file.path(output_dir, "gss_comparison_lnwt_sensitivity.csv"),
            row.names = FALSE)
  cat("\n  Saved: gss_comparison_lnwt_sensitivity.csv\n")

} else {
  cat("  Original table2 not found at:", original_path, "\n")
  cat("  Skipping comparison.\n")
}

# ==============================================================================
# Done
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("SENSITIVITY ANALYSIS COMPLETE\n")
cat("Total time:", round(difftime(Sys.time(), model_start, units = "mins"), 1), "minutes\n")
cat("Outputs saved to:", output_dir, "\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n")
