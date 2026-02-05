# ==============================================================================
# 19_bhapc_spline_sensitivity.R
# BHAPC Spline Sensitivity Analysis: Compare Model Specifications
# Author: Christine Lucille Kuryla
#
# Purpose: Compare 4 BHAPC model specifications using splines for age,
# with different combinations of random effects vs fixed splines
# for period and cohort.
#
# Models:
#   M1 (Base):          ns(age,6) + (1|period) + (1|cohort)
#   M2 (Period Smooth): ns(age,6) + ns(period_num,5) + (1|cohort)
#   M3 (Cohort Smooth): ns(age,6) + (1|period) + ns(cohort_num,10)
#   M4 (Both Smooth):   ns(age,6) + ns(period_num,5) + ns(cohort_num,10)
#
# Usage:
#   Rscript R/scripts/19_bhapc_spline_sensitivity.R 2>&1 | tee output/bhapc_spline_sensitivity/run.log
#
# ==============================================================================

# ==============================================================================
# Setup and Configuration
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BHAPC SPLINE SENSITIVITY ANALYSIS\n")
cat("Started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(patchwork)
  library(gridExtra)
  library(grid)
  library(splines)  # For ns()
  library(loo)      # For LOOIC comparison
})

# Source paths and functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Override DATA_DEPOT if not set
if (Sys.getenv("DATA_DEPOT") == "") {
  Sys.setenv(DATA_DEPOT = "/home/ubuntu/data_depot")
}

# Create output directories
output_dir <- here("output", "bhapc_spline_sensitivity")
model_dir <- file.path(output_dir, "models")
table_dir <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

# Track total runtime
script_start_time <- Sys.time()

# ==============================================================================
# Configuration
# ==============================================================================

N_CORES <- parallel::detectCores()
cat("Detected cores:", N_CORES, "\n")

# GSS-specific configuration
CONFIG <- list(
  srh_scale = 4,        # GSS uses 4-point scale
  age_min = 18,
  age_max = 89,
  # Model 1 settings (already fitted in script 17)
  iter_m1 = 6000,
  adapt_delta_m1 = 0.998,
  # Model 2, 3 settings (have random effects)
  iter_lmer = 6000,
  adapt_delta_lmer = 0.998,
  # Model 4 settings (no random effects, faster)
  iter_glm = 4000,
  adapt_delta_glm = 0.95,
  # Spline degrees of freedom
  age_df = 6,
  period_df = 5,
  cohort_df = 10
)

cat("Configuration:\n")
cat("  SRH scale: 1-", CONFIG$srh_scale, "\n")
cat("  Age range: ", CONFIG$age_min, "-", CONFIG$age_max, "\n")
cat("  Age spline df: ", CONFIG$age_df, "\n")
cat("  Period spline df: ", CONFIG$period_df, "\n")
cat("  Cohort spline df: ", CONFIG$cohort_df, "\n\n")

# ==============================================================================
# Step 1: Load and Prepare Data
# ==============================================================================

cat("--- STEP 1: DATA LOADING & PREPARATION ---\n")

# Load GSS data
data_path <- file.path(
  Sys.getenv("DATA_DEPOT"),
  "_derived", "srh_project", "essential_datasets",
  "data_essential_gss.rds"
)

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}

df <- readRDS(data_path)
df <- df %>% drop_na(srh, age, year, wt)
original_n <- nrow(df)

cat("  Loaded GSS data: ", format(original_n, big.mark = ","), " rows\n")

# Prepare BHAPC data
bhapc_df <- prepare_bhapc_data(
  df,
  survey = "gss",
  age_min = CONFIG$age_min,
  age_max = CONFIG$age_max,
  srh_scale = CONFIG$srh_scale
)

# Add numeric versions of period/cohort for spline terms
bhapc_df <- bhapc_df %>%
  mutate(
    period_num = as.numeric(period_4yr),
    cohort_num = as.numeric(cohort_4yr)
  )

cat("  BHAPC N: ", format(nrow(bhapc_df), big.mark = ","), "\n")
cat("  Periods: ", paste(sort(unique(bhapc_df$period_num)), collapse = ", "), "\n")
cat("  Period range for spline: ", min(bhapc_df$period_num), "-", max(bhapc_df$period_num), "\n")
cat("  Cohorts: ", length(unique(bhapc_df$cohort_num)), "\n")
cat("  Cohort range for spline: ", min(bhapc_df$cohort_num), "-", max(bhapc_df$cohort_num), "\n")

# Save prepared data
saveRDS(bhapc_df, file.path(output_dir, "gss_spline_sensitivity_data.rds"))

# ==============================================================================
# Step 2: Load Model 1 (Base Spline) from Script 17
# ==============================================================================

cat("\n--- STEP 2: LOADING MODEL 1 (BASE SPLINE) ---\n")

model1_path <- here("output", "bhapc_gss_spline", "gss_spline_main_model.rds")

if (file.exists(model1_path)) {
  cat("  Loading from: ", model1_path, "\n")
  model_1 <- readRDS(model1_path)
  cat("  Model 1 loaded successfully\n")
} else {
  # Fit Model 1 if not available
  cat("  Model 1 not found. Fitting Model 1 (Base Spline)...\n")
  cat("  Formula: srh ~ ns(age, df=", CONFIG$age_df, ") + lnWt + (1|period_4yr) + (1|cohort_4yr)\n")

  t1_start <- Sys.time()

  model_1 <- stan_lmer(
    srh ~ ns(age, df = CONFIG$age_df) + lnWt + (1|period_4yr) + (1|cohort_4yr),
    data = bhapc_df,
    prior = normal(0, 0.5, autoscale = TRUE),
    adapt_delta = CONFIG$adapt_delta_m1,
    iter = CONFIG$iter_m1,
    chains = 4,
    cores = N_CORES,
    seed = 20260205
  )

  t1_elapsed <- difftime(Sys.time(), t1_start, units = "mins")
  cat("  Model 1 completed in ", round(t1_elapsed, 1), " minutes\n")
}

saveRDS(model_1, file.path(model_dir, "model_1_base.rds"))

# Extract Model 1 diagnostics
diag_1 <- extract_bhapc_diagnostics(model_1)
var_1 <- extract_variance_components(model_1)
re_1 <- extract_random_effects(model_1, bhapc_df)

cat("  Model 1 variance decomposition:\n")
for (i in 1:nrow(var_1)) {
  if (var_1$component[i] != "Total") {
    cat("    ", var_1$component[i], ": ", round(var_1$pct_of_total[i], 1), "%\n")
  }
}

# ==============================================================================
# Step 3: Fit Model 2 (Period Smooth)
# ==============================================================================

cat("\n--- STEP 3: FITTING MODEL 2 (PERIOD SMOOTH) ---\n")

model2_path <- file.path(model_dir, "model_2_period_smooth.rds")
if (file.exists(model2_path)) {
  cat("  Loading from: ", model2_path, "\n")
  model_2 <- readRDS(model2_path)
  cat("  Model 2 loaded successfully\n")
} else {
  cat("  Formula: srh ~ ns(age, df=", CONFIG$age_df, ") + ns(period_num, df=", CONFIG$period_df, ") + lnWt + (1|cohort_4yr)\n")
  cat("  iter = ", CONFIG$iter_lmer, ", adapt_delta = ", CONFIG$adapt_delta_lmer, "\n")

  t2_start <- Sys.time()

  model_2 <- stan_lmer(
    srh ~ ns(age, df = CONFIG$age_df) + ns(period_num, df = CONFIG$period_df) + lnWt + (1|cohort_4yr),
    data = bhapc_df,
    prior = normal(0, 0.5, autoscale = TRUE),
    adapt_delta = CONFIG$adapt_delta_lmer,
    iter = CONFIG$iter_lmer,
    chains = 4,
    cores = N_CORES,
    seed = 20260205
  )

  t2_elapsed <- difftime(Sys.time(), t2_start, units = "mins")
  cat("  Model 2 completed in ", round(t2_elapsed, 1), " minutes\n")

  saveRDS(model_2, model2_path)
}

# Extract Model 2 results
diag_2 <- extract_bhapc_diagnostics(model_2)
var_2 <- extract_variance_components(model_2)
re_2 <- extract_random_effects(model_2, bhapc_df)

cat("  Model 2 variance decomposition:\n")
for (i in 1:nrow(var_2)) {
  if (var_2$component[i] != "Total") {
    cat("    ", var_2$component[i], ": ", round(var_2$pct_of_total[i], 1), "%\n")
  }
}

# ==============================================================================
# Step 4: Fit Model 3 (Cohort Smooth)
# ==============================================================================

cat("\n--- STEP 4: FITTING MODEL 3 (COHORT SMOOTH) ---\n")

model3_path <- file.path(model_dir, "model_3_cohort_smooth.rds")
if (file.exists(model3_path)) {
  cat("  Loading from: ", model3_path, "\n")
  model_3 <- readRDS(model3_path)
  cat("  Model 3 loaded successfully\n")
} else {
  cat("  Formula: srh ~ ns(age, df=", CONFIG$age_df, ") + lnWt + (1|period_4yr) + ns(cohort_num, df=", CONFIG$cohort_df, ")\n")
  cat("  iter = ", CONFIG$iter_lmer, ", adapt_delta = ", CONFIG$adapt_delta_lmer, "\n")

  t3_start <- Sys.time()

  model_3 <- stan_lmer(
    srh ~ ns(age, df = CONFIG$age_df) + lnWt + (1|period_4yr) + ns(cohort_num, df = CONFIG$cohort_df),
    data = bhapc_df,
    prior = normal(0, 0.5, autoscale = TRUE),
    adapt_delta = CONFIG$adapt_delta_lmer,
    iter = CONFIG$iter_lmer,
    chains = 4,
    cores = N_CORES,
    seed = 20260205
  )

  t3_elapsed <- difftime(Sys.time(), t3_start, units = "mins")
  cat("  Model 3 completed in ", round(t3_elapsed, 1), " minutes\n")

  saveRDS(model_3, model3_path)
}

# Extract Model 3 results
diag_3 <- extract_bhapc_diagnostics(model_3)
var_3 <- extract_variance_components(model_3)
re_3 <- extract_random_effects(model_3, bhapc_df)

cat("  Model 3 variance decomposition:\n")
for (i in 1:nrow(var_3)) {
  if (var_3$component[i] != "Total") {
    cat("    ", var_3$component[i], ": ", round(var_3$pct_of_total[i], 1), "%\n")
  }
}

# ==============================================================================
# Step 5: Fit Model 4 (Both Smooth)
# ==============================================================================

cat("\n--- STEP 5: FITTING MODEL 4 (BOTH SMOOTH) ---\n")

model4_path <- file.path(model_dir, "model_4_both_smooth.rds")
if (file.exists(model4_path)) {
  cat("  Loading from: ", model4_path, "\n")
  model_4 <- readRDS(model4_path)
  cat("  Model 4 loaded successfully\n")
} else {
  cat("  Formula: srh ~ ns(age, df=", CONFIG$age_df, ") + ns(period_num, df=", CONFIG$period_df, ") + ns(cohort_num, df=", CONFIG$cohort_df, ") + lnWt\n")
  cat("  NOTE: No random effects - using stan_glm\n")
  cat("  iter = ", CONFIG$iter_glm, ", adapt_delta = ", CONFIG$adapt_delta_glm, "\n")

  t4_start <- Sys.time()

  model_4 <- stan_glm(
    srh ~ ns(age, df = CONFIG$age_df) + ns(period_num, df = CONFIG$period_df) + ns(cohort_num, df = CONFIG$cohort_df) + lnWt,
    data = bhapc_df,
    family = gaussian(),
    prior = normal(0, 0.5, autoscale = TRUE),
    adapt_delta = CONFIG$adapt_delta_glm,
    iter = CONFIG$iter_glm,
    chains = 4,
    cores = N_CORES,
    seed = 20260205
  )

  t4_elapsed <- difftime(Sys.time(), t4_start, units = "mins")
  cat("  Model 4 completed in ", round(t4_elapsed, 1), " minutes\n")

  saveRDS(model_4, model4_path)
}

# Extract Model 4 diagnostics
diag_4 <- extract_bhapc_diagnostics(model_4)
# Note: Model 4 has no variance decomposition (no random effects)
sigma_4 <- sigma(model_4)
cat("  Model 4 residual SD: ", round(sigma_4, 4), "\n")
cat("  NOTE: Model 4 has no variance decomposition (all fixed effects)\n")

# ==============================================================================
# Step 6: Convergence Diagnostics
# ==============================================================================

cat("\n--- STEP 6: CONVERGENCE DIAGNOSTICS ---\n")

check_convergence <- function(diag_df, label) {
  max_rhat <- max(diag_df$Rhat, na.rm = TRUE)
  min_neff <- min(diag_df$n_eff, na.rm = TRUE)
  converged <- max_rhat < 1.01 && min_neff >= 400
  cat(sprintf("  %s: max Rhat = %.3f, min n_eff = %.0f — %s\n",
              label, max_rhat, min_neff,
              ifelse(converged, "CONVERGED", "WARNING")))
  tibble(
    model = label,
    max_rhat = max_rhat,
    min_neff = min_neff,
    converged = converged
  )
}

convergence_df <- bind_rows(
  check_convergence(diag_1, "M1: Base"),
  check_convergence(diag_2, "M2: Period Smooth"),
  check_convergence(diag_3, "M3: Cohort Smooth"),
  check_convergence(diag_4, "M4: Both Smooth")
)

write.csv(convergence_df, file.path(table_dir, "convergence_diagnostics.csv"), row.names = FALSE)

# ==============================================================================
# Step 7: Model Comparison using LOOIC
# ==============================================================================

cat("\n--- STEP 7: MODEL COMPARISON (LOOIC) ---\n")

compute_loo_safe <- function(model, label) {
  cat("  Computing LOO for ", label, "...\n")
  tryCatch({
    l <- loo(model)
    tibble(
      model = label,
      elpd_loo = l$estimates["elpd_loo", "Estimate"],
      se_elpd_loo = l$estimates["elpd_loo", "SE"],
      looic = -2 * l$estimates["elpd_loo", "Estimate"],
      p_loo = l$estimates["p_loo", "Estimate"],
      n_bad_k = sum(l$diagnostics$pareto_k > 0.7)
    )
  }, error = function(e) {
    warning("LOO failed for ", label, ": ", e$message)
    tibble(
      model = label,
      elpd_loo = NA_real_,
      se_elpd_loo = NA_real_,
      looic = NA_real_,
      p_loo = NA_real_,
      n_bad_k = NA_integer_
    )
  })
}

loo_comparison <- bind_rows(
  compute_loo_safe(model_1, "M1: Base"),
  compute_loo_safe(model_2, "M2: Period Smooth"),
  compute_loo_safe(model_3, "M3: Cohort Smooth"),
  compute_loo_safe(model_4, "M4: Both Smooth")
)

# Add delta LOOIC relative to best model
best_elpd <- max(loo_comparison$elpd_loo, na.rm = TRUE)
loo_comparison <- loo_comparison %>%
  mutate(
    delta_elpd = elpd_loo - best_elpd,
    delta_looic = -2 * delta_elpd
  ) %>%
  arrange(delta_looic)

cat("\n  LOOIC Comparison:\n")
print(as.data.frame(loo_comparison %>% select(model, elpd_loo, se_elpd_loo, looic, delta_looic, p_loo)), row.names = FALSE)

write.csv(loo_comparison, file.path(table_dir, "model_comparison_looic.csv"), row.names = FALSE)

# ==============================================================================
# Step 8: Extract Spline Effects
# ==============================================================================

cat("\n--- STEP 8: EXTRACTING SPLINE EFFECTS ---\n")

#' Extract spline effect via posterior_linpred
#'
#' Computes the effect of a spline term by varying one variable while holding
#' others at reference values.
#'
#' @param model Fitted rstanarm model
#' @param data Data frame used to fit model
#' @param var_name Name of variable to vary
#' @param n_points Number of points for effect curve
#' @return Data frame with effect estimates and CIs
extract_spline_effect <- function(model, data, var_name, n_points = 50) {

  # Create grid for the variable of interest
  var_range <- range(data[[var_name]], na.rm = TRUE)
  var_grid <- seq(var_range[1], var_range[2], length.out = n_points)

  # Create prediction data frame with median/mode values for other variables
  pred_df <- tibble(
    !!var_name := var_grid,
    age = median(data$age),
    lnWt = median(data$lnWt)
  )

  # Add other variables depending on what's in the model
  if ("period_num" %in% names(data) && var_name != "period_num") {
    if (inherits(model, "stanreg") && !is.null(model$call$formula)) {
      formula_str <- paste(deparse(model$call$formula), collapse = " ")
      if (grepl("ns\\(period_num", formula_str)) {
        pred_df$period_num <- median(data$period_num)
      }
    }
  }

  if ("cohort_num" %in% names(data) && var_name != "cohort_num") {
    if (inherits(model, "stanreg") && !is.null(model$call$formula)) {
      formula_str <- paste(deparse(model$call$formula), collapse = " ")
      if (grepl("ns\\(cohort_num", formula_str)) {
        pred_df$cohort_num <- median(data$cohort_num)
      }
    }
  }

  # Add random effect grouping factors (mode)
  if ("period_4yr" %in% names(model$glmod$reTrms$cnms) || "period_4yr" %in% names(data)) {
    # Use most common period
    pred_df$period_4yr <- names(which.max(table(data$period_4yr)))
  }
  if ("cohort_4yr" %in% names(model$glmod$reTrms$cnms) || "cohort_4yr" %in% names(data)) {
    # Use most common cohort
    pred_df$cohort_4yr <- names(which.max(table(data$cohort_4yr)))
  }

  # Get posterior predictions
  posterior_pred <- posterior_linpred(model, newdata = pred_df)

  # Compute mean and CIs
  effect_df <- tibble(
    !!var_name := var_grid,
    estimate = colMeans(posterior_pred),
    ci_lower_90 = apply(posterior_pred, 2, quantile, 0.05),
    ci_upper_90 = apply(posterior_pred, 2, quantile, 0.95)
  )

  # Center at median value for interpretability
  median_idx <- which.min(abs(var_grid - median(var_grid)))
  center_val <- effect_df$estimate[median_idx]

  effect_df <- effect_df %>%
    mutate(
      estimate_centered = estimate - center_val,
      ci_lower_centered = ci_lower_90 - center_val,
      ci_upper_centered = ci_upper_90 - center_val
    )

  effect_df
}

# --- Extract Age Effects for all 4 models ---
cat("  Extracting age effects...\n")

ages_grid <- seq(CONFIG$age_min, CONFIG$age_max, by = 1)

extract_age_effect_spline <- function(model, data, label) {
  pred_df <- tibble(
    age = ages_grid,
    lnWt = median(data$lnWt)
  )

  # Add reference values for other terms based on model type
  formula_str <- paste(deparse(model$call$formula), collapse = " ")

  if (grepl("ns\\(period_num", formula_str)) {
    pred_df$period_num <- median(data$period_num)
  }
  if (grepl("ns\\(cohort_num", formula_str)) {
    pred_df$cohort_num <- median(data$cohort_num)
  }

  # Add random effect levels if needed
  if (grepl("period_4yr", formula_str) && grepl("\\|", formula_str)) {
    pred_df$period_4yr <- names(which.max(table(data$period_4yr)))
  }
  if (grepl("cohort_4yr", formula_str) && grepl("\\|", formula_str)) {
    pred_df$cohort_4yr <- names(which.max(table(data$cohort_4yr)))
  }

  posterior_pred <- posterior_linpred(model, newdata = pred_df)

  tibble(
    age = ages_grid,
    estimate = colMeans(posterior_pred),
    ci_lower_90 = apply(posterior_pred, 2, quantile, 0.05),
    ci_upper_90 = apply(posterior_pred, 2, quantile, 0.95),
    model = label
  ) %>%
    # Center at minimum age
    mutate(
      min_effect = estimate[1],
      estimate_centered = estimate - min_effect,
      ci_lower_centered = ci_lower_90 - ci_lower_90[1],
      ci_upper_centered = ci_upper_90 - ci_upper_90[1]
    ) %>%
    select(-min_effect)
}

age_effects <- bind_rows(
  extract_age_effect_spline(model_1, bhapc_df, "M1: Base"),
  extract_age_effect_spline(model_2, bhapc_df, "M2: Period Smooth"),
  extract_age_effect_spline(model_3, bhapc_df, "M3: Cohort Smooth"),
  extract_age_effect_spline(model_4, bhapc_df, "M4: Both Smooth")
)

write.csv(age_effects, file.path(table_dir, "age_effects_comparison.csv"), row.names = FALSE)

# --- Extract Period Effects ---
cat("  Extracting period effects...\n")

# Models 1 and 3 have period random effects
period_re_1 <- re_1$period_effects %>% mutate(model = "M1: Base", type = "random")
period_re_3 <- re_3$period_effects %>% mutate(model = "M3: Cohort Smooth", type = "random")

# Models 2 and 4 have period splines - extract via prediction
extract_period_spline_effect <- function(model, data, label) {
  period_grid <- seq(min(data$period_num), max(data$period_num), length.out = 50)

  pred_df <- tibble(
    period_num = period_grid,
    age = median(data$age),
    lnWt = median(data$lnWt)
  )

  formula_str <- paste(deparse(model$call$formula), collapse = " ")

  if (grepl("ns\\(cohort_num", formula_str)) {
    pred_df$cohort_num <- median(data$cohort_num)
  }
  if (grepl("cohort_4yr", formula_str) && grepl("\\|", formula_str)) {
    pred_df$cohort_4yr <- names(which.max(table(data$cohort_4yr)))
  }

  posterior_pred <- posterior_linpred(model, newdata = pred_df)

  # Center at median period
  median_idx <- which.min(abs(period_grid - median(period_grid)))
  center_val <- colMeans(posterior_pred)[median_idx]

  tibble(
    period = period_grid,
    estimate = colMeans(posterior_pred) - center_val,
    ci_lower_90 = apply(posterior_pred, 2, quantile, 0.05) - center_val,
    ci_upper_90 = apply(posterior_pred, 2, quantile, 0.95) - center_val,
    model = label,
    type = "spline"
  )
}

period_spline_2 <- extract_period_spline_effect(model_2, bhapc_df, "M2: Period Smooth")
period_spline_4 <- extract_period_spline_effect(model_4, bhapc_df, "M4: Both Smooth")

period_effects_all <- bind_rows(
  period_re_1,
  period_re_3,
  period_spline_2,
  period_spline_4
)

write.csv(period_effects_all, file.path(table_dir, "period_effects_comparison.csv"), row.names = FALSE)

# --- Extract Cohort Effects ---
cat("  Extracting cohort effects...\n")

# Models 1 and 2 have cohort random effects
cohort_re_1 <- re_1$cohort_effects %>% mutate(model = "M1: Base", type = "random")
cohort_re_2 <- re_2$cohort_effects %>% mutate(model = "M2: Period Smooth", type = "random")

# Models 3 and 4 have cohort splines - extract via prediction
extract_cohort_spline_effect <- function(model, data, label) {
  cohort_grid <- seq(min(data$cohort_num), max(data$cohort_num), length.out = 50)

  pred_df <- tibble(
    cohort_num = cohort_grid,
    age = median(data$age),
    lnWt = median(data$lnWt)
  )

  formula_str <- paste(deparse(model$call$formula), collapse = " ")

  if (grepl("ns\\(period_num", formula_str)) {
    pred_df$period_num <- median(data$period_num)
  }
  if (grepl("period_4yr", formula_str) && grepl("\\|", formula_str)) {
    pred_df$period_4yr <- names(which.max(table(data$period_4yr)))
  }

  posterior_pred <- posterior_linpred(model, newdata = pred_df)

  # Center at median cohort
  median_idx <- which.min(abs(cohort_grid - median(cohort_grid)))
  center_val <- colMeans(posterior_pred)[median_idx]

  tibble(
    cohort = cohort_grid,
    estimate = colMeans(posterior_pred) - center_val,
    ci_lower_90 = apply(posterior_pred, 2, quantile, 0.05) - center_val,
    ci_upper_90 = apply(posterior_pred, 2, quantile, 0.95) - center_val,
    model = label,
    type = "spline"
  )
}

cohort_spline_3 <- extract_cohort_spline_effect(model_3, bhapc_df, "M3: Cohort Smooth")
cohort_spline_4 <- extract_cohort_spline_effect(model_4, bhapc_df, "M4: Both Smooth")

cohort_effects_all <- bind_rows(
  cohort_re_1,
  cohort_re_2,
  cohort_spline_3,
  cohort_spline_4
)

write.csv(cohort_effects_all, file.path(table_dir, "cohort_effects_comparison.csv"), row.names = FALSE)

# ==============================================================================
# Step 9: Generate Comparison Figures
# ==============================================================================

cat("\n--- STEP 9: GENERATING COMPARISON FIGURES ---\n")

# Color palette for models
model_colors <- c(
  "M1: Base" = "#0072B2",
  "M2: Period Smooth" = "#E69F00",
  "M3: Cohort Smooth" = "#009E73",
  "M4: Both Smooth" = "#CC79A7"
)

# --- Figure A: Variance Decomposition Comparison ---
cat("  Creating variance comparison figure...\n")

variance_comparison <- bind_rows(
  var_1 %>% filter(component %in% c("period_4yr", "cohort_4yr", "Residual")) %>% mutate(model = "M1: Base"),
  var_2 %>% filter(component %in% c("cohort_4yr", "Residual")) %>% mutate(model = "M2: Period Smooth"),
  var_3 %>% filter(component %in% c("period_4yr", "Residual")) %>% mutate(model = "M3: Cohort Smooth")
) %>%
  mutate(
    component = case_when(
      component == "period_4yr" ~ "Period",
      component == "cohort_4yr" ~ "Cohort",
      TRUE ~ "Residual"
    ),
    component = factor(component, levels = c("Period", "Cohort", "Residual"))
  )

write.csv(variance_comparison, file.path(table_dir, "variance_decomposition_comparison.csv"), row.names = FALSE)

p_variance <- ggplot(variance_comparison, aes(x = component, y = pct_of_total, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct_of_total)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  scale_fill_manual(values = model_colors[1:3]) +
  labs(
    title = "A. Variance Decomposition by Model",
    subtitle = "M4 excluded (no random effects)",
    x = NULL,
    y = "% of Total Variance",
    fill = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.title = element_text(face = "bold", size = 13)
  ) +
  ylim(0, max(variance_comparison$pct_of_total, na.rm = TRUE) * 1.2)

ggsave(file.path(figure_dir, "variance_comparison.png"), p_variance, width = 8, height = 7, dpi = 300)

# --- Figure B: Age Effects Comparison ---
cat("  Creating age effects comparison figure...\n")

p_age <- ggplot(age_effects, aes(x = age, y = estimate_centered, color = model, fill = model)) +
  geom_ribbon(aes(ymin = ci_lower_centered, ymax = ci_upper_centered),
              alpha = 0.1, color = NA) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "B. Age Effects (All 4 Models)",
    subtitle = "Sanity check: should be similar across models",
    x = "Age (years)",
    y = "Effect on SRH (relative to age 18)",
    color = "Model",
    fill = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

ggsave(file.path(figure_dir, "age_effects_comparison.png"), p_age, width = 10, height = 7, dpi = 300)

# --- Figure C: Period Effects Comparison ---
cat("  Creating period effects comparison figure...\n")

p_period <- ggplot() +
  # Random effects (points with error bars)
  geom_errorbar(
    data = period_effects_all %>% filter(type == "random"),
    aes(x = period, ymin = ci_lower_90, ymax = ci_upper_90, color = model),
    width = 0.8, linewidth = 0.8
  ) +
  geom_point(
    data = period_effects_all %>% filter(type == "random"),
    aes(x = period, y = estimate, color = model),
    size = 3
  ) +
  # Spline effects (lines with ribbons)
  geom_ribbon(
    data = period_effects_all %>% filter(type == "spline"),
    aes(x = period, ymin = ci_lower_90, ymax = ci_upper_90, fill = model),
    alpha = 0.2
  ) +
  geom_line(
    data = period_effects_all %>% filter(type == "spline"),
    aes(x = period, y = estimate, color = model),
    linewidth = 1.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "C. Period Effects Comparison",
    subtitle = "Random effects (points) vs splines (lines)",
    x = "Period (year)",
    y = "Period effect (SRH units)",
    color = "Model",
    fill = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

ggsave(file.path(figure_dir, "period_effects_comparison.png"), p_period, width = 10, height = 7, dpi = 300)

# --- Figure D: Cohort Effects Comparison ---
cat("  Creating cohort effects comparison figure...\n")

p_cohort <- ggplot() +
  # Random effects (points with error bars)
  geom_errorbar(
    data = cohort_effects_all %>% filter(type == "random"),
    aes(x = cohort, ymin = ci_lower_90, ymax = ci_upper_90, color = model),
    width = 2, linewidth = 0.6, alpha = 0.7,
    position = position_dodge(width = 3)
  ) +
  geom_point(
    data = cohort_effects_all %>% filter(type == "random"),
    aes(x = cohort, y = estimate, color = model),
    size = 2,
    position = position_dodge(width = 3)
  ) +
  # Spline effects (lines with ribbons)
  geom_ribbon(
    data = cohort_effects_all %>% filter(type == "spline"),
    aes(x = cohort, ymin = ci_lower_90, ymax = ci_upper_90, fill = model),
    alpha = 0.2
  ) +
  geom_line(
    data = cohort_effects_all %>% filter(type == "spline"),
    aes(x = cohort, y = estimate, color = model),
    linewidth = 1.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_x_continuous(breaks = seq(1920, 2000, 20)) +
  labs(
    title = "D. Cohort Effects Comparison",
    subtitle = "Random effects (points) vs splines (lines)",
    x = "Birth cohort (year)",
    y = "Cohort effect (SRH units)",
    color = "Model",
    fill = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

ggsave(file.path(figure_dir, "cohort_effects_comparison.png"), p_cohort, width = 10, height = 7, dpi = 300)

# ==============================================================================
# Step 10: Create Summary One-Pager
# ==============================================================================

cat("\n--- STEP 10: CREATING SUMMARY ONE-PAGER ---\n")

# LOOIC comparison table as grob
looic_table <- loo_comparison %>%
  select(model, elpd_loo, se_elpd_loo, delta_looic, p_loo) %>%
  mutate(
    elpd_loo = sprintf("%.1f", elpd_loo),
    se_elpd_loo = sprintf("%.1f", se_elpd_loo),
    delta_looic = sprintf("%.1f", delta_looic),
    p_loo = sprintf("%.1f", p_loo)
  ) %>%
  rename(
    Model = model,
    `elpd_loo` = elpd_loo,
    `SE` = se_elpd_loo,
    `Δ LOOIC` = delta_looic,
    `p_loo` = p_loo
  )

t_looic <- tableGrob(looic_table, rows = NULL,
                     theme = ttheme_minimal(
                       base_size = 9,
                       core = list(fg_params = list(hjust = 0, x = 0.05),
                                   bg_params = list(fill = "#e8e8e8")),
                       colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold"),
                                      bg_params = list(fill = "#d0d0d0"))
                     ))

wrap_table <- function(grob, title) {
  ggplot() +
    annotation_custom(grob) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0))
}

p_t_looic <- wrap_table(t_looic, "LOOIC Comparison")

# Combine all panels
summary_layout <- "
AABBBCCC
AABBBCCC
DDDDEEE
DDDDEEE
"

summary_fig <- p_t_looic + p_variance + p_age + p_period + p_cohort +
  plot_layout(design = summary_layout) +
  plot_annotation(
    title = "BHAPC Spline Sensitivity Analysis (GSS)",
    subtitle = paste0(
      "Comparing 4 model specifications: Random effects vs. splines for period and cohort\n",
      "N = ", format(nrow(bhapc_df), big.mark = ","), " observations"
    ),
    caption = paste0(
      "Models: M1=ns(age,6)+(1|period)+(1|cohort), M2=ns(age,6)+ns(period,5)+(1|cohort), ",
      "M3=ns(age,6)+(1|period)+ns(cohort,10), M4=ns(age,6)+ns(period,5)+ns(cohort,10)\n",
      "Error bars/ribbons show 90% CIs. Lower LOOIC = better fit."
    ),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

ggsave(file.path(figure_dir, "spline_sensitivity_summary.png"), summary_fig, width = 16, height = 12, dpi = 300)
ggsave(file.path(figure_dir, "spline_sensitivity_summary.pdf"), summary_fig, width = 16, height = 12)

cat("  Saved summary figure to: ", figure_dir, "\n")

# ==============================================================================
# Step 11: Interpretation
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("INTERPRETATION\n")
cat(strrep("=", 80), "\n\n")

# Find best model
best_model <- loo_comparison$model[1]
cat("Best-fitting model by LOOIC: ", best_model, "\n\n")

# Compare variance decomposition
cat("Variance Decomposition Comparison:\n")
var_wide <- variance_comparison %>%
  select(model, component, pct_of_total) %>%
  pivot_wider(names_from = model, values_from = pct_of_total)
print(as.data.frame(var_wide), row.names = FALSE)

# Check if age effects are similar (sanity check)
cat("\nAge Effect Sanity Check:\n")
age_at_50 <- age_effects %>%
  filter(age == 50) %>%
  select(model, estimate_centered)
cat("  Age effect at 50 (centered):\n")
print(as.data.frame(age_at_50), row.names = FALSE)

age_range <- age_effects %>%
  group_by(model) %>%
  summarise(
    effect_range = max(estimate_centered) - min(estimate_centered),
    .groups = "drop"
  )
cat("\n  Range of age effects:\n")
print(as.data.frame(age_range), row.names = FALSE)

# Overall conclusion
cat("\n", strrep("-", 40), "\n")
cat("CONCLUSION\n")
cat(strrep("-", 40), "\n")

looic_diff <- loo_comparison$delta_looic[2]  # Difference to second-best
if (abs(looic_diff) < 4) {
  cat("Models are comparable in fit (ΔLOOIC < 4).\n")
} else if (abs(looic_diff) < 10) {
  cat("Best model has moderately better fit (4 < ΔLOOIC < 10).\n")
} else {
  cat("Best model has substantially better fit (ΔLOOIC > 10).\n")
}

# ==============================================================================
# Final Summary
# ==============================================================================

total_elapsed <- difftime(Sys.time(), script_start_time, units = "mins")

cat("\n", strrep("=", 80), "\n")
cat("PIPELINE COMPLETE: BHAPC SPLINE SENSITIVITY ANALYSIS\n")
cat("Total time: ", round(total_elapsed, 1), " minutes\n")
cat("Finished at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

cat("Key Results:\n")
cat("  N observations: ", format(nrow(bhapc_df), big.mark = ","), "\n")
cat("  Best model: ", best_model, "\n")
cat("  All models converged: ", all(convergence_df$converged), "\n")

cat("\nOutputs saved to:\n")
cat("  Models: ", model_dir, "\n")
cat("  Tables: ", table_dir, "\n")
cat("  Figures: ", figure_dir, "\n")

cat("\nKey output files:\n")
cat("  - model_comparison_looic.csv\n")
cat("  - convergence_diagnostics.csv\n")
cat("  - variance_decomposition_comparison.csv\n")
cat("  - period_effects_comparison.csv\n")
cat("  - cohort_effects_comparison.csv\n")
cat("  - spline_sensitivity_summary.png/pdf\n")

cat("\n", strrep("=", 80), "\n")
