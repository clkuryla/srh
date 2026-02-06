# ==============================================================================
# 17_bhapc_nhis_meps_covariates.R
# BHAPC Covariate Sensitivity Analysis: NHIS and MEPS
#
# Purpose: Test how much of the cohort effect on SRH is explained by
#          demographics and psychological distress (K6) for NHIS and MEPS
#
# Models (per survey):
#   M1: Unadjusted (baseline BHAPC)
#   M2: + race + educ + sex (demographics)
#   M3: + K6 distress (mental health)
#   M4: + race + educ + sex + K6 (full model)
#
# Usage:
#   Rscript R/scripts/17_bhapc_nhis_meps_covariates.R 2>&1 | tee output/bhapc_nhis_meps_covariates/run.log
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("BHAPC COVARIATE ANALYSIS: NHIS & MEPS\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# 0. Setup
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(patchwork)
})

# Source shared functions
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))
source(here("R", "paths.R"))

set.seed(20260205)

# Output directory
output_dir <- here("output", "bhapc_nhis_meps_covariates")
dir.create(file.path(output_dir, "nhis"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "meps"), recursive = TRUE, showWarnings = FALSE)

# Stan settings
N_CORES     <- min(parallel::detectCores(), 10)
ITER        <- 6000
ADAPT_DELTA <- 0.998
SEED        <- 20260205

# Subsample size (for computational feasibility)
N_SUBSAMPLE <- 100000

cat("Configuration:\n")
cat("  Cores:", N_CORES, "\n")
cat("  Iterations:", ITER, "\n")
cat("  adapt_delta:", ADAPT_DELTA, "\n")
cat("  Subsample N:", format(N_SUBSAMPLE, big.mark = ","), "\n\n")

# ==============================================================================
# 1. Load and Prepare Data
# ==============================================================================

cat("--- STEP 1: LOADING AND PREPARING DATA ---\n\n")

# ------------------------------------------------------------------------------
# MEPS
# ------------------------------------------------------------------------------

cat("Loading MEPS...\n")

data_meps_raw <- read_rds(derived_path("data_meps.rds"))

cat("  Raw MEPS rows:", format(nrow(data_meps_raw), big.mark = ","), "\n")
cat("  Year range:", min(data_meps_raw$year), "-", max(data_meps_raw$year), "\n")

# Check column availability
meps_cols <- c("srh", "age", "year", "wt", "K6SUM", "race", "hispanic", "educ_4cat", "sex")
cat("  Required columns present:",
    all(meps_cols %in% names(data_meps_raw)), "\n")

data_meps <- data_meps_raw %>%
  drop_na(srh, age, year, wt) %>%
  mutate(
    # Rename K6SUM to k6 for consistency
    k6 = K6SUM,
    # Create education factor
    educ_f = factor(educ_4cat, levels = 1:4,
                    labels = c("LT HS", "HS", "Some college", "Bachelor+")),
    # Create race_includehisp_f from existing race + hispanic
    race_includehisp_f = case_when(
      hispanic == "Hispanic" ~ "Hispanic",
      race == "White" ~ "NH-White",
      race == "Black" ~ "NH-Black",
      race == "AIAN" ~ "NH-AIAN",
      race == "AAPI" ~ "NH-Asian/PI",
      TRUE ~ "Other"
    ) %>% factor(levels = c("NH-White", "NH-Black", "NH-AIAN",
                            "NH-Asian/PI", "Hispanic", "Other")),
    # Sex factor
    sex_f = factor(sex)
  )

cat("  After base filtering:", format(nrow(data_meps), big.mark = ","), "\n")

# Subsample for computational feasibility
if (nrow(data_meps) > N_SUBSAMPLE) {
  data_meps <- data_meps %>% slice_sample(n = N_SUBSAMPLE)
  cat("  After subsampling:", format(nrow(data_meps), big.mark = ","), "\n")
}

rm(data_meps_raw)
gc(verbose = FALSE)

# ------------------------------------------------------------------------------
# NHIS
# ------------------------------------------------------------------------------

cat("\nLoading NHIS...\n")

data_nhis_raw <- read_rds(derived_path("data_nhis.rds"))

cat("  Raw NHIS rows:", format(nrow(data_nhis_raw), big.mark = ","), "\n")
cat("  Year range:", min(data_nhis_raw$year), "-", max(data_nhis_raw$year), "\n")

# Check if race variables exist
if (!"race_includehisp_f" %in% names(data_nhis_raw)) {
  cat("\n  WARNING: race_includehisp_f not found in data_nhis.rds\n")
  cat("  Run R/scripts/16b_add_nhis_race.R first to add race variables.\n")
  cat("  Proceeding without NHIS race analysis...\n")
  nhis_has_race <- FALSE
} else {
  nhis_has_race <- TRUE
  cat("  race_includehisp_f present: TRUE\n")
}

# Filter to 1997+ for education availability
data_nhis <- data_nhis_raw %>%
  filter(year >= 1997) %>%
  drop_na(srh, age, year, wt) %>%
  mutate(
    # Education factor
    educ_f = factor(educ_4cat, levels = 1:4,
                    labels = c("LT HS", "HS", "Some college", "Bachelor+")),
    # Sex factor
    sex_f = factor(sex)
  )

if (nhis_has_race) {
  # Race factor should already exist from 16b script
  data_nhis <- data_nhis %>%
    mutate(race_f = race_includehisp_f)
}

cat("  After filtering (1997+):", format(nrow(data_nhis), big.mark = ","), "\n")

# Subsample
if (nrow(data_nhis) > N_SUBSAMPLE) {
  data_nhis <- data_nhis %>% slice_sample(n = N_SUBSAMPLE)
  cat("  After subsampling:", format(nrow(data_nhis), big.mark = ","), "\n")
}

rm(data_nhis_raw)
gc(verbose = FALSE)

# ==============================================================================
# 2. Missingness Report
# ==============================================================================

cat("\n--- STEP 2: MISSINGNESS REPORT ---\n\n")

# MEPS missingness
meps_missingness <- tibble(
  variable = c("srh", "age", "year", "wt", "race_includehisp_f", "educ_f", "sex_f", "k6"),
  n_total = nrow(data_meps),
  n_valid = sapply(
    c("srh", "age", "year", "wt", "race_includehisp_f", "educ_f", "sex_f", "k6"),
    function(v) sum(!is.na(data_meps[[v]]))
  ),
  n_missing = n_total - n_valid,
  pct_missing = round(n_missing / n_total * 100, 1)
)

cat("MEPS Missingness:\n")
print(as.data.frame(meps_missingness), row.names = FALSE)
write_csv(meps_missingness, file.path(output_dir, "meps", "missingness_report.csv"))

# NHIS missingness
nhis_miss_vars <- c("srh", "age", "year", "wt", "educ_f", "sex_f", "k6")
if (nhis_has_race) nhis_miss_vars <- c(nhis_miss_vars[1:4], "race_f", nhis_miss_vars[5:7])

nhis_missingness <- tibble(
  variable = nhis_miss_vars,
  n_total = nrow(data_nhis),
  n_valid = sapply(nhis_miss_vars, function(v) {
    if (v %in% names(data_nhis)) sum(!is.na(data_nhis[[v]])) else 0
  }),
  n_missing = n_total - n_valid,
  pct_missing = round(n_missing / n_total * 100, 1)
)

cat("\nNHIS Missingness:\n")
print(as.data.frame(nhis_missingness), row.names = FALSE)
write_csv(nhis_missingness, file.path(output_dir, "nhis", "missingness_report.csv"))

# ==============================================================================
# 3. Create Analysis Samples
# ==============================================================================

cat("\n--- STEP 3: CREATING ANALYSIS SAMPLES ---\n\n")

# ------------------------------------------------------------------------------
# MEPS samples
# ------------------------------------------------------------------------------

cat("MEPS samples:\n")

# M1: Base (srh, age, year, wt only)
meps_m1 <- data_meps %>%
  drop_na(srh, age, year, wt)
cat("  M1 (base):", format(nrow(meps_m1), big.mark = ","), "\n")

# M2: + demographics (race, educ, sex)
meps_m2 <- data_meps %>%
  drop_na(srh, age, year, wt, race_includehisp_f, educ_f, sex_f)
cat("  M2 (+ demographics):", format(nrow(meps_m2), big.mark = ","), "\n")

# M3: + K6 only
meps_m3 <- data_meps %>%
  drop_na(srh, age, year, wt, k6)
cat("  M3 (+ K6):", format(nrow(meps_m3), big.mark = ","), "\n")

# M4: Full model
meps_m4 <- data_meps %>%
  drop_na(srh, age, year, wt, race_includehisp_f, educ_f, sex_f, k6)
cat("  M4 (full):", format(nrow(meps_m4), big.mark = ","), "\n")

# ------------------------------------------------------------------------------
# NHIS samples
# ------------------------------------------------------------------------------

cat("\nNHIS samples:\n")

# M1: Base
nhis_m1 <- data_nhis %>%
  drop_na(srh, age, year, wt)
cat("  M1 (base):", format(nrow(nhis_m1), big.mark = ","), "\n")

# M2: + demographics (only if race available)
if (nhis_has_race) {
  nhis_m2 <- data_nhis %>%
    drop_na(srh, age, year, wt, race_f, educ_f, sex_f)
  cat("  M2 (+ demographics):", format(nrow(nhis_m2), big.mark = ","), "\n")
} else {
  nhis_m2 <- data_nhis %>%
    drop_na(srh, age, year, wt, educ_f, sex_f)
  cat("  M2 (+ educ + sex, no race):", format(nrow(nhis_m2), big.mark = ","), "\n")
}

# M3: + K6 only
nhis_m3 <- data_nhis %>%
  drop_na(srh, age, year, wt, k6)
cat("  M3 (+ K6):", format(nrow(nhis_m3), big.mark = ","), "\n")

# M4: Full model
if (nhis_has_race) {
  nhis_m4 <- data_nhis %>%
    drop_na(srh, age, year, wt, race_f, educ_f, sex_f, k6)
  cat("  M4 (full):", format(nrow(nhis_m4), big.mark = ","), "\n")
} else {
  nhis_m4 <- data_nhis %>%
    drop_na(srh, age, year, wt, educ_f, sex_f, k6)
  cat("  M4 (full, no race):", format(nrow(nhis_m4), big.mark = ","), "\n")
}

# ==============================================================================
# 4. Prepare BHAPC Data
# ==============================================================================

cat("\n--- STEP 4: PREPARING BHAPC DATA ---\n\n")

# MEPS
cat("Preparing MEPS BHAPC data...\n")
bhapc_meps_m1 <- prepare_bhapc_data(meps_m1, survey = "meps", srh_scale = 5)
bhapc_meps_m2 <- prepare_bhapc_data(meps_m2, survey = "meps", srh_scale = 5)
bhapc_meps_m3 <- prepare_bhapc_data(meps_m3, survey = "meps", srh_scale = 5)
bhapc_meps_m4 <- prepare_bhapc_data(meps_m4, survey = "meps", srh_scale = 5)

# NHIS
cat("\nPreparing NHIS BHAPC data...\n")
bhapc_nhis_m1 <- prepare_bhapc_data(nhis_m1, survey = "nhis", srh_scale = 5)
bhapc_nhis_m2 <- prepare_bhapc_data(nhis_m2, survey = "nhis", srh_scale = 5)
bhapc_nhis_m3 <- prepare_bhapc_data(nhis_m3, survey = "nhis", srh_scale = 5)
bhapc_nhis_m4 <- prepare_bhapc_data(nhis_m4, survey = "nhis", srh_scale = 5)

# Sample diagnostics
sample_diag_meps <- tibble(
  model = c("M1 (base)", "M2 (demog)", "M3 (K6)", "M4 (full)"),
  n = c(nrow(bhapc_meps_m1), nrow(bhapc_meps_m2), nrow(bhapc_meps_m3), nrow(bhapc_meps_m4)),
  mean_srh = c(mean(bhapc_meps_m1$srh), mean(bhapc_meps_m2$srh),
               mean(bhapc_meps_m3$srh), mean(bhapc_meps_m4$srh)),
  mean_age = c(mean(bhapc_meps_m1$age), mean(bhapc_meps_m2$age),
               mean(bhapc_meps_m3$age), mean(bhapc_meps_m4$age)),
  n_periods = c(length(unique(bhapc_meps_m1$period_4yr)), length(unique(bhapc_meps_m2$period_4yr)),
                length(unique(bhapc_meps_m3$period_4yr)), length(unique(bhapc_meps_m4$period_4yr))),
  n_cohorts = c(length(unique(bhapc_meps_m1$cohort_4yr)), length(unique(bhapc_meps_m2$cohort_4yr)),
                length(unique(bhapc_meps_m3$cohort_4yr)), length(unique(bhapc_meps_m4$cohort_4yr)))
)

sample_diag_nhis <- tibble(
  model = c("M1 (base)", "M2 (demog)", "M3 (K6)", "M4 (full)"),
  n = c(nrow(bhapc_nhis_m1), nrow(bhapc_nhis_m2), nrow(bhapc_nhis_m3), nrow(bhapc_nhis_m4)),
  mean_srh = c(mean(bhapc_nhis_m1$srh), mean(bhapc_nhis_m2$srh),
               mean(bhapc_nhis_m3$srh), mean(bhapc_nhis_m4$srh)),
  mean_age = c(mean(bhapc_nhis_m1$age), mean(bhapc_nhis_m2$age),
               mean(bhapc_nhis_m3$age), mean(bhapc_nhis_m4$age)),
  n_periods = c(length(unique(bhapc_nhis_m1$period_4yr)), length(unique(bhapc_nhis_m2$period_4yr)),
                length(unique(bhapc_nhis_m3$period_4yr)), length(unique(bhapc_nhis_m4$period_4yr))),
  n_cohorts = c(length(unique(bhapc_nhis_m1$cohort_4yr)), length(unique(bhapc_nhis_m2$cohort_4yr)),
                length(unique(bhapc_nhis_m3$cohort_4yr)), length(unique(bhapc_nhis_m4$cohort_4yr)))
)

cat("\nMEPS sample diagnostics:\n")
print(as.data.frame(sample_diag_meps), row.names = FALSE)
write_csv(sample_diag_meps, file.path(output_dir, "meps", "sample_diagnostics.csv"))

cat("\nNHIS sample diagnostics:\n")
print(as.data.frame(sample_diag_nhis), row.names = FALSE)
write_csv(sample_diag_nhis, file.path(output_dir, "nhis", "sample_diagnostics.csv"))

# Save BHAPC datasets
saveRDS(bhapc_meps_m1, file.path(output_dir, "meps", "bhapc_data_m1.rds"))
saveRDS(bhapc_meps_m2, file.path(output_dir, "meps", "bhapc_data_m2.rds"))
saveRDS(bhapc_meps_m3, file.path(output_dir, "meps", "bhapc_data_m3.rds"))
saveRDS(bhapc_meps_m4, file.path(output_dir, "meps", "bhapc_data_m4.rds"))

saveRDS(bhapc_nhis_m1, file.path(output_dir, "nhis", "bhapc_data_m1.rds"))
saveRDS(bhapc_nhis_m2, file.path(output_dir, "nhis", "bhapc_data_m2.rds"))
saveRDS(bhapc_nhis_m3, file.path(output_dir, "nhis", "bhapc_data_m3.rds"))
saveRDS(bhapc_nhis_m4, file.path(output_dir, "nhis", "bhapc_data_m4.rds"))

# ==============================================================================
# 5. Fit Models - MEPS
# ==============================================================================

cat("\n--- STEP 5: FITTING MEPS MODELS ---\n\n")

# Base formula
formula_m1 <- srh ~ age + scale(age_squared) + lnWt +
  (1 | period_4yr) + (1 | cohort_4yr)

# Demographics formula
formula_m2 <- srh ~ age + scale(age_squared) + lnWt +
  race_includehisp_f + educ_f + sex_f +
  (1 | period_4yr) + (1 | cohort_4yr)

# K6 formula
formula_m3 <- srh ~ age + scale(age_squared) + lnWt +
  k6 +
  (1 | period_4yr) + (1 | cohort_4yr)

# Full formula
formula_m4 <- srh ~ age + scale(age_squared) + lnWt +
  race_includehisp_f + educ_f + sex_f + k6 +
  (1 | period_4yr) + (1 | cohort_4yr)

# --- MEPS M1 ---
cat("MEPS M1 (base)...\n")
cat("  Formula:", deparse(formula_m1), "\n")
cat("  N:", format(nrow(bhapc_meps_m1), big.mark = ","), "\n")

t0 <- Sys.time()
model_meps_m1 <- stan_lmer(
  formula = formula_m1,
  data = bhapc_meps_m1,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = SEED
)
cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_meps_m1, file.path(output_dir, "meps", "model_m1.rds"))

# --- MEPS M2 ---
cat("\nMEPS M2 (demographics)...\n")
cat("  Formula:", deparse(formula_m2), "\n")
cat("  N:", format(nrow(bhapc_meps_m2), big.mark = ","), "\n")

t0 <- Sys.time()
model_meps_m2 <- stan_lmer(
  formula = formula_m2,
  data = bhapc_meps_m2,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = SEED
)
cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_meps_m2, file.path(output_dir, "meps", "model_m2.rds"))

# --- MEPS M3 ---
cat("\nMEPS M3 (K6)...\n")
cat("  Formula:", deparse(formula_m3), "\n")
cat("  N:", format(nrow(bhapc_meps_m3), big.mark = ","), "\n")

t0 <- Sys.time()
model_meps_m3 <- stan_lmer(
  formula = formula_m3,
  data = bhapc_meps_m3,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = SEED
)
cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_meps_m3, file.path(output_dir, "meps", "model_m3.rds"))

# --- MEPS M4 ---
cat("\nMEPS M4 (full)...\n")
cat("  Formula:", deparse(formula_m4), "\n")
cat("  N:", format(nrow(bhapc_meps_m4), big.mark = ","), "\n")

t0 <- Sys.time()
model_meps_m4 <- stan_lmer(
  formula = formula_m4,
  data = bhapc_meps_m4,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = SEED
)
cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_meps_m4, file.path(output_dir, "meps", "model_m4.rds"))

# ==============================================================================
# 6. Fit Models - NHIS
# ==============================================================================

cat("\n--- STEP 6: FITTING NHIS MODELS ---\n\n")

# Formulas for NHIS (race_f instead of race_includehisp_f)
if (nhis_has_race) {
  formula_nhis_m2 <- srh ~ age + scale(age_squared) + lnWt +
    race_f + educ_f + sex_f +
    (1 | period_4yr) + (1 | cohort_4yr)

  formula_nhis_m4 <- srh ~ age + scale(age_squared) + lnWt +
    race_f + educ_f + sex_f + k6 +
    (1 | period_4yr) + (1 | cohort_4yr)
} else {
  formula_nhis_m2 <- srh ~ age + scale(age_squared) + lnWt +
    educ_f + sex_f +
    (1 | period_4yr) + (1 | cohort_4yr)

  formula_nhis_m4 <- srh ~ age + scale(age_squared) + lnWt +
    educ_f + sex_f + k6 +
    (1 | period_4yr) + (1 | cohort_4yr)
}

# --- NHIS M1 ---
cat("NHIS M1 (base)...\n")
cat("  Formula:", deparse(formula_m1), "\n")
cat("  N:", format(nrow(bhapc_nhis_m1), big.mark = ","), "\n")

t0 <- Sys.time()
model_nhis_m1 <- stan_lmer(
  formula = formula_m1,
  data = bhapc_nhis_m1,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = SEED
)
cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_nhis_m1, file.path(output_dir, "nhis", "model_m1.rds"))

# --- NHIS M2 ---
cat("\nNHIS M2 (demographics)...\n")
cat("  Formula:", deparse(formula_nhis_m2), "\n")
cat("  N:", format(nrow(bhapc_nhis_m2), big.mark = ","), "\n")

t0 <- Sys.time()
model_nhis_m2 <- stan_lmer(
  formula = formula_nhis_m2,
  data = bhapc_nhis_m2,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = SEED
)
cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_nhis_m2, file.path(output_dir, "nhis", "model_m2.rds"))

# --- NHIS M3 ---
cat("\nNHIS M3 (K6)...\n")
cat("  Formula:", deparse(formula_m3), "\n")
cat("  N:", format(nrow(bhapc_nhis_m3), big.mark = ","), "\n")

t0 <- Sys.time()
model_nhis_m3 <- stan_lmer(
  formula = formula_m3,
  data = bhapc_nhis_m3,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = SEED
)
cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_nhis_m3, file.path(output_dir, "nhis", "model_m3.rds"))

# --- NHIS M4 ---
cat("\nNHIS M4 (full)...\n")
cat("  Formula:", deparse(formula_nhis_m4), "\n")
cat("  N:", format(nrow(bhapc_nhis_m4), big.mark = ","), "\n")

t0 <- Sys.time()
model_nhis_m4 <- stan_lmer(
  formula = formula_nhis_m4,
  data = bhapc_nhis_m4,
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = SEED
)
cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_nhis_m4, file.path(output_dir, "nhis", "model_m4.rds"))

# ==============================================================================
# 7. Extract Variance Components
# ==============================================================================

cat("\n--- STEP 7: EXTRACTING VARIANCE COMPONENTS ---\n\n")

# Helper function
get_pct <- function(var_df, comp) {
  var_df$pct_of_total[var_df$component == comp]
}

# MEPS variance
var_meps_m1 <- extract_variance_components(model_meps_m1)
var_meps_m2 <- extract_variance_components(model_meps_m2)
var_meps_m3 <- extract_variance_components(model_meps_m3)
var_meps_m4 <- extract_variance_components(model_meps_m4)

write_csv(var_meps_m1, file.path(output_dir, "meps", "variance_m1.csv"))
write_csv(var_meps_m2, file.path(output_dir, "meps", "variance_m2.csv"))
write_csv(var_meps_m3, file.path(output_dir, "meps", "variance_m3.csv"))
write_csv(var_meps_m4, file.path(output_dir, "meps", "variance_m4.csv"))

# NHIS variance
var_nhis_m1 <- extract_variance_components(model_nhis_m1)
var_nhis_m2 <- extract_variance_components(model_nhis_m2)
var_nhis_m3 <- extract_variance_components(model_nhis_m3)
var_nhis_m4 <- extract_variance_components(model_nhis_m4)

write_csv(var_nhis_m1, file.path(output_dir, "nhis", "variance_m1.csv"))
write_csv(var_nhis_m2, file.path(output_dir, "nhis", "variance_m2.csv"))
write_csv(var_nhis_m3, file.path(output_dir, "nhis", "variance_m3.csv"))
write_csv(var_nhis_m4, file.path(output_dir, "nhis", "variance_m4.csv"))

# Comparison tables
meps_comparison <- tibble(
  component = c("Cohort", "Period", "Residual"),
  M1_base = c(get_pct(var_meps_m1, "cohort_4yr"), get_pct(var_meps_m1, "period_4yr"),
              get_pct(var_meps_m1, "Residual")),
  M2_demog = c(get_pct(var_meps_m2, "cohort_4yr"), get_pct(var_meps_m2, "period_4yr"),
               get_pct(var_meps_m2, "Residual")),
  M3_k6 = c(get_pct(var_meps_m3, "cohort_4yr"), get_pct(var_meps_m3, "period_4yr"),
            get_pct(var_meps_m3, "Residual")),
  M4_full = c(get_pct(var_meps_m4, "cohort_4yr"), get_pct(var_meps_m4, "period_4yr"),
              get_pct(var_meps_m4, "Residual"))
) %>%
  bind_rows(tibble(
    component = "N",
    M1_base = nrow(bhapc_meps_m1),
    M2_demog = nrow(bhapc_meps_m2),
    M3_k6 = nrow(bhapc_meps_m3),
    M4_full = nrow(bhapc_meps_m4)
  ))

nhis_comparison <- tibble(
  component = c("Cohort", "Period", "Residual"),
  M1_base = c(get_pct(var_nhis_m1, "cohort_4yr"), get_pct(var_nhis_m1, "period_4yr"),
              get_pct(var_nhis_m1, "Residual")),
  M2_demog = c(get_pct(var_nhis_m2, "cohort_4yr"), get_pct(var_nhis_m2, "period_4yr"),
               get_pct(var_nhis_m2, "Residual")),
  M3_k6 = c(get_pct(var_nhis_m3, "cohort_4yr"), get_pct(var_nhis_m3, "period_4yr"),
            get_pct(var_nhis_m3, "Residual")),
  M4_full = c(get_pct(var_nhis_m4, "cohort_4yr"), get_pct(var_nhis_m4, "period_4yr"),
              get_pct(var_nhis_m4, "Residual"))
) %>%
  bind_rows(tibble(
    component = "N",
    M1_base = nrow(bhapc_nhis_m1),
    M2_demog = nrow(bhapc_nhis_m2),
    M3_k6 = nrow(bhapc_nhis_m3),
    M4_full = nrow(bhapc_nhis_m4)
  ))

cat("MEPS Variance Comparison (%):\n")
print(as.data.frame(meps_comparison), row.names = FALSE)
write_csv(meps_comparison, file.path(output_dir, "meps", "comparison_table.csv"))

cat("\nNHIS Variance Comparison (%):\n")
print(as.data.frame(nhis_comparison), row.names = FALSE)
write_csv(nhis_comparison, file.path(output_dir, "nhis", "comparison_table.csv"))

# ==============================================================================
# 8. Extract Fixed Effects
# ==============================================================================

cat("\n--- STEP 8: EXTRACTING FIXED EFFECTS ---\n\n")

# MEPS
fe_meps_m1 <- extract_fixed_effects(model_meps_m1)
fe_meps_m2 <- extract_fixed_effects(model_meps_m2)
fe_meps_m3 <- extract_fixed_effects(model_meps_m3)
fe_meps_m4 <- extract_fixed_effects(model_meps_m4)

write_csv(fe_meps_m1, file.path(output_dir, "meps", "fixed_effects_m1.csv"))
write_csv(fe_meps_m2, file.path(output_dir, "meps", "fixed_effects_m2.csv"))
write_csv(fe_meps_m3, file.path(output_dir, "meps", "fixed_effects_m3.csv"))
write_csv(fe_meps_m4, file.path(output_dir, "meps", "fixed_effects_m4.csv"))

# NHIS
fe_nhis_m1 <- extract_fixed_effects(model_nhis_m1)
fe_nhis_m2 <- extract_fixed_effects(model_nhis_m2)
fe_nhis_m3 <- extract_fixed_effects(model_nhis_m3)
fe_nhis_m4 <- extract_fixed_effects(model_nhis_m4)

write_csv(fe_nhis_m1, file.path(output_dir, "nhis", "fixed_effects_m1.csv"))
write_csv(fe_nhis_m2, file.path(output_dir, "nhis", "fixed_effects_m2.csv"))
write_csv(fe_nhis_m3, file.path(output_dir, "nhis", "fixed_effects_m3.csv"))
write_csv(fe_nhis_m4, file.path(output_dir, "nhis", "fixed_effects_m4.csv"))

cat("MEPS M4 (full) fixed effects:\n")
print(as.data.frame(fe_meps_m4), row.names = FALSE)

cat("\nNHIS M4 (full) fixed effects:\n")
print(as.data.frame(fe_nhis_m4), row.names = FALSE)

# ==============================================================================
# 9. Convergence Diagnostics
# ==============================================================================

cat("\n--- STEP 9: CONVERGENCE DIAGNOSTICS ---\n\n")

check_convergence <- function(model, label) {
  diag <- extract_bhapc_diagnostics(model)
  max_rhat <- max(diag$Rhat, na.rm = TRUE)
  min_neff <- min(diag$n_eff, na.rm = TRUE)
  converged <- max_rhat < 1.01 & min_neff >= 400
  cat(sprintf("  %-25s  Rhat_max=%.4f  n_eff_min=%5.0f  %s\n",
              label, max_rhat, min_neff,
              ifelse(converged, "OK", "*** CHECK ***")))
  tibble(model = label, max_rhat = max_rhat, min_neff = min_neff, converged = converged)
}

cat("MEPS:\n")
conv_meps <- bind_rows(
  check_convergence(model_meps_m1, "M1 (base)"),
  check_convergence(model_meps_m2, "M2 (demographics)"),
  check_convergence(model_meps_m3, "M3 (K6)"),
  check_convergence(model_meps_m4, "M4 (full)")
)
write_csv(conv_meps, file.path(output_dir, "meps", "convergence_diagnostics.csv"))

cat("\nNHIS:\n")
conv_nhis <- bind_rows(
  check_convergence(model_nhis_m1, "M1 (base)"),
  check_convergence(model_nhis_m2, "M2 (demographics)"),
  check_convergence(model_nhis_m3, "M3 (K6)"),
  check_convergence(model_nhis_m4, "M4 (full)")
)
write_csv(conv_nhis, file.path(output_dir, "nhis", "convergence_diagnostics.csv"))

# ==============================================================================
# 10. Extract Random Effects
# ==============================================================================

cat("\n--- STEP 10: EXTRACTING RANDOM EFFECTS ---\n\n")

# MEPS
re_meps_m1 <- extract_random_effects(model_meps_m1, bhapc_meps_m1)
re_meps_m2 <- extract_random_effects(model_meps_m2, bhapc_meps_m2)
re_meps_m3 <- extract_random_effects(model_meps_m3, bhapc_meps_m3)
re_meps_m4 <- extract_random_effects(model_meps_m4, bhapc_meps_m4)

# NHIS
re_nhis_m1 <- extract_random_effects(model_nhis_m1, bhapc_nhis_m1)
re_nhis_m2 <- extract_random_effects(model_nhis_m2, bhapc_nhis_m2)
re_nhis_m3 <- extract_random_effects(model_nhis_m3, bhapc_nhis_m3)
re_nhis_m4 <- extract_random_effects(model_nhis_m4, bhapc_nhis_m4)

# Combine for comparison plots
meps_cohort_comparison <- bind_rows(
  re_meps_m1$cohort_effects %>% mutate(model = "M1 (base)"),
  re_meps_m2$cohort_effects %>% mutate(model = "M2 (demographics)"),
  re_meps_m3$cohort_effects %>% mutate(model = "M3 (K6)"),
  re_meps_m4$cohort_effects %>% mutate(model = "M4 (full)")
)

meps_period_comparison <- bind_rows(
  re_meps_m1$period_effects %>% mutate(model = "M1 (base)"),
  re_meps_m2$period_effects %>% mutate(model = "M2 (demographics)"),
  re_meps_m3$period_effects %>% mutate(model = "M3 (K6)"),
  re_meps_m4$period_effects %>% mutate(model = "M4 (full)")
)

nhis_cohort_comparison <- bind_rows(
  re_nhis_m1$cohort_effects %>% mutate(model = "M1 (base)"),
  re_nhis_m2$cohort_effects %>% mutate(model = "M2 (demographics)"),
  re_nhis_m3$cohort_effects %>% mutate(model = "M3 (K6)"),
  re_nhis_m4$cohort_effects %>% mutate(model = "M4 (full)")
)

nhis_period_comparison <- bind_rows(
  re_nhis_m1$period_effects %>% mutate(model = "M1 (base)"),
  re_nhis_m2$period_effects %>% mutate(model = "M2 (demographics)"),
  re_nhis_m3$period_effects %>% mutate(model = "M3 (K6)"),
  re_nhis_m4$period_effects %>% mutate(model = "M4 (full)")
)

cat("  Random effects extracted.\n")

# ==============================================================================
# 11. Figures
# ==============================================================================

cat("\n--- STEP 11: GENERATING FIGURES ---\n\n")

model_colors <- c(
  "M1 (base)" = "gray40",
  "M2 (demographics)" = "#E69F00",
  "M3 (K6)" = "#0072B2",
  "M4 (full)" = "#D55E00"
)

# ------------------------------------------------------------------------------
# MEPS Figures
# ------------------------------------------------------------------------------

# Variance comparison bar chart
meps_var_plot_df <- meps_comparison %>%
  filter(component %in% c("Cohort", "Period", "Residual")) %>%
  pivot_longer(-component, names_to = "model", values_to = "pct") %>%
  mutate(
    model = factor(model, levels = c("M1_base", "M2_demog", "M3_k6", "M4_full"),
                   labels = c("M1 (base)", "M2 (demographics)", "M3 (K6)", "M4 (full)")),
    component = factor(component, levels = c("Cohort", "Period", "Residual"))
  )

p_meps_var <- meps_var_plot_df %>%
  filter(component != "Residual") %>%
  ggplot(aes(x = model, y = pct, fill = component)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", pct)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Cohort" = "#CC79A7", "Period" = "#009E73")) +
  labs(
    title = "Variance Explained by Cohort and Period (MEPS)",
    subtitle = "BHAPC models with increasing covariate adjustment",
    x = NULL, y = "% of Total Variance", fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(output_dir, "meps", "variance_comparison.png"),
       p_meps_var, width = 8, height = 6, dpi = 300)
cat("  Saved meps/variance_comparison.png\n")

# Cohort effects overlay
p_meps_cohort <- meps_cohort_comparison %>%
  mutate(model = factor(model, levels = names(model_colors))) %>%
  ggplot(aes(x = cohort, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                width = 2, linewidth = 0.4, alpha = 0.5) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Cohort Random Effects Across Models (MEPS)",
    subtitle = "90% credible intervals",
    x = "Birth Cohort", y = "Cohort Random Effect (SRH units)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "meps", "cohort_effects_comparison.png"),
       p_meps_cohort, width = 10, height = 7, dpi = 300)
cat("  Saved meps/cohort_effects_comparison.png\n")

# Period effects overlay
p_meps_period <- meps_period_comparison %>%
  mutate(model = factor(model, levels = names(model_colors))) %>%
  ggplot(aes(x = period, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 3, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                width = 0.6, linewidth = 0.6,
                position = position_dodge(width = 0.8)) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Period Random Effects Across Models (MEPS)",
    subtitle = "90% credible intervals",
    x = "Period (start year)", y = "Period Random Effect (SRH units)",
    color = "Model"
  ) +
  scale_x_continuous(breaks = unique(meps_period_comparison$period)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "meps", "period_effects_comparison.png"),
       p_meps_period, width = 10, height = 7, dpi = 300)
cat("  Saved meps/period_effects_comparison.png\n")

# Figure 3 APC panels for full model
fig3_meps <- create_figure3_apc_effects(
  model_meps_m4, bhapc_meps_m4,
  survey = "meps_m4",
  output_path = file.path(output_dir, "meps", "figure3_m4.png")
)
cat("  Saved meps/figure3_m4.png\n")

# ------------------------------------------------------------------------------
# NHIS Figures
# ------------------------------------------------------------------------------

# Variance comparison bar chart
nhis_var_plot_df <- nhis_comparison %>%
  filter(component %in% c("Cohort", "Period", "Residual")) %>%
  pivot_longer(-component, names_to = "model", values_to = "pct") %>%
  mutate(
    model = factor(model, levels = c("M1_base", "M2_demog", "M3_k6", "M4_full"),
                   labels = c("M1 (base)", "M2 (demographics)", "M3 (K6)", "M4 (full)")),
    component = factor(component, levels = c("Cohort", "Period", "Residual"))
  )

p_nhis_var <- nhis_var_plot_df %>%
  filter(component != "Residual") %>%
  ggplot(aes(x = model, y = pct, fill = component)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", pct)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Cohort" = "#CC79A7", "Period" = "#009E73")) +
  labs(
    title = "Variance Explained by Cohort and Period (NHIS)",
    subtitle = "BHAPC models with increasing covariate adjustment",
    x = NULL, y = "% of Total Variance", fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(output_dir, "nhis", "variance_comparison.png"),
       p_nhis_var, width = 8, height = 6, dpi = 300)
cat("  Saved nhis/variance_comparison.png\n")

# Cohort effects overlay
p_nhis_cohort <- nhis_cohort_comparison %>%
  mutate(model = factor(model, levels = names(model_colors))) %>%
  ggplot(aes(x = cohort, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                width = 2, linewidth = 0.4, alpha = 0.5) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Cohort Random Effects Across Models (NHIS)",
    subtitle = "90% credible intervals",
    x = "Birth Cohort", y = "Cohort Random Effect (SRH units)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "nhis", "cohort_effects_comparison.png"),
       p_nhis_cohort, width = 10, height = 7, dpi = 300)
cat("  Saved nhis/cohort_effects_comparison.png\n")

# Period effects overlay
p_nhis_period <- nhis_period_comparison %>%
  mutate(model = factor(model, levels = names(model_colors))) %>%
  ggplot(aes(x = period, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 3, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                width = 0.6, linewidth = 0.6,
                position = position_dodge(width = 0.8)) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Period Random Effects Across Models (NHIS)",
    subtitle = "90% credible intervals",
    x = "Period (start year)", y = "Period Random Effect (SRH units)",
    color = "Model"
  ) +
  scale_x_continuous(breaks = unique(nhis_period_comparison$period)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "nhis", "period_effects_comparison.png"),
       p_nhis_period, width = 10, height = 7, dpi = 300)
cat("  Saved nhis/period_effects_comparison.png\n")

# Figure 3 APC panels for full model
fig3_nhis <- create_figure3_apc_effects(
  model_nhis_m4, bhapc_nhis_m4,
  survey = "nhis_m4",
  output_path = file.path(output_dir, "nhis", "figure3_m4.png")
)
cat("  Saved nhis/figure3_m4.png\n")

# ==============================================================================
# 12. Combined Summary
# ==============================================================================

cat("\n--- STEP 12: COMBINED SUMMARY ---\n\n")

combined_summary <- bind_rows(
  meps_comparison %>%
    filter(component == "Cohort") %>%
    mutate(survey = "MEPS") %>%
    select(survey, component, everything()),
  nhis_comparison %>%
    filter(component == "Cohort") %>%
    mutate(survey = "NHIS") %>%
    select(survey, component, everything())
)

write_csv(combined_summary, file.path(output_dir, "combined_summary.csv"))

# ==============================================================================
# 13. Interpretation Summary
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("INTERPRETATION SUMMARY\n")
cat(strrep("=", 70), "\n\n")

# MEPS
meps_cohort_m1 <- get_pct(var_meps_m1, "cohort_4yr")
meps_cohort_m2 <- get_pct(var_meps_m2, "cohort_4yr")
meps_cohort_m3 <- get_pct(var_meps_m3, "cohort_4yr")
meps_cohort_m4 <- get_pct(var_meps_m4, "cohort_4yr")

cat("MEPS Cohort variance %:\n")
cat(sprintf("  M1 (base):          %.2f%%\n", meps_cohort_m1))
cat(sprintf("  M2 (demographics):  %.2f%%  (%.0f%% reduction from M1)\n",
            meps_cohort_m2, (1 - meps_cohort_m2 / meps_cohort_m1) * 100))
cat(sprintf("  M3 (K6):            %.2f%%  (%.0f%% reduction from M1)\n",
            meps_cohort_m3, (1 - meps_cohort_m3 / meps_cohort_m1) * 100))
cat(sprintf("  M4 (full):          %.2f%%  (%.0f%% reduction from M1)\n",
            meps_cohort_m4, (1 - meps_cohort_m4 / meps_cohort_m1) * 100))

# NHIS
nhis_cohort_m1 <- get_pct(var_nhis_m1, "cohort_4yr")
nhis_cohort_m2 <- get_pct(var_nhis_m2, "cohort_4yr")
nhis_cohort_m3 <- get_pct(var_nhis_m3, "cohort_4yr")
nhis_cohort_m4 <- get_pct(var_nhis_m4, "cohort_4yr")

cat("\nNHIS Cohort variance %:\n")
cat(sprintf("  M1 (base):          %.2f%%\n", nhis_cohort_m1))
cat(sprintf("  M2 (demographics):  %.2f%%  (%.0f%% reduction from M1)\n",
            nhis_cohort_m2, (1 - nhis_cohort_m2 / nhis_cohort_m1) * 100))
cat(sprintf("  M3 (K6):            %.2f%%  (%.0f%% reduction from M1)\n",
            nhis_cohort_m3, (1 - nhis_cohort_m3 / nhis_cohort_m1) * 100))
cat(sprintf("  M4 (full):          %.2f%%  (%.0f%% reduction from M1)\n",
            nhis_cohort_m4, (1 - nhis_cohort_m4 / nhis_cohort_m1) * 100))

# K6 effects
cat("\nK6 effect on SRH (higher K6 = more distress):\n")
k6_meps <- fe_meps_m4 %>% filter(term == "k6")
k6_nhis <- fe_nhis_m4 %>% filter(term == "k6")

if (nrow(k6_meps) > 0) {
  cat(sprintf("  MEPS: %.4f [%.4f, %.4f] (expected: negative)\n",
              k6_meps$estimate, k6_meps$ci_lower_90, k6_meps$ci_upper_90))
}
if (nrow(k6_nhis) > 0) {
  cat(sprintf("  NHIS: %.4f [%.4f, %.4f] (expected: negative)\n",
              k6_nhis$estimate, k6_nhis$ci_lower_90, k6_nhis$ci_upper_90))
}

cat("\nSample sizes:\n")
cat(sprintf("  MEPS M1: %s    M4: %s\n",
            format(nrow(bhapc_meps_m1), big.mark = ","),
            format(nrow(bhapc_meps_m4), big.mark = ",")))
cat(sprintf("  NHIS M1: %s    M4: %s\n",
            format(nrow(bhapc_nhis_m1), big.mark = ","),
            format(nrow(bhapc_nhis_m4), big.mark = ",")))

cat("\nCaveats:\n")
cat("  - NHIS K6 available starting ~1997\n")
cat("  - MEPS K6SUM available starting 2004\n")
cat("  - Subsampling used for computational feasibility\n")
if (!nhis_has_race) {
  cat("  - NHIS race variables not available; run 16b_add_nhis_race.R first\n")
}

cat("\n", strrep("=", 70), "\n")
cat("COMPLETE\n")
cat("Output directory:", output_dir, "\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
