# ==============================================================================
# 08a_mortality_sensitivity_adjusted.R
# Mortality Sensitivity Analysis: Covariate-Adjusted Cox PH Models
#
# Purpose:
#   Run the SRH-mortality Cox regression over 10-year rolling windows,
#   adjusted for each sociodemographic covariate (sex, race/ethnicity,
#   education) separately. Compare to the unadjusted baseline.
#
# Output:
#   output/sensitivity/mortality/mortality_adjusted_results_{date}.csv
#   output/sensitivity/mortality/mortality_adjusted_results_{date}.rds
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(survival)
library(here)

# Source shared functions
source(here("R", "paths.R"))
source(here("R", "srh_common_functions.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "mortality_utils.R"))
source(here("R", "functions", "run_mortality_sensitivity.R"))

# Set seed for reproducibility
set.seed(20260203)

# Output directory
output_dir <- here("output", "sensitivity", "mortality")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Analysis date for draft outputs
analysis_date <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# LOAD AND PREPARE DATA
# ==============================================================================

message("\n=== Loading NHIS data ===")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))
message(sprintf("Loaded %s records from data_nhis.rds",
                format(nrow(data_nhis), big.mark = ",")))

# Prepare mortality data with covariate factors
message("\n=== Preparing mortality data ===")

nhis_mort <- prepare_mortality_from_nhis(
  data_nhis,
  min_age = 18,
  max_age = 89,
  age_scheme = "B",
  min_year = 1982,
  follow_up_end_year = 2019
)

rm(data_nhis)
gc(verbose = FALSE)


# ==============================================================================
# CHECK COVARIATE AVAILABILITY
# ==============================================================================

message("\n=== Checking covariate availability ===")

has_sex <- "sex" %in% names(nhis_mort) && sum(!is.na(nhis_mort$sex)) > 0
has_educ <- "educ_3cat_f" %in% names(nhis_mort) && sum(!is.na(nhis_mort$educ_3cat_f)) > 0
has_race <- "race_includehisp_f" %in% names(nhis_mort) && sum(!is.na(nhis_mort$race_includehisp_f)) > 0

message(sprintf("  sex: %s", if (has_sex) "AVAILABLE" else "NOT AVAILABLE"))
message(sprintf("  educ_3cat_f: %s", if (has_educ) "AVAILABLE" else "NOT AVAILABLE"))
message(sprintf("  race_includehisp_f: %s", if (has_race) "AVAILABLE" else "NOT AVAILABLE"))

if (has_sex) {
  message("  Sex distribution:")
  print(table(nhis_mort$sex, useNA = "ifany"))
}
if (has_educ) {
  message("  Education distribution:")
  print(table(nhis_mort$educ_3cat_f, useNA = "ifany"))
}
if (has_race) {
  message("  Race/ethnicity distribution:")
  print(table(nhis_mort$race_includehisp_f, useNA = "ifany"))
}


# ==============================================================================
# DEFINE MODEL SPECIFICATIONS
# ==============================================================================

# Window parameters: 10-year rolling windows (matching main Figure panel A)
window_length <- 10
min_year <- min(nhis_mort$survey_year, na.rm = TRUE)
max_year <- max(nhis_mort$survey_year, na.rm = TRUE)
start_years <- get_window_starts(min_year, max_year, window_length)

message(sprintf(
  "\nWindow parameters: %d-year windows, start years %d to %d (%d windows)",
  window_length, min(start_years), max(start_years), length(start_years)
))

# Build covariate specifications
covariate_specs <- list(
  list(label = "Unadjusted", vars = character(0))
)

if (has_sex) {
  covariate_specs <- c(covariate_specs, list(
    list(label = "Adjusted: Sex", vars = "sex")
  ))
}

if (has_educ) {
  covariate_specs <- c(covariate_specs, list(
    list(label = "Adjusted: Education", vars = "educ_3cat_f")
  ))
}

if (has_race) {
  covariate_specs <- c(covariate_specs, list(
    list(label = "Adjusted: Race/Ethnicity", vars = "race_includehisp_f")
  ))
}

message(sprintf("\nModel specifications: %d", length(covariate_specs)))
for (spec in covariate_specs) {
  message(sprintf("  - %s", spec$label))
}


# ==============================================================================
# RUN ADJUSTED MODELS
# ==============================================================================

message("\n=== Running adjusted Cox models ===\n")

results_list <- list()

for (spec in covariate_specs) {
  message(sprintf("\n--- %s ---", spec$label))

  result <- run_sliding_windows_adjusted(
    nhis_mort,
    start_years = start_years,
    window_length = window_length,
    adjust_vars = spec$vars,
    model_label = spec$label,
    weighted = TRUE,
    weight_var = "weight",
    min_events = 10,
    min_n = 50,
    verbose = TRUE
  )

  results_list[[spec$label]] <- result
}

# Combine all results
results_all <- bind_rows(results_list)

message(sprintf("\n\nTotal results: %d rows", nrow(results_all)))
message("Model labels: ", paste(unique(results_all$model_label), collapse = ", "))


# ==============================================================================
# SAVE RESULTS
# ==============================================================================

message("\n=== Saving results ===")

readr::write_csv(
  results_all,
  file.path(output_dir, paste0("mortality_adjusted_results_", analysis_date, ".csv"))
)
readr::write_rds(
  results_all,
  file.path(output_dir, paste0("mortality_adjusted_results_", analysis_date, ".rds"))
)
message(sprintf("Saved to: %s/mortality_adjusted_results_%s.{csv,rds}",
                output_dir, analysis_date))


# ==============================================================================
# VERIFICATION
# ==============================================================================

message("\n=== Verification ===\n")

results_converged <- results_all %>% filter(converged)

# By model label
for (ml in unique(results_converged$model_label)) {
  sub <- results_converged %>% filter(model_label == ml)
  n_pos <- sum(sub$coef > 0, na.rm = TRUE)
  n_total <- nrow(sub)
  hr_range <- range(sub$hr, na.rm = TRUE)

  message(sprintf(
    "%s:\n  Converged: %d/%d\n  Positive coef: %d (should be 0)\n  HR range: %.3f - %.3f",
    ml, n_total,
    nrow(results_all %>% filter(model_label == ml)),
    n_pos, hr_range[1], hr_range[2]
  ))
}

# Compare unadjusted to adjusted (mean HR difference)
if (nrow(results_converged) > 0 && "Unadjusted" %in% results_converged$model_label) {
  message("\n--- Comparison to unadjusted ---")

  unadj <- results_converged %>%
    filter(model_label == "Unadjusted") %>%
    select(start_year, age_group, hr_unadj = hr)

  for (ml in setdiff(unique(results_converged$model_label), "Unadjusted")) {
    adj <- results_converged %>%
      filter(model_label == ml) %>%
      select(start_year, age_group, hr_adj = hr)

    comparison <- inner_join(unadj, adj, by = c("start_year", "age_group"))

    if (nrow(comparison) > 0) {
      mean_diff <- mean(comparison$hr_adj - comparison$hr_unadj, na.rm = TRUE)
      cor_val <- cor(comparison$hr_unadj, comparison$hr_adj, use = "complete.obs")
      message(sprintf(
        "  %s: mean HR difference = %.4f, correlation = %.4f (n=%d)",
        ml, mean_diff, cor_val, nrow(comparison)
      ))
    }
  }
}

message("\n=== Analysis complete ===")
