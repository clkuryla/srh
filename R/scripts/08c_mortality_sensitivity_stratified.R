# ==============================================================================
# 08c_mortality_sensitivity_stratified.R
# Mortality Sensitivity Analysis: Stratified Cox PH Models
#
# Purpose:
#   Run separate SRH-mortality Cox regressions within each stratum of
#   sex, race/ethnicity, and education. For each stratum, the model is
#   Surv(age_at_survey, age_at_end, event) ~ srh, run per age group
#   over 10-year rolling windows.
#
# Output:
#   output/sensitivity/mortality/mortality_stratified_results_{date}.csv
#   output/sensitivity/mortality/mortality_stratified_results_{date}.rds
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

has_sex <- "sex" %in% names(nhis_mort) &&
  sum(!is.na(nhis_mort$sex)) > 0
has_educ <- "educ_3cat_f" %in% names(nhis_mort) &&
  sum(!is.na(nhis_mort$educ_3cat_f)) > 0
has_race <- "race_includehisp_f" %in% names(nhis_mort) &&
  sum(!is.na(nhis_mort$race_includehisp_f)) > 0

message(sprintf("  sex: %s",
                if (has_sex) "AVAILABLE" else "NOT AVAILABLE"))
message(sprintf("  educ_3cat_f: %s",
                if (has_educ) "AVAILABLE" else "NOT AVAILABLE"))
message(sprintf("  race_includehisp_f: %s",
                if (has_race) "AVAILABLE" else "NOT AVAILABLE"))


# ==============================================================================
# DEFINE WINDOW PARAMETERS
# ==============================================================================

window_length <- 10
min_year <- min(nhis_mort$survey_year, na.rm = TRUE)
max_year <- max(nhis_mort$survey_year, na.rm = TRUE)
start_years <- get_window_starts(min_year, max_year, window_length)

message(sprintf(
  "\nWindow parameters: %d-year windows, %d to %d (%d windows)",
  window_length, min(start_years), max(start_years),
  length(start_years)
))


# ==============================================================================
# RUN STRATIFIED MODELS
# ==============================================================================

message("\n=== Running stratified Cox models ===\n")

results_list <- list()

# --- Stratify by Sex ---
if (has_sex) {
  message("\n========== Stratifying by Sex ==========")
  results_sex <- run_sliding_windows_stratified(
    nhis_mort,
    start_years = start_years,
    window_length = window_length,
    strat_var = "sex",
    weighted = TRUE,
    weight_var = "weight",
    min_events = 10,
    min_n = 50,
    verbose = TRUE
  )
  results_list[["sex"]] <- results_sex
}

# --- Stratify by Education ---
if (has_educ) {
  message("\n========== Stratifying by Education ==========")
  results_educ <- run_sliding_windows_stratified(
    nhis_mort,
    start_years = start_years,
    window_length = window_length,
    strat_var = "educ_3cat_f",
    weighted = TRUE,
    weight_var = "weight",
    min_events = 10,
    min_n = 50,
    verbose = TRUE
  )
  results_list[["educ"]] <- results_educ
}

# --- Stratify by Race/Ethnicity ---
if (has_race) {
  message("\n========== Stratifying by Race/Ethnicity ==========")
  results_race <- run_sliding_windows_stratified(
    nhis_mort,
    start_years = start_years,
    window_length = window_length,
    strat_var = "race_includehisp_f",
    weighted = TRUE,
    weight_var = "weight",
    min_events = 10,
    min_n = 50,
    verbose = TRUE
  )
  results_list[["race"]] <- results_race
}

# Combine all results
results_all <- bind_rows(results_list)

message(sprintf(
  "\n\nTotal results: %d rows",
  nrow(results_all)
))
message("Stratification variables: ",
        paste(unique(results_all$strat_var), collapse = ", "))
message("Strata: ",
        paste(unique(results_all$stratum), collapse = ", "))


# ==============================================================================
# SAVE RESULTS
# ==============================================================================

message("\n=== Saving results ===")

readr::write_csv(
  results_all,
  file.path(output_dir,
            paste0("mortality_stratified_results_", analysis_date, ".csv"))
)
readr::write_rds(
  results_all,
  file.path(output_dir,
            paste0("mortality_stratified_results_", analysis_date, ".rds"))
)
message(sprintf(
  "Saved to: %s/mortality_stratified_results_%s.{csv,rds}",
  output_dir, analysis_date
))


# ==============================================================================
# VERIFICATION
# ==============================================================================

message("\n=== Verification ===\n")

results_converged <- results_all %>% filter(converged)

# Summary by strat_var and stratum
for (sv in unique(results_converged$strat_var)) {
  message(sprintf("\n--- %s ---", sv))
  strata <- unique(
    results_converged %>% filter(strat_var == sv) %>% pull(stratum)
  )
  for (s in strata) {
    sub <- results_converged %>%
      filter(strat_var == sv, stratum == s)
    n_pos <- sum(sub$coef > 0, na.rm = TRUE)
    n_total <- nrow(sub)
    hr_range <- range(sub$hr, na.rm = TRUE)

    message(sprintf(
      "  %s: %d converged, %d positive coef (should be 0), HR range: %.3f-%.3f",
      s, n_total, n_pos, hr_range[1], hr_range[2]
    ))
  }
}

message("\n=== Stratified analysis complete ===")
