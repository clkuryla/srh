# ==============================================================================
# 03_mortality_analysis.R
# SRH-Mortality Rolling Window Analysis
#
# Scientific question: Does the relationship between self-rated health and
# mortality differ by age group, and has this relationship changed over time?
#
# Key finding: The SRH-mortality association is stronger for older ages and
# has weakened over time across all age groups.
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(survival)
library(here)
library(patchwork)

# Source shared functions
source(here("R", "paths.R"))
source(here("R", "srh_common_functions.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "plot_utils.R"))
source(here("R", "functions", "mortality_utils.R"))
source(here("R", "functions", "run_mortality_models.R"))
source(here("R", "functions", "plot_mortality.R"))

# Set seed for reproducibility
set.seed(20260119)

# Output directory
output_dir <- here("output", "figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Analysis date for draft outputs
analysis_date <- format(Sys.Date(), "%Y%m%d")

# ------------------------------------------------------------------------------
# LOAD AND PREPARE DATA
# ------------------------------------------------------------------------------

message("\n=== Loading NHIS mortality data ===")

# Load raw IPUMS NHIS data with mortality linkage
nhis_raw <- read_csv(
  depot_path("surveys", "NHIS", "nhis_00008.csv"),
  col_types = cols(.default = "c")  # Read as character first for safety
)

# Convert to appropriate types
nhis_raw <- nhis_raw %>%
  mutate(
    across(c(YEAR, AGE, HEALTH, MORTSTAT, MORTDODY), as.integer),
    MORTWT = as.numeric(MORTWT)
  )

message(sprintf("Loaded %s raw records", format(nrow(nhis_raw), big.mark = ",")))

# Prepare mortality analysis data
# Using age scheme "B" (18-29, 30-39, ..., 80-89) per plan
nhis_mort <- prepare_mortality_data(
  nhis_raw,
  min_age = 18,
  max_age = 89,
  age_scheme = "B",
  min_year = 1982,       # Start of 5-point SRH scale
  follow_up_end_year = 2019
)

# Quick summary by age group
summary_by_age <- summarize_mortality_data(nhis_mort) %>%
  group_by(age_group) %>%
  summarise(
    years = n(),
    total_n = sum(n),
    total_deaths = sum(n_deaths),
    death_rate = total_deaths / total_n,
    mean_srh = weighted.mean(mean_srh, n)
  )

message("\nSummary by age group:")
print(summary_by_age)

# ------------------------------------------------------------------------------
# PRIMARY ANALYSIS: 15-YEAR WINDOWS
# ------------------------------------------------------------------------------

message("\n=== Running 15-year window analysis ===")

# Window parameters
window_length_primary <- 15
min_year <- min(nhis_mort$survey_year)
max_year <- max(nhis_mort$survey_year)
start_years_15yr <- get_window_starts(min_year, max_year, window_length_primary)

message(sprintf(
  "Window range: %d to %d (n=%d windows)",
  min(start_years_15yr), max(start_years_15yr), length(start_years_15yr)
))

# Run weighted models
results_15yr <- run_sliding_windows_by_age(
  nhis_mort,
  start_years = start_years_15yr,
  window_length = window_length_primary,
  weighted = TRUE,
  weight_var = "weight",
  min_events = 10,
  min_n = 50,
  verbose = TRUE
)

# Summary statistics
message("\n--- Results summary (15-year windows) ---")
summary_15yr <- summarize_mortality_results(results_15yr)
print(summary_15yr)

# Save results
write_csv(results_15yr, here("output", paste0("mortality_results_15yr_", analysis_date, ".csv")))

# ------------------------------------------------------------------------------
# EXPLORATORY: MULTIPLE WINDOW LENGTHS
# ------------------------------------------------------------------------------

message("\n=== Running multiple window length analysis ===")

# Window lengths to explore
window_lengths_all <- c(2, 3, 5, 10, 15, 20)

results_all_windows <- run_multiple_windows(
  nhis_mort,
  window_lengths = window_lengths_all,
  weighted = TRUE,
  weight_var = "weight",
  min_events = 10,
  min_n = 50,
  verbose = TRUE
)

# Save all results
write_csv(results_all_windows, here("output", paste0("mortality_results_all_windows_", analysis_date, ".csv")))

# ------------------------------------------------------------------------------
# VERIFICATION: SANITY CHECKS
# ------------------------------------------------------------------------------

message("\n=== Verification checks ===")

# Check 1: Coefficients should be negative (higher SRH = lower mortality)
results_converged <- results_15yr %>% filter(converged)
n_positive <- sum(results_converged$coef > 0, na.rm = TRUE)
n_total <- sum(!is.na(results_converged$coef))

if (n_positive > 0) {
  warning(sprintf(
    "%d of %d converged models have positive coefficients (unexpected)",
    n_positive, n_total
  ))
} else {
  message(sprintf("Check 1 PASSED: All %d converged models have negative coefficients", n_total))
}

# Check 2: Hazard ratios should be < 1
n_hr_above_1 <- sum(results_converged$hr > 1, na.rm = TRUE)
if (n_hr_above_1 > 0) {
  warning(sprintf(
    "%d of %d converged models have HR > 1 (unexpected)",
    n_hr_above_1, n_total
  ))
} else {
  message(sprintf("Check 2 PASSED: All %d converged models have HR < 1", n_total))
}

# Check 3: Expected pattern - stronger association for older ages
mean_coef_by_age <- results_converged %>%
  group_by(age_group) %>%
  summarise(mean_coef = mean(coef, na.rm = TRUE)) %>%
  arrange(age_group)

message("\nMean coefficient by age group (more negative = stronger association):")
print(mean_coef_by_age)

# Check 4: Look for temporal trend (weakening over time)
coef_trend <- results_converged %>%
  group_by(start_year) %>%
  summarise(mean_coef = mean(coef, na.rm = TRUE))

early_coef <- coef_trend %>% filter(start_year <= 1990) %>% pull(mean_coef) %>% mean()
late_coef <- coef_trend %>% filter(start_year >= 2000) %>% pull(mean_coef) %>% mean()

message(sprintf(
  "\nTemporal trend check:\n  Early windows (<=1990) mean coef: %.4f\n  Late windows (>=2000) mean coef: %.4f",
  early_coef, late_coef
))

if (late_coef > early_coef) {
  message("  -> Coefficient is becoming less negative (association weakening) as expected")
} else {
  warning("  -> Unexpected temporal pattern - coefficient not becoming less negative")
}

# ------------------------------------------------------------------------------
# GENERATE FIGURES
# ------------------------------------------------------------------------------

message("\n=== Generating figures ===")

# --- Figure 3a: 15-year window coefficient plot ---
fig3a_coef <- plot_mortality_coef(
  results_15yr,
  window_length = 15,
  show_ci = TRUE,
  title = "SRH-Mortality Association Over Time",
  subtitle = "15-year follow-up windows, survey-weighted",
  y_label = "Coefficient (log-Hazard Ratio)",
  x_label = "Window Start Year"
)

# --- Figure 3b: 15-year window HR plot ---
fig3b_hr <- plot_mortality_hr(
  results_15yr,
  window_length = 15,
  show_ci = TRUE,
  log_scale = TRUE,
  title = "Hazard Ratio by Age Group",
  subtitle = "Per 1-unit increase in SRH (higher = better health)",
  y_label = "Hazard Ratio"
)

# --- Combined Figure 3 ---
fig3_combined <- plot_mortality_combined(
  results_15yr,
  window_length = 15,
  title = "SRH and Mortality: Age-Stratified Rolling Window Analysis",
  subtitle = "NHIS 1982-2018, 15-year follow-up windows, survey-weighted Cox PH models"
)

# --- Multi-window exploratory figure ---
fig_multiwindow <- plot_mortality_multi_window(
  results_all_windows,
  plot_type = "coef",
  ncol = 3,
  title = "SRH-Mortality Association Across Window Lengths",
  subtitle = "Comparing 2, 3, 5, 10, 15, and 20-year follow-up windows"
)

# --- Diagnostic plots ---
fig_sample_sizes <- plot_sample_sizes(results_15yr, window_length = 15) +
  labs(title = "Sample Sizes (15-year windows)")

fig_event_counts <- plot_event_counts(results_15yr, window_length = 15) +
  labs(title = "Death Counts (15-year windows)")

# ------------------------------------------------------------------------------
# SAVE FIGURES
# ------------------------------------------------------------------------------

message("\n=== Saving figures ===")

# Draft versions (with date)
ggsave(
  here(output_dir, paste0("fig3_mortality_15yr_draft_", analysis_date, ".png")),
  fig3_combined,
  width = 10, height = 12, dpi = 300
)

ggsave(
  here(output_dir, paste0("fig3_mortality_multiwindow_draft_", analysis_date, ".png")),
  fig_multiwindow,
  width = 14, height = 10, dpi = 300
)

# Diagnostic plots
ggsave(
  here(output_dir, paste0("fig3_diagnostics_", analysis_date, ".png")),
  fig_sample_sizes / fig_event_counts,
  width = 10, height = 10, dpi = 300
)

# Final versions (no date, for publication)
save_figure(fig3_combined, "fig3_mortality_15yr", width = 10, height = 12)
save_figure(fig_multiwindow, "fig3_mortality_multiwindow", width = 14, height = 10)

message("\nFigures saved to: ", output_dir)

# ------------------------------------------------------------------------------
# SESSION INFO
# ------------------------------------------------------------------------------

message("\n=== Session Info ===")
sessionInfo()

message("\n=== Analysis complete ===")
