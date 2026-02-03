# ==============================================================================
# 08e_mortality_sensitivity_tables.R
# Mortality Sensitivity Analysis: HR Tables for Most Recent Rolling Window
#
# Purpose:
#   Extract hazard ratio tables for the most recent 10-year rolling window:
#
#   Table (a): HR by age group for:
#     (i)  Unadjusted Cox PH on SRH (main analysis)
#     (ii) Cox PH adjusted for each covariate (sex, education, race)
#
#   Table (b): HR by age group for each stratum of:
#     - Sex (Male, Female)
#     - Education (< HS, HS/Some college, College+)
#     - Race/Ethnicity (NH White, NH Black, Hispanic, NH Asian, NH AIAN, Other)
#
#   All tables include 95% CIs.
#
# Input:
#   output/sensitivity/mortality/mortality_adjusted_results_{date}.csv
#   output/sensitivity/mortality/mortality_stratified_results_{date}.csv
#
# Output:
#   output/sensitivity/mortality/table_mort_hr_adjusted_{date}.csv
#   output/sensitivity/mortality/table_mort_hr_stratified_sex_{date}.csv
#   output/sensitivity/mortality/table_mort_hr_stratified_educ_{date}.csv
#   output/sensitivity/mortality/table_mort_hr_stratified_race_{date}.csv
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)

source(here("R", "paths.R"))

# Output directory
output_dir <- here("output", "sensitivity", "mortality")

# Analysis date
date_suffix <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# LOAD RESULTS
# ==============================================================================

message("\n=== Loading results ===\n")

# --- Adjusted results ---
adj_files <- list.files(
  output_dir,
  pattern = "mortality_adjusted_results_\\d+\\.csv$",
  full.names = TRUE
)
if (length(adj_files) == 0) {
  stop("No adjusted results found. Run 08a first.")
}
adj_results <- read_csv(
  sort(adj_files, decreasing = TRUE)[1],
  show_col_types = FALSE
)
message("Adjusted: ", nrow(adj_results), " rows")

# --- Stratified results ---
strat_files <- list.files(
  output_dir,
  pattern = "mortality_stratified_results_\\d+\\.csv$",
  full.names = TRUE
)
if (length(strat_files) == 0) {
  stop("No stratified results found. Run 08c first.")
}
strat_results <- read_csv(
  sort(strat_files, decreasing = TRUE)[1],
  show_col_types = FALSE
)
message("Stratified: ", nrow(strat_results), " rows")

# Age group ordering
age_levels <- c("18-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89")


# ==============================================================================
# IDENTIFY MOST RECENT WINDOW
# ==============================================================================

# Use the unadjusted model to find the most recent window
most_recent_start <- adj_results %>%
  filter(model_label == "Unadjusted", converged) %>%
  pull(start_year) %>%
  max(na.rm = TRUE)

most_recent_end <- most_recent_start + 9  # 10-year window

message(sprintf(
  "\nMost recent window: %d-%d (start_year = %d)",
  most_recent_start, most_recent_end, most_recent_start
))


# ==============================================================================
# HELPER: FORMAT HR WITH CI
# ==============================================================================

format_hr_ci <- function(hr, conf_low, conf_high, digits = 3) {
  ifelse(
    is.na(hr),
    NA_character_,
    sprintf(
      "%.*f (%.*f, %.*f)",
      digits, hr, digits, conf_low, digits, conf_high
    )
  )
}


# ==============================================================================
# TABLE (a): ADJUSTED MODELS — MOST RECENT WINDOW
# ==============================================================================

message("\n=== Table (a): Adjusted models ===\n")

adj_latest <- adj_results %>%
  filter(start_year == most_recent_start, converged) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    hr_ci = format_hr_ci(hr, conf_low, conf_high)
  )

# Wide format: age groups as rows, model specifications as columns
table_a <- adj_latest %>%
  select(age_group, model_label, hr, conf_low, conf_high, hr_ci,
         n, n_events) %>%
  arrange(age_group, model_label)

# Also create a wide version for easy reading
table_a_wide <- adj_latest %>%
  select(age_group, model_label, hr_ci) %>%
  pivot_wider(
    names_from = model_label,
    values_from = hr_ci
  ) %>%
  arrange(age_group)

message("Table (a) - HR (95% CI) by age group and model:")
print(as.data.frame(table_a_wide), row.names = FALSE)

# Save long format (full details)
write_csv(
  table_a,
  file.path(output_dir,
            paste0("table_mort_hr_adjusted_", date_suffix, ".csv"))
)
message(sprintf(
  "\nSaved: table_mort_hr_adjusted_%s.csv", date_suffix
))

# Save wide format (readable)
write_csv(
  table_a_wide,
  file.path(output_dir,
            paste0("table_mort_hr_adjusted_wide_", date_suffix, ".csv"))
)
message(sprintf(
  "Saved: table_mort_hr_adjusted_wide_%s.csv", date_suffix
))


# ==============================================================================
# TABLE (b): STRATIFIED MODELS — MOST RECENT WINDOW
# ==============================================================================

message("\n=== Table (b): Stratified models ===\n")

# For stratified results, the most recent window may differ by stratum
# (e.g., education/race start later). Use per-stratum most-recent.

make_strat_table <- function(data,
                              strat_var_name,
                              strata_order,
                              table_label) {

  strat_data <- data %>%
    filter(strat_var == strat_var_name, converged) %>%
    group_by(stratum) %>%
    filter(start_year == max(start_year, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      age_group = factor(age_group, levels = age_levels),
      hr_ci = format_hr_ci(hr, conf_low, conf_high)
    )

  if (nrow(strat_data) == 0) {
    message("  No data for ", strat_var_name)
    return(NULL)
  }

  # Report the window used
  windows_used <- strat_data %>%
    distinct(stratum, start_year, end_year)
  message(sprintf("\n%s — windows used:", table_label))
  for (i in seq_len(nrow(windows_used))) {
    row <- windows_used[i, ]
    message(sprintf("  %s: %d-%d",
                    row$stratum, row$start_year, row$end_year))
  }

  # Long format
  table_long <- strat_data %>%
    mutate(stratum = factor(stratum, levels = strata_order)) %>%
    select(age_group, stratum, hr, conf_low, conf_high, hr_ci,
           n, n_events, start_year, end_year) %>%
    arrange(stratum, age_group)

  # Wide format: rows = age group, columns = stratum
  table_wide <- strat_data %>%
    mutate(stratum = factor(stratum, levels = strata_order)) %>%
    select(age_group, stratum, hr_ci) %>%
    pivot_wider(names_from = stratum, values_from = hr_ci) %>%
    arrange(age_group)

  message(sprintf("\n%s — HR (95%% CI) by age group:", table_label))
  print(as.data.frame(table_wide), row.names = FALSE)

  list(long = table_long, wide = table_wide)
}


# --- Sex ---
tables_sex <- make_strat_table(
  strat_results,
  strat_var_name = "sex",
  strata_order = c("Male", "Female"),
  table_label = "Stratified by Sex"
)

if (!is.null(tables_sex)) {
  write_csv(
    tables_sex$long,
    file.path(output_dir, paste0(
      "table_mort_hr_stratified_sex_", date_suffix, ".csv"
    ))
  )
  write_csv(
    tables_sex$wide,
    file.path(output_dir, paste0(
      "table_mort_hr_stratified_sex_wide_", date_suffix, ".csv"
    ))
  )
  message(sprintf(
    "Saved: table_mort_hr_stratified_sex_%s.csv", date_suffix
  ))
}


# --- Education ---
tables_educ <- make_strat_table(
  strat_results,
  strat_var_name = "educ_3cat_f",
  strata_order = c("< HS", "HS/Some college", "College+"),
  table_label = "Stratified by Education"
)

if (!is.null(tables_educ)) {
  write_csv(
    tables_educ$long,
    file.path(output_dir, paste0(
      "table_mort_hr_stratified_educ_", date_suffix, ".csv"
    ))
  )
  write_csv(
    tables_educ$wide,
    file.path(output_dir, paste0(
      "table_mort_hr_stratified_educ_wide_", date_suffix, ".csv"
    ))
  )
  message(sprintf(
    "Saved: table_mort_hr_stratified_educ_%s.csv", date_suffix
  ))
}


# --- Race/Ethnicity ---
tables_race <- make_strat_table(
  strat_results,
  strat_var_name = "race_includehisp_f",
  strata_order = c("NH White", "NH Black", "Hispanic",
                    "NH Asian", "NH AIAN", "Other/Multi"),
  table_label = "Stratified by Race/Ethnicity"
)

if (!is.null(tables_race)) {
  write_csv(
    tables_race$long,
    file.path(output_dir, paste0(
      "table_mort_hr_stratified_race_", date_suffix, ".csv"
    ))
  )
  write_csv(
    tables_race$wide,
    file.path(output_dir, paste0(
      "table_mort_hr_stratified_race_wide_", date_suffix, ".csv"
    ))
  )
  message(sprintf(
    "Saved: table_mort_hr_stratified_race_%s.csv", date_suffix
  ))
}


message("\n=== Table generation complete ===")
