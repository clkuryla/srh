# ==============================================================================
# 06_meps_functional_limitations_prev.R
# MEPS Functional Limitation Variables: Prevalence/Mean Trends Analysis
#
# Purpose:
#   Compute survey-weighted means of functional limitation variables
#   by age group and year to show prevalence trends over time.
#
# Variables analyzed (VR-12/SF-12 items):
#   - ADDAYA: Health limits moderate activities (0-2)
#   - ADCLIM: Health limits climbing stairs (0-2)
#   - ANYLMT: Any limitation reported (0-2)
#   - ADPAIN: Pain interfered with work (0-4)
#   - ADPALS: Accomplished less due to physical health (0-4)
#   - ADPWLM: Limited in work type due to physical health (0-4)
#   - ADSOCA: Health interfered with social activities (0-4)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(srvyr)
library(here)
library(patchwork)

# Source paths and shared functions
source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "prevalence_by_age_year.R"))

# Set theme
theme_set(theme_srh())

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)

# Output directories
output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")

# Age group levels (Scheme B)
AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")


# ==============================================================================
# VARIABLE DEFINITIONS WITH QUESTION WORDING
# ==============================================================================

# Named vector: variable name -> label with question wording
func_limit_vars <- c(

"ADDAYA" = "ADDAYA: Moderate Activities\n\"Does your health now limit you in moderate activities,\nsuch as moving a table, pushing a vacuum, bowling, or golf?\"",

"ADCLIM" = "ADCLIM: Climbing Stairs\n\"Does your health now limit you in\nclimbing several flights of stairs?\"",

"ANYLMT" = "ANYLMT: Any Limitation\nSummary measure of any IADL, ADL,\nfunctional, or activity limitation",

"ADPAIN" = "ADPAIN: Pain Interference\n\"During the past 4 weeks, how much did pain\ninterfere with your normal work?\"",

"ADPALS" = "ADPALS: Accomplished Less (Physical)\n\"Past 4 weeks: how much of the time have you accomplished\nless than you would like due to physical health?\"",

"ADPWLM" = "ADPWLM: Work Limited (Physical)\n\"Past 4 weeks: how much of the time were you limited\nin the kind of work due to physical health?\"",

"ADSOCA" = "ADSOCA: Social Activity Interference\n\"Past 4 weeks: how much has physical/emotional health\ninterfered with your social activities?\""
)


# ==============================================================================
# LOAD DATA
# ==============================================================================

message("\n========== Loading MEPS data ==========\n")

data_meps <- readr::read_rds(derived_path("data_meps.rds"))

message("MEPS: ", nrow(data_meps), " rows, years ",
        min(data_meps$year), "-", max(data_meps$year))

# Add age group if not present
if (!"age_group" %in% names(data_meps)) {
  source(here("R", "srh_common_functions.R"))
  data_meps <- add_age_group(data_meps, age_var = age, scheme = "B")
}

# Ensure age_group is a factor with correct levels
data_meps <- data_meps %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS))


# ==============================================================================
# COMPUTE PREVALENCE/MEANS BY AGE GROUP AND YEAR
# ==============================================================================

message("\n========== Computing prevalence/means by age group ==========\n")

# Store results
prev_results <- list()

for (i in seq_along(func_limit_vars)) {
  var_name <- names(func_limit_vars)[i]
  var_label <- func_limit_vars[i]

  message("\n--- Processing: ", var_name, " ---")

  # Check variable exists and has valid data
  if (!var_name %in% names(data_meps)) {
    message("  Variable not found in data. Skipping.")
    next
  }

  # Check year range for this variable
  valid_years <- data_meps %>%
    filter(!is.na(.data[[var_name]])) %>%
    pull(year) %>%
    unique() %>%
    sort()

  message("  Years with data: ", min(valid_years), "-", max(valid_years))

  # Compute means
  prev_results[[var_name]] <- mean_by_age_year(
    data = data_meps,
    var_name = var_name,
    var_label = var_label,
    survey_name = "MEPS",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    min_n = 50
  )
}

# Combine all results
prev_all <- bind_rows(prev_results)

message("\n========== Prevalence computation complete ==========")
message("Total mean estimates: ", nrow(prev_all))


# ==============================================================================
# SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

# Save combined results
readr::write_csv(
  prev_all,
  file.path(tables_dir, paste0("meps_func_limit_prev_", date_suffix, ".csv"))
)
readr::write_rds(
  prev_all,
  file.path(tables_dir, paste0("meps_func_limit_prev_", date_suffix, ".rds"))
)
message("Saved: meps_func_limit_prev_", date_suffix, " (.csv and .rds)")


# ==============================================================================
# CREATE FIGURE: FACETED BY VARIABLE
# ==============================================================================

message("\n========== Creating prevalence figure ==========\n")

# Create shorter labels for facets
facet_labels <- c(
  "ADDAYA: Moderate Activities\n\"Does your health now limit you in moderate activities,\nsuch as moving a table, pushing a vacuum, bowling, or golf?\"" = "ADDAYA: Limits Moderate Activities\n(0=Not limited, 1=A little, 2=A lot)",
  "ADCLIM: Climbing Stairs\n\"Does your health now limit you in\nclimbing several flights of stairs?\"" = "ADCLIM: Limits Climbing Stairs\n(0=Not limited, 1=A little, 2=A lot)",
  "ANYLMT: Any Limitation\nSummary measure of any IADL, ADL,\nfunctional, or activity limitation" = "ANYLMT: Any Limitation (Summary)\n(1=No, 2=Yes)",
  "ADPAIN: Pain Interference\n\"During the past 4 weeks, how much did pain\ninterfere with your normal work?\"" = "ADPAIN: Pain Interfered with Work\n(0=None to 4=All of the time)",
  "ADPALS: Accomplished Less (Physical)\n\"Past 4 weeks: how much of the time have you accomplished\nless than you would like due to physical health?\"" = "ADPALS: Accomplished Less (Physical)\n(0=None to 4=All of the time)",
  "ADPWLM: Work Limited (Physical)\n\"Past 4 weeks: how much of the time were you limited\nin the kind of work due to physical health?\"" = "ADPWLM: Work Type Limited (Physical)\n(0=None to 4=All of the time)",
  "ADSOCA: Social Activity Interference\n\"Past 4 weeks: how much has physical/emotional health\ninterfered with your social activities?\"" = "ADSOCA: Social Activities Interfered\n(0=None to 4=All of the time)"
)

# Add short label for faceting
prev_all <- prev_all %>%
  mutate(
    facet_label = recode(variable_label, !!!facet_labels),
    facet_label = factor(facet_label, levels = unname(facet_labels))
  )

# Create the faceted figure
# Note: All variables affected by 2017 SF-12 → VR-12 transition
# ANYLMT also affected by Round 5 elimination and sensory questions reintroduced
fig_prev <- ggplot(prev_all, aes(x = year, y = mean,
                                  color = age_group, group = age_group)) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "gray40", alpha = 0.7) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.8) +
  facet_wrap(~ facet_label, scales = "free_y", ncol = 4) +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "MEPS: Prevalence/Mean Trends for Functional Limitation Variables",
    subtitle = "Survey-weighted means by age group and year\nDashed vertical line: 2017 SF-12 → VR-12 instrument change",
    x = "Year",
    y = "Mean Value",
    caption = paste0("Source: MEPS 2000-2023 | Generated: ", Sys.Date())
  ) +
  theme_srh(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40")
  ) +
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# SAVE FIGURE
# ==============================================================================

message("\n========== Saving figure ==========\n")

# Draft version with date
ggsave(
  filename = file.path(output_dir, paste0("meps_func_limit_prev_", date_suffix, ".png")),
  plot = fig_prev,
  width = 16, height = 8, dpi = 300
)

# Final version (overwritten)
ggsave(
  filename = file.path(output_dir, "meps_func_limit_prev.png"),
  plot = fig_prev,
  width = 16, height = 8, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "meps_func_limit_prev.pdf"),
  plot = fig_prev,
  width = 16, height = 8
)

message("Saved: meps_func_limit_prev (.png and .pdf)")


# ==============================================================================
# VERIFICATION SUMMARY
# ==============================================================================

message("\n========== Verification Summary ==========\n")

# Summary of means by variable and age group
prev_summary <- prev_all %>%
  group_by(variable, facet_label) %>%
  summarise(
    n_estimates = n(),
    overall_mean = mean(mean, na.rm = TRUE),
    min_mean = min(mean, na.rm = TRUE),
    max_mean = max(mean, na.rm = TRUE),
    .groups = "drop"
  )

message("Prevalence/Mean Summary:")
print(prev_summary, n = 20)

# Check for expected age gradient (older = more limitations)
age_gradient <- prev_all %>%
  group_by(variable, age_group) %>%
  summarise(
    avg_mean = mean(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = age_group, values_from = avg_mean)

message("\nAge gradient check (expect higher values for older ages):")
print(age_gradient, width = Inf)

message("\n========== MEPS Functional Limitation Prevalence Analysis Complete ==========\n")
