# ==============================================================================
# 06_meps_functional_limitations_selected.R
# MEPS Functional Limitations: Selected Variables (6-panel figures)
#
# Creates matching prevalence and coefficient figures for MEPS functional
# limitations, styled to match the NHIS selected figures.
#
# Selected variables (excluding stairs):
#   1. ADDAYA: Limits Moderate Activities
#   2. ANYLMT: Any Limitation (Summary)
#   3. ADPAIN: Pain Interfered with Work
#   4. ADPALS: Accomplished Less (Physical)
#   5. ADPWLM: Work Type Limited (Physical)
#   6. ADSOCA: Social Activities Interfered
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

source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "prevalence_by_age_year.R"))
source(here("R", "functions", "regress_covariate_by_year.R"))

theme_set(theme_srh())
options(dplyr.summarise.inform = FALSE)

output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")
date_suffix <- format(Sys.Date(), "%Y%m%d")

AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

# ------------------------------------------------------------------------------
# SELECTED VARIABLES (excluding ADCLIM - stairs)
# ------------------------------------------------------------------------------

selected_vars <- c(
  "ADDAYA" = "Limits Moderate Activities",
  "ANYLMT" = "Any Limitation (Summary)",
  "ADPAIN" = "Pain Interfered with Work",
  "ADPALS" = "Accomplished Less (Physical)",
  "ADPWLM" = "Work Type Limited (Physical)",
  "ADSOCA" = "Social Activities Interfered"
)

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

message("\n========== Loading MEPS data ==========\n")

data_meps <- readr::read_rds(derived_path("data_meps.rds"))

message("MEPS: ", nrow(data_meps), " rows, years ",
        min(data_meps$year), "-", max(data_meps$year))

# Add age group if not present
if (!"age_group" %in% names(data_meps)) {
  source(here("R", "srh_common_functions.R"))
  data_meps <- add_age_group(data_meps, age_var = age, scheme = "B")
}

data_meps <- data_meps %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS))


# ==============================================================================
# PART 1: PREVALENCE/MEAN TRENDS
# ==============================================================================

message("\n========== Computing prevalence/means by age group ==========\n")

prev_results <- list()

for (i in seq_along(selected_vars)) {
  var_name <- names(selected_vars)[i]
  var_label <- selected_vars[i]

  message("\n--- Processing prevalence: ", var_name, " ---")

  if (!var_name %in% names(data_meps)) {
    message("  Variable not found in data. Skipping.")
    next
  }

  valid_years <- data_meps %>%
    filter(!is.na(.data[[var_name]])) %>%
    pull(year) %>%
    unique() %>%
    sort()

  if (length(valid_years) == 0) {
    message("  No valid data. Skipping.")
    next
  }

  message("  Years with data: ", min(valid_years), "-", max(valid_years))

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

prev_all <- bind_rows(prev_results)

message("\nPrevalence estimates: ", nrow(prev_all))

# Prepare for plotting
prev_all <- prev_all %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    variable_label = factor(variable_label, levels = selected_vars)
  )


# ==============================================================================
# PART 2: COEFFICIENT STABILITY
# ==============================================================================

message("\n========== Running age-stratified regressions ==========\n")

coef_results <- list()

for (i in seq_along(selected_vars)) {
  var_name <- names(selected_vars)[i]
  var_label <- selected_vars[i]

  message("\n--- Processing coefficients: ", var_name, " ---")

  if (!var_name %in% names(data_meps)) {
    message("  Variable not found in data. Skipping.")
    next
  }

  valid_years <- data_meps %>%
    filter(!is.na(.data[[var_name]])) %>%
    pull(year) %>%
    unique() %>%
    sort()

  if (length(valid_years) == 0) {
    message("  No valid data. Skipping.")
    next
  }

  message("  Years with data: ", min(valid_years), "-", max(valid_years))

  coef_results[[var_name]] <- regress_covariate_by_age_year(
    data = data_meps,
    covariate_var = var_name,
    covariate_label = var_label,
    survey_name = "MEPS",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    min_n = 50
  )
}

coef_all <- bind_rows(coef_results)

message("\nCoefficient estimates: ", nrow(coef_all))

# Prepare for plotting
coef_all <- coef_all %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    covariate_label = factor(covariate_label, levels = selected_vars)
  )


# ==============================================================================
# CREATE PREVALENCE FIGURE (6-panel, 3x2 layout)
# ==============================================================================

message("\n========== Creating prevalence figure ==========\n")

fig_prev <- ggplot(prev_all, aes(x = year, y = mean,
                                  color = age_group, group = age_group)) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "gray40", alpha = 0.7) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.5, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "Prevalence of Functional Limitations by Age Group Over Time (MEPS)",
    subtitle = "Survey-weighted means | Dashed line: 2017 SF-12 to VR-12 change",
    x = "Year",
    y = "Mean Value",
    caption = paste0("Source: MEPS | Generated: ", Sys.Date())
  ) +
  theme_srh(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# CREATE COEFFICIENT FIGURE (6-panel, 3x2 layout)
# ==============================================================================

message("\n========== Creating coefficient figure ==========\n")

fig_coef <- ggplot(coef_all, aes(x = year, y = coefficient,
                                  color = age_group, group = age_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "gray40", alpha = 0.7) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.5, alpha = 0.9) +
  facet_wrap(~ covariate_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "Coefficient Stability: Functional Limitations on SRH (MEPS)",
    subtitle = "Survey-weighted regression: SRH ~ limitation | Dashed line: 2017 instrument change",
    x = "Year",
    y = "Coefficient (effect on SRH)",
    caption = paste0("Source: MEPS | Negative = more limitation associated with worse SRH | Generated: ", Sys.Date())
  ) +
  theme_srh(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# SAVE FIGURES
# ==============================================================================

message("\n========== Saving figures ==========\n")

# Prevalence
ggsave(file.path(output_dir, paste0("meps_limitations_selected_draft_", date_suffix, ".png")),
       fig_prev, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "meps_limitations_selected.png"),
       fig_prev, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "meps_limitations_selected.pdf"),
       fig_prev, width = 12, height = 8)
message("Saved: meps_limitations_selected.png/.pdf (prevalence)")

# Coefficients
ggsave(file.path(output_dir, paste0("meps_limitations_selected_coef_draft_", date_suffix, ".png")),
       fig_coef, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "meps_limitations_selected_coef.png"),
       fig_coef, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "meps_limitations_selected_coef.pdf"),
       fig_coef, width = 12, height = 8)
message("Saved: meps_limitations_selected_coef.png/.pdf (coefficients)")


# ==============================================================================
# SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

# Prevalence
readr::write_csv(prev_all,
  file.path(tables_dir, paste0("meps_limitations_selected_prev_", date_suffix, ".csv")))
readr::write_rds(prev_all,
  file.path(tables_dir, paste0("meps_limitations_selected_prev_", date_suffix, ".rds")))

# Coefficients
readr::write_csv(coef_all,
  file.path(tables_dir, paste0("meps_limitations_selected_coef_", date_suffix, ".csv")))
readr::write_rds(coef_all,
  file.path(tables_dir, paste0("meps_limitations_selected_coef_", date_suffix, ".rds")))

message("Saved tables with suffix: ", date_suffix)


# ==============================================================================
# VERIFICATION SUMMARY
# ==============================================================================

message("\n========== Verification Summary ==========\n")

# Prevalence summary
prev_summary <- prev_all %>%
  group_by(variable, variable_label) %>%
  summarise(
    n_estimates = n(),
    year_range = paste0(min(year), "-", max(year)),
    overall_mean = round(mean(mean, na.rm = TRUE), 3),
    .groups = "drop"
  )

message("Prevalence Summary:")
print(prev_summary, n = 10)

# Coefficient summary
coef_summary <- coef_all %>%
  group_by(covariate, covariate_label) %>%
  summarise(
    n_estimates = n(),
    year_range = paste0(min(year), "-", max(year)),
    mean_coef = round(mean(coefficient, na.rm = TRUE), 3),
    pct_negative = round(mean(coefficient < 0, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

message("\nCoefficient Summary (expect negative values):")
print(coef_summary, n = 10)


# ==============================================================================
# SUMMARY
# ==============================================================================

message("\n========== Summary ==========\n")
message("Selected 6 variables (excluding ADCLIM stairs):")
for (v in names(selected_vars)) {
  message("  - ", v, ": ", selected_vars[v])
}
message("\nYear range: ", min(prev_all$year), "-", max(prev_all$year))
message("Layout: 3 columns x 2 rows (matches NHIS selected figures)")
message("\nOutput files:")
message("  - meps_limitations_selected.png/.pdf (prevalence)")
message("  - meps_limitations_selected_coef.png/.pdf (coefficients)")

message("\n========== Done ==========\n")
