# ==============================================================================
# 06_nhis_functional_limitations_selected_coef.R
# NHIS Functional Limitations: Coefficient Stability (6-panel figure)
#
# Matches 06_nhis_functional_limitations_selected.R but shows coefficients
# from survey-weighted regressions: SRH ~ functional_limitation
# for each year × age group combination.
#
# Selected variables:
#   1. Bathing/Dressing Difficulty (lawashdresdif)
#   2. Climb 12 Steps Difficulty (walkdif12st1)
#   3. Hand/Finger Difficulty (lahanddif)
#   4. Lift/Carry 25lbs Difficulty (lara2litrdif)
#   5. Walk 1 Block Difficulty (walkdif1bl1)
#   6. Walk 5 Blocks Difficulty (walkdif5bl1)
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
source(here("R", "functions", "regress_covariate_by_year.R"))

theme_set(theme_srh())
options(dplyr.summarise.inform = FALSE)

output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")
date_suffix <- format(Sys.Date(), "%Y%m%d")

AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
MIN_YEAR <- 2010

# ------------------------------------------------------------------------------
# SELECTED VARIABLES
# ------------------------------------------------------------------------------

# Use the ordinal versions for regression (not _prev binary versions)
# Higher values = more difficulty, so expect NEGATIVE coefficients on SRH
selected_vars <- c(
  "lawashdresdif"  = "Bathing/Dressing Difficulty",
  "walkdif12st1"   = "Climb 12 Steps Difficulty",
  "lahanddif"      = "Hand/Finger Difficulty",
  "lara2litrdif"   = "Lift/Carry 25lbs Difficulty",
  "walkdif1bl1"    = "Walk 1 Block Difficulty",
  "walkdif5bl1"    = "Walk 5 Blocks Difficulty"
)

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

message("\n========== Loading NHIS data ==========\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

message("NHIS: ", nrow(data_nhis), " rows, years ",
        min(data_nhis$year), "-", max(data_nhis$year))

# Add age group if not present
if (!"age_group" %in% names(data_nhis)) {
  source(here("R", "srh_common_functions.R"))
  data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")
}

data_nhis <- data_nhis %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS))

# Filter to MIN_YEAR+
data_nhis <- data_nhis %>%
  filter(year >= MIN_YEAR)

message("Filtered to ", MIN_YEAR, "+: ", nrow(data_nhis), " rows")

# ------------------------------------------------------------------------------
# RUN AGE-STRATIFIED REGRESSIONS
# ------------------------------------------------------------------------------

message("\n========== Running age-stratified regressions ==========\n")

coef_results <- list()

for (i in seq_along(selected_vars)) {
  var_name <- names(selected_vars)[i]
  var_label <- selected_vars[i]

  message("\n--- Processing: ", var_name, " (", var_label, ") ---")

  if (!var_name %in% names(data_nhis)) {
    message("  Variable not found in data. Skipping.")
    next
  }

  # Check year range for this variable
  valid_years <- data_nhis %>%
    filter(!is.na(.data[[var_name]])) %>%
    pull(year) %>%
    unique() %>%
    sort()

  if (length(valid_years) == 0) {
    message("  No valid data. Skipping.")
    next
  }

  message("  Years with data: ", min(valid_years), "-", max(valid_years))

  # Run regression
  coef_results[[var_name]] <- regress_covariate_by_age_year(
    data = data_nhis,
    covariate_var = var_name,
    covariate_label = var_label,
    survey_name = "NHIS",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    min_n = 50
  )
}

# Combine all results
coef_all <- bind_rows(coef_results)

message("\n========== Regression complete ==========")
message("Total coefficient estimates: ", nrow(coef_all))

# ------------------------------------------------------------------------------
# PREPARE DATA FOR PLOTTING
# ------------------------------------------------------------------------------

coef_all <- coef_all %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    covariate_label = factor(covariate_label, levels = selected_vars)
  )

# ------------------------------------------------------------------------------
# CREATE COEFFICIENT FIGURE (6-panel, 3x2 layout)
# ------------------------------------------------------------------------------

message("\n========== Creating coefficient figure ==========\n")

fig_coef <- ggplot(coef_all, aes(x = year, y = coefficient,
                                  color = age_group, group = age_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.5, alpha = 0.9) +
  facet_wrap(~ covariate_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  labs(
    title = "Coefficient Stability: Functional Limitations on SRH (NHIS)",
    subtitle = "Survey-weighted regression: SRH ~ limitation, by age group and year | 2010-2024",
    x = "Year",
    y = "Coefficient (effect on SRH)",
    caption = paste0("Source: NHIS | Negative = more limitation associated with worse SRH | Generated: ", Sys.Date())
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

# ------------------------------------------------------------------------------
# SAVE FIGURES
# ------------------------------------------------------------------------------

message("\n========== Saving figures ==========\n")

# Draft with date
ggsave(file.path(output_dir, paste0("nhis_limitations_selected_coef_draft_", date_suffix, ".png")),
       fig_coef, width = 12, height = 8, dpi = 300)

# Final versions
ggsave(file.path(output_dir, "nhis_limitations_selected_coef.png"),
       fig_coef, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "nhis_limitations_selected_coef.pdf"),
       fig_coef, width = 12, height = 8)

message("Saved: nhis_limitations_selected_coef.png/.pdf")

# ------------------------------------------------------------------------------
# SAVE TABLES
# ------------------------------------------------------------------------------

message("\n========== Saving tables ==========\n")

readr::write_csv(coef_all,
  file.path(tables_dir, paste0("nhis_limitations_selected_coef_", date_suffix, ".csv")))
readr::write_rds(coef_all,
  file.path(tables_dir, paste0("nhis_limitations_selected_coef_", date_suffix, ".rds")))

message("Saved tables with suffix: ", date_suffix)

# ------------------------------------------------------------------------------
# VERIFICATION SUMMARY
# ------------------------------------------------------------------------------

message("\n========== Verification Summary ==========\n")

# Check coefficient directions (should all be negative - more difficulty = worse SRH)
coef_summary <- coef_all %>%
  group_by(covariate, covariate_label) %>%
  summarise(
    n_estimates = n(),
    year_range = paste0(min(year), "-", max(year)),
    mean_coef = mean(coefficient, na.rm = TRUE),
    min_coef = min(coefficient, na.rm = TRUE),
    max_coef = max(coefficient, na.rm = TRUE),
    pct_negative = mean(coefficient < 0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

message("Coefficient Summary (expect negative values):")
print(coef_summary, n = 10)

# Check for age variation
age_coef_summary <- coef_all %>%
  group_by(covariate_label, age_group) %>%
  summarise(
    avg_coef = round(mean(coefficient, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = age_group, values_from = avg_coef)

message("\nMean coefficient by age group:")
print(age_coef_summary, width = Inf)

message("\n========== Summary ==========\n")
message("Selected 6 variables:")
for (v in names(selected_vars)) {
  message("  - ", selected_vars[v])
}
message("\nYear range: ", MIN_YEAR, "-", max(coef_all$year))
message("Layout: 3 columns x 2 rows (matches prevalence figure)")
message("\nOutput files:")
message("  - nhis_limitations_selected_coef.png/.pdf")

message("\n========== Done ==========\n")
