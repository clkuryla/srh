# ==============================================================================
# 06_nhis_functional_limitations_coef_combined.R
# NHIS Functional Limitations: Combined Coefficient Plot
#
# Runs age-stratified regressions of SRH ~ functional limitation for all 15
# variables and creates a combined 5-column figure matching the prevalence plot.
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

# Output directories
output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# Date suffix
date_suffix <- format(Sys.Date(), "%Y%m%d")

# Age group levels
AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

# Year filter
MIN_YEAR <- 2010


# ==============================================================================
# VARIABLE DEFINITIONS (same as prevalence plot)
# ==============================================================================

# All functional limitation variables with short labels
all_func_vars <- c(
  # Part 1: General Limitations
  "lawashdresdif" = "Bathing/Dressing Difficulty",
  "lacomdifego"   = "Communication Difficulty",
  "lamemcondif"   = "Memory/Concentration Difficulty",
  "lawalkclimdif" = "Walking/Climbing Difficulty",
  "lamtwrk"       = "Work Limitation",
  # Part 2: Physical Limitations
  "walkdif12st1"  = "Climb 12 Steps Difficulty",
  "lahanddif"     = "Hand/Finger Difficulty",
  "lara2litrdif"  = "Lift/Carry 25lbs Difficulty",
  "walkdif1bl1"   = "Walk 1 Block Difficulty",
  "walkdif5bl1"   = "Walk 5 Blocks Difficulty",
  # Part 3: Memory & Other
  "lamemorcon"    = "Memory Condition",
  "lamemdifamt"   = "Memory Problems (Some/A lot)",
  "walkdif1bl2"   = "Walk 1 Block (alternate)",
  "lawalkclimper" = "Walk/Climb Period Limitation",
  "lawalkclimperq"= "Walk/Climb Permanent Limitation"
)


# ==============================================================================
# LOAD DATA
# ==============================================================================

message("\n========== Loading NHIS data ==========\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

message("NHIS: ", nrow(data_nhis), " rows, years ",
        min(data_nhis$year), "-", max(data_nhis$year))

# Add age group if not present
if (!"age_group" %in% names(data_nhis)) {
  source(here("R", "srh_common_functions.R"))
  data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")
}

# Filter to 2010+ and ensure age_group is factor
data_nhis <- data_nhis %>%
  filter(year >= MIN_YEAR) %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS))

message("After filtering to ", MIN_YEAR, "+: ", nrow(data_nhis), " rows")


# ==============================================================================
# RUN AGE-STRATIFIED REGRESSIONS
# ==============================================================================

message("\n========== Running age-stratified regressions ==========\n")
message("Model: SRH ~ functional_limitation (for each age group × year)")

coef_results <- list()

for (i in seq_along(all_func_vars)) {
  var_name <- names(all_func_vars)[i]
  var_label <- all_func_vars[i]

  message("\n--- ", i, "/", length(all_func_vars), ": ", var_name, " ---")

  # Check variable exists
  if (!var_name %in% names(data_nhis)) {
    message("  Variable not found. Skipping.")
    next
  }

  # Check for valid data
  n_valid <- sum(!is.na(data_nhis[[var_name]]))
  if (n_valid == 0) {
    message("  No valid data. Skipping.")
    next
  }

  # Run age-stratified regression
  result <- regress_covariate_by_age_year(
    data = data_nhis,
    covariate_var = var_name,
    covariate_label = var_label,
    survey_name = "NHIS",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    min_n = 50
  )

  if (!is.null(result) && nrow(result) > 0) {
    coef_results[[var_name]] <- result
  }
}

# Combine all results
coef_all <- bind_rows(coef_results)

message("\n========== Regression complete ==========")
message("Total coefficient estimates: ", nrow(coef_all))
message("Variables with results: ", length(unique(coef_all$covariate)))


# ==============================================================================
# SAVE COEFFICIENT TABLE
# ==============================================================================

message("\n========== Saving coefficient table ==========\n")

readr::write_csv(
  coef_all,
  file.path(tables_dir, paste0("nhis_limitations_coef_combined_", date_suffix, ".csv"))
)
message("Saved: nhis_limitations_coef_combined_", date_suffix, ".csv")


# ==============================================================================
# CREATE COMBINED COEFFICIENT FIGURE
# ==============================================================================

message("\n========== Creating combined coefficient figure ==========\n")

# Ensure ordering matches prevalence plot
coef_all <- coef_all %>%
  mutate(covariate_label = factor(covariate_label, levels = all_func_vars))

fig_coef_combined <- ggplot(coef_all, aes(x = year, y = coefficient,
                                           color = age_group, group = age_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.9) +
  facet_wrap(~ covariate_label, ncol = 5, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  labs(
    title = "Coefficient of Functional Limitations on SRH by Age Group Over Time (NHIS)",
    subtitle = "Survey-weighted regression: SRH ~ limitation | Negative = worse health | 2010-2024",
    x = "Year",
    y = "Coefficient (effect on SRH)",
    caption = paste0("Source: NHIS | Generated: ", Sys.Date())
  ) +
  theme_srh(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7)
  ) +
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# SAVE FIGURE
# ==============================================================================

message("\n========== Saving figure ==========\n")

ggsave(file.path(output_dir, "nhis_limitations_coef_combined.png"),
       fig_coef_combined, width = 14, height = 9, dpi = 300)
ggsave(file.path(output_dir, "nhis_limitations_coef_combined.pdf"),
       fig_coef_combined, width = 14, height = 9)

message("Saved: nhis_limitations_coef_combined.png and .pdf")


# ==============================================================================
# SUMMARY
# ==============================================================================

message("\n========== Summary ==========\n")

# Coefficient summary by variable
coef_summary <- coef_all %>%
  group_by(covariate_label) %>%
  summarise(
    n_estimates = n(),
    mean_coef = mean(coefficient, na.rm = TRUE),
    min_coef = min(coefficient, na.rm = TRUE),
    max_coef = max(coefficient, na.rm = TRUE),
    .groups = "drop"
  )

message("Coefficient summary (expect negative values = limitation predicts worse SRH):\n")
print(coef_summary, n = 20)

message("\nOkabe-Ito color palette applied:")
message("  18-29: #D55E00 (vermillion)")
message("  30-39: #E69F00 (orange)")
message("  40-49: #F0E442 (yellow)")
message("  50-59: #009E73 (bluish green)")
message("  60-69: #56B4E9 (sky blue)")
message("  70-79: #0072B2 (blue)")
message("  80-89: #CC79A7 (reddish purple)")

message("\n========== Done ==========\n")
