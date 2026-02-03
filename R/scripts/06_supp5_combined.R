# ==============================================================================
# 06_supp5_combined.R
# Supplementary Figure 5: Combined 4-panel figure
#
# Layout: Row 1: A (NHIS prev), B (NHIS coef)
#         Row 2: C (MEPS prev), D (MEPS coef)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(tidyverse)
library(survey)
library(srvyr)
library(here)
library(patchwork)

source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "prevalence_by_age_year.R"))
source(here("R", "functions", "regress_covariate_by_year.R"))

theme_set(theme_srh())
options(dplyr.summarise.inform = FALSE)

output_dir <- here("output", "figures")
date_suffix <- format(Sys.Date(), "%Y%m%d")

AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

# ------------------------------------------------------------------------------
# VARIABLE DEFINITIONS
# ------------------------------------------------------------------------------

nhis_vars_prev <- c(

  "lawashdresdif_prev"  = "Bathing/Dressing Difficulty",
  "walkdif12st1_prev"   = "Climb 12 Steps Difficulty",
  "lahanddif_prev"      = "Hand/Finger Difficulty",
  "lara2litrdif_prev"   = "Lift/Carry 25lbs Difficulty",
  "walkdif1bl1_prev"    = "Walk 1 Block Difficulty",
  "lawalkclimperq_prev" = "Walk/Climb Permanent Limitation"
)

nhis_vars_coef <- c(
  "lawashdresdif"  = "Bathing/Dressing Difficulty",
  "walkdif12st1"   = "Climb 12 Steps Difficulty",
  "lahanddif"      = "Hand/Finger Difficulty",
  "lara2litrdif"   = "Lift/Carry 25lbs Difficulty",
  "walkdif1bl1"    = "Walk 1 Block Difficulty",
  "lawalkclimperq" = "Walk/Climb Permanent Limitation"
)

meps_vars <- c(
  "ADDAYA" = "Limits Moderate Activities",
  "ANYLMT" = "Any Limitation (Summary)",
  "ADPAIN" = "Pain Interfered with Work",
  "ADPALS" = "Accomplished Less (Physical)",
  "ADPWLM" = "Work Type Limited (Physical)",
  "ADSOCA" = "Social Activities Interfered"
)

# ------------------------------------------------------------------------------
# LOAD AND PREPARE DATA
# ------------------------------------------------------------------------------

message("\n========== Loading data ==========\n")

# NHIS
data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))
if (!"age_group" %in% names(data_nhis)) {
  source(here("R", "srh_common_functions.R"))
  data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")
}
data_nhis <- data_nhis %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS)) %>%
  filter(year >= 2010)

# Create binary prevalence versions
ordinal_vars <- c("lawashdresdif", "walkdif12st1", "lahanddif", "lara2litrdif", "walkdif1bl1")
for (v in ordinal_vars) {
  if (v %in% names(data_nhis)) {
    new_var <- paste0(v, "_prev")
    data_nhis[[new_var]] <- ifelse(data_nhis[[v]] > 1, 1,
                                    ifelse(data_nhis[[v]] == 1, 0, NA_real_))
  }
}
if ("lawalkclimperq" %in% names(data_nhis)) {
  data_nhis$lawalkclimperq_prev <- data_nhis$lawalkclimperq
}

# MEPS
data_meps <- readr::read_rds(derived_path("data_meps.rds"))
if (!"age_group" %in% names(data_meps)) {
  source(here("R", "srh_common_functions.R"))
  data_meps <- add_age_group(data_meps, age_var = age, scheme = "B")
}
data_meps <- data_meps %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS))

# ------------------------------------------------------------------------------
# COMPUTE ALL ESTIMATES
# ------------------------------------------------------------------------------

message("\n========== Computing estimates ==========\n")

# NHIS Prevalence
nhis_prev_results <- list()
for (i in seq_along(nhis_vars_prev)) {
  var_name <- names(nhis_vars_prev)[i]
  var_label <- nhis_vars_prev[i]
  if (!var_name %in% names(data_nhis)) next
  nhis_prev_results[[var_name]] <- mean_by_age_year(
    data = data_nhis, var_name = var_name, var_label = var_label,
    survey_name = "NHIS", psu_var = "psu", strata_var = "strata", wt_var = "wt", min_n = 50
  )
}
nhis_prev <- bind_rows(nhis_prev_results) %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS),
         variable_label = factor(variable_label, levels = nhis_vars_prev))

# NHIS Coefficients
nhis_coef_results <- list()
for (i in seq_along(nhis_vars_coef)) {
  var_name <- names(nhis_vars_coef)[i]
  var_label <- nhis_vars_coef[i]
  if (!var_name %in% names(data_nhis)) next
  nhis_coef_results[[var_name]] <- regress_covariate_by_age_year(
    data = data_nhis, covariate_var = var_name, covariate_label = var_label,
    survey_name = "NHIS", psu_var = "psu", strata_var = "strata", wt_var = "wt", min_n = 50
  )
}
nhis_coef <- bind_rows(nhis_coef_results) %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS),
         covariate_label = factor(covariate_label, levels = nhis_vars_coef))

# MEPS Prevalence
meps_prev_results <- list()
for (i in seq_along(meps_vars)) {
  var_name <- names(meps_vars)[i]
  var_label <- meps_vars[i]
  if (!var_name %in% names(data_meps)) next
  meps_prev_results[[var_name]] <- mean_by_age_year(
    data = data_meps, var_name = var_name, var_label = var_label,
    survey_name = "MEPS", psu_var = "psu", strata_var = "strata", wt_var = "wt", min_n = 50
  )
}
meps_prev <- bind_rows(meps_prev_results) %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS),
         variable_label = factor(variable_label, levels = meps_vars))

# MEPS Coefficients
meps_coef_results <- list()
for (i in seq_along(meps_vars)) {
  var_name <- names(meps_vars)[i]
  var_label <- meps_vars[i]
  if (!var_name %in% names(data_meps)) next
  meps_coef_results[[var_name]] <- regress_covariate_by_age_year(
    data = data_meps, covariate_var = var_name, covariate_label = var_label,
    survey_name = "MEPS", psu_var = "psu", strata_var = "strata", wt_var = "wt", min_n = 50
  )
}
meps_coef <- bind_rows(meps_coef_results) %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS),
         covariate_label = factor(covariate_label, levels = meps_vars))

# ------------------------------------------------------------------------------
# CREATE INDIVIDUAL PANELS (without A/B/C/D in titles)
# ------------------------------------------------------------------------------

message("\n========== Creating panels ==========\n")

# Common theme for combined figure (slightly smaller for fitting)
theme_combined <- function() {
  theme_srh(base_size = 11) +
  theme(
    legend.position = "none",  # Will add shared legend
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(5, 10, 5, 5)
  )
}

# Panel A: NHIS Prevalence
panel_a <- ggplot(nhis_prev, aes(x = year, y = mean, color = age_group, group = age_group)) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    title = "NHIS: Prevalence of Functional Limitations",
    subtitle = "% with any difficulty | 2010-2024",
    x = "Year",
    y = "Prevalence"
  ) +
  theme_combined()

# Panel B: NHIS Coefficients
panel_b <- ggplot(nhis_coef, aes(x = year, y = coefficient, color = age_group, group = age_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.9) +
  facet_wrap(~ covariate_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  labs(
    title = "NHIS: Coefficient Stability",
    subtitle = "SRH ~ limitation | 2010-2024",
    x = "Year",
    y = "Coefficient"
  ) +
  theme_combined()

# Panel C: MEPS Prevalence
panel_c <- ggplot(meps_prev, aes(x = year, y = mean, color = age_group, group = age_group)) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "gray40", alpha = 0.7) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "MEPS: Prevalence of Functional Limitations",
    subtitle = "Mean values | Dashed line: 2017 instrument change",
    x = "Year",
    y = "Mean Value"
  ) +
  theme_combined()

# Panel D: MEPS Coefficients
panel_d <- ggplot(meps_coef, aes(x = year, y = coefficient, color = age_group, group = age_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "gray40", alpha = 0.7) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.9) +
  facet_wrap(~ covariate_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "MEPS: Coefficient Stability",
    subtitle = "SRH ~ limitation | Dashed line: 2017 instrument change",
    x = "Year",
    y = "Coefficient"
  ) +
  theme_combined()

# ------------------------------------------------------------------------------
# COMBINE WITH PATCHWORK
# ------------------------------------------------------------------------------

message("\n========== Combining panels ==========\n")

# Combine: Row 1 = A, B; Row 2 = C, D
combined <- (panel_a | panel_b) / (panel_c | panel_d) +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10, face = "bold"))

# ------------------------------------------------------------------------------
# SAVE
# ------------------------------------------------------------------------------

message("\n========== Saving combined figure ==========\n")

ggsave(file.path(output_dir, paste0("supp5_combined_draft_", date_suffix, ".png")),
       combined, width = 16, height = 14, dpi = 300)
ggsave(file.path(output_dir, "supp5_combined.png"),
       combined, width = 16, height = 14, dpi = 300)
ggsave(file.path(output_dir, "supp5_combined.pdf"),
       combined, width = 16, height = 14)

message("Saved: supp5_combined.png/.pdf")
message("\n========== Done ==========\n")
