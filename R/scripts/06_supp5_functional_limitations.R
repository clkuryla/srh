# ==============================================================================
# 06_supp5_functional_limitations.R
# Supplementary Figure 5: Functional Limitations (NHIS & MEPS)
#
# Generates 4 panels with larger fonts for publication:
#   supp5_a: NHIS prevalence (6 variables)
#   supp5_b: NHIS coefficients (6 variables)
#   supp5_c: MEPS prevalence (6 variables)
#   supp5_d: MEPS coefficients (6 variables)
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
# NHIS SELECTED VARIABLES
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

# ------------------------------------------------------------------------------
# MEPS SELECTED VARIABLES (excluding stairs)
# ------------------------------------------------------------------------------

meps_vars <- c(
  "ADDAYA" = "Limits Moderate Activities",
  "ANYLMT" = "Any Limitation (Summary)",
  "ADPAIN" = "Pain Interfered with Work",
  "ADPALS" = "Accomplished Less (Physical)",
  "ADPWLM" = "Work Type Limited (Physical)",
  "ADSOCA" = "Social Activities Interfered"
)


# ==============================================================================
# LOAD NHIS DATA
# ==============================================================================

message("\n========== Loading NHIS data ==========\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

message("NHIS: ", nrow(data_nhis), " rows, years ",
        min(data_nhis$year), "-", max(data_nhis$year))

if (!"age_group" %in% names(data_nhis)) {
  source(here("R", "srh_common_functions.R"))
  data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")
}

data_nhis <- data_nhis %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS)) %>%
  filter(year >= 2010)

# Create binary versions for prevalence
ordinal_vars <- c("lawashdresdif", "walkdif12st1", "lahanddif",
                  "lara2litrdif", "walkdif1bl1")

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


# ==============================================================================
# LOAD MEPS DATA
# ==============================================================================

message("\n========== Loading MEPS data ==========\n")

data_meps <- readr::read_rds(derived_path("data_meps.rds"))

message("MEPS: ", nrow(data_meps), " rows, years ",
        min(data_meps$year), "-", max(data_meps$year))

if (!"age_group" %in% names(data_meps)) {
  source(here("R", "srh_common_functions.R"))
  data_meps <- add_age_group(data_meps, age_var = age, scheme = "B")
}

data_meps <- data_meps %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS))


# ==============================================================================
# COMPUTE NHIS PREVALENCE
# ==============================================================================

message("\n========== NHIS: Computing prevalence ==========\n")

nhis_prev_results <- list()
for (i in seq_along(nhis_vars_prev)) {
  var_name <- names(nhis_vars_prev)[i]
  var_label <- nhis_vars_prev[i]

  if (!var_name %in% names(data_nhis)) next

  nhis_prev_results[[var_name]] <- mean_by_age_year(
    data = data_nhis,
    var_name = var_name,
    var_label = var_label,
    survey_name = "NHIS",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    min_n = 50
  )
}

nhis_prev <- bind_rows(nhis_prev_results) %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    variable_label = factor(variable_label, levels = nhis_vars_prev)
  )


# ==============================================================================
# COMPUTE NHIS COEFFICIENTS
# ==============================================================================

message("\n========== NHIS: Computing coefficients ==========\n")

nhis_coef_results <- list()
for (i in seq_along(nhis_vars_coef)) {
  var_name <- names(nhis_vars_coef)[i]
  var_label <- nhis_vars_coef[i]

  if (!var_name %in% names(data_nhis)) next

  nhis_coef_results[[var_name]] <- regress_covariate_by_age_year(
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

nhis_coef <- bind_rows(nhis_coef_results) %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    covariate_label = factor(covariate_label, levels = nhis_vars_coef)
  )


# ==============================================================================
# COMPUTE MEPS PREVALENCE
# ==============================================================================

message("\n========== MEPS: Computing prevalence ==========\n")

meps_prev_results <- list()
for (i in seq_along(meps_vars)) {
  var_name <- names(meps_vars)[i]
  var_label <- meps_vars[i]

  if (!var_name %in% names(data_meps)) next

  meps_prev_results[[var_name]] <- mean_by_age_year(
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

meps_prev <- bind_rows(meps_prev_results) %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    variable_label = factor(variable_label, levels = meps_vars)
  )


# ==============================================================================
# COMPUTE MEPS COEFFICIENTS
# ==============================================================================

message("\n========== MEPS: Computing coefficients ==========\n")

meps_coef_results <- list()
for (i in seq_along(meps_vars)) {
  var_name <- names(meps_vars)[i]
  var_label <- meps_vars[i]

  if (!var_name %in% names(data_meps)) next

  meps_coef_results[[var_name]] <- regress_covariate_by_age_year(
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

meps_coef <- bind_rows(meps_coef_results) %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    covariate_label = factor(covariate_label, levels = meps_vars)
  )


# ==============================================================================
# CREATE FIGURES WITH LARGER FONTS
# ==============================================================================

message("\n========== Creating figures with larger fonts ==========\n")

# Common theme adjustments for larger fonts
theme_supp <- function() {
  theme_srh(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    plot.caption = element_text(size = 10),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
}

# --- SUPP5_A: NHIS Prevalence ---
fig_supp5_a <- ggplot(nhis_prev, aes(x = year, y = mean,
                                      color = age_group, group = age_group)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    title = "A. NHIS: Prevalence of Functional Limitations by Age Group",
    subtitle = "Survey-weighted estimates | % with any difficulty | 2010-2024",
    x = "Year",
    y = "Prevalence (% with any difficulty)"
  ) +
  theme_supp() +
  guides(color = guide_legend(nrow = 1))


# --- SUPP5_B: NHIS Coefficients ---
fig_supp5_b <- ggplot(nhis_coef, aes(x = year, y = coefficient,
                                      color = age_group, group = age_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~ covariate_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  labs(
    title = "B. NHIS: Coefficient Stability for Functional Limitations",
    subtitle = "Survey-weighted regression: SRH ~ limitation | 2010-2024",
    x = "Year",
    y = "Coefficient (effect on SRH)"
  ) +
  theme_supp() +
  guides(color = guide_legend(nrow = 1))


# --- SUPP5_C: MEPS Prevalence ---
fig_supp5_c <- ggplot(meps_prev, aes(x = year, y = mean,
                                      color = age_group, group = age_group)) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "gray40", alpha = 0.7) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "C. MEPS: Prevalence of Functional Limitations by Age Group",
    subtitle = "Survey-weighted means | Dashed line: 2017 SF-12 to VR-12 change",
    x = "Year",
    y = "Mean Value"
  ) +
  theme_supp() +
  guides(color = guide_legend(nrow = 1))


# --- SUPP5_D: MEPS Coefficients ---
fig_supp5_d <- ggplot(meps_coef, aes(x = year, y = coefficient,
                                      color = age_group, group = age_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "gray40", alpha = 0.7) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~ covariate_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "D. MEPS: Coefficient Stability for Functional Limitations",
    subtitle = "Survey-weighted regression: SRH ~ limitation | Dashed line: 2017 instrument change",
    x = "Year",
    y = "Coefficient (effect on SRH)"
  ) +
  theme_supp() +
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# SAVE FIGURES
# ==============================================================================

message("\n========== Saving figures ==========\n")

# Supp5_a: NHIS Prevalence
ggsave(file.path(output_dir, paste0("supp5_a_draft_", date_suffix, ".png")),
       fig_supp5_a, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "supp5_a.png"),
       fig_supp5_a, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "supp5_a.pdf"),
       fig_supp5_a, width = 12, height = 8)
message("Saved: supp5_a (NHIS prevalence)")

# Supp5_b: NHIS Coefficients
ggsave(file.path(output_dir, paste0("supp5_b_draft_", date_suffix, ".png")),
       fig_supp5_b, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "supp5_b.png"),
       fig_supp5_b, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "supp5_b.pdf"),
       fig_supp5_b, width = 12, height = 8)
message("Saved: supp5_b (NHIS coefficients)")

# Supp5_c: MEPS Prevalence
ggsave(file.path(output_dir, paste0("supp5_c_draft_", date_suffix, ".png")),
       fig_supp5_c, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "supp5_c.png"),
       fig_supp5_c, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "supp5_c.pdf"),
       fig_supp5_c, width = 12, height = 8)
message("Saved: supp5_c (MEPS prevalence)")

# Supp5_d: MEPS Coefficients
ggsave(file.path(output_dir, paste0("supp5_d_draft_", date_suffix, ".png")),
       fig_supp5_d, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "supp5_d.png"),
       fig_supp5_d, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "supp5_d.pdf"),
       fig_supp5_d, width = 12, height = 8)
message("Saved: supp5_d (MEPS coefficients)")


# ==============================================================================
# SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

readr::write_csv(nhis_prev,
  file.path(tables_dir, paste0("supp5_a_nhis_prev_", date_suffix, ".csv")))
readr::write_csv(nhis_coef,
  file.path(tables_dir, paste0("supp5_b_nhis_coef_", date_suffix, ".csv")))
readr::write_csv(meps_prev,
  file.path(tables_dir, paste0("supp5_c_meps_prev_", date_suffix, ".csv")))
readr::write_csv(meps_coef,
  file.path(tables_dir, paste0("supp5_d_meps_coef_", date_suffix, ".csv")))

message("Saved tables")


# ==============================================================================
# SUMMARY
# ==============================================================================

message("\n========== Summary ==========\n")
message("Supplementary Figure 5: Functional Limitations")
message("")
message("  supp5_a: NHIS prevalence (", nrow(nhis_prev), " estimates)")
message("  supp5_b: NHIS coefficients (", nrow(nhis_coef), " estimates)")
message("  supp5_c: MEPS prevalence (", nrow(meps_prev), " estimates)")
message("  supp5_d: MEPS coefficients (", nrow(meps_coef), " estimates)")
message("")
message("Font sizes increased: base=14, strip=13, title=16, axis=11-13")

message("\n========== Done ==========\n")
