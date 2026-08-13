# ==============================================================================
# 26_combo_effects_figures.R
# Create combination figures for period and cohort effects across surveys
# Author: Christine Lucille Kuryla
# ==============================================================================
#
# This script creates:
# 1. Per-survey combo figures (period + cohort side-by-side):
#    - GSS: SRH, Education, Happiness
#    - NHIS: SRH, Education, K6
#    - MEPS: SRH, Education, K6
#
# 2. Cross-survey combo figures:
#    - All surveys' cohort effects (GSS, NHIS, MEPS)
#    - All surveys' period effects (GSS, NHIS, MEPS)
#
# ==============================================================================

library(tidyverse)
library(patchwork)
library(here)
library(gssr)

# Source functions
source(here::here("R/functions/bhapc_model_fitting.R"))
source(here::here("R/functions/bhapc_figure_generation.R"))
source(here::here("R/paths.R"))

# ==============================================================================
# Helper functions (shared across all surveys)
# ==============================================================================

map_to_period_4yr <- function(year, survey = "nhis") {
  period <- as.integer(floor((year - 1999) / 4) * 4 + 1999)
  period <- ifelse(year < 1999,
                   as.integer(floor((year - 1995) / 4) * 4 + 1995),
                   period)
  period
}

create_age_groups_mixed <- function(age) {
  age_group <- case_when(
    age >= 18 & age <= 21 ~ "18-21",
    age >= 22 & age <= 25 ~ "22-25",
    age >= 26 & age <= 29 ~ "26-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    TRUE ~ NA_character_
  )
  levels <- c("18-21", "22-25", "26-29", "30-34", "35-39", "40-44",
              "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
              "75-79", "80-84", "85-89")
  factor(age_group, levels = levels)
}

get_age_midpoint <- function(age_group) {
  bounds <- strsplit(as.character(age_group), "-")
  sapply(bounds, function(b) {
    if (length(b) == 2) {
      (as.numeric(b[1]) + as.numeric(b[2])) / 2
    } else {
      NA_real_
    }
  })
}

prepare_sanity_check_data <- function(df, outcome_var, survey = "nhis") {
  df %>%
    filter(!is.na(.data[[outcome_var]]), !is.na(age), !is.na(wt)) %>%
    filter(age >= 18, age <= 89) %>%
    mutate(
      period_4yr = map_to_period_4yr(year, survey),
      age_group = create_age_groups_mixed(age),
      age_midpoint = get_age_midpoint(age_group)
    ) %>%
    filter(!is.na(period_4yr), !is.na(age_group)) %>%
    mutate(
      cohort_4yr = round((period_4yr - age_midpoint) / 4) * 4,
      age_squared = age^2,
      lnWt = ifelse(wt == 0 | is.na(wt), 0, log(wt)),
      period_4yr = as.character(period_4yr),
      cohort_4yr = as.character(cohort_4yr)
    ) %>%
    filter(!is.na(cohort_4yr))
}

# Extract effects and standardize
extract_cohort_effects <- function(result, bhapc_df, outcome_label) {
  model <- result$model
  random_effects <- extract_random_effects(model, bhapc_df)

  cohort_effects <- random_effects$cohort_effects %>%
    mutate(
      outcome = outcome_label,
      estimate_std = estimate / sd(estimate),
      ci_lower_std = ci_lower_90 / sd(estimate),
      ci_upper_std = ci_upper_90 / sd(estimate)
    )

  cohort_effects
}

extract_period_effects <- function(result, bhapc_df, outcome_label) {
  model <- result$model
  random_effects <- extract_random_effects(model, bhapc_df)

  period_effects <- random_effects$period_effects %>%
    mutate(
      outcome = outcome_label,
      estimate_std = estimate / sd(estimate),
      ci_lower_std = ci_lower_90 / sd(estimate),
      ci_upper_std = ci_upper_90 / sd(estimate)
    )

  period_effects
}

get_pct <- function(var_df, component) {
  round(var_df$pct_of_total[var_df$component == component], 2)
}

# Colors for outcomes (consistent across figures)
outcome_colors <- c(
  "SRH" = "#0072B2",
  "Education" = "#E69F00",
  "K6" = "#D55E00",
  "Happiness" = "#56B4E9"
)

# Survey colors for cross-survey figures
survey_colors <- c(
  "GSS" = "#009E73",
  "NHIS" = "#CC79A7",
  "MEPS" = "#F0E442"
)

# ==============================================================================
# Load all data and models
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Loading all data and models")
message(paste(rep("=", 70), collapse = ""))

# --- GSS ---
message("\nLoading GSS...")
data("gss_all")
data_gss <- gss_all %>%
  haven::zap_labels() %>%
  select(year, cohort, age, health, sex, happy, educ, wtssps, wtssall) %>%
  filter(cohort != 9999) %>%
  filter(!is.na(age)) %>%
  filter(age >= 18) %>%
  filter(health %in% 1:4) %>%
  mutate(
    srh = 5 - health,
    happy = if_else(happy %in% 1:3, 4L - as.integer(happy), NA_integer_)
  ) %>%
  mutate(wt = coalesce(wtssall, wtssps)) %>%
  filter(!is.na(wt))
rm(gss_all)

gss_bhapc_srh <- prepare_sanity_check_data(data_gss, "srh", "gss")
gss_bhapc_educ <- prepare_sanity_check_data(data_gss, "educ", "gss")
gss_bhapc_happy <- prepare_sanity_check_data(data_gss, "happy", "gss")

gss_result_srh <- readRDS(here::here("output/bhapc_parallel/gss/gss_bhapc_model.rds"))
gss_result_educ <- readRDS(here::here("output/sanity_check_bhapc/result_educ.rds"))
gss_result_happy <- readRDS(here::here("output/sanity_check_bhapc/result_happy.rds"))

gss_var_srh <- read_csv(here::here("output/bhapc_parallel/gss/gss_variance_decomposition.csv"), show_col_types = FALSE)
gss_var_educ <- read_csv(here::here("output/sanity_check_bhapc/gss_education_variance.csv"), show_col_types = FALSE)
gss_var_happy <- read_csv(here::here("output/sanity_check_bhapc/gss_happiness_variance.csv"), show_col_types = FALSE)

# --- NHIS ---
message("Loading NHIS...")
data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

nhis_bhapc_srh <- prepare_sanity_check_data(data_nhis, "srh", "nhis")
nhis_bhapc_educ <- prepare_sanity_check_data(data_nhis, "educ_4cat", "nhis")
data_nhis_k6 <- data_nhis %>% filter(!is.na(k6))
nhis_bhapc_k6 <- prepare_sanity_check_data(data_nhis_k6, "k6", "nhis")

nhis_result_srh <- readRDS(here::here("output/sanity_check_nhis/result_srh.rds"))
nhis_result_educ <- readRDS(here::here("output/sanity_check_nhis/result_educ.rds"))
nhis_result_k6 <- readRDS(here::here("output/sanity_check_nhis/result_k6.rds"))

nhis_var_srh <- read_csv(here::here("output/sanity_check_nhis/nhis_srh_variance.csv"), show_col_types = FALSE)
nhis_var_educ <- read_csv(here::here("output/sanity_check_nhis/nhis_education_variance.csv"), show_col_types = FALSE)
nhis_var_k6 <- read_csv(here::here("output/sanity_check_nhis/nhis_k6_variance.csv"), show_col_types = FALSE)

# --- MEPS ---
message("Loading MEPS...")
data_meps <- readr::read_rds(derived_path("data_meps.rds"))

meps_bhapc_srh <- prepare_sanity_check_data(data_meps, "srh", "meps")
meps_bhapc_educ <- prepare_sanity_check_data(data_meps, "educ_4cat", "meps")
data_meps_k6 <- data_meps %>% filter(year >= 2004, !is.na(K6SUM))
meps_bhapc_k6 <- prepare_sanity_check_data(data_meps_k6, "K6SUM", "meps")

meps_result_srh <- readRDS(here::here("output/sanity_check_meps/result_srh.rds"))
meps_result_educ <- readRDS(here::here("output/sanity_check_meps/result_educ.rds"))
meps_result_k6 <- readRDS(here::here("output/sanity_check_meps/result_k6.rds"))

meps_var_srh <- read_csv(here::here("output/sanity_check_meps/meps_srh_variance.csv"), show_col_types = FALSE)
meps_var_educ <- read_csv(here::here("output/sanity_check_meps/meps_education_variance.csv"), show_col_types = FALSE)
meps_var_k6 <- read_csv(here::here("output/sanity_check_meps/meps_k6_variance.csv"), show_col_types = FALSE)

message("All data loaded successfully.")

# ==============================================================================
# Extract all effects
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Extracting effects from all models")
message(paste(rep("=", 70), collapse = ""))

# GSS effects
gss_cohort_srh <- extract_cohort_effects(gss_result_srh, gss_bhapc_srh, "SRH") %>% mutate(survey = "GSS")
gss_cohort_educ <- extract_cohort_effects(gss_result_educ, gss_bhapc_educ, "Education") %>% mutate(survey = "GSS")
gss_cohort_happy <- extract_cohort_effects(gss_result_happy, gss_bhapc_happy, "Happiness") %>% mutate(survey = "GSS")

gss_period_srh <- extract_period_effects(gss_result_srh, gss_bhapc_srh, "SRH") %>% mutate(survey = "GSS")
gss_period_educ <- extract_period_effects(gss_result_educ, gss_bhapc_educ, "Education") %>% mutate(survey = "GSS")
gss_period_happy <- extract_period_effects(gss_result_happy, gss_bhapc_happy, "Happiness") %>% mutate(survey = "GSS")

# NHIS effects
nhis_cohort_srh <- extract_cohort_effects(nhis_result_srh, nhis_bhapc_srh, "SRH") %>% mutate(survey = "NHIS")
nhis_cohort_educ <- extract_cohort_effects(nhis_result_educ, nhis_bhapc_educ, "Education") %>% mutate(survey = "NHIS")
nhis_cohort_k6 <- extract_cohort_effects(nhis_result_k6, nhis_bhapc_k6, "K6") %>% mutate(survey = "NHIS")

nhis_period_srh <- extract_period_effects(nhis_result_srh, nhis_bhapc_srh, "SRH") %>% mutate(survey = "NHIS")
nhis_period_educ <- extract_period_effects(nhis_result_educ, nhis_bhapc_educ, "Education") %>% mutate(survey = "NHIS")
nhis_period_k6 <- extract_period_effects(nhis_result_k6, nhis_bhapc_k6, "K6") %>% mutate(survey = "NHIS")

# MEPS effects
meps_cohort_srh <- extract_cohort_effects(meps_result_srh, meps_bhapc_srh, "SRH") %>% mutate(survey = "MEPS")
meps_cohort_educ <- extract_cohort_effects(meps_result_educ, meps_bhapc_educ, "Education") %>% mutate(survey = "MEPS")
meps_cohort_k6 <- extract_cohort_effects(meps_result_k6, meps_bhapc_k6, "K6") %>% mutate(survey = "MEPS")

meps_period_srh <- extract_period_effects(meps_result_srh, meps_bhapc_srh, "SRH") %>% mutate(survey = "MEPS")
meps_period_educ <- extract_period_effects(meps_result_educ, meps_bhapc_educ, "Education") %>% mutate(survey = "MEPS")
meps_period_k6 <- extract_period_effects(meps_result_k6, meps_bhapc_k6, "K6") %>% mutate(survey = "MEPS")

message("All effects extracted.")

# ==============================================================================
# PART 1: Per-Survey Combo Figures (Period + Cohort side-by-side)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("PART 1: Creating Per-Survey Combo Figures")
message(paste(rep("=", 70), collapse = ""))

# Panel creation functions for combo figures
create_cohort_panel_compact <- function(df, outcome_name, cohort_pct, color, y_limits, show_y_label = TRUE) {
  p <- ggplot(df, aes(x = cohort, y = estimate_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_std, ymax = ci_upper_std),
      width = 2, color = color, linewidth = 0.5, alpha = 0.7
    ) +
    geom_point(color = color, size = 2) +
    labs(
      title = paste0(outcome_name, " (", cohort_pct, "%)"),
      x = NULL,
      y = if(show_y_label) "Cohort effect (SD)" else NULL
    ) +
    coord_cartesian(ylim = y_limits) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 9)
    )
  p
}

create_period_panel_compact <- function(df, outcome_name, period_pct, color, y_limits, show_y_label = TRUE) {
  p <- ggplot(df, aes(x = period, y = estimate_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_std, ymax = ci_upper_std),
      width = 0.6, color = color, linewidth = 0.7
    ) +
    geom_point(color = color, size = 2.5) +
    labs(
      title = paste0(outcome_name, " (", period_pct, "%)"),
      x = NULL,
      y = if(show_y_label) "Period effect (SD)" else NULL
    ) +
    coord_cartesian(ylim = y_limits) +
    scale_x_continuous(
      breaks = unique(df$period),
      labels = as.character(unique(df$period))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 9)
    )
  p
}

# --- GSS Combo Figure ---
message("\nCreating GSS combo figure...")

# Compute common y-limits for GSS
gss_all_cohorts <- bind_rows(gss_cohort_srh, gss_cohort_educ, gss_cohort_happy)
gss_all_periods <- bind_rows(gss_period_srh, gss_period_educ, gss_period_happy)

gss_cohort_ylim <- range(c(gss_all_cohorts$ci_lower_std, gss_all_cohorts$ci_upper_std), na.rm = TRUE)
gss_cohort_ylim <- c(gss_cohort_ylim[1] - 0.1 * diff(gss_cohort_ylim),
                     gss_cohort_ylim[2] + 0.1 * diff(gss_cohort_ylim))

gss_period_ylim <- range(c(gss_all_periods$ci_lower_std, gss_all_periods$ci_upper_std), na.rm = TRUE)
gss_period_ylim <- c(gss_period_ylim[1] - 0.1 * diff(gss_period_ylim),
                     gss_period_ylim[2] + 0.1 * diff(gss_period_ylim))

# Create GSS panels
gss_p_cohort_srh <- create_cohort_panel_compact(gss_cohort_srh, "SRH", get_pct(gss_var_srh, "cohort_4yr"),
                                                  outcome_colors["SRH"], gss_cohort_ylim, TRUE)
gss_p_cohort_educ <- create_cohort_panel_compact(gss_cohort_educ, "Education", get_pct(gss_var_educ, "cohort_4yr"),
                                                   outcome_colors["Education"], gss_cohort_ylim, FALSE)
gss_p_cohort_happy <- create_cohort_panel_compact(gss_cohort_happy, "Happiness", get_pct(gss_var_happy, "cohort_4yr"),
                                                    outcome_colors["Happiness"], gss_cohort_ylim, FALSE)

gss_p_period_srh <- create_period_panel_compact(gss_period_srh, "SRH", get_pct(gss_var_srh, "period_4yr"),
                                                  outcome_colors["SRH"], gss_period_ylim, TRUE)
gss_p_period_educ <- create_period_panel_compact(gss_period_educ, "Education", get_pct(gss_var_educ, "period_4yr"),
                                                   outcome_colors["Education"], gss_period_ylim, FALSE)
gss_p_period_happy <- create_period_panel_compact(gss_period_happy, "Happiness", get_pct(gss_var_happy, "period_4yr"),
                                                    outcome_colors["Happiness"], gss_period_ylim, FALSE)

# Combine GSS
gss_combo <- (gss_p_cohort_srh | gss_p_cohort_educ | gss_p_cohort_happy) /
  (gss_p_period_srh | gss_p_period_educ | gss_p_period_happy) +
  plot_annotation(
    title = "GSS: Cohort and Period Effects Comparison",
    subtitle = "Row 1: Cohort effects | Row 2: Period effects (standardized to SD units)",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

ggsave(
  here::here("output/sanity_check_bhapc/gss_period_cohort_combo.png"),
  gss_combo, width = 12, height = 8, dpi = 300
)
ggsave(
  here::here("output/sanity_check_bhapc/gss_period_cohort_combo.pdf"),
  gss_combo, width = 12, height = 8
)
message("Saved: output/sanity_check_bhapc/gss_period_cohort_combo.png")

# --- NHIS Combo Figure ---
message("\nCreating NHIS combo figure...")

nhis_all_cohorts <- bind_rows(nhis_cohort_srh, nhis_cohort_educ, nhis_cohort_k6)
nhis_all_periods <- bind_rows(nhis_period_srh, nhis_period_educ, nhis_period_k6)

nhis_cohort_ylim <- range(c(nhis_all_cohorts$ci_lower_std, nhis_all_cohorts$ci_upper_std), na.rm = TRUE)
nhis_cohort_ylim <- c(nhis_cohort_ylim[1] - 0.1 * diff(nhis_cohort_ylim),
                      nhis_cohort_ylim[2] + 0.1 * diff(nhis_cohort_ylim))

nhis_period_ylim <- range(c(nhis_all_periods$ci_lower_std, nhis_all_periods$ci_upper_std), na.rm = TRUE)
nhis_period_ylim <- c(nhis_period_ylim[1] - 0.1 * diff(nhis_period_ylim),
                      nhis_period_ylim[2] + 0.1 * diff(nhis_period_ylim))

nhis_p_cohort_srh <- create_cohort_panel_compact(nhis_cohort_srh, "SRH", get_pct(nhis_var_srh, "cohort_4yr"),
                                                   outcome_colors["SRH"], nhis_cohort_ylim, TRUE)
nhis_p_cohort_educ <- create_cohort_panel_compact(nhis_cohort_educ, "Education", get_pct(nhis_var_educ, "cohort_4yr"),
                                                    outcome_colors["Education"], nhis_cohort_ylim, FALSE)
nhis_p_cohort_k6 <- create_cohort_panel_compact(nhis_cohort_k6, "K6 Distress", get_pct(nhis_var_k6, "cohort_4yr"),
                                                  outcome_colors["K6"], nhis_cohort_ylim, FALSE)

nhis_p_period_srh <- create_period_panel_compact(nhis_period_srh, "SRH", get_pct(nhis_var_srh, "period_4yr"),
                                                   outcome_colors["SRH"], nhis_period_ylim, TRUE)
nhis_p_period_educ <- create_period_panel_compact(nhis_period_educ, "Education", get_pct(nhis_var_educ, "period_4yr"),
                                                    outcome_colors["Education"], nhis_period_ylim, FALSE)
nhis_p_period_k6 <- create_period_panel_compact(nhis_period_k6, "K6 Distress", get_pct(nhis_var_k6, "period_4yr"),
                                                  outcome_colors["K6"], nhis_period_ylim, FALSE)

nhis_combo <- (nhis_p_cohort_srh | nhis_p_cohort_educ | nhis_p_cohort_k6) /
  (nhis_p_period_srh | nhis_p_period_educ | nhis_p_period_k6) +
  plot_annotation(
    title = "NHIS: Cohort and Period Effects Comparison",
    subtitle = "Row 1: Cohort effects | Row 2: Period effects (standardized to SD units)",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

ggsave(
  here::here("output/sanity_check_nhis/nhis_period_cohort_combo.png"),
  nhis_combo, width = 12, height = 8, dpi = 300
)
ggsave(
  here::here("output/sanity_check_nhis/nhis_period_cohort_combo.pdf"),
  nhis_combo, width = 12, height = 8
)
message("Saved: output/sanity_check_nhis/nhis_period_cohort_combo.png")

# --- MEPS Combo Figure ---
message("\nCreating MEPS combo figure...")

meps_all_cohorts <- bind_rows(meps_cohort_srh, meps_cohort_educ, meps_cohort_k6)
meps_all_periods <- bind_rows(meps_period_srh, meps_period_educ, meps_period_k6)

meps_cohort_ylim <- range(c(meps_all_cohorts$ci_lower_std, meps_all_cohorts$ci_upper_std), na.rm = TRUE)
meps_cohort_ylim <- c(meps_cohort_ylim[1] - 0.1 * diff(meps_cohort_ylim),
                      meps_cohort_ylim[2] + 0.1 * diff(meps_cohort_ylim))

meps_period_ylim <- range(c(meps_all_periods$ci_lower_std, meps_all_periods$ci_upper_std), na.rm = TRUE)
meps_period_ylim <- c(meps_period_ylim[1] - 0.1 * diff(meps_period_ylim),
                      meps_period_ylim[2] + 0.1 * diff(meps_period_ylim))

meps_p_cohort_srh <- create_cohort_panel_compact(meps_cohort_srh, "SRH", get_pct(meps_var_srh, "cohort_4yr"),
                                                   outcome_colors["SRH"], meps_cohort_ylim, TRUE)
meps_p_cohort_educ <- create_cohort_panel_compact(meps_cohort_educ, "Education", get_pct(meps_var_educ, "cohort_4yr"),
                                                    outcome_colors["Education"], meps_cohort_ylim, FALSE)
meps_p_cohort_k6 <- create_cohort_panel_compact(meps_cohort_k6, "K6 Distress", get_pct(meps_var_k6, "cohort_4yr"),
                                                  outcome_colors["K6"], meps_cohort_ylim, FALSE)

meps_p_period_srh <- create_period_panel_compact(meps_period_srh, "SRH", get_pct(meps_var_srh, "period_4yr"),
                                                   outcome_colors["SRH"], meps_period_ylim, TRUE)
meps_p_period_educ <- create_period_panel_compact(meps_period_educ, "Education", get_pct(meps_var_educ, "period_4yr"),
                                                    outcome_colors["Education"], meps_period_ylim, FALSE)
meps_p_period_k6 <- create_period_panel_compact(meps_period_k6, "K6 Distress", get_pct(meps_var_k6, "period_4yr"),
                                                  outcome_colors["K6"], meps_period_ylim, FALSE)

meps_combo <- (meps_p_cohort_srh | meps_p_cohort_educ | meps_p_cohort_k6) /
  (meps_p_period_srh | meps_p_period_educ | meps_p_period_k6) +
  plot_annotation(
    title = "MEPS: Cohort and Period Effects Comparison",
    subtitle = "Row 1: Cohort effects | Row 2: Period effects (standardized to SD units)",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

ggsave(
  here::here("output/sanity_check_meps/meps_period_cohort_combo.png"),
  meps_combo, width = 12, height = 8, dpi = 300
)
ggsave(
  here::here("output/sanity_check_meps/meps_period_cohort_combo.pdf"),
  meps_combo, width = 12, height = 8
)
message("Saved: output/sanity_check_meps/meps_period_cohort_combo.png")

# ==============================================================================
# PART 2: Cross-Survey Combo Figures
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("PART 2: Creating Cross-Survey Combo Figures")
message(paste(rep("=", 70), collapse = ""))

# Panel creation for cross-survey figures (with survey row label)
create_cohort_panel_survey <- function(df, outcome_name, cohort_pct, color, y_limits,
                                        show_y_label = TRUE, survey_label = NULL) {
  p <- ggplot(df, aes(x = cohort, y = estimate_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_std, ymax = ci_upper_std),
      width = 2, color = color, linewidth = 0.5, alpha = 0.7
    ) +
    geom_point(color = color, size = 2) +
    labs(
      title = paste0(outcome_name, " (", cohort_pct, "%)"),
      x = NULL,
      y = if(show_y_label && !is.null(survey_label)) paste0(survey_label, "\nEffect (SD)") else if(show_y_label) "Effect (SD)" else NULL
    ) +
    coord_cartesian(ylim = y_limits) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = element_text(size = 7),
      axis.title.y = element_text(size = 9, face = "bold")
    )
  p
}

create_period_panel_survey <- function(df, outcome_name, period_pct, color, y_limits,
                                        show_y_label = TRUE, survey_label = NULL) {
  p <- ggplot(df, aes(x = period, y = estimate_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_std, ymax = ci_upper_std),
      width = 0.5, color = color, linewidth = 0.6
    ) +
    geom_point(color = color, size = 2) +
    labs(
      title = paste0(outcome_name, " (", period_pct, "%)"),
      x = NULL,
      y = if(show_y_label && !is.null(survey_label)) paste0(survey_label, "\nEffect (SD)") else if(show_y_label) "Effect (SD)" else NULL
    ) +
    coord_cartesian(ylim = y_limits) +
    scale_x_continuous(
      breaks = unique(df$period),
      labels = as.character(unique(df$period))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.y = element_text(size = 9, face = "bold")
    )
  p
}

# --- Cross-Survey Cohort Effects Figure ---
message("\nCreating cross-survey cohort effects figure...")

# Compute unified y-limits for all cohort effects
all_surveys_cohorts <- bind_rows(
  gss_cohort_srh, gss_cohort_educ, gss_cohort_happy,
  nhis_cohort_srh, nhis_cohort_educ, nhis_cohort_k6,
  meps_cohort_srh, meps_cohort_educ, meps_cohort_k6
)
cross_cohort_ylim <- range(c(all_surveys_cohorts$ci_lower_std, all_surveys_cohorts$ci_upper_std), na.rm = TRUE)
cross_cohort_ylim <- c(cross_cohort_ylim[1] - 0.1 * diff(cross_cohort_ylim),
                       cross_cohort_ylim[2] + 0.1 * diff(cross_cohort_ylim))

# GSS row (SRH, Education, Happiness)
cross_gss_c_srh <- create_cohort_panel_survey(gss_cohort_srh, "SRH", get_pct(gss_var_srh, "cohort_4yr"),
                                                outcome_colors["SRH"], cross_cohort_ylim, TRUE, "GSS")
cross_gss_c_educ <- create_cohort_panel_survey(gss_cohort_educ, "Education", get_pct(gss_var_educ, "cohort_4yr"),
                                                 outcome_colors["Education"], cross_cohort_ylim, FALSE)
cross_gss_c_happy <- create_cohort_panel_survey(gss_cohort_happy, "Happiness", get_pct(gss_var_happy, "cohort_4yr"),
                                                  outcome_colors["Happiness"], cross_cohort_ylim, FALSE)

# NHIS row (SRH, Education, K6)
cross_nhis_c_srh <- create_cohort_panel_survey(nhis_cohort_srh, "SRH", get_pct(nhis_var_srh, "cohort_4yr"),
                                                 outcome_colors["SRH"], cross_cohort_ylim, TRUE, "NHIS")
cross_nhis_c_educ <- create_cohort_panel_survey(nhis_cohort_educ, "Education", get_pct(nhis_var_educ, "cohort_4yr"),
                                                  outcome_colors["Education"], cross_cohort_ylim, FALSE)
cross_nhis_c_k6 <- create_cohort_panel_survey(nhis_cohort_k6, "K6 Distress", get_pct(nhis_var_k6, "cohort_4yr"),
                                                outcome_colors["K6"], cross_cohort_ylim, FALSE)

# MEPS row (SRH, Education, K6)
cross_meps_c_srh <- create_cohort_panel_survey(meps_cohort_srh, "SRH", get_pct(meps_var_srh, "cohort_4yr"),
                                                 outcome_colors["SRH"], cross_cohort_ylim, TRUE, "MEPS")
cross_meps_c_educ <- create_cohort_panel_survey(meps_cohort_educ, "Education", get_pct(meps_var_educ, "cohort_4yr"),
                                                  outcome_colors["Education"], cross_cohort_ylim, FALSE)
cross_meps_c_k6 <- create_cohort_panel_survey(meps_cohort_k6, "K6 Distress", get_pct(meps_var_k6, "cohort_4yr"),
                                                outcome_colors["K6"], cross_cohort_ylim, FALSE)

# Combine cross-survey cohort figure
cross_cohort_fig <- (cross_gss_c_srh | cross_gss_c_educ | cross_gss_c_happy) /
  (cross_nhis_c_srh | cross_nhis_c_educ | cross_nhis_c_k6) /
  (cross_meps_c_srh | cross_meps_c_educ | cross_meps_c_k6) +
  plot_annotation(
    title = "Cohort Effects Across Surveys",
    subtitle = "GSS (SRH, Education, Happiness) | NHIS & MEPS (SRH, Education, K6) - All standardized to SD units",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained by cohort.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

ggsave(
  here::here("output/sanity_check_combined/all_surveys_cohort_effects.png"),
  cross_cohort_fig, width = 14, height = 10, dpi = 300
)
ggsave(
  here::here("output/sanity_check_combined/all_surveys_cohort_effects.pdf"),
  cross_cohort_fig, width = 14, height = 10
)
message("Saved: output/sanity_check_combined/all_surveys_cohort_effects.png")

# --- Cross-Survey Period Effects Figure ---
message("\nCreating cross-survey period effects figure...")

# Compute unified y-limits for all period effects
all_surveys_periods <- bind_rows(
  gss_period_srh, gss_period_educ, gss_period_happy,
  nhis_period_srh, nhis_period_educ, nhis_period_k6,
  meps_period_srh, meps_period_educ, meps_period_k6
)
cross_period_ylim <- range(c(all_surveys_periods$ci_lower_std, all_surveys_periods$ci_upper_std), na.rm = TRUE)
cross_period_ylim <- c(cross_period_ylim[1] - 0.1 * diff(cross_period_ylim),
                       cross_period_ylim[2] + 0.1 * diff(cross_period_ylim))

# GSS row (SRH, Education, Happiness)
cross_gss_p_srh <- create_period_panel_survey(gss_period_srh, "SRH", get_pct(gss_var_srh, "period_4yr"),
                                                outcome_colors["SRH"], cross_period_ylim, TRUE, "GSS")
cross_gss_p_educ <- create_period_panel_survey(gss_period_educ, "Education", get_pct(gss_var_educ, "period_4yr"),
                                                 outcome_colors["Education"], cross_period_ylim, FALSE)
cross_gss_p_happy <- create_period_panel_survey(gss_period_happy, "Happiness", get_pct(gss_var_happy, "period_4yr"),
                                                  outcome_colors["Happiness"], cross_period_ylim, FALSE)

# NHIS row (SRH, Education, K6)
cross_nhis_p_srh <- create_period_panel_survey(nhis_period_srh, "SRH", get_pct(nhis_var_srh, "period_4yr"),
                                                 outcome_colors["SRH"], cross_period_ylim, TRUE, "NHIS")
cross_nhis_p_educ <- create_period_panel_survey(nhis_period_educ, "Education", get_pct(nhis_var_educ, "period_4yr"),
                                                  outcome_colors["Education"], cross_period_ylim, FALSE)
cross_nhis_p_k6 <- create_period_panel_survey(nhis_period_k6, "K6 Distress", get_pct(nhis_var_k6, "period_4yr"),
                                                outcome_colors["K6"], cross_period_ylim, FALSE)

# MEPS row (SRH, Education, K6)
cross_meps_p_srh <- create_period_panel_survey(meps_period_srh, "SRH", get_pct(meps_var_srh, "period_4yr"),
                                                 outcome_colors["SRH"], cross_period_ylim, TRUE, "MEPS")
cross_meps_p_educ <- create_period_panel_survey(meps_period_educ, "Education", get_pct(meps_var_educ, "period_4yr"),
                                                  outcome_colors["Education"], cross_period_ylim, FALSE)
cross_meps_p_k6 <- create_period_panel_survey(meps_period_k6, "K6 Distress", get_pct(meps_var_k6, "period_4yr"),
                                                outcome_colors["K6"], cross_period_ylim, FALSE)

# Combine cross-survey period figure
cross_period_fig <- (cross_gss_p_srh | cross_gss_p_educ | cross_gss_p_happy) /
  (cross_nhis_p_srh | cross_nhis_p_educ | cross_nhis_p_k6) /
  (cross_meps_p_srh | cross_meps_p_educ | cross_meps_p_k6) +
  plot_annotation(
    title = "Period Effects Across Surveys",
    subtitle = "GSS (SRH, Education, Happiness) | NHIS & MEPS (SRH, Education, K6) - All standardized to SD units",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained by period.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

ggsave(
  here::here("output/sanity_check_combined/all_surveys_period_effects.png"),
  cross_period_fig, width = 14, height = 10, dpi = 300
)
ggsave(
  here::here("output/sanity_check_combined/all_surveys_period_effects.pdf"),
  cross_period_fig, width = 14, height = 10
)
message("Saved: output/sanity_check_combined/all_surveys_period_effects.png")

# ==============================================================================
# PART 3: Transposed Cross-Survey Figures (Rows=Outcomes, Cols=Surveys)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("PART 3: Creating Transposed Cross-Survey Figures")
message(paste(rep("=", 70), collapse = ""))

# Panel creation functions with x-axis limits parameter
create_cohort_panel_transposed <- function(df, outcome_name, cohort_pct, color, y_limits,
                                            x_limits = NULL, show_y_label = TRUE,
                                            show_title = TRUE, col_label = NULL) {
  p <- ggplot(df, aes(x = cohort, y = estimate_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_std, ymax = ci_upper_std),
      width = 2, color = color, linewidth = 0.5, alpha = 0.7
    ) +
    geom_point(color = color, size = 2) +
    labs(
      title = if(show_title && !is.null(col_label)) col_label else NULL,
      x = NULL,
      y = if(show_y_label) paste0(outcome_name, "\n(", cohort_pct, "%)") else NULL
    ) +
    coord_cartesian(ylim = y_limits, xlim = x_limits) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = element_text(size = 7),
      axis.title.y = element_text(size = 9, face = "bold")
    )
  p
}

create_period_panel_transposed <- function(df, outcome_name, period_pct, color, y_limits,
                                            x_limits = NULL, show_y_label = TRUE,
                                            show_title = TRUE, col_label = NULL) {
  p <- ggplot(df, aes(x = period, y = estimate_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_std, ymax = ci_upper_std),
      width = 0.5, color = color, linewidth = 0.6
    ) +
    geom_point(color = color, size = 2) +
    labs(
      title = if(show_title && !is.null(col_label)) col_label else NULL,
      x = NULL,
      y = if(show_y_label) paste0(outcome_name, "\n(", period_pct, "%)") else NULL
    ) +
    coord_cartesian(ylim = y_limits, xlim = x_limits) +
    scale_x_continuous(
      breaks = unique(df$period),
      labels = as.character(unique(df$period))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      axis.title.y = element_text(size = 9, face = "bold")
    )
  p
}

# Compute x-axis limits per survey (union of years across all outcomes for that survey)
# For cohort effects
meps_cohort_xlim <- range(c(meps_cohort_srh$cohort, meps_cohort_educ$cohort, meps_cohort_k6$cohort), na.rm = TRUE)
nhis_cohort_xlim <- range(c(nhis_cohort_srh$cohort, nhis_cohort_educ$cohort, nhis_cohort_k6$cohort), na.rm = TRUE)
gss_cohort_xlim <- range(c(gss_cohort_srh$cohort, gss_cohort_educ$cohort, gss_cohort_happy$cohort), na.rm = TRUE)

# For period effects
meps_period_xlim <- range(c(meps_period_srh$period, meps_period_educ$period, meps_period_k6$period), na.rm = TRUE)
nhis_period_xlim <- range(c(nhis_period_srh$period, nhis_period_educ$period, nhis_period_k6$period), na.rm = TRUE)
gss_period_xlim <- range(c(gss_period_srh$period, gss_period_educ$period, gss_period_happy$period), na.rm = TRUE)

message("  MEPS cohort x-range: ", meps_cohort_xlim[1], " to ", meps_cohort_xlim[2])
message("  NHIS cohort x-range: ", nhis_cohort_xlim[1], " to ", nhis_cohort_xlim[2])
message("  GSS cohort x-range: ", gss_cohort_xlim[1], " to ", gss_cohort_xlim[2])
message("  MEPS period x-range: ", meps_period_xlim[1], " to ", meps_period_xlim[2])
message("  NHIS period x-range: ", nhis_period_xlim[1], " to ", nhis_period_xlim[2])
message("  GSS period x-range: ", gss_period_xlim[1], " to ", gss_period_xlim[2])

# --- Transposed Cohort Effects Figure ---
message("\nCreating transposed cohort effects figure...")

# Row 1: SRH (MEPS | NHIS | GSS)
trans_c_srh_meps <- create_cohort_panel_transposed(
  meps_cohort_srh, "SRH", get_pct(meps_var_srh, "cohort_4yr"),
  outcome_colors["SRH"], cross_cohort_ylim, meps_cohort_xlim,
  show_y_label = TRUE, show_title = TRUE, col_label = "MEPS"
)
trans_c_srh_nhis <- create_cohort_panel_transposed(
  nhis_cohort_srh, "SRH", get_pct(nhis_var_srh, "cohort_4yr"),
  outcome_colors["SRH"], cross_cohort_ylim, nhis_cohort_xlim,
  show_y_label = FALSE, show_title = TRUE, col_label = "NHIS"
)
trans_c_srh_gss <- create_cohort_panel_transposed(
  gss_cohort_srh, "SRH", get_pct(gss_var_srh, "cohort_4yr"),
  outcome_colors["SRH"], cross_cohort_ylim, gss_cohort_xlim,
  show_y_label = FALSE, show_title = TRUE, col_label = "GSS"
)

# Row 2: Education (MEPS | NHIS | GSS)
trans_c_educ_meps <- create_cohort_panel_transposed(
  meps_cohort_educ, "Education", get_pct(meps_var_educ, "cohort_4yr"),
  outcome_colors["Education"], cross_cohort_ylim, meps_cohort_xlim,
  show_y_label = TRUE, show_title = FALSE
)
trans_c_educ_nhis <- create_cohort_panel_transposed(
  nhis_cohort_educ, "Education", get_pct(nhis_var_educ, "cohort_4yr"),
  outcome_colors["Education"], cross_cohort_ylim, nhis_cohort_xlim,
  show_y_label = FALSE, show_title = FALSE
)
trans_c_educ_gss <- create_cohort_panel_transposed(
  gss_cohort_educ, "Education", get_pct(gss_var_educ, "cohort_4yr"),
  outcome_colors["Education"], cross_cohort_ylim, gss_cohort_xlim,
  show_y_label = FALSE, show_title = FALSE
)

# Row 3: K6/Happiness (MEPS | NHIS | GSS)
trans_c_k6_meps <- create_cohort_panel_transposed(
  meps_cohort_k6, "K6", get_pct(meps_var_k6, "cohort_4yr"),
  outcome_colors["K6"], cross_cohort_ylim, meps_cohort_xlim,
  show_y_label = TRUE, show_title = FALSE
)
trans_c_k6_nhis <- create_cohort_panel_transposed(
  nhis_cohort_k6, "K6", get_pct(nhis_var_k6, "cohort_4yr"),
  outcome_colors["K6"], cross_cohort_ylim, nhis_cohort_xlim,
  show_y_label = FALSE, show_title = FALSE
)
trans_c_happy_gss <- create_cohort_panel_transposed(
  gss_cohort_happy, "Happiness", get_pct(gss_var_happy, "cohort_4yr"),
  outcome_colors["Happiness"], cross_cohort_ylim, gss_cohort_xlim,
  show_y_label = FALSE, show_title = FALSE
)

# Combine transposed cohort figure
transposed_cohort_fig <- (trans_c_srh_meps | trans_c_srh_nhis | trans_c_srh_gss) /
  (trans_c_educ_meps | trans_c_educ_nhis | trans_c_educ_gss) /
  (trans_c_k6_meps | trans_c_k6_nhis | trans_c_happy_gss) +
  plot_annotation(
    title = "Cohort Effects: Outcomes Across Surveys",
    subtitle = "Rows: SRH, Education, K6/Happiness | Columns: MEPS, NHIS, GSS (x-axis aligned within each survey)",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained by cohort.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

ggsave(
  here::here("output/sanity_check_combined/transposed_cohort_effects.png"),
  transposed_cohort_fig, width = 14, height = 10, dpi = 300
)
ggsave(
  here::here("output/sanity_check_combined/transposed_cohort_effects.pdf"),
  transposed_cohort_fig, width = 14, height = 10
)
message("Saved: output/sanity_check_combined/transposed_cohort_effects.png")

# --- Transposed Period Effects Figure ---
message("\nCreating transposed period effects figure...")

# Row 1: SRH (MEPS | NHIS | GSS)
trans_p_srh_meps <- create_period_panel_transposed(
  meps_period_srh, "SRH", get_pct(meps_var_srh, "period_4yr"),
  outcome_colors["SRH"], cross_period_ylim, meps_period_xlim,
  show_y_label = TRUE, show_title = TRUE, col_label = "MEPS"
)
trans_p_srh_nhis <- create_period_panel_transposed(
  nhis_period_srh, "SRH", get_pct(nhis_var_srh, "period_4yr"),
  outcome_colors["SRH"], cross_period_ylim, nhis_period_xlim,
  show_y_label = FALSE, show_title = TRUE, col_label = "NHIS"
)
trans_p_srh_gss <- create_period_panel_transposed(
  gss_period_srh, "SRH", get_pct(gss_var_srh, "period_4yr"),
  outcome_colors["SRH"], cross_period_ylim, gss_period_xlim,
  show_y_label = FALSE, show_title = TRUE, col_label = "GSS"
)

# Row 2: Education (MEPS | NHIS | GSS)
trans_p_educ_meps <- create_period_panel_transposed(
  meps_period_educ, "Education", get_pct(meps_var_educ, "period_4yr"),
  outcome_colors["Education"], cross_period_ylim, meps_period_xlim,
  show_y_label = TRUE, show_title = FALSE
)
trans_p_educ_nhis <- create_period_panel_transposed(
  nhis_period_educ, "Education", get_pct(nhis_var_educ, "period_4yr"),
  outcome_colors["Education"], cross_period_ylim, nhis_period_xlim,
  show_y_label = FALSE, show_title = FALSE
)
trans_p_educ_gss <- create_period_panel_transposed(
  gss_period_educ, "Education", get_pct(gss_var_educ, "period_4yr"),
  outcome_colors["Education"], cross_period_ylim, gss_period_xlim,
  show_y_label = FALSE, show_title = FALSE
)

# Row 3: K6/Happiness (MEPS | NHIS | GSS)
trans_p_k6_meps <- create_period_panel_transposed(
  meps_period_k6, "K6", get_pct(meps_var_k6, "period_4yr"),
  outcome_colors["K6"], cross_period_ylim, meps_period_xlim,
  show_y_label = TRUE, show_title = FALSE
)
trans_p_k6_nhis <- create_period_panel_transposed(
  nhis_period_k6, "K6", get_pct(nhis_var_k6, "period_4yr"),
  outcome_colors["K6"], cross_period_ylim, nhis_period_xlim,
  show_y_label = FALSE, show_title = FALSE
)
trans_p_happy_gss <- create_period_panel_transposed(
  gss_period_happy, "Happiness", get_pct(gss_var_happy, "period_4yr"),
  outcome_colors["Happiness"], cross_period_ylim, gss_period_xlim,
  show_y_label = FALSE, show_title = FALSE
)

# Combine transposed period figure
transposed_period_fig <- (trans_p_srh_meps | trans_p_srh_nhis | trans_p_srh_gss) /
  (trans_p_educ_meps | trans_p_educ_nhis | trans_p_educ_gss) /
  (trans_p_k6_meps | trans_p_k6_nhis | trans_p_happy_gss) +
  plot_annotation(
    title = "Period Effects: Outcomes Across Surveys",
    subtitle = "Rows: SRH, Education, K6/Happiness | Columns: MEPS, NHIS, GSS (x-axis aligned within each survey)",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained by period.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

ggsave(
  here::here("output/sanity_check_combined/transposed_period_effects.png"),
  transposed_period_fig, width = 14, height = 10, dpi = 300
)
ggsave(
  here::here("output/sanity_check_combined/transposed_period_effects.pdf"),
  transposed_period_fig, width = 14, height = 10
)
message("Saved: output/sanity_check_combined/transposed_period_effects.png")

# ==============================================================================
# Summary
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("DONE - All Combo Figures Created")
message(paste(rep("=", 70), collapse = ""))
message("\nPer-Survey Combo Figures (period + cohort):")
message("  - output/sanity_check_bhapc/gss_period_cohort_combo.png")
message("  - output/sanity_check_nhis/nhis_period_cohort_combo.png")
message("  - output/sanity_check_meps/meps_period_cohort_combo.png")
message("\nCross-Survey Combo Figures:")
message("  - output/sanity_check_combined/all_surveys_cohort_effects.png")
message("  - output/sanity_check_combined/all_surveys_period_effects.png")
message("\nTransposed Cross-Survey Figures (Rows=Outcomes, Cols=Surveys):")
message("  - output/sanity_check_combined/transposed_cohort_effects.png")
message("  - output/sanity_check_combined/transposed_period_effects.png")
