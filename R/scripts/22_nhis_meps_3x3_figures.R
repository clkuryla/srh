# ==============================================================================
# 22_nhis_meps_3x3_figures.R
# Create 3x3 combined APC figures for NHIS and MEPS: SRH, Education, K6
# Author: Christine Lucille Kuryla
# ==============================================================================

library(tidyverse)
library(patchwork)
library(here)

# Source functions
source(here::here("R/functions/bhapc_model_fitting.R"))
source(here::here("R/functions/bhapc_figure_generation.R"))
source(here::here("R/paths.R"))

# ==============================================================================
# Helper functions for data preparation
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

# ==============================================================================
# Helper to extract all APC effects from a result
# ==============================================================================

extract_all_effects <- function(result, bhapc_df, outcome_label) {
  model <- result$model

  # Age effect
  age_effect <- compute_age_effect(model, bhapc_df) %>%
    mutate(outcome = outcome_label)

  # Period and cohort effects
  random_effects <- extract_random_effects(model, bhapc_df)

  period_effects <- random_effects$period_effects %>%
    mutate(outcome = outcome_label)

  cohort_effects <- random_effects$cohort_effects %>%
    mutate(outcome = outcome_label)

  list(
    age = age_effect,
    period = period_effects,
    cohort = cohort_effects
  )
}

# ==============================================================================
# Panel creation functions
# ==============================================================================

# Colors
period_color <- "#009E73"
cohort_color <- "#CC79A7"
age_color <- "#0072B2"

create_age_panel <- function(age_df, title_label, outcome_name = "SRH") {
  ggplot(age_df, aes(x = age)) +
    geom_ribbon(
      aes(ymin = ci_lower_centered, ymax = ci_upper_centered),
      fill = age_color, alpha = 0.2
    ) +
    geom_line(aes(y = estimate_centered), color = age_color, linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = title_label,
      x = "Age (years)",
      y = paste0("Effect on ", outcome_name, "\n(relative to age 18)")
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold")
    )
}

create_period_panel <- function(period_df, title_label, outcome_name = "SRH") {
  ggplot(period_df, aes(x = period, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_90, ymax = ci_upper_90),
      width = 0.8, color = period_color, linewidth = 0.8
    ) +
    geom_point(color = period_color, size = 3) +
    labs(
      title = title_label,
      x = "Period (start year)",
      y = paste0("Random effect\n(", outcome_name, " units)")
    ) +
    scale_x_continuous(
      breaks = unique(period_df$period),
      labels = as.character(unique(period_df$period))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
    )
}

create_cohort_panel <- function(cohort_df, title_label, outcome_name = "SRH") {
  ggplot(cohort_df, aes(x = cohort, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_90, ymax = ci_upper_90),
      width = 2, color = cohort_color, linewidth = 0.6, alpha = 0.7
    ) +
    geom_point(color = cohort_color, size = 2) +
    labs(
      title = title_label,
      x = "Birth cohort (start year)",
      y = paste0("Random effect\n(", outcome_name, " units)")
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
    )
}

# Helper to get variance percentage
get_var_pct <- function(var_df, component) {
  round(var_df$pct_of_total[var_df$component == component], 2)
}

# ==============================================================================
# NHIS 3x3 Figure
# ==============================================================================

create_nhis_3x3 <- function() {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("Creating NHIS 3x3 Figure")
  message(paste(rep("=", 70), collapse = ""))

  output_dir <- here::here("output/sanity_check_nhis")

  # Load NHIS data
  message("Loading NHIS data...")
  data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

  # Load saved model results
  message("Loading saved model results...")
  result_srh <- readRDS(file.path(output_dir, "result_srh.rds"))
  result_educ <- readRDS(file.path(output_dir, "result_educ.rds"))
  result_k6 <- readRDS(file.path(output_dir, "result_k6.rds"))

  # Prepare data for each outcome (need matching data for effect extraction)
  message("Preparing data for effect extraction...")
  bhapc_srh <- prepare_sanity_check_data(data_nhis, "srh", "nhis")
  bhapc_educ <- prepare_sanity_check_data(data_nhis, "educ_4cat", "nhis")

  # K6 data needs filtering to years with K6
  data_nhis_k6 <- data_nhis %>% filter(!is.na(k6))
  bhapc_k6 <- prepare_sanity_check_data(data_nhis_k6, "k6", "nhis")

  # Extract effects
  message("Extracting APC effects...")
  effects_srh <- extract_all_effects(result_srh, bhapc_srh, "SRH")
  effects_educ <- extract_all_effects(result_educ, bhapc_educ, "Education")
  effects_k6 <- extract_all_effects(result_k6, bhapc_k6, "K6")

  # Load variance data
  var_srh <- read_csv(file.path(output_dir, "nhis_srh_variance.csv"), show_col_types = FALSE)
  var_educ <- read_csv(file.path(output_dir, "nhis_education_variance.csv"), show_col_types = FALSE)
  var_k6 <- read_csv(file.path(output_dir, "nhis_k6_variance.csv"), show_col_types = FALSE)

  # Create panels
  message("Creating panels...")

  # Row 1: SRH
  p_srh_age <- create_age_panel(effects_srh$age, "SRH: Age Effect", "SRH")
  p_srh_period <- create_period_panel(effects_srh$period,
    paste0("SRH: Period Effect (", get_var_pct(var_srh, "period_4yr"), "%)"), "SRH")
  p_srh_cohort <- create_cohort_panel(effects_srh$cohort,
    paste0("SRH: Cohort Effect (", get_var_pct(var_srh, "cohort_4yr"), "%)"), "SRH")

  # Row 2: Education
  p_educ_age <- create_age_panel(effects_educ$age, "Education: Age Effect", "Education")
  p_educ_period <- create_period_panel(effects_educ$period,
    paste0("Education: Period Effect (", get_var_pct(var_educ, "period_4yr"), "%)"), "Education")
  p_educ_cohort <- create_cohort_panel(effects_educ$cohort,
    paste0("Education: Cohort Effect (", get_var_pct(var_educ, "cohort_4yr"), "%)"), "Education")

  # Row 3: K6
  p_k6_age <- create_age_panel(effects_k6$age, "K6 Distress: Age Effect", "K6")
  p_k6_period <- create_period_panel(effects_k6$period,
    paste0("K6 Distress: Period Effect (", get_var_pct(var_k6, "period_4yr"), "%)"), "K6")
  p_k6_cohort <- create_cohort_panel(effects_k6$cohort,
    paste0("K6 Distress: Cohort Effect (", get_var_pct(var_k6, "cohort_4yr"), "%)"), "K6")

  # Combine into 3x3
  message("Combining into 3x3 figure...")

  combined_3x3 <- (
    (p_srh_age | p_srh_period | p_srh_cohort) /
    (p_educ_age | p_educ_period | p_educ_cohort) /
    (p_k6_age | p_k6_period | p_k6_cohort)
  ) +
    plot_annotation(
      title = "Age, Period, and Cohort Effects: NHIS Sanity Check",
      subtitle = paste0(
        "Row 1: Self-Rated Health (cohort = ", get_var_pct(var_srh, "cohort_4yr"), "%) | ",
        "Row 2: Education (cohort = ", get_var_pct(var_educ, "cohort_4yr"), "%) | ",
        "Row 3: K6 Distress (cohort = ", get_var_pct(var_k6, "cohort_4yr"), "%)"
      ),
      caption = "Bayesian HAPC model with 90% credible intervals. 100K subsample, 2000 iterations.",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
      )
    )

  # Save
  ggsave(
    file.path(output_dir, "nhis_sanity_check_3x3.png"),
    combined_3x3,
    width = 14, height = 12, dpi = 300
  )

  ggsave(
    file.path(output_dir, "nhis_sanity_check_3x3.pdf"),
    combined_3x3,
    width = 14, height = 12
  )

  message("Saved NHIS 3x3 figure to: ", file.path(output_dir, "nhis_sanity_check_3x3.png"))

  return(combined_3x3)
}

# ==============================================================================
# MEPS 3x3 Figure
# ==============================================================================

create_meps_3x3 <- function() {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("Creating MEPS 3x3 Figure")
  message(paste(rep("=", 70), collapse = ""))

  output_dir <- here::here("output/sanity_check_meps")

  # Load MEPS data
  message("Loading MEPS data...")
  data_meps <- readr::read_rds(derived_path("data_meps.rds"))

  # Load saved model results
  message("Loading saved model results...")
  result_srh <- readRDS(file.path(output_dir, "result_srh.rds"))
  result_educ <- readRDS(file.path(output_dir, "result_educ.rds"))
  result_k6 <- readRDS(file.path(output_dir, "result_k6.rds"))

  # Prepare data for each outcome (need matching data for effect extraction)
  message("Preparing data for effect extraction...")
  bhapc_srh <- prepare_sanity_check_data(data_meps, "srh", "meps")
  bhapc_educ <- prepare_sanity_check_data(data_meps, "educ_4cat", "meps")

  # K6 data needs filtering to years with K6 (2004+)
  data_meps_k6 <- data_meps %>% filter(year >= 2004, !is.na(K6SUM))
  bhapc_k6 <- prepare_sanity_check_data(data_meps_k6, "K6SUM", "meps")

  # Extract effects
  message("Extracting APC effects...")
  effects_srh <- extract_all_effects(result_srh, bhapc_srh, "SRH")
  effects_educ <- extract_all_effects(result_educ, bhapc_educ, "Education")
  effects_k6 <- extract_all_effects(result_k6, bhapc_k6, "K6")

  # Load variance data
  var_srh <- read_csv(file.path(output_dir, "meps_srh_variance.csv"), show_col_types = FALSE)
  var_educ <- read_csv(file.path(output_dir, "meps_education_variance.csv"), show_col_types = FALSE)
  var_k6 <- read_csv(file.path(output_dir, "meps_k6_variance.csv"), show_col_types = FALSE)

  # Create panels
  message("Creating panels...")

  # Row 1: SRH
  p_srh_age <- create_age_panel(effects_srh$age, "SRH: Age Effect", "SRH")
  p_srh_period <- create_period_panel(effects_srh$period,
    paste0("SRH: Period Effect (", get_var_pct(var_srh, "period_4yr"), "%)"), "SRH")
  p_srh_cohort <- create_cohort_panel(effects_srh$cohort,
    paste0("SRH: Cohort Effect (", get_var_pct(var_srh, "cohort_4yr"), "%)"), "SRH")

  # Row 2: Education
  p_educ_age <- create_age_panel(effects_educ$age, "Education: Age Effect", "Education")
  p_educ_period <- create_period_panel(effects_educ$period,
    paste0("Education: Period Effect (", get_var_pct(var_educ, "period_4yr"), "%)"), "Education")
  p_educ_cohort <- create_cohort_panel(effects_educ$cohort,
    paste0("Education: Cohort Effect (", get_var_pct(var_educ, "cohort_4yr"), "%)"), "Education")

  # Row 3: K6
  p_k6_age <- create_age_panel(effects_k6$age, "K6 Distress: Age Effect", "K6")
  p_k6_period <- create_period_panel(effects_k6$period,
    paste0("K6 Distress: Period Effect (", get_var_pct(var_k6, "period_4yr"), "%)"), "K6")
  p_k6_cohort <- create_cohort_panel(effects_k6$cohort,
    paste0("K6 Distress: Cohort Effect (", get_var_pct(var_k6, "cohort_4yr"), "%)"), "K6")

  # Combine into 3x3
  message("Combining into 3x3 figure...")

  combined_3x3 <- (
    (p_srh_age | p_srh_period | p_srh_cohort) /
    (p_educ_age | p_educ_period | p_educ_cohort) /
    (p_k6_age | p_k6_period | p_k6_cohort)
  ) +
    plot_annotation(
      title = "Age, Period, and Cohort Effects: MEPS Sanity Check",
      subtitle = paste0(
        "Row 1: Self-Rated Health (cohort = ", get_var_pct(var_srh, "cohort_4yr"), "%) | ",
        "Row 2: Education (cohort = ", get_var_pct(var_educ, "cohort_4yr"), "%) | ",
        "Row 3: K6 Distress (cohort = ", get_var_pct(var_k6, "cohort_4yr"), "%)"
      ),
      caption = "Bayesian HAPC model with 90% credible intervals. 100K subsample, 2000 iterations.",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
      )
    )

  # Save
  ggsave(
    file.path(output_dir, "meps_sanity_check_3x3.png"),
    combined_3x3,
    width = 14, height = 12, dpi = 300
  )

  ggsave(
    file.path(output_dir, "meps_sanity_check_3x3.pdf"),
    combined_3x3,
    width = 14, height = 12
  )

  message("Saved MEPS 3x3 figure to: ", file.path(output_dir, "meps_sanity_check_3x3.png"))

  return(combined_3x3)
}

# ==============================================================================
# Run both
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Creating 3x3 Sanity Check Figures for NHIS and MEPS")
message(paste(rep("=", 70), collapse = ""))

# NHIS
nhis_fig <- create_nhis_3x3()

# MEPS
meps_fig <- create_meps_3x3()

message("\n", paste(rep("=", 70), collapse = ""))
message("DONE")
message(paste(rep("=", 70), collapse = ""))
message("\nOutput files:")
message("  - output/sanity_check_nhis/nhis_sanity_check_3x3.png")
message("  - output/sanity_check_nhis/nhis_sanity_check_3x3.pdf")
message("  - output/sanity_check_meps/meps_sanity_check_3x3.png")
message("  - output/sanity_check_meps/meps_sanity_check_3x3.pdf")
