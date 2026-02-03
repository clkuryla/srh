# ==============================================================================
# 14_sanity_check_combined_figure.R
# Create 3x3 combined APC figure: SRH, Education, Happiness
# Author: Christine Lucille Kuryla
# ==============================================================================

library(tidyverse)
library(patchwork)
library(here)
library(gssr)

# Source functions
source(here::here("R/functions/bhapc_model_fitting.R"))
source(here::here("R/functions/bhapc_figure_generation.R"))

# ==============================================================================
# Helper functions for data preparation (from 13_sanity_check_gss.R)
# ==============================================================================

map_to_period_4yr <- function(year, survey = "gss") {
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

prepare_sanity_check_data <- function(df, outcome_var, survey = "gss") {
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
# Load GSS data and prepare for each outcome
# ==============================================================================

output_dir <- here::here("output/sanity_check_bhapc")

message("Loading GSS data...")
data("gss_all")

data_gss_full <- gss_all %>%
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

# Prepare data for each outcome
bhapc_srh <- prepare_sanity_check_data(data_gss_full, "srh")
bhapc_educ <- prepare_sanity_check_data(data_gss_full, "educ")
bhapc_happy <- prepare_sanity_check_data(data_gss_full, "happy")

message("Data prepared: SRH=", nrow(bhapc_srh), ", Educ=", nrow(bhapc_educ), ", Happy=", nrow(bhapc_happy))

# ==============================================================================
# Load saved model results
# ==============================================================================

# Load SRH model from parallel run
result_srh <- readRDS(here::here("output/bhapc_parallel/gss/gss_bhapc_model.rds"))

# Load Education and Happiness models from sanity check
result_educ <- readRDS(file.path(output_dir, "result_educ.rds"))
result_happy <- readRDS(file.path(output_dir, "result_happy.rds"))

message("Models loaded successfully")

# ==============================================================================
# Extract effects for each outcome
# ==============================================================================

# Helper to extract all APC effects from a result
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

# Extract for each outcome
effects_srh <- extract_all_effects(result_srh, bhapc_srh, "SRH")
effects_educ <- extract_all_effects(result_educ, bhapc_educ, "Education")
effects_happy <- extract_all_effects(result_happy, bhapc_happy, "Happiness")

# ==============================================================================
# Create individual panels
# ==============================================================================

# Color palette for outcomes
outcome_colors <- c(
  "SRH" = "#0072B2",
  "Education" = "#E69F00",
  "Happiness" = "#56B4E9"
)

# Period and cohort colors (consistent with existing figures)
period_color <- "#009E73"
cohort_color <- "#CC79A7"
age_color <- "#0072B2"

# --- Age Effect Panels ---
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

# --- Period Effect Panels ---
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

# --- Cohort Effect Panels ---
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

# ==============================================================================
# Build the 3x3 figure
# ==============================================================================

message("Creating 3x3 combined figure...")

# Load variance data for titles
var_srh <- read_csv(here::here("output/bhapc_parallel/gss/gss_variance_decomposition.csv"), show_col_types = FALSE)
var_educ <- read_csv(file.path(output_dir, "gss_education_variance.csv"), show_col_types = FALSE)
var_happy <- read_csv(file.path(output_dir, "gss_happiness_variance.csv"), show_col_types = FALSE)

# Helper to get variance percentage
get_var_pct <- function(var_df, component) {
  round(var_df$pct_of_total[var_df$component == component], 2)
}

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

# Row 3: Happiness
p_happy_age <- create_age_panel(effects_happy$age, "Happiness: Age Effect", "Happiness")
p_happy_period <- create_period_panel(effects_happy$period,
  paste0("Happiness: Period Effect (", get_var_pct(var_happy, "period_4yr"), "%)"), "Happiness")
p_happy_cohort <- create_cohort_panel(effects_happy$cohort,
  paste0("Happiness: Cohort Effect (", get_var_pct(var_happy, "cohort_4yr"), "%)"), "Happiness")

# Combine into 3x3
combined_3x3 <- (
  (p_srh_age | p_srh_period | p_srh_cohort) /
  (p_educ_age | p_educ_period | p_educ_cohort) /
  (p_happy_age | p_happy_period | p_happy_cohort)
) +
  plot_annotation(
    title = "Age, Period, and Cohort Effects: GSS Sanity Check",
    subtitle = "Comparing SRH to Education (benchmark with known cohort trend) and Happiness (another subjective measure)",
    caption = "Bayesian HAPC model with 90% credible intervals",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50")
    )
  )

# Save
ggsave(
  file.path(output_dir, "gss_sanity_check_3x3.png"),
  combined_3x3,
  width = 14, height = 12, dpi = 300
)

message("Saved 3x3 figure to: ", file.path(output_dir, "gss_sanity_check_3x3.png"))

# ==============================================================================
# Also create a version with shared y-axes per column (for better comparison)
# ==============================================================================

message("Creating version with row labels...")

# Add row labels using plot_layout
combined_labeled <- (
  (p_srh_age | p_srh_period | p_srh_cohort) /
  (p_educ_age | p_educ_period | p_educ_cohort) /
  (p_happy_age | p_happy_period | p_happy_cohort)
) +
  plot_annotation(
    title = "GSS Sanity Check: APC Effects Comparison",
    subtitle = paste0(
      "Row 1: Self-Rated Health (cohort = 2.35%) | ",
      "Row 2: Education (cohort = 6.41%) | ",
      "Row 3: Happiness (cohort = 0.06%)"
    ),
    caption = "Education shows strong cohort trend (positive control). Happiness shows minimal cohort variation (subjective measure comparison).",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
    )
  )

ggsave(
  file.path(output_dir, "gss_sanity_check_3x3_labeled.png"),
  combined_labeled,
  width = 14, height = 12, dpi = 300
)

message("Saved labeled 3x3 figure to: ", file.path(output_dir, "gss_sanity_check_3x3_labeled.png"))

message("\nDone!")
