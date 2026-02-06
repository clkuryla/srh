# ==============================================================================
# 15a_sanity_check_outputs.R
# Generate Figure 2 analogs and additional CSVs for sanity check
# Author: Christine Lucille Kuryla
#
# This script produces the same outputs as the main BHAPC pipeline:
# - Figure 2 (descriptive means by age and period) for education & happiness
# - Table 2 (fixed effects + variance components) for education & happiness
# - Gradient by period CSV (young vs old difference) for education & happiness
# - Combined variance decomposition table (SRH, Education, Happiness)
# - Combined Figure 2 (3-panel comparison: SRH, Education, Happiness)
# ==============================================================================

library(tidyverse)
library(here)
library(srvyr)
library(patchwork)
library(gssr)

# Source functions
source(here::here("R/functions/bhapc_model_fitting.R"))
source(here::here("R/functions/bhapc_figure_generation.R"))
source(here::here("R/functions/bhapc_table_generation.R"))

# ==============================================================================
# Setup
# ==============================================================================

output_dir <- here::here("output/sanity_check_bhapc")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260202)

message("Loading saved model results...")

# Load saved models
result_educ <- readRDS(file.path(output_dir, "result_educ.rds"))
result_happy <- readRDS(file.path(output_dir, "result_happy.rds"))

message("  Education model loaded")
message("  Happiness model loaded")

# ==============================================================================
# Reload and prepare data (same processing as 13_sanity_check_gss.R)
# ==============================================================================

message("\nReloading GSS data for Figure 2 generation...")

data("gss_all")

# Helper functions (copied from 13_sanity_check_gss.R)
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

# Prepare full GSS data
data_gss_full <- gss_all %>%
  haven::zap_labels() %>%
  select(
    year, cohort, age, health, sex,
    happy, educ,
    wtssps, wtssall
  ) %>%
  filter(cohort != 9999) %>%
  filter(!is.na(age)) %>%
  filter(age >= 18) %>%
  filter(health %in% 1:4) %>%
  mutate(
    srh = 5 - health,
    # Recode happy: higher = better
    happy = if_else(happy %in% 1:3, 4L - as.integer(happy), NA_integer_)
  ) %>%
  mutate(
    wt = coalesce(wtssall, wtssps)
  ) %>%
  filter(!is.na(wt))

rm(gss_all)

# Prepare BHAPC-formatted data for each outcome
prepare_bhapc_data <- function(df, outcome_var) {
  df %>%
    filter(!is.na(.data[[outcome_var]]), !is.na(age), !is.na(wt)) %>%
    filter(age >= 18, age <= 89) %>%
    mutate(
      period_4yr = map_to_period_4yr(year, "gss"),
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

bhapc_srh <- prepare_bhapc_data(data_gss_full, "srh")
bhapc_educ <- prepare_bhapc_data(data_gss_full, "educ")
bhapc_happy <- prepare_bhapc_data(data_gss_full, "happy")

message("Data prepared:")
message("  SRH N = ", format(nrow(bhapc_srh), big.mark = ","))
message("  Education N = ", format(nrow(bhapc_educ), big.mark = ","))
message("  Happiness N = ", format(nrow(bhapc_happy), big.mark = ","))

# ==============================================================================
# Figure 2: Descriptive Means (Generic Function)
# ==============================================================================

#' Create Figure 2 for any outcome variable
#'
#' @param bhapc_df Data frame with outcome, age_group, period_4yr, wt
#' @param outcome Character: column name for outcome variable
#' @param survey Character: survey name for title
#' @param outcome_label Character: y-axis label (e.g., "Mean Education (years)")
#' @param output_path Optional path to save figure
#' @return ggplot object
create_figure2_generic <- function(bhapc_df,
                                    outcome,
                                    survey = "gss",
                                    outcome_label = "Mean",
                                    output_path = NULL,
                                    width = 10,
                                    height = 7) {

  # Rename outcome column to generic name for processing
  df <- bhapc_df %>%
    rename(outcome_var = !!sym(outcome))

  # Create survey design
  svy_df <- df %>%
    as_survey_design(weights = wt)

  # Compute weighted means
  means_df <- svy_df %>%
    group_by(age_group, period_4yr) %>%
    summarise(
      mean_outcome = survey_mean(outcome_var, na.rm = TRUE, vartype = "se"),
      n = unweighted(n()),
      .groups = "drop"
    )

  # Create color palette for periods
  periods <- sort(unique(means_df$period_4yr))
  n_periods <- length(periods)
  period_colors <- scales::viridis_pal(option = "D", direction = -1)(n_periods)
  names(period_colors) <- periods

  # Create plot
  p <- ggplot(means_df, aes(x = age_group, y = mean_outcome,
                             color = period_4yr, group = period_4yr)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = mean_outcome - 1.96 * mean_outcome_se,
          ymax = mean_outcome + 1.96 * mean_outcome_se),
      width = 0.2, alpha = 0.5
    ) +
    scale_color_manual(values = period_colors, name = "Period") +
    labs(
      title = paste0(outcome_label, " by Age and Period (", toupper(survey), ")"),
      subtitle = "Survey-weighted means with 95% confidence intervals",
      x = "Age Group",
      y = outcome_label,
      caption = paste0("N = ", format(sum(means_df$n), big.mark = ","), " observations")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  # Save if path provided
  if (!is.null(output_path)) {
    ggsave(output_path, p, width = width, height = height, dpi = 300)
    message("Saved Figure 2 to: ", output_path)
  }

  p
}

# ==============================================================================
# 1. Generate Figure 2 for Education and Happiness
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("1. GENERATING FIGURE 2 (DESCRIPTIVE MEANS)")
message(paste(rep("=", 70), collapse = ""))

# Figure 2 for Education
message("\nGenerating Figure 2 for Education...")
fig2_educ <- create_figure2_generic(
  bhapc_educ,
  outcome = "educ",
  survey = "gss_education",
  outcome_label = "Mean Education (years)",
  output_path = file.path(output_dir, "figure2_gss_education.png")
)

# Figure 2 for Happiness
message("Generating Figure 2 for Happiness...")
fig2_happy <- create_figure2_generic(
  bhapc_happy,
  outcome = "happy",
  survey = "gss_happiness",
  outcome_label = "Mean Happiness (1-3 scale)",
  output_path = file.path(output_dir, "figure2_gss_happiness.png")
)

# Figure 2 for SRH (for combined figure)
message("Generating Figure 2 for SRH...")
fig2_srh <- create_figure2_generic(
  bhapc_srh,
  outcome = "srh",
  survey = "gss_srh",
  outcome_label = "Mean SRH (1-4 scale)",
  output_path = NULL  # Don't save individual, only combined
)

# ==============================================================================
# 2. Generate Combined Figure 2 (3-panel)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("2. GENERATING COMBINED FIGURE 2 (3-PANEL)")
message(paste(rep("=", 70), collapse = ""))

# Create cleaner versions without individual titles for combined plot
create_fig2_panel <- function(bhapc_df, outcome, outcome_label, panel_title) {

  df <- bhapc_df %>%
    rename(outcome_var = !!sym(outcome))

  svy_df <- df %>%
    as_survey_design(weights = wt)

  means_df <- svy_df %>%
    group_by(age_group, period_4yr) %>%
    summarise(
      mean_outcome = survey_mean(outcome_var, na.rm = TRUE, vartype = "se"),
      n = unweighted(n()),
      .groups = "drop"
    )

  periods <- sort(unique(means_df$period_4yr))
  n_periods <- length(periods)
  period_colors <- scales::viridis_pal(option = "D", direction = -1)(n_periods)
  names(period_colors) <- periods

  ggplot(means_df, aes(x = age_group, y = mean_outcome,
                        color = period_4yr, group = period_4yr)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    geom_errorbar(
      aes(ymin = mean_outcome - 1.96 * mean_outcome_se,
          ymax = mean_outcome + 1.96 * mean_outcome_se),
      width = 0.2, alpha = 0.4
    ) +
    scale_color_manual(values = period_colors, name = "Period") +
    labs(
      title = panel_title,
      x = "Age Group",
      y = outcome_label
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold")
    )
}

# Create individual panels
p_srh <- create_fig2_panel(bhapc_srh, "srh", "Mean SRH (1-4)", "Self-Rated Health")
p_educ <- create_fig2_panel(bhapc_educ, "educ", "Mean Education (years)", "Education")
p_happy <- create_fig2_panel(bhapc_happy, "happy", "Mean Happiness (1-3)", "Happiness")

# Extract legend from one panel
get_legend <- function(bhapc_df, outcome) {
  df <- bhapc_df %>%
    rename(outcome_var = !!sym(outcome))

  svy_df <- df %>%
    as_survey_design(weights = wt)

  means_df <- svy_df %>%
    group_by(age_group, period_4yr) %>%
    summarise(
      mean_outcome = survey_mean(outcome_var, na.rm = TRUE, vartype = "se"),
      n = unweighted(n()),
      .groups = "drop"
    )

  periods <- sort(unique(means_df$period_4yr))
  n_periods <- length(periods)
  period_colors <- scales::viridis_pal(option = "D", direction = -1)(n_periods)
  names(period_colors) <- periods

  p <- ggplot(means_df, aes(x = age_group, y = mean_outcome,
                             color = period_4yr, group = period_4yr)) +
    geom_line() +
    scale_color_manual(values = period_colors, name = "Period") +
    theme_minimal() +
    theme(legend.position = "bottom")

  cowplot::get_legend(p)
}

legend <- get_legend(bhapc_srh, "srh")

# Combine panels
combined_fig2 <- (p_srh | p_educ | p_happy) /
  cowplot::ggdraw(legend) +
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    title = "GSS Descriptive Means: SRH vs Education vs Happiness",
    subtitle = "Survey-weighted means by age group and period (with 95% CI)",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40")
    )
  )

ggsave(file.path(output_dir, "figure2_gss_combined.png"),
       combined_fig2, width = 14, height = 8, dpi = 300)
message("Saved combined Figure 2 to: ", file.path(output_dir, "figure2_gss_combined.png"))

# ==============================================================================
# 3. Generate Table 2 for Education and Happiness
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("3. GENERATING TABLE 2 (FIXED EFFECTS + VARIANCE)")
message(paste(rep("=", 70), collapse = ""))

# Table 2 for Education
message("\nGenerating Table 2 for Education...")
table2_educ <- create_table2(
  result_educ$model,
  bhapc_educ,
  survey = "gss_education",
  output_path = file.path(output_dir, "table2_gss_education.csv")
)
print(table2_educ)

# Table 2 for Happiness
message("\nGenerating Table 2 for Happiness...")
table2_happy <- create_table2(
  result_happy$model,
  bhapc_happy,
  survey = "gss_happiness",
  output_path = file.path(output_dir, "table2_gss_happiness.csv")
)
print(table2_happy)

# ==============================================================================
# 4. Generate Gradient by Period CSVs
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("4. GENERATING GRADIENT BY PERIOD CSVS")
message(paste(rep("=", 70), collapse = ""))

#' Compute gradient by period (young vs old difference)
#'
#' @param bhapc_df Data frame with outcome, period_4yr, age_group, wt
#' @param outcome Character: column name for outcome variable
#' @return Data frame with gradient by period
compute_gradient_by_period <- function(bhapc_df, outcome) {

  # Define young (18-21) and old (70-74 for GSS data coverage) age groups
  young_group <- "18-21"
  old_group <- "70-74"

  # Check available age groups
  available_groups <- unique(bhapc_df$age_group)
  if (!young_group %in% available_groups) {
    stop("Young age group '", young_group, "' not found in data")
  }
  if (!old_group %in% available_groups) {
    # Try 65-69 as fallback
    old_group <- "65-69"
    if (!old_group %in% available_groups) {
      stop("Old age group not found in data")
    }
    message("  Using '", old_group, "' as old age group")
  }

  # Filter to young and old groups
  df_filtered <- bhapc_df %>%
    filter(age_group %in% c(young_group, old_group)) %>%
    rename(outcome_var = !!sym(outcome))

  # Create survey design
  svy_df <- df_filtered %>%
    as_survey_design(weights = wt)

  # Compute weighted means by period and age group
  means_df <- svy_df %>%
    group_by(period_4yr, age_group) %>%
    summarise(
      mean_outcome = survey_mean(outcome_var, na.rm = TRUE, vartype = NULL),
      n = unweighted(n()),
      .groups = "drop"
    )

  # Compute gradient (young - old)
  gradient_df <- means_df %>%
    pivot_wider(
      names_from = age_group,
      values_from = c(mean_outcome, n),
      names_glue = "{.value}_{age_group}"
    ) %>%
    mutate(
      # Rename columns to standard format
      outcome_young = .data[[paste0("mean_outcome_", young_group)]],
      outcome_old = .data[[paste0("mean_outcome_", old_group)]],
      gradient = outcome_young - outcome_old,
      n_total = .data[[paste0("n_", young_group)]] + .data[[paste0("n_", old_group)]]
    ) %>%
    select(period_4yr, outcome_young, outcome_old, gradient, n_total) %>%
    arrange(period_4yr)

  # Rename columns to match main pipeline format
  outcome_name <- gsub("_.*", "", outcome)
  names(gradient_df) <- c("period_4yr",
                           paste0(outcome_name, "_young"),
                           paste0(outcome_name, "_old"),
                           "gradient", "n_total")

  gradient_df
}

# Gradient for Education
message("\nComputing gradient by period for Education...")
gradient_educ <- compute_gradient_by_period(bhapc_educ, "educ")
write_csv(gradient_educ, file.path(output_dir, "gradient_by_period_gss_education.csv"))
message("  Saved to: gradient_by_period_gss_education.csv")
print(gradient_educ)

# Gradient for Happiness
message("\nComputing gradient by period for Happiness...")
gradient_happy <- compute_gradient_by_period(bhapc_happy, "happy")
write_csv(gradient_happy, file.path(output_dir, "gradient_by_period_gss_happiness.csv"))
message("  Saved to: gradient_by_period_gss_happiness.csv")
print(gradient_happy)

# ==============================================================================
# 5. Generate Combined Variance Decomposition Table
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("5. GENERATING COMBINED VARIANCE TABLE")
message(paste(rep("=", 70), collapse = ""))

# Load individual variance files
var_srh <- read_csv(
  here::here("output/bhapc_parallel/gss/gss_variance_decomposition.csv"),
  show_col_types = FALSE
) %>%
  mutate(outcome = "SRH")

var_educ <- read_csv(
  file.path(output_dir, "gss_education_variance.csv"),
  show_col_types = FALSE
) %>%
  mutate(outcome = "Education")

var_happy <- read_csv(
  file.path(output_dir, "gss_happiness_variance.csv"),
  show_col_types = FALSE
) %>%
  mutate(outcome = "Happiness")

# Combine into single table
combined_variance <- bind_rows(var_srh, var_educ, var_happy) %>%
  select(outcome, component, variance, pct_of_total, sd) %>%
  arrange(outcome, component)

write_csv(combined_variance, file.path(output_dir, "gss_sanity_check_variance_combined.csv"))
message("Saved combined variance table to: gss_sanity_check_variance_combined.csv")

# Create summary view
variance_summary <- combined_variance %>%
  filter(component %in% c("cohort_4yr", "period_4yr", "Residual")) %>%
  select(outcome, component, pct_of_total) %>%
  pivot_wider(names_from = component, values_from = pct_of_total) %>%
  rename(
    cohort_pct = cohort_4yr,
    period_pct = period_4yr,
    residual_pct = Residual
  )

message("\nVariance Decomposition Summary:")
print(variance_summary)

# ==============================================================================
# 6. Summary
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SANITY CHECK OUTPUTS COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nOutput files created:")
message("  Figure 2 (descriptive means):")
message("    - figure2_gss_education.png")
message("    - figure2_gss_happiness.png")
message("    - figure2_gss_combined.png (3-panel comparison)")
message("  Table 2 (fixed effects + variance):")
message("    - table2_gss_education.csv")
message("    - table2_gss_happiness.csv")
message("  Gradient by period:")
message("    - gradient_by_period_gss_education.csv")
message("    - gradient_by_period_gss_happiness.csv")
message("  Combined variance:")
message("    - gss_sanity_check_variance_combined.csv")

message("\nInterpretation:")
message("  SRH: Cohort = ", round(variance_summary$cohort_pct[variance_summary$outcome == "SRH"], 2),
        "%, Period = ", round(variance_summary$period_pct[variance_summary$outcome == "SRH"], 2), "%")
message("  Education: Cohort = ", round(variance_summary$cohort_pct[variance_summary$outcome == "Education"], 2),
        "%, Period = ", round(variance_summary$period_pct[variance_summary$outcome == "Education"], 2), "%")
message("  Happiness: Cohort = ", round(variance_summary$cohort_pct[variance_summary$outcome == "Happiness"], 2),
        "%, Period = ", round(variance_summary$period_pct[variance_summary$outcome == "Happiness"], 2), "%")

message("\n")
