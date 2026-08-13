# ==============================================================================
# 24_period_effects_comparison.R
# Plot period effects for SRH, Education, K6 on same scale (side by side)
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
# Helper functions
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

# Extract period effects and standardize
extract_period_effects <- function(result, bhapc_df, outcome_label) {
  model <- result$model
  random_effects <- extract_random_effects(model, bhapc_df)

  period_effects <- random_effects$period_effects %>%
    mutate(
      outcome = outcome_label,
      # Standardize to SD units for comparison
      estimate_std = estimate / sd(estimate),
      ci_lower_std = ci_lower_90 / sd(estimate),
      ci_upper_std = ci_upper_90 / sd(estimate)
    )

  period_effects
}

# Colors for outcomes
outcome_colors <- c(
  "SRH" = "#0072B2",
  "Education" = "#E69F00",
  "K6" = "#D55E00"
)

# ==============================================================================
# NHIS Period Effects Comparison
# ==============================================================================

create_nhis_period_comparison <- function() {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("Creating NHIS Period Effects Comparison")
  message(paste(rep("=", 70), collapse = ""))

  output_dir <- here::here("output/sanity_check_nhis")

  # Load data
  message("Loading NHIS data...")
  data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

  # Load models
  message("Loading models...")
  result_srh <- readRDS(file.path(output_dir, "result_srh.rds"))
  result_educ <- readRDS(file.path(output_dir, "result_educ.rds"))
  result_k6 <- readRDS(file.path(output_dir, "result_k6.rds"))

  # Prepare data
  bhapc_srh <- prepare_sanity_check_data(data_nhis, "srh", "nhis")
  bhapc_educ <- prepare_sanity_check_data(data_nhis, "educ_4cat", "nhis")
  data_nhis_k6 <- data_nhis %>% filter(!is.na(k6))
  bhapc_k6 <- prepare_sanity_check_data(data_nhis_k6, "k6", "nhis")

  # Extract period effects
  message("Extracting period effects...")
  period_srh <- extract_period_effects(result_srh, bhapc_srh, "SRH")
  period_educ <- extract_period_effects(result_educ, bhapc_educ, "Education")
  period_k6 <- extract_period_effects(result_k6, bhapc_k6, "K6")

  # Load variance for labels
  var_srh <- read_csv(file.path(output_dir, "nhis_srh_variance.csv"), show_col_types = FALSE)
  var_educ <- read_csv(file.path(output_dir, "nhis_education_variance.csv"), show_col_types = FALSE)
  var_k6 <- read_csv(file.path(output_dir, "nhis_k6_variance.csv"), show_col_types = FALSE)

  get_period_pct <- function(var_df) {
    round(var_df$pct_of_total[var_df$component == "period_4yr"], 2)
  }

  # Combine all period effects
  all_periods <- bind_rows(period_srh, period_educ, period_k6) %>%
    mutate(outcome = factor(outcome, levels = c("SRH", "Education", "K6")))

  # Find common y-axis range (in standardized units)
  y_range <- range(c(all_periods$ci_lower_std, all_periods$ci_upper_std), na.rm = TRUE)
  y_pad <- diff(y_range) * 0.1
  y_limits <- c(y_range[1] - y_pad, y_range[2] + y_pad)

  # Create individual panels with same scale
  create_period_panel <- function(df, outcome_name, period_pct, color) {
    ggplot(df, aes(x = period, y = estimate_std)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbar(
        aes(ymin = ci_lower_std, ymax = ci_upper_std),
        width = 0.8, color = color, linewidth = 0.8
      ) +
      geom_point(color = color, size = 3) +
      labs(
        title = paste0(outcome_name, " (", period_pct, "%)"),
        x = "Period (start year)",
        y = "Period effect (SD units)"
      ) +
      coord_cartesian(ylim = y_limits) +
      scale_x_continuous(
        breaks = unique(df$period),
        labels = as.character(unique(df$period))
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
      )
  }

  p_srh <- create_period_panel(period_srh, "SRH", get_period_pct(var_srh), outcome_colors["SRH"])
  p_educ <- create_period_panel(period_educ, "Education", get_period_pct(var_educ), outcome_colors["Education"])
  p_k6 <- create_period_panel(period_k6, "K6 Distress", get_period_pct(var_k6), outcome_colors["K6"])

  # Combine
  combined <- (p_srh | p_educ | p_k6) +
    plot_annotation(
      title = "NHIS: Period Effects Comparison (Same Scale)",
      subtitle = "Effects standardized to SD units for comparison across outcomes",
      caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained by period.",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
      )
    )

  ggsave(
    file.path(output_dir, "nhis_period_effects_comparison.png"),
    combined, width = 14, height = 5, dpi = 300
  )

  ggsave(
    file.path(output_dir, "nhis_period_effects_comparison.pdf"),
    combined, width = 14, height = 5
  )

  message("Saved: ", file.path(output_dir, "nhis_period_effects_comparison.png"))

  return(combined)
}

# ==============================================================================
# MEPS Period Effects Comparison
# ==============================================================================

create_meps_period_comparison <- function() {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("Creating MEPS Period Effects Comparison")
  message(paste(rep("=", 70), collapse = ""))

  output_dir <- here::here("output/sanity_check_meps")

  # Load data
  message("Loading MEPS data...")
  data_meps <- readr::read_rds(derived_path("data_meps.rds"))

  # Load models
  message("Loading models...")
  result_srh <- readRDS(file.path(output_dir, "result_srh.rds"))
  result_educ <- readRDS(file.path(output_dir, "result_educ.rds"))
  result_k6 <- readRDS(file.path(output_dir, "result_k6.rds"))

  # Prepare data
  bhapc_srh <- prepare_sanity_check_data(data_meps, "srh", "meps")
  bhapc_educ <- prepare_sanity_check_data(data_meps, "educ_4cat", "meps")
  data_meps_k6 <- data_meps %>% filter(year >= 2004, !is.na(K6SUM))
  bhapc_k6 <- prepare_sanity_check_data(data_meps_k6, "K6SUM", "meps")

  # Extract period effects
  message("Extracting period effects...")
  period_srh <- extract_period_effects(result_srh, bhapc_srh, "SRH")
  period_educ <- extract_period_effects(result_educ, bhapc_educ, "Education")
  period_k6 <- extract_period_effects(result_k6, bhapc_k6, "K6")

  # Load variance for labels
  var_srh <- read_csv(file.path(output_dir, "meps_srh_variance.csv"), show_col_types = FALSE)
  var_educ <- read_csv(file.path(output_dir, "meps_education_variance.csv"), show_col_types = FALSE)
  var_k6 <- read_csv(file.path(output_dir, "meps_k6_variance.csv"), show_col_types = FALSE)

  get_period_pct <- function(var_df) {
    round(var_df$pct_of_total[var_df$component == "period_4yr"], 2)
  }

  # Combine all period effects
  all_periods <- bind_rows(period_srh, period_educ, period_k6) %>%
    mutate(outcome = factor(outcome, levels = c("SRH", "Education", "K6")))

  # Find common y-axis range (in standardized units)
  y_range <- range(c(all_periods$ci_lower_std, all_periods$ci_upper_std), na.rm = TRUE)
  y_pad <- diff(y_range) * 0.1
  y_limits <- c(y_range[1] - y_pad, y_range[2] + y_pad)

  # Create individual panels with same scale
  create_period_panel <- function(df, outcome_name, period_pct, color) {
    ggplot(df, aes(x = period, y = estimate_std)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbar(
        aes(ymin = ci_lower_std, ymax = ci_upper_std),
        width = 0.8, color = color, linewidth = 0.8
      ) +
      geom_point(color = color, size = 3) +
      labs(
        title = paste0(outcome_name, " (", period_pct, "%)"),
        x = "Period (start year)",
        y = "Period effect (SD units)"
      ) +
      coord_cartesian(ylim = y_limits) +
      scale_x_continuous(
        breaks = unique(df$period),
        labels = as.character(unique(df$period))
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
      )
  }

  p_srh <- create_period_panel(period_srh, "SRH", get_period_pct(var_srh), outcome_colors["SRH"])
  p_educ <- create_period_panel(period_educ, "Education", get_period_pct(var_educ), outcome_colors["Education"])
  p_k6 <- create_period_panel(period_k6, "K6 Distress", get_period_pct(var_k6), outcome_colors["K6"])

  # Combine
  combined <- (p_srh | p_educ | p_k6) +
    plot_annotation(
      title = "MEPS: Period Effects Comparison (Same Scale)",
      subtitle = "Effects standardized to SD units for comparison across outcomes",
      caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained by period.",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
      )
    )

  ggsave(
    file.path(output_dir, "meps_period_effects_comparison.png"),
    combined, width = 14, height = 5, dpi = 300
  )

  ggsave(
    file.path(output_dir, "meps_period_effects_comparison.pdf"),
    combined, width = 14, height = 5
  )

  message("Saved: ", file.path(output_dir, "meps_period_effects_comparison.png"))

  return(combined)
}

# ==============================================================================
# Run both
# ==============================================================================

nhis_fig <- create_nhis_period_comparison()
meps_fig <- create_meps_period_comparison()

message("\n", paste(rep("=", 70), collapse = ""))
message("DONE")
message(paste(rep("=", 70), collapse = ""))
message("\nOutput files:")
message("  - output/sanity_check_nhis/nhis_period_effects_comparison.png")
message("  - output/sanity_check_meps/meps_period_effects_comparison.png")
