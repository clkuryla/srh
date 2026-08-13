# ==============================================================================
# 25_gss_effects_comparison.R
# Plot cohort and period effects for SRH, Education, Happiness on same scale
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
# Helper functions
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

# Colors for outcomes
outcome_colors <- c(
  "SRH" = "#0072B2",
  "Education" = "#E69F00",
  "Happiness" = "#56B4E9"
)

# ==============================================================================
# Load GSS data and models
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Loading GSS Data and Models")
message(paste(rep("=", 70), collapse = ""))

output_dir <- here::here("output/sanity_check_bhapc")

# Load GSS data
message("Loading GSS data...")
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

# Prepare data for each outcome
bhapc_srh <- prepare_sanity_check_data(data_gss, "srh")
bhapc_educ <- prepare_sanity_check_data(data_gss, "educ")
bhapc_happy <- prepare_sanity_check_data(data_gss, "happy")

message("Data prepared: SRH=", nrow(bhapc_srh), ", Educ=", nrow(bhapc_educ), ", Happy=", nrow(bhapc_happy))

# Load models
message("Loading models...")
result_srh <- readRDS(here::here("output/bhapc_parallel/gss/gss_bhapc_model.rds"))
result_educ <- readRDS(file.path(output_dir, "result_educ.rds"))
result_happy <- readRDS(file.path(output_dir, "result_happy.rds"))

# Load variance data
var_srh <- read_csv(here::here("output/bhapc_parallel/gss/gss_variance_decomposition.csv"), show_col_types = FALSE)
var_educ <- read_csv(file.path(output_dir, "gss_education_variance.csv"), show_col_types = FALSE)
var_happy <- read_csv(file.path(output_dir, "gss_happiness_variance.csv"), show_col_types = FALSE)

get_pct <- function(var_df, component) {
  round(var_df$pct_of_total[var_df$component == component], 2)
}

# ==============================================================================
# Cohort Effects Comparison
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Creating GSS Cohort Effects Comparison")
message(paste(rep("=", 70), collapse = ""))

# Extract cohort effects
cohort_srh <- extract_cohort_effects(result_srh, bhapc_srh, "SRH")
cohort_educ <- extract_cohort_effects(result_educ, bhapc_educ, "Education")
cohort_happy <- extract_cohort_effects(result_happy, bhapc_happy, "Happiness")

# Combine for common scale
all_cohorts <- bind_rows(cohort_srh, cohort_educ, cohort_happy)
y_range <- range(c(all_cohorts$ci_lower_std, all_cohorts$ci_upper_std), na.rm = TRUE)
y_pad <- diff(y_range) * 0.1
y_limits_cohort <- c(y_range[1] - y_pad, y_range[2] + y_pad)

# Create cohort panels
create_cohort_panel <- function(df, outcome_name, cohort_pct, color, y_limits) {
  ggplot(df, aes(x = cohort, y = estimate_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_std, ymax = ci_upper_std),
      width = 2, color = color, linewidth = 0.6, alpha = 0.7
    ) +
    geom_point(color = color, size = 2.5) +
    labs(
      title = paste0(outcome_name, " (", cohort_pct, "%)"),
      x = "Birth cohort",
      y = "Cohort effect (SD units)"
    ) +
    coord_cartesian(ylim = y_limits) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
    )
}

p_cohort_srh <- create_cohort_panel(cohort_srh, "SRH", get_pct(var_srh, "cohort_4yr"),
                                     outcome_colors["SRH"], y_limits_cohort)
p_cohort_educ <- create_cohort_panel(cohort_educ, "Education", get_pct(var_educ, "cohort_4yr"),
                                      outcome_colors["Education"], y_limits_cohort)
p_cohort_happy <- create_cohort_panel(cohort_happy, "Happiness", get_pct(var_happy, "cohort_4yr"),
                                       outcome_colors["Happiness"], y_limits_cohort)

combined_cohort <- (p_cohort_srh | p_cohort_educ | p_cohort_happy) +
  plot_annotation(
    title = "GSS: Cohort Effects Comparison (Same Scale)",
    subtitle = "Effects standardized to SD units for comparison across outcomes",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained by cohort.",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
    )
  )

ggsave(
  file.path(output_dir, "gss_cohort_effects_comparison.png"),
  combined_cohort, width = 14, height = 5, dpi = 300
)

ggsave(
  file.path(output_dir, "gss_cohort_effects_comparison.pdf"),
  combined_cohort, width = 14, height = 5
)

message("Saved: ", file.path(output_dir, "gss_cohort_effects_comparison.png"))

# ==============================================================================
# Period Effects Comparison
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("Creating GSS Period Effects Comparison")
message(paste(rep("=", 70), collapse = ""))

# Extract period effects
period_srh <- extract_period_effects(result_srh, bhapc_srh, "SRH")
period_educ <- extract_period_effects(result_educ, bhapc_educ, "Education")
period_happy <- extract_period_effects(result_happy, bhapc_happy, "Happiness")

# Combine for common scale
all_periods <- bind_rows(period_srh, period_educ, period_happy)
y_range <- range(c(all_periods$ci_lower_std, all_periods$ci_upper_std), na.rm = TRUE)
y_pad <- diff(y_range) * 0.1
y_limits_period <- c(y_range[1] - y_pad, y_range[2] + y_pad)

# Create period panels
create_period_panel <- function(df, outcome_name, period_pct, color, y_limits) {
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

p_period_srh <- create_period_panel(period_srh, "SRH", get_pct(var_srh, "period_4yr"),
                                     outcome_colors["SRH"], y_limits_period)
p_period_educ <- create_period_panel(period_educ, "Education", get_pct(var_educ, "period_4yr"),
                                      outcome_colors["Education"], y_limits_period)
p_period_happy <- create_period_panel(period_happy, "Happiness", get_pct(var_happy, "period_4yr"),
                                       outcome_colors["Happiness"], y_limits_period)

combined_period <- (p_period_srh | p_period_educ | p_period_happy) +
  plot_annotation(
    title = "GSS: Period Effects Comparison (Same Scale)",
    subtitle = "Effects standardized to SD units for comparison across outcomes",
    caption = "Bayesian HAPC model with 90% credible intervals. Percentages show variance explained by period.",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
    )
  )

ggsave(
  file.path(output_dir, "gss_period_effects_comparison.png"),
  combined_period, width = 14, height = 5, dpi = 300
)

ggsave(
  file.path(output_dir, "gss_period_effects_comparison.pdf"),
  combined_period, width = 14, height = 5
)

message("Saved: ", file.path(output_dir, "gss_period_effects_comparison.png"))

# ==============================================================================
# Done
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("DONE")
message(paste(rep("=", 70), collapse = ""))
message("\nOutput files:")
message("  - output/sanity_check_bhapc/gss_cohort_effects_comparison.png")
message("  - output/sanity_check_bhapc/gss_period_effects_comparison.png")
