# ==============================================================================
# 21_sanity_check_combined.R
# Combined Cross-Survey Sanity Check Comparison
# Author: Christine Lucille Kuryla
#
# This script combines variance decomposition results from GSS, NHIS, and MEPS
# to create a unified comparison across surveys and outcomes
#
# Expected outcomes:
# - Education: Clear upward cohort trend across all surveys (~5-8%)
# - SRH: Modest cohort effect (~1-3%)
# - K6/Happiness: Modest cohort effect, similar to SRH (~0-3%)
# ==============================================================================

library(tidyverse)
library(patchwork)
library(here)

# ==============================================================================
# Setup
# ==============================================================================

# Create output directory
output_dir <- here::here("output/sanity_check_combined")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

# ==============================================================================
# 1. Load variance decomposition results from each survey
# ==============================================================================

message("Loading variance decomposition results from each survey...")

# GSS results
gss_dir <- here::here("output/sanity_check_bhapc")
if (file.exists(file.path(gss_dir, "gss_sanity_check_variance_combined.csv"))) {
  gss_combined <- read_csv(file.path(gss_dir, "gss_sanity_check_variance_combined.csv"), show_col_types = FALSE)
  message("  GSS: loaded combined variance (", nrow(gss_combined), " rows)")
} else {
  # Load individual variance files
  gss_srh <- read_csv(here::here("output/bhapc_parallel/gss/gss_variance_decomposition.csv"), show_col_types = FALSE) %>%
    mutate(variable = "SRH", survey = "GSS")
  gss_educ <- read_csv(file.path(gss_dir, "gss_education_variance.csv"), show_col_types = FALSE) %>%
    mutate(variable = "Education", survey = "GSS")
  gss_happy <- read_csv(file.path(gss_dir, "gss_happiness_variance.csv"), show_col_types = FALSE) %>%
    mutate(variable = "Happiness", survey = "GSS")
  gss_combined <- bind_rows(gss_srh, gss_educ, gss_happy)
  message("  GSS: loaded from individual files")
}

# NHIS results
nhis_dir <- here::here("output/sanity_check_nhis")
if (file.exists(file.path(nhis_dir, "nhis_sanity_check_comparison.csv"))) {
  nhis_comparison <- read_csv(file.path(nhis_dir, "nhis_sanity_check_comparison.csv"), show_col_types = FALSE)
  message("  NHIS: loaded comparison (", nrow(nhis_comparison), " rows)")
} else {
  warning("NHIS comparison file not found. Run script 16 first.")
  nhis_comparison <- tibble()
}

# MEPS results
meps_dir <- here::here("output/sanity_check_meps")
if (file.exists(file.path(meps_dir, "meps_sanity_check_comparison.csv"))) {
  meps_comparison <- read_csv(file.path(meps_dir, "meps_sanity_check_comparison.csv"), show_col_types = FALSE)
  message("  MEPS: loaded comparison (", nrow(meps_comparison), " rows)")
} else {
  warning("MEPS comparison file not found. Run script 20 first.")
  meps_comparison <- tibble()
}

# ==============================================================================
# 2. Create unified comparison table
# ==============================================================================

message("\nCreating unified comparison table...")

# Helper to extract cohort/period percentages from variance data
extract_pcts_from_variance <- function(var_df, variable_name, survey_name) {
  cohort_pct <- var_df %>%
    filter(component == "cohort_4yr") %>%
    pull(pct_of_total) %>%
    first()

  period_pct <- var_df %>%
    filter(component == "period_4yr") %>%
    pull(pct_of_total) %>%
    first()

  residual_pct <- var_df %>%
    filter(component == "Residual") %>%
    pull(pct_of_total) %>%
    first()

  tibble(
    survey = survey_name,
    variable = variable_name,
    cohort_pct = cohort_pct,
    period_pct = period_pct,
    residual_pct = residual_pct
  )
}

# Combine all comparison data
combined_comparison <- tibble()

# Add NHIS if available
if (nrow(nhis_comparison) > 0) {
  combined_comparison <- bind_rows(
    combined_comparison,
    nhis_comparison %>% select(survey, variable, cohort_pct, period_pct, residual_pct)
  )
}

# Add MEPS if available
if (nrow(meps_comparison) > 0) {
  combined_comparison <- bind_rows(
    combined_comparison,
    meps_comparison %>% select(survey, variable, cohort_pct, period_pct, residual_pct)
  )
}

# Add GSS from individual variance files if not loaded from comparison
if (exists("gss_combined") && nrow(gss_combined) > 0) {
  gss_summary <- gss_combined %>%
    filter(!is.na(variable)) %>%
    group_by(variable) %>%
    summarise(
      cohort_pct = pct_of_total[component == "cohort_4yr"],
      period_pct = pct_of_total[component == "period_4yr"],
      residual_pct = pct_of_total[component == "Residual"],
      .groups = "drop"
    ) %>%
    mutate(survey = "GSS") %>%
    select(survey, variable, cohort_pct, period_pct, residual_pct)

  combined_comparison <- bind_rows(combined_comparison, gss_summary)
}

# Harmonize variable names
combined_comparison <- combined_comparison %>%
  mutate(
    variable = case_when(
      variable %in% c("Education (4-cat)", "Education") ~ "Education",
      variable == "K6 Distress" ~ "K6",
      TRUE ~ variable
    )
  )

message("Combined comparison table:")
print(combined_comparison %>% arrange(variable, survey))

# Save combined table
write_csv(combined_comparison, file.path(output_dir, "combined_variance_comparison.csv"))
message("\nSaved combined variance comparison to: ", file.path(output_dir, "combined_variance_comparison.csv"))

# ==============================================================================
# 3. Create grouped bar chart (surveys x outcomes)
# ==============================================================================

message("\nCreating comparison figure...")

# Prepare data for plotting
plot_data <- combined_comparison %>%
  select(survey, variable, cohort_pct, period_pct) %>%
  pivot_longer(
    cols = c(cohort_pct, period_pct),
    names_to = "effect_type",
    values_to = "pct"
  ) %>%
  mutate(
    effect_type = case_when(
      effect_type == "cohort_pct" ~ "Cohort",
      effect_type == "period_pct" ~ "Period"
    ),
    variable = factor(variable, levels = c("SRH", "Education", "Happiness", "K6")),
    survey = factor(survey, levels = c("GSS", "NHIS", "MEPS"))
  ) %>%
  filter(!is.na(pct))

# Create faceted bar chart
p_comparison <- ggplot(plot_data,
                        aes(x = survey, y = pct, fill = effect_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  facet_wrap(~ variable, nrow = 1, scales = "free_x") +
  scale_fill_manual(
    values = c("Cohort" = "#CC79A7", "Period" = "#009E73"),
    name = "Effect Type"
  ) +
  labs(
    title = "Variance Explained by Period and Cohort Effects",
    subtitle = "Cross-Survey Sanity Check: Comparing SRH to Education and Mental Health Measures",
    x = "Survey",
    y = "% of Total Variance",
    caption = "Education shows strong cohort trend (positive control). SRH and mental health (Happiness, K6) show smaller cohort effects."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10)
  )

ggsave(file.path(output_dir, "combined_comparison_figure.png"),
       p_comparison, width = 14, height = 7, dpi = 300)

message("Saved comparison figure to: ", file.path(output_dir, "combined_comparison_figure.png"))

# ==============================================================================
# 4. Create cohort-focused comparison (bar chart by variable)
# ==============================================================================

message("\nCreating cohort-focused comparison...")

cohort_only <- combined_comparison %>%
  mutate(
    variable = factor(variable, levels = c("Education", "SRH", "Happiness", "K6")),
    survey = factor(survey, levels = c("GSS", "NHIS", "MEPS"))
  ) %>%
  filter(!is.na(cohort_pct))

p_cohort <- ggplot(cohort_only,
                   aes(x = variable, y = cohort_pct, fill = survey)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(cohort_pct, 1), "%")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(
    values = c("GSS" = "#0072B2", "NHIS" = "#E69F00", "MEPS" = "#56B4E9"),
    name = "Survey"
  ) +
  labs(
    title = "Cohort Effect Variance Explained Across Surveys",
    subtitle = "Education as positive control; SRH, Happiness, K6 as subjective measures",
    x = "",
    y = "% of Total Variance Explained by Cohort",
    caption = "Expected: Education > 5%; SRH, Happiness, K6 ~ 1-3%"
  ) +
  ylim(0, max(cohort_only$cohort_pct, na.rm = TRUE) * 1.3) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
  )

ggsave(file.path(output_dir, "cohort_comparison_figure.png"),
       p_cohort, width = 10, height = 7, dpi = 300)

message("Saved cohort comparison figure to: ", file.path(output_dir, "cohort_comparison_figure.png"))

# ==============================================================================
# 5. Summary statistics
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SUMMARY STATISTICS")
message(paste(rep("=", 70), collapse = ""))

# Summarize by variable
summary_by_var <- combined_comparison %>%
  group_by(variable) %>%
  summarise(
    n_surveys = n(),
    mean_cohort_pct = mean(cohort_pct, na.rm = TRUE),
    sd_cohort_pct = sd(cohort_pct, na.rm = TRUE),
    mean_period_pct = mean(period_pct, na.rm = TRUE),
    sd_period_pct = sd(period_pct, na.rm = TRUE),
    .groups = "drop"
  )

message("\nSummary by variable (across surveys):")
print(summary_by_var)

write_csv(summary_by_var, file.path(output_dir, "summary_by_variable.csv"))

# Interpretation
message("\nInterpretation:")

educ_mean <- summary_by_var %>% filter(variable == "Education") %>% pull(mean_cohort_pct)
srh_mean <- summary_by_var %>% filter(variable == "SRH") %>% pull(mean_cohort_pct)

if (!is.na(educ_mean) && !is.na(srh_mean)) {
  message("  - Education average cohort effect: ", round(educ_mean, 2), "%")
  message("  - SRH average cohort effect: ", round(srh_mean, 2), "%")
  message("  - Ratio (Education/SRH): ", round(educ_mean / srh_mean, 2), "x")

  if (educ_mean > 5) {
    message("  - Education shows strong cohort trend as expected (positive control)")
  }
  if (srh_mean < 5) {
    message("  - SRH shows modest cohort effect, consistent with expectations")
  }
}

# Mental health measures
k6_mean <- summary_by_var %>% filter(variable == "K6") %>% pull(mean_cohort_pct)
happy_mean <- summary_by_var %>% filter(variable == "Happiness") %>% pull(mean_cohort_pct)

if (!is.na(k6_mean)) {
  message("  - K6 average cohort effect: ", round(k6_mean, 2), "%")
}
if (!is.na(happy_mean)) {
  message("  - Happiness average cohort effect: ", round(happy_mean, 2), "%")
}

# ==============================================================================
# Done
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("COMBINED SANITY CHECK COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nOutput files saved to: ", output_dir)
message("  - combined_variance_comparison.csv")
message("  - combined_comparison_figure.png")
message("  - cohort_comparison_figure.png")
message("  - summary_by_variable.csv")

message("\n")
