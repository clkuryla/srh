# ==============================================================================
# 27_multi_outcome_variance_comparison.R
# Create variance comparison figure and table for multiple outcomes across surveys
# Author: Christine Lucille Kuryla
#
# Purpose: Generate a comparison figure showing Period and Cohort variance
# explained across MEPS, NHIS, and GSS for multiple outcomes (SRH, Education,
# K6/Happiness), analogous to fig_hapc_bhapc_comparison but faceted by outcome.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(patchwork)
  library(gridExtra)
  library(grid)
})

# Output directory
OUTPUT_DIR <- here::here("output", "sanity_check_combined")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Colors for Period and Cohort
PC_COLORS <- c(Period = "#009E73", Cohort = "#CC79A7")

# Survey display order and labels
SURVEY_ORDER <- c("MEPS", "NHIS", "GSS")

# ==============================================================================
# Load All Variance Data (no recomputation)
# ==============================================================================

cat("=== Loading variance decomposition data ===\n")

# Helper function to read and label variance file
read_variance <- function(file_path, survey, outcome) {
  if (!file.exists(file_path)) {
    warning("File not found: ", file_path)
    return(NULL)
  }
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(survey = survey, outcome = outcome)
}

# Load all variance files
variance_data <- bind_rows(
  # MEPS
  read_variance(here::here("output/sanity_check_meps/meps_srh_variance.csv"), "MEPS", "SRH"),
  read_variance(here::here("output/sanity_check_meps/meps_education_variance.csv"), "MEPS", "Education"),
  read_variance(here::here("output/sanity_check_meps/meps_k6_variance.csv"), "MEPS", "K6"),

  # NHIS
  read_variance(here::here("output/sanity_check_nhis/nhis_srh_variance.csv"), "NHIS", "SRH"),
  read_variance(here::here("output/sanity_check_nhis/nhis_education_variance.csv"), "NHIS", "Education"),
  read_variance(here::here("output/sanity_check_nhis/nhis_k6_variance.csv"), "NHIS", "K6"),

  # GSS
  read_variance(here::here("output/bhapc_parallel/gss/gss_variance_decomposition.csv"), "GSS", "SRH"),
  read_variance(here::here("output/sanity_check_bhapc/gss_education_variance.csv"), "GSS", "Education"),
  read_variance(here::here("output/sanity_check_bhapc/gss_happiness_variance.csv"), "GSS", "Happiness")
)

cat("Loaded variance data for", nrow(variance_data), "rows\n")

# ==============================================================================
# Prepare Data for Plotting
# ==============================================================================

# Filter to period and cohort only, clean labels
plot_data <- variance_data %>%
  filter(component %in% c("period_4yr", "cohort_4yr")) %>%
  mutate(
    Component = case_when(
      component == "period_4yr" ~ "Period",
      component == "cohort_4yr" ~ "Cohort"
    ),
    Component = factor(Component, levels = c("Period", "Cohort")),
    survey = factor(survey, levels = SURVEY_ORDER),
    # Create outcome label with K6/Happiness distinction
    outcome_label = case_when(
      outcome == "K6" & survey == "GSS" ~ "Happiness",  # Should not happen but safety
      outcome == "Happiness" ~ "Happiness",
      outcome == "K6" ~ "K6",
      TRUE ~ outcome
    ),
    # Facet label combines outcome type
    outcome_facet = case_when(
      outcome %in% c("K6", "Happiness") ~ "K6 / Happiness",
      TRUE ~ outcome
    ),
    outcome_facet = factor(outcome_facet, levels = c("SRH", "Education", "K6 / Happiness"))
  )

# Print summary
cat("\nVariance summary by survey and outcome:\n")
plot_data %>%
  select(survey, outcome, Component, pct_of_total) %>%
  pivot_wider(names_from = Component, values_from = pct_of_total) %>%
  arrange(survey, outcome) %>%
  print(n = 20)

# ==============================================================================
# Figure: Multi-Outcome Variance Comparison (faceted by outcome)
# ==============================================================================

cat("\n=== Creating multi-outcome variance comparison figure ===\n")

p_comparison <- ggplot(plot_data,
                       aes(x = pct_of_total, y = fct_rev(survey), fill = Component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~outcome_facet, ncol = 3) +
  scale_fill_manual(values = PC_COLORS, name = "Component") +
  scale_x_continuous(
    labels = function(x) paste0(round(x, 1), "%"),
    expand = c(0, 0.2)
  ) +
  labs(
    title = "Period and Cohort Random Effects: Multiple Outcomes Across Surveys",
    subtitle = "Percentage of total variance explained by period and cohort (BHAPC models)",
    x = "% of Total Variance Explained",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    strip.text = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

# Save figure
ggsave(file.path(OUTPUT_DIR, "fig_multi_outcome_variance_comparison.png"),
       p_comparison, width = 14, height = 6, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "fig_multi_outcome_variance_comparison.pdf"),
       p_comparison, width = 14, height = 6)
cat("Saved: fig_multi_outcome_variance_comparison.png and .pdf\n")

# ==============================================================================
# Table: CSV Summary
# ==============================================================================

cat("\n=== Creating variance summary table (CSV) ===\n")

# Wide format table
variance_table <- plot_data %>%
  select(survey, outcome, Component, pct_of_total) %>%
  pivot_wider(names_from = Component, values_from = pct_of_total) %>%
  arrange(outcome, survey) %>%
  transmute(
    Outcome = outcome,
    Survey = survey,
    `Period %` = round(Period, 2),
    `Cohort %` = round(Cohort, 2)
  )

write_csv(variance_table, file.path(OUTPUT_DIR, "multi_outcome_variance_table.csv"))
cat("Saved: multi_outcome_variance_table.csv\n")

# Print table
cat("\nVariance Decomposition Summary:\n")
print(variance_table, n = 20)

# ==============================================================================
# Table Figure: Rendered Table Visualization
# ==============================================================================

cat("\n=== Creating rendered table figure ===\n")

# Format for display
display_table <- variance_table %>%
  mutate(
    `Period %` = sprintf("%.2f%%", `Period %`),
    `Cohort %` = sprintf("%.2f%%", `Cohort %`)
  )

# Create table grob
table_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 11),
    bg_params = list(
      fill = c(rep(c("white", "gray95"), length.out = nrow(display_table)))
    )
  ),
  colhead = list(
    fg_params = list(fontsize = 12, fontface = "bold"),
    bg_params = list(fill = "gray80")
  )
)

table_grob <- tableGrob(display_table, rows = NULL, theme = table_theme)

# Add title
title_grob <- textGrob(
  "Multi-Outcome Variance Decomposition",
  gp = gpar(fontsize = 14, fontface = "bold")
)

subtitle_grob <- textGrob(
  "Period and Cohort Random Effects by Survey and Outcome (BHAPC)",
  gp = gpar(fontsize = 11, col = "gray40")
)

# Combine title and table
padding <- unit(0.5, "lines")
full_table <- gtable::gtable_add_rows(table_grob, heights = grobHeight(title_grob) + padding, pos = 0)
full_table <- gtable::gtable_add_rows(full_table, heights = grobHeight(subtitle_grob) + padding, pos = 1)
full_table <- gtable::gtable_add_grob(full_table, title_grob, t = 1, l = 1, r = ncol(full_table))
full_table <- gtable::gtable_add_grob(full_table, subtitle_grob, t = 2, l = 1, r = ncol(full_table))

# Save table figure
png(file.path(OUTPUT_DIR, "fig_multi_outcome_variance_table.png"),
    width = 8, height = 6, units = "in", res = 300)
grid.newpage()
grid.draw(full_table)
dev.off()

pdf(file.path(OUTPUT_DIR, "fig_multi_outcome_variance_table.pdf"),
    width = 8, height = 6)
grid.newpage()
grid.draw(full_table)
dev.off()
cat("Saved: fig_multi_outcome_variance_table.png and .pdf\n")

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== Summary ===\n")
cat("\nOutput files created:\n")
cat("  - output/sanity_check_combined/fig_multi_outcome_variance_comparison.png\n")
cat("  - output/sanity_check_combined/fig_multi_outcome_variance_comparison.pdf\n")
cat("  - output/sanity_check_combined/fig_multi_outcome_variance_table.png\n")
cat("  - output/sanity_check_combined/fig_multi_outcome_variance_table.pdf\n")
cat("  - output/sanity_check_combined/multi_outcome_variance_table.csv\n")

# Summary statistics
cat("\n=== Key findings ===\n")
summary_stats <- plot_data %>%
  group_by(outcome_facet, Component) %>%
  summarise(
    mean_pct = mean(pct_of_total),
    min_pct = min(pct_of_total),
    max_pct = max(pct_of_total),
    .groups = "drop"
  )

cat("\nAverage variance explained by outcome type:\n")
print(summary_stats)
