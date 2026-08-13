# ==============================================================================
# 04d_bhapc_random_effects_summary.R
# Create BHAPC Period/Cohort Variance Figure, Table, and Combined HAPC/BHAPC Figure
# Author: Christine Lucille Kuryla
#
# Purpose: Generate summary figures and tables showing Period and Cohort
# random effects variance from BHAPC (Bayesian) models, and create a combined
# figure comparing HAPC (Frequentist) vs BHAPC (Bayesian) results.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(patchwork)
  library(gridExtra)
  library(grid)
})

source(here::here("R", "srh_common_functions.R"))

# Configuration
SURVEY_DISPLAY_ORDER <- c("brfss", "meps", "nhis", "cps", "nhanes", "gss")
SURVEY_LABELS <- c(brfss = "BRFSS", meps = "MEPS", nhis = "NHIS",
                   cps = "CPS", nhanes = "NHANES", gss = "GSS")

# Colors for Period and Cohort only (no Age)
PC_COLORS <- c(Period = "#009E73", Cohort = "#CC79A7")

# Output directories
BHAPC_OUTPUT_DIR <- here::here("output", "bhapc_parallel")
BHAPC_FIGURES_DIR <- file.path(BHAPC_OUTPUT_DIR, "figures")
BHAPC_TABLES_DIR <- file.path(BHAPC_OUTPUT_DIR, "tables")
APC_FIGURES_DIR <- here::here("output", "apc", "figures")
APC_TABLES_DIR <- here::here("output", "apc", "tables")

# Ensure output directories exist
dir.create(BHAPC_FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(BHAPC_TABLES_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# Load BHAPC Variance Data
# ==============================================================================

cat("=== Loading BHAPC variance decomposition data ===\n")

# Read all survey variance files
bhapc_data <- map_dfr(SURVEY_DISPLAY_ORDER, function(survey) {
  file_path <- file.path(BHAPC_OUTPUT_DIR, survey,
                         paste0(survey, "_variance_decomposition.csv"))

  if (!file.exists(file_path)) {
    warning("File not found: ", file_path)
    return(NULL)
  }

  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(survey = survey)
})

cat("Loaded BHAPC data for surveys:", paste(unique(bhapc_data$survey), collapse = ", "), "\n")

# Filter to period_4yr and cohort_4yr components only
bhapc_pc <- bhapc_data %>%
  filter(component %in% c("period_4yr", "cohort_4yr", "Residual")) %>%
  mutate(
    component_clean = case_when(
      component == "period_4yr" ~ "Period",
      component == "cohort_4yr" ~ "Cohort",
      component == "Residual" ~ "Residual",
      TRUE ~ component
    )
  )

# Pivot to wide format for easier use
bhapc_wide <- bhapc_pc %>%
  select(survey, component_clean, pct_of_total) %>%
  pivot_wider(names_from = component_clean, values_from = pct_of_total)

cat("\nBHAPC variance percentages:\n")
print(bhapc_wide)

# ==============================================================================
# Load Survey Year Ranges
# ==============================================================================

cat("\n=== Loading survey year ranges ===\n")

survey_summary <- read_csv(here::here("output", "tables", "table_survey_summary.csv"),
                           show_col_types = FALSE)

# Create year range lookup
year_ranges <- survey_summary %>%
  select(Survey, `Year Range`) %>%
  mutate(survey_lower = tolower(Survey)) %>%
  select(survey_lower, years = `Year Range`)

# ==============================================================================
# Load HAPC Variance Data (for combined figure)
# ==============================================================================

cat("\n=== Loading HAPC variance decomposition data ===\n")

hapc_data <- read_csv(file.path(APC_TABLES_DIR, "variance_decomposition_complete.csv"),
                      show_col_types = FALSE)

# Filter to continuous SRH only (comparable to BHAPC which is continuous)
hapc_continuous <- hapc_data %>%
  filter(outcome == "continuous") %>%
  mutate(survey_lower = tolower(survey))

cat("HAPC continuous outcome surveys:", paste(unique(hapc_continuous$survey), collapse = ", "), "\n")

# ==============================================================================
# Prepare BHAPC Plot Data
# ==============================================================================

bhapc_plot_data <- bhapc_wide %>%
  left_join(year_ranges, by = c("survey" = "survey_lower")) %>%
  mutate(
    survey_factor = factor(survey, levels = SURVEY_DISPLAY_ORDER),
    survey_label = SURVEY_LABELS[survey]
  )

# ==============================================================================
# Figure 1: BHAPC Period and Cohort Bar Chart
# ==============================================================================

cat("\n=== Creating BHAPC Period/Cohort bar chart ===\n")

bhapc_long <- bhapc_plot_data %>%
  select(survey_factor, survey_label, Period, Cohort) %>%
  pivot_longer(cols = c(Period, Cohort),
               names_to = "Component",
               values_to = "Percent") %>%
  mutate(Component = factor(Component, levels = c("Period", "Cohort")))

p_bhapc <- ggplot(bhapc_long,
                  aes(x = Percent, y = fct_rev(survey_factor), fill = Component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = PC_COLORS, name = "Component") +
  scale_x_continuous(labels = function(x) paste0(round(x, 2), "%"),
                     expand = c(0, 0.1)) +
  scale_y_discrete(labels = function(x) SURVEY_LABELS[x]) +
  labs(
    title = "BHAPC Random Effects: Period and Cohort Variance",
    subtitle = "Percentage of total variance explained by period and cohort random effects (Bayesian HAPC)",
    x = "% of Total Variance Explained",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

# Save BHAPC bar chart
ggsave(file.path(BHAPC_FIGURES_DIR, "fig_bhapc_random_effects.png"),
       p_bhapc, width = 10, height = 6, dpi = 300)
ggsave(file.path(BHAPC_FIGURES_DIR, "fig_bhapc_random_effects.pdf"),
       p_bhapc, width = 10, height = 6)
cat("Saved: fig_bhapc_random_effects.png and .pdf\n")

# ==============================================================================
# Table 1: BHAPC CSV Summary
# ==============================================================================

cat("\n=== Creating BHAPC summary table (CSV) ===\n")

bhapc_summary_table <- bhapc_plot_data %>%
  arrange(survey_factor) %>%
  transmute(
    Survey = survey_label,
    Years = years,
    `Period %` = round(Period, 2),
    `Cohort %` = round(Cohort, 2),
    `Residual %` = round(Residual, 2)
  )

write_csv(bhapc_summary_table, file.path(BHAPC_TABLES_DIR, "bhapc_random_effects_table.csv"))
cat("Saved: bhapc_random_effects_table.csv\n")

# Print table to console
cat("\nBHAPC Variance Decomposition Summary:\n")
print(bhapc_summary_table, n = 10)

# ==============================================================================
# Table Figure: BHAPC Rendered Table Visualization
# ==============================================================================

cat("\n=== Creating BHAPC rendered table figure ===\n")

# Create a cleaner version for display
bhapc_display_table <- bhapc_summary_table %>%
  mutate(
    `Period %` = sprintf("%.2f%%", `Period %`),
    `Cohort %` = sprintf("%.2f%%", `Cohort %`),
    `Residual %` = sprintf("%.1f%%", `Residual %`)
  )

# Create table grob with styling
table_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 11),
    bg_params = list(
      fill = c(rep(c("white", "gray95"), length.out = nrow(bhapc_display_table)))
    )
  ),
  colhead = list(
    fg_params = list(fontsize = 12, fontface = "bold"),
    bg_params = list(fill = "gray80")
  )
)

table_grob <- tableGrob(bhapc_display_table, rows = NULL, theme = table_theme)

# Add title
title_grob <- textGrob(
  "BHAPC Variance Decomposition",
  gp = gpar(fontsize = 14, fontface = "bold")
)

subtitle_grob <- textGrob(
  "Period and Cohort Random Effects by Survey (Bayesian HAPC)",
  gp = gpar(fontsize = 11, col = "gray40")
)

# Combine title and table
padding <- unit(0.5, "lines")
full_table <- gtable::gtable_add_rows(table_grob, heights = grobHeight(title_grob) + padding, pos = 0)
full_table <- gtable::gtable_add_rows(full_table, heights = grobHeight(subtitle_grob) + padding, pos = 1)
full_table <- gtable::gtable_add_grob(full_table, title_grob, t = 1, l = 1, r = ncol(full_table))
full_table <- gtable::gtable_add_grob(full_table, subtitle_grob, t = 2, l = 1, r = ncol(full_table))

# Save BHAPC table figure
png(file.path(BHAPC_FIGURES_DIR, "fig_bhapc_table.png"),
    width = 10, height = 5, units = "in", res = 300)
grid.newpage()
grid.draw(full_table)
dev.off()

pdf(file.path(BHAPC_FIGURES_DIR, "fig_bhapc_table.pdf"),
    width = 10, height = 5)
grid.newpage()
grid.draw(full_table)
dev.off()

cat("Saved: fig_bhapc_table.png and .pdf\n")

# ==============================================================================
# Combined Figure: HAPC vs BHAPC Comparison
# ==============================================================================

cat("\n=== Creating combined HAPC vs BHAPC comparison figure ===\n")

# Prepare HAPC data (continuous only)
hapc_for_compare <- hapc_continuous %>%
  mutate(
    survey_factor = factor(survey_lower, levels = SURVEY_DISPLAY_ORDER),
    Model = "HAPC (Frequentist)"
  ) %>%
  select(survey_factor, Period = period_pct, Cohort = cohort_pct, Model) %>%
  pivot_longer(cols = c(Period, Cohort),
               names_to = "Component",
               values_to = "Percent")

# Prepare BHAPC data for comparison
bhapc_for_compare <- bhapc_plot_data %>%
  mutate(Model = "BHAPC (Bayesian)") %>%
  select(survey_factor, Period, Cohort, Model) %>%
  pivot_longer(cols = c(Period, Cohort),
               names_to = "Component",
               values_to = "Percent")

# Combine both datasets
combined_data <- bind_rows(hapc_for_compare, bhapc_for_compare) %>%
  mutate(
    Component = factor(Component, levels = c("Period", "Cohort")),
    Model = factor(Model, levels = c("HAPC (Frequentist)", "BHAPC (Bayesian)"))
  )

# Create combined faceted figure
p_combined <- ggplot(combined_data,
                     aes(x = Percent, y = fct_rev(survey_factor), fill = Component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~Model) +
  scale_fill_manual(values = PC_COLORS, name = "Component") +
  scale_x_continuous(labels = function(x) paste0(round(x, 2), "%"),
                     expand = c(0, 0.1)) +
  scale_y_discrete(labels = function(x) SURVEY_LABELS[x]) +
  labs(
    title = "Period and Cohort Random Effects: HAPC vs BHAPC",
    subtitle = "Percentage of total variance explained (Continuous SRH only)",
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

# Save combined figure
ggsave(file.path(APC_FIGURES_DIR, "fig_hapc_bhapc_comparison.png"),
       p_combined, width = 12, height = 6, dpi = 300)
ggsave(file.path(APC_FIGURES_DIR, "fig_hapc_bhapc_comparison.pdf"),
       p_combined, width = 12, height = 6)
cat("Saved: fig_hapc_bhapc_comparison.png and .pdf\n")

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== Summary complete ===\n")
cat("\nOutput files created:\n")
cat("  BHAPC outputs:\n")
cat("    - output/bhapc_parallel/figures/fig_bhapc_random_effects.png\n")
cat("    - output/bhapc_parallel/figures/fig_bhapc_random_effects.pdf\n")
cat("    - output/bhapc_parallel/figures/fig_bhapc_table.png\n")
cat("    - output/bhapc_parallel/figures/fig_bhapc_table.pdf\n")
cat("    - output/bhapc_parallel/tables/bhapc_random_effects_table.csv\n")
cat("  Combined HAPC vs BHAPC:\n")
cat("    - output/apc/figures/fig_hapc_bhapc_comparison.png\n")
cat("    - output/apc/figures/fig_hapc_bhapc_comparison.pdf\n")

cat("\n=== Key findings ===\n")

# BHAPC summary stats
cat("\nBHAPC averages across surveys:\n")
cat(sprintf("  Average Period %%: %.2f%%\n", mean(bhapc_wide$Period)))
cat(sprintf("  Average Cohort %%: %.2f%%\n", mean(bhapc_wide$Cohort)))

# HAPC continuous summary stats
cat("\nHAPC (continuous) averages across surveys:\n")
cat(sprintf("  Average Period %%: %.2f%%\n", mean(hapc_continuous$period_pct)))
cat(sprintf("  Average Cohort %%: %.2f%%\n", mean(hapc_continuous$cohort_pct)))

# Comparison
cat("\nComparison (BHAPC vs HAPC):\n")
comparison <- bhapc_wide %>%
  rename(bhapc_period = Period, bhapc_cohort = Cohort) %>%
  left_join(
    hapc_continuous %>%
      select(survey_lower, hapc_period = period_pct, hapc_cohort = cohort_pct),
    by = c("survey" = "survey_lower")
  ) %>%
  mutate(
    period_diff = bhapc_period - hapc_period,
    cohort_diff = bhapc_cohort - hapc_cohort
  ) %>%
  select(survey, bhapc_period, hapc_period, period_diff,
         bhapc_cohort, hapc_cohort, cohort_diff)

print(comparison)
