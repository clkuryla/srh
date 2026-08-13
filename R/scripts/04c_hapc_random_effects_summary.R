# ==============================================================================
# 04c_hapc_random_effects_summary.R
# Create HAPC Period/Cohort Variance Figure and Table
# Author: Christine Lucille Kuryla
#
# Purpose: Generate summary figures and tables showing only Period and Cohort
# random effects variance (excluding Age) from HAPC models.
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

OUTPUT_DIR <- here::here("output", "apc")
FIGURES_DIR <- file.path(OUTPUT_DIR, "figures")
TABLES_DIR <- file.path(OUTPUT_DIR, "tables")

# Ensure output directories exist
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# Load Data
# ==============================================================================

cat("=== Loading variance decomposition data ===\n")

# Load variance decomposition
decomp <- read_csv(file.path(TABLES_DIR, "variance_decomposition_complete.csv"),
                   show_col_types = FALSE)

# Load survey summary for year ranges
survey_summary <- read_csv(here::here("output", "tables", "table_survey_summary.csv"),
                           show_col_types = FALSE)

cat("Surveys in decomposition:", paste(unique(decomp$survey), collapse = ", "), "\n")

# Extract year ranges and create lookup
year_ranges <- survey_summary %>%
  select(Survey, `Year Range`) %>%
  mutate(survey_lower = tolower(Survey)) %>%
  select(survey_lower, years = `Year Range`)

# Join year ranges to decomposition data
decomp_with_years <- decomp %>%
  mutate(survey_lower = tolower(survey)) %>%
  left_join(year_ranges, by = "survey_lower")

# Prepare data for plotting
plot_data <- decomp_with_years %>%
  mutate(
    survey_upper = toupper(survey),
    outcome_label = case_when(
      outcome == "continuous" ~ "Continuous SRH",
      outcome == "binary" ~ "Fair/Poor (Binary)",
      TRUE ~ outcome
    ),
    survey_factor = factor(survey_lower, levels = SURVEY_DISPLAY_ORDER),
    survey_label = SURVEY_LABELS[survey_lower]
  )

cat("Data prepared with", nrow(plot_data), "rows\n\n")

# ==============================================================================
# Figure: Period and Cohort Only Bar Chart
# ==============================================================================

cat("=== Creating Period/Cohort bar chart ===\n")

plot_variance <- plot_data %>%
  select(survey_factor, survey_label, outcome_label,
         Period = period_pct, Cohort = cohort_pct) %>%
  pivot_longer(cols = c(Period, Cohort),
               names_to = "Component",
               values_to = "Percent") %>%
  mutate(Component = factor(Component, levels = c("Period", "Cohort")))

p_variance <- ggplot(plot_variance,
                     aes(x = Percent, y = fct_rev(survey_factor), fill = Component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~outcome_label) +
  scale_fill_manual(values = PC_COLORS, name = "Component") +
  scale_x_continuous(labels = function(x) paste0(round(x, 2), "%"),
                     expand = c(0, 0.1)) +
  scale_y_discrete(labels = function(x) SURVEY_LABELS[x]) +
  labs(
    title = "HAPC Random Effects: Period and Cohort Variance",
    subtitle = "Percentage of total variance explained by period and cohort random effects",
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

# Save bar chart
ggsave(file.path(FIGURES_DIR, "fig_hapc_random_effects.png"),
       p_variance, width = 10, height = 6, dpi = 300)
ggsave(file.path(FIGURES_DIR, "fig_hapc_random_effects.pdf"),
       p_variance, width = 10, height = 6)
cat("Saved: fig_hapc_random_effects.png and .pdf\n")

# ==============================================================================
# Table: CSV Summary
# ==============================================================================

cat("\n=== Creating summary table ===\n")

summary_table <- plot_data %>%
  arrange(survey_factor, outcome_label) %>%
  transmute(
    Survey = survey_label,
    Years = years,
    Outcome = outcome_label,
    `Period %` = round(period_pct, 2),
    `Cohort %` = round(cohort_pct, 2),
    `Residual %` = round(residual_pct, 2),
    `R2 Marginal` = round(r2_marginal, 3),
    `R2 Conditional` = round(r2_conditional, 3)
  )

write_csv(summary_table, file.path(TABLES_DIR, "hapc_random_effects_table.csv"))
cat("Saved: hapc_random_effects_table.csv\n")

# Print table to console
cat("\nHAPC Variance Decomposition Summary:\n")
print(summary_table, n = 20)

# ==============================================================================
# Table Figure: Rendered Table Visualization
# ==============================================================================

cat("\n=== Creating rendered table figure ===\n")

# Create a cleaner version for display
display_table <- summary_table %>%
  mutate(
    `Period %` = sprintf("%.2f%%", `Period %`),
    `Cohort %` = sprintf("%.2f%%", `Cohort %`),
    `Residual %` = sprintf("%.1f%%", as.numeric(gsub("%", "", `Residual %`))),
    `R2 Marginal` = sprintf("%.3f", `R2 Marginal`),
    `R2 Conditional` = sprintf("%.3f", `R2 Conditional`)
  )

# Create table grob with styling
table_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 10),
    bg_params = list(
      fill = c(rep(c("white", "gray95"), length.out = nrow(display_table)))
    )
  ),
  colhead = list(
    fg_params = list(fontsize = 11, fontface = "bold"),
    bg_params = list(fill = "gray80")
  )
)

table_grob <- tableGrob(display_table, rows = NULL, theme = table_theme)

# Add title
title_grob <- textGrob(
  "HAPC Variance Decomposition",
  gp = gpar(fontsize = 14, fontface = "bold")
)

subtitle_grob <- textGrob(
  "Period and Cohort Random Effects by Survey and Outcome",
  gp = gpar(fontsize = 11, col = "gray40")
)

# Combine title and table
padding <- unit(0.5, "lines")
full_table <- gtable::gtable_add_rows(table_grob, heights = grobHeight(title_grob) + padding, pos = 0)
full_table <- gtable::gtable_add_rows(full_table, heights = grobHeight(subtitle_grob) + padding, pos = 1)
full_table <- gtable::gtable_add_grob(full_table, title_grob, t = 1, l = 1, r = ncol(full_table))
full_table <- gtable::gtable_add_grob(full_table, subtitle_grob, t = 2, l = 1, r = ncol(full_table))

# Save table figure
png(file.path(FIGURES_DIR, "fig_hapc_table.png"),
    width = 12, height = 8, units = "in", res = 300)
grid.newpage()
grid.draw(full_table)
dev.off()

pdf(file.path(FIGURES_DIR, "fig_hapc_table.pdf"),
    width = 12, height = 8)
grid.newpage()
grid.draw(full_table)
dev.off()

cat("Saved: fig_hapc_table.png and .pdf\n")

# ==============================================================================
# Summary
# ==============================================================================

cat("\n=== Summary complete ===\n")
cat("\nOutput files created:\n")
cat("  - output/apc/figures/fig_hapc_random_effects.png\n")
cat("  - output/apc/figures/fig_hapc_random_effects.pdf\n")
cat("  - output/apc/figures/fig_hapc_table.png\n")
cat("  - output/apc/figures/fig_hapc_table.pdf\n")
cat("  - output/apc/tables/hapc_random_effects_table.csv\n")

cat("\nKey findings:\n")
summary_table %>%
  group_by(Outcome) %>%
  summarise(
    `Avg Period %` = mean(`Period %`),
    `Avg Cohort %` = mean(`Cohort %`),
    .groups = "drop"
  ) %>%
  print()
