# GSS Sensitivity Analysis - Gantt Chart Timeline
# Shows cohort/period effects across different year restrictions
# Author: Claude
# Date: 2026-02-06

library(tidyverse)
library(here)

source(here::here("R/functions/theme_srh.R"))

# ============================================================================
# DATA: Compile variance decomposition from all GSS restrictions
# ============================================================================

gss_restrictions <- tribble(
  ~label,           ~start_year, ~end_year, ~cohort_pct, ~period_pct,
  "Full (1972-2024)",      1972,      2024,       2.35,       0.14,
  "1994-2024",             1994,      2024,       2.09,       0.48,
  "2004-2024",             2004,      2024,       1.54,       0.43,
  "1972-2014",             1972,      2014,       1.13,       0.17,
  "1994-2014",             1994,      2014,       0.06,       0.68
) |>
  mutate(
    label = fct_reorder(label, start_year + (end_year - start_year)/2),
    midpoint = (start_year + end_year) / 2,
    span = end_year - start_year
  )

# Order by midpoint for visual clarity
gss_restrictions <- gss_restrictions |>
  arrange(desc(end_year), start_year) |>
  mutate(y_pos = row_number())

# ============================================================================
# PLOT 1: Timeline with cohort effect as color
# ============================================================================

p1 <- ggplot(gss_restrictions) +
  geom_segment(
    aes(x = start_year, xend = end_year, y = y_pos, yend = y_pos, color = cohort_pct),
    linewidth = 12,
    lineend = "round"
  ) +
  geom_text(
    aes(x = midpoint, y = y_pos, label = sprintf("C: %.1f%%  P: %.1f%%", cohort_pct, period_pct)),
    color = "white", fontface = "bold", size = 3.5
  ) +
  geom_text(
    aes(x = start_year - 1, y = y_pos, label = label),
    hjust = 1, size = 3.5
  ) +
  scale_color_gradient(
    low = "#2166AC", high = "#B2182B",
    name = "Cohort\nVariance %",
    limits = c(0, 2.5)
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2025, 10),
    limits = c(1955, 2026)
  ) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  labs(
    title = "GSS BHAPC Sensitivity: Variance Decomposition by Time Window",
    subtitle = "C = Cohort %, P = Period % of total variance",
    x = "Survey Year",
    y = NULL
  ) +
  theme_srh() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right"
  )

# ============================================================================
# PLOT 2: Dual bars showing both cohort and period
# ============================================================================

gss_long <- gss_restrictions |>
  select(label, start_year, end_year, y_pos, cohort_pct, period_pct) |>
  pivot_longer(
    cols = c(cohort_pct, period_pct),
    names_to = "component",
    values_to = "pct"
  ) |>
  mutate(
    component = if_else(component == "cohort_pct", "Cohort", "Period"),
    y_offset = if_else(component == "Cohort", 0.15, -0.15)
  )

p2 <- ggplot(gss_restrictions) +
  # Main timeline bar (gray background)
  geom_segment(
    aes(x = start_year, xend = end_year, y = y_pos, yend = y_pos),
    color = "gray80", linewidth = 20, lineend = "butt"
  ) +
  # Cohort bar (upper)
  geom_segment(
    aes(x = start_year, xend = end_year, y = y_pos + 0.15, yend = y_pos + 0.15,
        color = cohort_pct),
    linewidth = 8, lineend = "round"
  ) +
  # Period bar (lower)
  geom_segment(
    aes(x = start_year, xend = end_year, y = y_pos - 0.15, yend = y_pos - 0.15),
    color = "#7570B3", linewidth = 8, lineend = "round", alpha = 0.7
  ) +
  # Labels

geom_text(
    aes(x = start_year - 1, y = y_pos, label = label),
    hjust = 1, size = 3.2, fontface = "bold"
  ) +
  # Cohort % annotation
  geom_text(
    aes(x = end_year + 1, y = y_pos + 0.15, label = sprintf("%.1f%%", cohort_pct)),
    hjust = 0, size = 3, color = "#B2182B"
  ) +
  # Period % annotation
  geom_text(
    aes(x = end_year + 1, y = y_pos - 0.15, label = sprintf("%.1f%%", period_pct)),
    hjust = 0, size = 3, color = "#7570B3"
  ) +
  scale_color_gradient(
    low = "#FDDBC7", high = "#B2182B",
    name = "Cohort %",
    limits = c(0, 2.5)
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2025, 10),
    limits = c(1955, 2030)
  ) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  labs(
    title = "GSS BHAPC: Cohort & Period Variance by Time Window",
    subtitle = expression(paste("Upper bar (red scale) = Cohort variance %; Lower bar (purple) = Period variance %")),
    x = "Survey Year",
    y = NULL
  ) +
  theme_srh() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  )

# ============================================================================
# PLOT 3: Simple clean version with annotations
# ============================================================================

p3 <- ggplot(gss_restrictions) +
  # Timeline bars
  geom_segment(
    aes(x = start_year, xend = end_year, y = reorder(label, -y_pos), yend = reorder(label, -y_pos)),
    color = "#1F78B4", linewidth = 14, lineend = "round", alpha = 0.8
  ) +
  # Cohort annotation (left side of bar)
  geom_point(
    aes(x = start_year + 2, y = reorder(label, -y_pos), size = cohort_pct),
    color = "#E31A1C", alpha = 0.9
  ) +
  geom_text(
    aes(x = start_year + 2, y = reorder(label, -y_pos),
        label = sprintf("C:%.1f%%", cohort_pct)),
    color = "white", size = 2.5, fontface = "bold"
  ) +
  # Period annotation (right side of bar)
  geom_point(
    aes(x = end_year - 2, y = reorder(label, -y_pos), size = period_pct * 5),
    color = "#6A3D9A", alpha = 0.9
  ) +
  geom_text(
    aes(x = end_year - 2, y = reorder(label, -y_pos),
        label = sprintf("P:%.1f%%", period_pct)),
    color = "white", size = 2.5, fontface = "bold"
  ) +
  scale_size_continuous(range = c(8, 18), guide = "none") +
  scale_x_continuous(
    breaks = seq(1970, 2025, 10),
    limits = c(1968, 2028)
  ) +
  labs(
    title = "GSS BHAPC Sensitivity: Variance by Time Window",
    subtitle = "Circle size proportional to variance %; C = Cohort, P = Period",
    x = "Survey Year",
    y = NULL
  ) +
  theme_srh() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# ============================================================================
# PLOT 4: Clean horizontal bar chart (most readable)
# ============================================================================

gss_restrictions <- gss_restrictions |>
  mutate(
    year_label = sprintf("%d-%d", start_year, end_year),
    display_label = paste0(label, "\n(", year_label, ")")
  )

# Define explicit order to match Gantt chart (top to bottom: 1994-2014, 1972-2014, 2004-2024, 1994-2024, Full)
bar_order <- c("Full (1972-2024)", "1994-2024", "2004-2024", "1972-2014", "1994-2014")

# Reshape for grouped bars
gss_bar <- gss_restrictions |>
  select(label, cohort_pct, period_pct) |>
  pivot_longer(cols = c(cohort_pct, period_pct), names_to = "component", values_to = "pct") |>
  mutate(
    component = if_else(component == "cohort_pct", "Cohort", "Period"),
    label = factor(label, levels = bar_order)
  )

p4 <- ggplot(gss_bar, aes(x = pct, y = label, fill = component)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = sprintf("%.2f%%", pct)),
    position = position_dodge(width = 0.7),
    hjust = -0.1, size = 3
  ) +
  scale_fill_manual(
    values = c("Cohort" = "#E31A1C", "Period" = "#6A3D9A"),
    name = "Component"
  ) +
  scale_x_continuous(
    limits = c(0, 3),
    breaks = seq(0, 3, 0.5),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "GSS BHAPC: Variance Decomposition by Time Restriction",
    subtitle = "Cohort and period effects as % of total variance",
    x = "% of Total Variance",
    y = NULL
  ) +
  theme_srh() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

# ============================================================================
# COMBINED FIGURE: Gantt + Bar
# ============================================================================

library(patchwork)

p_combined <- p1 / p4 +
  plot_annotation(
    title = "GSS BHAPC Sensitivity Analysis",
    subtitle = "How cohort and period variance change with different time windows",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  ) +
  plot_layout(heights = c(1, 1))

# ============================================================================
# SAVE
# ============================================================================

output_dir <- here("output/bhapc_parallel/gss_sensitivity_timeline")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(output_dir, "gss_gantt_cohort.png"), p1, width = 10, height = 5, dpi = 300)
ggsave(file.path(output_dir, "gss_gantt_dual.png"), p2, width = 10, height = 5, dpi = 300)
ggsave(file.path(output_dir, "gss_gantt_circles.png"), p3, width = 10, height = 5, dpi = 300)
ggsave(file.path(output_dir, "gss_variance_bars.png"), p4, width = 9, height = 5, dpi = 300)
ggsave(file.path(output_dir, "gss_sensitivity_combined.png"), p_combined, width = 10, height = 9, dpi = 300)
ggsave(file.path(output_dir, "gss_sensitivity_combined.pdf"), p_combined, width = 10, height = 9)

cat("\n=== GSS Sensitivity Gantt Chart ===\n")
cat("Saved to:", output_dir, "\n")
cat("\nData summary:\n")
print(gss_restrictions |> select(label, start_year, end_year, cohort_pct, period_pct))
