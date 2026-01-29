# ==============================================================================
# 07_fig_mortality_util_combined.R
# Combined Mortality + Hospital Utilization Figure
#
# Purpose:
#   Create a publication figure with:
#   - Panel A (left): 10-year rolling window mortality Cox regression by age (NHIS)
#   - Panel B (right): 2x2 hospital utilization coefficients (NHIS/MEPS rows x
#                      Hospitalized/Any ER columns)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)
library(cowplot)

# Source paths and shared functions
source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "plot_mortality.R"))

# Set theme
theme_set(theme_srh())

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)

# Output directories
output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# PART 1: LOAD DATA
# ==============================================================================

message("\n========== Loading data ==========\n")

# --- Mortality Results ---
mortality_path <- here("output", "mortality_results_all_windows_20260119.csv")
if (!file.exists(mortality_path)) {
  stop("Mortality results file not found: ", mortality_path)
}
mortality_results <- read_csv(mortality_path, show_col_types = FALSE) %>%
  filter(window_length == 10)

message("Loaded mortality results: ", nrow(mortality_results), " rows (10-year window)")
message("  Years: ", min(mortality_results$start_year), "-", max(mortality_results$start_year))
message("  Age groups: ", paste(unique(mortality_results$age_group), collapse = ", "))

# --- Utilization Coefficients ---
coef_nhis_path <- here("output", "tables", "fig_util_coef_nhis_20260124.rds")
coef_meps_path <- here("output", "tables", "fig_util_coef_meps_20260124.rds")

if (!file.exists(coef_nhis_path)) {
  stop("NHIS utilization coefficient file not found: ", coef_nhis_path)
}
if (!file.exists(coef_meps_path)) {
  stop("MEPS utilization coefficient file not found: ", coef_meps_path)
}

coef_nhis <- read_rds(coef_nhis_path)
coef_meps <- read_rds(coef_meps_path)

message("Loaded NHIS coefficients: ", nrow(coef_nhis), " rows")
message("Loaded MEPS coefficients: ", nrow(coef_meps), " rows")


# ==============================================================================
# PART 2: CREATE PANEL A - MORTALITY HAZARD RATIOS
# ==============================================================================

message("\n========== Creating Panel A: Mortality ==========\n")

# Use the existing plot_mortality_hr function with modifications for this figure
# No title - we'll add A/B labels via patchwork
# Add top margin to align with Panel B which has column titles
panel_a <- plot_mortality_hr(
  results = mortality_results,
  window_length = 10,
  show_ci = TRUE,
  log_scale = TRUE,
  title = NULL,
  y_label = "Hazard Ratio per 1-unit SRH",
  x_label = "Window Start Year",
  point_size = 2.0,
  line_width = 0.8,
  ci_alpha = 0.12
) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    plot.margin = margin(t = 25, r = 10, b = 5, l = 5)
  ) +
  guides(
    color = guide_legend(nrow = 1, title.position = "left"),
    fill = "none"
  )


# ==============================================================================
# PART 3: CREATE PANEL B - UTILIZATION 2x2 GRID
# ==============================================================================

message("\n========== Creating Panel B: Utilization ==========\n")

# Helper function to create a single utilization panel
# Adapted from 06_hosp_util_analysis.R create_age_subplot
create_util_panel <- function(
    data,
    show_col_title = FALSE,
    col_title = NULL,
    row_label = NULL,
    base_size = 12,
    xlim = NULL
) {

  if (is.null(data) || nrow(data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 5, color = "gray50") +
      theme_void()
    return(p)
  }

  # Build base plot
  p <- ggplot(data, aes(x = year, y = coefficient,
                        color = age_group, group = age_group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50",
               linewidth = 0.4, alpha = 0.6) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 1.5, alpha = 0.8) +
    scale_color_manual(values = age_colors_oi, name = "Age Group") +
    labs(
      x = NULL,
      y = "Coef (SRH)",
      title = if (show_col_title) col_title else NULL,
      tag = row_label
    )

  # Set x-axis limits
  if (!is.null(xlim)) {
    p <- p + scale_x_continuous(limits = xlim, breaks = scales::pretty_breaks(n = 4))
  } else {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }

  # Apply theme
  left_margin <- if (!is.null(row_label)) 28 else 4

  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.25),
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, color = "gray30"),
      plot.margin = margin(4, 6, 4, left_margin),
      legend.position = "none",
      plot.tag = element_text(size = base_size, face = "bold", angle = 90,
                              vjust = 0.5, hjust = 0.5),
      plot.tag.position = c(-0.06, 0.5)
    )

  return(p)
}

# Filter to just Hospitalized and Any ER Visit categories
categories_util <- c("Hospitalized", "Any ER Visit")

# Get year ranges
xlim_nhis <- c(min(coef_nhis$year, na.rm = TRUE), max(coef_nhis$year, na.rm = TRUE))
xlim_meps <- c(min(coef_meps$year, na.rm = TRUE), max(coef_meps$year, na.rm = TRUE))

# --- Create NHIS row ---
panel_nhis_hosp <- create_util_panel(
  data = coef_nhis %>% filter(category == "Hospitalized"),
  show_col_title = TRUE,
  col_title = "Hospitalized",
  row_label = "NHIS",
  xlim = xlim_nhis
)

panel_nhis_er <- create_util_panel(
  data = coef_nhis %>% filter(category == "Any ER Visit"),
  show_col_title = TRUE,
  col_title = "Any ER Visit",
  row_label = NULL,
  xlim = xlim_nhis
)

# --- Create MEPS row ---
panel_meps_hosp <- create_util_panel(
  data = coef_meps %>% filter(category == "Hospitalized"),
  show_col_title = FALSE,
  row_label = "MEPS",
  xlim = xlim_meps
)

panel_meps_er <- create_util_panel(
  data = coef_meps %>% filter(category == "Any ER Visit"),
  show_col_title = FALSE,
  row_label = NULL,
  xlim = xlim_meps
)

# --- Assemble Panel B content ---
row_nhis <- panel_nhis_hosp | panel_nhis_er
row_meps <- panel_meps_hosp | panel_meps_er

panel_b_content <- (row_nhis / row_meps)

# Wrap panel_b as a single element so it gets one tag
panel_b <- wrap_elements(full = panel_b_content)


# ==============================================================================
# PART 4: COMBINE PANELS A + B
# ==============================================================================

message("\n========== Combining panels ==========\n")

# Combine panels using patchwork with A/B labels at top
# Keep legend on panel_a, position under panel A (left-justified)
fig_combined <- (panel_a | panel_b) +
  plot_layout(widths = c(1, 1.3), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.box.margin = margin(t = 10),
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0, 1),
    plot.background = element_rect(fill = "white", color = NA)
  ) &
  guides(color = guide_legend(nrow = 1, title.position = "left"), fill = "none")


# ==============================================================================
# PART 5: SAVE OUTPUTS
# ==============================================================================

message("\n========== Saving figure ==========\n")

# Draft version with date
ggsave(
  filename = file.path(output_dir, paste0("fig_mortality_util_combined_draft_", date_suffix, ".png")),
  plot = fig_combined,
  width = 14, height = 7, dpi = 300,
  bg = "white"
)
message("Saved: fig_mortality_util_combined_draft_", date_suffix, ".png")

# Final versions (no date)
ggsave(
  filename = file.path(output_dir, "fig_mortality_util_combined.png"),
  plot = fig_combined,
  width = 14, height = 7, dpi = 300,
  bg = "white"
)
message("Saved: fig_mortality_util_combined.png")

ggsave(
  filename = file.path(output_dir, "fig_mortality_util_combined.pdf"),
  plot = fig_combined,
  width = 14, height = 7,
  bg = "white"
)
message("Saved: fig_mortality_util_combined.pdf")


# ==============================================================================
# PART 6: VERIFICATION
# ==============================================================================

message("\n========== Verification ==========\n")

# Check mortality HR range
mortality_hr_summary <- mortality_results %>%
  group_by(age_group) %>%
  summarise(
    n_windows = n(),
    mean_hr = mean(hr, na.rm = TRUE),
    min_hr = min(hr, na.rm = TRUE),
    max_hr = max(hr, na.rm = TRUE),
    .groups = "drop"
  )

message("Mortality HR summary (10-year window):")
for (i in 1:nrow(mortality_hr_summary)) {
  row <- mortality_hr_summary[i, ]
  message("  ", row$age_group, ": mean HR = ", round(row$mean_hr, 3),
          " (range: ", round(row$min_hr, 3), " - ", round(row$max_hr, 3), ")")
}

# Check utilization coefficient summary
check_util_coef <- function(df, name, categories) {
  if (is.null(df) || nrow(df) == 0) {
    message("  ", name, ": No data")
    return()
  }

  summary_df <- df %>%
    filter(category %in% categories) %>%
    group_by(category) %>%
    summarise(
      n_obs = n(),
      mean_coef = mean(coefficient, na.rm = TRUE),
      .groups = "drop"
    )

  message("\n", name, " utilization coefficients:")
  for (i in 1:nrow(summary_df)) {
    row <- summary_df[i, ]
    direction <- if (row$mean_coef < 0) "negative (expected)" else "positive"
    message("  ", row$category, ": mean coef = ", round(row$mean_coef, 4),
            " (", direction, ")")
  }
}

check_util_coef(coef_nhis, "NHIS", categories_util)
check_util_coef(coef_meps, "MEPS", categories_util)

message("\n========== Figure generation complete ==========\n")
