# ==============================================================================
# 03_figure3_transposed.R
# Figure 3 & 3B: Transposed Layout (Rows = Surveys, Cols = Categories)
#
# Purpose:
#   Creates the same figures as 03_figure3.R but with a transposed layout:
#   - Original: Rows = Categories, Cols = Surveys
#   - Transposed: Rows = Surveys, Cols = Categories
#
# Layout:
#   Row 1: BRFSS (Comorbidity Count | Mental Health | Physical Health Limitations)
#   Row 2: MEPS  (Comorbidity Count | Mental Health | Physical Health Limitations)
#   Row 3: NHIS  (Comorbidity Count | Mental Health | Physical Health Limitations)
#
# This script loads pre-computed tables from 03_figure3.R - no data loading or
# regression calculations are performed here.
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)

source(here("R", "functions", "theme_srh.R"))

# Set theme
theme_set(theme_srh())

# Output directories
output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")

# Date suffix for input files (must match 03_figure3.R output)
date_suffix <- "20260119"

# Current date for draft filenames
current_date <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# LOAD PRE-COMPUTED TABLES
# ==============================================================================

message("\n========== Loading pre-computed tables ==========\n")

# Coefficient tables (Figure 3)
coef_brfss <- read_rds(file.path(tables_dir, paste0("fig3_coef_by_age_brfss_", date_suffix, ".rds")))
coef_meps <- read_rds(file.path(tables_dir, paste0("fig3_coef_by_age_meps_", date_suffix, ".rds")))
coef_nhis <- read_rds(file.path(tables_dir, paste0("fig3_coef_by_age_nhis_", date_suffix, ".rds")))

message("  Loaded coefficient tables: BRFSS (", nrow(coef_brfss), " rows), MEPS (",
        nrow(coef_meps), " rows), NHIS (", nrow(coef_nhis), " rows)")

# Prevalence tables (Figure 3B)
prev_brfss <- read_rds(file.path(tables_dir, paste0("fig3b_prev_by_age_brfss_", date_suffix, ".rds")))
prev_meps <- read_rds(file.path(tables_dir, paste0("fig3b_prev_by_age_meps_", date_suffix, ".rds")))
prev_nhis <- read_rds(file.path(tables_dir, paste0("fig3b_prev_by_age_nhis_", date_suffix, ".rds")))

message("  Loaded prevalence tables: BRFSS (", nrow(prev_brfss), " rows), MEPS (",
        nrow(prev_meps), " rows), NHIS (", nrow(prev_nhis), " rows)")


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Variable descriptions for each survey/category combination
var_descriptions <- list(
  # Chronic conditions
  BRFSS_chronic = "Comorbidity count",
  MEPS_chronic = "Comorbidity count",
  NHIS_chronic = "Comorbidity count",
  # Mental health
  BRFSS_mental = "Mental bad days (0-30)",
  MEPS_mental = "K6 distress (0-6)",
  NHIS_mental = "K6 distress (0-6)",
  # Functional health
  BRFSS_functional = "Physical bad days (0-30)",
  MEPS_functional = "Physical limitation (0-4)",
  NHIS_functional = "Difficulty climbing (1-4)"
)

# Helper function to create a single panel
create_age_subplot <- function(
    data,
    y_var = "coefficient",
    show_title = FALSE,
    title = NULL,
    subtitle = NULL,
    show_ylabel = FALSE,
    ylabel = "Coefficient",
    show_legend = FALSE,
    base_size = 11,
    xlim = NULL,
    show_hline = TRUE,
    discontinuity_year = NULL
) {

  # Check if data has results
  if (is.null(data) || nrow(data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 4, color = "gray50") +
      theme_void()
    if (show_title && !is.null(title)) {
      p <- p + labs(title = title, subtitle = subtitle) +
        theme(plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = base_size - 1, hjust = 0.5, color = "gray50"))
    }
    return(p)
  }

  p <- ggplot(data, aes(x = year, y = .data[[y_var]],
                        color = age_group, group = age_group)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    geom_point(size = 1.2, alpha = 0.8) +
    scale_color_manual(values = age_colors, name = "Age Group") +
    labs(
      x = NULL,
      y = if (show_ylabel) ylabel else NULL,
      title = if (show_title) title else NULL,
      subtitle = subtitle
    )

  if (show_hline) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6)
  }

  # Add discontinuity annotation (e.g., NHIS 2019 redesign)
  if (!is.null(discontinuity_year)) {
    p <- p + geom_vline(xintercept = discontinuity_year, linetype = "dotted",
                        color = "red", alpha = 0.7, linewidth = 0.8)
  }

  if (!is.null(xlim)) {
    p <- p + scale_x_continuous(limits = xlim, breaks = scales::pretty_breaks(n = 4))
  } else {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }

  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size - 1, hjust = 0.5, color = "gray50"),
      axis.title.y = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1, color = "gray30"),
      plot.margin = margin(4, 6, 4, 6),
      legend.position = if (show_legend) "bottom" else "none"
    )

  return(p)
}


# ==============================================================================
# CALCULATE X-AXIS LIMITS FOR EACH SURVEY
# ==============================================================================

get_year_range <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(c(NA, NA))
  c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE))
}

xlim_brfss <- get_year_range(coef_brfss)
xlim_meps <- get_year_range(coef_meps)
xlim_nhis <- get_year_range(coef_nhis)


# ==============================================================================
# CREATE FIGURE 3 TRANSPOSED (COEFFICIENTS)
# ==============================================================================
# TRANSPOSED LAYOUT: Rows = Surveys, Columns = Categories
# Row 1: BRFSS (Chronic | Mental | Functional)
# Row 2: MEPS  (Chronic | Mental | Functional)
# Row 3: NHIS  (Chronic | Mental | Functional)
# ==============================================================================

message("\n========== Creating Figure 3 (Transposed) ==========\n")

# --- Row 1: BRFSS ---
p_brfss_chronic <- create_age_subplot(
  coef_brfss %>% filter(category == "Comorbidity Count"),
  show_title = TRUE, title = "Comorbidity Count",
  subtitle = var_descriptions$BRFSS_chronic,
  show_ylabel = TRUE, ylabel = "BRFSS",
  xlim = xlim_brfss
)

p_brfss_mental <- create_age_subplot(
  coef_brfss %>% filter(category == "Mental Health"),
  show_title = TRUE, title = "Mental Health",
  subtitle = var_descriptions$BRFSS_mental,
  xlim = xlim_brfss
)

p_brfss_functional <- create_age_subplot(
  coef_brfss %>% filter(category == "Physical Health Limitations"),
  show_title = TRUE, title = "Physical Health Limitations",
  subtitle = var_descriptions$BRFSS_functional,
  xlim = xlim_brfss
)

# --- Row 2: MEPS ---
p_meps_chronic <- create_age_subplot(
  coef_meps %>% filter(category == "Comorbidity Count"),
  subtitle = var_descriptions$MEPS_chronic,
  show_ylabel = TRUE, ylabel = "MEPS",
  xlim = xlim_meps
)

p_meps_mental <- create_age_subplot(
  coef_meps %>% filter(category == "Mental Health"),
  subtitle = var_descriptions$MEPS_mental,
  xlim = xlim_meps
)

p_meps_functional <- create_age_subplot(
  coef_meps %>% filter(category == "Physical Health Limitations"),
  subtitle = var_descriptions$MEPS_functional,
  xlim = xlim_meps
)

# --- Row 3: NHIS ---
p_nhis_chronic <- create_age_subplot(
  coef_nhis %>% filter(category == "Comorbidity Count"),
  subtitle = var_descriptions$NHIS_chronic,
  show_ylabel = TRUE, ylabel = "NHIS",
  xlim = xlim_nhis
)

p_nhis_mental <- create_age_subplot(
  coef_nhis %>% filter(category == "Mental Health"),
  subtitle = var_descriptions$NHIS_mental,
  xlim = xlim_nhis
)

p_nhis_functional <- create_age_subplot(
  coef_nhis %>% filter(category == "Physical Health Limitations"),
  subtitle = var_descriptions$NHIS_functional,
  xlim = xlim_nhis
)

# --- Assemble Figure 3 Transposed (rows=surveys, cols=categories) ---
fig3_transposed <- ((p_brfss_chronic | p_brfss_mental | p_brfss_functional) /
                    (p_meps_chronic | p_meps_mental | p_meps_functional) /
                    (p_nhis_chronic | p_nhis_mental | p_nhis_functional) /
                    guide_area()) +
  plot_layout(heights = c(1, 1, 1, 0.1), guides = "collect") +
  plot_annotation(
    title = "Coefficient Stability Across Age Groups",
    subtitle = "Effect of each covariate on SRH (higher covariate = worse health → negative coefficient)",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    )
  ) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10)) &
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# CREATE FIGURE 3B TRANSPOSED (PREVALENCE)
# ==============================================================================

message("\n========== Creating Figure 3B (Transposed) ==========\n")

# --- Row 1: BRFSS ---
p3b_brfss_chronic <- create_age_subplot(
  prev_brfss %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  show_title = TRUE, title = "Comorbidity Count",
  subtitle = var_descriptions$BRFSS_chronic,
  show_ylabel = TRUE, ylabel = "BRFSS",
  xlim = xlim_brfss, show_hline = FALSE
)

p3b_brfss_mental <- create_age_subplot(
  prev_brfss %>% filter(category == "Mental Health"),
  y_var = "mean",
  show_title = TRUE, title = "Mental Health",
  subtitle = var_descriptions$BRFSS_mental,
  xlim = xlim_brfss, show_hline = FALSE
)

p3b_brfss_functional <- create_age_subplot(
  prev_brfss %>% filter(category == "Physical Health Limitations"),
  y_var = "mean",
  show_title = TRUE, title = "Physical Health Limitations",
  subtitle = var_descriptions$BRFSS_functional,
  xlim = xlim_brfss, show_hline = FALSE
)

# --- Row 2: MEPS ---
p3b_meps_chronic <- create_age_subplot(
  prev_meps %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  subtitle = var_descriptions$MEPS_chronic,
  show_ylabel = TRUE, ylabel = "MEPS",
  xlim = xlim_meps, show_hline = FALSE
)

p3b_meps_mental <- create_age_subplot(
  prev_meps %>% filter(category == "Mental Health"),
  y_var = "mean",
  subtitle = var_descriptions$MEPS_mental,
  xlim = xlim_meps, show_hline = FALSE
)

p3b_meps_functional <- create_age_subplot(
  prev_meps %>% filter(category == "Physical Health Limitations"),
  y_var = "mean",
  subtitle = var_descriptions$MEPS_functional,
  xlim = xlim_meps, show_hline = FALSE
)

# --- Row 3: NHIS ---
p3b_nhis_chronic <- create_age_subplot(
  prev_nhis %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  subtitle = var_descriptions$NHIS_chronic,
  show_ylabel = TRUE, ylabel = "NHIS",
  xlim = xlim_nhis, show_hline = FALSE
)

p3b_nhis_mental <- create_age_subplot(
  prev_nhis %>% filter(category == "Mental Health"),
  y_var = "mean",
  subtitle = var_descriptions$NHIS_mental,
  xlim = xlim_nhis, show_hline = FALSE
)

p3b_nhis_functional <- create_age_subplot(
  prev_nhis %>% filter(category == "Physical Health Limitations"),
  y_var = "mean",
  subtitle = var_descriptions$NHIS_functional,
  xlim = xlim_nhis, show_hline = FALSE
)

# --- Assemble Figure 3B Transposed (rows=surveys, cols=categories) ---
fig3b_transposed <- ((p3b_brfss_chronic | p3b_brfss_mental | p3b_brfss_functional) /
                     (p3b_meps_chronic | p3b_meps_mental | p3b_meps_functional) /
                     (p3b_nhis_chronic | p3b_nhis_mental | p3b_nhis_functional) /
                     guide_area()) +
  plot_layout(heights = c(1, 1, 1, 0.1), guides = "collect") +
  plot_annotation(
    title = "Prevalence Trends by Age Group",
    subtitle = "Mean/prevalence of each covariate within age groups over time",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    )
  ) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10)) &
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# SAVE FIGURES
# ==============================================================================

message("\n========== Saving transposed figures ==========\n")

# Figure 3 Transposed (coefficients)
ggsave(
  filename = file.path(output_dir, paste0("fig3_coef_by_age_transposed_draft_", current_date, ".png")),
  plot = fig3_transposed,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3_coef_by_age_transposed.png"),
  plot = fig3_transposed,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3_coef_by_age_transposed.pdf"),
  plot = fig3_transposed,
  width = 12, height = 10
)
message("Saved: fig3_coef_by_age_transposed (.png and .pdf)")

# Figure 3B Transposed (prevalence)
ggsave(
  filename = file.path(output_dir, paste0("fig3b_prev_by_age_transposed_draft_", current_date, ".png")),
  plot = fig3b_transposed,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3b_prev_by_age_transposed.png"),
  plot = fig3b_transposed,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3b_prev_by_age_transposed.pdf"),
  plot = fig3b_transposed,
  width = 12, height = 10
)
message("Saved: fig3b_prev_by_age_transposed (.png and .pdf)")

message("\n========== Figure 3 & 3B Transposed Complete ==========\n")
