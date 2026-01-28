# ==============================================================================
# 05_apc_supplementary_figure.R
#
# Composite APC Supplementary Figure
#
# Purpose:
#   Generate a publication-ready supplementary figure with scientifically
#   defensible claims. De-emphasizes Period vs Cohort comparison (due to
#   APC identification problem) and focuses on:
#   - Age dominates variance (5-15%)
#   - Temporal effects (P+C combined) are small (<3%)
#   - Age × Period interaction is significant
#   - Lexis diagrams show no diagonal banding
#
# Inputs:
#   - output/figures/fig2a_lexis_independent.png (existing Lexis figure)
#   - output/apc/tables/interaction_test_results.csv
#   - output/apc/tables/variance_decomposition_complete.csv
#
# Outputs:
#   - output/apc/figures/fig_apc_supplementary.png
#   - output/apc/figures/fig_apc_supplementary.pdf
#
# Author: Christine Lucille Kuryla
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(patchwork)
  library(grid)
  library(gridExtra)
  library(png)
})

source(here::here("R", "srh_common_functions.R"))

cat("========================================\n")
cat("APC Supplementary Figure\n")
cat("========================================\n\n")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SURVEY_ORDER <- c("BRFSS", "NHIS", "MEPS", "NHANES", "GSS", "CPS")

APC_COLORS <- c(
  "Age" = "#0072B2",      # blue
  "Period" = "#009E73",   # bluish green
  "Cohort" = "#CC79A7"    # reddish purple
)

OUTPUT_DIR <- here::here("output", "apc", "figures")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data...\n")

# Interaction test results
interaction_df <- read_csv(
  here::here("output", "apc", "tables", "interaction_test_results.csv"),
  show_col_types = FALSE
)

# Variance decomposition
variance_df <- read_csv(
  here::here("output", "apc", "tables", "variance_decomposition_complete.csv"),
  show_col_types = FALSE
)

cat("  Interaction results: ", nrow(interaction_df), " rows\n")
cat("  Variance decomposition: ", nrow(variance_df), " rows\n\n")

# ==============================================================================
# ROW 1: LEXIS DIAGRAMS (from existing image) - UNCHANGED
# ==============================================================================

cat("Loading Lexis diagram...\n")

lexis_path <- here::here("output", "figures", "fig2a_lexis_independent.png")

if (!file.exists(lexis_path)) {
  cat("  WARNING: Lexis figure not found at ", lexis_path, "\n")
  cat("  Run R/scripts/02_figure2_lexis.R first to generate it.\n")
  cat("  Creating placeholder...\n")

  row1_lexis <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Lexis Diagrams\n(Run 02_figure2_lexis.R to generate)",
             size = 6, hjust = 0.5) +
    theme_void() +
    labs(tag = "A")
} else {
  # Read PNG and convert to ggplot grob
  lexis_img <- readPNG(lexis_path)

  row1_lexis <- ggplot() +
    annotation_raster(lexis_img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    coord_fixed(ratio = nrow(lexis_img) / ncol(lexis_img)) +
    theme_void() +
    theme(plot.margin = margin(5, 5, 5, 5)) +
    labs(tag = "A")

  cat("  Loaded: fig2a_lexis_independent.png\n")
}

# ==============================================================================
# ROW 2: INTERACTION TEST TABLE - TWO SECTIONS (CONTINUOUS / BINARY)
# ==============================================================================

cat("Creating interaction test table (two sections)...\n")

# Format p-values as p < 10^order
format_p_order <- function(p) {
  if (is.na(p) || p == 0) return("p < 10\u207b\u00b3\u2070\u2070")

  # Get order of magnitude
  order <- floor(log10(p))

  # Use Unicode superscripts for cleaner display
  superscripts <- c("\u2070", "\u00b9", "\u00b2", "\u00b3", "\u2074", "\u2075", "\u2076", "\u2077", "\u2078", "\u2079")

  # Convert order to superscript string
  order_abs <- abs(order)
  order_str <- ""
  for (digit in strsplit(as.character(order_abs), "")[[1]]) {
    order_str <- paste0(order_str, superscripts[as.integer(digit) + 1])
  }

  if (order < 0) {
    paste0("p < 10\u207b", order_str)
  } else {
    paste0("p < 10", order_str)
  }
}

# Prepare table data - split by outcome type
table_base <- interaction_df %>%
  mutate(
    survey = factor(survey, levels = SURVEY_ORDER),
    f_display = sprintf("%.2f", f_stat),
    p_display = sapply(p_value, format_p_order)
  ) %>%
  arrange(survey)

# Continuous table
table_continuous <- table_base %>%
  filter(outcome == "continuous") %>%
  select(Survey = survey, `F-statistic` = f_display, `p-value` = p_display)

# Binary table
table_binary <- table_base %>%
  filter(outcome == "binary") %>%
  select(Survey = survey, `F-statistic` = f_display, `p-value` = p_display)

# Create table theme
table_theme <- ttheme_minimal(
  base_size = 10,
  core = list(
    fg_params = list(hjust = 0.5, x = 0.5),
    bg_params = list(fill = c("gray95", "white"))
  ),
  colhead = list(
    fg_params = list(fontface = "bold", hjust = 0.5),
    bg_params = list(fill = "gray85")
  )
)

# Create table grobs
table_grob_continuous <- tableGrob(table_continuous, rows = NULL, theme = table_theme)
table_grob_binary <- tableGrob(table_binary, rows = NULL, theme = table_theme)

# Add section headers
header_continuous <- textGrob("Continuous SRH", gp = gpar(fontface = "bold", fontsize = 11))
header_binary <- textGrob("Fair/Poor (Binary)", gp = gpar(fontface = "bold", fontsize = 11))

# Combine header + table for each section
section_continuous <- arrangeGrob(header_continuous, table_grob_continuous,
                                   ncol = 1, heights = c(0.15, 0.85))
section_binary <- arrangeGrob(header_binary, table_grob_binary,
                               ncol = 1, heights = c(0.15, 0.85))

# Combine both sections side by side
combined_table <- arrangeGrob(section_continuous, section_binary, ncol = 2)

# Wrap in ggplot for patchwork compatibility
row2_table <- ggplot() +
  annotation_custom(combined_table) +
  theme_void() +
  labs(
    title = "Age \u00d7 Period Interaction Test Results",
    subtitle = "All surveys show significant interaction (age gradient changed over time)",
    tag = "B"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.margin = margin(10, 10, 10, 10)
  )

cat("  Table created: ", nrow(table_continuous), " continuous + ", nrow(table_binary), " binary rows\n")

# ==============================================================================
# ROW 3: VARIANCE DECOMPOSITION BAR CHART - UPDATED CAPTIONS
# ==============================================================================

cat("Creating variance decomposition chart (with temporal effects framing)...\n")

# Check which surveys have data
available_surveys <- unique(toupper(variance_df$survey))
missing_surveys <- setdiff(SURVEY_ORDER, available_surveys)

if (length(missing_surveys) > 0) {
  cat("  Note: Missing variance data for: ", paste(missing_surveys, collapse = ", "), "\n")
}

# Prepare variance data
variance_plot_data <- variance_df %>%
  mutate(
    survey = toupper(survey),
    outcome_label = case_when(
      outcome == "continuous" ~ "Continuous SRH",
      outcome == "binary" ~ "Fair/Poor (Binary)",
      TRUE ~ outcome
    )
  ) %>%
  select(survey, outcome_label, Age = age_pct, Period = period_pct, Cohort = cohort_pct) %>%
  pivot_longer(cols = c(Age, Period, Cohort), names_to = "Component", values_to = "Percent") %>%
  mutate(
    survey = factor(survey, levels = SURVEY_ORDER),
    Component = factor(Component, levels = c("Age", "Period", "Cohort"))
  ) %>%
  filter(!is.na(survey))

# Add "coming soon" placeholder for missing surveys
if (length(missing_surveys) > 0) {
  placeholder_data <- expand_grid(
    survey = factor(missing_surveys, levels = SURVEY_ORDER),
    outcome_label = c("Continuous SRH", "Fair/Poor (Binary)"),
    Component = factor(c("Age", "Period", "Cohort"), levels = c("Age", "Period", "Cohort")),
    Percent = NA_real_
  )
  variance_plot_data <- bind_rows(variance_plot_data, placeholder_data)
}

# Calculate combined temporal effects for annotation
temporal_summary <- variance_df %>%
  mutate(survey = toupper(survey)) %>%
  group_by(survey) %>%
  summarize(
    max_temporal = max(period_pct + cohort_pct, na.rm = TRUE),
    .groups = "drop"
  )

max_temporal <- max(temporal_summary$max_temporal, na.rm = TRUE)

row3_variance <- ggplot(variance_plot_data,
                        aes(x = Percent, y = fct_rev(survey), fill = Component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, na.rm = TRUE) +
  geom_text(
    data = variance_plot_data %>%
      filter(is.na(Percent)) %>%
      distinct(survey, outcome_label) %>%
      mutate(x = 5, label = "(pending)"),
    aes(x = x, y = fct_rev(survey), label = label),
    inherit.aes = FALSE,
    size = 3, color = "gray50", hjust = 0.5
  ) +
  facet_wrap(~outcome_label) +
  scale_fill_manual(
    values = APC_COLORS,
    name = "Component",
    labels = c("Age (fixed)", "Period (random)", "Cohort (random)")
  ) +
  scale_x_continuous(
    labels = function(x) paste0(round(x, 1), "%"),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Variance Decomposition: Age (fixed) vs Temporal Effects (random)",
    subtitle = sprintf("Age dominates (5-15%%); temporal effects (period + cohort) together explain <%.0f%%",
                       ceiling(max_temporal)),
    x = "% of Total Variance Explained",
    y = NULL,
    tag = "C"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )

cat("  Variance chart created with temporal effects framing\n")

# ==============================================================================
# ROW 4: METHODOLOGICAL NOTE PANEL (REPLACES P/C RATIO)
# ==============================================================================

cat("Creating methodological note panel...\n")

# Create interpretation and methodological note text
interpretation_text <- paste0(
  "Interpretation:\n",
  "\u2022 Age (fixed effects) explains 5-15% of SRH variance\n",
  "\u2022 Temporal effects (period + cohort, random effects) add <3%\n",
  "\u2022 Lexis diagrams show no diagonal (cohort) patterns\n",
  "\u2022 Age \u00d7 Period interaction is significant in all surveys"
)

methods_note_text <- paste0(
  "Methodological Note:\n",
  "Due to the APC identification problem (Cohort = Period - Age),\n",
  "period and cohort effects cannot be definitively separated.\n",
  "We report their combined contribution as \"temporal effects.\"\n",
  "The absence of cohort patterns in Lexis diagrams, combined\n",
  "with significant age \u00d7 period interactions, supports the\n",
  "interpretation of period-driven convergence."
)

# Create text panel using ggplot
row4_methods <- ggplot() +
  # Interpretation box (left side)
  annotate(
    "label",
    x = 0.25, y = 0.5,
    label = interpretation_text,
    hjust = 0.5, vjust = 0.5,
    size = 3.5,
    lineheight = 1.2,
    fill = "#E8F4E8",  # light green background
    label.size = 0.5,
    label.padding = unit(0.5, "lines"),
    label.r = unit(0.15, "lines")
  ) +
  # Methodological note box (right side)
  annotate(
    "label",
    x = 0.75, y = 0.5,
    label = methods_note_text,
    hjust = 0.5, vjust = 0.5,
    size = 3.5,
    lineheight = 1.2,
    fill = "#FFF8E7",  # light yellow/cream background
    label.size = 0.5,
    label.padding = unit(0.5, "lines"),
    label.r = unit(0.15, "lines")
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  labs(
    title = "Summary and Methodological Considerations",
    tag = "D"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.margin = margin(10, 20, 10, 20)
  )

cat("  Methodological note panel created\n")

# ==============================================================================
# COMBINE ALL ROWS
# ==============================================================================

cat("\nAssembling composite figure...\n")

# Combine using patchwork
# Row 1 is the Lexis image (full width)
# Rows 2-4 are the charts

composite_figure <- row1_lexis /
  row2_table /
  row3_variance /
  row4_methods +
  plot_layout(heights = c(1.5, 1.2, 1, 0.8))

cat("  Composite assembled\n")

# ==============================================================================
# SAVE FIGURE
# ==============================================================================

cat("\nSaving figure...\n")

# PNG (high resolution for publication)
ggsave(
  filename = file.path(OUTPUT_DIR, "fig_apc_supplementary.png"),
  plot = composite_figure,
  width = 14,
  height = 20,
  dpi = 300,
  bg = "white"
)
cat("  Saved: fig_apc_supplementary.png\n")

# PDF (vector format)
ggsave(
  filename = file.path(OUTPUT_DIR, "fig_apc_supplementary.pdf"),
  plot = composite_figure,
  width = 14,
  height = 20,
  bg = "white"
)
cat("  Saved: fig_apc_supplementary.pdf\n")

# Also save with date for versioning
draft_date <- format(Sys.Date(), "%Y%m%d")
ggsave(
  filename = file.path(OUTPUT_DIR, paste0("fig_apc_supplementary_", draft_date, ".png")),
  plot = composite_figure,
  width = 14,
  height = 20,
  dpi = 300,
  bg = "white"
)
cat("  Saved: fig_apc_supplementary_", draft_date, ".png\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("APC Supplementary Figure complete!\n")
cat("========================================\n")
cat("\nOutputs:\n")
cat("  output/apc/figures/fig_apc_supplementary.png\n")
cat("  output/apc/figures/fig_apc_supplementary.pdf\n")
cat("\nFigure components:\n")
cat("  A. Lexis diagrams (6 surveys)\n")
cat("  B. Interaction test table (Continuous | Binary sections)\n")
cat("  C. Variance decomposition (temporal effects framing)\n")
cat("  D. Methodological note panel\n")
cat("\nSurveys with complete data: ", paste(available_surveys, collapse = ", "), "\n")
if (length(missing_surveys) > 0) {
  cat("Surveys pending: ", paste(missing_surveys, collapse = ", "), "\n")
}
cat("\n")
