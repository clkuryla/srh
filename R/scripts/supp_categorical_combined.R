# ==============================================================================
# supp_categorical_combined.R
# Generate combined multi-survey supplemental categorical SRH figures
#
# Figures generated:
#   - C1a: Variance over time (1×6 layout)
#   - C1b: Entropy over time (1×6 layout)
#   - C2a: SRH category distribution by age (7×6 layout)
#   - C2b: Stacked area by age (7×6 layout)
#   - C2c: SRH category prevalence by age (5×6 layout)
#   - C3:  Age composition by SRH category (5×6 layout)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(here)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)

# Source functions
source(here::here("R", "functions", "theme_srh.R"))
source(here::here("R", "functions", "plot_categorical_combined.R"))

# ------------------------------------------------------------------------------
# Load combined tables
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Loading combined categorical tables\n")
cat("========================================\n")

props_all <- readRDS(here::here("output", "tables", "supp_srh_props_all.rds"))
age_comp_all <- readRDS(here::here("output", "tables", "supp_srh_age_comp_all.rds"))
spread_all <- readRDS(here::here("output", "tables", "supp_srh_spread_all.rds"))

cat(sprintf("Props: %d rows\n", nrow(props_all)))
cat(sprintf("Age comp: %d rows\n", nrow(age_comp_all)))
cat(sprintf("Spread: %d rows\n", nrow(spread_all)))

# Output directory
fig_dir <- here::here("output", "figures")

# Date suffix for draft versions
date_suffix <- format(Sys.Date(), "%Y%m%d")

# ------------------------------------------------------------------------------
# C1a: Variance over time (1×6)
# ------------------------------------------------------------------------------

cat("\n=== Generating C1a: Variance combined ===\n")

c1a <- plot_spread_combined(
  data = spread_all,
  metric = "variance",
  base_size = 11,
  line_width = 1.0,
  point_size = 1.5,
  show_points = TRUE,
  tilt_x_labels = 45,
  title = "SRH Distribution Variance Over Time by Age Group"
)

# Save draft and final versions
ggsave(
  file.path(fig_dir, sprintf("supp_c1a_variance_combined_%s.png", date_suffix)),
  c1a, width = 16, height = 5, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c1a_variance_combined.png"),
  c1a, width = 16, height = 5, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c1a_variance_combined.pdf"),
  c1a, width = 16, height = 5
)

cat("Saved: supp_c1a_variance_combined\n")

# ------------------------------------------------------------------------------
# C1b: Entropy over time (1×6)
# ------------------------------------------------------------------------------

cat("\n=== Generating C1b: Entropy combined ===\n")

c1b <- plot_spread_combined(
  data = spread_all,
  metric = "entropy",
  base_size = 11,
  line_width = 1.0,
  point_size = 1.5,
  show_points = TRUE,
  tilt_x_labels = 45,
  title = "SRH Distribution Entropy Over Time by Age Group"
)

ggsave(
  file.path(fig_dir, sprintf("supp_c1b_entropy_combined_%s.png", date_suffix)),
  c1b, width = 16, height = 5, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c1b_entropy_combined.png"),
  c1b, width = 16, height = 5, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c1b_entropy_combined.pdf"),
  c1b, width = 16, height = 5
)

cat("Saved: supp_c1b_entropy_combined\n")

# ------------------------------------------------------------------------------
# C2a: SRH category distribution by age (7×6)
# ------------------------------------------------------------------------------

cat("\n=== Generating C2a: Props by age combined ===\n")

c2a <- plot_props_by_age_combined(
  data = props_all,
  base_size = 9,
  line_width = 0.8,
  point_size = 1.0,
  show_points = FALSE,
  tilt_x_labels = 45,
  title = "SRH Category Distribution by Age Group Over Time"
)

ggsave(
  file.path(fig_dir, sprintf("supp_c2a_by_age_combined_%s.png", date_suffix)),
  c2a, width = 18, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c2a_by_age_combined.png"),
  c2a, width = 18, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c2a_by_age_combined.pdf"),
  c2a, width = 18, height = 14
)

cat("Saved: supp_c2a_by_age_combined\n")

# ------------------------------------------------------------------------------
# C2b: Stacked area by age (7×6)
# ------------------------------------------------------------------------------

cat("\n=== Generating C2b: Stacked area combined ===\n")

c2b <- plot_stacked_combined(
  data = props_all,
  base_size = 9,
  area_alpha = 0.85,
  tilt_x_labels = 45,
  title = "SRH Distribution Composition by Age Group Over Time"
)

ggsave(
  file.path(fig_dir, sprintf("supp_c2b_stacked_combined_%s.png", date_suffix)),
  c2b, width = 18, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c2b_stacked_combined.png"),
  c2b, width = 18, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c2b_stacked_combined.pdf"),
  c2b, width = 18, height = 14
)

cat("Saved: supp_c2b_stacked_combined\n")

# ------------------------------------------------------------------------------
# C2c: SRH category prevalence by age (5×6)
# ------------------------------------------------------------------------------

cat("\n=== Generating C2c: Category prevalence combined ===\n")

c2c <- plot_cat_prevalence_combined(
  data = props_all,
  base_size = 9,
  line_width = 0.8,
  point_size = 1.0,
  show_points = FALSE,
  tilt_x_labels = 45,
  title = "SRH Category Prevalence by Age Group Over Time"
)

ggsave(
  file.path(fig_dir, sprintf("supp_c2c_cat_prevalence_combined_%s.png", date_suffix)),
  c2c, width = 14, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c2c_cat_prevalence_combined.png"),
  c2c, width = 14, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c2c_cat_prevalence_combined.pdf"),
  c2c, width = 14, height = 14
)

cat("Saved: supp_c2c_cat_prevalence_combined\n")

# ------------------------------------------------------------------------------
# C3: Age composition by SRH category (5×6)
# ------------------------------------------------------------------------------

cat("\n=== Generating C3: Age composition combined ===\n")

c3 <- plot_age_comp_combined(
  data = age_comp_all,
  base_size = 9,
  line_width = 0.8,
  point_size = 1.0,
  show_points = FALSE,
  tilt_x_labels = 45,
  title = "Age Composition by SRH Category Over Time"
)

ggsave(
  file.path(fig_dir, sprintf("supp_c3_age_comp_combined_%s.png", date_suffix)),
  c3, width = 14, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c3_age_comp_combined.png"),
  c3, width = 14, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_c3_age_comp_combined.pdf"),
  c3, width = 14, height = 14
)

cat("Saved: supp_c3_age_comp_combined\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("All combined categorical figures generated!\n")
cat("========================================\n")

cat("\nFiles saved to:", fig_dir, "\n")
cat("\nFigures:\n")
cat("  - supp_c1a_variance_combined.{png,pdf}   (16x5)\n")
cat("  - supp_c1b_entropy_combined.{png,pdf}    (16x5)\n")
cat("  - supp_c2a_by_age_combined.{png,pdf}     (18x14)\n")
cat("  - supp_c2b_stacked_combined.{png,pdf}    (18x14)\n")
cat("  - supp_c2c_cat_prevalence_combined.{png,pdf} (14x14)\n")
cat("  - supp_c3_age_comp_combined.{png,pdf}    (14x14)\n")

cat("\n========================================\n")
