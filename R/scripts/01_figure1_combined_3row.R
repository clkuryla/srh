# ==============================================================================
# 01_figure1_combined_3row.R
# Generate Figure 1: Combined Panel A, Panel B, and Panel C (3x6 layout)
#
# Layout: 3 rows x 6 columns
#   - Row 1: Weighted mean SRH by age group over time (Panel A)
#   - Row 2: Age coefficient on SRH over time (Panel B)
#   - Row 3: R² (variance explained by age) over time (Panel C)
#   - Columns: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/functions/theme_srh.R"))
source(here::here("R/functions/regress_srh_on_age.R"))
source(here::here("R/functions/plot_fig1_combined.R"))

cat("========================================\n")
cat("Figure 1 Combined (3 Rows): A + B + C\n")
cat("========================================\n\n")

# ------------------------------------------------------------------------------
# Load Pre-computed Estimates
# ------------------------------------------------------------------------------

# Survey order for the figure
survey_order <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")

# Date suffix for loading files (update as needed)
data_date <- "20260126"

cat("Loading pre-computed data...\n")

# --- Panel A estimates (mean SRH by age group and year) ---
estimates_list <- lapply(survey_order, function(svy) {
  path <- here::here("output", "tables",
                     paste0("fig1a_estimates_", tolower(svy), "_", data_date, ".rds"))
  if (file.exists(path)) {
    readr::read_rds(path)
  } else {
    stop("Missing Panel A estimates for ", svy, ": ", path)
  }
})
names(estimates_list) <- survey_order
cat("  Loaded Panel A estimates (mean SRH).\n")

# --- Panel B coefficients (age coefficient by year) ---
coefficients_list <- lapply(survey_order, function(svy) {
  path <- here::here("output", "tables",
                     paste0("fig1b_coefficients_", tolower(svy), "_", data_date, ".rds"))
  if (file.exists(path)) {
    readr::read_rds(path)
  } else {
    stop("Missing Panel B coefficients for ", svy, ": ", path)
  }
})
names(coefficients_list) <- survey_order
cat("  Loaded Panel B coefficients (age coef).\n")

# --- Panel C R² values (variance explained by age) ---
# Try to load from today's date first, then fall back to data_date
r2_date <- format(Sys.Date(), "%Y%m%d")
r2_list <- lapply(survey_order, function(svy) {
  # Try today's date first
  path <- here::here("output", "tables",
                     paste0("fig1c_r2_", tolower(svy), "_", r2_date, ".rds"))
  if (file.exists(path)) {
    return(readr::read_rds(path))
  }
  # Fall back to data_date
  path <- here::here("output", "tables",
                     paste0("fig1c_r2_", tolower(svy), "_", data_date, ".rds"))
  if (file.exists(path)) {
    return(readr::read_rds(path))
  }
  stop("Missing Panel C R² for ", svy, ". Run 01_figure1_panel_c.R first.")
})
names(r2_list) <- survey_order
cat("  Loaded Panel C R² values.\n\n")

# ------------------------------------------------------------------------------
# Compute Metaregression Results
# ------------------------------------------------------------------------------

cat("Computing metaregression results for Panel B...\n")
meta_results_list <- lapply(survey_order, function(svy) {
  run_metaregression(coefficients_list[[svy]], svy)
})
names(meta_results_list) <- survey_order
cat("  Done.\n\n")

# ------------------------------------------------------------------------------
# Generate Combined 3-Row Figure
# ------------------------------------------------------------------------------

cat("Generating combined 3x6 figure...\n")

fig1_3row <- plot_fig1_combined_3row(
  estimates_list = estimates_list,
  coefficients_list = coefficients_list,
  r2_list = r2_list,
  meta_results_list = meta_results_list,
  colors = age_colors,
  title = NULL,
  base_size = 18
)

cat("  Figure generated.\n\n")

# ------------------------------------------------------------------------------
# Save Outputs
# ------------------------------------------------------------------------------

fig_dir <- here::here("output", "figures")
draft_date <- format(Sys.Date(), "%Y%m%d")

cat("Saving figure...\n")

# Draft version (with date)
ggsave(
  filename = file.path(fig_dir, paste0("fig1_combined_3row_", draft_date, ".png")),
  plot = fig1_3row,
  width = 16,
  height = 10,
  dpi = 300
)
cat("  Saved: fig1_combined_3row_", draft_date, ".png\n", sep = "")

# Final versions (no date)
ggsave(
  filename = file.path(fig_dir, "fig1_combined_3row.png"),
  plot = fig1_3row,
  width = 16,
  height = 10,
  dpi = 300
)
cat("  Saved: fig1_combined_3row.png\n")

ggsave(
  filename = file.path(fig_dir, "fig1_combined_3row.pdf"),
  plot = fig1_3row,
  width = 16,
  height = 10
)
cat("  Saved: fig1_combined_3row.pdf\n\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

cat("========================================\n")
cat("Figure 1 (3-row) complete!\n")
cat("========================================\n")
cat("\nOutputs:\n")
cat("  output/figures/fig1_combined_3row.{png,pdf}\n")
cat("  output/figures/fig1_combined_3row_", draft_date, ".png\n", sep = "")
cat("\nLayout:\n")
cat("  Row A: Mean SRH by age group (convergence in levels)\n")
cat("  Row B: Age coefficient (convergence in association)\n")
cat("  Row C: R² (declining variance explained by age)\n")
cat("\n")
