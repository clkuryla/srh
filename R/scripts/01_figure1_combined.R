# ==============================================================================
# 01_figure1_combined.R
# Generate Figure 1: Combined Panel A (Mean SRH) over Panel B (Age Coefficient)
#
# Layout: 2 rows x 6 columns
#   - Row 1: Weighted mean SRH by age group over time (6 surveys)
#   - Row 2: Age coefficient on SRH over time (6 surveys)
#   - Columns: BRFSS, MEPS, NHIS, GSS, CPS, NHANES
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

cat("Functions loaded.\n")

# ------------------------------------------------------------------------------
# Load Pre-computed Estimates
# ------------------------------------------------------------------------------

# Survey order for the figure
survey_order <- c("BRFSS", "MEPS", "NHIS", "GSS", "CPS", "NHANES")

# --- Panel A estimates (mean SRH by age group and year) ---
estimates_list <- lapply(survey_order, function(svy) {
  path <- here::here("output", "tables",
                     paste0("fig1a_estimates_", tolower(svy), "_20260118.rds"))
  if (file.exists(path)) {
    readr::read_rds(path)
  } else {
    stop("Missing Panel A estimates for ", svy, ": ", path)
  }
})
names(estimates_list) <- survey_order
cat("Loaded Panel A estimates.\n")

# --- Panel B coefficients (age coefficient by year) ---
coefficients_list <- lapply(survey_order, function(svy) {
  path <- here::here("output", "tables",
                     paste0("fig1b_coefficients_", tolower(svy), "_20260118.rds"))
  if (file.exists(path)) {
    readr::read_rds(path)
  } else {
    stop("Missing Panel B coefficients for ", svy, ": ", path)
  }
})
names(coefficients_list) <- survey_order
cat("Loaded Panel B coefficients.\n")

# ------------------------------------------------------------------------------
# Compute Metaregression Results
# ------------------------------------------------------------------------------

meta_results_list <- lapply(survey_order, function(svy) {
  run_metaregression(coefficients_list[[svy]], svy)
})
names(meta_results_list) <- survey_order
cat("Computed metaregression results.\n")

# ------------------------------------------------------------------------------
# Generate Combined Figure
# ------------------------------------------------------------------------------

fig1_combined <- plot_fig1_combined(
  estimates_list = estimates_list,
  coefficients_list = coefficients_list,
  meta_results_list = meta_results_list,
  colors = age_colors,
  title = "Self Rated Health and Age Over Time",
  base_size = 14
)

cat("Figure generated.\n")

# ------------------------------------------------------------------------------
# Save Outputs
# ------------------------------------------------------------------------------

fig_dir <- here::here("output", "figures")
draft_date <- format(Sys.Date(), "%Y%m%d")

# Draft version (with date)
ggsave(
  filename = file.path(fig_dir, paste0("fig1_combined_draft_", draft_date, ".png")),
  plot = fig1_combined,
  width = 16,
  height = 8,
  dpi = 300
)
cat("Saved: fig1_combined_draft_", draft_date, ".png\n", sep = "")

# Final versions (no date)
ggsave(
  filename = file.path(fig_dir, "fig1_combined.png"),
  plot = fig1_combined,
  width = 16,
  height = 8,
  dpi = 300
)
cat("Saved: fig1_combined.png\n")

ggsave(
  filename = file.path(fig_dir, "fig1_combined.pdf"),
  plot = fig1_combined,
  width = 16,
  height = 8
)
cat("Saved: fig1_combined.pdf\n")

cat("\nDone. Figures saved to: ", fig_dir, "\n")
