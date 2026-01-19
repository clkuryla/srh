# ==============================================================================
# 02_figure2_lexis.R
#
# Figure 2: Lexis Diagrams for APC Analysis
#
# Purpose:
#   Generate Lexis surface plots showing mean SRH by age and year for all six
#   surveys. The goal is to demonstrate the ABSENCE of diagonal (cohort) patterns,
#   supporting the interpretation that SRH convergence is not driven by
#   generational differences.
#
# Key interpretation:
#   - Diagonal patterns = cohort effects (birth year matters)
#   - Vertical patterns = period effects (calendar year matters)
#   - Horizontal patterns = age effects (age matters)
#   - NO diagonal banding = weak/absent cohort effects
#
# Inputs:
#   - Wrangled survey data from data depot (_derived/srh_project/)
#
# Outputs:
#   - output/figures/fig2_lexis_combined.png (300 dpi)
#   - output/figures/fig2_lexis_combined.pdf
#   - output/figures/fig2_lexis_[survey].png (individual surveys)
#
# Author: Christine Lucille Kuryla
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/functions/theme_srh.R"))
source(here::here("R/functions/plot_lexis.R"))

cat("========================================\n")
cat("Figure 2: Lexis Diagrams (APC Analysis)\n")
cat("========================================\n\n")


# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data...\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_meps <- readr::read_rds(derived_path("data_meps.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_brfss <- readr::read_rds(derived_path("data_brfss.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_gss <- readr::read_rds(derived_path("data_gss.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_cps <- readr::read_rds(derived_path("data_cps.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_nhanes <- readr::read_rds(derived_path("data_nhanes.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

cat("  NHIS:   ", format(nrow(data_nhis), big.mark = ","), " obs\n")
cat("  MEPS:   ", format(nrow(data_meps), big.mark = ","), " obs\n")
cat("  BRFSS:  ", format(nrow(data_brfss), big.mark = ","), " obs\n")
cat("  GSS:    ", format(nrow(data_gss), big.mark = ","), " obs\n")
cat("  CPS:    ", format(nrow(data_cps), big.mark = ","), " obs\n")
cat("  NHANES: ", format(nrow(data_nhanes), big.mark = ","), " obs\n\n")


# ==============================================================================
# PREPARE LEXIS DATA
# ==============================================================================
# Calculate weighted mean SRH by year and age bin for each survey.
# Settings:
#   - 5-year age bins for smooth visualization
#   - Standardized age range (20-80) across all surveys
#   - Year binning for surveys with gaps (GSS biennial, NHANES 2-year cycles)
# ==============================================================================

cat("Preparing Lexis data...\n")

# Common settings across surveys
age_bin <- 5       # 5-year age bins
min_age <- 20      # Standardized minimum age
max_age <- 89      # Standardized maximum age
min_cell_n <- 30   # Minimum obs per cell

cat("  NHIS (2-year periods)...\n")
lexis_nhis <- prepare_lexis_data(
  data_nhis,
  age_binwidth = age_bin,
  year_binwidth = 2,  # Smooth out year-to-year variation
  min_age = min_age,
  max_age = max_age,
  min_n = min_cell_n,
  rescale_01 = TRUE,  # Normalize to 0-1 for shared scale
  srh_scale = "5"
)

cat("  MEPS...\n")
lexis_meps <- prepare_lexis_data(
  data_meps,
  age_binwidth = age_bin,
  year_binwidth = 1,
  min_age = min_age,
  max_age = max_age,
  min_n = min_cell_n,
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("  BRFSS (combining ages 80+ due to top-coding)...\n")
# BRFSS top-codes ages 70+ as 72, 77, or 89 only (no ages 80-88)
# Recode 89 to 82 so all 80+ fall into one age bin
data_brfss_recoded <- data_brfss |>
  mutate(age = if_else(age >= 80, 82, age))

lexis_brfss <- prepare_lexis_data(
  data_brfss_recoded,
  age_binwidth = age_bin,
  year_binwidth = 1,
  min_age = min_age,
  max_age = max_age,
  min_n = min_cell_n,
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("  GSS (2-year periods for biennial data)...\n")
lexis_gss <- prepare_lexis_data(
  data_gss,
  age_binwidth = age_bin,
  year_binwidth = 2,  # Biennial survey - group to reduce striping
  min_age = min_age,
  max_age = max_age,
  min_n = 20,  # Lower threshold for smaller survey
  rescale_01 = TRUE,
  srh_scale = "4"  # GSS uses 4-point scale
)

cat("  CPS...\n")
lexis_cps <- prepare_lexis_data(
  data_cps,
  age_binwidth = age_bin,
  year_binwidth = 1,
  min_age = min_age,
  max_age = max_age,
  min_n = min_cell_n,
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("  NHANES (2-year periods for survey cycles)...\n")
lexis_nhanes <- prepare_lexis_data(
  data_nhanes,
  age_binwidth = age_bin,
  year_binwidth = 2,  # 2-year survey cycles
  min_age = min_age,
  max_age = max_age,
  min_n = 20,  # Lower threshold for smaller survey
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("Done.\n\n")

# Report cell counts
cat("Lexis data cell counts:\n")
cat("  NHIS:   ", nrow(lexis_nhis), " cells\n")
cat("  MEPS:   ", nrow(lexis_meps), " cells\n")
cat("  BRFSS:  ", nrow(lexis_brfss), " cells\n")
cat("  GSS:    ", nrow(lexis_gss), " cells\n")
cat("  CPS:    ", nrow(lexis_cps), " cells\n")
cat("  NHANES: ", nrow(lexis_nhanes), " cells\n\n")


# ==============================================================================
# ASSEMBLE LEXIS LIST
# ==============================================================================
# Order determines position in figure: left-to-right, top-to-bottom.
# Top row:    BRFSS, MEPS, NHIS
# Bottom row: GSS, CPS, NHANES
# (Same order as Figure 1 for consistency)
# ==============================================================================

lexis_list <- list(
  "BRFSS"  = lexis_brfss,
  "MEPS"   = lexis_meps,
  "NHIS"   = lexis_nhis,
  "GSS"    = lexis_gss,
  "CPS"    = lexis_cps,
  "NHANES" = lexis_nhanes
)

# Calculate global min/max mean SRH across all surveys for shared color scale
all_mean_srh <- unlist(lapply(lexis_list, function(x) x$mean_srh))
global_min <- min(all_mean_srh, na.rm = TRUE)
global_max <- max(all_mean_srh, na.rm = TRUE)

cat("Global SRH range for color scale:\n")
cat("  Min: ", round(global_min, 3), "\n")
cat("  Max: ", round(global_max, 3), "\n\n")


# ==============================================================================
# GENERATE COMBINED FIGURE
# ==============================================================================

cat("Generating combined 2x3 Lexis figure...\n")

# Color scale options:
#   - "turbo" (default): Rainbow, high contrast, good for showing range compression
#   - "viridis": Yellow-green-blue, perceptually uniform
#   - "plasma": Purple-orange-yellow
#   - "magma": Black-purple-orange-yellow
#   - "inferno": Black-purple-red-yellow
#   - "diverging": Blue-white-red (set reverse_colors=FALSE for blue=low, red=high)
#
# reverse_colors = TRUE (default) means higher SRH (better health) = warm colors (red/yellow)

fig2 <- plot_lexis_combined(
  lexis_list,
  ncol = 6,                     # 1 row, same order as Figure 1
  show_cohort_lines = TRUE,
  cohort_line_spacing = 10,     # Years between cohort lines
  cohort_line_width = 1.0,      # Thick lines for visibility
  cohort_line_color = "gray30",
  cohort_line_alpha = 0.7,
  color_scale = "turbo",        # Rainbow scale highlights range compression
  reverse_colors = TRUE,        # Higher SRH = warmer colors
  shared_scale = TRUE,          # Single legend at bottom
  scale_limits = c(global_min, global_max),  # Actual data range
  title = "Lexis Diagrams of Mean SRH",
  subtitle = NULL,
  tilt_x_labels = 45,           # Match Figure 1 style
  base_size = 12                # Match Figure 1 font size
)

# To use independent scales per survey (original behavior), set:
# shared_scale = FALSE

cat("Done.\n\n")


# ==============================================================================
# SAVE FIGURE
# ==============================================================================

fig_dir <- here::here("output", "figures")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

cat("Saving figure...\n")

# Figure dimensions for 1x6 layout (wide, with room for legend at bottom)
fig_width <- 16
fig_height <- 5.5

# Final PNG (publication)
ggsave(
  filename = file.path(fig_dir, "fig2_lexis_combined.png"),
  plot = fig2,
  width = fig_width,
  height = fig_height,
  dpi = 300
)
cat("  Saved: fig2_lexis_combined.png\n")

# Final PDF (publication)
ggsave(
  filename = file.path(fig_dir, "fig2_lexis_combined.pdf"),
  plot = fig2,
  width = fig_width,
  height = fig_height
)
cat("  Saved: fig2_lexis_combined.pdf\n")

# Draft with date (for version tracking)
draft_date <- format(Sys.Date(), "%Y%m%d")
ggsave(
  filename = file.path(fig_dir, paste0("fig2_lexis_draft_", draft_date, ".png")),
  plot = fig2,
  width = fig_width,
  height = fig_height,
  dpi = 300
)
cat("  Saved: fig2_lexis_draft_", draft_date, ".png\n\n")


# ==============================================================================
# GENERATE FIGURE 2A: INDEPENDENT SCALES (RAW VALUES)
# ==============================================================================
# Each survey has its own color scale with raw (non-normalized) SRH values

cat("Preparing raw (non-normalized) Lexis data for Figure 2a...\n")

# Prepare data WITHOUT normalization for independent scales
lexis_nhis_raw <- prepare_lexis_data(
  data_nhis, age_binwidth = age_bin, year_binwidth = 2,
  min_age = min_age, max_age = max_age, min_n = min_cell_n,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_meps_raw <- prepare_lexis_data(
  data_meps, age_binwidth = age_bin, year_binwidth = 1,
  min_age = min_age, max_age = max_age, min_n = min_cell_n,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_brfss_raw <- prepare_lexis_data(
  data_brfss_recoded, age_binwidth = age_bin, year_binwidth = 1,
  min_age = min_age, max_age = max_age, min_n = min_cell_n,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_gss_raw <- prepare_lexis_data(
  data_gss, age_binwidth = age_bin, year_binwidth = 2,
  min_age = min_age, max_age = max_age, min_n = 20,
  rescale_01 = FALSE, srh_scale = "4"
)

lexis_cps_raw <- prepare_lexis_data(
  data_cps, age_binwidth = age_bin, year_binwidth = 1,
  min_age = min_age, max_age = max_age, min_n = min_cell_n,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_nhanes_raw <- prepare_lexis_data(
  data_nhanes, age_binwidth = age_bin, year_binwidth = 2,
  min_age = min_age, max_age = max_age, min_n = 20,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_list_raw <- list(
  "BRFSS"  = lexis_brfss_raw,
  "MEPS"   = lexis_meps_raw,
  "NHIS"   = lexis_nhis_raw,
  "GSS"    = lexis_gss_raw,
  "CPS"    = lexis_cps_raw,
  "NHANES" = lexis_nhanes_raw
)

cat("Generating Figure 2a (independent scales per survey)...\n")

fig2a <- plot_lexis_combined(
  lexis_list_raw,
  ncol = 6,
  show_cohort_lines = TRUE,
  cohort_line_spacing = 10,
  cohort_line_width = 1.0,
  cohort_line_color = "gray30",
  cohort_line_alpha = 0.7,
  color_scale = "turbo",
  reverse_colors = TRUE,
  shared_scale = FALSE,           # Independent scales per survey
  title = "Lexis Diagrams of Mean SRH",
  subtitle = NULL,
  tilt_x_labels = 45,
  base_size = 12
)

cat("Done.\n\n")

# Save Figure 2a
cat("Saving Figure 2a...\n")

fig2a_height <- 6  # Taller to accommodate individual legends

ggsave(
  filename = file.path(fig_dir, "fig2a_lexis_independent.png"),
  plot = fig2a,
  width = fig_width,
  height = fig2a_height,
  dpi = 300
)
cat("  Saved: fig2a_lexis_independent.png\n")

ggsave(
  filename = file.path(fig_dir, "fig2a_lexis_independent.pdf"),
  plot = fig2a,
  width = fig_width,
  height = fig2a_height
)
cat("  Saved: fig2a_lexis_independent.pdf\n\n")


# ==============================================================================
# OPTIONAL: INDIVIDUAL SURVEY PLOTS
# ==============================================================================
# Uncomment to generate individual survey Lexis diagrams

# cat("Generating individual survey plots...\n")
#
# for (svy in names(lexis_list)) {
#   p <- plot_lexis_surface(
#     lexis_list[[svy]],
#     title = paste(svy, ": Self-Rated Health by Age and Year"),
#     show_cohort_lines = TRUE
#   ) |>
#     add_lexis_guide()
#
#   ggsave(
#     filename = file.path(fig_dir, paste0("fig2_lexis_", tolower(svy), ".png")),
#     plot = p,
#     width = 8,
#     height = 6,
#     dpi = 300
#   )
#   cat("  Saved: fig2_lexis_", tolower(svy), ".png\n")
# }


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("========================================\n")
cat("Figure 2 complete!\n")
cat("========================================\n")
cat("\nOutputs:\n")
cat("  Figures: output/figures/fig2_lexis_combined.{png,pdf}\n")
cat("\nInterpretation guide:\n")
cat("  - Diagonal patterns would indicate cohort (birth year) effects\n")
cat("  - Vertical patterns indicate period (calendar year) effects\n")
cat("  - Horizontal patterns indicate age effects\n")
cat("  - ABSENCE of diagonal banding supports weak/absent cohort effects\n")
cat("\n")
