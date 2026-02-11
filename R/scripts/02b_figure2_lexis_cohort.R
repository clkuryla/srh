# ==============================================================================
# 02b_figure2_lexis_cohort.R
#
# Figure 2 (Cohort Axis): Lexis Diagrams with Cohort (Birth Year) on X-Axis
#
# Purpose:
#   Generate Lexis surface plots showing mean SRH by age and cohort (birth year)
#   for all six surveys. This is the complement to the year-axis Lexis diagrams
#   in 02_figure2_lexis.R — same APC space, different projection.
#
# Key interpretation:
#   - Horizontal patterns = age effects (age matters)
#   - Vertical patterns = cohort effects (birth year matters)
#   - Diagonal patterns (slope = -1) = period effects (calendar year matters)
#
# Inputs:
#   - Wrangled survey data from data depot (_derived/srh_project/)
#
# Outputs:
#   - output/figures/fig2_lexis_cohort_combined.{png,pdf} (shared 0-1 scale)
#   - output/figures/fig2a_lexis_cohort_independent.{png,pdf} (independent scales)
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

cat("================================================\n")
cat("Figure 2 (Cohort Axis): Lexis Diagrams by Cohort\n")
cat("================================================\n\n")


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
# PREPARE COHORT-AXIS LEXIS DATA
# ==============================================================================
# Calculate weighted mean SRH by cohort (birth year) and age bin.
# Settings:
#   - 5-year age bins and 5-year cohort bins → square tiles
#   - Standardized age range (20-89) across all surveys
# ==============================================================================

cat("Preparing cohort-axis Lexis data...\n")

# Common settings
age_bin <- 5
cohort_bin <- 5
min_age <- 20
max_age <- 89
min_cell_n <- 30

# BRFSS age recoding (same as year-axis version)
data_brfss_recoded <- data_brfss |>
  mutate(age = if_else(age >= 80, 82, age))

cat("  NHIS...\n")
lexis_nhis <- prepare_lexis_data_cohort(
  data_nhis,
  age_binwidth = age_bin,
  cohort_binwidth = cohort_bin,
  min_age = min_age,
  max_age = max_age,
  min_n = min_cell_n,
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("  MEPS...\n")
lexis_meps <- prepare_lexis_data_cohort(
  data_meps,
  age_binwidth = age_bin,
  cohort_binwidth = cohort_bin,
  min_age = min_age,
  max_age = max_age,
  min_n = min_cell_n,
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("  BRFSS...\n")
lexis_brfss <- prepare_lexis_data_cohort(
  data_brfss_recoded,
  age_binwidth = age_bin,
  cohort_binwidth = cohort_bin,
  min_age = min_age,
  max_age = max_age,
  min_n = min_cell_n,
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("  GSS...\n")
lexis_gss <- prepare_lexis_data_cohort(
  data_gss,
  age_binwidth = age_bin,
  cohort_binwidth = cohort_bin,
  min_age = min_age,
  max_age = max_age,
  min_n = 20,
  rescale_01 = TRUE,
  srh_scale = "4"
)

cat("  CPS...\n")
lexis_cps <- prepare_lexis_data_cohort(
  data_cps,
  age_binwidth = age_bin,
  cohort_binwidth = cohort_bin,
  min_age = min_age,
  max_age = max_age,
  min_n = min_cell_n,
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("  NHANES...\n")
lexis_nhanes <- prepare_lexis_data_cohort(
  data_nhanes,
  age_binwidth = age_bin,
  cohort_binwidth = cohort_bin,
  min_age = min_age,
  max_age = max_age,
  min_n = 20,
  rescale_01 = TRUE,
  srh_scale = "5"
)

cat("Done.\n\n")

# Report cell counts
cat("Cohort-axis Lexis data cell counts:\n")
cat("  NHIS:   ", nrow(lexis_nhis), " cells\n")
cat("  MEPS:   ", nrow(lexis_meps), " cells\n")
cat("  BRFSS:  ", nrow(lexis_brfss), " cells\n")
cat("  GSS:    ", nrow(lexis_gss), " cells\n")
cat("  CPS:    ", nrow(lexis_cps), " cells\n")
cat("  NHANES: ", nrow(lexis_nhanes), " cells\n\n")


# ==============================================================================
# ASSEMBLE LEXIS LIST
# ==============================================================================

lexis_list <- list(
  "BRFSS"  = lexis_brfss,
  "MEPS"   = lexis_meps,
  "NHIS"   = lexis_nhis,
  "GSS"    = lexis_gss,
  "CPS"    = lexis_cps,
  "NHANES" = lexis_nhanes
)

# Global SRH range for shared color scale
all_mean_srh <- unlist(lapply(lexis_list, function(x) x$mean_srh))
global_min <- min(all_mean_srh, na.rm = TRUE)
global_max <- max(all_mean_srh, na.rm = TRUE)

cat("Global SRH range for color scale:\n")
cat("  Min: ", round(global_min, 3), "\n")
cat("  Max: ", round(global_max, 3), "\n\n")


# ==============================================================================
# GENERATE COMBINED FIGURE (SHARED SCALE)
# ==============================================================================

cat("Generating combined cohort-axis Lexis figure (shared scale)...\n")

fig2_cohort <- plot_lexis_combined(
  lexis_list,
  ncol = 6,
  x_axis = "cohort",
  show_cohort_lines = FALSE,
  color_scale = "turbo",
  reverse_colors = TRUE,
  shared_scale = TRUE,
  scale_limits = c(global_min, global_max),
  title = "Lexis Diagrams of Mean SRH (Cohort Axis)",
  subtitle = NULL,
  tilt_x_labels = 45,
  base_size = 12
)

cat("Done.\n\n")


# ==============================================================================
# SAVE SHARED-SCALE FIGURE
# ==============================================================================

fig_dir <- here::here("output", "figures")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

cat("Saving shared-scale cohort-axis figure...\n")

fig_width <- 16
fig_height <- 5.5

ggsave(
  filename = file.path(fig_dir, "fig2_lexis_cohort_combined.png"),
  plot = fig2_cohort,
  width = fig_width,
  height = fig_height,
  dpi = 300
)
cat("  Saved: fig2_lexis_cohort_combined.png\n")

ggsave(
  filename = file.path(fig_dir, "fig2_lexis_cohort_combined.pdf"),
  plot = fig2_cohort,
  width = fig_width,
  height = fig_height
)
cat("  Saved: fig2_lexis_cohort_combined.pdf\n")

draft_date <- format(Sys.Date(), "%Y%m%d")
ggsave(
  filename = file.path(fig_dir, paste0("fig2_lexis_cohort_draft_", draft_date, ".png")),
  plot = fig2_cohort,
  width = fig_width,
  height = fig_height,
  dpi = 300
)
cat("  Saved: fig2_lexis_cohort_draft_", draft_date, ".png\n\n")


# ==============================================================================
# GENERATE FIGURE 2A: INDEPENDENT SCALES (RAW VALUES)
# ==============================================================================

cat("Preparing raw (non-normalized) cohort-axis Lexis data...\n")

lexis_nhis_raw <- prepare_lexis_data_cohort(
  data_nhis, age_binwidth = age_bin, cohort_binwidth = cohort_bin,
  min_age = min_age, max_age = max_age, min_n = min_cell_n,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_meps_raw <- prepare_lexis_data_cohort(
  data_meps, age_binwidth = age_bin, cohort_binwidth = cohort_bin,
  min_age = min_age, max_age = max_age, min_n = min_cell_n,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_brfss_raw <- prepare_lexis_data_cohort(
  data_brfss_recoded, age_binwidth = age_bin, cohort_binwidth = cohort_bin,
  min_age = min_age, max_age = max_age, min_n = min_cell_n,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_gss_raw <- prepare_lexis_data_cohort(
  data_gss, age_binwidth = age_bin, cohort_binwidth = cohort_bin,
  min_age = min_age, max_age = max_age, min_n = 20,
  rescale_01 = FALSE, srh_scale = "4"
)

lexis_cps_raw <- prepare_lexis_data_cohort(
  data_cps, age_binwidth = age_bin, cohort_binwidth = cohort_bin,
  min_age = min_age, max_age = max_age, min_n = min_cell_n,
  rescale_01 = FALSE, srh_scale = "5"
)

lexis_nhanes_raw <- prepare_lexis_data_cohort(
  data_nhanes, age_binwidth = age_bin, cohort_binwidth = cohort_bin,
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

cat("Generating Figure 2a cohort-axis (independent scales)...\n")

fig2a_cohort <- plot_lexis_combined(
  lexis_list_raw,
  ncol = 6,
  x_axis = "cohort",
  show_cohort_lines = FALSE,
  color_scale = "turbo",
  reverse_colors = TRUE,
  shared_scale = FALSE,
  title = "Lexis Diagrams of Mean SRH (Cohort Axis)",
  subtitle = NULL,
  tilt_x_labels = 45,
  base_size = 12
)

cat("Done.\n\n")

# Save Figure 2a (cohort-axis, independent scales)
cat("Saving independent-scale cohort-axis figure...\n")

fig2a_height <- 6

ggsave(
  filename = file.path(fig_dir, "fig2a_lexis_cohort_independent.png"),
  plot = fig2a_cohort,
  width = fig_width,
  height = fig2a_height,
  dpi = 300
)
cat("  Saved: fig2a_lexis_cohort_independent.png\n")

ggsave(
  filename = file.path(fig_dir, "fig2a_lexis_cohort_independent.pdf"),
  plot = fig2a_cohort,
  width = fig_width,
  height = fig2a_height
)
cat("  Saved: fig2a_lexis_cohort_independent.pdf\n\n")


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("================================================\n")
cat("Figure 2 (Cohort Axis) complete!\n")
cat("================================================\n")
cat("\nOutputs:\n")
cat("  Shared scale:      output/figures/fig2_lexis_cohort_combined.{png,pdf}\n")
cat("  Independent scale: output/figures/fig2a_lexis_cohort_independent.{png,pdf}\n")
cat("\nInterpretation guide:\n")
cat("  - Horizontal patterns = age effects\n")
cat("  - Vertical patterns = cohort effects (birth year matters)\n")
cat("  - Diagonal patterns (slope = -1) = period effects (calendar year matters)\n")
cat("\n")
