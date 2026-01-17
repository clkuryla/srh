# ==============================================================================
# 01_figure1_panel_a.R
#
# Figure 1 Panel A: Weighted Mean SRH by Age Group Over Time
#
# Purpose:
#   Generate publication-ready 2×3 multi-panel figure showing the convergence
#   phenomenon across all six surveys (NHIS, MEPS, BRFSS, GSS, CPS, NHANES).
#
# Inputs:
#   - Wrangled survey data from data depot (_derived/srh_project/)
#
# Outputs:
#   - output/figures/fig1a_panel_a.png (300 dpi)
#   - output/figures/fig1a_panel_a.pdf
#   - output/tables/fig1a_estimates_*.csv (one per survey)
#   - output/tables/fig1a_estimates_*.rds (one per survey)
#   - output/tables/fig1a_estimates_all_surveys_*.csv (combined)
#
# Author: Christine Lucille Kuryla
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(tidyverse)
library(here)
library(srvyr)
library(patchwork)
library(cowplot)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/srh_common_functions.R"))
source(here::here("R/functions/theme_srh.R"))
source(here::here("R/functions/srh_and_age_over_time.R"))
source(here::here("R/functions/plot_fig1_panel_a.R"))

cat("========================================\n")
cat("Figure 1 Panel A: Mean SRH Over Time\n")
cat("========================================\n\n")


# ==============================================================================
# LOAD DATA
# ==============================================================================
# Load all six wrangled survey datasets.
# Each is filtered to valid weights and has age_group added using scheme "B".
# ==============================================================================

cat("Loading data...\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) |>
  select(srh, age, year, psu, wt, strata) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt) |>
  add_age_group(scheme = "B", new_var = age_group)

data_meps <- readr::read_rds(derived_path("data_meps.rds")) |>
  select(srh, age, year, psu, wt, strata) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt) |>
  add_age_group(scheme = "B", new_var = age_group)

data_brfss <- readr::read_rds(derived_path("data_brfss.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt) |>
  add_age_group(scheme = "B", new_var = age_group)

data_gss <- readr::read_rds(derived_path("data_gss.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt) |>
  add_age_group(scheme = "B", new_var = age_group)

data_cps <- readr::read_rds(derived_path("data_cps.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt) |>
  add_age_group(scheme = "B", new_var = age_group)

data_nhanes <- readr::read_rds(derived_path("data_nhanes.rds")) |>
  select(srh, age, year, psu, wt, strata) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt) |>
  add_age_group(scheme = "B", new_var = age_group)

cat("  NHIS:   ", format(nrow(data_nhis), big.mark = ","), " obs\n")
cat("  MEPS:   ", format(nrow(data_meps), big.mark = ","), " obs\n")
cat("  BRFSS:  ", format(nrow(data_brfss), big.mark = ","), " obs\n")
cat("  GSS:    ", format(nrow(data_gss), big.mark = ","), " obs\n")
cat("  CPS:    ", format(nrow(data_cps), big.mark = ","), " obs\n")
cat("  NHANES: ", format(nrow(data_nhanes), big.mark = ","), " obs\n\n")


# ==============================================================================
# COMPUTE ESTIMATES
# ==============================================================================
# Compute weighted mean SRH by age group and year for each survey.
# BRFSS uses chunked processing due to its large size.
# ==============================================================================

cat("Computing survey-weighted estimates...\n")

cat("  NHIS...\n")
srh_means_nhis <- summarize_srh_over_time(data_nhis, survey_name = "NHIS",
                                           title_style = "dataset")

cat("  MEPS...\n")
srh_means_meps <- summarize_srh_over_time(data_meps, survey_name = "MEPS",
                                           title_style = "dataset")

cat("  BRFSS (chunked)...\n")
srh_means_brfss <- summarize_srh_over_time_chunked(data_brfss,
                                                    survey_name = "BRFSS",
                                                    title_style = "dataset",
                                                    cache_dir = derived_path("cache"))

cat("  GSS...\n")
srh_means_gss <- summarize_srh_over_time(data_gss, survey_name = "GSS",
                                          title_style = "dataset")

cat("  CPS...\n")
srh_means_cps <- summarize_srh_over_time(data_cps, survey_name = "CPS",
                                          title_style = "dataset")

cat("  NHANES...\n")
srh_means_nhanes <- summarize_srh_over_time(data_nhanes, survey_name = "NHANES",
                                             title_style = "dataset")

cat("Done.\n\n")


# ==============================================================================
# ASSEMBLE ESTIMATES LIST
# ==============================================================================
# Order determines position in figure: left-to-right, top-to-bottom.
# Top row:    BRFSS, MEPS, NHIS
# Bottom row: GSS, CPS, NHANES
# ==============================================================================

estimates_list <- list(
  "BRFSS"  = srh_means_brfss$estimates,
  "MEPS"   = srh_means_meps$estimates,
  "NHIS"   = srh_means_nhis$estimates,
  "GSS"    = srh_means_gss$estimates,
  "CPS"    = srh_means_cps$estimates,
  "NHANES" = srh_means_nhanes$estimates
)


# ==============================================================================
# GENERATE FIGURE
# ==============================================================================

cat("Generating combined 2x3 figure...\n")

fig1a <- plot_fig1_panel_a(
  estimates_list = estimates_list,
  colors = age_colors,
  show_ci = FALSE,
  ncol = 3,
  y_label = "Weighted mean SRH",
  x_label = "Year"
)

cat("Done.\n\n")


# ==============================================================================
# SAVE FIGURE
# ==============================================================================

fig_dir <- here::here("output", "figures")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

cat("Saving figure...\n")

# Final PNG (publication)
ggsave(
  filename = file.path(fig_dir, "fig1a_panel_a.png"),
  plot = fig1a,
  width = 9,
  height = 6,
  dpi = 300
)
cat("  Saved: fig1a_panel_a.png\n")

# Final PDF (publication)
ggsave(
  filename = file.path(fig_dir, "fig1a_panel_a.pdf"),
  plot = fig1a,
  width = 9,
  height = 6
)
cat("  Saved: fig1a_panel_a.pdf\n")

# Draft with date (for version tracking)
draft_date <- format(Sys.Date(), "%Y%m%d")
ggsave(
  filename = file.path(fig_dir, paste0("fig1a_draft_", draft_date, ".png")),
  plot = fig1a,
  width = 9,
  height = 6,
  dpi = 300
)
cat("  Saved: fig1a_draft_", draft_date, ".png\n\n")


# ==============================================================================
# SAVE TABLES
# ==============================================================================

cat("Saving estimates tables...\n")

# Individual survey tables (CSV + RDS)
save_all_estimates_tables(
  estimates_list = estimates_list,
  output_dir = here::here("output", "tables"),
  date_suffix = TRUE
)

# Combined table with all surveys
combined_estimates <- bind_rows(estimates_list, .id = "survey")

combined_path <- here::here("output", "tables",
                            paste0("fig1a_estimates_all_surveys_", draft_date, ".csv"))

readr::write_csv(
  combined_estimates |> mutate(across(where(is.numeric), ~ round(.x, 4))),
  combined_path
)
cat("Saved: ", basename(combined_path), "\n\n")


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("========================================\n")
cat("Figure 1 Panel A complete!\n")
cat("========================================\n")
cat("\nOutputs:\n")
cat("  Figures: output/figures/fig1a_panel_a.{png,pdf}\n")
cat("  Tables:  output/tables/fig1a_estimates_*.{csv,rds}\n")
cat("\n")
