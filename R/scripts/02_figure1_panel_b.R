# ==============================================================================
# 02_figure1_panel_b.R
#
# Figure 1 Panel B: Age Coefficient on SRH Over Time
#
# Purpose:
#   Generate publication-ready 2×3 multi-panel figure showing how the age
#   coefficient (from SRH ~ age regression) trends toward zero over time,
#   demonstrating the convergence phenomenon from a regression perspective.
#
# Model: SRH ~ age (simple linear regression, survey-weighted)
#
# Inputs:
#   - Wrangled survey data from data depot (_derived/srh_project/)
#
# Outputs:
#   - output/figures/fig1b_panel_b.png (300 dpi)
#   - output/figures/fig1b_panel_b.pdf
#   - output/tables/fig1b_coefficients_*.csv (one per survey)
#   - output/tables/fig1b_coefficients_*.rds (one per survey)
#   - output/tables/fig1b_coefficients_all_surveys_*.csv (combined)
#   - output/tables/fig1b_metaregression_*.csv (all surveys + pooled)
#
# Author: Christine Lucille Kuryla
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(tidyverse)
library(here)
library(survey)
library(srvyr)
library(patchwork)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/srh_common_functions.R"))
source(here::here("R/functions/theme_srh.R"))
source(here::here("R/functions/regress_srh_on_age.R"))
source(here::here("R/functions/plot_fig1_panel_b.R"))

cat("========================================\n")
cat("Figure 1 Panel B: Age Coefficient Over Time\n")
cat("========================================\n\n")


# ==============================================================================
# LOAD DATA
# ==============================================================================
# Load all six wrangled survey datasets.
# For regression, we need: srh, age (continuous), year, wt, psu, strata.
# ==============================================================================

cat("Loading data...\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) |>
  select(srh, age, year, psu, wt, strata) |>
  filter(wt > 0) |>
  drop_na(srh, age, year, wt)

data_meps <- readr::read_rds(derived_path("data_meps.rds")) |>
  select(srh, age, year, psu, wt, strata) |>
  filter(wt > 0) |>
  drop_na(srh, age, year, wt)

data_brfss <- readr::read_rds(derived_path("data_brfss.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt > 0) |>
  drop_na(srh, age, year, wt)

data_gss <- readr::read_rds(derived_path("data_gss.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt > 0) |>
  drop_na(srh, age, year, wt)

data_cps <- readr::read_rds(derived_path("data_cps.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt > 0) |>
  drop_na(srh, age, year, wt)

data_nhanes <- readr::read_rds(derived_path("data_nhanes.rds")) |>
  select(srh, age, year, psu, wt, strata) |>
  filter(wt > 0) |>
  drop_na(srh, age, year, wt)

cat("  NHIS:   ", format(nrow(data_nhis), big.mark = ","), " obs, years ",
    min(data_nhis$year), "-", max(data_nhis$year), "\n")
cat("  MEPS:   ", format(nrow(data_meps), big.mark = ","), " obs, years ",
    min(data_meps$year), "-", max(data_meps$year), "\n")
cat("  BRFSS:  ", format(nrow(data_brfss), big.mark = ","), " obs, years ",
    min(data_brfss$year), "-", max(data_brfss$year), "\n")
cat("  GSS:    ", format(nrow(data_gss), big.mark = ","), " obs, years ",
    min(data_gss$year), "-", max(data_gss$year), "\n")
cat("  CPS:    ", format(nrow(data_cps), big.mark = ","), " obs, years ",
    min(data_cps$year), "-", max(data_cps$year), "\n")
cat("  NHANES: ", format(nrow(data_nhanes), big.mark = ","), " obs, years ",
    min(data_nhanes$year), "-", max(data_nhanes$year), "\n\n")


# ==============================================================================
# RUN YEARLY REGRESSIONS
# ==============================================================================
# For each survey, run survey-weighted regression SRH ~ age for each year
# and extract the age coefficient.
# ==============================================================================

cat("Running yearly regressions (SRH ~ age)...\n\n")

cat("  NHIS...\n")
coefficients_nhis <- regress_age_coefficient_by_year(data_nhis, "NHIS")

cat("  MEPS...\n")
coefficients_meps <- regress_age_coefficient_by_year(data_meps, "MEPS")

cat("  BRFSS...\n")
coefficients_brfss <- regress_age_coefficient_by_year(data_brfss, "BRFSS")

cat("  GSS...\n")
coefficients_gss <- regress_age_coefficient_by_year(data_gss, "GSS")

cat("  CPS...\n")
coefficients_cps <- regress_age_coefficient_by_year(data_cps, "CPS")

cat("  NHANES...\n")
coefficients_nhanes <- regress_age_coefficient_by_year(data_nhanes, "NHANES")

cat("\nAll regressions complete.\n\n")


# ==============================================================================
# RUN METAREGRESSIONS
# ==============================================================================
# Regress coefficients on year (weighted by inverse variance) to quantify
# the trend in the age-SRH relationship over time.
# ==============================================================================

cat("Running metaregressions (coefficient ~ year)...\n")

meta_nhis <- run_metaregression(coefficients_nhis, "NHIS")
meta_meps <- run_metaregression(coefficients_meps, "MEPS")
meta_brfss <- run_metaregression(coefficients_brfss, "BRFSS")
meta_gss <- run_metaregression(coefficients_gss, "GSS")
meta_cps <- run_metaregression(coefficients_cps, "CPS")
meta_nhanes <- run_metaregression(coefficients_nhanes, "NHANES")

# Combine per-survey results
meta_results_all <- bind_rows(
  meta_nhis, meta_meps, meta_brfss, meta_gss, meta_cps, meta_nhanes
)

# Run pooled metaregression across all surveys
coefficients_list <- list(
  "BRFSS"  = coefficients_brfss,
  "MEPS"   = coefficients_meps,
  "NHIS"   = coefficients_nhis,
  "GSS"    = coefficients_gss,
  "CPS"    = coefficients_cps,
  "NHANES" = coefficients_nhanes
)

meta_pooled <- run_pooled_metaregression(coefficients_list)

cat("Done.\n\n")

# Print pooled results
cat("=== Pooled Metaregression (All Surveys) ===\n")
cat("Slope: ", round(meta_pooled$slope, 6), " (SE: ", round(meta_pooled$slope_se, 6), ")\n")
cat("P-value: ", format.pval(meta_pooled$slope_p_value, digits = 3), "\n")
cat("R-squared: ", round(meta_pooled$r_squared, 4), "\n")
cat("Interpretation: ", meta_pooled$interpretation, "\n\n")


# ==============================================================================
# ASSEMBLE LISTS FOR PLOTTING
# ==============================================================================
# Order determines position in figure: left-to-right, top-to-bottom.
# Top row:    BRFSS, MEPS, NHIS
# Bottom row: GSS, CPS, NHANES
# ==============================================================================

coefficients_list_ordered <- list(
  "BRFSS"  = coefficients_brfss,
  "MEPS"   = coefficients_meps,
  "NHIS"   = coefficients_nhis,
  "GSS"    = coefficients_gss,
  "CPS"    = coefficients_cps,
  "NHANES" = coefficients_nhanes
)

meta_results_list_ordered <- list(
  "BRFSS"  = meta_brfss,
  "MEPS"   = meta_meps,
  "NHIS"   = meta_nhis,
  "GSS"    = meta_gss,
  "CPS"    = meta_cps,
  "NHANES" = meta_nhanes
)


# ==============================================================================
# GENERATE FIGURE
# ==============================================================================

cat("Generating combined 2x3 figure...\n")

fig1b <- plot_fig1_panel_b(
  coefficients_list = coefficients_list_ordered,
  meta_results_list = meta_results_list_ordered,
  show_metareg = TRUE,
  ncol = 3,
  y_label = "Age coefficient",
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
  filename = file.path(fig_dir, "fig1b_panel_b.png"),
  plot = fig1b,
  width = 9,
  height = 6,
  dpi = 300
)
cat("  Saved: fig1b_panel_b.png\n")

# Final PDF (publication)
ggsave(
  filename = file.path(fig_dir, "fig1b_panel_b.pdf"),
  plot = fig1b,
  width = 9,
  height = 6
)
cat("  Saved: fig1b_panel_b.pdf\n")

# Draft with date (for version tracking)
draft_date <- format(Sys.Date(), "%Y%m%d")
ggsave(
  filename = file.path(fig_dir, paste0("fig1b_draft_", draft_date, ".png")),
  plot = fig1b,
  width = 9,
  height = 6,
  dpi = 300
)
cat("  Saved: fig1b_draft_", draft_date, ".png\n\n")


# ==============================================================================
# SAVE TABLES
# ==============================================================================

cat("Saving coefficient and metaregression tables...\n")

# Individual survey coefficient tables (CSV + RDS)
save_all_coefficients_tables(
  coefficients_list = coefficients_list_ordered,
  output_dir = here::here("output", "tables"),
  date_suffix = TRUE
)

# Combined coefficient table with all surveys
combined_coefficients <- bind_rows(coefficients_list_ordered, .id = "survey")

combined_coef_path <- here::here("output", "tables",
                                  paste0("fig1b_coefficients_all_surveys_", draft_date, ".csv"))

readr::write_csv(
  combined_coefficients |> mutate(across(where(is.numeric), ~ round(.x, 6))),
  combined_coef_path
)
cat("Saved: ", basename(combined_coef_path), "\n")

# Metaregression results table (per-survey + pooled)
meta_all_with_pooled <- bind_rows(meta_results_all, meta_pooled)

save_metaregression_table(
  meta_all_with_pooled,
  output_dir = here::here("output", "tables"),
  date_suffix = TRUE
)

cat("\n")


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("========================================\n")
cat("Figure 1 Panel B complete!\n")
cat("========================================\n")
cat("\nOutputs:\n")
cat("  Figures: output/figures/fig1b_panel_b.{png,pdf}\n")
cat("  Tables:  output/tables/fig1b_coefficients_*.{csv,rds}\n")
cat("           output/tables/fig1b_metaregression_*.{csv,rds}\n")
cat("\nKey finding:\n")
cat("  Pooled slope = ", round(meta_pooled$slope, 6),
    " (p ", format.pval(meta_pooled$slope_p_value, digits = 2), ")\n")
cat("  ", meta_pooled$interpretation, "\n")
cat("\n")
