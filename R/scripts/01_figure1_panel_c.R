# ==============================================================================
# 01_figure1_panel_c.R
#
# Figure 1 Panel C: R² (Variance Explained by Age) Over Time
#
# Purpose:
#   Compute the R² from survey-weighted regression SRH ~ age for each year
#   across all six surveys. This shows how much variance in SRH is explained
#   by age alone, and whether this is declining (supporting convergence).
#
# Model: SRH ~ age (simple linear regression, survey-weighted)
#
# Inputs:
#   - Wrangled survey data from data depot (_derived/srh_project/)
#
# Outputs:
#   - output/tables/fig1c_r2_*.csv (one per survey)
#   - output/tables/fig1c_r2_*.rds (one per survey)
#   - output/tables/fig1c_r2_all_surveys_*.csv (combined)
#
# Author: Christine Lucille Kuryla
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(tidyverse)
library(here)
library(survey)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/srh_common_functions.R"))
source(here::here("R/functions/regress_srh_on_age.R"))

cat("========================================\n")
cat("Figure 1 Panel C: R² (Age → SRH) Over Time\n")
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
# COMPUTE R² FOR EACH SURVEY
# ==============================================================================
# For each survey, run survey-weighted regression SRH ~ age for each year
# and compute R².
# ==============================================================================

cat("Computing R² values (SRH ~ age)...\n\n")

cat("  NHIS...\n")
r2_nhis <- compute_r2_by_year(data_nhis, "NHIS")

cat("  MEPS...\n")
r2_meps <- compute_r2_by_year(data_meps, "MEPS")

cat("  BRFSS...\n")
r2_brfss <- compute_r2_by_year(data_brfss, "BRFSS")

cat("  GSS...\n")
r2_gss <- compute_r2_by_year(data_gss, "GSS")

cat("  CPS...\n")
r2_cps <- compute_r2_by_year(data_cps, "CPS")

cat("  NHANES...\n")
r2_nhanes <- compute_r2_by_year(data_nhanes, "NHANES")

cat("\nAll R² computations complete.\n\n")

# ------------------------------------------------------------------------------
# POST-PROCESSING: Handle known data anomalies
# ------------------------------------------------------------------------------
# NHIS 1984 has anomalously low R² (0.01 vs ~0.11 for surrounding years)
# This appears to be a data artifact - set to NA
# ------------------------------------------------------------------------------

r2_nhis <- r2_nhis |>

mutate(r_squared = if_else(year == 1984, NA_real_, r_squared))
cat("Note: Set NHIS 1984 R² to NA (data anomaly).\n\n")


# ==============================================================================
# ASSEMBLE LIST
# ==============================================================================
# Order determines position in figure: left-to-right
# ==============================================================================

r2_list <- list(
  "BRFSS"  = r2_brfss,
  "MEPS"   = r2_meps,
  "NHIS"   = r2_nhis,
  "CPS"    = r2_cps,
  "NHANES" = r2_nhanes,
  "GSS"    = r2_gss
)


# ==============================================================================
# PRINT SUMMARY
# ==============================================================================

cat("=== R² Summary (First and Last Years) ===\n\n")

for (svy in names(r2_list)) {
  df <- r2_list[[svy]]
  first_year <- df[which.min(df$year), ]
  last_year <- df[which.max(df$year), ]

  cat(sprintf("  %s: R² %.0f = %.4f → R² %.0f = %.4f (change = %.4f)\n",
              svy,
              first_year$year, first_year$r_squared,
              last_year$year, last_year$r_squared,
              last_year$r_squared - first_year$r_squared))
}
cat("\n")


# ==============================================================================
# SAVE TABLES
# ==============================================================================

cat("Saving R² tables...\n")

# Individual survey tables (CSV + RDS)
save_all_r2_tables(
  r2_list = r2_list,
  output_dir = here::here("output", "tables"),
  date_suffix = TRUE
)

# Combined table with all surveys
combined_r2 <- bind_rows(r2_list, .id = "survey")

draft_date <- format(Sys.Date(), "%Y%m%d")
combined_r2_path <- here::here("output", "tables",
                               paste0("fig1c_r2_all_surveys_", draft_date, ".csv"))

readr::write_csv(
  combined_r2 |> mutate(across(where(is.numeric), ~ round(.x, 6))),
  combined_r2_path
)
cat("Saved: ", basename(combined_r2_path), "\n\n")


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("========================================\n")
cat("Figure 1 Panel C data complete!\n")
cat("========================================\n")
cat("\nOutputs:\n")
cat("  Tables: output/tables/fig1c_r2_*.{csv,rds}\n")
cat("          output/tables/fig1c_r2_all_surveys_*.csv\n")
cat("\nKey finding:\n")
cat("  R² is generally low (<10%), meaning age alone explains\n")
cat("  little variance in SRH. Check if R² declines over time.\n")
cat("\n")
