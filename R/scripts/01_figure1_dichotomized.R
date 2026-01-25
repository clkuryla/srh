# ==============================================================================
# 01_figure1_dichotomized.R
# Generate Figure 1 Analogs for Dichotomized SRH (Supplementary Materials)
#
# Purpose:
#   Create supplementary figures showing the convergence phenomenon using
#   multiple dichotomized SRH operationalizations:
#   1. Good or Better (srh >= 3)
#   2. Excellent (srh == 5)
#   3. Excellent or Very Good (srh >= 4)
#
# Each dichotomization produces a 2x6 figure (2 rows x 6 surveys):
#   - Row 1: Prevalence by age group over time (probability scale)
#   - Row 2: Age coefficient from logistic regression
#
# Two coefficient scales:
#   - Log-odds: Raw logistic regression coefficient
#   - Marginal: Average change in predicted probability per 10-year age increase
#
# Outputs:
#   - output/sensitivity/dichotomized/figures/fig1_dichot_*.png
#   - output/sensitivity/dichotomized/figures/fig1_dichot_*.pdf
#   - output/sensitivity/dichotomized/tables/fig1_dichot_*.rds
#
# Author: Christine Lucille Kuryla
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(tidyverse)
library(here)
library(srvyr)
library(survey)
library(patchwork)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/srh_common_functions.R"))
source(here::here("R/functions/theme_srh.R"))
source(here::here("R/functions/srh_prevalence_over_time.R"))
source(here::here("R/functions/regress_srh_on_age.R"))
source(here::here("R/functions/plot_fig1_combined.R"))

cat("========================================\n")
cat("Figure 1 Dichotomized SRH Analogs\n")
cat("(Supplementary Materials)\n")
cat("========================================\n\n")


# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Output directories
fig_dir <- here::here("output", "sensitivity", "dichotomized", "figures")
tbl_dir <- here::here("output", "sensitivity", "dichotomized", "tables")
cache_dir <- derived_path("cache")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tbl_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Date suffix for output files
draft_date <- format(Sys.Date(), "%Y%m%d")

# Survey order for the figure
survey_order <- c("BRFSS", "MEPS", "NHIS", "GSS", "CPS", "NHANES")

# Dichotomization schemes
# All coded so higher = better (1 = good/excellent health)
dichotomizations <- list(
  good_plus = list(
    name = "good_plus",
    label = "Good or Better Health",
    threshold = 3,  # srh >= 3
    direction = ">=",
    description = "Good, Very Good, or Excellent vs Fair/Poor"
  ),
  excellent = list(
    name = "excellent",
    label = "Excellent Health",
    threshold = 5,  # srh == 5
    direction = "==",
    description = "Excellent vs all others"
  ),
  excellent_vgood = list(
    name = "excellent_vgood",
    label = "Excellent or Very Good Health",
    threshold = 4,  # srh >= 4
    direction = ">=",
    description = "Excellent or Very Good vs Good/Fair/Poor"
  )
)


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Create dichotomized SRH variable
#'
#' @param data Data frame with srh column
#' @param dichot_spec Dichotomization specification (from dichotomizations list)
#' @param new_var Name for new variable (default "srh_dichot")
#'
#' @return Data frame with new dichotomized column
#'
add_dichot_srh <- function(data, dichot_spec, new_var = "srh_dichot") {

  threshold <- dichot_spec$threshold
  direction <- dichot_spec$direction

  data %>%
    mutate(
      !!new_var := case_when(
        direction == ">=" ~ as.integer(srh >= threshold),
        direction == "==" ~ as.integer(srh == threshold),
        direction == "<=" ~ as.integer(srh <= threshold),
        TRUE ~ NA_integer_
      )
    )
}


#' Process one survey for one dichotomization
#'
#' @param data Survey data
#' @param survey_name Survey name
#' @param dichot_spec Dichotomization specification
#' @param use_chunked Use chunked processing (for BRFSS)
#'
#' @return List with prevalence_result and coefficients
#'
process_survey_dichot <- function(data, survey_name, dichot_spec, use_chunked = FALSE) {

  # Add dichotomized variable
  data <- add_dichot_srh(data, dichot_spec, new_var = "srh_dichot")

  # Check dichotomization worked
  n_valid <- sum(!is.na(data$srh_dichot))
  n_positive <- sum(data$srh_dichot == 1, na.rm = TRUE)
  cat("    N =", format(n_valid, big.mark = ","),
      ", prevalence =", round(100 * n_positive / n_valid, 1), "%\n")

  # Determine if survey has PSU/strata
  has_psu <- "psu" %in% names(data)
  has_strata <- "strata" %in% names(data)

  # Compute prevalence by age group and year
  if (use_chunked) {
    prev_result <- summarize_srh_prevalence_over_time_chunked(
      data = data,
      survey_name = survey_name,
      srh_dichot_var = "srh_dichot",
      psu_var = if (has_psu) "psu" else NULL,
      strata_var = if (has_strata) "strata" else NULL,
      title_style = "dataset",
      cache_dir = file.path(cache_dir, dichot_spec$name),
      force_recompute = FALSE
    )
  } else {
    prev_result <- summarize_srh_prevalence_over_time(
      data = data,
      survey_name = survey_name,
      srh_dichot_var = "srh_dichot",
      psu_var = if (has_psu) "psu" else NULL,
      strata_var = if (has_strata) "strata" else NULL,
      title_style = "dataset"
    )
  }

  # Compute logistic regression coefficients by year
  coefficients <- regress_age_coefficient_by_year_logistic(
    data = data,
    survey_name = survey_name,
    srh_dichot_var = "srh_dichot",
    psu_var = if (has_psu) "psu" else NULL,
    strata_var = if (has_strata) "strata" else NULL,
    compute_marginal = TRUE,
    age_increment = 10
  )

  list(
    prevalence = prev_result$estimates,
    coefficients = coefficients
  )
}


#' Save estimates tables
#'
#' @param estimates Data frame
#' @param survey_name Survey name
#' @param dichot_name Dichotomization name
#' @param estimate_type "prev" or "coef"
#'
save_dichot_table <- function(estimates, survey_name, dichot_name, estimate_type) {

  base_name <- paste0("fig1_dichot_", dichot_name, "_", estimate_type, "_",
                      tolower(survey_name), "_", draft_date)

  # RDS (full precision)
  rds_path <- file.path(tbl_dir, paste0(base_name, ".rds"))
  readr::write_rds(estimates, rds_path)

  # CSV (rounded)
  estimates_rounded <- estimates %>%
    mutate(across(where(is.numeric), ~ round(.x, 6)))
  csv_path <- file.path(tbl_dir, paste0(base_name, ".csv"))
  readr::write_csv(estimates_rounded, csv_path)
}


# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data...\n")

# Load all six surveys with necessary columns
# For BRFSS, don't use PSU/strata due to computational constraints (per CLAUDE.md)

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

# BRFSS: weights only (no PSU/strata per computational constraints)
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

# Create named list of data
data_list <- list(
  BRFSS = data_brfss,
  MEPS = data_meps,
  NHIS = data_nhis,
  GSS = data_gss,
  CPS = data_cps,
  NHANES = data_nhanes
)


# ==============================================================================
# PROCESS EACH DICHOTOMIZATION
# ==============================================================================

for (dichot_name in names(dichotomizations)) {

  dichot_spec <- dichotomizations[[dichot_name]]

  cat("========================================\n")
  cat("Processing: ", dichot_spec$label, "\n")
  cat("  (", dichot_spec$description, ")\n")
  cat("========================================\n\n")

  # Storage for this dichotomization
  prevalence_list <- list()
  coefficients_list <- list()

  # Process each survey
  for (survey_name in survey_order) {
    cat("  ", survey_name, "...\n")

    # Use chunked processing for BRFSS
    use_chunked <- (survey_name == "BRFSS")

    result <- process_survey_dichot(
      data = data_list[[survey_name]],
      survey_name = survey_name,
      dichot_spec = dichot_spec,
      use_chunked = use_chunked
    )

    prevalence_list[[survey_name]] <- result$prevalence
    coefficients_list[[survey_name]] <- result$coefficients

    # Save tables
    save_dichot_table(result$prevalence, survey_name, dichot_name, "prev")
    save_dichot_table(result$coefficients, survey_name, dichot_name, "coef")
  }

  cat("\n")

  # --------------------------------------------------------------------------
  # Run metaregression for each survey
  # --------------------------------------------------------------------------

  cat("Running metaregression...\n")

  # Log-odds metaregression
  meta_logodds_list <- lapply(survey_order, function(svy) {
    run_metaregression_logistic(coefficients_list[[svy]], svy, use_marginal = FALSE)
  })
  names(meta_logodds_list) <- survey_order

  # Marginal effect metaregression
  meta_marginal_list <- lapply(survey_order, function(svy) {
    run_metaregression_logistic(coefficients_list[[svy]], svy, use_marginal = TRUE)
  })
  names(meta_marginal_list) <- survey_order

  # Save metaregression results
  meta_logodds_df <- bind_rows(meta_logodds_list) %>%
    mutate(coefficient_type = "log_odds")
  meta_marginal_df <- bind_rows(meta_marginal_list) %>%
    mutate(coefficient_type = "marginal")

  meta_all <- bind_rows(meta_logodds_df, meta_marginal_df)
  meta_path <- file.path(tbl_dir, paste0("fig1_dichot_", dichot_name, "_meta_", draft_date, ".rds"))
  readr::write_rds(meta_all, meta_path)
  cat("  Saved metaregression results\n\n")

  # --------------------------------------------------------------------------
  # Generate figures
  # --------------------------------------------------------------------------

  cat("Generating figures...\n")

  # --- Figure with log-odds coefficient ---
  fig_logodds <- plot_fig1_dichotomized_combined(
    prevalence_list = prevalence_list,
    coefficients_list = coefficients_list,
    meta_results_list = meta_logodds_list,
    colors = age_colors,
    use_marginal = FALSE,
    show_ci = FALSE,
    show_metareg = TRUE,
    title = paste0("Dichotomized SRH: ", dichot_spec$label),
    subtitle = dichot_spec$description,
    panel_a_ylabel = "Prevalence",
    panel_b_ylabel = "Age coefficient (log-odds)",
    base_size = 14,
    tilt_x_labels = 45
  )

  # Save log-odds figure
  ggsave(
    filename = file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_logodds.png")),
    plot = fig_logodds,
    width = 16,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_logodds.pdf")),
    plot = fig_logodds,
    width = 16,
    height = 8
  )
  cat("  Saved: fig1_dichot_", dichot_name, "_logodds.{png,pdf}\n")

  # --- Figure with marginal effect ---
  fig_marginal <- plot_fig1_dichotomized_combined(
    prevalence_list = prevalence_list,
    coefficients_list = coefficients_list,
    meta_results_list = meta_marginal_list,
    colors = age_colors,
    use_marginal = TRUE,
    show_ci = FALSE,
    show_metareg = TRUE,
    title = paste0("Dichotomized SRH: ", dichot_spec$label),
    subtitle = paste0(dichot_spec$description, " (Marginal effects)"),
    panel_a_ylabel = "Prevalence",
    panel_b_ylabel = "Marginal effect (prob. per 10 years)",
    base_size = 14,
    tilt_x_labels = 45
  )

  # Save marginal figure
  ggsave(
    filename = file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_marginal.png")),
    plot = fig_marginal,
    width = 16,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_marginal.pdf")),
    plot = fig_marginal,
    width = 16,
    height = 8
  )
  cat("  Saved: fig1_dichot_", dichot_name, "_marginal.{png,pdf}\n")

  # --- Draft version with date ---
  ggsave(
    filename = file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_draft_", draft_date, ".png")),
    plot = fig_logodds,
    width = 16,
    height = 8,
    dpi = 300
  )
  cat("  Saved: fig1_dichot_", dichot_name, "_draft_", draft_date, ".png\n")

  cat("\n")

  # Clean up memory
  rm(prevalence_list, coefficients_list, meta_logodds_list, meta_marginal_list)
  rm(fig_logodds, fig_marginal)
  gc(verbose = FALSE)
}


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("========================================\n")
cat("Figure 1 Dichotomized SRH Complete!\n")
cat("========================================\n\n")

cat("Output files created:\n")
cat("  Figures: ", fig_dir, "/\n")
cat("    - fig1_dichot_good_plus_{logodds,marginal}.{png,pdf}\n")
cat("    - fig1_dichot_excellent_{logodds,marginal}.{png,pdf}\n")
cat("    - fig1_dichot_excellent_vgood_{logodds,marginal}.{png,pdf}\n")
cat("  Tables:  ", tbl_dir, "/\n")
cat("    - fig1_dichot_*_prev_*.{csv,rds}\n")
cat("    - fig1_dichot_*_coef_*.{csv,rds}\n")
cat("    - fig1_dichot_*_meta_*.rds\n")
cat("\n")

# List created files
cat("Files created:\n")
list.files(fig_dir, pattern = "fig1_dichot.*\\.(png|pdf)$") %>%
  paste0("  ", .) %>%
  cat(sep = "\n")
cat("\n")
