# ==============================================================================
# 11_medpolish_apc.R
# Median Polish APC Decomposition — All 6 Surveys
# Author: Christine Lucille Kuryla
#
# Purpose: Non-parametric APC decomposition via median polish on survey-weighted
# cell means. Complements the parametric BHAPC analysis (06_bhapc_analysis.R)
# by triangulating APC effects without distributional assumptions.
#
# Approach:
#   1. Compute survey-weighted mean SRH in (5-yr age group x 5-yr period) cells
#   2. Build three 2D matrices (AP, AC, PC)
#   3. Run medpolish() on each → extract Age, Period, Cohort effects
#   4. Fit linear trends to each APC dimension
#
# Outputs:
#   output/apc/medpolish/
#     Per-survey: effects_{survey}.csv, cell_means_{survey}.csv,
#                 medpolish_full_{survey}.rds,
#                 medpolish_effects_{survey}.{png,pdf},
#                 medpolish_residuals_{survey}.{png,pdf}
#     Combined:   medpolish_effects_all_surveys.csv,
#                 medpolish_lm_summary.csv,
#                 medpolish_{age,period,cohort}_all_surveys.{png,pdf}
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(survey)
library(srvyr)
library(patchwork)
library(broom)

source(here::here("R", "paths.R"))
source(here::here("R", "functions", "theme_srh.R"))
source(here::here("R", "functions", "plot_utils.R"))
source(here::here("R", "functions", "medpolish_apc.R"))

# Create output directory
mp_output_dir <- here::here("output", "apc", "medpolish")
dir.create(mp_output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Survey metadata for median polish
MEDPOLISH_META <- list(
  gss = list(
    srh_scale = 4,
    has_design = FALSE  # weights only
  ),
  nhanes = list(
    srh_scale = 5,
    has_design = TRUE   # full design (strata + PSU + wt)
  ),
  meps = list(
    srh_scale = 5,
    has_design = TRUE
  ),
  nhis = list(
    srh_scale = 5,
    has_design = TRUE
  ),
  cps = list(
    srh_scale = 5,
    has_design = FALSE  # weights only
  ),
  brfss = list(
    srh_scale = 5,
    has_design = FALSE  # weights only per CLAUDE.md (computational constraints)
  )
)

# Processing order: smallest to largest for testing
SURVEY_ORDER <- c("gss", "nhanes", "meps", "nhis", "cps", "brfss")

# Display names for plots
SURVEY_LABELS <- c(
  gss    = "GSS",
  nhanes = "NHANES",
  meps   = "MEPS",
  nhis   = "NHIS",
  cps    = "CPS",
  brfss  = "BRFSS"
)


# ------------------------------------------------------------------------------
# Per-Survey Pipeline
# ------------------------------------------------------------------------------

#' Run median polish APC analysis for one survey
#'
#' @param survey_name Character: one of names(MEDPOLISH_META)
#' @return List with cell_means, mp_result, effects, lm_summary
run_medpolish_survey <- function(survey_name) {
  survey_name <- tolower(survey_name)
  meta <- MEDPOLISH_META[[survey_name]]

  if (is.null(meta)) {
    stop("Unknown survey: ", survey_name)
  }

  message("\n", strrep("=", 60))
  message("MEDIAN POLISH: ", toupper(survey_name))
  message(strrep("=", 60))

  # --- Load data ---
  file_path <- derived_path(paste0("data_", survey_name, ".rds"))
  if (!file.exists(file_path)) {
    stop("Data file not found: ", file_path)
  }
  message("Loading data from: ", file_path)
  df <- readr::read_rds(file_path) |>
    drop_na(srh, age, year, wt)

  message("  Loaded ", format(nrow(df), big.mark = ","), " rows")
  message("  Year range: ", min(df$year), "-", max(df$year))

  # Verify SRH coding
  srh_range <- range(df$srh, na.rm = TRUE)
  stopifnot(srh_range[1] >= 1, srh_range[2] <= meta$srh_scale)

  # --- Prepare cell means ---
  cell_means <- prepare_medpolish_data(
    df,
    survey         = survey_name,
    srh_var        = "srh",
    age_min        = 20,
    age_max        = 84,
    use_full_design = meta$has_design
  )

  message("  Cell means computed: ", nrow(cell_means), " cells")
  message("  Mean SRH range: [",
          round(min(cell_means$mean_srh, na.rm = TRUE), 2), ", ",
          round(max(cell_means$mean_srh, na.rm = TRUE), 2), "]")

  # --- Run median polish ---
  mp_result <- run_medpolish_apc(cell_means)

  # --- Extract effects ---
  effects <- extract_medpolish_effects(mp_result)

  # --- Direct row/col effects ---
  direct_effects <- extract_direct_effects(mp_result)

  # --- Variance decomposition ---
  var_decomp <- compute_variance_decomposition(mp_result)

  # --- Fit linear trends ---
  lm_summary <- fit_effect_lm(effects)

  # --- Log key results ---
  message("\n  Linear trend slopes:")
  lm_slopes <- lm_summary |> filter(term == "value")
  for (i in seq_len(nrow(lm_slopes))) {
    row <- lm_slopes[i, ]
    message("    ", row$dimension, ": ",
            sprintf("%.4f", row$estimate),
            " [", sprintf("%.4f", row$conf.low), ", ",
            sprintf("%.4f", row$conf.high), "]",
            if (row$p.value < 0.05) " *" else "")
  }

  message("\n  Variance decomposition (% of total SS):")
  for (sl in c("AP", "AC", "PC")) {
    vd <- var_decomp |> filter(slice == sl, component != "Total")
    msg <- paste(vd$component, sprintf("%.1f%%", vd$pct), collapse = ", ")
    message("    ", sl, ": ", msg)
  }

  # --- Generate & save per-survey outputs ---

  # Effects plot (mean-residual based)
  p_effects <- plot_medpolish_effects(effects, survey_name)
  save_figure(p_effects,
              paste0("medpolish_effects_", survey_name),
              path = mp_output_dir,
              width = 14, height = 5)

  # Direct effects plot (row/col from medpolish)
  p_direct <- plot_direct_effects(direct_effects, survey_name)
  save_figure(p_direct,
              paste0("medpolish_direct_effects_", survey_name),
              path = mp_output_dir,
              width = 14, height = 6)

  # Residuals plot
  p_resid <- plot_medpolish_residuals(mp_result, survey_name)
  save_figure(p_resid,
              paste0("medpolish_residuals_", survey_name),
              path = mp_output_dir,
              width = 14, height = 5)

  # CSV tables
  write_csv(effects,
            file.path(mp_output_dir, paste0("effects_", survey_name, ".csv")))
  write_csv(direct_effects,
            file.path(mp_output_dir, paste0("direct_effects_", survey_name, ".csv")))
  write_csv(var_decomp,
            file.path(mp_output_dir, paste0("variance_decomp_", survey_name, ".csv")))
  write_csv(cell_means,
            file.path(mp_output_dir, paste0("cell_means_", survey_name, ".csv")))

  # Full results RDS
  full_results <- list(
    survey         = survey_name,
    cell_means     = cell_means,
    mp_result      = mp_result,
    effects        = effects,
    direct_effects = direct_effects,
    var_decomp     = var_decomp,
    lm_summary     = lm_summary,
    timestamp      = Sys.time()
  )
  saveRDS(full_results,
          file.path(mp_output_dir, paste0("medpolish_full_", survey_name, ".rds")))

  message("  Outputs saved to: ", mp_output_dir)

  full_results
}


# ------------------------------------------------------------------------------
# Cross-Survey Summary
# ------------------------------------------------------------------------------

#' Run median polish for all surveys and generate combined outputs
#'
#' @param surveys Character vector of survey names (default SURVEY_ORDER)
#' @return List of per-survey results
run_all_medpolish <- function(surveys = SURVEY_ORDER) {

  message("\n", strrep("=", 70))
  message("MEDIAN POLISH APC PIPELINE — ALL SURVEYS")
  message("Starting at: ", Sys.time())
  message(strrep("=", 70))

  all_results  <- list()
  all_effects  <- tibble()
  all_direct   <- tibble()
  all_lm       <- tibble()
  all_var      <- tibble()

  for (sv in surveys) {
    tryCatch({
      result <- run_medpolish_survey(sv)
      all_results[[sv]] <- result

      # Collect effects with survey label
      all_effects <- bind_rows(
        all_effects,
        result$effects |> mutate(survey = toupper(sv))
      )

      # Collect direct effects
      all_direct <- bind_rows(
        all_direct,
        result$direct_effects |> mutate(survey = toupper(sv))
      )

      # Collect LM summaries
      all_lm <- bind_rows(
        all_lm,
        result$lm_summary |> mutate(survey = toupper(sv))
      )

      # Collect variance decomposition
      all_var <- bind_rows(
        all_var,
        result$var_decomp |> mutate(survey = toupper(sv))
      )

    }, error = function(e) {
      message("\nERROR processing ", toupper(sv), ": ", e$message)
    })
  }

  # --- Combined outputs ---

  if (nrow(all_effects) > 0) {
    # Set survey factor order for plots
    sv_levels <- toupper(surveys)
    all_effects <- all_effects |>
      mutate(survey = factor(survey, levels = sv_levels))

    # --- CSV tables ---
    write_csv(all_effects,
              file.path(mp_output_dir, "medpolish_effects_all_surveys.csv"))
    write_csv(all_direct,
              file.path(mp_output_dir, "medpolish_direct_effects_all_surveys.csv"))
    write_csv(all_lm,
              file.path(mp_output_dir, "medpolish_lm_summary.csv"))
    write_csv(all_var,
              file.path(mp_output_dir, "medpolish_variance_decomp_all_surveys.csv"))

    # --- Cross-survey comparison plots (one per dimension) ---
    for (dim_name in c("Age", "Period", "Cohort")) {
      p_cross <- plot_medpolish_all_surveys(all_effects, dim_name)
      save_figure(p_cross,
                  paste0("medpolish_", tolower(dim_name), "_all_surveys"),
                  path = mp_output_dir,
                  width = 12, height = 8)
    }

    # --- Combined 6x3 publication figure ---
    # Use simple version (no ggtext dependency)
    p_combined <- plot_medpolish_combined_simple(all_effects,
                                                 survey_order = sv_levels)
    save_figure(p_combined,
                "medpolish_combined_all_surveys",
                path = mp_output_dir,
                width = 12, height = 14)

    # Try ggtext version if available
    if (requireNamespace("ggtext", quietly = TRUE)) {
      p_combined_gt <- plot_medpolish_combined(all_effects,
                                               survey_order = sv_levels)
      save_figure(p_combined_gt,
                  "medpolish_combined_all_surveys_styled",
                  path = mp_output_dir,
                  width = 12, height = 14)
    }

    message("\n  Combined outputs saved.")
  }

  message("\n", strrep("=", 70))
  message("PIPELINE COMPLETE")
  message("Finished at: ", Sys.time())
  message(strrep("=", 70))

  invisible(all_results)
}


# ==============================================================================
# Run
# ==============================================================================

# Uncomment to run individual surveys for testing:
# results_gss <- run_medpolish_survey("gss")
# results_nhanes <- run_medpolish_survey("nhanes")

# Uncomment to run all surveys:
# all_results <- run_all_medpolish()
