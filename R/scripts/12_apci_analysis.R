# ==============================================================================
# 12_apci_analysis.R
# APCI (Age-Period-Cohort-Interaction) Analysis — All 6 Surveys
# Author: Christine Lucille Kuryla
#
# Purpose: Run the APCI model (Luo & Hodges, 2020) on all 6 SRH surveys.
# APCI conceptualizes cohort effects as age-by-period interactions,
# providing formal hypothesis tests for:
#   - Global F-test: Do cohort effects exist (age x period interaction)?
#   - Inter-cohort deviations: Which birth cohorts deviate from main effects?
#   - Intra-cohort slopes: Do effects accumulate, diminish, or stay constant?
#
# Complements median polish (descriptive) and BHAPC (Bayesian parametric)
# by adding frequentist inferential statistics.
#
# Note: Uses glm() with survey weights for weighted point estimates.
# Standard errors do NOT account for complex survey design (strata/PSU).
# See report for discussion of this limitation.
#
# Outputs:
#   output/apc/apci/
#     Per-survey: apci_results_{survey}.rds, apci_cohort_avgs_{survey}.csv,
#                 apci_cohort_slopes_{survey}.csv,
#                 apci_main_effects_{survey}.{png,pdf},
#                 apci_cohort_averages_{survey}.{png,pdf},
#                 apci_cohort_slopes_{survey}.{png,pdf},
#                 apci_combined_{survey}.{png,pdf},
#                 apci_heatmap_{survey}.png,
#                 apci_bar_{survey}.png
#     Combined:   apci_cohort_avgs_all.csv, apci_cohort_slopes_all.csv,
#                 apci_deviance_tests.csv, apci_age_effects_all.csv,
#                 apci_period_effects_all.csv,
#                 apci_cohort_avgs_all_surveys.{png,pdf},
#                 apci_cohort_slopes_all_surveys.{png,pdf},
#                 apci_main_effects_all_surveys.{png,pdf}
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(APCI)
library(patchwork)

source(here::here("R", "paths.R"))
source(here::here("R", "functions", "theme_srh.R"))
source(here::here("R", "functions", "plot_utils.R"))
source(here::here("R", "functions", "apci_analysis.R"))

# Create output directory
apci_output_dir <- here::here("output", "apc", "apci")
dir.create(apci_output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Survey metadata
APCI_META <- list(
  gss = list(
    srh_scale = 4,
    label = "GSS"
  ),
  nhanes = list(
    srh_scale = 5,
    label = "NHANES"
  ),
  meps = list(
    srh_scale = 5,
    label = "MEPS"
  ),
  nhis = list(
    srh_scale = 5,
    label = "NHIS"
  ),
  cps = list(
    srh_scale = 5,
    label = "CPS"
  ),
  brfss = list(
    srh_scale = 5,
    label = "BRFSS"
  )
)

# Processing order: smallest to largest for testing
SURVEY_ORDER <- c("gss", "nhanes", "meps", "nhis", "cps", "brfss")


# ------------------------------------------------------------------------------
# Per-Survey Pipeline
# ------------------------------------------------------------------------------

#' Run APCI analysis for one survey
#'
#' @param survey_name Character: one of names(APCI_META)
#' @return List with df_prep, model, results, and metadata
run_apci_survey <- function(survey_name) {
  survey_name <- tolower(survey_name)
  meta <- APCI_META[[survey_name]]

  if (is.null(meta)) {
    stop("Unknown survey: ", survey_name)
  }

  message("\n", strrep("=", 60))
  message("APCI: ", toupper(survey_name))
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

  # --- Prepare data ---
  df_prep <- prep_apci_data(
    df,
    survey  = survey_name,
    srh_var = "srh",
    age_min = 20,
    age_max = 89
  )

  # Quick weighted mean sanity check
  overall_mean <- weighted.mean(df_prep$srh, df_prep$wt, na.rm = TRUE)
  message("  Overall weighted mean SRH: ", round(overall_mean, 3))

  # --- Run APCI model ---
  message("\n  Fitting APCI model...")
  t_start <- Sys.time()

  model <- run_apci_model(df_prep, srh_var = "srh", family = "gaussian")

  t_elapsed <- difftime(Sys.time(), t_start, units = "secs")
  message("  Model fit in ", round(as.numeric(t_elapsed), 1), " seconds")

  # --- Extract results ---
  results <- extract_apci_results(model, df_prep)

  # --- Log key results ---
  message("\n  Intercept: ", round(results$intercept, 4))

  n_sig_cohort <- sum(results$cohort_averages$p_value < 0.05, na.rm = TRUE)
  n_total_cohort <- nrow(results$cohort_averages)
  message("  Significant cohort deviations: ", n_sig_cohort, "/", n_total_cohort)

  n_sig_slope <- sum(results$cohort_slopes$p_value < 0.05, na.rm = TRUE)
  message("  Significant cohort slopes: ", n_sig_slope, "/", nrow(results$cohort_slopes))

  # --- Generate package built-in plots ---
  message("\n  Generating built-in APCI plots...")

  # Heatmap
  tryCatch({
    png(file.path(apci_output_dir, paste0("apci_heatmap_", survey_name, ".png")),
        width = 10, height = 8, units = "in", res = 300)
    apci.plot.heatmap(model, age = "age_group", period = "period_group")
    dev.off()
    message("  Saved heatmap")
  }, error = function(e) {
    message("  Warning: Heatmap plot failed: ", e$message)
    tryCatch(dev.off(), error = function(e2) NULL)
  })

  # Bar plot (cohort deviations + slopes)
  tryCatch({
    png(file.path(apci_output_dir, paste0("apci_bar_", survey_name, ".png")),
        width = 12, height = 8, units = "in", res = 300)
    apci.bar(model, age = "age_group", period = "period_group",
             outcome_var = "srh")
    dev.off()
    message("  Saved bar plot")
  }, error = function(e) {
    message("  Warning: Bar plot failed: ", e$message)
    tryCatch(dev.off(), error = function(e2) NULL)
  })

  # Standard APCI 4-panel plot
  tryCatch({
    png(file.path(apci_output_dir, paste0("apci_plot_", survey_name, ".png")),
        width = 12, height = 10, units = "in", res = 300)
    apci.plot(model, age = "age_group", period = "period_group",
              outcome_var = "srh")
    dev.off()
    message("  Saved standard plot")
  }, error = function(e) {
    message("  Warning: Standard plot failed: ", e$message)
    tryCatch(dev.off(), error = function(e2) NULL)
  })

  # --- Generate custom plots with theme_srh ---
  message("  Generating custom plots...")

  # Main effects
  p_main <- plot_apci_main_effects(results, survey_name)
  save_figure(p_main,
              paste0("apci_main_effects_", survey_name),
              path = apci_output_dir,
              width = 12, height = 5)

  # Cohort averages
  p_avg <- plot_apci_cohort_averages(results, survey_name)
  save_figure(p_avg,
              paste0("apci_cohort_averages_", survey_name),
              path = apci_output_dir,
              width = 10, height = 6)

  # Cohort slopes
  p_slopes <- plot_apci_cohort_slopes(results, survey_name)
  save_figure(p_slopes,
              paste0("apci_cohort_slopes_", survey_name),
              path = apci_output_dir,
              width = 10, height = 6)

  # Combined per-survey figure
  p_combined <- plot_apci_combined(results, survey_name)
  save_figure(p_combined,
              paste0("apci_combined_", survey_name),
              path = apci_output_dir,
              width = 14, height = 12)

  # --- Save CSV tables ---
  write_csv(results$age_effects,
            file.path(apci_output_dir, paste0("apci_age_effects_", survey_name, ".csv")))
  write_csv(results$period_effects,
            file.path(apci_output_dir, paste0("apci_period_effects_", survey_name, ".csv")))
  write_csv(results$cohort_averages,
            file.path(apci_output_dir, paste0("apci_cohort_avgs_", survey_name, ".csv")))
  write_csv(results$cohort_slopes,
            file.path(apci_output_dir, paste0("apci_cohort_slopes_", survey_name, ".csv")))

  # --- Save full results RDS ---
  full_results <- list(
    survey    = survey_name,
    df_prep   = df_prep,
    model     = model,
    results   = results,
    meta      = meta,
    timestamp = Sys.time()
  )
  saveRDS(full_results,
          file.path(apci_output_dir, paste0("apci_full_", survey_name, ".rds")))

  message("  Outputs saved to: ", apci_output_dir)

  full_results
}


# ------------------------------------------------------------------------------
# Cross-Survey Summary
# ------------------------------------------------------------------------------

#' Run APCI for all surveys and generate combined outputs
#'
#' @param surveys Character vector of survey names (default SURVEY_ORDER)
#' @return List of per-survey results
run_all_apci <- function(surveys = SURVEY_ORDER) {

  message("\n", strrep("=", 70))
  message("APCI PIPELINE — ALL SURVEYS")
  message("Starting at: ", Sys.time())
  message(strrep("=", 70))

  all_results       <- list()
  all_age_effects   <- tibble()
  all_period_effects <- tibble()
  all_cohort_avgs   <- tibble()
  all_cohort_slopes <- tibble()
  deviance_summary  <- tibble()

  for (sv in surveys) {
    tryCatch({
      result <- run_apci_survey(sv)
      all_results[[sv]] <- result

      sv_label <- toupper(sv)

      # Collect age effects
      all_age_effects <- bind_rows(
        all_age_effects,
        result$results$age_effects |> mutate(survey = sv_label)
      )

      # Collect period effects
      all_period_effects <- bind_rows(
        all_period_effects,
        result$results$period_effects |> mutate(survey = sv_label)
      )

      # Collect cohort averages
      all_cohort_avgs <- bind_rows(
        all_cohort_avgs,
        result$results$cohort_averages |> mutate(survey = sv_label)
      )

      # Collect cohort slopes
      all_cohort_slopes <- bind_rows(
        all_cohort_slopes,
        result$results$cohort_slopes |> mutate(survey = sv_label)
      )

      # Collect deviance test info
      deviance_summary <- bind_rows(
        deviance_summary,
        tibble(
          survey    = sv_label,
          intercept = result$results$intercept,
          n_cohorts = nrow(result$results$cohort_averages),
          n_sig_cohort_avg = sum(result$results$cohort_averages$p_value < 0.05,
                                 na.rm = TRUE),
          n_sig_cohort_slope = sum(result$results$cohort_slopes$p_value < 0.05,
                                    na.rm = TRUE),
          n_obs = nrow(result$df_prep)
        )
      )

    }, error = function(e) {
      message("\nERROR processing ", toupper(sv), ": ", e$message)
      message("  Traceback: ", paste(capture.output(traceback()), collapse = "\n  "))
    })
  }

  # --- Combined outputs ---

  if (nrow(all_cohort_avgs) > 0) {

    # --- Save combined CSV tables ---
    write_csv(all_age_effects,
              file.path(apci_output_dir, "apci_age_effects_all.csv"))
    write_csv(all_period_effects,
              file.path(apci_output_dir, "apci_period_effects_all.csv"))
    write_csv(all_cohort_avgs,
              file.path(apci_output_dir, "apci_cohort_avgs_all.csv"))
    write_csv(all_cohort_slopes,
              file.path(apci_output_dir, "apci_cohort_slopes_all.csv"))
    write_csv(deviance_summary,
              file.path(apci_output_dir, "apci_deviance_summary.csv"))

    # --- Cross-survey plots ---
    sv_levels <- toupper(surveys[surveys %in% names(all_results)])

    # Cohort averages across surveys
    p_avgs <- plot_apci_cohort_avgs_all(all_cohort_avgs, survey_order = sv_levels)
    save_figure(p_avgs,
                "apci_cohort_avgs_all_surveys",
                path = apci_output_dir,
                width = 12, height = 8)

    # Cohort slopes across surveys
    p_slopes <- plot_apci_cohort_slopes_all(all_cohort_slopes, survey_order = sv_levels)
    save_figure(p_slopes,
                "apci_cohort_slopes_all_surveys",
                path = apci_output_dir,
                width = 12, height = 8)

    # Main effects across surveys
    p_main <- plot_apci_main_effects_all(all_age_effects, all_period_effects,
                                          survey_order = sv_levels)
    save_figure(p_main,
                "apci_main_effects_all_surveys",
                path = apci_output_dir,
                width = 12, height = 14)

    message("\n  Combined outputs saved.")
  }

  # --- Print summary ---
  message("\n", strrep("=", 70))
  message("APCI PIPELINE SUMMARY")
  message(strrep("=", 70))

  if (nrow(deviance_summary) > 0) {
    message("\nSurvey | N | Intercept | Sig Cohort Avgs | Sig Cohort Slopes")
    message(strrep("-", 65))
    for (i in seq_len(nrow(deviance_summary))) {
      row <- deviance_summary[i, ]
      message(sprintf("%-7s| %s | %.3f | %d/%d | %d/%d",
                      row$survey,
                      format(row$n_obs, big.mark = ","),
                      row$intercept,
                      row$n_sig_cohort_avg, row$n_cohorts,
                      row$n_sig_cohort_slope, row$n_cohorts))
    }
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
# results_gss <- run_apci_survey("gss")

# Uncomment to run all surveys (excluding NHANES/BRFSS which need special handling):
# all_results <- run_all_apci(surveys = c("gss", "meps", "nhis", "cps"))

# --- NHANES: requires age_max = 79 due to sparse cells in 80-89 age group ---
# df <- readr::read_rds(derived_path("data_nhanes.rds")) |> drop_na(srh, age, year, wt)
# df_prep <- prep_apci_data(df, survey = "nhanes", srh_var = "srh", age_min = 20, age_max = 79)
# model <- run_apci_model(df_prep)
# results <- extract_apci_results(model, df_prep)
# (then save outputs manually — see the per-survey pipeline above)

# --- BRFSS: requires 50% subsample due to memory constraints (10.7M rows) ---
# set.seed(42)
# df <- readr::read_rds(derived_path("data_brfss.rds")) |> drop_na(srh, age, year, wt)
# df_sub <- df |> slice_sample(prop = 0.50)
# df_prep <- prep_apci_data(df_sub, survey = "brfss", srh_var = "srh", age_min = 20, age_max = 89)
# model <- run_apci_model(df_prep)
# results <- extract_apci_results(model, df_prep)
# (then save outputs manually)
