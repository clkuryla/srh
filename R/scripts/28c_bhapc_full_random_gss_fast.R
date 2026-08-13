# ==============================================================================
# 28c_bhapc_full_random_gss_fast.R
# GSS re-run with the SAME settings as the other five surveys (28b)
# Author: Christine Lucille Kuryla
#
# Purpose: the original full-random GSS fit came from 28_bhapc_full_random.R
# with iter = 6000 / adapt_delta = 0.998, while the other five surveys were
# fit by 28b with iter = 5000 / adapt_delta = 0.95. This script re-fits GSS
# with 28b's settings so all six surveys in Fig S6/S7 use one specification.
# It is a copy of 28b with: SURVEYS_TO_RUN = "gss", a GSS entry in
# SURVEY_CONFIG (srh_scale = 4, ages 18-89, no subsampling), and a final
# summary step that UPDATES the existing all_surveys_summary.csv /
# all_surveys_results.rds in place instead of rebuilding them.
# Overwrites output/bhapc_full_random/gss/ (previous run preserved in git).
# After this completes, re-run 29_bhapc_full_random_combined_figures.R to
# regenerate the manuscript figures.
#
# Usage:
#   Rscript R/scripts/28c_bhapc_full_random_gss_fast.R 2>&1 | tee output/bhapc_full_random/run_gss_fast.log
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BHAPC FULL RANDOM EFFECTS - GSS RE-RUN, FAST SETTINGS\n")
cat("Model: srh ~ lnWt + (1|age_group) + (1|period_4yr) + (1|cohort_4yr)\n")
cat("Settings: adapt_delta=0.95, iter=5000, SEQUENTIAL execution\n")
cat("Started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(srvyr)
  library(patchwork)
  library(gridExtra)
  library(grid)
})

# Source paths and functions
source(here("R", "paths.R"))
source(here("R", "functions", "subsample_survey.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_table_generation.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Override DATA_DEPOT if not set
if (Sys.getenv("DATA_DEPOT") == "") {
  Sys.setenv(DATA_DEPOT = "/home/ubuntu/data_depot")
}

# Output directory (same as original)
output_dir <- here("output", "bhapc_full_random")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260210)

# ==============================================================================
# Configuration - FAST SETTINGS
# ==============================================================================

N_CORES <- parallel::detectCores()
cat("Detected cores:", N_CORES, "(will use all for each survey)\n")

# GSS only (re-run with fast settings; full sample, no subsampling)
SURVEYS_TO_RUN <- c("gss")

# Subsampling configuration
SUBSAMPLE_TARGET <- 200000
SUBSAMPLE_SURVEYS <- c("meps", "nhis", "cps", "brfss")

# FAST settings
ADAPT_DELTA <- 0.95
ITER <- 5000

# Survey-specific configuration (gss matches 28_bhapc_full_random.R's config)
SURVEY_CONFIG <- list(
  nhanes = list(srh_scale = 5, age_min = 18, age_max = 80),
  meps   = list(srh_scale = 5, age_min = 18, age_max = 85),
  nhis   = list(srh_scale = 5, age_min = 18, age_max = 85),
  cps    = list(srh_scale = 5, age_min = 18, age_max = 85),
  brfss  = list(srh_scale = 5, age_min = 18, age_max = 89),
  gss    = list(srh_scale = 4, age_min = 18, age_max = 89)
)

cat("Surveys to run:", paste(SURVEYS_TO_RUN, collapse = ", "), "\n")
cat("adapt_delta:", ADAPT_DELTA, "\n")
cat("iter:", ITER, "\n")
cat("chains: 4\n\n")

# ==============================================================================
# Helper Functions
# ==============================================================================

load_survey_data <- function(survey_name) {
  data_path <- file.path(
    Sys.getenv("DATA_DEPOT"),
    "_derived", "srh_project", "essential_datasets",
    paste0("data_essential_", survey_name, ".rds")
  )

  if (!file.exists(data_path)) {
    stop("Data file not found: ", data_path)
  }

  df <- readRDS(data_path)
  df <- df %>% drop_na(srh, age, year, wt)
  original_n <- nrow(df)

  if (survey_name %in% SUBSAMPLE_SURVEYS && nrow(df) > SUBSAMPLE_TARGET) {
    cat("  Subsampling", toupper(survey_name), "from",
        format(nrow(df), big.mark = ","), "to",
        format(SUBSAMPLE_TARGET, big.mark = ","), "...\n")

    sub_result <- subsample_survey(
      df,
      target_n = SUBSAMPLE_TARGET,
      seed = 20260210 + which(SUBSAMPLE_SURVEYS == survey_name)
    )

    df <- sub_result$data %>%
      select(-wt) %>%
      rename(wt = wt_sub)

    cat("  Subsampled to:", format(nrow(df), big.mark = ","), "rows\n")
  }

  list(data = df, original_n = original_n, final_n = nrow(df))
}


create_full_random_summary_figure <- function(survey_name, bhapc_df, model_result,
                                               variance_df, age_effects, period_effects,
                                               cohort_effects, output_dir) {

  model <- model_result$model
  n_obs <- nrow(bhapc_df)

  # --- Lexis Diagram ---
  lexis_data <- bhapc_df %>%
    mutate(
      age_num = as.numeric(gsub("-.*", "", as.character(age_group))),
      period_num = as.numeric(period_4yr)
    ) %>%
    group_by(period_num, age_num) %>%
    summarise(mean_srh = weighted.mean(srh, wt, na.rm = TRUE), .groups = "drop")

  srh_range <- range(lexis_data$mean_srh, na.rm = TRUE)

  p_lexis <- ggplot(lexis_data, aes(x = period_num, y = age_num, fill = mean_srh)) +
    geom_tile() +
    geom_abline(intercept = seq(-2020, -1900, by = 10), slope = 1,
                color = "gray30", linetype = "dashed", alpha = 0.4, linewidth = 0.2) +
    scale_fill_viridis_c(option = "plasma", name = "SRH",
                         limits = c(floor(srh_range[1]*10)/10, ceiling(srh_range[2]*10)/10)) +
    labs(title = "Lexis Diagram", x = "Period", y = "Age") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.2, "cm"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )

  # --- Descriptive Means ---
  means_data <- bhapc_df %>%
    group_by(age_group, period_4yr) %>%
    summarise(mean_srh = weighted.mean(srh, wt, na.rm = TRUE), .groups = "drop")

  p_descriptive <- ggplot(means_data, aes(x = age_group, y = mean_srh,
                                           color = period_4yr, group = period_4yr)) +
    geom_line(linewidth = 0.4) +
    geom_point(size = 0.6) +
    scale_color_viridis_d(option = "D", direction = -1, name = "Period") +
    labs(title = "Mean SRH by Age & Period", x = "Age Group", y = "Mean SRH") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "right",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )

  # --- Age Effects (Random) ---
  p_age <- ggplot(age_effects, aes(x = age_group, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90), width = 0.3, color = "#0072B2") +
    geom_point(size = 1.5, color = "#0072B2") +
    labs(title = "Age Effects (Random)", x = "Age Group", y = "Effect on SRH") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )

  # --- Period Effects ---
  p_period <- ggplot(period_effects, aes(x = factor(period), y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90), width = 0.3, color = "#009E73") +
    geom_point(size = 1.5, color = "#009E73") +
    labs(title = "Period Effects", x = "Period", y = "Effect") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))

  # --- Cohort Effects ---
  p_cohort <- ggplot(cohort_effects, aes(x = cohort, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90), width = 2, color = "#CC79A7", alpha = 0.7) +
    geom_point(size = 0.8, color = "#CC79A7") +
    labs(title = "Cohort Effects", x = "Birth Cohort", y = "Effect") +
    scale_x_continuous(breaks = seq(1920, 2000, 20)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))

  # --- Variance Decomposition Table ---
  var_table <- variance_df %>%
    filter(component != "Total") %>%
    mutate(
      Component = case_when(
        component == "age_group" ~ "Age",
        component == "period_4yr" ~ "Period",
        component == "cohort_4yr" ~ "Cohort",
        TRUE ~ "Residual"
      ),
      Var = sprintf("%.3f", variance),
      `%` = sprintf("%.1f%%", pct_of_total)
    ) %>%
    select(Component, Var, `%`)

  t_variance <- tableGrob(var_table, rows = NULL,
                          theme = ttheme_minimal(base_size = 10,
                                                 core = list(fg_params = list(hjust = 0, x = 0.05),
                                                             bg_params = list(fill = "#e8e8e8")),
                                                 colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold"),
                                                                bg_params = list(fill = "#d0d0d0"))))

  p_t_variance <- ggplot() +
    annotation_custom(t_variance) +
    labs(title = "Variance Decomposition") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0))

  # --- Variance Bar Chart ---
  var_plot_data <- variance_df %>%
    filter(component != "Total", component != "Residual") %>%
    mutate(
      Component = case_when(
        component == "age_group" ~ "Age",
        component == "period_4yr" ~ "Period",
        component == "cohort_4yr" ~ "Cohort"
      ),
      Component = factor(Component, levels = c("Age", "Period", "Cohort"))
    )

  p_var_bar <- ggplot(var_plot_data, aes(x = Component, y = pct_of_total, fill = Component)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(round(pct_of_total, 1), "%")), vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("Age" = "#0072B2", "Period" = "#009E73", "Cohort" = "#CC79A7")) +
    labs(title = "Variance by APC", x = "", y = "% of Total") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "none"
    ) +
    ylim(0, max(var_plot_data$pct_of_total, na.rm = TRUE) * 1.2)

  # --- Combine ---
  layout <- "
AABBCCCCDDDD
AABBCCCCDDDD
EEEEFFFFGGGG
EEEEFFFFGGGG
"

  combined <- p_t_variance + p_var_bar + p_lexis + p_descriptive +
    p_age + p_period + p_cohort +
    plot_layout(design = layout) +
    plot_annotation(
      title = paste0(toupper(survey_name), " BHAPC Full Random Effects Analysis"),
      subtitle = paste0("Model: srh ~ lnWt + (1|age_group) + (1|period_4yr) + (1|cohort_4yr)  |  N = ",
                        format(n_obs, big.mark = ","), "  |  adapt_delta=0.95, iter=5000"),
      caption = "Higher SRH = better health. Error bars show 90% CIs. All APC components treated as random effects.",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 10, color = "gray50")
      )
    ) &
    theme(plot.margin = margin(2, 2, 2, 2))

  # Save
  png_path <- file.path(output_dir, paste0(survey_name, "_bhapc_full_random_summary.png"))
  ggsave(png_path, combined, width = 16, height = 10, dpi = 300)

  pdf_path <- file.path(output_dir, paste0(survey_name, "_bhapc_full_random_summary.pdf"))
  ggsave(pdf_path, combined, width = 16, height = 10)
}


# ==============================================================================
# Main Pipeline Function
# ==============================================================================

run_survey <- function(survey_name) {

  config <- SURVEY_CONFIG[[survey_name]]
  survey_output_dir <- file.path(output_dir, survey_name)
  dir.create(survey_output_dir, recursive = TRUE, showWarnings = FALSE)

  log_file <- file.path(survey_output_dir, paste0(survey_name, "_log.txt"))

  # Clear previous log
  cat("", file = log_file)

  log_msg <- function(...) {
    msg <- paste0(...)
    cat(msg, "\n")
    cat(msg, "\n", file = log_file, append = TRUE)
  }

  log_msg("\n", strrep("=", 70))
  log_msg("BHAPC FULL RANDOM (FAST): ", toupper(survey_name))
  log_msg("Started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  log_msg("Settings: adapt_delta=", ADAPT_DELTA, ", iter=", ITER, ", cores=", N_CORES)
  log_msg(strrep("=", 70))

  result <- tryCatch({

    # =========================================================================
    # STEP 1: Load and prepare data
    # =========================================================================

    log_msg("\n--- STEP 1: DATA LOADING & PREPARATION ---")

    data_result <- load_survey_data(survey_name)
    df <- data_result$data

    log_msg("  Original N: ", format(data_result$original_n, big.mark = ","))
    log_msg("  Final N: ", format(data_result$final_n, big.mark = ","))

    bhapc_df <- prepare_bhapc_data(
      df,
      survey = survey_name,
      age_min = config$age_min,
      age_max = config$age_max,
      srh_scale = config$srh_scale
    )

    log_msg("  BHAPC N: ", format(nrow(bhapc_df), big.mark = ","))
    log_msg("  Age groups: ", length(unique(bhapc_df$age_group)))
    log_msg("  Periods: ", paste(unique(bhapc_df$period_4yr), collapse = ", "))
    log_msg("  Cohorts: ", length(unique(bhapc_df$cohort_4yr)))

    saveRDS(bhapc_df, file.path(survey_output_dir, paste0(survey_name, "_bhapc_data.rds")))

    # =========================================================================
    # STEP 2: Fit model with FAST settings
    # =========================================================================

    log_msg("\n--- STEP 2: FULL RANDOM EFFECTS BHAPC MODEL (FAST) ---")
    log_msg("  Formula: srh ~ lnWt + (1|age_group) + (1|period_4yr) + (1|cohort_4yr)")
    log_msg("  adapt_delta = ", ADAPT_DELTA)
    log_msg("  iter = ", ITER)
    log_msg("  cores = ", N_CORES)

    model_start <- Sys.time()

    # Use the existing function but override settings
    model_result <- fit_bhapc_full_random(
      bhapc_df,
      outcome = "srh",
      adapt_delta = ADAPT_DELTA,
      iter = ITER,
      chains = 4,
      cores = N_CORES,
      seed = 20260210
    )

    model_elapsed <- difftime(Sys.time(), model_start, units = "mins")
    log_msg("  Model completed in ", round(model_elapsed, 1), " minutes")

    saveRDS(model_result, file.path(survey_output_dir, paste0(survey_name, "_bhapc_full_random_model.rds")))

    # Variance decomposition
    variance_df <- extract_variance_components(model_result$model)
    write.csv(variance_df, file.path(survey_output_dir, paste0(survey_name, "_variance_decomposition.csv")), row.names = FALSE)

    log_msg("\n  Variance Decomposition:")
    for (i in 1:nrow(variance_df)) {
      log_msg("    ", variance_df$component[i], ": ",
              round(variance_df$variance[i], 4), " (",
              round(variance_df$pct_of_total[i], 1), "%)")
    }

    # =========================================================================
    # STEP 3: Extract random effects
    # =========================================================================

    log_msg("\n--- STEP 3: EXTRACTING RANDOM EFFECTS ---")

    age_effects <- extract_age_group_effects(model_result$model)
    write.csv(age_effects, file.path(survey_output_dir, paste0(survey_name, "_age_effects.csv")), row.names = FALSE)
    log_msg("  Saved age group effects (", nrow(age_effects), " groups)")

    random_effects <- extract_random_effects(model_result$model, bhapc_df)
    write.csv(random_effects$period_effects, file.path(survey_output_dir, paste0(survey_name, "_period_effects.csv")), row.names = FALSE)
    write.csv(random_effects$cohort_effects, file.path(survey_output_dir, paste0(survey_name, "_cohort_effects.csv")), row.names = FALSE)
    log_msg("  Saved period effects (", nrow(random_effects$period_effects), " periods)")
    log_msg("  Saved cohort effects (", nrow(random_effects$cohort_effects), " cohorts)")

    # =========================================================================
    # STEP 4: Generate figures
    # =========================================================================

    log_msg("\n--- STEP 4: GENERATING FIGURES ---")

    create_full_random_summary_figure(
      survey_name = survey_name,
      bhapc_df = bhapc_df,
      model_result = model_result,
      variance_df = variance_df,
      age_effects = age_effects,
      period_effects = random_effects$period_effects,
      cohort_effects = random_effects$cohort_effects,
      output_dir = survey_output_dir
    )

    log_msg("  Saved summary figure")

    # =========================================================================
    # STEP 5: Diagnostics
    # =========================================================================

    log_msg("\n--- STEP 5: DIAGNOSTICS ---")
    log_msg("  Max Rhat: ", round(max(model_result$diagnostics$Rhat, na.rm = TRUE), 3))
    log_msg("  Min n_eff: ", round(min(model_result$diagnostics$n_eff, na.rm = TRUE), 0))
    log_msg("  Converged: ", max(model_result$diagnostics$Rhat, na.rm = TRUE) < 1.01)

    write.csv(model_result$diagnostics,
              file.path(survey_output_dir, paste0(survey_name, "_diagnostics.csv")),
              row.names = FALSE)

    total_elapsed <- difftime(Sys.time(), model_start, units = "mins")

    log_msg("\n", strrep("=", 70))
    log_msg("COMPLETE: ", toupper(survey_name))
    log_msg("Total time: ", round(total_elapsed, 1), " minutes")
    log_msg("Finished at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    log_msg(strrep("=", 70))

    list(
      status = "success",
      survey = survey_name,
      n_obs = nrow(bhapc_df),
      elapsed_minutes = as.numeric(total_elapsed),
      max_rhat = max(model_result$diagnostics$Rhat, na.rm = TRUE),
      min_neff = min(model_result$diagnostics$n_eff, na.rm = TRUE),
      converged = max(model_result$diagnostics$Rhat, na.rm = TRUE) < 1.01,
      variance_age_pct = variance_df$pct_of_total[variance_df$component == "age_group"],
      variance_period_pct = variance_df$pct_of_total[variance_df$component == "period_4yr"],
      variance_cohort_pct = variance_df$pct_of_total[variance_df$component == "cohort_4yr"]
    )

  }, error = function(e) {
    log_msg("\nERROR: ", e$message)
    list(
      status = "error",
      survey = survey_name,
      error = e$message,
      elapsed_minutes = NA
    )
  })

  return(result)
}


# ==============================================================================
# Main Execution - SEQUENTIAL
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("RUNNING SURVEYS SEQUENTIALLY (all", N_CORES, "cores per survey)\n")
cat("Surveys:", paste(SURVEYS_TO_RUN, collapse = ", "), "\n")
cat(strrep("=", 80), "\n\n")

start_time <- Sys.time()
results <- list()

for (survey in SURVEYS_TO_RUN) {
  cat("\n>>> Starting", toupper(survey), "at", format(Sys.time(), "%H:%M:%S"), "<<<\n\n")
  results[[survey]] <- run_survey(survey)
  cat("\n>>> Finished", toupper(survey), "<<<\n")
}

total_elapsed <- difftime(Sys.time(), start_time, units = "hours")

# ==============================================================================
# Final Summary
# ==============================================================================

cat("\n\n", strrep("=", 80), "\n")
cat("ALL SURVEYS COMPLETE\n")
cat("Total time: ", round(as.numeric(total_elapsed), 2), " hours\n")
cat(strrep("=", 80), "\n\n")

# Build the new GSS summary row
summary_new <- map_dfr(results, function(r) {
  tibble(
    survey = r$survey,
    status = r$status,
    n_obs = r$n_obs %||% NA,
    elapsed_min = round(r$elapsed_minutes %||% NA, 1),
    max_rhat = round(r$max_rhat %||% NA, 3),
    min_neff = round(r$min_neff %||% NA, 0),
    converged = r$converged %||% NA,
    age_var_pct = round(r$variance_age_pct %||% NA, 1),
    period_var_pct = round(r$variance_period_pct %||% NA, 1),
    cohort_var_pct = round(r$variance_cohort_pct %||% NA, 1)
  )
})

# Update the existing all-surveys summary in place (replace the gss row)
summary_path <- file.path(output_dir, "all_surveys_summary.csv")
if (file.exists(summary_path)) {
  summary_df <- read.csv(summary_path) %>%
    filter(!survey %in% summary_new$survey) %>%
    bind_rows(summary_new)
} else {
  summary_df <- summary_new
}

cat("Summary:\n")
print(summary_df, n = Inf)

write.csv(summary_df, summary_path, row.names = FALSE)

# Same in-place update for the results list
results_path <- file.path(output_dir, "all_surveys_results.rds")
all_results <- if (file.exists(results_path)) readRDS(results_path) else list()
for (s in names(results)) all_results[[s]] <- results[[s]]
saveRDS(all_results, results_path)

cat("\n\nOutputs saved to:", output_dir, "\n")

# ==============================================================================
# Cross-Survey Comparison Figures
# ==============================================================================

cat("\n--- Creating Cross-Survey Comparison Figures ---\n")

ALL_SURVEYS <- c("nhanes", "gss", "meps", "nhis", "cps", "brfss")

all_variance <- map_dfr(ALL_SURVEYS, function(s) {
  var_path <- file.path(output_dir, s, paste0(s, "_variance_decomposition.csv"))
  if (file.exists(var_path)) {
    df <- read.csv(var_path)
    df$survey <- toupper(s)
    df
  } else {
    NULL
  }
})

if (nrow(all_variance) > 0) {
  var_plot_data <- all_variance %>%
    filter(component %in% c("age_group", "period_4yr", "cohort_4yr")) %>%
    mutate(
      component = case_when(
        component == "age_group" ~ "Age",
        component == "period_4yr" ~ "Period",
        component == "cohort_4yr" ~ "Cohort"
      ),
      component = factor(component, levels = c("Age", "Period", "Cohort"))
    )

  p_var_comparison <- ggplot(var_plot_data, aes(x = survey, y = pct_of_total, fill = component)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = paste0(round(pct_of_total, 1), "%")),
              position = position_dodge(width = 0.7), vjust = -0.3, size = 2.5) +
    scale_fill_manual(values = c("Age" = "#0072B2", "Period" = "#009E73", "Cohort" = "#CC79A7")) +
    labs(
      title = "Variance Explained by Age, Period, and Cohort Effects",
      subtitle = "Full random effects BHAPC model across all 6 surveys",
      x = "", y = "% of Total Variance", fill = "Component"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    ylim(0, max(var_plot_data$pct_of_total, na.rm = TRUE) * 1.15)

  ggsave(file.path(output_dir, "variance_comparison_all_surveys.png"),
         p_var_comparison, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "variance_comparison_all_surveys.pdf"),
         p_var_comparison, width = 12, height = 6)

  cat("Saved: variance_comparison_all_surveys.png\n")
}

# Age effects comparison
all_age_effects <- map_dfr(ALL_SURVEYS, function(s) {
  age_path <- file.path(output_dir, s, paste0(s, "_age_effects.csv"))
  if (file.exists(age_path)) {
    df <- read.csv(age_path)
    df$survey <- toupper(s)
    df
  } else {
    NULL
  }
})

if (nrow(all_age_effects) > 0) {
  age_levels <- c("18-21", "22-25", "26-29", "30-34", "35-39", "40-44",
                  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                  "75-79", "80-84", "85-89")

  all_age_effects <- all_age_effects %>%
    mutate(age_group = factor(age_group, levels = age_levels))

  p_age_comparison <- ggplot(all_age_effects, aes(x = age_group, y = estimate, color = survey, group = survey)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(linewidth = 0.5, alpha = 0.7) +
    geom_point(size = 1, alpha = 0.7) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = "Age Effects Across All Surveys",
      subtitle = "Full random effects BHAPC model: (1|age_group)",
      x = "Age Group", y = "Effect on SRH", color = "Survey"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  ggsave(file.path(output_dir, "age_effects_comparison_all_surveys.png"),
         p_age_comparison, width = 12, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "age_effects_comparison_all_surveys.pdf"),
         p_age_comparison, width = 12, height = 6)

  cat("Saved: age_effects_comparison_all_surveys.png\n")
}

cat("\n", strrep("=", 80), "\n")
cat("BHAPC FULL RANDOM EFFECTS (FAST) COMPLETE\n")
cat("Finished at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n")
