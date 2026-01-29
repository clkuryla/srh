# ==============================================================================
# 10_bhapc_parallel_all_surveys.R
# Parallelized BHAPC Analysis for All 6 Surveys
# Author: Christine Lucille Kuryla
#
# Purpose: Run BHAPC analysis on all 6 surveys in parallel, optimized for
# 64-core cloud machines. Uses stratified subsampling for large datasets
# (NHIS, CPS, BRFSS, MEPS) to target 200k observations each.
#
# Usage (run with screen):
#   screen -S bhapc
#   Rscript R/scripts/10_bhapc_parallel_all_surveys.R 2>&1 | tee output/bhapc_parallel.log
#   # Ctrl+A, D to detach
#   # screen -r bhapc to reattach
#
# Expected runtime: Several hours on 64-core machine
# ==============================================================================

# ==============================================================================
# Setup and Configuration
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BHAPC PARALLEL PIPELINE - ALL 6 SURVEYS\n")
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
  library(future)
  library(furrr)
})

# Source paths and functions
source(here("R", "paths.R"))
source(here("R", "functions", "subsample_survey.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_table_generation.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

# Null-coalescing operator (in case rlang not loaded)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Override DATA_DEPOT if not set
if (Sys.getenv("DATA_DEPOT") == "") {
  Sys.setenv(DATA_DEPOT = "/home/ubuntu/data_depot")
}

# Create output directory
output_dir <- here("output", "bhapc_parallel")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260129)

# ==============================================================================
# Configuration
# ==============================================================================

# Number of cores available
N_CORES <- parallel::detectCores()
cat("Detected cores:", N_CORES, "\n")

# Surveys to run
ALL_SURVEYS <- c("nhanes", "gss", "meps", "nhis", "cps", "brfss")

# Subsampling configuration: target 200k for large surveys
SUBSAMPLE_TARGET <- 200000
SUBSAMPLE_SURVEYS <- c("meps", "nhis", "cps", "brfss")  # Surveys that need subsampling

# Survey-specific configuration
SURVEY_CONFIG <- list(
  nhanes = list(
    srh_scale = 5, age_min = 18, age_max = 80,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "80-84",
    interaction_iter = 2000, interaction_adapt_delta = 0.95
  ),
  gss = list(
    srh_scale = 4, age_min = 18, age_max = 89,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "85-89",
    interaction_iter = 2000, interaction_adapt_delta = 0.95
  ),
  meps = list(
    srh_scale = 5, age_min = 18, age_max = 85,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "85-89",
    interaction_iter = 2000, interaction_adapt_delta = 0.95
  ),
  nhis = list(
    srh_scale = 5, age_min = 18, age_max = 85,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "85-89",
    interaction_iter = 2000, interaction_adapt_delta = 0.95
  ),
  brfss = list(
    srh_scale = 5, age_min = 18, age_max = 89,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "80-84",
    interaction_iter = 2000, interaction_adapt_delta = 0.95
  ),
  cps = list(
    srh_scale = 5, age_min = 18, age_max = 85,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "80-84",
    interaction_iter = 2000, interaction_adapt_delta = 0.95
  )
)

# Cores per survey (for Stan parallelization)
# With 6 surveys running in parallel, give each ~10 cores
CORES_PER_SURVEY <- floor(N_CORES / 6)
cat("Cores per survey:", CORES_PER_SURVEY, "\n\n")

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Load and optionally subsample survey data
load_survey_data <- function(survey_name) {
  # Data path using essential datasets
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

  # Subsample if needed
  if (survey_name %in% SUBSAMPLE_SURVEYS && nrow(df) > SUBSAMPLE_TARGET) {
    cat("  Subsampling", toupper(survey_name), "from",
        format(nrow(df), big.mark = ","), "to",
        format(SUBSAMPLE_TARGET, big.mark = ","), "...\n")

    sub_result <- subsample_survey(
      df,
      target_n = SUBSAMPLE_TARGET,
      seed = 20260129 + which(SUBSAMPLE_SURVEYS == survey_name)
    )

    df <- sub_result$data %>%
      select(-wt) %>%
      rename(wt = wt_sub)

    cat("  Subsampled to:", format(nrow(df), big.mark = ","), "rows\n")
  }

  list(data = df, original_n = original_n, final_n = nrow(df))
}

#' Run full BHAPC pipeline for a single survey
run_survey_pipeline <- function(survey_name) {

  config <- SURVEY_CONFIG[[survey_name]]
  survey_output_dir <- file.path(output_dir, survey_name)
  dir.create(survey_output_dir, recursive = TRUE, showWarnings = FALSE)

  # Log file for this survey
  log_file <- file.path(survey_output_dir, paste0(survey_name, "_log.txt"))

  # Helper to log messages
  log_msg <- function(...) {
    msg <- paste0(...)
    cat(msg, "\n")
    cat(msg, "\n", file = log_file, append = TRUE)
  }

  log_msg("\n", strrep("=", 70))
  log_msg("BHAPC PIPELINE: ", toupper(survey_name))
  log_msg("Started at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
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

    # Prepare BHAPC data
    bhapc_df <- prepare_bhapc_data(
      df,
      survey = survey_name,
      age_min = config$age_min,
      age_max = config$age_max,
      srh_scale = config$srh_scale
    )

    log_msg("  BHAPC N: ", format(nrow(bhapc_df), big.mark = ","))
    log_msg("  Periods: ", paste(unique(bhapc_df$period_4yr), collapse = ", "))
    log_msg("  Cohorts: ", length(unique(bhapc_df$cohort_4yr)))

    # Save prepared data
    saveRDS(bhapc_df, file.path(survey_output_dir, paste0(survey_name, "_bhapc_data.rds")))

    # =========================================================================
    # STEP 2: Main BHAPC model
    # =========================================================================

    log_msg("\n--- STEP 2: MAIN BHAPC MODEL ---")
    log_msg("  Formula: srh ~ age + scale(age_squared) + lnWt + (1|period_4yr) + (1|cohort_4yr)")
    log_msg("  iter = ", config$iter, ", adapt_delta = ", config$adapt_delta)
    log_msg("  cores = ", CORES_PER_SURVEY)

    model_start <- Sys.time()

    model_result <- fit_bhapc_model(
      bhapc_df,
      outcome = "srh",
      adapt_delta = config$adapt_delta,
      iter = config$iter,
      chains = 4,
      cores = CORES_PER_SURVEY,
      seed = 20260129
    )

    model_elapsed <- difftime(Sys.time(), model_start, units = "mins")
    log_msg("  Model completed in ", round(model_elapsed, 1), " minutes")

    # Save model
    saveRDS(model_result, file.path(survey_output_dir, paste0(survey_name, "_bhapc_model.rds")))

    # Extract and save variance decomposition
    variance_df <- extract_variance_components(model_result$model)
    write.csv(variance_df, file.path(survey_output_dir, paste0(survey_name, "_variance_decomposition.csv")), row.names = FALSE)

    log_msg("\n  Variance Decomposition:")
    for (i in 1:nrow(variance_df)) {
      log_msg("    ", variance_df$component[i], ": ",
              round(variance_df$variance[i], 4), " (",
              round(variance_df$pct_of_total[i], 1), "%)")
    }

    # Create Table 2
    table2 <- create_table2(
      model_result$model,
      bhapc_df,
      survey = survey_name,
      output_path = file.path(survey_output_dir, paste0("table2_", survey_name, ".csv"))
    )

    # =========================================================================
    # STEP 3: Interaction model (Age × Period)
    # =========================================================================

    log_msg("\n--- STEP 3: INTERACTION MODEL (AGE × PERIOD) ---")

    # Compute descriptive gradients
    age_groups <- sort(unique(bhapc_df$age_group))
    youngest_group <- age_groups[1]
    oldest_group <- config$oldest_age_group
    if (!(oldest_group %in% age_groups)) {
      oldest_group <- age_groups[length(age_groups)]
    }

    age_period_means <- bhapc_df %>%
      group_by(period_4yr, age_group) %>%
      summarise(
        mean_srh = weighted.mean(srh, wt, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )

    gradient_by_period <- age_period_means %>%
      group_by(period_4yr) %>%
      summarise(
        srh_young = mean_srh[age_group == youngest_group],
        srh_old = mean_srh[age_group == oldest_group],
        gradient = srh_young - srh_old,
        n_total = sum(n),
        .groups = "drop"
      ) %>%
      arrange(period_4yr)

    write.csv(gradient_by_period,
              file.path(survey_output_dir, paste0("gradient_by_period_", survey_name, ".csv")),
              row.names = FALSE)

    # Fit interaction model
    mean_age <- mean(bhapc_df$age)
    bhapc_df <- bhapc_df %>%
      mutate(age_centered = age - mean_age)

    log_msg("  Formula: srh ~ age_centered * period_4yr + lnWt + (1|cohort_4yr)")
    log_msg("  iter = ", config$interaction_iter, ", adapt_delta = ", config$interaction_adapt_delta)

    interaction_start <- Sys.time()

    interaction_model <- rstanarm::stan_lmer(
      srh ~ age_centered * period_4yr + lnWt + (1|cohort_4yr),
      data = bhapc_df,
      adapt_delta = config$interaction_adapt_delta,
      iter = config$interaction_iter,
      chains = 4,
      cores = CORES_PER_SURVEY,
      seed = 20260129
    )

    interaction_elapsed <- difftime(Sys.time(), interaction_start, units = "mins")
    log_msg("  Interaction model completed in ", round(interaction_elapsed, 1), " minutes")

    # Save interaction model
    saveRDS(interaction_model, file.path(survey_output_dir, paste0(survey_name, "_interaction_model.rds")))

    # Extract interaction effects
    posterior <- as.matrix(interaction_model)
    interaction_cols <- grep("age_centered:period_4yr", colnames(posterior), value = TRUE)

    age_main <- mean(posterior[, "age_centered"])
    age_main_ci <- quantile(posterior[, "age_centered"], c(0.10, 0.90))

    if (length(interaction_cols) > 0) {
      interaction_effects <- data.frame(
        period = gsub("age_centered:period_4yr", "", interaction_cols),
        estimate = sapply(interaction_cols, function(col) mean(posterior[, col])),
        ci_10 = sapply(interaction_cols, function(col) quantile(posterior[, col], 0.10)),
        ci_90 = sapply(interaction_cols, function(col) quantile(posterior[, col], 0.90)),
        stringsAsFactors = FALSE
      )
      interaction_effects$significant <- ifelse(
        interaction_effects$ci_10 > 0 | interaction_effects$ci_90 < 0, "*", ""
      )

      ref_period <- as.character(min(as.numeric(unique(bhapc_df$period_4yr))))

      interaction_summary <- data.frame(
        period = c(ref_period, interaction_effects$period),
        age_slope = c(age_main, age_main + interaction_effects$estimate),
        interaction = c(0, interaction_effects$estimate),
        ci_10 = c(age_main_ci[1], interaction_effects$ci_10),
        ci_90 = c(age_main_ci[2], interaction_effects$ci_90),
        significant = c("ref", interaction_effects$significant)
      )
    } else {
      ref_period <- as.character(min(as.numeric(unique(bhapc_df$period_4yr))))
      interaction_summary <- data.frame(
        period = ref_period,
        age_slope = age_main,
        interaction = 0,
        ci_10 = age_main_ci[1],
        ci_90 = age_main_ci[2],
        significant = "ref"
      )
    }

    write.csv(interaction_summary,
              file.path(survey_output_dir, paste0("age_period_interaction_", survey_name, ".csv")),
              row.names = FALSE)

    # =========================================================================
    # STEP 4: Generate figures
    # =========================================================================

    log_msg("\n--- STEP 4: GENERATING FIGURES ---")

    # Figure 2: Descriptive means
    fig2 <- create_figure2_descriptive(
      bhapc_df,
      survey = survey_name,
      output_path = file.path(survey_output_dir, paste0("figure2_", survey_name, ".png"))
    )

    # Figure 3: APC effects
    fig3 <- create_figure3_apc_effects(
      model_result$model,
      bhapc_df,
      survey = survey_name,
      output_path = file.path(survey_output_dir, paste0("figure3_", survey_name, ".png"))
    )

    # Variance chart
    var_chart <- create_variance_chart(variance_df, survey = survey_name)
    ggsave(file.path(survey_output_dir, paste0("variance_chart_", survey_name, ".png")),
           var_chart, width = 6, height = 5, dpi = 300)

    # Create summary figure (from 09_bhapc_full_pipeline.R)
    create_bhapc_summary_figure_local(
      survey_name = survey_name,
      bhapc_df = bhapc_df,
      model_result = model_result,
      variance_df = variance_df,
      gradient_df = gradient_by_period,
      interaction_df = interaction_summary,
      table2 = table2,
      output_dir = survey_output_dir
    )

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
    log_msg("PIPELINE COMPLETE: ", toupper(survey_name))
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


#' Create BHAPC summary figure (local copy for parallel execution)
create_bhapc_summary_figure_local <- function(survey_name, bhapc_df, model_result,
                                               variance_df, gradient_df, interaction_df,
                                               table2, output_dir) {

  model <- model_result$model
  posterior <- as.matrix(model)
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

  # --- Age Slope by Period (interaction) ---
  p_interaction <- ggplot(interaction_df, aes(x = factor(period), y = age_slope)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(aes(group = 1), color = "#D55E00", linewidth = 0.5) +
    geom_point(size = 1.5, color = "#D55E00") +
    geom_errorbar(aes(ymin = ci_10, ymax = ci_90), width = 0.2, color = "#D55E00") +
    labs(title = "Age Slope by Period", x = "Period", y = "Age slope") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))

  # --- Age Effect ---
  age_sq_mean <- mean(bhapc_df$age_squared)
  age_sq_sd <- sd(bhapc_df$age_squared)
  ages <- seq(min(bhapc_df$age), max(bhapc_df$age), by = 1)

  age_effect_samples <- sapply(ages, function(a) {
    linear <- posterior[, "age"] * (a - min(bhapc_df$age))
    scaled_sq <- (a^2 - age_sq_mean) / age_sq_sd
    scaled_sq_ref <- (min(bhapc_df$age)^2 - age_sq_mean) / age_sq_sd
    quadratic <- posterior[, "scale(age_squared)"] * (scaled_sq - scaled_sq_ref)
    linear + quadratic
  })

  age_df <- data.frame(
    age = ages,
    effect = colMeans(age_effect_samples),
    ci_lower = apply(age_effect_samples, 2, quantile, 0.05),
    ci_upper = apply(age_effect_samples, 2, quantile, 0.95)
  )

  p_age <- ggplot(age_df, aes(x = age, y = effect)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "#0072B2", alpha = 0.2) +
    geom_line(color = "#0072B2", linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(title = "Age Effect", x = "Age", y = "Effect on SRH") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))

  # --- Period Effects ---
  period_cols <- grep("b\\[\\(Intercept\\) period_4yr:", colnames(posterior), value = TRUE)
  if (length(period_cols) > 0) {
    period_names <- gsub(".*period_4yr:([0-9]+)\\]", "\\1", period_cols)
    period_effects <- data.frame(
      period = as.numeric(period_names),
      estimate = sapply(period_cols, function(col) mean(posterior[, col])),
      ci_lower = sapply(period_cols, function(col) quantile(posterior[, col], 0.05)),
      ci_upper = sapply(period_cols, function(col) quantile(posterior[, col], 0.95))
    )
  } else {
    period_effects <- data.frame(period = NA, estimate = NA, ci_lower = NA, ci_upper = NA)
  }

  p_period <- ggplot(period_effects, aes(x = factor(period), y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, color = "#009E73") +
    geom_point(size = 1.5, color = "#009E73") +
    labs(title = "Period Effects", x = "Period", y = "Effect") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))

  # --- Cohort Effects ---
  cohort_cols <- grep("b\\[\\(Intercept\\) cohort_4yr:", colnames(posterior), value = TRUE)
  if (length(cohort_cols) > 0) {
    cohort_names <- as.numeric(gsub(".*cohort_4yr:([0-9-]+)\\]", "\\1", cohort_cols))
    cohort_effects <- data.frame(
      cohort = cohort_names,
      estimate = sapply(cohort_cols, function(col) mean(posterior[, col])),
      ci_lower = sapply(cohort_cols, function(col) quantile(posterior[, col], 0.05)),
      ci_upper = sapply(cohort_cols, function(col) quantile(posterior[, col], 0.95))
    ) %>% arrange(cohort)
  } else {
    cohort_effects <- data.frame(cohort = NA, estimate = NA, ci_lower = NA, ci_upper = NA)
  }

  p_cohort <- ggplot(cohort_effects, aes(x = cohort, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 2, color = "#CC79A7", alpha = 0.7) +
    geom_point(size = 0.8, color = "#CC79A7") +
    labs(title = "Cohort Effects", x = "Birth Cohort", y = "Effect") +
    scale_x_continuous(breaks = seq(1920, 2000, 20)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))

  # --- Tables ---

  # Variance Decomposition
  var_table <- variance_df %>%
    filter(component != "Total") %>%
    mutate(
      Component = case_when(
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

  # Interaction table
  int_table <- interaction_df %>%
    mutate(
      Period = as.character(period),
      Slope = sprintf("%.4f", age_slope),
      `vs ref` = sprintf("%+.4f", interaction),
      ` ` = ifelse(significant == "*", "***", significant)
    ) %>%
    select(Period, Slope, `vs ref`, ` `)

  t_interaction <- tableGrob(int_table, rows = NULL,
                             theme = ttheme_minimal(base_size = 10,
                                                    core = list(fg_params = list(hjust = 0, x = 0.05),
                                                                bg_params = list(fill = "#f0f0f0")),
                                                    colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold"),
                                                                   bg_params = list(fill = "#d8d8d8"))))

  # Wrap tables
  wrap_table <- function(grob, title, valign = "center") {
    if (valign == "top") {
      p <- ggplot() +
        annotation_custom(grob, ymin = 0.5, ymax = 1) +
        labs(title = title) +
        theme_void() +
        theme(plot.title = element_text(face = "bold", size = 12, hjust = 0)) +
        coord_cartesian(clip = "off") +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
    } else {
      p <- ggplot() +
        annotation_custom(grob) +
        labs(title = title) +
        theme_void() +
        theme(plot.title = element_text(face = "bold", size = 12, hjust = 0))
    }
    p
  }

  p_t_variance <- wrap_table(t_variance, "Variance Decomposition")
  p_t_interaction <- wrap_table(t_interaction, "Age x Period Interaction", valign = "top")

  # --- Combine ---
  # A=p_t_variance, B=p_age, C=p_period, D=p_cohort
  # E=p_lexis, F=p_descriptive, G=p_t_interaction, H=p_interaction
  layout <- "
AADDDEEEFFGG
AADDDEEEFFGG
BBDDDEEEFFHH
BBDDDEEEFFHH
CCDDDEEEFFHH
CCDDDEEEFFHH
"

  combined <- p_t_variance + p_age + p_period + p_cohort +
    p_lexis + p_descriptive + p_t_interaction + p_interaction +
    plot_layout(design = layout) +
    plot_annotation(
      title = paste0(toupper(survey_name), " BHAPC Analysis Summary"),
      subtitle = paste0("Bayesian Hierarchical Age-Period-Cohort decomposition (N = ",
                        format(n_obs, big.mark = ","), ")"),
      caption = "Higher SRH = better health. Error bars/ribbons show 90% CIs. *** = significant (CI excludes 0).",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 10, color = "gray50")
      )
    ) &
    theme(plot.margin = margin(2, 2, 2, 2))

  # Save
  png_path <- file.path(output_dir, paste0(survey_name, "_bhapc_summary.png"))
  ggsave(png_path, combined, width = 16, height = 10, dpi = 300)

  pdf_path <- file.path(output_dir, paste0(survey_name, "_bhapc_summary.pdf"))
  ggsave(pdf_path, combined, width = 16, height = 10)
}


# ==============================================================================
# Main Execution
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("STARTING PARALLEL EXECUTION\n")
cat("Running", length(ALL_SURVEYS), "surveys in parallel using future/furrr\n")
cat(strrep("=", 80), "\n\n")

# Configure future for parallel execution
# Use multisession to run surveys in parallel
plan(multisession, workers = 6)

# Run all surveys in parallel
start_time <- Sys.time()

results <- future_map(
  ALL_SURVEYS,
  run_survey_pipeline,
  .options = furrr_options(
    seed = TRUE,
    globals = c("SURVEY_CONFIG", "SUBSAMPLE_TARGET", "SUBSAMPLE_SURVEYS",
                "CORES_PER_SURVEY", "output_dir", "load_survey_data",
                "create_bhapc_summary_figure_local")
  ),
  .progress = TRUE
)

names(results) <- ALL_SURVEYS

total_elapsed <- difftime(Sys.time(), start_time, units = "hours")

# ==============================================================================
# Final Summary
# ==============================================================================

cat("\n\n", strrep("=", 80), "\n")
cat("ALL SURVEYS COMPLETE\n")
cat("Total time: ", round(as.numeric(total_elapsed), 2), " hours\n")
cat(strrep("=", 80), "\n\n")

# Create summary table
summary_df <- map_dfr(results, function(r) {
  tibble(
    survey = r$survey,
    status = r$status,
    n_obs = r$n_obs %||% NA,
    elapsed_min = round(r$elapsed_minutes %||% NA, 1),
    max_rhat = round(r$max_rhat %||% NA, 3),
    min_neff = round(r$min_neff %||% NA, 0),
    converged = r$converged %||% NA,
    period_var_pct = round(r$variance_period_pct %||% NA, 1),
    cohort_var_pct = round(r$variance_cohort_pct %||% NA, 1)
  )
})

cat("Summary:\n")
print(summary_df, n = Inf)

# Save summary
write.csv(summary_df, file.path(output_dir, "all_surveys_summary.csv"), row.names = FALSE)

# Save full results object
saveRDS(results, file.path(output_dir, "all_surveys_results.rds"))

cat("\n\nOutputs saved to:", output_dir, "\n")
cat("Check individual survey subdirectories for detailed outputs.\n")

# ==============================================================================
# Cross-Survey Comparison Figures
# ==============================================================================

cat("\n--- Creating Cross-Survey Comparison Figures ---\n")

# Load all variance decompositions
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
  # Combined variance comparison
  var_plot_data <- all_variance %>%
    filter(component %in% c("period_4yr", "cohort_4yr")) %>%
    mutate(
      component = case_when(
        component == "period_4yr" ~ "Period",
        component == "cohort_4yr" ~ "Cohort"
      )
    )

  p_var_comparison <- ggplot(var_plot_data, aes(x = survey, y = pct_of_total, fill = component)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = paste0(round(pct_of_total, 1), "%")),
              position = position_dodge(width = 0.7), vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("Period" = "#009E73", "Cohort" = "#CC79A7")) +
    labs(
      title = "Variance Explained by Period and Cohort Effects",
      subtitle = "Across all 6 surveys (BHAPC models)",
      x = "", y = "% of Total Variance", fill = "Component"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    ylim(0, max(var_plot_data$pct_of_total, na.rm = TRUE) * 1.15)

  ggsave(file.path(output_dir, "variance_comparison_all_surveys.png"),
         p_var_comparison, width = 10, height = 6, dpi = 300)
  ggsave(file.path(output_dir, "variance_comparison_all_surveys.pdf"),
         p_var_comparison, width = 10, height = 6)

  cat("Saved: variance_comparison_all_surveys.png\n")
}

cat("\n", strrep("=", 80), "\n")
cat("BHAPC PARALLEL PIPELINE COMPLETE\n")
cat("Finished at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n")

# Clean up
plan(sequential)
