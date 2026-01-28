# ==============================================================================
# 09_bhapc_full_pipeline.R
# Complete BHAPC Analysis Pipeline for Any Survey
# Author: Christine Lucille Kuryla
#
# Purpose: Single script to run full BHAPC analysis for a survey:
#   1. Data preparation
#   2. Main BHAPC model (age + period/cohort random effects)
#   3. Interaction model (age × period to test convergence)
#   4. Generate one-pager summary figure
#
# Usage:
#   Rscript 09_bhapc_full_pipeline.R nhanes
#   Rscript 09_bhapc_full_pipeline.R gss
#   ... etc for meps, nhis, cps, brfss
#
# Or source and call:
#   source("R/scripts/09_bhapc_full_pipeline.R")
#   run_full_bhapc_pipeline("nhanes")
#
# Designed for Ronin (large memory/cores for Bayesian models)
# ==============================================================================

library(tidyverse)
library(here)
library(rstanarm)
library(broom.mixed)
library(srvyr)
library(patchwork)
library(gridExtra)
library(grid)

# Source paths and functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_table_generation.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

# Create output directory
output_dir <- here("output", "apc", "bhapc")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260128)

# ==============================================================================
# Survey Configuration
# ==============================================================================

SURVEY_CONFIG <- list(
  nhanes = list(
    srh_scale = 5, age_min = 18, age_max = 80,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "80-84"
  ),
  gss = list(
    srh_scale = 4, age_min = 18, age_max = 89,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "85-89"
  ),
  meps = list(
    srh_scale = 5, age_min = 18, age_max = 85,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "85-89"
  ),
  nhis = list(
    srh_scale = 5, age_min = 18, age_max = 85,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "85-89"
  ),
  brfss = list(
    srh_scale = 5, age_min = 18, age_max = 89,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "80-84"
  ),
  cps = list(
    srh_scale = 5, age_min = 18, age_max = 85,
    iter = 6000, adapt_delta = 0.998,
    oldest_age_group = "80-84"
  )
)

# ==============================================================================
# Main Pipeline Function
# ==============================================================================

run_full_bhapc_pipeline <- function(survey_name,
                                     skip_if_exists = FALSE,
                                     interaction_iter = 2000,
                                     interaction_adapt_delta = 0.95) {

  survey_name <- tolower(survey_name)
  config <- SURVEY_CONFIG[[survey_name]]

  if (is.null(config)) {
    stop("Unknown survey: ", survey_name,
         "\nAvailable: ", paste(names(SURVEY_CONFIG), collapse = ", "))
  }

  message("\n", strrep("=", 70))
  message("FULL BHAPC PIPELINE: ", toupper(survey_name))
  message("Started at: ", Sys.time())
  message(strrep("=", 70))

  # Check if already completed
  summary_path <- here(output_dir, paste0(survey_name, "_bhapc_summary.png"))
  if (skip_if_exists && file.exists(summary_path)) {
    message("\nSummary figure already exists. Skipping. Set skip_if_exists=FALSE to re-run.")
    return(invisible(NULL))
  }

  # ============================================================================
  # STEP 1: Data Preparation
  # ============================================================================

  message("\n", strrep("-", 70))
  message("STEP 1: DATA PREPARATION")
  message(strrep("-", 70))

  # Load raw data
  data_path <- derived_path(paste0("data_", survey_name, ".rds"))
  message("\nLoading: ", data_path)

  if (!file.exists(data_path)) {
    stop("Data file not found: ", data_path)
  }

  df <- readRDS(data_path)
  message("  Raw data: ", format(nrow(df), big.mark = ","), " rows")

  # Prepare BHAPC data
  bhapc_df <- prepare_bhapc_data(
    df,
    survey = survey_name,
    age_min = config$age_min,
    age_max = config$age_max,
    srh_scale = config$srh_scale
  )

  message("  BHAPC data: ", format(nrow(bhapc_df), big.mark = ","), " rows")
  message("  Periods: ", paste(unique(bhapc_df$period_4yr), collapse = ", "))
  message("  Cohorts: ", length(unique(bhapc_df$cohort_4yr)))

  # Save prepared data
  bhapc_data_path <- here(output_dir, paste0(survey_name, "_bhapc_data.rds"))
  saveRDS(bhapc_df, bhapc_data_path)
  message("  Saved: ", bhapc_data_path)

  # ============================================================================
  # STEP 2: Main BHAPC Model
  # ============================================================================

  message("\n", strrep("-", 70))
  message("STEP 2: MAIN BHAPC MODEL")
  message(strrep("-", 70))

  message("\nFitting: srh ~ age + scale(age_squared) + lnWt + (1|period_4yr) + (1|cohort_4yr)")
  message("  iter = ", config$iter, ", adapt_delta = ", config$adapt_delta)
  message("  This will take a while...\n")

  model_start <- Sys.time()

  model_result <- fit_bhapc_model(
    bhapc_df,
    outcome = "srh",
    adapt_delta = config$adapt_delta,
    iter = config$iter
  )

  model_elapsed <- difftime(Sys.time(), model_start, units = "mins")
  message("\n  Model completed in ", round(model_elapsed, 1), " minutes")

  # Save model
  model_path <- here(output_dir, paste0(survey_name, "_bhapc_model.rds"))
  saveRDS(model_result, model_path)
  message("  Saved: ", model_path)

  # Extract and save variance decomposition
  variance_df <- extract_variance_components(model_result$model)
  variance_path <- here(output_dir, paste0(survey_name, "_variance_decomposition.csv"))
  write.csv(variance_df, variance_path, row.names = FALSE)
  message("  Saved: ", variance_path)

  message("\n  Variance Decomposition:")
  print(variance_df)

  # Create Table 2 (Gloria format)
  table2 <- create_table2(
    model_result$model,
    bhapc_df,
    survey = survey_name,
    output_path = here(output_dir, paste0("table2_", survey_name, "_gloria_format.csv"))
  )

  # ============================================================================
  # STEP 3: Interaction Model (Age × Period)
  # ============================================================================

  message("\n", strrep("-", 70))
  message("STEP 3: INTERACTION MODEL (AGE × PERIOD)")
  message(strrep("-", 70))

  # Compute descriptive gradients first
  message("\nComputing descriptive age gradients by period...")

  # Find the youngest and oldest age groups in this data
  age_groups <- sort(unique(bhapc_df$age_group))
  youngest_group <- age_groups[1]
  oldest_group <- config$oldest_age_group

  # If specified oldest group doesn't exist, use the oldest available
  if (!(oldest_group %in% age_groups)) {
    oldest_group <- age_groups[length(age_groups)]
  }

  message("  Youngest group: ", youngest_group)
  message("  Oldest group: ", oldest_group)

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

  # Rename columns for clarity
  names(gradient_by_period)[2:3] <- c(
    paste0("srh_", gsub("-", "_", youngest_group)),
    paste0("srh_", gsub("-", "_", oldest_group))
  )

  gradient_path <- here(output_dir, paste0("gradient_by_period_", survey_name, ".csv"))
  write.csv(gradient_by_period, gradient_path, row.names = FALSE)
  message("  Saved: ", gradient_path)

  # Print gradient summary
  message("\n  Gradient by Period:")
  print(gradient_by_period)

  # Center age and fit interaction model
  mean_age <- mean(bhapc_df$age)
  bhapc_df <- bhapc_df %>%
    mutate(age_centered = age - mean_age)

  message("\nFitting interaction model: srh ~ age_centered * period_4yr + lnWt + (1|cohort_4yr)")
  message("  iter = ", interaction_iter, ", adapt_delta = ", interaction_adapt_delta)

  interaction_start <- Sys.time()

  interaction_model <- rstanarm::stan_lmer(
    srh ~ age_centered * period_4yr + lnWt + (1|cohort_4yr),
    data = bhapc_df,
    adapt_delta = interaction_adapt_delta,
    iter = interaction_iter,
    chains = 4,
    cores = parallel::detectCores(),
    seed = 20260128
  )

  interaction_elapsed <- difftime(Sys.time(), interaction_start, units = "mins")
  message("\n  Interaction model completed in ", round(interaction_elapsed, 1), " minutes")

  # Save interaction model
  interaction_path <- here(output_dir, paste0(survey_name, "_interaction_model.rds"))
  saveRDS(interaction_model, interaction_path)
  message("  Saved: ", interaction_path)

  # Extract interaction effects
  posterior <- as.matrix(interaction_model)
  interaction_cols <- grep("age_centered:period_4yr", colnames(posterior), value = TRUE)

  # Get main age effect and interaction terms
  age_main <- mean(posterior[, "age_centered"])
  age_main_ci <- quantile(posterior[, "age_centered"], c(0.10, 0.90))

  # Build interaction summary
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

    # Reference period (first period)
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
    # Only one period - no interaction possible
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

  interaction_summary_path <- here(output_dir, paste0("age_period_interaction_", survey_name, ".csv"))
  write.csv(interaction_summary, interaction_summary_path, row.names = FALSE)
  message("  Saved: ", interaction_summary_path)

  message("\n  Age × Period Interaction:")
  print(interaction_summary)

  # ============================================================================
  # STEP 4: Generate Summary Figure
  # ============================================================================

  message("\n", strrep("-", 70))
  message("STEP 4: GENERATE SUMMARY FIGURE")
  message(strrep("-", 70))

  create_bhapc_summary_figure(
    survey_name = survey_name,
    bhapc_df = bhapc_df,
    model_result = model_result,
    variance_df = variance_df,
    gradient_df = gradient_by_period,
    interaction_df = interaction_summary,
    table2 = table2,
    output_dir = output_dir
  )

  # ============================================================================
  # DONE
  # ============================================================================

  total_elapsed <- difftime(Sys.time(), model_start, units = "mins")

  message("\n", strrep("=", 70))
  message("PIPELINE COMPLETE: ", toupper(survey_name))
  message("Total time: ", round(total_elapsed, 1), " minutes")
  message("Finished at: ", Sys.time())
  message(strrep("=", 70))

  invisible(list(
    survey = survey_name,
    bhapc_df = bhapc_df,
    model_result = model_result,
    variance_df = variance_df,
    gradient_df = gradient_by_period,
    interaction_summary = interaction_summary,
    table2 = table2
  ))
}


# ==============================================================================
# Summary Figure Generation Function
# ==============================================================================

create_bhapc_summary_figure <- function(survey_name, bhapc_df, model_result,
                                         variance_df, gradient_df, interaction_df,
                                         table2, output_dir) {

  message("\nCreating summary figure for ", toupper(survey_name), "...")

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

  # Gloria Table 2
  gloria_display <- table2 %>%
    mutate(
      Effect = effect,
      Est. = ifelse(is.na(estimate), "", sprintf("%.2f", estimate)),
      `10%` = ifelse(is.na(ci_10), "", sprintf("%.2f", ci_10)),
      `90%` = ifelse(is.na(ci_90), "", sprintf("%.2f", ci_90)),
      ` ` = ifelse(sig == "*", "***", "")
    ) %>%
    select(Effect, Est., `10%`, `90%`, ` `)

  t_gloria <- tableGrob(gloria_display, rows = NULL,
                        theme = ttheme_minimal(base_size = 9,
                                               core = list(fg_params = list(hjust = 0, x = 0.02),
                                                           bg_params = list(fill = "#f0f0f0")),
                                               colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold"),
                                                              bg_params = list(fill = "#d8d8d8"))))

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
  p_t_gloria <- wrap_table(t_gloria, "BHAPC Model Effects")
  p_t_interaction <- wrap_table(t_interaction, "Age×Period Interaction", valign = "top")

  # --- Combine ---
  layout <- "
AAEEEEFFFGGG
AAEEEEFFFGGG
BBEEEEFFFGGG
BBEEEEFFFHHH
CCEEEEFFFHHH
CCEEEEFFFHHH
DDEEEEFFFIII
DDEEEEFFFIII
DDEEEEFFFIII
"

  combined <- p_t_variance + p_age + p_period + p_cohort +
    p_t_gloria + p_t_interaction +
    p_lexis + p_descriptive + p_interaction +
    plot_layout(design = layout) +
    plot_annotation(
      title = paste0(toupper(survey_name), " BHAPC Analysis Summary"),
      subtitle = paste0("Bayesian Hierarchical Age-Period-Cohort decomposition of Self-Rated Health (N = ",
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
  png_path <- here(output_dir, paste0(survey_name, "_bhapc_summary.png"))
  ggsave(png_path, combined, width = 16, height = 11, dpi = 300)
  message("  Saved: ", png_path)

  pdf_path <- here(output_dir, paste0(survey_name, "_bhapc_summary.pdf"))
  ggsave(pdf_path, combined, width = 16, height = 11)
  message("  Saved: ", pdf_path)
}


# ==============================================================================
# Command Line Interface
# ==============================================================================

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    message("Usage: Rscript 09_bhapc_full_pipeline.R <survey_name>")
    message("Available surveys: ", paste(names(SURVEY_CONFIG), collapse = ", "))
    quit(status = 1)
  }

  survey <- args[1]
  run_full_bhapc_pipeline(survey)
}

message("\n09_bhapc_full_pipeline.R loaded")
message("Run: run_full_bhapc_pipeline('nhanes') or any survey name")
message("Available surveys: ", paste(names(SURVEY_CONFIG), collapse = ", "))
