# ==============================================================================
# 17d_nhis_meps_4x3_apc_figures.R
# Combined Figure 3: NHIS and MEPS APC effects across covariate adjustments (4x3 grid)
#
# Rows: M1 Base, M2 Demographics, M3 K6, M4 Full
# Cols: Age, Period, Cohort
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(patchwork)
  library(rstanarm)
})

source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

# ==============================================================================
# Section 0: Setup
# ==============================================================================

output_dir <- here("output", "bhapc_nhis_meps_covariates")

# Model labels
model_labels <- c(
  m1 = "M1: Base",
  m2 = "M2: + Demographics",
  m3 = "M3: + K6",
  m4 = "M4: Full"
)

# ==============================================================================
# Section 1: Panel creation functions
# ==============================================================================

create_age_panel <- function(age_effect, label, show_ylab = TRUE) {
  ggplot(age_effect, aes(x = age)) +
    geom_ribbon(
      aes(ymin = ci_lower_centered, ymax = ci_upper_centered),
      fill = "#0072B2", alpha = 0.2
    ) +
    geom_line(aes(y = estimate_centered), color = "#0072B2", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = paste0("Age: ", label),
      x = "Age",
      y = if (show_ylab) "Effect on SRH" else ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 9, face = "bold")
    )
}

create_period_panel <- function(period_effects, label, show_ylab = TRUE) {
  ggplot(period_effects, aes(x = period, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_90, ymax = ci_upper_90),
      width = 1.5, color = "#009E73", linewidth = 0.7
    ) +
    geom_point(color = "#009E73", size = 2) +
    labs(
      title = paste0("Period: ", label),
      x = "Period",
      y = if (show_ylab) "Random effect" else ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )
}

create_cohort_panel <- function(cohort_effects, label, show_ylab = TRUE) {
  ggplot(cohort_effects, aes(x = cohort, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_90, ymax = ci_upper_90),
      width = 2, color = "#CC79A7", linewidth = 0.5, alpha = 0.7
    ) +
    geom_point(color = "#CC79A7", size = 1.5) +
    labs(
      title = paste0("Cohort: ", label),
      x = "Birth cohort",
      y = if (show_ylab) "Random effect" else ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )
}

# ==============================================================================
# Section 2: Function to create 4x3 figure for a survey
# ==============================================================================

create_4x3_figure <- function(survey_name) {

  cat("\n========================================\n")
  cat("Processing", toupper(survey_name), "\n")
  cat("========================================\n")

  survey_dir <- file.path(output_dir, survey_name)

  # Define model files
  models_info <- list(
    m1 = list(
      label = model_labels["m1"],
      model_file = file.path(survey_dir, "model_m1.rds"),
      data_file = file.path(survey_dir, "bhapc_data_m1.rds"),
      var_file = file.path(survey_dir, "variance_m1.csv")
    ),
    m2 = list(
      label = model_labels["m2"],
      model_file = file.path(survey_dir, "model_m2.rds"),
      data_file = file.path(survey_dir, "bhapc_data_m2.rds"),
      var_file = file.path(survey_dir, "variance_m2.csv")
    ),
    m3 = list(
      label = model_labels["m3"],
      model_file = file.path(survey_dir, "model_m3.rds"),
      data_file = file.path(survey_dir, "bhapc_data_m3.rds"),
      var_file = file.path(survey_dir, "variance_m3.csv")
    ),
    m4 = list(
      label = model_labels["m4"],
      model_file = file.path(survey_dir, "model_m4.rds"),
      data_file = file.path(survey_dir, "bhapc_data_m4.rds"),
      var_file = file.path(survey_dir, "variance_m4.csv")
    )
  )

  # Load and process each model
  all_panels <- list()
  variance_info <- list()

  for (i in seq_along(models_info)) {
    m_name <- names(models_info)[i]
    info <- models_info[[m_name]]
    cat("  Processing:", info$label, "\n")

    # Load model and data
    model_result <- readRDS(info$model_file)
    bhapc_df <- readRDS(info$data_file)

    # Handle model structure (some are wrapped in lists, some are direct stanreg objects)
    if (inherits(model_result, "stanreg")) {
      model <- model_result
    } else if (is.list(model_result) && "model" %in% names(model_result)) {
      model <- model_result$model
    } else {
      model <- model_result
    }

    # Extract effects
    age_effect <- compute_age_effect(model, bhapc_df)
    random_effects <- extract_random_effects(model, bhapc_df)

    # Create panels
    all_panels[[paste0("age_", i)]] <- create_age_panel(age_effect, info$label)
    all_panels[[paste0("period_", i)]] <- create_period_panel(random_effects$period_effects, info$label)
    all_panels[[paste0("cohort_", i)]] <- create_cohort_panel(random_effects$cohort_effects, info$label)

    # Store variance info
    variance_info[[info$label]] <- read_csv(info$var_file, show_col_types = FALSE)
  }

  # Build variance caption
  get_pct <- function(var_df, comp) {
    val <- var_df$pct_of_total[var_df$component == comp]
    if (length(val) == 0) return(NA)
    round(val, 1)
  }

  # Model descriptions for caption
  model_descriptions <- paste0(
    "Model specifications:\n",
    "M1 Base: age + age² + log(weight)\n",
    "M2 Demographics: + education + race + sex\n",
    "M3 K6: + K6 psychological distress scale\n",
    "M4 Full: demographics + K6\n\n"
  )

  var_caption <- paste0(
    model_descriptions,
    "Variance decomposition (Period / Cohort / Residual):\n",
    "M1: ", get_pct(variance_info[[model_labels["m1"]]], "period_4yr"), "% / ",
    get_pct(variance_info[[model_labels["m1"]]], "cohort_4yr"), "% / ",
    get_pct(variance_info[[model_labels["m1"]]], "Residual"), "%  |  ",
    "M2: ", get_pct(variance_info[[model_labels["m2"]]], "period_4yr"), "% / ",
    get_pct(variance_info[[model_labels["m2"]]], "cohort_4yr"), "% / ",
    get_pct(variance_info[[model_labels["m2"]]], "Residual"), "%  |  ",
    "M3: ", get_pct(variance_info[[model_labels["m3"]]], "period_4yr"), "% / ",
    get_pct(variance_info[[model_labels["m3"]]], "cohort_4yr"), "% / ",
    get_pct(variance_info[[model_labels["m3"]]], "Residual"), "%  |  ",
    "M4: ", get_pct(variance_info[[model_labels["m4"]]], "period_4yr"), "% / ",
    get_pct(variance_info[[model_labels["m4"]]], "cohort_4yr"), "% / ",
    get_pct(variance_info[[model_labels["m4"]]], "Residual"), "%"
  )

  # Arrange in 4x3 grid: rows = models, cols = Age/Period/Cohort
  combined_fig <- (
    all_panels$age_1 | all_panels$period_1 | all_panels$cohort_1
  ) / (
    all_panels$age_2 | all_panels$period_2 | all_panels$cohort_2
  ) / (
    all_panels$age_3 | all_panels$period_3 | all_panels$cohort_3
  ) / (
    all_panels$age_4 | all_panels$period_4 | all_panels$cohort_4
  ) +
    plot_annotation(
      title = paste0(toupper(survey_name), " BHAPC: Effect of Covariate Adjustment on APC Decomposition"),
      subtitle = "Testing whether compositional changes in demographics and K6 explain cohort effects (90% credible intervals)",
      caption = var_caption,
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        plot.caption = element_text(size = 10, hjust = 0, color = "gray30", lineheight = 1.3),
        plot.margin = margin(10, 10, 20, 10)
      )
    )

  # Return figure and info
  list(
    figure = combined_fig,
    variance_info = variance_info
  )
}

# ==============================================================================
# Section 3: Generate figures for both surveys
# ==============================================================================

# NHIS figure
nhis_result <- create_4x3_figure("nhis")

# Save NHIS figure
output_path_nhis_png <- file.path(output_dir, "nhis_figure3_4x3.png")
ggsave(output_path_nhis_png, nhis_result$figure, width = 14, height = 18, dpi = 300)
cat("\nSaved NHIS figure to:", output_path_nhis_png, "\n")

output_path_nhis_pdf <- file.path(output_dir, "nhis_figure3_4x3.pdf")
ggsave(output_path_nhis_pdf, nhis_result$figure, width = 14, height = 18)
cat("Saved NHIS PDF to:", output_path_nhis_pdf, "\n")

# MEPS figure
meps_result <- create_4x3_figure("meps")

# Save MEPS figure
output_path_meps_png <- file.path(output_dir, "meps_figure3_4x3.png")
ggsave(output_path_meps_png, meps_result$figure, width = 14, height = 18, dpi = 300)
cat("\nSaved MEPS figure to:", output_path_meps_png, "\n")

output_path_meps_pdf <- file.path(output_dir, "meps_figure3_4x3.pdf")
ggsave(output_path_meps_pdf, meps_result$figure, width = 14, height = 18)
cat("Saved MEPS PDF to:", output_path_meps_pdf, "\n")

cat("\n========================================\n")
cat("All figures generated successfully!\n")
cat("========================================\n")
