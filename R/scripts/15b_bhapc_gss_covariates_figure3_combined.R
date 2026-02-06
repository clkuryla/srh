# ==============================================================================
# 15b_bhapc_gss_covariates_figure3_combined.R
# Combined Figure 3: GSS APC effects across covariate adjustments (4x3 grid)
#
# Rows: Unadjusted, Demographics, All (excl satjob), All (incl satjob)
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

# Input directories
cov_dir <- here("output", "bhapc_gss_covariates")
base_dir <- here("output", "bhapc_parallel", "gss")
output_dir <- cov_dir

# Define the 4 models (unadjusted + 3 covariate models)
gss_models <- list(
  list(
    name = "Unadjusted",
    short = "unadj",
    model_file = file.path(base_dir, "gss_bhapc_model.rds"),
    data_file = file.path(base_dir, "gss_bhapc_data.rds"),
    var_file = file.path(base_dir, "gss_variance_decomposition.csv")
  ),
  list(
    name = "Demographics",
    short = "demog",
    model_file = file.path(cov_dir, "model_demog.rds"),
    data_file = file.path(cov_dir, "bhapc_data_demog.rds"),
    var_file = file.path(cov_dir, "variance_demog.csv")
  ),
  list(
    name = "All (excl satjob)",
    short = "all_excl",
    model_file = file.path(cov_dir, "model_all.rds"),
    data_file = file.path(cov_dir, "bhapc_data_all.rds"),
    var_file = file.path(cov_dir, "variance_all.csv")
  ),
  list(
    name = "All (incl satjob)",
    short = "all_incl",
    model_file = file.path(cov_dir, "model_all_satjob.rds"),
    data_file = file.path(cov_dir, "bhapc_data_all_satjob.rds"),
    var_file = file.path(cov_dir, "variance_all_satjob.csv")
  )
)

# Panel creation functions
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

# Load and process each model
cat("Loading GSS covariate models and creating panels...\n")

all_panels <- list()
variance_info <- list()

for (i in seq_along(gss_models)) {
  analysis <- gss_models[[i]]
  cat("  Processing:", analysis$name, "\n")

  # Load model and data
  model_result <- readRDS(analysis$model_file)
  bhapc_df <- readRDS(analysis$data_file)

  # Handle model structure (some are lists, some are direct model objects)
  if (is.list(model_result) && "model" %in% names(model_result)) {
    model <- model_result$model
  } else {
    model <- model_result
  }

  # Extract effects
  age_effect <- compute_age_effect(model, bhapc_df)
  random_effects <- extract_random_effects(model, bhapc_df)

  # Create panels
  all_panels[[paste0("age_", i)]] <- create_age_panel(age_effect, analysis$name)
  all_panels[[paste0("period_", i)]] <- create_period_panel(random_effects$period_effects, analysis$name)
  all_panels[[paste0("cohort_", i)]] <- create_cohort_panel(random_effects$cohort_effects, analysis$name)

  # Store variance info
  variance_info[[analysis$name]] <- read_csv(analysis$var_file, show_col_types = FALSE)
}

# Build variance caption
get_pct <- function(var_df, comp) {
  round(var_df$pct_of_total[var_df$component == comp], 1)
}

var_caption <- paste0(
  "Variance decomposition (Period / Cohort / Residual):\n",
  "Unadjusted: ", get_pct(variance_info[["Unadjusted"]], "period_4yr"), "% / ",
  get_pct(variance_info[["Unadjusted"]], "cohort_4yr"), "% / ",
  get_pct(variance_info[["Unadjusted"]], "Residual"), "%  |  ",
  "Demographics: ", get_pct(variance_info[["Demographics"]], "period_4yr"), "% / ",
  get_pct(variance_info[["Demographics"]], "cohort_4yr"), "% / ",
  get_pct(variance_info[["Demographics"]], "Residual"), "%  |  ",
  "All (excl): ", get_pct(variance_info[["All (excl satjob)"]], "period_4yr"), "% / ",
  get_pct(variance_info[["All (excl satjob)"]], "cohort_4yr"), "% / ",
  get_pct(variance_info[["All (excl satjob)"]], "Residual"), "%  |  ",
  "All (incl): ", get_pct(variance_info[["All (incl satjob)"]], "period_4yr"), "% / ",
  get_pct(variance_info[["All (incl satjob)"]], "cohort_4yr"), "% / ",
  get_pct(variance_info[["All (incl satjob)"]], "Residual"), "%"
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
    title = "GSS BHAPC: Effect of Covariate Adjustment",
    subtitle = "Age, Period, and Cohort effects across models (90% credible intervals)",
    caption = var_caption,
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      plot.caption = element_text(size = 8, hjust = 0, color = "gray30")
    )
  )

# Save
output_path <- file.path(output_dir, "figure3_covariates_combined.png")
ggsave(output_path, combined_fig, width = 14, height = 16, dpi = 300)
cat("\nSaved combined figure to:", output_path, "\n")

# Also save PDF
output_pdf <- file.path(output_dir, "figure3_covariates_combined.pdf")
ggsave(output_pdf, combined_fig, width = 14, height = 16)
cat("Saved PDF to:", output_pdf, "\n")
