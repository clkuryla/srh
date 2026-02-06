# ==============================================================================
# 11d_gss_combined_figure3.R
# Combined Figure 3: GSS APC effects across time restrictions (4x3 grid)
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

# Output directory
output_dir <- here("output", "bhapc_parallel")

# Define the 4 GSS analyses
gss_analyses <- list(
  list(
    name = "Full (1971-2024)",
    short = "full",
    dir = "gss",
    model_file = "gss_bhapc_model.rds",
    data_file = "gss_bhapc_data.rds"
  ),
  list(
    name = "1994-2024",
    short = "1994_2024",
    dir = "restricted_gss_1994",
    model_file = "gss_restricted_bhapc_model.rds",
    data_file = "gss_restricted_bhapc_data.rds"
  ),
  list(
    name = "1994-2014",
    short = "1994_2014",
    dir = "restricted_gss_1994_2014",
    model_file = "gss_restricted_bhapc_model.rds",
    data_file = "gss_restricted_bhapc_data.rds"
  ),
  list(
    name = "2004-2024",
    short = "2004_2024",
    dir = "restricted_gss_2004_2024",
    model_file = "gss_restricted_bhapc_model.rds",
    data_file = "gss_restricted_bhapc_data.rds"
  )
)

# Function to create individual panels with consistent styling
create_age_panel <- function(age_effect, label, show_ylab = TRUE) {
  p <- ggplot(age_effect, aes(x = age)) +
    geom_ribbon(
      aes(ymin = ci_lower_centered, ymax = ci_upper_centered),
      fill = "#0072B2", alpha = 0.2
    ) +
    geom_line(aes(y = estimate_centered), color = "#0072B2", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = paste0("Age Effect: ", label),
      x = "Age",
      y = if (show_ylab) "Effect on SRH" else ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 9, face = "bold")
    )
  p
}

create_period_panel <- function(period_effects, label, show_ylab = TRUE) {
  p <- ggplot(period_effects, aes(x = period, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_90, ymax = ci_upper_90),
      width = 1.5, color = "#009E73", linewidth = 0.7
    ) +
    geom_point(color = "#009E73", size = 2) +
    labs(
      title = paste0("Period Effect: ", label),
      x = "Period",
      y = if (show_ylab) "Random effect" else ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )
  p
}

create_cohort_panel <- function(cohort_effects, label, show_ylab = TRUE) {
  p <- ggplot(cohort_effects, aes(x = cohort, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_90, ymax = ci_upper_90),
      width = 2, color = "#CC79A7", linewidth = 0.5, alpha = 0.7
    ) +
    geom_point(color = "#CC79A7", size = 1.5) +
    labs(
      title = paste0("Cohort Effect: ", label),
      x = "Birth cohort",
      y = if (show_ylab) "Random effect" else ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )
  p
}

# Load and process each analysis
cat("Loading GSS models and creating panels...\n")

all_panels <- list()
variance_info <- list()

for (i in seq_along(gss_analyses)) {
  analysis <- gss_analyses[[i]]
  cat("  Processing:", analysis$name, "\n")

  model_path <- file.path(output_dir, analysis$dir, analysis$model_file)
  data_path <- file.path(output_dir, analysis$dir, analysis$data_file)

  # Load model and data
  model_result <- readRDS(model_path)
  bhapc_df <- readRDS(data_path)

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

  # Store variance info for annotation
  var_path <- file.path(output_dir, analysis$dir,
                        ifelse(analysis$dir == "gss", "gss_variance_decomposition.csv",
                               "gss_restricted_variance_decomposition.csv"))
  var_df <- read.csv(var_path)
  variance_info[[analysis$name]] <- var_df
}

# Arrange in 4x3 grid: rows = analyses, cols = Age/Period/Cohort
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
    title = "GSS BHAPC Results: Sensitivity to Time Period Restrictions",
    subtitle = "Age, Period, and Cohort effects across different year ranges (90% credible intervals)",
    caption = paste0(
      "Variance decomposition (Period / Cohort / Residual):\n",
      "Full 1971-2024: 0.1% / 2.3% / 97.5%  |  ",
      "1994-2024: 0.5% / 2.1% / 97.4%  |  ",
      "1994-2014: 0.7% / 0.1% / 99.3%  |  ",
      "2004-2024: 0.4% / 1.5% / 98.0%"
    ),
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 0, color = "gray30")
    )
  )

# Save
output_path <- file.path(output_dir, "gss_sensitivity_figure3_combined.png")
ggsave(output_path, combined_fig, width = 14, height = 16, dpi = 300)
cat("\nSaved combined figure to:", output_path, "\n")

# Also save PDF
output_pdf <- file.path(output_dir, "gss_sensitivity_figure3_combined.pdf")
ggsave(output_pdf, combined_fig, width = 14, height = 16)
cat("Saved PDF to:", output_pdf, "\n")
