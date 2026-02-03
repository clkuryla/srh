# ==============================================================================
# 08b_mortality_sensitivity_adjusted_plots.R
# Mortality Sensitivity Analysis: Plots for Adjusted Cox PH Results
#
# Purpose:
#   Load pre-computed adjusted mortality results and create comparison figures
#   showing how covariate adjustment affects the SRH-mortality hazard ratio.
#
# Input:
#   output/sensitivity/mortality/mortality_adjusted_results_{date}.csv
#
# Output:
#   output/sensitivity/mortality/fig_mort_sensitivity_adjusted_draft_{date}.png
#   output/sensitivity/mortality/fig_mort_sensitivity_adjusted.{png,pdf}
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)

# Source shared functions
source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))

# Set theme
theme_set(theme_srh())

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)

# Output directory
output_dir <- here("output", "sensitivity", "mortality")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# LOAD RESULTS
# ==============================================================================

message("\n=== Loading adjusted mortality results ===\n")

# Find the most recent results file
result_files <- list.files(
  output_dir,
  pattern = "mortality_adjusted_results_\\d+\\.csv$",
  full.names = TRUE
)

if (length(result_files) == 0) {
  stop("No adjusted mortality results found in ", output_dir,
       "\nRun 08a_mortality_sensitivity_adjusted.R first.")
}

# Use the most recent file
result_file <- sort(result_files, decreasing = TRUE)[1]
message("Loading: ", basename(result_file))

results <- read_csv(result_file, show_col_types = FALSE)

# Ensure age_group is an ordered factor
age_levels <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
results <- results %>%
  mutate(age_group = factor(age_group, levels = age_levels))

message(sprintf("  %d rows, %d model specifications",
                nrow(results), n_distinct(results$model_label)))
message("  Models: ", paste(unique(results$model_label), collapse = ", "))


# ==============================================================================
# FIGURE: FACETED HR COMPARISON
# ==============================================================================

message("\n=== Creating faceted HR comparison figure ===\n")

plot_data <- results %>%
  filter(converged)

if (nrow(plot_data) == 0) {
  stop("No converged models to plot.")
}

# Determine facet order (Unadjusted first, then alphabetical)
model_labels <- unique(plot_data$model_label)
model_order <- c("Unadjusted", sort(setdiff(model_labels, "Unadjusted")))
model_order <- intersect(model_order, model_labels)

plot_data <- plot_data %>%
  mutate(model_label = factor(model_label, levels = model_order))

# Determine number of columns for facet layout
n_models <- length(model_order)
ncol_facet <- min(n_models, 2)

fig_adjusted <- ggplot(plot_data, aes(
  x = start_year,
  y = hr,
  color = age_group,
  fill = age_group,
  group = age_group
)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_ribbon(
    aes(ymin = conf_low, ymax = conf_high),
    alpha = 0.10,
    color = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~model_label, ncol = ncol_facet, scales = "free_x") +
  scale_color_age() +
  scale_fill_age() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_log10() +
  labs(
    x = "Window Start Year",
    y = "Hazard Ratio per 1-unit SRH",
    color = "Age Group",
    title = "SRH-Mortality HR: Sensitivity to Sociodemographic Adjustment",
    subtitle = "NHIS, 10-year rolling windows, survey-weighted Cox PH"
  ) +
  theme_srh(base_size = 22) +
  theme(
    strip.text = element_text(size = 20, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 18),
    legend.key.size = unit(2, "lines"),
    legend.key.width = unit(2.5, "lines"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 16, color = "gray40")
  ) +
  guides(
    fill = "none",
    color = guide_legend(nrow = 1, override.aes = list(size = 5, linewidth = 2))
  )


# ==============================================================================
# SAVE FIGURES
# ==============================================================================

message("\n=== Saving figures ===\n")

# Determine figure dimensions based on number of models
fig_width <- 8 * ncol_facet
fig_height <- 6 * ceiling(n_models / ncol_facet) + 1.5

# Draft version with date
ggsave(
  filename = file.path(output_dir, paste0("fig_mort_sensitivity_adjusted_draft_", date_suffix, ".png")),
  plot = fig_adjusted,
  width = fig_width, height = fig_height, dpi = 300,
  bg = "white"
)
message("Saved: fig_mort_sensitivity_adjusted_draft_", date_suffix, ".png")

# Final versions (no date)
ggsave(
  filename = file.path(output_dir, "fig_mort_sensitivity_adjusted.png"),
  plot = fig_adjusted,
  width = fig_width, height = fig_height, dpi = 300,
  bg = "white"
)
message("Saved: fig_mort_sensitivity_adjusted.png")

ggsave(
  filename = file.path(output_dir, "fig_mort_sensitivity_adjusted.pdf"),
  plot = fig_adjusted,
  width = fig_width, height = fig_height,
  bg = "white"
)
message("Saved: fig_mort_sensitivity_adjusted.pdf")


# ==============================================================================
# VERIFICATION SUMMARY
# ==============================================================================

message("\n=== Verification summary ===\n")

# HR summary by model and age group
hr_summary <- plot_data %>%
  group_by(model_label, age_group) %>%
  summarise(
    n_windows = n(),
    mean_hr = mean(hr, na.rm = TRUE),
    min_hr = min(hr, na.rm = TRUE),
    max_hr = max(hr, na.rm = TRUE),
    .groups = "drop"
  )

for (ml in model_order) {
  message(sprintf("\n%s:", ml))
  sub <- hr_summary %>% filter(model_label == ml)
  for (i in 1:nrow(sub)) {
    row <- sub[i, ]
    message(sprintf("  %s: mean HR = %.3f (range: %.3f - %.3f, n=%d)",
                    row$age_group, row$mean_hr, row$min_hr, row$max_hr, row$n_windows))
  }
}

message("\n=== Plotting complete ===")
