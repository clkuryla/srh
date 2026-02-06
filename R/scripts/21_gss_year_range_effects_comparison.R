# ==============================================================================
# 21_gss_year_range_effects_comparison.R
# Compare Cohort and Period Effects Across GSS Year Restrictions
# Author: Claude
# Date: 2026-02-06
#
# Purpose: Create comparison figures showing how cohort and period random
# effects change across different GSS time restrictions.
# ==============================================================================

library(tidyverse)
library(here)
library(rstanarm)
library(patchwork)

source(here("R/functions/theme_srh.R"))
source(here("R/functions/bhapc_model_fitting.R"))

# ==============================================================================
# Configuration
# ==============================================================================

output_dir <- here("output/bhapc_parallel/gss_sensitivity_timeline")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Define the models to compare
model_configs <- tribble(
  ~label,              ~path,                                                    ~start_year, ~end_year,
  "Full (1972-2024)",  "output/bhapc_parallel/gss/gss_bhapc_model.rds",         1972,        2024,
  "1994-2024",         "output/bhapc_parallel/restricted_gss_1994/gss_restricted_bhapc_model.rds", 1994, 2024,
  "2004-2024",         "output/bhapc_parallel/restricted_gss_2004_2024/gss_restricted_bhapc_model.rds", 2004, 2024,
  "1972-2014",         "output/bhapc_parallel/restricted_gss_1971_2014/gss_restricted_bhapc_model.rds", 1972, 2014,
  "1994-2014",         "output/bhapc_parallel/restricted_gss_1994_2014/gss_restricted_bhapc_model.rds", 1994, 2014
)

# Color palette for models
model_colors <- c(
  "Full (1972-2024)" = "#1B9E77",
  "1994-2024" = "#D95F02",
  "2004-2024" = "#7570B3",
  "1972-2014" = "#E7298A",
  "1994-2014" = "#66A61E"
)

# ==============================================================================
# Extract Random Effects from All Models
# ==============================================================================

cat("Loading models and extracting random effects...\n")

all_cohort_effects <- list()
all_period_effects <- list()

for (i in seq_len(nrow(model_configs))) {
  config <- model_configs[i, ]
  cat("  Loading:", config$label, "\n")


  model_path <- here(config$path)
  if (!file.exists(model_path)) {
    cat("    WARNING: Model file not found, skipping\n")
    next
  }

  model_obj <- readRDS(model_path)
  # Model is stored inside a list with $model element
  model <- model_obj$model
  re <- extract_random_effects(model)

  # Add metadata
  cohort_df <- re$cohort_effects %>%
    mutate(
      model = config$label,
      start_year = config$start_year,
      end_year = config$end_year
    )

  period_df <- re$period_effects %>%
    mutate(
      model = config$label,
      start_year = config$start_year,
      end_year = config$end_year
    )

  all_cohort_effects[[i]] <- cohort_df
  all_period_effects[[i]] <- period_df

  rm(model, model_obj)
  gc()
}

cohort_effects <- bind_rows(all_cohort_effects)
period_effects <- bind_rows(all_period_effects)

cat("  Cohort effects extracted:", nrow(cohort_effects), "rows\n")
cat("  Period effects extracted:", nrow(period_effects), "rows\n")

# ==============================================================================
# Save Data Tables
# ==============================================================================

write_csv(cohort_effects, file.path(output_dir, "cohort_effects_by_year_range.csv"))
write_csv(period_effects, file.path(output_dir, "period_effects_by_year_range.csv"))

cat("Saved effect tables to:", output_dir, "\n")

# ==============================================================================
# Figure 1: Cohort Effects Comparison
# ==============================================================================

cat("Creating cohort effects comparison figure...\n")

# Set factor order for consistent legend
cohort_effects <- cohort_effects %>%
  mutate(model = factor(model, levels = names(model_colors)))

period_effects <- period_effects %>%
  mutate(model = factor(model, levels = names(model_colors)))

p_cohort <- ggplot(cohort_effects, aes(x = cohort, y = estimate, color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(
    aes(ymin = ci_lower_90, ymax = ci_upper_90),
    alpha = 0.15, color = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = model_colors, name = "Time Window") +
  scale_fill_manual(values = model_colors, name = "Time Window") +
  scale_x_continuous(breaks = seq(1880, 2010, 20)) +
  labs(
    title = "Cohort Effects Across GSS Time Restrictions",
    subtitle = "Random effects with 90% credible intervals",
    x = "Birth Cohort (midpoint of 4-year bin)",
    y = "Cohort Effect (SRH units)"
  ) +
  theme_srh() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))

# ==============================================================================
# Figure 2: Period Effects Comparison
# ==============================================================================

cat("Creating period effects comparison figure...\n")

p_period <- ggplot(period_effects, aes(x = period, y = estimate, color = model, fill = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(
    aes(ymin = ci_lower_90, ymax = ci_upper_90),
    alpha = 0.15, color = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = model_colors, name = "Time Window") +
  scale_fill_manual(values = model_colors, name = "Time Window") +
  scale_x_continuous(breaks = seq(1970, 2025, 10)) +
  labs(
    title = "Period Effects Across GSS Time Restrictions",
    subtitle = "Random effects with 90% credible intervals",
    x = "Survey Period (midpoint of 4-year bin)",
    y = "Period Effect (SRH units)"
  ) +
  theme_srh() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))

# ==============================================================================
# Combined Figure
# ==============================================================================

p_combined <- p_cohort / p_period +
  plot_annotation(
    title = "GSS BHAPC: Cohort and Period Effects by Time Restriction",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# ==============================================================================
# Save Figures
# ==============================================================================

ggsave(
  file.path(output_dir, "cohort_effects_comparison.png"),
  p_cohort, width = 10, height = 6, dpi = 300
)
ggsave(
  file.path(output_dir, "cohort_effects_comparison.pdf"),
  p_cohort, width = 10, height = 6
)

ggsave(
  file.path(output_dir, "period_effects_comparison.png"),
  p_period, width = 10, height = 6, dpi = 300
)
ggsave(
  file.path(output_dir, "period_effects_comparison.pdf"),
  p_period, width = 10, height = 6
)

ggsave(
  file.path(output_dir, "effects_comparison_combined.png"),
  p_combined, width = 10, height = 10, dpi = 300
)
ggsave(
  file.path(output_dir, "effects_comparison_combined.pdf"),
  p_combined, width = 10, height = 10
)

cat("\n=== Figures Saved ===\n")
cat("  cohort_effects_comparison.png/pdf\n")
cat("  period_effects_comparison.png/pdf\n")
cat("  effects_comparison_combined.png/pdf\n")
cat("Output directory:", output_dir, "\n")
