# ==============================================================================
# 17c_combined_covariate_figure.R
# Combined figure comparing NHIS and MEPS covariate sensitivity analyses
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(patchwork)
  library(gt)
  library(gridExtra)
  library(grid)
})

source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "theme_srh.R"))

output_dir <- here("output", "bhapc_nhis_meps_covariates")

cat("Loading models and extracting cohort effects...\n")

# ==============================================================================
# 1. Load Models and BHAPC Data
# ==============================================================================

# MEPS
meps_models <- list(
  M1 = read_rds(file.path(output_dir, "meps", "model_m1.rds")),
  M2 = read_rds(file.path(output_dir, "meps", "model_m2.rds")),
  M3 = read_rds(file.path(output_dir, "meps", "model_m3.rds")),
  M4 = read_rds(file.path(output_dir, "meps", "model_m4.rds"))
)

meps_data <- list(
  M1 = read_rds(file.path(output_dir, "meps", "bhapc_data_m1.rds")),
  M2 = read_rds(file.path(output_dir, "meps", "bhapc_data_m2.rds")),
  M3 = read_rds(file.path(output_dir, "meps", "bhapc_data_m3.rds")),
  M4 = read_rds(file.path(output_dir, "meps", "bhapc_data_m4.rds"))
)

# NHIS
nhis_models <- list(
  M1 = read_rds(file.path(output_dir, "nhis", "model_m1.rds")),
  M2 = read_rds(file.path(output_dir, "nhis", "model_m2.rds")),
  M3 = read_rds(file.path(output_dir, "nhis", "model_m3.rds")),
  M4 = read_rds(file.path(output_dir, "nhis", "model_m4.rds"))
)

nhis_data <- list(
  M1 = read_rds(file.path(output_dir, "nhis", "bhapc_data_m1.rds")),
  M2 = read_rds(file.path(output_dir, "nhis", "bhapc_data_m2.rds")),
  M3 = read_rds(file.path(output_dir, "nhis", "bhapc_data_m3.rds")),
  M4 = read_rds(file.path(output_dir, "nhis", "bhapc_data_m4.rds"))
)

cat("Models loaded.\n")

# ==============================================================================
# 2. Extract Cohort Effects
# ==============================================================================

extract_cohort_effects <- function(models, data_list, survey_name) {
  model_names <- c("M1 (base)", "M2 (demographics)", "M3 (K6)", "M4 (full)")

  map2_dfr(models, data_list, function(model, bhapc_data) {
    re <- extract_random_effects(model, bhapc_data)
    re$cohort_effects
  }, .id = "model_id") %>%
    mutate(
      model = factor(model_names[as.numeric(gsub("M", "", model_id))], levels = model_names),
      survey = survey_name
    )
}

meps_cohort <- extract_cohort_effects(meps_models, meps_data, "MEPS")
nhis_cohort <- extract_cohort_effects(nhis_models, nhis_data, "NHIS")

cohort_all <- bind_rows(meps_cohort, nhis_cohort)

cat("Cohort effects extracted.\n")

# ==============================================================================
# 3. Load Variance Comparison Tables
# ==============================================================================

meps_var <- read_csv(file.path(output_dir, "meps", "comparison_table.csv"), show_col_types = FALSE)
nhis_var <- read_csv(file.path(output_dir, "nhis", "comparison_table.csv"), show_col_types = FALSE)

# ==============================================================================
# 4. Create Combined Figure
# ==============================================================================

model_colors <- c(
  "M1 (base)" = "gray40",
  "M2 (demographics)" = "#E69F00",
  "M3 (K6)" = "#0072B2",
  "M4 (full)" = "#D55E00"
)

# Panel A: MEPS Cohort Effects
p_meps <- cohort_all %>%
  filter(survey == "MEPS") %>%
  ggplot(aes(x = cohort, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 2, alpha = 0.85) +
  scale_color_manual(values = model_colors) +
  scale_x_continuous(breaks = seq(1920, 2000, 20)) +
  labs(
    title = "A. MEPS Cohort Effects",
    x = "Birth Cohort",
    y = "Cohort Effect (SRH units)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# Panel B: NHIS Cohort Effects
p_nhis <- cohort_all %>%
  filter(survey == "NHIS") %>%
  ggplot(aes(x = cohort, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 2, alpha = 0.85) +
  scale_color_manual(values = model_colors) +
  scale_x_continuous(breaks = seq(1920, 2000, 20)) +
  labs(
    title = "B. NHIS Cohort Effects",
    x = "Birth Cohort",
    y = "Cohort Effect (SRH units)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# Shared legend - use all models to get complete legend
legend_plot <- cohort_all %>%
  filter(survey == "MEPS") %>%
  ggplot(aes(x = cohort, y = estimate, color = model)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = model_colors, name = "Model") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold")
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 2, size = 4)))

legend <- cowplot::get_legend(legend_plot)

# ==============================================================================
# 5. Create Variance Table
# ==============================================================================

# Create formatted table data
variance_table <- tibble(
  Model = c("M1: Base (age + period + cohort)",
            "M2: + Demographics (race, education, sex)",
            "M3: + K6 psychological distress",
            "M4: Full (demographics + K6)"),
  `MEPS Cohort %` = sprintf("%.2f", c(meps_var$M1_base[1], meps_var$M2_demog[1],
                                       meps_var$M3_k6[1], meps_var$M4_full[1])),
  `MEPS Period %` = sprintf("%.2f", c(meps_var$M1_base[2], meps_var$M2_demog[2],
                                       meps_var$M3_k6[2], meps_var$M4_full[2])),
  `NHIS Cohort %` = sprintf("%.2f", c(nhis_var$M1_base[1], nhis_var$M2_demog[1],
                                       nhis_var$M3_k6[1], nhis_var$M4_full[1])),
  `NHIS Period %` = sprintf("%.2f", c(nhis_var$M1_base[2], nhis_var$M2_demog[2],
                                       nhis_var$M3_k6[2], nhis_var$M4_full[2]))
)

# Calculate reductions
meps_cohort_reduction <- (1 - meps_var$M4_full[1] / meps_var$M1_base[1]) * 100
nhis_cohort_reduction <- (1 - nhis_var$M4_full[1] / nhis_var$M1_base[1]) * 100

# Add reduction row
variance_table <- variance_table %>%
  add_row(
    Model = "Cohort % Reduction (M1→M4)",
    `MEPS Cohort %` = sprintf("%.0f%%", meps_cohort_reduction),
    `MEPS Period %` = "—",
    `NHIS Cohort %` = sprintf("%.0f%%", nhis_cohort_reduction),
    `NHIS Period %` = "—"
  )

# Create table grob
table_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 11, hjust = 0, x = 0.02),
    bg_params = list(fill = c("white", "gray95"), col = NA)
  ),
  colhead = list(
    fg_params = list(fontsize = 12, fontface = "bold", hjust = 0.5),
    bg_params = list(fill = "gray85", col = NA)
  )
)

table_grob <- tableGrob(variance_table, rows = NULL, theme = table_theme)

# ==============================================================================
# 6. Model Description Text
# ==============================================================================

model_desc <- paste0(
  "Model Specifications:\n",
  "• M1 (Base): SRH ~ age + age² + log(weight) + (1|period) + (1|cohort)\n",
  "• M2 (Demographics): M1 + race/ethnicity + education + sex\n",
  "• M3 (K6): M1 + K6 psychological distress score (0-24)\n",
  "• M4 (Full): M1 + demographics + K6\n\n",
  "Notes: Random effects for 4-year period and cohort bins. ",
  "N = 100,000 subsampled per survey. NHIS restricted to 1997+ (education availability)."
)

desc_grob <- textGrob(
  model_desc,
  x = 0.02, y = 0.5,
  hjust = 0, vjust = 0.5,
  gp = gpar(fontsize = 10, fontfamily = "sans")
)

# ==============================================================================
# 7. Combine All Elements
# ==============================================================================

# Top row: cohort plots side by side
top_row <- (p_meps | p_nhis) + plot_layout(widths = c(1, 1))

# Combine with patchwork
combined_figure <- wrap_elements(top_row) /
  wrap_elements(legend) /
  wrap_elements(table_grob) /
  wrap_elements(desc_grob) +
  plot_layout(heights = c(5, 0.5, 2, 1.2))

# Save
ggsave(
  file.path(output_dir, "combined_covariate_comparison.png"),
  combined_figure,
  width = 14, height = 12, dpi = 300, bg = "white"
)

ggsave(
  file.path(output_dir, "combined_covariate_comparison.pdf"),
  combined_figure,
  width = 14, height = 12, bg = "white"
)

cat("\nSaved combined figure to:\n")
cat("  ", file.path(output_dir, "combined_covariate_comparison.png"), "\n")
cat("  ", file.path(output_dir, "combined_covariate_comparison.pdf"), "\n")

# ==============================================================================
# 8. Print Summary
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("VARIANCE DECOMPOSITION SUMMARY\n")
cat(strrep("=", 70), "\n\n")

print(variance_table, n = Inf)

cat("\n\nKey Findings:\n")
cat(sprintf("• MEPS: Cohort variance reduced from %.2f%% to %.2f%% (%.0f%% reduction)\n",
            meps_var$M1_base[1], meps_var$M4_full[1], meps_cohort_reduction))
cat(sprintf("• NHIS: Cohort variance reduced from %.2f%% to %.2f%% (%.0f%% reduction)\n",
            nhis_var$M1_base[1], nhis_var$M4_full[1], nhis_cohort_reduction))
cat("\n• K6 psychological distress (M3) accounts for most of the reduction\n")
cat("• Demographics alone (M2) have smaller effect\n")
cat(strrep("=", 70), "\n")
