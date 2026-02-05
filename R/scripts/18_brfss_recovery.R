# ==============================================================================
# 18_brfss_recovery.R
# Recovery Script: Generate outputs from crashed BRFSS BHAPC pipeline
# Author: Christine Lucille Kuryla
#
# Purpose: Load the saved 41GB BRFSS model and generate all missing outputs
# that were not created before the pipeline crashed.
#
# Context:
# - The BRFSS model completed after ~159 hours (9546.8 minutes)
# - Pipeline crashed right after saving brfss_variance_decomposition.csv
# - This script recovers all remaining outputs from the saved model
#
# Usage:
#   Rscript R/scripts/18_brfss_recovery.R 2>&1 | tee output/bhapc_parallel/brfss/recovery.log
#
# Expected runtime: 5-15 minutes (loading model + generating outputs)
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BRFSS BHAPC RECOVERY SCRIPT\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# 1. Setup - Load packages and source functions
# ==============================================================================

cat("--- STEP 1: LOADING PACKAGES AND FUNCTIONS ---\n")

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

# Source project functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_table_generation.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Configuration
SURVEY_NAME <- "brfss"
SURVEY_CONFIG <- list(
  srh_scale = 5,
  age_min = 18,
  age_max = 89,
  oldest_age_group = "80-84"
)

# Paths
output_dir <- here("output", "bhapc_parallel", "brfss")
model_path <- file.path(output_dir, "brfss_bhapc_model.rds")
data_path <- file.path(output_dir, "brfss_bhapc_data.rds")
variance_path <- file.path(output_dir, "brfss_variance_decomposition.csv")

cat("  Output directory:", output_dir, "\n")
cat("  Model path:", model_path, "\n")
cat("  Data path:", data_path, "\n")
cat("\n")

# ==============================================================================
# 2. Load existing model and data
# ==============================================================================

cat("--- STEP 2: LOADING MODEL AND DATA ---\n")

# Check files exist
stopifnot(
  "Model file not found" = file.exists(model_path),
  "Data file not found" = file.exists(data_path),
  "Variance decomposition not found" = file.exists(variance_path)
)

cat("  Loading BHAPC data...\n")
bhapc_df <- readRDS(data_path)
cat("  Data loaded: ", format(nrow(bhapc_df), big.mark = ","), " observations\n")
cat("  Periods:", paste(sort(unique(bhapc_df$period_4yr)), collapse = ", "), "\n")
cat("  Cohorts:", length(unique(bhapc_df$cohort_4yr)), "\n")

cat("\n  Loading BHAPC model (41GB - this may take a few minutes)...\n")
load_start <- Sys.time()
model_result <- readRDS(model_path)
load_elapsed <- difftime(Sys.time(), load_start, units = "mins")
cat("  Model loaded in", round(load_elapsed, 1), "minutes\n")

# Extract the model object
model <- model_result$model
cat("  Model class:", class(model)[1], "\n")
cat("  Model formula:", model_result$formula %||% "N/A", "\n")

# Load existing variance decomposition
variance_df <- read.csv(variance_path)
cat("\n  Variance decomposition (already saved):\n")
for (i in 1:nrow(variance_df)) {
  cat("    ", variance_df$component[i], ": ",
      round(variance_df$variance[i], 4), " (",
      round(variance_df$pct_of_total[i], 1), "%)\n", sep = "")
}

cat("\n")

# ==============================================================================
# 3. Generate Table 2
# ==============================================================================

cat("--- STEP 3: GENERATING TABLE 2 ---\n")

table2 <- create_table2(
  model,
  bhapc_df,
  survey = SURVEY_NAME,
  output_path = file.path(output_dir, "table2_brfss.csv")
)

cat("  Saved: table2_brfss.csv\n")

# ==============================================================================
# 4. Extract and save diagnostics
# ==============================================================================

cat("\n--- STEP 4: EXTRACTING DIAGNOSTICS ---\n")

diagnostics <- extract_bhapc_diagnostics(model)
write.csv(diagnostics, file.path(output_dir, "brfss_diagnostics.csv"), row.names = FALSE)

max_rhat <- max(diagnostics$Rhat, na.rm = TRUE)
min_neff <- min(diagnostics$n_eff, na.rm = TRUE)
converged <- max_rhat < 1.01

cat("  Max Rhat:", round(max_rhat, 3), "\n")
cat("  Min n_eff:", round(min_neff, 0), "\n")
cat("  Converged:", converged, "\n")
cat("  Saved: brfss_diagnostics.csv\n")

# ==============================================================================
# 5. Compute descriptive gradients by period
# ==============================================================================

cat("\n--- STEP 5: COMPUTING DESCRIPTIVE GRADIENTS ---\n")

age_groups <- sort(unique(bhapc_df$age_group))
youngest_group <- age_groups[1]
oldest_group <- SURVEY_CONFIG$oldest_age_group
if (!(oldest_group %in% age_groups)) {
  oldest_group <- age_groups[length(age_groups)]
}

cat("  Youngest age group:", youngest_group, "\n")
cat("  Oldest age group:", oldest_group, "\n")

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
          file.path(output_dir, "gradient_by_period_brfss.csv"),
          row.names = FALSE)

cat("  Saved: gradient_by_period_brfss.csv\n")
cat("\n  Gradients by period:\n")
for (i in 1:nrow(gradient_by_period)) {
  cat("    ", gradient_by_period$period_4yr[i], ": ",
      round(gradient_by_period$gradient[i], 3), "\n", sep = "")
}

# ==============================================================================
# 6. Generate Figure 2: Descriptive means
# ==============================================================================

cat("\n--- STEP 6: GENERATING FIGURE 2 (DESCRIPTIVE) ---\n")

fig2 <- create_figure2_descriptive(
  bhapc_df,
  survey = SURVEY_NAME,
  output_path = file.path(output_dir, "figure2_brfss.png")
)

cat("  Saved: figure2_brfss.png\n")

# ==============================================================================
# 7. Generate Figure 3: APC effects
# ==============================================================================

cat("\n--- STEP 7: GENERATING FIGURE 3 (APC EFFECTS) ---\n")

fig3 <- create_figure3_apc_effects(
  model,
  bhapc_df,
  survey = SURVEY_NAME,
  output_path = file.path(output_dir, "figure3_brfss.png")
)

cat("  Saved: figure3_brfss.png\n")

# ==============================================================================
# 8. Generate variance chart
# ==============================================================================

cat("\n--- STEP 8: GENERATING VARIANCE CHART ---\n")

var_chart <- create_variance_chart(variance_df, survey = SURVEY_NAME)
ggsave(file.path(output_dir, "variance_chart_brfss.png"),
       var_chart, width = 6, height = 5, dpi = 300)

cat("  Saved: variance_chart_brfss.png\n")

# ==============================================================================
# 9. Generate summary figure (modified - skip interaction panel)
# ==============================================================================

cat("\n--- STEP 9: GENERATING SUMMARY FIGURE (WITHOUT INTERACTION) ---\n")

# Create a simplified summary figure without interaction panel
# since we're skipping the interaction model

n_obs <- nrow(bhapc_df)

# Use subsampled posterior to avoid memory issues (full posterior is very large)
cat("  Extracting posterior samples (subsampled to 1000 draws)...\n")
full_posterior <- as.matrix(model)
n_draws <- nrow(full_posterior)
cat("  Full posterior has", n_draws, "draws\n")

# Subsample to 1000 draws for plotting
set.seed(20260205)
sample_idx <- sample(1:n_draws, min(1000, n_draws))
posterior <- full_posterior[sample_idx, ]
rm(full_posterior)  # Free memory
gc()

cat("  Using", nrow(posterior), "posterior draws for summary figure\n")

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

# --- Variance Table ---
var_table <- variance_df %>%
  filter(component != "Total") %>%
  mutate(
    Component = case_when(
      component == "period_4yr" ~ "Period",
      component == "cohort_4yr" ~ "Cohort",
      component == "psu:strata" ~ "PSU:Strata",
      component == "strata" ~ "Strata",
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

# --- Gradient Table (use instead of interaction) ---
grad_table <- gradient_by_period %>%
  mutate(
    Period = as.character(period_4yr),
    Young = sprintf("%.2f", srh_young),
    Old = sprintf("%.2f", srh_old),
    Gradient = sprintf("%.3f", gradient)
  ) %>%
  select(Period, Young, Old, Gradient)

t_gradient <- tableGrob(grad_table, rows = NULL,
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
p_t_gradient <- wrap_table(t_gradient, "Age Gradient by Period", valign = "top")

# --- Gradient trend plot ---
p_gradient <- ggplot(gradient_by_period, aes(x = period_4yr, y = gradient)) +
  geom_line(color = "#D55E00", linewidth = 0.8) +
  geom_point(size = 2, color = "#D55E00") +
  labs(title = "Age Gradient Trend", x = "Period", y = "SRH (Young - Old)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- Combine (modified layout without interaction model) ---
layout <- "
AADDDEEEFFGG
AADDDEEEFFGG
BBDDDEEEFFHH
BBDDDEEEFFHH
CCDDDEEEFFHH
CCDDDEEEFFHH
"

combined <- p_t_variance + p_age + p_period + p_cohort +
  p_lexis + p_descriptive + p_t_gradient + p_gradient +
  plot_layout(design = layout) +
  plot_annotation(
    title = paste0("BRFSS BHAPC Analysis Summary"),
    subtitle = paste0("Bayesian Hierarchical Age-Period-Cohort decomposition (N = ",
                      format(n_obs, big.mark = ","), ")"),
    caption = "Higher SRH = better health. Error bars/ribbons show 90% CIs. Interaction model skipped (recovery run).",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      plot.caption = element_text(size = 10, color = "gray50")
    )
  ) &
  theme(plot.margin = margin(2, 2, 2, 2))

# Save
ggsave(file.path(output_dir, "brfss_bhapc_summary.png"),
       combined, width = 16, height = 10, dpi = 300)
ggsave(file.path(output_dir, "brfss_bhapc_summary.pdf"),
       combined, width = 16, height = 10)

cat("  Saved: brfss_bhapc_summary.png\n")
cat("  Saved: brfss_bhapc_summary.pdf\n")

# ==============================================================================
# 10. Save compact model version
# ==============================================================================

cat("\n--- STEP 10: SAVING COMPACT MODEL VERSION ---\n")

compact_model <- list(
  posterior_draws = as.matrix(model),
  fixed_effects = extract_fixed_effects(model),
  random_effects = extract_random_effects(model, bhapc_df),
  variance_components = variance_df,
  diagnostics = diagnostics,
  formula = model_result$formula %||% "srh ~ age + scale(age_squared) + lnWt + (1|period_4yr) + (1|cohort_4yr)",
  n_obs = nrow(bhapc_df),
  n_periods = length(unique(bhapc_df$period_4yr)),
  n_cohorts = length(unique(bhapc_df$cohort_4yr)),
  original_model_elapsed_min = model_result$elapsed_minutes %||% 9546.8,
  recovery_timestamp = Sys.time()
)

compact_path <- file.path(output_dir, "brfss_bhapc_compact.rds")
saveRDS(compact_model, compact_path)

compact_size <- file.size(compact_path) / 1024^2  # MB
cat("  Saved: brfss_bhapc_compact.rds (", round(compact_size, 1), " MB)\n", sep = "")

# ==============================================================================
# 11. Verification summary
# ==============================================================================

cat("\n--- STEP 11: VERIFICATION SUMMARY ---\n")

# List all outputs
output_files <- c(
  "table2_brfss.csv",
  "brfss_diagnostics.csv",
  "gradient_by_period_brfss.csv",
  "figure2_brfss.png",
  "figure3_brfss.png",
  "variance_chart_brfss.png",
  "brfss_bhapc_summary.png",
  "brfss_bhapc_summary.pdf",
  "brfss_bhapc_compact.rds"
)

cat("\n  Generated outputs:\n")
for (f in output_files) {
  full_path <- file.path(output_dir, f)
  if (file.exists(full_path)) {
    size <- file.size(full_path)
    if (size > 1024^2) {
      size_str <- paste0(round(size / 1024^2, 1), " MB")
    } else if (size > 1024) {
      size_str <- paste0(round(size / 1024, 1), " KB")
    } else {
      size_str <- paste0(size, " bytes")
    }
    cat("    [OK]", f, "(", size_str, ")\n")
  } else {
    cat("    [MISSING]", f, "\n")
  }
}

# Skipped outputs
cat("\n  Skipped outputs (require interaction model):\n")
cat("    [SKIPPED] brfss_interaction_model.rds\n")
cat("    [SKIPPED] age_period_interaction_brfss.csv\n")

# Model summary
cat("\n  Model convergence:\n")
cat("    Max Rhat:", round(max_rhat, 4), ifelse(max_rhat < 1.01, "(OK)", "(WARNING)"), "\n")
cat("    Min n_eff:", round(min_neff, 0), ifelse(min_neff > 400, "(OK)", "(WARNING)"), "\n")
cat("    Overall:", ifelse(converged, "CONVERGED", "POTENTIAL ISSUES"), "\n")

# Variance decomposition summary
cat("\n  Variance decomposition:\n")
period_pct <- variance_df$pct_of_total[variance_df$component == "period_4yr"]
cohort_pct <- variance_df$pct_of_total[variance_df$component == "cohort_4yr"]
cat("    Period:", round(period_pct, 1), "%\n")
cat("    Cohort:", round(cohort_pct, 1), "%\n")

# ==============================================================================
# 12. Instructions for cleanup
# ==============================================================================

cat("\n--- STEP 12: CLEANUP INSTRUCTIONS ---\n")

original_size <- file.size(model_path) / 1024^3  # GB
cat("\n  After verifying outputs are correct:\n")
cat("  The original 41GB model can be deleted with:\n")
cat("    rm", model_path, "\n")
cat("\n  Original model size:", round(original_size, 1), "GB\n")
cat("  Compact model size:", round(compact_size, 1), "MB\n")
cat("  Space savings:", round(original_size * 1024 - compact_size, 0), "MB\n")

# ==============================================================================
# Done
# ==============================================================================

total_elapsed <- difftime(Sys.time(), load_start, units = "mins")

cat("\n", strrep("=", 80), "\n")
cat("BRFSS RECOVERY COMPLETE\n")
cat("Total time:", round(total_elapsed, 1), "minutes\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n")
