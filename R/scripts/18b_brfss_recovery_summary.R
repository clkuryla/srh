# ==============================================================================
# 18b_brfss_recovery_summary.R
# Generate summary figure and compact model from BRFSS BHAPC model
# (Continuation of recovery after crash on step 9)
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BRFSS BHAPC RECOVERY - SUMMARY FIGURE\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# Setup
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(patchwork)
  library(gridExtra)
  library(grid)
})

# Source functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))

# Paths
output_dir <- here("output", "bhapc_parallel", "brfss")
model_path <- file.path(output_dir, "brfss_bhapc_model.rds")
data_path <- file.path(output_dir, "brfss_bhapc_data.rds")

# ==============================================================================
# Load data and model
# ==============================================================================

cat("--- Loading data and model ---\n")

bhapc_df <- readRDS(data_path)
cat("Data loaded:", format(nrow(bhapc_df), big.mark = ","), "observations\n")

cat("Loading model (41GB - this takes a few minutes)...\n")
load_start <- Sys.time()
model_result <- readRDS(model_path)
model <- model_result$model
cat("Model loaded in", round(difftime(Sys.time(), load_start, units = "mins"), 1), "minutes\n")

# Load already-saved outputs
variance_df <- read.csv(file.path(output_dir, "brfss_variance_decomposition.csv"))
gradient_by_period <- read.csv(file.path(output_dir, "gradient_by_period_brfss.csv"))
diagnostics <- read.csv(file.path(output_dir, "brfss_diagnostics.csv"))

# ==============================================================================
# Generate summary figure with subsampled posterior
# ==============================================================================

cat("\n--- Generating summary figure ---\n")

n_obs <- nrow(bhapc_df)

# Subsample posterior to avoid memory issues
cat("Extracting subsampled posterior (1000 draws)...\n")
full_posterior <- as.matrix(model)
n_draws <- nrow(full_posterior)
cat("Full posterior has", n_draws, "draws,", ncol(full_posterior), "parameters\n")

set.seed(20260205)
sample_idx <- sample(1:n_draws, min(1000, n_draws))
posterior <- full_posterior[sample_idx, ]
rm(full_posterior)
gc()
cat("Using", nrow(posterior), "draws for plotting\n")

# --- Lexis Diagram ---
cat("Creating Lexis diagram...\n")
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
cat("Creating descriptive means plot...\n")
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
cat("Computing age effect curve...\n")
age_sq_mean <- mean(bhapc_df$age_squared)
age_sq_sd <- sd(bhapc_df$age_squared)
ages <- seq(min(bhapc_df$age), max(bhapc_df$age), by = 1)
min_age <- min(bhapc_df$age)

age_effect_samples <- sapply(ages, function(a) {
  linear <- posterior[, "age"] * (a - min_age)
  scaled_sq <- (a^2 - age_sq_mean) / age_sq_sd
  scaled_sq_ref <- (min_age^2 - age_sq_mean) / age_sq_sd
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
cat("Extracting period effects...\n")
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
cat("Extracting cohort effects...\n")
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
cat("Creating tables...\n")

# Variance table
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

# Gradient table
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

# Gradient trend plot
p_gradient <- ggplot(gradient_by_period, aes(x = period_4yr, y = gradient)) +
  geom_line(color = "#D55E00", linewidth = 0.8) +
  geom_point(size = 2, color = "#D55E00") +
  labs(title = "Age Gradient Trend", x = "Period", y = "SRH (Young - Old)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- Combine ---
cat("Combining panels...\n")

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
    title = "BRFSS BHAPC Analysis Summary",
    subtitle = paste0("Bayesian Hierarchical Age-Period-Cohort decomposition (N = ",
                      format(n_obs, big.mark = ","), ")"),
    caption = "Higher SRH = better health. Error bars/ribbons show 90% CIs. Note: Model may have convergence issues (Rhat > 1.01).",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      plot.caption = element_text(size = 10, color = "gray50")
    )
  ) &
  theme(plot.margin = margin(2, 2, 2, 2))

# Save
cat("Saving summary figure...\n")
ggsave(file.path(output_dir, "brfss_bhapc_summary.png"),
       combined, width = 16, height = 10, dpi = 300)
ggsave(file.path(output_dir, "brfss_bhapc_summary.pdf"),
       combined, width = 16, height = 10)

cat("Saved: brfss_bhapc_summary.png\n")
cat("Saved: brfss_bhapc_summary.pdf\n")

# ==============================================================================
# Save compact model
# ==============================================================================

cat("\n--- Saving compact model ---\n")

compact_model <- list(
  posterior_draws = posterior,  # Already subsampled
  fixed_effects = extract_fixed_effects(model),
  random_effects = extract_random_effects(model, bhapc_df),
  variance_components = variance_df,
  diagnostics = diagnostics,
  formula = model_result$formula %||% "srh ~ age + scale(age_squared) + lnWt + (1|period_4yr) + (1|cohort_4yr) + (1|strata/psu)",
  n_obs = n_obs,
  n_periods = length(unique(bhapc_df$period_4yr)),
  n_cohorts = length(unique(bhapc_df$cohort_4yr)),
  original_model_elapsed_min = model_result$elapsed_minutes %||% 9546.8,
  recovery_timestamp = Sys.time()
)

compact_path <- file.path(output_dir, "brfss_bhapc_compact.rds")
saveRDS(compact_model, compact_path)

compact_size <- file.size(compact_path) / 1024^2
cat("Saved: brfss_bhapc_compact.rds (", round(compact_size, 1), " MB)\n", sep = "")

# ==============================================================================
# Summary
# ==============================================================================

cat("\n--- VERIFICATION SUMMARY ---\n")

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

cat("\nGenerated outputs:\n")
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
    cat("  [OK]", f, "(", size_str, ")\n")
  } else {
    cat("  [MISSING]", f, "\n")
  }
}

# Convergence warning
max_rhat <- max(diagnostics$Rhat, na.rm = TRUE)
min_neff <- min(diagnostics$n_eff, na.rm = TRUE)

cat("\nModel convergence:\n")
cat("  Max Rhat:", round(max_rhat, 4), ifelse(max_rhat < 1.01, "(OK)", "(WARNING: > 1.01)"), "\n")
cat("  Min n_eff:", round(min_neff, 0), ifelse(min_neff > 400, "(OK)", "(WARNING: < 400)"), "\n")

if (max_rhat >= 1.01 || min_neff < 400) {
  cat("\nWARNING: The BRFSS model shows convergence issues.\n")
  cat("Results should be interpreted with caution or the model may need to be re-run\n")
  cat("with more iterations or different settings.\n")
}

cat("\n", strrep("=", 80), "\n")
cat("RECOVERY COMPLETE\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n")
