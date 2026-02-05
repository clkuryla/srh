# ==============================================================================
# 17_bhapc_gss_spline.R
# BHAPC Analysis for GSS with Natural Splines for Age
# Author: Christine Lucille Kuryla
#
# Purpose: Run BHAPC analysis on GSS using ns(age, df=6) instead of the
# quadratic age + scale(age_squared) specification. This allows for more
# flexible age effects while maintaining the period and cohort random effects.
#
# Usage:
#   Rscript R/scripts/17_bhapc_gss_spline.R 2>&1 | tee output/bhapc_gss_spline/run.log
#
# ==============================================================================

# ==============================================================================
# Setup and Configuration
# ==============================================================================

cat("\n", strrep("=", 80), "\n")
cat("BHAPC GSS ANALYSIS WITH NATURAL SPLINES\n")
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
  library(splines)  # For ns()
})

# Source paths and functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_table_generation.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Override DATA_DEPOT if not set
if (Sys.getenv("DATA_DEPOT") == "") {
  Sys.setenv(DATA_DEPOT = "/home/ubuntu/data_depot")
}

# Create output directory
output_dir <- here("output", "bhapc_gss_spline")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

# ==============================================================================
# Configuration
# ==============================================================================

N_CORES <- parallel::detectCores()
cat("Detected cores:", N_CORES, "\n")

# GSS-specific configuration
CONFIG <- list(
  srh_scale = 4,       # GSS uses 4-point scale
  age_min = 18,
  age_max = 89,
  iter = 6000,
  adapt_delta = 0.998,
  oldest_age_group = "85-89",
  interaction_iter = 2000,
  interaction_adapt_delta = 0.95,
  spline_df = 6        # Degrees of freedom for natural spline
)

cat("Configuration:\n")
cat("  SRH scale: 1-", CONFIG$srh_scale, "\n")
cat("  Age range: ", CONFIG$age_min, "-", CONFIG$age_max, "\n")
cat("  Spline df: ", CONFIG$spline_df, "\n")
cat("  iter: ", CONFIG$iter, ", adapt_delta: ", CONFIG$adapt_delta, "\n\n")

# ==============================================================================
# Step 1: Load and Prepare Data
# ==============================================================================

cat("--- STEP 1: DATA LOADING & PREPARATION ---\n")

# Load GSS data
data_path <- file.path(
  Sys.getenv("DATA_DEPOT"),
  "_derived", "srh_project", "essential_datasets",
  "data_essential_gss.rds"
)

if (!file.exists(data_path)) {
  stop("Data file not found: ", data_path)
}

df <- readRDS(data_path)
df <- df %>% drop_na(srh, age, year, wt)
original_n <- nrow(df)

cat("  Loaded GSS data: ", format(original_n, big.mark = ","), " rows\n")

# Prepare BHAPC data
bhapc_df <- prepare_bhapc_data(
  df,
  survey = "gss",
  age_min = CONFIG$age_min,
  age_max = CONFIG$age_max,
  srh_scale = CONFIG$srh_scale
)

cat("  BHAPC N: ", format(nrow(bhapc_df), big.mark = ","), "\n")
cat("  Periods: ", paste(unique(bhapc_df$period_4yr), collapse = ", "), "\n")
cat("  Cohorts: ", length(unique(bhapc_df$cohort_4yr)), "\n")

# Save prepared data
saveRDS(bhapc_df, file.path(output_dir, "gss_spline_bhapc_data.rds"))

# ==============================================================================
# Step 2: Main BHAPC Model with Natural Splines
# ==============================================================================

cat("\n--- STEP 2: MAIN BHAPC MODEL (SPLINE) ---\n")
cat("  Formula: srh ~ ns(age, df=", CONFIG$spline_df, ") + lnWt + (1|period_4yr) + (1|cohort_4yr)\n")
cat("  iter = ", CONFIG$iter, ", adapt_delta = ", CONFIG$adapt_delta, "\n")
cat("  cores = ", N_CORES, "\n")

model_start <- Sys.time()

# Fit model with natural splines
main_model <- stan_lmer(
  srh ~ ns(age, df = CONFIG$spline_df) + lnWt + (1|period_4yr) + (1|cohort_4yr),
  data = bhapc_df,
  adapt_delta = CONFIG$adapt_delta,
  iter = CONFIG$iter,
  chains = 4,
  cores = N_CORES,
  seed = 20260205
)

model_elapsed <- difftime(Sys.time(), model_start, units = "mins")
cat("  Model completed in ", round(model_elapsed, 1), " minutes\n")

# Save model
saveRDS(main_model, file.path(output_dir, "gss_spline_main_model.rds"))

# Extract diagnostics
diagnostics <- extract_bhapc_diagnostics(main_model)
write.csv(diagnostics, file.path(output_dir, "gss_spline_diagnostics.csv"), row.names = FALSE)

cat("\n  Diagnostics:\n")
cat("    Max Rhat: ", round(max(diagnostics$Rhat, na.rm = TRUE), 3), "\n")
cat("    Min n_eff: ", round(min(diagnostics$n_eff, na.rm = TRUE), 0), "\n")
cat("    Converged: ", max(diagnostics$Rhat, na.rm = TRUE) < 1.01, "\n")

# Extract and save variance decomposition
variance_df <- extract_variance_components(main_model)
write.csv(variance_df, file.path(output_dir, "gss_spline_variance_decomposition.csv"), row.names = FALSE)

cat("\n  Variance Decomposition:\n")
for (i in 1:nrow(variance_df)) {
  cat("    ", variance_df$component[i], ": ",
      round(variance_df$variance[i], 4), " (",
      round(variance_df$pct_of_total[i], 1), "%)\n")
}

# ==============================================================================
# Step 3: Interaction Model (Age Spline x Period)
# ==============================================================================

cat("\n--- STEP 3: INTERACTION MODEL (SPLINE x PERIOD) ---\n")

# Compute descriptive gradients first
age_groups <- sort(unique(bhapc_df$age_group))
youngest_group <- age_groups[1]
oldest_group <- CONFIG$oldest_age_group
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
          file.path(output_dir, "gradient_by_period_gss_spline.csv"),
          row.names = FALSE)

# Fit interaction model with spline x period
cat("  Formula: srh ~ period_4yr * ns(age, df=", CONFIG$spline_df, ") + lnWt + (1|cohort_4yr)\n")
cat("  iter = ", CONFIG$interaction_iter, ", adapt_delta = ", CONFIG$interaction_adapt_delta, "\n")

interaction_start <- Sys.time()

interaction_model <- stan_lmer(
  srh ~ period_4yr * ns(age, df = CONFIG$spline_df) + lnWt + (1|cohort_4yr),
  data = bhapc_df,
  adapt_delta = CONFIG$interaction_adapt_delta,
  iter = CONFIG$interaction_iter,
  chains = 4,
  cores = N_CORES,
  seed = 20260205
)

interaction_elapsed <- difftime(Sys.time(), interaction_start, units = "mins")
cat("  Interaction model completed in ", round(interaction_elapsed, 1), " minutes\n")

# Save interaction model
saveRDS(interaction_model, file.path(output_dir, "gss_spline_interaction_model.rds"))

# ==============================================================================
# Step 4: Generate Figures
# ==============================================================================

cat("\n--- STEP 4: GENERATING FIGURES ---\n")

# --- Figure: Age Effect using Predictions ---
# For spline models, we use predict() to generate the age curve

ages_grid <- seq(CONFIG$age_min, CONFIG$age_max, by = 1)

# Create prediction data frame with median values for other variables
pred_df <- tibble(
  age = ages_grid,
  lnWt = median(bhapc_df$lnWt),
  period_4yr = levels(factor(bhapc_df$period_4yr))[1],  # Reference period

cohort_4yr = levels(factor(bhapc_df$cohort_4yr))[
    which.max(table(bhapc_df$cohort_4yr))  # Most common cohort
  ]
)

# Get posterior predictions
posterior_pred <- posterior_linpred(main_model, newdata = pred_df)

# Compute mean and credible intervals
age_effect_df <- tibble(
  age = ages_grid,
  estimate = colMeans(posterior_pred),
  ci_lower = apply(posterior_pred, 2, quantile, 0.05),
  ci_upper = apply(posterior_pred, 2, quantile, 0.95)
)

# Center at minimum age for interpretability
min_effect <- age_effect_df$estimate[1]
age_effect_df <- age_effect_df %>%
  mutate(
    estimate_centered = estimate - min_effect,
    ci_lower_centered = ci_lower - age_effect_df$ci_lower[1],
    ci_upper_centered = ci_upper - age_effect_df$ci_upper[1]
  )

# Save age effect data
write.csv(age_effect_df, file.path(output_dir, "gss_spline_age_effect.csv"), row.names = FALSE)

# Create age effect plot
p_age <- ggplot(age_effect_df, aes(x = age)) +
  geom_ribbon(
    aes(ymin = ci_lower_centered, ymax = ci_upper_centered),
    fill = "#0072B2", alpha = 0.2
  ) +
  geom_line(aes(y = estimate_centered), color = "#0072B2", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Age Effect (Natural Spline, df=6)",
    subtitle = "GSS: Effect on SRH relative to age 18",
    x = "Age (years)",
    y = "Effect on SRH"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  )

ggsave(file.path(output_dir, "gss_spline_age_effect.png"), p_age, width = 8, height = 6, dpi = 300)

# --- Figure: Period and Cohort Effects ---
random_effects <- extract_random_effects(main_model, bhapc_df)

p_period <- ggplot(random_effects$period_effects, aes(x = period, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    aes(ymin = ci_lower_90, ymax = ci_upper_90),
    width = 0.8, color = "#009E73", linewidth = 0.8
  ) +
  geom_point(color = "#009E73", size = 3) +
  labs(
    title = "Period Effects",
    x = "Period (start year)",
    y = "Random effect (SRH units)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  )

p_cohort <- ggplot(random_effects$cohort_effects, aes(x = cohort, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(
    aes(ymin = ci_lower_90, ymax = ci_upper_90),
    width = 2, color = "#CC79A7", linewidth = 0.6, alpha = 0.7
  ) +
  geom_point(color = "#CC79A7", size = 2) +
  labs(
    title = "Cohort Effects",
    x = "Birth cohort (start year)",
    y = "Random effect (SRH units)"
  ) +
  scale_x_continuous(breaks = seq(1920, 2000, 20)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  )

# Save APC effects combined
apc_combined <- (p_age | p_period | p_cohort) +
  plot_annotation(
    title = "GSS BHAPC Analysis with Natural Splines (df=6)",
    subtitle = paste0("N = ", format(nrow(bhapc_df), big.mark = ","), " | 90% credible intervals"),
    tag_levels = "A"
  )

ggsave(file.path(output_dir, "gss_spline_apc_effects.png"), apc_combined, width = 14, height = 5, dpi = 300)

# --- Figure: Variance Chart ---
var_chart <- create_variance_chart(variance_df, survey = "gss")
ggsave(file.path(output_dir, "gss_spline_variance_chart.png"), var_chart, width = 6, height = 5, dpi = 300)

# --- Figure: Descriptive Means ---
means_data <- bhapc_df %>%
  group_by(age_group, period_4yr) %>%
  summarise(mean_srh = weighted.mean(srh, wt, na.rm = TRUE), .groups = "drop")

p_descriptive <- ggplot(means_data, aes(x = age_group, y = mean_srh,
                                         color = period_4yr, group = period_4yr)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  scale_color_viridis_d(option = "D", direction = -1, name = "Period") +
  labs(
    title = "Mean SRH by Age & Period (GSS)",
    x = "Age Group",
    y = "Mean SRH (1=Poor, 4=Excellent)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "right"
  )

ggsave(file.path(output_dir, "gss_spline_descriptive_means.png"), p_descriptive, width = 10, height = 6, dpi = 300)

# ==============================================================================
# Step 5: Create Summary One-Pager
# ==============================================================================

cat("\n--- STEP 5: CREATING SUMMARY ONE-PAGER ---\n")

# Create comprehensive summary figure
create_spline_summary_figure <- function() {

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
      legend.key.width = unit(0.2, "cm")
    )

  # --- Variance Decomposition Table ---
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

  wrap_table <- function(grob, title) {
    ggplot() +
      annotation_custom(grob) +
      labs(title = title) +
      theme_void() +
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0))
  }

  p_t_variance <- wrap_table(t_variance, "Variance Decomposition")

  # --- Combine all panels ---
  layout <- "
AADDDEEEFF
AADDDEEEFF
BBDDDEEEFF
BBDDDEEEFF
CCDDDEEEFF
CCDDDEEEFF
"

  combined <- p_t_variance + p_age + p_period + p_cohort +
    p_lexis + p_descriptive +
    plot_layout(design = layout) +
    plot_annotation(
      title = "GSS BHAPC Analysis with Natural Splines (df=6)",
      subtitle = paste0("Bayesian Hierarchical Age-Period-Cohort decomposition (N = ",
                        format(n_obs, big.mark = ","), ")"),
      caption = paste0("Age modeled with ns(age, df=6). Higher SRH = better health (1-4 scale). ",
                       "Error bars/ribbons show 90% CIs."),
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray30"),
        plot.caption = element_text(size = 10, color = "gray50")
      )
    ) &
    theme(plot.margin = margin(2, 2, 2, 2))

  combined
}

summary_fig <- create_spline_summary_figure()
ggsave(file.path(output_dir, "gss_spline_summary.png"), summary_fig, width = 16, height = 10, dpi = 300)
ggsave(file.path(output_dir, "gss_spline_summary.pdf"), summary_fig, width = 16, height = 10)

# ==============================================================================
# Step 6: Create Comparison with Quadratic Model
# ==============================================================================

cat("\n--- STEP 6: COMPARISON WITH QUADRATIC MODEL ---\n")

# Load quadratic model results if available
quad_variance_path <- here("output", "bhapc_parallel", "gss", "gss_variance_decomposition.csv")

if (file.exists(quad_variance_path)) {
  quad_variance <- read.csv(quad_variance_path)

  comparison_df <- bind_rows(
    variance_df %>%
      filter(component %in% c("period_4yr", "cohort_4yr", "Residual")) %>%
      mutate(model = "Spline (df=6)"),
    quad_variance %>%
      filter(component %in% c("period_4yr", "cohort_4yr", "Residual")) %>%
      mutate(model = "Quadratic")
  ) %>%
    mutate(
      component = case_when(
        component == "period_4yr" ~ "Period",
        component == "cohort_4yr" ~ "Cohort",
        TRUE ~ "Residual"
      )
    )

  p_comparison <- ggplot(comparison_df, aes(x = component, y = pct_of_total, fill = model)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = paste0(round(pct_of_total, 1), "%")),
              position = position_dodge(width = 0.7), vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("Quadratic" = "#E69F00", "Spline (df=6)" = "#56B4E9")) +
    labs(
      title = "Variance Decomposition: Spline vs Quadratic Age Specification",
      subtitle = "GSS BHAPC models",
      x = "", y = "% of Total Variance", fill = "Age Model"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    ylim(0, max(comparison_df$pct_of_total, na.rm = TRUE) * 1.15)

  ggsave(file.path(output_dir, "gss_spline_vs_quadratic_variance.png"),
         p_comparison, width = 8, height = 6, dpi = 300)

  write.csv(comparison_df, file.path(output_dir, "gss_spline_vs_quadratic_comparison.csv"), row.names = FALSE)

  cat("  Variance comparison:\n")
  print(comparison_df %>% select(model, component, pct_of_total) %>% pivot_wider(names_from = model, values_from = pct_of_total))

} else {
  cat("  Quadratic model results not found - skipping comparison\n")
}

# ==============================================================================
# Final Summary
# ==============================================================================

total_elapsed <- difftime(Sys.time(), model_start, units = "mins")

cat("\n", strrep("=", 80), "\n")
cat("PIPELINE COMPLETE: GSS BHAPC WITH NATURAL SPLINES\n")
cat("Total time: ", round(total_elapsed, 1), " minutes\n")
cat("Finished at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 80), "\n\n")

cat("Key Results:\n")
cat("  N observations: ", format(nrow(bhapc_df), big.mark = ","), "\n")
cat("  Max Rhat: ", round(max(diagnostics$Rhat, na.rm = TRUE), 3), "\n")
cat("  Min n_eff: ", round(min(diagnostics$n_eff, na.rm = TRUE), 0), "\n")
cat("  Converged: ", max(diagnostics$Rhat, na.rm = TRUE) < 1.01, "\n")
cat("  Period variance %: ", round(variance_df$pct_of_total[variance_df$component == "period_4yr"], 1), "\n")
cat("  Cohort variance %: ", round(variance_df$pct_of_total[variance_df$component == "cohort_4yr"], 1), "\n")

cat("\nOutputs saved to:", output_dir, "\n")
cat("  - gss_spline_main_model.rds\n")
cat("  - gss_spline_interaction_model.rds\n")
cat("  - gss_spline_variance_decomposition.csv\n")
cat("  - gss_spline_age_effect.csv\n")
cat("  - gss_spline_summary.png/pdf\n")
cat("  - gss_spline_apc_effects.png\n")

cat("\n", strrep("=", 80), "\n")
