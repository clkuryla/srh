# ==============================================================================
# 16_bhapc_prior_sensitivity.R
# Prior Sensitivity Check for BHAPC Random Effects
# Purpose: Test whether variance decomposition is sensitive to the choice of
#          prior on random effect SDs (decov concentration parameter)
# Uses GSS (smallest dataset, fastest to fit)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("BHAPC PRIOR SENSITIVITY CHECK (GSS)\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n\n")

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(patchwork)
})

# Source functions
source(here("R", "functions", "bhapc_model_fitting.R"))

# Output directory
output_dir <- here("output", "bhapc_prior_sensitivity")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Configuration
N_CORES <- 10
ITER <- 6000
ADAPT_DELTA <- 0.998
SEED <- 20260128

# ==============================================================================
# STEP 1: Load existing data and default-prior model
# ==============================================================================

cat("--- STEP 1: LOADING EXISTING DATA & DEFAULT MODEL ---\n")

bhapc_df <- readRDS(here("output", "bhapc_parallel", "gss", "gss_bhapc_data.rds"))
cat("  N observations:", format(nrow(bhapc_df), big.mark = ","), "\n")
cat("  N periods:", length(unique(bhapc_df$period_4yr)), "\n")
cat("  N cohorts:", length(unique(bhapc_df$cohort_4yr)), "\n")

default_result <- readRDS(here("output", "bhapc_parallel", "gss", "gss_bhapc_model.rds"))
default_model <- default_result$model
cat("  Default model loaded (decov(1,1,1,1))\n\n")

# Extract variance components from existing model
var_default <- extract_variance_components(default_model) %>%
  filter(component != "Total") %>%
  mutate(prior = "Default: decov(1,1,1,1)")

re_default <- extract_random_effects(default_model, bhapc_df)

# ==============================================================================
# STEP 2: Fit concentrated prior model — decov(2, 1, 1, 1)
# ==============================================================================

cat("--- STEP 2: CONCENTRATED PRIOR — decov(2,1,1,1) ---\n")
cat("  (Stronger pull toward equal variance allocation)\n")

formula <- srh ~ age + scale(age_squared) + lnWt + (1|period_4yr) + (1|cohort_4yr)

set.seed(SEED)
t1 <- Sys.time()

model_concentrated <- stan_lmer(
  formula = formula,
  data = bhapc_df,
  prior_covariance = decov(concentration = 2, shape = 1, scale = 1),
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES
)

elapsed_conc <- difftime(Sys.time(), t1, units = "mins")
cat("  Completed in", round(elapsed_conc, 1), "minutes\n")

# Force evaluation and save immediately
conc_sigma <- sigma(model_concentrated)
cat("  Concentrated sigma:", conc_sigma, "\n")
saveRDS(model_concentrated, file.path(output_dir, "model_concentrated.rds"))

var_concentrated <- extract_variance_components(model_concentrated) %>%
  filter(component != "Total") %>%
  mutate(prior = "Concentrated: decov(2,1,1,1)")

re_concentrated <- extract_random_effects(model_concentrated, bhapc_df)

# ==============================================================================
# STEP 3: Fit diffuse prior model — decov(0.5, 1, 1, 1)
# ==============================================================================

cat("\n--- STEP 3: DIFFUSE PRIOR — decov(0.5,1,1,1) ---\n")
cat("  (More permissive, allows one grouping factor to dominate)\n")

set.seed(SEED + 1)  # Different seed to ensure different chain initialization
t2 <- Sys.time()

model_diffuse <- stan_lmer(
  formula = formula,
  data = bhapc_df,
  prior_covariance = decov(concentration = 0.5, shape = 1, scale = 1),
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES
)

elapsed_diff <- difftime(Sys.time(), t2, units = "mins")
cat("  Completed in", round(elapsed_diff, 1), "minutes\n")

# Force evaluation and save immediately
diff_sigma <- sigma(model_diffuse)
cat("  Diffuse sigma:", diff_sigma, "\n")
saveRDS(model_diffuse, file.path(output_dir, "model_diffuse.rds"))

# Verify models are different
cat("\n  VERIFICATION: Checking models are different...\n")
post_conc <- as.matrix(model_concentrated)[1:10, 1]
post_diff <- as.matrix(model_diffuse)[1:10, 1]
if (identical(post_conc, post_diff)) {
  stop("ERROR: Concentrated and diffuse models have identical posteriors!")
} else {
  cat("  OK: Models have different posterior samples\n")
}

var_diffuse <- extract_variance_components(model_diffuse) %>%
  filter(component != "Total") %>%
  mutate(prior = "Diffuse: decov(0.5,1,1,1)")

re_diffuse <- extract_random_effects(model_diffuse, bhapc_df)

# ==============================================================================
# STEP 4: Check convergence for alternative models
# ==============================================================================

cat("\n--- STEP 4: CONVERGENCE DIAGNOSTICS ---\n")

check_convergence <- function(model, label) {
  s <- as.data.frame(summary(model))
  s$parameter <- rownames(s)
  diag <- s %>%
    filter(!grepl("^b\\[", parameter)) %>%
    select(parameter, n_eff, Rhat)
  max_rhat <- max(diag$Rhat, na.rm = TRUE)
  min_neff <- min(diag$n_eff, na.rm = TRUE)
  converged <- max_rhat < 1.01 && min_neff >= 400
  cat(sprintf("  %s: max Rhat = %.3f, min n_eff = %.0f — %s\n",
              label, max_rhat, min_neff,
              ifelse(converged, "CONVERGED", "WARNING")))
  tibble(prior = label, max_rhat = max_rhat, min_neff = min_neff,
         converged = converged)
}

diag_default <- check_convergence(default_model, "Default")
diag_conc <- check_convergence(model_concentrated, "Concentrated")
diag_diff <- check_convergence(model_diffuse, "Diffuse")

diagnostics_summary <- bind_rows(diag_default, diag_conc, diag_diff)
write.csv(diagnostics_summary, file.path(output_dir, "convergence_diagnostics.csv"),
          row.names = FALSE)

# ==============================================================================
# STEP 5: Build variance comparison table
# ==============================================================================

cat("\n--- STEP 5: VARIANCE COMPARISON ---\n")

var_comparison <- bind_rows(var_default, var_concentrated, var_diffuse)

# Wide-format table for easy reading
var_wide <- var_comparison %>%
  select(component, prior, pct_of_total) %>%
  pivot_wider(names_from = prior, values_from = pct_of_total)

cat("\n  Variance % of Total by Prior:\n")
print(as.data.frame(var_wide), row.names = FALSE)

write.csv(var_comparison, file.path(output_dir, "prior_sensitivity_variance.csv"),
          row.names = FALSE)
write.csv(var_wide, file.path(output_dir, "prior_sensitivity_variance_wide.csv"),
          row.names = FALSE)

# ==============================================================================
# STEP 6: Compare cohort random effects across priors
# ==============================================================================

cat("\n--- STEP 6: COHORT EFFECTS COMPARISON ---\n")

cohort_comparison <- bind_rows(
  re_default$cohort_effects %>% mutate(prior = "Default"),
  re_concentrated$cohort_effects %>% mutate(prior = "Concentrated"),
  re_diffuse$cohort_effects %>% mutate(prior = "Diffuse")
)

write.csv(cohort_comparison, file.path(output_dir, "prior_sensitivity_cohort_effects.csv"),
          row.names = FALSE)

# Compute correlations between cohort effects under different priors
cohort_wide <- cohort_comparison %>%
  select(cohort, prior, estimate) %>%
  pivot_wider(names_from = prior, values_from = estimate)

if (nrow(cohort_wide) >= 3) {
  cor_def_conc <- cor(cohort_wide$Default, cohort_wide$Concentrated)
  cor_def_diff <- cor(cohort_wide$Default, cohort_wide$Diffuse)
  cor_conc_diff <- cor(cohort_wide$Concentrated, cohort_wide$Diffuse)

  cat(sprintf("  Correlation of cohort effects:\n"))
  cat(sprintf("    Default vs Concentrated:  r = %.4f\n", cor_def_conc))
  cat(sprintf("    Default vs Diffuse:       r = %.4f\n", cor_def_diff))
  cat(sprintf("    Concentrated vs Diffuse:  r = %.4f\n", cor_conc_diff))
}

# ==============================================================================
# STEP 7: Generate comparison figure
# ==============================================================================

cat("\n--- STEP 7: GENERATING FIGURES ---\n")

# Panel A: Variance decomposition comparison
p_var <- var_comparison %>%
  mutate(
    component = case_when(
      component == "period_4yr" ~ "Period",
      component == "cohort_4yr" ~ "Cohort",
      component == "Residual" ~ "Residual",
      TRUE ~ component
    ),
    component = factor(component, levels = c("Period", "Cohort", "Residual")),
    prior = factor(prior, levels = c(
      "Default: decov(1,1,1,1)",
      "Concentrated: decov(2,1,1,1)",
      "Diffuse: decov(0.5,1,1,1)"
    ))
  ) %>%
  ggplot(aes(x = component, y = pct_of_total, fill = prior)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct_of_total)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "A. Variance Decomposition by Prior",
    x = NULL,
    y = "% of Total Variance",
    fill = "Prior"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.title = element_text(face = "bold", size = 13)
  ) +
  ylim(0, max(var_comparison$pct_of_total) * 1.15)

# Panel B: Cohort effects comparison
p_cohort <- cohort_comparison %>%
  mutate(prior = factor(prior, levels = c("Default", "Concentrated", "Diffuse"))) %>%
  ggplot(aes(x = cohort, y = estimate, color = prior, shape = prior)) +
  geom_point(size = 2.5, position = position_dodge(width = 2)) +
  geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                width = 1.5, position = position_dodge(width = 2),
                linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "B. Cohort Random Effects by Prior",
    x = "Birth Cohort (4-year bin)",
    y = "Cohort Effect (SRH units)",
    color = "Prior",
    shape = "Prior"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13)
  )

# Combine
fig_combined <- p_var + p_cohort +
  plot_layout(widths = c(1, 1.3)) +
  plot_annotation(
    title = "Prior Sensitivity Check: BHAPC Random Effects (GSS)",
    subtitle = "Varying the Dirichlet concentration parameter on the decov prior",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "grey30")
    )
  )

ggsave(file.path(output_dir, "prior_sensitivity_comparison.png"),
       fig_combined, width = 14, height = 7, dpi = 300)

cat("  Saved: prior_sensitivity_comparison.png\n")

# ==============================================================================
# STEP 8: Interpretation
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("INTERPRETATION\n")
cat(strrep("=", 70), "\n\n")

# Get cohort % across priors
cohort_pcts <- var_comparison %>%
  filter(component == "cohort_4yr") %>%
  pull(pct_of_total)
cohort_range <- diff(range(cohort_pcts))
cohort_mean <- mean(cohort_pcts)
cohort_rel_range <- cohort_range / cohort_mean * 100

period_pcts <- var_comparison %>%
  filter(component == "period_4yr") %>%
  pull(pct_of_total)
period_range <- diff(range(period_pcts))

cat(sprintf("Cohort variance %%: %.1f%% - %.1f%% (range: %.1f pp, %.0f%% relative)\n",
            min(cohort_pcts), max(cohort_pcts), cohort_range, cohort_rel_range))
cat(sprintf("Period variance %%: %.1f%% - %.1f%% (range: %.1f pp)\n",
            min(period_pcts), max(period_pcts), period_range))

if (nrow(cohort_wide) >= 3) {
  cat(sprintf("Cohort effect correlations: r = %.3f to %.3f\n",
              min(cor_def_conc, cor_def_diff, cor_conc_diff),
              max(cor_def_conc, cor_def_diff, cor_conc_diff)))
}

cat("\n")
if (cohort_rel_range < 50) {
  cat("CONCLUSION: Variance decomposition is ROBUST to prior choice.\n")
  cat("  The cohort % varies by less than 50% relative across priors.\n")
  cat("  The qualitative story (which component dominates) is unchanged.\n")
} else {
  cat("WARNING: Variance decomposition shows NOTABLE sensitivity to prior.\n")
  cat("  The cohort % varies by >=50% relative across priors.\n")
  cat("  This warrants discussion in the manuscript.\n")
}

cat("\n", strrep("=", 70), "\n")
cat("COMPLETE\n")
cat("Output directory:", output_dir, "\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
