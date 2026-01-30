# ==============================================================================
# test_bhapc_subsampled.R
# Test BHAPC pipeline on subsampled NHANES data
# Generates variance decomposition, APC effects tables, and figures
# Author: Christine Lucille Kuryla
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Source functions
source(here::here("R/paths.R"))
source(here::here("R/functions/subsample_survey.R"))
source(here::here("R/functions/bhapc_data_prep.R"))
source(here::here("R/functions/bhapc_model_fitting.R"))
source(here::here("R/functions/bhapc_table_generation.R"))
source(here::here("R/functions/bhapc_figure_generation.R"))

# Ensure output directory exists
dir.create(here::here("output/bhapc_test"), recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. Load and subsample data
# ==============================================================================

cat("\n========== LOADING DATA ==========\n")
data_nhanes <- readr::read_rds(derived_path("essential_datasets/data_essential_nhanes.rds"))
data_nhanes_clean <- data_nhanes %>% drop_na(srh, age, year, wt)
cat("Original N:", nrow(data_nhanes_clean), "\n")

cat("\n========== SUBSAMPLING TO 40k ==========\n")
sub_result <- subsample_survey(data_nhanes_clean, target_n = 40000, seed = 1248)
data_sub <- sub_result$data

# Rename wt_sub to wt for the pipeline
data_sub <- data_sub %>%
  select(-wt) %>%
  rename(wt = wt_sub)

cat("Subsampled N:", nrow(data_sub), "\n")

# ==============================================================================
# 2. Prepare BHAPC data
# ==============================================================================

cat("\n========== PREPARING BHAPC DATA ==========\n")
bhapc_df <- prepare_bhapc_data(data_sub, survey = "nhanes", age_max = 85)

# Cell coverage
cat("\n========== CELL COVERAGE ==========\n")
coverage <- create_cell_coverage(bhapc_df)
print(as.data.frame(coverage), row.names = FALSE)

# ==============================================================================
# 3. Fit BHAPC model
# ==============================================================================

cat("\n========== FITTING BHAPC MODEL ==========\n")
cat("Using 2000 iterations for testing (increase for production)\n")

result <- fit_bhapc_model(
  bhapc_df,
  outcome = "srh",
  include_strata = TRUE,
  adapt_delta = 0.99,
  iter = 2000,
  chains = 4,
  cores = 4,
  seed = 1248
)

# ==============================================================================
# 4. Variance decomposition
# ==============================================================================

cat("\n========== VARIANCE DECOMPOSITION ==========\n")
var_components <- extract_variance_components(result$model)
print(as.data.frame(var_components))

# Save variance decomposition
readr::write_csv(var_components, here::here("output/bhapc_test/variance_decomposition_40k.csv"))

# ==============================================================================
# 5. Table 2 (Gloria format)
# ==============================================================================

cat("\n========== TABLE 2 (Gloria format) ==========\n")
table2 <- create_table2(result$model, bhapc_df, survey = "nhanes")
print(as.data.frame(table2), row.names = FALSE)

# Save Table 2
readr::write_csv(table2, here::here("output/bhapc_test/table2_40k.csv"))

# ==============================================================================
# 6. Fixed effects
# ==============================================================================

cat("\n========== FIXED EFFECTS ==========\n")
fixed_eff <- extract_fixed_effects(result$model)
print(as.data.frame(fixed_eff))

# Save fixed effects
readr::write_csv(fixed_eff, here::here("output/bhapc_test/fixed_effects_40k.csv"))

# ==============================================================================
# 7. Random effects (Period and Cohort)
# ==============================================================================

cat("\n========== PERIOD EFFECTS ==========\n")
random_eff <- extract_random_effects(result$model, bhapc_df)
print(as.data.frame(random_eff$period_effects))

cat("\n========== COHORT EFFECTS ==========\n")
print(as.data.frame(random_eff$cohort_effects))

# Save random effects
readr::write_csv(random_eff$period_effects, here::here("output/bhapc_test/period_effects_40k.csv"))
readr::write_csv(random_eff$cohort_effects, here::here("output/bhapc_test/cohort_effects_40k.csv"))

# ==============================================================================
# 8. Age effect curve
# ==============================================================================

cat("\n========== AGE EFFECT ==========\n")
age_effect <- compute_age_effect(result$model, bhapc_df)
cat("Age effect at selected ages:\n")
age_effect %>%
  filter(age %in% c(18, 30, 45, 60, 75, 85)) %>%
  select(age, estimate_centered, ci_lower_centered, ci_upper_centered) %>%
  print()

# Save age effect
readr::write_csv(age_effect, here::here("output/bhapc_test/age_effect_40k.csv"))

# ==============================================================================
# 9. Generate figures
# ==============================================================================

cat("\n========== GENERATING FIGURES ==========\n")

# Figure 2: Descriptive means by age and period
cat("Creating Figure 2 (descriptive)...\n")
fig2 <- create_figure2_descriptive(
  bhapc_df,
  survey = "nhanes_40k",
  output_path = here::here("output/bhapc_test/fig2_descriptive_40k.png")
)

# Figure 3: APC effects (3 panels)
cat("Creating Figure 3 (APC effects)...\n")
fig3 <- create_figure3_apc_effects(
  result$model,
  bhapc_df,
  survey = "nhanes_40k",
  output_path = here::here("output/bhapc_test/fig3_apc_effects_40k.png")
)

# Variance chart
cat("Creating variance chart...\n")
var_chart <- create_variance_chart(var_components, survey = "nhanes_40k")
ggsave(here::here("output/bhapc_test/variance_chart_40k.png"), var_chart, width = 6, height = 5, dpi = 300)

# ==============================================================================
# 10. Diagnostics
# ==============================================================================

cat("\n========== DIAGNOSTICS ==========\n")
cat("Max Rhat:", max(result$diagnostics$Rhat, na.rm = TRUE), "\n")
cat("Min n_eff:", min(result$diagnostics$n_eff, na.rm = TRUE), "\n")
cat("Elapsed time:", round(result$elapsed_minutes, 1), "minutes\n")
cat("Convergence OK:", max(result$diagnostics$Rhat, na.rm = TRUE) < 1.01, "\n")

# Save diagnostics
readr::write_csv(result$diagnostics, here::here("output/bhapc_test/diagnostics_40k.csv"))

# ==============================================================================
# 11. Save full model result (for later comparison)
# ==============================================================================

cat("\n========== SAVING MODEL ==========\n")
saveRDS(result, here::here("output/bhapc_test/bhapc_result_40k.rds"))
cat("Model saved to output/bhapc_test/bhapc_result_40k.rds\n")

cat("\n========== ALL OUTPUTS SAVED ==========\n")
cat("Check output/bhapc_test/ for:\n")
cat("  - variance_decomposition_40k.csv\n")
cat("  - table2_40k.csv\n")
cat("  - fixed_effects_40k.csv\n")
cat("  - period_effects_40k.csv\n")
cat("  - cohort_effects_40k.csv\n")
cat("  - age_effect_40k.csv\n")
cat("  - fig2_descriptive_40k.png\n")
cat("  - fig3_apc_effects_40k.png\n")
cat("  - variance_chart_40k.png\n")
cat("  - diagnostics_40k.csv\n")
cat("  - bhapc_result_40k.rds\n")
