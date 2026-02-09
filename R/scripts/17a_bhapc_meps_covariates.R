# ==============================================================================
# 17a_bhapc_meps_covariates.R
# BHAPC Covariate Sensitivity Analysis: MEPS ONLY
# (Split from 17_bhapc_nhis_meps_covariates.R for parallel execution)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("BHAPC COVARIATE ANALYSIS: MEPS\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(patchwork)
})

source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))
source(here("R", "paths.R"))

set.seed(20260205)

output_dir <- here("output", "bhapc_nhis_meps_covariates", "meps")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

N_CORES     <- min(parallel::detectCores(), 8)
ITER        <- 6000
ADAPT_DELTA <- 0.998
SEED        <- 20260205
N_SUBSAMPLE <- 100000

cat("Configuration:\n")
cat("  Cores:", N_CORES, "\n")
cat("  Iterations:", ITER, "\n")
cat("  Subsample N:", format(N_SUBSAMPLE, big.mark = ","), "\n\n")

# ==============================================================================
# 1. Load and Prepare MEPS Data
# ==============================================================================

cat("--- STEP 1: LOADING MEPS DATA ---\n")

data_meps_raw <- read_rds(derived_path("data_meps.rds"))
cat("  Raw rows:", format(nrow(data_meps_raw), big.mark = ","), "\n")

data_meps <- data_meps_raw %>%
  drop_na(srh, age, year, wt) %>%
  mutate(
    k6 = K6SUM,
    educ_f = factor(educ_4cat, levels = 1:4,
                    labels = c("LT HS", "HS", "Some college", "Bachelor+")),
    race_f = race_includehisp_f,
    sex_f = factor(sex)
  )

cat("  After base filtering:", format(nrow(data_meps), big.mark = ","), "\n")

if (nrow(data_meps) > N_SUBSAMPLE) {
  data_meps <- data_meps %>% slice_sample(n = N_SUBSAMPLE)
  cat("  After subsampling:", format(nrow(data_meps), big.mark = ","), "\n")
}

rm(data_meps_raw); gc(verbose = FALSE)

# ==============================================================================
# 2. Missingness Report
# ==============================================================================

cat("\n--- STEP 2: MISSINGNESS ---\n")

missingness <- tibble(
  variable = c("srh", "age", "year", "wt", "race_f", "educ_f", "sex_f", "k6"),
  n_total = nrow(data_meps),
  n_valid = sapply(c("srh", "age", "year", "wt", "race_f", "educ_f", "sex_f", "k6"),
                   function(v) sum(!is.na(data_meps[[v]]))),
  n_missing = n_total - n_valid,
  pct_missing = round(n_missing / n_total * 100, 1)
)
print(as.data.frame(missingness), row.names = FALSE)
write_csv(missingness, file.path(output_dir, "missingness_report.csv"))

# ==============================================================================
# 3. Create Analysis Samples
# ==============================================================================

cat("\n--- STEP 3: SAMPLES ---\n")

meps_m1 <- data_meps %>% drop_na(srh, age, year, wt)
meps_m2 <- data_meps %>% drop_na(srh, age, year, wt, race_f, educ_f, sex_f)
meps_m3 <- data_meps %>% drop_na(srh, age, year, wt, k6)
meps_m4 <- data_meps %>% drop_na(srh, age, year, wt, race_f, educ_f, sex_f, k6)

cat("  M1:", nrow(meps_m1), "  M2:", nrow(meps_m2),
    "  M3:", nrow(meps_m3), "  M4:", nrow(meps_m4), "\n")

# ==============================================================================
# 4. Prepare BHAPC Data
# ==============================================================================

cat("\n--- STEP 4: BHAPC PREP ---\n")

bhapc_m1 <- prepare_bhapc_data(meps_m1, survey = "meps", srh_scale = 5)
bhapc_m2 <- prepare_bhapc_data(meps_m2, survey = "meps", srh_scale = 5)
bhapc_m3 <- prepare_bhapc_data(meps_m3, survey = "meps", srh_scale = 5)
bhapc_m4 <- prepare_bhapc_data(meps_m4, survey = "meps", srh_scale = 5)

saveRDS(bhapc_m1, file.path(output_dir, "bhapc_data_m1.rds"))
saveRDS(bhapc_m2, file.path(output_dir, "bhapc_data_m2.rds"))
saveRDS(bhapc_m3, file.path(output_dir, "bhapc_data_m3.rds"))
saveRDS(bhapc_m4, file.path(output_dir, "bhapc_data_m4.rds"))

# ==============================================================================
# 5. Fit Models
# ==============================================================================

cat("\n--- STEP 5: FITTING MODELS ---\n")

formula_m1 <- srh ~ age + scale(age_squared) + lnWt + (1 | period_4yr) + (1 | cohort_4yr)
formula_m2 <- srh ~ age + scale(age_squared) + lnWt + race_f + educ_f + sex_f + (1 | period_4yr) + (1 | cohort_4yr)
formula_m3 <- srh ~ age + scale(age_squared) + lnWt + k6 + (1 | period_4yr) + (1 | cohort_4yr)
formula_m4 <- srh ~ age + scale(age_squared) + lnWt + race_f + educ_f + sex_f + k6 + (1 | period_4yr) + (1 | cohort_4yr)

fit_and_save <- function(formula, data, name) {
  cat("\n  Fitting", name, "- N:", nrow(data), "\n")
  t0 <- Sys.time()
  model <- stan_lmer(formula = formula, data = data, adapt_delta = ADAPT_DELTA,
                     iter = ITER, chains = 4, cores = N_CORES, seed = SEED)
  elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
  cat("    Completed in", elapsed, "minutes\n")
  saveRDS(model, file.path(output_dir, paste0("model_", name, ".rds")))
  model
}

model_m1 <- fit_and_save(formula_m1, bhapc_m1, "m1")
model_m2 <- fit_and_save(formula_m2, bhapc_m2, "m2")
model_m3 <- fit_and_save(formula_m3, bhapc_m3, "m3")
model_m4 <- fit_and_save(formula_m4, bhapc_m4, "m4")

# ==============================================================================
# 6. Extract Results
# ==============================================================================

cat("\n--- STEP 6: EXTRACTING RESULTS ---\n")

get_pct <- function(var_df, comp) var_df$pct_of_total[var_df$component == comp]

var_m1 <- extract_variance_components(model_m1)
var_m2 <- extract_variance_components(model_m2)
var_m3 <- extract_variance_components(model_m3)
var_m4 <- extract_variance_components(model_m4)

write_csv(var_m1, file.path(output_dir, "variance_m1.csv"))
write_csv(var_m2, file.path(output_dir, "variance_m2.csv"))
write_csv(var_m3, file.path(output_dir, "variance_m3.csv"))
write_csv(var_m4, file.path(output_dir, "variance_m4.csv"))

comparison <- tibble(
  component = c("Cohort", "Period", "Residual"),
  M1_base = c(get_pct(var_m1, "cohort_4yr"), get_pct(var_m1, "period_4yr"), get_pct(var_m1, "Residual")),
  M2_demog = c(get_pct(var_m2, "cohort_4yr"), get_pct(var_m2, "period_4yr"), get_pct(var_m2, "Residual")),
  M3_k6 = c(get_pct(var_m3, "cohort_4yr"), get_pct(var_m3, "period_4yr"), get_pct(var_m3, "Residual")),
  M4_full = c(get_pct(var_m4, "cohort_4yr"), get_pct(var_m4, "period_4yr"), get_pct(var_m4, "Residual"))
)
write_csv(comparison, file.path(output_dir, "comparison_table.csv"))

cat("\nVariance Comparison (%):\n")
print(as.data.frame(comparison), row.names = FALSE)

# Fixed effects
fe_m1 <- extract_fixed_effects(model_m1)
fe_m2 <- extract_fixed_effects(model_m2)
fe_m3 <- extract_fixed_effects(model_m3)
fe_m4 <- extract_fixed_effects(model_m4)

write_csv(fe_m1, file.path(output_dir, "fixed_effects_m1.csv"))
write_csv(fe_m2, file.path(output_dir, "fixed_effects_m2.csv"))
write_csv(fe_m3, file.path(output_dir, "fixed_effects_m3.csv"))
write_csv(fe_m4, file.path(output_dir, "fixed_effects_m4.csv"))

# Convergence
check_conv <- function(model, label) {
  diag <- extract_bhapc_diagnostics(model)
  tibble(model = label, max_rhat = max(diag$Rhat, na.rm = TRUE),
         min_neff = min(diag$n_eff, na.rm = TRUE),
         converged = max(diag$Rhat, na.rm = TRUE) < 1.01 & min(diag$n_eff, na.rm = TRUE) >= 400)
}

convergence <- bind_rows(
  check_conv(model_m1, "M1"), check_conv(model_m2, "M2"),
  check_conv(model_m3, "M3"), check_conv(model_m4, "M4")
)
write_csv(convergence, file.path(output_dir, "convergence_diagnostics.csv"))

cat("\nConvergence:\n")
print(as.data.frame(convergence), row.names = FALSE)

# ==============================================================================
# 7. Figures
# ==============================================================================

cat("\n--- STEP 7: FIGURES ---\n")

re_m1 <- extract_random_effects(model_m1, bhapc_m1)
re_m2 <- extract_random_effects(model_m2, bhapc_m2)
re_m3 <- extract_random_effects(model_m3, bhapc_m3)
re_m4 <- extract_random_effects(model_m4, bhapc_m4)

model_colors <- c("M1 (base)" = "gray40", "M2 (demographics)" = "#E69F00",
                  "M3 (K6)" = "#0072B2", "M4 (full)" = "#D55E00")

cohort_comp <- bind_rows(
  re_m1$cohort_effects %>% mutate(model = "M1 (base)"),
  re_m2$cohort_effects %>% mutate(model = "M2 (demographics)"),
  re_m3$cohort_effects %>% mutate(model = "M3 (K6)"),
  re_m4$cohort_effects %>% mutate(model = "M4 (full)")
)

p_cohort <- cohort_comp %>%
  mutate(model = factor(model, levels = names(model_colors))) %>%
  ggplot(aes(x = cohort, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_manual(values = model_colors) +
  labs(title = "Cohort Effects (MEPS)", x = "Birth Cohort", y = "Cohort Effect") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave(file.path(output_dir, "cohort_effects_comparison.png"), p_cohort, width = 10, height = 7, dpi = 300)

fig3 <- create_figure3_apc_effects(model_m4, bhapc_m4, survey = "meps_m4",
                                    output_path = file.path(output_dir, "figure3_m4.png"))

cat("  Figures saved\n")

# ==============================================================================
# Summary
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("MEPS COMPLETE\n")
cohort_m1 <- get_pct(var_m1, "cohort_4yr")
cohort_m4 <- get_pct(var_m4, "cohort_4yr")
cat(sprintf("Cohort variance: M1=%.2f%% -> M4=%.2f%% (%.0f%% reduction)\n",
            cohort_m1, cohort_m4, (1 - cohort_m4/cohort_m1) * 100))
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
