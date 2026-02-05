# ==============================================================================
# 15_bhapc_gss_covariates.R
# BHAPC Analysis for GSS SRH with Covariates
# Purpose: Test how much of the cohort effect is explained by compositional
#          changes in demographics and subjective well-being measures
#
# Models:
#   0. Unadjusted (loaded from existing output)
#   1. Demographics: educ + race + sex
#   2. All (excl. satjob): happy + satfin + absingle + educ + race + sex
#   3. All (incl. satjob): happy + satfin + satjob + absingle + educ + race + sex
#
# Data source: gssr package (GSS Cumulative Data File) — needed for covariates
#              not present in the essential datasets
#
# Usage:
#   Rscript R/scripts/15_bhapc_gss_covariates.R 2>&1 | tee output/bhapc_gss_covariates/run.log
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("BHAPC GSS WITH COVARIATES\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# 0. Setup
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(broom.mixed)
  library(patchwork)
  library(gssr)
})

# Source shared functions
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

set.seed(20260203)

# Output directory
output_dir <- here("output", "bhapc_gss_covariates")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Stan settings
N_CORES  <- min(parallel::detectCores(), 10)
ITER     <- 6000
ADAPT_DELTA <- 0.998
SEED     <- 20260203

cat("Cores:", N_CORES, "\n")
cat("Iterations:", ITER, "\n")
cat("adapt_delta:", ADAPT_DELTA, "\n\n")

# ==============================================================================
# 1. Load & Wrangle GSS from gssr
# ==============================================================================

cat("--- STEP 1: LOADING & WRANGLING GSS DATA ---\n")

data(gss_all)

# Select and recode variables
gss_raw <- gss_all %>%
  select(year, age, health, happy, satfin, satjob, absingle, degree, race, sex,
         wtssps) %>%
  # Convert haven_labelled to plain numeric

  mutate(across(everything(), as.numeric)) %>%
  rename(wt = wtssps)

cat("  Raw GSS rows:", format(nrow(gss_raw), big.mark = ","), "\n")

# --- Recode SRH ---
# Raw GSS health: 1=Excellent, 2=Good, 3=Fair, 4=Poor
# Target: 1=Poor ... 4=Excellent (higher = better)
gss_raw <- gss_raw %>%
  mutate(srh = ifelse(health %in% 1:4, 5L - as.integer(health), NA_integer_))

# --- Recode covariates ---
gss_raw <- gss_raw %>%
  mutate(
    # happy: raw 1=Very happy, 2=Pretty happy, 3=Not too happy
    # Recode: higher = happier -> 4 - happy => 1=Not too happy, 2=Pretty happy, 3=Very happy
    happy_r = ifelse(happy %in% 1:3, 4L - as.integer(happy), NA_integer_),

    # satfin: raw 1=Satisfied, 2=More or less, 3=Not at all
    # Recode: higher = more satisfied -> 4 - satfin
    satfin_r = ifelse(satfin %in% 1:3, 4L - as.integer(satfin), NA_integer_),

    # satjob: raw 1=Very satisfied, 2=Mod satisfied, 3=Little dissatisfied, 4=Very dissatisfied
    # Recode: higher = more satisfied -> 5 - satjob
    satjob_r = ifelse(satjob %in% 1:4, 5L - as.integer(satjob), NA_integer_),

    # absingle: raw 1=yes, 2=no -> binary 0/1
    absingle_r = case_when(
      absingle == 1 ~ 1L,
      absingle == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),

    # education: degree 0=LT HS, 1=HS, 2=Associate, 3=Bachelor, 4=Graduate
    # Crosswalk to 4 categories: 1=LT HS, 2=HS, 3=Some college/Assoc, 4=Bachelor+
    educ_4cat = case_when(
      degree == 0 ~ 1L,
      degree == 1 ~ 2L,
      degree == 2 ~ 3L,
      degree %in% 3:4 ~ 4L,
      TRUE ~ NA_integer_
    ),
    educ_f = factor(educ_4cat,
                    levels = 1:4,
                    labels = c("LT HS", "HS", "Some college", "Bachelor+")),

    # race: 1=White, 2=Black, 3=Other
    race_f = factor(
      ifelse(race %in% 1:3, race, NA_real_),
      levels = 1:3,
      labels = c("White", "Black", "Other")
    ),

    # sex: 1=Male, 2=Female
    sex_f = factor(
      ifelse(sex %in% 1:2, sex, NA_real_),
      levels = 1:2,
      labels = c("Male", "Female")
    )
  )

# Filter to valid base observations (srh, age, year, wt)
gss_base <- gss_raw %>%
  filter(!is.na(srh), !is.na(age), !is.na(year), !is.na(wt), wt > 0)

cat("  Valid base observations (srh + age + year + wt):",
    format(nrow(gss_base), big.mark = ","), "\n")
cat("  Year range:", min(gss_base$year), "-", max(gss_base$year), "\n")
cat("  Mean SRH:", round(mean(gss_base$srh), 3), "\n")

# ==============================================================================
# 2. Missingness report
# ==============================================================================

cat("\n--- STEP 2: MISSINGNESS REPORT ---\n")

covariate_vars <- c("happy_r", "satfin_r", "satjob_r", "absingle_r",
                     "educ_f", "race_f", "sex_f")

missingness <- tibble(
  variable = c("srh", "age", "year", "wt", covariate_vars),
  n_total = nrow(gss_raw),
  n_valid = sapply(c("srh", "age", "year", "wt", covariate_vars),
                   function(v) sum(!is.na(gss_raw[[v]]))),
  n_missing = n_total - n_valid,
  pct_missing = round(n_missing / n_total * 100, 1)
)

cat("\n")
print(as.data.frame(missingness), row.names = FALSE)

write_csv(missingness, file.path(output_dir, "missingness_report.csv"))

# ==============================================================================
# 3. Prepare BHAPC data & create 3 analysis samples
# ==============================================================================

cat("\n--- STEP 3: BHAPC DATA PREPARATION ---\n")

# Prepare base BHAPC variables (period_4yr, cohort_4yr, age_squared, lnWt)
bhapc_all <- prepare_bhapc_data(
  gss_base,
  survey   = "gss",
  age_min  = 18,
  age_max  = 89,
  srh_scale = 4
)

# Carry covariates through (they were already in gss_base)
# prepare_bhapc_data preserves extra columns

# --- Sample 1: Demographics (educ + race + sex) ---
bhapc_demog <- bhapc_all %>%
  drop_na(educ_f, race_f, sex_f)

# --- Sample 2: All covariates except satjob ---
bhapc_all_excl <- bhapc_all %>%
  drop_na(happy_r, satfin_r, absingle_r, educ_f, race_f, sex_f)

# --- Sample 3: All covariates including satjob (employed subsample) ---
bhapc_all_incl <- bhapc_all %>%
  drop_na(happy_r, satfin_r, satjob_r, absingle_r, educ_f, race_f, sex_f)

# Sample diagnostics
sample_diagnostics <- tibble(
  sample = c("Full BHAPC", "Demographics", "All (excl satjob)", "All (incl satjob)"),
  n = c(nrow(bhapc_all), nrow(bhapc_demog), nrow(bhapc_all_excl), nrow(bhapc_all_incl)),
  mean_srh = c(mean(bhapc_all$srh), mean(bhapc_demog$srh),
               mean(bhapc_all_excl$srh), mean(bhapc_all_incl$srh)),
  mean_age = c(mean(bhapc_all$age), mean(bhapc_demog$age),
               mean(bhapc_all_excl$age), mean(bhapc_all_incl$age)),
  year_min = c(min(bhapc_all$year), min(bhapc_demog$year),
               min(bhapc_all_excl$year), min(bhapc_all_incl$year)),
  year_max = c(max(bhapc_all$year), max(bhapc_demog$year),
               max(bhapc_all_excl$year), max(bhapc_all_incl$year)),
  n_periods = c(length(unique(bhapc_all$period_4yr)),
                length(unique(bhapc_demog$period_4yr)),
                length(unique(bhapc_all_excl$period_4yr)),
                length(unique(bhapc_all_incl$period_4yr))),
  n_cohorts = c(length(unique(bhapc_all$cohort_4yr)),
                length(unique(bhapc_demog$cohort_4yr)),
                length(unique(bhapc_all_excl$cohort_4yr)),
                length(unique(bhapc_all_incl$cohort_4yr)))
)

cat("\n  Sample diagnostics:\n")
print(as.data.frame(sample_diagnostics), row.names = FALSE)

write_csv(sample_diagnostics, file.path(output_dir, "sample_diagnostics.csv"))

# Save analysis datasets
saveRDS(bhapc_demog,    file.path(output_dir, "bhapc_data_demog.rds"))
saveRDS(bhapc_all_excl, file.path(output_dir, "bhapc_data_all.rds"))
saveRDS(bhapc_all_incl, file.path(output_dir, "bhapc_data_all_satjob.rds"))

# ==============================================================================
# 4. Model 1: Demographics only
# ==============================================================================

cat("\n--- STEP 4: MODEL 1 — DEMOGRAPHICS (educ + race + sex) ---\n")

formula_demog <- srh ~ age + scale(age_squared) + lnWt +
  educ_f + race_f + sex_f +
  (1 | period_4yr) + (1 | cohort_4yr)

cat("  Formula:", deparse(formula_demog), "\n")
cat("  N:", format(nrow(bhapc_demog), big.mark = ","), "\n")

t0 <- Sys.time()

model_demog <- stan_lmer(
  formula   = formula_demog,
  data      = bhapc_demog,
  adapt_delta = ADAPT_DELTA,
  iter      = ITER,
  chains    = 4,
  cores     = N_CORES,
  seed      = SEED
)

cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_demog, file.path(output_dir, "model_demog.rds"))

# ==============================================================================
# 5. Model 2: All covariates except satjob
# ==============================================================================

cat("\n--- STEP 5: MODEL 2 — ALL (excl. satjob) ---\n")

formula_all <- srh ~ age + scale(age_squared) + lnWt +
  happy_r + satfin_r + absingle_r +
  educ_f + race_f + sex_f +
  (1 | period_4yr) + (1 | cohort_4yr)

cat("  Formula:", deparse(formula_all), "\n")
cat("  N:", format(nrow(bhapc_all_excl), big.mark = ","), "\n")

t0 <- Sys.time()

model_all <- stan_lmer(
  formula   = formula_all,
  data      = bhapc_all_excl,
  adapt_delta = ADAPT_DELTA,
  iter      = ITER,
  chains    = 4,
  cores     = N_CORES,
  seed      = SEED
)

cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_all, file.path(output_dir, "model_all.rds"))

# ==============================================================================
# 6. Model 3: All covariates including satjob
# ==============================================================================

cat("\n--- STEP 6: MODEL 3 — ALL (incl. satjob) ---\n")

formula_all_satjob <- srh ~ age + scale(age_squared) + lnWt +
  happy_r + satfin_r + satjob_r + absingle_r +
  educ_f + race_f + sex_f +
  (1 | period_4yr) + (1 | cohort_4yr)

cat("  Formula:", deparse(formula_all_satjob), "\n")
cat("  N:", format(nrow(bhapc_all_incl), big.mark = ","), "\n")

t0 <- Sys.time()

model_all_satjob <- stan_lmer(
  formula   = formula_all_satjob,
  data      = bhapc_all_incl,
  adapt_delta = ADAPT_DELTA,
  iter      = ITER,
  chains    = 4,
  cores     = N_CORES,
  seed      = SEED
)

cat("  Completed in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
saveRDS(model_all_satjob, file.path(output_dir, "model_all_satjob.rds"))

# ==============================================================================
# 7. Load unadjusted baseline from existing output
# ==============================================================================

cat("\n--- STEP 7: LOADING UNADJUSTED BASELINE ---\n")

unadj_var <- read_csv(
  here("output", "bhapc_parallel", "gss", "gss_variance_decomposition.csv"),
  show_col_types = FALSE
)

unadj_model_result <- readRDS(
  here("output", "bhapc_parallel", "gss", "gss_bhapc_model.rds")
)

# The pipeline saves either the model directly or a list with $model
unadj_model <- if (inherits(unadj_model_result, "stanreg")) {
  unadj_model_result
} else {
  unadj_model_result$model
}

unadj_bhapc_df <- readRDS(
  here("output", "bhapc_parallel", "gss", "gss_bhapc_data.rds")
)

cat("  Unadjusted cohort %:", round(
  unadj_var$pct_of_total[unadj_var$component == "cohort_4yr"], 2), "\n")
cat("  Unadjusted period %:", round(
  unadj_var$pct_of_total[unadj_var$component == "period_4yr"], 2), "\n")
cat("  Unadjusted N:", format(nrow(unadj_bhapc_df), big.mark = ","), "\n")

# ==============================================================================
# 8. Extract variance components
# ==============================================================================

cat("\n--- STEP 8: VARIANCE COMPONENTS ---\n")

var_demog      <- extract_variance_components(model_demog)
var_all        <- extract_variance_components(model_all)
var_all_satjob <- extract_variance_components(model_all_satjob)

# Save individual variance tables
write_csv(var_demog,      file.path(output_dir, "variance_demog.csv"))
write_csv(var_all,        file.path(output_dir, "variance_all.csv"))
write_csv(var_all_satjob, file.path(output_dir, "variance_all_satjob.csv"))

# Print comparison
get_pct <- function(var_df, comp) {
  var_df$pct_of_total[var_df$component == comp]
}

comparison_table <- tibble(
  component = c("Cohort", "Period", "Residual"),
  unadjusted_pct = c(
    get_pct(unadj_var, "cohort_4yr"),
    get_pct(unadj_var, "period_4yr"),
    get_pct(unadj_var, "Residual")
  ),
  demographics_pct = c(
    get_pct(var_demog, "cohort_4yr"),
    get_pct(var_demog, "period_4yr"),
    get_pct(var_demog, "Residual")
  ),
  all_excl_satjob_pct = c(
    get_pct(var_all, "cohort_4yr"),
    get_pct(var_all, "period_4yr"),
    get_pct(var_all, "Residual")
  ),
  all_incl_satjob_pct = c(
    get_pct(var_all_satjob, "cohort_4yr"),
    get_pct(var_all_satjob, "period_4yr"),
    get_pct(var_all_satjob, "Residual")
  )
)

# Add sample sizes as a row
comparison_table <- bind_rows(
  comparison_table,
  tibble(
    component = "N",
    unadjusted_pct      = nrow(unadj_bhapc_df),
    demographics_pct     = nrow(bhapc_demog),
    all_excl_satjob_pct  = nrow(bhapc_all_excl),
    all_incl_satjob_pct  = nrow(bhapc_all_incl)
  )
)

cat("\n  Variance % Comparison:\n")
print(as.data.frame(comparison_table), row.names = FALSE)

write_csv(comparison_table, file.path(output_dir, "comparison_table.csv"))

# ==============================================================================
# 9. Extract fixed effects
# ==============================================================================

cat("\n--- STEP 9: FIXED EFFECTS ---\n")

fe_demog      <- extract_fixed_effects(model_demog)
fe_all        <- extract_fixed_effects(model_all)
fe_all_satjob <- extract_fixed_effects(model_all_satjob)

write_csv(fe_demog,      file.path(output_dir, "fixed_effects_demog.csv"))
write_csv(fe_all,        file.path(output_dir, "fixed_effects_all.csv"))
write_csv(fe_all_satjob, file.path(output_dir, "fixed_effects_all_satjob.csv"))

cat("\n  Fixed effects — Demographics model:\n")
print(as.data.frame(fe_demog), row.names = FALSE)

cat("\n  Fixed effects — All (excl. satjob):\n")
print(as.data.frame(fe_all), row.names = FALSE)

cat("\n  Fixed effects — All (incl. satjob):\n")
print(as.data.frame(fe_all_satjob), row.names = FALSE)

# ==============================================================================
# 10. Extract & compare random effects
# ==============================================================================

cat("\n--- STEP 10: RANDOM EFFECTS COMPARISON ---\n")

re_unadj      <- extract_random_effects(unadj_model, unadj_bhapc_df)
re_demog      <- extract_random_effects(model_demog, bhapc_demog)
re_all        <- extract_random_effects(model_all, bhapc_all_excl)
re_all_satjob <- extract_random_effects(model_all_satjob, bhapc_all_incl)

# Combine cohort effects for comparison
cohort_comparison <- bind_rows(
  re_unadj$cohort_effects      %>% mutate(model = "Unadjusted"),
  re_demog$cohort_effects      %>% mutate(model = "Demographics"),
  re_all$cohort_effects        %>% mutate(model = "All (excl satjob)"),
  re_all_satjob$cohort_effects %>% mutate(model = "All (incl satjob)")
)

# Combine period effects for comparison
period_comparison <- bind_rows(
  re_unadj$period_effects      %>% mutate(model = "Unadjusted"),
  re_demog$period_effects      %>% mutate(model = "Demographics"),
  re_all$period_effects        %>% mutate(model = "All (excl satjob)"),
  re_all_satjob$period_effects %>% mutate(model = "All (incl satjob)")
)

# ==============================================================================
# 11. Convergence checks
# ==============================================================================

cat("\n--- STEP 11: CONVERGENCE DIAGNOSTICS ---\n")

check_convergence <- function(model, label) {
  diag <- extract_bhapc_diagnostics(model)
  max_rhat <- max(diag$Rhat, na.rm = TRUE)
  min_neff <- min(diag$n_eff, na.rm = TRUE)
  converged <- max_rhat < 1.01 & min_neff >= 400
  cat(sprintf("  %-25s  Rhat_max=%.4f  n_eff_min=%5.0f  %s\n",
              label, max_rhat, min_neff,
              ifelse(converged, "OK", "*** CHECK ***")))
  tibble(model = label, max_rhat = max_rhat, min_neff = min_neff,
         converged = converged)
}

convergence <- bind_rows(
  check_convergence(model_demog,      "Demographics"),
  check_convergence(model_all,        "All (excl satjob)"),
  check_convergence(model_all_satjob, "All (incl satjob)")
)

write_csv(convergence, file.path(output_dir, "convergence_diagnostics.csv"))

# ==============================================================================
# 12. Figures
# ==============================================================================

cat("\n--- STEP 12: GENERATING FIGURES ---\n")

# --- 12a. Variance comparison bar chart ---

var_plot_df <- comparison_table %>%
  filter(component %in% c("Cohort", "Period", "Residual")) %>%
  pivot_longer(-component, names_to = "model_label", values_to = "pct") %>%
  mutate(
    model_label = case_when(
      model_label == "unadjusted_pct"      ~ "Unadjusted",
      model_label == "demographics_pct"     ~ "Demographics",
      model_label == "all_excl_satjob_pct"  ~ "All (excl satjob)",
      model_label == "all_incl_satjob_pct"  ~ "All (incl satjob)"
    ),
    model_label = factor(model_label, levels = c(
      "Unadjusted", "Demographics", "All (excl satjob)", "All (incl satjob)"
    )),
    component = factor(component, levels = c("Cohort", "Period", "Residual"))
  )

# Two-panel version: cohort/period separate from residual for readability
p_var_cp <- var_plot_df %>%
  filter(component != "Residual") %>%
  ggplot(aes(x = model_label, y = pct, fill = component)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", pct)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Cohort" = "#CC79A7", "Period" = "#009E73")) +
  labs(
    title = "Variance Explained by Cohort and Period",
    subtitle = "GSS BHAPC models with increasing covariate adjustment",
    x = NULL, y = "% of Total Variance", fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(output_dir, "variance_comparison.png"),
       p_var_cp, width = 8, height = 6, dpi = 300)

cat("  Saved variance_comparison.png\n")

# --- 12b. Cohort effects overlay ---

model_colors <- c(
  "Unadjusted"         = "gray40",
  "Demographics"       = "#E69F00",
  "All (excl satjob)"  = "#0072B2",
  "All (incl satjob)"  = "#D55E00"
)

p_cohort <- cohort_comparison %>%
  mutate(model = factor(model, levels = names(model_colors))) %>%
  ggplot(aes(x = cohort, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                width = 2, linewidth = 0.4, alpha = 0.5) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Cohort Random Effects Across Models",
    subtitle = "GSS BHAPC — 90% credible intervals",
    x = "Birth Cohort", y = "Cohort Random Effect (SRH units)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "cohort_effects_comparison.png"),
       p_cohort, width = 10, height = 7, dpi = 300)

cat("  Saved cohort_effects_comparison.png\n")

# --- 12c. Period effects overlay ---

p_period <- period_comparison %>%
  mutate(model = factor(model, levels = names(model_colors))) %>%
  ggplot(aes(x = period, y = estimate, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 3, position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                width = 0.6, linewidth = 0.6,
                position = position_dodge(width = 0.8)) +
  scale_color_manual(values = model_colors) +
  labs(
    title = "Period Random Effects Across Models",
    subtitle = "GSS BHAPC — 90% credible intervals",
    x = "Period (start year)", y = "Period Random Effect (SRH units)",
    color = "Model"
  ) +
  scale_x_continuous(breaks = unique(period_comparison$period)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "period_effects_comparison.png"),
       p_period, width = 10, height = 7, dpi = 300)

cat("  Saved period_effects_comparison.png\n")

# --- 12d. Figure 3 APC panels for each covariate model ---

fig3_demog <- create_figure3_apc_effects(
  model_demog, bhapc_demog,
  survey = "gss_demog",
  output_path = file.path(output_dir, "figure3_model_demog.png")
)

fig3_all <- create_figure3_apc_effects(
  model_all, bhapc_all_excl,
  survey = "gss_all",
  output_path = file.path(output_dir, "figure3_model_all.png")
)

fig3_all_satjob <- create_figure3_apc_effects(
  model_all_satjob, bhapc_all_incl,
  survey = "gss_all_satjob",
  output_path = file.path(output_dir, "figure3_model_all_satjob.png")
)

cat("  Saved figure3 APC panels\n")

# ==============================================================================
# 13. Interpretation summary
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("INTERPRETATION SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cohort_unadj   <- get_pct(unadj_var, "cohort_4yr")
cohort_demog    <- get_pct(var_demog, "cohort_4yr")
cohort_all      <- get_pct(var_all, "cohort_4yr")
cohort_all_sj   <- get_pct(var_all_satjob, "cohort_4yr")

cat(sprintf("Cohort variance %%:\n"))
cat(sprintf("  Unadjusted:          %.2f%%\n", cohort_unadj))
cat(sprintf("  + Demographics:      %.2f%%  (%.0f%% reduction)\n",
            cohort_demog,
            (1 - cohort_demog / cohort_unadj) * 100))
cat(sprintf("  + All (excl satjob): %.2f%%  (%.0f%% reduction)\n",
            cohort_all,
            (1 - cohort_all / cohort_unadj) * 100))
cat(sprintf("  + All (incl satjob): %.2f%%  (%.0f%% reduction)\n",
            cohort_all_sj,
            (1 - cohort_all_sj / cohort_unadj) * 100))

cat("\nKey covariate effects (All excl satjob model):\n")
fe_key <- fe_all %>% filter(term %in% c("happy_r", "satfin_r", "absingle_r",
                                          "educ_fHS", "educ_fSome college",
                                          "educ_fBachelor+",
                                          "race_fBlack", "race_fOther",
                                          "sex_fFemale"))
for (i in seq_len(nrow(fe_key))) {
  cat(sprintf("  %-25s  %.4f  [%.4f, %.4f]\n",
              fe_key$term[i], fe_key$estimate[i],
              fe_key$ci_lower_90[i], fe_key$ci_upper_90[i]))
}

cat("\nSample sizes:\n")
cat(sprintf("  Unadjusted:          %s\n", format(nrow(unadj_bhapc_df), big.mark = ",")))
cat(sprintf("  Demographics:        %s\n", format(nrow(bhapc_demog), big.mark = ",")))
cat(sprintf("  All (excl satjob):   %s\n", format(nrow(bhapc_all_excl), big.mark = ",")))
cat(sprintf("  All (incl satjob):   %s\n", format(nrow(bhapc_all_incl), big.mark = ",")))

cat("\n", strrep("=", 70), "\n")
cat("COMPLETE\n")
cat("Output directory:", output_dir, "\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
