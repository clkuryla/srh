# ==============================================================================
# 11b_bhapc_gss_2004_2024.R
# BHAPC Analysis for GSS restricted to 2004-2024
# Purpose: Test if cohort effects persist with only recent 20 years of data
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("BHAPC GSS RESTRICTED (2004-2024)\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n\n")

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
})

# Source functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))
source(here("R", "functions", "bhapc_table_generation.R"))
source(here("R", "functions", "bhapc_figure_generation.R"))

# Set DATA_DEPOT if not set
if (Sys.getenv("DATA_DEPOT") == "") {
  Sys.setenv(DATA_DEPOT = "/home/ubuntu/data_depot")
}

# Output directory
output_dir <- here("output", "bhapc_parallel", "restricted_gss_2004_2024")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

# Configuration
YEAR_MIN <- 2004
YEAR_MAX <- 2024
N_CORES <- 10
ITER <- 6000
ADAPT_DELTA <- 0.998

cat("Year restriction:", YEAR_MIN, "-", YEAR_MAX, "\n")
cat("Iterations:", ITER, "\n")
cat("Cores:", N_CORES, "\n\n")

# ==============================================================================
# Load and filter data
# ==============================================================================

cat("--- STEP 1: DATA LOADING & PREPARATION ---\n")

data_path <- file.path(
  Sys.getenv("DATA_DEPOT"),
  "_derived", "srh_project", "essential_datasets",
  "data_essential_gss.rds"
)

df_full <- readRDS(data_path) %>%
  drop_na(srh, age, year, wt)

df <- df_full %>%
  filter(year >= YEAR_MIN, year <= YEAR_MAX)

cat("  Original GSS N (all years):", format(nrow(df_full), big.mark = ","), "\n")
cat("  Filtered N (", YEAR_MIN, "-", YEAR_MAX, "):", format(nrow(df), big.mark = ","), "\n")
cat("  Year range:", min(df$year), "-", max(df$year), "\n")

# Prepare BHAPC data
bhapc_df <- prepare_bhapc_data(
  df,
  survey = "gss",
  age_min = 18,
  age_max = 89,
  srh_scale = 4
)

cat("  BHAPC N:", format(nrow(bhapc_df), big.mark = ","), "\n")
cat("  Periods:", paste(sort(unique(bhapc_df$period_4yr)), collapse = ", "), "\n")
cat("  Cohorts:", length(unique(bhapc_df$cohort_4yr)), "\n\n")

# Save prepared data
saveRDS(bhapc_df, file.path(output_dir, "gss_restricted_bhapc_data.rds"))

# ==============================================================================
# Fit main BHAPC model
# ==============================================================================

cat("--- STEP 2: MAIN BHAPC MODEL ---\n")
cat("  Formula: srh ~ age + scale(age_squared) + lnWt + (1|period_4yr) + (1|cohort_4yr)\n")
cat("  iter =", ITER, ", adapt_delta =", ADAPT_DELTA, "\n")

model_start <- Sys.time()

model_result <- fit_bhapc_model(
  bhapc_df,
  outcome = "srh",
  adapt_delta = ADAPT_DELTA,
  iter = ITER,
  chains = 4,
  cores = N_CORES,
  seed = 20260205
)

model_elapsed <- difftime(Sys.time(), model_start, units = "mins")
cat("  Model completed in", round(model_elapsed, 1), "minutes\n\n")

# Save model
saveRDS(model_result, file.path(output_dir, "gss_restricted_bhapc_model.rds"))

# ==============================================================================
# Extract results
# ==============================================================================

cat("--- STEP 3: EXTRACTING RESULTS ---\n")

# Variance decomposition
variance_df <- extract_variance_components(model_result$model)
write.csv(variance_df, file.path(output_dir, "gss_restricted_variance_decomposition.csv"), row.names = FALSE)

cat("\n  Variance Decomposition:\n")
for (i in 1:nrow(variance_df)) {
  cat("    ", variance_df$component[i], ": ",
      round(variance_df$variance[i], 4), " (",
      round(variance_df$pct_of_total[i], 1), "%)\n")
}

# Diagnostics
cat("\n  Diagnostics:\n")
cat("    Max Rhat:", round(max(model_result$diagnostics$Rhat, na.rm = TRUE), 3), "\n")
cat("    Min n_eff:", round(min(model_result$diagnostics$n_eff, na.rm = TRUE), 0), "\n")
cat("    Converged:", max(model_result$diagnostics$Rhat, na.rm = TRUE) < 1.01, "\n")

write.csv(model_result$diagnostics, file.path(output_dir, "gss_restricted_diagnostics.csv"), row.names = FALSE)

# Table 2
table2 <- create_table2(
  model_result$model,
  bhapc_df,
  survey = "gss_2004_2024",
  output_path = file.path(output_dir, "table2_gss_restricted.csv")
)

# ==============================================================================
# Interaction model
# ==============================================================================

cat("\n--- STEP 4: INTERACTION MODEL (AGE × PERIOD) ---\n")

# Compute descriptive gradients
age_groups <- sort(unique(bhapc_df$age_group))
youngest_group <- age_groups[1]
oldest_group <- "85-89"
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

write.csv(gradient_by_period, file.path(output_dir, "gradient_by_period_gss_restricted.csv"), row.names = FALSE)

# Fit interaction model
mean_age <- mean(bhapc_df$age)
bhapc_df <- bhapc_df %>%
  mutate(age_centered = age - mean_age)

cat("  Formula: srh ~ age_centered * period_4yr + lnWt + (1|cohort_4yr)\n")

interaction_start <- Sys.time()

interaction_model <- rstanarm::stan_lmer(
  srh ~ age_centered * period_4yr + lnWt + (1|cohort_4yr),
  data = bhapc_df,
  adapt_delta = 0.95,
  iter = 2000,
  chains = 4,
  cores = N_CORES,
  seed = 20260205
)

interaction_elapsed <- difftime(Sys.time(), interaction_start, units = "mins")
cat("  Interaction model completed in", round(interaction_elapsed, 1), "minutes\n")

saveRDS(interaction_model, file.path(output_dir, "gss_restricted_interaction_model.rds"))

# Extract interaction effects
posterior <- as.matrix(interaction_model)
interaction_cols <- grep("age_centered:period_4yr", colnames(posterior), value = TRUE)

age_main <- mean(posterior[, "age_centered"])
age_main_ci <- quantile(posterior[, "age_centered"], c(0.10, 0.90))

if (length(interaction_cols) > 0) {
  interaction_effects <- data.frame(
    period = gsub("age_centered:period_4yr", "", interaction_cols),
    estimate = sapply(interaction_cols, function(col) mean(posterior[, col])),
    ci_10 = sapply(interaction_cols, function(col) quantile(posterior[, col], 0.10)),
    ci_90 = sapply(interaction_cols, function(col) quantile(posterior[, col], 0.90)),
    stringsAsFactors = FALSE
  )
  interaction_effects$significant <- ifelse(
    interaction_effects$ci_10 > 0 | interaction_effects$ci_90 < 0, "*", ""
  )

  ref_period <- as.character(min(as.numeric(unique(bhapc_df$period_4yr))))

  interaction_summary <- data.frame(
    period = c(ref_period, interaction_effects$period),
    age_slope = c(age_main, age_main + interaction_effects$estimate),
    interaction = c(0, interaction_effects$estimate),
    ci_10 = c(age_main_ci[1], interaction_effects$ci_10),
    ci_90 = c(age_main_ci[2], interaction_effects$ci_90),
    significant = c("ref", interaction_effects$significant)
  )
} else {
  ref_period <- as.character(min(as.numeric(unique(bhapc_df$period_4yr))))
  interaction_summary <- data.frame(
    period = ref_period,
    age_slope = age_main,
    interaction = 0,
    ci_10 = age_main_ci[1],
    ci_90 = age_main_ci[2],
    significant = "ref"
  )
}

write.csv(interaction_summary, file.path(output_dir, "age_period_interaction_gss_restricted.csv"), row.names = FALSE)

cat("\n  Age×Period Interaction Summary:\n")
print(interaction_summary)

# ==============================================================================
# Generate figures
# ==============================================================================

cat("\n--- STEP 5: GENERATING FIGURES ---\n")

# Figure 2: Descriptive means
fig2 <- create_figure2_descriptive(
  bhapc_df,
  survey = "gss_2004_2024",
  output_path = file.path(output_dir, "figure2_gss_restricted.png")
)

# Figure 3: APC effects
fig3 <- create_figure3_apc_effects(
  model_result$model,
  bhapc_df,
  survey = "gss_2004_2024",
  output_path = file.path(output_dir, "figure3_gss_restricted.png")
)

# Variance chart
var_chart <- create_variance_chart(variance_df, survey = "gss_2004_2024")
ggsave(file.path(output_dir, "variance_chart_gss_restricted.png"),
       var_chart, width = 6, height = 5, dpi = 300)

cat("  Figures saved\n")

# ==============================================================================
# Comparison with full GSS
# ==============================================================================

cat("\n--- COMPARISON WITH FULL GSS ---\n")

full_gss_var <- read.csv(here("output", "bhapc_parallel", "gss", "gss_variance_decomposition.csv"))

comparison <- data.frame(
  Component = c("Period", "Cohort", "Residual"),
  `Full GSS (1971-2024)` = c(
    full_gss_var$pct_of_total[full_gss_var$component == "period_4yr"],
    full_gss_var$pct_of_total[full_gss_var$component == "cohort_4yr"],
    full_gss_var$pct_of_total[full_gss_var$component == "Residual"]
  ),
  `Restricted GSS (2004-2024)` = c(
    variance_df$pct_of_total[variance_df$component == "period_4yr"],
    variance_df$pct_of_total[variance_df$component == "cohort_4yr"],
    variance_df$pct_of_total[variance_df$component == "Residual"]
  ),
  check.names = FALSE
)

cat("\n  Variance % Comparison:\n")
print(comparison, row.names = FALSE)

write.csv(comparison, file.path(output_dir, "comparison_full_vs_restricted.csv"), row.names = FALSE)

# ==============================================================================
# Done
# ==============================================================================

total_elapsed <- difftime(Sys.time(), model_start, units = "mins")

cat("\n", strrep("=", 70), "\n")
cat("COMPLETE: GSS RESTRICTED (2004-2024)\n")
cat("Total time:", round(total_elapsed, 1), "minutes\n")
cat("Output directory:", output_dir, "\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
