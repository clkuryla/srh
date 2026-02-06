# ==============================================================================
# 16_sanity_check_nhis.R
# Sanity Check: NHIS SRH, Education, and K6 Distress BHAPC
# Benchmark cohort effect analysis
# Author: Christine Lucille Kuryla
#
# This script runs the BHAPC model on SRH, education, and K6 distress
# to benchmark the cohort effect
#
# Expected outcomes:
# - SRH: Modest cohort effect (~1-3%)
# - Education: Clear upward cohort trend (known secular increase, ~5-8%)
# - K6: Modest cohort effect, similar to SRH (~1-3%)
#
# NOTE: NHIS is large, so we subsample to ~100K observations for tractability
# ==============================================================================

library(tidyverse)
library(rstanarm)
library(broom.mixed)
library(here)

# Source project paths and functions
source(here::here("R/paths.R"))
source(here::here("R/functions/bhapc_model_fitting.R"))
source(here::here("R/functions/bhapc_figure_generation.R"))
source(here::here("R/functions/subsample_survey.R"))

# ==============================================================================
# Setup
# ==============================================================================

# Create output directory
output_dir <- here::here("output/sanity_check_nhis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

# ==============================================================================
# Load NHIS data
# ==============================================================================

message("Loading NHIS data from data_nhis.rds...")

data_nhis_full <- read_rds(derived_path("data_nhis.rds"))

message("Full NHIS data loaded: ", format(nrow(data_nhis_full), big.mark = ","), " observations")

# Check available columns
message("\nKey columns available:")
message("  - srh: ", sum(!is.na(data_nhis_full$srh)), " valid")
message("  - educ_4cat: ", sum(!is.na(data_nhis_full$educ_4cat)), " valid")
message("  - age: ", sum(!is.na(data_nhis_full$age)), " valid")
message("  - year: ", sum(!is.na(data_nhis_full$year)), " valid")
message("  - wt: ", sum(!is.na(data_nhis_full$wt)), " valid")

# ==============================================================================
# Helper Functions (adapted from 13_sanity_check_gss.R)
# ==============================================================================

#' Map survey years to 4-year periods
map_to_period_4yr <- function(year, survey = "nhis") {
  survey <- tolower(survey)
  # For annual surveys, use simple 4-year windows
  # 1999-2002 -> 1999, 2003-2006 -> 2003, etc.
  period <- as.integer(floor((year - 1999) / 4) * 4 + 1999)
  # Handle pre-1999 years if present
  period <- ifelse(year < 1999,
                   as.integer(floor((year - 1995) / 4) * 4 + 1995),
                   period)
  period
}

#' Create mixed age groups (4-yr young, 5-yr older)
create_age_groups_mixed <- function(age) {
  age_group <- case_when(
    age >= 18 & age <= 21 ~ "18-21",
    age >= 22 & age <= 25 ~ "22-25",
    age >= 26 & age <= 29 ~ "26-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    TRUE ~ NA_character_
  )

  levels <- c("18-21", "22-25", "26-29", "30-34", "35-39", "40-44",
              "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
              "75-79", "80-84", "85-89")

  factor(age_group, levels = levels)
}

#' Get age group midpoint for cohort calculation
get_age_midpoint <- function(age_group) {
  bounds <- strsplit(as.character(age_group), "-")
  sapply(bounds, function(b) {
    if (length(b) == 2) {
      (as.numeric(b[1]) + as.numeric(b[2])) / 2
    } else {
      NA_real_
    }
  })
}

#' Prepare data for BHAPC analysis (generic version for any outcome)
prepare_sanity_check_data <- function(df, outcome_var, survey = "nhis") {

  stopifnot(outcome_var %in% names(df))

  df %>%
    filter(!is.na(.data[[outcome_var]]), !is.na(age), !is.na(wt)) %>%
    filter(age >= 18, age <= 89) %>%
    mutate(
      period_4yr = map_to_period_4yr(year, survey),
      age_group = create_age_groups_mixed(age),
      age_midpoint = get_age_midpoint(age_group)
    ) %>%
    filter(!is.na(period_4yr), !is.na(age_group)) %>%
    mutate(
      cohort_4yr = round((period_4yr - age_midpoint) / 4) * 4,
      age_squared = age^2,
      lnWt = ifelse(wt == 0 | is.na(wt), 0, log(wt)),
      period_4yr = as.character(period_4yr),
      cohort_4yr = as.character(cohort_4yr)
    ) %>%
    filter(!is.na(cohort_4yr))
}

# ==============================================================================
# 1. Subsample NHIS for tractable analysis
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("1. SUBSAMPLING NHIS DATA")
message(paste(rep("=", 70), collapse = ""))

# First, filter to observations with valid srh, age, year, wt for subsampling
data_nhis_valid <- data_nhis_full %>%
  filter(!is.na(srh), !is.na(age), !is.na(year), !is.na(wt)) %>%
  filter(age >= 18, age <= 89)

message("Valid observations for analysis: ", format(nrow(data_nhis_valid), big.mark = ","))

# Subsample to 100K using stratified random sampling
target_n <- 100000

if (nrow(data_nhis_valid) > target_n) {
  message("\nSubsampling to ", format(target_n, big.mark = ","), " observations...")
  subsample_result <- subsample_survey(data_nhis_valid, target_n = target_n, seed = 20260205)
  data_nhis_sub <- subsample_result$data

  # Print validation summary
  print_validation_summary(subsample_result)

  # Save validation results
  write_csv(subsample_result$validation, file.path(output_dir, "subsample_validation.csv"))
} else {
  message("Data size (", nrow(data_nhis_valid), ") <= target (", target_n, "), using full data")
  data_nhis_sub <- data_nhis_valid
  data_nhis_sub$wt_sub <- data_nhis_sub$wt
}

message("Subsampled NHIS: ", format(nrow(data_nhis_sub), big.mark = ","), " observations")

# Use wt_sub as the weight going forward
data_nhis_sub <- data_nhis_sub %>%
  mutate(wt = wt_sub)

# ==============================================================================
# 2. SRH Benchmark (for comparison)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("2. SRH BENCHMARK")
message(paste(rep("=", 70), collapse = ""))

# Prepare SRH data
bhapc_srh <- prepare_sanity_check_data(data_nhis_sub, "srh")

message("SRH data prepared:")
message("  N observations: ", format(nrow(bhapc_srh), big.mark = ","))
message("  SRH range: ", min(bhapc_srh$srh), " - ", max(bhapc_srh$srh))
message("  Mean SRH: ", round(mean(bhapc_srh$srh), 3))
message("  N periods: ", length(unique(bhapc_srh$period_4yr)))
message("  N cohorts: ", length(unique(bhapc_srh$cohort_4yr)))

# Fit BHAPC model for SRH
message("\nFitting BHAPC model for SRH (2000 iterations)...")
result_srh <- fit_bhapc_model(
  bhapc_srh,
  outcome = "srh",
  iter = 2000,
  adapt_delta = 0.95,
  include_strata = FALSE,
  seed = 20260205
)

# Save model
saveRDS(result_srh, file.path(output_dir, "result_srh.rds"))

# Extract variance components
var_srh <- extract_variance_components(result_srh$model)
message("\nSRH variance decomposition:")
print(var_srh)

write_csv(var_srh, file.path(output_dir, "nhis_srh_variance.csv"))

# ==============================================================================
# 3. Education Benchmark
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("3. EDUCATION BENCHMARK")
message(paste(rep("=", 70), collapse = ""))

# Check educ_4cat distribution
message("Education (educ_4cat) distribution in subsampled data:")
print(table(data_nhis_sub$educ_4cat, useNA = "ifany"))

# Prepare education data
bhapc_educ <- prepare_sanity_check_data(data_nhis_sub, "educ_4cat")

message("\nEducation data prepared:")
message("  N observations: ", format(nrow(bhapc_educ), big.mark = ","))
message("  Education range: ", min(bhapc_educ$educ_4cat), " - ", max(bhapc_educ$educ_4cat))
message("  Mean education: ", round(mean(bhapc_educ$educ_4cat), 3))
message("  N periods: ", length(unique(bhapc_educ$period_4yr)))
message("  N cohorts: ", length(unique(bhapc_educ$cohort_4yr)))

# Fit BHAPC model for education
message("\nFitting BHAPC model for education (2000 iterations)...")
result_educ <- fit_bhapc_model(
  bhapc_educ,
  outcome = "educ_4cat",
  iter = 2000,
  adapt_delta = 0.95,
  include_strata = FALSE,
  seed = 20260205
)

# Save model
saveRDS(result_educ, file.path(output_dir, "result_educ.rds"))

# Extract variance components
var_educ <- extract_variance_components(result_educ$model)
message("\nEducation variance decomposition:")
print(var_educ)

write_csv(var_educ, file.path(output_dir, "nhis_education_variance.csv"))

# ==============================================================================
# 4. Smoking Benchmark (when available)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("4. SMOKING BENCHMARK (if available)")
message(paste(rep("=", 70), collapse = ""))

# Check for smoking variable
smoking_vars <- c("smoke", "smokestatus", "smoking", "smoker", "smokenow")
available_smoking <- intersect(smoking_vars, names(data_nhis_sub))

if (length(available_smoking) > 0) {
  smoke_var <- available_smoking[1]
  message("Found smoking variable: ", smoke_var)

  # Prepare smoking data
  bhapc_smoke <- prepare_sanity_check_data(data_nhis_sub, smoke_var)

  message("Smoking data prepared:")
  message("  N observations: ", format(nrow(bhapc_smoke), big.mark = ","))
  message("  N periods: ", length(unique(bhapc_smoke$period_4yr)))
  message("  N cohorts: ", length(unique(bhapc_smoke$cohort_4yr)))

  # Fit BHAPC model for smoking
  message("\nFitting BHAPC model for smoking (2000 iterations)...")
  result_smoke <- fit_bhapc_model(
    bhapc_smoke,
    outcome = smoke_var,
    iter = 2000,
    adapt_delta = 0.95,
    include_strata = FALSE,
    seed = 20260205
  )

  # Save model
  saveRDS(result_smoke, file.path(output_dir, "result_smoke.rds"))

  # Extract variance components
  var_smoke <- extract_variance_components(result_smoke$model)
  message("\nSmoking variance decomposition:")
  print(var_smoke)

  write_csv(var_smoke, file.path(output_dir, "nhis_smoking_variance.csv"))

  smoke_available <- TRUE
} else {
  message("No smoking variable found in dataset.")
  message("Smoking variables typically available from IPUMS: SMOKESTATUS, SMOKENOW, etc.")
  message("Skip smoking analysis for now.")
  smoke_available <- FALSE
}

# ==============================================================================
# 5. K6 Distress Benchmark
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("5. K6 DISTRESS BENCHMARK")
message(paste(rep("=", 70), collapse = ""))

# Check for K6 variable (available 1997-2020 in NHIS)
if ("k6" %in% names(data_nhis_sub)) {
  k6_available <- sum(!is.na(data_nhis_sub$k6))
  message("K6 distress variable found: ", format(k6_available, big.mark = ","), " valid observations")

  # Check K6 range
  message("K6 range: ", min(data_nhis_sub$k6, na.rm = TRUE), " - ",
          max(data_nhis_sub$k6, na.rm = TRUE), " (0-24 scale, higher = more distress)")

  # Prepare K6 data
  bhapc_k6 <- prepare_sanity_check_data(data_nhis_sub, "k6")

  message("\nK6 data prepared:")
  message("  N observations: ", format(nrow(bhapc_k6), big.mark = ","))
  message("  K6 range: ", min(bhapc_k6$k6), " - ", max(bhapc_k6$k6))
  message("  Mean K6: ", round(mean(bhapc_k6$k6), 3))
  message("  N periods: ", length(unique(bhapc_k6$period_4yr)))
  message("  N cohorts: ", length(unique(bhapc_k6$cohort_4yr)))

  # Fit BHAPC model for K6
  message("\nFitting BHAPC model for K6 (2000 iterations)...")
  result_k6 <- fit_bhapc_model(
    bhapc_k6,
    outcome = "k6",
    iter = 2000,
    adapt_delta = 0.95,
    include_strata = FALSE,
    seed = 20260205
  )

  # Save model
  saveRDS(result_k6, file.path(output_dir, "result_k6.rds"))

  # Extract variance components
  var_k6 <- extract_variance_components(result_k6$model)
  message("\nK6 variance decomposition:")
  print(var_k6)

  write_csv(var_k6, file.path(output_dir, "nhis_k6_variance.csv"))

  k6_available <- TRUE
} else {
  message("No K6 variable found in dataset.")
  message("K6 should be available in NHIS from 1997-2020.")
  k6_available <- FALSE
}

# ==============================================================================
# 6. Generate Figures
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("6. GENERATING FIGURES")
message(paste(rep("=", 70), collapse = ""))

# Figure 3: APC effects for SRH
message("\nGenerating Figure 3 for SRH...")
fig3_srh <- create_figure3_apc_effects(
  result_srh$model,
  bhapc_srh,
  survey = "nhis_srh",
  output_path = file.path(output_dir, "figure3_nhis_srh.png")
)

# Figure 3: APC effects for education
message("Generating Figure 3 for education...")
fig3_educ <- create_figure3_apc_effects(
  result_educ$model,
  bhapc_educ,
  survey = "nhis_education",
  output_path = file.path(output_dir, "figure3_nhis_education.png")
)

# Variance bar charts
message("Generating variance charts...")

p_var_srh <- create_variance_chart(var_srh, survey = "nhis_srh")
ggsave(file.path(output_dir, "variance_chart_nhis_srh.png"),
       p_var_srh, width = 8, height = 6, dpi = 300)

p_var_educ <- create_variance_chart(var_educ, survey = "nhis_education")
ggsave(file.path(output_dir, "variance_chart_nhis_education.png"),
       p_var_educ, width = 8, height = 6, dpi = 300)

# Smoking figures if available
if (smoke_available) {
  message("Generating Figure 3 for smoking...")
  fig3_smoke <- create_figure3_apc_effects(
    result_smoke$model,
    bhapc_smoke,
    survey = "nhis_smoking",
    output_path = file.path(output_dir, "figure3_nhis_smoking.png")
  )

  p_var_smoke <- create_variance_chart(var_smoke, survey = "nhis_smoking")
  ggsave(file.path(output_dir, "variance_chart_nhis_smoking.png"),
         p_var_smoke, width = 8, height = 6, dpi = 300)
}

# K6 figures if available
if (k6_available) {
  message("Generating Figure 3 for K6...")
  fig3_k6 <- create_figure3_apc_effects(
    result_k6$model,
    bhapc_k6,
    survey = "nhis_k6",
    output_path = file.path(output_dir, "figure3_nhis_k6.png")
  )

  p_var_k6 <- create_variance_chart(var_k6, survey = "nhis_k6")
  ggsave(file.path(output_dir, "variance_chart_nhis_k6.png"),
         p_var_k6, width = 8, height = 6, dpi = 300)
}

# ==============================================================================
# 7. Create Comparison Table
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("7. COMPARISON TABLE")
message(paste(rep("=", 70), collapse = ""))

# Extract cohort and period percentages
get_pct <- function(var_df, component_name) {
  var_df %>%
    filter(component == component_name) %>%
    pull(pct_of_total)
}

# Build comparison table
comparison <- tibble(
  variable = c("SRH", "Education (4-cat)"),
  survey = "NHIS",
  n_obs = c(nrow(bhapc_srh), nrow(bhapc_educ)),
  cohort_pct = c(
    get_pct(var_srh, "cohort_4yr"),
    get_pct(var_educ, "cohort_4yr")
  ),
  period_pct = c(
    get_pct(var_srh, "period_4yr"),
    get_pct(var_educ, "period_4yr")
  ),
  residual_pct = c(
    get_pct(var_srh, "Residual"),
    get_pct(var_educ, "Residual")
  )
)

# Add K6 if available
if (k6_available) {
  comparison <- bind_rows(
    comparison,
    tibble(
      variable = "K6 Distress",
      survey = "NHIS",
      n_obs = nrow(bhapc_k6),
      cohort_pct = get_pct(var_k6, "cohort_4yr"),
      period_pct = get_pct(var_k6, "period_4yr"),
      residual_pct = get_pct(var_k6, "Residual")
    )
  )
}

# Add smoking if available
if (smoke_available) {
  comparison <- bind_rows(
    comparison,
    tibble(
      variable = "Smoking",
      survey = "NHIS",
      n_obs = nrow(bhapc_smoke),
      cohort_pct = get_pct(var_smoke, "cohort_4yr"),
      period_pct = get_pct(var_smoke, "period_4yr"),
      residual_pct = get_pct(var_smoke, "Residual")
    )
  )
}

message("\nComparison of variance decomposition:")
print(comparison)

write_csv(comparison, file.path(output_dir, "nhis_sanity_check_comparison.csv"))

# ==============================================================================
# 8. Interpretation
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("8. INTERPRETATION")
message(paste(rep("=", 70), collapse = ""))

srh_cohort <- get_pct(var_srh, "cohort_4yr")
educ_cohort <- get_pct(var_educ, "cohort_4yr")

message("\nResults Summary:")
message("  SRH cohort effect: ", round(srh_cohort, 2), "%")
message("  Education cohort effect: ", round(educ_cohort, 2), "%")
if (k6_available) {
  k6_cohort <- get_pct(var_k6, "cohort_4yr")
  message("  K6 distress cohort effect: ", round(k6_cohort, 2), "%")
}
if (smoke_available) {
  smoke_cohort <- get_pct(var_smoke, "cohort_4yr")
  message("  Smoking cohort effect: ", round(smoke_cohort, 2), "%")
}

message("\nInterpretation:")
if (educ_cohort > 5) {
  message("  - Education shows strong cohort effect (", round(educ_cohort, 1),
          "%), indicating model captures cohort trends well")
  message("  - SRH cohort effect (", round(srh_cohort, 1), "%) is modest in comparison")
} else if (educ_cohort > srh_cohort) {
  message("  - Education cohort effect (", round(educ_cohort, 1),
          "%) is larger than SRH (", round(srh_cohort, 1), "%)")
  message("  - This aligns with known secular increase in educational attainment")
} else {
  message("  - Education cohort effect (", round(educ_cohort, 1),
          "%) is similar to or smaller than SRH (", round(srh_cohort, 1), "%)")
  message("  - This may warrant further investigation")
}

if (k6_available) {
  message("  - K6 distress cohort effect (", round(k6_cohort, 1),
          "%) provides another subjective measure for comparison")
}

# ==============================================================================
# 9. Create Summary Figure (multi-panel comparison)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("9. SUMMARY COMPARISON FIGURE")
message(paste(rep("=", 70), collapse = ""))

# Create a comparison bar chart
comparison_plot_data <- comparison %>%
  select(variable, cohort_pct, period_pct) %>%
  pivot_longer(cols = c(cohort_pct, period_pct),
               names_to = "effect_type",
               values_to = "pct") %>%
  mutate(
    effect_type = case_when(
      effect_type == "cohort_pct" ~ "Cohort",
      effect_type == "period_pct" ~ "Period"
    ),
    variable = factor(variable, levels = c("SRH", "Education (4-cat)", "K6 Distress", "Smoking"))
  )

p_comparison <- ggplot(comparison_plot_data,
                        aes(x = variable, y = pct, fill = effect_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 2), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(
    values = c("Cohort" = "#CC79A7", "Period" = "#009E73"),
    name = "Effect Type"
  ) +
  labs(
    title = "Variance Explained by Period and Cohort Effects",
    subtitle = "NHIS Sanity Check: Comparing SRH to Education and K6 Distress",
    x = "",
    y = "% of Total Variance",
    caption = paste0("N = ", format(nrow(bhapc_srh), big.mark = ","),
                     " (subsampled from ", format(nrow(data_nhis_valid), big.mark = ","), ")")
  ) +
  ylim(0, max(comparison_plot_data$pct, na.rm = TRUE) * 1.3) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave(file.path(output_dir, "nhis_sanity_check_comparison_figure.png"),
       p_comparison, width = 10, height = 7, dpi = 300)

message("Saved comparison figure to: ", file.path(output_dir, "nhis_sanity_check_comparison_figure.png"))

# ==============================================================================
# Done
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("NHIS SANITY CHECK COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nOutput files saved to: ", output_dir)
message("  - subsample_validation.csv")
message("  - nhis_srh_variance.csv")
message("  - nhis_education_variance.csv")
if (k6_available) {
  message("  - nhis_k6_variance.csv")
}
if (smoke_available) {
  message("  - nhis_smoking_variance.csv")
}
message("  - nhis_sanity_check_comparison.csv")
message("  - figure3_nhis_srh.png")
message("  - figure3_nhis_education.png")
if (k6_available) {
  message("  - figure3_nhis_k6.png")
}
if (smoke_available) {
  message("  - figure3_nhis_smoking.png")
}
message("  - variance_chart_nhis_srh.png")
message("  - variance_chart_nhis_education.png")
if (k6_available) {
  message("  - variance_chart_nhis_k6.png")
}
if (smoke_available) {
  message("  - variance_chart_nhis_smoking.png")
}
message("  - nhis_sanity_check_comparison_figure.png")
message("  - result_srh.rds")
message("  - result_educ.rds")
if (k6_available) {
  message("  - result_k6.rds")
}
if (smoke_available) {
  message("  - result_smoke.rds")
}

message("\nEstimated runtime: 20-30 min per model with 100K obs and iter=2000")

message("\n")
