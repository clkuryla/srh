# ==============================================================================
# 20_sanity_check_meps.R
# Sanity Check: MEPS SRH, Education, and K6 Distress BHAPC
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
# NOTE: MEPS K6 available from 2004 onwards
# NOTE: Subsample to ~100K observations for tractability
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
output_dir <- here::here("output/sanity_check_meps")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260205)

# ==============================================================================
# Load MEPS data
# ==============================================================================

message("Loading MEPS data from data_meps.rds...")

data_meps_full <- read_rds(derived_path("data_meps.rds"))

message("Full MEPS data loaded: ", format(nrow(data_meps_full), big.mark = ","), " observations")

# Check available columns
message("\nKey columns available:")
message("  - srh: ", sum(!is.na(data_meps_full$srh)), " valid")
message("  - educ_4cat: ", sum(!is.na(data_meps_full$educ_4cat)), " valid")
message("  - K6SUM: ", sum(!is.na(data_meps_full$K6SUM)), " valid")
message("  - age: ", sum(!is.na(data_meps_full$age)), " valid")
message("  - year: ", sum(!is.na(data_meps_full$year)), " valid")
message("  - wt: ", sum(!is.na(data_meps_full$wt)), " valid")

# ==============================================================================
# Helper Functions (adapted from 16_sanity_check_nhis.R)
# ==============================================================================

#' Map survey years to 4-year periods
map_to_period_4yr <- function(year, survey = "meps") {
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
prepare_sanity_check_data <- function(df, outcome_var, survey = "meps") {

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
# 1. Subsample MEPS for tractable analysis
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("1. SUBSAMPLING MEPS DATA")
message(paste(rep("=", 70), collapse = ""))

# First, filter to observations with valid srh, age, year, wt for subsampling
data_meps_valid <- data_meps_full %>%
  filter(!is.na(srh), !is.na(age), !is.na(year), !is.na(wt)) %>%
  filter(age >= 18, age <= 89)

message("Valid observations for analysis: ", format(nrow(data_meps_valid), big.mark = ","))

# Subsample to 100K using stratified random sampling
target_n <- 100000

if (nrow(data_meps_valid) > target_n) {
  message("\nSubsampling to ", format(target_n, big.mark = ","), " observations...")
  subsample_result <- subsample_survey(data_meps_valid, target_n = target_n, seed = 20260205)
  data_meps_sub <- subsample_result$data

  # Print validation summary
  print_validation_summary(subsample_result)

  # Save validation results
  write_csv(subsample_result$validation, file.path(output_dir, "subsample_validation.csv"))
} else {
  message("Data size (", nrow(data_meps_valid), ") <= target (", target_n, "), using full data")
  data_meps_sub <- data_meps_valid
  data_meps_sub$wt_sub <- data_meps_sub$wt
}

message("Subsampled MEPS: ", format(nrow(data_meps_sub), big.mark = ","), " observations")

# Use wt_sub as the weight going forward
data_meps_sub <- data_meps_sub %>%
  mutate(wt = wt_sub)

# ==============================================================================
# 2. SRH Benchmark (for comparison)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("2. SRH BENCHMARK")
message(paste(rep("=", 70), collapse = ""))

# Prepare SRH data
bhapc_srh <- prepare_sanity_check_data(data_meps_sub, "srh")

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

write_csv(var_srh, file.path(output_dir, "meps_srh_variance.csv"))

# ==============================================================================
# 3. Education Benchmark
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("3. EDUCATION BENCHMARK")
message(paste(rep("=", 70), collapse = ""))

# Check educ_4cat distribution
message("Education (educ_4cat) distribution in subsampled data:")
print(table(data_meps_sub$educ_4cat, useNA = "ifany"))

# Prepare education data
bhapc_educ <- prepare_sanity_check_data(data_meps_sub, "educ_4cat")

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

write_csv(var_educ, file.path(output_dir, "meps_education_variance.csv"))

# ==============================================================================
# 4. K6 Distress Benchmark
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("4. K6 DISTRESS BENCHMARK")
message(paste(rep("=", 70), collapse = ""))

# Check for K6 variable (K6SUM in MEPS, available from 2004)
if ("K6SUM" %in% names(data_meps_sub)) {
  # Filter to years with K6 data (2004+)
  data_meps_k6 <- data_meps_sub %>%
    filter(year >= 2004, !is.na(K6SUM))

  k6_n <- nrow(data_meps_k6)
  message("K6 distress variable (K6SUM) found: ", format(k6_n, big.mark = ","), " valid observations (2004+)")

  if (k6_n > 1000) {
    # Check K6 range
    message("K6 range: ", min(data_meps_k6$K6SUM, na.rm = TRUE), " - ",
            max(data_meps_k6$K6SUM, na.rm = TRUE), " (0-24 scale, higher = more distress)")

    # Prepare K6 data
    bhapc_k6 <- prepare_sanity_check_data(data_meps_k6, "K6SUM")

    message("\nK6 data prepared:")
    message("  N observations: ", format(nrow(bhapc_k6), big.mark = ","))
    message("  K6 range: ", min(bhapc_k6$K6SUM), " - ", max(bhapc_k6$K6SUM))
    message("  Mean K6: ", round(mean(bhapc_k6$K6SUM), 3))
    message("  N periods: ", length(unique(bhapc_k6$period_4yr)))
    message("  N cohorts: ", length(unique(bhapc_k6$cohort_4yr)))

    # Fit BHAPC model for K6
    message("\nFitting BHAPC model for K6 (2000 iterations)...")
    result_k6 <- fit_bhapc_model(
      bhapc_k6,
      outcome = "K6SUM",
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

    write_csv(var_k6, file.path(output_dir, "meps_k6_variance.csv"))

    k6_available <- TRUE
  } else {
    message("Insufficient K6 observations (", k6_n, ") for analysis. Skipping K6.")
    k6_available <- FALSE
  }
} else {
  message("No K6 variable (K6SUM) found in dataset.")
  message("K6 should be available in MEPS from 2004 onwards.")
  k6_available <- FALSE
}

# ==============================================================================
# 5. Generate Figures
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("5. GENERATING FIGURES")
message(paste(rep("=", 70), collapse = ""))

# Figure 3: APC effects for SRH
message("\nGenerating Figure 3 for SRH...")
fig3_srh <- create_figure3_apc_effects(
  result_srh$model,
  bhapc_srh,
  survey = "meps_srh",
  output_path = file.path(output_dir, "figure3_meps_srh.png")
)

# Figure 3: APC effects for education
message("Generating Figure 3 for education...")
fig3_educ <- create_figure3_apc_effects(
  result_educ$model,
  bhapc_educ,
  survey = "meps_education",
  output_path = file.path(output_dir, "figure3_meps_education.png")
)

# Variance bar charts
message("Generating variance charts...")

p_var_srh <- create_variance_chart(var_srh, survey = "meps_srh")
ggsave(file.path(output_dir, "variance_chart_meps_srh.png"),
       p_var_srh, width = 8, height = 6, dpi = 300)

p_var_educ <- create_variance_chart(var_educ, survey = "meps_education")
ggsave(file.path(output_dir, "variance_chart_meps_education.png"),
       p_var_educ, width = 8, height = 6, dpi = 300)

# K6 figures if available
if (k6_available) {
  message("Generating Figure 3 for K6...")
  fig3_k6 <- create_figure3_apc_effects(
    result_k6$model,
    bhapc_k6,
    survey = "meps_k6",
    output_path = file.path(output_dir, "figure3_meps_k6.png")
  )

  p_var_k6 <- create_variance_chart(var_k6, survey = "meps_k6")
  ggsave(file.path(output_dir, "variance_chart_meps_k6.png"),
         p_var_k6, width = 8, height = 6, dpi = 300)
}

# ==============================================================================
# 6. Create Comparison Table
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("6. COMPARISON TABLE")
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
  survey = "MEPS",
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
      survey = "MEPS",
      n_obs = nrow(bhapc_k6),
      cohort_pct = get_pct(var_k6, "cohort_4yr"),
      period_pct = get_pct(var_k6, "period_4yr"),
      residual_pct = get_pct(var_k6, "Residual")
    )
  )
}

message("\nComparison of variance decomposition:")
print(comparison)

write_csv(comparison, file.path(output_dir, "meps_sanity_check_comparison.csv"))

# ==============================================================================
# 7. Interpretation
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("7. INTERPRETATION")
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
# 8. Create Summary Figure (multi-panel comparison)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("8. SUMMARY COMPARISON FIGURE")
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
    variable = factor(variable, levels = c("SRH", "Education (4-cat)", "K6 Distress"))
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
    subtitle = "MEPS Sanity Check: Comparing SRH to Education and K6 Distress",
    x = "",
    y = "% of Total Variance",
    caption = paste0("N = ", format(nrow(bhapc_srh), big.mark = ","),
                     " (subsampled from ", format(nrow(data_meps_valid), big.mark = ","), ")")
  ) +
  ylim(0, max(comparison_plot_data$pct, na.rm = TRUE) * 1.3) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave(file.path(output_dir, "meps_sanity_check_comparison_figure.png"),
       p_comparison, width = 10, height = 7, dpi = 300)

message("Saved comparison figure to: ", file.path(output_dir, "meps_sanity_check_comparison_figure.png"))

# ==============================================================================
# Done
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("MEPS SANITY CHECK COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nOutput files saved to: ", output_dir)
message("  - subsample_validation.csv")
message("  - meps_srh_variance.csv")
message("  - meps_education_variance.csv")
if (k6_available) {
  message("  - meps_k6_variance.csv")
}
message("  - meps_sanity_check_comparison.csv")
message("  - figure3_meps_srh.png")
message("  - figure3_meps_education.png")
if (k6_available) {
  message("  - figure3_meps_k6.png")
}
message("  - variance_chart_meps_srh.png")
message("  - variance_chart_meps_education.png")
if (k6_available) {
  message("  - variance_chart_meps_k6.png")
}
message("  - meps_sanity_check_comparison_figure.png")
message("  - result_srh.rds")
message("  - result_educ.rds")
if (k6_available) {
  message("  - result_k6.rds")
}

message("\nEstimated runtime: 20-30 min per model with 100K obs and iter=2000")

message("\n")
