# ==============================================================================
# 13_sanity_check_gss.R
# Sanity Check: GSS Education and Happiness BHAPC
# Benchmark cohort effect analysis
# Author: Christine Lucille Kuryla
#
# This script runs the BHAPC model on education and happiness
# to benchmark the cohort effect against SRH (2.35%)
#
# Expected outcomes:
# - Education: Clear upward cohort trend (known secular increase)
# - Happiness: Subjective measure similar to SRH for comparison
# ==============================================================================

library(tidyverse)
library(rstanarm)
library(broom.mixed)
library(here)
library(gssr)

# Source BHAPC model fitting functions
source(here::here("R/functions/bhapc_model_fitting.R"))
source(here::here("R/functions/bhapc_figure_generation.R"))

# ==============================================================================
# Setup
# ==============================================================================

# Create output directory
output_dir <- here::here("output/sanity_check_bhapc")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260202)

# ==============================================================================
# Load GSS data with education and happiness
# ==============================================================================

message("Loading GSS data from gssr package...")

data("gss_all")

# Wrangle GSS data (same as wrangle_gss.R but keep educ and happy)
data_gss_full <- gss_all %>%
  haven::zap_labels() %>%
  select(
    year, cohort, age, health, sex,
    happy, educ,
    wtssps, wtssall
  ) %>%
  filter(cohort != 9999) %>%
  filter(!is.na(age)) %>%
  filter(age >= 18) %>%
  # health is coded 1=Excellent ... 4=Poor in raw data
  # Recode so higher = better: 4=Excellent, 3=Good, 2=Fair, 1=Poor
  filter(health %in% 1:4) %>%
  mutate(
    srh = 5 - health,
    # Recode happy: original 1=Very happy, 2=Pretty happy, 3=Not too happy
    # Recode so higher = better: 3=Very happy, 2=Pretty happy, 1=Not too happy
    happy = if_else(happy %in% 1:3, 4L - as.integer(happy), NA_integer_)
  ) %>%
  mutate(
    wt = coalesce(wtssall, wtssps)
  ) %>%
  filter(!is.na(wt))

rm(gss_all)

message("GSS data loaded: ", format(nrow(data_gss_full), big.mark = ","), " observations")

# ==============================================================================
# Helper Functions (copied from bhapc_data_prep.R to avoid SRH filtering)
# ==============================================================================

#' Map survey years to 4-year periods
map_to_period_4yr <- function(year, survey = "gss") {
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
prepare_sanity_check_data <- function(df, outcome_var, survey = "gss") {

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
# 1. Education Benchmark
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("1. EDUCATION BENCHMARK")
message(paste(rep("=", 70), collapse = ""))

# Prepare education data
bhapc_educ <- prepare_sanity_check_data(data_gss_full, "educ")

message("Education data prepared:")
message("  N observations: ", format(nrow(bhapc_educ), big.mark = ","))
message("  Education range: ", min(bhapc_educ$educ), " - ", max(bhapc_educ$educ))
message("  Mean education: ", round(mean(bhapc_educ$educ), 2), " years")
message("  N periods: ", length(unique(bhapc_educ$period_4yr)))
message("  N cohorts: ", length(unique(bhapc_educ$cohort_4yr)))

# Fit BHAPC model for education
message("\nFitting BHAPC model for education (1000 iterations)...")
result_educ <- fit_bhapc_model(
  bhapc_educ,
  outcome = "educ",
  iter = 1000,
  include_strata = FALSE,
  seed = 20260202
)

# Save model
saveRDS(result_educ, file.path(output_dir, "result_educ.rds"))

# Extract variance components
var_educ <- extract_variance_components(result_educ$model)
message("\nEducation variance decomposition:")
print(var_educ)

write_csv(var_educ, file.path(output_dir, "gss_education_variance.csv"))

# ==============================================================================
# 2. Happiness Benchmark
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("2. HAPPINESS BENCHMARK")
message(paste(rep("=", 70), collapse = ""))

# Prepare happiness data
bhapc_happy <- prepare_sanity_check_data(data_gss_full, "happy")

message("Happiness data prepared:")
message("  N observations: ", format(nrow(bhapc_happy), big.mark = ","))
message("  Happiness range: ", min(bhapc_happy$happy), " - ", max(bhapc_happy$happy))
message("  Mean happiness: ", round(mean(bhapc_happy$happy), 2))
message("  N periods: ", length(unique(bhapc_happy$period_4yr)))
message("  N cohorts: ", length(unique(bhapc_happy$cohort_4yr)))

# Fit BHAPC model for happiness
message("\nFitting BHAPC model for happiness (1000 iterations)...")
result_happy <- fit_bhapc_model(
  bhapc_happy,
  outcome = "happy",
  iter = 1000,
  include_strata = FALSE,
  seed = 20260202
)

# Save model
saveRDS(result_happy, file.path(output_dir, "result_happy.rds"))

# Extract variance components
var_happy <- extract_variance_components(result_happy$model)
message("\nHappiness variance decomposition:")
print(var_happy)

write_csv(var_happy, file.path(output_dir, "gss_happiness_variance.csv"))

# ==============================================================================
# 3. Generate Figures
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("3. GENERATING FIGURES")
message(paste(rep("=", 70), collapse = ""))

# Figure 3: APC effects for education
message("\nGenerating Figure 3 for education...")
fig3_educ <- create_figure3_apc_effects(
  result_educ$model,
  bhapc_educ,
  survey = "gss_education",
  output_path = file.path(output_dir, "figure3_gss_education.png")
)

# Figure 3: APC effects for happiness
message("Generating Figure 3 for happiness...")
fig3_happy <- create_figure3_apc_effects(
  result_happy$model,
  bhapc_happy,
  survey = "gss_happiness",
  output_path = file.path(output_dir, "figure3_gss_happiness.png")
)

# Variance bar charts
message("Generating variance charts...")

p_var_educ <- create_variance_chart(var_educ, survey = "gss_education")
ggsave(file.path(output_dir, "variance_chart_gss_education.png"),
       p_var_educ, width = 8, height = 6, dpi = 300)

p_var_happy <- create_variance_chart(var_happy, survey = "gss_happiness")
ggsave(file.path(output_dir, "variance_chart_gss_happiness.png"),
       p_var_happy, width = 8, height = 6, dpi = 300)

# ==============================================================================
# 4. Create Comparison Table
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("4. COMPARISON TABLE")
message(paste(rep("=", 70), collapse = ""))

# Extract cohort and period percentages
get_pct <- function(var_df, component_name) {
  var_df %>%
    filter(component == component_name) %>%
    pull(pct_of_total)
}

comparison <- tibble(
  variable = c("SRH", "Education", "Happiness"),
  survey = "GSS",
  n_obs = c(
    NA,  # SRH reference from existing results
    nrow(bhapc_educ),
    nrow(bhapc_happy)
  ),
  cohort_pct = c(
    2.35,  # Reference from existing GSS SRH analysis
    get_pct(var_educ, "cohort_4yr"),
    get_pct(var_happy, "cohort_4yr")
  ),
  period_pct = c(
    0.14,  # Reference from existing GSS SRH analysis
    get_pct(var_educ, "period_4yr"),
    get_pct(var_happy, "period_4yr")
  ),
  residual_pct = c(
    97.51,  # Reference from existing GSS SRH analysis
    get_pct(var_educ, "Residual"),
    get_pct(var_happy, "Residual")
  )
)

message("\nComparison of cohort effects:")
print(comparison)

write_csv(comparison, file.path(output_dir, "gss_sanity_check_comparison.csv"))

# ==============================================================================
# 5. Interpretation
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("5. INTERPRETATION")
message(paste(rep("=", 70), collapse = ""))

educ_cohort <- get_pct(var_educ, "cohort_4yr")
happy_cohort <- get_pct(var_happy, "cohort_4yr")
srh_cohort <- 2.35

message("\nResults Summary:")
message("  SRH cohort effect: ", round(srh_cohort, 2), "%")
message("  Education cohort effect: ", round(educ_cohort, 2), "%")
message("  Happiness cohort effect: ", round(happy_cohort, 2), "%")

message("\nInterpretation:")
if (educ_cohort > 5) {
  message("  - Education shows strong cohort effect (", round(educ_cohort, 1),
          "%), indicating model captures cohort trends well")
  message("  - SRH cohort effect (2.35%) is modest in comparison")
} else if (educ_cohort > 2) {
  message("  - Education cohort effect (", round(educ_cohort, 1),
          "%) is similar to SRH (2.35%)")
  message("  - Both show comparable secular trends")
} else {
  message("  - Education cohort effect (", round(educ_cohort, 1),
          "%) is smaller than expected")
  message("  - This may warrant further investigation")
}

if (abs(happy_cohort - srh_cohort) < 1) {
  message("  - Happiness and SRH show similar cohort patterns (both subjective measures)")
} else if (happy_cohort > srh_cohort) {
  message("  - Happiness shows stronger cohort effect than SRH")
} else {
  message("  - Happiness shows weaker cohort effect than SRH")
}

# ==============================================================================
# 6. Create Summary Figure (multi-panel comparison)
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("6. SUMMARY COMPARISON FIGURE")
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
    variable = factor(variable, levels = c("SRH", "Education", "Happiness"))
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
    subtitle = "GSS Sanity Check: Comparing SRH to Education and Happiness",
    x = "",
    y = "% of Total Variance",
    caption = paste0("Education N = ", format(nrow(bhapc_educ), big.mark = ","),
                     "; Happiness N = ", format(nrow(bhapc_happy), big.mark = ","))
  ) +
  ylim(0, max(comparison_plot_data$pct) * 1.3) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave(file.path(output_dir, "gss_sanity_check_comparison_figure.png"),
       p_comparison, width = 10, height = 7, dpi = 300)

message("Saved comparison figure to: ", file.path(output_dir, "gss_sanity_check_comparison_figure.png"))

# ==============================================================================
# Done
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("SANITY CHECK COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nOutput files saved to: ", output_dir)
message("  - gss_education_variance.csv")
message("  - gss_happiness_variance.csv")
message("  - gss_sanity_check_comparison.csv")
message("  - figure3_gss_education.png")
message("  - figure3_gss_happiness.png")
message("  - variance_chart_gss_education.png")
message("  - variance_chart_gss_happiness.png")
message("  - gss_sanity_check_comparison_figure.png")
message("  - result_educ.rds")
message("  - result_happy.rds")

message("\n")
