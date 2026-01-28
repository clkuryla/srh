# ==============================================================================
# 07_bhapc_interaction_analysis.R
# BHAPC with Age × Period Interaction to Test Convergence
# Author: Christine Lucille Kuryla
#
# Purpose: Extend BHAPC to include age × period interaction, which directly
# tests whether the age-SRH gradient is changing over time (convergence).
#
# The standard BHAPC model assumes constant age effect across periods.
# This script adds interaction terms to capture the flattening gradient.
# ==============================================================================

library(tidyverse)
library(here)
library(rstanarm)
library(broom.mixed)
library(srvyr)

# Source paths and functions
source(here("R", "paths.R"))
source(here("R", "functions", "bhapc_data_prep.R"))

# Create output directory
output_dir <- here("output", "apc", "bhapc")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260128)

# ==============================================================================
# Part 1: Descriptive Gradients by Period
# ==============================================================================

message("\n", strrep("=", 70))
message("PART 1: DESCRIPTIVE AGE GRADIENTS BY PERIOD")
message(strrep("=", 70))

# Load prepared data
bhapc_df <- readRDS(here("output/apc/bhapc/nhanes_bhapc_data.rds"))

# Compute weighted mean SRH by age group and period
age_period_means <- bhapc_df %>%
  group_by(period_4yr, age_group) %>%
  summarise(
    mean_srh = weighted.mean(srh, wt, na.rm = TRUE),
    se_srh = sqrt(weighted.mean((srh - weighted.mean(srh, wt))^2, wt) / n()),
    n = n(),
    .groups = "drop"
  )

# Compute gradient (youngest - oldest) by period
gradient_by_period <- age_period_means %>%
  group_by(period_4yr) %>%
  summarise(
    srh_18_21 = mean_srh[age_group == "18-21"],
    srh_80_84 = mean_srh[age_group == "80-84"],
    gradient = srh_18_21 - srh_80_84,
    n_total = sum(n),
    .groups = "drop"
  ) %>%
  arrange(period_4yr)

# Print gradient table
message("\nAge Gradient by Period (survey-weighted):\n")
message(sprintf("%-10s %12s %12s %12s %10s", "Period", "Age 18-21", "Age 80-84", "Gradient", "N"))
message(strrep("-", 60))
for (i in 1:nrow(gradient_by_period)) {
  row <- gradient_by_period[i,]
  message(sprintf("%-10s %12.3f %12.3f %12.3f %10s",
                  row$period_4yr, row$srh_18_21, row$srh_80_84, row$gradient,
                  format(row$n_total, big.mark = ",")))
}
message(strrep("-", 60))

# Compute change in gradient
first_period <- gradient_by_period$period_4yr[1]
last_period <- gradient_by_period$period_4yr[nrow(gradient_by_period)]
gradient_change <- gradient_by_period$gradient[nrow(gradient_by_period)] - gradient_by_period$gradient[1]

message(sprintf("\nGradient in %s: %.3f", first_period, gradient_by_period$gradient[1]))
message(sprintf("Gradient in %s: %.3f", last_period, gradient_by_period$gradient[nrow(gradient_by_period)]))
message(sprintf("Change: %.3f (%.1f%% reduction)", gradient_change,
                abs(gradient_change) / gradient_by_period$gradient[1] * 100))

# Save gradient table
write.csv(gradient_by_period,
          here(output_dir, "gradient_by_period_nhanes.csv"),
          row.names = FALSE)
message("\nSaved: gradient_by_period_nhanes.csv")

# Save full age × period means
write.csv(age_period_means,
          here(output_dir, "age_period_means_nhanes.csv"),
          row.names = FALSE)
message("Saved: age_period_means_nhanes.csv")


# ==============================================================================
# Part 2: BHAPC with Age × Period Interaction
# ==============================================================================

message("\n", strrep("=", 70))
message("PART 2: BHAPC MODEL WITH AGE × PERIOD INTERACTION")
message(strrep("=", 70))

# Center age for interpretability (so intercept is at mean age)
mean_age <- mean(bhapc_df$age)
bhapc_df <- bhapc_df %>%
  mutate(age_centered = age - mean_age)

message(sprintf("\nCentering age at mean = %.1f years", mean_age))
message("N = ", format(nrow(bhapc_df), big.mark = ","))

# Fit model with age × period interaction
# This tests whether the age slope varies by period
message("\nFitting BHAPC model with age × period interaction...")
message("Model: srh ~ age_centered * period_4yr + (1|cohort_4yr)")
message("This may take 30-45 minutes...\n")

# Use period as fixed effect to get interaction terms
# (Can't have random slope × random intercept interaction easily)
interaction_model <- rstanarm::stan_lmer(
  srh ~ age_centered * period_4yr + lnWt + (1|cohort_4yr),
  data = bhapc_df,
  adapt_delta = 0.95,
  iter = 2000,
  chains = 4,
  cores = 4,
  seed = 20260128
)

message("\nModel fitting complete!")

# Extract interaction effects
posterior <- as.matrix(interaction_model)

# Find age × period interaction columns
interaction_cols <- grep("age_centered:period_4yr", colnames(posterior), value = TRUE)

message("\n--- Age × Period Interaction Effects ---")
message("(How much does the age slope change relative to reference period)\n")

interaction_effects <- data.frame(
  term = interaction_cols,
  estimate = sapply(interaction_cols, function(col) mean(posterior[, col])),
  ci_10 = sapply(interaction_cols, function(col) quantile(posterior[, col], 0.10)),
  ci_90 = sapply(interaction_cols, function(col) quantile(posterior[, col], 0.90)),
  stringsAsFactors = FALSE
)
interaction_effects$significant <- ifelse(
  interaction_effects$ci_10 > 0 | interaction_effects$ci_90 < 0, "*", ""
)

# Clean up term names
interaction_effects$period <- gsub("age_centered:period_4yr", "", interaction_effects$term)

# Print results
message(sprintf("%-12s %10s %10s %10s %5s", "Period", "Estimate", "10%", "90%", "Sig"))
message(strrep("-", 50))
message(sprintf("%-12s %10s %10s %10s %5s", "1999 (ref)", "0.000", "-", "-", ""))

for (i in 1:nrow(interaction_effects)) {
  row <- interaction_effects[i,]
  message(sprintf("%-12s %10.4f %10.4f %10.4f %5s",
                  row$period, row$estimate, row$ci_10, row$ci_90, row$significant))
}

# Get main age effect (at reference period)
age_main <- mean(posterior[, "age_centered"])
age_main_ci <- quantile(posterior[, "age_centered"], c(0.10, 0.90))

message(sprintf("\nMain age effect (at period 1999): %.4f [%.4f, %.4f]",
                age_main, age_main_ci[1], age_main_ci[2]))

# Compute age effect at each period
message("\n--- Age Effect by Period (main + interaction) ---\n")
message(sprintf("%-12s %12s %20s", "Period", "Age Effect", "Interpretation"))
message(strrep("-", 50))

# Reference period
message(sprintf("%-12s %12.4f %20s", "1999", age_main, "steepest decline"))

for (i in 1:nrow(interaction_effects)) {
  row <- interaction_effects[i,]
  total_effect <- age_main + row$estimate
  interp <- ifelse(row$estimate > 0, "flatter (convergence)", "steeper")
  message(sprintf("%-12s %12.4f %20s", row$period, total_effect, interp))
}

# Save interaction model results
interaction_summary <- data.frame(
  period = c("1999", interaction_effects$period),
  age_slope = c(age_main, age_main + interaction_effects$estimate),
  interaction = c(0, interaction_effects$estimate),
  ci_10 = c(age_main_ci[1], interaction_effects$ci_10),
  ci_90 = c(age_main_ci[2], interaction_effects$ci_90),
  significant = c("ref", interaction_effects$significant)
)

write.csv(interaction_summary,
          here(output_dir, "age_period_interaction_nhanes.csv"),
          row.names = FALSE)
message("\nSaved: age_period_interaction_nhanes.csv")

# Save model object
saveRDS(interaction_model, here(output_dir, "nhanes_interaction_model.rds"))
message("Saved: nhanes_interaction_model.rds")


# ==============================================================================
# Part 3: Summary Comparison Table
# ==============================================================================

message("\n", strrep("=", 70))
message("SUMMARY: CONVERGENCE EVIDENCE")
message(strrep("=", 70))

# Combine descriptive and model-based evidence
summary_table <- gradient_by_period %>%
  left_join(interaction_summary, by = c("period_4yr" = "period")) %>%
  select(period = period_4yr,
         observed_gradient = gradient,
         model_age_slope = age_slope,
         interaction_vs_1999 = interaction,
         significant)

message("\n")
message(sprintf("%-10s %18s %18s %18s %8s",
                "Period", "Observed Gradient", "Model Age Slope", "vs 1999", "Sig"))
message(strrep("-", 75))

for (i in 1:nrow(summary_table)) {
  row <- summary_table[i,]
  message(sprintf("%-10s %18.3f %18.4f %18.4f %8s",
                  row$period, row$observed_gradient, row$model_age_slope,
                  row$interaction_vs_1999, row$significant))
}

write.csv(summary_table,
          here(output_dir, "convergence_summary_nhanes.csv"),
          row.names = FALSE)
message("\nSaved: convergence_summary_nhanes.csv")

message("\n", strrep("=", 70))
message("ANALYSIS COMPLETE")
message(strrep("=", 70))
