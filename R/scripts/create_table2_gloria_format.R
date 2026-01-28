# Create Table 2 in Gloria's format
# APC decomposition results from BHAPC model

library(tidyverse)
library(rstanarm)
library(here)

# Load the model and data
model_result <- readRDS(here("output/apc/bhapc/nhanes_bhapc_model.rds"))
model <- model_result$model
bhapc_df <- readRDS(here("output/apc/bhapc/nhanes_bhapc_data.rds"))

# Get posterior samples
posterior <- as.matrix(model)

# =============================================================================
# Extract Fixed Effects
# =============================================================================

# Age (linear)
age_samples <- posterior[, "age"]
age_est <- mean(age_samples)
age_ci <- quantile(age_samples, probs = c(0.10, 0.90))
age_sig <- ifelse(age_ci[1] > 0 | age_ci[2] < 0, "*", "")

# Age (quadratic, scaled)
age_sq_samples <- posterior[, "scale(age_squared)"]
age_sq_est <- mean(age_sq_samples)
age_sq_ci <- quantile(age_sq_samples, probs = c(0.10, 0.90))
age_sq_sig <- ifelse(age_sq_ci[1] > 0 | age_sq_ci[2] < 0, "*", "")

# =============================================================================
# Compute Total Age Effect
# =============================================================================

# Get scaling parameters for age_squared
age_sq_mean <- mean(bhapc_df$age_squared)
age_sq_sd <- sd(bhapc_df$age_squared)

# Function to compute combined age effect relative to reference age
compute_age_effect <- function(age, ref_age = 18, age_coef, age_sq_coef, age_sq_mean, age_sq_sd) {
  linear <- age_coef * (age - ref_age)
  scaled_sq <- (age^2 - age_sq_mean) / age_sq_sd
  scaled_sq_ref <- (ref_age^2 - age_sq_mean) / age_sq_sd
  quadratic <- age_sq_coef * (scaled_sq - scaled_sq_ref)
  linear + quadratic
}

total_age_effect_80 <- compute_age_effect(80, 18, age_est, age_sq_est, age_sq_mean, age_sq_sd)

# =============================================================================
# Compute Observed Means by Age Group
# =============================================================================

observed_age_means <- bhapc_df %>%
  group_by(age_group) %>%
  summarise(
    mean_srh = weighted.mean(srh, wt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

youngest_srh <- observed_age_means$mean_srh[1]
oldest_srh <- observed_age_means$mean_srh[nrow(observed_age_means)]
observed_decline <- youngest_srh - oldest_srh

# =============================================================================
# Extract Period Random Effects
# =============================================================================

period_cols <- grep("b\\[\\(Intercept\\) period_4yr:", colnames(posterior), value = TRUE)
period_names <- gsub(".*period_4yr:([0-9]+)\\]", "\\1", period_cols)

period_effects <- data.frame(
  effect = paste0("  ", period_names),
  estimate = sapply(period_cols, function(col) mean(posterior[, col])),
  ci_10 = sapply(period_cols, function(col) quantile(posterior[, col], 0.10)),
  ci_90 = sapply(period_cols, function(col) quantile(posterior[, col], 0.90)),
  stringsAsFactors = FALSE
)
period_effects$sig <- ifelse(period_effects$ci_10 > 0 | period_effects$ci_90 < 0, "*", "")
period_effects <- period_effects[order(as.numeric(period_names)), ]
rownames(period_effects) <- NULL

# =============================================================================
# Extract Cohort Random Effects
# =============================================================================

cohort_cols <- grep("b\\[\\(Intercept\\) cohort_4yr:", colnames(posterior), value = TRUE)
cohort_names <- gsub(".*cohort_4yr:([0-9-]+)\\]", "\\1", cohort_cols)

cohort_effects <- data.frame(
  effect = paste0("  ", cohort_names),
  estimate = sapply(cohort_cols, function(col) mean(posterior[, col])),
  ci_10 = sapply(cohort_cols, function(col) quantile(posterior[, col], 0.10)),
  ci_90 = sapply(cohort_cols, function(col) quantile(posterior[, col], 0.90)),
  stringsAsFactors = FALSE
)
cohort_effects$sig <- ifelse(cohort_effects$ci_10 > 0 | cohort_effects$ci_90 < 0, "*", "")
cohort_effects <- cohort_effects[order(as.numeric(cohort_names)), ]
rownames(cohort_effects) <- NULL

# =============================================================================
# Build the full table
# =============================================================================

table2 <- rbind(
  # Fixed effects
  data.frame(effect = "Age (linear)", estimate = age_est, ci_10 = age_ci[1], ci_90 = age_ci[2], sig = age_sig),
  data.frame(effect = "Age (quadratic, scaled)", estimate = age_sq_est, ci_10 = age_sq_ci[1], ci_90 = age_sq_ci[2], sig = age_sq_sig),
  # Period header
  data.frame(effect = "Period", estimate = NA, ci_10 = NA, ci_90 = NA, sig = ""),
  # Period effects
  period_effects,
  # Cohort header
  data.frame(effect = "Cohort", estimate = NA, ci_10 = NA, ci_90 = NA, sig = ""),
  # Cohort effects
  cohort_effects
)

rownames(table2) <- NULL

# Round for display
table2_display <- table2 %>%
  mutate(
    estimate = round(estimate, 2),
    ci_10 = round(ci_10, 2),
    ci_90 = round(ci_90, 2)
  )

# =============================================================================
# Print table
# =============================================================================

cat("\n")
cat("Table 2. Results of APC decomposition using Bayesian Hierarchical Age-Period-Cohort models (NHANES)\n")
cat(strrep("=", 80), "\n\n")
cat(sprintf("%-25s %8s %8s %8s %3s\n", "", "Est.", "10%", "90%", ""))
cat(strrep("-", 55), "\n")

for (i in 1:nrow(table2_display)) {
  row <- table2_display[i, ]
  if (is.na(row$estimate)) {
    cat(sprintf("%-25s\n", row$effect))
  } else {
    cat(sprintf("%-25s %8.2f %8.2f %8.2f %3s\n",
                row$effect, row$estimate, row$ci_10, row$ci_90, row$sig))
  }
}

cat(strrep("-", 55), "\n")
cat("\n* 90% credible interval excludes zero\n")

# =============================================================================
# Print observed means and age effect summary
# =============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("Observed Mean SRH by Age Group (survey-weighted)\n")
cat(strrep("-", 55), "\n")
cat(sprintf("%-12s %8s %8s\n", "Age Group", "Mean SRH", "N"))
cat(strrep("-", 35), "\n")

for (i in 1:nrow(observed_age_means)) {
  cat(sprintf("%-12s %8.2f %8s\n",
              observed_age_means$age_group[i],
              observed_age_means$mean_srh[i],
              format(observed_age_means$n[i], big.mark = ",")))
}

cat(strrep("-", 35), "\n")
cat("\n")
cat("Summary of Age Effect:\n")
cat(sprintf("  Observed decline (age 18-21 to 80-84): %.2f points (%.2f to %.2f)\n",
            observed_decline, youngest_srh, oldest_srh))
cat(sprintf("  Model-predicted decline (age 18 to 80): %.2f points\n", total_age_effect_80))
cat(sprintf("  As %% of 1-5 scale: %.0f%%\n", abs(total_age_effect_80) / 4 * 100))
cat("\n")

# =============================================================================
# Save to CSV
# =============================================================================

write.csv(table2_display,
          here("output/apc/bhapc/table2_nhanes_gloria_format.csv"),
          row.names = FALSE)
cat("Saved table to: output/apc/bhapc/table2_nhanes_gloria_format.csv\n")

# Save observed means
write.csv(observed_age_means,
          here("output/apc/bhapc/observed_means_by_age_nhanes.csv"),
          row.names = FALSE)
cat("Saved observed means to: output/apc/bhapc/observed_means_by_age_nhanes.csv\n")

# =============================================================================
# Create combined table for paper (with observed means at bottom)
# =============================================================================

# Add observed means section
obs_means_section <- observed_age_means %>%
  mutate(
    effect = paste0("  ", age_group),
    estimate = round(mean_srh, 2),
    ci_10 = NA,
    ci_90 = NA,
    sig = ""
  ) %>%
  select(effect, estimate, ci_10, ci_90, sig)

table2_with_obs <- rbind(
  table2_display,
  data.frame(effect = "", estimate = NA, ci_10 = NA, ci_90 = NA, sig = ""),
  data.frame(effect = "Observed Mean SRH", estimate = NA, ci_10 = NA, ci_90 = NA, sig = ""),
  obs_means_section
)

write.csv(table2_with_obs,
          here("output/apc/bhapc/table2_nhanes_complete.csv"),
          row.names = FALSE)
cat("Saved complete table to: output/apc/bhapc/table2_nhanes_complete.csv\n")
