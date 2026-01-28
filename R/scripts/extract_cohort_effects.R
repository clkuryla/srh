# Extract cohort effects from BHAPC model
library(tidyverse)
library(rstanarm)

# Load the model
model_result <- readRDS(here::here("output/apc/bhapc/nhanes_bhapc_model.rds"))
model <- model_result$model

# Extract cohort random effects (posterior means)
cohort_re <- as.data.frame(ranef(model)$cohort_4yr)
cohort_re$cohort <- rownames(cohort_re)
names(cohort_re)[1] <- "estimate"

# Get posterior samples
posterior <- as.matrix(model)

# Find cohort columns
cohort_cols <- grep("cohort_4yr:", colnames(posterior), value = TRUE)
cat("Found", length(cohort_cols), "cohort columns\n")

# Extract cohort names from column names
cohort_names <- gsub(".*cohort_4yr:([0-9-]+).*", "\\1", cohort_cols)

# Compute 90% credible intervals
ci_90 <- t(apply(posterior[, cohort_cols, drop = FALSE], 2, quantile, probs = c(0.05, 0.5, 0.95)))
ci_90 <- as.data.frame(ci_90)
names(ci_90) <- c("ci_lower_90", "median", "ci_upper_90")
ci_90$cohort <- cohort_names

# Merge
cohort_effects <- merge(cohort_re, ci_90, by = "cohort")
cohort_effects$cohort_num <- as.numeric(cohort_effects$cohort)
cohort_effects <- cohort_effects[order(cohort_effects$cohort_num), ]

# Add significance indicator (CI doesn't include zero)
cohort_effects$significant_90 <- ifelse(
  (cohort_effects$ci_lower_90 > 0) | (cohort_effects$ci_upper_90 < 0),
  "*", ""
)

# Print nicely formatted
cat("\n=== Cohort Effects from NHANES BHAPC Model ===\n\n")
cohort_out <- cohort_effects %>%
  select(cohort, estimate, ci_lower_90, ci_upper_90, significant_90) %>%
  mutate(
    estimate = round(estimate, 4),
    ci_lower_90 = round(ci_lower_90, 4),
    ci_upper_90 = round(ci_upper_90, 4)
  )
print(as.data.frame(cohort_out))

# Summary stats
cat("\n--- Summary ---\n")
cat("Range of estimates:", round(min(cohort_effects$estimate), 3), "to", round(max(cohort_effects$estimate), 3), "\n")
cat("Number significant at 90% level:", sum(cohort_effects$significant_90 == "*"), "of", nrow(cohort_effects), "\n")

# Also extract period effects for comparison
cat("\n\n=== Period Effects for Comparison ===\n\n")
period_re <- as.data.frame(ranef(model)$period_4yr)
period_re$period <- rownames(period_re)
names(period_re)[1] <- "estimate"

period_cols <- grep("period_4yr:", colnames(posterior), value = TRUE)
period_names <- gsub(".*period_4yr:([0-9]+).*", "\\1", period_cols)

ci_period <- t(apply(posterior[, period_cols, drop = FALSE], 2, quantile, probs = c(0.05, 0.5, 0.95)))
ci_period <- as.data.frame(ci_period)
names(ci_period) <- c("ci_lower_90", "median", "ci_upper_90")
ci_period$period <- period_names

period_effects <- merge(period_re, ci_period, by = "period")
period_effects$period_num <- as.numeric(period_effects$period)
period_effects <- period_effects[order(period_effects$period_num), ]

period_effects$significant_90 <- ifelse(
  (period_effects$ci_lower_90 > 0) | (period_effects$ci_upper_90 < 0),
  "*", ""
)

period_out <- period_effects %>%
  select(period, estimate, ci_lower_90, ci_upper_90, significant_90) %>%
  mutate(
    estimate = round(estimate, 4),
    ci_lower_90 = round(ci_lower_90, 4),
    ci_upper_90 = round(ci_upper_90, 4)
  )
print(as.data.frame(period_out))

cat("\nRange of period estimates:", round(min(period_effects$estimate), 3), "to", round(max(period_effects$estimate), 3), "\n")
cat("Number significant at 90% level:", sum(period_effects$significant_90 == "*"), "of", nrow(period_effects), "\n")
