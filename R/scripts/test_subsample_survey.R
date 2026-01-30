# ==============================================================================
# test_subsample_survey.R
# Test script for the subsample_survey function
# Author: Christine Lucille Kuryla
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)

# Source the function
source(here::here("R/functions/subsample_survey.R"))

# ==============================================================================
# Generate fake data
# ==============================================================================

set.seed(42)

# Parameters for fake data
n_obs <- 1000
years <- 2015:2019
age_range <- 20:80

# Generate fake survey data
fake_data <- tibble(
  id = 1:n_obs,
  year = sample(years, n_obs, replace = TRUE),
  age = sample(age_range, n_obs, replace = TRUE),
  srh = sample(1:5, n_obs, replace = TRUE, prob = c(0.05, 0.10, 0.25, 0.35, 0.25)),
  wt = runif(n_obs, min = 0.5, max = 2.0)
)

cat("\n=== Fake Data Summary ===\n")
cat("N observations:", nrow(fake_data), "\n")
cat("Years:", paste(range(fake_data$year), collapse = "-"), "\n")
cat("Ages:", paste(range(fake_data$age), collapse = "-"), "\n")
cat("Unweighted mean SRH:", round(mean(fake_data$srh), 4), "\n")
cat("Weighted mean SRH:", round(weighted.mean(fake_data$srh, fake_data$wt), 4), "\n\n")

# ==============================================================================
# Test 1: Basic subsampling
# ==============================================================================

cat("=== Test 1: Subsample to 300 ===\n")
cat("\nNote: With 1000 obs across ~60 strata (5 years × 12 age bins),\n")
cat("      and min_cell_size=30, minimum required is ~1800.\n")
cat("      Strata smaller than target keep all observations.\n\n")

result <- subsample_survey(fake_data, target_n = 300)

# Access results
subsampled <- result$data
validation <- result$validation

cat("\nSubsampled data has", nrow(subsampled), "rows\n")
cat("Has wt_sub column:", "wt_sub" %in% names(subsampled), "\n")

# Print detailed validation
print_validation_summary(result)

# ==============================================================================
# Test 2: Target larger than data
# ==============================================================================

cat("\n=== Test 2: Target larger than data ===\n")
result2 <- subsample_survey(fake_data, target_n = 2000)
cat("Returned data has", nrow(result2$data), "rows (should be", nrow(fake_data), ")\n\n")

# ==============================================================================
# Test 3: Different parameters
# ==============================================================================

cat("=== Test 3: Different age bins and min cell size ===\n")
result3 <- subsample_survey(
  fake_data,
  target_n = 200,
  age_bin_width = 10,   # 10-year age bins
  min_cell_size = 20    # Lower minimum
)

print_validation_summary(result3)

# ==============================================================================
# Test 4: Verify weighted distributions
# ==============================================================================

cat("\n=== Test 4: Verify weighted distributions are preserved ===\n")

# Compare SRH
srh_val <- validation %>% filter(variable == "srh_mean")
cat("SRH weighted mean difference:", round(srh_val$diff, 6), "\n")

# Compare age
age_val <- validation %>% filter(variable == "age_mean")
cat("Age weighted mean difference:", round(age_val$diff, 6), "\n")

# Year proportions
year_val <- validation %>% filter(variable == "year_prop")
cat("Max year proportion difference:", round(max(abs(year_val$diff)), 6), "\n")

# Acceptance threshold
threshold <- 0.05  # 5% difference
all_close <- all(abs(year_val$diff) < threshold) &
             abs(srh_val$diff) < 0.1 &
             abs(age_val$diff) < 1

if (all_close) {
  cat("\n✓ All weighted distributions are within acceptable tolerance.\n")
} else {
  cat("\n✗ Some distributions exceed tolerance - check results.\n")
}

# ==============================================================================
# Test 5: Check weight adjustment
# ==============================================================================

cat("\n=== Test 5: Weight adjustment verification ===\n")

# Sum of original weights
orig_wt_sum <- sum(fake_data$wt)
# Sum of adjusted weights in subsample
sub_wt_sum <- sum(result$data$wt_sub)

cat("Sum of original weights:", round(orig_wt_sum, 2), "\n")
cat("Sum of adjusted weights:", round(sub_wt_sum, 2), "\n")
cat("Ratio (should be close to 1):", round(sub_wt_sum / orig_wt_sum, 4), "\n")

# ==============================================================================
# Test 6: Missing column error
# ==============================================================================

cat("\n=== Test 6: Missing column error handling ===\n")
bad_data <- fake_data %>% select(-srh)
tryCatch(
  subsample_survey(bad_data, target_n = 100),
  error = function(e) cat("Expected error caught:", e$message, "\n")
)

# ==============================================================================
# Test 7: Larger dataset where subsampling actually reduces size
# ==============================================================================

cat("\n=== Test 7: Larger dataset (10,000 -> 2,000) ===\n")

set.seed(123)
large_fake <- tibble(
  id = 1:10000,
  year = sample(years, 10000, replace = TRUE),
  age = sample(age_range, 10000, replace = TRUE),
  srh = sample(1:5, 10000, replace = TRUE, prob = c(0.05, 0.10, 0.25, 0.35, 0.25)),
  wt = runif(10000, min = 0.5, max = 2.0)
)

result_large <- subsample_survey(large_fake, target_n = 2000)

cat("Original n:", nrow(large_fake), "\n")
cat("Target n: 2000\n")
cat("Final n:", nrow(result_large$data), "\n")
cat("Actual reduction achieved: ",
    round(100 * (1 - nrow(result_large$data) / nrow(large_fake)), 1), "%\n")

# Check weighted means preserved
val <- result_large$validation
srh_diff <- val$diff[val$variable == "srh_mean"]
age_diff <- val$diff[val$variable == "age_mean"]
year_max_diff <- max(abs(val$diff[val$variable == "year_prop"]))

cat("\nWeighted distribution preservation:\n")
cat("  SRH mean diff:", round(srh_diff, 5), "\n")
cat("  Age mean diff:", round(age_diff, 5), "\n")
cat("  Max year prop diff:", round(year_max_diff, 5), "\n")

cat("\n=== All tests completed ===\n")
