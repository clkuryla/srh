# ==============================================================================
# subsample_survey.R
# Stratified random subsampling for large survey datasets
# For Bayesian APC analysis where full datasets are computationally prohibitive
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(tidyr)

#' Subsample survey data with stratified random sampling
#'
#' Performs stratified random subsampling on large survey datasets while
#' preserving weighted distributions. Stratifies by year × age group to
#' maintain APC structure.
#'
#' @param data Data frame with survey data (must contain: age, year, srh, wt)
#' @param target_n Target sample size (integer)
#' @param age_bin_width Width of age bins for stratification (default = 5)
#' @param min_cell_size Minimum observations per year × age stratum (default = 30)
#' @param seed Random seed for reproducibility (default = 1248)
#'
#' @return A list with two elements:
#'   - data: the subsampled data frame with wt_sub column added
#'   - validation: data frame comparing weighted means before/after
#'
#' @details
#' Method:
#' 1. Creates age groups by binning age into age_bin_width-year bins
#' 2. Creates strata as year × age group
#' 3. Calculates overall sampling fraction f = target_n / nrow(data)
#' 4. Within each stratum:
#'    - Target n = max(min_cell_size, round(n_in_stratum × f))
#'    - If stratum is smaller than target, keep all observations
#'    - Otherwise, sample without replacement
#'    - Adjust weights: wt_sub = wt / f_stratum
#'
#' @examples
#' result <- subsample_survey(large_df, target_n = 50000)
#' subsampled_df <- result$data
#' print(result$validation)
subsample_survey <- function(data,
                              target_n,
                              age_bin_width = 5,
                              min_cell_size = 30,
                              seed = 1248) {


  # ---- Input validation ----
  required_cols <- c("age", "year", "srh", "wt")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.numeric(target_n) || target_n <= 0) {
    stop("target_n must be a positive integer")
  }

  n_original <- nrow(data)

  if (target_n >= n_original) {
    message("target_n (", target_n, ") >= data size (", n_original, "). Returning original data.")
    data$wt_sub <- data$wt
    validation <- compute_validation(data, data, "wt", "wt_sub")
    return(list(data = data, validation = validation))
  }

  set.seed(seed)

  # ---- Create age bins for stratification ----
  data <- data %>%
    mutate(
      age_bin = floor(age / age_bin_width) * age_bin_width,
      stratum = paste(year, age_bin, sep = "_")
    )

  # ---- Calculate stratum sizes and sampling targets ----
  overall_f <- target_n / n_original

  stratum_info <- data %>%
    group_by(stratum) %>%
    summarise(n_stratum = n(), .groups = "drop") %>%
    mutate(
      # Target n for this stratum
      target_stratum = pmax(min_cell_size, round(n_stratum * overall_f)),
      # Cannot sample more than stratum size
      n_sample = pmin(target_stratum, n_stratum)
    )

  # ---- Perform stratified sampling ----
  subsampled <- data %>%
    group_by(stratum) %>%
    group_modify(function(stratum_data, key) {
      n_stratum <- nrow(stratum_data)

      # Look up target for this stratum
      target_info <- stratum_info %>%
        filter(stratum == key$stratum)

      n_sample <- target_info$n_sample

      if (n_sample >= n_stratum) {
        # Keep all observations
        stratum_data$f_stratum <- 1
        return(stratum_data)
      }

      # Sample without replacement
      sampled_idx <- sample(seq_len(n_stratum), size = n_sample, replace = FALSE)
      sampled_data <- stratum_data[sampled_idx, ]

      # Calculate stratum sampling fraction
      sampled_data$f_stratum <- n_sample / n_stratum

      return(sampled_data)
    }) %>%
    ungroup()

  # ---- Adjust weights ----
  subsampled <- subsampled %>%
    mutate(wt_sub = wt / f_stratum) %>%
    select(-age_bin, -stratum, -f_stratum)

  n_final <- nrow(subsampled)
  actual_f <- n_final / n_original

  # ---- Compute validation ----
  validation <- compute_validation(data, subsampled, "wt", "wt_sub")

  # ---- Print summary ----
  message("\n=== Subsampling Summary ===")
  message("  Original n:     ", format(n_original, big.mark = ","))
  message("  Target n:       ", format(target_n, big.mark = ","))
  message("  Final n:        ", format(n_final, big.mark = ","))
  message("  Sampling frac:  ", round(actual_f, 4))

  # Year proportion validation
  year_val <- validation %>% filter(variable == "year_prop")
  if (nrow(year_val) > 0) {
    max_diff <- max(abs(year_val$original - year_val$subsampled), na.rm = TRUE)
    message("  Max |diff| in year proportions: ", round(max_diff, 5))
  }

  # SRH validation
  srh_val <- validation %>% filter(variable == "srh_mean")
  if (nrow(srh_val) > 0) {
    message("  Weighted mean SRH: original = ", round(srh_val$original, 4),
            ", subsampled = ", round(srh_val$subsampled, 4))
  }

  message("===========================\n")

  # Clean up helper columns from original data before returning
  data <- data %>% select(-age_bin, -stratum)

  list(
    data = subsampled,
    validation = validation
  )
}


#' Compute validation metrics comparing original and subsampled data
#'
#' @param original Original data frame (with wt column)
#' @param subsampled Subsampled data frame (with wt_sub column)
#' @param wt_orig Name of weight column in original data
#' @param wt_sub Name of weight column in subsampled data
#'
#' @return Data frame with validation metrics
compute_validation <- function(original, subsampled, wt_orig = "wt", wt_sub = "wt_sub") {

  # Weighted mean SRH
  wt_mean_srh_orig <- weighted.mean(original$srh, original[[wt_orig]], na.rm = TRUE)
  wt_mean_srh_sub <- weighted.mean(subsampled$srh, subsampled[[wt_sub]], na.rm = TRUE)

  # Weighted mean age
  wt_mean_age_orig <- weighted.mean(original$age, original[[wt_orig]], na.rm = TRUE)
  wt_mean_age_sub <- weighted.mean(subsampled$age, subsampled[[wt_sub]], na.rm = TRUE)

  # Start building validation data frame
  val_df <- tibble(
    variable = c("srh_mean", "age_mean"),
    original = c(wt_mean_srh_orig, wt_mean_age_orig),
    subsampled = c(wt_mean_srh_sub, wt_mean_age_sub),
    diff = c(wt_mean_srh_sub - wt_mean_srh_orig, wt_mean_age_sub - wt_mean_age_orig),
    level = c(NA_character_, NA_character_)
  )

  # Weighted proportion by year
  total_wt_orig <- sum(original[[wt_orig]], na.rm = TRUE)
  total_wt_sub <- sum(subsampled[[wt_sub]], na.rm = TRUE)

  year_props_orig <- original %>%
    group_by(year) %>%
    summarise(wt_sum = sum(.data[[wt_orig]], na.rm = TRUE), .groups = "drop") %>%
    mutate(prop = wt_sum / total_wt_orig) %>%
    select(year, prop_orig = prop)

  year_props_sub <- subsampled %>%
    group_by(year) %>%
    summarise(wt_sum = sum(.data[[wt_sub]], na.rm = TRUE), .groups = "drop") %>%
    mutate(prop = wt_sum / total_wt_sub) %>%
    select(year, prop_sub = prop)

  year_props <- full_join(year_props_orig, year_props_sub, by = "year") %>%
    mutate(
      prop_orig = replace_na(prop_orig, 0),
      prop_sub = replace_na(prop_sub, 0)
    )

  year_val <- tibble(
    variable = "year_prop",
    original = year_props$prop_orig,
    subsampled = year_props$prop_sub,
    diff = year_props$prop_sub - year_props$prop_orig,
    level = as.character(year_props$year)
  )

  bind_rows(val_df, year_val)
}


#' Quick validation print for subsampled data
#'
#' @param result Output from subsample_survey()
print_validation_summary <- function(result) {
  val <- result$validation

  cat("\n=== Validation Summary ===\n\n")

  # Marginal means
  marginals <- val %>% filter(is.na(level))
  cat("Weighted Marginal Means:\n")
  cat(sprintf("  %-12s  %10s  %10s  %10s\n", "Variable", "Original", "Subsampled", "Diff"))
  cat(sprintf("  %-12s  %10s  %10s  %10s\n", "--------", "--------", "----------", "----"))
  for (i in seq_len(nrow(marginals))) {
    cat(sprintf("  %-12s  %10.4f  %10.4f  %10.5f\n",
                marginals$variable[i],
                marginals$original[i],
                marginals$subsampled[i],
                marginals$diff[i]))
  }

  # Year proportions
  year_val <- val %>% filter(variable == "year_prop") %>% arrange(as.numeric(level))
  cat("\nWeighted Year Proportions:\n")
  cat(sprintf("  %-8s  %10s  %10s  %10s\n", "Year", "Original", "Subsampled", "Diff"))
  cat(sprintf("  %-8s  %10s  %10s  %10s\n", "----", "--------", "----------", "----"))
  for (i in seq_len(nrow(year_val))) {
    cat(sprintf("  %-8s  %10.4f  %10.4f  %10.5f\n",
                year_val$level[i],
                year_val$original[i],
                year_val$subsampled[i],
                year_val$diff[i]))
  }

  cat("\nMax |diff| in year proportions:",
      round(max(abs(year_val$diff), na.rm = TRUE), 6), "\n")
  cat("==========================\n")
}
