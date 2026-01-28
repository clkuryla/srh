# ==============================================================================
# bhapc_data_prep.R
# Data preparation for Bayesian Hierarchical APC (BHAPC) analysis
# Based on Gloria Graf's methodology
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(tidyr)

# ==============================================================================
# Survey-Specific Period Mapping
# ==============================================================================

#' Map survey years to 4-year periods
#'
#' Creates 4-year period groupings following Gloria's approach.
#' Different surveys have different year structures.
#'
#' @param year Numeric vector of survey years
#' @param survey Character: survey name (nhanes, brfss, meps, nhis, gss, cps)
#' @return Numeric vector of period start years (e.g., 1999, 2003, 2007, ...)
map_to_period_4yr <- function(year, survey = "nhanes") {
  survey <- tolower(survey)

  if (survey == "nhanes") {
    # NHANES: 2-year cycles with midpoint years (1999.5, 2001.5, etc.)
    # Map to 4-year windows using numeric ranges
    # 1999-2000 (1999.5), 2001-2002 (2001.5) -> 1999
    # 2003-2004 (2003.5), 2005-2006 (2005.5) -> 2003
    # etc.
    period <- case_when(
      year >= 1999 & year < 2003 ~ 1999L,
      year >= 2003 & year < 2007 ~ 2003L,
      year >= 2007 & year < 2011 ~ 2007L,
      year >= 2011 & year < 2015 ~ 2011L,
      year >= 2015 & year < 2019 ~ 2015L,
      year >= 2019 & year <= 2023 ~ 2019L,
      TRUE ~ NA_integer_
    )
  } else {
    # For annual surveys, use simple 4-year windows
    # 1999-2002 -> 1999, 2003-2006 -> 2003, etc.
    period <- as.integer(floor((year - 1999) / 4) * 4 + 1999)

    # Handle pre-1999 years if present
    period <- ifelse(year < 1999,
                     as.integer(floor((year - 1995) / 4) * 4 + 1995),
                     period)
  }

  period
}


# ==============================================================================
# Age Group Creation
# ==============================================================================

#' Create mixed age groups (4-yr young, 5-yr older)
#'
#' Creates age groups with finer granularity for young adults:
#' 18-21, 22-25, 26-29 (4-year bins)
#' 30-34, 35-39, ..., 85-89 (5-year bins)
#'
#' @param age Numeric vector of ages
#' @return Factor with age group labels
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

  # Define factor levels in order
  levels <- c("18-21", "22-25", "26-29", "30-34", "35-39", "40-44",
              "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
              "75-79", "80-84", "85-89")

  factor(age_group, levels = levels)
}


#' Get age group midpoint for cohort calculation
#'
#' @param age_group Factor or character age group
#' @return Numeric midpoint
get_age_midpoint <- function(age_group) {
  # Parse the age group string to get bounds
  bounds <- strsplit(as.character(age_group), "-")

  sapply(bounds, function(b) {
    if (length(b) == 2) {
      (as.numeric(b[1]) + as.numeric(b[2])) / 2
    } else {
      NA_real_
    }
  })
}


# ==============================================================================
# Main Data Preparation Function
# ==============================================================================

#' Prepare data for BHAPC analysis
#'
#' Prepares survey data for Bayesian HAPC analysis following Gloria's methodology.
#' Creates APC variables, survey weight covariate, and filters to valid observations.
#'
#' @param df Data frame with columns: year, age, srh, and optionally wt, strata, psu
#' @param survey Character: survey name (for period mapping)
#' @param age_min Minimum age to include (default 18)
#' @param age_max Maximum age to include (default 90)
#' @param srh_scale Maximum SRH value (4 for GSS, 5 for others)
#' @return Data frame ready for BHAPC analysis
#'
#' @details
#' Creates the following variables:
#' - period_4yr: 4-year period grouping
#' - age_group: Mixed age bins (4-yr young, 5-yr older)
#' - age_midpoint: Midpoint of age group
#' - cohort_4yr: Birth cohort (period - age_midpoint)
#' - age_squared: Age squared for quadratic term
#' - lnWt: Log of survey weight (or 0 if weight is 0)
#'
#' @examples
#' bhapc_df <- prepare_bhapc_data(nhanes_df, survey = "nhanes")
prepare_bhapc_data <- function(df,
                                survey = "nhanes",
                                age_min = 18,
                                age_max = 90,
                                srh_scale = 5) {

  survey <- tolower(survey)

  # Check required columns

stopifnot("year" %in% names(df))
  stopifnot("age" %in% names(df))
  stopifnot("srh" %in% names(df))

  message("Preparing BHAPC data for ", toupper(survey))
  message("  Initial rows: ", format(nrow(df), big.mark = ","))

  # Check actual age range in data
  actual_min_age <- min(df$age, na.rm = TRUE)
  actual_max_age <- max(df$age, na.rm = TRUE)

  message("  Observed age range: ", actual_min_age, " - ", actual_max_age)

  # Apply effective bounds
  effective_min_age <- max(age_min, actual_min_age)
  effective_max_age <- min(age_max, actual_max_age)

  if (effective_max_age < age_max) {
    message("  NOTE: Survey age cap detected. Using max age = ", effective_max_age)
  }

  # Determine weight column
  wt_col <- intersect(c("wt", "weight", "WTMEC4YR", "WTMEC2YR"), names(df))
  has_weights <- length(wt_col) > 0

  if (has_weights) {
    wt_col <- wt_col[1]
    message("  Using weight column: ", wt_col)
  } else {
    message("  No weight column found; setting lnWt = 0")
  }

  # Create BHAPC dataset
  bhapc_df <- df %>%
    # Filter to valid ages
    filter(!is.na(age),
           age >= effective_min_age,
           age <= effective_max_age) %>%
    # Filter to valid SRH
    filter(!is.na(srh),
           srh >= 1,
           srh <= srh_scale) %>%
    # Create period
    mutate(period_4yr = map_to_period_4yr(year, survey)) %>%
    filter(!is.na(period_4yr)) %>%
    # Create age groups
    mutate(
      age_group = create_age_groups_mixed(age),
      age_midpoint = get_age_midpoint(age_group)
    ) %>%
    filter(!is.na(age_group)) %>%
    # Create cohort
    mutate(
      cohort_4yr = period_4yr - age_midpoint,
      # Round to nearest 4-year cohort for grouping
      cohort_4yr = round(cohort_4yr / 4) * 4
    ) %>%
    # Create age squared
    mutate(age_squared = age^2) %>%
    # Create log weight covariate
    mutate(
      lnWt = if (has_weights) {
        ifelse(.data[[wt_col]] == 0 | is.na(.data[[wt_col]]), 0, log(.data[[wt_col]]))
      } else {
        0
      }
    ) %>%
    # Convert period and cohort to factors for random effects
    mutate(
      period_4yr = as.character(period_4yr),
      cohort_4yr = as.character(cohort_4yr)
    )

  # Report results
  message("  Final rows: ", format(nrow(bhapc_df), big.mark = ","))
  message("  Years: ", min(df$year[df$year %in% as.numeric(unique(bhapc_df$period_4yr))], na.rm = TRUE),
          "-", max(df$year, na.rm = TRUE))
  message("  Periods: ", paste(sort(unique(bhapc_df$period_4yr)), collapse = ", "))
  message("  Age groups: ", length(unique(bhapc_df$age_group)))
  message("  Cohorts: ", length(unique(bhapc_df$cohort_4yr)))
  message("  Mean SRH: ", round(mean(bhapc_df$srh, na.rm = TRUE), 2))

  bhapc_df
}


# ==============================================================================
# Diagnostic Functions
# ==============================================================================

#' Create cell coverage table
#'
#' Generates a table showing N per age group x period cell
#' Useful for identifying sparse cells that may cause model issues
#'
#' @param bhapc_df Data frame from prepare_bhapc_data()
#' @return Data frame with cell counts
create_cell_coverage <- function(bhapc_df) {
  coverage <- bhapc_df %>%
    group_by(age_group, period_4yr) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = period_4yr, values_from = n, values_fill = 0)

  # Add row totals
  coverage <- coverage %>%
    mutate(Total = rowSums(select(., -age_group)))

  # Add column totals
  totals <- coverage %>%
    summarise(across(-age_group, sum)) %>%
    mutate(age_group = "Total")

  coverage <- bind_rows(coverage, totals)

  coverage
}


#' Create cohort coverage table
#'
#' Generates a table showing N per cohort
#'
#' @param bhapc_df Data frame from prepare_bhapc_data()
#' @return Data frame with cohort counts
create_cohort_coverage <- function(bhapc_df) {
  bhapc_df %>%
    group_by(cohort_4yr) %>%
    summarise(
      n = n(),
      mean_age = round(mean(age, na.rm = TRUE), 1),
      min_year = min(as.numeric(period_4yr), na.rm = TRUE),
      max_year = max(as.numeric(period_4yr), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(as.numeric(cohort_4yr))
}


#' Summarize BHAPC data preparation
#'
#' Creates a summary of the prepared BHAPC dataset
#'
#' @param bhapc_df Data frame from prepare_bhapc_data()
#' @return List with summary statistics
summarize_bhapc_data <- function(bhapc_df) {
  list(
    n_obs = nrow(bhapc_df),
    n_periods = length(unique(bhapc_df$period_4yr)),
    n_age_groups = length(unique(bhapc_df$age_group)),
    n_cohorts = length(unique(bhapc_df$cohort_4yr)),
    periods = sort(unique(bhapc_df$period_4yr)),
    age_groups = levels(bhapc_df$age_group)[levels(bhapc_df$age_group) %in% unique(bhapc_df$age_group)],
    cohorts = sort(unique(bhapc_df$cohort_4yr)),
    srh_mean = mean(bhapc_df$srh, na.rm = TRUE),
    srh_sd = sd(bhapc_df$srh, na.rm = TRUE),
    age_range = range(bhapc_df$age, na.rm = TRUE),
    has_strata = "strata" %in% names(bhapc_df) && !all(is.na(bhapc_df$strata)),
    has_psu = "psu" %in% names(bhapc_df) && !all(is.na(bhapc_df$psu))
  )
}
