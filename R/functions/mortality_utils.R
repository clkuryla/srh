# ==============================================================================
# mortality_utils.R
# Data preparation functions for SRH-mortality analysis
# ==============================================================================

library(dplyr)
library(rlang)

# Source the age group function if not already available
# source(here::here("R", "srh_common_functions.R"))

# ------------------------------------------------------------------------------
# DATA PREPARATION
# ------------------------------------------------------------------------------

#' Prepare NHIS mortality data for analysis
#'
#' @description Filters to valid mortality records, reverses SRH coding so
#'   higher = better, creates age groups, and calculates mortality outcomes.
#'
#' @param data Raw NHIS data with mortality variables
#' @param min_age Minimum age to include (default 18)
#' @param max_age Maximum age to include (default 89)
#' @param age_scheme Age grouping scheme ("A", "B", or "C") from add_age_group()
#' @param min_year Minimum survey year (default 1982, start of 5-point SRH)
#' @param follow_up_end_year Last year of mortality follow-up (default 2019)
#'
#' @return Data frame ready for mortality analysis with columns:
#'   - survey_year: Year of survey
#'   - age_at_survey: Age at time of survey
#'   - srh: Self-rated health (1=Poor to 5=Excellent)
#'   - age_group: Factor with age bins
#'   - mortality_status: 1=deceased, 0=alive
#'   - death_year: Year of death (NA if alive)
#'   - weight: Mortality weight (MORTWT)
#'
#' @export
prepare_mortality_data <- function(data,
                                   min_age = 18,
                                   max_age = 89,
                                   age_scheme = "B",
                                   min_year = 1982,
                                   follow_up_end_year = 2019) {

  # Input checks
  required_cols <- c("YEAR", "AGE", "HEALTH", "MORTSTAT", "MORTWT")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  data_clean <- data %>%
    # Filter to valid mortality records
    filter(
      MORTSTAT %in% 1:2,      # 1=deceased, 2=alive (IPUMS coding)
      !is.na(MORTWT),         # Valid mortality weight
      MORTWT > 0,
      YEAR >= min_year,       # 5-point SRH scale
      AGE >= min_age,
      AGE <= max_age,
      HEALTH %in% 1:5         # Valid SRH response
    ) %>%
    # Create analysis variables
    mutate(
      survey_year = YEAR,
      age_at_survey = AGE,
      # Reverse SRH: IPUMS has 1=Excellent...5=Poor, we want 5=Excellent...1=Poor
      srh = 6L - as.integer(HEALTH),
      # Mortality outcome
      mortality_status = if_else(MORTSTAT == 1L, 1L, 0L),
      # Death year (if available)
      death_year = if_else(
        MORTSTAT == 1L & !is.na(MORTDODY) & MORTDODY > 0,
        as.integer(MORTDODY),
        NA_integer_
      ),
      weight = MORTWT
    )

  # Add age groups using shared function
  data_clean <- add_age_group(data_clean, age_var = age_at_survey, scheme = age_scheme)

  # Summary checks
  n_orig <- nrow(data)
  n_clean <- nrow(data_clean)
  n_deaths <- sum(data_clean$mortality_status, na.rm = TRUE)

  message(sprintf(
    "Mortality data prepared: %s records (%s from original)\n  Deaths: %s (%.1f%%)\n  Years: %d-%d\n  Ages: %d-%d",
    format(n_clean, big.mark = ","),
    format(n_orig, big.mark = ","),
    format(n_deaths, big.mark = ","),
    100 * n_deaths / n_clean,
    min(data_clean$survey_year, na.rm = TRUE),
    max(data_clean$survey_year, na.rm = TRUE),
    min(data_clean$age_at_survey, na.rm = TRUE),
    max(data_clean$age_at_survey, na.rm = TRUE)
  ))

  return(data_clean)
}

# ------------------------------------------------------------------------------
# WINDOW PREPARATION
# ------------------------------------------------------------------------------

#' Prepare data for a specific follow-up window
#'
#' @description Filters data to a survey year window and calculates
#'   survival outcomes for Cox PH models with age as the time scale.
#'
#' @param data Prepared mortality data from prepare_mortality_data()
#' @param start_year First survey year in the window
#' @param window_length Length of survey window in years (e.g., 15)
#' @param follow_up_years Length of mortality follow-up (default same as window_length)
#' @param follow_up_end_year Administrative censoring date (default 2019)
#'
#' @return Data frame with added columns:
#'   - event: 1 if died within follow-up, 0 otherwise
#'   - age_at_end: Age at death or censoring
#'
#' @export
prepare_window <- function(data,
                           start_year,
                           window_length,
                           follow_up_years = NULL,
                           follow_up_end_year = 2019) {

  # Default follow-up same as window

if (is.null(follow_up_years)) {
    follow_up_years <- window_length
  }

  end_year <- start_year + window_length - 1

  data %>%
    # Filter to survey years in this window
    filter(survey_year >= start_year, survey_year <= end_year) %>%
    mutate(
      # Max follow-up date (from survey year, not window start)
      max_follow_up_year = pmin(survey_year + follow_up_years, follow_up_end_year),

      # Event indicator: died within follow-up window
      event = case_when(
        mortality_status == 0L ~ 0L,  # Still alive at end of follow-up data
        is.na(death_year) ~ 0L,        # No death year recorded
        death_year <= max_follow_up_year ~ 1L,  # Died within window
        TRUE ~ 0L  # Died after window (treat as censored)
      ),

      # Age at end of observation (death or censoring)
      age_at_end = case_when(
        event == 1L ~ age_at_survey + (death_year - survey_year),
        TRUE ~ age_at_survey + (max_follow_up_year - survey_year)
      )
    ) %>%
    # Ensure age_at_end > age_at_survey (required for Surv())
    filter(age_at_end > age_at_survey)
}

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------------------------

#' Generate sliding window start years
#'
#' @param min_year Earliest survey year in data
#' @param max_year Latest survey year in data
#' @param window_length Length of each window
#' @param step Step size between windows (default 1 for overlapping)
#'
#' @return Vector of start years
#' @export
get_window_starts <- function(min_year, max_year, window_length, step = 1) {
  # Last window start must allow full window
  last_start <- max_year - window_length + 1
  seq(min_year, last_start, by = step)
}

#' Summarize mortality data by age group and year
#'
#' @param data Prepared mortality data
#' @return Summary tibble
#' @export
summarize_mortality_data <- function(data) {
  data %>%
    group_by(survey_year, age_group) %>%
    summarise(
      n = n(),
      n_deaths = sum(mortality_status, na.rm = TRUE),
      death_rate = mean(mortality_status, na.rm = TRUE),
      mean_srh = mean(srh, na.rm = TRUE),
      mean_age = mean(age_at_survey, na.rm = TRUE),
      .groups = "drop"
    )
}
