# Harmonize race/ethnicity variables across all surveys
#
# Run AFTER all individual survey wrangling scripts have been run.
# This script adds/overwrites harmonized race variables to each survey data file:
#   - race_5cat: Numeric 1-5 (1=White, 2=Black, 3=AIAN, 4=Asian/PI, 5=Other)
#   - race_5cat_f: Factor version of race_5cat
#   - hispanic: Binary 0/1 (0=Not Hispanic, 1=Hispanic, NA=Unknown)
#   - hispanic_f: Factor version ("Not Hispanic", "Hispanic")
#   - race_includehisp: Numeric 1-6 (1=NH-White, 2=NH-Black, 3=NH-AIAN,
#                       4=NH-Asian/PI, 5=Hispanic, 6=Other)
#   - race_includehisp_f: Factor version with full labels
#
# KEY FIX: When hispanic is NA (unknown), we use the race category directly
# rather than coding everyone as "Other". This preserves race information
# for surveys/years that don't have Hispanic ethnicity data.
#
# Original coding across surveys:
#   BRFSS:  _IMPRACE = 1-6 (computed variable), HISPANIC/HISPANC2/HISPANC3
#   CPS:    RACE = IPUMS codes (100=White, 200=Black, etc.), HISPAN = detailed codes
#   GSS:    race = 1=White, 2=Black, 3=Other; hispanic = 0/1/NA
#   MEPS:   race = character (already cleaned), hispanic = 0/1
#   NHIS:   RACEA = IPUMS codes, HISPYN/HISPETH for Hispanic
#   NHANES: RIDRETH1 = combined race/ethnicity (already includes Hispanic)
#
# This script is idempotent: safe to re-run without corrupting data.

library(tidyverse)
library(here)
source(here::here("R/paths.R"))

# ==============================================================================
# CONSTANTS
# ==============================================================================

# Standard race levels
RACE_5CAT_LEVELS <- c("White", "Black", "AIAN", "Asian/PI", "Other")

# Race with Hispanic levels
RACE_HISP_LEVELS <- c("NH-White", "NH-Black", "NH-AIAN", "NH-Asian/PI", "Hispanic", "Other")

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Convert numeric race_5cat to factor
race_5cat_to_factor <- function(x) {
  factor(x, levels = 1:5, labels = RACE_5CAT_LEVELS)
}

#' Convert numeric race_includehisp to factor
race_includehisp_to_factor <- function(x) {
  factor(x, levels = 1:6, labels = RACE_HISP_LEVELS)
}

#' Create race_includehisp from race_5cat and hispanic
#'
#' KEY LOGIC: When Hispanic is unknown (NA), we assume non-Hispanic and use
#' the race category. This preserves race information rather than coding
#' everyone as "Other".
#'
#' @param race_5cat Numeric 1-5 race category
#' @param hispanic Binary 0/1 Hispanic indicator (can be NA)
#' @return Numeric 1-6 race with Hispanic category
create_race_includehisp <- function(race_5cat, hispanic) {
  case_when(
    # If Hispanic == 1, they are Hispanic (category 5)
    !is.na(hispanic) & hispanic == 1 ~ 5L,
    # If Hispanic == 0 or NA, use race category
    # (NH-White=1, NH-Black=2, NH-AIAN=3, NH-Asian/PI=4)
    # "Other" race stays as "Other" (category 6)
    race_5cat == 1 ~ 1L,  # White -> NH-White
    race_5cat == 2 ~ 2L,  # Black -> NH-Black
    race_5cat == 3 ~ 3L,  # AIAN -> NH-AIAN
    race_5cat == 4 ~ 4L,  # Asian/PI -> NH-Asian/PI
    race_5cat == 5 ~ 6L,  # Other -> Other
    TRUE ~ NA_integer_
  )
}

# ==============================================================================
# BRFSS PROCESSING
# ==============================================================================

process_brfss_race <- function(df) {
  message("  Processing BRFSS race...")

  # BRFSS has multiple race variables across years
  # _IMPRACE (2013+): 1=NH-White, 2=NH-Black, 3=NH-Asian, 4=NH-AIAN, 5=Hispanic, 6=Other
  # _RACE (2001-2012): 1=White, 2=Black, 3=AIAN, 4=Asian, 5=NHOPI, 6=Other, 7=Multi, 8=Hisp
  # ORACE (1993-2000): 1=White, 2=Black, 3=Asian/PI, 4=AIAN, 5=Other
  # HISPANIC: 1=Hispanic, 2=Not Hispanic

  df <- df %>%
    mutate(
      # Create race_5cat from available race variables
      race_5cat = case_when(
        # Priority 1: _IMPRACE (2013+)
        !is.na(`_IMPRACE`) & `_IMPRACE` == 1 ~ 1L,  # White
        !is.na(`_IMPRACE`) & `_IMPRACE` == 2 ~ 2L,  # Black
        !is.na(`_IMPRACE`) & `_IMPRACE` == 4 ~ 3L,  # AIAN
        !is.na(`_IMPRACE`) & `_IMPRACE` == 3 ~ 4L,  # Asian
        !is.na(`_IMPRACE`) & `_IMPRACE` %in% c(5, 6) ~ 5L,  # Hispanic or Other

        # Priority 2: _RACE (2001-2012)
        !is.na(`_RACE`) & `_RACE` == 1 ~ 1L,  # White
        !is.na(`_RACE`) & `_RACE` == 2 ~ 2L,  # Black
        !is.na(`_RACE`) & `_RACE` == 3 ~ 3L,  # AIAN
        !is.na(`_RACE`) & `_RACE` %in% c(4, 5) ~ 4L,  # Asian/NHOPI
        !is.na(`_RACE`) & `_RACE` %in% c(6, 7, 8, 9) ~ 5L,  # Other/Multi

        # Priority 3: ORACE (1993-2000)
        !is.na(ORACE) & ORACE == 1 ~ 1L,  # White
        !is.na(ORACE) & ORACE == 2 ~ 2L,  # Black
        !is.na(ORACE) & ORACE == 4 ~ 3L,  # AIAN (ORACE 4=AIAN)
        !is.na(ORACE) & ORACE == 3 ~ 4L,  # Asian/PI (ORACE 3=Asian/PI)
        !is.na(ORACE) & ORACE %in% c(5, 6, 7, 8, 9) ~ 5L,  # Other
        TRUE ~ NA_integer_
      ),

      # Hispanic from available variables
      hispanic = case_when(
        # From _IMPRACE
        !is.na(`_IMPRACE`) & `_IMPRACE` == 5 ~ 1L,
        !is.na(`_IMPRACE`) & `_IMPRACE` %in% 1:4 ~ 0L,
        # From _RACE
        !is.na(`_RACE`) & `_RACE` == 8 ~ 1L,
        # From HISPANIC variable
        !is.na(HISPANIC) & HISPANIC == 1 ~ 1L,
        !is.na(HISPANIC) & HISPANIC == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),

      # race_includehisp: Use _IMPRACE directly if available, else compute
      race_includehisp = case_when(
        !is.na(`_IMPRACE`) & `_IMPRACE` == 1 ~ 1L,  # NH-White
        !is.na(`_IMPRACE`) & `_IMPRACE` == 2 ~ 2L,  # NH-Black
        !is.na(`_IMPRACE`) & `_IMPRACE` == 4 ~ 3L,  # NH-AIAN
        !is.na(`_IMPRACE`) & `_IMPRACE` == 3 ~ 4L,  # NH-Asian/PI
        !is.na(`_IMPRACE`) & `_IMPRACE` == 5 ~ 5L,  # Hispanic
        !is.na(`_IMPRACE`) & `_IMPRACE` == 6 ~ 6L,  # Other
        # Fallback: compute from race_5cat and hispanic
        TRUE ~ create_race_includehisp(race_5cat, hispanic)
      ),

      # Factor versions
      race_5cat_f = race_5cat_to_factor(race_5cat),
      hispanic_f = factor(hispanic, levels = 0:1, labels = c("Not Hispanic", "Hispanic")),
      race_includehisp_f = race_includehisp_to_factor(race_includehisp)
    )

  return(df)
}

# ==============================================================================
# CPS PROCESSING
# ==============================================================================

process_cps_race <- function(df) {
  message("  Processing CPS race...")

  # CPS RACE codes (IPUMS):
  # 100 = White
  # 200 = Black
  # 300 = AIAN
  # 650-652 = Asian/Pacific Islander variants
  # 700+ = Multiple races
  # 801-830 = Detailed Asian/PI or multi-race combinations

  # HISPAN codes:
  # 0 = Not Hispanic
  # 100-612 = Various Hispanic origins (Mexican, Puerto Rican, Cuban, etc.)
  # 901-902 = Hispanic type not specified

  df <- df %>%
    mutate(
      race_5cat = case_when(
        RACE == 100 ~ 1L,  # White
        RACE == 200 ~ 2L,  # Black
        RACE == 300 ~ 3L,  # AIAN
        RACE >= 650 & RACE < 700 ~ 4L,  # Asian/PI
        RACE >= 700 ~ 5L,  # Multi-race -> Other
        TRUE ~ NA_integer_
      ),

      hispanic = case_when(
        HISPAN == 0 ~ 0L,  # Not Hispanic
        HISPAN > 0 & HISPAN < 900 ~ 1L,  # Hispanic (various origins)
        HISPAN >= 901 ~ 1L,  # Hispanic (type not specified)
        TRUE ~ NA_integer_
      ),

      race_includehisp = create_race_includehisp(race_5cat, hispanic),

      race_5cat_f = race_5cat_to_factor(race_5cat),
      hispanic_f = factor(hispanic, levels = 0:1, labels = c("Not Hispanic", "Hispanic")),
      race_includehisp_f = race_includehisp_to_factor(race_includehisp)
    )

  return(df)
}

# ==============================================================================
# GSS PROCESSING
# ==============================================================================

process_gss_race <- function(df) {
  message("  Processing GSS race...")

  # GSS race: 1=White, 2=Black, 3=Other
  # GSS hispanic: 0=Not Hispanic, 1=Hispanic, NA=not asked

  df <- df %>%
    mutate(
      race_5cat = case_when(
        race == 1 ~ 1L,  # White
        race == 2 ~ 2L,  # Black
        race == 3 ~ 5L,  # Other
        TRUE ~ NA_integer_
      ),

      # hispanic is already 0/1/NA
      hispanic = as.integer(hispanic),

      race_includehisp = create_race_includehisp(race_5cat, hispanic),

      race_5cat_f = race_5cat_to_factor(race_5cat),
      hispanic_f = factor(hispanic, levels = 0:1, labels = c("Not Hispanic", "Hispanic")),
      race_includehisp_f = race_includehisp_to_factor(race_includehisp)
    )

  return(df)
}

# ==============================================================================
# MEPS PROCESSING
# ==============================================================================

process_meps_race <- function(df) {
  message("  Processing MEPS race...")

  # MEPS race is already cleaned to character: AAPI, AIAN, Black, White, Other
  # hispanic is already 0/1

  df <- df %>%
    mutate(
      race_5cat = case_when(
        race == "White" ~ 1L,
        race == "Black" ~ 2L,
        race == "AIAN" ~ 3L,
        race == "AAPI" ~ 4L,  # AAPI = Asian American / Pacific Islander
        race == "Other" ~ 5L,
        TRUE ~ NA_integer_
      ),

      # hispanic is already 0/1
      hispanic = as.integer(hispanic),

      race_includehisp = create_race_includehisp(race_5cat, hispanic),

      race_5cat_f = race_5cat_to_factor(race_5cat),
      hispanic_f = factor(hispanic, levels = 0:1, labels = c("Not Hispanic", "Hispanic")),
      race_includehisp_f = race_includehisp_to_factor(race_includehisp)
    )

  return(df)
}

# ==============================================================================
# NHIS PROCESSING
# ==============================================================================

process_nhis_race <- function(df) {
  message("  Processing NHIS race...")

  # NHIS has different race variables across years due to 2019 redesign:
  #
  # Pre-2019 (RACEA - IPUMS):
  #   100 = White, 200 = Black, 300 = AIAN, 400 = Asian/PI, 500+ = Multiple
  #
  # Post-2019 (RACENEW - IPUMS):
  #   100 = White, 200 = Black, 300 = AIAN, 400 = Asian, 510+ = Multiple
  #
  # Hispanic variables:
  #   HISPYN: 1=Not Hispanic, 2=Hispanic (1997+)
  #   HISPETH: 10=Not Hispanic, 20-70=Hispanic (available all years)

  df <- df %>%
    mutate(
      race_5cat = case_when(
        # Priority 1: RACEA (pre-2019)
        !is.na(RACEA) & RACEA == 100 ~ 1L,  # White
        !is.na(RACEA) & RACEA == 200 ~ 2L,  # Black
        !is.na(RACEA) & RACEA == 300 ~ 3L,  # AIAN
        !is.na(RACEA) & RACEA == 400 ~ 4L,  # Asian/PI
        !is.na(RACEA) & RACEA >= 500 & RACEA < 900 ~ 5L,  # Multiple/Other

        # Priority 2: RACENEW (post-2019)
        !is.na(RACENEW) & RACENEW == 100 ~ 1L,  # White
        !is.na(RACENEW) & RACENEW == 200 ~ 2L,  # Black
        !is.na(RACENEW) & RACENEW == 300 ~ 3L,  # AIAN
        !is.na(RACENEW) & RACENEW == 400 ~ 4L,  # Asian
        !is.na(RACENEW) & RACENEW >= 500 & RACENEW < 900 ~ 5L,  # Multiple/Other

        TRUE ~ NA_integer_
      ),

      # Hispanic: Use HISPYN if available, else derive from HISPETH
      hispanic = case_when(
        # HISPYN coding: 1=Not Hispanic, 2=Hispanic
        !is.na(HISPYN) & HISPYN == 1 ~ 0L,
        !is.na(HISPYN) & HISPYN == 2 ~ 1L,
        !is.na(HISPYN) & HISPYN %in% c(7, 9) ~ NA_integer_,
        # Fallback to HISPETH: 10=Not Hispanic, 20-70=Hispanic
        !is.na(HISPETH) & HISPETH == 10 ~ 0L,
        !is.na(HISPETH) & HISPETH >= 20 & HISPETH < 90 ~ 1L,
        TRUE ~ NA_integer_
      ),

      race_includehisp = create_race_includehisp(race_5cat, hispanic),

      race_5cat_f = race_5cat_to_factor(race_5cat),
      hispanic_f = factor(hispanic, levels = 0:1, labels = c("Not Hispanic", "Hispanic")),
      race_includehisp_f = race_includehisp_to_factor(race_includehisp)
    )

  return(df)
}

# ==============================================================================
# NHANES PROCESSING
# ==============================================================================

process_nhanes_race <- function(df) {
  message("  Processing NHANES race...")

  # NHANES RIDRETH1 is already a combined race/ethnicity variable:
  # "Mexican American"
  # "Other Hispanic"
  # "Non-Hispanic White"
  # "Non-Hispanic Black"
  # "Other Race - Including Multi-Racial"

  df <- df %>%
    mutate(
      race_5cat = case_when(
        RIDRETH1 == "Non-Hispanic White" ~ 1L,
        RIDRETH1 == "Non-Hispanic Black" ~ 2L,
        # NHANES doesn't have AIAN as separate category
        # Asian is sometimes in "Other Race"
        RIDRETH1 == "Other Race - Including Multi-Racial" ~ 5L,  # Other
        # Mexican American and Other Hispanic go to "Other" for race_5cat
        # (they'll become Hispanic in race_includehisp)
        RIDRETH1 %in% c("Mexican American", "Other Hispanic") ~ 5L,
        TRUE ~ NA_integer_
      ),

      hispanic = case_when(
        RIDRETH1 %in% c("Mexican American", "Other Hispanic") ~ 1L,
        RIDRETH1 %in% c("Non-Hispanic White", "Non-Hispanic Black",
                        "Other Race - Including Multi-Racial") ~ 0L,
        TRUE ~ NA_integer_
      ),

      # For NHANES, use RIDRETH1 directly for race_includehisp
      race_includehisp = case_when(
        RIDRETH1 == "Non-Hispanic White" ~ 1L,
        RIDRETH1 == "Non-Hispanic Black" ~ 2L,
        RIDRETH1 %in% c("Mexican American", "Other Hispanic") ~ 5L,  # Hispanic
        RIDRETH1 == "Other Race - Including Multi-Racial" ~ 6L,  # Other
        TRUE ~ NA_integer_
      ),

      race_5cat_f = race_5cat_to_factor(race_5cat),
      hispanic_f = factor(hispanic, levels = 0:1, labels = c("Not Hispanic", "Hispanic")),
      race_includehisp_f = race_includehisp_to_factor(race_includehisp)
    )

  return(df)
}

# ==============================================================================
# VALIDATION FUNCTION
# ==============================================================================

validate_race <- function(df, survey_name) {
  # Check required columns exist
  required_cols <- c("race_5cat", "race_5cat_f", "hispanic", "hispanic_f",
                     "race_includehisp", "race_includehisp_f")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(survey_name, ": Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check race_5cat values are in {1-5, NA}
  valid_race <- all(na.omit(df$race_5cat) %in% 1:5)
  if (!valid_race) {
    invalid_vals <- unique(df$race_5cat[!df$race_5cat %in% c(1:5, NA)])
    stop(survey_name, ": race_5cat has invalid values: ", paste(invalid_vals, collapse = ", "))
  }

  # Check hispanic values are in {0, 1, NA}
  valid_hisp <- all(na.omit(df$hispanic) %in% c(0L, 1L))
  if (!valid_hisp) {
    invalid_vals <- unique(df$hispanic[!df$hispanic %in% c(0L, 1L, NA)])
    stop(survey_name, ": hispanic has invalid values: ", paste(invalid_vals, collapse = ", "))
  }

  # Check race_includehisp values are in {1-6, NA}
  valid_race_hisp <- all(na.omit(df$race_includehisp) %in% 1:6)
  if (!valid_race_hisp) {
    invalid_vals <- unique(df$race_includehisp[!df$race_includehisp %in% c(1:6, NA)])
    stop(survey_name, ": race_includehisp has invalid values: ", paste(invalid_vals, collapse = ", "))
  }

  # Check factor levels are correct
  if (!all(levels(df$race_5cat_f) == RACE_5CAT_LEVELS)) {
    stop(survey_name, ": race_5cat_f has incorrect factor levels")
  }
  if (!all(levels(df$race_includehisp_f) == RACE_HISP_LEVELS)) {
    stop(survey_name, ": race_includehisp_f has incorrect factor levels")
  }

  invisible(TRUE)
}

# ==============================================================================
# MAIN PROCESSING
# ==============================================================================

cat("=== Harmonizing race/ethnicity variables across surveys ===\n\n")

# Define surveys and their processing functions
surveys <- list(
  list(name = "BRFSS",  file = "data_brfss.rds",  process = process_brfss_race),
  list(name = "CPS",    file = "data_cps.rds",    process = process_cps_race),
  list(name = "GSS",    file = "data_gss.rds",    process = process_gss_race),
  list(name = "MEPS",   file = "data_meps.rds",   process = process_meps_race),
  list(name = "NHANES", file = "data_nhanes.rds", process = process_nhanes_race),
  list(name = "NHIS",   file = "data_nhis.rds",   process = process_nhis_race)
)

# Track results for summary
results <- list()

for (s in surveys) {
  file_path <- derived_path(s$file)

  # Check file exists
  if (!file.exists(file_path)) {
    message(s$name, ": File not found (", s$file, "), skipping")
    next
  }

  message("Processing ", s$name, "...")

  # Load, process, validate, save
  df <- readRDS(file_path)
  df <- s$process(df)
  validate_race(df, s$name)
  saveRDS(df, file_path)

  # Store results for summary
  race_dist <- df %>%
    count(race_includehisp_f) %>%
    mutate(pct = round(100 * n / sum(n), 1))

  results[[s$name]] <- tibble(
    Survey = s$name,
    N = nrow(df),
    pct_race_known = round(100 * mean(!is.na(df$race_5cat)), 1),
    pct_hisp_known = round(100 * mean(!is.na(df$hispanic)), 1),
    pct_NH_White = race_dist$pct[race_dist$race_includehisp_f == "NH-White"],
    pct_NH_Black = race_dist$pct[race_dist$race_includehisp_f == "NH-Black"],
    pct_Hispanic = race_dist$pct[race_dist$race_includehisp_f == "Hispanic"],
    pct_Other = race_dist$pct[race_dist$race_includehisp_f == "Other"]
  )

  message("  Saved: ", file_path, "\n")
  rm(df)
  gc(verbose = FALSE)
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n=== Summary ===\n\n")

summary_df <- bind_rows(results)
print(summary_df, n = 10, width = Inf)

cat("\nRace/ethnicity variable labels:\n")
cat("  race_5cat: 1=White, 2=Black, 3=AIAN, 4=Asian/PI, 5=Other\n")
cat("  race_includehisp: 1=NH-White, 2=NH-Black, 3=NH-AIAN, 4=NH-Asian/PI, 5=Hispanic, 6=Other\n")
cat("  hispanic: 0=Not Hispanic, 1=Hispanic, NA=Unknown\n")
cat("\nKEY: When Hispanic is unknown (NA), race categories are preserved as NH-* rather than Other.\n")

cat("\nDone.\n")
