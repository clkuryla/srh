# Harmonize sex variables across all surveys
#
# Run AFTER all individual survey wrangling scripts have been run.
# This script adds harmonized sex variables to each survey data file:
#   - sex_orig: Original numeric value (1 = Male, 2 = Female)
#   - sex: Harmonized character ("Male" or "Female")
#
# Original coding across surveys:
#   BRFSS:  SEX/SEX1/_SEX = 1=Male, 2=Female (already converted to character in .rds)
#   CPS:    SEX = 1=Male, 2=Female (IPUMS)
#   GSS:    sex = 1=Male, 2=Female (gssr package, already converted to character in .rds)
#   MEPS:   SEX = 1=Male, 2=Female (IPUMS, already converted to character in .rds)
#   NHIS:   SEX = 1=Male, 2=Female, 7/9=NA (IPUMS)
#   NHANES: RIAGENDR = 1=Male, 2=Female (already converted to character in .rds)
#
# This script is idempotent: safe to re-run without corrupting data.

library(tidyverse)
library(here)
source(here::here("R/paths.R"))

# ------------------------------------------------------------------------------
# Processing functions
# ------------------------------------------------------------------------------

# Reverse-map character "Male"/"Female" back to numeric 1/2
sex_char_to_numeric <- function(sex_char) {
  case_when(
    sex_char == "Male" ~ 1L,
    sex_char == "Female" ~ 2L,
    TRUE ~ NA_integer_
  )
}

# Convert numeric 1/2 to character "Male"/"Female"
sex_numeric_to_char <- function(sex_num) {
  case_when(
    sex_num == 1 ~ "Male",
    sex_num == 2 ~ "Female",
    TRUE ~ NA_character_
  )
}

process_brfss_sex <- function(df) {
  # BRFSS: sex is already character "Male"/"Female"; original numeric dropped
  # Need to reverse-map to create sex_orig

  # Check if already processed
  if ("sex_orig" %in% names(df)) {
    message("  BRFSS: Already processed (sex_orig exists), skipping")
    return(df)
  }

  stopifnot("sex" %in% names(df))
  stopifnot(is.character(df$sex))

  df %>%
    mutate(sex_orig = sex_char_to_numeric(sex))
}

process_cps_sex <- function(df) {
  # CPS: Has both SEX (numeric) and sex (character) in .rds
  # Copy SEX → sex_orig

  # Check if already processed
  if ("sex_orig" %in% names(df)) {
    message("  CPS: Already processed (sex_orig exists), skipping")
    return(df)
  }

  stopifnot("SEX" %in% names(df))

  df %>%
    mutate(sex_orig = as.integer(SEX))
}

process_gss_sex <- function(df) {
  # GSS: sex is already character "Male"/"Female"; original numeric overwritten
  # Need to reverse-map to create sex_orig

  # Check if already processed
  if ("sex_orig" %in% names(df)) {
    message("  GSS: Already processed (sex_orig exists), skipping")
    return(df)
  }

  stopifnot("sex" %in% names(df))
  stopifnot(is.character(df$sex))

  df %>%
    mutate(sex_orig = sex_char_to_numeric(sex))
}

process_meps_sex <- function(df) {
  # MEPS: sex is already character "Male"/"Female"; original SEX dropped
  # Need to reverse-map to create sex_orig

  # Check if already processed
  if ("sex_orig" %in% names(df)) {
    message("  MEPS: Already processed (sex_orig exists), skipping")
    return(df)
  }

  stopifnot("sex" %in% names(df))
  stopifnot(is.character(df$sex))

  df %>%
    mutate(sex_orig = sex_char_to_numeric(sex))
}

process_nhis_sex <- function(df) {
  # NHIS: sex may be numeric (1, 2, 7, 9) or already character ("Male", "Female")
  # Store numeric as sex_orig, ensure sex is character
  # Values 7 and 9 are coded as NA

  # Check if already processed
  if ("sex_orig" %in% names(df)) {
    message("  NHIS: Already processed (sex_orig exists), skipping")
    # Ensure sex is character
    if (is.numeric(df$sex)) {
      df <- df %>%
        mutate(sex = sex_numeric_to_char(sex))
    }
    return(df)
  }

  stopifnot("sex" %in% names(df))

  # Check if sex is already character (from wrangle_nhis.R)
  if (is.character(df$sex)) {
    # Sex is already character - reverse-map to get sex_orig
    df <- df %>%
      mutate(sex_orig = sex_char_to_numeric(sex))
    return(df)
  }

  # Sex is numeric - convert to character
  df %>%
    mutate(
      # Store original numeric value; recode 7/9 to NA
      sex_orig = if_else(sex %in% c(1L, 2L), as.integer(sex), NA_integer_),
      # Convert to character
      sex = sex_numeric_to_char(sex)
    )
}

process_nhanes_sex <- function(df) {
  # NHANES: sex is already character "Male"/"Female"
  # Need to reverse-map to create sex_orig

  # Check if already processed
  if ("sex_orig" %in% names(df)) {
    message("  NHANES: Already processed (sex_orig exists), skipping")
    return(df)
  }

  stopifnot("sex" %in% names(df))
  stopifnot(is.character(df$sex))

  df %>%
    mutate(sex_orig = sex_char_to_numeric(sex))
}

# ------------------------------------------------------------------------------
# Validation function
# ------------------------------------------------------------------------------

validate_sex <- function(df, survey_name) {
  # Check required columns exist
  if (!("sex_orig" %in% names(df))) {
    stop(survey_name, ": sex_orig column missing")
  }
  if (!("sex" %in% names(df))) {
    stop(survey_name, ": sex column missing")
  }

  # Check sex_orig values are in {1, 2, NA}
  valid_orig <- all(na.omit(df$sex_orig) %in% c(1L, 2L))
  if (!valid_orig) {
    invalid_vals <- unique(df$sex_orig[!df$sex_orig %in% c(1L, 2L, NA)])
    stop(survey_name, ": sex_orig has invalid values: ", paste(invalid_vals, collapse = ", "))
  }

  # Check sex values are in {"Male", "Female", NA}
  valid_sex <- all(na.omit(df$sex) %in% c("Male", "Female"))
  if (!valid_sex) {
    invalid_vals <- unique(df$sex[!df$sex %in% c("Male", "Female", NA)])
    stop(survey_name, ": sex has invalid values: ", paste(invalid_vals, collapse = ", "))
  }

  # Check that sex and sex_orig are consistent
  consistent <- df %>%
    filter(!is.na(sex) & !is.na(sex_orig)) %>%
    mutate(
      expected_sex = sex_numeric_to_char(sex_orig),
      matches = sex == expected_sex
    ) %>%
    pull(matches) %>%
    all()

  if (!consistent) {
    stop(survey_name, ": sex and sex_orig values are inconsistent")
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Main processing
# ------------------------------------------------------------------------------

cat("=== Harmonizing sex variables across surveys ===\n\n")

# Define surveys and their processing functions
surveys <- list(
  list(name = "BRFSS",  file = "data_brfss.rds",  process = process_brfss_sex),
  list(name = "CPS",    file = "data_cps.rds",    process = process_cps_sex),
  list(name = "GSS",    file = "data_gss.rds",    process = process_gss_sex),
  list(name = "MEPS",   file = "data_meps.rds",   process = process_meps_sex),
  list(name = "NHANES", file = "data_nhanes.rds", process = process_nhanes_sex),
  list(name = "NHIS",   file = "data_nhis.rds",   process = process_nhis_sex)
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
  validate_sex(df, s$name)
  saveRDS(df, file_path)

  # Store results for summary
  results[[s$name]] <- tibble(
    Survey = s$name,
    N = nrow(df),
    sex_orig_pct = round(100 * mean(!is.na(df$sex_orig)), 1),
    sex_pct = round(100 * mean(!is.na(df$sex)), 1),
    pct_male = round(100 * mean(df$sex == "Male", na.rm = TRUE), 1),
    pct_female = round(100 * mean(df$sex == "Female", na.rm = TRUE), 1)
  )

  message("  Saved: ", file_path, "\n")
  rm(df)
  gc(verbose = FALSE)
}

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

cat("\n=== Summary ===\n\n")

summary_df <- bind_rows(results)
print(summary_df, n = 10)

cat("\nSex variable labels:\n")
cat("  sex_orig: Original numeric value (1 = Male, 2 = Female)\n")
cat("  sex: Harmonized character ('Male' or 'Female')\n")

cat("\nDone.\n")
