# Harmonize education variables across all surveys
#
# Run AFTER all individual survey wrangling scripts have been run.
# This script adds two harmonized education variables to each survey data file:
#   - educ_4cat: 4-level education (available for all 6 surveys)
#   - educ_5cat: 5-level education (CPS, GSS, MEPS, NHIS only; NA for BRFSS/NHANES)
#
# The original education variable is preserved as educ_orig.
#
# This script is idempotent: safe to re-run without corrupting data.

library(tidyverse)
library(here)
source(here::here("R/paths.R"))

# ------------------------------------------------------------------------------
# Crosswalk definitions
# ------------------------------------------------------------------------------

# CPS crosswalk (source: EDUC)
# IPUMS CPS codes: https://cps.ipums.org/cps-action/variables/EDUC
crosswalk_cps <- function(educ_orig) {
  educ_4cat <- case_when(
    educ_orig %in% c(2, 10:14, 20:22, 30:32, 40, 50, 60, 71, 72) ~ 1L,
    educ_orig == 73 ~ 2L,
    educ_orig %in% c(80, 81, 90, 91, 92, 100) ~ 3L,
    educ_orig %in% c(110, 111, 120:125) ~ 4L,
    TRUE ~ NA_integer_
  )
  educ_5cat <- case_when(
    educ_orig %in% c(2, 10:14, 20:22, 30:32, 40, 50, 60, 71, 72) ~ 1L,
    educ_orig == 73 ~ 2L,
    educ_orig %in% c(80, 81, 90, 91, 92, 100) ~ 3L,
    educ_orig %in% c(110, 111) ~ 4L,
    educ_orig %in% 120:125 ~ 5L,
    TRUE ~ NA_integer_
  )
  list(educ_4cat = educ_4cat, educ_5cat = educ_5cat)
}

# GSS crosswalk (source: degree)
# 0 = Less than HS, 1 = HS, 2 = Junior college, 3 = Bachelor's, 4 = Graduate
crosswalk_gss <- function(educ_orig) {
  educ_4cat <- case_when(
    educ_orig == 0 ~ 1L,
    educ_orig == 1 ~ 2L,
    educ_orig == 2 ~ 3L,
    educ_orig %in% c(3, 4) ~ 4L,
    TRUE ~ NA_integer_
  )
  educ_5cat <- case_when(
    educ_orig == 0 ~ 1L,
    educ_orig == 1 ~ 2L,
    educ_orig == 2 ~ 3L,
    educ_orig == 3 ~ 4L,
    educ_orig == 4 ~ 5L,
    TRUE ~ NA_integer_
  )
  list(educ_4cat = educ_4cat, educ_5cat = educ_5cat)
}

# MEPS/NHIS crosswalk (source: EDUC)
# IPUMS codes: 0-200 = LT HS, 201-202 = HS, 300-303 = Some college, 400+ = BA+
crosswalk_ipums <- function(educ_orig) {
  educ_4cat <- case_when(
    educ_orig >= 0 & educ_orig <= 200 ~ 1L,
    educ_orig >= 201 & educ_orig <= 202 ~ 2L,
    educ_orig >= 300 & educ_orig <= 303 ~ 3L,
    educ_orig >= 400 & educ_orig <= 530 ~ 4L,
    TRUE ~ NA_integer_
  )
  educ_5cat <- case_when(
    educ_orig >= 0 & educ_orig <= 200 ~ 1L,
    educ_orig >= 201 & educ_orig <= 202 ~ 2L,
    educ_orig >= 300 & educ_orig <= 303 ~ 3L,
    educ_orig == 400 ~ 4L,
    educ_orig >= 500 & educ_orig <= 530 ~ 5L,
    TRUE ~ NA_integer_
  )
  list(educ_4cat = educ_4cat, educ_5cat = educ_5cat)
}

# ------------------------------------------------------------------------------
# Processing functions
# ------------------------------------------------------------------------------

process_brfss <- function(df) {
  # BRFSS already has 4-category educ; original is EDUCA

  # Check if already processed

  if ("educ_orig" %in% names(df)) {
    message("  BRFSS: Already processed (educ_orig exists), skipping rename")
    # Ensure educ_5cat exists
    if (!"educ_5cat" %in% names(df)) {
      df <- df %>% mutate(educ_5cat = NA_integer_)
    }
    return(df)
  }

  stopifnot("educ" %in% names(df), "EDUCA" %in% names(df))

  df %>%
    rename(educ_4cat = educ, educ_orig = EDUCA) %>%
    mutate(educ_5cat = NA_integer_)
}

process_nhanes <- function(df) {
  # NHANES already has 4-category educ; original is DMDEDUC2
  # Check if already processed
  if ("educ_orig" %in% names(df)) {
    message("  NHANES: Already processed (educ_orig exists), skipping rename")
    if (!"educ_5cat" %in% names(df)) {
      df <- df %>% mutate(educ_5cat = NA_integer_)
    }
    return(df)
  }

  stopifnot("educ" %in% names(df), "DMDEDUC2" %in% names(df))

  df %>%
    rename(educ_4cat = educ, educ_orig = DMDEDUC2) %>%
    mutate(educ_5cat = NA_integer_)
}

process_cps <- function(df) {
  # Check if already processed
  if ("educ_orig" %in% names(df)) {
    message("  CPS: Already processed (educ_orig exists), skipping")
    return(df)
  }

  stopifnot("EDUC" %in% names(df))

  df %>%
    rename(educ_orig = EDUC) %>%
    mutate(
      educ_4cat = crosswalk_cps(educ_orig)$educ_4cat,
      educ_5cat = crosswalk_cps(educ_orig)$educ_5cat
    )
}

process_gss <- function(df) {
  # Check if already processed
  if ("educ_orig" %in% names(df)) {
    message("  GSS: Already processed (educ_orig exists), skipping")
    return(df)
  }

  stopifnot("degree" %in% names(df))

  df %>%
    rename(educ_orig = degree) %>%
    mutate(
      educ_4cat = crosswalk_gss(educ_orig)$educ_4cat,
      educ_5cat = crosswalk_gss(educ_orig)$educ_5cat
    )
}

process_meps <- function(df) {
  # Check if already processed
  if ("educ_orig" %in% names(df)) {
    message("  MEPS: Already processed (educ_orig exists), skipping")
    return(df)
  }

  stopifnot("EDUC" %in% names(df))

  df %>%
    rename(educ_orig = EDUC) %>%
    mutate(
      educ_4cat = crosswalk_ipums(educ_orig)$educ_4cat,
      educ_5cat = crosswalk_ipums(educ_orig)$educ_5cat
    )
}

process_nhis <- function(df) {
  # Check if already processed
  if ("educ_orig" %in% names(df)) {
    message("  NHIS: Already processed (educ_orig exists), skipping")
    return(df)
  }

  stopifnot("EDUC" %in% names(df))

  df %>%
    rename(educ_orig = EDUC) %>%
    mutate(
      educ_4cat = crosswalk_ipums(educ_orig)$educ_4cat,
      educ_5cat = crosswalk_ipums(educ_orig)$educ_5cat
    )
}

# ------------------------------------------------------------------------------
# Validation function
# ------------------------------------------------------------------------------

validate_education <- function(df, survey_name) {
  # Check required columns exist
  stopifnot(
    "educ_orig" %in% names(df),
    "educ_4cat" %in% names(df),
    "educ_5cat" %in% names(df)
  )


  # Check value ranges (excluding NA)
  valid_4cat <- all(na.omit(df$educ_4cat) %in% 1:4)
  valid_5cat <- all(na.omit(df$educ_5cat) %in% c(NA_integer_, 1:5))

  if (!valid_4cat) {
    stop(survey_name, ": educ_4cat has values outside 1-4")
  }
  if (!valid_5cat) {
    stop(survey_name, ": educ_5cat has values outside 1-5")
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Main processing
# ------------------------------------------------------------------------------

cat("=== Harmonizing education variables across surveys ===\n\n")

# Define surveys and their processing functions
surveys <- list(
  list(name = "BRFSS",  file = "data_brfss.rds",  process = process_brfss),
  list(name = "CPS",    file = "data_cps.rds",    process = process_cps),
  list(name = "GSS",    file = "data_gss.rds",    process = process_gss),
  list(name = "MEPS",   file = "data_meps.rds",   process = process_meps),
  list(name = "NHANES", file = "data_nhanes.rds", process = process_nhanes),
  list(name = "NHIS",   file = "data_nhis.rds",   process = process_nhis)
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
  validate_education(df, s$name)
  saveRDS(df, file_path)

  # Store results for summary
  year_col <- if ("year" %in% names(df)) "year" else if ("YEAR" %in% names(df)) "YEAR" else NA
  results[[s$name]] <- tibble(
    Survey = s$name,
    N = nrow(df),
    Year_Range = if (!is.na(year_col)) {
      paste(min(df[[year_col]], na.rm = TRUE), "-", max(df[[year_col]], na.rm = TRUE))
    } else NA_character_,
    educ_4cat_pct = round(100 * mean(!is.na(df$educ_4cat)), 1),
    educ_5cat_pct = round(100 * mean(!is.na(df$educ_5cat)), 1)
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

cat("\nEducation variable labels:\n")
cat("  educ_4cat: 1=Less than HS, 2=HS graduate, 3=Some college, 4=BA or higher\n")
cat("  educ_5cat: 1=Less than HS, 2=HS graduate, 3=Some college, 4=BA, 5=Graduate degree\n")
cat("  educ_orig: Original source variable (preserved)\n")

cat("\nNotes:\n")
cat("- BRFSS/NHANES: educ_5cat = NA (original source is 4-category)\
")
cat("- NHIS: Education only available 1997+ (earlier years are NA)\n")

cat("\nDone.\n")
