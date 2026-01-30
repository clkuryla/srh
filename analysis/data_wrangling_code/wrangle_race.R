# Harmonize race/ethnicity variables across all surveys
#
# Run AFTER all individual survey wrangling scripts have been run.
# This script adds harmonized race/ethnicity variables to each survey data file:
#   - race_5cat: 5-category race (White, Black, AIAN, Asian, Other)
#   - race_5cat_f: Labeled factor version
#   - hispanic: Hispanic ethnicity (1 = Hispanic, 0 = Not Hispanic, NA = Unknown)
#   - hispanic_f: Labeled factor version
#   - race_includehisp: Race with Hispanic as separate category
#   - race_includehisp_f: Labeled factor version
#
# Race coding notes:
#   - race_5cat captures non-Hispanic race when possible
#   - When Hispanic status is unknown, race is preserved (not set to "Other")
#   - race_includehisp treats Hispanic as a mutually exclusive category
#     (Hispanic individuals are coded as "Hispanic" regardless of race)
#
# Survey-specific source variables:
#   BRFSS:  _IMPRACE (2013+), _RACE (2001-2012), ORACE (1993-2000), HISPANIC/HISPANC2/HISPANC3
#   CPS:    RACE, HISPAN (IPUMS)
#   GSS:    race, hispanic (gssr package)
#   MEPS:   race, hispanic (already character in wrangled data)
#   NHIS:   RACENEW, HISPETH (IPUMS) - to be added if available
#   NHANES: (uses Kamaryn's preprocessed data - race may already be present)
#
# This script is idempotent: safe to re-run without corrupting data.

library(tidyverse)
library(here)
source(here::here("R/paths.R"))

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

# Convert numeric to character helper
to_int <- function(x) suppressWarnings(as.integer(as.character(x)))

# Create factor versions of race variables
add_race_factors <- function(df) {
  df %>%
    mutate(
      race_5cat_f = factor(
        race_5cat,
        levels = c("White", "Black", "AIAN", "Asian", "Other"),
        labels = c("NH White", "NH Black", "NH AIAN", "NH Asian", "Other/Multi")
      ),
      hispanic_f = factor(
        hispanic,
        levels = c(0, 1),
        labels = c("Not Hispanic", "Hispanic")
      ),
      race_includehisp_f = factor(
        race_includehisp,
        levels = c("White", "Black", "AIAN", "Asian", "Hispanic", "Other"),
        labels = c("NH White", "NH Black", "NH AIAN", "NH Asian", "Hispanic", "Other/Multi")
      )
    )
}

# Create race_includehisp from race_5cat and hispanic
derive_race_includehisp <- function(df) {
  df %>%
    mutate(
      race_includehisp = case_when(
        hispanic == 1 ~ "Hispanic",
        !is.na(race_5cat) ~ race_5cat,
        TRUE ~ NA_character_
      )
    )
}

# ------------------------------------------------------------------------------
# Processing functions for each survey
# ------------------------------------------------------------------------------

process_brfss_race <- function(df) {
  # BRFSS race/ethnicity variables:
  # _IMPRACE (2013+): Imputed race/ethnicity (includes Hispanic)
  #   1=White NH, 2=Black NH, 3=Asian NH, 4=AIAN NH, 5=Hispanic, 6=Other NH
  # _RACE (2001-2012): Similar to _IMPRACE
  # ORACE (1993-2000): 1=White, 2=Black, 3=Asian/PI, 4=AIAN, 5=Other
  # HISPANIC/HISPANC2: 1=Yes, 2=No, 7/9=NA
  # HISPANC3: 1-4=specific Hispanic, 5=No

  # Check if already processed
  if ("race_5cat" %in% names(df)) {
    message("  BRFSS: Already processed (race_5cat exists), skipping")
    return(df)
  }

  # Check which race variables are available
  has_imprace <- "_IMPRACE" %in% names(df)
  has_race <- "_RACE" %in% names(df)
  has_orace <- "ORACE" %in% names(df)
  has_hisp <- any(c("HISPANIC", "HISPANC2", "HISPANC3") %in% names(df))

  if (!has_imprace && !has_race && !has_orace) {
    warning("BRFSS: No race variables found (_IMPRACE, _RACE, ORACE). Setting race to NA.")
    df <- df %>%
      mutate(
        race_5cat = NA_character_,
        hispanic = NA_integer_
      )
    df <- derive_race_includehisp(df)
    df <- add_race_factors(df)
    return(df)
  }

  # Unify Hispanic status from available variables
  df <- df %>%
    mutate(
      # Determine Hispanic status
      hispanic = case_when(
        # HISPANC3: 1-4 = specific Hispanic origins, 5 = No
        "HISPANC3" %in% names(.) & !is.na(HISPANC3) & to_int(HISPANC3) %in% 1:4 ~ 1L,
        "HISPANC3" %in% names(.) & !is.na(HISPANC3) & to_int(HISPANC3) == 5 ~ 0L,
        # HISPANC2: 1=Yes, 2=No
        "HISPANC2" %in% names(.) & !is.na(HISPANC2) & to_int(HISPANC2) == 1 ~ 1L,
        "HISPANC2" %in% names(.) & !is.na(HISPANC2) & to_int(HISPANC2) == 2 ~ 0L,
        # HISPANIC: 1=Yes, 2=No
        "HISPANIC" %in% names(.) & !is.na(HISPANIC) & to_int(HISPANIC) == 1 ~ 1L,
        "HISPANIC" %in% names(.) & !is.na(HISPANIC) & to_int(HISPANIC) == 2 ~ 0L,
        TRUE ~ NA_integer_
      )
    )

  # Determine race from _IMPRACE, _RACE, or ORACE
  df <- df %>%
    mutate(
      race_5cat = case_when(
        # _IMPRACE (2013+): 1=White NH, 2=Black NH, 3=Asian NH, 4=AIAN NH, 5=Hispanic, 6=Other NH
        has_imprace & !is.na(`_IMPRACE`) & to_int(`_IMPRACE`) == 1 ~ "White",
        has_imprace & !is.na(`_IMPRACE`) & to_int(`_IMPRACE`) == 2 ~ "Black",
        has_imprace & !is.na(`_IMPRACE`) & to_int(`_IMPRACE`) == 3 ~ "Asian",
        has_imprace & !is.na(`_IMPRACE`) & to_int(`_IMPRACE`) == 4 ~ "AIAN",
        has_imprace & !is.na(`_IMPRACE`) & to_int(`_IMPRACE`) == 5 ~ NA_character_, # Hispanic - race from other source
        has_imprace & !is.na(`_IMPRACE`) & to_int(`_IMPRACE`) == 6 ~ "Other",

        # _RACE (2001-2012): Same coding as _IMPRACE
        has_race & !is.na(`_RACE`) & to_int(`_RACE`) == 1 ~ "White",
        has_race & !is.na(`_RACE`) & to_int(`_RACE`) == 2 ~ "Black",
        has_race & !is.na(`_RACE`) & to_int(`_RACE`) == 3 ~ "Asian",
        has_race & !is.na(`_RACE`) & to_int(`_RACE`) == 4 ~ "AIAN",
        has_race & !is.na(`_RACE`) & to_int(`_RACE`) == 5 ~ NA_character_, # Hispanic
        has_race & !is.na(`_RACE`) & to_int(`_RACE`) == 6 ~ "Other",

        # ORACE (1993-2000): 1=White, 2=Black, 3=Asian/PI, 4=AIAN, 5=Other
        has_orace & !is.na(ORACE) & to_int(ORACE) == 1 ~ "White",
        has_orace & !is.na(ORACE) & to_int(ORACE) == 2 ~ "Black",
        has_orace & !is.na(ORACE) & to_int(ORACE) == 3 ~ "Asian",
        has_orace & !is.na(ORACE) & to_int(ORACE) == 4 ~ "AIAN",
        has_orace & !is.na(ORACE) & to_int(ORACE) == 5 ~ "Other",

        TRUE ~ NA_character_
      )
    )

  # For IMPRACE/RACE code 5 (Hispanic), we need to determine underlying race
  # Since BRFSS doesn't provide this, we leave race_5cat as NA for Hispanics
  # This is handled appropriately in race_includehisp

  df <- derive_race_includehisp(df)
  df <- add_race_factors(df)

  df
}

process_cps_race <- function(df) {
  # CPS race variables (IPUMS):
  # RACE: 100=White, 200=Black, 300=AIAN, 651=Asian, 652=Hawaiian/PI,
  #       700+=Multi, 801-830=Other
  # HISPAN: 0=Not Hispanic, 100-412=specific Hispanic origins

  # Check if already processed
  if ("race_5cat" %in% names(df)) {
    message("  CPS: Already processed (race_5cat exists), skipping")
    return(df)
  }

  has_race <- "RACE" %in% names(df)
  has_hispan <- "HISPAN" %in% names(df)

  if (!has_race) {
    warning("CPS: RACE variable not found. Setting race to NA.")
    df <- df %>%
      mutate(
        race_5cat = NA_character_,
        hispanic = NA_integer_
      )
    df <- derive_race_includehisp(df)
    df <- add_race_factors(df)
    return(df)
  }

  df <- df %>%
    mutate(
      hispanic = case_when(
        has_hispan & HISPAN == 0 ~ 0L,
        has_hispan & HISPAN >= 100 & HISPAN <= 412 ~ 1L,
        TRUE ~ NA_integer_
      ),
      race_5cat = case_when(
        RACE == 100 ~ "White",
        RACE == 200 ~ "Black",
        RACE == 300 ~ "AIAN",
        RACE %in% c(651, 652) ~ "Asian",  # Asian + Hawaiian/PI
        RACE >= 700 ~ "Other",  # Multi-race and other
        TRUE ~ NA_character_
      )
    )

  df <- derive_race_includehisp(df)
  df <- add_race_factors(df)

  df
}

process_gss_race <- function(df) {
  # GSS race/ethnicity (gssr package):
  # race: numeric (1=White, 2=Black, 3=Other)
  # hispanic: numeric (1=Not Hispanic, 2-50=specific Hispanic origins)

  # Check if already processed
  if ("race_5cat" %in% names(df)) {
    message("  GSS: Already processed (race_5cat exists), skipping")
    return(df)
  }

  has_race <- "race" %in% names(df)
  has_hisp <- "hispanic" %in% names(df)

  if (!has_race) {
    warning("GSS: race variable not found. Setting race_5cat to NA.")
    df <- df %>%
      mutate(
        race_5cat = NA_character_,
        hispanic = NA_integer_
      )
    df <- derive_race_includehisp(df)
    df <- add_race_factors(df)
    return(df)
  }

  # Store original race variable
  df <- df %>%
    rename(race_orig = race)

  # Process hispanic first (since 'hispanic' column exists but needs recoding)
  if (has_hisp) {
    df <- df %>%
      rename(hispanic_orig = hispanic) %>%
      mutate(
        hispanic = case_when(
          hispanic_orig == 1 ~ 0L,  # 1 = Not Hispanic
          hispanic_orig >= 2 & hispanic_orig <= 50 ~ 1L,  # 2-50 = Hispanic origins
          TRUE ~ NA_integer_
        )
      )
  } else {
    df <- df %>%
      mutate(hispanic = NA_integer_)
  }

  df <- df %>%
    mutate(
      race_5cat = case_when(
        race_orig == 1 ~ "White",
        race_orig == 2 ~ "Black",
        race_orig == 3 ~ "Other",  # GSS only has 3 categories
        TRUE ~ NA_character_
      )
    )

  df <- derive_race_includehisp(df)
  df <- add_race_factors(df)

  df
}

process_meps_race <- function(df) {
  # MEPS: race and hispanic are already character variables
  # race: "White", "Black", "AIAN", "AAPI", "Other"
  # hispanic: "Hispanic", "Not Hispanic"

  # Check if already processed
  if ("race_5cat" %in% names(df)) {
    message("  MEPS: Already processed (race_5cat exists), skipping")
    return(df)
  }

  has_race <- "race" %in% names(df)
  has_hisp <- "hispanic" %in% names(df)

  if (!has_race) {
    warning("MEPS: race variable not found. Setting race_5cat to NA.")
    df <- df %>%
      mutate(
        race_5cat = NA_character_,
        hispanic = NA_integer_
      )
    df <- derive_race_includehisp(df)
    df <- add_race_factors(df)
    return(df)
  }

  # Store original and harmonize
  df <- df %>%
    rename(race_orig = race)

  if (has_hisp) {
    df <- df %>%
      rename(hispanic_orig = hispanic) %>%
      mutate(
        hispanic = case_when(
          hispanic_orig == "Hispanic" ~ 1L,
          hispanic_orig == "Not Hispanic" ~ 0L,
          TRUE ~ NA_integer_
        )
      )
  } else {
    df <- df %>%
      mutate(hispanic = NA_integer_)
  }

  df <- df %>%
    mutate(
      # Map MEPS categories to harmonized 5-category
      race_5cat = case_when(
        race_orig == "White" ~ "White",
        race_orig == "Black" ~ "Black",
        race_orig == "AIAN" ~ "AIAN",
        race_orig == "AAPI" ~ "Asian",  # MEPS combines Asian + Pacific Islander
        race_orig == "Other" ~ "Other",
        TRUE ~ NA_character_
      )
    )

  df <- derive_race_includehisp(df)
  df <- add_race_factors(df)

  df
}

process_nhis_race <- function(df) {
  # NHIS (IPUMS) race/ethnicity variables:
  # RACENEW: 100=White, 200=Black, 300=AIAN, 400=Asian, 500-650=PI/Multi/Other
  # HISPETH: 10=Not Hispanic, 20-70=specific Hispanic origins
  # Note: Variables may vary depending on IPUMS extract

  # Check if already processed
  if ("race_5cat" %in% names(df)) {
    message("  NHIS: Already processed (race_5cat exists), skipping")
    return(df)
  }

  has_racenew <- "RACENEW" %in% names(df)
  has_hispeth <- "HISPETH" %in% names(df)

  if (!has_racenew) {
    warning("NHIS: RACENEW variable not found. Setting race_5cat to NA.")
    df <- df %>%
      mutate(
        race_5cat = NA_character_,
        hispanic = NA_integer_
      )
    df <- derive_race_includehisp(df)
    df <- add_race_factors(df)
    return(df)
  }

  df <- df %>%
    mutate(
      hispanic = case_when(
        has_hispeth & HISPETH == 10 ~ 0L,  # Not Hispanic
        has_hispeth & HISPETH >= 20 & HISPETH <= 70 ~ 1L,  # Hispanic
        TRUE ~ NA_integer_
      ),
      race_5cat = case_when(
        RACENEW == 100 ~ "White",
        RACENEW == 200 ~ "Black",
        RACENEW == 300 ~ "AIAN",
        RACENEW == 400 ~ "Asian",
        RACENEW >= 500 ~ "Other",  # PI, Multiple, Other
        TRUE ~ NA_character_
      )
    )

  df <- derive_race_includehisp(df)
  df <- add_race_factors(df)

  df
}

process_nhanes_race <- function(df) {
  # NHANES (Kamaryn's preprocessed data)
  # Check what race/ethnicity variables are available

  # Check if already processed
  if ("race_5cat" %in% names(df)) {
    message("  NHANES: Already processed (race_5cat exists), skipping")
    return(df)
  }

  # Look for common NHANES race variables
  # RIDRETH1/RIDRETH3: Race/ethnicity (includes Hispanic)
  #   1=Mexican American, 2=Other Hispanic, 3=NH White, 4=NH Black, 5=Other
  # Or race_eth from Kamaryn's preprocessing

  has_ridreth1 <- "RIDRETH1" %in% names(df)
  has_ridreth3 <- "RIDRETH3" %in% names(df)
  has_race_eth <- "race_eth" %in% names(df)

  if (has_race_eth) {
    # Kamaryn's preprocessed variable
    df <- df %>%
      mutate(
        hispanic = case_when(
          race_eth %in% c(1, 2) ~ 1L,  # Mexican American, Other Hispanic
          race_eth %in% c(3, 4, 5, 6, 7) ~ 0L,  # NH categories
          TRUE ~ NA_integer_
        ),
        race_5cat = case_when(
          race_eth == 3 ~ "White",   # NH White
          race_eth == 4 ~ "Black",   # NH Black
          race_eth == 6 ~ "Asian",   # NH Asian (if available)
          race_eth %in% c(5, 7) ~ "Other",  # Other
          race_eth %in% c(1, 2) ~ NA_character_,  # Hispanic - underlying race unknown
          TRUE ~ NA_character_
        )
      )
  } else if (has_ridreth3 || has_ridreth1) {
    # Use RIDRETH3 if available, otherwise RIDRETH1
    race_var <- if (has_ridreth3) "RIDRETH3" else "RIDRETH1"

    df <- df %>%
      mutate(
        hispanic = case_when(
          .data[[race_var]] %in% c(1, 2) ~ 1L,  # Mexican American, Other Hispanic
          .data[[race_var]] %in% c(3, 4, 5, 6, 7) ~ 0L,  # NH categories
          TRUE ~ NA_integer_
        ),
        race_5cat = case_when(
          .data[[race_var]] == 3 ~ "White",   # NH White
          .data[[race_var]] == 4 ~ "Black",   # NH Black
          .data[[race_var]] == 6 ~ "Asian",   # NH Asian (RIDRETH3 only)
          .data[[race_var]] %in% c(5, 7) ~ "Other",
          .data[[race_var]] %in% c(1, 2) ~ NA_character_,  # Hispanic
          TRUE ~ NA_character_
        )
      )
  } else {
    warning("NHANES: No recognized race/ethnicity variable found. Setting race to NA.")
    df <- df %>%
      mutate(
        race_5cat = NA_character_,
        hispanic = NA_integer_
      )
  }

  df <- derive_race_includehisp(df)
  df <- add_race_factors(df)

  df
}

# ------------------------------------------------------------------------------
# Validation function
# ------------------------------------------------------------------------------

validate_race <- function(df, survey_name) {
  # Check required columns exist
  required_cols <- c("race_5cat", "race_5cat_f", "hispanic", "hispanic_f",
                     "race_includehisp", "race_includehisp_f")

  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(survey_name, ": Missing columns: ", paste(missing, collapse = ", "))
  }

  # Check race_5cat values
  valid_race <- c("White", "Black", "AIAN", "Asian", "Other", NA_character_)
  invalid_race <- unique(df$race_5cat[!df$race_5cat %in% valid_race])
  if (length(invalid_race) > 0) {
    stop(survey_name, ": Invalid race_5cat values: ", paste(invalid_race, collapse = ", "))
  }

  # Check hispanic values
  valid_hisp <- c(0L, 1L, NA_integer_)
  invalid_hisp <- unique(df$hispanic[!df$hispanic %in% valid_hisp])
  if (length(invalid_hisp) > 0) {
    stop(survey_name, ": Invalid hispanic values: ", paste(invalid_hisp, collapse = ", "))
  }

  # Check race_includehisp values
  valid_race_incl <- c("White", "Black", "AIAN", "Asian", "Hispanic", "Other", NA_character_)
  invalid_race_incl <- unique(df$race_includehisp[!df$race_includehisp %in% valid_race_incl])
  if (length(invalid_race_incl) > 0) {
    stop(survey_name, ": Invalid race_includehisp values: ", paste(invalid_race_incl, collapse = ", "))
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Main processing
# ------------------------------------------------------------------------------

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
  results[[s$name]] <- tibble(
    Survey = s$name,
    N = nrow(df),
    race_5cat_pct = round(100 * mean(!is.na(df$race_5cat)), 1),
    hispanic_pct = round(100 * mean(!is.na(df$hispanic)), 1),
    race_incl_pct = round(100 * mean(!is.na(df$race_includehisp)), 1),
    pct_white = round(100 * mean(df$race_5cat == "White", na.rm = TRUE), 1),
    pct_hisp = round(100 * mean(df$hispanic == 1, na.rm = TRUE), 1)
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

cat("\nRace/ethnicity variable labels:\n")
cat("  race_5cat: 5-category race (White, Black, AIAN, Asian, Other)\n")
cat("  race_5cat_f: Factor version with 'NH' prefix labels\n")
cat("  hispanic: Hispanic ethnicity (0=Not Hispanic, 1=Hispanic)\n")
cat("  hispanic_f: Factor version\n")
cat("  race_includehisp: Race with Hispanic as separate category\n")
cat("  race_includehisp_f: Factor version\n")

cat("\nNotes:\n")
cat("- For Hispanic individuals, race_5cat may be NA (underlying race unknown)\n")
cat("- race_includehisp treats Hispanic as mutually exclusive with race\n")
cat("- GSS only has 3 race categories (White, Black, Other)\n")

cat("\nDone.\n")
