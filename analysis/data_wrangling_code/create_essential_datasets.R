# Create essential datasets from full survey data files
#
# This script creates smaller "essential" versions of each survey dataset
# containing only the harmonized core variables needed for main analyses.
#
# Run AFTER:
#   1. All individual survey wrangling scripts (wrangle_*.R)
#   2. wrangle_education.R
#   3. wrangle_sex.R
#   4. wrangle_race.R
#
# Output location: data_depot/_derived/srh_project/essential_datasets/
#
# Essential variables included:
#   - Core: age, year, srh, wt, psu, strata, sex
#   - Race/ethnicity: race_5cat, race_5cat_f, hispanic, hispanic_f,
#                     race_includehisp, race_includehisp_f
#   - Education: educ_3cat, educ_4cat
#
# This script is idempotent: safe to re-run.

library(tidyverse)
library(here)
source(here::here("R/paths.R"))
ensure_dirs()

# ------------------------------------------------------------------------------
# Define essential columns
# ------------------------------------------------------------------------------

# Core columns (always required)
core_cols <- c("age", "year", "srh", "wt")

# Survey design columns (may be NA for some surveys)
design_cols <- c("psu", "strata")

# Demographic columns
demo_cols <- c("sex")

# Harmonized race/ethnicity columns
race_cols <- c(
  "race_5cat", "race_5cat_f",
  "hispanic", "hispanic_f",
  "race_includehisp", "race_includehisp_f"
)

# Harmonized education columns
educ_cols <- c("educ_3cat", "educ_4cat")

# All essential columns
essential_cols <- c(core_cols, design_cols, demo_cols, race_cols, educ_cols)

# ------------------------------------------------------------------------------
# Helper function to safely select columns
# ------------------------------------------------------------------------------

safe_select_essential <- function(df, survey_name) {
  # Check which essential columns exist in the data
  available <- intersect(essential_cols, names(df))
  missing <- setdiff(essential_cols, names(df))

  if (length(missing) > 0) {
    warning(survey_name, ": Missing columns will be added as NA: ",
            paste(missing, collapse = ", "))
  }

  # Select available columns
  df_selected <- df %>%
    select(all_of(available))

  # Add missing columns as NA with appropriate type
  for (col in missing) {
    if (col %in% c("race_5cat", "race_includehisp", "sex")) {
      df_selected[[col]] <- NA_character_
    } else if (col %in% c("race_5cat_f", "hispanic_f", "race_includehisp_f")) {
      df_selected[[col]] <- factor(NA)
    } else if (col %in% c("hispanic", "educ_3cat", "educ_4cat")) {
      df_selected[[col]] <- NA_integer_
    } else {
      df_selected[[col]] <- NA_real_
    }
  }

  # Reorder columns
  df_selected <- df_selected %>%
    select(all_of(essential_cols))

  df_selected
}

# ------------------------------------------------------------------------------
# Create output directory
# ------------------------------------------------------------------------------

essential_dir <- depot_path("_derived", "srh_project", "essential_datasets")
if (!dir.exists(essential_dir)) {
  dir.create(essential_dir, recursive = TRUE)
  message("Created directory: ", essential_dir)
}

# ------------------------------------------------------------------------------
# Process each survey
# ------------------------------------------------------------------------------

cat("=== Creating essential datasets ===\n\n")

surveys <- c("brfss", "cps", "gss", "meps", "nhanes", "nhis")
results <- list()

for (survey in surveys) {
  input_file <- derived_path(paste0("data_", survey, ".rds"))
  output_file <- file.path(essential_dir, paste0("data_essential_", survey, ".rds"))

  # Check if input file exists
  if (!file.exists(input_file)) {
    message(toupper(survey), ": Input file not found, skipping")
    next
  }

  message("Processing ", toupper(survey), "...")

  # Load full dataset
  df_full <- readRDS(input_file)
  n_full <- nrow(df_full)
  n_cols_full <- ncol(df_full)

  # Select essential columns
  df_essential <- safe_select_essential(df_full, toupper(survey))

  # Verify core columns are not all NA
  for (col in core_cols) {
    if (all(is.na(df_essential[[col]]))) {
      warning(toupper(survey), ": Core column '", col, "' is all NA!")
    }
  }

  # Save essential dataset
  saveRDS(df_essential, output_file)

  # Store results
  results[[survey]] <- tibble(
    Survey = toupper(survey),
    N = n_full,
    Full_cols = n_cols_full,
    Essential_cols = ncol(df_essential),
    Size_full_MB = round(file.size(input_file) / 1024^2, 1),
    Size_essential_MB = round(file.size(output_file) / 1024^2, 1),
    Reduction_pct = round(100 * (1 - file.size(output_file) / file.size(input_file)), 0)
  )

  message("  Input:  ", n_cols_full, " columns, ",
          round(file.size(input_file) / 1024^2, 1), " MB")
  message("  Output: ", ncol(df_essential), " columns, ",
          round(file.size(output_file) / 1024^2, 1), " MB")
  message("  Saved:  ", output_file, "\n")

  rm(df_full, df_essential)
  gc(verbose = FALSE)
}

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

cat("\n=== Summary ===\n\n")

summary_df <- bind_rows(results)
print(summary_df, n = 10)

cat("\nEssential columns included:\n")
cat("  Core:      ", paste(core_cols, collapse = ", "), "\n")
cat("  Design:    ", paste(design_cols, collapse = ", "), "\n")
cat("  Demographic:", paste(demo_cols, collapse = ", "), "\n")
cat("  Race:      ", paste(race_cols, collapse = ", "), "\n")
cat("  Education: ", paste(educ_cols, collapse = ", "), "\n")

cat("\nOutput location: ", essential_dir, "\n")

cat("\nTo load essential datasets:\n")
cat("  source(here::here('R/paths.R'))\n")
cat("  essential_dir <- depot_path('_derived', 'srh_project', 'essential_datasets')\n")
cat("  data_brfss <- readRDS(file.path(essential_dir, 'data_essential_brfss.rds'))\n")

cat("\nDone.\n")
