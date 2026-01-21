# Wrangle NHANES new cycles (2021-2023) using nhanesA package
#
# This script downloads NHANES data directly from CDC using the nhanesA package
# and wrangles it to match the format of existing NHANES data.
#
# Existing data: cycles 1-10 (1999-2018), from Kamaryn's preprocessed file
# New data: cycle 11 (2021-2023), downloaded via nhanesA
#
# Note: The Pre-pandemic cycle (2017-March 2020) overlaps with existing cycle 10
# and has survey design complications, so we skip it for now.
#
# NHANES has a 5-point SRH scale:
#   Original (HUQ010): Excellent, Very Good, Good, Fair, Poor
#   Recoded (srh):     5=Excellent, 4=Very Good, 3=Good, 2=Fair, 1=Poor (higher = better)

library(tidyverse)
library(nhanesA)
library(here)
source(here::here("R/paths.R"))
ensure_dirs()

# ------------------------------------------------------------------------------
# Load existing NHANES data
# ------------------------------------------------------------------------------

data_nhanes_existing <- read_rds(derived_path("data_nhanes.rds"))

cat("Existing NHANES data:\n")
cat("  Rows:", nrow(data_nhanes_existing), "\n")
cat("  Year range:", range(data_nhanes_existing$year, na.rm = TRUE), "\n")
cat("  Year counts:\n")
print(table(data_nhanes_existing$year))

# ------------------------------------------------------------------------------
# Download new NHANES cycle (2021-2023) via nhanesA
# ------------------------------------------------------------------------------

cat("\nDownloading NHANES 2021-2023 data from CDC...\n")

# Demographics (DEMO_L)
demo_l <- nhanes("DEMO_L")
cat("  Downloaded DEMO_L:", nrow(demo_l), "rows\n")

# Health status questionnaire (HUQ_L) - contains SRH
huq_l <- nhanes("HUQ_L")
cat("  Downloaded HUQ_L:", nrow(huq_l), "rows\n")

# Merge on SEQN
data_nhanes_new_raw <- demo_l %>%
  inner_join(huq_l, by = "SEQN")

cat("  After merge:", nrow(data_nhanes_new_raw), "rows\n")

# ------------------------------------------------------------------------------
# Wrangle new NHANES data to match existing format
# ------------------------------------------------------------------------------

# Recode SRH from text to numeric (higher = better)
data_nhanes_new <- data_nhanes_new_raw %>%
  # Filter to adults with valid SRH
  filter(RIDAGEYR >= 18) %>%
  filter(!is.na(HUQ010)) %>%
  filter(HUQ010 %in% c("Excellent,", "Very good,", "Good,", "Fair, or", "Poor?")) %>%
  # Recode SRH so higher = better (1=Poor ... 5=Excellent)
  mutate(
    srh = case_when(
      HUQ010 == "Poor?" ~ 1L,
      HUQ010 == "Fair, or" ~ 2L,
      HUQ010 == "Good," ~ 3L,
      HUQ010 == "Very good," ~ 4L,
      HUQ010 == "Excellent," ~ 5L,
      TRUE ~ NA_integer_
    ),
    srh_cat = factor(
      srh,
      levels = 1:5,
      labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")
    )
  ) %>%
  # Standardize variable names to match existing data
  mutate(
    age = as.numeric(RIDAGEYR),
    # 2021-2023 cycle midpoint = 2022
    year = 2022,
    cohort = year - age,
    # Sex recode (1=Male, 2=Female in RIAGENDR)
    sex = case_when(
      RIAGENDR == "Male" ~ "Male",
      RIAGENDR == "Female" ~ "Female",
      RIAGENDR == 1 ~ "Male",
      RIAGENDR == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    # Weights: use 2-year MEC exam weight
    wt = as.numeric(WTMEC2YR),
    # Survey design variables
    strata = as.numeric(SDMVSTRA),
    psu = as.numeric(SDMVPSU),
    # Keep SEQN for potential linkage
    SEQN = as.numeric(SEQN)
  ) %>%
  # Create age groups consistent with existing data
  mutate(
    age_group = factor(
      cut(
        age,
        breaks = c(17, 29, 39, 49, 59, 69, 79, Inf),
        labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
        right = TRUE
      ),
      ordered = TRUE
    ),
    age_group_6 = factor(
      cut(
        age,
        breaks = c(17, 29, 39, 49, 59, 69, Inf),
        labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
        right = TRUE
      ),
      ordered = TRUE
    )
  )

cat("\nAfter filtering to adults with valid SRH:", nrow(data_nhanes_new), "rows\n")
cat("SRH distribution:\n")
print(table(data_nhanes_new$srh_cat))

# ------------------------------------------------------------------------------
# Select columns to match existing data structure (core columns only)
# ------------------------------------------------------------------------------

# Get common columns between existing and new data
existing_cols <- names(data_nhanes_existing)
new_cols <- names(data_nhanes_new)
common_cols <- intersect(existing_cols, new_cols)

cat("\nCommon columns between existing and new data:\n")
print(common_cols)

# Core columns needed for SRH convergence analysis
core_cols <- c("SEQN", "age", "sex", "year", "cohort",
               "srh", "srh_cat", "wt", "strata", "psu",
               "age_group", "age_group_6")

# Select core columns from new data
data_nhanes_new_core <- data_nhanes_new %>%
  select(all_of(core_cols))

# ------------------------------------------------------------------------------
# Combine existing and new data
# ------------------------------------------------------------------------------

# Select same core columns from existing data
data_nhanes_existing_core <- data_nhanes_existing %>%
  select(any_of(core_cols))

# Check which core columns are missing from existing
missing_cols <- setdiff(core_cols, names(data_nhanes_existing_core))
if (length(missing_cols) > 0) {
  cat("\nNote: Following core columns missing from existing data:",
      paste(missing_cols, collapse = ", "), "\n")
}

# Combine
data_nhanes_combined <- bind_rows(
  data_nhanes_existing_core,
  data_nhanes_new_core
)

cat("\n\nCombined NHANES data:\n")
cat("  Rows:", nrow(data_nhanes_combined), "\n")
cat("  Year range:", range(data_nhanes_combined$year, na.rm = TRUE), "\n")
cat("  Year counts:\n")
print(table(data_nhanes_combined$year))

# ------------------------------------------------------------------------------
# Verify data integrity
# ------------------------------------------------------------------------------

# Check SRH coding
stopifnot("SRH should be 1-5" = all(data_nhanes_combined$srh %in% 1:5, na.rm = TRUE))

# Check year range
stopifnot("Year should be >= 1999" = all(data_nhanes_combined$year >= 1999, na.rm = TRUE))
stopifnot("Year should be <= 2025" = all(data_nhanes_combined$year <= 2025, na.rm = TRUE))

cat("\nData integrity checks passed.\n")

# ------------------------------------------------------------------------------
# Save combined data
# ------------------------------------------------------------------------------

# Backup existing file
backup_path <- derived_path("data_nhanes_backup_20260117.rds")
if (!file.exists(backup_path)) {
  file.copy(derived_path("data_nhanes.rds"), backup_path)
  cat("\nBacked up existing data to:", backup_path, "\n")
}

# Save combined data
write_rds(data_nhanes_combined, derived_path("data_nhanes.rds"))
cat("Saved combined NHANES data to:", derived_path("data_nhanes.rds"), "\n")

cat("\n=== NHANES update complete ===\n")
