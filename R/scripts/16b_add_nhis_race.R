# ==============================================================================
# 16b_add_nhis_race.R
# Add harmonized race variables to NHIS data
#
# Purpose: Create harmonized race variables from existing RACEA/RACENEW/HISPETH
#          columns in data_nhis.rds (these raw columns are already preserved)
#
# Variables added:
#   - race_5cat: 5-category race (White, Black, AIAN, Asian/PI, Other)
#   - hispanic: Hispanic ethnicity (0/1)
#   - race_includehisp: Race with Hispanic as separate category (6 levels)
#   - race_includehisp_f: Factor version for modeling
#
# Usage:
#   Rscript R/scripts/16b_add_nhis_race.R
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("ADD RACE VARIABLES TO NHIS DATA\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# 0. Setup
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

source(here("R", "paths.R"))

# ==============================================================================
# 1. Load existing NHIS data
# ==============================================================================

cat("--- STEP 1: LOADING data_nhis.rds ---\n")

nhis_derived_path <- derived_path("data_nhis.rds")

if (!file.exists(nhis_derived_path)) {
  stop("Derived NHIS file not found: ", nhis_derived_path)
}

data_nhis <- readRDS(nhis_derived_path)
cat("  Rows:", format(nrow(data_nhis), big.mark = ","), "\n")
cat("  Year range:", min(data_nhis$year, na.rm = TRUE), "-",
    max(data_nhis$year, na.rm = TRUE), "\n")

# Check if required source columns exist
required_cols <- c("RACEA", "HISPETH")
missing_cols <- setdiff(required_cols, names(data_nhis))

if (length(missing_cols) > 0) {
  stop("Missing required columns for race harmonization: ",
       paste(missing_cols, collapse = ", "),
       "\nRe-run wrangle_nhis.R to preserve these columns from raw NHIS data.")
}

cat("  Source columns present: RACEA, RACENEW, HISPETH\n")

# ==============================================================================
# 2. Create harmonized race variables
# ==============================================================================

cat("\n--- STEP 2: CREATING HARMONIZED RACE VARIABLES ---\n")

# NHIS race coding (IPUMS RACEA):
#   100 = White
#   200 = Black/African American
#   300 = American Indian/Alaska Native
#   400-434 = Asian/Pacific Islander groups
#   500+ = Other/Multiple races
#
# RACENEW (post-2019 redesign) uses similar structure
#
# HISPETH: 0 = NIU/Unknown, 10-50 = Hispanic origin codes

# Check if race columns already exist
if ("race_includehisp_f" %in% names(data_nhis)) {
  cat("  WARNING: race_includehisp_f already exists in data_nhis.rds\n")
  cat("  Overwriting existing race columns...\n")
  data_nhis <- data_nhis %>%
    select(-any_of(c("race_5cat", "hispanic", "race_includehisp", "race_includehisp_f")))
}

# Check if RACENEW column exists (for post-2019 data)
has_racenew <- "RACENEW" %in% names(data_nhis)
cat("  RACENEW column present:", has_racenew, "\n")

data_nhis <- data_nhis %>%
  mutate(
    # Create 5-category race variable
    # Use RACEA for most years, RACENEW for post-2019 if RACEA missing/0
    race_5cat = case_when(
      # White
      RACEA == 100 ~ 1L,
      has_racenew & is.na(RACEA) & RACENEW == 100 ~ 1L,
      has_racenew & RACEA == 0 & RACENEW == 100 ~ 1L,
      # Black
      RACEA == 200 ~ 2L,
      has_racenew & is.na(RACEA) & RACENEW == 200 ~ 2L,
      has_racenew & RACEA == 0 & RACENEW == 200 ~ 2L,
      # AIAN
      RACEA == 300 ~ 3L,
      has_racenew & is.na(RACEA) & RACENEW == 300 ~ 3L,
      has_racenew & RACEA == 0 & RACENEW == 300 ~ 3L,
      # Asian/Pacific Islander (400-434)
      RACEA >= 400 & RACEA < 500 ~ 4L,
      has_racenew & is.na(RACEA) & RACENEW >= 400 & RACENEW < 500 ~ 4L,
      has_racenew & RACEA == 0 & RACENEW >= 400 & RACENEW < 500 ~ 4L,
      # Multiple/Other (500+)
      RACEA >= 500 ~ 5L,
      has_racenew & is.na(RACEA) & RACENEW >= 500 ~ 5L,
      has_racenew & RACEA == 0 & RACENEW >= 500 ~ 5L,
      TRUE ~ NA_integer_
    ),

    # Hispanic ethnicity (binary)
    # HISPYN: 1 = Not Hispanic, 2 = Hispanic, 7/8/9 = Unknown
    # HISPETH: 10 = Not Hispanic, 20+ = Hispanic origin codes
    hispanic = case_when(
      HISPYN == 1 ~ 0L,                     # Not Hispanic (from HISPYN)
      HISPYN == 2 ~ 1L,                     # Hispanic (from HISPYN)
      # Fallback to HISPETH when HISPYN is NA or unknown
      HISPETH == 10 ~ 0L,                   # Not Hispanic (from HISPETH)
      HISPETH >= 20 & HISPETH < 70 ~ 1L,   # Hispanic (from HISPETH)
      TRUE ~ NA_integer_
    ),

    # Race including Hispanic as separate category
    # 1 = NH-White, 2 = NH-Black, 3 = NH-AIAN, 4 = NH-Asian/PI, 5 = Hispanic, 6 = Other
    race_includehisp = case_when(
      hispanic == 1 ~ 5L,               # Hispanic (any race)
      race_5cat == 1 ~ 1L,              # NH-White
      race_5cat == 2 ~ 2L,              # NH-Black
      race_5cat == 3 ~ 3L,              # NH-AIAN
      race_5cat == 4 ~ 4L,              # NH-Asian/PI
      race_5cat == 5 ~ 6L,              # NH-Other/Multiple
      TRUE ~ NA_integer_
    ),

    # Factor version for modeling
    race_includehisp_f = factor(
      race_includehisp,
      levels = 1:6,
      labels = c("NH-White", "NH-Black", "NH-AIAN", "NH-Asian/PI", "Hispanic", "Other")
    )
  )

# Report race distribution
cat("\n  Race distribution (race_includehisp_f):\n")
race_dist <- data_nhis %>%
  count(race_includehisp_f) %>%
  mutate(pct = round(n / sum(n) * 100, 1))
print(as.data.frame(race_dist), row.names = FALSE)

# ==============================================================================
# 3. Validate and save
# ==============================================================================

cat("\n--- STEP 3: VALIDATION AND SAVE ---\n")

# Check coverage
n_with_race <- sum(!is.na(data_nhis$race_includehisp_f))
pct_with_race <- round(n_with_race / nrow(data_nhis) * 100, 1)

cat("  Records with race data:", format(n_with_race, big.mark = ","),
    "(", pct_with_race, "%)\n")

# Validate race variable ranges
stopifnot("race_5cat values in 1-5", all(na.omit(data_nhis$race_5cat) %in% 1:5))
stopifnot("hispanic values in 0-1", all(na.omit(data_nhis$hispanic) %in% 0:1))
stopifnot("race_includehisp values in 1-6", all(na.omit(data_nhis$race_includehisp) %in% 1:6))

# Check that race_includehisp_f has correct levels
expected_levels <- c("NH-White", "NH-Black", "NH-AIAN", "NH-Asian/PI", "Hispanic", "Other")
actual_levels <- levels(data_nhis$race_includehisp_f)
stopifnot("race_includehisp_f has correct levels", identical(actual_levels, expected_levels))

cat("  Validation passed\n")

# Save
saveRDS(data_nhis, nhis_derived_path)
cat("\n  Saved:", nhis_derived_path, "\n")

# ==============================================================================
# 4. Summary
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Variables added to data_nhis.rds:\n")
cat("  - race_5cat:         5-category race (1=White, 2=Black, 3=AIAN, 4=Asian/PI, 5=Other)\n")
cat("  - hispanic:          Hispanic ethnicity (0=Not Hispanic, 1=Hispanic)\n")
cat("  - race_includehisp:  Race with Hispanic (1-6)\n")
cat("  - race_includehisp_f: Factor version for modeling\n")
cat("\nFactor levels for race_includehisp_f:\n")
cat("  1 = NH-White\n")
cat("  2 = NH-Black\n")
cat("  3 = NH-AIAN\n")
cat("  4 = NH-Asian/PI\n")
cat("  5 = Hispanic\n")
cat("  6 = Other\n")

cat("\n", strrep("=", 70), "\n")
cat("COMPLETE\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
