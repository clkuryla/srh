# Wrangle MEPS Round-Level Data to Person-Year Format
# Input: meps_00004_round.csv (round-level, includes PROXY variable)
# Output: data_meps_round_personyear.rds (person-year level)
#
# Purpose: Harmonize person-level (P) and round-level (R) limitation variables
# by aggregating round-level data to person-year using "any positive" logic.

library(tidyverse)
library(here)
source(here::here("R/paths.R"))
ensure_dirs()

# ==============================================================================
# 1. LOAD AND CLEAN DATA
# ==============================================================================

message("Loading round-level MEPS data...")
data_raw <- read_csv(
  depot_path("surveys", "MEPS", "ipums_extracts", "meps_00004_round.csv"),
  show_col_types = FALSE
)

message(sprintf("Loaded %s rows, %s columns",
                format(nrow(data_raw), big.mark = ","),
                ncol(data_raw)))

# ------------------------------------------------------------------------------
# FILTER TO SELF-RESPONDENTS ONLY (PROXY == 1)
# ------------------------------------------------------------------------------
# PROXY variable coding:
#   1 = Respondent is RU member (self-report) <- KEEP
#   2 = Respondent is a proxy <- EXCLUDE
#
# Rationale: Proxy responses introduce measurement error, especially for
# subjective measures like mental health. Proxies (often family members)
# systematically overreport mental health problems for elderly respondents.

n_before_proxy <- nrow(data_raw)

# Check PROXY variable distribution before filtering
message("\n--- PROXY variable distribution (before filter) ---")
proxy_dist <- data_raw %>%
  count(PROXY) %>%
  mutate(pct = round(100 * n / sum(n), 1))
print(proxy_dist)

# Filter to self-respondents only
data_raw <- data_raw %>%
  filter(PROXY == 1)

n_after_proxy <- nrow(data_raw)
n_excluded <- n_before_proxy - n_after_proxy

message(sprintf("\nProxy filter: %s rows -> %s rows",
                format(n_before_proxy, big.mark = ","),
                format(n_after_proxy, big.mark = ",")))
message(sprintf("Excluded %s proxy responses (%.1f%%)",
                format(n_excluded, big.mark = ","),
                100 * n_excluded / n_before_proxy))

# Define missing codes (IPUMS convention)
missing_codes <- c(-1, -7, -8, -9)

# Variables to clean (health/limitation variables that use these codes)
vars_to_clean <- c(
  # Person-level limitation vars
  "LADL", "LAIADL", "USEAID", "LMTPHYS", "LMTLIFT", "LMTWALK",
  "LMTBEND", "LMTWORK", "LMTSOC", "LMTCOG", "ANYLMT",
  # Round-level limitation vars
  "LADLRD", "LAIADLRD", "USEAIDRD", "LMTPHYSRD", "LMTLIFTRD", "LMTWALKRD",
  "LMTBENDRD", "LMTWORKRD", "LMTSOCRD", "LMTCOGRD",
  # Round-level health/condition vars
  "MHLTHRD", "DIABETICRD", "CHRBRONCRD", "JOINTPAINRD", "ASTHMARD"
)

# Convert missing codes to NA
data_clean <- data_raw %>%
  mutate(across(any_of(vars_to_clean), ~ na_if(.x, -1))) %>%
  mutate(across(any_of(vars_to_clean), ~ na_if(.x, -7))) %>%
  mutate(across(any_of(vars_to_clean), ~ na_if(.x, -8))) %>%
  mutate(across(any_of(vars_to_clean), ~ na_if(.x, -9)))

# ==============================================================================
# 2. DEFINE VARIABLE GROUPS
# ==============================================================================

# Person-level variables (constant within person-year, take first non-NA)
person_vars <- c(
  "MEPSID", "YEAR", "PANEL", "PSUANN", "STRATANN", "PSUPLD", "STRATAPLD",
  "PERWEIGHT", "SAQWEIGHT", "DIABWEIGHT", "SDOHWT",
  "AGE", "SEX", "MARSTAT", "REGIONMEPS", "RACEA", "HISPETH", "HISPYN",
  "EDUC", "EDUCYR", "STUDENT", "HIDEG", "HEALTH", "ANYLMT",
  "LADL", "LAIADL", "USEAID", "LMTPHYS", "LMTLIFT", "LMTWALK",
  "LMTBEND", "LMTWORK", "LMTSOC", "LMTCOG", "DIFWLKCLM", "ADDAYA"
)

# Round-level limitation variables to aggregate
round_lmt_vars <- c(
  "LADLRD", "LAIADLRD", "USEAIDRD", "LMTPHYSRD", "LMTLIFTRD", "LMTWALKRD",
  "LMTBENDRD", "LMTWORKRD", "LMTSOCRD", "LMTCOGRD"
)

# Round-level condition variables
round_cond_vars <- c("MHLTHRD", "DIABETICRD", "CHRBRONCRD", "JOINTPAINRD", "ASTHMARD")

# ==============================================================================
# 3. HELPER FUNCTIONS FOR AGGREGATION
# ==============================================================================

# IPUMS MEPS coding for limitation variables:
# Binary vars (LADL, LAIADL, USEAID, LMTPHYS, LMTWORK, LMTSOC, LMTCOG):
#   0 = NIU, 1 = No, 2 = Yes, 7/8/9 = Unknown
# Severity vars (LMTLIFT, LMTWALK, LMTBEND):
#   0 = NIU, 1 = Not at all, 2 = A little, 3 = Somewhat, 4 = Very, 7/8/9 = Unknown

# "Any has limitation" for binary vars: 1 if any round = 2 (Yes), else 0
any_has_limitation_binary <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  if (any(x == 2, na.rm = TRUE)) return(1)  # 2 = Yes
  if (any(x == 1, na.rm = TRUE)) return(0)  # 1 = No (if no Yes found)
  return(NA_real_)  # All NIU or unknown
}

# "Any has limitation" for severity vars: 1 if any round >= 2, else 0
any_has_limitation_severity <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  if (any(x >= 2 & x <= 4, na.rm = TRUE)) return(1)  # 2-4 = some limitation
  if (any(x == 1, na.rm = TRUE)) return(0)  # 1 = No limitation
  return(NA_real_)  # All NIU or unknown
}

# First non-NA value
first_nonmissing <- function(x) {
  if (all(is.na(x))) return(NA)
  x[!is.na(x)][1]
}

# ==============================================================================
# 4. EXTRACT PERSON-LEVEL DATA
# ==============================================================================

message("Extracting person-level data...")

# Check for inconsistent person-level values within person-year
check_consistency <- data_clean %>%
  group_by(MEPSID, YEAR) %>%
  summarize(
    n_rounds = n(),
    n_unique_age = n_distinct(AGE, na.rm = TRUE),
    n_unique_sex = n_distinct(SEX, na.rm = TRUE),
    n_unique_health = n_distinct(HEALTH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_unique_age > 1 | n_unique_sex > 1 | n_unique_health > 1)

if (nrow(check_consistency) > 0) {
  warning(sprintf(
    "Found %d person-years with inconsistent P-level values across rounds. Using first non-NA.",
    nrow(check_consistency)
  ))
}

# Aggregate person-level data (take first non-missing value for each)
person_data <- data_clean %>%
  group_by(MEPSID, YEAR) %>%
  summarize(
    across(all_of(setdiff(person_vars, c("MEPSID", "YEAR"))), first_nonmissing),
    n_rounds = n(),
    .groups = "drop"
  )

message(sprintf("Created %s unique person-years from person-level data",
                format(nrow(person_data), big.mark = ",")))

# ==============================================================================
# 5. AGGREGATE ROUND-LEVEL DATA
# ==============================================================================

message("Aggregating round-level limitation data...")

# Define which vars are binary vs severity
round_binary_vars <- c("LADLRD", "LAIADLRD", "USEAIDRD", "LMTPHYSRD",
                       "LMTWORKRD", "LMTSOCRD", "LMTCOGRD")
round_severity_vars <- c("LMTLIFTRD", "LMTWALKRD", "LMTBENDRD")

# Aggregate limitation variables using appropriate logic
round_lmt_agg <- data_clean %>%
  group_by(MEPSID, YEAR) %>%
  summarize(
    # Binary vars: 2 = Yes limitation
    across(all_of(round_binary_vars), any_has_limitation_binary, .names = "{.col}_agg"),
    # Severity vars: >=2 = any limitation
    across(all_of(round_severity_vars), any_has_limitation_severity, .names = "{.col}_agg"),
    .groups = "drop"
  )

message("Aggregating round-level condition data...")

# Aggregate MHLTHRD specially: both binary and mean
# MHLTHRD coding: 1=Excellent, 2=Very Good, 3=Good, 4=Fair, 5=Poor
round_mhlth_agg <- data_clean %>%
  group_by(MEPSID, YEAR) %>%
  summarize(
    # Any fair/poor (4 or 5)
    mhlth_any_fairpoor = {
      x <- MHLTHRD
      if (all(is.na(x))) NA_real_
      else if (any(x >= 4, na.rm = TRUE)) 1
      else 0
    },
    # Mean mental health rating (higher = worse in original coding)
    mhlth_mean_raw = mean(MHLTHRD, na.rm = TRUE),
    # Number of rounds with MHLTHRD data
    mhlth_n_rounds = sum(!is.na(MHLTHRD)),
    .groups = "drop"
  ) %>%
  mutate(
    # Convert mean to NA if no data
    mhlth_mean_raw = if_else(is.nan(mhlth_mean_raw), NA_real_, mhlth_mean_raw)
  )

# Aggregate other condition variables (binary: 2 = Yes)
round_cond_agg <- data_clean %>%
  group_by(MEPSID, YEAR) %>%
  summarize(
    across(all_of(setdiff(round_cond_vars, "MHLTHRD")), any_has_limitation_binary, .names = "{.col}_agg"),
    .groups = "drop"
  )

# ==============================================================================
# 6. JOIN AND HARMONIZE
# ==============================================================================

message("Joining and harmonizing data...")

# Join all pieces
data_joined <- person_data %>%
  left_join(round_lmt_agg, by = c("MEPSID", "YEAR")) %>%
  left_join(round_mhlth_agg, by = c("MEPSID", "YEAR")) %>%
  left_join(round_cond_agg, by = c("MEPSID", "YEAR"))

# Create harmonized variables: prefer P-version, fall back to R-aggregated
# P-level coding: 0=NIU, 1=No, 2=Yes (binary); 1=Not at all, 2-4=Limited (severity)
data_harmonized <- data_joined %>%
  mutate(
    # Binary vars: P has limitation if value == 2
    ladl_p = case_when(LADL == 2 ~ 1, LADL == 1 ~ 0, TRUE ~ NA_real_),
    laiadl_p = case_when(LAIADL == 2 ~ 1, LAIADL == 1 ~ 0, TRUE ~ NA_real_),
    useaid_p = case_when(USEAID == 2 ~ 1, USEAID == 1 ~ 0, TRUE ~ NA_real_),
    lmtphys_p = case_when(LMTPHYS == 2 ~ 1, LMTPHYS == 1 ~ 0, TRUE ~ NA_real_),
    lmtwork_p = case_when(LMTWORK == 2 ~ 1, LMTWORK == 1 ~ 0, TRUE ~ NA_real_),
    lmtsoc_p = case_when(LMTSOC == 2 ~ 1, LMTSOC == 1 ~ 0, TRUE ~ NA_real_),
    lmtcog_p = case_when(LMTCOG == 2 ~ 1, LMTCOG == 1 ~ 0, TRUE ~ NA_real_),
    # Severity vars: P has limitation if value >= 2
    lmtlift_p = case_when(LMTLIFT >= 2 & LMTLIFT <= 4 ~ 1, LMTLIFT == 1 ~ 0, TRUE ~ NA_real_),
    lmtwalk_p = case_when(LMTWALK >= 2 & LMTWALK <= 4 ~ 1, LMTWALK == 1 ~ 0, TRUE ~ NA_real_),
    lmtbend_p = case_when(LMTBEND >= 2 & LMTBEND <= 4 ~ 1, LMTBEND == 1 ~ 0, TRUE ~ NA_real_)
  ) %>%
  mutate(
    # Harmonize: prefer P-version, fall back to R-aggregated
    ladl = coalesce(ladl_p, LADLRD_agg),
    laiadl = coalesce(laiadl_p, LAIADLRD_agg),
    useaid = coalesce(useaid_p, USEAIDRD_agg),
    lmtphys = coalesce(lmtphys_p, LMTPHYSRD_agg),
    lmtlift = coalesce(lmtlift_p, LMTLIFTRD_agg),
    lmtwalk = coalesce(lmtwalk_p, LMTWALKRD_agg),
    lmtbend = coalesce(lmtbend_p, LMTBENDRD_agg),
    lmtwork = coalesce(lmtwork_p, LMTWORKRD_agg),
    lmtsoc = coalesce(lmtsoc_p, LMTSOCRD_agg),
    lmtcog = coalesce(lmtcog_p, LMTCOGRD_agg)
  )

# ==============================================================================
# 7. FINAL VARIABLE CREATION AND SELECTION
# ==============================================================================

message("Creating final variables...")

data_final <- data_harmonized %>%
  # Standard SRH coding: higher = better
  mutate(
    srh = 6 - HEALTH,
    wt = SAQWEIGHT,
    psu = PSUANN,
    strata = STRATANN
  ) %>%
  # Age groups (matching wrangle_meps.R)
  mutate(
    age = AGE,
    year = YEAR,
    age_group = factor(
      cut(age,
          breaks = c(17, 29, 39, 49, 59, 69, 79, 89),
          labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
          right = TRUE)
    ),
    cohort = year - age
  ) %>%
  # Demographics
  mutate(
    sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      .default = NA_character_
    ),
    region = case_when(
      REGIONMEPS == 0 ~ NA_character_,
      REGIONMEPS == 1 ~ "Northeast",
      REGIONMEPS == 2 ~ "Midwest",
      REGIONMEPS == 3 ~ "South",
      REGIONMEPS == 4 ~ "West",
      .default = NA_character_
    ),
    race = case_when(
      RACEA %in% c(100) ~ "White",
      RACEA %in% c(200) ~ "Black",
      RACEA %in% c(300, 310, 320, 330, 340, 350) ~ "AIAN",
      RACEA %in% c(400, 410, 411, 412, 413, 414, 415, 416,
                   420, 421, 422, 423, 430, 431, 432, 433, 434) ~ "AAPI",
      .default = NA_character_
    ),
    hispanic = case_when(
      HISPYN == 1 ~ "Not Hispanic",
      HISPYN == 2 ~ "Hispanic",
      .default = NA_character_
    )
  ) %>%
  # Select final columns
  select(
    # Identifiers
    MEPSID, year, PANEL, n_rounds,
    # Weights and design
    PERWEIGHT, SAQWEIGHT, wt, psu, strata, PSUANN, STRATANN,
    # Demographics
    age, AGE, age_group, cohort, sex, SEX, race, RACEA, hispanic, HISPYN,
    region, REGIONMEPS, EDUC, EDUCYR, MARSTAT,
    # SRH
    HEALTH, srh,
    # Person-level limitation vars (original IPUMS coding)
    ANYLMT, LADL, LAIADL, USEAID, LMTPHYS, LMTLIFT, LMTWALK,
    LMTBEND, LMTWORK, LMTSOC, LMTCOG, DIFWLKCLM, ADDAYA,
    # Person-level recoded (binary: 1=has limitation)
    ends_with("_p"),
    # Round-aggregated vars (binary: 1=has limitation)
    ends_with("_agg"),
    # Harmonized limitation vars (P preferred, R fallback)
    ladl, laiadl, useaid, lmtphys, lmtlift, lmtwalk,
    lmtbend, lmtwork, lmtsoc, lmtcog,
    # Mental health aggregations
    mhlth_any_fairpoor, mhlth_mean_raw, mhlth_n_rounds
  )

# ==============================================================================
# 8. VALIDATION CHECKS
# ==============================================================================

message("\n========== VALIDATION CHECKS ==========\n")

# 1. Row counts
n_input <- nrow(data_raw)
n_output <- nrow(data_final)
n_expected <- data_raw %>% distinct(MEPSID, YEAR) %>% nrow()

message(sprintf("Input rows: %s", format(n_input, big.mark = ",")))
message(sprintf("Output rows: %s", format(n_output, big.mark = ",")))
message(sprintf("Expected unique MEPSID x YEAR: %s", format(n_expected, big.mark = ",")))
message(sprintf("Compression ratio: %.1fx", n_input / n_output))

stopifnot(
  "Output row count doesn't match expected unique person-years" =
    n_output == n_expected
)

# 2. No duplicate person-years
n_dups <- data_final %>%
  count(MEPSID, year) %>%
  filter(n > 1) %>%
  nrow()

message(sprintf("\nDuplicate MEPSID x year combinations: %d", n_dups))
stopifnot("Found duplicate person-years!" = n_dups == 0)

# 3. Harmonization source breakdown
harmonization_sources <- tibble(
  variable = c("ladl", "laiadl", "useaid", "lmtphys", "lmtlift",
               "lmtwalk", "lmtbend", "lmtwork", "lmtsoc", "lmtcog"),
  p_var = c("ladl_p", "laiadl_p", "useaid_p", "lmtphys_p", "lmtlift_p",
            "lmtwalk_p", "lmtbend_p", "lmtwork_p", "lmtsoc_p", "lmtcog_p"),
  r_var = c("LADLRD_agg", "LAIADLRD_agg", "USEAIDRD_agg", "LMTPHYSRD_agg",
            "LMTLIFTRD_agg", "LMTWALKRD_agg", "LMTBENDRD_agg",
            "LMTWORKRD_agg", "LMTSOCRD_agg", "LMTCOGRD_agg")
)

message("\n--- Harmonization Source Breakdown ---")
for (i in seq_len(nrow(harmonization_sources))) {
  v <- harmonization_sources$variable[i]
  p <- harmonization_sources$p_var[i]
  r <- harmonization_sources$r_var[i]

  from_p <- sum(!is.na(data_final[[p]]), na.rm = TRUE)
  from_r <- sum(is.na(data_final[[p]]) & !is.na(data_final[[r]]), na.rm = TRUE)
  total_nonmissing <- sum(!is.na(data_final[[v]]))

  message(sprintf("  %s: %d from P, %d from R, %d total non-missing",
                  v, from_p, from_r, total_nonmissing))
}

# 4. Missingness summary
message("\n--- Missingness Summary (harmonized vars) ---")
harm_vars <- c("ladl", "laiadl", "useaid", "lmtphys", "lmtlift",
               "lmtwalk", "lmtbend", "lmtwork", "lmtsoc", "lmtcog",
               "mhlth_any_fairpoor")
for (v in harm_vars) {
  pct_missing <- mean(is.na(data_final[[v]])) * 100
  message(sprintf("  %s: %.1f%% missing", v, pct_missing))
}

# 5. Value distributions (% yes for binary vars)
message("\n--- Limitation Prevalence (% yes among non-missing) ---")
for (v in c("ladl", "laiadl", "useaid", "lmtphys", "lmtlift",
            "lmtwalk", "lmtbend", "lmtwork", "lmtsoc", "lmtcog")) {
  pct_yes <- mean(data_final[[v]] == 1, na.rm = TRUE) * 100
  message(sprintf("  %s: %.1f%%", v, pct_yes))
}

message(sprintf("\n  mhlth_any_fairpoor: %.1f%%",
                mean(data_final$mhlth_any_fairpoor == 1, na.rm = TRUE) * 100))

# 6. Weight sanity checks
message("\n--- Weight Summary ---")
message(sprintf("  SAQWEIGHT: %d non-missing, %d zeros, mean = %.1f",
                sum(!is.na(data_final$SAQWEIGHT)),
                sum(data_final$SAQWEIGHT == 0, na.rm = TRUE),
                mean(data_final$SAQWEIGHT, na.rm = TRUE)))

# 7. Year distribution
message("\n--- Person-Years by Year ---")
year_dist <- data_final %>% count(year) %>% arrange(year)
print(year_dist, n = Inf)

# 8. Rounds distribution
message("\n--- Rounds per Person-Year ---")
rounds_dist <- data_final %>% count(n_rounds) %>% arrange(n_rounds)
print(rounds_dist)

# ==============================================================================
# 9. SAVE OUTPUT
# ==============================================================================

message("\n========== SAVING OUTPUT ==========\n")

# Save as RDS (primary)
output_rds <- derived_path("data_meps_round_personyear.rds")
write_rds(data_final, output_rds)
message(sprintf("Saved: %s", output_rds))

# Save as CSV (secondary)
output_csv <- derived_path("data_meps_round_personyear.csv")
write_csv(data_final, output_csv)
message(sprintf("Saved: %s", output_csv))

message("\nDone!")

# ==============================================================================
# 10. QUICK SUMMARY FOR REFERENCE
# ==============================================================================

message("\n========== FINAL DATA SUMMARY ==========\n")
message(sprintf("Dimensions: %s rows x %s columns",
                format(nrow(data_final), big.mark = ","),
                ncol(data_final)))
message(sprintf("Years: %d - %d", min(data_final$year), max(data_final$year)))

# Note: AGE 996 is IPUMS topcode for very old ages (typically 85+)
n_topcode_age <- sum(data_final$age >= 996, na.rm = TRUE)
valid_ages <- data_final$age[data_final$age < 996]
message(sprintf("Age range (valid): %d - %d", min(valid_ages, na.rm = TRUE),
                max(valid_ages, na.rm = TRUE)))
message(sprintf("Note: %d person-years have age topcode (996) - recode as needed",
                n_topcode_age))

# Summary for adults (age 18-89)
n_adults <- sum(data_final$age >= 18 & data_final$age <= 89, na.rm = TRUE)
message(sprintf("\nAdults age 18-89: %s person-years (%.1f%% of total)",
                format(n_adults, big.mark = ","),
                100 * n_adults / nrow(data_final)))
