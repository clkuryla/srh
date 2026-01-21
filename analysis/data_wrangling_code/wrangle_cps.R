# Wrangle CPS data
#
# Data source: IPUMS CPS extract (https://cps.ipums.org/)
# Uses ipumsr package to read IPUMS DDI + microdata files
#
# CPS has a 5-point SRH scale (like NHIS, MEPS, BRFSS, NHANES):
#   Original: 1=Excellent, 2=Very Good, 3=Good, 4=Fair, 5=Poor
#   Recoded:  5=Excellent, 4=Very Good, 3=Good, 2=Fair, 1=Poor (higher = better)
#
# Survey design limitation:
#   Using weights-only design with ASECWT (Annual Social and Economic Supplement weight).
#   PSU/strata may be available in some IPUMS extracts but not used here.

library(tidyverse)
library(here)
source(here::here("R/paths.R"))
ensure_dirs()

# ------------------------------------------------------------------------------
# Load data from IPUMS CPS extract
# ------------------------------------------------------------------------------

# Read IPUMS CPS CSV extract
# Adjust filename if your extract has a different number
data_cps_raw <- readr::read_csv(
  depot_path("surveys", "CPS", "cps_00003.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# Wrangle CPS data
# ------------------------------------------------------------------------------

data_cps <- data_cps_raw %>%

  # Filter to valid SRH responses (1-5) and quality flag

  filter(HEALTH %in% 1:5) %>%
  # QHEALTH == 0 means no allocation/imputation issues (if available)
  # filter(QHEALTH == 0) %>%
  filter(!is.na(AGE)) %>%
  filter(AGE >= 18) %>%
  filter(!is.na(ASECWT)) %>%
  # Recode SRH so higher = better (1=Poor ... 5=Excellent)
  mutate(
    srh = 6L - as.integer(HEALTH),
    srh_cat = factor(
      srh,
      levels = 1:5,
      labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")
    )
  ) %>%
  # Standardize variable names
  mutate(
    age = AGE,
    year = YEAR,
    wt = ASECWT
  ) %>%
  # Sex recode (if available)
  mutate(
    sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  # Create age groups consistent with other surveys
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
  ) %>%
  # Select and reorder columns

  select(
    year, age, sex, srh, srh_cat,
    age_group, age_group_6,
    wt,
    # Keep original variables for reference
    HEALTH, AGE, YEAR, SEX, ASECWT,
    everything()
  )

rm(data_cps_raw)

# ------------------------------------------------------------------------------
# Survey object
# ------------------------------------------------------------------------------
#
# Using weights-only design with ASECWT.
# If PSU/strata are available in your extract, update to:
#   as_survey_design(ids = PSU_VAR, strata = STRATA_VAR, weights = wt, nest = TRUE)

library(srvyr)

svy_cps <- data_cps %>%
  filter(!is.na(wt)) %>%
  as_survey_design(
    ids = 1,
    weights = wt
  )

# ------------------------------------------------------------------------------
# Save wrangled data
# ------------------------------------------------------------------------------

# readr::write_rds(data_cps, derived_path("data_cps.rds"))
