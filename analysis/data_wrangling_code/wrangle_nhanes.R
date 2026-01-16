# Wrangle NHANES data
#
# Data source: Kamaryn's preprocessed NHANES 1999-2018 dataset
# Location: data_depot/surveys/NHANES/kamaryn_nhanes/nhanes_1999-2018_2023-11-29.csv
#
# NHANES has a 5-point SRH scale:
#   Original (health): 1=Excellent, 2=Very Good, 3=Good, 4=Fair, 5=Poor
#   Recoded (srh):     5=Excellent, 4=Very Good, 3=Good, 2=Fair, 1=Poor (higher = better)
#
# Survey design notes:
#   - Uses MEC exam weights (dem_wghts = WTMEC2YR)
#   - Strata: strat_mvu
#   - PSU: id_mvu (if available, otherwise weights-only design)
#   - NHANES uses 2-year survey cycles; each cycle should be analyzed separately
#     or weights adjusted for pooling multiple cycles

library(tidyverse)
library(here)
source(here::here("R/paths.R"))
ensure_dirs()

# ------------------------------------------------------------------------------
# Load data from Kamaryn's preprocessed NHANES file
# ------------------------------------------------------------------------------

# File is gzip compressed but has .csv extension - read_csv handles this
data_nhanes_raw <- read_csv(

  depot_path("surveys", "NHANES", "kamaryn_nhanes", "nhanes_1999-2018_2023-11-29.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# Wrangle NHANES data
# ------------------------------------------------------------------------------

data_nhanes <- data_nhanes_raw %>%
  # Filter to first visit only (NHANES has some repeat visits)
  filter(visit == 1) %>%
  # Filter to adults with valid SRH
  filter(age_visit >= 18) %>%
  filter(!is.na(health)) %>%
  filter(health %in% 1:5) %>%
  # Recode SRH so higher = better (1=Poor ... 5=Excellent)
  mutate(
    srh = 6L - as.integer(health),
    srh_cat = factor(
      srh,
      levels = 1:5,
      labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")
    )
  ) %>%
  # Standardize variable names
  mutate(
    age = age_visit,
    # release_nb is the NHANES cycle number
    # Each cycle spans 2 years; use midpoint as "year" for this project
    # yr_period (1 or 2) indicates which year within the wave, not calendar year
    year = case_when(
      release_nb == 1 ~ 1999.5,
      release_nb == 2 ~ 2001.5,
      release_nb == 3 ~ 2003.5,
      release_nb == 4 ~ 2005.5,
      release_nb == 5 ~ 2007.5,
      release_nb == 6 ~ 2009.5,
      release_nb == 7 ~ 2011.5,
      release_nb == 8 ~ 2013.5,
      release_nb == 9 ~ 2015.5,
      release_nb == 10 ~ 2017.5,
      TRUE ~ NA_real_
    ),
    cohort = year - age,
    # Weights: use 2-year MEC exam weight
    wt = dem_wghts,
    # Survey design variables
    strata = strat_mvu,
    psu = id_mvu
  ) %>%
  # Sex recode
  mutate(
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
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
  # Keep SEQN for potential linkage
 rename(SEQN = BaseID)

rm(data_nhanes_raw)

# ------------------------------------------------------------------------------
# Survey object
# ------------------------------------------------------------------------------
#
# NHANES has complex survey design with strata and PSUs.
# If strata and PSU are available, use full design; otherwise weights-only.

library(srvyr)

# Check if we have valid strata and PSU
has_design_vars <- !all(is.na(data_nhanes$strata)) && !all(is.na(data_nhanes$psu))

if (has_design_vars) {
  message("Using full survey design with strata and PSU")
  svy_nhanes <- data_nhanes %>%
    filter(!is.na(wt)) %>%
    filter(!is.na(strata)) %>%
    filter(!is.na(psu)) %>%
    as_survey_design(
      ids = psu,
      strata = strata,
      weights = wt,
      nest = TRUE
    )
} else {
  message("Using weights-only survey design (strata/PSU not available)")
  svy_nhanes <- data_nhanes %>%
    filter(!is.na(wt)) %>%
    as_survey_design(
      ids = 1,
      weights = wt
    )
}

# ------------------------------------------------------------------------------
# Save wrangled data
# ------------------------------------------------------------------------------

# readr::write_rds(data_nhanes, derived_path("data_nhanes.rds"))
