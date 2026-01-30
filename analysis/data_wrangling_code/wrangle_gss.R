# Wrangle GSS data
#
# Data source: gssr package (https://kjhealy.r-universe.dev)
#
# IMPORTANT: GSS has a 4-point SRH scale (no "Very Good" category):
#   Original: 1=Excellent, 2=Good, 3=Fair, 4=Poor
#   Recoded:  4=Excellent, 3=Good, 2=Fair, 1=Poor (higher = better)
#
# Survey design limitation:
#   The gssr package does not provide PSU (vpsu) or strata (vstrat) variables.
#   We use a weights-only design, which may underestimate standard errors.
#   If PSU/strata become available from another source, update the survey object.

library(tidyverse)
library(here)
source(here::here("R/paths.R"))
ensure_dirs()

# ------------------------------------------------------------------------------
# Load data from gssr package
# ------------------------------------------------------------------------------

# Install if needed:
# install.packages('gssr', repos = c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))

if (!requireNamespace("gssr", quietly = TRUE)) {
  stop("Package 'gssr' is required. Install with:\n
       install.packages('gssr', repos = c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))")
}

library(gssr)
data("gss_all")

data_gss_raw <- gss_all %>%
  haven::zap_labels() %>%
  select(
    # Core variables
    year,
    cohort,
    age,
    health,
    sex,
    # Covariates
    happy,
    life,
    educ,
    polviews,
    class,
    satfin,
    region,
    attend,
    race,
    wrkstat,
    hispanic,
    degree,
    marital,
    partyid,
    # Weights (vpsu and vstrat not available in gssr)
    # Note: wtsscomp was removed in gssr v0.8+
    wtssps,
    wtssall
  )

# ------------------------------------------------------------------------------
# Wrangle GSS data
# ------------------------------------------------------------------------------

data_gss <- data_gss_raw %>%
  # Filter invalid values

  filter(cohort != 9999) %>%
  filter(!is.na(health)) %>%
  filter(!is.na(age)) %>%
  filter(age >= 18) %>%
  # health is coded 1=Excellent ... 4=Poor in raw data
  # Recode so higher = better: 4=Excellent, 3=Good, 2=Fair, 1=Poor
  filter(health %in% 1:4) %>%
  mutate(
    srh = 5 - health,
    srh_cat = factor(
      srh,
      levels = 1:4,
      labels = c("Poor", "Fair", "Good", "Excellent")
    )
  ) %>%
  # Recode other variables to be more intuitive (higher = better)
  mutate(
    happy = if_else(happy %in% 1:3, 4L - as.integer(happy), NA_integer_),
    life = if_else(life %in% 1:3, 4L - as.integer(life), NA_integer_),
    satfin = if_else(satfin %in% 1:3, 4L - as.integer(satfin), NA_integer_)
  ) %>%
  # Standardize variable names
  rename(
    year = year,
    age = age
  ) %>%
  mutate(
    # Sex recode
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    # Weights: wtssall for multi-year analyses, wtssps for single-year
    wt = coalesce(wtssall, wtssps)
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
  )

rm(data_gss_raw)
rm(gss_all)

# ------------------------------------------------------------------------------
# Survey object
# ------------------------------------------------------------------------------
#
# LIMITATION: vpsu and vstrat are not available from gssr package.
# Using weights-only design. This may underestimate standard errors.
# If PSU/strata become available, update to:
#   as_survey_design(ids = vpsu, strata = vstrat, weights = wt, nest = TRUE)

library(srvyr)

svy_gss <- data_gss %>%
  filter(!is.na(wt)) %>%
  as_survey_design(
    ids = 1,
    weights = wt
  )

# ------------------------------------------------------------------------------
# Save wrangled data
# ------------------------------------------------------------------------------
 
# Strip all haven/Stata metadata (variable labels, format.stata attributes)
data_gss <- labelled::remove_attributes(data_gss, c("label", "format.stata"))

readr::write_rds(data_gss, derived_path("data_gss.rds"))
