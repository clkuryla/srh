# Wrangle NHIS data
# https://nhis.ipums.org/nhis/
#
# 1. Download NHIS data and desired variables from IPUMS into a designated folder
# 2. Unzip it
# 3. Run this script

library(tidyverse)
library(here)
source(here::here("R/paths.R")) # To access path for data as depot_path
source(here::here("R/srh_common_functions.R")) # For add_age_group()
ensure_dirs()

# =============================================================================
# Variable definitions
# =============================================================================

# Comorbidities (binary ever-told): NHIS uses 1=No, 2=Yes
nhis_vars_comorb <- c(
  "ADDEV",        # ADHD ever
  "ANGIPECEV",    # Angina pectoris ever
  "ARTHGLUPEV",   # Arthritis/gout/lupus ever

  "ASTHMAEV",     # Asthma ever
  "ASTHMASTIL",   # Still have asthma
  "AUTISMEV",     # Autism ever
  "CANCEREV",     # Cancer ever
  "CHEARTDIEV",   # Coronary heart disease ever
  "CHOLHIGHEV",   # High cholesterol ever
  "COPDEV",       # COPD ever
  "DEPRESSEV",    # Depression ever
  "DIABETICEV",   # Diabetes ever
  "HEPATEV",      # Hepatitis ever
  "HYPERTENEV",   # Hypertension ever
  "KIDNEYWKYR",   # Kidney weak past year
  "LEARNDEV",     # Learning disability ever
  "ODDEV",        # Oppositional defiant disorder
  "RETEV",        # Retinopathy ever
  "STROKEV"       # Stroke ever
)

# Mental health variables
nhis_vars_mental <- c(
  "ANXIETYEV",    # Anxiety ever (1=No, 2=Yes)
  "WORFREQ",      # Worry frequency
  "DEPFREQ",      # Depression frequency
  "DEPRX",        # Takes depression medication
  "DEPFEELEVL",   # Depression feeling level
  "SAWMENT",      # Saw mental health professional
  "SATISFIED",    # Life satisfaction
  "UNHAPPY"       # Unhappiness
)

# Diabetes detail variables
nhis_vars_diabetes <- c(
  "DIABETICAGE",  # Age at diabetes diagnosis
  "DIABTYPE",     # Diabetes type
  "INSULIN",      # Uses insulin
  "DIAPILLS",     # Takes diabetes pills
  "DIAYRSAGO"     # Years since diabetes
)

# Hypertension detail variables
nhis_vars_htn <- c(
  "HYP2TIME",     # HTN told on 2+ visits
  "HYPMEDNOW"     # Currently on HTN meds
)

# Physical measures
nhis_vars_physical <- c(
  "BMICALC",      # BMI calculated
  "BMICAT",       # BMI category
  "HEIGHT",       # Height
  "WEIGHT"        # Weight
)

# Mortality linkage variables
nhis_vars_mort <- c(
  "MORTELIG",     # Mortality eligible
  "MORTSTAT",     # Mortality status
  "MORTDODQ",     # Death date quarter
  "MORTDODY",     # Death date year
  "MORTUCOD",     # Underlying cause of death
  "MORTUCODLD",   # Leading cause
  "MORTWT",       # Mortality weight
  "MORTDIAB",     # Diabetes on death cert
  "MORTHIPFX",    # Hip fracture on death cert
  "MORTHYPR"      # Hypertension on death cert
)

# Asthma detail variables
nhis_vars_asthma <- c(
  "ASTHATAKYR",   # Asthma attacks past year
  "ASTHERYR"      # Asthma ER visits past year
)

# Healthcare utilization
nhis_vars_utilization <- c(
  "HOSPNGHT",     # Hospital nights
  "ERYRNO",       # ER visits past year
  "HOMECAREYR",   # Home care past year
  "DVINT",        # Doctor visit interval
  "SLDAYR"        # Sleep days
)

# Comorbidity variables that are binary (1=No, 2=Yes) and need recoding
# Note: ANXIETYEV also uses this coding
nhis_vars_binary_recode <- c(nhis_vars_comorb, "ANXIETYEV")

# =============================================================================
# Load and wrangle data
# =============================================================================

data_nhis_raw <- readr::read_csv(
  depot_path("surveys", "NHIS", "nhis_00006.csv"),
  show_col_types = FALSE
)

data_nhis <- data_nhis_raw %>%
  # Basic filters
  filter(!is.na(HEALTH)) %>%
  filter(!is.na(AGE)) %>%
  filter(AGE >= 18) %>%
  filter(YEAR >= 1982) %>% # SRH has no "Very Good" in NHIS pre-1982
  filter(HEALTH %in% 1:5) %>%
  # Create standardized variable names
  mutate(
    age = AGE,
    year = YEAR,
    srh = 6 - HEALTH,  # Recode so higher = better (1=Poor -> 5=Excellent)
    psu = PSU,
    wt = SAMPWEIGHT,
    strata = STRATA,
    sex = SEX
  ) %>%
  # Recode binary comorbidity variables: 1=No -> 0, 2=Yes -> 1
  mutate(across(
    all_of(nhis_vars_binary_recode),
    ~ case_when(
      .x == 1 ~ 0L,    # No
      .x == 2 ~ 1L,    # Yes
      TRUE ~ NA_integer_
    )
  )) %>%
  # Add age group using scheme B (18-29, 30-39, etc.)
  add_age_group(age_var = age, scheme = "B")

rm(data_nhis_raw)

# =============================================================================
# Survey object
# =============================================================================

library(survey)
library(srvyr)

svy_nhis <- data_nhis %>%
  filter(!is.na(SAMPWEIGHT)) %>%
  as_survey_design(
    ids = psu,           # PSU identifiers
    weights = wt,        # Sample weight
    strata = strata,
    nest = TRUE
  )

# =============================================================================
# Optional: save processed data
# =============================================================================
# readr::write_rds(data_nhis, derived_path("data_nhis.rds"))
