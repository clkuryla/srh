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
  depot_path("surveys", "NHIS", "nhis_00009.csv"),
  show_col_types = FALSE
)

data_nhis <- data_nhis_raw %>%
  # Basic filters
  filter(!is.na(HEALTH)) %>%
  filter(!is.na(AGE)) %>%
  filter(AGE >= 18, AGE < 900) %>%  # Filter out AGE 997/999 codes (unknown/top-coded)
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
  add_age_group(age_var = age, scheme = "B") %>%
  # Additional variable processing
  mutate(
         flclimb = case_when(
           FLCLIMB %in% c(0, 50, 97, 98, 99) ~ NA_real_,
           FLCLIMB == 10 ~ 1,
           FLCLIMB %in% c(20, 21, 22) ~ 2,
           FLCLIMB == 30 ~ 3,
           FLCLIMB == 40 ~ 4,
           TRUE ~ NA_real_
         ),
         # K6 items: 0=none, 1=a little, 2=some, 3=most, 4=all of the time
         # 6=NIU, 7/8/9=unknown → NA
         aeffort = if_else(AEFFORT %in% 0:4, AEFFORT, NA_integer_),
         afeelint1mo = if_else(AFEELINT1MO %in% 0:4, AFEELINT1MO, NA_integer_),
         ahopeless = if_else(AHOPELESS %in% 0:4, AHOPELESS, NA_integer_),
         anervous = if_else(ANERVOUS %in% 0:4, ANERVOUS, NA_integer_),
         arestless = if_else(ARESTLESS %in% 0:4, ARESTLESS, NA_integer_),
         asad = if_else(ASAD %in% 0:4, ASAD, NA_integer_),
         aworthless = if_else(AWORTHLESS %in% 0:4, AWORTHLESS, NA_integer_),
         # K6 summary: 0=low, 1=medium, 2=serious distress; 8=unknown → NA
         pdistressk6 = if_else(PDISTRESSK6 %in% 0:2, PDISTRESSK6, NA_integer_),
         # K6 score (0-24): sum of 6 items, NA if any item missing
         k6 = aeffort + ahopeless + anervous + arestless + asad + aworthless,

         # =======================================================================
         # Hospital Utilization Variables
         # =======================================================================

         # HOSPNGHT: Hospital stay past year (0=NIU, 1=No, 2=Yes, 7/8/9=Unknown)
         hospitalized = case_when(
           HOSPNGHT == 2 ~ 1L,    # Yes
           HOSPNGHT == 1 ~ 0L,    # No
           TRUE ~ NA_integer_
         ),

         # ERYRNO: ER visits past year - binary (any visits)
         any_er = case_when(
           ERYRNO == 10 ~ 0L,                    # No visits
           ERYRNO %in% c(20, 30:48) ~ 1L,        # Any visits
           TRUE ~ NA_integer_
         ),

         # ERYRNO: ER visits (midpoint-imputed from categorical bands)
         # Code 10=No visits, 20=1, 30=2-3, 31=2, 32=3, 40=4+, 41=4-5, 42=6-7,
         # 43=8-9, 45=10-12, 46-48=13+, 97-99=Unknown
         er_visits = case_when(
           ERYRNO == 10 ~ 0,      # No visits
           ERYRNO == 20 ~ 1,      # 1 visit
           ERYRNO == 30 ~ 2.5,    # 2-3 visits (midpoint)
           ERYRNO == 31 ~ 2,      # 2 visits (granular code)
           ERYRNO == 32 ~ 3,      # 3 visits
           ERYRNO == 40 ~ 5,      # 4+ visits (conservative)
           ERYRNO == 41 ~ 4.5,    # 4-5 visits
           ERYRNO == 42 ~ 6.5,    # 6-7 visits
           ERYRNO == 43 ~ 8.5,    # 8-9 visits
           ERYRNO == 45 ~ 11,     # 10-12 visits
           ERYRNO %in% 46:48 ~ 15, # 13+ visits (top-coded)
           TRUE ~ NA_real_
         ),

         # HOMECAREYR: Home care past year (1=No, 2=Yes)
         home_care = case_when(
           HOMECAREYR == 2 ~ 1L,
           HOMECAREYR == 1 ~ 0L,
           TRUE ~ NA_integer_
         ),

         # SLDAYR: Sick leave days (count of days illness kept person in bed)
         # 0-365 = actual count, 997+ = unknown/refused
         sickdays = case_when(
           SLDAYR %in% 0:365 ~ as.numeric(SLDAYR),
           TRUE ~ NA_real_
         ),

         # USUALPL: Usual place of care (1=No, 2=Yes)
         has_usual_care = case_when(
           USUALPL == 2 ~ 1L,    # Yes
           USUALPL == 1 ~ 0L,    # No
           TRUE ~ NA_integer_
         ),

         # HINOTCOVE: No health insurance coverage (1=Covered, 2=Not covered)
         uninsured = case_when(
           HINOTCOVE == 2 ~ 1L,  # Not covered
           HINOTCOVE == 1 ~ 0L,  # Covered
           TRUE ~ NA_integer_
         )
  )

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

readr::write_rds(data_nhis, derived_path("data_nhis.rds"))
