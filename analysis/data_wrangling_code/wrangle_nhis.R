# Wrangle NHIS data
# https://nhis.ipums.org/nhis/

# 1. Download NHIS data and desired variables from IPUMS into a designated folder
# 2. Unzip it
# 3. Run this script

library(tidyverse)
library(here)
source(here::here("R/paths.R")) # To access path for data as depot_path
ensure_dirs()

data_nhis_raw <- readr::read_csv(
  depot_path("surveys", "NHIS", "nhis_00008.csv"),
  show_col_types = FALSE
)

data_nhis <- data_nhis_raw %>%
  filter(!(is.na(HEALTH))) %>% 
  filter(!(is.na(AGE))) %>% 
  filter(AGE >= 18, AGE < 900) %>%  # Filter out AGE 997/999 codes (unknown/top-coded)
  filter(YEAR >= 1982) %>% # SRH has no "Very Good" in NHIS pre-1982
#  select(AGE, YEAR, HEALTH, PSU, STRATA, SAMPWEIGHT, SEX) %>% 
  filter(HEALTH %in% 1:5) %>% 
  mutate(age = AGE,
         year = YEAR,
    #     cohort = year - age,
         srh = 6 - HEALTH,
         psu = PSU,
         wt = SAMPWEIGHT,
         strata = STRATA,
         sex = SEX,
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
         k6 = aeffort + ahopeless + anervous + arestless + asad + aworthless) 

rm(data_nhis_raw)

####### Survey object
library(survey)
library(srvyr)

svy_nhis <- data_nhis %>%
  #  filter(!(is.na(HHWEIGHT))) %>%
  filter(!(is.na(SAMPWEIGHT))) %>%
  as_survey_design(
    ids = psu,           # PSU identifiers (use 1 if not available)
    weights = wt,  # missing some every once in a while
    #  weights = HHWEIGHT, # missing 2019+, also this is for households, not individuals
    strata = strata,
    nest = TRUE
  )

readr::write_rds(data_nhis, derived_path("data_nhis.rds"))
