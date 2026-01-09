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
  depot_path("surveys", "NHIS", "nhis_00002.csv"),
  show_col_types = FALSE
)

data_nhis <- data_nhis_raw %>%
  filter(!(is.na(HEALTH))) %>% 
  filter(!(is.na(AGE))) %>% 
  filter(AGE > 18) %>% 
  filter(AGE < 90) %>% 
#  select(AGE, YEAR, HEALTH, PSU, STRATA, SAMPWEIGHT, SEX) %>% 
  filter(HEALTH %in% 1:5) %>% 
  mutate(age = AGE, 
         year = YEAR, 
    #     cohort = year - age,
         srh = 6 - HEALTH,
         psu = PSU,
         wt = SAMPWEIGHT,
         strata = STRATA,
         sex = SEX) 

rm(data_nhis_raw)

####### Survey object

# svy_nhis <- data_nhis %>%
#   #  filter(!(is.na(HHWEIGHT))) %>% 
#   filter(!(is.na(SAMPWEIGHT))) %>% 
#   as_survey_design(
#     ids = PSU,           # PSU identifiers (use 1 if not available)
#     weights = SAMPWEIGHT,  # missing some every once in a while
#     #  weights = HHWEIGHT, # missing 2019+, also this is for households, not individuals
#     strata = STRATA, 
#     nest = TRUE
#   )

readr::write_rds(data_nhis, derived_path("data_nhis.rds"))
