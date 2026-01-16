# Wrangle MEPS data
# https://meps.ipums.org/meps/

# 1. Download MEPS data and desired variables from IPUMS into a designated folder
# 2. Unzip it
# 3. Run this script

library(tidyverse)
library(here)
source(here::here("R/paths.R")) # To access path for data as depot_path
ensure_dirs()

data_meps_raw <- readr::read_csv(
  depot_path("surveys", "MEPS", "ipums_extracts", "meps_00001.csv"),
  show_col_types = FALSE
)

####################################
###### MEPS COVARS VECTORS WITH MEANING
###### https://meps.ipums.org/meps-action/variables/group?id=health_mental
####################################

meps_vars_demog <- c(
  "SEX", # 1=Male, 2=Female # != 7, 8, 9
  "REGIONMEPS", # 0 = NIU; 1=Northeast; 2=Midwest; 3=South; 4=West
  "RACEA",
  "HISPYN", # 1=No, not of Hispanic ethnicity; 2=Yes, of Hispanic ethnicity
  "EDUC", "EDUCYR", "STUDENT", "HIDEG"
)

meps_vars_comorb <- c(# 012; # != 789 # 0=NIU; 1=No, 2=Yes
  "ADDEV", #	Ever told had ADHD/ADD # 2008-2023
  "ANGIPECEV", #	Ever told had angina pectoris # 2007-2023
  "ARTHGLUPEV", #	Ever told had arthritis/rheumatoid arthritis/gout/lupus/fibromyalgia
  "ASTHMAEV", #	Ever told had asthma
  "CANCEREV",
  "CHEARTDIEV", #	Ever told had coronary heart disease
  "CHOLHIGHEV", #	Ever told had high cholesterol
  "DIABETICEV", #	Ever told had diabetes
  "EMPHYSEMEV", #	Ever told had emphysema
  "HEARTATTEV", #	Ever told had heart attack
  "HEARTCONEV", #	Ever told had heart condition/disease
  "HYPERTENEV", #	Ever told had hypertension
  "HYP2TIME", #	Ever told had hypertension on 2+ visits
  "STROKEV" #	Ever told had a stroke )
)

mep_vars_cancer <- c(
  "CANCEREV", # Ever told had cancer; 0=NIU; 1=No; 2=Yes
  "CNMELN", # Ever had cancer: Melanoma: 0=NIU; 1=Not mentioned; 2=Mentioned
  "CNSKDK", # Ever had cancer: Skin (don't know what kind)
  "CNSKNM" # Ever had cancer: Skin (non-melanoma)
  # THERE ARE MORE SPECIFICS IF WE WANT TO GO THERE LATER
)

meps_vars_k6_specific <- c(# filter != 7, 8, 9 # Specific: 01234 
  "AEFFORT", # Felt everything an effort, past 30 days 2004-2023
  "AHOPELESS", # How often felt hopeless, past 30 days
  "ANERVOUS", #	How often felt nervous, past 30 days (adults)
  "ARESTLESS", #	How often felt restless, past 30 days (adults)
  "ASAD", #	How often felt sad, past 30 days (adults)
  "AWORTHLESS" #	How often felt worthless, past 30 days (adults)
)

meps_vars_phq2_2weeks <- c(#phq 2 weeks: 0123
  "PHQINTR", #	Little interest in doing things: past two weeks
  "PHQDEP" #	Feeling down, depressed, or hopeless: past two weeks
)

meps_vars_health_limit <- c(# health limit: 012
  "ADDAYA", #	Health now limits moderate activities
  "ADCLIM", #	Health now limits climbing several flights of stairs
  #012 (!= 8)
  "ANYLMT" #	Any limitation reported
)

meps_vars_limit_4weeks <- c(# health limit 4 weeks: 01234
  "ADPALS", #	Accomplished less because of physical health: past 4 weeks
  "ADPWLM", #	Limited in kind of work because of physical health: past 4 weeks
  "ADMALS", #	Accomplished less because of emotional problems: past 4 weeks
  "ADMWLM", #	Did work less carefully because of emotional problems: past 4 weeks
  "ADPAIN", #	Pain interfered with normal work: past 4 weeks
  "ADSOCA" #	Physical or emotional health interfered with social activities: past 4 weeks
)

meps_vars_4weeks_weird_coding <- c(# https://meps.ipums.org/meps-action/variables/ADDAYA#description_section 
  # 0123456
  "ADPCFL", #	How often felt calm/peaceful, past 4 weeks 2017-2023
  # 012346 (no 5?)
  "ADCAPE", #	Felt calm/peaceful: past 4 weeks 2003-2016
  # same pattern of weird variable
  "ADENGY", #	How often had a lot of energy, past 4 weeks 2017-2023
  "ADNRGY", #	Had a lot of energy: past 4 weeks 2003-2016
  "ADPRST", #	How often felt downhearted/depressed, past 4 weeks 2017-2023
  "ADDOWN" #	Felt depressed: past 4 weeks 2003-2016
)

meps_vars_sum_k6_phq2 <- c(# 0-24 (!= 96, 98)
  "K6SUM", #	K6 score for nonspecific psychological distress: last 30 days
  # 0123456 (!= 96, 98)
  "PHQ2" #	PHQ-2 depression screen summary score
)

### MEPS VARS CONSOLIDATED 

# meps_vars_demog
# meps_vars_comorb
meps_covar_notcomorb <- combine(meps_vars_k6_specific,
                                mep_vars_cancer,
                                meps_vars_phq2_2weeks,
                                meps_vars_health_limit,
                                meps_vars_limit_4weeks,
                                meps_vars_4weeks_weird_coding,
                                meps_vars_sum_k6_phq2
)

# select(all_of(chosen_columns)) where chosen_columns = c("name1", "name2")
# select(all_of(c(cols_set1, cols_set2))) %>%
# mutate(across(all_of(cols_to_clean), ~ replace(., . %in% c(7, 8, 9), NA)))

####################################################
###### SELECT VARS OF INTEREST
####################################################

data_meps <- data_meps_raw %>% 
  select(MEPSID, 
         YEAR, AGE, HEALTH, 
         BIRTHYR, 
         PERWEIGHT, # person weight for general variables
         SAQWEIGHT, #For HEALTH (srh)
         PSUANN, STRATANN, #, 
         PSUPLD, STRATAPLD, 
         #   PANELYR, RELYR,
         all_of(c(meps_vars_demog,
                  meps_vars_comorb, 
                  meps_covar_notcomorb))
  )

####################################################
###### FILTER, RENAME, AND CLEAN
####################################################

data_meps <- data_meps %>% 
  filter(!(is.na(HEALTH))) %>% 
  filter(!(is.na(AGE))) %>% 
  filter(!(is.na(SAQWEIGHT))) %>% 
  filter(AGE >= 18) %>% 
  filter(HEALTH %in% 1:5) %>% 
  rename(age = AGE, 
         year = YEAR) %>% 
  mutate(cohort = year - age,
         srh = 6 - HEALTH,
         id = MEPSID,
         wt = SAQWEIGHT,
         psu = PSUANN, 
         strata = STRATANN,
         age_group = as.factor( 
           cut(
             age,
             breaks = c(17, 29, 39, 49, 59, 69, 79, 89),
             labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
             right = TRUE)),
         age_group_2 = as.factor( 
           cut(
             age,
             breaks = c(17, 25, 35, 45, 55, 65, 75, 85),
             labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76-85"),
             right = TRUE))
  ) %>% 
  mutate(age_group_char = as.character(age_group),
         age_group_2_char = as.character(age_group_2)) %>% 
  mutate(
    sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      .default = NA),
    region = case_when(# 0 = NIU; 1=Northeast; 2=Midwest; 3=South; 4=West
      REGIONMEPS == 0 ~ NA,
      REGIONMEPS == 1 ~ "Northeast",
      REGIONMEPS == 2 ~ "Midwest",
      REGIONMEPS == 3 ~ "South",
      REGIONMEPS == 4 ~ "West",
      .default = NA),
    race = case_when(
      RACEA %in% c(100) ~ "White",
      RACEA %in% c(200) ~ "Black",
      RACEA %in% c(300, 310, 320, 330, 340, 350) ~ "AIAN",
      RACEA %in% c(
        400, 410, 411, 412, 413, 414, 415, 416,
        420, 421, 422, 423,
        430, 431, 432, 433, 434
      ) ~ "AAPI",
      RACEA %in% c(  
        500, 510, 520, 530, 540, 550, 560, 570, 580 # Other Race categories
      ) ~ "Other",
      RACEA %in% c(
        600, 610, 611, 612, 613, 614, 615, 616, 617 # Multiple race
      ) ~ "Other",
      RACEA %in% c(900, 970, 980, 990) ~ "Other", # Unknowns --> Other
      .default = NA_character_),
    hispanic = case_when( #   "HISPYN", # 1=No, not of Hispanic ethnicity; 2=Yes, of Hispanic ethnicity
      HISPYN == 1 ~ "Not Hispanic",
      HISPYN == 2 ~ "Hispanic",
      .default = NA_character_)
  ) %>% # end mutate
  select(-SEX, -REGIONMEPS, -RACEA, -HISPYN) %>% 
  # NA for unknown/not answered/etc
  mutate(across(all_of(combine(meps_vars_comorb,
                               meps_covar_notcomorb)), ~ replace(., . %in% c(7, 8, 9), NA))) %>% 
  mutate(across(all_of(meps_vars_sum_k6_phq2), ~ replace(., . %in% c(96, 98), NA))) %>% 
  mutate(across(all_of(meps_vars_comorb),
                ~ case_when(
                  .x == 0 ~ NA_real_, # NIU
                  .x == 1 ~ 0, # No
                  .x == 2 ~ 1, # Yes
                  TRUE    ~ NA_real_
                )))


# readr::write_rds(data_meps, derived_path("data_meps.rds"))

rm(data_meps_raw)
#################################################### 
########################## CREATE SURVEY OBJECT
#################################################### 

#library(survey)
library(srvyr)
svy_meps <- data_meps %>% 
  as_survey_design(
    ids = psu,
    weights = wt,
    strata = strata,
    nest = TRUE)
