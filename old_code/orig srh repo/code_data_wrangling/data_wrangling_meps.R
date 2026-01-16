###### MEPS Data Import and Wrangle
###### MEPS downloaded as an extract from IPUMS

library(here)
library(tidyverse)

data_meps_raw <- read_csv("big_data/MEPS/meps_00001.csv")
colnames(data_meps_raw)


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
  filter(AGE <= 89) %>% 
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
  
  
  


##########################################
######## Double check #############

data_meps %>%
  group_by(YEAR) %>%
  summarise(
    across(
      c(#MORTSTAT, ASTATFLG, 
        HEALTH, AGE, MEPSID), 
      ~ mean(!is.na(.), na.rm = TRUE)  # fraction of non-NA
    )
  ) %>%
  View()

#################################################### 
########################## CREATE SURVEY OBJECT
#################################################### 

#library(survey)
library(srvyr)
svy_meps <- data_meps %>% 
  as_survey_design(
    ids = psu,
    weights = wt)#,
    # strata = ~strata,
    #  nest = TRUE,)


################################################################################
# ADD THIS TO YOUR EXISTING MEPS WRANGLING SCRIPT
# Insert after your existing mutate() blocks, before creating the survey object
################################################################################

# ==============================================================================
# DIAGNOSTIC: First, let's see what's happening with the 2017 transition
# ==============================================================================

# Check value ranges by year for the problematic variables
cat("\n=== Checking value ranges for 4-week variables ===\n")
data_meps %>%
  filter(year %in% 2014:2020) %>%
  group_by(year) %>%
  summarise(
    ADMALS_mean = mean(ADMALS, na.rm = TRUE),
    ADMWLM_mean = mean(ADMWLM, na.rm = TRUE),
    ADPALS_mean = mean(ADPALS, na.rm = TRUE),
    ADPWLM_mean = mean(ADPWLM, na.rm = TRUE),
    ADSOCA_mean = mean(ADSOCA, na.rm = TRUE),
    ADPAIN_mean = mean(ADPAIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Check if scale direction is consistent (correlation with self-rated health)
# Higher HEALTH values = worse health (1=Excellent to 5=Poor in original)
# After your recoding, higher srh = better health (you did srh = 6 - HEALTH)
# So if scale is correct: worse health (low srh) should correlate with 
# MORE problems (high ADMALS etc.)

cat("\n=== Checking scale direction (correlation with SRH) ===\n")
data_meps %>%
  filter(year %in% c(2015, 2016, 2017, 2018)) %>%
  group_by(year) %>%
  summarise(
    cor_srh_ADMALS = cor(srh, ADMALS, use = "complete.obs"),
    cor_srh_ADPALS = cor(srh, ADPALS, use = "complete.obs"),
    cor_srh_ADPAIN = cor(srh, ADPAIN, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  print()

# If correlations are NEGATIVE (worse health = fewer problems reported), 
# the scale direction is wrong for that period

# ==============================================================================
# HARMONIZATION CODE
# ==============================================================================

data_meps <- data_meps %>%
  mutate(
    # -------------------------------------------------------------------------
    # PART A: Harmonize the RENAMED variables (5-point → 6-point scale)
    # These variables changed names AND expanded from 5 to 6 response options
    # Strategy: Collapse "A good bit of the time" (code 3) into "Some" (code 2)
    # -------------------------------------------------------------------------
    
    # Calm/Peaceful: Combine ADCAPE (2003-2016) with ADPCFL (2017+)
    # Note: Higher values = more calm/peaceful (POSITIVE outcome)
    calm_peaceful = case_when(
      !is.na(ADCAPE) & year < 2017 ~ ADCAPE,
      !is.na(ADPCFL) & year >= 2017 ~ case_when(
        ADPCFL == 0 ~ 0,  # None of the time
        ADPCFL == 1 ~ 1,  # A little of the time
        ADPCFL == 2 ~ 2,  # Some of the time
        ADPCFL == 3 ~ 2,  # A good bit of the time → Some of the time
        ADPCFL == 4 ~ 3,  # Most of the time
        ADPCFL == 5 ~ 4,  # All of the time
        TRUE ~ NA_real_
      ),
      TRUE ~ NA_real_
    ),
    
    # Energy: Combine ADNRGY (2003-2016) with ADENGY (2017+)
    # Note: Higher values = more energy (POSITIVE outcome)
    energy = case_when(
      !is.na(ADNRGY) & year < 2017 ~ ADNRGY,
      !is.na(ADENGY) & year >= 2017 ~ case_when(
        ADENGY == 0 ~ 0,
        ADENGY == 1 ~ 1,
        ADENGY == 2 ~ 2,
        ADENGY == 3 ~ 2,  # Collapse "good bit" into "some"
        ADENGY == 4 ~ 3,
        ADENGY == 5 ~ 4,
        TRUE ~ NA_real_
      ),
      TRUE ~ NA_real_
    ),
    
    # Depressed/Downhearted: Combine ADDOWN (2003-2016) with ADPRST (2017+)
    # Note: Higher values = MORE depressed (NEGATIVE outcome)
    depressed = case_when(
      !is.na(ADDOWN) & year < 2017 ~ ADDOWN,
      !is.na(ADPRST) & year >= 2017 ~ case_when(
        ADPRST == 0 ~ 0,
        ADPRST == 1 ~ 1,
        ADPRST == 2 ~ 2,
        ADPRST == 3 ~ 2,  # Collapse "good bit" into "some"
        ADPRST == 4 ~ 3,
        ADPRST == 5 ~ 4,
        TRUE ~ NA_real_
      ),
      TRUE ~ NA_real_
    ),
    
    # -------------------------------------------------------------------------
    # PART B: Harmonize the NON-RENAMED variables (scale reversal issue)
    # According to IPUMS docs, they recoded these to consistent 0-4 scale
    # BUT if you're still seeing shifts, the issue might be different
    # 
    # Looking at your graphs: values DROP sharply in 2017
    # This suggests that in your current coding:
    # - Pre-2017: Higher values = MORE problems
    # - Post-2017: Values appear lower
    # 
    # If IPUMS harmonization is correct and both periods have:
    #   0 = None of the time, 4 = All of the time
    # Then the drop could reflect:
    # 1. Real response differences due to question wording changes
    # 2. Or incomplete harmonization
    #
    # Let's check and fix if needed:
    # -------------------------------------------------------------------------
    
    # Check your diagnostic results first!
    # If correlation with health FLIPS sign in 2017, uncomment the fixes below:
    
    ADMALS_harm = if_else(
      year < 2017,
      4 - ADMALS,  # Reverse pre-2017: 0↔4, 1↔3, 2↔2
      ADMALS
    ),
    
    ADMWLM_harm = if_else(
      year < 2017,
      4 - ADMWLM,
      ADMWLM
    ),
    
    ADPALS_harm = if_else(
      year < 2017,
      4 - ADPALS,
      ADPALS
    ),
    
    ADPWLM_harm = if_else(
      year < 2017,
      4 - ADPWLM,
      ADPWLM
    ),
    
    ADSOCA_harm = if_else(
      year < 2017,
      4 - ADSOCA,
      ADSOCA
    )
  )

# ==============================================================================
# VALIDATION: Check if harmonization worked
# ==============================================================================

cat("\n=== Validation: Check harmonized variables ===\n")

# Check the renamed variables after harmonization
data_meps %>%
  filter(year %in% 2014:2020) %>%
  group_by(year) %>%
  summarise(
    calm_peaceful_mean = mean(calm_peaceful, na.rm = TRUE),
    energy_mean = mean(energy, na.rm = TRUE),
    depressed_mean = mean(depressed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Plot to verify
library(ggplot2)

# Quick validation plot for the renamed variables
validation_data <- data_meps %>%
  filter(year >= 2004) %>%
  group_by(year) %>%
  summarise(
    calm_peaceful = mean(calm_peaceful, na.rm = TRUE),
    energy = mean(energy, na.rm = TRUE),
    depressed = mean(depressed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "mean_value")

ggplot(validation_data, aes(x = year, y = mean_value, color = variable)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 2016.5, linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 2017, y = max(validation_data$mean_value, na.rm = TRUE), 
           label = "VR-12\ntransition", hjust = 0, vjust = 1, size = 3) +
  labs(
    title = "Harmonized Variables: Check for 2017 Discontinuity",
    subtitle = "If harmonization worked, there should be no sharp break at 2017",
    x = "Year", y = "Mean Value"
  ) +
  theme_minimal()

# ==============================================================================
# ADDITIONAL: Update your variable vectors for analysis
# ==============================================================================

# Add harmonized variables to your analysis
meps_vars_4weeks_harmonized <- c(
  "calm_peaceful",  # Harmonized ADCAPE/ADPCFL
  "energy",         # Harmonized ADNRGY/ADENGY  
  "depressed"       # Harmonized ADDOWN/ADPRST
)

# If you applied the reversal fixes, also update:
meps_vars_limit_4weeks_harm <- c(
  "ADMALS_harm",
  "ADMWLM_harm",
  "ADPALS_harm",
  "ADPWLM_harm",
  "ADSOCA_harm"
)

################################################################################
# NOTE ON INTERPRETATION
################################################################################
#
# Even with harmonization, you may still see SOME shift in 2017 because:
#
# 1. The VR-12 added "Yes/No" prefixes to response options
#    - Pre-2017: "All of the time" / "None of the time"
#    - Post-2017: "Yes, all of the time" / "No, none of the time"
#    This can affect how people respond even with the same scale points
#
# 2. The collapsing of 6→5 categories is imperfect
#    - "A good bit of the time" falls between "Some" and "Most"
#    - Our conservative approach assigns it to "Some"
#    - This will systematically lower post-2017 means slightly
#
# 3. Context effects from surrounding questions may have changed
#
# For your analysis, consider:
# - Running sensitivity analyses with both pre-2017 and post-2017 periods separately
# - Including a 2017+ indicator variable in regression models
# - Testing both "conservative" (good bit → some) and "liberal" (good bit → most) 
#   collapsing strategies
#
################################################################################
