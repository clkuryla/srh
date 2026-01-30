# BRFSS


library(tidyverse)
library(here)
source(here::here("R/paths.R")) # To access path for data as depot_path
ensure_dirs()

# BRFSS 

library(haven)
library(tidyverse)

health_days_col <- c("_STSTR", "_PSU",  "IDATE" , "IYEAR"  , "GENHLTH",  "PHYSHLTH", "MENTHLTH", "POORHLTH", "_AGEG5YR", 
                     # these are not in all years:
                     "_AGE80", "AGE", "_AGE", "_IMPAGE", "_LLCPWT", "_FINALWT", "_STRWT", "_FINALWT2")

# intersect col 1993 to 2023
all_year_col <- c( "_STATE",   "_STSTR",   "_PSU",     "IDATE" , "IYEAR"  ,  "DISPCODE" , "GENHLTH",  "PHYSHLTH", "MENTHLTH", "POORHLTH", "SEATBELT", "CHILDREN", "SMOKE100",
                   "MARITAL" ,# "EDUCA"  , 
                   "PREGNANT", "_AGEG5YR", # "EXEROFT1" "EXERHMM1" "EXEROFT2", "EXERHMM2",
                   # these are not in all years:
                   "_AGE80", "AGE", "_AGE", "_IMPAGE", "_LLCPWT", "_MMSA", "_MMSAWT", "_MMSANAM", "ADJMMSA", "EXERANY2", "DECIDE", "DIFFALON", "DIFFWALK", "EMTSUPRT",
                   # "_LLCPWT", "_PSU", 
                   "_STSTR",
                   "_FINALWT", "_STRWT", "_FINALWT2", "_RAWRAKE", "_WT2RAKE", "_DUALWT", "_LANDWT", "_CELLWT", 
                   "_IMPRACE", "_CHISPNC", "_RACE", "_RACEG21", "_RACEGR3"
)

# Need to recode and filter AND RENAME to match: diabetes, highbp, prediabetes
# Need to just rename and filter to match: sex, depression, asthma, copd, cancer, kidney, checkup, angina/chd, mi, stroke

# EDUCA
# 1984-1992: 1=Eighth grade or less  2=Some high school  3=High school graduate or GED Certificate  4=Some technical school  5=Technical school graduate  6=Some college  7=College graduate  8=Postgraduate or professional degree  9=Refused
# 1993-2023: 1=Never attended school or kindergarten only  2=Grades 1 through 8 (Elementary)  3=Grades 9 through 11 (Some high school)  4=Grade 12 or GED (High school graduate)  5=College 1 year to 3 years (Some college or technical school)  6=College 4 years or more

more_col <- c(
  # Sex
  "SEX", # 1=Male  2=Female
  "SEX1", # 1=Male 2=Female 7=Don‚Äôt know/Not Sure 9=Refused
  "BIRTHSEX", # 1=Male 2=Female 7=Don‚Äôt know/Not Sure 9=Refused
  "_SEX", #unsure
  # (Ever told) you that you have a depressive disorder, including depression,
  # major depression, dysthymia, or minor depression?
  "ADDEPEV", # 1=Yes 2=No 7=DK/Not sure 9=Refused
  "ADDEPEV2", # same
  "ADDEPEV3", # same
  # Ever told you have asthma?
  "ASTHMA", # # 1=Yes  2=No  7=DK/NS  9=Refused
  "ASTHMA2", # same
  "ASTHMA3", # same
  # Do you still have asthma?
  "ASTHNOW",  # Do you still have asthma?
  # consistent coding: 1=Yes  2=No  7=DK/NS  9=Refused
  # Ever told you have diabetes?
  "DIABETES", # 1=Yes  2=No  7=DK/Ns  9=Refused
  "DIABETE2", # 1=Yes  2=Yes, but female told only during pregnancy  
  # 3=No  4=No, pre-diabetes or borderline diabetes  7=DK/NS  9=Refused
  "DIABETE3", # 1=Yes 2=Yes, but female told only during pregnancy 
  # 3=No 4=No, pre-diabetes or borderline diabetes 7=DK/NS 9=Refused
  "DIABETE4", # 1=Yes 2=Yes, but female told only during pregnancy 
  # 3=No 4=No, pre-diabetes or borderline diabetes 7=Don‚Äôt know/Not Sure 9=Refused
  # Ever told high blood pressures / htn?
  "BPHIGH6", # 1=Yes 2=Yes, but female told only during pregnancy 
  # 3=No 4=Told borderline high or pre-hypertensive or elevated blood pressure 
  #7=Don¬¥t know/Not Sure 9=Refused
  "BPHIGH4", # same as BPHIGH6
  "BPHIGH3", # same as BPHIGH6
  "BPHIGH2", # 1=Yes  2=No  7=DK/NS  9=Refused
  "BPHIGH", # 1=No  2=Yes, by a doctor  3=Yes, by a nurse  
  # 4=Yes, by other health professional  7=DK/NS  9=Refused
  # COPD, cancer, kidney disease (not as many years) --- all are 1 = yes, 2 = no, 7 = dk, 9=idk
  "CHCCOPD", "CHCCOPD1", "CHCCOPD2", "CHCCOPD3", # COPD: 1=Yes 2=No 7=DK / Not sure 9=Refused
  "CHCKDNY1", "CHCKDNY2", "CHCKIDNY", # chronic kidney disease: 1=Yes 2=No 7=DK / Not sure 9=Refused
  "CHCOCNC1", # (Ever told) (you had) melanoma or any other types of cancer?
  "CHCOCNCR", # (Ever told) you had any other types of cancer?
  "CHCSCNC1", # (Ever told) (you had) skin cancer that is not melanoma?
  "CHCSCNCR", # (Ever told) you had skin cancer?
  # How long since visiting a healthcare professional for a routine checkup?
  "CHECKUP", # 1=Within past year (anytime less than 12 months ago)  2=Within past 2 years (1 year but
  # less than 2 years ago)  3=Within past 5 years (2 years but less than 5 years ago)   
  # 4=5 or more years ago  7=DK/NS  8=Never  9=Refused
  "CHECKUP1", # same as CHECKUP
  # Has a doctor ever told you that you had  angina or coronary heart disease?
  "CVDCORHD", # 1=Yes   2=No   7=DK/NS   9=Refused
  "CVDCRHD2", # same as "CVDCORHD"
  "CVDCRHD3", # same as "CVDCORHD"
  "CVDCRHD4", # same as "CVDCORHD"
  # Ever told you had a heart attack/MI?
  "CVDINFAR", # 1=Yes   2=No   7=DK/NS   9=Refused
  "CVDINFR2",
  "CVDINFR3",
  "CVDINFR4",
  "CVDINFRS",
  # Ever told you had a stroke?
  "CVDSTRK2", # 1=Yes   2=No   7=DK/NS   9=Refused
  "CVDSTRK3",
  "CVDSTROK",
  # In general, how satisfied are you with your life?
  "LSATISFY", # 1=Very satisfied  2=Satisfied  3=Dissatisfied  4=Very satisfied  7=DK/NS  9=Refused
  # Ever told arthritis?
  "HAVARTH", # 1=Yes  2=No  7=DK/NS  9=Refused
  "HAVARTH2",
  "HAVARTH3",
  "HAVARTH4",
  "HAVARTH5",
  # Ever told prediabetes or borderline diabetes?
  "PREDIAB", # 1=Yes  2=No  7=DK/NS  9=Refused
  "PREDIAB1", # 1=Yes 2=Yes, during pregnancy 3=No 7=Don‚Äôt know/Not Sure 9=Refused
  "PREDIAB2", # 1=Yes 2=Yes, during pregnancy 3=No 7=Don‚Äôt know/Not Sure 9=Refused
  # Do you now have any health problem that requires you to use special equipment, such as a cane, a wheelchair, a special bed, or a special telephone?
  "USEEQUIP", # 1=Yes  2=No  7=DK/NS  9=Refused
  # Education (see above)
  "EDUCA",
  # Hispanic or latino?
  "HISPANIC", # 1=Yes  2=No  7=DK/NS  9=Refused
  "HISPANC2", # same as HISPANIC
  "HISPANC3", # 1=Mexican, Mexican American, Chicano/a 2=Puerto Rican 
  # 3=Cuban 4=Another Hispanic, Latino/a, or Spanish origin 5=No 
  # 7=Don‚Äôt know/Not Sure 12-4321=Multiple responses 9=Refused HIDDEN=Data not displayed
  # Annual household income from all sources
  "INCOME", # 1=Less than $10,000  2=$10,000 to less than $15,000  3=$15,000 to less than $20,000  4=$20,000 to less than $25,000  5=$25,000 to less than $35,000  6=$35,000 to $50,000  7=Over $50,000  8=DK/NS  9=Refused
  "INCOME2", # 04=Less than $25,000   ($20,000 to less than $25,000)   03=Less than $20,000   ($15,000 to less than $20,000)   02=Less than $15,000   ($10,000 to less than $15,000)   01=Less than $10,000   05=Less than $35,000   ($25,000 to less than $35,000)   06=Less
  "INCOME3", # 1=Less than $10,000 2=Less than $15,000 ($10,000 to < $15,000) 3=Less than $20,000 ($15,000 to < $20,000) 4=Less than $25,000 ($20,000 to < $25,000) 5=Less than $35,000 ($25,000 to < $35,000) 6=Less than $50,000 ($35,000 to < $50,000) 7=Less than $75,000 ($50,000 to < $75,000) 8=Less than $100,000? ($75,000 to < $100,000) 9=Less than $150,000? ($100,000 to < $150,000)? 10=Less than $200,000? ($150,000 to < $200,000) 11=$200,000 or more 77=Don‚Äôt know/Not sure 99=Refused
  "INCOME95", # 04=Less than $25,000   ($20,000 to less than $25,000)   03=Less than $20,000   ($15,000 to less than $20,000)   02=Less than $15,000   ($10,000 to less than $15,000)   01=Less than $10,000   05=Less than $35,000   ($25,000 to less than $35,000)   06=Less
  # Race
  "MRACE", # 1=White   2=Black or African American   3=Asian   4=Native Hawaiian or Other Pacific Islander   5=American Indian, Alaska Native   6=Other: [specify]__________   8=No additional choices  7=DK/NS   9=Refused
  "MRACE1", # 10=White 20=Black or African American 30=American Indian or Alaska Native 40=Asian 41=Asian Indian 42=Chinese 43=Filipino 44=Japanese 45=Korean 46=Vietnamese 47=Other Asian 50=Pacific Islander 51=Native Hawaiian 52=Guamanian or Chamorro 53=Samoan 54=Other
  "MRACE2", # 10=White 20=Black or African American 30=American Indian or Alaska Native 40=Asian 41=Asian Indian 42=Chinese 43=Filipino 44=Japanese 45=Korean 46=Vietnamese 47=Other Asian 50=Pacific Islander 51=Native Hawaiian 52=Guamanian or Chamorro 53=Samoan 54=Other Pacific Islander 1020-605453525150474645444342414088=Multiple responses 88=No choices 77=Don‚Äôt know/Not Sure 99=Refused HIDDEN=Data not displayed
  # Race
  "ORACE", # 1=White  2=Black  3=Asian, Pacific Islander  4=American Indian, Alaska Native  5=Other: (specify)__________  7=DK/NS  9=Refused
  "ORACE2", # 1=White  2=Black or African American  3=Asian  4=Native Hawaiian or Other Pacific Islander  5=American Indian, Alaska Native  6=Other: [specify]__________  7=DK/NS  9=Refused
  "ORACE3", # 10=White 20=Black or African American 30=American Indian or Alaska Native 40=Asian 41=Asian Indian 42=Chinese 43=Filipino 44=Japanese 45=Korean 46=Vietnamese 47=Other Asian 50=Pacific Islander 51=Native Hawaiian 52=Guamanian or Chamorro 53=Samoan 54=Other Pacific Islander 60=Other 77=Don¬¥t know/Not Sure 99=Refused
  "ORACE4", # 10=White 20=Black or African American 30=American Indian or Alaska Native 40=Asian 41=Asian Indian 42=Chinese 43=Filipino 44=Japanese 45=Korean 46=Vietnamese 47=Other Asian 50=Pacific Islander 51=Native Hawaiian 52=Guamanian or Chamorro 53=Samoan 54=Other Pacific Islander 77=Don¬¥t know/Not Sure 99=Refused
  # Stops before 2020: During the past 30 days, for about how many days did pain make it hard for you to do your usual activities, such as self-care, work, or recreation?
  "PAINACTV", # _ _=Number of days  8 8=None  7 7=DK/NS  9 9=Refused
  "PAINACT2", # _ _ =Number of days  8 8=None  7 7=DK/NS  9 9=Refused
  # What county do you live in?
  "CTYCODE", # _ _ _=FIPS county code  7 7 7=DK/NS  9 9 9=Refused
  "CTYCODE1", # 001-776=ANSI county code (formerly FIPS code) 
  # 888=County from another state (cell phone data only) 
  # 777=DK/Not sure 999=Refused HIDDEN=Data not displayed
  "CTYCODE2", # 001-776=ANSI county code (formerly FIPS code) 777=Don‚Äôt know/Not sure 
  # 778-887=ANSI county code (formerly FIPS code) 
  # 888=County from another state (cell phone data only) 999=Refused HIDDEN=Data not displayed
  # Zipcode
  "ZIPCODE", # _ _ _ _ _=ZIP Code  7 7 7 7 7= DK/NS  9 9 9 9 9= Refused
  "ZIPCODE1" # 01001-77776=Zipcode 77778-99950=Zipcode 77777=Don‚Äôt know/Not Sure 99999=Refused HIDDEN=Data not displayed
)

# file list
brfss_year_to_file <- read_csv(depot_path("surveys/BRFSS/BRFSS_year_file_key.csv"))

# data_brfss_1993 <- read_xpt(here("big_data/BRFSS/CDBRFS93.XPT"))

file_paths <- brfss_year_to_file %>% 
  filter(!(brfss_year %in% 1990:1992)) %>% 
  pull(file_name) 


# Not all variables are in every year
safe_select <- function(df, cols) {
  # 1. Identify which columns from cols are missing in df
  missing_cols <- setdiff(cols, names(df))
  # 2. Create those missing columns in df filled with NA
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA
  }
  # 3. Finally, select and return only the columns in col_list  (now we are assured they exist, even if NA)
  df %>% select(all_of(cols))
}

# columns of interest
col_list <- vctrs::vec_c(all_year_col, more_col)

# col_list <- vctrs::vec_c(health_days_col)

# create an empty list to store processed data frames
all_dfs <- vector("list", length = length(file_paths))

# get data from files
for (i in seq_along(file_paths)) {
  
  tmp_df <- read_xpt(paste0(depot_path("surveys/BRFSS/"), file_paths[i], ".XPT"))
  
  # Safely select our columns of interest (any columns not in tmp_df will be NA)
  tmp_df_selected <- safe_select(tmp_df, col_list)
  
  # Store processed data frame
  all_dfs[[i]] <- tmp_df_selected
}

data_brfss_raw <- bind_rows(all_dfs)

# write_csv(data_brfss_raw, derived_path("brfss_selected_not_recoded.csv"))


# data_brfss_raw <- read_csv(derived_path("brfss_selected_not_recoded.csv"))

################## Wrangle BRFSS data ##################
########################################################

### Load from file, filter, add age groupings, recode IYEAR

data_brfss <- data_brfss_raw %>%
  select(   -SEATBELT, -CHILDREN, -SMOKE100, -MARITAL, -PREGNANT,
            -CTYCODE, -CTYCODE1, -CTYCODE2, -ZIPCODE, -ZIPCODE1,
            # Keep HISPANIC, HISPANC2, HISPANC3 for wrangle_race.R
            -INCOME, -INCOME2, -INCOME3, -INCOME95,
            -`_FINALWT2`,
            -`_RAWRAKE`, -`_WT2RAKE`, -`_DUALWT`, -`_LANDWT`, -`_CELLWT`,
            # Keep _IMPRACE and _RACE for wrangle_race.R
            -`_CHISPNC`, -`_RACEG21`, -`_RACEGR3`,
            -`_MMSA`, -`_MMSAWT`, -`_MMSANAM`, -ADJMMSA, -EXERANY2
  )


data_brfss <- data_brfss %>% 
  mutate(
    wt = coalesce(`_LLCPWT`, `_FINALWT`),
    strata_year = interaction(IYEAR, `_STSTR`, drop = TRUE)
  ) %>% 
  mutate(psu = `_PSU`)

data_brfss <- data_brfss %>% 
  filter(GENHLTH %in% 1:5) %>% 
  mutate(srh = 6 - GENHLTH) %>% # recode for intuitive order
  mutate(srh_cat = factor(
    srh,
    levels = 1:5,
    labels = c(
      "Poor",
      "Fair",
      "Good",
      "Very Good",
      "Excellent"
    ))) 

  
data_brfss <- data_brfss %>% 
    filter(`_AGEG5YR` != 14) %>%  # 14 = unknown
  mutate(
    # 1) age_5yr_cat: ordered factor with intuitive labels
    age_5yr_cat = factor(
      `_AGEG5YR`,
      levels = 1:13,  # Must match all possible codes
      labels = c(
        "18-24",    # 1
        "25-29",    # 2
        "30-34",    # 3
        "35-39",    # 4
        "40-44",    # 5
        "45-49",    # 6
        "50-54",    # 7
        "55-59",    # 8
        "60-64",    # 9
        "65-69",    # 10
        "70-74",    # 11
        "75-79",    # 12
        "80+" # 13
      ),
      ordered = TRUE  # Make it an ordered factor
    ),
    
    # 2) age_5yr_num: a numeric version based on the midpoint of each age band
    #    (or NA for unknown/refused)
    age_5yr_num = case_when(
      `_AGEG5YR` == 1  ~ (18 + 24)/2,   # 21
      `_AGEG5YR` == 2  ~ (25 + 29)/2,   # 27
      `_AGEG5YR` == 3  ~ (30 + 34)/2,   # 32
      `_AGEG5YR` == 4  ~ (35 + 39)/2,   # 37
      `_AGEG5YR` == 5  ~ (40 + 44)/2,   # 42
      `_AGEG5YR` == 6  ~ (45 + 49)/2,   # 47
      `_AGEG5YR` == 7  ~ (50 + 54)/2,   # 52
      `_AGEG5YR` == 8  ~ (55 + 59)/2,   # 57
      `_AGEG5YR` == 9  ~ (60 + 64)/2,   # 62
      `_AGEG5YR` == 10 ~ (65 + 69)/2,   # 67
      `_AGEG5YR` == 11 ~ (70 + 74)/2,   # 72
      `_AGEG5YR` == 12 ~ (75 + 79)/2,   # 77
      `_AGEG5YR` == 13 ~ (80 + 98)/2,   # 89 (if your codebook upper bound is 98)
      `_AGEG5YR` == 14 ~ NA_real_       # Unknown
    ),
    
    # 3) age_decade_cat: ordered factor collapsing adjacent age_5yr codes 
    #    into ~10-year bands (plus "80+" and "Unknown")
    age_decade_cat = case_when(
      `_AGEG5YR` %in% c(1,2)   ~ "18-29",
      `_AGEG5YR` %in% c(3,4)   ~ "30-39",
      `_AGEG5YR` %in% c(5,6)   ~ "40-49",
      `_AGEG5YR` %in% c(7,8)   ~ "50-59",
      `_AGEG5YR` %in% c(9,10)  ~ "60-69",
      `_AGEG5YR` %in% c(11,12) ~ "70-79",
      `_AGEG5YR` == 13         ~ "80+",
      `_AGEG5YR` == 14         ~ NA
    )) %>% 
  mutate(
    # Convert the decade-level variable to an ordered factor
    age_decade_cat = factor(
      age_decade_cat,
      levels = c("18-29", "30-39", "40-49", "50-59", 
                 "60-69", "70-79", "80+", "Unknown"),
      ordered = TRUE
    )
  )  
  
data_brfss <- data_brfss %>%
  mutate(IYEAR = as.integer(IYEAR)) %>% 
  mutate(year = case_when(
    IYEAR == 93 ~ 1993,
    IYEAR == 94 ~ 1994,
    IYEAR == 95 ~ 1995,
    IYEAR == 96 ~ 1996,
    IYEAR == 97 ~ 1997,
    IYEAR == 98 ~ 1998,
    TRUE ~ IYEAR
  ))

# # ---- Sanity checks ----
# # 1) Check that all categories are present and coded as expected:
# table(data_brfss$`_AGEG5YR`, useNA = "always")
# table(data_brfss$age_5yr_cat, useNA = "always")
# table(data_brfss$age_decade_cat, useNA = "always")
# 
# # 2) Brief check of numeric distribution:
# summary(data_brfss$age_5yr_num)
# 
# # 3) Look at a cross-tab to confirm consistency across new variables:
# with(data_brfss, table(age_5yr_cat, age_decade_cat))```


### Healthy days variables


library(dplyr)

# Helper: recode BRFSS Healthy Days responses to "bad day" counts (0–30, NA for 77/99/other)
recode_bad_days <- function(x) {
  case_when(
    x == 88 ~ 0,                                     # "None" -> 0 bad days
    dplyr::between(x, 1L, 30L) ~ as.numeric(x),      # 1–30 days
    x %in% c(77, 99) ~ NA_real_,                     # DK/Refused -> NA
    TRUE ~ NA_real_                                   # Any other unexpected value -> NA
  )
}

data_brfss <- data_brfss %>%
  #df <- df %>% 
  # 1) Recode to bad-day counts first (0–30)
  mutate(
    ment_bad = recode_bad_days(MENTHLTH),
    phys_bad = recode_bad_days(PHYSHLTH),
    
    # Activity limitation bad days with skip rule:
    # If POORHLTH is missing *because* both ment & phys are 88 ("none"), impute 0
    activlim_bad = case_when(
      is.na(POORHLTH) & MENTHLTH == 88 & PHYSHLTH == 88 ~ 0,
      TRUE ~ recode_bad_days(POORHLTH)
    )
  ) %>%
  
  # 2) Compute good-day counts (30 - bad), preserving NA
  mutate(
    mental_health_good_days          = if_else(!is.na(ment_bad), 30 - ment_bad, NA_real_),
    physical_health_good_days        = if_else(!is.na(phys_bad), 30 - phys_bad, NA_real_),
    usual_activities_health_good_days = if_else(!is.na(activlim_bad), 30 - activlim_bad, NA_real_)
  ) %>%
  
  # 3) CDC-consistent categories (≥14 = frequent distress)
  mutate(
    mental_health_cat = case_when(
      is.na(ment_bad) ~ NA_character_,
      ment_bad == 0   ~ "0 days (none)",
      ment_bad < 14   ~ "1–13 days",
      ment_bad >= 14  ~ "14–30 days (frequent mental distress)"
    ) |> factor(levels = c("0 days (none)", "1–13 days", "14–30 days (frequent mental distress)")),
    
    physical_health_cat = case_when(
      is.na(phys_bad) ~ NA_character_,
      phys_bad == 0   ~ "0 days (none)",
      phys_bad < 14   ~ "1–13 days",
      phys_bad >= 14  ~ "14–30 days (frequent physical distress)"
    ) |> factor(levels = c("0 days (none)", "1–13 days", "14–30 days (frequent physical distress)")),
    
    usual_activities_health_cat = case_when(
      is.na(activlim_bad) ~ NA_character_,
      activlim_bad == 0   ~ "0 days (none)",
      activlim_bad < 14   ~ "1–13 days",
      activlim_bad >= 14  ~ "14–30 days"
    ) |> factor(levels = c("0 days (none)", "1–13 days", "14–30 days"))
  ) %>%
  
  # 4) CDC Healthy Days summary: cap at 30; elementwise min
  mutate(
    unhealthy_days = pmin(ment_bad + phys_bad, 30L),
    healthy_days   = if_else(!is.na(unhealthy_days), 30 - unhealthy_days, NA_real_)
  ) %>% 
  
  # 5) category of poor heath days
  mutate(physical_or_mental_poor_days = case_when(
    is.na(ment_bad) | is.na(phys_bad) ~ NA_character_,
    ment_bad > 0 & phys_bad == 0 ~ "mental_health_only",
    ment_bad == 0 & phys_bad > 0 ~ "physical_health_only",
    ment_bad > 0 & phys_bad > 0 ~ "both",
    ment_bad == 0 & phys_bad == 0 ~ "neither"
  )) 




# # Old: If want mental, physical, and activities health recoded
# 
# data_brfss <- data_brfss %>% 
#   mutate(mental_health_good_days = case_when( # Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good?
#     MENTHLTH == 88 ~ 30,  # 88 = No bad days
#     MENTHLTH <= 30 ~ 30 - MENTHLTH, # MENTHLTH is number of bad mental health days in past 30 days
#     MENTHLTH == 77 ~ NA_real_, # 77 = Don't Know / Not Sure
#     MENTHLTH == 99 ~ NA_real_, # 99 = Refused
#     TRUE ~ NA_real_
#   )) %>% 
# mutate(mental_health =  case_when(
#   MENTHLTH == 88 ~ 3,
#   MENTHLTH >= 1 & MENTHLTH <= 14 ~ 2,
#   MENTHLTH > 14 & MENTHLTH <= 30 ~ 1,
#   MENTHLTH == 99 ~ NA,
#   TRUE ~ NA)) %>% 
# mutate(physical_health_good_days = case_when( # Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good?
#     PHYSHLTH == 88 ~ 30,  # 88 = No bad days
#     PHYSHLTH <= 30 ~ 30 - PHYSHLTH, # PHYSHLTH is number of bad physical health days in past 30 days
#     PHYSHLTH == 77 ~ NA_real_, # 77 = Don't Know / Not Sure
#     PHYSHLTH == 99 ~ NA_real_, # 99 = Refused
#     TRUE ~ NA_real_
#   )) %>% 
# mutate(physical_health =  case_when(
#   PHYSHLTH == 88 ~ 3,
#   PHYSHLTH >= 1 & PHYSHLTH <= 14 ~ 2,
#   PHYSHLTH > 14 & PHYSHLTH <= 30 ~ 1,
#   PHYSHLTH == 99 ~ NA,
#   TRUE ~ NA)) %>%
# mutate(usual_activities_health_good_days = case_when( #During the past 30 days, for about how many days did poor physical or mental health keep you from doing your usual activities, such as self-care, work, or recreation?
#     POORHLTH == 88 ~ 30,  # 88 = No bad days
#     PHYSHLTH == 88 & MENTHLTH == 88 ~ 30, # skip pattern: question not asked if participant responded "no bad days" to both physical and mental health, which means they had no bad days of health of either type (so no days where health prevented function)
#     POORHLTH <= 30 ~ 30 - POORHLTH, # POORHLTH is number of days health prevented notmal functioning
#     POORHLTH == 77 ~ NA_real_, # 77 = Don't Know / Not Sure
#     POORHLTH == 99 ~ NA_real_, # 99 = Refused
#     TRUE ~ NA
#   )) %>% 
# mutate(usual_activities_health =  case_when(
#   POORHLTH == 88 ~ 3,
#   POORHLTH >= 1 & POORHLTH <= 14 ~ 2,
#   POORHLTH > 14 & POORHLTH <= 30 ~ 1,
#   POORHLTH == 99 ~ NA,
#   TRUE ~ NA)) %>% 
# mutate(unhealthy_days = min(PHYSHLTH + MENTHLTH, 30)) %>%  # CDC recommendation/convention (https://archive.cdc.gov/www_cdc_gov/hrqol/methods.htm)
# mutate(healthy_days = 30 - unhealthy_days)  # CDC recommendationconvention
# %>% 
# filter(SEX != 9) %>% 
# mutate(sex = case_when(
#   year <= 2018 ~ SEX,
#   year >= 2019 ~ `_SEX`,
#   TRUE ~ NA)) %>% 
# filter(EDUCA != 9)

# 

# with(data_brfss, table(MENTHLTH, mental_health))
# with(data_brfss, table(PHYSHLTH, physical_health))
# with(data_brfss, table(POORHLTH, usual_activities_health))

#  select(!(MENTHLTH %in% c(77, 88, 99))) %>% 
#  mutate(mental_health_good_days = 31 - MENTHLTH) # MENTLHLTH is number of bad mental health days in month 




### Recode and harmonize other variables


data_brfss <- data_brfss %>%
  # Keep ORACE for wrangle_race.R (needed for 1993-2000)
  select(-c(MRACE, MRACE1, MRACE2, ORACE2, ORACE3, ORACE4))

# Recode



# glimpse(data_brfss)

library(dplyr)
library(rlang)
library(stringr)

# ---------- Small helpers ----------

# Unify across candidate columns, keep NAs, record source name; never drops rows.
unify_one <- function(df, new_name, candidates, keep_source = TRUE, drop_sources = FALSE) {
  present <- intersect(names(df), candidates)
  if (length(present) == 0L) {
    df[[new_name]] <- NA
    if (keep_source) df[[paste0(new_name, "_source")]] <- NA_character_
    warning(sprintf("No candidates present for '%s': %s",
                    new_name, paste(candidates, collapse = ", ")))
    return(df)
  }
  vals <- df[present]
  df[[new_name]] <- dplyr::coalesce(!!!vals)
  
  if (keep_source) {
    nonmiss <- !is.na(as.matrix(vals))
    nn <- rowSums(nonmiss)
    idx <- max.col(nonmiss, ties.method = "first")
    src <- rep(NA_character_, nrow(df))
    if (length(present) == 1L) {
      src[nn > 0] <- present
    } else {
      src[nn > 0] <- present[idx[nn > 0]]
    }
    df[[paste0(new_name, "_source")]] <- src
    if (any(nn > 1L)) {
      warning(sprintf(
        "%s: %s rows have >1 non-missing among candidates (%s). Check merges.",
        new_name, sum(nn > 1L), paste(present, collapse = ", ")
      ))
    }
  }
  if (drop_sources) df[present] <- NULL
  df
}

to_int <- function(x) suppressWarnings(as.integer(as.character(x)))
to_chr <- function(x) as.character(x)

# A generic "1=yes, 2=no, 7/9=NA" recoder for convenience
yn_1yes_2no <- function(x) case_when(
  to_int(x) == 1L ~ 1L,
  to_int(x) == 2L ~ 0L,
  TRUE            ~ NA_integer_
)

# ---------- Now, variable-by-variable ----------

df <- data_brfss

# 1) SEX (SEX, SEX1, BIRTHSEX) -> sex (1=Male, 2=Female)
#df <- unify_one(df, "sex_raw", c("SEX","SEX1","BIRTHSEX", "_SEX"))
df <- unify_one(df, "sex_raw", c("SEX","SEX1", "_SEX"))
df <- df %>% mutate(
  sex = case_when(
    to_int(sex_raw) == 1 ~ "Male",
    to_int(sex_raw) == 2 ~ "Female",
    TRUE ~ NA
  )
)

# table(df$sex_raw, useNA = "always")
# table(df$sex, useNA = "always")
# with(df, table(sex, year, useNA = "always"))



# 2) Depressive disorder ever (ADDEPEV, ADDEPEV2, ADDEPEV3) -> dep_dx (1/0)
df <- unify_one(df, "dep_raw", c("ADDEPEV","ADDEPEV2","ADDEPEV3"))
df <- df %>% mutate(dep_dx = yn_1yes_2no(dep_raw))
df <- df %>% mutate(dep_dx = if_else(!(year %in% c(2007, 2009)),
                                     dep_dx,
                                     NA))

# with(df, table(dep_dx, year, useNA = "always"))

# 3) Asthma ever (ASTHMA, ASTHMA2, ASTHMA3) and current (ASTHNOW)
df <- unify_one(df, "asthma_ever_raw", c("ASTHMA","ASTHMA2","ASTHMA3"))
df <- df %>% mutate(asthma_ever = yn_1yes_2no(asthma_ever_raw))
df <- df %>% mutate(asthma_ever = if_else(!(year %in% c(1993, 1994)),
                                          asthma_ever,
                                          NA))

# with(df, table(asthma_ever, year, useNA = "always"))

df <- unify_one(df, "asthma_now_raw", c("ASTHNOW"))
df <- df %>% mutate(asthma_current = yn_1yes_2no(asthma_now_raw))

# with(df, table(asthma_current, year, useNA = "always"))

# people with asthma and without asthma now
df <- df %>% mutate(asthma = case_when(
  asthma_ever == 0 ~ 0L,
  asthma_ever == 1 & asthma_current == 0 ~ 0L,
  asthma_ever == 1 & asthma_current == 1 ~ 1L,
  TRUE ~ NA_integer_
))

# with(df, table(asthma, year, useNA = "always"))

# 4) Diabetes (DIABETES, DIABETE2/3/4) -> diabetes_dx (1/0),
#    plus flags for pregnancy-only and prediabetes/borderline.
df <- unify_one(df, "diab_raw", c("DIABETES","DIABETE2","DIABETE3","DIABETE4"), keep_source = TRUE)
df <- df %>% mutate(
  diabetes_dx = case_when(
    diab_raw_source %in% c("DIABETES") & to_int(diab_raw) == 1L ~ 1L, # yes
    diab_raw_source %in% c("DIABETES") & year %in% c(1988:1993, 2004:2024) & to_int(diab_raw) == 2 ~ 0L,
    diab_raw_source %in% c("DIABETES") & year %in% c(1994:2003) & to_int(diab_raw) == 3 ~ 0L,
    
    diab_raw_source %in% c("DIABETE2","DIABETE3","DIABETE4") & to_int(diab_raw) == 1L ~ 1L,
    diab_raw_source %in% c("DIABETE2","DIABETE3","DIABETE4") & to_int(diab_raw) == 3L ~ 0L,
    
    # pregnancy-only and prediabetes categories are handled as separate flags
    diab_raw_source %in% c("DIABETE2","DIABETE3","DIABETE4") & to_int(diab_raw) %in% c(2L,4L) ~ 0L,
    TRUE ~ NA_integer_
  ),
  diabetes_pregnancy_only = case_when(
    diab_raw_source %in% c("DIABETE2","DIABETE3","DIABETE4") & to_int(diab_raw) == 2L ~ 1L,
    TRUE ~ 0L
  ),
  prediabetes_from_diabvar = case_when(
    diab_raw_source %in% c("DIABETE2","DIABETE3","DIABETE4") & to_int(diab_raw) == 4L ~ 1L,
    TRUE ~ 0L
  )
)

# table((df %>% filter(year == 1999))$DIABETES)
# with(df, table(diabetes_dx, year, useNA = "always"))

# 5) High blood pressure (BPHIGH variants)
# Create:
#   htn_dx: 1 yes, 0 no
#   htn_pregnancy_only: 1 if yes-only-during-pregnancy
#   htn_borderline: 1 if told borderline/pre-hypertensive
df <- unify_one(df, "bp_raw", c("BPHIGH","BPHIGH2","BPHIGH3","BPHIGH4","BPHIGH6"), keep_source = TRUE)
df <- df %>% mutate(
  htn_dx = case_when(
    bp_raw_source %in% c("BPHIGH2") & to_int(bp_raw) == 1L ~ 1L, # 1=yes
    bp_raw_source %in% c("BPHIGH2") & to_int(bp_raw) == 2L ~ 0L, # 2=no
    
    bp_raw_source %in% c("BPHIGH3","BPHIGH4","BPHIGH6") & to_int(bp_raw) == 1L ~ 1L, # 1=yes
    bp_raw_source %in% c("BPHIGH3","BPHIGH4","BPHIGH6") & to_int(bp_raw) == 3L ~ 0L, # 3=no
    
    bp_raw_source %in% c("BPHIGH") & year %in% c(1993:2000) & to_int(bp_raw) == 1L ~ 1L, # yes 
    bp_raw_source %in% c("BPHIGH") & year %in% c(1993:2000) & to_int(bp_raw) == 2L ~ 0L, # no
    bp_raw_source %in% c("BPHIGH") & year %in% c(1984:1992) & to_int(bp_raw) %in% c(2L,3L,4L) ~ 1L, # yes by pro
    bp_raw_source %in% c("BPHIGH") & year %in% c(1984:1992) & to_int(bp_raw) == 1L ~ 0L, # no
    TRUE ~ NA_integer_
  ),
  htn_pregnancy_only = case_when(
    bp_raw_source %in% c("BPHIGH3","BPHIGH4","BPHIGH6") & to_int(bp_raw) == 2L ~ 1L,
    TRUE ~ 0L
  ),
  htn_borderline = case_when(
    bp_raw_source %in% c("BPHIGH3","BPHIGH4","BPHIGH6") & to_int(bp_raw) == 4L ~ 1L,
    TRUE ~ 0L
  )
)

#  with(df, table(htn_dx, year, useNA = "always"))


#  with(df, table(htn_dx, year, useNA = "always"))
# > with(df, table(htn_dx, year, useNA = "always"))
#       year
# htn_dx   1993   1994   1995   1996   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006
#   0     77927   9645  85030  23166 100005  22440 116722  12688 151266  28135 182687  33343 231083    623
#   1     23224   3086  26312   8020  32532   7712  39743   4691  55901  11992  75617  16559 111098    268
#   <NA>    397  92439   1932  92132   2041 118199   2247 164875   3076 204470   3999 250996   9665 349248
#       year
# htn_dx   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016   2017   2018   2019   2020
#   0    261378    941 248499    899 288186   1905 275641   3482 243591   6958 249122   6900 224663  10779
#   1    150899    402 165108    559 197449   1052 194151   2005 172120   3620 174023   4360 159583   7194
#   <NA>  13306 407542  12933 441353  14666 466109  13279 452601  13204 464831  21906 409550  26599 381302
#       year
# htn_dx   2021   2022   2023   2024   <NA>
#   0    236522  13801 228340  14470      0
#   1    159753   8603 162929   9870      0
#   <NA>  20736 410471  33306    498      0

# It seems like they only do HTN fully on odd numbered years, so I'll make the even NA for the main analysis to maintain a large sample size, but keep all in another variable

df <- df %>% mutate(htn_dx_all = htn_dx)
df <- df %>% mutate(htn_dx = if_else(year %in% c(1993, 1995, 1997, 1999, 2001, 
                                                 2003, 2005, 2007, 2009, 2011,
                                                 2013, 2015, 2017, 2019, 2021,
                                                 2023),
                                     htn_dx,
                                     NA))
#  with(df, table(htn_dx, year, useNA = "always"))

# 6) COPD (CHCCOPD*), CKD (CHCKDNY* / CHCKIDNY), Cancers (CHCOCNC1/CHCOCNCR, CHCSCNC1/CHCSCNCR)
df <- unify_one(df, "copd_raw", c("CHCCOPD","CHCCOPD1","CHCCOPD2","CHCCOPD3"))
df <- df %>% mutate(copd_dx = yn_1yes_2no(copd_raw))

# with(df, table(copd_dx, year, useNA = "always"))

df <- unify_one(df, "ckd_raw", c("CHCKDNY1","CHCKDNY2","CHCKIDNY"))
df <- df %>% mutate(ckd_dx = yn_1yes_2no(ckd_raw))
# with(df, table(ckd_dx, year, useNA = "always"))

df <- unify_one(df, "cancer_other_raw", c("CHCOCNC1","CHCOCNCR"))
df <- unify_one(df, "cancer_skin_raw",  c("CHCSCNC1","CHCSCNCR"))
df <- df %>% mutate(
  cancer_other_dx = yn_1yes_2no(cancer_other_raw),
  cancer_skin_dx  = yn_1yes_2no(cancer_skin_raw),
  cancer_any_dx   = case_when(
    cancer_other_dx == 1L | cancer_skin_dx == 1L ~ 1L,
    cancer_other_dx == 0L & cancer_skin_dx == 0L ~ 0L,
    TRUE ~ NA_integer_
  )
)

# with(df, table(cancer_other_dx, year, useNA = "always"))
# with(df, table(cancer_skin_dx, year, useNA = "always"))
# with(df, table(cancer_any_dx, year, useNA = "always"))

# 7) Routine checkup recode (CHECKUP, CHECKUP1)
# Keep an ordered category consistent with BRFSS wording; keep "never" separate.
df <- unify_one(df, "checkup_raw", c("CHECKUP","CHECKUP1"))
df <- df %>% mutate(
  checkup_cat = case_when(
    to_int(checkup_raw) %in% c(1L) ~ 1L,  # within past year
    to_int(checkup_raw) %in% c(2L) ~ 2L,  # within past 2 years
    to_int(checkup_raw) %in% c(3L) ~ 3L,  # within past 5 years
    to_int(checkup_raw) %in% c(4L) ~ 4L,  # 5+ years
    to_int(checkup_raw) %in% c(8L) ~ 8L,  # NEVER (separate code)
    TRUE ~ NA_integer_
  ),
  checkup_never = as.integer(checkup_cat == 8L)
)

# with(df, table(checkup_cat, year, useNA = "always"))
# with(df, table(checkup_never, year, useNA = "always"))


# 8) CHD / Angina (CVDCORHD, CVDCRHD2/3/4) -> chd_dx
df <- unify_one(df, "chd_raw", c("CVDCORHD","CVDCRHD2","CVDCRHD3","CVDCRHD4"))
df <- df %>% mutate(chd_dx = yn_1yes_2no(chd_raw))
# with(df, table(chd_dx, year, useNA = "always"))

# 9) MI (CVDINFAR, CVDINFR2/3/4, CVDINFRS) -> mi_dx
df <- unify_one(df, "mi_raw", c("CVDINFAR","CVDINFR2","CVDINFR3","CVDINFR4","CVDINFRS"))
df <- df %>% mutate(mi_dx = yn_1yes_2no(mi_raw))
# with(df, table(mi_dx, year, useNA = "always"))

# 10) Stroke (CVDSTRK2/3, CVDSTROK) -> stroke_dx
df <- unify_one(df, "stroke_raw", c("CVDSTRK2","CVDSTRK3","CVDSTROK"))
df <- df %>% mutate(stroke_dx = yn_1yes_2no(stroke_raw))
# with(df, table(stroke_dx, year, useNA = "always"))

# 11) Life satisfaction (LSATISFY) -> life_satisfaction (1 very satisfied ... 4 very dissatisfied)
# NOTE: codebook says "4=Very satisfied" which duplicates '1'. In BRFSS it's usually:
#  1=Very satisfied 2=Satisfied 3=Dissatisfied 4=Very dissatisfied (77/99 missing).
# We assume that here and then reverse code
df <- df %>% mutate(
  life_satisfaction = case_when(
    to_int(LSATISFY) %in% 1:4 ~ to_int(LSATISFY),
    TRUE ~ NA_integer_
  ) 
) %>% 
  mutate(life_satisfaction = 5 - life_satisfaction)
table(df$life_satisfaction, useNA = "always")
# with(df, table(life_satisfaction, year, useNA = "always"))

# 12) Arthritis (HAVARTH*) -> arthritis_dx
df <- unify_one(df, "arth_raw", c("HAVARTH","HAVARTH2","HAVARTH3","HAVARTH4","HAVARTH5"))
df <- df %>% mutate(arthritis_dx = yn_1yes_2no(arth_raw))
# with(df, table(arthritis_dx, year, useNA = "always"))
# with(df, table(arth_raw, year, useNA = "always"))

# this does not match prevalence data:
# > with(df, table(arthritis_dx, year, useNA = "always"))
#             year
# arthritis_dx   1993   1994   1995   1996   1997   1998   1999   2000   2001   2002   2003   2004   2005
#         0         0      0      0   8938   7652   7178  15887  88711 155942  88868 175017  33312 227714
#         1         0      0      0   3280   2340   2271   5834  29332  51676  40498  81622  17715 117945
#         <NA> 101548 105170 113274 111100 124586 138902 136991  64211   2625 115231   5664 249871   6187
#             year
# arthritis_dx   2006   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016   2017   2018
#         0       595 262784    896 258928  20541 326953 306253 317719 296089 281698 307224 296402 274471
#         1       289 152394    423 151438  15772 165375 160431 162581 159706 144644 165886 146025 144094
#         <NA> 349255  10405 407566  16174 406498   7973   2382   2771   2293   2573   2299   2624   2245
#             year
# arthritis_dx   2019   2020   2021   2022   2023   2024   <NA>
#         0    270647 273098 278025 283817 281762  16791      0
#         1    137908 124114 136542 146730 140533   7920      0
#         <NA>   2290   2063   2444   2328   2280    127      0

# 13) Prediabetes (PREDIAB, PREDIAB1/2) -> prediab_dx
#  - PREDIAB = 1 yes, 2 no
#  - PREDIAB1/2 = 1 yes, 2 yes during pregnancy, 3 no
df <- unify_one(df, "predi_raw", c("PREDIAB","PREDIAB1","PREDIAB2"), keep_source = TRUE)
df <- df %>% mutate(
  prediab_dx = case_when(
    predi_raw_source == "PREDIAB"  & to_int(predi_raw) == 1L ~ 1L,
    predi_raw_source == "PREDIAB"  & to_int(predi_raw) == 2L ~ 0L,
    predi_raw_source %in% c("PREDIAB1","PREDIAB2") & to_int(predi_raw) == 1L ~ 1L,
    predi_raw_source %in% c("PREDIAB1","PREDIAB2") & to_int(predi_raw) == 3L ~ 0L,
    TRUE ~ NA_integer_
  ),
  prediab_pregnancy_only = case_when(
    predi_raw_source %in% c("PREDIAB1","PREDIAB2") & to_int(predi_raw) == 2L ~ 1L,
    TRUE ~ 0L
  ),
  # merge with DIABETE* info if desired:
  prediab_any = as.integer(coalesce(prediab_dx, prediabetes_from_diabvar, 0L) == 1L)
)

# with(df, table(prediab_any, year, useNA = "always"))

# 14) Difficulty walking or climbing stairs (DIFFWALK) -> diffwalk (1/0)
# DIFFWALK: Do you have serious difficulty walking or climbing stairs?
# 1=Yes, 2=No, 7=DK/NS, 9=Refused
# Available from 2013 onwards
df <- df %>% mutate(
  diffwalk = case_when(
    to_int(DIFFWALK) == 1L ~ 1L,
    to_int(DIFFWALK) == 2L ~ 0L,
    TRUE ~ NA_integer_
  )
)

# with(df, table(diffwalk, year, useNA = "always"))

# # 15) Special equipment (USEEQUIP) -> useequip (1/0)
# df <- unify_one(df, "useequip_raw", c("USEEQUIP"))
# df <- df %>% mutate(useequip = yn_1yes_2no(useequip_raw))
# 
# # 15) EDUCATION (EDUCA) -> educa6 (1..6 BRFSS 1993+ scale)
# # Codes:
# #   1993+  : 1 Never/Kindergarten, 2 Grades 1–8, 3 Grades 9–11,
# #            4 Grade 12/GED, 5 Some college/tech, 6 College 4+ years
# #   1984-92: 1 ≤8th, 2 Some HS, 3 HS/GED, 4 Some tech, 5 Tech grad,
# #            6 Some college, 7 College grad, 8 Postgrad, 9 Refused
# # Map 1984–92 into the 1..6 scale (lossy but standard):
# #   1 -> 2; 2 -> 3; 3 -> 4; 4/5/6 -> 5; 7/8 -> 6; 9 -> NA
# df <- unify_one(df, "educa_raw", c("EDUCA"), keep_source = TRUE)
# df <- df %>% mutate(
#   educa6 = case_when(
#     # 1993+ already on target scale
#     to_int(educa_raw) %in% 1:6 ~ to_int(educa_raw),
#     # 1984–92 mapping (applied when those codes appear in unified column)
#     to_int(educa_raw) == 1L ~ 2L,
#     to_int(educa_raw) == 2L ~ 3L,
#     to_int(educa_raw) == 3L ~ 4L,
#     to_int(educa_raw) %in% c(4L,5L,6L) ~ 5L,
#     to_int(educa_raw) %in% c(7L,8L) ~ 6L,
#     to_int(educa_raw) %in% c(9L) ~ NA_integer_,
#     TRUE ~ NA_integer_
#   )
# )
# # (Optional) labelled factor
# # df <- df %>% mutate(
# #   educa6_f = factor(educa6, levels = c(1,2,3,4,5,6),
# #          labels = c("Never/Kindergarten","Grades 1–8","Grades 9–11",
# #                     "HS/GED","Some college/tech","College 4+ years"), ordered = TRUE)
# # )
# 
# # 16) Hispanic origin (HISPANIC, HISPANC2, HISPANC3) -> hispanic (1 yes, 0 no)
# # HISPANIC/HISPANC2: 1 Yes, 2 No (7/9 NA)
# # HISPANC3: detailed origin (1..4 = specific Hispanic origins; 5=No)
# df <- unify_one(df, "hisp_raw", c("HISPANIC","HISPANC2","HISPANC3"), keep_source = TRUE)
# df <- df %>% mutate(
#   hispanic = case_when(
#     hisp_raw_source %in% c("HISPANIC","HISPANC2") & to_int(hisp_raw) == 1L ~ 1L,
#     hisp_raw_source %in% c("HISPANIC","HISPANC2") & to_int(hisp_raw) == 2L ~ 0L,
#     hisp_raw_source == "HISPANC3" & to_int(hisp_raw) %in% 1:4 ~ 1L,
#     hisp_raw_source == "HISPANC3" & to_int(hisp_raw) == 5L ~ 0L,
#     TRUE ~ NA_integer_
#   )
# )
# 
# # 17) Income (INCOME, INCOME2, INCOME3, INCOME95)
# # Goal: an ordinal "income_rank" increasing with income.
# # INCOME: 1..7 (ascending)
# # INCOME3: 1..11 (ascending)
# # INCOME2 / INCOME95 use two-digit codes; mapping differs by year. Without the full codebook here,
# # we set a cautious placeholder (NA) and recommend plugging the exact map.
# df <- unify_one(df, "inc_raw", c("INCOME","INCOME2","INCOME3","INCOME95"), keep_source = TRUE)
# df <- df %>% mutate(
#   income_rank = case_when(
#     inc_raw_source == "INCOME"  & to_int(inc_raw) %in% 1:7   ~ to_int(inc_raw),                # 1..7
#     inc_raw_source == "INCOME3" & to_int(inc_raw) %in% 1:11  ~ to_int(inc_raw),                 # 1..11
#     # TODO: replace these with your official crosswalk:
#     inc_raw_source %in% c("INCOME2","INCOME95") ~ NA_integer_,
#     TRUE ~ NA_integer_
#   )
# )
# # If you can share the INCOME2/95 codebook table, I’ll drop in the exact mapping.
# 
# # 18) Race (MRACE*, ORACE*)
# # Output: race5 (1 White, 2 Black, 3 AI/AN, 4 Asian, 5 NH/PI), 6 Other, 7 Multiple
# # MRACE (old): 1 White, 2 Black, 3 Asian, 4 NH/PI, 5 AI/AN, 6 Other
# df <- unify_one(df, "mrace_raw", c("MRACE","MRACE1","MRACE2"), keep_source = TRUE)
# df <- df %>% mutate(
#   race5 = case_when(
#     mrace_raw_source == "MRACE" & to_int(mrace_raw) %in% c(1,2,5,3,4,6) ~
#       recode(to_int(mrace_raw), `1`=1L, `2`=2L, `5`=3L, `3`=4L, `4`=5L, `6`=6L),
#     mrace_raw_source %in% c("MRACE1","MRACE2") ~ {
#       code <- to_int(mrace_raw)
#       case_when(
#         code %in% c(10L) ~ 1L,                      # White
#         code %in% c(20L) ~ 2L,                      # Black
#         code %in% c(30L) ~ 3L,                      # AI/AN
#         code %in% c(40L,41L,42L,43L,44L,45L,46L,47L) ~ 4L,  # Asian
#         code %in% c(50L,51L,52L,53L,54L) ~ 5L,      # NH/PI
#         # Multiple-response mega-codes (e.g., '1020...' or 1020, 6054..., 88, etc.)
#         code %in% c(88L) ~ 6L,                      # "No additional choices" / Other-ish
#         TRUE ~ 7L                                   # Treat non-atomic/multi as Multiple
#       )
#     },
#     TRUE ~ NA_integer_
#   )
# )
# 
# # ORACE variants (older single-race question). Use if MRACE* absent.
# df <- unify_one(df, "orace_raw", c("ORACE","ORACE2","ORACE3","ORACE4"), keep_source = TRUE)
# df <- df %>% mutate(
#   race5 = case_when(
#     !is.na(race5) ~ race5, # keep MRACE-based if present
#     orace_raw_source %in% c("ORACE","ORACE2") & to_int(orace_raw) %in% 1:6 ~
#       recode(to_int(orace_raw),
#              `1`=1L, `2`=2L, `5`=3L, `3`=5L, `4`=3L, .default = 6L),
#     orace_raw_source %in% c("ORACE3","ORACE4") ~ {
#       code <- to_int(orace_raw)
#       case_when(
#         code == 10L ~ 1L,  # White
#         code == 20L ~ 2L,  # Black
#         code == 30L ~ 3L,  # AI/AN
#         code %in% c(40L,41L,42L,43L,44L,45L,46L,47L) ~ 4L, # Asian
#         code %in% c(50L,51L,52L,53L,54L) ~ 5L, # NH/PI
#         code %in% c(60L) ~ 6L,  # Other
#         TRUE ~ 7L               # Multiple / non-atomic
#       )
#     },
#     TRUE ~ NA_integer_
#   )
# )
# 
# # 19) Pain days (PAINACTV, PAINACT2): 88 -> 0 days; 77/99 -> NA
# df <- unify_one(df, "pain_raw", c("PAINACTV","PAINACT2"))
# df <- df %>% mutate(
#   pain_days_30 = case_when(
#     to_int(pain_raw) %in% 0:30 ~ to_int(pain_raw),
#     to_int(pain_raw) == 88L   ~ 0L,
#     TRUE ~ NA_integer_
#   )
# )
# 
# # 20) County code (CTYCODE*, ANSI/FIPS) -> county_code (3-char, zero-padded); NA for 777/888/999
# df <- unify_one(df, "cty_raw", c("CTYCODE","CTYCODE1","CTYCODE2"))
# df <- df %>% mutate(
#   county_code = case_when(
#     str_detect(to_chr(cty_raw), "^[0-9]+$") ~ str_pad(as.character(to_int(cty_raw)), width = 3, side = "left", pad = "0"),
#     TRUE ~ NA_character_
#   ),
#   county_code = case_when(
#     county_code %in% c("777","888","999") ~ NA_character_,  # unknown/out-of-state/refused
#     TRUE ~ county_code
#   )
# )
# 
# # 21) Zip code (ZIPCODE, ZIPCODE1) -> zip5 (5-char), NA for 77777/99999
# df <- unify_one(df, "zip_raw", c("ZIPCODE","ZIPCODE1"))
# df <- df %>% mutate(
#   zip5 = case_when(
#     str_detect(to_chr(zip_raw), "^[0-9]+$") ~ str_pad(as.character(to_int(zip_raw)), width = 5, side = "left", pad = "0"),
#     TRUE ~ NA_character_
#   ),
#   zip5 = case_when(
#     zip5 %in% c("77777","99999") ~ NA_character_,
#     TRUE ~ zip5
#   )
# )


# ---------- Optional: drop original source columns once validated ----------
df <- df %>% select(
  -SEX, -SEX1, -`_SEX`, -BIRTHSEX,
  -ADDEPEV, -ADDEPEV2, -ADDEPEV3,
  -ASTHMA, -ASTHMA2, -ASTHMA3, -ASTHNOW,
  -DIABETES, -DIABETE2, -DIABETE3, -DIABETE4,
  -BPHIGH, -BPHIGH2, -BPHIGH3, -BPHIGH4, -BPHIGH6,
  -CHCCOPD, -CHCCOPD1, -CHCCOPD2, -CHCCOPD3,
  -CHCKDNY1, -CHCKDNY2, -CHCKIDNY,
  -CHCOCNC1, -CHCOCNCR, -CHCSCNC1, -CHCSCNCR,
  -CHECKUP, -CHECKUP1,
  -CVDCORHD, -CVDCRHD2, -CVDCRHD3, -CVDCRHD4,
  -CVDINFAR, -CVDINFR2, -CVDINFR3, -CVDINFR4, -CVDINFRS,
  -CVDSTRK2, -CVDSTRK3, -CVDSTROK,
  -LSATISFY,
  -HAVARTH, -HAVARTH2, -HAVARTH3, -HAVARTH4, -HAVARTH5,
  -PREDIAB, -PREDIAB1, -PREDIAB2,
  -USEEQUIP,
  -DIFFWALK,
  # Keep EDUCA for wrangle_education.R
  # -HISPANIC, -HISPANC2, -HISPANC3,
  # -INCOME, -INCOME2, -INCOME3, -INCOME95,
  # -MRACE, -MRACE1, -MRACE2,
  # -ORACE, -ORACE2, -ORACE3, -ORACE4,
  #  -PAINACTV, -PAINACT2,
  # -CTYCODE, -CTYCODE1, -CTYCODE2,
  # -ZIPCODE, -ZIPCODE1,
  -IYEAR, -DISPCODE, 
  #  -GENHLTH, -PHYSHLTH, -MENTHLTH, -POORHLTH,
  #  -SEATBELT, -CHILDREN, -SMOKE100, -MARITAL, -PREGNANT
) %>% 
  select(-ends_with("_source")) %>% 
  select(-ends_with("_raw")) 

df <- df %>% 
  mutate(age = age_5yr_num) %>% 
  select(-`IDATE`, -`_AGEG5YR`, -`_AGE80`, -`AGE`, -`_AGE`) #, -`_SEX`)

df <- df  %>%
 mutate(strata = strata_year) %>%
  select(-strata_year)

colnames(df)

data_brfss <- df


readr::write_rds(data_brfss, derived_path("data_brfss.rds"))

# write_csv(df, here("big_data/BRFSS/brfss_selected_20250916.csv"))

#############################################
# ---------- Analyze prevalence of selected conditions over time ----------
#############################################

# Sanity check: look up USA prevalence
# 
# 
# library(dplyr)
# library(ggplot2)
# library(srvyr)
# library(rlang)
# library(scales)
# 
# plot_prevalence_svy <- function(svy, var, label, year = year, ci = FALSE, digits = 0) {
#   var  <- ensym(var)
#   year <- ensym(year)
#   
#   # Build the summary table using survey weights
#   prev_df <- svy %>%
#     mutate(.ind = (!!var) == 1) %>%                         # indicator for "yes"
#     group_by(!!year) %>%
#     summarise(
#       prevalence = survey_mean(.ind, na.rm = TRUE,
#                                vartype = if (ci) c("ci") else NULL),
#       n = unweighted(sum(!is.na(.ind))),                    # unweighted denominator (optional)
#       .groups = "drop"
#     )
#   
#   # Plot
#   p <- ggplot(prev_df, aes(x = factor(!!year), y = prevalence)) +
#     geom_col() +
#     geom_text(aes(label = percent(prevalence, accuracy = 10^(-digits))),
#               vjust = -0.5, size = 3) +
#     scale_y_continuous(labels = percent_format(accuracy = 1),
#                        expand = expansion(mult = c(0, 0.05))) +
#     labs(
#       x = "Year",
#       y = paste("Weighted prevalence of", label),
#       title = paste("Weighted Prevalence of", label, "by Year")
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   if (ci) {
#     p <- p +
#       geom_errorbar(aes(ymin = prevalence_low, ymax = prevalence_upp),
#                     width = 0.2)
#   }
#   
#   return(p)
# }
# 
# svy_brfss <- df %>%
#   as_survey_design(
#     ids    = psu,
#     # strata = ststr,
#     weights = wt#,
#     #  nest = TRUE
#   )
# 
# # Plot prevalence
# 
# plot_prevalence <- function(data, var, label) {
#   # Use tidy evaluation so column name can be passed unquoted
#   var <- rlang::ensym(var)
#   
#   data %>%
#     group_by(year) %>%
#     summarise(
#       n_yes   = sum(!!var == 1, na.rm = TRUE),
#       n_total = sum(!!var %in% c(0, 1)),
#       prevalence = ifelse(n_total > 0, n_yes / n_total, NA_real_),
#       .groups = "drop"
#     ) %>%
#     ggplot(aes(x = factor(year), y = prevalence)) +
#     geom_col(fill = "steelblue") +
#     geom_text(
#       aes(label = scales::percent(prevalence, accuracy = 1)), #accuracy = 0.1
#       vjust = -0.3, 
#       size = 3
#     ) +
#     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#     labs(
#       x = "Year",
#       y = paste("Prevalence of", label),
#       title = paste("Prevalence of", label, "by Year")
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# }
# 
# plot_prevalence(df, htn_dx, "Hypertension Diagnosis")
# 
# colnames(df)
# glimpse(df)
# 
# 
# # Sex: 
# with(df, table(sex, year, useNA = "always")) 
# # available all years (1993-2024)
# # they seem to oversample women a little bit so it isn't 50:50, but it's not too bad
# 
# # Depression:
# with(df, table(dep_dx, year, useNA = "always"))
# plot_prevalence(df, dep_dx, "Depression Diagnosis")
# plot_prevalence_svy(svy_brfss, var = dep_dx, label = "Depression Diagnosis")
# # Available 2006, 2008, 2010-2024
# # Approx % from brfss data: 15-20%
# # Approx % from internet: 18% current, 29% ever
# # Trend over time: relatively stable slight increase
# # Agreement of brfss with current online: reasonable agreement
# 
# # Asthma
# with(df, table(asthma, year, useNA = "always"))
# plot_prevalence(df, asthma, "Asthma")
# plot_prevalence_svy(svy_brfss, var = asthma, label = "Asthma")
# # Available: 1999-2024
# # Approx % from brfss data: 6-10%
# # Approx % from internet: 8%
# # Trend over time: slightly increasing
# # Agreement of brfss with current online: reasonable agreement
# 
# # Diabetes
# with(df, table(diabetes_dx, year, useNA = "always"))
# plot_prevalence(df, diabetes_dx, "Diabetes Diagnosis")
# plot_prevalence_svy(svy_brfss, var = diabetes_dx, label = "Diabetes Diagnosis")
# # Available: all years (1993-2024)
# # Approx % from brfss data: 5% in the 90s and 12% now
# # Approx % from internet: 14.7%
# # Trend over time: increasing dramatically
# # Agreement of brfss with current online: reasonable agreement
# 
# # Prediabetes from diabetes variable  
# with(df, table(prediabetes_from_diabvar, year, useNA = "always"))
# plot_prevalence(df, prediabetes_from_diabvar, "Prediabetes Diagnosis")
# # Available: 2004-2024
# # Approx % from brfss data: 1-3%
# # Approx % from internet: 38%
# # Trend over time: increasing 
# # Agreement of brfss with current online: DOES NOT AGREE
# 
# # Prediabetes from prediabetes variable    
# with(df, table(prediab_dx, year, useNA = "always"))
# plot_prevalence(df, prediab_dx, "Prediabetes Diagnosis")
# # Available: 2008-2024
# # Approx % from brfss data: 7-15%
# # Approx % from internet: 38%
# # Trend over time: increasing dramatically
# # Agreement of brfss with current online: DOES NOT AGREE  
# 
# # Prediabetes from either variable    
# with(df, table(prediab_any, year, useNA = "always"))
# plot_prevalence(df, prediab_any, "Prediabetes Diagnosis")
# # Available: 2004-2024
# # Approx % from brfss data: 1-8%
# # Approx % from internet: 38%
# # Trend over time: increasing but VERY CHOPPY
# # Agreement of brfss with current online: NOT RELIABLE    
# 
# # Hypertension diagnosis  
# with(df, table(htn_dx, year, useNA = "always"))
# plot_prevalence(df, htn_dx, "Hypertension Diagnosis")
# plot_prevalence_svy(svy_brfss, var = htn_dx, label = "Hypertension Diagnosis")
# # Available: all years (1993-2024)
# # Approx % from brfss data: 23% in the 90s and 42% unweighted, 35% weighted now
# # Approx % from internet: 47.7%
# # Trend over time: increasing dramatically
# # Agreement of brfss with current online: reasonable enough agreement in unweighted, seems not too close in weighted
# 
# # COPD diagnosis  
# with(df, table(copd_dx, year, useNA = "always"))
# plot_prevalence(df, copd_dx, "COPD Diagnosis")
# plot_prevalence_svy(svy_brfss, var = copd_dx, label = "COPD Diagnosis")
# # Available: 2011-2024
# # Approx % from brfss data: 8% unweighted, 6-7% weighted
# # Approx % from internet: 6-7%
# # Trend over time: relatively constant
# # Agreement of brfss with current online: reasonable agreement  
# 
# # CKD diagnosis  
# with(df, table(ckd_dx, year, useNA = "always"))
# plot_prevalence(df, ckd_dx, "CKD Diagnosis")
# plot_prevalence_svy(svy_brfss, var = ckd_dx, label = "CKD Diagnosis")
# # Available: 2011-2024
# # Approx % from brfss data: 3-4%
# # Approx % from internet: 1 in 7 (14%) but 90% of people don't know
# # Trend over time: increasing 
# # Agreement of brfss with current online: UNCLEAR
# 
# # CHD diagnosis
# with(df, table(chd_dx, year, useNA = "always"))
# plot_prevalence(df, chd_dx, "CHD Diagnosis")
# plot_prevalence_svy(svy_brfss, var = chd_dx, label = "CHD Diagnosis")
# # Available: 1996-2024
# # Approx % from brfss data: 5-6% unweighted, 4-5% weighted
# # Approx % from internet: 7%
# # Trend over time: relatively constant
# # Agreement of brfss with current online: reasonable agreement unweighted, not great weighted 
# 
# # MI diagnosis
# with(df, table(mi_dx, year, useNA = "always"))
# plot_prevalence(df, mi_dx, "MI Diagnosis")
# plot_prevalence_svy(svy_brfss, var = mi_dx, label = "MI Diagnosis")
# # Available: 1996-2024
# # Approx % from brfss data: 4-6%
# # Approx % from internet: 6-7%
# # Trend over time: relatively constant
# # Agreement of brfss with current online: reasonable agreement, a little low 
# 
# # Stroke diagnosis
# with(df, table(stroke_dx, year, useNA = "always"))
# plot_prevalence(df, stroke_dx, "Stroke Diagnosis")
# plot_prevalence_svy(svy_brfss, var = stroke_dx, label = "Stroke Diagnosis")
# # Available: 1996-2024
# # Approx % from brfss data: 2-4%
# # Approx % from internet: 2.9%
# # Trend over time: increasing 
# # Agreement of brfss with current online: reasonable agreement
# 
# # Arthritis diagnosis
# with(df, table(arthritis_dx, year, useNA = "always"))
# plot_prevalence(df, arthritis_dx, "Arthritis Diagnosis")
# plot_prevalence_svy(svy_brfss, var = arthritis_dx, label = "Arthritis Diagnosis")
# # Available: 1996-2024
# # Approx % from brfss data: 23-43/32 unweighted%, 22-27% weighted
# # Approx % from internet: 21.2%
# # Trend over time: relatively constant
# # Agreement of brfss with current online: reasonable agreement, a little low
# 
# # Cancer but not skin  
# with(df, table(cancer_other_dx, year, useNA = "always"))
# plot_prevalence(df, cancer_other_dx, "Cancer Other Not Skin Diagnosis")
# plot_prevalence_svy(svy_brfss, var = cancer_other_dx, label = "Cancer Other Not Skin Diagnosis")
# # Available: 2011-2024
# # Approx % from brfss data: 9-12% unweighted, 6-8% weighted
# # Approx % from internet: 5% (18 million/340 million)
# # Trend over time: slightly increasing
# # Agreement of brfss with current online: somewhat reasonable agreement, although notably higher  
# 
# # Skin cancer
# with(df, table(cancer_skin_dx, year, useNA = "always"))
# plot_prevalence(df, cancer_skin_dx, "Skin Cancer Diagnosis")
# plot_prevalence_svy(svy_brfss, var = cancer_skin_dx, label = "Skin Cancer Diagnosis")
# # Available: 2011-2024
# # Approx % from brfss data: 9-10% unweighted 5-6% weighted
# # Approx % from internet: 1 in 5 expected? not sure what that means
# # Trend over time: somewhat constant
# # Agreement of brfss with current online: NOT SURE
# 
# # Any cancer
# with(df, table(cancer_any_dx, year, useNA = "always"))
# plot_prevalence(df, cancer_any_dx, "Any Cancer Diagnosis")
# plot_prevalence_svy(svy_brfss, var = cancer_any_dx, label = "Any Cancer Diagnosis")
# # Available: 2011-2024
# # Approx % from brfss data: 16-17% unweighted 11-12% weighted
# # Approx % from internet: lifetime risk 40%?
# # Trend over time: somewhat constant
# # Agreement of brfss with current online: NOT SURE
# 
# # Check ups 
# with(df, table(checkup_cat, year, useNA = "always"))
# plot_prevalence(df, checkup_cat, "Check Ups")
# plot_prevalence_svy(svy_brfss, var = checkup_cat, label = "Check Ups")
# 
# with(df, table(checkup_never, year, useNA = "always"))
# plot_prevalence(df, checkup_never, "Check Ups")
# plot_prevalence_svy(svy_brfss, var = checkup_never, label = "Check Ups")
# 
# 
# 
# # diabetes	diabetes_dx	1993-2024	reasonable agreement
# # htn	htn_dx	1993-2024	maybe questionable
# # chd	chd_dx	1996-2024	reasonable unweighted, not great weighted
# # mi	mi_dx	1996-2024	reasonable agreement
# # stroke	stroke_dx	1996-2024	reasonable agreement
# # arthritis	arthritis_dx	1996-2024	reasonable agreement
# # asthma	asthma	1999-2024	reasonable agreement
# # depression	dep_dx	2006, 2008, 2010-2024	reasonable agreement
# # copd	copd_dx	2011-2024	reasonable agreement
# # ckd	ckd_dx	2011-2024	unclear
# # any cancer	cancer_any	2011-2024	
# # cancer not skin	cancer_other_dx	2011-2024

