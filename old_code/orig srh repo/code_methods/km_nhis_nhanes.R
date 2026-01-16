# KM

# NHIS

# Load libraries
library(dplyr)
library(survival)
library(survminer)
library(ggplot2)

# === 1. Prepare the Data ===
# Assume your NHIS dataset is named data_nhis and has:
#   survey_year, age_at_survey, death_year, and srh (coded 1-5).


# Create a follow-up time (in years) and event indicator.
# Here we assume that if death_year is missing, we censor at 2016.
data_nhis <- data_nhis %>%
  mutate(
    time_followup = if_else(!is.na(death_year),
                            death_year - survey_year,
                            2016 - survey_year),
    event = if_else(is.na(death_year), 0, 1)
  )

# Create an age_group variable (for example, using age at survey)
data_nhis <- data_nhis %>%
  mutate(age_group = cut(age_at_survey,
                         breaks = c(18,30,40,50,60,70, Inf),
                         labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
                         right = FALSE))

# Create a year_range variable (6 groups)
# Here we use quantiles of survey_year to get roughly equal‐sized groups.
data_nhis <- data_nhis %>%
  mutate(year_range = cut(survey_year,
                          breaks = quantile(survey_year, probs = seq(0, 1, length.out = 7), na.rm = TRUE),
                          include.lowest = TRUE,
                          labels = paste0("YR", 1:6)))

# Create dichotomized SRH variables:
data_nhis <- data_nhis %>%
  mutate(
    # Dichotomy 1: Fair/Poor vs. Good/VeryGood/Excellent
    srh_dich1 = if_else(srh_factor %in% c("Fair", "Poor"),
                        "Fair/Poor", "Good/VeryGood/Excellent"),
    # Dichotomy 2: Poor vs. Excellent – restrict to those two.
    srh_dich2 = if_else(srh_factor %in% c("Poor", "Excellent"),
                        as.character(srh_factor), NA_character_)
  )

# === 2. Kaplan–Meier Curves for NHIS ===

# (i) **Full SRH categories**  
km_fit_full <- survfit(Surv(time_followup, event) ~ srh_factor, data = data_nhis)

fig_nhis_full <- ggsurvplot_facet(km_fit_full, data = data_nhis,
                                  facet.by = c("age_group", "year_range"),
                                  surv.median.line = "hv",
                                  ggtheme = theme_bw(),
                                  palette = "Dark2",
                                  title = "NHIS KM Curves by Full SRH Category")
print(fig_nhis_full)

# (ii) **Dichotomized as Fair/Poor vs. Good/VeryGood/Excellent**
km_fit_dich1 <- survfit(Surv(time_followup, event) ~ srh_dich1, data = data_nhis)

fig_nhis_dich1 <- ggsurvplot_facet(km_fit_dich1, data = data_nhis,
                                   facet.by = c("age_group", "year_range"),
                                   surv.median.line = "hv",
                                   ggtheme = theme_bw(),
                                   palette = "Set1",
                                   title = "NHIS KM Curves: Fair/Poor vs. Good/VeryGood/Excellent")
print(fig_nhis_dich1)

# (iii) **Dichotomized as Poor vs. Excellent**  
# Restrict to rows where srh_dich2 is not NA.
data_nhis_poor_excellent <- data_nhis %>% filter(!is.na(srh_dich2))
km_fit_dich2 <- survfit(Surv(time_followup, event) ~ srh_dich2, data = data_nhis_poor_excellent)

fig_nhis_dich2 <- ggsurvplot_facet(km_fit_dich2, data = data_nhis_poor_excellent,
                                   facet.by = c("age_group", "year_range"),
                                   surv.median.line = "hv",
                                   ggtheme = theme_bw(),
                                   palette = "Set2",
                                   title = "NHIS KM Curves: Poor vs. Excellent")
print(fig_nhis_dich2)
