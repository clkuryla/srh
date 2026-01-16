



library(tidyverse)
library(haven)

# Separate keys into DEMO and HUQ
demo_key <- nhanes4_key %>%
  filter(str_detect(nhanes_file, "^DEMO") | nhanes_file == "P_DEMO")

huq_key <- nhanes4_key %>%
  filter(str_detect(nhanes_file, "^HUQ") | nhanes_file == "P_HUQ")

### Process DEMO files ###

demo_data <- demo_key %>%
  group_split(nhanes_file) %>%  # Split data by each DEMO file
  map_df(~ {
    current_file <- unique(.x$nhanes_file)
    file_path <- paste0("big_data/NHANES/nhanes_4/", current_file, ".xpt")
    
    df <- read_xpt(file_path)
    vars_map <- setNames(.x$my_var, .x$nhanes_var)
    
    # We select SEQN, the nhanes_var columns, plus the additional survey design and weighting variables.
    # Using any_of() prevents errors if some files are missing these columns.
    df_selected <- df %>%
      select(
        SEQN,
        all_of(.x$nhanes_var),
        any_of(c("SDMVSTRA", "SDMVPSU", "WTINT2YR"))
      ) %>%
      rename(any_of(vars_map))
    
    df_selected
  })

### Process HUQ files ###

huq_data <- huq_key %>%
  group_split(nhanes_file) %>%
  map_df(~ {
    current_file <- unique(.x$nhanes_file)
    file_path <- paste0("big_data/NHANES/nhanes_4/", current_file, ".xpt")
    
    df <- read_xpt(file_path)
    vars_map <- setNames(.x$my_var, .x$nhanes_var)
    
    df_selected <- df %>%
      select(SEQN, all_of(.x$nhanes_var)) %>%
      rename(any_of(vars_map))
    
    df_selected
  })

### Merge DEMO and HUQ data by SEQN ###

final_data <- full_join(demo_data, huq_data, by = "SEQN")

# final_data now has columns:
# SEQN, age, SDDSRVYR, srh_huq010, and also SDMVSTRA, SDMVPSU, WTINT2YR if available in the DEMO files.


nhanes_data1 <- merge(nhanes_data, final_data, by = "SEQN")

nhanes_data_4 <- nhanes_data1 %>% 
  filter(year > 1996)

install.packages("survey")
library(survey)


nhanes_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTINT2YR,   # or WTINT2YR if you are analyzing interview-only data
  nest = TRUE,
  data = nhanes_data_4
)

nhanes_svy <- nhanes_design %>%
  as_survey_design(
    ids = SDMVPSU,           # PSU identifiers (use 1 if not available)
    weights = WTINT2YR,  # wtssall pre 2018, wtsscomp combined //Use 'wtss' for single-year analysis
    strata = SDMVSTRA
    )

library(srvyr)

nhanes_svy %>%
  mutate(age_group = cut(age, breaks = 6)) %>% 
  group_by(age_group, year) %>%
  summarise(
    mean_health = survey_mean(srh, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = year, y = mean_health, color = age_group)) +
  #  geom_smooth(alpha = 0.2) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average SRH Per Year for Each Age Group",
    subtitle = "GSS Dataset with Survey Weights",
    x = "Year",
    y = "Average Self-Rated Health",
    color = "Age Group"
  ) +
  theme_minimal()

svymean(~age, nhanes_design)

# health vs age per year
nhanes_svy %>% 
  group_by(age, year) %>% 
  summarize(mean_health = survey_mean(srh)) %>% 
  ggplot(aes(x = age, y = mean_health)) +
  geom_line(color = "cornflowerblue") +
  facet_wrap(~ year) +
  labs(title = "Self-Rated Health By Age (Per Year)",
       subtitle = "GSS Dataset",
       x = "Age of Respondent", 
       y = "Average SRH",
  )

weighted_lm_by_year <- nhanes_svy %>%
  group_by(year) %>%
  group_map_dfr(~ {
    model <- survey::svyglm(srh ~ age, design = .x)
    tidy(model, conf.int = TRUE)
  }) %>%
  filter(term == "age") %>%
  select(year, estimate, std.error, conf.low, conf.high, statistic, p.value)

knitr::kable(weighted_lm_by_year)

# Visualize
ggplot(weighted_lm_by_year, aes(x = year, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(0.05)) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
  labs(
    title = "Regression of 'Age' Coefficient Over Years",
    subtitle = "GSS Dataset",
    x = "Year",
    y = "Coefficient of Age"
  ) +
  theme_minimal()

# Perform linear regression of 'coef' (age coefficient) vs 'year'
lm_coef_vs_year <- lm(estimate ~ year, data = weighted_lm_by_year)

# View the summary of the regression
summary(lm_coef_vs_year)
