vars <- c("year", "trust", "sex", "age", "partyid", "wrkstat", "happy", "degree", "realinc", "health", "life", "polviews", "satfin")
sub <- GSS[, vars]

for_ts <- sub %>% 
  mutate(ntrust = ifelse(trust == 1, 1, 0), 
         baplus = ifelse(degree >= 3, 1, 0),
         happiness = ifelse(happy == 1, 1, 0),
         SRH_Excellent = ifelse(health == 1, 1, 0),
         SRH_Good = ifelse(health == 2, 1, 0),
         SRH_Fair = ifelse(health == 3, 1, 0),
         SRH_Poor = ifelse(health == 4, 1, 0),
         goodlife = ifelse(life == 1, 1, 0),
         nonextreme_views = ifelse(polviews %in% c(3, 4, 5), 1, 0),
         moderate_views = ifelse(polviews == 4, 1, 0),
         cohort = floor(year - age),
         over50 = ifelse(age >= 50, 1, 0),
         boomer = ifelse(cohort >= 1946 & cohort <= 1964, 1, 0) ,
         millenial = ifelse(cohort >= 1981 & cohort <= 1996, 1, 0) ,
         bornin40s = ifelse(cohort >= 1940 & cohort <= 1949, 1, 0),
         sat_w_finances = ifelse(satfin == 1, 1, 0),
         income = realinc
  ) 


# get means by year
by.year <- aggregate(subset(sub, sel = -year), list(year = sub$year), mean, na.rm = T) 

# interpolate for some missing years

# First, add the extra years
# unique(sub$year) # years in dataset
# extra_years <- setdiff(seq(1972, 2018), unique(sub$year)) # years missing for a continus TS; skip 2020+ because of covid
# dim(by.year)[1] # number of years in original data (34)
# length(extra_years) # number of years to add (15)
# dim(by.year)[1] + length(extra_years) # sum (49)
# by.year[35:49, "year"] <- as.vector(extra_years) # add the extra years
# by.year <- arrange(by.year, year) # arrange by year


by.year.keep <- by.year %>% na.omit()

by.year.keep %>% 
  pivot_longer(cols = SRH_Excellent:SRH_Poor,
               names_prefix = "SRH_",
               names_to = "SRH",
               values_to = "Percent") %>% 
  mutate(age_groups = cut(age, breaks = 5)) %>% 
  mutate(Percent = Percent*100) %>% 
  ggplot(aes(x = year, y = Percent, color = SRH)) +
  geom_line() +
  labs(title = "Self-Rated Health Category Distribution",
       subtitle = "GSS Dataset",
       y = "Percent",
       x = "Year") +
  theme_minimal()

by.year.keep %>% 
  ggplot(aes(x = year, y = 5 - health)) +
  geom_line() +
  labs(title = "Mean Self-Rated Health",
       subtitle = "GSS Dataset",
       y = "Self-Rated Health",
       x = "Year") +
  theme_minimal()




library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("filter", "dplyr")

library(tidyverse)

# Self-Rated Health Category Distribution
data_gss %>%
  select(year, health) %>%
  filter(!is.na(health)) %>%
  mutate(
    srh = factor(health, 
                    levels = 1:4,
                    labels = c("Poor", "Fair", "Good", "Excellent"))) %>% 
  # Remove missing values
  # Calculate percentages by year
  group_by(year) %>%
  count(srh) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>% 
  ggplot(aes(x = year, y = percent, color = srh)) +
  geom_line() +
  labs(title = "Self-Rated Health Category Distribution",
       subtitle = "GSS Dataset",
       y = "Percent",
       x = "Year") +
  theme_minimal()

# Self-Rated Health Category Distribution by Age Group
data_gss %>%
  select(year, health, age_group) %>%
  filter(!is.na(health)) %>%
  mutate(
    srh = factor(health, 
                 levels = 1:4,
                 labels = c("Poor", "Fair", "Good", "Excellent"))) %>% 
  # Remove missing values
  # Calculate percentages by year
  group_by(year, age_group) %>%
  count(srh) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>% 
  ggplot(aes(x = year, y = percent, color = srh)) +
  geom_line() +
  facet_wrap(~age_group) +
  labs(title = "Self-Rated Health Category Distribution by Age Group",
       subtitle = "GSS Dataset",
       y = "Percent",
       x = "Year") +
  theme_minimal()
