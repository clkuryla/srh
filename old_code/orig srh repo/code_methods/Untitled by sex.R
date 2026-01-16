
data_brfss %>% 
  mutate(sex= as.factor(sex)) %>% 
  filter(!(is.na(sex))) %>% 
  group_by(age_decade_cat, year, sex) %>% 
  summarize(mean_health = mean(srh, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_health, color = age_decade_cat)) +
  facet_grid(. ~ sex) +
  geom_line() +
  labs(title = "Average SRH Per Year for Each Age Group by Sex",
       subtitle = "BRFSS 1993 - 2023 Dataset",
       y = "Average SRH", 
       x = "Year",
       color = "Age Group") +
  scale_color_discrete() +
  theme_minimal() #+
#  scale_color_brewer(palette = "Set2")

data_nhis %>% 
  mutate(sex= as.factor(SEX)) %>% 
  filter(SEX %in% (c(1,2))) %>% 
  group_by(age_decade_cat_6, year, SEX) %>% 
  summarize(mean_health = mean(srh, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_health, color = age_decade_cat_6)) +
  facet_grid(. ~ SEX) +
  geom_line() +
  labs(title = "Average SRH Per Year for Each Age Group by Sex",
       subtitle = "NHIS Dataset",
       y = "Average SRH", 
       x = "Year",
       color = "Age Group") +
  scale_color_discrete() +
  theme_minimal() 
