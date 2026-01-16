data_brfss %>% 
 # group_by(age_5yr_num) %>% 
 # filter(age_5yr_num == 37) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  facet_wrap(~ age_5yr_cat) +
  geom_point() +
  geom_line()

data_brfss %>% 
  # group_by(age_5yr_num) %>% 
  # filter(age_5yr_num == 37) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh, color = age_5yr_cat)) +
 # facet_wrap(~ age_5yr_cat) +
  geom_point() +
  geom_line()

data_brfss %>% 
  # group_by(age_5yr_num) %>% 
   filter(age_5yr_num == 27) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  # facet_wrap(~ age_5yr_cat) +
  geom_point() +
  geom_line()

data_brfss %>% 
  filter(age_5yr_num == 32) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  geom_point() +
  geom_line()

data_brfss %>% 
  filter(age_5yr_num == 37) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  geom_point() +
  geom_line()

data_brfss %>% 
  filter(age_5yr_num == 42) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  geom_point() +
  geom_line()

data_brfss %>% 
  filter(age_5yr_num == 47) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  geom_point() +
  geom_line()

data_brfss %>% 
  filter(age_5yr_num == 52) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  geom_point() +
  geom_line()

data_brfss %>% 
  filter(age_5yr_num == 57) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  geom_point() +
  geom_line()

data_brfss %>% 
  filter(age_5yr_num == 62) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  geom_point() +
  geom_line()

data_brfss %>% 
  filter(age_5yr_num == 67) %>% 
  mutate(cohort = year - age_5yr_num) %>% 
  group_by(cohort, age_5yr_cat) %>% 
  summarize(avg_srh = mean(srh)) %>% 
  ggplot(aes(x = cohort, y = avg_srh)) +
  geom_point() +
  geom_line()


fctn_plot_brfss_combo <- function(age_of_interest) {
    data_brfss %>% 
      filter(age_5yr_num == age_of_interest) %>% 
      mutate(cohort = year - age_5yr_num) %>% 
      group_by(cohort, age_5yr_cat) %>% 
      summarize(avg_srh_10 = 10*mean(srh, na.rm = TRUE),
                avg_mental = mean(mental_health_good_days, na.rm = TRUE),
                avg_physical = mean(physical_health_good_days, na.rm = TRUE),
                avg_functional = mean(usual_activities_health_good_days, na.rm = TRUE)) %>% 
      pivot_longer(cols = c("avg_srh_10", "avg_mental", "avg_physical", "avg_functional"),
                   names_to = "avg",
                   values_to = "value"
                   ) %>% 
      ggplot(aes(x = cohort, y = value, color = avg)) +
      labs(title = paste0("Age ", age_of_interest)) +
      geom_point() +
      geom_line() +
      theme(legend.position = "none")
    }

table(data_brfss$age_5yr_num)

ages_held_constant <- unique(data_brfss$age_5yr_num) %>% sort()

library(gridExtra)
grid.arrange(
fctn_plot_brfss_combo(ages_held_constant[1]),
fctn_plot_brfss_combo(ages_held_constant[2]),
fctn_plot_brfss_combo(ages_held_constant[3]),
fctn_plot_brfss_combo(ages_held_constant[4]),
fctn_plot_brfss_combo(ages_held_constant[5]),
fctn_plot_brfss_combo(ages_held_constant[6]),
fctn_plot_brfss_combo(ages_held_constant[7]),
fctn_plot_brfss_combo(ages_held_constant[8]),
fctn_plot_brfss_combo(ages_held_constant[9]),
fctn_plot_brfss_combo(ages_held_constant[10]),
fctn_plot_brfss_combo(ages_held_constant[11]),
fctn_plot_brfss_combo(ages_held_constant[12]),
fctn_plot_brfss_combo(ages_held_constant[13]) + theme(legend.position = "right"))
nrow = 4)
