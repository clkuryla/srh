
data_gss <- data_gss %>% 
  filter(age < 70) %>% 
  mutate(age_4yr = cut(age, breaks = 8)) %>% 
  mutate(cohort_10yr = cut(cohort, breaks = 10) ) %>% 
  mutate(period_4yr = cut(year, breaks = 14))
           

clk_lmer =
  lmer(health ~ age_4yr + (1|period_4yr) + (1|cohort_10yr) + log(wtsscomp) , data = data_gss, REML = FALSE)
summary(clk_lmer)

clk_lmer2 =
  lmer(health ~ (1|age_4yr) + (1|period_4yr) + cohort_10yr +log(wtsscomp) , data = data_gss, REML = FALSE)
summary(clk_lmer2)

clk_lmer3 =
  lmer(srh ~ (1|age_4yr) + generation_two_sections + (1|SDMVSTRA/SDMVPSU), data = data_nhanes, REML = FALSE)
summary(clk_lmer3)

clk_lmer4 =
  lmer(health ~ age_4yr + (1|period_4yr) + cohort_10yr +log(wtsscomp) , data = data_gss, REML = FALSE)
summary(clk_lmer4)


model <- lmer(Y ~ age + health + 
                (1 | period) + 
                (age + health | period:cohort),
              data = df)