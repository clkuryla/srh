library(tidyverse)
library(survival)
library(survey)
library(broom)

# Create binary SRH variable and calculate survival time
data_km <- data_nhis_mort_filter %>%
  mutate(
    srh_binary = ifelse(srh <= 3, "Good-Excellent", "Fair-Poor"),
    survtime = MORTDODY - YEAR  # Calculate survival time
  ) %>%
  filter(!is.na(MORTWT)) %>% 
  select(YEAR, AGE, srh, MORTDODY, MORTWT, MORTWTSA, mortstat_recoded) 

# Create survey design object
svy_design <- svydesign(
  ids = ~1,
  weights = ~MORTWT,
  data = data_km
)

# Fit the weighted survival model
km_fit <- svykm(
  Surv(survtime, MORTSTAT) ~ srh_binary,
  design = svy_design,
  se = TRUE
)

# Convert to data frame for plotting
km_df <- data.frame(
  time = km_fit$time,
  surv_good = km_fit$surv[,1],
  surv_poor = km_fit$surv[,2],
  se_good = sqrt(km_fit$var[,1]),
  se_poor = sqrt(km_fit$var[,2])
)

# Create the plot
ggplot(km_df) +
  # Survival curves
  geom_step(aes(x = time, y = surv_good, color = "Good-Excellent"), size = 1) +
  geom_step(aes(x = time, y = surv_poor, color = "Fair-Poor"), size = 1) +
  # Confidence intervals
  geom_ribbon(aes(x = time, 
                  ymin = surv_good - 1.96*se_good,
                  ymax = surv_good + 1.96*se_good,
                  fill = "Good-Excellent"), alpha = 0.2) +
  geom_ribbon(aes(x = time, 
                  ymin = surv_poor - 1.96*se_poor,
                  ymax = surv_poor + 1.96*se_poor,
                  fill = "Fair-Poor"), alpha = 0.2) +
  # Customize appearance
  scale_color_manual(values = c("Good-Excellent" = "blue", "Fair-Poor" = "red")) +
  scale_fill_manual(values = c("Good-Excellent" = "blue", "Fair-Poor" = "red")) +
  theme_minimal() +
  labs(
    title = "Weighted Kaplan-Meier Survival Curves by Self-Rated Health",
    x = "Time (Years)",
    y = "Survival Probability",
    color = "Self-Rated Health",
    fill = "95% CI"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Add log-rank test
survdiff_result <- svylogrank(
  Surv(survtime, MORTSTAT) ~ srh_binary,
  design = svy_design
)

print(survdiff_result)