 data_brfss <- data_brfss %>%
  # Create the activity limitation days for each group
  mutate(
    # Activity limitation days for mental-only group
    activlim_mental_only = ifelse(
      physical_or_mental_poor_days == "mental_health_only", 
      activlim_bad, 
      NA_real_
    ),
    
    # Activity limitation days for physical-only group  
    activlim_physical_only = ifelse(
      physical_or_mental_poor_days == "physical_health_only",
      activlim_bad,
      NA_real_
    ),
    
    # Activity limitation days for both group
    activlim_both = ifelse(
      physical_or_mental_poor_days == "both",
      activlim_bad, 
      NA_real_
    ),
    
    # For normalized versions (0-1 scale for regression)
    activlim_mental_only_norm = activlim_mental_only / 30,
    activlim_physical_only_norm = activlim_physical_only / 30,
    activlim_both_norm = activlim_both / 30
  )

# data_healthy_days <- df %>% 
#   select(GENHLTH:POORHLTH, wt, psu, srh, age_decade_cat, year, ment_bad:dep_dx, physical_or_mental_poor_days:activlim_both_norm)

# Update survey design
svy_design <- svydesign(
  ids = ~psu,
 # strata = ~strata, 
  weights = ~wt,
# data = data_healthy_days,
 # data = df
  data = data_brfss#,
#  nest = TRUE
)



# Plot mean activity limitation days for each group
prev_activlim_source_days <- plot_survey_trends_2(
  svy_data = svy_design,
  plot_type = "prevalence",
  variables = c("activlim_mental_only", 
                "activlim_physical_only", 
                "activlim_both"),
  age_var = "age_decade_cat",
  year_var = "year",
  legend_labels = c("Mental Only", "Physical Only", "Both"),
  overall_title = "Mean Activity Limitation Days by Health Problem Source",
  y_label = "Activity Limitation Days per Month",
  legend_title = "Health Problem Type"
)
prev_activlim_source_days$plot

# Using normalized variables for cleaner coefficients
coef_activlim_source <- plot_survey_trends_2(
  svy_data = svy_design,
  plot_type = "coefficient",
  outcome = "srh",  
  predictors = c("activlim_mental_only", 
                 "activlim_physical_only", 
                 "activlim_both"),
  # predictors = c("activlim_mental_only_norm",
  #                "activlim_physical_only_norm",
  #                "activlim_both_norm"),
 # adjust_for = c("sex", "race_cat", "educ_cat"),  # your standard controls
  age_var = "age_decade_cat",
  year_var = "year",
  legend_labels = c("Mental-Source Limitation", 
                    "Physical-Source Limitation", 
                    "Both-Source Limitation"),
  overall_title = "Effect of Activity Limitations on Self-Rated Health by Source",
  y_label = "Coefficient on SRH", # (per 30 days)",
  legend_title = "Limitation Source"
)
coef_activlim_source$plot

table(data_brfss$physical_or_mental_poor_days)
hist(data_brfss$physical_or_mental_poor_days)