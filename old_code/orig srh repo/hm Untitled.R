# Health Measure Trends by Age Group
# Variables renamed to uppercase with _harm suffix

library(tidyverse)

# Assuming your data is in a dataframe called 'df' with columns like:
# year, age_group, and the health measure columns

# Step 1: Rename variables to uppercase with _harm suffix
df_renamed <- df %>%
  rename(
    ADMALS_harm = Admals,
    ADMWLM_harm = Admwlm,
    ADPAIN_harm = Adpain,
    ADPALS_harm = Adpals,
    ADPWLM_harm = Adpwlm,
    ADSOCA_harm = Adsoca
  )

# Step 2: Pivot to long format for ggplot 
df_long <- data_meps %>%
  pivot_longer(
    cols = ends_with("_harm"),
    names_to = "measure",
    values_to = "value"
  )

# Step 3: Calculate summary statistics by year, age_group, and measure
# (if your data is at the individual level)
df_summary <- df_long %>%
  group_by(year, age_group, measure) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(n()),
    lower = mean_value - 1.96 * se,
    upper = mean_value + 1.96 * se,
    .groups = "drop"
  )

# Step 4: Define age group order
df_summary <- df_summary %>%
  mutate(
    age_group = factor(age_group, 
                       levels = c("18-29", "30-39", "40-49", "50-59", 
                                  "60-69", "70-79", "80-89"))
  )

# Step 5: Define color palette matching the original plot
measure_colors <- c(
  "ADMALS_harm" = "#66C2A5",
  "ADMWLM_harm" = "#FC8D62",
  "ADPAIN_harm" = "#B19CD9",
  "ADPALS_harm" = "#8DA0CB",
  "ADPWLM_harm" = "#E78AC3",
  "ADSOCA_harm" = "#A6761D"
)

# Step 6: Create the plot
p <- ggplot(df_summary, aes(x = year, y = mean_value, color = measure, fill = measure)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ age_group, nrow = 1, strip.position = "top") +
  scale_color_manual(values = measure_colors, name = "Healthy Days") +
  scale_fill_manual(values = measure_colors, name = "Healthy Days") +
  scale_x_continuous(
    breaks = c(2005, 2010, 2015, 2020),
    labels = c("2005", "2010", "2015", "2020")
  ) +
  labs(
    title = "Health Measure Trends by Age Group",
    subtitle = "Age Group (Years)",
    x = "Year",
    y = "Average Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 11),
    strip.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed")
  ) +
  guides(
    color = guide_legend(nrow = 2),
    fill = guide_legend(nrow = 2)
  )

# Display the plot
print(p)

# Save the plot
ggsave("health_measures_plot.png", p, width = 12, height = 6, dpi = 300)