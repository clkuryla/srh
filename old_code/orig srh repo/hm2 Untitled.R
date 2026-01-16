# Lexis APC Heatmap for Self-Rated Health (NHIS)
# Creates a heatmap visualization with Age on Y-axis, Period (Year) on X-axis,
# and diagonal cohort lines

library(tidyverse)
library(srvyr)
library(viridis)

# =============================================================================
# 1. Calculate Survey-Weighted Mean SRH by Age and Year
# =============================================================================

# Compute weighted mean SRH for each age-year combination
srh_by_age_year <- svy_nhis %>%
  filter(!is.na(srh), !is.na(age), !is.na(year)) %>%
  group_by(age, year) %>%
  summarize(
    mean_srh = survey_mean(srh, na.rm = TRUE, vartype = c("se", "ci")),
    n = unweighted(n())
  ) %>%
  ungroup()

# View the data structure
glimpse(srh_by_age_year)

# Check data ranges
cat("\n--- Data Ranges ---\n")
cat("Age range:", range(srh_by_age_year$age), "\n")
cat("Year range:", range(srh_by_age_year$year), "\n")
cat("SRH range:", range(srh_by_age_year$mean_srh, na.rm = TRUE), "\n")

# =============================================================================
# 2. Basic Lexis Heatmap
# =============================================================================

# Create the basic heatmap
p_basic <- ggplot(srh_by_age_year, aes(x = year, y = age, fill = mean_srh)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Mean SRH",
    direction = -1,  # Higher values (better health) = lighter colors
    na.value = "transparent"  # Don't show NA cells
  ) +
  labs(
    title = "Self-Rated Health by Age and Year (NHIS)",
    subtitle = "Survey-weighted mean SRH (1 = Poor, 5 = Excellent)",
    x = "Survey Year (Period)",
    y = "Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),  # Show gaps clearly
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p_basic)

# Save basic version
ggsave("lexis_heatmap_basic.png", p_basic, width = 14, height = 10, dpi = 300)

# =============================================================================
# 3. Lexis Heatmap with Cohort Diagonal Lines
# =============================================================================

# Get data ranges for cohort lines
year_range <- range(srh_by_age_year$year)
age_range <- range(srh_by_age_year$age)

# Create cohort lines data (birth year = survey year - age)
# Select cohorts at regular intervals (e.g., every 10 years)
cohort_years <- seq(
  from = floor((year_range[1] - age_range[2]) / 10) * 10,
  to = ceiling((year_range[2] - age_range[1]) / 10) * 10,
  by = 10
)

# Create line segments for each cohort
cohort_lines <- map_dfr(cohort_years, function(cohort) {
  tibble(
    cohort = cohort,
    # Line goes from (cohort + min_age, min_age) to (cohort + max_age, max_age)
    x_start = pmax(cohort + age_range[1], year_range[1]),
    x_end = pmin(cohort + age_range[2], year_range[2]),
    y_start = pmax(age_range[1], year_range[1] - cohort),
    y_end = pmin(age_range[2], year_range[2] - cohort)
  ) %>%
    filter(x_start <= x_end, y_start <= y_end)
})

# Create labels for cohort lines (positioned at the TOP of each line for visibility)
cohort_labels <- cohort_lines %>%
  mutate(
    label_x = x_end,
    label_y = y_end,
    label = paste0(cohort)
  )

# Heatmap with cohort lines
p_with_cohorts <- ggplot(srh_by_age_year, aes(x = year, y = age)) +
  geom_tile(aes(fill = mean_srh)) +
  # Add cohort diagonal lines
  geom_segment(
    data = cohort_lines,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    color = "white",
    linewidth = 0.3,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  # Add cohort labels at top of lines
  geom_text(
    data = cohort_labels,
    aes(x = label_x, y = label_y, label = label),
    color = "white",
    size = 2.5,
    hjust = -0.1,
    vjust = 1.2,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Mean SRH",
    direction = -1,
    na.value = "transparent"  # Don't show NA cells
  ) +
  labs(
    title = "Lexis Diagram: Self-Rated Health (NHIS)",
    subtitle = "Survey-weighted mean SRH | Diagonal lines = birth cohorts",
    x = "Survey Year (Period)",
    y = "Age",
    caption = "SRH Scale: 1 = Poor, 5 = Excellent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),  # Show gaps clearly
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )
# Removed coord_fixed() - let ggplot choose appropriate aspect ratio

print(p_with_cohorts)

ggsave("lexis_heatmap_with_cohorts.png", p_with_cohorts, width = 14, height = 10, dpi = 300)

# =============================================================================
# 4. Alternative: Centered/Deviation Heatmap
# =============================================================================

# Calculate deviations from grand mean (helpful for seeing APC patterns)
grand_mean <- weighted.mean(
  srh_by_age_year$mean_srh, 
  srh_by_age_year$n, 
  na.rm = TRUE
)

srh_by_age_year <- srh_by_age_year %>%
  mutate(
    srh_deviation = mean_srh - grand_mean,
    cohort = year - age
  )

# Deviation heatmap
p_deviation <- ggplot(srh_by_age_year, aes(x = year, y = age, fill = srh_deviation)) +
  geom_tile() +
  geom_segment(
    data = cohort_lines,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    color = "gray30",
    linewidth = 0.3,
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_gradient2(
    low = "#d73027",      # Red for below average
    mid = "white",         # White for average
    high = "#1a9850",      # Green for above average
    midpoint = 0,
    name = "SRH\nDeviation",
    limits = c(-max(abs(srh_by_age_year$srh_deviation), na.rm = TRUE),
               max(abs(srh_by_age_year$srh_deviation), na.rm = TRUE)),
    na.value = "transparent"  # Don't show NA cells
  ) +
  labs(
    title = "Lexis Diagram: SRH Deviations from Grand Mean (NHIS)",
    subtitle = paste0("Grand mean SRH = ", round(grand_mean, 2)),
    x = "Survey Year (Period)",
    y = "Age",
    caption = "Green = better than average | Red = worse than average"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),  # Show gaps clearly
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )
# Removed coord_fixed() - let ggplot choose appropriate aspect ratio

print(p_deviation)

ggsave("lexis_heatmap_deviation.png", p_deviation, width = 14, height = 10, dpi = 300)

# =============================================================================
# 5. Smoothed Version (Optional - reduces noise)
# =============================================================================

# For a cleaner visualization, you might want to bin ages
srh_binned <- svy_nhis %>%
  filter(!is.na(srh), !is.na(age), !is.na(year)) %>%
  mutate(age_group = floor(age / 5) * 5) %>%  # 5-year age bins
  group_by(age_group, year) %>%
  summarize(
    mean_srh = survey_mean(srh, na.rm = TRUE),
    n = unweighted(n())
  ) %>%
  ungroup()

# Binned version with cohort lines
cohort_lines_binned <- map_dfr(cohort_years, function(cohort) {
  tibble(
    cohort = cohort,
    x_start = pmax(cohort + min(srh_binned$age_group), min(srh_binned$year)),
    x_end = pmin(cohort + max(srh_binned$age_group), max(srh_binned$year)),
    y_start = pmax(min(srh_binned$age_group), min(srh_binned$year) - cohort),
    y_end = pmin(max(srh_binned$age_group), max(srh_binned$year) - cohort)
  ) %>%
    filter(x_start <= x_end, y_start <= y_end)
})

p_binned <- ggplot(srh_binned, aes(x = year, y = age_group, fill = mean_srh)) +
  geom_tile() +
  geom_segment(
    data = cohort_lines_binned,
    aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
    color = "white",
    linewidth = 0.4,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Mean SRH",
    direction = -1,
    na.value = "transparent"  # Don't show NA cells
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    title = "Lexis Diagram: Self-Rated Health (NHIS)",
    subtitle = "5-year age bins | Survey-weighted means",
    x = "Survey Year (Period)",
    y = "Age Group",
    caption = "SRH Scale: 1 = Poor, 5 = Excellent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),  # Show gaps clearly
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p_binned)

ggsave("lexis_heatmap_binned.png", p_binned, width = 14, height = 10, dpi = 300)

# =============================================================================
# 6. Summary Statistics
# =============================================================================

cat("\n=== Summary Statistics ===\n\n")

# By period
period_summary <- srh_by_age_year %>%
  group_by(year) %>%
  summarize(
    mean_srh = weighted.mean(mean_srh, n, na.rm = TRUE),
    .groups = "drop"
  )

cat("Period (Year) Effects:\n")
cat("  Earliest year mean SRH:", 
    round(period_summary$mean_srh[which.min(period_summary$year)], 3), "\n")
cat("  Latest year mean SRH:", 
    round(period_summary$mean_srh[which.max(period_summary$year)], 3), "\n")

# By age
age_summary <- srh_by_age_year %>%
  group_by(age) %>%
  summarize(
    mean_srh = weighted.mean(mean_srh, n, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nAge Effects:\n")
cat("  Youngest age mean SRH:", 
    round(age_summary$mean_srh[which.min(age_summary$age)], 3), "\n")
cat("  Oldest age mean SRH:", 
    round(age_summary$mean_srh[which.max(age_summary$age)], 3), "\n")

# By cohort
cohort_summary <- srh_by_age_year %>%
  group_by(cohort) %>%
  summarize(
    mean_srh = weighted.mean(mean_srh, n, na.rm = TRUE),
    n_cells = n(),
    .groups = "drop"
  ) %>%
  filter(n_cells >= 5)  # Only cohorts with enough data points

cat("\nCohort Effects (cohorts with 5+ observations):\n")
cat("  Earliest cohort mean SRH:", 
    round(cohort_summary$mean_srh[which.min(cohort_summary$cohort)], 3),
    "(born", min(cohort_summary$cohort), ")\n")
cat("  Latest cohort mean SRH:", 
    round(cohort_summary$mean_srh[which.max(cohort_summary$cohort)], 3),
    "(born", max(cohort_summary$cohort), ")\n")

cat("\n=== Plots saved ===\n")
cat("1. lexis_heatmap_basic.png - Basic heatmap\n")
cat("2. lexis_heatmap_with_cohorts.png - With cohort diagonal lines\n")
cat("3. lexis_heatmap_deviation.png - Deviation from grand mean\n")
cat("4. lexis_heatmap_binned.png - 5-year age bins\n")