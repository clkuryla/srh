rm(list = ls())

library(tidyverse)
library(srvyr)
library(rlang)

# ---- 0) Load ----
raw <- read_csv("big_data/MEPS/meps_00001.csv", show_col_types = FALSE)

# These are exactly the measures you said are shifting in 2017 (from your plot legend)
vars_plot <- c("ADMALS","ADMWLM","ADPALS","ADPWLM","ADPAIN","ADSOCA")

# ---- 1) Prep function (build dataset for plotting) ----
prep_meps_for_plot <- function(dat, na_codes_for_plot = c(7,8,9)) {
  
  dat %>%
    transmute(
      MEPSID, YEAR, AGE, HEALTH, SAQWEIGHT, PSUANN, STRATANN,
      across(all_of(vars_plot), as.numeric)
    ) %>%
    filter(
      !is.na(HEALTH),
      !is.na(AGE),
      !is.na(SAQWEIGHT),
      AGE >= 18,
      AGE <= 85,
      HEALTH %in% 1:5
    ) %>%
    mutate(
      age_group = cut(
        AGE,
        breaks = c(17, 29, 39, 49, 59, 69, 79, 89),
        labels = c("18-29","30-39","40-49","50-59","60-69","70-79","80-89"),
        right = TRUE
      ),
      # Apply missing-code cleaning ONLY to the plot variables
      across(all_of(vars_plot), ~ replace(.x, .x %in% na_codes_for_plot, NA_real_))
    )
}

# Old version (what your code effectively does for these vars): only 7/8/9 -> NA
d_old <- prep_meps_for_plot(raw, na_codes_for_plot = c(7,8,9))

# New version: 6/7/8/9 -> NA (NIU + unknowns)
d_new <- prep_meps_for_plot(raw, na_codes_for_plot = c(6,7,8,9))

# ---- 2) HARD CHECK: Are there any 6s in the plotted vars after your filters? ----
# If this prints nothing (0 rows), then 6s were not present and recoding 6->NA cannot change your plot.
six_check <- d_old %>%
  pivot_longer(all_of(vars_plot), names_to = "var", values_to = "val") %>%
  group_by(YEAR, var) %>%
  summarise(
    n = n(),
    n6 = sum(val == 6, na.rm = TRUE),
    p6_unweighted = n6 / n,
    p6_weighted = weighted.mean(val == 6, w = SAQWEIGHT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n6 > 0) %>%
  arrange(var, YEAR)

print(six_check, n = 200)

# Also show the overall fraction of 6s across all years (weighted), by variable:
six_check_overall <- d_old %>%
  pivot_longer(all_of(vars_plot), names_to = "var", values_to = "val") %>%
  group_by(var) %>%
  summarise(
    p6_weighted_overall = weighted.mean(val == 6, w = SAQWEIGHT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(p6_weighted_overall))

print(six_check_overall)

# ---- 3) Survey design + trend summary ----
# IMPORTANT: use MEPS design PSU/strata (not MEPSID)
trend_svy <- function(dat) {
  des <- dat %>%
    as_survey_design(
      ids = PSUANN,
    #  strata = STRATANN,
      weights = SAQWEIGHT#,
     # nest = TRUE
    )
  
  map_dfr(vars_plot, function(v) {
    des %>%
      group_by(YEAR, age_group) %>%
      summarise(
        mean = survey_mean(!!sym(v), na.rm = TRUE, vartype = "ci"),
        .groups = "drop"
      ) %>%
      mutate(var = v)
  })
}

t_old <- trend_svy(d_old) %>% mutate(version = "old: NA {7,8,9}")
t_new <- trend_svy(d_new) %>% mutate(version = "new: NA {6,7,8,9}")

# ---- 4) Plot function (matches your faceted-by-age style) ----
plot_trends <- function(trend_df, title) {
  ggplot(trend_df, aes(x = YEAR, y = mean, color = var, fill = var, group = var)) +
    geom_ribbon(aes(ymin = mean_low, ymax = mean_upp), alpha = 0.20, colour = NA) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ age_group, nrow = 1) +
    labs(
      title = title,
      x = "Year",
      y = "Weighted mean (SAQWEIGHT; MEPS PSUANN/STRATANN)",
      color = "Measure",
      fill = "Measure"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# (A) Your "new cleaning" plot
p_new <- plot_trends(t_new, "MEPS 4-week items (new cleaning: 6/7/8/9 -> NA)")
print(p_new)

# ---- 5) Overlay old vs new (lines only, easier to see overlap) ----
t_both <- bind_rows(t_old, t_new)

p_overlay <- ggplot(t_both, aes(x = YEAR, y = mean, color = var, linetype = version)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ age_group, nrow = 1) +
  labs(
    title = "Overlay: old vs new cleaning (if lines overlap, recoding 6->NA did nothing)",
    x = "Year",
    y = "Weighted mean",
    color = "Measure",
    linetype = "Cleaning"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_overlay)

# ---- 6) Delta plot: (new - old) by year ----
delta <- t_new %>%
  select(YEAR, age_group, var, mean_new = mean) %>%
  left_join(
    t_old %>% select(YEAR, age_group, var, mean_old = mean),
    by = c("YEAR","age_group","var")
  ) %>%
  mutate(delta = mean_new - mean_old)

p_delta <- ggplot(delta, aes(x = YEAR, y = delta, color = var, group = var)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ age_group, nrow = 1) +
  labs(
    title = "Delta = (new cleaning) − (old cleaning)",
    x = "Year",
    y = "Difference in weighted means",
    color = "Measure"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_delta)
