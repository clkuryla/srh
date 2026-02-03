# ==============================================================================
# 06_nhis_functional_limitations_selected.R
# NHIS Functional Limitations: Selected Variables (6-panel figure)
#
# Selected variables:
#   1. Bathing/Dressing Difficulty (lawashdresdif)
#   2. Climb 12 Steps Difficulty (walkdif12st1)
#   3. Hand/Finger Difficulty (lahanddif)
#   4. Lift/Carry 25lbs Difficulty (lara2litrdif)
#   5. Walk 1 Block Difficulty (walkdif1bl1)
#   6. Walk/Climb Permanent Limitation (lawalkclimperq)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(srvyr)
library(here)

source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "prevalence_by_age_year.R"))

theme_set(theme_srh())
options(dplyr.summarise.inform = FALSE)

output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")
date_suffix <- format(Sys.Date(), "%Y%m%d")

AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
MIN_YEAR <- 2010

# ------------------------------------------------------------------------------
# SELECTED VARIABLES
# ------------------------------------------------------------------------------

# Binary prevalence versions (% with any difficulty)
selected_vars_prev <- c(
  "lawashdresdif_prev"  = "Bathing/Dressing Difficulty",
  "walkdif12st1_prev"   = "Climb 12 Steps Difficulty",
  "lahanddif_prev"      = "Hand/Finger Difficulty",
  "lara2litrdif_prev"   = "Lift/Carry 25lbs Difficulty",
  "walkdif1bl1_prev"    = "Walk 1 Block Difficulty",
  "lawalkclimperq_prev" = "Walk/Climb Permanent Limitation"
)

# Ordinal mean versions
selected_vars_mean <- c(
  "lawashdresdif"  = "Bathing/Dressing Difficulty",
  "walkdif12st1"   = "Climb 12 Steps Difficulty",
  "lahanddif"      = "Hand/Finger Difficulty",
  "lara2litrdif"   = "Lift/Carry 25lbs Difficulty",
  "walkdif1bl1"    = "Walk 1 Block Difficulty",
  "lawalkclimperq" = "Walk/Climb Permanent Limitation"
)

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

message("\n========== Loading NHIS data ==========\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

message("NHIS: ", nrow(data_nhis), " rows, years ",
        min(data_nhis$year), "-", max(data_nhis$year))

# Add age group if not present
if (!"age_group" %in% names(data_nhis)) {
  source(here("R", "srh_common_functions.R"))
  data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")
}

data_nhis <- data_nhis %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS))

# ------------------------------------------------------------------------------
# CREATE BINARY VERSIONS FOR PREVALENCE
# ------------------------------------------------------------------------------

message("\n========== Creating binary versions for prevalence ==========\n")

# Ordinal variables: 1 = No difficulty, >1 = Any difficulty
ordinal_vars <- c("lawashdresdif", "walkdif12st1", "lahanddif",
                  "lara2litrdif", "walkdif1bl1")

for (v in ordinal_vars) {
  if (v %in% names(data_nhis)) {
    new_var <- paste0(v, "_prev")
    data_nhis[[new_var]] <- ifelse(data_nhis[[v]] > 1, 1,
                                    ifelse(data_nhis[[v]] == 1, 0, NA_real_))
    n_valid <- sum(!is.na(data_nhis[[new_var]]))
    message("  Created ", new_var, ": ", n_valid, " valid obs")
  }
}

# lawalkclimperq is already binary (0/1)
if ("lawalkclimperq" %in% names(data_nhis)) {
  data_nhis$lawalkclimperq_prev <- data_nhis$lawalkclimperq
  message("  Copied lawalkclimperq_prev (already binary)")
}

# ------------------------------------------------------------------------------
# COMPUTE PREVALENCE ESTIMATES
# ------------------------------------------------------------------------------

message("\n========== Computing prevalence estimates ==========\n")

compute_prevalence <- function(data, var_list) {
  results <- list()

  for (i in seq_along(var_list)) {
    var_name <- names(var_list)[i]
    var_label <- var_list[i]

    message("\n--- Processing: ", var_name, " ---")

    if (!var_name %in% names(data)) {
      message("  Variable not found. Skipping.")
      next
    }

    valid_years <- data %>%
      filter(!is.na(.data[[var_name]])) %>%
      pull(year) %>%
      unique() %>%
      sort()

    if (length(valid_years) == 0) {
      message("  No valid data. Skipping.")
      next
    }

    message("  Years with data: ", min(valid_years), "-", max(valid_years))

    result <- mean_by_age_year(
      data = data,
      var_name = var_name,
      var_label = var_label,
      survey_name = "NHIS",
      psu_var = "psu",
      strata_var = "strata",
      wt_var = "wt",
      min_n = 50
    )

    if (!is.null(result) && nrow(result) > 0) {
      results[[var_name]] <- result
    }
  }

  if (length(results) > 0) {
    bind_rows(results)
  } else {
    NULL
  }
}

# Compute prevalence
prev_selected <- compute_prevalence(data_nhis, selected_vars_prev)

# Compute means
mean_selected <- compute_prevalence(data_nhis, selected_vars_mean)

# ------------------------------------------------------------------------------
# FILTER TO MIN_YEAR AND PREPARE DATA
# ------------------------------------------------------------------------------

message("\n========== Filtering to ", MIN_YEAR, "+ ==========\n")

prev_selected <- prev_selected %>%
  filter(year >= MIN_YEAR) %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    variable_label = factor(variable_label, levels = selected_vars_prev)
  )

mean_selected <- mean_selected %>%
  filter(year >= MIN_YEAR) %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    variable_label = factor(variable_label, levels = selected_vars_mean)
  )

message("Prevalence: ", nrow(prev_selected), " estimates")
message("Mean: ", nrow(mean_selected), " estimates")

# ------------------------------------------------------------------------------
# CREATE PREVALENCE FIGURE (6-panel, 3x2 layout)
# ------------------------------------------------------------------------------

message("\n========== Creating prevalence figure ==========\n")

fig_prev <- ggplot(prev_selected, aes(x = year, y = mean,
                                       color = age_group, group = age_group)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.5, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    title = "Prevalence of Functional Limitations by Age Group Over Time (NHIS)",
    subtitle = "Survey-weighted estimates | % with any difficulty | 2010-2024",
    x = "Year",
    y = "Prevalence (% with any difficulty)",
    caption = paste0("Source: NHIS | Generated: ", Sys.Date())
  ) +
  theme_srh(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 1))

# ------------------------------------------------------------------------------
# CREATE MEAN FIGURE (6-panel, 3x2 layout)
# ------------------------------------------------------------------------------

message("\n========== Creating mean figure ==========\n")

fig_mean <- ggplot(mean_selected, aes(x = year, y = mean,
                                       color = age_group, group = age_group)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.5, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 3, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  labs(
    title = "Mean Functional Limitation Severity by Age Group Over Time (NHIS)",
    subtitle = "Survey-weighted estimates | Ordinal scale mean | 2010-2024",
    x = "Year",
    y = "Mean Value (ordinal scale)",
    caption = paste0("Source: NHIS | Generated: ", Sys.Date())
  ) +
  theme_srh(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 1))

# ------------------------------------------------------------------------------
# SAVE FIGURES
# ------------------------------------------------------------------------------

message("\n========== Saving figures ==========\n")

# Prevalence
ggsave(file.path(output_dir, paste0("nhis_limitations_selected_draft_", date_suffix, ".png")),
       fig_prev, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "nhis_limitations_selected.png"),
       fig_prev, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "nhis_limitations_selected.pdf"),
       fig_prev, width = 12, height = 8)
message("Saved: nhis_limitations_selected.png/.pdf (prevalence)")

# Mean
ggsave(file.path(output_dir, paste0("nhis_limitations_selected_mean_draft_", date_suffix, ".png")),
       fig_mean, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "nhis_limitations_selected_mean.png"),
       fig_mean, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_dir, "nhis_limitations_selected_mean.pdf"),
       fig_mean, width = 12, height = 8)
message("Saved: nhis_limitations_selected_mean.png/.pdf (mean)")

# ------------------------------------------------------------------------------
# SAVE TABLES
# ------------------------------------------------------------------------------

message("\n========== Saving tables ==========\n")

readr::write_csv(prev_selected,
  file.path(tables_dir, paste0("nhis_limitations_selected_prev_", date_suffix, ".csv")))
readr::write_rds(prev_selected,
  file.path(tables_dir, paste0("nhis_limitations_selected_prev_", date_suffix, ".rds")))

readr::write_csv(mean_selected,
  file.path(tables_dir, paste0("nhis_limitations_selected_mean_", date_suffix, ".csv")))
readr::write_rds(mean_selected,
  file.path(tables_dir, paste0("nhis_limitations_selected_mean_", date_suffix, ".rds")))

message("Saved tables with suffix: ", date_suffix)

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

message("\n========== Summary ==========\n")
message("Selected 6 variables:")
for (v in names(selected_vars_prev)) {
  message("  - ", selected_vars_prev[v])
}
message("\nYear range: ", MIN_YEAR, "-", max(prev_selected$year))
message("Layout: 3 columns x 2 rows")
message("\nOutput files:")
message("  - nhis_limitations_selected.png/.pdf (prevalence)")
message("  - nhis_limitations_selected_mean.png/.pdf (mean)")

message("\n========== Done ==========\n")
