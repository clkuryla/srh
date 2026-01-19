# ==============================================================================
# 03_figure3_alternative.R
# Alternative layout for Figure 3 & 3B: Age-stratified panels with category lines
#
# Layout: Rows = surveys (BRFSS, MEPS, NHIS)
#         Columns = age groups (18-29, ..., 80-89) via facet_wrap
#         Lines = 3 health categories (Comorbidity, Mental, Physical)
#
# This contrasts with the original layout:
#         Rows = health categories, Columns = surveys, Lines = age groups
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)

source(here("R", "functions", "theme_srh.R"))

# ------------------------------------------------------------------------------
# Load existing data tables
# ------------------------------------------------------------------------------

date_suffix <- "20260119"
tables_dir <- here("output", "tables")

message("Loading coefficient data...")
coef_all <- bind_rows(
  read_rds(file.path(tables_dir, paste0("fig3_coef_by_age_brfss_", date_suffix, ".rds"))),
  read_rds(file.path(tables_dir, paste0("fig3_coef_by_age_meps_", date_suffix, ".rds"))),
  read_rds(file.path(tables_dir, paste0("fig3_coef_by_age_nhis_", date_suffix, ".rds")))
)

message("Loading prevalence data...")
prev_all <- bind_rows(
  read_rds(file.path(tables_dir, paste0("fig3b_prev_by_age_brfss_", date_suffix, ".rds"))),
  read_rds(file.path(tables_dir, paste0("fig3b_prev_by_age_meps_", date_suffix, ".rds"))),
  read_rds(file.path(tables_dir, paste0("fig3b_prev_by_age_nhis_", date_suffix, ".rds")))
)

message("Coefficient data: ", nrow(coef_all), " rows")
message("Prevalence data: ", nrow(prev_all), " rows")

# ------------------------------------------------------------------------------
# Define category colors (from CLAUDE.md health domain colors)
# ------------------------------------------------------------------------------

category_colors <- c(
  "Comorbidity Count" = "#CC79A7",          # Reddish purple/pink
  "Mental Health" = "#56B4E9",              # Sky blue
  "Physical Health Limitations" = "#009E73" # Bluish green
)

# Define age group factor levels for consistent ordering
age_levels <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

# ------------------------------------------------------------------------------
# Rescale function: 0-1 within each survey × category
# ------------------------------------------------------------------------------

rescale_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# ------------------------------------------------------------------------------
# Prepare data: raw coefficients, scaled prevalence
# ------------------------------------------------------------------------------

message("Preparing coefficient data (raw values)...")
coef_prepared <- coef_all %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    survey = factor(survey, levels = c("BRFSS", "MEPS", "NHIS"))
  )

message("Rescaling prevalence data...")
prev_scaled <- prev_all %>%
  group_by(survey, category) %>%
  mutate(value_scaled = rescale_01(mean)) %>%
  ungroup() %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    survey = factor(survey, levels = c("BRFSS", "MEPS", "NHIS"))
  )

# ------------------------------------------------------------------------------
# Create subplot functions for survey rows
# ------------------------------------------------------------------------------

# For prevalence (scaled 0-1)
create_survey_row_scaled <- function(data, survey_name, show_strip = TRUE, show_ylabel = TRUE) {
  data_sub <- data %>%
    filter(survey == survey_name)

  p <- ggplot(data_sub, aes(x = year, y = value_scaled, color = category)) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    geom_point(size = 0.8, alpha = 0.6) +
    facet_wrap(~ age_group, nrow = 1, scales = "fixed") +
    scale_color_manual(values = category_colors, name = "Health Domain") +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(
      y = if (show_ylabel) survey_name else NULL,
      x = NULL
    ) +
    theme_srh() +
    theme(
      legend.position = "none",
      strip.text = if (show_strip) element_text(size = 9) else element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.title.y = element_text(size = 10, face = "bold"),
      panel.spacing = unit(0.3, "lines")
    )

  return(p)
}

# For coefficients (raw values)
create_survey_row_coef <- function(data, survey_name, show_strip = TRUE, show_ylabel = TRUE) {
  data_sub <- data %>%
    filter(survey == survey_name)

  p <- ggplot(data_sub, aes(x = year, y = coefficient, color = category)) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    geom_point(size = 0.8, alpha = 0.6) +
    facet_wrap(~ age_group, nrow = 1, scales = "fixed") +
    scale_color_manual(values = category_colors, name = "Health Domain") +
    labs(
      y = if (show_ylabel) survey_name else NULL,
      x = NULL
    ) +
    theme_srh() +
    theme(
      legend.position = "none",
      strip.text = if (show_strip) element_text(size = 9) else element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.title.y = element_text(size = 10, face = "bold"),
      panel.spacing = unit(0.3, "lines")
    )

  return(p)
}

# ------------------------------------------------------------------------------
# Create coefficient figure (3 rows: BRFSS, MEPS, NHIS) - RAW VALUES
# ------------------------------------------------------------------------------

message("Creating coefficient figure (raw values)...")

p_coef_brfss <- create_survey_row_coef(coef_prepared, "BRFSS", show_strip = TRUE, show_ylabel = TRUE)
p_coef_meps <- create_survey_row_coef(coef_prepared, "MEPS", show_strip = FALSE, show_ylabel = TRUE)
p_coef_nhis <- create_survey_row_coef(coef_prepared, "NHIS", show_strip = FALSE, show_ylabel = TRUE)

fig3_coef_alt <- p_coef_brfss / p_coef_meps / p_coef_nhis +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Coefficient of Health Measures on SRH by Age Group",
    subtitle = "Raw coefficient values"
  ) &
  theme(legend.position = "bottom")

# ------------------------------------------------------------------------------
# Create prevalence figure (3 rows: BRFSS, MEPS, NHIS)
# ------------------------------------------------------------------------------

message("Creating prevalence figure (scaled 0-1)...")

p_prev_brfss <- create_survey_row_scaled(prev_scaled, "BRFSS", show_strip = TRUE, show_ylabel = TRUE)
p_prev_meps <- create_survey_row_scaled(prev_scaled, "MEPS", show_strip = FALSE, show_ylabel = TRUE)
p_prev_nhis <- create_survey_row_scaled(prev_scaled, "NHIS", show_strip = FALSE, show_ylabel = TRUE)

fig3b_prev_alt <- p_prev_brfss / p_prev_meps / p_prev_nhis +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Prevalence/Mean of Health Measures by Age Group",
    subtitle = "Rescaled 0-1 within each survey for comparability"
  ) &
  theme(legend.position = "bottom")

# ------------------------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------------------------

date_stamp <- format(Sys.Date(), "%Y%m%d")
figures_dir <- here("output", "figures")

# Coefficient figure
message("Saving coefficient figure...")
ggsave(file.path(figures_dir, paste0("fig3_coef_by_age_alternative_", date_stamp, ".png")),
       fig3_coef_alt, width = 14, height = 8, dpi = 300)
ggsave(file.path(figures_dir, "fig3_coef_by_age_alternative.pdf"),
       fig3_coef_alt, width = 14, height = 8)
ggsave(file.path(figures_dir, "fig3_coef_by_age_alternative.png"),
       fig3_coef_alt, width = 14, height = 8, dpi = 300)

# Prevalence figure
message("Saving prevalence figure...")
ggsave(file.path(figures_dir, paste0("fig3b_prev_by_age_alternative_", date_stamp, ".png")),
       fig3b_prev_alt, width = 14, height = 8, dpi = 300)
ggsave(file.path(figures_dir, "fig3b_prev_by_age_alternative.pdf"),
       fig3b_prev_alt, width = 14, height = 8)
ggsave(file.path(figures_dir, "fig3b_prev_by_age_alternative.png"),
       fig3b_prev_alt, width = 14, height = 8, dpi = 300)

# Save data tables
message("Saving data tables...")
write_csv(coef_prepared, file.path(tables_dir, paste0("fig3_coef_alternative_", date_stamp, ".csv")))
write_csv(prev_scaled, file.path(tables_dir, paste0("fig3b_prev_alternative_scaled_", date_stamp, ".csv")))

message("Done! Outputs saved to:")
message("  - output/figures/fig3_coef_by_age_alternative.png/.pdf")
message("  - output/figures/fig3b_prev_by_age_alternative.png/.pdf")
message("  - output/tables/fig3_coef_alternative_", date_stamp, ".csv")
message("  - output/tables/fig3b_prev_alternative_scaled_", date_stamp, ".csv")
