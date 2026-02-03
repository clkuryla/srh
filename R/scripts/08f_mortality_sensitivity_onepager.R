# ==============================================================================
# 08f_mortality_sensitivity_onepager.R
# Mortality Sensitivity Analysis: Combined One-Pager Summary Figure
#
# Purpose:
#   Combine adjusted and stratified mortality results into a single
#   supplementary figure with three rows:
#     Row A: Adjusted models (Unadjusted, +Sex, +Education, +Race/Ethnicity)
#     Row B: Stratified by Sex (Male, Female) + Education (< HS, HS/Some, BA+)
#     Row C: Stratified by Race/Ethnicity (6 panels)
#
# Input:
#   output/sensitivity/mortality/mortality_adjusted_results_{date}.csv
#   output/sensitivity/mortality/mortality_stratified_results_{date}.csv
#
# Output:
#   output/sensitivity/mortality/fig_mort_sensitivity_onepager_draft_{date}.png
#   output/sensitivity/mortality/fig_mort_sensitivity_onepager.{png,pdf}
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)

# Source shared functions
source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))

# Set theme
theme_set(theme_srh())

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)

# Output directory
output_dir <- here("output", "sensitivity", "mortality")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# LOAD RESULTS
# ==============================================================================

message("\n=== Loading results ===\n")

# --- Adjusted results ---
adj_files <- list.files(
  output_dir,
  pattern = "mortality_adjusted_results_\\d+\\.csv$",
  full.names = TRUE
)
if (length(adj_files) == 0) stop("No adjusted results found. Run 08a first.")
adj_results <- read_csv(sort(adj_files, decreasing = TRUE)[1], show_col_types = FALSE)
message("Adjusted: ", nrow(adj_results), " rows")

# --- Stratified results ---
strat_files <- list.files(
  output_dir,
  pattern = "mortality_stratified_results_\\d+\\.csv$",
  full.names = TRUE
)
if (length(strat_files) == 0) stop("No stratified results found. Run 08c first.")
strat_results <- read_csv(sort(strat_files, decreasing = TRUE)[1], show_col_types = FALSE)
message("Stratified: ", nrow(strat_results), " rows")

# Age group ordering
age_levels <- c("18-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89")


# ==============================================================================
# HELPER: COMPACT HR PANEL
# ==============================================================================

# Smaller panels for the combined figure
create_compact_panel <- function(data,
                                  panel_title = NULL,
                                  show_legend = FALSE,
                                  base_size = 14,
                                  x_angle = 45) {

  if (is.null(data) || nrow(data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 4, color = "gray50") +
      theme_void() +
      labs(title = panel_title)
    return(p)
  }

  p <- ggplot(data, aes(
    x = start_year,
    y = hr,
    color = age_group,
    fill = age_group,
    group = age_group
  )) +
    geom_hline(yintercept = 1, linetype = "dashed",
               color = "gray50", linewidth = 0.4) +
    geom_ribbon(
      aes(ymin = conf_low, ymax = conf_high),
      alpha = 0.08, color = NA
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.2) +
    scale_color_age() +
    scale_fill_age() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    scale_y_log10() +
    labs(
      x = NULL,
      y = "HR per 1-unit SRH",
      color = "Age Group",
      title = panel_title
    ) +
    theme_srh(base_size = base_size) +
    theme(
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = x_angle, hjust = 1, vjust = 1,
                                  size = base_size - 3),
      axis.text.y = element_text(size = base_size - 3),
      axis.title.y = element_text(size = base_size - 1),
      legend.position = if (show_legend) "bottom" else "none",
      plot.margin = margin(4, 6, 4, 4)
    ) +
    guides(fill = "none")

  return(p)
}


# ==============================================================================
# ROW A: ADJUSTED MODELS
# ==============================================================================

message("\n=== Building Row A: Adjusted models ===\n")

adj_plot <- adj_results %>%
  filter(converged) %>%
  mutate(age_group = factor(age_group, levels = age_levels))

model_labels <- unique(adj_plot$model_label)
model_order <- c("Unadjusted", sort(setdiff(model_labels, "Unadjusted")))
model_order <- intersect(model_order, model_labels)

adj_panels <- lapply(model_order, function(ml) {
  create_compact_panel(
    adj_plot %>% filter(model_label == ml),
    panel_title = ml,
    show_legend = FALSE
  )
})

row_a <- wrap_plots(adj_panels, nrow = 1)


# ==============================================================================
# ROW B: STRATIFIED BY SEX + EDUCATION
# ==============================================================================

message("=== Building Row B: Sex + Education ===\n")

strat_plot <- strat_results %>%
  filter(converged) %>%
  mutate(age_group = factor(age_group, levels = age_levels))

# --- Sex panels ---
sex_order <- c("Male", "Female")
sex_panels <- lapply(sex_order, function(s) {
  d <- strat_plot %>% filter(strat_var == "sex", stratum == s)
  create_compact_panel(d, panel_title = s)
})

# --- Education panels ---
educ_order <- c("< HS", "HS/Some college", "College+")
educ_strata <- intersect(educ_order,
                          unique(strat_plot %>%
                                   filter(strat_var == "educ_3cat_f") %>%
                                   pull(stratum)))

educ_panels <- lapply(educ_strata, function(s) {
  d <- strat_plot %>% filter(strat_var == "educ_3cat_f", stratum == s)
  create_compact_panel(d, panel_title = s)
})

row_b <- wrap_plots(c(sex_panels, educ_panels), nrow = 1)


# ==============================================================================
# ROW C: STRATIFIED BY RACE/ETHNICITY
# ==============================================================================

message("=== Building Row C: Race/Ethnicity ===\n")

race_order <- c("NH White", "NH Black", "Hispanic",
                "NH Asian", "NH AIAN", "Other/Multi")
race_strata <- intersect(race_order,
                          unique(strat_plot %>%
                                   filter(strat_var == "race_includehisp_f") %>%
                                   pull(stratum)))

race_panels <- lapply(race_strata, function(s) {
  d <- strat_plot %>% filter(strat_var == "race_includehisp_f", stratum == s)
  create_compact_panel(d, panel_title = s)
})

row_c <- wrap_plots(race_panels, nrow = 1)


# ==============================================================================
# COMBINE ALL ROWS
# ==============================================================================

message("\n=== Combining into one-pager ===\n")

# Row labels as text annotations
label_a <- wrap_elements(
  full = grid::textGrob(
    "A. Covariate-Adjusted Models",
    x = 0.02, hjust = 0,
    gp = grid::gpar(fontsize = 16, fontface = "bold")
  )
)

label_b <- wrap_elements(
  full = grid::textGrob(
    "B. Stratified by Sex and Education",
    x = 0.02, hjust = 0,
    gp = grid::gpar(fontsize = 16, fontface = "bold")
  )
)

label_c <- wrap_elements(
  full = grid::textGrob(
    "C. Stratified by Race/Ethnicity",
    x = 0.02, hjust = 0,
    gp = grid::gpar(fontsize = 16, fontface = "bold")
  )
)

# Stack: label + row for each section
fig_onepager <- (
  label_a / row_a /
  label_b / row_b /
  label_c / row_c
) +
  plot_layout(
    heights = c(0.04, 1, 0.04, 1, 0.04, 1),
    guides = "collect"
  ) +
  plot_annotation(
    title = "SRH-Mortality Hazard Ratio: Sociodemographic Sensitivity Analysis",
    subtitle = "NHIS, 10-year rolling windows, survey-weighted Cox PH",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA)
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.5, "lines"),
    legend.key.width = unit(2, "lines")
  ) &
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 4, linewidth = 1.5)
    ),
    fill = "none"
  )


# ==============================================================================
# SAVE
# ==============================================================================

message("\n=== Saving one-pager ===\n")

# Draft version
ggsave(
  file.path(output_dir, paste0(
    "fig_mort_sensitivity_onepager_draft_", date_suffix, ".png"
  )),
  fig_onepager, width = 22, height = 22, dpi = 300, bg = "white"
)

# Final versions
ggsave(
  file.path(output_dir, "fig_mort_sensitivity_onepager.png"),
  fig_onepager, width = 22, height = 22, dpi = 300, bg = "white"
)
ggsave(
  file.path(output_dir, "fig_mort_sensitivity_onepager.pdf"),
  fig_onepager, width = 22, height = 22, bg = "white"
)

message("Saved: fig_mort_sensitivity_onepager.{png,pdf}")
message("\n=== One-pager complete ===")
