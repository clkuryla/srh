# ==============================================================================
# 08d_mortality_sensitivity_stratified_plots.R
# Mortality Sensitivity Analysis: Plots for Stratified Cox PH Results
#
# Purpose:
#   Load pre-computed stratified mortality results and create comparison
#   figures showing the SRH-mortality HR within each stratum of sex,
#   race/ethnicity, and education.
#
# Input:
#   output/sensitivity/mortality/mortality_stratified_results_{date}.csv
#
# Output:
#   output/sensitivity/mortality/fig_mort_stratified_{var}*.{png,pdf}
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

message("\n=== Loading stratified mortality results ===\n")

# Find the most recent results file
result_files <- list.files(
  output_dir,
  pattern = "mortality_stratified_results_\\d+\\.csv$",
  full.names = TRUE
)

if (length(result_files) == 0) {
  stop("No stratified mortality results found in ", output_dir,
       "\nRun 08c_mortality_sensitivity_stratified.R first.")
}

result_file <- sort(result_files, decreasing = TRUE)[1]
message("Loading: ", basename(result_file))

results <- read_csv(result_file, show_col_types = FALSE)

# Ensure age_group is an ordered factor
age_levels <- c("18-29", "30-39", "40-49", "50-59",
                "60-69", "70-79", "80-89")
results <- results %>%
  mutate(age_group = factor(age_group, levels = age_levels))

message(sprintf("  %d rows", nrow(results)))
message("  Stratification vars: ",
        paste(unique(results$strat_var), collapse = ", "))
message("  Strata: ",
        paste(unique(results$stratum), collapse = ", "))


# ==============================================================================
# HELPER: CREATE A SINGLE STRATUM HR PANEL
# ==============================================================================

create_stratum_panel <- function(data,
                                  panel_title = NULL,
                                  show_legend = FALSE,
                                  base_size = 22) {

  if (is.null(data) || nrow(data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 6, color = "gray50") +
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
               color = "gray50", linewidth = 0.5) +
    geom_ribbon(
      aes(ymin = conf_low, ymax = conf_high),
      alpha = 0.10, color = NA
    ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.0) +
    scale_color_age() +
    scale_fill_age() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_log10() +
    labs(
      x = "Window Start Year",
      y = "Hazard Ratio per 1-unit SRH",
      color = "Age Group",
      title = panel_title
    ) +
    theme_srh(base_size = base_size) +
    theme(
      plot.title = element_text(size = base_size, face = "bold",
                                hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = if (show_legend) "bottom" else "none"
    ) +
    guides(fill = "none")

  return(p)
}


# ==============================================================================
# FIGURE: STRATIFIED BY SEX
# ==============================================================================

if ("sex" %in% results$strat_var) {

  message("\n=== Creating stratified-by-sex figure ===\n")

  sex_data <- results %>%
    filter(strat_var == "sex", converged)

  sex_strata <- unique(sex_data$stratum)

  sex_panels <- lapply(sex_strata, function(s) {
    create_stratum_panel(
      sex_data %>% filter(stratum == s),
      panel_title = s,
      show_legend = FALSE
    )
  })

  fig_sex <- wrap_plots(sex_panels, ncol = 2) +
    plot_annotation(
      title = "SRH-Mortality HR: Stratified by Sex",
      subtitle = "NHIS, 10-year rolling windows, survey-weighted Cox PH",
      tag_levels = NULL,
      theme = theme(
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16, color = "gray40"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    ) +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 20),
      legend.text = element_text(size = 18),
      legend.key.size = unit(2, "lines"),
      legend.key.width = unit(2.5, "lines")
    ) &
    guides(
      color = guide_legend(
        nrow = 1,
        override.aes = list(size = 5, linewidth = 2)
      ),
      fill = "none"
    )

  ggsave(
    file.path(output_dir, paste0(
      "fig_mort_stratified_sex_draft_", date_suffix, ".png"
    )),
    fig_sex, width = 16, height = 8, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(output_dir, "fig_mort_stratified_sex.png"),
    fig_sex, width = 16, height = 8, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(output_dir, "fig_mort_stratified_sex.pdf"),
    fig_sex, width = 16, height = 8, bg = "white"
  )
  message("Saved: fig_mort_stratified_sex.{png,pdf}")
}


# ==============================================================================
# FIGURE: STRATIFIED BY EDUCATION
# ==============================================================================

if ("educ_3cat_f" %in% results$strat_var) {

  message("\n=== Creating stratified-by-education figure ===\n")

  educ_data <- results %>%
    filter(strat_var == "educ_3cat_f", converged)

  # Order: < HS, HS/Some college, College+
  educ_order <- c("< HS", "HS/Some college", "College+")
  educ_strata <- intersect(educ_order, unique(educ_data$stratum))

  educ_panels <- lapply(educ_strata, function(s) {
    create_stratum_panel(
      educ_data %>% filter(stratum == s),
      panel_title = s,
      show_legend = FALSE
    )
  })

  fig_educ <- wrap_plots(educ_panels, ncol = 3) +
    plot_annotation(
      title = "SRH-Mortality HR: Stratified by Education",
      subtitle = "NHIS, 10-year rolling windows, survey-weighted Cox PH",
      tag_levels = NULL,
      theme = theme(
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16, color = "gray40"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    ) +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 20),
      legend.text = element_text(size = 18),
      legend.key.size = unit(2, "lines"),
      legend.key.width = unit(2.5, "lines")
    ) &
    guides(
      color = guide_legend(
        nrow = 1,
        override.aes = list(size = 5, linewidth = 2)
      ),
      fill = "none"
    )

  ggsave(
    file.path(output_dir, paste0(
      "fig_mort_stratified_educ_draft_", date_suffix, ".png"
    )),
    fig_educ, width = 21, height = 8, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(output_dir, "fig_mort_stratified_educ.png"),
    fig_educ, width = 21, height = 8, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(output_dir, "fig_mort_stratified_educ.pdf"),
    fig_educ, width = 21, height = 8, bg = "white"
  )
  message("Saved: fig_mort_stratified_educ.{png,pdf}")
}


# ==============================================================================
# FIGURE: STRATIFIED BY RACE/ETHNICITY
# ==============================================================================

if ("race_includehisp_f" %in% results$strat_var) {

  message("\n=== Creating stratified-by-race figure ===\n")

  race_data <- results %>%
    filter(strat_var == "race_includehisp_f", converged)

  # Order by race_includehisp_f factor levels
  race_order <- c("NH White", "NH Black", "Hispanic",
                   "NH Asian", "NH AIAN", "Other/Multi")
  race_strata <- intersect(race_order, unique(race_data$stratum))

  race_panels <- lapply(race_strata, function(s) {
    create_stratum_panel(
      race_data %>% filter(stratum == s),
      panel_title = s,
      show_legend = FALSE
    )
  })

  n_race <- length(race_strata)
  ncol_race <- min(n_race, 3)
  nrow_race <- ceiling(n_race / ncol_race)

  fig_race <- wrap_plots(race_panels, ncol = ncol_race) +
    plot_annotation(
      title = "SRH-Mortality HR: Stratified by Race/Ethnicity",
      subtitle = "NHIS, 10-year rolling windows, survey-weighted Cox PH",
      tag_levels = NULL,
      theme = theme(
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16, color = "gray40"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    ) +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 20),
      legend.text = element_text(size = 18),
      legend.key.size = unit(2, "lines"),
      legend.key.width = unit(2.5, "lines")
    ) &
    guides(
      color = guide_legend(
        nrow = 1,
        override.aes = list(size = 5, linewidth = 2)
      ),
      fill = "none"
    )

  fig_width_race <- 7 * ncol_race
  fig_height_race <- 7 * nrow_race + 1.5

  ggsave(
    file.path(output_dir, paste0(
      "fig_mort_stratified_race_draft_", date_suffix, ".png"
    )),
    fig_race,
    width = fig_width_race, height = fig_height_race,
    dpi = 300, bg = "white"
  )
  ggsave(
    file.path(output_dir, "fig_mort_stratified_race.png"),
    fig_race,
    width = fig_width_race, height = fig_height_race,
    dpi = 300, bg = "white"
  )
  ggsave(
    file.path(output_dir, "fig_mort_stratified_race.pdf"),
    fig_race,
    width = fig_width_race, height = fig_height_race,
    bg = "white"
  )
  message("Saved: fig_mort_stratified_race.{png,pdf}")
}


# ==============================================================================
# VERIFICATION SUMMARY
# ==============================================================================

message("\n=== Verification summary ===\n")

conv_data <- results %>% filter(converged)

for (sv in unique(conv_data$strat_var)) {
  message(sprintf("\n--- %s ---", sv))
  strata_vals <- unique(
    conv_data %>% filter(strat_var == sv) %>% pull(stratum)
  )
  for (s in strata_vals) {
    sub <- conv_data %>% filter(strat_var == sv, stratum == s)
    hr_by_age <- sub %>%
      group_by(age_group) %>%
      summarise(mean_hr = mean(hr, na.rm = TRUE), .groups = "drop")

    message(sprintf("  %s (n=%d converged):", s, nrow(sub)))
    for (j in seq_len(nrow(hr_by_age))) {
      row <- hr_by_age[j, ]
      message(sprintf("    %s: mean HR = %.3f",
                      row$age_group, row$mean_hr))
    }
  }
}

message("\n=== Stratified plotting complete ===")
