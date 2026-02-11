# Quick script to regenerate Figure S2b from saved tables
# Run this after 03_sensitivity_sociodemog.R failed on S2b

library(tidyverse)
library(here)
library(patchwork)

source(here::here("R/functions/theme_srh.R"))

# Output directories
fig_dir <- here::here("output", "figures")
tables_dir <- here::here("output", "tables")
date_suffix <- format(Sys.Date(), "%Y%m%d")

# Age groups
AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
survey_order <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")

# Load the saved table
coef_byage_all <- readr::read_rds(
  file.path(tables_dir,
            paste0("figS2b_covariate_byage_coefficients_", date_suffix, ".rds"))
)

cat("Loaded ", nrow(coef_byage_all), " rows\n")

# Helper function
create_byage_panel <- function(data, survey_name, covariate_label, level_name,
                                show_title = FALSE, show_ylabel = FALSE) {

  plot_data <- data %>%
    filter(survey == survey_name,
           covariate_label == !!covariate_label,
           level == level_name)

  if (nrow(plot_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 3, color = "gray50") +
      theme_void() +
      labs(title = if (show_title) survey_name else NULL)
    return(p)
  }

  plot_data$age_group <- factor(plot_data$age_group, levels = AGE_GROUPS)

  p <- ggplot(plot_data, aes(x = year, y = coefficient,
                              color = age_group, group = age_group)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray50", linewidth = 0.4) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 1.2, alpha = 0.8) +
    scale_color_manual(values = age_colors_oi, name = "Age Group") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    labs(
      title = if (show_title) survey_name else NULL,
      x = NULL,
      y = if (show_ylabel) "Coefficient" else NULL
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.25),
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 7, color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(2, 3, 2, 3),
      legend.position = "none"
    )

  return(p)
}

# Build all panels for a 4-row x 6-column grid
# Row 1: Sex (Female)
# Row 2: Race (Black)
# Row 3: Race (Hispanic)
# Row 4: Education (BA+)

all_panels_b <- list()

# Row 1: Sex
for (i in seq_along(survey_order)) {
  svy <- survey_order[i]
  p <- create_byage_panel(coef_byage_all, svy, "Sex", "Female",
                          show_title = TRUE, show_ylabel = (i == 1))
  all_panels_b[[length(all_panels_b) + 1]] <- p
}

# Row 2: Black
for (i in seq_along(survey_order)) {
  svy <- survey_order[i]
  p <- create_byage_panel(coef_byage_all, svy, "Race/Ethnicity", "Black",
                          show_title = FALSE, show_ylabel = (i == 1))
  all_panels_b[[length(all_panels_b) + 1]] <- p
}

# Row 3: Hispanic
for (i in seq_along(survey_order)) {
  svy <- survey_order[i]
  p <- create_byage_panel(coef_byage_all, svy, "Race/Ethnicity", "Hispanic",
                          show_title = FALSE, show_ylabel = (i == 1))
  all_panels_b[[length(all_panels_b) + 1]] <- p
}

# Row 4: Education BA+
for (i in seq_along(survey_order)) {
  svy <- survey_order[i]
  p <- create_byage_panel(coef_byage_all, svy, "Education", "BA_plus",
                          show_title = FALSE, show_ylabel = (i == 1))
  all_panels_b[[length(all_panels_b) + 1]] <- p
}

# Combine all 24 panels in a 4x6 grid
panel_b <- wrap_plots(all_panels_b, ncol = 6, nrow = 4) +
  plot_annotation(
    title = "Panel B: Covariate Coefficient on SRH by Age Group",
    subtitle = "Row 1: Sex (Female vs Male) | Row 2: Black vs White | Row 3: Hispanic vs White | Row 4: BA+ vs LT HS",
    theme = theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0.5)
    )
  )

# Create legend
legend_plot <- ggplot(
  data.frame(age_group = factor(AGE_GROUPS, levels = AGE_GROUPS), y = 1:7),
  aes(x = 1, y = y, color = age_group)
) +
  geom_point(size = 3) +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  guides(color = guide_legend(nrow = 1)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11, face = "bold"))

legend_grob <- cowplot::get_legend(legend_plot)

panel_b_with_legend <- panel_b / wrap_elements(legend_grob) +
  plot_layout(heights = c(1, 0.08))

# Save
ggsave(
  filename = file.path(fig_dir,
                       paste0("figS2b_covariate_byage_draft_", date_suffix, ".png")),
  plot = panel_b_with_legend,
  width = 14, height = 12, dpi = 300
)
ggsave(
  filename = file.path(fig_dir, "figS2b_covariate_byage.png"),
  plot = panel_b_with_legend,
  width = 14, height = 12, dpi = 300
)
ggsave(
  filename = file.path(fig_dir, "figS2b_covariate_byage.pdf"),
  plot = panel_b_with_legend,
  width = 14, height = 12
)
cat("Saved: figS2b_covariate_byage (.png and .pdf)\n")

# Also create combined figure
cat("Loading Panel A data...\n")
coef_pooled_all <- readr::read_rds(
  file.path(tables_dir,
            paste0("figS2a_covariate_pooled_coefficients_", date_suffix, ".rds"))
)

# Recreate Panel A
create_pooled_panel <- function(data, survey_name, covariate_label,
                                 show_title = FALSE, show_ylabel = FALSE) {

  plot_data <- data %>%
    filter(survey == survey_name, covariate_label == !!covariate_label)

  if (nrow(plot_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 4, color = "gray50") +
      theme_void() +
      labs(title = if (show_title) survey_name else NULL)
    return(p)
  }

  n_levels <- length(unique(plot_data$level))

  if (n_levels > 1) {
    p <- ggplot(plot_data,
                aes(x = year, y = coefficient, color = level, group = level)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.5) +
      geom_line(linewidth = 0.8, alpha = 0.8) +
      geom_point(size = 1.5, alpha = 0.8) +
      scale_color_viridis_d(option = "D", end = 0.9) +
      labs(color = NULL)
  } else {
    p <- ggplot(plot_data, aes(x = year, y = coefficient)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.5) +
      geom_line(linewidth = 0.8, color = "#3C5488") +
      geom_point(size = 1.5, color = "#3C5488")
  }

  p <- p +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    labs(
      title = if (show_title) survey_name else NULL,
      x = NULL,
      y = if (show_ylabel) "Coefficient" else NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 8, color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(2, 4, 2, 4),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.6, "lines")
    )

  return(p)
}

sex_panels_a <- lapply(survey_order, function(svy) {
  create_pooled_panel(coef_pooled_all, svy, "Sex",
                      show_title = TRUE, show_ylabel = (svy == survey_order[1]))
})
row_sex_a <- wrap_plots(sex_panels_a, ncol = 6) +
  plot_annotation(subtitle = "Sex (Female vs Male)") &
  theme(legend.position = "none")

race_panels_a <- lapply(survey_order, function(svy) {
  create_pooled_panel(coef_pooled_all, svy, "Race/Ethnicity",
                      show_title = FALSE, show_ylabel = (svy == survey_order[1]))
})
row_race_a <- wrap_plots(race_panels_a, ncol = 6, guides = "collect") +
  plot_annotation(subtitle = "Race/Ethnicity (vs White)") &
  theme(legend.position = "bottom")

educ_panels_a <- lapply(survey_order, function(svy) {
  create_pooled_panel(coef_pooled_all, svy, "Education",
                      show_title = FALSE, show_ylabel = (svy == survey_order[1]))
})
row_educ_a <- wrap_plots(educ_panels_a, ncol = 6, guides = "collect") +
  plot_annotation(subtitle = "Education (vs Less than HS)") &
  theme(legend.position = "bottom")

panel_a <- (row_sex_a / row_race_a / row_educ_a) +
  plot_annotation(
    title = "Panel A: Covariate Coefficient on SRH Over Time (All Ages Pooled)",
    theme = theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  )

# Combined figure
fig_s2_combined <- (panel_a / panel_b_with_legend) +
  plot_annotation(
    title = "Figure S2: Sociodemographic Covariate Effects on SRH Over Time",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave(
  filename = file.path(fig_dir,
                       paste0("figS2_covariate_effects_draft_", date_suffix, ".png")),
  plot = fig_s2_combined,
  width = 14, height = 20, dpi = 300
)
ggsave(
  filename = file.path(fig_dir, "figS2_covariate_effects.png"),
  plot = fig_s2_combined,
  width = 14, height = 20, dpi = 300
)
ggsave(
  filename = file.path(fig_dir, "figS2_covariate_effects.pdf"),
  plot = fig_s2_combined,
  width = 14, height = 20
)
cat("Saved: figS2_covariate_effects (.png and .pdf)\n")

cat("\nDone!\n")
