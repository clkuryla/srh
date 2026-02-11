# ==============================================================================
# supp_sensitivity_stacked.R
# Sensitivity analysis: Stacked area grid with alternate survey ordering
#
# Produces a 6 (datasets) x 7 (age groups) faceted grid of stacked area charts
# showing SRH Distribution Composition by Age Group Over Time.
#
# Same as C2b but with survey order: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
# (GSS moved to last row). Legend shows all 5 SRH levels (sourced from NHANES
# row rather than GSS row, since GSS only has 4 levels).
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(here)
library(dplyr)
library(ggplot2)
library(patchwork)
library(forcats)

# Source shared functions
source(here::here("R", "functions", "theme_srh.R"))
source(here::here("R", "functions", "plot_categorical_combined.R"))

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Sensitivity: Stacked area grid (alternate survey order)\n")
cat("========================================\n")

props_all <- readRDS(here::here("output", "tables", "supp_srh_props_all.rds"))
cat(sprintf("Loaded props: %d rows, surveys: %s\n",
            nrow(props_all),
            paste(unique(props_all$survey), collapse = ", ")))

# ------------------------------------------------------------------------------
# Re-level survey factor: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
# ------------------------------------------------------------------------------

sensitivity_survey_order <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
props_sensitivity <- props_all %>%
  mutate(survey = factor(survey, levels = sensitivity_survey_order))

cat(sprintf("Survey order: %s\n", paste(levels(props_sensitivity$survey), collapse = ", ")))

# ------------------------------------------------------------------------------
# Create output directories
# ------------------------------------------------------------------------------

fig_dir <- here::here("output", "sensitivity", "categorical", "figures")
tbl_dir <- here::here("output", "sensitivity", "categorical", "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tbl_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Build stacked area grid inline
# (Based on plot_stacked_combined logic, but with legend on NHANES row
# instead of GSS row, so all 5 SRH levels appear in the legend)
# ------------------------------------------------------------------------------

cat("\n=== Generating sensitivity stacked area grid ===\n")

base_sz <- 16
area_alpha <- 1.0
tilt_x <- 45

# Ensure factor levels
data <- props_sensitivity
if (!is.factor(data$age_group)) {
  data$age_group <- factor(data$age_group, levels = age_group_order)
}
if (!is.factor(data$srh_cat)) {
  data$srh_cat <- factor(data$srh_cat, levels = srh_cat_order_5pt)
}
data <- data %>% mutate(prop_pct = prop * 100)

surveys <- levels(data$survey)
age_groups <- levels(data$age_group)
n_surveys <- length(surveys)
n_age_groups <- length(age_groups)

# Row index for legend: NHANES (5-point scale, has all 5 levels)
legend_row <- which(surveys == "NHANES")

plot_list <- list()

for (i in seq_along(surveys)) {
  svy <- surveys[i]
  svy_data <- data %>% filter(survey == svy)
  # Use 5-point palette for all surveys so "Good" is always yellow;
  # GSS data simply won't use the "Very Good" color
  colors <- srh_cat_colors

  for (j in seq_along(age_groups)) {
    ag <- age_groups[j]
    cell_data <- svy_data %>% filter(age_group == ag)

    if (nrow(cell_data) == 0) {
      plot_list[[length(plot_list) + 1]] <- plot_spacer()
      next
    }

    # Keep original factor order: Poor (bottom) → Excellent (top)
    # so green "Excellent" band is clearly visible at top of stack

    # Show legend on last cell of the NHANES row (5-point, all levels)
    show_legend <- (i == legend_row) && (j == n_age_groups)

    p <- ggplot(cell_data, aes(x = year, y = prop_pct, fill = srh_cat)) +
      geom_area(alpha = area_alpha, position = "stack", color = NA) +
      scale_y_continuous(
        labels = function(x) paste0(round(x), "%"),
        expand = expansion(mult = c(0, 0))
      ) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(
        title = if (i == 1) ag else NULL,
        y = if (j == 1) svy else NULL,
        x = NULL
      ) +
      theme_minimal(base_size = base_sz) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.3),
        plot.title = element_text(size = base_sz, face = "bold", hjust = 0.5),
        axis.title = element_text(size = base_sz - 1),
        axis.title.y = element_text(margin = margin(r = 2)),
        axis.text = element_text(size = base_sz - 2, color = "gray30"),
        axis.text.x = element_text(size = base_sz - 2, angle = tilt_x, hjust = 1, vjust = 1),
        plot.margin = margin(2, 3, 2, 3),
        legend.position = if (show_legend) "bottom" else "none"
      )

    if (show_legend) {
      p <- p +
        scale_fill_manual(values = colors, name = "SRH Category") +
        guides(fill = guide_legend(nrow = 1)) +
        theme(
          legend.title = element_text(size = base_sz, face = "bold"),
          legend.text = element_text(size = base_sz - 1)
        )
    } else {
      p <- p + scale_fill_manual(values = colors, guide = "none")
    }

    plot_list[[length(plot_list) + 1]] <- p
  }
}

# Combine into grid
grid <- wrap_plots(plot_list, ncol = n_age_groups, nrow = n_surveys, byrow = TRUE)

# Shared x-axis label
x_label <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Year", size = (base_sz + 2) / .pt) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

p_final <- grid / x_label +
  plot_layout(heights = c(1, 0.02), guides = "collect") &
  theme(legend.position = "bottom")

# Add title
p_final <- p_final +
  plot_annotation(
    title = "SRH Distribution Composition by Age Group Over Time",
    theme = theme(
      plot.title = element_text(size = base_sz + 3, face = "bold", hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    )
  )

cat("Grid generated with 5-level legend (from NHANES row)\n")

# ------------------------------------------------------------------------------
# Save figures
# ------------------------------------------------------------------------------

ggsave(
  file.path(fig_dir, "supp_stacked_sensitivity.png"),
  p_final, width = 18, height = 14, dpi = 300
)
ggsave(
  file.path(fig_dir, "supp_stacked_sensitivity.pdf"),
  p_final, width = 18, height = 14
)

cat(sprintf("Saved figures to: %s\n", fig_dir))

# ------------------------------------------------------------------------------
# Save tables
# ------------------------------------------------------------------------------

saveRDS(props_sensitivity, file.path(tbl_dir, "supp_srh_props_sensitivity.rds"))
readr::write_csv(props_sensitivity, file.path(tbl_dir, "supp_srh_props_sensitivity.csv"))

cat(sprintf("Saved tables to: %s\n", tbl_dir))

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Sensitivity stacked area grid complete!\n")
cat("========================================\n")
cat(sprintf("Survey order: %s\n", paste(sensitivity_survey_order, collapse = ", ")))
cat(sprintf("Age groups:   %s\n", paste(age_group_order, collapse = ", ")))
cat("\nOutputs:\n")
cat(sprintf("  %s/supp_stacked_sensitivity.{png,pdf}\n", fig_dir))
cat(sprintf("  %s/supp_srh_props_sensitivity.{csv,rds}\n", tbl_dir))
