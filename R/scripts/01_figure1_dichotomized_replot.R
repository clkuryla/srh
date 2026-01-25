# ==============================================================================
# 01_figure1_dichotomized_replot.R
# Regenerate Figure 1 Dichotomized plots from saved tables
# Uses Okabe-Ito colors and fixes margin for "Year" label
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)
library(grid)

source(here::here("R/paths.R"))
source(here::here("R/functions/theme_srh.R"))
source(here::here("R/functions/regress_srh_on_age.R"))

cat("Regenerating dichotomized SRH figures...\n")

# Output directories
fig_dir <- here::here("output", "sensitivity", "dichotomized", "figures")
tbl_dir <- here::here("output", "sensitivity", "dichotomized", "tables")

# Date suffix (use today's date for new figures)
draft_date <- format(Sys.Date(), "%Y%m%d")
# Date from saved tables
tbl_date <- "20260125"

# Survey order
survey_order <- c("BRFSS", "MEPS", "NHIS", "GSS", "CPS", "NHANES")

# Use Okabe-Ito colors
colors <- age_colors_oi

# Dichotomization info
dichotomizations <- list(
  good_plus = list(
    name = "good_plus",
    label = "Good or Better Health",
    description = "Good, Very Good, or Excellent vs Fair/Poor"
  ),
  excellent = list(
    name = "excellent",
    label = "Excellent Health",
    description = "Excellent vs all others"
  ),
  excellent_vgood = list(
    name = "excellent_vgood",
    label = "Excellent or Very Good Health",
    description = "Excellent or Very Good vs Good/Fair/Poor"
  )
)

# ==============================================================================
# PLOTTING FUNCTIONS (with fixed margins)
# ==============================================================================

create_panel_a_prevalence <- function(
    estimates, survey_name, colors,
    show_title = TRUE, show_ylabel = FALSE,
    base_size = 12, tilt_x_labels = 45
) {
  if (!is.factor(estimates$age_group)) {
    estimates$age_group <- factor(estimates$age_group, levels = unique(estimates$age_group))
  }

  p <- ggplot(estimates, aes(x = year, y = prevalence, color = age_group, group = age_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.0) +
    scale_color_manual(values = colors, name = "Age group") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    labs(
      title = if (show_title) survey_name else NULL,
      x = NULL,
      y = if (show_ylabel) "Prevalence" else NULL
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = element_text(angle = tilt_x_labels, hjust = 1, vjust = 1),
      plot.margin = margin(2, 4, 2, 4),
      legend.position = "bottom",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1)
    ) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}

create_panel_b_logistic <- function(
    coefficients, survey_name, meta_result = NULL,
    use_marginal = FALSE, show_ylabel = FALSE,
    base_size = 12, tilt_x_labels = 45
) {

  coef_col <- if (use_marginal) "marginal_effect" else "coefficient"
  se_col <- if (use_marginal) "marginal_se" else "se"

  plot_data <- coefficients %>%
    transmute(
      year = year,
      coefficient = .data[[coef_col]],
      se = .data[[se_col]]
    ) %>%
    filter(!is.na(coefficient)) %>%
    mutate(
      ci_lower = coefficient - 1.96 * se,
      ci_upper = coefficient + 1.96 * se
    )

  default_ylabel <- if (use_marginal) "Marginal effect (prob. per 10 years)" else "Age coefficient (log-odds)"

  p <- ggplot(plot_data, aes(x = year, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, color = "#3C5488", linewidth = 0.3) +
    geom_point(size = 1.5, color = "#3C5488")

  if (!is.null(meta_result)) {
    year_range <- range(plot_data$year)
    pred_data <- data.frame(year = seq(year_range[1], year_range[2], length.out = 100))
    pred_data$predicted <- meta_result$intercept + meta_result$slope * pred_data$year
    p <- p + geom_line(data = pred_data, aes(x = year, y = predicted), color = "#56B4E9", linewidth = 0.8)
  }

  p <- p +
    labs(x = NULL, y = if (show_ylabel) default_ylabel else NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = element_text(angle = tilt_x_labels, hjust = 1, vjust = 1),
      plot.margin = margin(2, 4, 2, 4)
    )

  return(p)
}

plot_combined <- function(
    prevalence_list, coefficients_list, meta_results_list,
    colors, use_marginal, title, subtitle, base_size = 14
) {

  survey_names <- names(prevalence_list)
  n_surveys <- length(survey_names)

  # Row 1: Prevalence
  row1_plots <- lapply(seq_along(survey_names), function(i) {
    create_panel_a_prevalence(
      prevalence_list[[survey_names[i]]], survey_names[i], colors,
      show_title = TRUE, show_ylabel = (i == 1), base_size = base_size
    )
  })

  # Row 2: Coefficients
  row2_plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]
    meta <- if (!is.null(meta_results_list) && svy %in% names(meta_results_list)) meta_results_list[[svy]] else NULL
    create_panel_b_logistic(
      coefficients_list[[svy]], svy, meta,
      use_marginal = use_marginal, show_ylabel = (i == 1), base_size = base_size
    )
  })

  row1 <- wrap_plots(row1_plots, ncol = n_surveys)
  row2 <- wrap_plots(row2_plots, ncol = n_surveys)

  # Shared x-axis label with extra bottom margin
  x_label_grob <- wrap_elements(
    grid::textGrob("Year", gp = grid::gpar(fontsize = base_size + 2))
  )

  # Stack with more space for x-label
  combined <- row1 / row2 / x_label_grob +
    plot_layout(heights = c(1, 1, 0.08))  # Increased from 0.05 to 0.08

  combined <- combined +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 5),
      legend.background = element_blank(),
      legend.title = element_text(size = base_size + 1, face = "bold"),
      legend.text = element_text(size = base_size),
      legend.key.size = unit(1.2, "lines")
    )

  if (!is.null(title) || !is.null(subtitle)) {
    combined <- combined +
      plot_annotation(
        title = title,
        subtitle = subtitle,
        theme = theme(
          plot.title = element_text(size = base_size + 4, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = base_size + 1, color = "gray40", hjust = 0.5),
          plot.margin = margin(10, 10, 10, 10)  # Increased bottom margin
        )
      )
  }

  return(combined)
}

# ==============================================================================
# REGENERATE FIGURES
# ==============================================================================

for (dichot_name in names(dichotomizations)) {

  dichot_spec <- dichotomizations[[dichot_name]]
  cat("\nProcessing:", dichot_spec$label, "\n")

  # Load saved tables
  prevalence_list <- lapply(survey_order, function(svy) {
    path <- file.path(tbl_dir, paste0("fig1_dichot_", dichot_name, "_prev_", tolower(svy), "_", tbl_date, ".rds"))
    readr::read_rds(path)
  })
  names(prevalence_list) <- survey_order

  coefficients_list <- lapply(survey_order, function(svy) {
    path <- file.path(tbl_dir, paste0("fig1_dichot_", dichot_name, "_coef_", tolower(svy), "_", tbl_date, ".rds"))
    readr::read_rds(path)
  })
  names(coefficients_list) <- survey_order

  # Load metaregression
  meta_path <- file.path(tbl_dir, paste0("fig1_dichot_", dichot_name, "_meta_", tbl_date, ".rds"))
  meta_all <- readr::read_rds(meta_path)

  # Split by coefficient type
  meta_logodds <- meta_all %>% filter(coefficient_type == "log_odds")
  meta_logodds_list <- split(meta_logodds, meta_logodds$survey)

  meta_marginal <- meta_all %>% filter(coefficient_type == "marginal")
  meta_marginal_list <- split(meta_marginal, meta_marginal$survey)

  # --- Log-odds figure ---
  fig_logodds <- plot_combined(
    prevalence_list, coefficients_list, meta_logodds_list,
    colors = colors,
    use_marginal = FALSE,
    title = paste0("Dichotomized SRH: ", dichot_spec$label),
    subtitle = dichot_spec$description
  )

  ggsave(file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_logodds.png")),
         fig_logodds, width = 16, height = 8, dpi = 300)
  ggsave(file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_logodds.pdf")),
         fig_logodds, width = 16, height = 8)
  cat("  Saved: fig1_dichot_", dichot_name, "_logodds.{png,pdf}\n")

  # --- Marginal figure ---
  fig_marginal <- plot_combined(
    prevalence_list, coefficients_list, meta_marginal_list,
    colors = colors,
    use_marginal = TRUE,
    title = paste0("Dichotomized SRH: ", dichot_spec$label),
    subtitle = paste0(dichot_spec$description, " (Marginal effects)")
  )

  ggsave(file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_marginal.png")),
         fig_marginal, width = 16, height = 8, dpi = 300)
  ggsave(file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_marginal.pdf")),
         fig_marginal, width = 16, height = 8)
  cat("  Saved: fig1_dichot_", dichot_name, "_marginal.{png,pdf}\n")

  # Draft with date
  ggsave(file.path(fig_dir, paste0("fig1_dichot_", dichot_name, "_draft_", draft_date, ".png")),
         fig_logodds, width = 16, height = 8, dpi = 300)
}

cat("\nDone! Figures regenerated with Okabe-Ito colors.\n")
