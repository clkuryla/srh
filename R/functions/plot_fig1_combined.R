# ==============================================================================
# plot_fig1_combined.R
# Figure 1 Combined: Panel A (Mean SRH) over Panel B (Age Coefficient)
#
# Purpose: Create 2x6 figure where each column is a survey, with Panel A on top
#          and Panel B below. This allows direct visual comparison of the two
#          aspects of convergence for each survey.
#
# Layout:
#   Row 1: Mean SRH by age group (Panel A) - 6 surveys
#   Row 2: Age coefficient on SRH (Panel B) - 6 surveys (same order)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)

# Source dependencies (caller should source, but provide fallback)
if (!exists("age_colors")) {
  source(here::here("R", "functions", "theme_srh.R"))
}

# ------------------------------------------------------------------------------
# INDIVIDUAL PANEL FUNCTIONS (simplified for combined figure)
# ------------------------------------------------------------------------------

#' Create a Panel A subplot for one survey (for combined figure)
#'
#' @param estimates Data frame with age_group, year, mean_srh columns
#' @param survey_name Survey name for title
#' @param colors Age group color palette
#' @param show_title Show survey name as title? (TRUE for top row)
#' @param show_ylabel Show y-axis label? (TRUE for leftmost column only)
#' @param show_xlabel Show x-axis label? (FALSE for top row)
#' @param show_legend Keep legend elements? (TRUE, collected by patchwork)
#' @param base_size Base font size for the plot
#' @param tilt_x_labels Angle to tilt x-axis labels (0 = horizontal, 45 = tilted)
#'
#' @return ggplot object
#'
create_panel_a_subplot <- function(
    estimates,
    survey_name,
    colors = NULL,
    show_title = TRUE,
    show_ylabel = FALSE,
    show_x_axis_title = FALSE,
    show_x_tick_labels = TRUE,
    show_legend = TRUE,
    show_ci = FALSE,
    line_width = 0.6,
    point_size = 1.0,
    base_size = 12,
    tilt_x_labels = 0
) {

  if (is.null(colors)) colors <- age_colors

  # Ensure age_group is factor
  if (!is.factor(estimates$age_group)) {
    estimates$age_group <- factor(estimates$age_group,
                                   levels = unique(estimates$age_group))
  }

  p <- ggplot(estimates, aes(x = year, y = mean_srh,
                              color = age_group,
                              group = age_group)) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    scale_color_manual(values = colors, name = "Age group")

  # Add CI ribbons if requested
  if (show_ci && all(c("ci_lower", "ci_upper") %in% names(estimates))) {
    p <- p +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = age_group),
                  alpha = 0.15, color = NA) +
      scale_fill_manual(values = colors, guide = "none")
  }

  # Labels (x-axis title controlled separately from tick labels)
  p <- p + labs(
    title = if (show_title) survey_name else NULL,
    x = if (show_x_axis_title) "Year" else NULL,
    y = if (show_ylabel) "Weighted mean SRH" else NULL
  )

  # X-axis tick label styling (tilted or horizontal)
  x_tick_style <- if (!show_x_tick_labels) {
    element_blank()
  } else if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = x_tick_style,
      axis.ticks.x = if (!show_x_tick_labels) element_blank() else element_line(),
      plot.margin = margin(2, 4, 2, 4),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1)
    ) +
    guides(color = guide_legend(nrow = 1, override.aes = list(size = 4, linewidth = 1.5)))

  return(p)
}


#' Create a Panel B subplot for one survey (for combined figure)
#'
#' @param coefficients Data frame with year, coefficient, ci_lower, ci_upper
#' @param survey_name Survey name (not used as title in combined figure)
#' @param meta_result Optional metaregression result for trend line
#' @param show_title Show survey name as title? (FALSE for bottom row)
#' @param show_ylabel Show y-axis label? (TRUE for leftmost column only)
#' @param show_x_axis_title Show "Year" axis title? (FALSE, use shared label)
#' @param show_x_tick_labels Show year numbers on x-axis? (TRUE)
#' @param show_metareg Show metaregression trend line?
#' @param base_size Base font size for the plot
#' @param tilt_x_labels Angle to tilt x-axis labels (0 = horizontal, 45 = tilted)
#'
#' @return ggplot object
#'
create_panel_b_subplot <- function(
    coefficients,
    survey_name,
    meta_result = NULL,
    show_title = FALSE,
    show_ylabel = FALSE,
    show_x_axis_title = FALSE,
    show_x_tick_labels = TRUE,
    show_metareg = TRUE,
    point_color = "#3C5488",
    line_color = "#56B4E9",
    point_size = 1.5,
    line_width = 0.8,
    base_size = 12,
    tilt_x_labels = 0
) {

  has_ci <- all(c("ci_lower", "ci_upper") %in% names(coefficients))

  p <- ggplot(coefficients, aes(x = year, y = coefficient)) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4)

  # Error bars if available
  if (has_ci) {
    p <- p + geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.3,
      color = point_color,
      linewidth = 0.3
    )
  }

  # Points
  p <- p + geom_point(size = point_size, color = point_color)

  # Metaregression line
  if (show_metareg && !is.null(meta_result)) {
    year_range <- range(coefficients$year)
    pred_data <- data.frame(
      year = seq(year_range[1], year_range[2], length.out = 100)
    )
    pred_data$predicted <- meta_result$intercept + meta_result$slope * pred_data$year

    p <- p + geom_line(
      data = pred_data,
      aes(x = year, y = predicted),
      color = line_color,
      linewidth = line_width
    )
  }

  # Labels (x-axis title controlled separately from tick labels)
  p <- p + labs(
    title = if (show_title) survey_name else NULL,
    x = if (show_x_axis_title) "Year" else NULL,
    y = if (show_ylabel) "Age coefficient on SRH" else NULL
  )

  # X-axis tick label styling (tilted or horizontal)
  x_tick_style <- if (!show_x_tick_labels) {
    element_blank()
  } else if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = x_tick_style,
      axis.ticks.x = if (!show_x_tick_labels) element_blank() else element_line(),
      plot.margin = margin(2, 4, 2, 4)
    )

  return(p)
}


# ------------------------------------------------------------------------------
# COMBINED 2x6 FIGURE
# ------------------------------------------------------------------------------

#' Create combined Figure 1 with Panel A over Panel B (2x6 layout)
#'
#' @description
#' Creates a 2-row by 6-column figure where:
#' - Row 1: Panel A content (weighted mean SRH by age group over time)
#' - Row 2: Panel B content (age coefficient on SRH over time)
#' - Columns: Each survey (BRFSS, MEPS, NHIS, GSS, CPS, NHANES)
#'
#' @param estimates_list Named list of Panel A estimates (from summarize_srh_over_time)
#' @param coefficients_list Named list of Panel B coefficients (from regress_age_coefficient_by_year)
#' @param meta_results_list Optional named list of metaregression results for Panel B
#' @param colors Age group color palette for Panel A
#' @param show_ci Show confidence intervals in Panel A? Default FALSE
#' @param show_metareg Show metaregression lines in Panel B? Default TRUE
#' @param title Overall figure title. Default NULL (no title)
#' @param subtitle Overall figure subtitle. Default NULL
#' @param base_size Base font size for all text. Default 12
#' @param tilt_x_labels Angle to tilt x-axis labels (0 = horizontal, 45 = tilted). Default 0
#' @param metareg_line_color Color for metaregression trend line. Default "#56B4E9" (sky blue)
#'
#' @return A patchwork object
#'
#' @details
#' The surveys should be in the same order in both lists. The column order
#' in the figure follows the order in the lists.
#'
#' Y-axis labels appear only on the leftmost column of each row.
#' X-axis label "Year" appears only on the bottom row.
#' Survey names appear as titles on the top row only.
#' Age group legend appears at the bottom.
#'
#' @examples
#' # fig1_combined <- plot_fig1_combined(
#' #   estimates_list = list(BRFSS = ..., MEPS = ..., ...),
#' #   coefficients_list = list(BRFSS = ..., MEPS = ..., ...),
#' #   meta_results_list = list(BRFSS = ..., MEPS = ..., ...),
#' #   title = "Self Rated Health and Age Over Time",
#' #   base_size = 14,
#' #   tilt_x_labels = 45
#' # )
#' # ggsave("fig1_combined.png", fig1_combined, width = 16, height = 8)
#'
plot_fig1_combined <- function(
    estimates_list,
    coefficients_list,
    meta_results_list = NULL,
    colors = NULL,
    show_ci = FALSE,
    show_metareg = TRUE,
    title = NULL,
    subtitle = NULL,
    base_size = 12,
    tilt_x_labels = 45,
    metareg_line_color = "#56B4E9"
) {

  # --- Input validation ---
  stopifnot(is.list(estimates_list), is.list(coefficients_list))

  if (is.null(names(estimates_list)) || is.null(names(coefficients_list))) {
    stop("Both lists must be named (names = survey names)")
  }

  # Check that survey names match
  if (!identical(names(estimates_list), names(coefficients_list))) {
    warning("Survey names in estimates_list and coefficients_list don't match exactly.")
  }

  # Use default colors if not provided
  if (is.null(colors)) colors <- age_colors

  # --- Get survey order ---
  survey_names <- names(estimates_list)
  n_surveys <- length(survey_names)

  # --- Create Row 1: Panel A subplots ---
  # Show year tick labels on all plots, but no axis title (shared below)
  row1_plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]
    create_panel_a_subplot(
      estimates = estimates_list[[svy]],
      survey_name = svy,
      colors = colors,
      show_title = TRUE,
      show_ylabel = (i == 1),
      show_x_axis_title = FALSE,
      show_x_tick_labels = TRUE,
      show_legend = TRUE,
      show_ci = show_ci,
      base_size = base_size,
      tilt_x_labels = tilt_x_labels
    )
  })
  names(row1_plots) <- survey_names

  # --- Create Row 2: Panel B subplots ---
  # Show year tick labels on all plots, but no axis title (shared below)
  row2_plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]

    meta_result <- if (!is.null(meta_results_list) && svy %in% names(meta_results_list)) {
      meta_results_list[[svy]]
    } else {
      NULL
    }

    create_panel_b_subplot(
      coefficients = coefficients_list[[svy]],
      survey_name = svy,
      meta_result = meta_result,
      show_title = FALSE,
      show_ylabel = (i == 1),
      show_x_axis_title = FALSE,
      show_x_tick_labels = TRUE,
      show_metareg = show_metareg,
      line_color = metareg_line_color,
      base_size = base_size,
      tilt_x_labels = tilt_x_labels
    )
  })
  names(row2_plots) <- survey_names

  # --- Assemble the grid ---
  # Row 1: Panel A plots side by side
  row1 <- wrap_plots(row1_plots, ncol = n_surveys)

  # Row 2: Panel B plots side by side
  row2 <- wrap_plots(row2_plots, ncol = n_surveys)

  # --- Create shared x-axis label as a grob ---
  x_label_grob <- wrap_elements(
    grid::textGrob(
      "Year",
      gp = grid::gpar(fontsize = base_size + 2)
    )
  )

  # Stack rows: Panel A / Panel B / x-label
  combined <- row1 / row2 / x_label_grob +
    plot_layout(heights = c(1, 1, 0.05))

  # --- Configure layout ---
  # Collect guides (legend) at bottom
  combined <- combined +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10),
      legend.background = element_blank(),
      legend.title = element_text(size = base_size + 1, face = "bold"),
      legend.text = element_text(size = base_size),
      legend.key.size = unit(1.2, "lines")
    )

  # --- Add title/subtitle if provided ---
  if (!is.null(title) || !is.null(subtitle)) {
    combined <- combined +
      plot_annotation(
        title = title,
        subtitle = subtitle,
        theme = theme(
          plot.title = element_text(size = base_size + 4, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = base_size + 1, color = "gray40", hjust = 0.5),
          plot.margin = margin(10, 10, 5, 10)
        )
      )
  }

  return(combined)
}


# ------------------------------------------------------------------------------
# PANEL C SUBPLOT (R-SQUARED)
# ------------------------------------------------------------------------------

#' Create a Panel C subplot for one survey (R² over time)
#'
#' @param r2_data Data frame with year, r_squared columns
#' @param survey_name Survey name (not used as title in combined figure)
#' @param show_title Show survey name as title? (FALSE for bottom row)
#' @param show_ylabel Show y-axis label? (TRUE for leftmost column only)
#' @param show_x_axis_title Show "Year" axis title? (FALSE, use shared label)
#' @param show_x_tick_labels Show year numbers on x-axis? (TRUE)
#' @param base_size Base font size for the plot
#' @param tilt_x_labels Angle to tilt x-axis labels (0 = horizontal, 45 = tilted)
#' @param point_color Color for points. Default "#3C5488" (dark blue)
#' @param line_color Color for connecting line. Default "#3C5488"
#' @param point_size Point size. Default 1.5
#' @param line_width Line width. Default 0.6
#'
#' @return ggplot object
#'
create_panel_c_subplot <- function(
    r2_data,
    survey_name,
    show_title = FALSE,
    show_ylabel = FALSE,
    show_x_axis_title = FALSE,
    show_x_tick_labels = TRUE,
    point_color = "#3C5488",
    line_color = "#3C5488",
    point_size = 1.5,
    line_width = 0.6,
    base_size = 12,
    tilt_x_labels = 0
) {

  p <- ggplot(r2_data, aes(x = year, y = r_squared)) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    # Line and points
    geom_line(color = line_color, linewidth = line_width) +
    geom_point(size = point_size, color = point_color)

  # Labels (x-axis title controlled separately from tick labels)
  p <- p + labs(
    title = if (show_title) survey_name else NULL,
    x = if (show_x_axis_title) "Year" else NULL,
    y = if (show_ylabel) expression(R^2 ~ "(Age" %->% "SRH)") else NULL
  )

  # X-axis tick label styling (tilted or horizontal)
  x_tick_style <- if (!show_x_tick_labels) {
    element_blank()
  } else if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = x_tick_style,
      axis.ticks.x = if (!show_x_tick_labels) element_blank() else element_line(),
      plot.margin = margin(2, 4, 2, 4)
    )

  return(p)
}


# ------------------------------------------------------------------------------
# COMBINED 3x6 FIGURE (Panel A, Panel B, Panel C)
# ------------------------------------------------------------------------------

#' Create combined Figure 1 with Panel A, Panel B, and Panel C (3x6 layout)
#'
#' @description
#' Creates a 3-row by 6-column figure where:
#' - Row 1: Panel A content (weighted mean SRH by age group over time)
#' - Row 2: Panel B content (age coefficient on SRH over time)
#' - Row 3: Panel C content (R² from SRH ~ age over time)
#' - Columns: Each survey (BRFSS, MEPS, NHIS, CPS, NHANES, GSS)
#'
#' @param estimates_list Named list of Panel A estimates (from summarize_srh_over_time)
#' @param coefficients_list Named list of Panel B coefficients (from regress_age_coefficient_by_year)
#' @param r2_list Named list of Panel C R² values (from compute_r2_by_year)
#' @param meta_results_list Optional named list of metaregression results for Panel B
#' @param colors Age group color palette for Panel A
#' @param show_ci Show confidence intervals in Panel A? Default FALSE
#' @param show_metareg Show metaregression lines in Panel B? Default TRUE
#' @param title Overall figure title. Default NULL (no title)
#' @param subtitle Overall figure subtitle. Default NULL
#' @param base_size Base font size for all text. Default 12
#' @param tilt_x_labels Angle to tilt x-axis labels (0 = horizontal, 45 = tilted). Default 45
#' @param metareg_line_color Color for metaregression trend line. Default "#56B4E9" (sky blue)
#'
#' @return A patchwork object
#'
#' @details
#' The surveys should be in the same order in all lists. The column order
#' in the figure follows the order in the lists.
#'
#' Y-axis labels appear only on the leftmost column of each row.
#' X-axis label "Year" appears only below the bottom row.
#' Survey names appear as titles on the top row only.
#' Age group legend appears at the bottom.
#'
#' @examples
#' # fig1_3row <- plot_fig1_combined_3row(
#' #   estimates_list = list(BRFSS = ..., MEPS = ..., ...),
#' #   coefficients_list = list(BRFSS = ..., MEPS = ..., ...),
#' #   r2_list = list(BRFSS = ..., MEPS = ..., ...),
#' #   meta_results_list = list(BRFSS = ..., MEPS = ..., ...),
#' #   title = "Self Rated Health and Age Over Time",
#' #   base_size = 14,
#' #   tilt_x_labels = 45
#' # )
#' # ggsave("fig1_combined_3row.png", fig1_3row, width = 16, height = 10)
#'
plot_fig1_combined_3row <- function(
    estimates_list,
    coefficients_list,
    r2_list,
    meta_results_list = NULL,
    colors = NULL,
    show_ci = FALSE,
    show_metareg = TRUE,
    title = NULL,
    subtitle = NULL,
    base_size = 12,
    tilt_x_labels = 45,
    metareg_line_color = "#56B4E9"
) {

  # --- Input validation ---
  stopifnot(is.list(estimates_list), is.list(coefficients_list), is.list(r2_list))

  if (is.null(names(estimates_list)) || is.null(names(coefficients_list)) ||
      is.null(names(r2_list))) {
    stop("All lists must be named (names = survey names)")
  }

  # Check that survey names match
  if (!identical(names(estimates_list), names(coefficients_list)) ||
      !identical(names(estimates_list), names(r2_list))) {
    warning("Survey names in lists don't match exactly.")
  }

  # Use default colors if not provided
  if (is.null(colors)) colors <- age_colors

  # --- Get survey order ---
  survey_names <- names(estimates_list)
  n_surveys <- length(survey_names)

  # --- Create Row 1: Panel A subplots ---
  row1_plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]
    create_panel_a_subplot(
      estimates = estimates_list[[svy]],
      survey_name = svy,
      colors = colors,
      show_title = TRUE,
      show_ylabel = (i == 1),
      show_x_axis_title = FALSE,
      show_x_tick_labels = TRUE,
      show_legend = TRUE,
      show_ci = show_ci,
      base_size = base_size,
      tilt_x_labels = tilt_x_labels
    )
  })
  names(row1_plots) <- survey_names

  # --- Create Row 2: Panel B subplots ---
  row2_plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]

    meta_result <- if (!is.null(meta_results_list) && svy %in% names(meta_results_list)) {
      meta_results_list[[svy]]
    } else {
      NULL
    }

    create_panel_b_subplot(
      coefficients = coefficients_list[[svy]],
      survey_name = svy,
      meta_result = meta_result,
      show_title = FALSE,
      show_ylabel = (i == 1),
      show_x_axis_title = FALSE,
      show_x_tick_labels = TRUE,
      show_metareg = show_metareg,
      line_color = metareg_line_color,
      base_size = base_size,
      tilt_x_labels = tilt_x_labels
    )
  })
  names(row2_plots) <- survey_names

  # --- Create Row 3: Panel C subplots (R²) ---
  row3_plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]

    create_panel_c_subplot(
      r2_data = r2_list[[svy]],
      survey_name = svy,
      show_title = FALSE,
      show_ylabel = (i == 1),
      show_x_axis_title = FALSE,
      show_x_tick_labels = TRUE,
      base_size = base_size,
      tilt_x_labels = tilt_x_labels
    )
  })
  names(row3_plots) <- survey_names

  # --- Assemble the grid ---
  # Row 1: Panel A plots side by side
  row1 <- wrap_plots(row1_plots, ncol = n_surveys)

  # Row 2: Panel B plots side by side
  row2 <- wrap_plots(row2_plots, ncol = n_surveys)

  # Row 3: Panel C plots side by side
  row3 <- wrap_plots(row3_plots, ncol = n_surveys)

  # --- Create shared x-axis label as a grob ---
  x_label_grob <- wrap_elements(
    grid::textGrob(
      "Year",
      gp = grid::gpar(fontsize = base_size + 2)
    )
  )

  # Add spacing between rows B and C
  spacer <- wrap_elements(grid::nullGrob())

  # Stack rows: Panel A / Panel B / spacer / Panel C / x-label
  combined <- row1 / row2 / spacer / row3 / x_label_grob +
    plot_layout(heights = c(1, 0.8, 0.03, 0.8, 0.08))

  # --- Configure layout ---
  # Collect guides (legend) at bottom
  combined <- combined +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10),
      legend.background = element_blank(),
      legend.title = element_text(size = base_size + 1, face = "bold"),
      legend.text = element_text(size = base_size),
      legend.key.size = unit(2, "lines"),
      legend.key.width = unit(2.5, "lines")
    )

  # --- Add title/subtitle if provided ---
  if (!is.null(title) || !is.null(subtitle)) {
    combined <- combined +
      plot_annotation(
        title = title,
        subtitle = subtitle,
        theme = theme(
          plot.title = element_text(size = base_size + 4, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = base_size + 1, color = "gray40", hjust = 0.5),
          plot.margin = margin(10, 10, 5, 10)
        )
      )
  }

  # --- Add panel labels (A, B, C) using cowplot overlay ---
  # Heights: c(1, 0.8, 0.03, 0.8, 0.08) + legend
  total_h <- 1 + 0.8 + 0.03 + 0.8 + 0.08  # 2.71
  legend_frac <- 0.06  # approximate legend fraction of figure
  scale <- 1 - legend_frac
  row1_top <- 1.0
  row2_top <- 1.0 - (1.0 / total_h) * scale
  row3_top <- 1.0 - ((1.0 + 0.8 + 0.03) / total_h) * scale

  label_x <- 0.003
  combined <- cowplot::ggdraw() +
    theme(plot.background = element_rect(fill = "white", colour = NA)) +
    cowplot::draw_plot(combined, x = 0.02, y = 0, width = 0.98, height = 1) +
    cowplot::draw_label("A", x = label_x, y = row1_top - 0.04,
                        hjust = 0, vjust = 1, fontface = "bold", size = 18,
                        colour = "black") +
    cowplot::draw_label("B", x = label_x, y = row2_top,
                        hjust = 0, vjust = 1, fontface = "bold", size = 18,
                        colour = "black") +
    cowplot::draw_label("C", x = label_x, y = row3_top,
                        hjust = 0, vjust = 1, fontface = "bold", size = 18,
                        colour = "black")

  return(combined)
}


# ==============================================================================
# DICHOTOMIZED SRH PLOTTING FUNCTIONS
# For supplementary materials showing Figure 1 analogs with binary outcomes
# ==============================================================================

#' Create a Panel A subplot for dichotomized SRH (prevalence scale)
#'
#' @param estimates Data frame with age_group, year, prevalence columns
#' @param survey_name Survey name for title
#' @param colors Age group color palette
#' @param show_title Show survey name as title?
#' @param show_ylabel Show y-axis label?
#' @param show_x_axis_title Show "Year" axis title?
#' @param show_x_tick_labels Show year numbers on x-axis?
#' @param show_legend Keep legend elements?
#' @param show_ci Show confidence intervals?
#' @param y_limits Y-axis limits (default c(0, 1) for probability scale)
#' @param base_size Base font size for the plot
#' @param tilt_x_labels Angle to tilt x-axis labels
#'
#' @return ggplot object
#'
create_panel_a_prevalence_subplot <- function(
    estimates,
    survey_name,
    colors = NULL,
    show_title = TRUE,
    show_ylabel = FALSE,
    show_x_axis_title = FALSE,
    show_x_tick_labels = TRUE,
    show_legend = TRUE,
    show_ci = FALSE,
    y_limits = c(0, 1),
    line_width = 0.6,
    point_size = 1.0,
    base_size = 12,
    tilt_x_labels = 0
) {

  if (is.null(colors)) colors <- age_colors

  # Ensure age_group is factor
  if (!is.factor(estimates$age_group)) {
    estimates$age_group <- factor(estimates$age_group,
                                   levels = unique(estimates$age_group))
  }

  p <- ggplot(estimates, aes(x = year, y = prevalence,
                              color = age_group,
                              group = age_group)) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    scale_color_manual(values = colors, name = "Age group") +
    scale_y_continuous(limits = y_limits, labels = scales::percent_format())

  # Add CI ribbons if requested
  if (show_ci && all(c("ci_lower", "ci_upper") %in% names(estimates))) {
    p <- p +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = age_group),
                  alpha = 0.15, color = NA) +
      scale_fill_manual(values = colors, guide = "none")
  }

  # Labels
  p <- p + labs(
    title = if (show_title) survey_name else NULL,
    x = if (show_x_axis_title) "Year" else NULL,
    y = if (show_ylabel) "Prevalence" else NULL
  )

  # X-axis tick label styling
  x_tick_style <- if (!show_x_tick_labels) {
    element_blank()
  } else if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = x_tick_style,
      axis.ticks.x = if (!show_x_tick_labels) element_blank() else element_line(),
      plot.margin = margin(2, 4, 2, 4),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1)
    ) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}


#' Create a Panel B subplot for logistic coefficient (log-odds or marginal scale)
#'
#' @param coefficients Data frame with year, coefficient (or marginal_effect), se, ci_lower, ci_upper
#' @param survey_name Survey name
#' @param meta_result Optional metaregression result for trend line
#' @param use_marginal Use marginal_effect instead of coefficient?
#' @param show_title Show survey name as title?
#' @param show_ylabel Show y-axis label?
#' @param show_x_axis_title Show "Year" axis title?
#' @param show_x_tick_labels Show year numbers on x-axis?
#' @param show_metareg Show metaregression trend line?
#' @param y_label Custom y-axis label
#' @param base_size Base font size
#' @param tilt_x_labels Angle to tilt x-axis labels
#'
#' @return ggplot object
#'
create_panel_b_logistic_subplot <- function(
    coefficients,
    survey_name,
    meta_result = NULL,
    use_marginal = FALSE,
    show_title = FALSE,
    show_ylabel = FALSE,
    show_x_axis_title = FALSE,
    show_x_tick_labels = TRUE,
    show_metareg = TRUE,
    y_label = NULL,
    point_color = "#3C5488",
    line_color = "#56B4E9",
    point_size = 1.5,
    line_width = 0.8,
    base_size = 12,
    tilt_x_labels = 0
) {

  # Select which coefficient to plot
  if (use_marginal) {
    coef_col <- "marginal_effect"
    se_col <- "marginal_se"
    default_ylabel <- "Marginal effect (prob. per 10 years)"
  } else {
    coef_col <- "coefficient"
    se_col <- "se"
    default_ylabel <- "Age coefficient (log-odds)"
  }

  if (is.null(y_label)) y_label <- default_ylabel

  # Create working copy with standardized names
  plot_data <- coefficients %>%
    transmute(
      year = year,
      coefficient = .data[[coef_col]],
      se = .data[[se_col]]
    ) %>%
    filter(!is.na(coefficient))

  # Compute CI if not provided directly
  plot_data <- plot_data %>%
    mutate(
      ci_lower = coefficient - 1.96 * se,
      ci_upper = coefficient + 1.96 * se
    )

  has_ci <- all(c("ci_lower", "ci_upper") %in% names(plot_data))

  p <- ggplot(plot_data, aes(x = year, y = coefficient)) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4)

  # Error bars if available
  if (has_ci) {
    p <- p + geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.3,
      color = point_color,
      linewidth = 0.3
    )
  }

  # Points
  p <- p + geom_point(size = point_size, color = point_color)

  # Metaregression line
  if (show_metareg && !is.null(meta_result)) {
    year_range <- range(plot_data$year)
    pred_data <- data.frame(
      year = seq(year_range[1], year_range[2], length.out = 100)
    )
    pred_data$predicted <- meta_result$intercept + meta_result$slope * pred_data$year

    p <- p + geom_line(
      data = pred_data,
      aes(x = year, y = predicted),
      color = line_color,
      linewidth = line_width
    )
  }

  # Labels
  p <- p + labs(
    title = if (show_title) survey_name else NULL,
    x = if (show_x_axis_title) "Year" else NULL,
    y = if (show_ylabel) y_label else NULL
  )

  # X-axis tick label styling
  x_tick_style <- if (!show_x_tick_labels) {
    element_blank()
  } else if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = x_tick_style,
      axis.ticks.x = if (!show_x_tick_labels) element_blank() else element_line(),
      plot.margin = margin(2, 4, 2, 4)
    )

  return(p)
}


#' Create combined Figure 1 for dichotomized SRH (2x6 layout)
#'
#' @description
#' Creates a 2-row by 6-column figure where:
#' - Row 1: Panel A content (prevalence of dichotomized SRH by age group over time)
#' - Row 2: Panel B content (age coefficient from logistic regression)
#' - Columns: Each survey (BRFSS, MEPS, NHIS, GSS, CPS, NHANES)
#'
#' @param prevalence_list Named list of prevalence estimates (from summarize_srh_prevalence_over_time)
#' @param coefficients_list Named list of logistic coefficients (from regress_age_coefficient_by_year_logistic)
#' @param meta_results_list Optional named list of metaregression results for Panel B
#' @param colors Age group color palette for Panel A
#' @param use_marginal Use marginal_effect instead of log-odds coefficient for Panel B?
#' @param show_ci Show confidence intervals in Panel A?
#' @param show_metareg Show metaregression lines in Panel B?
#' @param title Overall figure title
#' @param subtitle Overall figure subtitle
#' @param panel_a_ylabel Y-axis label for Panel A (default "Prevalence")
#' @param panel_b_ylabel Y-axis label for Panel B (auto-set based on use_marginal)
#' @param base_size Base font size
#' @param tilt_x_labels Angle to tilt x-axis labels
#' @param metareg_line_color Color for metaregression trend line
#'
#' @return A patchwork object
#'
plot_fig1_dichotomized_combined <- function(
    prevalence_list,
    coefficients_list,
    meta_results_list = NULL,
    colors = NULL,
    use_marginal = FALSE,
    show_ci = FALSE,
    show_metareg = TRUE,
    title = NULL,
    subtitle = NULL,
    panel_a_ylabel = "Prevalence",
    panel_b_ylabel = NULL,
    base_size = 12,
    tilt_x_labels = 45,
    metareg_line_color = "#56B4E9"
) {

  # --- Input validation ---
  stopifnot(is.list(prevalence_list), is.list(coefficients_list))

  if (is.null(names(prevalence_list)) || is.null(names(coefficients_list))) {
    stop("Both lists must be named (names = survey names)")
  }

  if (!identical(names(prevalence_list), names(coefficients_list))) {
    warning("Survey names in prevalence_list and coefficients_list don't match exactly.")
  }

  if (is.null(colors)) colors <- age_colors

  # Set Panel B y-axis label based on coefficient type
  if (is.null(panel_b_ylabel)) {
    panel_b_ylabel <- if (use_marginal) {
      "Marginal effect (prob. per 10 years)"
    } else {
      "Age coefficient (log-odds)"
    }
  }

  # --- Get survey order ---
  survey_names <- names(prevalence_list)
  n_surveys <- length(survey_names)

  # --- Create Row 1: Panel A (prevalence) subplots ---
  row1_plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]
    create_panel_a_prevalence_subplot(
      estimates = prevalence_list[[svy]],
      survey_name = svy,
      colors = colors,
      show_title = TRUE,
      show_ylabel = (i == 1),
      show_x_axis_title = FALSE,
      show_x_tick_labels = TRUE,
      show_legend = TRUE,
      show_ci = show_ci,
      base_size = base_size,
      tilt_x_labels = tilt_x_labels
    )
  })
  names(row1_plots) <- survey_names

  # --- Create Row 2: Panel B (logistic coefficient) subplots ---
  row2_plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]

    meta_result <- if (!is.null(meta_results_list) && svy %in% names(meta_results_list)) {
      meta_results_list[[svy]]
    } else {
      NULL
    }

    create_panel_b_logistic_subplot(
      coefficients = coefficients_list[[svy]],
      survey_name = svy,
      meta_result = meta_result,
      use_marginal = use_marginal,
      show_title = FALSE,
      show_ylabel = (i == 1),
      show_x_axis_title = FALSE,
      show_x_tick_labels = TRUE,
      show_metareg = show_metareg,
      y_label = if (i == 1) panel_b_ylabel else NULL,
      line_color = metareg_line_color,
      base_size = base_size,
      tilt_x_labels = tilt_x_labels
    )
  })
  names(row2_plots) <- survey_names

  # --- Assemble the grid ---
  row1 <- wrap_plots(row1_plots, ncol = n_surveys)
  row2 <- wrap_plots(row2_plots, ncol = n_surveys)

  # --- Create shared x-axis label ---
  x_label_grob <- wrap_elements(
    grid::textGrob(
      "Year",
      gp = grid::gpar(fontsize = base_size + 2)
    )
  )

  # Stack rows: Panel A / Panel B / x-label
  combined <- row1 / row2 / x_label_grob +
    plot_layout(heights = c(1, 1, 0.05))

  # --- Configure layout ---
  combined <- combined +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10),
      legend.background = element_blank(),
      legend.title = element_text(size = base_size + 1, face = "bold"),
      legend.text = element_text(size = base_size),
      legend.key.size = unit(1.2, "lines")
    )

  # --- Add title/subtitle if provided ---
  if (!is.null(title) || !is.null(subtitle)) {
    combined <- combined +
      plot_annotation(
        title = title,
        subtitle = subtitle,
        theme = theme(
          plot.title = element_text(size = base_size + 4, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = base_size + 1, color = "gray40", hjust = 0.5),
          plot.margin = margin(10, 10, 5, 10)
        )
      )
  }

  return(combined)
}
