# ==============================================================================
# plot_fig1_panel_b.R
# Figure 1 Panel B: Age Coefficient on SRH Over Time
#
# Purpose: Create publication-quality 2x3 multi-panel figure showing how the
#          age coefficient (from SRH ~ age regression) changes over time.
#          Demonstrates the convergence phenomenon across all 6 surveys.
#
# Key features:
#   - Each survey gets its own panel with FREE y-axis and x-axis
#   - Points with error bars (95% CI) for each year's coefficient
#   - Optional metaregression trend line (on by default)
#   - Horizontal reference line at y = 0 (no age effect)
#   - Consistent styling with Panel A
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)

# Source theme (for colors) - caller should source this, but we define fallback
if (!exists("survey_colors")) {
  source(here::here("R", "functions", "theme_srh.R"))
}

# ------------------------------------------------------------------------------
# SINGLE SURVEY PANEL
# ------------------------------------------------------------------------------

#' Create a single panel for one survey's age coefficient over time
#'
#' @description
#' Generates one ggplot showing the age coefficient (y-axis) vs year (x-axis)
#' for a single survey. Used as building block for the combined figure.
#'
#' @param coefficients Data frame with columns: year, coefficient, se, ci_lower, ci_upper
#' @param survey_name Character string for panel title (e.g., "NHIS")
#' @param meta_result Optional data frame row with metaregression results
#'   (slope, intercept) for adding trend line. NULL to skip.
#' @param show_metareg Logical. Show metaregression trend line? Default TRUE.
#' @param point_color Color for points and error bars. Default "#3C5488" (blue).
#' @param line_color Color for metaregression line. Default "#E64B35" (red).
#' @param point_size Numeric. Size of points. Default 2.
#' @param line_width Numeric. Width of metaregression line. Default 1.
#' @param errorbar_width Numeric. Width of error bar caps. Default 0.3.
#'
#' @return A ggplot object (single panel)
#'
#' @details
#' - Horizontal dashed line at y = 0 shows the reference (no age effect)
#' - Error bars show 95% confidence intervals
#' - Metaregression line (if shown) indicates the trend direction
#'
plot_single_survey_coefficient <- function(
    coefficients,
    survey_name,
    meta_result = NULL,
    show_metareg = TRUE,
    point_color = "#3C5488",
    line_color = "#E64B35",
    point_size = 2,
    line_width = 1,
    errorbar_width = 0.3
) {

  # --- Input validation ---
  stopifnot(is.data.frame(coefficients))
  required_cols <- c("year", "coefficient")
  missing_cols <- setdiff(required_cols, names(coefficients))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for CI columns
  has_ci <- all(c("ci_lower", "ci_upper") %in% names(coefficients))

  # --- Build the plot ---
  p <- ggplot(coefficients, aes(x = year, y = coefficient)) +
    # Reference line at zero (no age effect)
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5)

  # Add error bars if CI available
  if (has_ci) {
    p <- p + geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = errorbar_width,
      color = point_color,
      linewidth = 0.4
    )
  }

  # Add points
  p <- p + geom_point(size = point_size, color = point_color)

  # Add metaregression trend line if requested and available
  if (show_metareg && !is.null(meta_result)) {
    # Create predicted values from metaregression
    year_range <- range(coefficients$year)
    pred_data <- data.frame(
      year = seq(year_range[1], year_range[2], length.out = 100)
    )
    pred_data$predicted <- meta_result$intercept + meta_result$slope * pred_data$year

    p <- p + geom_line(
      data = pred_data,
      aes(x = year, y = predicted),
      color = line_color,
      linewidth = line_width,
      linetype = "solid"
    )
  }

  # Theme and labels
  p <- p +
    labs(
      title = survey_name,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 9, color = "gray30"),
      plot.margin = margin(5, 5, 5, 5)
    )

  return(p)
}


# ------------------------------------------------------------------------------
# COMBINED 2x3 FIGURE
# ------------------------------------------------------------------------------

#' Create combined 2x3 figure for Figure 1 Panel B
#'
#' @description
#' Combines 6 survey panels into a publication-ready 2x3 figure using patchwork.
#' Shows age coefficient on SRH over time for each survey.
#'
#' @param coefficients_list Named list of 6 data frames, each containing
#'   coefficient estimates from regress_age_coefficient_by_year().
#' @param meta_results_list Optional named list of metaregression results
#'   (one per survey). If NULL and show_metareg=TRUE, metaregression is computed.
#' @param show_metareg Logical. Show metaregression trend lines? Default TRUE.
#' @param ncol Integer. Number of columns in layout. Default 3.
#' @param y_label Character. Shared y-axis label. Default "Age coefficient".
#' @param x_label Character. Shared x-axis label. Default "Year".
#' @param title Character. Overall figure title.
#' @param subtitle Character. Overall figure subtitle. Default NULL.
#' @param point_color Color for coefficient points. Default "#3C5488".
#' @param line_color Color for metaregression lines. Default "#E64B35".
#'
#' @return A patchwork object ready for display or saving
#'
#' @details
#' Survey order in the figure follows the order in coefficients_list.
#' Recommended order: BRFSS, MEPS, NHIS (top row), GSS, CPS, NHANES (bottom).
#'
plot_fig1_panel_b <- function(
    coefficients_list,
    meta_results_list = NULL,
    show_metareg = TRUE,
    ncol = 3,
    y_label = "Age coefficient",
    x_label = "Year",
    title = "Age Coefficient on SRH Over Time",
    subtitle = NULL,
    point_color = "#3C5488",
    line_color = "#E64B35"
) {

  # --- Input validation ---
  stopifnot(is.list(coefficients_list))
  stopifnot(length(coefficients_list) >= 1)

  if (is.null(names(coefficients_list))) {
    stop("coefficients_list must be a named list (names = survey names)")
  }

  # --- Compute metaregressions if needed ---
  if (show_metareg && is.null(meta_results_list)) {
    # Source the regression functions if not already loaded
    if (!exists("run_metaregression")) {
      source(here::here("R", "functions", "regress_srh_on_age.R"))
    }

    meta_results_list <- lapply(names(coefficients_list), function(svy) {
      run_metaregression(coefficients_list[[svy]], svy)
    })
    names(meta_results_list) <- names(coefficients_list)
  }

  # --- Create individual panels ---
  survey_names <- names(coefficients_list)

  panels <- lapply(survey_names, function(svy) {
    meta_result <- if (show_metareg && !is.null(meta_results_list[[svy]])) {
      meta_results_list[[svy]]
    } else {
      NULL
    }

    plot_single_survey_coefficient(
      coefficients = coefficients_list[[svy]],
      survey_name = svy,
      meta_result = meta_result,
      show_metareg = show_metareg,
      point_color = point_color,
      line_color = line_color
    )
  })
  names(panels) <- survey_names

  # --- Create shared axis labels as grobs ---
  y_label_grob <- grid::textGrob(
    y_label,
    rot = 90,
    gp = grid::gpar(fontsize = 12, fontface = "plain")
  )

  x_label_grob <- grid::textGrob(
    x_label,
    gp = grid::gpar(fontsize = 12, fontface = "plain")
  )

  # --- Combine panels with patchwork ---
  n_panels <- length(panels)
  nrow_panels <- ceiling(n_panels / ncol)

  panel_grid <- wrap_plots(panels, ncol = ncol, nrow = nrow_panels)

  # --- Build the full layout ---
  # Combine panel grid with y-label on left
  panels_with_ylabel <- wrap_elements(y_label_grob) | panel_grid
  panels_with_ylabel <- panels_with_ylabel + plot_layout(widths = c(0.03, 1))

  # Stack with x_label
  final_figure <- panels_with_ylabel /
    wrap_elements(x_label_grob)

  # Set relative heights
  final_figure <- final_figure +
    plot_layout(heights = c(1, 0.06))

  # Add overall title and subtitle
  final_figure <- final_figure +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10)
      )
    )

  return(final_figure)
}


# ------------------------------------------------------------------------------
# COMBINED PANELS A + B (OPTIONAL)
# ------------------------------------------------------------------------------

#' Combine Panel A and Panel B into full Figure 1
#'
#' @description
#' Creates the complete Figure 1 with Panel A (mean SRH by age group)
#' and Panel B (age coefficient over time) side by side or stacked.
#'
#' @param panel_a Patchwork object from plot_fig1_panel_a()
#' @param panel_b Patchwork object from plot_fig1_panel_b()
#' @param layout Character. "horizontal" or "vertical". Default "vertical".
#' @param title Overall figure title. Default NULL (panels have own titles).
#'
#' @return A patchwork object
#'
combine_figure1_panels <- function(
    panel_a,
    panel_b,
    layout = "vertical",
    title = NULL
) {

  if (layout == "horizontal") {
    combined <- panel_a | panel_b
  } else {
    combined <- panel_a / panel_b
  }

  if (!is.null(title)) {
    combined <- combined +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )
      )
  }

  return(combined)
}
