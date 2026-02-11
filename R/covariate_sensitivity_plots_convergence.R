# ==============================================================================
# covariate_sensitivity_plots_convergence.R
# Plotting functions for Convergence page (Page 1) of covariate sensitivity analyses
#
# Purpose: Visualize stratified Figure 1 analyses - age coefficients and mean SRH
#          trends within covariate strata (e.g., by sex, education, race)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)
library(srvyr)
library(here)

# Source theme for colors and styling
source(here::here("R", "functions", "theme_srh.R"))

# ==============================================================================
# PLOT 1: STRATIFIED AGE COEFFICIENTS (Row 1 of Convergence Page)
# ==============================================================================

#' Plot stratified age coefficients across 6 datasets
#'
#' @description
#' Creates a figure showing how the age coefficient on SRH (from model SRH ~ age)
#' varies over time within each covariate stratum. Each panel shows one dataset
#' with points colored by stratum and trend lines indicating the meta-regression fit.
#'
#' @param coef_data_list Named list of 6 data frames (one per dataset) from
#'   compute_stratified_age_coefficients. Each data frame has columns:
#'   year, stratum, beta_age, se, p_value, n
#' @param meta_reg_list Named list where each element is a named list of
#'   meta-regression results (one per stratum). Each result has: slope, se,
#'   p_value, is_significant. Structure: list(BRFSS = list(Male = ..., Female = ...), ...)
#' @param covariate_levels Character vector of covariate levels (stratum names)
#' @param covariate_colors Named vector of colors for covariate levels
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 11.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#' @param point_size Size of coefficient points. Default 1.2.
#' @param line_width Width of trend lines. Default 0.8.
#' @param transpose Logical. If TRUE, rows = datasets (single column of panels).
#'   If FALSE (default), columns = datasets (single row of panels).
#'
#' @return A ggplot/patchwork object
#'
#' @examples
#' # coef_plot <- plot_stratified_age_coefficients(
#' #   coef_data_list = coef_results,
#' #   meta_reg_list = meta_results,
#' #   covariate_levels = c("Male", "Female"),
#' #   covariate_colors = c(Male = "#4477AA", Female = "#CC6677"),
#' #   transpose = TRUE
#' # )
#'
plot_stratified_age_coefficients <- function(
    coef_data_list,
    meta_reg_list,
    covariate_levels,
    covariate_colors,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 11,
    tilt_x_labels = 45,
    point_size = 1.2,
    line_width = 0.8,
    transpose = FALSE
) {

  # Input validation
  stopifnot(is.list(coef_data_list))
  stopifnot(is.list(meta_reg_list))
  stopifnot(is.character(covariate_levels))
  stopifnot(is.character(covariate_colors) || is.null(names(covariate_colors)))

  # Filter to available datasets
  available_datasets <- intersect(dataset_names, names(coef_data_list))
  if (length(available_datasets) == 0) {
    stop("No datasets found in coef_data_list matching dataset_names")
  }

  # Create individual panels
  panels <- lapply(seq_along(available_datasets), function(i) {
    dataset <- available_datasets[i]
    coef_data <- coef_data_list[[dataset]]
    meta_results <- meta_reg_list[[dataset]]

    create_stratified_coef_panel(
      coef_data = coef_data,
      meta_results = meta_results,
      covariate_levels = covariate_levels,
      covariate_colors = covariate_colors,
      dataset_name = dataset,
      show_ylabel = TRUE,  # Always show y-axis label
      show_title = if (transpose) FALSE else TRUE,
      row_label = NULL,  # Row labels added separately in assembly
      show_legend = TRUE,
      base_size = base_size,
      tilt_x_labels = tilt_x_labels,
      point_size = point_size,
      line_width = line_width
    )
  })
  names(panels) <- available_datasets

  # Combine panels
  if (transpose) {
    # Single column (datasets as rows)
    combined <- wrap_plots(panels, ncol = 1)
  } else {
    # Single row (datasets as columns)
    combined <- wrap_plots(panels, ncol = length(panels))
  }

  # Collect legend at bottom
  combined <- combined +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1)
    )

  return(combined)
}


#' Create a single panel for stratified age coefficients
#'
#' @description
#' Internal helper function to create one panel of the stratified coefficient plot.
#' Uses plot.tag for row labels (dataset names) on the left side.
#'
#' @param coef_data Data frame with year, stratum, beta_age, se, p_value, n
#' @param meta_results Named list of meta-regression results (one per stratum)
#' @param covariate_levels Character vector of covariate levels
#' @param covariate_colors Named vector of colors
#' @param dataset_name Character string for panel title
#' @param show_ylabel Show y-axis label?
#' @param show_title Show dataset name as title?
#' @param row_label Text to show as row label on left side (NULL to hide)
#' @param show_legend Show legend elements?
#' @param base_size Base font size
#' @param tilt_x_labels X-axis label angle
#' @param point_size Point size
#' @param line_width Line width
#'
#' @return ggplot object
#'
create_stratified_coef_panel <- function(
    coef_data,
    meta_results,
    covariate_levels,
    covariate_colors,
    dataset_name,
    show_ylabel = TRUE,
    show_title = TRUE,
    row_label = NULL,
    show_legend = TRUE,
    base_size = 11,
    tilt_x_labels = 45,
    point_size = 1.2,
    line_width = 0.8
) {

  # Filter to valid data and specified levels
  plot_data <- coef_data %>%
    filter(
      !is.na(beta_age),
      stratum %in% covariate_levels
    ) %>%
    mutate(stratum = factor(stratum, levels = covariate_levels))

  if (nrow(plot_data) == 0) {
    # Return empty plot with title
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = 4) +
      theme_void()

    if (show_title) {
      p <- p + labs(title = dataset_name) +
        theme(plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5))
    }
    return(p)
  }

  # Build the plot
  p <- ggplot(plot_data, aes(x = year, y = beta_age, color = stratum)) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    # Points
    geom_point(size = point_size, alpha = 0.8)

  # Add meta-regression trend lines
  if (!is.null(meta_results)) {
    for (stratum_name in covariate_levels) {
      if (!is.null(meta_results[[stratum_name]])) {
        meta <- meta_results[[stratum_name]]

        # Skip if slope is NA
        if (is.na(meta$slope)) next

        # Get year range for this stratum
        stratum_data <- plot_data %>% filter(stratum == stratum_name)
        if (nrow(stratum_data) < 2) next

        year_range <- range(stratum_data$year)
        pred_years <- seq(year_range[1], year_range[2], length.out = 50)

        # Compute predicted values (need intercept)
        # Meta-regression is: beta_age ~ year, so we need to compute intercept
        # from the mean-centered relationship
        mean_year <- mean(stratum_data$year)
        mean_beta <- mean(stratum_data$beta_age, na.rm = TRUE)
        intercept <- mean_beta - meta$slope * mean_year

        pred_data <- data.frame(
          year = pred_years,
          predicted = intercept + meta$slope * pred_years,
          stratum = stratum_name
        )

        # Determine line type based on significance
        line_type <- if (!is.na(meta$is_significant) && meta$is_significant) "solid" else "dashed"

        p <- p +
          geom_line(
            data = pred_data,
            aes(x = year, y = predicted),
            color = covariate_colors[stratum_name],
            linewidth = line_width,
            linetype = line_type
          )
      }
    }
  }

  # Apply colors and labels - use tag for row label (dataset name on left)
  p <- p +
    scale_color_manual(values = covariate_colors, name = "Stratum") +
    labs(
      title = if (show_title) dataset_name else NULL,
      x = NULL,
      y = if (show_ylabel) "Age coef on SRH" else NULL,
      tag = row_label
    ) +
    # Allow tag to display outside plot boundaries
    coord_cartesian(clip = "off")

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Adjust left margin for row label
  left_margin <- if (!is.null(row_label)) 40 else 4

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
      plot.margin = margin(4, 12, 4, left_margin),
      plot.tag = element_text(size = base_size + 2, face = "bold", angle = 90, vjust = 0.5),
      plot.tag.position = c(-0.12, 0.5),
      legend.position = if (show_legend) "bottom" else "none"
    )

  return(p)
}


# ==============================================================================
# PLOT 2: STRATIFIED SRH TRENDS (Rows 2+ of Convergence Page)
# ==============================================================================

#' Plot stratified SRH trends across 6 datasets
#'
#' @description
#' Creates a figure showing survey-weighted mean SRH over time within each
#' covariate stratum, with lines colored by age group (like Figure 1a but stratified).
#'
#' @param datasets_list Named list of 6 data frames (one per dataset). Each
#'   data frame should have columns: srh, age_group, year, wt, and the covariate variable.
#' @param covariate_var Name of the covariate variable in the data
#' @param covariate_levels Character vector of covariate levels (stratum names)
#' @param covariate_colors Named vector of colors for covariate levels (not used
#'   for line colors, but can be used for row labels)
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param age_var Name of age group variable. Default "age_group".
#' @param weight_var Name of weight variable. Default "wt".
#' @param base_size Base font size. Default 11.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#' @param line_width Width of age group lines. Default 0.6.
#' @param point_size Size of points. Default 1.0.
#' @param transpose Logical. If TRUE, rows = datasets, columns = strata.
#'   If FALSE (default), rows = strata, columns = datasets.
#' @param y_limits Optional vector of length 2 for y-axis limits. If NULL,
#'   computed from data to ensure alignment across all panels.
#'
#' @return A ggplot/patchwork object
#'
#' @examples
#' # srh_plot <- plot_stratified_srh_trends(
#' #   datasets_list = datasets,
#' #   covariate_var = "sex",
#' #   covariate_levels = c("Male", "Female"),
#' #   covariate_colors = c(Male = "#4477AA", Female = "#CC6677"),
#' #   transpose = TRUE
#' # )
#'
plot_stratified_srh_trends <- function(
    datasets_list,
    covariate_var,
    covariate_levels,
    covariate_colors,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    age_var = "age_group",
    weight_var = "wt",
    base_size = 11,
    tilt_x_labels = 45,
    line_width = 0.6,
    point_size = 1.0,
    transpose = FALSE,
    y_limits = NULL
) {

  # Input validation
  stopifnot(is.list(datasets_list))
  stopifnot(is.character(covariate_var))
  stopifnot(is.character(covariate_levels))

  # Filter to available datasets
  available_datasets <- intersect(dataset_names, names(datasets_list))
  if (length(available_datasets) == 0) {
    stop("No datasets found in datasets_list matching dataset_names")
  }

  n_strata <- length(covariate_levels)
  n_datasets <- length(available_datasets)

  # Compute survey-weighted means for all datasets
  means_list <- lapply(available_datasets, function(dataset) {
    data <- datasets_list[[dataset]]
    compute_weighted_mean_srh(
      data = data,
      covariate_var = covariate_var,
      covariate_levels = covariate_levels,
      age_var = age_var,
      weight_var = weight_var
    )
  })
  names(means_list) <- available_datasets

  # Compute global y-axis limits if not provided (for alignment)
  if (is.null(y_limits)) {
    all_means <- bind_rows(means_list)
    if (nrow(all_means) > 0) {
      y_min <- floor(min(all_means$mean_srh, na.rm = TRUE) * 10) / 10
      y_max <- ceiling(max(all_means$mean_srh, na.rm = TRUE) * 10) / 10
      # Add small padding
      y_range <- y_max - y_min
      y_limits <- c(y_min - 0.05 * y_range, y_max + 0.05 * y_range)
    } else {
      y_limits <- c(1, 5)  # Default SRH range
    }
  }

  # Create grid of panels
  all_panels <- list()

  if (transpose) {
    # Transposed: rows = datasets, columns = strata
    for (row_idx in seq_along(available_datasets)) {
      dataset <- available_datasets[row_idx]

      for (col_idx in seq_along(covariate_levels)) {
        stratum <- covariate_levels[col_idx]

        # Filter means to this stratum
        stratum_means <- means_list[[dataset]] %>%
          filter(stratum == !!stratum)

        # Add row label (dataset name) only to first column
        row_label_text <- if (col_idx == 1) dataset else NULL

        panel <- create_srh_trend_panel(
          means_data = stratum_means,
          dataset_name = dataset,
          stratum_name = stratum,
          show_title = (row_idx == 1),
          show_ylabel = (col_idx == 1),
          row_label = row_label_text,  # Dataset name on left of first column
          show_col_label = (row_idx == 1),
          use_stratum_as_title = TRUE,
          show_legend = TRUE,
          base_size = base_size,
          tilt_x_labels = tilt_x_labels,
          line_width = line_width,
          point_size = point_size,
          y_limits = y_limits
        )

        panel_name <- paste0(dataset, "_", stratum)
        all_panels[[panel_name]] <- panel
      }
    }
    # Arrange panels: rows = datasets, columns = strata
    combined <- wrap_plots(all_panels, ncol = n_strata, byrow = TRUE)

  } else {
    # Original: rows = strata, columns = datasets
    for (row_idx in seq_along(covariate_levels)) {
      stratum <- covariate_levels[row_idx]

      for (col_idx in seq_along(available_datasets)) {
        dataset <- available_datasets[col_idx]

        # Filter means to this stratum
        stratum_means <- means_list[[dataset]] %>%
          filter(stratum == !!stratum)

        panel <- create_srh_trend_panel(
          means_data = stratum_means,
          dataset_name = dataset,
          stratum_name = stratum,
          show_title = (row_idx == 1),
          show_ylabel = (col_idx == 1),
          show_row_label = (col_idx == 1),
          show_col_label = FALSE,
          use_stratum_as_title = FALSE,
          show_legend = TRUE,
          base_size = base_size,
          tilt_x_labels = tilt_x_labels,
          line_width = line_width,
          point_size = point_size,
          y_limits = y_limits
        )

        panel_name <- paste0(stratum, "_", dataset)
        all_panels[[panel_name]] <- panel
      }
    }
    # Arrange panels: rows = strata, columns = datasets
    combined <- wrap_plots(all_panels, ncol = n_datasets, byrow = TRUE)
  }

  # Collect legend at bottom
  combined <- combined +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1)
    )

  return(combined)
}


#' Compute survey-weighted mean SRH by year, age group, and stratum
#'
#' @description
#' Internal helper to compute weighted mean SRH for stratified plots.
#'
#' @param data Data frame with srh, age_group, year, wt, and covariate
#' @param covariate_var Name of covariate variable
#' @param covariate_levels Vector of covariate levels to include
#' @param age_var Name of age group variable
#' @param weight_var Name of weight variable
#'
#' @return Data frame with year, age_group, stratum, mean_srh, n
#'
compute_weighted_mean_srh <- function(
    data,
    covariate_var,
    covariate_levels,
    age_var = "age_group",
    weight_var = "wt"
) {

  # Filter to valid data
  filtered_data <- data %>%
    filter(
      !is.na(srh),
      !is.na(.data[[age_var]]),
      !is.na(.data[[covariate_var]]),
      .data[[covariate_var]] %in% covariate_levels,
      !is.na(.data[[weight_var]]),
      .data[[weight_var]] > 0
    )

  if (nrow(filtered_data) == 0) {
    return(data.frame(
      year = integer(),
      age_group = character(),
      stratum = character(),
      mean_srh = numeric(),
      n = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Compute weighted means
  result <- filtered_data %>%
    group_by(year, .data[[age_var]], .data[[covariate_var]]) %>%
    summarize(
      mean_srh = weighted.mean(srh, w = .data[[weight_var]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    rename(
      age_group = !!sym(age_var),
      stratum = !!sym(covariate_var)
    )

  return(result)
}


#' Create a single panel for stratified SRH trends
#'
#' @description
#' Internal helper function to create one panel of the stratified SRH plot.
#' Uses plot.tag for row labels (dataset names) on the left side.
#'
#' @param means_data Data frame with year, age_group, mean_srh, n
#' @param dataset_name Character string for column/row label
#' @param stratum_name Character string for row/column label
#' @param show_title Show title (dataset or stratum depending on use_stratum_as_title)?
#' @param show_ylabel Show y-axis label?
#' @param row_label Text to display as row label on left (NULL to hide)
#' @param show_col_label Show stratum name as column title? (for transposed layout)
#' @param use_stratum_as_title If TRUE, use stratum_name for title; else use dataset_name
#' @param show_legend Show legend elements?
#' @param base_size Base font size
#' @param tilt_x_labels X-axis label angle
#' @param line_width Line width
#' @param point_size Point size
#' @param y_limits Optional vector of length 2 for y-axis limits
#'
#' @return ggplot object
#'
create_srh_trend_panel <- function(
    means_data,
    dataset_name,
    stratum_name,
    show_title = FALSE,
    show_ylabel = FALSE,
    row_label = NULL,
    show_col_label = FALSE,
    use_stratum_as_title = FALSE,
    show_legend = TRUE,
    base_size = 11,
    tilt_x_labels = 45,
    line_width = 0.6,
    point_size = 1.0,
    y_limits = NULL
) {

  if (nrow(means_data) == 0) {
    # Return empty plot
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = base_size / 3) +
      theme_void()

    if (show_title) {
      title_text <- if (use_stratum_as_title) stratum_name else dataset_name
      p <- p + labs(title = title_text) +
        theme(plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5))
    }

    return(p)
  }

  # Ensure age_group is factor with proper levels
  age_levels <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
  means_data <- means_data %>%
    mutate(age_group = factor(age_group, levels = age_levels))

  # Build plot
  p <- ggplot(means_data, aes(x = year, y = mean_srh,
                               color = age_group, group = age_group)) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    scale_color_manual(values = age_colors, name = "Age group") +
    coord_cartesian(clip = "off")

  # Apply y-axis limits if provided
  if (!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits)
  }

  # Determine title
  title_text <- NULL
  if (show_title) {
    title_text <- if (use_stratum_as_title) stratum_name else dataset_name
  }

  # Title and labels - use tag for row label
  p <- p + labs(
    title = title_text,
    x = NULL,
    y = if (show_ylabel) "Mean SRH" else NULL,
    tag = row_label
  )

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 1, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 1)
  }

  # Adjust left margin for row label
  left_margin <- if (!is.null(row_label)) 25 else 6

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, color = "gray30"),
      axis.text.x = x_tick_style,
      plot.margin = margin(4, 6, 4, left_margin),
      plot.tag = element_text(size = base_size + 1, face = "bold", angle = 90, vjust = 0.5),
      plot.tag.position = c(-0.06, 0.5),
      legend.position = if (show_legend) "bottom" else "none"
    ) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}


# ==============================================================================
# ASSEMBLY: CONVERGENCE PAGE
# ==============================================================================

#' Create a text label plot for row labels
#'
#' @description
#' Internal helper to create a simple text label plot for use as row headers.
#'
#' @param label Text to display
#' @param base_size Base font size
#' @param angle Rotation angle (90 for vertical, 0 for horizontal)
#'
#' @return ggplot object
#'
create_row_label_plot <- function(label, base_size = 14, angle = 90) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label,
             size = base_size / 2.5, fontface = "bold", angle = angle) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

#' Assemble the complete Convergence page
#'
#' @description
#' Combines the stratified age coefficients plot and stratified SRH trends plot
#' into a single figure suitable for saving.
#'
#' Layout:
#' - Row 1: Coefficient plots (1 row × 6 datasets)
#' - Rows 2+: SRH trends (6 dataset rows × N covariate columns)
#' - Row labels (dataset names) on left for SRH section
#' - Column headers (covariate levels) at top of SRH section
#'
#' @param stratified_coef_plot ggplot/patchwork object from plot_stratified_age_coefficients
#' @param stratified_srh_plot ggplot/patchwork object from plot_stratified_srh_trends (transposed)
#' @param covariate_name Character string (e.g., "Sex", "Education") for title
#' @param covariate_levels Character vector of covariate level names (for column headers)
#' @param dataset_names Character vector of dataset names (for row labels)
#' @param base_size Base font size for title. Default 14.
#' @param n_datasets Number of datasets. Default 6.
#' @param n_covariate_levels Number of covariate levels. Default 2.
#'
#' @return A patchwork object ready for saving
#'
assemble_convergence_page <- function(
    stratified_coef_plot,
    stratified_srh_plot,
    covariate_name,
    covariate_levels = NULL,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 14,
    n_datasets = 6,
    n_covariate_levels = 2
) {

  # Create shared x-axis label
  x_label_grob <- wrap_elements(
    grid::textGrob(
      "Year",
      gp = grid::gpar(fontsize = base_size + 1)
    )
  )

  # Layout: coefficient row on top, SRH trends below (row labels embedded in panels)
  combined <- stratified_coef_plot / stratified_srh_plot / x_label_grob +
    plot_layout(heights = c(1, n_datasets, 0.05))

  # Collect legends
  combined <- combined +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  # Add overall title
  combined <- combined +
    plot_annotation(
      title = paste0("Convergence Analysis: ", covariate_name),
      theme = theme(
        plot.title = element_text(
          size = base_size + 4,
          face = "bold",
          hjust = 0.5,
          margin = margin(b = 12)
        ),
        plot.margin = margin(10, 10, 10, 10)
      )
    )

  return(combined)
}


# ==============================================================================
# CONVENIENCE FUNCTION: RUN COMPLETE CONVERGENCE ANALYSIS
# ==============================================================================

#' Run complete convergence analysis and generate plot
#'
#' @description
#' Convenience function that computes all necessary statistics and generates
#' the complete convergence page plot for a given covariate.
#'
#' @param datasets_list Named list of 6 data frames (one per dataset)
#' @param covariate_var Name of the covariate variable
#' @param covariate_levels Character vector of covariate levels
#' @param covariate_colors Named vector of colors for covariate levels
#' @param covariate_name Display name for the covariate (for title)
#' @param dataset_names Character vector of dataset names in display order
#' @param weight_var Name of weight variable. Default "wt".
#' @param age_var Name of age group variable. Default "age_group".
#' @param min_n Minimum sample size. Default 30.
#' @param base_size Base font size. Default 11.
#'
#' @return A list with:
#'   - coef_data_list: Computed coefficients
#'   - meta_reg_list: Meta-regression results
#'   - coef_plot: Stratified coefficient plot
#'   - srh_plot: Stratified SRH trends plot
#'   - combined: Full convergence page
#'
#' @examples
#' # results <- run_convergence_analysis(
#' #   datasets_list = datasets,
#' #   covariate_var = "sex",
#' #   covariate_levels = c("Male", "Female"),
#' #   covariate_colors = c(Male = "#4477AA", Female = "#CC6677"),
#' #   covariate_name = "Sex"
#' # )
#' # ggsave("convergence_sex.pdf", results$combined, width = 16, height = 12)
#'
run_convergence_analysis <- function(
    datasets_list,
    covariate_var,
    covariate_levels,
    covariate_colors,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    weight_var = "wt",
    age_var = "age_group",
    min_n = 30,
    base_size = 11
) {

  # Source computation functions if not loaded
  if (!exists("compute_stratified_age_coefficients")) {
    source(here::here("R", "covariate_sensitivity_functions.R"))
  }

  available_datasets <- intersect(dataset_names, names(datasets_list))

  # Compute stratified age coefficients for each dataset
  message("Computing stratified age coefficients...")
  coef_data_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_stratified_age_coefficients(
      data = datasets_list[[dataset]],
      covariate_var = covariate_var,
      covariate_levels = covariate_levels,
      weight_var = weight_var,
      min_n = min_n
    )
  })
  names(coef_data_list) <- available_datasets

  # Compute meta-regression trends for each dataset and stratum
  message("Computing meta-regression trends...")
  meta_reg_list <- lapply(available_datasets, function(dataset) {
    stratum_results <- lapply(covariate_levels, function(stratum) {
      coef_stratum <- coef_data_list[[dataset]] %>%
        filter(stratum == !!stratum)
      compute_meta_regression_trend(coef_stratum)
    })
    names(stratum_results) <- covariate_levels
    return(stratum_results)
  })
  names(meta_reg_list) <- available_datasets

  # Create plots
  message("Creating stratified coefficient plot...")
  coef_plot <- plot_stratified_age_coefficients(
    coef_data_list = coef_data_list,
    meta_reg_list = meta_reg_list,
    covariate_levels = covariate_levels,
    covariate_colors = covariate_colors,
    dataset_names = available_datasets,
    base_size = base_size
  )

  message("Creating stratified SRH trends plot...")
  srh_plot <- plot_stratified_srh_trends(
    datasets_list = datasets_list,
    covariate_var = covariate_var,
    covariate_levels = covariate_levels,
    covariate_colors = covariate_colors,
    dataset_names = available_datasets,
    age_var = age_var,
    weight_var = weight_var,
    base_size = base_size
  )

  message("Assembling convergence page...")
  combined <- assemble_convergence_page(
    stratified_coef_plot = coef_plot,
    stratified_srh_plot = srh_plot,
    covariate_name = covariate_name
  )

  return(list(
    coef_data_list = coef_data_list,
    meta_reg_list = meta_reg_list,
    coef_plot = coef_plot,
    srh_plot = srh_plot,
    combined = combined
  ))
}


# ==============================================================================
# TEST EXAMPLE (commented out)
# ==============================================================================

# # Test with sex covariate on NHIS only
# if (FALSE) {
#   library(here)
#   library(tidyr)
#   source(here::here("R/paths.R"))
#   source(here::here("R/covariate_sensitivity_functions.R"))
#
#   # Load NHIS data
#   data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) %>%
#     drop_na(srh, age, year, wt, sex, age_group)
#
#   # Define covariate settings
#   sex_levels <- c("Male", "Female")
#   sex_colors <- c(Male = "#4477AA", Female = "#CC6677")
#
#   # Run analysis for NHIS only
#   datasets_list <- list(NHIS = data_nhis)
#
#   # Compute coefficients
#   coef_nhis <- compute_stratified_age_coefficients(
#     data = data_nhis,
#     covariate_var = "sex",
#     covariate_levels = sex_levels,
#     weight_var = "wt"
#   )
#   print(head(coef_nhis))
#
#   # Compute meta-regression
#   meta_male <- compute_meta_regression_trend(
#     coef_nhis %>% filter(stratum == "Male")
#   )
#   meta_female <- compute_meta_regression_trend(
#     coef_nhis %>% filter(stratum == "Female")
#   )
#   print(meta_male)
#   print(meta_female)
#
#   # Create coefficient plot
#   coef_plot <- plot_stratified_age_coefficients(
#     coef_data_list = list(NHIS = coef_nhis),
#     meta_reg_list = list(NHIS = list(Male = meta_male, Female = meta_female)),
#     covariate_levels = sex_levels,
#     covariate_colors = sex_colors,
#     dataset_names = "NHIS"
#   )
#   print(coef_plot)
#
#   # Create SRH trends plot
#   srh_plot <- plot_stratified_srh_trends(
#     datasets_list = datasets_list,
#     covariate_var = "sex",
#     covariate_levels = sex_levels,
#     covariate_colors = sex_colors,
#     dataset_names = "NHIS"
#   )
#   print(srh_plot)
#
#   # Assemble page
#   page <- assemble_convergence_page(
#     stratified_coef_plot = coef_plot,
#     stratified_srh_plot = srh_plot,
#     covariate_name = "Sex"
#   )
#   print(page)
#
#   # Save test output
#   ggsave(
#     here::here("output", "test_convergence_sex_nhis.png"),
#     page, width = 8, height = 10, dpi = 150
#   )
# }
