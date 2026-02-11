# ==============================================================================
# covariate_sensitivity_plots_coefficients.R
# Plotting functions for Coefficients page (Page 2) of covariate sensitivity analyses
#
# Purpose: Visualize how covariate categories relate to SRH over time
#          SC1: Covariate effect on SRH over time (vs reference)
#          SC2: Covariate effect by age group
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)
library(here)

# Source theme for colors and styling
source(here::here("R", "functions", "theme_srh.R"))

# ==============================================================================
# SC1: COVARIATE EFFECT ON SRH OVER TIME
# ==============================================================================

#' Plot covariate effects on SRH over time (SC1)
#'
#' @description
#' Creates a figure showing how each non-reference covariate level relates to SRH
#' over time. Layout: (N-1) rows x 6 columns where N is the number of covariate
#' levels. Each panel shows coefficient with 95% CI ribbon.
#'
#' @param coef_data_list Named list of 6 data frames (one per dataset) from
#'   compute_covariate_coefficients. Each data frame has columns:
#'   year, level, beta, se, ci_lower, ci_upper, p_value, n, is_significant
#' @param covariate_levels Character vector of all covariate levels (including reference)
#' @param reference_level The reference level for the covariate
#' @param covariate_colors Named vector of colors for covariate levels
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 11.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#' @param point_size Size of coefficient points. Default 1.2.
#' @param line_width Width of lines. Default 0.8.
#' @param ribbon_alpha Alpha for CI ribbon. Default 0.2.
#'
#' @return A ggplot/patchwork object
#'
#' @examples
#' # sc1_plot <- plot_sc1_covariate_effect(
#' #   coef_data_list = coef_results,
#' #   covariate_levels = c("NH-White", "NH-Black", "Hispanic", "Other"),
#' #   reference_level = "NH-White",
#' #   covariate_colors = race_colors
#' # )
#'
plot_sc1_covariate_effect <- function(
    coef_data_list,
    covariate_levels,
    reference_level,
    covariate_colors,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 11,
    tilt_x_labels = 45,
    point_size = 1.2,
    line_width = 0.8,
    ribbon_alpha = 0.2
) {

  # Input validation
  stopifnot(is.list(coef_data_list))
  stopifnot(is.character(covariate_levels))
  stopifnot(reference_level %in% covariate_levels)

  # Filter to available datasets
  available_datasets <- intersect(dataset_names, names(coef_data_list))
  if (length(available_datasets) == 0) {
    stop("No datasets found in coef_data_list matching dataset_names")
  }

  # Non-reference levels (these are the rows)
  non_ref_levels <- setdiff(covariate_levels, reference_level)
  n_levels <- length(non_ref_levels)
  n_datasets <- length(available_datasets)

  # Compute global y-axis limits for alignment
  all_coefs <- bind_rows(coef_data_list)
  if (nrow(all_coefs) > 0 && any(!is.na(all_coefs$ci_lower))) {
    y_min <- min(all_coefs$ci_lower, na.rm = TRUE)
    y_max <- max(all_coefs$ci_upper, na.rm = TRUE)
    # Add padding
    y_range <- y_max - y_min
    y_limits <- c(y_min - 0.1 * y_range, y_max + 0.1 * y_range)
  } else {
    y_limits <- NULL
  }

  # Create grid of panels: rows = non-ref levels, columns = datasets
  all_panels <- list()

  for (row_idx in seq_along(non_ref_levels)) {
    level <- non_ref_levels[row_idx]

    for (col_idx in seq_along(available_datasets)) {
      dataset <- available_datasets[col_idx]

      # Filter data to this level
      level_data <- coef_data_list[[dataset]] %>%
        filter(level == !!level)

      # Row label on first column
      row_label_text <- if (col_idx == 1) level else NULL

      panel <- create_sc1_panel(
        coef_data = level_data,
        level_name = level,
        dataset_name = dataset,
        level_color = covariate_colors[level],
        show_title = (row_idx == 1),
        show_ylabel = (col_idx == 1),
        row_label = row_label_text,
        base_size = base_size,
        tilt_x_labels = tilt_x_labels,
        point_size = point_size,
        line_width = line_width,
        ribbon_alpha = ribbon_alpha,
        y_limits = y_limits,
        reference_level = reference_level
      )

      panel_name <- paste0(level, "_", dataset)
      all_panels[[panel_name]] <- panel
    }
  }

  # Arrange panels: rows = levels, columns = datasets
  combined <- wrap_plots(all_panels, ncol = n_datasets, byrow = TRUE)

  # Collect legend at bottom (not really needed since each panel is one color)
  combined <- combined +
    plot_layout(guides = "collect") &
    theme(legend.position = "none")  # No legend needed - row labels show level

  return(combined)
}


#' Create a single panel for SC1 (covariate effect over time)
#'
#' @description
#' Internal helper function to create one panel showing coefficient over time
#' with 95% CI ribbon. Uses plot.tag for row labels.
#'
#' @param coef_data Data frame with year, beta, se, ci_lower, ci_upper
#' @param level_name Name of the covariate level
#' @param dataset_name Name of the dataset
#' @param level_color Color for this level
#' @param show_title Show dataset name as title?
#' @param show_ylabel Show y-axis label?
#' @param row_label Text to show as row label on left (NULL to hide)
#' @param base_size Base font size
#' @param tilt_x_labels X-axis label angle
#' @param point_size Point size
#' @param line_width Line width
#' @param ribbon_alpha Alpha for CI ribbon
#' @param y_limits Y-axis limits for alignment
#' @param reference_level Reference level name for y-axis label
#'
#' @return ggplot object
#'
create_sc1_panel <- function(
    coef_data,
    level_name,
    dataset_name,
    level_color,
    show_title = FALSE,
    show_ylabel = FALSE,
    row_label = NULL,
    base_size = 11,
    tilt_x_labels = 45,
    point_size = 1.2,
    line_width = 0.8,
    ribbon_alpha = 0.2,
    y_limits = NULL,
    reference_level = NULL
) {

  # Handle empty or all-NA data
  valid_data <- coef_data %>%
    filter(!is.na(beta))

  if (nrow(valid_data) == 0) {
    # Return empty plot
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = base_size / 3) +
      theme_void()

    if (show_title) {
      p <- p + labs(title = dataset_name) +
        theme(plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5))
    }

    return(p)
  }

  # Build plot
  p <- ggplot(valid_data, aes(x = year, y = beta)) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4)

  # Add CI ribbon if available
  if (all(c("ci_lower", "ci_upper") %in% names(valid_data))) {
    ribbon_data <- valid_data %>% filter(!is.na(ci_lower), !is.na(ci_upper))
    if (nrow(ribbon_data) > 0) {
      p <- p + geom_ribbon(
        data = ribbon_data,
        aes(ymin = ci_lower, ymax = ci_upper),
        fill = level_color,
        alpha = ribbon_alpha
      )
    }
  }

  # Add line and points
  p <- p +
    geom_line(color = level_color, linewidth = line_width) +
    geom_point(color = level_color, size = point_size)

  # Apply y-axis limits
  if (!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits)
  }

  # Labels
  ylabel <- if (show_ylabel) {
    if (!is.null(reference_level)) {
      paste0("Coef (vs ", reference_level, ")")
    } else {
      "Coefficient"
    }
  } else {
    NULL
  }

  p <- p + labs(
    title = if (show_title) dataset_name else NULL,
    x = NULL,
    y = ylabel,
    tag = row_label
  ) +
  coord_cartesian(clip = "off")

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Adjust left margin for row label
  left_margin <- if (!is.null(row_label)) 45 else 6

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size - 1),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = x_tick_style,
      plot.margin = margin(4, 6, 4, left_margin),
      plot.tag = element_text(size = base_size, face = "bold", angle = 90, vjust = 0.5),
      plot.tag.position = c(-0.08, 0.5)
    )

  return(p)
}


# ==============================================================================
# SC2: COVARIATE EFFECT BY AGE GROUP
# ==============================================================================

#' Plot covariate effects by age group (SC2)
#'
#' @description
#' Creates a figure showing how covariate effects on SRH vary by age group.
#' Layout: 6 rows (datasets) x 7 columns (age groups).
#' Each panel shows N-1 lines (one per non-reference covariate level).
#'
#' @param coef_by_age_data_list Named list of 6 data frames (one per dataset) from
#'   compute_covariate_coefficients_by_age. Each data frame has columns:
#'   year, age_group, level, beta, se, ci_lower, ci_upper, p_value, n, is_significant
#' @param covariate_levels Character vector of all covariate levels (including reference)
#' @param reference_level The reference level for the covariate
#' @param covariate_colors Named vector of colors for covariate levels
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param age_groups Character vector of age group names.
#'   Default: c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
#' @param base_size Base font size. Default 11.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#' @param point_size Size of coefficient points. Default 0.8.
#' @param line_width Width of lines. Default 0.6.
#'
#' @return A ggplot/patchwork object
#'
#' @examples
#' # sc2_plot <- plot_sc2_covariate_effect_by_age(
#' #   coef_by_age_data_list = coef_by_age_results,
#' #   covariate_levels = c("NH-White", "NH-Black", "Hispanic", "Other"),
#' #   reference_level = "NH-White",
#' #   covariate_colors = race_colors
#' # )
#'
plot_sc2_covariate_effect_by_age <- function(
    coef_by_age_data_list,
    covariate_levels,
    reference_level,
    covariate_colors,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    age_groups = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
    base_size = 11,
    tilt_x_labels = 45,
    point_size = 0.8,
    line_width = 0.6
) {

  # Input validation
  stopifnot(is.list(coef_by_age_data_list))
  stopifnot(is.character(covariate_levels))
  stopifnot(reference_level %in% covariate_levels)

  # Filter to available datasets
  available_datasets <- intersect(dataset_names, names(coef_by_age_data_list))
  if (length(available_datasets) == 0) {
    stop("No datasets found in coef_by_age_data_list matching dataset_names")
  }

  # Non-reference levels
  non_ref_levels <- setdiff(covariate_levels, reference_level)

  n_datasets <- length(available_datasets)
  n_age_groups <- length(age_groups)

  # Compute global y-axis limits for alignment
  all_coefs <- bind_rows(coef_by_age_data_list)
  if (nrow(all_coefs) > 0 && any(!is.na(all_coefs$beta))) {
    y_min <- min(all_coefs$beta, na.rm = TRUE)
    y_max <- max(all_coefs$beta, na.rm = TRUE)
    # Add padding
    y_range <- max(y_max - y_min, 0.1)  # Minimum range
    y_limits <- c(y_min - 0.15 * y_range, y_max + 0.15 * y_range)
  } else {
    y_limits <- NULL
  }

  # Create grid of panels: rows = datasets, columns = age groups
  all_panels <- list()

  for (row_idx in seq_along(available_datasets)) {
    dataset <- available_datasets[row_idx]

    for (col_idx in seq_along(age_groups)) {
      age_grp <- age_groups[col_idx]

      # Filter data to this dataset and age group
      panel_data <- coef_by_age_data_list[[dataset]] %>%
        filter(age_group == !!age_grp, level %in% non_ref_levels)

      # Row label on first column
      row_label_text <- if (col_idx == 1) dataset else NULL

      panel <- create_sc2_panel(
        coef_data = panel_data,
        dataset_name = dataset,
        age_group_name = age_grp,
        non_ref_levels = non_ref_levels,
        covariate_colors = covariate_colors,
        show_title = (row_idx == 1),
        show_ylabel = (col_idx == 1),
        row_label = row_label_text,
        show_legend = FALSE,  # Legend collected separately
        base_size = base_size,
        tilt_x_labels = tilt_x_labels,
        point_size = point_size,
        line_width = line_width,
        y_limits = y_limits
      )

      panel_name <- paste0(dataset, "_", age_grp)
      all_panels[[panel_name]] <- panel
    }
  }

  # Arrange panels: rows = datasets, columns = age groups
  combined <- wrap_plots(all_panels, ncol = n_age_groups, byrow = TRUE)

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


#' Create a single panel for SC2 (covariate effect by age group)
#'
#' @description
#' Internal helper function to create one panel showing multiple covariate level
#' coefficients over time. Uses plot.tag for row labels.
#'
#' @param coef_data Data frame with year, level, beta, se
#' @param dataset_name Name of the dataset
#' @param age_group_name Name of the age group
#' @param non_ref_levels Vector of non-reference covariate level names
#' @param covariate_colors Named vector of colors for levels
#' @param show_title Show age group name as title?
#' @param show_ylabel Show y-axis label?
#' @param row_label Text to show as row label on left (NULL to hide)
#' @param show_legend Show legend elements?
#' @param base_size Base font size
#' @param tilt_x_labels X-axis label angle
#' @param point_size Point size
#' @param line_width Line width
#' @param y_limits Y-axis limits for alignment
#'
#' @return ggplot object
#'
create_sc2_panel <- function(
    coef_data,
    dataset_name,
    age_group_name,
    non_ref_levels,
    covariate_colors,
    show_title = FALSE,
    show_ylabel = FALSE,
    row_label = NULL,
    show_legend = TRUE,
    base_size = 11,
    tilt_x_labels = 45,
    point_size = 0.8,
    line_width = 0.6,
    y_limits = NULL
) {

  # Handle empty or all-NA data
  valid_data <- coef_data %>%
    filter(!is.na(beta))

  if (nrow(valid_data) == 0) {
    # Return empty plot
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data", size = base_size / 3.5) +
      theme_void()

    if (show_title) {
      p <- p + labs(title = age_group_name) +
        theme(plot.title = element_text(size = base_size, face = "bold", hjust = 0.5))
    }

    # Still add tag for row label
    if (!is.null(row_label)) {
      p <- p + labs(tag = row_label) +
        theme(
          plot.tag = element_text(size = base_size, face = "bold", angle = 90, vjust = 0.5),
          plot.tag.position = c(-0.1, 0.5),
          plot.margin = margin(4, 6, 4, 30)
        )
    }

    return(p)
  }

  # Ensure level is factor with consistent ordering
  valid_data <- valid_data %>%
    mutate(level = factor(level, levels = non_ref_levels))

  # Build plot
  p <- ggplot(valid_data, aes(x = year, y = beta, color = level, group = level)) +
    # Reference line at zero
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    # Lines and points
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    # Colors
    scale_color_manual(values = covariate_colors, name = "Level", drop = FALSE)

  # Apply y-axis limits
  if (!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits)
  }

  # Labels
  p <- p + labs(
    title = if (show_title) age_group_name else NULL,
    x = NULL,
    y = if (show_ylabel) "Coefficient" else NULL,
    tag = row_label
  ) +
  coord_cartesian(clip = "off")

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 3, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 3)
  }

  # Adjust left margin for row label
  left_margin <- if (!is.null(row_label)) 30 else 4

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size - 2),
      axis.text = element_text(size = base_size - 3, color = "gray30"),
      axis.text.x = x_tick_style,
      plot.margin = margin(2, 4, 2, left_margin),
      plot.tag = element_text(size = base_size, face = "bold", angle = 90, vjust = 0.5),
      plot.tag.position = c(-0.1, 0.5),
      legend.position = if (show_legend) "bottom" else "none"
    ) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}


# ==============================================================================
# ASSEMBLY: COEFFICIENTS PAGE
# ==============================================================================

#' Assemble the complete Coefficients page
#'
#' @description
#' Combines the SC1 (covariate effect over time) and SC2 (covariate effect by
#' age group) plots into a single figure.
#'
#' @param sc1_plot ggplot/patchwork object from plot_sc1_covariate_effect
#' @param sc2_plot ggplot/patchwork object from plot_sc2_covariate_effect_by_age
#' @param covariate_name Character string (e.g., "Race", "Sex") for title
#' @param reference_level Reference level name for subtitle
#' @param base_size Base font size for title. Default 14.
#' @param sc1_height Relative height of SC1 section. Default 1.
#' @param sc2_height Relative height of SC2 section. Default 2.
#'
#' @return A patchwork object ready for saving
#'
assemble_coefficients_page <- function(
    sc1_plot,
    sc2_plot,
    covariate_name,
    reference_level = NULL,
    base_size = 14,
    sc1_height = 1,
    sc2_height = 2
) {

  # Create section labels
  sc1_label <- wrap_elements(
    grid::textGrob(
      "SC1: Covariate Effect on SRH Over Time",
      gp = grid::gpar(fontsize = base_size, fontface = "bold"),
      hjust = 0,
      x = unit(0.02, "npc")
    )
  )

  sc2_label <- wrap_elements(
    grid::textGrob(
      "SC2: Covariate Effect by Age Group",
      gp = grid::gpar(fontsize = base_size, fontface = "bold"),
      hjust = 0,
      x = unit(0.02, "npc")
    )
  )

  # Create shared x-axis label
  x_label_grob <- wrap_elements(
    grid::textGrob(
      "Year",
      gp = grid::gpar(fontsize = base_size)
    )
  )

  # Combine: SC1 section, SC2 section
  combined <- sc1_label / sc1_plot / sc2_label / sc2_plot / x_label_grob +
    plot_layout(heights = c(0.05, sc1_height, 0.05, sc2_height, 0.03))

  # Add overall title
  subtitle_text <- if (!is.null(reference_level)) {
    paste0("Reference level: ", reference_level)
  } else {
    NULL
  }

  combined <- combined +
    plot_annotation(
      title = paste0("Coefficient Analysis: ", covariate_name),
      subtitle = subtitle_text,
      theme = theme(
        plot.title = element_text(
          size = base_size + 4,
          face = "bold",
          hjust = 0.5,
          margin = margin(b = 4)
        ),
        plot.subtitle = element_text(
          size = base_size,
          hjust = 0.5,
          color = "gray40",
          margin = margin(b = 10)
        ),
        plot.margin = margin(10, 10, 10, 10)
      )
    )

  return(combined)
}


# ==============================================================================
# CONVENIENCE FUNCTION: RUN COMPLETE COEFFICIENTS ANALYSIS
# ==============================================================================

#' Run complete coefficients analysis and generate plot
#'
#' @description
#' Convenience function that computes all necessary statistics and generates
#' the complete coefficients page plot for a given covariate.
#'
#' @param datasets_list Named list of 6 data frames (one per dataset)
#' @param covariate_var Name of the covariate variable
#' @param covariate_levels Character vector of covariate levels
#' @param reference_level The reference level for the covariate
#' @param covariate_colors Named vector of colors for covariate levels
#' @param covariate_name Display name for the covariate (for title)
#' @param dataset_names Character vector of dataset names in display order
#' @param weight_var Name of weight variable. Default "wt".
#' @param age_var Name of age group variable. Default "age_group".
#' @param min_n Minimum sample size. Default 30.
#' @param base_size Base font size. Default 11.
#'
#' @return A list with:
#'   - coef_data_list: Computed coefficients (SC1)
#'   - coef_by_age_data_list: Computed coefficients by age (SC2)
#'   - sc1_plot: SC1 plot
#'   - sc2_plot: SC2 plot
#'   - combined: Full coefficients page
#'
#' @examples
#' # results <- run_coefficients_analysis(
#' #   datasets_list = datasets,
#' #   covariate_var = "race_includehisp_f",
#' #   covariate_levels = c("NH-White", "NH-Black", "Hispanic", "Other"),
#' #   reference_level = "NH-White",
#' #   covariate_colors = race_colors,
#' #   covariate_name = "Race/Ethnicity"
#' # )
#' # ggsave("coefficients_race.pdf", results$combined, width = 16, height = 20)
#'
run_coefficients_analysis <- function(
    datasets_list,
    covariate_var,
    covariate_levels,
    reference_level,
    covariate_colors,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    weight_var = "wt",
    age_var = "age_group",
    min_n = 30,
    base_size = 11
) {

  # Source computation functions if not loaded
  if (!exists("compute_covariate_coefficients")) {
    source(here::here("R", "covariate_sensitivity_functions.R"))
  }

  available_datasets <- intersect(dataset_names, names(datasets_list))

  # ===== SC1: Compute covariate coefficients over time =====
  message("Computing covariate coefficients (SC1)...")
  coef_data_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_covariate_coefficients(
      data = datasets_list[[dataset]],
      covariate_var = covariate_var,
      covariate_levels = covariate_levels,
      reference_level = reference_level,
      weight_var = weight_var,
      min_n = min_n
    )
  })
  names(coef_data_list) <- available_datasets

  # ===== SC2: Compute covariate coefficients by age group =====
  message("Computing covariate coefficients by age group (SC2)...")
  coef_by_age_data_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_covariate_coefficients_by_age(
      data = datasets_list[[dataset]],
      covariate_var = covariate_var,
      covariate_levels = covariate_levels,
      reference_level = reference_level,
      age_var = age_var,
      weight_var = weight_var,
      min_n = min_n
    )
  })
  names(coef_by_age_data_list) <- available_datasets

  # ===== Create SC1 plot =====
  message("Creating SC1 plot...")
  sc1_plot <- plot_sc1_covariate_effect(
    coef_data_list = coef_data_list,
    covariate_levels = covariate_levels,
    reference_level = reference_level,
    covariate_colors = covariate_colors,
    dataset_names = available_datasets,
    base_size = base_size
  )

  # ===== Create SC2 plot =====
  message("Creating SC2 plot...")
  sc2_plot <- plot_sc2_covariate_effect_by_age(
    coef_by_age_data_list = coef_by_age_data_list,
    covariate_levels = covariate_levels,
    reference_level = reference_level,
    covariate_colors = covariate_colors,
    dataset_names = available_datasets,
    base_size = base_size
  )

  # ===== Assemble page =====
  message("Assembling coefficients page...")
  combined <- assemble_coefficients_page(
    sc1_plot = sc1_plot,
    sc2_plot = sc2_plot,
    covariate_name = covariate_name,
    reference_level = reference_level,
    base_size = base_size + 2
  )

  return(list(
    coef_data_list = coef_data_list,
    coef_by_age_data_list = coef_by_age_data_list,
    sc1_plot = sc1_plot,
    sc2_plot = sc2_plot,
    combined = combined
  ))
}


# ==============================================================================
# TEST EXAMPLE (commented out)
# ==============================================================================

# # Test with race covariate on subset of data
# if (FALSE) {
#   library(here)
#   library(tidyr)
#   source(here::here("R/paths.R"))
#   source(here::here("R/covariate_sensitivity_functions.R"))
#
#   # Load NHIS and CPS (smaller datasets for testing)
#   data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) %>%
#     drop_na(srh, age, year, wt, race_includehisp_f, age_group)
#   data_cps <- readr::read_rds(derived_path("data_cps.rds")) %>%
#     drop_na(srh, age, year, wt, race_includehisp_f, age_group)
#
#   # Define covariate settings
#   race_levels <- c("NH-White", "NH-Black", "NH-AIAN", "NH-Asian/PI", "Hispanic", "Other")
#   race_ref <- "NH-White"
#   race_colors <- c(
#     "NH-White"    = "#4477AA",
#     "NH-Black"    = "#EE6677",
#     "NH-AIAN"     = "#228833",
#     "NH-Asian/PI" = "#CCBB44",
#     "Hispanic"    = "#66CCEE",
#     "Other"       = "#AA3377"
#   )
#
#   datasets_list <- list(NHIS = data_nhis, CPS = data_cps)
#
#   # Run full analysis
#   results <- run_coefficients_analysis(
#     datasets_list = datasets_list,
#     covariate_var = "race_includehisp_f",
#     covariate_levels = race_levels,
#     reference_level = race_ref,
#     covariate_colors = race_colors,
#     covariate_name = "Race/Ethnicity",
#     dataset_names = c("NHIS", "CPS")
#   )
#
#   # Save test output
#   ggsave(
#     here::here("output", "test_coefficients_race.png"),
#     results$combined, width = 14, height = 18, dpi = 150
#   )
#
#   # View individual plots
#   print(results$sc1_plot)
#   print(results$sc2_plot)
# }
