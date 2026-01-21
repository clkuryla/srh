# ==============================================================================
# plot_categorical_combined.R
# Combined multi-survey plotting functions for supplemental categorical figures
#
# Creates all-surveys-in-one figures following the visual style of fig_1_combined
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(patchwork)
library(grid)

# Source dependencies (caller should source, but provide fallback)
if (!exists("age_colors")) {
  source(here::here("R", "functions", "theme_srh.R"))
}

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Survey order (same as fig_1_combined)
survey_order <- c("BRFSS", "MEPS", "NHIS", "GSS", "CPS", "NHANES")

# Age group order (scheme B)
age_group_order <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

# SRH category order
srh_cat_order_5pt <- c("Poor", "Fair", "Good", "Very Good", "Excellent")
srh_cat_order_4pt <- c("Poor", "Fair", "Good", "Excellent")

# ------------------------------------------------------------------------------
# HELPER: Create a single subplot
# ------------------------------------------------------------------------------

#' Create a single subplot for the combined figures
#'
#' @param data Data for this subplot
#' @param x_var X aesthetic variable (string)
#' @param y_var Y aesthetic variable (string)
#' @param color_var Color/group aesthetic variable (string)
#' @param colors Named vector of colors
#' @param geom_type "line", "area", or "both"
#' @param show_points Show points on line plots
#' @param show_title Show title
#' @param title_text Title text (if show_title = TRUE)
#' @param show_y_title Show y-axis title
#' @param y_title Y-axis title text
#' @param show_x_title Show x-axis title
#' @param y_pct Format y-axis as percentage
#' @param y_limits Y-axis limits
#' @param base_size Base font size
#' @param line_width Line width
#' @param point_size Point size
#' @param tilt_x_labels Angle for x-axis labels
#' @param show_legend Whether to show legend (for patchwork collection)
#' @param legend_title Title for the legend
#'
#' @return A ggplot object
create_subplot <- function(
    data,
    x_var = "year",
    y_var = "value",
    color_var = "group",
    colors = NULL,
    geom_type = "line",
    show_points = FALSE,
    show_title = FALSE,
    title_text = NULL,
    show_y_title = FALSE,
    y_title = NULL,
    show_x_title = FALSE,
    y_pct = FALSE,
    y_limits = NULL,
    base_size = 10,
    line_width = 0.8,
    point_size = 1.0,
    tilt_x_labels = 45,
    area_alpha = 0.8,
    show_legend = FALSE,
    legend_title = NULL
) {

  x_sym <- rlang::sym(x_var)
  y_sym <- rlang::sym(y_var)
  color_sym <- rlang::sym(color_var)

  p <- ggplot(data, aes(x = !!x_sym, y = !!y_sym, color = !!color_sym, fill = !!color_sym, group = !!color_sym))

  # Add geoms based on type
  if (geom_type == "area") {
    p <- p + geom_area(alpha = area_alpha, position = "stack", color = NA)
  } else if (geom_type == "line" || geom_type == "both") {
    p <- p + geom_line(linewidth = line_width)
    if (show_points) {
      p <- p + geom_point(size = point_size)
    }
  }

  # Color/fill scales - control legend visibility
  if (!is.null(colors)) {
    if (show_legend) {
      p <- p +
        scale_color_manual(values = colors, name = legend_title) +
        scale_fill_manual(values = colors, name = legend_title)
    } else {
      p <- p +
        scale_color_manual(values = colors, guide = "none") +
        scale_fill_manual(values = colors, guide = "none")
    }
  }

  # Y-axis formatting
  if (y_pct) {
    p <- p + scale_y_continuous(
      limits = y_limits,
      labels = function(x) paste0(round(x), "%"),
      expand = expansion(mult = c(0.02, 0.05))
    )
  } else if (!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits)
  }

  # Labels
  p <- p + labs(
    title = if (show_title && !is.null(title_text)) title_text else NULL,
    y = if (show_y_title && !is.null(y_title)) y_title else NULL,
    x = if (show_x_title) "Year" else NULL
  )

  # X-axis tick style
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Theme
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.3),
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size - 1),
      axis.title.y = element_text(margin = margin(r = 2)),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = x_tick_style,
      plot.margin = margin(2, 3, 2, 3),
      legend.position = if (show_legend) "bottom" else "none"
    )

  # Legend styling if shown
  if (show_legend) {
    p <- p +
      guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2, size = 3))) +
      theme(
        legend.title = element_text(size = base_size, face = "bold"),
        legend.text = element_text(size = base_size - 1),
        legend.key.width = unit(1.5, "lines")
      )
  }

  return(p)
}


# ==============================================================================
# C1a and C1b: SPREAD STATISTICS (1 row x 6 surveys)
# ==============================================================================

#' Plot variance or entropy over time for all surveys (1x6 layout)
#'
#' @param data Combined spread table with columns: survey, age_group, year, variance, entropy
#' @param metric "variance" or "entropy"
#' @param colors Age group color palette (default: age_colors)
#' @param base_size Base font size
#' @param line_width Line width
#' @param point_size Point size
#' @param show_points Show points on lines
#' @param tilt_x_labels Angle for x-axis labels
#' @param title Optional overall title
#'
#' @return A patchwork object
#' @export
plot_spread_combined <- function(
    data,
    metric = c("variance", "entropy"),
    colors = NULL,
    base_size = 11,
    line_width = 1.0,
    point_size = 1.2,
    show_points = TRUE,
    tilt_x_labels = 45,
    title = NULL
) {

  metric <- match.arg(metric)

  if (is.null(colors)) colors <- age_colors

  # Validate columns
  required_cols <- c("survey", "age_group", "year", metric)
  stopifnot(all(required_cols %in% names(data)))

  # Ensure factor levels
  if (!is.factor(data$survey)) {
    data$survey <- factor(data$survey, levels = survey_order)
  }
  if (!is.factor(data$age_group)) {
    data$age_group <- factor(data$age_group, levels = age_group_order)
  }

  # Y-axis label
  y_label <- if (metric == "variance") "Variance" else "Entropy"

  # Create subplot for each survey
  surveys <- levels(data$survey)
  n_surveys <- length(surveys)

  plots <- lapply(seq_along(surveys), function(i) {
    svy <- surveys[i]
    svy_data <- data %>% filter(survey == svy)

    # Show legend on last plot only (will be collected by patchwork)
    create_subplot(
      data = svy_data,
      x_var = "year",
      y_var = metric,
      color_var = "age_group",
      colors = colors,
      geom_type = "line",
      show_points = show_points,
      show_title = TRUE,
      title_text = svy,
      show_y_title = (i == 1),
      y_title = y_label,
      show_x_title = FALSE,
      base_size = base_size,
      line_width = line_width,
      point_size = point_size,
      tilt_x_labels = tilt_x_labels,
      show_legend = (i == n_surveys),
      legend_title = "Age Group"
    )
  })

  # Combine row of plots
  row <- wrap_plots(plots, nrow = 1)

  # Shared x-axis label
  x_label <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Year", size = (base_size + 2) / .pt) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  # Stack: plots / x-label
  combined <- row / x_label +
    plot_layout(heights = c(1, 0.04), guides = "collect") &
    theme(legend.position = "bottom")

  # Add title if provided
  if (!is.null(title)) {
    combined <- combined +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(size = base_size + 3, face = "bold", hjust = 0.5),
          plot.margin = margin(5, 5, 5, 5)
        )
      )
  }

  return(combined)
}


# ==============================================================================
# C2a: SRH CATEGORY DISTRIBUTION BY AGE (7 cols x 6 rows)
# ==============================================================================

#' Plot SRH category distribution by age group for all surveys (7x6 grid)
#'
#' Columns = age groups, Rows = surveys
#' Each cell shows lines colored by SRH category
#'
#' @param data Combined props table with columns: survey, age_group, year, srh_cat, prop
#' @param base_size Base font size
#' @param line_width Line width
#' @param point_size Point size
#' @param show_points Show points on lines
#' @param tilt_x_labels Angle for x-axis labels
#' @param title Optional overall title
#'
#' @return A patchwork object
#' @export
plot_props_by_age_combined <- function(
    data,
    base_size = 9,
    line_width = 0.7,
    point_size = 0.8,
    show_points = FALSE,
    tilt_x_labels = 45,
    title = NULL
) {

  # Validate columns
  required_cols <- c("survey", "age_group", "year", "srh_cat", "prop")
  stopifnot(all(required_cols %in% names(data)))

  # Ensure factor levels
  if (!is.factor(data$survey)) {
    data$survey <- factor(data$survey, levels = survey_order)
  }
  if (!is.factor(data$age_group)) {
    data$age_group <- factor(data$age_group, levels = age_group_order)
  }
  if (!is.factor(data$srh_cat)) {
    data$srh_cat <- factor(data$srh_cat, levels = srh_cat_order_5pt)
  }

  # Convert prop to percentage
  data <- data %>% mutate(prop_pct = prop * 100)

  surveys <- levels(data$survey)
  age_groups <- levels(data$age_group)
  n_surveys <- length(surveys)
  n_age_groups <- length(age_groups)

  # Build grid of plots
  plot_list <- list()

  for (i in seq_along(surveys)) {
    svy <- surveys[i]
    svy_data <- data %>% filter(survey == svy)

    # Use appropriate colors for GSS (4-point) vs others (5-point)
    colors <- if (svy == "GSS") srh_cat_colors_gss else srh_cat_colors

    for (j in seq_along(age_groups)) {
      ag <- age_groups[j]
      cell_data <- svy_data %>% filter(age_group == ag)

      # Skip if no data for this age group
      if (nrow(cell_data) == 0) {
        plot_list[[length(plot_list) + 1]] <- plot_spacer()
        next
      }

      # Show legend only on last cell of last row (bottom-right)
      is_last_cell <- (i == n_surveys) && (j == n_age_groups)

      p <- create_subplot(
        data = cell_data,
        x_var = "year",
        y_var = "prop_pct",
        color_var = "srh_cat",
        colors = colors,
        geom_type = "line",
        show_points = show_points,
        show_title = (i == 1),
        title_text = if (i == 1) ag else NULL,
        show_y_title = (j == 1),
        y_title = if (j == 1) svy else NULL,
        show_x_title = FALSE,
        y_pct = TRUE,
        y_limits = c(0, NA),
        base_size = base_size,
        line_width = line_width,
        point_size = point_size,
        tilt_x_labels = tilt_x_labels,
        show_legend = is_last_cell,
        legend_title = "SRH Category"
      )

      plot_list[[length(plot_list) + 1]] <- p
    }
  }

  # Combine into grid
  n_cols <- length(age_groups)
  n_rows <- length(surveys)

  grid <- wrap_plots(plot_list, ncol = n_cols, nrow = n_rows, byrow = TRUE)

  # Shared x-axis label
  x_label <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Year", size = (base_size + 2) / .pt) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  combined <- grid / x_label +
    plot_layout(heights = c(1, 0.02), guides = "collect") &
    theme(legend.position = "bottom")

  # Add title if provided
  if (!is.null(title)) {
    combined <- combined +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(size = base_size + 3, face = "bold", hjust = 0.5),
          plot.margin = margin(5, 5, 5, 5)
        )
      )
  }

  return(combined)
}


# ==============================================================================
# C2b: STACKED AREA CHARTS BY AGE (7 cols x 6 rows)
# ==============================================================================

#' Plot stacked area charts by age group for all surveys (7x6 grid)
#'
#' Columns = age groups, Rows = surveys
#' Each cell shows stacked area filled by SRH category
#'
#' @param data Combined props table with columns: survey, age_group, year, srh_cat, prop
#' @param base_size Base font size
#' @param area_alpha Fill transparency
#' @param tilt_x_labels Angle for x-axis labels
#' @param title Optional overall title
#'
#' @return A patchwork object
#' @export
plot_stacked_combined <- function(
    data,
    base_size = 9,
    area_alpha = 0.85,
    tilt_x_labels = 45,
    title = NULL
) {

  # Validate columns
  required_cols <- c("survey", "age_group", "year", "srh_cat", "prop")
  stopifnot(all(required_cols %in% names(data)))

  # Ensure factor levels
  if (!is.factor(data$survey)) {
    data$survey <- factor(data$survey, levels = survey_order)
  }
  if (!is.factor(data$age_group)) {
    data$age_group <- factor(data$age_group, levels = age_group_order)
  }
  if (!is.factor(data$srh_cat)) {
    data$srh_cat <- factor(data$srh_cat, levels = srh_cat_order_5pt)
  }

  # Convert prop to percentage
  data <- data %>% mutate(prop_pct = prop * 100)

  surveys <- levels(data$survey)
  age_groups <- levels(data$age_group)
  n_surveys <- length(surveys)
  n_age_groups <- length(age_groups)

  # Build grid of plots
  plot_list <- list()

  for (i in seq_along(surveys)) {
    svy <- surveys[i]
    svy_data <- data %>% filter(survey == svy)

    # Use appropriate colors for GSS (4-point) vs others (5-point)
    colors <- if (svy == "GSS") srh_cat_colors_gss else srh_cat_colors

    for (j in seq_along(age_groups)) {
      ag <- age_groups[j]
      cell_data <- svy_data %>% filter(age_group == ag)

      # Skip if no data for this age group
      if (nrow(cell_data) == 0) {
        plot_list[[length(plot_list) + 1]] <- plot_spacer()
        next
      }

      # Reverse factor for stacking (Excellent on top)
      cell_data <- cell_data %>%
        mutate(srh_cat = fct_rev(srh_cat))

      # Show legend only on last cell of last row (bottom-right)
      is_last_cell <- (i == n_surveys) && (j == n_age_groups)

      # Build stacked area plot manually
      p <- ggplot(cell_data, aes(x = year, y = prop_pct, fill = srh_cat)) +
        geom_area(alpha = area_alpha, position = "stack", color = NA) +
        scale_y_continuous(
          limits = c(0, 100),
          labels = function(x) paste0(round(x), "%"),
          expand = expansion(mult = c(0, 0.02))
        ) +
        labs(
          title = if (i == 1) ag else NULL,
          y = if (j == 1) svy else NULL,
          x = NULL
        ) +
        theme_minimal(base_size = base_size) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
          panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.3),
          plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
          axis.title = element_text(size = base_size - 1),
          axis.title.y = element_text(margin = margin(r = 2)),
          axis.text = element_text(size = base_size - 2, color = "gray30"),
          axis.text.x = element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1),
          plot.margin = margin(2, 3, 2, 3),
          legend.position = if (is_last_cell) "bottom" else "none"
        )

      # Add fill scale with or without legend
      if (is_last_cell) {
        p <- p +
          scale_fill_manual(values = colors, name = "SRH Category") +
          guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
          theme(
            legend.title = element_text(size = base_size, face = "bold"),
            legend.text = element_text(size = base_size - 1)
          )
      } else {
        p <- p + scale_fill_manual(values = colors, guide = "none")
      }

      plot_list[[length(plot_list) + 1]] <- p
    }
  }

  # Combine into grid
  n_cols <- length(age_groups)
  n_rows <- length(surveys)

  grid <- wrap_plots(plot_list, ncol = n_cols, nrow = n_rows, byrow = TRUE)

  # Shared x-axis label
  x_label <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Year", size = (base_size + 2) / .pt) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  combined <- grid / x_label +
    plot_layout(heights = c(1, 0.02), guides = "collect") &
    theme(legend.position = "bottom")

  # Add title if provided
  if (!is.null(title)) {
    combined <- combined +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(size = base_size + 3, face = "bold", hjust = 0.5),
          plot.margin = margin(5, 5, 5, 5)
        )
      )
  }

  return(combined)
}


# ==============================================================================
# C2c: SRH CATEGORY PREVALENCE BY AGE (5 cols x 6 rows)
# ==============================================================================

#' Plot SRH category prevalence faceted by SRH category for all surveys (5x6 grid)
#'
#' Columns = SRH categories, Rows = surveys
#' Each cell shows lines colored by age group
#' GSS row has empty "Very Good" column
#'
#' @param data Combined props table with columns: survey, age_group, year, srh_cat, prop
#' @param colors Age group color palette
#' @param base_size Base font size
#' @param line_width Line width
#' @param point_size Point size
#' @param show_points Show points on lines
#' @param tilt_x_labels Angle for x-axis labels
#' @param title Optional overall title
#'
#' @return A patchwork object
#' @export
plot_cat_prevalence_combined <- function(
    data,
    colors = NULL,
    base_size = 9,
    line_width = 0.7,
    point_size = 0.8,
    show_points = FALSE,
    tilt_x_labels = 45,
    title = NULL
) {

  if (is.null(colors)) colors <- age_colors

  # Validate columns
  required_cols <- c("survey", "age_group", "year", "srh_cat", "prop")
  stopifnot(all(required_cols %in% names(data)))

  # Ensure factor levels
  if (!is.factor(data$survey)) {
    data$survey <- factor(data$survey, levels = survey_order)
  }
  if (!is.factor(data$age_group)) {
    data$age_group <- factor(data$age_group, levels = age_group_order)
  }
  if (!is.factor(data$srh_cat)) {
    data$srh_cat <- factor(data$srh_cat, levels = srh_cat_order_5pt)
  }

  # Convert prop to percentage
  data <- data %>% mutate(prop_pct = prop * 100)

  surveys <- levels(data$survey)
  srh_cats <- srh_cat_order_5pt
  n_surveys <- length(surveys)
  n_cats <- length(srh_cats)

  # Build grid of plots
  plot_list <- list()

  for (i in seq_along(surveys)) {
    svy <- surveys[i]
    svy_data <- data %>% filter(survey == svy)

    for (j in seq_along(srh_cats)) {
      cat_name <- srh_cats[j]
      cell_data <- svy_data %>% filter(srh_cat == cat_name)

      # GSS doesn't have "Very Good" - use spacer
      if (svy == "GSS" && cat_name == "Very Good") {
        plot_list[[length(plot_list) + 1]] <- plot_spacer()
        next
      }

      # Skip if no data
      if (nrow(cell_data) == 0) {
        plot_list[[length(plot_list) + 1]] <- plot_spacer()
        next
      }

      # Show legend only on last cell of last row (bottom-right)
      is_last_cell <- (i == n_surveys) && (j == n_cats)

      p <- create_subplot(
        data = cell_data,
        x_var = "year",
        y_var = "prop_pct",
        color_var = "age_group",
        colors = colors,
        geom_type = "line",
        show_points = show_points,
        show_title = (i == 1),
        title_text = if (i == 1) cat_name else NULL,
        show_y_title = (j == 1),
        y_title = if (j == 1) svy else NULL,
        show_x_title = FALSE,
        y_pct = TRUE,
        y_limits = c(0, NA),
        base_size = base_size,
        line_width = line_width,
        point_size = point_size,
        tilt_x_labels = tilt_x_labels,
        show_legend = is_last_cell,
        legend_title = "Age Group"
      )

      plot_list[[length(plot_list) + 1]] <- p
    }
  }

  # Combine into grid
  n_cols <- length(srh_cats)
  n_rows <- length(surveys)

  grid <- wrap_plots(plot_list, ncol = n_cols, nrow = n_rows, byrow = TRUE)

  # Shared x-axis label
  x_label <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Year", size = (base_size + 2) / .pt) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  combined <- grid / x_label +
    plot_layout(heights = c(1, 0.02), guides = "collect") &
    theme(legend.position = "bottom")

  # Add title if provided
  if (!is.null(title)) {
    combined <- combined +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(size = base_size + 3, face = "bold", hjust = 0.5),
          plot.margin = margin(5, 5, 5, 5)
        )
      )
  }

  return(combined)
}


# ==============================================================================
# C3: AGE COMPOSITION BY SRH CATEGORY (5 cols x 6 rows)
# ==============================================================================

#' Plot age composition within SRH categories for all surveys (5x6 grid)
#'
#' Columns = SRH categories, Rows = surveys
#' Each cell shows lines colored by age group (age groups sum to 100%)
#' GSS row has empty "Very Good" column
#'
#' @param data Combined age_comp table with columns: survey, srh_cat, year, age_group, prop
#' @param colors Age group color palette
#' @param base_size Base font size
#' @param line_width Line width
#' @param point_size Point size
#' @param show_points Show points on lines
#' @param tilt_x_labels Angle for x-axis labels
#' @param title Optional overall title
#'
#' @return A patchwork object
#' @export
plot_age_comp_combined <- function(
    data,
    colors = NULL,
    base_size = 9,
    line_width = 0.7,
    point_size = 0.8,
    show_points = FALSE,
    tilt_x_labels = 45,
    title = NULL
) {

  if (is.null(colors)) colors <- age_colors

  # Validate columns
  required_cols <- c("survey", "srh_cat", "year", "age_group", "prop")
  stopifnot(all(required_cols %in% names(data)))

  # Ensure factor levels
  if (!is.factor(data$survey)) {
    data$survey <- factor(data$survey, levels = survey_order)
  }
  if (!is.factor(data$age_group)) {
    data$age_group <- factor(data$age_group, levels = age_group_order)
  }
  if (!is.factor(data$srh_cat)) {
    data$srh_cat <- factor(data$srh_cat, levels = srh_cat_order_5pt)
  }

  # Convert prop to percentage
  data <- data %>% mutate(prop_pct = prop * 100)

  surveys <- levels(data$survey)
  srh_cats <- srh_cat_order_5pt
  n_surveys <- length(surveys)
  n_cats <- length(srh_cats)

  # Build grid of plots
  plot_list <- list()

  for (i in seq_along(surveys)) {
    svy <- surveys[i]
    svy_data <- data %>% filter(survey == svy)

    for (j in seq_along(srh_cats)) {
      cat_name <- srh_cats[j]
      cell_data <- svy_data %>% filter(srh_cat == cat_name)

      # GSS doesn't have "Very Good" - use spacer
      if (svy == "GSS" && cat_name == "Very Good") {
        plot_list[[length(plot_list) + 1]] <- plot_spacer()
        next
      }

      # Skip if no data
      if (nrow(cell_data) == 0) {
        plot_list[[length(plot_list) + 1]] <- plot_spacer()
        next
      }

      # Show legend only on last cell of last row (bottom-right)
      is_last_cell <- (i == n_surveys) && (j == n_cats)

      p <- create_subplot(
        data = cell_data,
        x_var = "year",
        y_var = "prop_pct",
        color_var = "age_group",
        colors = colors,
        geom_type = "line",
        show_points = show_points,
        show_title = (i == 1),
        title_text = if (i == 1) cat_name else NULL,
        show_y_title = (j == 1),
        y_title = if (j == 1) svy else NULL,
        show_x_title = FALSE,
        y_pct = TRUE,
        y_limits = c(0, NA),
        base_size = base_size,
        line_width = line_width,
        point_size = point_size,
        tilt_x_labels = tilt_x_labels,
        show_legend = is_last_cell,
        legend_title = "Age Group"
      )

      plot_list[[length(plot_list) + 1]] <- p
    }
  }

  # Combine into grid
  n_cols <- length(srh_cats)
  n_rows <- length(surveys)

  grid <- wrap_plots(plot_list, ncol = n_cols, nrow = n_rows, byrow = TRUE)

  # Shared x-axis label
  x_label <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Year", size = (base_size + 2) / .pt) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  combined <- grid / x_label +
    plot_layout(heights = c(1, 0.02), guides = "collect") &
    theme(legend.position = "bottom")

  # Add title if provided
  if (!is.null(title)) {
    combined <- combined +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(size = base_size + 3, face = "bold", hjust = 0.5),
          plot.margin = margin(5, 5, 5, 5)
        )
      )
  }

  return(combined)
}
