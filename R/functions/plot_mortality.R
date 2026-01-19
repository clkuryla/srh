# ==============================================================================
# plot_mortality.R
# Figure generation functions for SRH-mortality analysis
# Uses shared theme and palettes from theme_srh.R
# ==============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)

# Source theme (assumes it's loaded via 00_setup.R)
# source(here::here("R", "functions", "theme_srh.R"))

# ------------------------------------------------------------------------------
# COEFFICIENT PLOTS (LOG-HAZARD RATIO)
# ------------------------------------------------------------------------------

#' Plot mortality coefficients over time by age group
#'
#' @description Main figure: SRH-mortality coefficient (log-HR) vs window start year,
#'   colored by age group. Reference line at y=0.
#'
#' @param results Results from run_sliding_windows_by_age()
#' @param window_length Filter to specific window length (optional)
#' @param show_ci Show confidence interval ribbons (default TRUE)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param y_label Y-axis label
#' @param x_label X-axis label
#' @param point_size Size of points (default 2)
#' @param line_width Width of lines (default 0.8)
#' @param ci_alpha Alpha for CI ribbons (default 0.15)
#'
#' @return A ggplot object
#' @export
plot_mortality_coef <- function(results,
                                window_length = NULL,
                                show_ci = TRUE,
                                title = NULL,
                                subtitle = NULL,
                                y_label = "Coefficient (log-HR)",
                                x_label = "Window Start Year",
                                point_size = 2,
                                line_width = 0.8,
                                ci_alpha = 0.15) {

  # Filter to specific window length if provided
  plot_data <- results %>%
    filter(converged)

  if (!is.null(window_length)) {
    plot_data <- plot_data %>%
      filter(.data$window_length == .env$window_length)
  }

  if (nrow(plot_data) == 0) {
    rlang::warn("No converged models to plot")
    return(ggplot() + theme_void())
  }

  # Compute CI for coefficient (log scale)
  # conf_low/conf_high are for HR, need to convert to log scale
  plot_data <- plot_data %>%
    mutate(
      coef_ci_low = log(conf_low),
      coef_ci_high = log(conf_high)
    )

  # Base plot
  p <- ggplot(plot_data, aes(
    x = start_year,
    y = coef,
    color = age_group,
    fill = age_group,
    group = age_group
  ))

  # Reference line at 0
  p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5)

  # Add CI ribbons if requested
  if (show_ci) {
    p <- p + geom_ribbon(
      aes(ymin = coef_ci_low, ymax = coef_ci_high),
      alpha = ci_alpha,
      color = NA
    )
  }

  # Lines and points
  p <- p +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size)

  # Apply age color palette and theme
  p <- p +
    scale_color_age() +
    scale_fill_age() +
    scale_x_year() +
    labs(
      x = x_label,
      y = y_label,
      color = "Age Group",
      title = title,
      subtitle = subtitle
    ) +
    theme_srh() +
    guides(fill = "none")

  return(p)
}

# ------------------------------------------------------------------------------
# HAZARD RATIO PLOTS
# ------------------------------------------------------------------------------

#' Plot hazard ratios over time by age group
#'
#' @description Hazard ratio (not log-transformed) vs window start year,
#'   with reference line at y=1. Y-axis on log scale.
#'
#' @param results Results from run_sliding_windows_by_age()
#' @param window_length Filter to specific window length (optional)
#' @param show_ci Show confidence interval ribbons (default TRUE)
#' @param log_scale Use log scale for y-axis (default TRUE)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param y_label Y-axis label
#' @param x_label X-axis label
#'
#' @return A ggplot object
#' @export
plot_mortality_hr <- function(results,
                              window_length = NULL,
                              show_ci = TRUE,
                              log_scale = TRUE,
                              title = NULL,
                              subtitle = NULL,
                              y_label = "Hazard Ratio per 1-unit increase in SRH",
                              x_label = "Window Start Year",
                              point_size = 2,
                              line_width = 0.8,
                              ci_alpha = 0.15) {

  # Filter to specific window length if provided
  plot_data <- results %>%
    filter(converged)

  if (!is.null(window_length)) {
    plot_data <- plot_data %>%
      filter(.data$window_length == .env$window_length)
  }

  if (nrow(plot_data) == 0) {
    rlang::warn("No converged models to plot")
    return(ggplot() + theme_void())
  }

  # Base plot
  p <- ggplot(plot_data, aes(
    x = start_year,
    y = hr,
    color = age_group,
    fill = age_group,
    group = age_group
  ))

  # Reference line at HR=1
  p <- p + geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5)

  # Add CI ribbons
  if (show_ci) {
    p <- p + geom_ribbon(
      aes(ymin = conf_low, ymax = conf_high),
      alpha = ci_alpha,
      color = NA
    )
  }

  # Lines and points
  p <- p +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size)

  # Apply color palette and theme
  p <- p +
    scale_color_age() +
    scale_fill_age() +
    scale_x_year()

  # Log scale for y-axis
  if (log_scale) {
    p <- p + scale_y_log10()
  }

  p <- p +
    labs(
      x = x_label,
      y = y_label,
      color = "Age Group",
      title = title,
      subtitle = subtitle
    ) +
    theme_srh() +
    guides(fill = "none")

  return(p)
}

# ------------------------------------------------------------------------------
# MULTI-WINDOW FACETED PLOTS
# ------------------------------------------------------------------------------

#' Plot coefficients for multiple window lengths
#'
#' @description Faceted figure showing coefficient trends across
#'   multiple window lengths for comparison.
#'
#' @param results Results including multiple window lengths
#' @param plot_type "coef" for coefficients, "hr" for hazard ratios
#' @param ncol Number of facet columns
#' @param title Overall plot title
#'
#' @return A ggplot object
#' @export
plot_mortality_multi_window <- function(results,
                                        plot_type = c("coef", "hr"),
                                        ncol = 3,
                                        title = NULL,
                                        subtitle = NULL,
                                        point_size = 1.5,
                                        line_width = 0.6,
                                        ci_alpha = 0.12) {

  plot_type <- match.arg(plot_type)

  plot_data <- results %>%
    filter(converged) %>%
    mutate(
      # Create facet label
      window_label = paste0(window_length, "-Year Window"),
      window_label = factor(window_label, levels = paste0(sort(unique(window_length)), "-Year Window")),
      # CIs for coefficient
      coef_ci_low = log(conf_low),
      coef_ci_high = log(conf_high)
    )

  if (nrow(plot_data) == 0) {
    rlang::warn("No converged models to plot")
    return(ggplot() + theme_void())
  }

  if (plot_type == "coef") {
    # Coefficient plot
    p <- ggplot(plot_data, aes(
      x = start_year,
      y = coef,
      color = age_group,
      fill = age_group,
      group = age_group
    )) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
      geom_ribbon(
        aes(ymin = coef_ci_low, ymax = coef_ci_high),
        alpha = ci_alpha,
        color = NA
      ) +
      geom_line(linewidth = line_width) +
      geom_point(size = point_size) +
      labs(y = "Coefficient (log-HR)")

  } else {
    # Hazard ratio plot
    p <- ggplot(plot_data, aes(
      x = start_year,
      y = hr,
      color = age_group,
      fill = age_group,
      group = age_group
    )) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.4) +
      geom_ribbon(
        aes(ymin = conf_low, ymax = conf_high),
        alpha = ci_alpha,
        color = NA
      ) +
      geom_line(linewidth = line_width) +
      geom_point(size = point_size) +
      scale_y_log10() +
      labs(y = "Hazard Ratio")
  }

  p <- p +
    facet_wrap(~window_label, ncol = ncol, scales = "free_x") +
    scale_color_age() +
    scale_fill_age() +
    scale_x_year() +
    labs(
      x = "Window Start Year",
      color = "Age Group",
      title = title,
      subtitle = subtitle
    ) +
    theme_srh() +
    theme(
      strip.text = element_text(size = rel(0.9)),
      legend.position = "bottom"
    ) +
    guides(fill = "none")

  return(p)
}

# ------------------------------------------------------------------------------
# COMBINED FIGURE (COEFFICIENT + HR PANELS)
# ------------------------------------------------------------------------------

#' Create combined mortality figure
#'
#' @description Creates two-panel figure with coefficient and HR plots
#'   for a specific window length.
#'
#' @param results Results from run_sliding_windows_by_age()
#' @param window_length Window length to plot
#' @param title Overall figure title
#'
#' @return A patchwork object
#' @export
plot_mortality_combined <- function(results,
                                    window_length,
                                    title = NULL,
                                    subtitle = NULL) {

  # Panel A: Coefficients
  p_coef <- plot_mortality_coef(
    results,
    window_length = window_length,
    title = "A. Coefficient (Log-Hazard Ratio)",
    y_label = "Coefficient (log-HR)"
  ) +
    theme(legend.position = "none")

  # Panel B: Hazard Ratios
  p_hr <- plot_mortality_hr(
    results,
    window_length = window_length,
    title = "B. Hazard Ratio",
    y_label = "Hazard Ratio"
  )

  # Combine with patchwork
  combined <- p_coef / p_hr +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40")
      )
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  return(combined)
}

# ------------------------------------------------------------------------------
# DIAGNOSTIC PLOTS
# ------------------------------------------------------------------------------

#' Plot sample sizes by age group and window
#'
#' @description Diagnostic plot showing sample sizes across windows
#'
#' @param results Results from run_sliding_windows_by_age()
#' @param window_length Filter to specific window length (optional)
#'
#' @return A ggplot object
#' @export
plot_sample_sizes <- function(results, window_length = NULL) {

  plot_data <- results

  if (!is.null(window_length)) {
    plot_data <- plot_data %>%
      filter(.data$window_length == .env$window_length)
  }

  ggplot(plot_data, aes(x = start_year, y = n, color = age_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    scale_color_age() +
    scale_x_year() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = "Window Start Year",
      y = "Sample Size",
      color = "Age Group",
      title = "Sample Sizes by Age Group and Window"
    ) +
    theme_srh()
}

#' Plot number of events by age group and window
#'
#' @param results Results from run_sliding_windows_by_age()
#' @param window_length Filter to specific window length (optional)
#'
#' @return A ggplot object
#' @export
plot_event_counts <- function(results, window_length = NULL) {

  plot_data <- results

  if (!is.null(window_length)) {
    plot_data <- plot_data %>%
      filter(.data$window_length == .env$window_length)
  }

  ggplot(plot_data, aes(x = start_year, y = n_events, color = age_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    scale_color_age() +
    scale_x_year() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = "Window Start Year",
      y = "Number of Deaths",
      color = "Age Group",
      title = "Event Counts by Age Group and Window"
    ) +
    theme_srh()
}
