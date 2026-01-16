# ==============================================================================
# plot_utils.R
# Shared plotting functions for SRH convergence paper
# Ensures visual consistency across Figures 1, 3, and 4
# ==============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)

# Source the theme (assumes it's in same directory)
# source(here::here("R", "functions", "theme_srh.R"))

# ------------------------------------------------------------------------------
# FIGURE 1 / GENERAL TREND PLOTS
# ------------------------------------------------------------------------------

#' Plot trends over time by group
#' 
#' @description Flexible function for plotting any outcome over time by age group.
#'   Used for Figure 1 (mean SRH) and Figure 4 (prevalence).
#'   
#' @param data Data frame with columns: year, group (e.g., age_group), 
#'   estimate, se (or ci_lower/ci_upper)
#' @param y_var Name of the y variable (default "estimate")
#' @param group_var Name of the grouping variable (default "age_group")
#' @param se_var Name of SE variable (optional, for CI ribbons)
#' @param ci_vars Vector of c("ci_lower", "ci_upper") if pre-computed
#' @param y_label Y-axis label
#' @param title Plot title (optional)
#' @param subtitle Plot subtitle (optional)
#' @param show_ci Whether to show confidence interval ribbons (default TRUE)
#' @param color_palette Named vector of colors (default age_colors)
#' @param y_limits Y-axis limits (optional)
#' @param point_size Size of points (default 2)
#' @param line_width Width of lines (default 0.8)
#' @param ci_alpha Alpha for CI ribbons (default 0.2)
#' 
#' @return A ggplot object
#' @export
plot_trends <- function(data,
                        y_var = "estimate",
                        group_var = "age_group",
                        se_var = NULL,
                        ci_vars = NULL,
                        y_label = "Estimate",
                        title = NULL,
                        subtitle = NULL,
                        show_ci = TRUE,
                        color_palette = NULL,
                        y_limits = NULL,
                        point_size = 2,
                        line_width = 0.8,
                        ci_alpha = 0.2) {
  
# Compute CIs if SE provided but not CIs
  if (show_ci && !is.null(se_var) && is.null(ci_vars)) {
    data <- data %>%
      mutate(
        ci_lower = .data[[y_var]] - 1.96 * .data[[se_var]],
        ci_upper = .data[[y_var]] + 1.96 * .data[[se_var]]
      )
    ci_vars <- c("ci_lower", "ci_upper")
  }
  
  # Base plot
  p <- ggplot(data, aes(x = year, y = .data[[y_var]], 
                        color = .data[[group_var]],
                        fill = .data[[group_var]],
                        group = .data[[group_var]])) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size)
  
  # Add CI ribbons if available
  if (show_ci && !is.null(ci_vars)) {
    p <- p + geom_ribbon(
      aes(ymin = .data[[ci_vars[1]]], ymax = .data[[ci_vars[2]]]),
      alpha = ci_alpha,
      color = NA
    )
  }
  
  # Apply color palette
  if (is.null(color_palette)) {
    p <- p + scale_color_age() + scale_fill_age()
  } else {
    p <- p + 
      scale_color_manual(values = color_palette) +
      scale_fill_manual(values = color_palette)
  }
  
  # Apply y limits if specified
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  # Labels and theme
  p <- p +
    labs(
      x = "Year",
      y = y_label,
      color = str_to_title(gsub("_", " ", group_var)),
      fill = str_to_title(gsub("_", " ", group_var)),
      title = title,
      subtitle = subtitle
    ) +
    scale_x_year() +
    theme_srh() +
    guides(fill = "none")  # Don't show fill legend separately
  
  return(p)
}

# ------------------------------------------------------------------------------
# FIGURE 1 PANEL B: COEFFICIENT OVER TIME (SINGLE LINE)
# ------------------------------------------------------------------------------

#' Plot single coefficient trend over time
#' 
#' @description For Figure 1 Panel B: Age coefficient trending toward zero
#' 
#' @param data Data frame with: year, estimate, se (or ci_lower/ci_upper)
#' @param y_label Y-axis label
#' @param hline Y-intercept for reference line (default 0)
#' @param title Plot title
#' @param color Line color
#' 
#' @return A ggplot object
#' @export
plot_coefficient_single <- function(data,
                                    y_label = "Age Coefficient",
                                    hline = 0,
                                    title = NULL,
                                    subtitle = NULL,
                                    color = "#3C5488") {
  
  # Compute CIs if needed
  if (!"ci_lower" %in% names(data) && "se" %in% names(data)) {
    data <- data %>%
      mutate(
        ci_lower = estimate - 1.96 * se,
        ci_upper = estimate + 1.96 * se
      )
  }
  
  p <- ggplot(data, aes(x = year, y = estimate)) +
    geom_hline(yintercept = hline, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                fill = color, alpha = 0.2) +
    geom_line(color = color, linewidth = 0.8) +
    geom_point(color = color, size = 2) +
    labs(
      x = "Year",
      y = y_label,
      title = title,
      subtitle = subtitle
    ) +
    scale_x_year() +
    theme_srh()
  
  return(p)
}

# ------------------------------------------------------------------------------
# FIGURE 3: COEFFICIENT TRENDS BY COVARIATE
# ------------------------------------------------------------------------------

#' Plot coefficient trends for multiple covariates
#' 
#' @description For Figure 3: Stability of covariate-SRH associations over time
#' 
#' @param data Data frame with: year, covariate, estimate, se
#' @param facet_var Variable to facet by (default "covariate")
#' @param hline Reference line y-intercept (default NULL, no line)
#' @param ncol Number of facet columns
#' @param free_y Whether to free y-axis scales across facets
#' 
#' @return A ggplot object
#' @export
plot_coefficient_trends <- function(data,
                                    facet_var = "covariate",
                                    hline = NULL,
                                    ncol = 3,
                                    free_y = TRUE,
                                    title = NULL,
                                    subtitle = NULL) {
  
  # Compute CIs if needed
  if (!"ci_lower" %in% names(data) && "se" %in% names(data)) {
    data <- data %>%
      mutate(
        ci_lower = estimate - 1.96 * se,
        ci_upper = estimate + 1.96 * se
      )
  }
  
  p <- ggplot(data, aes(x = year, y = estimate)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                fill = "#3C5488", alpha = 0.2) +
    geom_line(color = "#3C5488", linewidth = 0.7) +
    geom_point(color = "#3C5488", size = 1.5)
  
  # Add reference line if specified
  if (!is.null(hline)) {
    p <- p + geom_hline(yintercept = hline, linetype = "dashed", color = "gray50")
  }
  
  # Facet
  scales_arg <- if (free_y) "free_y" else "fixed"
  p <- p + facet_wrap(as.formula(paste("~", facet_var)), 
                      ncol = ncol, scales = scales_arg)
  
  p <- p +
    labs(
      x = "Year",
      y = "Coefficient on SRH",
      title = title,
      subtitle = subtitle
    ) +
    scale_x_year() +
    theme_srh() +
    theme(
      strip.text = element_text(size = rel(0.85))
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# MULTI-SURVEY FACETED PLOTS
# ------------------------------------------------------------------------------

#' Plot trends faceted by survey
#' 
#' @description For figures showing same trend across multiple surveys
#' 
#' @param data Data frame with: year, age_group, estimate, se, survey
#' @param ncol Number of facet columns (default 3)
#' @param y_label Y-axis label
#' @param free_y Whether to free y-axis (default FALSE for comparability)
#' 
#' @return A ggplot object
#' @export
plot_trends_by_survey <- function(data,
                                  ncol = 3,
                                  y_label = "Mean Self-Rated Health",
                                  y_limits = NULL,
                                  free_y = FALSE,
                                  title = NULL,
                                  subtitle = NULL) {
  
  # Compute CIs if needed
  if (!"ci_lower" %in% names(data) && "se" %in% names(data)) {
    data <- data %>%
      mutate(
        ci_lower = estimate - 1.96 * se,
        ci_upper = estimate + 1.96 * se
      )
  }
  
  p <- ggplot(data, aes(x = year, y = estimate, 
                        color = age_group, fill = age_group)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.5) +
    facet_wrap(~survey, ncol = ncol, scales = if(free_y) "free_y" else "fixed") +
    scale_color_age() +
    scale_fill_age() +
    labs(
      x = "Year",
      y = y_label,
      color = "Age Group",
      title = title,
      subtitle = subtitle
    ) +
    scale_x_year() +
    theme_srh() +
    guides(fill = "none")
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}

# ------------------------------------------------------------------------------
# COMBINING PANELS
# ------------------------------------------------------------------------------

#' Combine multiple plots into a figure
#' 
#' @description Wrapper around patchwork for consistent multi-panel figures
#' 
#' @param ... ggplot objects to combine
#' @param ncol Number of columns
#' @param labels Panel labels (e.g., c("A", "B"))
#' @param title Overall figure title
#' @param tag_levels Patchwork tag levels (default "A")
#' 
#' @return A patchwork object
#' @export
combine_panels <- function(...,
                           ncol = NULL,
                           nrow = NULL,
                           labels = NULL,
                           title = NULL,
                           subtitle = NULL,
                           tag_levels = "A") {
  
  plots <- list(...)
  
  combined <- wrap_plots(plots, ncol = ncol, nrow = nrow) +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      tag_levels = tag_levels,
      theme = theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40")
      )
    )
  
  return(combined)
}

# ------------------------------------------------------------------------------
# SAVING FIGURES
# ------------------------------------------------------------------------------

#' Save figure in multiple formats
#' 
#' @description Saves PNG (for review) and PDF (for publication)
#' 
#' @param plot ggplot or patchwork object
#' @param filename Base filename without extension (e.g., "fig1_convergence")
#' @param path Directory path (default "output/figures")
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution for PNG
#' 
#' @export
save_figure <- function(plot,
                        filename,
                        path = here::here("output", "figures"),
                        width = 10,
                        height = 7,
                        dpi = 300) {
  
  # Ensure directory exists
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  # Save PNG (for review)
  ggsave(
    filename = file.path(path, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  
  # Save PDF (for publication)
  ggsave(
    filename = file.path(path, paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height
  )
  
  cat("Saved:", filename, "(.png and .pdf)\n")
}
