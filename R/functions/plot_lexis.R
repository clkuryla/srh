# ==============================================================================
# plot_lexis.R
# Lexis diagram functions for APC visualization (Figure 2)
#
# Purpose: Create Lexis surface plots (heatmaps) showing mean SRH by age and year.
#   - Diagonal patterns indicate COHORT effects (birth year)
#   - Vertical patterns indicate PERIOD effects (calendar year)
#   - Horizontal patterns indicate AGE effects
#
# Key insight: Cohort runs along diagonals because birth_year = year - age.
# If there's no diagonal banding, cohort effects are weak/absent.
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)
library(cowplot)

# Source the theme (caller should source, but provide fallback)
if (!exists("theme_srh")) {
  source(here::here("R", "functions", "theme_srh.R"))
}

# ------------------------------------------------------------------------------
# LEXIS SURFACE PLOT
# ------------------------------------------------------------------------------

#' Create Lexis surface diagram for a single survey
#'
#' @description Creates a heatmap with year on x-axis, age on y-axis, and
#'   color representing the outcome (e.g., mean SRH). Diagonal patterns
#'   indicate cohort effects; vertical patterns indicate period effects;
#'   horizontal patterns indicate age effects.
#'
#' @param data Data frame with columns: year, age, and value variable
#' @param value_var Name of the value variable (default "mean_srh")
#' @param age_var Name of the age variable (default "age")
#' @param color_scale Color scale to use. Options:
#'   - "viridis" (default): Yellow-green-blue, perceptually uniform
#'   - "turbo": Rainbow-like, high contrast
#'   - "plasma": Purple-orange-yellow
#'   - "magma": Black-purple-orange-yellow
#'   - "inferno": Black-purple-red-yellow
#'   - "diverging": Blue-white-red diverging scale (uses low/mid/high_color)
#' @param low_color Color for low values (only used if color_scale = "diverging")
#' @param high_color Color for high values (only used if color_scale = "diverging")
#' @param mid_color Color for midpoint (only used if color_scale = "diverging")
#' @param midpoint Midpoint for diverging scale (default NULL, uses median)
#' @param reverse_colors Reverse the color direction? (default FALSE)
#'   Set TRUE so that higher SRH (better health) = warmer colors
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param legend_title Legend title (default "Mean SRH")
#' @param show_cohort_lines Whether to show diagonal cohort lines (default TRUE)
#' @param cohort_years Birth years for reference lines (default auto-calculated)
#' @param cohort_line_color Color of cohort lines (default "gray30")
#' @param cohort_line_alpha Alpha of cohort lines (default 0.6)
#' @param cohort_line_spacing Years between cohort lines (default 10). Set to 20
#'   for cleaner visuals with fewer lines.
#' @param base_size Base font size
#'
#' @return A ggplot object
#' @export
plot_lexis_surface <- function(data,
                               value_var = "mean_srh",
                               age_var = "age",
                               color_scale = "turbo",
                               low_color = "#2166AC",
                               high_color = "#B2182B",
                               mid_color = "#F7F7F7",
                               midpoint = NULL,
                               reverse_colors = TRUE,
                               title = NULL,
                               subtitle = NULL,
                               legend_title = "Mean SRH",
                               show_cohort_lines = TRUE,
                               cohort_years = NULL,
                               cohort_line_color = "gray30",
                               cohort_line_alpha = 0.6,
                               cohort_line_spacing = 10,
                               base_size = 11) {

  # --- Input validation ---
  stopifnot(is.data.frame(data))
  stopifnot(all(c("year", age_var, value_var) %in% names(data)))

  # Auto-calculate cohort years if not provided and lines are requested
  if (show_cohort_lines && is.null(cohort_years)) {
    year_range <- range(data$year, na.rm = TRUE)
    age_range <- range(data[[age_var]], na.rm = TRUE)

    birth_min <- year_range[1] - age_range[2]
    birth_max <- year_range[2] - age_range[1]

    cohort_years <- seq(
      floor(birth_min / cohort_line_spacing) * cohort_line_spacing,
      ceiling(birth_max / cohort_line_spacing) * cohort_line_spacing,
      by = cohort_line_spacing
    )
  }

  # --- Base plot ---
  p <- ggplot(data, aes(x = year, y = .data[[age_var]], fill = .data[[value_var]])) +
    geom_tile()

  # --- Color scale ---
  if (color_scale == "diverging") {
    # Diverging scale (blue-white-red)
    if (is.null(midpoint)) {
      midpoint <- median(data[[value_var]], na.rm = TRUE)
    }
    p <- p + scale_fill_gradient2(
      low = if (reverse_colors) high_color else low_color,
      mid = mid_color,
      high = if (reverse_colors) low_color else high_color,
      midpoint = midpoint,
      name = legend_title,
      guide = guide_colorbar(barwidth = 1, barheight = 8)
    )
  } else {
    # Continuous viridis-family scales
    viridis_option <- switch(
      color_scale,
      "viridis" = "D",
      "turbo" = "H",
      "plasma" = "C",
      "magma" = "A",
      "inferno" = "B",
      "D"  # default to viridis
    )
    p <- p + scale_fill_viridis_c(
      option = viridis_option,
      direction = if (reverse_colors) -1 else 1,
      name = legend_title,
      guide = guide_colorbar(barwidth = 1, barheight = 8)
    )
  }

  # --- Add cohort lines ---
  if (show_cohort_lines && length(cohort_years) > 0) {
    for (birth_year in cohort_years) {
      p <- p + geom_abline(
        intercept = -birth_year,
        slope = 1,
        linetype = "dashed",
        color = cohort_line_color,
        alpha = cohort_line_alpha,
        linewidth = 0.4
      )
    }
  }

  # --- Labels and theme ---
  p <- p +
    labs(
      x = "Year",
      y = "Age",
      title = title,
      subtitle = subtitle
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_srh(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )

  return(p)
}

# ------------------------------------------------------------------------------
# LEXIS WITH CONTOURS (Alternative visualization)
# ------------------------------------------------------------------------------

#' Create Lexis diagram with contour lines
#'
#' @description Alternative to tile plot using filled contours. Can be cleaner
#'   for showing smooth patterns, but may obscure fine structure.
#'
#' @param data Data frame with: year, age, value
#' @param value_var Name of value variable (default "mean_srh")
#' @param age_var Name of age variable (default "age")
#' @param n_bins Number of contour levels (default 10)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param show_cohort_lines Whether to show diagonal cohort lines (default TRUE)
#' @param cohort_line_spacing Spacing between cohort lines in years (default 10)
#' @param base_size Base font size
#'
#' @return A ggplot object
#' @export
plot_lexis_contour <- function(data,
                               value_var = "mean_srh",
                               age_var = "age",
                               n_bins = 10,
                               color_scale = "turbo",
                               reverse_colors = TRUE,
                               title = NULL,
                               subtitle = NULL,
                               show_cohort_lines = TRUE,
                               cohort_line_spacing = 10,
                               base_size = 11) {

  viridis_option <- switch(
    color_scale,
    "viridis" = "D",
    "turbo" = "H",
    "plasma" = "C",
    "magma" = "A",
    "inferno" = "B",
    "H"  # default to turbo
  )

  p <- ggplot(data, aes(x = year, y = .data[[age_var]], z = .data[[value_var]])) +
    geom_contour_filled(bins = n_bins) +
    scale_fill_viridis_d(
      option = viridis_option,
      direction = if (reverse_colors) -1 else 1,
      name = "Mean SRH"
    )

  # Add cohort lines if requested

  if (show_cohort_lines) {
    year_range <- range(data$year, na.rm = TRUE)
    age_range <- range(data[[age_var]], na.rm = TRUE)
    birth_min <- year_range[1] - age_range[2]
    birth_max <- year_range[2] - age_range[1]

    cohort_years <- seq(
      floor(birth_min / cohort_line_spacing) * cohort_line_spacing,
      ceiling(birth_max / cohort_line_spacing) * cohort_line_spacing,
      by = cohort_line_spacing
    )

    for (birth_year in cohort_years) {
      p <- p + geom_abline(
        intercept = -birth_year,
        slope = 1,
        linetype = "dashed",
        color = "white",
        alpha = 0.5,
        linewidth = 0.4
      )
    }
  }

  p <- p +
    labs(
      x = "Year",
      y = "Age",
      title = title,
      subtitle = subtitle
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_srh(base_size = base_size) +
    theme(
      panel.grid = element_blank()
    )

  return(p)
}

# ------------------------------------------------------------------------------
# SINGLE SURVEY SUBPLOT (for combined figure)
# ------------------------------------------------------------------------------

#' Create a Lexis subplot for one survey (for combined figure)
#'
#' @description Internal function to create a single Lexis panel with
#'   configurable labels for use in combined multi-survey figures.
#'
#' @param data Lexis data for one survey (from prepare_lexis_data)
#' @param survey_name Survey name for title
#' @param show_title Show survey name as title?
#' @param show_legend Show color legend?
#' @param show_cohort_lines Show diagonal cohort lines?
#' @param color_scale Color scale: "turbo", "viridis", "plasma", "magma", "inferno", or "diverging"
#' @param reverse_colors Reverse color direction?
#' @param low_color Color for low values (only if color_scale = "diverging")
#' @param high_color Color for high values (only if color_scale = "diverging")
#' @param mid_color Color for midpoint (only if color_scale = "diverging")
#' @param cohort_line_spacing Years between cohort lines
#' @param cohort_line_width Line width for cohort lines (default 0.6)
#' @param cohort_line_color Color for cohort lines (default "gray30")
#' @param cohort_line_alpha Alpha for cohort lines (default 0.7)
#' @param tilt_x_labels Angle to tilt x-axis labels (default 0)
#' @param base_size Base font size
#'
#' @return A ggplot object
#' @keywords internal
create_lexis_subplot <- function(data,
                                  survey_name,
                                  show_title = TRUE,
                                  show_legend = TRUE,
                                  show_cohort_lines = TRUE,
                                  color_scale = "turbo",
                                  reverse_colors = TRUE,
                                  shared_scale = TRUE,
                                  scale_limits = c(0, 1),
                                  low_color = "#2166AC",
                                  high_color = "#B2182B",
                                  mid_color = "#F7F7F7",
                                  cohort_line_spacing = 10,
                                  cohort_line_width = 0.6,
                                  cohort_line_color = "gray30",
                                  cohort_line_alpha = 0.7,
                                  tilt_x_labels = 45,
                                  na_fill = "gray80",
                                  base_size = 12) {


  # Complete the grid so missing cells show as gray instead of white gaps
  # Infer year step from data (handles annual, biennial, etc.)
  all_years_data <- sort(unique(data$year))
  if (length(all_years_data) > 1) {
    year_step <- min(diff(all_years_data))
    all_years <- seq(min(all_years_data), max(all_years_data), by = year_step)
  } else {
    all_years <- all_years_data
  }
  all_ages <- sort(unique(data$age))
  complete_grid <- expand.grid(year = all_years, age = all_ages)
  data <- merge(complete_grid, data, by = c("year", "age"), all.x = TRUE)

  # Base plot
  p <- ggplot(data, aes(x = year, y = age, fill = mean_srh)) +
    geom_tile()

  # Determine scale limits
  if (shared_scale) {
    # Use fixed limits for shared scale
    limits <- scale_limits
    legend_title <- "Mean SRH"
  } else {
    # Use data-driven limits for independent scales
    limits <- NULL
    legend_title <- "Mean SRH"
  }

  # Color scale
  if (color_scale == "diverging") {
    midpoint <- if (shared_scale) mean(scale_limits) else median(data$mean_srh, na.rm = TRUE)
    p <- p + scale_fill_gradient2(
      low = if (reverse_colors) high_color else low_color,
      mid = mid_color,
      high = if (reverse_colors) low_color else high_color,
      midpoint = midpoint,
      limits = limits,
      na.value = na_fill,
      name = legend_title,
      guide = guide_colorbar(barwidth = 0.8, barheight = 5)
    )
  } else {
    viridis_option <- switch(
      color_scale,
      "viridis" = "D",
      "turbo" = "H",
      "plasma" = "C",
      "magma" = "A",
      "inferno" = "B",
      "H"  # default to turbo
    )
    p <- p + scale_fill_viridis_c(
      option = viridis_option,
      direction = if (reverse_colors) -1 else 1,
      limits = limits,
      na.value = na_fill,
      name = legend_title,
      guide = guide_colorbar(barwidth = 0.8, barheight = 5)
    )
  }

  # Cohort lines
  if (show_cohort_lines && nrow(data) > 0) {
    year_range <- range(data$year, na.rm = TRUE)
    age_range <- range(data$age, na.rm = TRUE)
    birth_min <- year_range[1] - age_range[2]
    birth_max <- year_range[2] - age_range[1]

    cohort_years <- seq(
      floor(birth_min / cohort_line_spacing) * cohort_line_spacing,
      ceiling(birth_max / cohort_line_spacing) * cohort_line_spacing,
      by = cohort_line_spacing
    )

    for (birth_year in cohort_years) {
      p <- p + geom_abline(
        intercept = -birth_year,
        slope = 1,
        linetype = "dashed",
        color = cohort_line_color,
        alpha = cohort_line_alpha,
        linewidth = cohort_line_width
      )
    }
  }

  # X-axis tick label styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Labels - no axis labels here (shared labels added by plot_lexis_combined)
  p <- p + labs(
    title = if (show_title) survey_name else NULL,
    x = NULL,
    y = NULL
  )

  # Scales and theme
  p <- p +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = x_tick_style,
      plot.margin = margin(2, 4, 2, 4),
      # Legend position: bottom for patchwork collection, or none if disabled
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = base_size - 1, face = "bold"),
      legend.text = element_text(size = base_size - 2)
    ) +
    guides(fill = guide_colorbar(
      barwidth = unit(6, "cm"),
      barheight = unit(0.4, "cm")
    ))

  return(p)
}


# ------------------------------------------------------------------------------
# COMBINED 2x3 FIGURE (like Figure 1)
# ------------------------------------------------------------------------------

#' Create combined Lexis figure for multiple surveys (2x3 grid)
#'
#' @description Creates a multi-panel figure where each survey gets its own
#'   Lexis diagram with independent color scale. This allows visual comparison
#'   of patterns while accommodating different SRH ranges across surveys.
#'
#' Layout follows Figure 1 style:
#' - Survey names as titles on each panel
#' - Shared "Year" x-axis label at bottom
#' - Shared "Age" y-axis label on left
#' - Individual color scales per panel
#'
#' @param lexis_list Named list of Lexis data frames (from prepare_lexis_data).
#'   Names become panel titles. Order determines panel arrangement.
#' @param ncol Number of columns in grid (default 3)
#' @param show_cohort_lines Show diagonal cohort lines? (default TRUE)
#' @param color_scale Color scale: "turbo" (default), "viridis", "plasma",
#'   "magma", "inferno", or "diverging"
#' @param reverse_colors Reverse color direction? (default TRUE, so higher
#'   SRH = warmer colors like red/yellow)
#' @param low_color Color for low values (only if color_scale = "diverging")
#' @param high_color Color for high values (only if color_scale = "diverging")
#' @param mid_color Color for midpoint (only if color_scale = "diverging")
#' @param cohort_line_spacing Years between cohort lines (default 10)
#' @param cohort_line_width Line width for cohort lines (default 0.6)
#' @param cohort_line_color Color for cohort lines (default "gray30")
#' @param cohort_line_alpha Alpha for cohort lines (default 0.7)
#' @param title Overall figure title (optional)
#' @param subtitle Overall figure subtitle (optional)
#' @param tilt_x_labels Angle to tilt x-axis labels (default 45)
#' @param base_size Base font size (default 12, matches Figure 1)
#'
#' @return A patchwork object
#'
#' @details
#' Each panel has its own color scale. This is intentional because:
#' 1. GSS uses a 4-point scale vs 5-point for others
#' 2. Survey populations differ, so absolute SRH levels aren't comparable
#' 3. The goal is to see PATTERNS (diagonal vs horizontal vs vertical), not levels
#'
#' The default turbo palette with reverse_colors=TRUE means:
#' - Higher SRH (better health) = red/yellow (warm)
#' - Lower SRH (worse health) = blue (cool)
#'
#' @examples
#' # Create combined figure
#' fig2 <- plot_lexis_combined(
#'   lexis_list,
#'   title = "Lexis Diagrams of Mean SRH"
#' )
#'
#' ggsave("fig2_lexis.png", fig2, width = 12, height = 8, dpi = 300)
#'
#' @export
plot_lexis_combined <- function(lexis_list,
                                 ncol = 6,
                                 show_cohort_lines = TRUE,
                                 color_scale = "turbo",
                                 reverse_colors = TRUE,
                                 shared_scale = TRUE,
                                 scale_limits = c(0, 1),
                                 low_color = "#2166AC",
                                 high_color = "#B2182B",
                                 mid_color = "#F7F7F7",
                                 cohort_line_spacing = 10,
                                 cohort_line_width = 1.0,
                                 cohort_line_color = "gray30",
                                 cohort_line_alpha = 0.7,
                                 title = NULL,
                                 subtitle = NULL,
                                 tilt_x_labels = 45,
                                 base_size = 12) {

  # --- Input validation ---
  stopifnot(is.list(lexis_list))
  if (is.null(names(lexis_list))) {
    stop("lexis_list must be a named list (names = survey names)")
  }

  survey_names <- names(lexis_list)
  n_surveys <- length(survey_names)
  nrow_grid <- ceiling(n_surveys / ncol)

  # --- Create individual plots ---
  # All plots keep their legend with position="bottom"
  # patchwork's guides="collect" will merge identical legends
  plots <- lapply(seq_along(survey_names), function(i) {
    svy <- survey_names[i]

    create_lexis_subplot(
      data = lexis_list[[svy]],
      survey_name = svy,
      show_title = TRUE,
      show_legend = TRUE,  # All plots keep legend - patchwork will collect
      show_cohort_lines = show_cohort_lines,
      color_scale = color_scale,
      reverse_colors = reverse_colors,
      shared_scale = shared_scale,
      scale_limits = scale_limits,
      low_color = low_color,
      high_color = high_color,
      mid_color = mid_color,
      cohort_line_spacing = cohort_line_spacing,
      cohort_line_width = cohort_line_width,
      cohort_line_color = cohort_line_color,
      cohort_line_alpha = cohort_line_alpha,
      tilt_x_labels = tilt_x_labels,
      base_size = base_size
    )
  })
  names(plots) <- survey_names

  # --- Assemble grid ---
  plot_grid <- wrap_plots(plots, ncol = ncol)

  # --- Create shared axis labels as grobs (like Figure 1) ---
  y_label_grob <- grid::textGrob(
    "Age",
    rot = 90,
    gp = grid::gpar(fontsize = base_size + 2, fontface = "plain")
  )

  x_label_grob <- grid::textGrob(
    "Year",
    gp = grid::gpar(fontsize = base_size + 2, fontface = "plain")
  )

  # --- Combine: y-label | plots / x-label ---
  # Following exact same pattern as plot_fig1_panel_a
  panels_with_ylabel <- wrap_elements(y_label_grob) | plot_grid
  panels_with_ylabel <- panels_with_ylabel + plot_layout(widths = c(0.03, 1))

  # Stack: panels_with_ylabel / x_label
  combined <- panels_with_ylabel / wrap_elements(x_label_grob)

  # Set heights and collect guides (same pattern as Figure 1)
  combined <- combined +
    plot_layout(heights = c(1, 0.06), guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.box.background = element_blank()
    )

  # --- Add overall title/subtitle ---
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
# FACETED VERSION (shared scale - for supplementary use)
# ------------------------------------------------------------------------------

#' Create faceted Lexis diagrams with shared color scale
#'
#' @description Alternative to plot_lexis_combined() that uses faceting.
#'   This enforces a shared color scale, which can be useful when surveys
#'   are on the same SRH scale and you want to compare absolute levels.
#'
#' @param data Combined data frame with: survey, year, age, mean_srh columns
#' @param ncol Number of facet columns (default 3)
#' @param show_cohort_lines Show diagonal cohort lines? (default TRUE)
#' @param title Plot title
#' @param subtitle Plot subtitle
#'
#' @return A ggplot object
#' @export
plot_lexis_faceted <- function(data,
                               ncol = 3,
                               show_cohort_lines = TRUE,
                               color_scale = "turbo",
                               reverse_colors = TRUE,
                               low_color = "#2166AC",
                               high_color = "#B2182B",
                               mid_color = "#F7F7F7",
                               title = NULL,
                               subtitle = NULL,
                               base_size = 11) {

  p <- ggplot(data, aes(x = year, y = age, fill = mean_srh)) +
    geom_tile() +
    facet_wrap(~survey, ncol = ncol, scales = "fixed")

  # Color scale
  if (color_scale == "diverging") {
    midpoint <- median(data$mean_srh, na.rm = TRUE)
    p <- p + scale_fill_gradient2(
      low = if (reverse_colors) high_color else low_color,
      mid = mid_color,
      high = if (reverse_colors) low_color else high_color,
      midpoint = midpoint,
      name = "Mean SRH"
    )
  } else {
    viridis_option <- switch(
      color_scale,
      "viridis" = "D",
      "turbo" = "H",
      "plasma" = "C",
      "magma" = "A",
      "inferno" = "B",
      "H"
    )
    p <- p + scale_fill_viridis_c(
      option = viridis_option,
      direction = if (reverse_colors) -1 else 1,
      name = "Mean SRH"
    )
  }

  # Add cohort lines to each facet
  if (show_cohort_lines) {
    # Calculate global cohort range
    year_range <- range(data$year, na.rm = TRUE)
    age_range <- range(data$age, na.rm = TRUE)
    birth_min <- year_range[1] - age_range[2]
    birth_max <- year_range[2] - age_range[1]

    cohort_years <- seq(
      floor(birth_min / 10) * 10,
      ceiling(birth_max / 10) * 10,
      by = 10
    )

    for (birth_year in cohort_years) {
      p <- p + geom_abline(
        intercept = -birth_year,
        slope = 1,
        linetype = "dashed",
        color = "gray30",
        alpha = 0.5,
        linewidth = 0.3
      )
    }
  }

  p <- p +
    labs(
      x = "Year",
      y = "Age",
      title = title,
      subtitle = subtitle
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_srh(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(size = rel(0.9), face = "bold")
    )

  return(p)
}

# ------------------------------------------------------------------------------
# INTERPRETATION GUIDE
# ------------------------------------------------------------------------------

#' Add interpretation annotation to Lexis plot
#' 
#' @description Adds a text box explaining how to read the Lexis diagram
#' 
#' @param plot A ggplot Lexis diagram
#' @param position Where to place annotation ("corner" or "caption")
#' 
#' @return Modified ggplot object
#' @export
add_lexis_guide <- function(plot, position = "caption") {
  
  guide_text <- paste(
    "Reading guide: Diagonal patterns = cohort effects;",
    "Vertical patterns = period effects;",
    "Horizontal patterns = age effects"
  )
  
  if (position == "caption") {
    plot <- plot + labs(caption = guide_text)
  }
  
  return(plot)
}

# ------------------------------------------------------------------------------
# HELPER: PREPARE DATA FOR LEXIS
# ------------------------------------------------------------------------------

#' Prepare survey data for Lexis diagram
#'
#' @description Calculates weighted mean SRH by year and age from survey data.
#'   Supports flexible binning for both age and period (year).
#'
#' @param data Survey data frame. Must contain: year, age, srh (numeric), wt (weight).
#'   Column names are flexible via parameters.
#' @param srh_var Name of the SRH variable (default "srh")
#' @param age_var Name of the age variable (default "age")
#' @param year_var Name of the year variable (default "year")
#' @param weight_var Name of the weight variable (default "wt")
#' @param age_binwidth Width of age bins in years (default 5). Use 1 for single-year ages.
#' @param year_binwidth Width of year bins (default 1 for annual). Use 3 or 5 for multi-year.
#' @param min_n Minimum unweighted observations per cell (default 30). Cells below
#'   this threshold are excluded to reduce noise.
#' @param weighted Use survey weights? (default TRUE)
#' @param rescale_01 Rescale SRH to 0-1 scale? (default FALSE, uses raw values)
#' @param srh_scale The original scale of SRH. Used for rescaling.
#'   - "5" for NHIS/MEPS/BRFSS/CPS/NHANES (1=Poor to 5=Excellent)
#'   - "4" for GSS (1=Poor to 4=Excellent)
#'   If rescale_01=TRUE and this is NULL, will auto-detect from data.
#'
#' @return Data frame with columns: year, age, mean_srh, n (cell count)
#'   Ready for plot_lexis_surface()
#'
#' @details
#' For smooth Lexis diagrams, 5-year age bins typically work well.
#' Finer bins (1-2 years) can be used with large samples (BRFSS).
#'
#' When rescale_01=TRUE, values are rescaled to 0-1 where 0=worst and 1=best.
#' This enables cross-survey comparison, though each panel should still
#' have its own color scale for visual clarity.
#'
#' @examples
#' # Basic usage with 5-year age bins
#' lexis_data <- prepare_lexis_data(nhis_data, age_binwidth = 5)
#'
#' # Finer 2-year bins for large dataset
#' lexis_data <- prepare_lexis_data(brfss_data, age_binwidth = 2, min_n = 100)
#'
#' # Multi-year period grouping (reduces noise)
#' lexis_data <- prepare_lexis_data(gss_data, age_binwidth = 5, year_binwidth = 3)
#'
#' # Rescale to 0-1 for cross-survey comparison
#' lexis_data <- prepare_lexis_data(nhis_data, rescale_01 = TRUE, srh_scale = "5")
#'
#' @export
prepare_lexis_data <- function(data,
                               srh_var = "srh",
                               age_var = "age",
                               year_var = "year",
                               weight_var = "wt",
                               age_binwidth = 5,
                               year_binwidth = 1,
                               min_age = NULL,
                               max_age = NULL,
                               min_n = 30,
                               weighted = TRUE,
                               rescale_01 = TRUE,
                               srh_scale = NULL) {

  # --- Input validation ---
  required_cols <- c(year_var, age_var, srh_var)
  if (weighted) required_cols <- c(required_cols, weight_var)

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }


  # --- Helper to safely convert to numeric (handles haven_labelled) ---
  safe_numeric <- function(x) {
    if (inherits(x, "haven_labelled")) {
      # Use unclass to get underlying numeric values, stripping labels
      as.numeric(unclass(x))
    } else if (is.factor(x)) {
      as.numeric(levels(x))[x]
    } else {
      as.numeric(x)
    }
  }

  # --- Rename to standard names for processing ---
  work_data <- data %>%
    mutate(
      srh_raw = safe_numeric(!!sym(srh_var)),
      age_raw = safe_numeric(!!sym(age_var)),
      year_raw = safe_numeric(!!sym(year_var))
    )

  if (weighted && weight_var %in% names(data)) {
    work_data <- work_data %>%
      mutate(wt = safe_numeric(!!sym(weight_var)))
  }

  # --- Filter by age range if specified ---
  if (!is.null(min_age)) {
    work_data <- work_data %>% filter(age_raw >= min_age)
  }
  if (!is.null(max_age)) {
    work_data <- work_data %>% filter(age_raw <= max_age)
  }

  # --- Auto-detect SRH scale if needed ---
  if (rescale_01 && is.null(srh_scale)) {
    srh_max <- max(work_data$srh_raw, na.rm = TRUE)
    srh_scale <- if (srh_max <= 4) "4" else "5"
    message("Auto-detected SRH scale: ", srh_scale, "-point")
  }

  # --- Rescale SRH if requested ---
  if (rescale_01) {
    # Convert to 0-1 scale where 0 = worst, 1 = best
    # Original: higher = better, so rescaling preserves direction
    scale_max <- as.numeric(srh_scale)
    work_data <- work_data %>%
      mutate(srh_value = (srh_raw - 1) / (scale_max - 1))
  } else {
    work_data <- work_data %>%
      mutate(srh_value = srh_raw)
  }

  # --- Apply age binning ---
  if (age_binwidth > 1) {
    # Bin to midpoint of interval (e.g., 20-24 -> 22)
    work_data <- work_data %>%
      mutate(age = floor(age_raw / age_binwidth) * age_binwidth + age_binwidth / 2)
  } else {
    work_data <- work_data %>%
      mutate(age = age_raw)
  }

  # --- Apply year binning ---
  if (year_binwidth > 1) {
    # Bin to midpoint of interval
    work_data <- work_data %>%
      mutate(year = floor(year_raw / year_binwidth) * year_binwidth + year_binwidth / 2)
  } else {
    work_data <- work_data %>%
      mutate(year = year_raw)
  }

  # --- Calculate weighted means ---
  if (weighted && "wt" %in% names(work_data)) {
    lexis_data <- work_data %>%
      filter(!is.na(srh_value), !is.na(age), !is.na(year), !is.na(wt)) %>%
      group_by(year, age) %>%
      summarize(
        mean_srh = weighted.mean(srh_value, wt, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
  } else {
    lexis_data <- work_data %>%
      filter(!is.na(srh_value), !is.na(age), !is.na(year)) %>%
      group_by(year, age) %>%
      summarize(
        mean_srh = mean(srh_value, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
  }

  # --- Filter by minimum cell size ---
  lexis_data <- lexis_data %>%
    filter(n >= min_n)

  # --- Check for empty result ---
  if (nrow(lexis_data) == 0) {
    warning("No cells met minimum n threshold (", min_n, "). ",
            "Consider lowering min_n or using wider bins.")
  }

  return(lexis_data)
}

# ------------------------------------------------------------------------------
# EXAMPLE USAGE
# ------------------------------------------------------------------------------
#
# # ============================================================================
# # Single survey Lexis diagram
# # ============================================================================
#
# # Load and prepare data
# source(here::here("R/paths.R"))
# data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))
#
# # Prepare Lexis data (5-year age bins, annual periods)
# lexis_nhis <- prepare_lexis_data(
#   data_nhis,
#   srh_var = "srh",
#   age_binwidth = 5,
#   year_binwidth = 1,
#   min_n = 50
# )
#
# # Plot single survey with cohort lines (default ON)
# p <- plot_lexis_surface(
#   lexis_nhis,
#   title = "NHIS: Self-Rated Health by Age and Year",
#   subtitle = "Blue = better health, Red = worse health",
#   show_cohort_lines = TRUE,    # default
#   cohort_line_spacing = 10     # default
# )
#
# # Add interpretation guide
# p <- add_lexis_guide(p)
#
# ggsave("lexis_nhis.png", p, width = 8, height = 6)
#
#
# # ============================================================================
# # Combined 2x3 figure for all surveys (MAIN USE CASE)
# # ============================================================================
#
# # Prepare data for each survey (5-year age bins work well)
# lexis_list <- list(
#   "BRFSS"  = prepare_lexis_data(data_brfss, age_binwidth = 5),
#   "MEPS"   = prepare_lexis_data(data_meps, age_binwidth = 5),
#   "NHIS"   = prepare_lexis_data(data_nhis, age_binwidth = 5),
#   "GSS"    = prepare_lexis_data(data_gss, age_binwidth = 5),  # auto-detects 4-point scale
#   "CPS"    = prepare_lexis_data(data_cps, age_binwidth = 5),
#   "NHANES" = prepare_lexis_data(data_nhanes, age_binwidth = 5)
# )
#
# # Create combined figure (each panel has independent color scale)
# fig2 <- plot_lexis_combined(
#   lexis_list,
#   ncol = 3,
#   show_cohort_lines = TRUE,
#   title = "Figure 2: Lexis Diagrams by Survey",
#   subtitle = "Diagonal lines show birth cohorts. Absence of diagonal banding suggests weak cohort effects."
# )
#
# ggsave("fig2_lexis_combined.png", fig2, width = 12, height = 8, dpi = 300)
#
#
# # ============================================================================
# # Options: Rescaled 0-1 for cross-survey comparison
# # ============================================================================
#
# # If you want to put all surveys on the same 0-1 scale
# lexis_list_scaled <- list(
#   "NHIS"  = prepare_lexis_data(data_nhis, rescale_01 = TRUE, srh_scale = "5"),
#   "GSS"   = prepare_lexis_data(data_gss, rescale_01 = TRUE, srh_scale = "4")
# )
#
#
# # ============================================================================
# # Options: Multi-year period binning (reduces noise for small samples)
# # ============================================================================
#
# # For GSS (smaller sample), use 3-year periods
# lexis_gss <- prepare_lexis_data(
#   data_gss,
#   age_binwidth = 5,
#   year_binwidth = 3,  # 3-year periods
#   min_n = 20          # Lower threshold for small samples
# )
#
#
# # ============================================================================
# # Options: Turn off cohort lines
# # ============================================================================
#
# fig2_no_lines <- plot_lexis_combined(
#   lexis_list,
#   show_cohort_lines = FALSE
# )
