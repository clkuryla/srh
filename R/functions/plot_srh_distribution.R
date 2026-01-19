# ==============================================================================
# plot_srh_distribution.R
# Plotting functions for SRH categorical distribution figures
# Supplemental analyses exploring the ordinal nature of SRH
# ==============================================================================

library(tidyverse)

# Source the theme if not already loaded
# source(here::here("R/functions/theme_srh.R"))

# ------------------------------------------------------------------------------
# Approach 1: Facet by Age Group
# Line plot showing proportion of each SRH category over time per age group
# ------------------------------------------------------------------------------

#' Plot SRH category distribution by age group over time
#'
#' @param data Tibble from summarize_srh_proportions() with columns:
#'   age_group, year, srh_cat, prop, se
#' @param survey_name String: name of the survey for title
#' @param srh_scale 5 (default) or 4 (for GSS)
#' @param base_size Base font size for theme
#' @param line_width Line width for geom_line
#' @param point_size Point size for geom_point
#' @param show_points Whether to show points (default TRUE)
#' @param ncol Number of facet columns (default 4)
#'
#' @return A ggplot object
plot_srh_by_age_group <- function(
  data,
  survey_name,
  srh_scale = 5,
  base_size = 11,
  line_width = 0.8,
  point_size = 1.0,
  show_points = TRUE,
  ncol = 4
) {

  stopifnot(all(c("age_group", "year", "srh_cat", "prop") %in% names(data)))

  # Use appropriate color scale
  if (srh_scale == 5) {
    color_scale <- scale_color_srh_cat(name = "SRH Category")
  } else {
    color_scale <- scale_color_srh_cat_gss(name = "SRH Category")
  }

  p <- ggplot(data, aes(x = year, y = prop * 100, color = srh_cat, group = srh_cat)) +
    geom_line(linewidth = line_width) +
    facet_wrap(~ age_group, ncol = ncol) +
    color_scale +
    scale_y_continuous(
      limits = c(0, NA),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste0(survey_name, ": SRH Category Distribution by Age Group"),
      subtitle = "Proportion of each SRH category over time",
      x = "Year",
      y = "Proportion (%)",
      color = "SRH Category"
    ) +
    theme_srh(base_size = base_size)

  if (show_points) {
    p <- p + geom_point(size = point_size)
  }

  p
}


# ------------------------------------------------------------------------------
# Approach 2a: Facet by SRH Category (Age Composition)
# Shows which age groups contribute to each SRH rating level
# Age groups sum to 100% within each SRH category x year
# ------------------------------------------------------------------------------

#' Plot age composition within each SRH category over time
#'
#' @param data Tibble from summarize_age_composition_by_srh() with columns:
#'   srh_cat, year, age_group, prop, se
#' @param survey_name String: name of the survey for title
#' @param base_size Base font size for theme
#' @param line_width Line width for geom_line
#' @param point_size Point size for geom_point
#' @param show_points Whether to show points (default TRUE)
#' @param ncol Number of facet columns (default for 5 categories)
#'
#' @return A ggplot object
plot_srh_age_composition <- function(
  data,
  survey_name,
  base_size = 11,
  line_width = 0.8,
  point_size = 1.0,
  show_points = TRUE,
  ncol = 5
) {

  stopifnot(all(c("srh_cat", "year", "age_group", "prop") %in% names(data)))

  p <- ggplot(data, aes(x = year, y = prop * 100, color = age_group, group = age_group)) +
    geom_line(linewidth = line_width) +
    facet_wrap(~ srh_cat, ncol = ncol) +
    scale_color_age(name = "Age Group") +
    scale_y_continuous(
      limits = c(0, NA),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste0(survey_name, ": Age Composition by SRH Category"),
      subtitle = "Which age groups report each SRH level (age groups sum to 100% within each category)",
      x = "Year",
      y = "Proportion of Category (%)",
      color = "Age Group"
    ) +
    theme_srh(base_size = base_size)

  if (show_points) {
    p <- p + geom_point(size = point_size)
  }

  p
}


# ------------------------------------------------------------------------------
# Approach 2b: Facet by SRH Category (Age-Specific Prevalence)
# Shows how each age group's prevalence of a specific rating changes over time
# Same data as Approach 1, different faceting (SRH categories sum to 100% within age group x year)
# ------------------------------------------------------------------------------

#' Plot SRH category prevalence by age group, faceted by SRH category
#'
#' @param data Tibble from summarize_srh_proportions() with columns:
#'   age_group, year, srh_cat, prop, se
#' @param survey_name String: name of the survey for title
#' @param base_size Base font size for theme
#' @param line_width Line width for geom_line
#' @param point_size Point size for geom_point
#' @param show_points Whether to show points (default TRUE)
#' @param ncol Number of facet columns
#'
#' @return A ggplot object
plot_srh_category_by_age <- function(
  data,
  survey_name,
  base_size = 11,
  line_width = 0.8,
  point_size = 1.0,
  show_points = TRUE,
  ncol = 5
) {

  stopifnot(all(c("age_group", "year", "srh_cat", "prop") %in% names(data)))

  p <- ggplot(data, aes(x = year, y = prop * 100, color = age_group, group = age_group)) +
    geom_line(linewidth = line_width) +
    facet_wrap(~ srh_cat, ncol = ncol) +
    scale_color_age(name = "Age Group") +
    scale_y_continuous(
      limits = c(0, NA),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste0(survey_name, ": SRH Category Prevalence by Age Group"),
      subtitle = "How prevalence of each SRH category varies by age group over time",
      x = "Year",
      y = "Prevalence (%)",
      color = "Age Group"
    ) +
    theme_srh(base_size = base_size)

  if (show_points) {
    p <- p + geom_point(size = point_size)
  }

  p
}


# ------------------------------------------------------------------------------
# Approach 3: Stacked Area Chart
# Shows compositional shifts in SRH distribution within each age group
# ------------------------------------------------------------------------------

#' Plot stacked area chart of SRH distribution by age group
#'
#' @param data Tibble from summarize_srh_proportions() with columns:
#'   age_group, year, srh_cat, prop, se
#' @param survey_name String: name of the survey for title
#' @param srh_scale 5 (default) or 4 (for GSS)
#' @param base_size Base font size for theme
#' @param alpha Fill transparency (default 0.8)
#' @param ncol Number of facet columns (default 4)
#'
#' @return A ggplot object
plot_srh_stacked_area <- function(
  data,
  survey_name,
  srh_scale = 5,
  base_size = 11,
  alpha = 0.8,
  ncol = 4
) {

  stopifnot(all(c("age_group", "year", "srh_cat", "prop") %in% names(data)))

  # Use appropriate fill scale
  if (srh_scale == 5) {
    fill_scale <- scale_fill_srh_cat(name = "SRH Category")
  } else {
    fill_scale <- scale_fill_srh_cat_gss(name = "SRH Category")
  }

  # Reverse the factor order so "Excellent" is on top (stacked from bottom)
  data <- data %>%
    mutate(srh_cat = fct_rev(srh_cat))

  p <- ggplot(data, aes(x = year, y = prop * 100, fill = srh_cat)) +
    geom_area(alpha = alpha, position = "stack") +
    facet_wrap(~ age_group, ncol = ncol) +
    fill_scale +
    scale_y_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = paste0(survey_name, ": SRH Distribution Composition by Age Group"),
      subtitle = "Stacked area showing shifts in SRH category proportions over time",
      x = "Year",
      y = "Proportion (%)",
      fill = "SRH Category"
    ) +
    theme_srh(base_size = base_size) +
    guides(fill = guide_legend(reverse = TRUE))  # Reverse legend to match stack order

  p
}


# ------------------------------------------------------------------------------
# Approach 6: Variance/Entropy over Time
# Shows how the spread of SRH distribution changes by age group
# ------------------------------------------------------------------------------

#' Plot SRH distribution variance over time by age group
#'
#' @param data Tibble from summarize_srh_spread() with columns:
#'   age_group, year, variance, entropy, n_weighted
#' @param survey_name String: name of the survey for title
#' @param metric Which metric to plot: "variance" (default) or "entropy"
#' @param base_size Base font size for theme
#' @param line_width Line width for geom_line
#' @param point_size Point size for geom_point
#' @param show_points Whether to show points (default TRUE)
#'
#' @return A ggplot object
plot_srh_spread <- function(
  data,
  survey_name,
  metric = c("variance", "entropy"),
  base_size = 11,
  line_width = 0.8,
  point_size = 1.2,
  show_points = TRUE
) {

  metric <- match.arg(metric)

  stopifnot(all(c("age_group", "year", metric) %in% names(data)))

  y_var <- rlang::sym(metric)

  if (metric == "variance") {
    y_label <- "Variance of SRH"
    subtitle <- "Higher values indicate more spread in responses"
  } else {
    y_label <- "Shannon Entropy"
    subtitle <- "Higher values indicate more uniform distribution across categories"
  }

  p <- ggplot(data, aes(x = year, y = !!y_var, color = age_group, group = age_group)) +
    geom_line(linewidth = line_width) +
    scale_color_age(name = "Age Group") +
    labs(
      title = paste0(survey_name, ": SRH Distribution ", str_to_title(metric), " Over Time"),
      subtitle = subtitle,
      x = "Year",
      y = y_label,
      color = "Age Group"
    ) +
    theme_srh(base_size = base_size)

  if (show_points) {
    p <- p + geom_point(size = point_size)
  }

  p
}


# ------------------------------------------------------------------------------
# Combined panel figure helper
# ------------------------------------------------------------------------------

#' Combine multiple SRH distribution plots into a panel figure
#'
#' @param plots Named list of ggplot objects
#' @param ncol Number of columns for layout
#' @param tag_levels Tag style (default "A")
#'
#' @return A patchwork object
combine_srh_panels <- function(plots, ncol = 2, tag_levels = "A") {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required. Please install it.")
  }

  patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_annotation(tag_levels = tag_levels) &
    theme(plot.tag = element_text(face = "bold", size = 14))
}
