# ==============================================================================
# plot_lexis.R
# Lexis diagram functions for APC visualization (Figure 2)
# ==============================================================================

library(ggplot2)
library(dplyr)

# Source the theme
# source(here::here("R", "functions", "theme_srh.R"))

# ------------------------------------------------------------------------------
# LEXIS SURFACE PLOT
# ------------------------------------------------------------------------------

#' Create Lexis surface diagram
#' 
#' @description Creates a heat map with year on x-axis, age on y-axis, and
#'   color representing the outcome (e.g., mean SRH). Diagonal patterns
#'   indicate cohort effects; vertical patterns indicate period effects;
#'   horizontal patterns indicate age effects.
#'   
#' @param data Data frame with columns: year, age (or age_group), value
#' @param value_var Name of the value variable (default "mean_srh")
#' @param age_var Name of the age variable (default "age")
#' @param low_color Color for low values (default "#2166AC" blue = better health)
#' @param high_color Color for high values (default "#B2182B" red = worse health)
#' @param mid_color Color for midpoint (default "#F7F7F7" white)
#' @param midpoint Midpoint for color scale (default NULL, uses median)
#' @param title Plot title
#' @param show_cohort_lines Whether to show diagonal cohort reference lines
#' @param cohort_years Birth years to show as reference lines
#' 
#' @return A ggplot object
#' @export
plot_lexis_surface <- function(data,
                               value_var = "mean_srh",
                               age_var = "age",
                               low_color = "#2166AC",
                               high_color = "#B2182B",
                               mid_color = "#F7F7F7",
                               midpoint = NULL,
                               title = NULL,
                               subtitle = NULL,
                               legend_title = "Mean SRH",
                               show_cohort_lines = FALSE,
                               cohort_years = NULL) {
  
  # Set midpoint if not specified
  if (is.null(midpoint)) {
    midpoint <- median(data[[value_var]], na.rm = TRUE)
  }
  
  # Base plot
  p <- ggplot(data, aes(x = year, y = .data[[age_var]], fill = .data[[value_var]])) +
    geom_tile() +
    scale_fill_gradient2(
      low = low_color,
      mid = mid_color,
      high = high_color,
      midpoint = midpoint,
      name = legend_title
    )
  
  # Add cohort lines if requested
  if (show_cohort_lines && !is.null(cohort_years)) {
    for (birth_year in cohort_years) {
      p <- p + geom_abline(
        intercept = -birth_year,
        slope = 1,
        linetype = "dashed",
        color = "gray40",
        alpha = 0.5
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
    theme_srh() +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# LEXIS WITH CONTOURS
# ------------------------------------------------------------------------------

#' Create Lexis diagram with contour lines
#' 
#' @description Alternative to tile plot using contours. Can be cleaner
#'   for showing patterns.
#'   
#' @param data Data frame with: year, age, value
#' @param value_var Name of value variable
#' @param n_bins Number of contour levels
#' 
#' @return A ggplot object
#' @export
plot_lexis_contour <- function(data,
                               value_var = "mean_srh",
                               age_var = "age",
                               n_bins = 10,
                               title = NULL,
                               subtitle = NULL) {
  
  p <- ggplot(data, aes(x = year, y = .data[[age_var]], z = .data[[value_var]])) +
    geom_contour_filled(bins = n_bins) +
    scale_fill_viridis_d(option = "plasma", name = "Mean SRH") +
    labs(
      x = "Year",
      y = "Age",
      title = title,
      subtitle = subtitle
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_srh() +
    theme(
      panel.grid = element_blank()
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# MULTI-SURVEY LEXIS GRID
# ------------------------------------------------------------------------------

#' Create Lexis diagrams for multiple surveys
#' 
#' @description Faceted Lexis diagrams for comparing patterns across surveys
#' 
#' @param data Data frame with: survey, year, age, value
#' @param ncol Number of facet columns
#' 
#' @return A ggplot object
#' @export
plot_lexis_by_survey <- function(data,
                                 value_var = "mean_srh",
                                 age_var = "age",
                                 ncol = 3,
                                 low_color = "#2166AC",
                                 high_color = "#B2182B",
                                 mid_color = "#F7F7F7",
                                 midpoint = NULL,
                                 title = NULL,
                                 subtitle = NULL) {
  
  if (is.null(midpoint)) {
    midpoint <- median(data[[value_var]], na.rm = TRUE)
  }
  
  p <- ggplot(data, aes(x = year, y = .data[[age_var]], fill = .data[[value_var]])) +
    geom_tile() +
    facet_wrap(~survey, ncol = ncol) +
    scale_fill_gradient2(
      low = low_color,
      mid = mid_color,
      high = high_color,
      midpoint = midpoint,
      name = "Mean SRH"
    ) +
    labs(
      x = "Year",
      y = "Age",
      title = title,
      subtitle = subtitle
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_srh() +
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
#' @description Calculates mean SRH by year and single-year age
#'   (or age bins) from survey data
#'   
#' @param data Survey data with year, age, srh_numeric, weight
#' @param age_binwidth Width of age bins (default 1 for single year)
#' @param min_n Minimum observations per cell (default 30)
#' @param design Survey design object (if using survey-weighted means)
#' 
#' @return Data frame ready for plot_lexis_surface()
#' @export
prepare_lexis_data <- function(data,
                               age_binwidth = 1,
                               min_n = 30,
                               weighted = TRUE) {
  
  if (age_binwidth > 1) {
    data <- data %>%
      mutate(age_bin = floor(age / age_binwidth) * age_binwidth)
    age_var <- "age_bin"
  } else {
    age_var <- "age"
  }
  
  if (weighted && "weight" %in% names(data)) {
    lexis_data <- data %>%
      group_by(year, .data[[age_var]]) %>%
      summarize(
        mean_srh = weighted.mean(srh_numeric, weight, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= min_n)
  } else {
    lexis_data <- data %>%
      group_by(year, .data[[age_var]]) %>%
      summarize(
        mean_srh = mean(srh_numeric, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= min_n)
  }
  
  if (age_binwidth > 1) {
    lexis_data <- lexis_data %>%
      rename(age = age_bin)
  }
  
  return(lexis_data)
}

# ------------------------------------------------------------------------------
# EXAMPLE USAGE
# ------------------------------------------------------------------------------

# # Prepare data
# lexis_nhis <- prepare_lexis_data(nhis_data, age_binwidth = 1, min_n = 50)
# 
# # Single survey
# p <- plot_lexis_surface(
#   lexis_nhis,
#   title = "NHIS: Self-Rated Health by Age and Year",
#   subtitle = "Blue = better health, Red = worse health"
# ) %>%
#   add_lexis_guide()
# 
# # Multiple surveys
# lexis_all <- bind_rows(
#   prepare_lexis_data(nhis_data) %>% mutate(survey = "NHIS"),
#   prepare_lexis_data(brfss_data) %>% mutate(survey = "BRFSS"),
#   ...
# )
# 
# p_multi <- plot_lexis_by_survey(lexis_all, ncol = 3)
