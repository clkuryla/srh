# ==============================================================================
# theme_srh.R
# Shared ggplot theme and color palettes for SRH convergence paper
# All figures should use these for visual consistency
# ==============================================================================

library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# COLOR PALETTES
# ------------------------------------------------------------------------------

#' Age group color palette (colorblind-friendly)
#' @description Consistent colors for age groups across all figures.
#'   Uses colorblind-friendly palette matching Figure 1.
#' @export
age_colors <- c(
  "18-29" = "#D55E00",
  "30-39" = "#E69F00",
  "40-49" = "#F0E442",
  "50-59" = "#009E73",
  "60-69" = "#56B4E9",
  "70-79" = "#0072B2",
  "80-89" = "#CC79A7",
  "80+"   = "#CC79A7"   # Same as 80-89 for scheme A compatibility
)

#' Broader age group palette (if using fewer categories)
age_colors_broad <- c(
  "18-44" = "#E64B35",
  "45-64" = "#8491B4",
  "65+"   = "#00A087"
)

#' Survey color palette
#' @description Consistent colors for surveys in multi-survey plots
#' @export
survey_colors <- c(
  "NHIS"   = "#1B9E77",
  "MEPS"   = "#D95F02",
  "BRFSS"  = "#7570B3",
  "GSS"    = "#E7298A",
  "NHANES" = "#66A61E",
  "CPS"    = "#E6AB02"
)

#' Survey shape palette (for B&W compatibility)
#' @export
survey_shapes <- c(
  "NHIS"   = 16,  # filled circle
  "MEPS"   = 17,  # filled triangle
  "BRFSS"  = 15,  # filled square
  "GSS"    = 18,  # filled diamond
  "NHANES" = 8,   # asterisk
  "CPS"    = 3    # plus
)

# ------------------------------------------------------------------------------
# GGPLOT THEME
# ------------------------------------------------------------------------------

#' SRH Paper Theme
#' @description Consistent theme for all figures in the paper.
#'   Clean, publication-ready, works well in both color and grayscale.
#' @param base_size Base font size (default 11)
#' @param base_family Base font family (default "sans")
#' @export
theme_srh <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Panel
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Axes
      axis.title = element_text(size = rel(1), face = "plain", color = "gray20"),
      axis.text = element_text(size = rel(0.9), color = "gray30"),
      axis.ticks = element_line(color = "gray70", linewidth = 0.3),
      axis.line = element_blank(),
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.85)),
      legend.key.size = unit(0.8, "lines"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      
      # Facets
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      strip.text = element_text(size = rel(0.95), face = "bold", margin = margin(4, 4, 4, 4)),
      
      # Plot title and caption
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0, 
                                margin = margin(b = 8)),
      plot.subtitle = element_text(size = rel(1), hjust = 0, color = "gray40",
                                   margin = margin(b = 10)),
      plot.caption = element_text(size = rel(0.8), hjust = 1, color = "gray50",
                                  margin = margin(t = 10)),
      plot.margin = margin(10, 10, 10, 10),
      
      # Complete
      complete = TRUE
    )
}

# ------------------------------------------------------------------------------
# SCALE FUNCTIONS
# ------------------------------------------------------------------------------

#' Age group color scale
#' @description Use this for consistent age group coloring
#' @param ... Additional arguments passed to scale_color_manual
#' @export
scale_color_age <- function(...) {
  scale_color_manual(values = age_colors, ...)
}

#' Age group fill scale
#' @export
scale_fill_age <- function(...) {
  scale_fill_manual(values = age_colors, ...)
}

#' Survey color scale
#' @export
scale_color_survey <- function(...) {
  scale_color_manual(values = survey_colors, ...)
}

#' Survey fill scale
#' @export
scale_fill_survey <- function(...) {
  scale_fill_manual(values = survey_colors, ...)
}

#' Survey shape scale
#' @export
scale_shape_survey <- function(...) {
  scale_shape_manual(values = survey_shapes, ...)
}

# ------------------------------------------------------------------------------
# SRH AXIS HELPERS
# ------------------------------------------------------------------------------
#' Standard SRH y-axis scale
#' @description Consistent y-axis for mean SRH plots (1-5 scale)
#' @param limits Y-axis limits (default c(1.5, 3.5) for typical range)
#' @export
scale_y_srh <- function(limits = c(1.5, 3.5)) {
  scale_y_continuous(
    limits = limits,
    breaks = seq(1, 5, by = 0.5),
    labels = function(x) sprintf("%.1f", x)
  )
}

#' Year axis scale
#' @description Consistent x-axis for year
#' @param breaks Year breaks (default every 5 years)
#' @export
scale_x_year <- function(breaks = waiver()) {
  scale_x_continuous(
    breaks = breaks,
    labels = function(x) as.character(as.integer(x))
  )
}

# ------------------------------------------------------------------------------
# LABEL HELPERS
# ------------------------------------------------------------------------------

#' Standard SRH y-axis label
#' @export
lab_srh_mean <- "Mean Self-Rated Health\n(1 = Excellent, 5 = Poor)"

#' Standard coefficient y-axis label
#' @export
lab_coefficient <- "Coefficient"

#' Standard prevalence y-axis label
#' @export
lab_prevalence <- "Prevalence (%)"

# ------------------------------------------------------------------------------
# EXAMPLE USAGE
# ------------------------------------------------------------------------------

# ggplot(data, aes(x = year, y = mean_srh, color = age_group)) +
#   geom_line() +
#   geom_point() +
#   scale_color_age() +
#   scale_y_srh() +
#   scale_x_year() +
#   labs(y = lab_srh_mean, x = "Year", color = "Age Group") +
#   theme_srh()
