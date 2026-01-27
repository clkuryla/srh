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


# =============================================================================
# Color Palettes for SRH Convergence Analysis
# theme_srh.R
# =============================================================================

# -----------------------------------------------------------------------------
# AGE GROUP COLORS (7 categories: 18-29 through 80-89)
# Warm→cool gradient maps young→old
# -----------------------------------------------------------------------------

# CB-friendly palette
age_colors_cb <- c(
  "18-29" = "#DA53BE",  # pink
  "30-39" = "#F38697",  # orange-pink
  "40-49" = "#EAD56A",  # yellow
  "50-59" = "#A2D05C",  # green
  "60-69" = "#6FCCBC",  # teal
  "70-79" = "#5083F5",  # blue
  "80-89" = "#B1A3F3"   # purple
)

# Okabe-Ito palette (preferred for colorblind accessibility)
age_colors_oi <- c(
  "18-29" = "#D55E00",  # vermillion (orange-red)
  "30-39" = "#E69F00",  # orange
  "40-49" = "#F0E442",  # yellow
  "50-59" = "#009E73",  # bluish green
  "60-69" = "#56B4E9",  # sky blue
  "70-79" = "#0072B2",  # blue
  "80-89" = "#CC79A7"   # reddish purple
)

# Default age palette (Okabe-Ito for colorblind accessibility)
age_colors <- age_colors_oi

# -----------------------------------------------------------------------------
# SELF-RATED HEALTH COLORS (5 categories: Poor=1 to Excellent=5, except GSS remove Very Good)
# Cool→warm gradient maps poor→excellent health
# -----------------------------------------------------------------------------

# Dark palette
srh_colors_dark <- c(
  "Poor"      = "#7498F5",  # blue
  "Fair"      = "#3BC78B",  # green
  "Good"      = "#FFB000",  # yellow
  "Very Good" = "#F57323",  # orange
  "Excellent" = "#E46AA6"   # pink
)

srh_colors_dark_gss <- c(
  "Poor"      = "#7498F5",  # blue
  "Fair"      = "#3BC78B",  # green
  "Good"      = "#FFB000",  # yellow
  "Excellent" = "#E46AA6"   # pink
)

# Pastel palette
srh_colors_pastel <- c(
  "Poor"      = "#9AAEE0",  # light blue
  "Fair"      = "#8FE2BE",  # light green
  "Good"      = "#ECCB80",  # light yellow
  "Very Good" = "#F7A776",  # light orange
  "Excellent" = "#F18CBD"   # light pink
)

#' SRH category color palette (4-point scale, for GSS)
#' @description Colors for GSS 4-point SRH scale (no "Very Good" option).
#' @export
srh_colors_pastel_gss <- c(
  "Poor"      = "#9AAEE0",  # light blue
  "Fair"      = "#8FE2BE",  # light green
  "Good"      = "#ECCB80",  # light yellow
  "Excellent" = "#F18CBD"   # light pink
)

# Numeric versions (for SRH coded 1-5)
srh_colors_dark_num <- setNames(srh_colors_dark, 1:5)
srh_colors_pastel_num <- setNames(srh_colors_pastel, 1:5)

# Default SRH palette
srh_colors <- srh_colors_pastel



#' Alternative age group palette for scheme "C" (70+ top group)
#' @description For analyses that combine 70+ into single group
#' @export
age_colors_scheme_c <- c(
  "18-29" = "#D55E00",
  "30-39" = "#E69F00",
  "40-49" = "#F0E442",
  "50-59" = "#009E73",
  "60-69" = "#56B4E9",
  "70+"   = "#0072B2"
)

#' Broader age group palette (if using fewer categories)
age_colors_broad <- c(
  "18-44" = "#E64B35",
  "45-64" = "#8491B4",
  "65+"   = "#00A087"
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

# -----------------------------------------------------------------------------
# ggplot2 SCALE FUNCTIONS
# -----------------------------------------------------------------------------

scale_color_age <- function(palette = c("oi", "cb"), ...) {
  palette <- match.arg(palette)
  colors <- if (palette == "oi") age_colors_oi else age_colors_cb
  ggplot2::scale_color_manual(values = colors, ...)
}

scale_fill_age <- function(palette = c("oi", "cb"), ...) {
  palette <- match.arg(palette)
  colors <- if (palette == "oi") age_colors_oi else age_colors_cb
  ggplot2::scale_fill_manual(values = colors, ...)
}

scale_color_srh <- function(palette = c("dark", "pastel"), numeric = FALSE, ...) {
  palette <- match.arg(palette)
  colors <- if (palette == "dark") {
    if (numeric) srh_colors_dark_num else srh_colors_dark
  } else {
    if (numeric) srh_colors_pastel_num else srh_colors_pastel
  }
  ggplot2::scale_color_manual(values = colors, ...)
}

scale_fill_srh <- function(palette = c("dark", "pastel"), numeric = FALSE, ...) {
  palette <- match.arg(palette)
  colors <- if (palette == "dark") {
    if (numeric) srh_colors_dark_num else srh_colors_dark
  } else {
    if (numeric) srh_colors_pastel_num else srh_colors_pastel
  }
  ggplot2::scale_fill_manual(values = colors, ...)
}

#' SRH category labels (5-point scale)
#' @description Standard labels for SRH categories, ordered Poor to Excellent.
#' @export
srh_cat_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")

#' SRH category labels (4-point scale, for GSS)
#' @export
srh_cat_labels_gss <- c("Poor", "Fair", "Good", "Excellent")

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

#' SRH category color scale (5-point)
#' @export
scale_color_srh_cat <- function(...) {
  scale_color_manual(values = srh_cat_colors, ...)
}

#' SRH category fill scale (5-point)
#' @export
scale_fill_srh_cat <- function(...) {
  scale_fill_manual(values = srh_cat_colors, ...)
}

#' SRH category color scale (4-point, for GSS)
#' @export
scale_color_srh_cat_gss <- function(...) {
  scale_color_manual(values = srh_cat_colors_gss, ...)
}

#' SRH category fill scale (4-point, for GSS)
#' @export
scale_fill_srh_cat_gss <- function(...) {
  scale_fill_manual(values = srh_cat_colors_gss, ...)
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
