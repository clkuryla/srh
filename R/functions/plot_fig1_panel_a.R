# ==============================================================================
# plot_fig1_panel_a.R
# Figure 1 Panel A: Weighted Mean SRH by Age Group Over Time
#
# Purpose: Create publication-quality 2x3 multi-panel figure showing the
#          convergence phenomenon across all 6 surveys (NHIS, MEPS, BRFSS,
#          GSS, CPS, NHANES).
#
# Key features:
#   - Each survey gets its own panel with FREE y-axis (different SRH scales)
#   - Each survey gets its own panel with FREE x-axis (different year ranges)
#   - Shared legend at bottom (age groups)
#   - Consistent Okabe-Ito colorblind-friendly colors
#   - Clean, minimal theme for publication
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)      # For textGrob axis labels

# Source theme (for colors) - caller should source this, but we define fallback
if (!exists("age_colors")) {
source(here::here("R", "functions", "theme_srh.R"))
}

# ------------------------------------------------------------------------------
# SINGLE SURVEY PANEL
# ------------------------------------------------------------------------------

#' Create a single panel for one survey's mean SRH over time
#'
#' @description
#' Generates one ggplot showing weighted mean SRH by age group over time
#' for a single survey. Used as building block for the combined figure.
#'
#' @param estimates Data frame with columns: age_group, year, mean_srh, se,
#'   ci_lower, ci_upper. Output from summarize_srh_over_time().
#' @param survey_name Character string for panel title (e.g., "NHIS")
#' @param colors Named vector of colors for age groups. Defaults to age_colors.
#' @param show_ci Logical. Show confidence interval ribbons? Default FALSE
#'   for cleaner publication figure.
#' @param show_points Logical. Show points at each year? Default TRUE.
#' @param line_width Numeric. Width of trend lines. Default 0.6.
#' @param point_size Numeric. Size of points. Default 1.2.
#' @param ci_alpha Numeric. Alpha for CI ribbons if shown. Default 0.15.
#'
#' @return A ggplot object (single panel, no legend)
#'
#' @details
#' The y-axis label is intentionally blank because the shared label will be

#' added by the patchwork annotation. The survey name appears as the title.
#' Legend is suppressed here; it will be extracted and shared across panels.
#'
#' @examples
#' # p <- plot_single_survey_panel(srh_means_nhis$estimates, "NHIS")
#'
plot_single_survey_panel <- function(
    estimates,
    survey_name,
    colors = NULL,
    show_ci = FALSE,
    show_points = TRUE,
    line_width = 0.6,
    point_size = 1.2,
    ci_alpha = 0.15
) {

# --- Input validation ---
  stopifnot(is.data.frame(estimates))
  required_cols <- c("age_group", "year", "mean_srh")
  missing_cols <- setdiff(required_cols, names(estimates))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (show_ci) {
    ci_cols <- c("ci_lower", "ci_upper")
    missing_ci <- setdiff(ci_cols, names(estimates))
    if (length(missing_ci) > 0) {
      stop("show_ci=TRUE but missing columns: ", paste(missing_ci, collapse = ", "))
    }
  }

  # --- Use default colors if not provided ---
  if (is.null(colors)) {
    colors <- age_colors
  }

  # --- Ensure age_group is factor with correct order ---
  # This preserves the ordering from the estimates (which should already be ordered)
  if (!is.factor(estimates$age_group)) {
    estimates$age_group <- factor(estimates$age_group,
                                   levels = unique(estimates$age_group))
  }

  # --- Build the plot ---
  p <- ggplot(estimates, aes(x = year, y = mean_srh,
                              color = age_group,
                              group = age_group)) +
    geom_line(linewidth = line_width)

  # Add points if requested
  if (show_points) {
    p <- p + geom_point(size = point_size)
  }

  # Add CI ribbons if requested
  if (show_ci) {
    p <- p +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = age_group),
                  alpha = ci_alpha, color = NA) +
      scale_fill_manual(values = colors, guide = "none")
  }

  # Apply color scale
  p <- p + scale_color_manual(values = colors)

  # Theme and labels
  # - Title: survey name only
  # - Y-axis label blank (will be shared via patchwork)
  # - X-axis label blank (will be shared via patchwork)
  # - Legend kept for patchwork to collect and display at bottom
  p <- p +
    labs(
      title = survey_name,
      x = NULL,
      y = NULL,
      color = "Age group"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      # Clean, publication-ready appearance
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      # Keep legend - patchwork will collect it
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      axis.text = element_text(size = 7, color = "gray30"),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}


# ------------------------------------------------------------------------------
# COMBINED 2x3 FIGURE
# ------------------------------------------------------------------------------

#' Create combined 2x3 figure for Figure 1 Panel A
#'
#' @description
#' Combines 6 survey panels into a publication-ready 2x3 figure using patchwork.
#' Each panel has its own y-axis (different SRH scales) and x-axis (different
#' year ranges). A shared legend appears at the bottom, with shared axis labels.
#'
#' @param estimates_list Named list of 6 data frames, each containing estimates
#'   from summarize_srh_over_time(). Names should be survey names
#'   (e.g., "NHIS", "MEPS", etc.).
#' @param colors Named vector of colors for age groups. Defaults to age_colors.
#' @param show_ci Logical. Show confidence interval ribbons? Default FALSE.
#' @param ncol Integer. Number of columns in layout. Default 3.
#' @param y_label Character. Shared y-axis label. Default "Weighted mean SRH".
#' @param x_label Character. Shared x-axis label. Default "Year".
#' @param title Character. Overall figure title.
#'   Default "Weighted Mean SRH Per Age Group Over Time".
#' @param subtitle Character. Overall figure subtitle. Default NULL.
#'
#' @return A patchwork object ready for display or saving
#'
#' @details
#' The function:
#' 1. Creates individual panels for each survey
#' 2. Extracts the legend from one panel
#' 3. Arranges panels in 2x3 grid
#' 4. Adds shared legend at bottom
#' 5. Adds rotated y-axis label on the left
#' 6. Adds centered x-axis label at the bottom
#'
#' Survey order in the figure follows the order in estimates_list.
#' Recommended order: BRFSS, MEPS, NHIS (top row), GSS, CPS, NHANES (bottom).
#'
#' @examples
#' # estimates_list <- list(
#' #   "BRFSS" = srh_means_brfss$estimates,
#' #   "MEPS"  = srh_means_meps$estimates,
#' #   "NHIS"  = srh_means_nhis$estimates,
#' #   "GSS"   = srh_means_gss$estimates,
#' #   "CPS"   = srh_means_cps$estimates,
#' #   "NHANES"= srh_means_nhanes$estimates
#' # )
#' # fig <- plot_fig1_panel_a(estimates_list)
#' # ggsave("fig1a.png", fig, width = 12, height = 8)
#'
plot_fig1_panel_a <- function(
    estimates_list,
    colors = NULL,
    show_ci = FALSE,
    ncol = 3,
    y_label = "Weighted mean SRH",
    x_label = "Year",
    title = "Weighted Mean SRH Per Age Group Over Time",
    subtitle = NULL
) {

  # --- Input validation ---
  stopifnot(is.list(estimates_list))
  stopifnot(length(estimates_list) >= 1)

  if (is.null(names(estimates_list))) {
    stop("estimates_list must be a named list (names = survey names)")
  }

  # --- Use default colors if not provided ---
  if (is.null(colors)) {
    colors <- age_colors
  }

  # --- Create individual panels ---
  survey_names <- names(estimates_list)

  panels <- lapply(survey_names, function(svy) {
    plot_single_survey_panel(
      estimates = estimates_list[[svy]],
      survey_name = svy,
      colors = colors,
      show_ci = show_ci
    )
  })
  names(panels) <- survey_names

  # --- Create shared axis labels as grobs ---
  # Y-axis label: rotated 90 degrees, on the left
  y_label_grob <- grid::textGrob(
    y_label,
    rot = 90,
    gp = grid::gpar(fontsize = 12, fontface = "plain")
  )

  # X-axis label: horizontal, centered at bottom
  x_label_grob <- grid::textGrob(
    x_label,
    gp = grid::gpar(fontsize = 12, fontface = "plain")
  )

  # --- Combine panels with patchwork ---
  # Calculate number of rows needed

  n_panels <- length(panels)
  nrow_panels <- ceiling(n_panels / ncol)

  # Combine panels into grid
  panel_grid <- wrap_plots(panels, ncol = ncol, nrow = nrow_panels)

  # --- Build the full layout ---
  # Structure:
  #   [y_label] [panel_grid]
  #             [x_label]
  #             [collected legend at bottom]
  #
  # Using patchwork's native guide collection for reliable legend handling

  # Combine panel grid with y-label on left
  # wrap_elements converts the grob to a patchwork-compatible object
  panels_with_ylabel <- wrap_elements(y_label_grob) | panel_grid
  panels_with_ylabel <- panels_with_ylabel + plot_layout(widths = c(0.03, 1))

  # Then stack: panels_with_ylabel / x_label
  final_figure <- panels_with_ylabel /
    wrap_elements(x_label_grob)

  # Set relative heights and collect guides
  # Heights: panels_with_ylabel (1), x_label (0.06 - enough room to avoid clipping)
  # guides = "collect" moves all legends to the bottom
  final_figure <- final_figure +
    plot_layout(heights = c(1, 0.06), guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.box.background = element_blank()
    )

  # Add overall title and subtitle
  final_figure <- final_figure +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10)
      )
    )

  return(final_figure)
}


# ------------------------------------------------------------------------------
# TABLE EXPORT HELPER
# ------------------------------------------------------------------------------

#' Save estimates table to CSV and RDS
#'
#' @description
#' Saves the estimates data frame for a single survey in both CSV (human-readable)
#' and RDS (R-native) formats. Useful for archiving and verification.
#'
#' @param estimates Data frame with columns: age_group, year, mean_srh, se,
#'   ci_lower, ci_upper
#' @param survey_name Character. Survey name for filename (e.g., "NHIS")
#' @param output_dir Character. Directory to save files. Default "output/tables".
#' @param date_suffix Logical. Add date suffix to filename? Default TRUE for drafts.
#'
#' @return Invisible. Writes files to disk.
#'
#' @examples
#' # save_estimates_table(srh_means_nhis$estimates, "NHIS")
#'
save_estimates_table <- function(
    estimates,
    survey_name,
    output_dir = here::here("output", "tables"),
    date_suffix = TRUE
) {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Build filename
  base_name <- paste0("fig1a_estimates_", tolower(survey_name))
  if (date_suffix) {
    base_name <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"))
  }

  # Round numeric columns for readability in CSV
  estimates_rounded <- estimates %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))

  # Save CSV
  csv_path <- file.path(output_dir, paste0(base_name, ".csv"))
  readr::write_csv(estimates_rounded, csv_path)
  message("Saved: ", csv_path)

  # Save RDS (full precision)
  rds_path <- file.path(output_dir, paste0(base_name, ".rds"))
  readr::write_rds(estimates, rds_path)
  message("Saved: ", rds_path)

  invisible(NULL)
}


#' Save all survey estimates tables
#'
#' @description
#' Convenience wrapper to save estimates tables for all surveys at once.
#'
#' @param estimates_list Named list of estimates data frames
#' @param output_dir Character. Directory to save files.
#' @param date_suffix Logical. Add date suffix to filenames?
#'
#' @return Invisible. Writes files to disk.
#'
save_all_estimates_tables <- function(
    estimates_list,
    output_dir = here::here("output", "tables"),
    date_suffix = TRUE
) {

  survey_names <- names(estimates_list)

  for (svy in survey_names) {
    save_estimates_table(
      estimates = estimates_list[[svy]],
      survey_name = svy,
      output_dir = output_dir,
      date_suffix = date_suffix
    )
  }

  message("\nAll ", length(survey_names), " tables saved to: ", output_dir)
  invisible(NULL)
}
