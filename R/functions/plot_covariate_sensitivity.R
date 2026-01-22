# ==============================================================================
# plot_covariate_sensitivity.R
# Plotting Functions for Covariate Sensitivity Analysis
#
# Purpose: Generate multi-dataset faceted figures for covariate sensitivity
#          reports. All figures display all 6 surveys together for comparison.
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)
library(gt)

# Source dependencies (caller should source, but provide fallback)
if (!exists("age_colors")) {
  source(here::here("R", "functions", "theme_srh.R"))
}

# ==============================================================================
# CONSTANTS
# ==============================================================================

# Dataset ordering for consistent facet display
DATASET_ORDER <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")

# Theme settings for multi-panel figures
COMPACT_BASE_SIZE <- 10
COMPACT_STRIP_SIZE <- 11
COMPACT_POINT_SIZE <- 1.2
COMPACT_LINE_WIDTH <- 0.5
COMPACT_PANEL_SPACING <- unit(3, "pt")


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Prepare data for plotting with consistent dataset ordering
#'
#' @param data Data frame with dataset column
#' @param dataset_order Order of datasets for faceting
#'
#' @return Data frame with dataset as ordered factor
#' @keywords internal
.prepare_plot_data <- function(data, dataset_order = DATASET_ORDER) {
  data %>%
    filter(dataset %in% dataset_order) %>%
    mutate(dataset = factor(dataset, levels = dataset_order))
}


#' Create compact theme for multi-panel covariate figures
#'
#' @param base_size Base font size (default 8)
#'
#' @return ggplot2 theme object
#' @export
theme_covariate_compact <- function(base_size = COMPACT_BASE_SIZE) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Panel
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.2),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.3),
      panel.background = element_rect(fill = "white", color = NA),
      panel.spacing = COMPACT_PANEL_SPACING,

      # Axes
      axis.title = element_text(size = rel(0.9), face = "plain", color = "gray30"),
      axis.text = element_text(size = rel(0.8), color = "gray40"),
      axis.ticks = element_line(color = "gray70", linewidth = 0.2),
      axis.line = element_blank(),

      # Legend
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.8)),
      legend.key.size = unit(0.6, "lines"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.margin = margin(t = 2, b = 2),

      # Facets
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      strip.text = element_text(size = COMPACT_STRIP_SIZE, face = "bold",
                                 margin = margin(2, 2, 2, 2)),

      # Plot
      plot.title = element_text(size = rel(1.1), face = "bold", hjust = 0.5,
                                 margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.95), hjust = 0.5, color = "gray50",
                                    margin = margin(b = 4)),
      plot.margin = margin(4, 4, 4, 4),

      complete = TRUE
    )
}


# ==============================================================================
# PAGE 1: DESCRIPTIVE TRENDS
# ==============================================================================

#' Plot SRH trends by age group across all datasets
#'
#' @description
#' Creates a faceted figure showing mean SRH over time, colored by age group,
#' with rows = datasets (6) and columns = covariate levels (k).
#'
#' @param age_stratum_data Tibble from compute_age_distribution_by_stratum()
#'   with columns: dataset, year, covariate_level, age_group, mean_srh, se_srh
#' @param covariate Character string: covariate name (for filename)
#' @param covariate_label Character string: display name for axis/title
#' @param level_labels Named vector mapping covariate levels to display labels
#'   (e.g., c("1" = "< HS", "2" = "HS/Some", "3" = "BA+"))
#' @param show_ci Logical: show confidence interval ribbons? (default FALSE)
#' @param y_limits Numeric vector of length 2: y-axis limits (default NULL)
#'
#' @return A ggplot object
#'
#' @export
plot_srh_trends_by_age <- function(
    age_stratum_data,
    covariate,
    covariate_label = NULL,
    level_labels = NULL,
    show_ci = FALSE,
    y_limits = NULL
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Prepare data
  plot_data <- age_stratum_data %>%
    .prepare_plot_data() %>%
    mutate(
      age_group = factor(age_group, levels = names(age_colors))
    ) %>%
    filter(!is.na(age_group), !is.na(mean_srh))

  # Apply custom level labels if provided

  if (!is.null(level_labels)) {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(
        level_labels[as.character(covariate_level)],
        levels = level_labels
      ))
  } else {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(covariate_level))
  }

  # Base plot

  p <- ggplot(plot_data, aes(x = year, y = mean_srh,
                             color = age_group, group = age_group)) +
    geom_line(linewidth = COMPACT_LINE_WIDTH) +
    geom_point(size = COMPACT_POINT_SIZE)

  # Add CI ribbons if requested
  if (show_ci && "se_srh" %in% names(plot_data)) {
    plot_data <- plot_data %>%
      mutate(
        ci_low = mean_srh - 1.96 * se_srh,
        ci_high = mean_srh + 1.96 * se_srh
      )

    p <- ggplot(plot_data, aes(x = year, y = mean_srh,
                                color = age_group, fill = age_group,
                                group = age_group)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
                  alpha = 0.15, color = NA) +
      geom_line(linewidth = COMPACT_LINE_WIDTH) +
      geom_point(size = COMPACT_POINT_SIZE) +
      scale_fill_manual(values = age_colors, guide = "none")
  }

  # Facet grid: rows = datasets, columns = covariate levels
  p <- p +
    facet_grid(dataset ~ covariate_level, scales = "free_y") +
    scale_color_manual(values = age_colors, name = "Age Group") +
    labs(
      title = "Mean SRH by Year and Age Group",
      subtitle = paste0("Rows: Surveys | Columns: ", covariate_label),
      x = "Year",
      y = "Mean SRH"
    ) +
    theme_covariate_compact() +
    guides(color = guide_legend(nrow = 1))

  # Apply y-limits if specified
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }

  return(p)
}


# ==============================================================================
# PAGE 2: AGE GRADIENT ANALYSIS
# ==============================================================================

#' Plot beta_age trends over time by covariate level
#'
#' @description
#' Creates a 2x3 faceted figure showing age coefficient trends over time,
#' with one line per covariate level within each dataset panel.
#'
#' @param stratified_betas Tibble from compute_stratified_betas()
#'   with columns: dataset, year, covariate_level, beta_age, se, ci_low, ci_high
#' @param beta_trends Tibble from compute_beta_trends()
#'   with columns: dataset, covariate_level, slope, p_value, r_squared
#' @param covariate Character string: covariate name
#' @param covariate_label Character string: display name
#' @param level_labels Named vector mapping covariate levels to display labels
#' @param show_points Logical: show data points? (default TRUE)
#' @param show_fit Logical: show linear fit lines? (default TRUE)
#' @param show_table Logical: add summary table? (default TRUE)
#'
#' @return A ggplot or patchwork object
#'
#' @export
plot_beta_age_trends <- function(
    stratified_betas,
    beta_trends = NULL,
    covariate,
    covariate_label = NULL,
    level_labels = NULL,
    show_points = TRUE,
    show_fit = TRUE,
    show_table = TRUE
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Prepare data
  plot_data <- stratified_betas %>%
    .prepare_plot_data()

  # Apply custom level labels if provided
  if (!is.null(level_labels)) {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(
        level_labels[as.character(covariate_level)],
        levels = level_labels
      ))
  } else {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(covariate_level))
  }

  # Get covariate level colors
  n_levels <- length(unique(plot_data$covariate_level))
  level_colors <- scales::hue_pal()(n_levels)
  names(level_colors) <- levels(plot_data$covariate_level)

  # Base plot
  p <- ggplot(plot_data, aes(x = year, y = beta_age,
                              color = covariate_level,
                              group = covariate_level)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60",
               linewidth = 0.3)

  # Add points
  if (show_points) {
    p <- p + geom_point(size = COMPACT_POINT_SIZE * 1.2, alpha = 0.7)
  }

  # Add lines
  p <- p + geom_line(linewidth = COMPACT_LINE_WIDTH * 1.2, alpha = 0.8)

  # Add linear fit
  if (show_fit) {
    p <- p + geom_smooth(method = "lm", se = FALSE,
                         linewidth = COMPACT_LINE_WIDTH * 1.5,
                         linetype = "solid")
  }

  # Facet by dataset (1x6 row)
  p <- p +
    facet_wrap(~dataset, nrow = 1, ncol = 6, scales = "free") +
    scale_color_manual(values = level_colors, name = covariate_label) +
    labs(
      title = expression(paste(beta[age], " Trends Over Time")),
      subtitle = paste0("By ", covariate_label, " | Lines show linear fit"),
      x = "Year",
      y = expression(beta[age])
    ) +
    theme_covariate_compact() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8))) +
    guides(color = guide_legend(nrow = 1))

  # Add table if requested and beta_trends provided
  if (show_table && !is.null(beta_trends)) {
    # Create summary table
    trend_summary <- beta_trends %>%
      .prepare_plot_data() %>%
      mutate(
        sig = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        direction = case_when(
          p_value > 0.05 ~ "NS",
          slope > 0 ~ paste0("Conv.", sig),
          slope < 0 ~ paste0("Div.", sig),
          TRUE ~ "NS"
        )
      ) %>%
      select(dataset, covariate_level, direction) %>%
      pivot_wider(names_from = covariate_level, values_from = direction)

    # Return plot with annotation noting table available
    p <- p + labs(caption = "Conv. = Converging (positive slope), Div. = Diverging")
  }

  return(p)
}


#' Plot predicted SRH at representative ages
#'
#' @description
#' Creates a faceted figure showing predicted SRH at specific ages over time,
#' with rows = datasets and columns = covariate levels.
#'
#' @param predicted_srh Tibble from compute_predicted_srh()
#'   with columns: dataset, year, age, covariate_level, predicted_srh, se, ci_low, ci_high
#' @param covariate Character string: covariate name
#' @param covariate_label Character string: display name
#' @param level_labels Named vector mapping covariate levels to display labels
#' @param ages_to_show Numeric vector: ages to display (default c(25, 45, 65, 85))
#' @param show_ci Logical: show confidence intervals? (default TRUE)
#'
#' @return A ggplot object
#'
#' @export
plot_predicted_srh <- function(
    predicted_srh,
    covariate,
    covariate_label = NULL,
    level_labels = NULL,
    ages_to_show = c(25, 45, 65, 85),
    show_ci = TRUE
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Prepare data
  plot_data <- predicted_srh %>%
    .prepare_plot_data() %>%
    filter(age %in% ages_to_show) %>%
    mutate(
      age_label = factor(paste0("Age ", age), levels = paste0("Age ", ages_to_show))
    )

  # Apply custom level labels if provided
  if (!is.null(level_labels)) {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(
        level_labels[as.character(covariate_level)],
        levels = level_labels
      ))
  } else {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(covariate_level))
  }

  # Color palette for ages (sequential)
  n_ages <- length(ages_to_show)
  age_pred_colors <- scales::viridis_pal(option = "D", direction = -1)(n_ages)
  names(age_pred_colors) <- paste0("Age ", ages_to_show)

  # Base plot
  p <- ggplot(plot_data, aes(x = year, y = predicted_srh,
                              color = age_label, group = age_label))

  # Add CI ribbons if requested
  if (show_ci && all(c("ci_low", "ci_high") %in% names(plot_data))) {
    p <- p +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = age_label),
                  alpha = 0.1, color = NA) +
      scale_fill_manual(values = age_pred_colors, guide = "none")
  }

  p <- p +
    geom_line(linewidth = COMPACT_LINE_WIDTH) +
    geom_point(size = COMPACT_POINT_SIZE) +
    facet_grid(dataset ~ covariate_level, scales = "free_y") +
    scale_color_manual(values = age_pred_colors, name = "Age") +
    labs(
      title = "Predicted SRH at Representative Ages",
      subtitle = paste0("Rows: Surveys | Columns: ", covariate_label),
      x = "Year",
      y = "Predicted SRH"
    ) +
    theme_covariate_compact() +
    guides(color = guide_legend(nrow = 1))

  return(p)
}


# ==============================================================================
# PAGE 3: MODEL DIAGNOSTICS
# ==============================================================================

#' Plot covariate coefficients over time
#'
#' @description
#' Creates a 2x3 faceted figure showing covariate effect on SRH over time,
#' with one panel per dataset.
#'
#' @param beta_covariate Tibble from compute_beta_covariate()
#'   with columns: dataset, year, term, contrast, beta, se, ci_low, ci_high
#' @param covariate Character string: covariate name
#' @param covariate_label Character string: display name
#' @param reference_level Character string: reference level for display
#'
#' @return A ggplot object
#'
#' @export
plot_beta_covariate <- function(
    beta_covariate,
    covariate,
    covariate_label = NULL,
    reference_level = NULL
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Prepare data
  plot_data <- beta_covariate %>%
    .prepare_plot_data() %>%
    mutate(contrast = factor(contrast))

  # Color by contrast level
  n_contrasts <- length(unique(plot_data$contrast))
  contrast_colors <- scales::hue_pal()(n_contrasts)
  names(contrast_colors) <- levels(plot_data$contrast)

  # Subtitle
  sub_text <- paste0("Adjusted for age")
  if (!is.null(reference_level)) {
    sub_text <- paste0(sub_text, " | Reference: ", reference_level)
  }

  # Base plot
  p <- ggplot(plot_data, aes(x = year, y = beta,
                              color = contrast, fill = contrast,
                              group = contrast)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60",
               linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = COMPACT_LINE_WIDTH * 1.2) +
    geom_point(size = COMPACT_POINT_SIZE * 1.2) +
    facet_wrap(~dataset, nrow = 1, ncol = 6, scales = "free_y") +
    scale_color_manual(values = contrast_colors, name = covariate_label) +
    scale_fill_manual(values = contrast_colors, guide = "none") +
    labs(
      title = paste0(covariate_label, " Effect on SRH Over Time"),
      subtitle = sub_text,
      x = "Year",
      y = expression(beta[covariate])
    ) +
    theme_covariate_compact() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8))) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}


#' Plot interaction coefficients over time
#'
#' @description
#' Creates a 2x3 faceted figure showing age x covariate interaction
#' coefficients over time, with one panel per dataset.
#'
#' @param interaction_data Tibble from compute_interaction()
#'   with columns: dataset, year, term, beta_interaction, se, ci_low, ci_high,
#'                 wald_fstat, wald_p_value
#' @param covariate Character string: covariate name
#' @param covariate_label Character string: display name
#' @param show_lr_test Logical: annotate with Wald test results? (default TRUE)
#'
#' @return A ggplot object
#'
#' @export
plot_interaction_coefficients <- function(
    interaction_data,
    covariate,
    covariate_label = NULL,
    show_lr_test = TRUE
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Prepare data - extract contrast from term
  plot_data <- interaction_data %>%
    .prepare_plot_data() %>%
    mutate(
      # Extract level from term like "age:factor(educ_3cat)2"
      contrast = gsub(paste0(".*factor\\(", covariate, "\\)"), "", term),
      contrast = factor(contrast)
    )

  # Color by contrast level
  n_contrasts <- length(unique(plot_data$contrast))
  if (n_contrasts > 0) {
    contrast_colors <- scales::hue_pal()(n_contrasts)
    names(contrast_colors) <- levels(plot_data$contrast)
  } else {
    contrast_colors <- c("1" = "#3C5488")
  }

  # Base plot
  p <- ggplot(plot_data, aes(x = year, y = beta_interaction,
                              color = contrast, fill = contrast,
                              group = contrast)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60",
               linewidth = 0.3)

  # Add CI ribbons if available
  if (all(c("ci_low", "ci_high") %in% names(plot_data))) {
    p <- p +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
                  alpha = 0.15, color = NA) +
      scale_fill_manual(values = contrast_colors, guide = "none")
  }

  p <- p +
    geom_line(linewidth = COMPACT_LINE_WIDTH * 1.2) +
    geom_point(size = COMPACT_POINT_SIZE * 1.2) +
    facet_wrap(~dataset, nrow = 1, ncol = 6, scales = "free_y") +
    scale_color_manual(values = contrast_colors,
                       name = paste0("Age \u00D7 ", covariate_label)) +
    labs(
      title = paste0("Age \u00D7 ", covariate_label, " Interaction Over Time"),
      subtitle = "Values near zero indicate no differential age gradient",
      x = "Year",
      y = expression(beta[interaction])
    ) +
    theme_covariate_compact() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8))) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}


# ==============================================================================
# SUMMARY TABLES
# ==============================================================================

#' Create variance decomposition table
#'
#' @description
#' Creates a formatted table showing R-squared values from nested models
#' across all datasets.
#'
#' @param variance_data Tibble from compute_variance_decomposition()
#'   with columns: dataset, year, r2_age, r2_age_cov, r2_full,
#'                 delta_r2_covariate, delta_r2_interaction
#' @param year_bins Optional: vector of year breakpoints for binning
#'   (e.g., c(2000, 2010, 2020) creates early/late comparison)
#'
#' @return A tibble formatted for display
#'
#' @export
create_variance_table <- function(variance_data, year_bins = NULL) {

  data <- variance_data %>%
    .prepare_plot_data()

  if (!is.null(year_bins) && length(year_bins) >= 2) {
    # Create period bins
    data <- data %>%
      mutate(
        period = cut(year, breaks = year_bins,
                     labels = paste0(head(year_bins, -1), "-", tail(year_bins, -1)),
                     include.lowest = TRUE)
      ) %>%
      filter(!is.na(period)) %>%
      group_by(dataset, period) %>%
      summarise(
        r2_age = mean(r2_age, na.rm = TRUE),
        r2_age_cov = mean(r2_age_cov, na.rm = TRUE),
        r2_full = mean(r2_full, na.rm = TRUE),
        delta_r2_covariate = mean(delta_r2_covariate, na.rm = TRUE),
        delta_r2_interaction = mean(delta_r2_interaction, na.rm = TRUE),
        n_years = n(),
        .groups = "drop"
      )
  } else {
    # Use first and last years
    data <- data %>%
      group_by(dataset) %>%
      filter(year == min(year) | year == max(year)) %>%
      mutate(period = ifelse(year == min(year), "Early", "Late")) %>%
      ungroup() %>%
      select(dataset, period, r2_age, r2_age_cov, r2_full,
             delta_r2_covariate, delta_r2_interaction)
  }

  # Format for display
  result <- data %>%
    mutate(across(starts_with("r2") | starts_with("delta"),
                  ~ sprintf("%.3f", .))) %>%
    arrange(dataset, period)

  return(result)
}


#' Create beta trends summary table
#'
#' @description
#' Creates a formatted table showing convergence/divergence patterns
#' across datasets and covariate levels.
#'
#' @param beta_trends Tibble from compute_beta_trends()
#'   with columns: dataset, covariate_level, slope, p_value, interpretation
#' @param covariate_labels Named vector mapping levels to display names
#'
#' @return A tibble formatted for display
#'
#' @export
create_beta_trends_table <- function(beta_trends, covariate_labels = NULL) {

  data <- beta_trends %>%
    .prepare_plot_data() %>%
    mutate(
      sig = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      ),
      direction = case_when(
        p_value > 0.05 ~ "NS",
        slope > 0 ~ paste0("Converging", sig),
        slope < 0 ~ paste0("Diverging", sig),
        TRUE ~ "NS"
      )
    )

  # Apply labels if provided
  if (!is.null(covariate_labels)) {
    data <- data %>%
      mutate(covariate_level = covariate_labels[as.character(covariate_level)])
  }

  # Pivot wider
  result <- data %>%
    select(dataset, covariate_level, direction) %>%
    pivot_wider(names_from = covariate_level, values_from = direction)

  return(result)
}


#' Create SRH distribution table (% Poor/Fair)
#'
#' @description
#' Creates a table showing % Poor/Fair SRH by covariate level for a specific year.
#'
#' @param srh_distribution Tibble from compute_srh_distribution()
#' @param target_year Numeric: year to display (uses most recent if NULL)
#'
#' @return A tibble formatted for display
#'
#' @export
create_srh_distribution_table <- function(srh_distribution, target_year = NULL) {

  data <- srh_distribution %>%
    .prepare_plot_data() %>%
    filter(srh_category %in% c("Poor", "Fair"))

  if (is.null(target_year)) {
    target_year <- max(data$year)
  }

  result <- data %>%
    filter(year == target_year) %>%
    group_by(dataset, covariate_level) %>%
    summarise(
      pct_poor_fair = sum(proportion, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = covariate_level,
      values_from = pct_poor_fair,
      names_prefix = "Level_"
    ) %>%
    mutate(
      gap = .[[ncol(.)]] - .[[3]]  # First vs last level
    )

  # Format percentages
  result <- result %>%
    mutate(across(starts_with("Level"), ~ sprintf("%.1f%%", .)),
           gap = sprintf("%.1f pp", gap))

  return(result)
}


#' Create sample composition changes table
#'
#' @description
#' Creates a table showing % in each covariate level, early vs late period.
#'
#' @param sample_sizes Tibble from compute_sample_diagnostics()$sample_sizes
#'
#' @return A tibble formatted for display
#'
#' @export
create_composition_table <- function(sample_sizes) {

  data <- sample_sizes %>%
    .prepare_plot_data()

  # Get early and late years per dataset
  result <- data %>%
    group_by(dataset) %>%
    mutate(
      period = case_when(
        year == min(year) ~ "Early",
        year == max(year) ~ "Late",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period)) %>%
    ungroup() %>%
    select(dataset, period, covariate_level, pct_of_total) %>%
    pivot_wider(
      names_from = c(covariate_level, period),
      values_from = pct_of_total,
      names_sep = "_"
    )

  return(result)
}


#' Create age gradient table by education
#'
#' @description
#' Creates a table showing age gradient (80-89 minus 18-29) by covariate level.
#'
#' @param age_stratum_data Tibble from compute_age_distribution_by_stratum()
#'
#' @return A tibble formatted for display
#'
#' @export
create_age_gradient_table <- function(age_stratum_data) {

  data <- age_stratum_data %>%
    .prepare_plot_data() %>%
    filter(age_group %in% c("18-29", "80-89"))

  # Calculate gradient
  result <- data %>%
    select(dataset, year, covariate_level, age_group, mean_srh) %>%
    pivot_wider(names_from = age_group, values_from = mean_srh) %>%
    mutate(gradient = `80-89` - `18-29`) %>%
    group_by(dataset) %>%
    mutate(
      period = case_when(
        year == min(year) ~ "Early",
        year == max(year) ~ "Late",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period)) %>%
    ungroup() %>%
    select(dataset, period, covariate_level, gradient) %>%
    pivot_wider(
      names_from = c(covariate_level, period),
      values_from = gradient,
      names_sep = "_"
    )

  # Format
  result <- result %>%
    mutate(across(where(is.numeric), ~ sprintf("%.2f", .)))

  return(result)
}


# ==============================================================================
# PAGE 4: COMPOSITION & DISTRIBUTION
# ==============================================================================

#' Plot SRH standard deviation over time
#'
#' @description
#' Creates a faceted figure showing SD of SRH by covariate level over time.
#'
#' @param srh_variability Tibble from compute_srh_variability()
#'   with columns: dataset, year, covariate_level, sd_srh
#' @param covariate Character string: covariate name
#' @param covariate_label Character string: display name
#' @param level_labels Named vector mapping covariate levels to display labels
#'
#' @return A ggplot object
#'
#' @export
plot_srh_sd <- function(
    srh_variability,
    covariate,
    covariate_label = NULL,
    level_labels = NULL
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Prepare data
  plot_data <- srh_variability %>%
    .prepare_plot_data()

  # Apply custom level labels if provided
  if (!is.null(level_labels)) {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(
        level_labels[as.character(covariate_level)],
        levels = level_labels
      ))
  } else {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(covariate_level))
  }

  # Color by covariate level
  n_levels <- length(unique(plot_data$covariate_level))
  level_colors <- scales::hue_pal()(n_levels)
  names(level_colors) <- levels(plot_data$covariate_level)

  p <- ggplot(plot_data, aes(x = year, y = sd_srh,
                              color = covariate_level,
                              group = covariate_level)) +
    geom_line(linewidth = COMPACT_LINE_WIDTH * 1.2) +
    geom_point(size = COMPACT_POINT_SIZE * 1.2) +
    facet_wrap(~dataset, nrow = 1, ncol = 6, scales = "free_y") +
    scale_color_manual(values = level_colors, name = covariate_label) +
    labs(
      title = "SRH Variability Over Time",
      subtitle = "Standard deviation of self-rated health",
      x = "Year",
      y = "SD of SRH"
    ) +
    theme_covariate_compact() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8))) +
    guides(color = guide_legend(nrow = 1))

  return(p)
}


#' Plot SRH distribution over time (stacked area)
#'
#' @description
#' Creates a faceted figure showing % in each SRH category over time,
#' with rows = datasets and columns = covariate levels.
#'
#' @param srh_distribution Tibble from compute_srh_distribution()
#'   with columns: dataset, year, covariate_level, srh_category, proportion
#' @param covariate Character string: covariate name
#' @param covariate_label Character string: display name
#' @param level_labels Named vector mapping covariate levels to display labels
#'
#' @return A ggplot object
#'
#' @export
plot_srh_distribution <- function(
    srh_distribution,
    covariate,
    covariate_label = NULL,
    level_labels = NULL
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Prepare data
  plot_data <- srh_distribution %>%
    .prepare_plot_data() %>%
    mutate(
      srh_category = factor(srh_category,
                            levels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))
    ) %>%
    filter(!is.na(srh_category))

  # Apply custom level labels if provided
  if (!is.null(level_labels)) {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(
        level_labels[as.character(covariate_level)],
        levels = level_labels
      ))
  } else {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(covariate_level))
  }

  p <- ggplot(plot_data, aes(x = year, y = proportion,
                              fill = srh_category)) +
    geom_area(position = "stack", alpha = 0.8) +
    facet_grid(dataset ~ covariate_level) +
    scale_fill_manual(values = srh_colors_pastel, name = "SRH") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "SRH Distribution Over Time",
      subtitle = paste0("Rows: Surveys | Columns: ", covariate_label),
      x = "Year",
      y = "Proportion"
    ) +
    theme_covariate_compact() +
    guides(fill = guide_legend(nrow = 1))

  return(p)
}


#' Plot sample composition over time (stacked area)
#'
#' @description
#' Creates a 2x3 faceted figure showing % in each covariate category over time.
#'
#' @param sample_sizes Tibble from compute_sample_diagnostics()$sample_sizes
#'   with columns: dataset, year, covariate_level, pct_of_total
#' @param covariate Character string: covariate name
#' @param covariate_label Character string: display name
#' @param level_labels Named vector mapping covariate levels to display labels
#'
#' @return A ggplot object
#'
#' @export
plot_composition <- function(
    sample_sizes,
    covariate,
    covariate_label = NULL,
    level_labels = NULL
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Prepare data
  plot_data <- sample_sizes %>%
    .prepare_plot_data()

  # Apply custom level labels if provided
  if (!is.null(level_labels)) {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(
        level_labels[as.character(covariate_level)],
        levels = level_labels
      ))
  } else {
    plot_data <- plot_data %>%
      mutate(covariate_level = factor(covariate_level))
  }

  # Color by covariate level
  n_levels <- length(unique(plot_data$covariate_level))
  level_colors <- scales::hue_pal()(n_levels)
  names(level_colors) <- levels(plot_data$covariate_level)

  p <- ggplot(plot_data, aes(x = year, y = pct_of_total / 100,
                              fill = covariate_level)) +
    geom_area(position = "stack", alpha = 0.8) +
    facet_wrap(~dataset, nrow = 1, ncol = 6) +
    scale_fill_manual(values = level_colors, name = covariate_label) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Sample Composition Over Time",
      subtitle = paste0("Percent in each ", covariate_label, " category"),
      x = "Year",
      y = "Percent of Sample"
    ) +
    theme_covariate_compact() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8))) +
    guides(fill = guide_legend(nrow = 1))

  return(p)
}


#' Create sample diagnostics table
#'
#' @description
#' Creates a formatted table showing sample sizes and missingness by dataset.
#'
#' @param sample_diagnostics List from compute_sample_diagnostics()
#'   with sample_sizes and missingness tibbles
#' @param target_year Numeric: year to display (uses most recent if NULL)
#'
#' @return A tibble formatted for display
#'
#' @export
create_sample_table <- function(sample_diagnostics, target_year = NULL) {

  sample_sizes <- sample_diagnostics$sample_sizes %>%
    .prepare_plot_data()
  missingness <- sample_diagnostics$missingness %>%
    .prepare_plot_data()

  if (is.null(target_year)) {
    target_year <- max(sample_sizes$year)
  }

  # Sample sizes summary
  sizes <- sample_sizes %>%
    filter(year == target_year) %>%
    group_by(dataset) %>%
    summarise(
      n_total = sum(n_unweighted),
      n_by_level = paste(n_unweighted, collapse = " / "),
      .groups = "drop"
    )

  # Missingness summary
  missing <- missingness %>%
    filter(year == target_year) %>%
    select(dataset, pct_missing_covariate, pct_missing_srh, pct_complete)

  # Combine
  result <- sizes %>%
    left_join(missing, by = "dataset") %>%
    mutate(
      across(starts_with("pct"), ~ sprintf("%.1f%%", .))
    )

  return(result)
}


# ==============================================================================
# COMBINED REPORT FUNCTIONS
# ==============================================================================

#' Generate all figures for covariate sensitivity report
#'
#' @description
#' Creates all figures needed for the 4-page covariate sensitivity report.
#'
#' @param analysis_results List containing all analysis results from
#'   the compute_* functions
#' @param covariate Character string: covariate name
#' @param covariate_label Character string: display name
#' @param output_dir Directory to save figures
#'
#' @return List of ggplot objects
#'
#' @export
generate_covariate_figures <- function(
    analysis_results,
    covariate,
    covariate_label = NULL,
    output_dir = here::here("output", "sensitivity", "figures")
) {

  if (is.null(covariate_label)) covariate_label <- covariate

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  figures <- list()

  # Page 1: SRH trends by age
  if ("age_distribution" %in% names(analysis_results)) {
    figures$page1_srh_trends <- plot_srh_trends_by_age(
      analysis_results$age_distribution,
      covariate = covariate,
      covariate_label = covariate_label
    )
  }

  # Page 2: Beta age trends
  if ("stratified_betas" %in% names(analysis_results)) {
    figures$page2a_beta_trends <- plot_beta_age_trends(
      analysis_results$stratified_betas,
      beta_trends = analysis_results$beta_trends,
      covariate = covariate,
      covariate_label = covariate_label
    )
  }

  # Page 2: Predicted SRH
  if ("predicted_srh" %in% names(analysis_results)) {
    figures$page2b_predicted <- plot_predicted_srh(
      analysis_results$predicted_srh,
      covariate = covariate,
      covariate_label = covariate_label
    )
  }

  # Page 3: Beta covariate
  if ("beta_covariate" %in% names(analysis_results)) {
    figures$page3a_beta_cov <- plot_beta_covariate(
      analysis_results$beta_covariate,
      covariate = covariate,
      covariate_label = covariate_label
    )
  }

  # Page 3: Interaction
  if ("interaction" %in% names(analysis_results)) {
    figures$page3b_interaction <- plot_interaction_coefficients(
      analysis_results$interaction,
      covariate = covariate,
      covariate_label = covariate_label
    )
  }

  # Page 4: SD
  if ("srh_variability" %in% names(analysis_results)) {
    figures$page4a_sd <- plot_srh_sd(
      analysis_results$srh_variability,
      covariate = covariate,
      covariate_label = covariate_label
    )
  }

  # Page 4: Distribution
  if ("srh_distribution" %in% names(analysis_results)) {
    figures$page4b_distribution <- plot_srh_distribution(
      analysis_results$srh_distribution,
      covariate = covariate,
      covariate_label = covariate_label
    )
  }

  # Page 4: Composition
  if ("sample_diagnostics" %in% names(analysis_results)) {
    figures$page4c_composition <- plot_composition(
      analysis_results$sample_diagnostics$sample_sizes,
      covariate = covariate,
      covariate_label = covariate_label
    )
  }

  return(figures)
}


#' Save covariate sensitivity figures
#'
#' @description
#' Saves all figures to specified directory.
#'
#' @param figures List of ggplot objects from generate_covariate_figures()
#' @param covariate Character string: covariate name (for filenames)
#' @param output_dir Directory to save figures
#' @param width Figure width in inches (default 8.5 for letter)
#' @param height Figure height in inches (default 11 for letter)
#'
#' @export
save_covariate_figures <- function(
    figures,
    covariate,
    output_dir = here::here("output", "sensitivity", "figures"),
    width = 8.5,
    height = 11
) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  for (name in names(figures)) {
    filename <- paste0(covariate, "_", name)

    # Save PNG
    ggsave(
      filename = file.path(output_dir, paste0(filename, ".png")),
      plot = figures[[name]],
      width = width,
      height = height,
      dpi = 300
    )

    # Save PDF
    ggsave(
      filename = file.path(output_dir, paste0(filename, ".pdf")),
      plot = figures[[name]],
      width = width,
      height = height
    )

    message("Saved: ", filename)
  }

  invisible(NULL)
}
