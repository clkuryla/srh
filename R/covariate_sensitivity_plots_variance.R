# ==============================================================================
# covariate_sensitivity_plots_variance.R
# Plotting functions for Variance Explained page (Page 3) of covariate sensitivity analyses
#
# Purpose: Visualize R² and ΔR² for various models comparing age and covariate effects
#          on SRH over time across 6 surveys.
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)
library(here)

# Source theme for colors and styling
source(here::here("R", "functions", "theme_srh.R"))

# ==============================================================================
# COLOR PALETTES FOR VARIANCE PLOTS
# ==============================================================================

#' Survey color palette for combined plots
survey_colors <- c(
  "BRFSS"  = "#E69F00",
  "MEPS"   = "#56B4E9",
  "NHIS"   = "#009E73",
  "CPS"    = "#F0E442",
  "NHANES" = "#0072B2",
  "GSS"    = "#D55E00"
)

#' Model color palette for model comparison plots
model_colors <- c(
  "SRH ~ age" = "#000000",
  "SRH ~ covariate" = "#0072B2",
  "SRH ~ age + covariate" = "#009E73",
  "SRH ~ age * covariate" = "#D55E00"
)

#' Model linetype palette for model comparison plots
model_linetypes <- c(
  "SRH ~ age" = "solid",
  "SRH ~ covariate" = "dashed",
  "SRH ~ age + covariate" = "solid",
  "SRH ~ age * covariate" = "dotted"
)

# ==============================================================================
# SV1: R² FOR AGE ONLY (SRH ~ age)
# ==============================================================================

#' Plot R² for SRH ~ age over time
#'
#' @description
#' Creates two versions of a plot showing R² for SRH ~ age over time:
#'   a. Faceted: 1×6 panels, one per dataset
#'   b. Combined: single panel with 6 colored lines (one per dataset)
#'
#' @param r2_data_list Named list of data frames (one per dataset), each with
#'   columns: year, r_squared, n (from compute_r_squared)
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 11.
#' @param point_size Size of points. Default 1.5.
#' @param line_width Width of lines. Default 0.7.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#'
#' @return List with two ggplot objects: list(faceted = ..., combined = ...)
#'
plot_sv1_age_r2 <- function(
    r2_data_list,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 11,
    point_size = 1.5,
    line_width = 0.7,
    tilt_x_labels = 45
) {

  # Filter to available datasets and combine
  available_datasets <- intersect(dataset_names, names(r2_data_list))

  all_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      r2_data_list[[ds]] %>%
        mutate(dataset = ds)
    })
  ) %>%
    filter(!is.na(r_squared)) %>%
    mutate(dataset = factor(dataset, levels = dataset_names))

  if (nrow(all_data) == 0) {
    return(list(
      faceted = ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data") + theme_void(),
      combined = ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data") + theme_void()
    ))
  }

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Faceted version: 1×6 panels
  faceted <- ggplot(all_data, aes(x = year, y = r_squared)) +
    geom_line(linewidth = line_width, color = "gray30") +
    geom_point(size = point_size, color = "gray30") +
    facet_wrap(~dataset, nrow = 1, scales = "free_x") +
    labs(
      x = "Year",
      y = expression(R^2),
      title = expression("R"^2 ~ "for SRH ~ age")
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.text.x = x_tick_style,
      plot.margin = margin(10, 10, 10, 10)
    )

  # Combined version: single panel with 6 lines
  combined <- ggplot(all_data, aes(x = year, y = r_squared, color = dataset)) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    scale_color_manual(values = survey_colors, name = "Dataset") +
    labs(
      x = "Year",
      y = expression(R^2),
      title = expression("R"^2 ~ "for SRH ~ age")
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.text.x = x_tick_style,
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(color = guide_legend(nrow = 1))

  return(list(faceted = faceted, combined = combined))
}


# ==============================================================================
# SV2: R² FOR COVARIATE ONLY (SRH ~ covariate)
# ==============================================================================

#' Plot R² for SRH ~ covariate over time
#'
#' @description
#' Creates two versions of a plot showing R² for SRH ~ covariate over time:
#'   a. Faceted: 1×6 panels, one per dataset
#'   b. Combined: single panel with 6 colored lines (one per dataset)
#'
#' @param r2_data_list Named list of data frames (one per dataset), each with
#'   columns: year, r_squared, n (from compute_r_squared)
#' @param covariate_name Display name for the covariate (for title)
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 11.
#' @param point_size Size of points. Default 1.5.
#' @param line_width Width of lines. Default 0.7.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#'
#' @return List with two ggplot objects: list(faceted = ..., combined = ...)
#'
plot_sv2_covariate_r2 <- function(
    r2_data_list,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 11,
    point_size = 1.5,
    line_width = 0.7,
    tilt_x_labels = 45
) {

  # Filter to available datasets and combine
  available_datasets <- intersect(dataset_names, names(r2_data_list))

  all_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      r2_data_list[[ds]] %>%
        mutate(dataset = ds)
    })
  ) %>%
    filter(!is.na(r_squared)) %>%
    mutate(dataset = factor(dataset, levels = dataset_names))

  if (nrow(all_data) == 0) {
    return(list(
      faceted = ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data") + theme_void(),
      combined = ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data") + theme_void()
    ))
  }

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Faceted version: 1×6 panels
  faceted <- ggplot(all_data, aes(x = year, y = r_squared)) +
    geom_line(linewidth = line_width, color = "steelblue") +
    geom_point(size = point_size, color = "steelblue") +
    facet_wrap(~dataset, nrow = 1, scales = "free_x") +
    labs(
      x = "Year",
      y = expression(R^2),
      title = bquote(R^2 ~ "for SRH ~" ~ .(covariate_name))
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.text.x = x_tick_style,
      plot.margin = margin(10, 10, 10, 10)
    )

  # Combined version: single panel with 6 lines
  combined <- ggplot(all_data, aes(x = year, y = r_squared, color = dataset)) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    scale_color_manual(values = survey_colors, name = "Dataset") +
    labs(
      x = "Year",
      y = expression(R^2),
      title = bquote(R^2 ~ "for SRH ~" ~ .(covariate_name))
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.text.x = x_tick_style,
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(color = guide_legend(nrow = 1))

  return(list(faceted = faceted, combined = combined))
}


# ==============================================================================
# SV7: MODEL COMPARISON (4 models per dataset)
# ==============================================================================

#' Plot R² for 4 models comparing age and covariate effects
#'
#' @description
#' Creates a 1×6 panel figure showing R² over time for 4 nested models:
#'   1. SRH ~ age
#'   2. SRH ~ covariate
#'   3. SRH ~ age + covariate
#'   4. SRH ~ age * covariate (with interaction)
#'
#' @param model_r2_data_list Named list of data frames (one per dataset), each
#'   with columns: year, model (1-4), r_squared, n (from compute_model_comparison_r2)
#' @param covariate_name Display name for the covariate (for title and legend)
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 11.
#' @param point_size Size of points. Default 1.2.
#' @param line_width Width of lines. Default 0.7.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#'
#' @return A ggplot object
#'
plot_sv7_model_comparison <- function(
    model_r2_data_list,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 11,
    point_size = 1.2,
    line_width = 0.7,
    tilt_x_labels = 45
) {

  # Filter to available datasets and combine
  available_datasets <- intersect(dataset_names, names(model_r2_data_list))

  # Create model labels
  model_labels <- c(
    "1" = "SRH ~ age",
    "2" = "SRH ~ covariate",
    "3" = "SRH ~ age + covariate",
    "4" = "SRH ~ age * covariate"
  )

  all_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      model_r2_data_list[[ds]] %>%
        mutate(dataset = ds)
    })
  ) %>%
    filter(!is.na(r_squared)) %>%
    mutate(
      dataset = factor(dataset, levels = dataset_names),
      model_label = factor(model_labels[as.character(model)], levels = model_labels)
    )

  if (nrow(all_data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data") + theme_void())
  }

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Create plot with facets by dataset
  p <- ggplot(all_data, aes(x = year, y = r_squared,
                             color = model_label, linetype = model_label)) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size) +
    facet_wrap(~dataset, nrow = 1, scales = "free_x") +
    scale_color_manual(values = model_colors, name = "Model") +
    scale_linetype_manual(values = model_linetypes, name = "Model") +
    labs(
      x = "Year",
      y = expression(R^2),
      title = paste0("Model Comparison: ", covariate_name)
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.text.x = x_tick_style,
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(
      color = guide_legend(nrow = 2),
      linetype = guide_legend(nrow = 2)
    )

  return(p)
}


# ==============================================================================
# SV8A: DELTA R² FROM ADDING COVARIATE TO AGE MODEL
# ==============================================================================

#' Plot ΔR² from adding covariate to age model
#'
#' @description
#' Creates a 1×6 panel figure showing ΔR² over time, computed as:
#' R²(SRH ~ age + covariate) - R²(SRH ~ age)
#'
#' @param model_r2_data_list Named list of data frames (one per dataset), each
#'   with columns: year, model (1-4), r_squared, n (from compute_model_comparison_r2)
#' @param covariate_name Display name for the covariate (for title)
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 11.
#' @param point_size Size of points. Default 1.5.
#' @param line_width Width of lines. Default 0.7.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#'
#' @return A ggplot object
#'
plot_sv8a_delta_r2_covariate <- function(
    model_r2_data_list,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 11,
    point_size = 1.5,
    line_width = 0.7,
    tilt_x_labels = 45
) {

  # Filter to available datasets and compute delta R²
  available_datasets <- intersect(dataset_names, names(model_r2_data_list))

  delta_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      df <- model_r2_data_list[[ds]]

      # Get R² for model 1 (age only) and model 3 (age + covariate)
      r2_age <- df %>% filter(model == 1) %>% select(year, r2_age = r_squared)
      r2_both <- df %>% filter(model == 3) %>% select(year, r2_both = r_squared)

      # Compute delta
      left_join(r2_age, r2_both, by = "year") %>%
        mutate(
          delta_r2 = r2_both - r2_age,
          dataset = ds
        ) %>%
        filter(!is.na(delta_r2))
    })
  ) %>%
    mutate(dataset = factor(dataset, levels = dataset_names))

  if (nrow(delta_data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data") + theme_void())
  }

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  p <- ggplot(delta_data, aes(x = year, y = delta_r2)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_line(linewidth = line_width, color = "#009E73") +
    geom_point(size = point_size, color = "#009E73") +
    facet_wrap(~dataset, nrow = 1, scales = "free_x") +
    labs(
      x = "Year",
      y = expression(Delta * R^2),
      title = bquote(Delta * R^2 ~ "from adding" ~ .(covariate_name) ~ "to age model")
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.text.x = x_tick_style,
      plot.margin = margin(10, 10, 10, 10)
    )

  return(p)
}


# ==============================================================================
# SV8B: DELTA R² FROM ADDING INTERACTION
# ==============================================================================

#' Plot ΔR² from adding interaction term
#'
#' @description
#' Creates a 1×6 panel figure showing ΔR² over time, computed as:
#' R²(SRH ~ age * covariate) - R²(SRH ~ age + covariate)
#'
#' @param model_r2_data_list Named list of data frames (one per dataset), each
#'   with columns: year, model (1-4), r_squared, n (from compute_model_comparison_r2)
#' @param covariate_name Display name for the covariate (for title)
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 11.
#' @param point_size Size of points. Default 1.5.
#' @param line_width Width of lines. Default 0.7.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#'
#' @return A ggplot object
#'
plot_sv8b_delta_r2_interaction <- function(
    model_r2_data_list,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 11,
    point_size = 1.5,
    line_width = 0.7,
    tilt_x_labels = 45
) {

  # Filter to available datasets and compute delta R²
  available_datasets <- intersect(dataset_names, names(model_r2_data_list))

  delta_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      df <- model_r2_data_list[[ds]]

      # Get R² for model 3 (age + covariate) and model 4 (with interaction)
      r2_additive <- df %>% filter(model == 3) %>% select(year, r2_additive = r_squared)
      r2_interaction <- df %>% filter(model == 4) %>% select(year, r2_interaction = r_squared)

      # Compute delta
      left_join(r2_additive, r2_interaction, by = "year") %>%
        mutate(
          delta_r2 = r2_interaction - r2_additive,
          dataset = ds
        ) %>%
        filter(!is.na(delta_r2))
    })
  ) %>%
    mutate(dataset = factor(dataset, levels = dataset_names))

  if (nrow(delta_data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data") + theme_void())
  }

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  p <- ggplot(delta_data, aes(x = year, y = delta_r2)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_line(linewidth = line_width, color = "#D55E00") +
    geom_point(size = point_size, color = "#D55E00") +
    facet_wrap(~dataset, nrow = 1, scales = "free_x") +
    labs(
      x = "Year",
      y = expression(Delta * R^2),
      title = bquote(Delta * R^2 ~ "from adding age ×" ~ .(covariate_name) ~ "interaction")
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.text.x = x_tick_style,
      plot.margin = margin(10, 10, 10, 10)
    )

  return(p)
}


# ==============================================================================
# ASSEMBLY: VARIANCE EXPLAINED PAGE
# ==============================================================================

#' Assemble the complete Variance Explained page
#'
#' @description
#' Combines all variance plots (SV1, SV2, SV7, SV8A, SV8B) into a single figure.
#' Uses the faceted versions of SV1 and SV2.
#'
#' @param sv1 List from plot_sv1_age_r2() with $faceted and $combined
#' @param sv2 List from plot_sv2_covariate_r2() with $faceted and $combined
#' @param sv7 ggplot from plot_sv7_model_comparison()
#' @param sv8a ggplot from plot_sv8a_delta_r2_covariate()
#' @param sv8b ggplot from plot_sv8b_delta_r2_interaction()
#' @param covariate_name Display name for the covariate (for overall title)
#' @param base_size Base font size for title. Default 14.
#'
#' @return A patchwork object ready for saving
#'
assemble_variance_page <- function(
    sv1,
    sv2,
    sv7,
    sv8a,
    sv8b,
    covariate_name,
    base_size = 14
) {

  # Use faceted versions for SV1 and SV2
  sv1_plot <- sv1$faceted
  sv2_plot <- sv2$faceted

  # Stack all plots vertically
  combined <- sv1_plot / sv2_plot / sv7 / sv8a / sv8b +
    plot_layout(heights = c(1, 1, 1.2, 1, 1))

  # Add overall title
  combined <- combined +
    plot_annotation(
      title = paste0("Variance Explained: ", covariate_name),
      theme = theme(
        plot.title = element_text(
          size = base_size + 4,
          face = "bold",
          hjust = 0.5,
          margin = margin(b = 12)
        ),
        plot.margin = margin(10, 10, 10, 10)
      )
    )

  return(combined)
}


# ==============================================================================
# CONVENIENCE FUNCTION: RUN COMPLETE VARIANCE ANALYSIS
# ==============================================================================

#' Run complete variance analysis and generate plots
#'
#' @description
#' Convenience function that computes all necessary R² statistics and generates
#' the complete variance page plot for a given covariate.
#'
#' @param datasets_list Named list of 6 data frames (one per dataset)
#' @param covariate_var Name of the covariate variable
#' @param covariate_name Display name for the covariate (for titles)
#' @param dataset_names Character vector of dataset names in display order
#' @param weight_var Name of weight variable. Default "wt".
#' @param min_n Minimum sample size. Default 30.
#' @param base_size Base font size. Default 11.
#'
#' @return A list with:
#'   - r2_age_list: R² for SRH ~ age by dataset
#'   - r2_cov_list: R² for SRH ~ covariate by dataset
#'   - r2_model_list: R² for all 4 models by dataset
#'   - sv1: SV1 plots (faceted and combined)
#'   - sv2: SV2 plots (faceted and combined)
#'   - sv7: SV7 model comparison plot
#'   - sv8a: SV8A delta R² plot
#'   - sv8b: SV8B delta R² interaction plot
#'   - combined: Full variance page
#'
run_variance_analysis <- function(
    datasets_list,
    covariate_var,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    weight_var = "wt",
    min_n = 30,
    base_size = 11
) {

  # Source computation functions if not loaded
  if (!exists("compute_r_squared")) {
    source(here::here("R", "covariate_sensitivity_functions.R"))
  }

  available_datasets <- intersect(dataset_names, names(datasets_list))

  # Compute R² for SRH ~ age for each dataset
  message("Computing R² for SRH ~ age...")
  r2_age_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_r_squared(
      data = datasets_list[[dataset]],
      formula = "srh ~ age",
      weight_var = weight_var,
      min_n = min_n
    )
  })
  names(r2_age_list) <- available_datasets

  # Compute R² for SRH ~ covariate for each dataset
  message("Computing R² for SRH ~ ", covariate_var, "...")
  r2_cov_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_r_squared(
      data = datasets_list[[dataset]],
      formula = paste0("srh ~ ", covariate_var),
      weight_var = weight_var,
      min_n = min_n
    )
  })
  names(r2_cov_list) <- available_datasets

  # Compute model comparison R² for each dataset
  message("Computing model comparison R²...")
  r2_model_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_model_comparison_r2(
      data = datasets_list[[dataset]],
      covariate_var = covariate_var,
      weight_var = weight_var,
      min_n = min_n
    )
  })
  names(r2_model_list) <- available_datasets

  # Create plots
  message("Creating SV1 (age R²) plots...")
  sv1 <- plot_sv1_age_r2(
    r2_data_list = r2_age_list,
    dataset_names = available_datasets,
    base_size = base_size
  )

  message("Creating SV2 (covariate R²) plots...")
  sv2 <- plot_sv2_covariate_r2(
    r2_data_list = r2_cov_list,
    covariate_name = covariate_name,
    dataset_names = available_datasets,
    base_size = base_size
  )

  message("Creating SV7 (model comparison) plot...")
  sv7 <- plot_sv7_model_comparison(
    model_r2_data_list = r2_model_list,
    covariate_name = covariate_name,
    dataset_names = available_datasets,
    base_size = base_size
  )

  message("Creating SV8A (delta R² covariate) plot...")
  sv8a <- plot_sv8a_delta_r2_covariate(
    model_r2_data_list = r2_model_list,
    covariate_name = covariate_name,
    dataset_names = available_datasets,
    base_size = base_size
  )

  message("Creating SV8B (delta R² interaction) plot...")
  sv8b <- plot_sv8b_delta_r2_interaction(
    model_r2_data_list = r2_model_list,
    covariate_name = covariate_name,
    dataset_names = available_datasets,
    base_size = base_size
  )

  message("Assembling variance page...")
  combined <- assemble_variance_page(
    sv1 = sv1,
    sv2 = sv2,
    sv7 = sv7,
    sv8a = sv8a,
    sv8b = sv8b,
    covariate_name = covariate_name,
    base_size = base_size + 3
  )

  return(list(
    r2_age_list = r2_age_list,
    r2_cov_list = r2_cov_list,
    r2_model_list = r2_model_list,
    sv1 = sv1,
    sv2 = sv2,
    sv7 = sv7,
    sv8a = sv8a,
    sv8b = sv8b,
    combined = combined
  ))
}


# ==============================================================================
# TEST EXAMPLE (commented out)
# ==============================================================================

# # Test with sex covariate
# if (FALSE) {
#   library(here)
#   library(tidyr)
#   source(here::here("R/paths.R"))
#   source(here::here("R/covariate_sensitivity_functions.R"))
#
#   # Load all datasets
#   datasets <- load_all_datasets()
#
#   # Prepare datasets (drop NAs)
#   datasets_clean <- lapply(datasets, function(df) {
#     df %>% drop_na(srh, age, year, wt, sex, age_group)
#   })
#
#   # Run variance analysis for sex
#   results <- run_variance_analysis(
#     datasets_list = datasets_clean,
#     covariate_var = "sex",
#     covariate_name = "Sex"
#   )
#
#   # View individual plots
#   print(results$sv1$faceted)
#   print(results$sv1$combined)
#   print(results$sv7)
#
#   # Save combined page
#   ggsave(
#     here::here("output", "sensitivity", "sociodemographic", "figures", "sex_variance.png"),
#     results$combined,
#     width = 16, height = 20, dpi = 300
#   )
# }
