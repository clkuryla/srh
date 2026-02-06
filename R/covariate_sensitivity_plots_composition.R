# ==============================================================================
# covariate_sensitivity_plots_composition.R
# Plotting functions for Composition pages (Pages 4 & 5) of covariate sensitivity analyses
#
# Purpose: Visualize covariate composition by age group over time (Page 4)
#          and SRH distribution within covariate strata (Page 5)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(here)

# Source theme for colors and styling
source(here::here("R", "functions", "theme_srh.R"))

# ==============================================================================
# PAGE 4: COVARIATE COMPOSITION BY AGE GROUP
# ==============================================================================

#' Plot covariate composition over time by dataset and age group
#'
#' @description
#' Creates a 6 rows (datasets) × 7 columns (age groups) figure where each panel
#' shows a stacked area chart of covariate proportions over time.
#'
#' @param comp_data_list Named list of data frames (one per dataset), each with
#'   columns: year, age_group, level, proportion, n (from compute_composition)
#' @param covariate_levels Character vector of covariate levels in desired stack order
#'   (bottom to top)
#' @param covariate_colors Named vector of colors for covariate levels
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param age_groups Character vector of age group names in display order.
#'   Default: c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")
#' @param base_size Base font size. Default 9 (smaller for many panels).
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#'
#' @return A ggplot object
#'
plot_covariate_composition <- function(
    comp_data_list,
    covariate_levels,
    covariate_colors,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    age_groups = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
    base_size = 9,
    tilt_x_labels = 45
) {

  # Filter to available datasets and combine
  available_datasets <- intersect(dataset_names, names(comp_data_list))

  all_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      comp_data_list[[ds]] %>%
        mutate(
          dataset = ds,
          # Convert age_group to character to avoid factor type conflicts
          age_group = as.character(age_group)
        )
    })
  ) %>%
    filter(!is.na(proportion))

  if (nrow(all_data) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data") +
             theme_void())
  }

  # Set factor levels for ordering
  all_data <- all_data %>%
    mutate(
      dataset = factor(dataset, levels = dataset_names),
      age_group = factor(age_group, levels = age_groups),
      level = factor(level, levels = covariate_levels)
    ) %>%
    filter(!is.na(level), !is.na(age_group))

  # Fill in missing level combinations for each year × age_group × dataset

  # Get all years per dataset for proper completion
  years_by_dataset <- all_data %>%
    group_by(dataset) %>%
    summarise(years = list(sort(unique(year))), .groups = "drop")

  # Complete within each dataset-age_group combination
  all_data <- all_data %>%
    left_join(years_by_dataset, by = "dataset") %>%
    group_by(dataset, age_group) %>%
    complete(
      year = unlist(years[1]),  # Use all years for this dataset
      level = factor(covariate_levels, levels = covariate_levels),
      fill = list(proportion = 0, n = 0)
    ) %>%
    ungroup() %>%
    select(-years) %>%
    # Ensure proper sorting for geom_area
    arrange(dataset, age_group, level, year) %>%
    # Filter any remaining NAs in key columns
    filter(!is.na(proportion), !is.na(year), !is.na(level))

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Compute cumulative proportions explicitly for geom_ribbon
  # This avoids rendering artifacts from geom_area position="stack"
  all_data <- all_data %>%
    arrange(dataset, age_group, year, level) %>%
    group_by(dataset, age_group, year) %>%
    mutate(
      ymax = cumsum(proportion),
      ymin = lag(ymax, default = 0)
    ) %>%
    ungroup()

  # Create plot with 6 rows (datasets) x 7 columns (age groups)
  p <- ggplot(all_data, aes(x = year, fill = level)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.9, color = NA) +
    facet_grid(dataset ~ age_group, scales = "free_x") +
    scale_fill_manual(values = covariate_colors, name = "Category") +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    labs(
      x = "Year",
      y = "Proportion"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = base_size, face = "bold"),
      strip.text.y = element_text(size = base_size, face = "bold", angle = 0),
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      axis.text.x = x_tick_style,
      axis.text.y = element_text(size = base_size - 2),
      legend.position = "bottom",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    guides(fill = guide_legend(nrow = 1))

  return(p)
}


# ==============================================================================
# PAGE 5: SRH COMPOSITION BY COVARIATE STRATUM
# ==============================================================================

#' Plot SRH distribution within each covariate stratum
#'
#' @description
#' Creates an N rows (covariate categories) × 6 columns (datasets) figure where
#' each panel shows a stacked area chart of SRH proportions over time.
#'
#' @param srh_comp_data_list Named list of data frames (one per dataset), each with
#'   columns: year, stratum, srh_level, proportion, n (from compute_srh_composition)
#' @param covariate_levels Character vector of covariate levels in desired row order
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 9.
#' @param tilt_x_labels Angle to tilt x-axis labels. Default 45.
#' @param srh_palette Which SRH color palette to use: "pastel" or "dark". Default "pastel".
#' @param reverse_srh_order If TRUE, Poor at top; if FALSE, Excellent at top. Default FALSE.
#'
#' @return A ggplot object
#'
plot_srh_composition <- function(
    srh_comp_data_list,
    covariate_levels,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 9,
    tilt_x_labels = 45,
    srh_palette = "pastel",
    reverse_srh_order = FALSE
) {

  # Filter to available datasets and combine
  available_datasets <- intersect(dataset_names, names(srh_comp_data_list))

  all_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      srh_comp_data_list[[ds]] %>%
        mutate(dataset = ds)
    })
  ) %>%
    filter(!is.na(proportion))

  if (nrow(all_data) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data") +
             theme_void())
  }

  # Define SRH levels (numeric 1-5, with labels)
  # Determine if GSS is present (4-point scale)
  has_gss <- "GSS" %in% available_datasets

  # Get SRH colors based on palette
  if (srh_palette == "dark") {
    srh_colors_5pt <- srh_colors_dark
    srh_colors_4pt <- srh_colors_dark_gss
  } else {
    srh_colors_5pt <- srh_colors_pastel
    srh_colors_4pt <- srh_colors_pastel_gss
  }

  # Create SRH level factor (numeric values map to labels)
  # 5-point scale: 1=Poor, 2=Fair, 3=Good, 4=Very Good, 5=Excellent
  srh_label_5pt <- c("1" = "Poor", "2" = "Fair", "3" = "Good",
                     "4" = "Very Good", "5" = "Excellent")
  # 4-point scale (GSS): 1=Poor, 2=Fair, 3=Good, 4=Excellent
  srh_label_4pt <- c("1" = "Poor", "2" = "Fair", "3" = "Good", "4" = "Excellent")

  # Map numeric SRH to labels
  all_data <- all_data %>%
    mutate(
      srh_label = case_when(
        dataset == "GSS" ~ srh_label_4pt[as.character(srh_level)],
        TRUE ~ srh_label_5pt[as.character(srh_level)]
      )
    ) %>%
    filter(!is.na(srh_label))

  # Define factor order for SRH (stacking order: bottom to top)
  # reverse_srh_order=FALSE: Poor at bottom, Excellent at top
  # reverse_srh_order=TRUE: Excellent at bottom, Poor at top
  srh_level_order <- if (reverse_srh_order) {
    c("Excellent", "Very Good", "Good", "Fair", "Poor")
  } else {
    c("Poor", "Fair", "Good", "Very Good", "Excellent")
  }

  all_data <- all_data %>%
    mutate(
      dataset = factor(dataset, levels = dataset_names),
      stratum = factor(stratum, levels = covariate_levels),
      srh_label = factor(srh_label, levels = srh_level_order)
    ) %>%
    filter(!is.na(stratum))

  # Get all years per dataset for proper completion
  years_by_dataset <- all_data %>%
    group_by(dataset) %>%
    summarise(years = list(sort(unique(year))), .groups = "drop")

  # Fill in missing srh_label combinations for each year × stratum × dataset
  all_data <- all_data %>%
    left_join(years_by_dataset, by = "dataset") %>%
    group_by(dataset, stratum) %>%
    complete(
      year = unlist(years[1]),
      srh_label = factor(srh_level_order, levels = srh_level_order),
      fill = list(proportion = 0, n = 0)
    ) %>%
    ungroup() %>%
    select(-years) %>%
    arrange(dataset, stratum, year, srh_label) %>%
    filter(!is.na(proportion), !is.na(year), !is.na(srh_label))

  # Compute cumulative proportions explicitly for geom_ribbon
  all_data <- all_data %>%
    group_by(dataset, stratum, year) %>%
    mutate(
      ymax = cumsum(proportion),
      ymin = lag(ymax, default = 0)
    ) %>%
    ungroup()

  # Combined color palette (5-point scale covers all categories)
  combined_colors <- srh_colors_5pt

  # X-axis tick styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  # Create plot with N rows (covariate strata) x 6 columns (datasets)
  p <- ggplot(all_data, aes(x = year, fill = srh_label)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.9, color = NA) +
    facet_grid(stratum ~ dataset, scales = "free_x") +
    scale_fill_manual(values = combined_colors, name = "SRH", drop = FALSE) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    labs(
      x = "Year",
      y = "Proportion"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = base_size, face = "bold"),
      strip.text.y = element_text(size = base_size, face = "bold", angle = 0),
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      axis.text.x = x_tick_style,
      axis.text.y = element_text(size = base_size - 2),
      legend.position = "bottom",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    guides(fill = guide_legend(nrow = 1))

  return(p)
}


# ==============================================================================
# ASSEMBLY FUNCTIONS
# ==============================================================================

#' Assemble covariate composition page with title
#'
#' @param comp_plot ggplot object from plot_covariate_composition()
#' @param covariate_name Display name for the covariate
#' @param base_size Base font size for title. Default 14.
#'
#' @return A ggplot/patchwork object
#'
assemble_covariate_composition_page <- function(
    comp_plot,
    covariate_name,
    base_size = 14
) {

  # Add title using patchwork
  titled_plot <- comp_plot +
    plot_annotation(
      title = paste0("Covariate Composition: ", covariate_name),
      subtitle = "Proportion in each category by dataset and age group over time",
      theme = theme(
        plot.title = element_text(
          size = base_size + 2,
          face = "bold",
          hjust = 0.5,
          margin = margin(b = 4)
        ),
        plot.subtitle = element_text(
          size = base_size,
          hjust = 0.5,
          color = "gray40",
          margin = margin(b = 8)
        ),
        plot.margin = margin(10, 10, 10, 10)
      )
    )

  return(titled_plot)
}


#' Assemble SRH composition page with title
#'
#' @param srh_comp_plot ggplot object from plot_srh_composition()
#' @param covariate_name Display name for the covariate
#' @param base_size Base font size for title. Default 14.
#'
#' @return A ggplot/patchwork object
#'
assemble_srh_composition_page <- function(
    srh_comp_plot,
    covariate_name,
    base_size = 14
) {

  # Add title using patchwork
  titled_plot <- srh_comp_plot +
    plot_annotation(
      title = paste0("SRH Distribution by ", covariate_name),
      subtitle = "Proportion in each SRH category by stratum and dataset over time",
      theme = theme(
        plot.title = element_text(
          size = base_size + 2,
          face = "bold",
          hjust = 0.5,
          margin = margin(b = 4)
        ),
        plot.subtitle = element_text(
          size = base_size,
          hjust = 0.5,
          color = "gray40",
          margin = margin(b = 8)
        ),
        plot.margin = margin(10, 10, 10, 10)
      )
    )

  return(titled_plot)
}


# ==============================================================================
# CONVENIENCE FUNCTION: RUN COMPLETE COMPOSITION ANALYSIS
# ==============================================================================

#' Run complete composition analysis and generate plots
#'
#' @description
#' Convenience function that computes composition statistics and generates
#' both composition page plots for a given covariate.
#'
#' @param datasets_list Named list of 6 data frames (one per dataset)
#' @param covariate_var Name of the covariate variable
#' @param covariate_levels Character vector of covariate levels
#' @param covariate_colors Named vector of colors for covariate levels
#' @param covariate_name Display name for the covariate (for titles)
#' @param dataset_names Character vector of dataset names in display order
#' @param age_groups Character vector of age group names
#' @param age_var Name of the age group variable in the data. Default "age_group".
#' @param weight_var Name of weight variable. Default "wt".
#' @param base_size Base font size. Default 9.
#'
#' @return A list with:
#'   - comp_data_list: Covariate composition data by dataset
#'   - srh_comp_data_list: SRH composition data by dataset
#'   - covariate_comp_plot: Page 4 plot (covariate composition)
#'   - srh_comp_plot: Page 5 plot (SRH composition)
#'   - covariate_comp_page: Page 4 with title
#'   - srh_comp_page: Page 5 with title
#'
run_composition_analysis <- function(
    datasets_list,
    covariate_var,
    covariate_levels,
    covariate_colors,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    age_groups = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
    age_var = "age_group",
    weight_var = "wt",
    base_size = 9
) {

  # Source computation functions if not loaded
  if (!exists("compute_composition")) {
    source(here::here("R", "covariate_sensitivity_functions.R"))
  }

  available_datasets <- intersect(dataset_names, names(datasets_list))

  # Compute covariate composition for each dataset
  message("Computing covariate composition...")
  comp_data_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_composition(
      data = datasets_list[[dataset]],
      covariate_var = covariate_var,
      covariate_levels = covariate_levels,
      age_var = age_var,
      weight_var = weight_var
    )
  })
  names(comp_data_list) <- available_datasets

  # Compute SRH composition for each dataset
  message("Computing SRH composition...")
  srh_comp_data_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_srh_composition(
      data = datasets_list[[dataset]],
      covariate_var = covariate_var,
      covariate_levels = covariate_levels,
      weight_var = weight_var
    )
  })
  names(srh_comp_data_list) <- available_datasets

  # Create plots
  message("Creating covariate composition plot (Page 4)...")
  covariate_comp_plot <- plot_covariate_composition(
    comp_data_list = comp_data_list,
    covariate_levels = covariate_levels,
    covariate_colors = covariate_colors,
    dataset_names = available_datasets,
    age_groups = age_groups,
    base_size = base_size
  )

  message("Creating SRH composition plot (Page 5)...")
  srh_comp_plot <- plot_srh_composition(
    srh_comp_data_list = srh_comp_data_list,
    covariate_levels = covariate_levels,
    dataset_names = available_datasets,
    base_size = base_size
  )

  # Assemble pages with titles
  message("Assembling composition pages...")
  covariate_comp_page <- assemble_covariate_composition_page(
    comp_plot = covariate_comp_plot,
    covariate_name = covariate_name,
    base_size = base_size + 3
  )

  srh_comp_page <- assemble_srh_composition_page(
    srh_comp_plot = srh_comp_plot,
    covariate_name = covariate_name,
    base_size = base_size + 3
  )

  return(list(
    comp_data_list = comp_data_list,
    srh_comp_data_list = srh_comp_data_list,
    covariate_comp_plot = covariate_comp_plot,
    srh_comp_plot = srh_comp_plot,
    covariate_comp_page = covariate_comp_page,
    srh_comp_page = srh_comp_page
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
#   # Define sex colors
#   sex_colors <- c(
#     "Male" = "#0072B2",
#     "Female" = "#D55E00"
#   )
#
#   # Run composition analysis for sex
#   results <- run_composition_analysis(
#     datasets_list = datasets_clean,
#     covariate_var = "sex",
#     covariate_levels = c("Male", "Female"),
#     covariate_colors = sex_colors,
#     covariate_name = "Sex"
#   )
#
#   # View plots
#   print(results$covariate_comp_page)
#   print(results$srh_comp_page)
#
#   # Save figures
#   ggsave(
#     here::here("output", "sensitivity", "sociodemographic", "figures", "sex_composition.png"),
#     results$covariate_comp_page,
#     width = 16, height = 12, dpi = 300
#   )
#
#   ggsave(
#     here::here("output", "sensitivity", "sociodemographic", "figures", "sex_srh_composition.png"),
#     results$srh_comp_page,
#     width = 14, height = 6, dpi = 300
#   )
# }
