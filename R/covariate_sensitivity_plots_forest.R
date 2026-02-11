# ==============================================================================
# covariate_sensitivity_plots_forest.R
# Plotting functions for Forest plot page (Page 6) of covariate sensitivity analyses
#
# Purpose: Visualize covariate coefficients (controlling for age) across years
#          using forest plots
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
# PAGE 6: FOREST PLOT OF COVARIATE COEFFICIENTS BY YEAR
# ==============================================================================

#' Plot forest plot of covariate coefficients controlling for age
#'
#' @description
#' Creates a forest plot with 6 adjacent panels (one per dataset), showing
#' coefficient estimates and 95% CIs for each year. Model: SRH ~ age + covariate
#'
#' @param forest_data_list Named list of data frames (one per dataset), each with
#'   columns: year, level, beta, se, ci_lower, ci_upper, p_value, is_significant, n
#'   (from compute_forest_coefficients)
#' @param covariate_levels Character vector of covariate levels (non-reference)
#' @param reference_level The reference level (not plotted, but shown in legend)
#' @param covariate_colors Named vector of colors for covariate levels
#' @param dataset_names Character vector of dataset names in display order.
#'   Default: c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
#' @param base_size Base font size. Default 10.
#' @param point_size Size of point estimates. Default 2.
#' @param ci_linewidth Linewidth for CI error bars. Default 0.5.
#' @param dodge_width Width for position_dodge when multiple levels. Default 0.6.
#' @param x_limits Optional x-axis limits as c(min, max). Default NULL (auto).
#'
#' @return A ggplot object
#'
plot_forest <- function(
    forest_data_list,
    covariate_levels,
    reference_level,
    covariate_colors,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 10,
    point_size = 2,
    ci_linewidth = 0.5,
    dodge_width = 0.6,
    x_limits = NULL
) {

  # Filter to available datasets and combine
  available_datasets <- intersect(dataset_names, names(forest_data_list))

  all_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      forest_data_list[[ds]] %>%
        mutate(dataset = ds)
    })
  ) %>%
    filter(!is.na(beta), !is.na(ci_lower), !is.na(ci_upper))

  if (nrow(all_data) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data") +
             theme_void())
  }

  # Set factor levels for ordering
  # Year as factor, reverse chronological (most recent at TOP of y-axis)
  all_years <- sort(unique(all_data$year), decreasing = FALSE)  # oldest first
  all_data <- all_data %>%
    mutate(
      dataset = factor(dataset, levels = dataset_names),
      level = factor(level, levels = covariate_levels),
      year = factor(year, levels = all_years)  # oldest at bottom, newest at top
    ) %>%
    filter(!is.na(level))

  # Determine shapes based on significance
  # Shape 19 = filled circle (significant)
  # Shape 21 = open circle (not significant) - uses fill aesthetic
  # We'll use shape aesthetic mapped to is_significant

  # Number of non-reference levels
  n_levels <- length(covariate_levels)
  use_dodge <- n_levels > 1

  # Create plot
  if (use_dodge) {
    # Multiple levels: use position_dodge for spacing within year rows
    p <- ggplot(all_data, aes(x = beta, y = year, color = level)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      geom_errorbarh(
        aes(xmin = ci_lower, xmax = ci_upper),
        height = 0,
        linewidth = ci_linewidth,
        position = position_dodge(width = dodge_width)
      ) +
      geom_point(
        aes(shape = is_significant),
        size = point_size,
        position = position_dodge(width = dodge_width)
      ) +
      scale_shape_manual(
        values = c("TRUE" = 19, "FALSE" = 1),  # 19=filled, 1=open
        name = "p < 0.05",
        labels = c("TRUE" = "Yes", "FALSE" = "No"),
        na.value = 4  # X for NA
      ) +
      scale_color_manual(
        values = covariate_colors,
        name = paste0("Category\n(ref: ", reference_level, ")")
      )
  } else {
    # Single level: no dodge needed
    level_color <- covariate_colors[covariate_levels[1]]

    p <- ggplot(all_data, aes(x = beta, y = year)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      geom_errorbarh(
        aes(xmin = ci_lower, xmax = ci_upper),
        height = 0,
        linewidth = ci_linewidth,
        color = level_color
      ) +
      geom_point(
        aes(shape = is_significant),
        size = point_size,
        color = level_color
      ) +
      scale_shape_manual(
        values = c("TRUE" = 19, "FALSE" = 1),
        name = "p < 0.05",
        labels = c("TRUE" = "Yes", "FALSE" = "No"),
        na.value = 4
      )
  }

  # Add faceting, scales, and theme
  p <- p +
    facet_wrap(~ dataset, nrow = 1, scales = "free_y") +
    labs(
      x = "Coefficient (vs. reference)",
      y = "Year"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray95", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
      panel.spacing = unit(0.5, "lines"),
      strip.text = element_text(size = base_size + 1, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      axis.text.y = element_text(size = base_size - 1),
      axis.text.x = element_text(size = base_size - 1),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    )

  # Apply x-axis limits if specified
  if (!is.null(x_limits)) {
    p <- p + scale_x_continuous(limits = x_limits)
  }

  return(p)
}


# ==============================================================================
# OPTIONAL: ADD TREND LINE TO FOREST PLOT
# ==============================================================================

#' Add meta-regression trend line to forest plot
#'
#' @description
#' Overlays a trend line showing meta-regression of coefficients over time
#' onto an existing forest plot.
#'
#' @param forest_plot A ggplot object from plot_forest()
#' @param trend_data Data frame with columns: dataset, level, slope, intercept
#'   or computed from meta-regression
#' @param trend_color Color for trend line. Default "red".
#' @param trend_linewidth Linewidth for trend line. Default 0.8.
#' @param trend_alpha Alpha for trend line. Default 0.7.
#'
#' @return Modified ggplot object
#'
add_forest_trend_line <- function(
    forest_plot,
    trend_data,
    trend_color = "red",
    trend_linewidth = 0.8,
    trend_alpha = 0.7
) {

  # Extract the data from the plot
  plot_data <- ggplot_build(forest_plot)$data[[1]]

  # Note: Adding trend lines to a forest plot with categorical y-axis (year as factor)
  # is non-trivial. This function provides a basic implementation that can be enhanced.
  #
  # For now, we add a simple annotation showing the trend slope
  # More sophisticated implementations could convert year to numeric and overlay geom_abline

  if (nrow(trend_data) == 0) {
    return(forest_plot)
  }

  # Add trend annotation to each panel
  # This is a simplified approach - for full trend lines, would need to
  # convert year factor to numeric positions

  warning("add_forest_trend_line: Full trend line overlay not yet implemented. ",
          "Consider using separate trend summary table.")

  return(forest_plot)
}


# ==============================================================================
# ASSEMBLY FUNCTION
# ==============================================================================

#' Assemble forest plot page with title
#'
#' @param forest_plot ggplot object from plot_forest()
#' @param covariate_name Display name for the covariate
#' @param base_size Base font size for title. Default 14.
#'
#' @return A ggplot/patchwork object
#'
assemble_forest_page <- function(
    forest_plot,
    covariate_name,
    base_size = 14
) {

  titled_plot <- forest_plot +
    plot_annotation(
      title = paste0("Forest Plot: ", covariate_name, " Effect on SRH (controlling for age)"),
      subtitle = paste0("Coefficient estimates and 95% CIs from SRH ~ age + ", tolower(covariate_name),
                        "\nFilled points: p < 0.05; Open points: p >= 0.05; Dashed line: null effect"),
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
# INTERACTION MODEL: SRH ~ age + covariate * year
# ==============================================================================

#' Compute covariate × year interaction coefficients
#'
#' @description
#' For each dataset, runs survey-weighted regression SRH ~ age + covariate * year
#' and returns coefficients for covariate:year interactions. This shows how the
#' covariate effect changes over time.
#'
#' @param data Data frame with srh, age, year, covariate, weights
#' @param covariate_var Name of the covariate variable (character)
#' @param covariate_levels Character vector of covariate levels
#' @param reference_level The reference level for the covariate
#' @param weight_var Name of weight variable (default "wt")
#' @param min_n Minimum sample size (default 100)
#' @param psu_var Name of PSU variable (NULL if not available)
#' @param strata_var Name of strata variable (NULL if not available)
#'
#' @return Data frame with: level, year_coef, beta, se, ci_lower, ci_upper,
#'   p_value, is_significant, n
#'
compute_interaction_coefficients <- function(
    data,
    covariate_var,
    covariate_levels,
    reference_level,
    weight_var = "wt",
    min_n = 100,
    psu_var = NULL,
    strata_var = NULL
) {

  # Input validation
  stopifnot(is.data.frame(data))
  stopifnot(covariate_var %in% names(data))
  stopifnot("srh" %in% names(data))
  stopifnot("age" %in% names(data))
  stopifnot("year" %in% names(data))
  stopifnot(weight_var %in% names(data))
  stopifnot(reference_level %in% covariate_levels)

  # Source helper functions if needed
  if (!exists("create_survey_design")) {
    source(here::here("R", "covariate_sensitivity_functions.R"))
  }

  # Set survey options
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  non_ref_levels <- setdiff(covariate_levels, reference_level)

  # Filter and prepare data
  data_clean <- data %>%
    filter(
      !is.na(srh),
      !is.na(age),
      !is.na(year),
      !is.na(.data[[covariate_var]]),
      .data[[covariate_var]] %in% covariate_levels,
      !is.na(.data[[weight_var]]),
      .data[[weight_var]] > 0
    )

  # Set factor with reference level
  data_clean[[covariate_var]] <- factor(
    data_clean[[covariate_var]],
    levels = c(reference_level, non_ref_levels)
  )

  n <- nrow(data_clean)

  if (n < min_n) {
    return(data.frame(
      level = character(),
      year_coef = character(),
      beta = numeric(),
      se = numeric(),
      ci_lower = numeric(),
      ci_upper = numeric(),
      p_value = numeric(),
      is_significant = logical(),
      n = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Get unique years and create year factor
  years <- sort(unique(data_clean$year))
  ref_year <- years[1]  # Use first year as reference
  data_clean$year_f <- factor(data_clean$year, levels = years)

  results_list <- list()

  tryCatch({
    svy_design <- create_survey_design(
      data_clean, weight_var, psu_var, strata_var
    )

    # Model: SRH ~ age + covariate * year_f
    formula <- as.formula(paste0("srh ~ age + ", covariate_var, " * year_f"))
    model <- survey::svyglm(formula, design = svy_design, family = gaussian())

    if (is.null(model)) {
      return(data.frame(
        level = character(),
        year_coef = character(),
        beta = numeric(),
        se = numeric(),
        ci_lower = numeric(),
        ci_upper = numeric(),
        p_value = numeric(),
        is_significant = logical(),
        n = integer(),
        stringsAsFactors = FALSE
      ))
    }

    coef_summary <- summary(model)$coefficients
    ci <- confint(model, level = 0.95)

    # Extract interaction terms: covariate_var<level>:year_f<year>
    for (lvl in non_ref_levels) {
      for (yr in years[-1]) {  # Skip reference year
        # Interaction coefficient name
        coef_name <- paste0(covariate_var, lvl, ":year_f", yr)

        if (coef_name %in% rownames(coef_summary)) {
          p_val <- coef_summary[coef_name, "Pr(>|t|)"]

          results_list[[length(results_list) + 1]] <- data.frame(
            level = lvl,
            year_coef = as.character(yr),
            beta = coef_summary[coef_name, "Estimate"],
            se = coef_summary[coef_name, "Std. Error"],
            ci_lower = ci[coef_name, 1],
            ci_upper = ci[coef_name, 2],
            p_value = p_val,
            is_significant = p_val < 0.05,
            n = n,
            stringsAsFactors = FALSE
          )
        }
      }
    }

  }, error = function(e) {
    warning("Interaction model failed: ", e$message)
  })

  if (length(results_list) == 0) {
    return(data.frame(
      level = character(),
      year_coef = character(),
      beta = numeric(),
      se = numeric(),
      ci_lower = numeric(),
      ci_upper = numeric(),
      p_value = numeric(),
      is_significant = logical(),
      n = integer(),
      stringsAsFactors = FALSE
    ))
  }

  results <- bind_rows(results_list)
  return(results)
}


#' Plot forest plot of covariate × year interaction coefficients
#'
#' @description
#' Creates a forest plot showing how covariate effects change over time,
#' with separate panels per dataset and columns per covariate level.
#' Based on model: SRH ~ age + covariate * year
#'
#' @param interaction_data_list Named list of data frames (one per dataset),
#'   each from compute_interaction_coefficients()
#' @param covariate_levels Character vector of covariate levels (non-reference)
#' @param reference_level The reference level
#' @param covariate_colors Named vector of colors for covariate levels
#' @param dataset_names Character vector of dataset names
#' @param base_size Base font size. Default 10.
#' @param point_size Size of point estimates. Default 2.
#' @param ci_linewidth Linewidth for CI error bars. Default 0.5.
#' @param x_limits Optional x-axis limits. Default NULL.
#'
#' @return A ggplot object
#'
plot_interaction_forest <- function(
    interaction_data_list,
    covariate_levels,
    reference_level,
    covariate_colors,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    base_size = 10,
    point_size = 2,
    ci_linewidth = 0.5,
    x_limits = NULL
) {

  # Filter to available datasets and combine
  available_datasets <- intersect(dataset_names, names(interaction_data_list))

  all_data <- bind_rows(
    lapply(available_datasets, function(ds) {
      df <- interaction_data_list[[ds]]
      if (nrow(df) > 0) {
        df %>% mutate(dataset = ds)
      } else {
        NULL
      }
    })
  )

  if (is.null(all_data) || nrow(all_data) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data") +
             theme_void())
  }

  all_data <- all_data %>%
    filter(!is.na(beta), !is.na(ci_lower), !is.na(ci_upper))

  if (nrow(all_data) == 0) {
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No valid coefficients") +
             theme_void())
  }

  # Set factor levels
  all_years <- sort(unique(as.numeric(all_data$year_coef)), decreasing = FALSE)
  all_data <- all_data %>%
    mutate(
      dataset = factor(dataset, levels = dataset_names),
      level = factor(level, levels = covariate_levels),
      year_coef = factor(year_coef, levels = as.character(all_years))
    ) %>%
    filter(!is.na(level))

  # Create plot with facet_grid: rows = datasets, columns = covariate levels
  p <- ggplot(all_data, aes(x = beta, y = year_coef)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper, color = level),
      height = 0,
      linewidth = ci_linewidth
    ) +
    geom_point(
      aes(shape = is_significant, color = level),
      size = point_size
    ) +
    facet_grid(dataset ~ level, scales = "free") +
    scale_shape_manual(
      values = c("TRUE" = 19, "FALSE" = 1),
      name = "p < 0.05",
      labels = c("TRUE" = "Yes", "FALSE" = "No"),
      na.value = 4
    ) +
    scale_color_manual(
      values = covariate_colors,
      name = "Category",
      guide = "none"  # Hide color legend since it's in facet labels
    ) +
    labs(
      x = paste0("Interaction coefficient (vs. ", reference_level, " × reference year)"),
      y = "Year"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray95", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
      panel.spacing = unit(0.5, "lines"),
      strip.text = element_text(size = base_size, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      axis.text.y = element_text(size = base_size - 2),
      axis.text.x = element_text(size = base_size - 1),
      legend.position = "bottom",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      plot.margin = margin(5, 5, 5, 5)
    )

  if (!is.null(x_limits)) {
    p <- p + scale_x_continuous(limits = x_limits)
  }

  return(p)
}


#' Assemble interaction forest plot page with title
#'
#' @param interaction_plot ggplot object from plot_interaction_forest()
#' @param covariate_name Display name for the covariate
#' @param base_size Base font size for title. Default 14.
#'
#' @return A ggplot/patchwork object
#'
assemble_interaction_forest_page <- function(
    interaction_plot,
    covariate_name,
    base_size = 14
) {

  titled_plot <- interaction_plot +
    plot_annotation(
      title = paste0("Interaction Forest Plot: ", covariate_name, " × Year Effect on SRH"),
      subtitle = paste0("Interaction coefficients from SRH ~ age + ", tolower(covariate_name),
                        " × year\nShows how ", tolower(covariate_name),
                        " effect changes relative to reference year"),
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
# CONVENIENCE FUNCTION: RUN COMPLETE FOREST ANALYSIS
# ==============================================================================

#' Run complete forest plot analysis
#'
#' @description
#' Convenience function that computes forest coefficients and generates
#' both the main forest plot and interaction forest plot.
#'
#' @param datasets_list Named list of 6 data frames (one per dataset)
#' @param covariate_var Name of the covariate variable
#' @param covariate_levels Character vector of all covariate levels
#' @param reference_level The reference level for the covariate
#' @param covariate_colors Named vector of colors for covariate levels
#' @param covariate_name Display name for the covariate (for titles)
#' @param dataset_names Character vector of dataset names in display order
#' @param weight_var Name of weight variable. Default "wt".
#' @param base_size Base font size. Default 10.
#' @param include_interaction Whether to compute and plot interaction model. Default TRUE.
#'
#' @return A list with:
#'   - forest_data_list: Forest coefficient data by dataset
#'   - forest_plot: Page 6 plot (forest plot)
#'   - forest_page: Page 6 with title
#'   - interaction_data_list: Interaction coefficient data by dataset (if include_interaction)
#'   - interaction_plot: Interaction forest plot (if include_interaction)
#'   - interaction_page: Interaction plot with title (if include_interaction)
#'
run_forest_analysis <- function(
    datasets_list,
    covariate_var,
    covariate_levels,
    reference_level,
    covariate_colors,
    covariate_name,
    dataset_names = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
    weight_var = "wt",
    base_size = 10,
    include_interaction = TRUE
) {

  # Source computation functions if not loaded
  if (!exists("compute_forest_coefficients")) {
    source(here::here("R", "covariate_sensitivity_functions.R"))
  }

  available_datasets <- intersect(dataset_names, names(datasets_list))
  non_ref_levels <- setdiff(covariate_levels, reference_level)

  # Compute forest coefficients for each dataset
  message("Computing forest coefficients (SRH ~ age + covariate)...")
  forest_data_list <- lapply(available_datasets, function(dataset) {
    message("  ", dataset, "...")
    compute_forest_coefficients(
      data = datasets_list[[dataset]],
      covariate_var = covariate_var,
      covariate_levels = covariate_levels,
      reference_level = reference_level,
      weight_var = weight_var
    )
  })
  names(forest_data_list) <- available_datasets

  # Create main forest plot
  message("Creating forest plot (Page 6)...")
  forest_plot <- plot_forest(
    forest_data_list = forest_data_list,
    covariate_levels = non_ref_levels,
    reference_level = reference_level,
    covariate_colors = covariate_colors,
    dataset_names = available_datasets,
    base_size = base_size
  )

  forest_page <- assemble_forest_page(
    forest_plot = forest_plot,
    covariate_name = covariate_name,
    base_size = base_size + 2
  )

  result <- list(
    forest_data_list = forest_data_list,
    forest_plot = forest_plot,
    forest_page = forest_page
  )

  # Optionally compute interaction model
  if (include_interaction) {
    message("Computing interaction coefficients (SRH ~ age + covariate * year)...")
    interaction_data_list <- lapply(available_datasets, function(dataset) {
      message("  ", dataset, "...")
      tryCatch({
        compute_interaction_coefficients(
          data = datasets_list[[dataset]],
          covariate_var = covariate_var,
          covariate_levels = covariate_levels,
          reference_level = reference_level,
          weight_var = weight_var
        )
      }, error = function(e) {
        warning("Interaction model failed for ", dataset, ": ", e$message)
        data.frame()
      })
    })
    names(interaction_data_list) <- available_datasets

    # Check if any data was returned
    has_interaction_data <- any(sapply(interaction_data_list, nrow) > 0)

    if (has_interaction_data) {
      message("Creating interaction forest plot...")
      interaction_plot <- plot_interaction_forest(
        interaction_data_list = interaction_data_list,
        covariate_levels = non_ref_levels,
        reference_level = reference_level,
        covariate_colors = covariate_colors,
        dataset_names = available_datasets,
        base_size = base_size
      )

      interaction_page <- assemble_interaction_forest_page(
        interaction_plot = interaction_plot,
        covariate_name = covariate_name,
        base_size = base_size + 2
      )

      result$interaction_data_list <- interaction_data_list
      result$interaction_plot <- interaction_plot
      result$interaction_page <- interaction_page
    } else {
      message("No interaction data available.")
      result$interaction_data_list <- interaction_data_list
      result$interaction_plot <- NULL
      result$interaction_page <- NULL
    }
  }

  return(result)
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
#     df %>% drop_na(srh, age, year, wt, sex)
#   })
#
#   # Define sex colors
#   sex_colors <- c(
#     "Male" = "#0072B2",
#     "Female" = "#D55E00"
#   )
#
#   # Run forest analysis for sex
#   results <- run_forest_analysis(
#     datasets_list = datasets_clean,
#     covariate_var = "sex",
#     covariate_levels = c("Male", "Female"),
#     reference_level = "Male",
#     covariate_colors = sex_colors,
#     covariate_name = "Sex",
#     include_interaction = FALSE  # Interaction model is computationally intensive
#   )
#
#   # View plot
#   print(results$forest_page)
#
#   # Save figure
#   ggsave(
#     here::here("output", "sensitivity", "sociodemographic", "figures", "sex_forest.png"),
#     results$forest_page,
#     width = 16, height = 10, dpi = 300
#   )
# }
