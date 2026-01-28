# ==============================================================================
# bhapc_figure_generation.R
# Figure generation for BHAPC analysis results
# Based on Gloria Graf's Figure 2 and Figure 3 formats
# Author: Christine Lucille Kuryla
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(srvyr)

# Source theme if not already loaded
# source(here::here("R", "functions", "theme_srh.R"))

# ==============================================================================
# Figure 2: Descriptive - Survey-weighted mean SRH by age and period
# ==============================================================================

#' Create Figure 2: Descriptive means by age and period
#'
#' Creates a line plot showing survey-weighted mean SRH by age group
#' for each period.
#'
#' @param bhapc_df Data frame from prepare_bhapc_data()
#' @param survey Character: survey name for title
#' @param output_path Optional path to save figure
#' @param width Figure width in inches (default 10)
#' @param height Figure height in inches (default 7)
#' @return ggplot object
#'
#' @details
#' Uses survey weights if available (wt column).
#' X-axis: Age groups
#' Y-axis: Mean SRH
#' Lines: One per period, colored by period
create_figure2_descriptive <- function(bhapc_df,
                                        survey = "nhanes",
                                        output_path = NULL,
                                        width = 10,
                                        height = 7) {

  # Check for weight column
  wt_col <- intersect(c("wt", "weight", "WTMEC4YR", "WTMEC2YR"), names(bhapc_df))
  has_weights <- length(wt_col) > 0

  if (has_weights) {
    wt_col <- wt_col[1]
    message("Computing survey-weighted means using: ", wt_col)

    # Create survey design
    svy_df <- bhapc_df %>%
      as_survey_design(weights = !!sym(wt_col))

    # Compute weighted means
    means_df <- svy_df %>%
      group_by(age_group, period_4yr) %>%
      summarise(
        mean_srh = survey_mean(srh, na.rm = TRUE, vartype = "se"),
        n = unweighted(n()),
        .groups = "drop"
      )
  } else {
    message("No weights found; computing unweighted means")

    means_df <- bhapc_df %>%
      group_by(age_group, period_4yr) %>%
      summarise(
        mean_srh = mean(srh, na.rm = TRUE),
        mean_srh_se = sd(srh, na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      )
  }

  # Create color palette for periods
  periods <- sort(unique(means_df$period_4yr))
  n_periods <- length(periods)

  # Use a sequential color palette
  period_colors <- scales::viridis_pal(option = "D", direction = -1)(n_periods)
  names(period_colors) <- periods

  # Create plot
  p <- ggplot(means_df, aes(x = age_group, y = mean_srh,
                             color = period_4yr, group = period_4yr)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = mean_srh - 1.96 * mean_srh_se,
          ymax = mean_srh + 1.96 * mean_srh_se),
      width = 0.2, alpha = 0.5
    ) +
    scale_color_manual(values = period_colors, name = "Period") +
    labs(
      title = paste0("Mean Self-Rated Health by Age and Period (", toupper(survey), ")"),
      subtitle = "Survey-weighted means with 95% confidence intervals",
      x = "Age Group",
      y = "Mean SRH (1=Poor, 5=Excellent)",
      caption = paste0("N = ", format(sum(means_df$n), big.mark = ","), " observations")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  # Save if path provided
  if (!is.null(output_path)) {
    ggsave(output_path, p, width = width, height = height, dpi = 300)
    message("Saved Figure 2 to: ", output_path)
  }

  p
}


# ==============================================================================
# Figure 3: APC Effects from BHAPC Model
# ==============================================================================

#' Create Figure 3: Age, Period, and Cohort effects
#'
#' Creates a three-panel plot showing estimated APC effects from the BHAPC model:
#' - Panel A: Age effect (quadratic curve with 90% CI)
#' - Panel B: Period random effects with 90% CI
#' - Panel C: Cohort random effects with 90% CI
#'
#' @param model rstanarm model object from fit_bhapc_model()
#' @param bhapc_df Data frame used to fit model
#' @param survey Character: survey name for title
#' @param output_path Optional path to save figure
#' @param width Figure width in inches (default 12)
#' @param height Figure height in inches (default 10)
#' @return patchwork object with three panels
create_figure3_apc_effects <- function(model,
                                        bhapc_df,
                                        survey = "nhanes",
                                        output_path = NULL,
                                        width = 12,
                                        height = 10) {

  # Source helper functions
  source(here::here("R", "functions", "bhapc_model_fitting.R"))

  # Panel A: Age Effect
  age_effect <- compute_age_effect(model, bhapc_df)
  p_age <- create_age_effect_panel(age_effect, survey)

  # Panel B & C: Period and Cohort Effects
  random_effects <- extract_random_effects(model, bhapc_df)
  p_period <- create_period_effect_panel(random_effects$period_effects, survey)
  p_cohort <- create_cohort_effect_panel(random_effects$cohort_effects, survey)

  # Combine panels
  combined <- (p_age | p_period | p_cohort) +
    plot_annotation(
      title = paste0("Age, Period, and Cohort Effects on SRH (", toupper(survey), ")"),
      subtitle = "From Bayesian HAPC model with 90% credible intervals",
      tag_levels = "A",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40")
      )
    )

  # Save if path provided
  if (!is.null(output_path)) {
    ggsave(output_path, combined, width = width, height = height, dpi = 300)
    message("Saved Figure 3 to: ", output_path)
  }

  combined
}


#' Create age effect panel
#'
#' @param age_effect Data frame from compute_age_effect()
#' @param survey Survey name
#' @return ggplot object
create_age_effect_panel <- function(age_effect, survey = "nhanes") {

  # Use centered effects for cleaner visualization
  ggplot(age_effect, aes(x = age)) +
    geom_ribbon(
      aes(ymin = ci_lower_centered, ymax = ci_upper_centered),
      fill = "#0072B2", alpha = 0.2
    ) +
    geom_line(aes(y = estimate_centered), color = "#0072B2", linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Age Effect",
      x = "Age (years)",
      y = "Effect on SRH\n(relative to age 18)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold")
    )
}


#' Create period effect panel
#'
#' @param period_effects Data frame from extract_random_effects()
#' @param survey Survey name
#' @return ggplot object
create_period_effect_panel <- function(period_effects, survey = "nhanes") {

  ggplot(period_effects, aes(x = period, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_90, ymax = ci_upper_90),
      width = 0.8, color = "#009E73", linewidth = 0.8
    ) +
    geom_point(color = "#009E73", size = 3) +
    labs(
      title = "Period Effect",
      x = "Period (start year)",
      y = "Random effect\n(SRH units)"
    ) +
    scale_x_continuous(
      breaks = unique(period_effects$period),
      labels = as.character(unique(period_effects$period))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


#' Create cohort effect panel
#'
#' @param cohort_effects Data frame from extract_random_effects()
#' @param survey Survey name
#' @return ggplot object
create_cohort_effect_panel <- function(cohort_effects, survey = "nhanes") {

  ggplot(cohort_effects, aes(x = cohort, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = ci_lower_90, ymax = ci_upper_90),
      width = 2, color = "#CC79A7", linewidth = 0.6, alpha = 0.7
    ) +
    geom_point(color = "#CC79A7", size = 2) +
    labs(
      title = "Cohort Effect",
      x = "Birth cohort (start year)",
      y = "Random effect\n(SRH units)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


# ==============================================================================
# Additional Visualization Functions
# ==============================================================================

#' Create variance decomposition bar chart
#'
#' Visualizes the relative contribution of age, period, and cohort to total variance
#'
#' @param variance_df Data frame from extract_variance_components()
#' @param survey Survey name
#' @return ggplot object
create_variance_chart <- function(variance_df, survey = "nhanes") {

  # Filter to main components (exclude Total and strata/psu)
  plot_df <- variance_df %>%
    filter(component %in% c("period_4yr", "cohort_4yr", "Residual")) %>%
    mutate(
      component = case_when(
        component == "period_4yr" ~ "Period",
        component == "cohort_4yr" ~ "Cohort",
        component == "Residual" ~ "Residual",
        TRUE ~ component
      ),
      component = factor(component, levels = c("Period", "Cohort", "Residual"))
    )

  # Define colors
  apc_colors <- c(
    "Period" = "#009E73",
    "Cohort" = "#CC79A7",
    "Residual" = "gray60"
  )

  ggplot(plot_df, aes(x = component, y = pct_of_total, fill = component)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = paste0(round(pct_of_total, 1), "%")),
      vjust = -0.5, size = 4
    ) +
    scale_fill_manual(values = apc_colors) +
    labs(
      title = paste0("Variance Decomposition (", toupper(survey), ")"),
      x = "",
      y = "% of Total Variance"
    ) +
    ylim(0, 100) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
}


#' Create diagnostic plot for BHAPC model
#'
#' Creates posterior predictive check plot
#'
#' @param model rstanarm model object
#' @param n_draws Number of posterior draws to show
#' @return ggplot object
create_posterior_check <- function(model, n_draws = 50) {

  if (!requireNamespace("bayesplot", quietly = TRUE)) {
    message("bayesplot package required for posterior predictive checks")
    return(NULL)
  }

  bayesplot::pp_check(model, ndraws = n_draws) +
    labs(
      title = "Posterior Predictive Check",
      subtitle = paste0(n_draws, " posterior draws (thin lines) vs. observed data (thick line)")
    )
}


#' Create multi-survey APC comparison figure
#'
#' Creates a faceted plot comparing APC effects across surveys
#'
#' @param results_list Named list of model results from fit_bhapc_model()
#' @param bhapc_dfs Named list of data frames used to fit models
#' @param effect_type Character: "age", "period", or "cohort"
#' @param output_path Optional path to save figure
#' @return ggplot object
create_multi_survey_comparison <- function(results_list,
                                            bhapc_dfs,
                                            effect_type = "age",
                                            output_path = NULL) {

  # Extract effects from each model
  all_effects <- lapply(names(results_list), function(survey) {

    model <- results_list[[survey]]$model
    bhapc_df <- bhapc_dfs[[survey]]

    if (is.null(model)) return(NULL)

    if (effect_type == "age") {
      effect <- compute_age_effect(model, bhapc_df)
      effect$survey <- toupper(survey)
      effect
    } else {
      re <- extract_random_effects(model, bhapc_df)
      if (effect_type == "period") {
        effect <- re$period_effects
        effect$x <- effect$period
      } else {
        effect <- re$cohort_effects
        effect$x <- effect$cohort
      }
      effect$survey <- toupper(survey)
      effect
    }
  })

  combined_df <- bind_rows(all_effects)

  # Create plot based on effect type
  if (effect_type == "age") {
    p <- ggplot(combined_df, aes(x = age, y = estimate_centered, color = survey)) +
      geom_line(linewidth = 1) +
      geom_ribbon(
        aes(ymin = ci_lower_centered, ymax = ci_upper_centered, fill = survey),
        alpha = 0.1, color = NA
      ) +
      labs(
        title = "Age Effects Across Surveys",
        x = "Age (years)",
        y = "Effect on SRH (relative to age 18)"
      )
  } else {
    p <- ggplot(combined_df, aes(x = x, y = estimate, color = survey)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbar(
        aes(ymin = ci_lower_90, ymax = ci_upper_90),
        width = 1, position = position_dodge(width = 2)
      ) +
      geom_point(size = 2, position = position_dodge(width = 2)) +
      labs(
        title = paste0(tools::toTitleCase(effect_type), " Effects Across Surveys"),
        x = paste0(tools::toTitleCase(effect_type), " (start year)"),
        y = "Random effect (SRH units)"
      )
  }

  p <- p +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  # Save if path provided
  if (!is.null(output_path)) {
    ggsave(output_path, p, width = 10, height = 7, dpi = 300)
    message("Saved comparison figure to: ", output_path)
  }

  p
}
