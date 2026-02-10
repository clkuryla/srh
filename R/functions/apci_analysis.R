# ==============================================================================
# apci_analysis.R
# APCI (Age-Period-Cohort-Interaction) Analysis Functions
# Author: Christine Lucille Kuryla
#
# Purpose: Helper functions for running the APCI model (Luo & Hodges, 2020)
# across all 6 SRH surveys. APCI conceptualizes cohort effects as
# age-by-period interactions, providing formal hypothesis tests for
# whether cohort effects exist, which cohorts deviate, and whether
# effects accumulate or diminish with age.
#
# Reference: Luo, L., & Hodges, J. (2020). The Age-Period-Cohort-Interaction
# Model for Describing and Investigating Inter-cohort Deviations and Intra-cohort
# Life-course Dynamics. Sociological Methods & Research.
#
# Note: APCI uses glm() internally with weights argument. This produces
# survey-weighted point estimates but SEs do not account for complex survey
# design (strata/PSU clustering). See report for discussion.
# ==============================================================================

library(dplyr)
library(tidyr)
library(APCI)
library(ggplot2)
library(patchwork)

# ==============================================================================
# Data Preparation
# ==============================================================================

#' Prepare survey data for APCI analysis
#'
#' Filters to age range, creates 5-year age/period groups as integer-coded
#' factors (matching APCI package expectations), and computes cohort index.
#'
#' @param df Data frame with srh, age, year, wt columns
#' @param survey Character: survey name (for period mapping)
#' @param srh_var Character: name of SRH variable (default "srh")
#' @param age_min Minimum age (inclusive, default 20)
#' @param age_max Maximum age (inclusive, default 89)
#' @return Data frame with age_group, period_group, cohort_group as factors,
#'   plus age_group_label/period_group_label for display
prep_apci_data <- function(df,
                           survey,
                           srh_var = "srh",
                           age_min = 20,
                           age_max = 89) {

  survey <- tolower(survey)

  # --- Validate required columns ---
  required <- c(srh_var, "age", "year", "wt")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # --- Filter and drop NAs ---
  df_prep <- df |>
    filter(
      !is.na(.data[[srh_var]]),
      !is.na(age), !is.na(year), !is.na(wt),
      age >= age_min, age < (age_max + 1)
    )

  # --- Create 5-year age groups ---
  age_breaks <- seq(age_min, age_max + 1, by = 5)
  age_labels <- paste0(head(age_breaks, -1), "-", tail(age_breaks, -1) - 1)

  df_prep <- df_prep |>
    mutate(
      age_group_label = cut(age,
                            breaks = age_breaks,
                            labels = age_labels,
                            right = FALSE),
      # Integer index for APCI (1-based)
      age_group = as.integer(age_group_label)
    ) |>
    filter(!is.na(age_group_label))

  # --- Create 5-year period groups ---
  if (survey == "nhanes") {
    # NHANES: 2-year cycles mapped to 5-year windows
    df_prep <- df_prep |>
      mutate(
        period_midpoint = case_when(
          year >= 1999 & year < 2004 ~ 2002L,
          year >= 2004 & year < 2009 ~ 2007L,
          year >= 2009 & year < 2014 ~ 2012L,
          year >= 2014 & year < 2019 ~ 2017L,
          year >= 2019 & year <= 2023 ~ 2021L,
          TRUE ~ NA_integer_
        ),
        period_label = case_when(
          year >= 1999 & year < 2004 ~ "1999-2003",
          year >= 2004 & year < 2009 ~ "2004-2008",
          year >= 2009 & year < 2014 ~ "2009-2013",
          year >= 2014 & year < 2019 ~ "2014-2018",
          year >= 2019 & year <= 2023 ~ "2019-2023",
          TRUE ~ NA_character_
        )
      )
  } else {
    # Annual surveys: 5-year bins (2000-2004 -> "2000-2004", etc.)
    period_start <- as.integer(floor(df_prep$year / 5) * 5)
    df_prep <- df_prep |>
      mutate(
        period_start = as.integer(floor(year / 5) * 5),
        period_midpoint = period_start + 2L,
        period_label = paste0(period_start, "-", period_start + 4)
      ) |>
      select(-period_start)
  }

  df_prep <- df_prep |>
    filter(!is.na(period_midpoint))

  # Convert period to ordered factor, then integer index
  period_levels <- sort(unique(df_prep$period_label))
  df_prep <- df_prep |>
    mutate(
      period_group_label = factor(period_label, levels = period_levels),
      period_group = as.integer(period_group_label)
    )

  # --- Compute cohort index ---
  # Cohort = period_midpoint - age_midpoint (using midpoints of bins)
  age_midpoints <- (head(age_breaks, -1) + tail(age_breaks, -1)) / 2
  df_prep <- df_prep |>
    mutate(
      age_midpoint = age_midpoints[age_group],
      cohort_midpoint = period_midpoint - age_midpoint,
      # Integer cohort index for APCI
      cohort_group = as.integer(factor(cohort_midpoint))
    )

  # --- Convert age/period group to factor (APCI expects factor) ---
  df_prep <- df_prep |>
    mutate(
      age_group = factor(age_group),
      period_group = factor(period_group),
      cohort_group = factor(cohort_group)
    )

  # --- Sanity checks ---
  n_age <- length(unique(df_prep$age_group))
  n_period <- length(unique(df_prep$period_group))
  n_cohort <- length(unique(df_prep$cohort_group))

  message("  ", toupper(survey), " APCI prep: ",
          format(nrow(df_prep), big.mark = ","), " rows, ",
          n_age, " age groups, ",
          n_period, " periods, ",
          n_cohort, " cohorts")
  message("  Age groups: ", paste(age_labels[sort(unique(as.integer(df_prep$age_group)))],
                                   collapse = ", "))
  message("  Periods: ", paste(period_levels, collapse = ", "))

  df_prep
}


# ==============================================================================
# Run APCI Model
# ==============================================================================

#' Run APCI model for a single survey
#'
#' @param df_prep Data frame from prep_apci_data()
#' @param srh_var Character: name of SRH variable (default "srh")
#' @param family Character: GLM family (default "gaussian")
#' @param dev_test Logical: run global deviance test? (default TRUE)
#' @return APCI model object (list)
run_apci_model <- function(df_prep,
                           srh_var = "srh",
                           family = "gaussian",
                           dev_test = FALSE) {

  # Note: dev.test = FALSE by default because APCI::tests() can fail

  # with singular matrix errors on some data configurations.
  # The deviance test info is still available in model$dev_global.
  model <- APCI::apci(
    outcome = srh_var,
    age     = "age_group",
    period  = "period_group",
    cohort  = "cohort_group",
    weight  = "wt",
    data    = as.data.frame(df_prep),
    family  = family,
    dev.test = dev_test,
    print   = FALSE
  )

  model
}


# ==============================================================================
# Extract Results
# ==============================================================================

#' Extract tidy results from APCI model
#'
#' Extracts age effects, period effects, cohort averages, cohort slopes,
#' and deviance test results into clean data frames.
#'
#' @param model APCI model object from run_apci_model()
#' @param df_prep Data frame from prep_apci_data() (for label lookup)
#' @return Named list of tibbles: age_effects, period_effects,
#'   cohort_averages, cohort_slopes, deviance_test, intercept
extract_apci_results <- function(model, df_prep) {

  # --- Label lookup tables ---
  age_lookup <- df_prep |>
    distinct(age_group, age_group_label, age_midpoint) |>
    arrange(age_group) |>
    mutate(age_index = as.integer(as.character(age_group)))

  period_lookup <- df_prep |>
    distinct(period_group, period_group_label, period_midpoint) |>
    arrange(period_group) |>
    mutate(period_index = as.integer(as.character(period_group)))

  cohort_lookup <- df_prep |>
    distinct(cohort_group, cohort_midpoint) |>
    arrange(cohort_group) |>
    mutate(cohort_index = as.integer(as.character(cohort_group)))

  # --- Intercept ---
  # APCI returns intercept as character vector: c(estimate, se, p, sig)
  intercept_raw <- model$intercept
  intercept <- as.numeric(intercept_raw[1])

  # --- Helper: safely convert APCI character matrix columns to numeric ---
  safe_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

  # --- Age effects ---
  # APCI returns character matrix with columns: group, estimate, se, p, sig
  age_eff_raw <- model$age_effect
  age_effects <- tibble(
    age_index = safe_numeric(age_eff_raw[, 1]),
    estimate  = safe_numeric(age_eff_raw[, 2]),
    se        = safe_numeric(age_eff_raw[, 3]),
    p_value   = safe_numeric(age_eff_raw[, 4])
  ) |>
    left_join(age_lookup, by = "age_index") |>
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )

  # --- Period effects ---
  period_eff_raw <- model$period_effect
  period_effects <- tibble(
    period_index = safe_numeric(period_eff_raw[, 1]),
    estimate     = safe_numeric(period_eff_raw[, 2]),
    se           = safe_numeric(period_eff_raw[, 3]),
    p_value      = safe_numeric(period_eff_raw[, 4])
  ) |>
    left_join(period_lookup, by = "period_index") |>
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )

  # --- Cohort averages (inter-cohort deviations) ---
  # Columns: cohort_group, cohort_average, cohort_average_se,
  #          cohort_average_t, cohort_average_p, sig
  cohort_avg_raw <- model$cohort_average
  cohort_averages <- tibble(
    cohort_index = safe_numeric(cohort_avg_raw[, 1]),
    estimate     = safe_numeric(cohort_avg_raw[, 2]),
    se           = safe_numeric(cohort_avg_raw[, 3]),
    t_stat       = safe_numeric(cohort_avg_raw[, 4]),
    p_value      = safe_numeric(cohort_avg_raw[, 5])
  ) |>
    left_join(cohort_lookup, by = "cohort_index") |>
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      sig = case_when(
        is.na(p_value) ~ "",
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.10  ~ ".",
        TRUE ~ ""
      ),
      birth_year_approx = round(cohort_midpoint)
    )

  # --- Cohort slopes (intra-cohort life-course dynamics) ---
  # Columns: cohort_group, cohort_slope, cohort_slope_se,
  #          cohort_slope_t, cohort_slope_p, sig
  cohort_slope_raw <- model$cohort_slope
  cohort_slopes <- tibble(
    cohort_index = safe_numeric(cohort_slope_raw[, 1]),
    estimate     = safe_numeric(cohort_slope_raw[, 2]),
    se           = safe_numeric(cohort_slope_raw[, 3]),
    t_stat       = safe_numeric(cohort_slope_raw[, 4]),
    p_value      = safe_numeric(cohort_slope_raw[, 5])
  ) |>
    left_join(cohort_lookup, by = "cohort_index") |>
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      sig = case_when(
        is.na(p_value) ~ "",
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.10  ~ ".",
        TRUE ~ ""
      ),
      interpretation = case_when(
        is.na(p_value) ~ "Not estimable",
        estimate > 0 & p_value < 0.05 ~ "Accumulating advantage",
        estimate < 0 & p_value < 0.05 ~ "Accumulating disadvantage",
        TRUE ~ "Constant/non-significant"
      ),
      birth_year_approx = round(cohort_midpoint)
    )

  # --- Interaction matrix ---
  int_matrix <- model$int_matrix

  # --- Global deviance test ---
  # Stored in model$dev_global (if dev.test was TRUE)
  deviance_test <- model$dev_global

  list(
    intercept       = intercept,
    age_effects     = age_effects,
    period_effects  = period_effects,
    cohort_averages = cohort_averages,
    cohort_slopes   = cohort_slopes,
    int_matrix      = int_matrix,
    deviance_test   = deviance_test
  )
}


# ==============================================================================
# Custom Plotting Functions (using theme_srh)
# ==============================================================================

# APC dimension colors (consistent with medpolish_apc.R)
APCI_COLORS <- c(
  "Age"    = "#0072B2",
  "Period" = "#009E73",
  "Cohort" = "#CC79A7"
)


#' Plot APCI age and period main effects
#'
#' @param results List from extract_apci_results()
#' @param survey Character: survey name for title
#' @return patchwork object (2 panels)
plot_apci_main_effects <- function(results, survey) {

  # Age effects
  p_age <- ggplot(results$age_effects,
                  aes(x = age_midpoint, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = APCI_COLORS[["Age"]], alpha = 0.2) +
    geom_line(color = APCI_COLORS[["Age"]], linewidth = 0.8) +
    geom_point(color = APCI_COLORS[["Age"]], size = 2.5) +
    labs(x = "Age (midpoint)", y = "Effect (SRH units)",
         title = "Age Main Effects") +
    theme_srh() +
    theme(plot.title = element_text(color = APCI_COLORS[["Age"]]))

  # Period effects
  p_period <- ggplot(results$period_effects,
                     aes(x = period_midpoint, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = APCI_COLORS[["Period"]], alpha = 0.2) +
    geom_line(color = APCI_COLORS[["Period"]], linewidth = 0.8) +
    geom_point(color = APCI_COLORS[["Period"]], size = 2.5) +
    labs(x = "Period (midpoint)", y = "Effect (SRH units)",
         title = "Period Main Effects") +
    theme_srh() +
    theme(plot.title = element_text(color = APCI_COLORS[["Period"]]))

  combined <- p_age + p_period +
    plot_annotation(
      title    = paste0("APCI Main Effects: ", toupper(survey)),
      subtitle = "Age and period effects from APC-I model (cohort = age x period interaction)",
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40")
      )
    )

  combined
}


#' Plot APCI cohort deviations (inter-cohort averages)
#'
#' @param results List from extract_apci_results()
#' @param survey Character: survey name for title
#' @return ggplot object
plot_apci_cohort_averages <- function(results, survey) {

  df <- results$cohort_averages |>
    filter(!is.na(cohort_midpoint))

  ggplot(df, aes(x = cohort_midpoint, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = APCI_COLORS[["Cohort"]], alpha = 0.2) +
    geom_line(color = APCI_COLORS[["Cohort"]], linewidth = 0.8) +
    geom_point(aes(shape = sig != ""),
               color = APCI_COLORS[["Cohort"]], size = 2.5) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                       labels = c("Not sig.", "p < 0.05"),
                       name = "Significance") +
    labs(
      x = "Cohort (approx. birth year)",
      y = "Cohort Deviation (SRH units)",
      title = paste0("Inter-Cohort Deviations: ", toupper(survey)),
      subtitle = "Average deviation of each cohort from age + period main effects",
      caption = "Filled points = p < 0.05"
    ) +
    theme_srh()
}


#' Plot APCI cohort slopes (intra-cohort dynamics)
#'
#' @param results List from extract_apci_results()
#' @param survey Character: survey name for title
#' @return ggplot object
plot_apci_cohort_slopes <- function(results, survey) {

  df <- results$cohort_slopes |>
    filter(!is.na(cohort_midpoint))

  ggplot(df, aes(x = cohort_midpoint, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = "#E69F00", alpha = 0.2) +
    geom_line(color = "#E69F00", linewidth = 0.8) +
    geom_point(aes(shape = sig != ""),
               color = "#E69F00", size = 2.5) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                       labels = c("Not sig.", "p < 0.05"),
                       name = "Significance") +
    labs(
      x = "Cohort (approx. birth year)",
      y = "Intra-Cohort Slope",
      title = paste0("Intra-Cohort Life-Course Slopes: ", toupper(survey)),
      subtitle = "Positive = accumulating advantage; Negative = accumulating disadvantage",
      caption = "Filled points = p < 0.05"
    ) +
    theme_srh()
}


#' Combined 3-panel APCI figure per survey
#'
#' @param results List from extract_apci_results()
#' @param survey Character: survey name
#' @return patchwork object
plot_apci_combined <- function(results, survey) {

  p_main <- plot_apci_main_effects(results, survey)
  p_avg  <- plot_apci_cohort_averages(results, survey)
  p_slope <- plot_apci_cohort_slopes(results, survey)

  combined <- p_main / (p_avg | p_slope) +
    plot_layout(heights = c(1, 1.2)) +
    plot_annotation(
      title    = paste0("APCI Analysis: ", toupper(survey)),
      subtitle = "Age-Period-Cohort-Interaction model (Luo & Hodges, 2020)",
      theme = theme(
        plot.title    = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13, color = "gray40")
      )
    )

  combined
}


# ==============================================================================
# Cross-Survey Summary Plots
# ==============================================================================

#' Plot cohort averages across all surveys
#'
#' @param all_cohort_avgs Tibble with cohort averages from all surveys
#'   (must have survey column)
#' @param survey_order Character vector for facet ordering
#' @return ggplot object
plot_apci_cohort_avgs_all <- function(all_cohort_avgs,
                                      survey_order = c("GSS", "NHANES", "MEPS",
                                                        "NHIS", "CPS", "BRFSS")) {
  df <- all_cohort_avgs |>
    filter(!is.na(cohort_midpoint)) |>
    mutate(survey = factor(survey, levels = survey_order))

  ggplot(df, aes(x = cohort_midpoint, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = APCI_COLORS[["Cohort"]], alpha = 0.15) +
    geom_line(color = APCI_COLORS[["Cohort"]], linewidth = 0.6) +
    geom_point(aes(shape = sig != ""),
               color = APCI_COLORS[["Cohort"]], size = 1.8) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                       labels = c("Not sig.", "p < 0.05"),
                       name = NULL) +
    facet_wrap(~ survey, scales = "free", ncol = 3) +
    labs(
      x = "Cohort (approx. birth year)",
      y = "Cohort Deviation (SRH units)",
      title = "APCI Inter-Cohort Deviations Across Surveys",
      subtitle = "Average deviation from age + period main effects; filled = p < 0.05"
    ) +
    theme_srh() +
    theme(legend.position = "bottom")
}


#' Plot cohort slopes across all surveys
#'
#' @param all_cohort_slopes Tibble with cohort slopes from all surveys
#' @param survey_order Character vector for facet ordering
#' @return ggplot object
plot_apci_cohort_slopes_all <- function(all_cohort_slopes,
                                        survey_order = c("GSS", "NHANES", "MEPS",
                                                          "NHIS", "CPS", "BRFSS")) {
  df <- all_cohort_slopes |>
    filter(!is.na(cohort_midpoint)) |>
    mutate(survey = factor(survey, levels = survey_order))

  ggplot(df, aes(x = cohort_midpoint, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = "#E69F00", alpha = 0.15) +
    geom_line(color = "#E69F00", linewidth = 0.6) +
    geom_point(aes(shape = sig != ""),
               color = "#E69F00", size = 1.8) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                       labels = c("Not sig.", "p < 0.05"),
                       name = NULL) +
    facet_wrap(~ survey, scales = "free", ncol = 3) +
    labs(
      x = "Cohort (approx. birth year)",
      y = "Intra-Cohort Slope",
      title = "APCI Intra-Cohort Life-Course Slopes Across Surveys",
      subtitle = "Positive = accumulating advantage; Negative = accumulating disadvantage; filled = p < 0.05"
    ) +
    theme_srh() +
    theme(legend.position = "bottom")
}


#' Plot age and period main effects across all surveys (grid)
#'
#' @param all_age_effects Tibble with age effects from all surveys
#' @param all_period_effects Tibble with period effects from all surveys
#' @param survey_order Character vector for row ordering
#' @return patchwork object
plot_apci_main_effects_all <- function(all_age_effects,
                                       all_period_effects,
                                       survey_order = c("GSS", "NHANES", "MEPS",
                                                         "NHIS", "CPS", "BRFSS")) {

  df_age <- all_age_effects |>
    filter(!is.na(age_midpoint)) |>
    mutate(survey = factor(survey, levels = survey_order))

  df_period <- all_period_effects |>
    filter(!is.na(period_midpoint)) |>
    mutate(survey = factor(survey, levels = survey_order))

  p_age <- ggplot(df_age, aes(x = age_midpoint, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = APCI_COLORS[["Age"]], alpha = 0.15) +
    geom_line(color = APCI_COLORS[["Age"]], linewidth = 0.6) +
    geom_point(color = APCI_COLORS[["Age"]], size = 1.5) +
    facet_wrap(~ survey, scales = "free_y", ncol = 3) +
    labs(x = "Age (midpoint)", y = "Effect (SRH units)",
         title = "Age Main Effects") +
    theme_srh() +
    theme(plot.title = element_text(color = APCI_COLORS[["Age"]]))

  p_period <- ggplot(df_period, aes(x = period_midpoint, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = APCI_COLORS[["Period"]], alpha = 0.15) +
    geom_line(color = APCI_COLORS[["Period"]], linewidth = 0.6) +
    geom_point(color = APCI_COLORS[["Period"]], size = 1.5) +
    facet_wrap(~ survey, scales = "free_y", ncol = 3) +
    labs(x = "Period (midpoint)", y = "Effect (SRH units)",
         title = "Period Main Effects") +
    theme_srh() +
    theme(plot.title = element_text(color = APCI_COLORS[["Period"]]))

  combined <- p_age / p_period +
    plot_annotation(
      title    = "APCI Main Effects Across Surveys",
      subtitle = "Age (blue) and Period (green) main effects from APC-I model",
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40")
      )
    )

  combined
}


# ==============================================================================
# Combined Grid Figure (4 rows × 6 surveys)
# ==============================================================================

#' Create a single APCI subplot for the combined grid
#'
#' @param df Data frame with estimate, ci_lower, ci_upper, and x_var column
#' @param x_var Character: column name for x-axis (e.g., "cohort_midpoint")
#' @param color Character: line/point color (hex)
#' @param show_title Show survey name above plot? (TRUE for row 1 only)
#' @param title_text Survey name text
#' @param show_ylabel Show y-axis label? (TRUE for column 1 only)
#' @param ylabel_text Y-axis label text
#' @param show_significance Show filled/hollow significance markers?
#' @param y_limits Numeric vector c(lo, hi) for shared y-axis, or NULL for free
#' @param base_size Base font size
#' @return ggplot object
create_apci_subplot <- function(df, x_var, color,
                                show_title = FALSE, title_text = "",
                                show_ylabel = FALSE, ylabel_text = "",
                                panel_label = NULL,
                                tag_position = c(-0.05, 0.88),
                                show_significance = FALSE,
                                y_limits = NULL,
                                base_size = 20) {

  df <- df |>
    filter(!is.na(.data[[x_var]]), !is.na(estimate))

  # Add significance indicator using p_value directly (more reliable than sig
  # column after CSV round-trip where empty strings become NA)
  if (show_significance && "p_value" %in% names(df)) {
    df <- df |>
      mutate(is_sig = !is.na(p_value) & p_value < 0.05)
  }

  # Base plot
  p <- ggplot(df, aes(x = .data[[x_var]], y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60",
               linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = color, alpha = 0.15) +
    geom_line(color = color, linewidth = 0.6)

  # Points: shaped by significance for cohort rows
  if (show_significance && "is_sig" %in% names(df)) {
    p <- p +
      geom_point(aes(shape = is_sig), color = color, size = 2) +
      scale_shape_manual(
        values = c("FALSE" = 1, "TRUE" = 16),
        labels = c("Not sig.", "p < 0.05"),
        name = "Significance"
      )
  } else {
    p <- p + geom_point(color = color, size = 2)
  }

  # Shared y-limits for age/period rows
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }

  # Labels
  p <- p + labs(
    title = if (show_title) title_text else NULL,
    tag = panel_label,
    x = NULL,
    y = if (show_ylabel) ylabel_text else NULL
  )

  # Compact theme for grid layout (matches Figure 1 subplot style)
  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      plot.tag = element_text(size = base_size + 4, face = "bold"),
      plot.tag.position = tag_position,
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, color = "gray30"),
      axis.text.x = element_text(size = base_size - 1, color = "gray30",
                                  angle = 45, hjust = 1),
      plot.margin = margin(3, 5, 3, 5),
      legend.position = "none"
    )

  p
}


#' Combined 4-row x 6-column APCI grid figure
#'
#' Creates a publication-ready figure with all APCI results in a grid:
#'   Row 1: Cohort deviations (inter-cohort averages)
#'   Row 2: Cohort slopes (intra-cohort life-course dynamics)
#'   Row 3: Age main effects
#'   Row 4: Period main effects
#'
#' Rows 1-2 have free y-axes (cohort effects vary in magnitude across surveys).
#' Rows 3-4 share the same y-axis to emphasize the magnitude difference between
#' age and period effects.
#'
#' @param all_cohort_avgs Tibble with cohort averages from all surveys
#' @param all_cohort_slopes Tibble with cohort slopes from all surveys
#' @param all_age_effects Tibble with age effects from all surveys
#' @param all_period_effects Tibble with period effects from all surveys
#' @param survey_order Character vector for column ordering
#' @param base_size Base font size (default 12)
#' @return patchwork object
plot_apci_combined_grid <- function(all_cohort_avgs, all_cohort_slopes,
                                    all_age_effects, all_period_effects,
                                    survey_order = c("BRFSS", "MEPS", "NHIS",
                                                      "CPS", "NHANES", "GSS"),
                                    base_size = 20) {

  n_surveys <- length(survey_order)

  # --- Shared y-limits for age and period rows ---
  age_ylim <- all_age_effects |>
    filter(!is.na(ci_lower), !is.na(ci_upper)) |>
    summarise(lo = min(ci_lower), hi = max(ci_upper))
  period_ylim <- all_period_effects |>
    filter(!is.na(ci_lower), !is.na(ci_upper)) |>
    summarise(lo = min(ci_lower), hi = max(ci_upper))

  shared_ylim <- c(
    min(age_ylim$lo, period_ylim$lo),
    max(age_ylim$hi, period_ylim$hi)
  )
  # 5% padding
  pad <- diff(shared_ylim) * 0.05
  shared_ylim <- shared_ylim + c(-pad, pad)

  # --- Build 4 x n_surveys subplot lists ---
  row1_plots <- row2_plots <- row3_plots <- row4_plots <-
    vector("list", n_surveys)

  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    is_first <- (i == 1)

    df_avg    <- all_cohort_avgs    |> filter(survey == svy)
    df_slope  <- all_cohort_slopes  |> filter(survey == svy)
    df_age    <- all_age_effects    |> filter(survey == svy)
    df_period <- all_period_effects |> filter(survey == svy)

    # Row 1: Age main effects (shared y-axis)
    row1_plots[[i]] <- create_apci_subplot(
      df = df_age, x_var = "age_midpoint", color = "#0072B2",
      show_title = TRUE, title_text = svy,
      show_ylabel = is_first, ylabel_text = "Age Effect",
      panel_label = if (is_first) "A" else NULL,
      tag_position = c(-0.05, 0.82),
      y_limits = shared_ylim, base_size = base_size
    )

    # Row 2: Period main effects (shared y-axis)
    row2_plots[[i]] <- create_apci_subplot(
      df = df_period, x_var = "period_midpoint", color = "#009E73",
      show_ylabel = is_first, ylabel_text = "Period Effect",
      panel_label = if (is_first) "B" else NULL,
      y_limits = shared_ylim, base_size = base_size
    )

    # Row 3: Cohort deviations
    row3_plots[[i]] <- create_apci_subplot(
      df = df_avg, x_var = "cohort_midpoint", color = "#CC79A7",
      show_ylabel = is_first, ylabel_text = "Cohort Deviation",
      panel_label = if (is_first) "C" else NULL,
      show_significance = TRUE, base_size = base_size
    )

    # Row 4: Cohort slopes
    row4_plots[[i]] <- create_apci_subplot(
      df = df_slope, x_var = "cohort_midpoint", color = "#E69F00",
      show_ylabel = is_first, ylabel_text = "Cohort Slope",
      panel_label = if (is_first) "D" else NULL,
      show_significance = TRUE, base_size = base_size
    )
  }

  # --- Section labels (rotated text on left margin) ---
  label_cohort <- wrap_elements(full = grid::textGrob(
    "Cohort Deviations and Slopes", rot = 90,
    gp = grid::gpar(fontsize = base_size + 4, fontface = "bold")
  ))

  label_main <- wrap_elements(full = grid::textGrob(
    "Age and Period Main Effects", rot = 90,
    gp = grid::gpar(fontsize = base_size + 4, fontface = "bold")
  ))

  # --- Assemble rows ---
  row1 <- wrap_plots(row1_plots, ncol = n_surveys)
  row2 <- wrap_plots(row2_plots, ncol = n_surveys)
  row3 <- wrap_plots(row3_plots, ncol = n_surveys)
  row4 <- wrap_plots(row4_plots, ncol = n_surveys)

  # --- Combine: label | content rows for each section ---
  main_section <- (label_main | (row1 / row2)) +
    plot_layout(widths = c(0.04, 1))
  cohort_section <- (label_cohort | (row3 / row4)) +
    plot_layout(widths = c(0.04, 1))

  spacer <- plot_spacer()

  # Stack sections with thin spacer between
  combined <- main_section / spacer / cohort_section +
    plot_layout(heights = c(1, 0.03, 1))

  # --- Title and subtitle ---
  combined <- combined +
    plot_annotation(
      title = "Age-Period-Cohort Interaction Model Analysis",
      subtitle = paste0(
        "Filled circles = p < 0.05 | ",
        "Cohort rows: free y-axis | ",
        "Age & period rows: shared y-axis"
      ),
      theme = theme(
        plot.title = element_text(
          size = base_size + 8, face = "bold", hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = base_size + 2, color = "gray40", hjust = 0.5
        ),
        plot.margin = margin(10, 10, 5, 10)
      )
    )

  combined
}
