# ==============================================================================
# medpolish_apc.R
# Median Polish APC Decomposition Functions
# Author: Christine Lucille Kuryla
#
# Purpose: Non-parametric APC decomposition via median polish.
# Computes survey-weighted cell means in (age group x period) cells,
# then applies medpolish() to three 2D slices (AP, AC, PC) to extract
# age, period, and cohort effects from residuals.
#
# Reference: Gloria Graf's approach — complements the parametric BHAPC.
# ==============================================================================

library(dplyr)
library(tidyr)
library(srvyr)
library(ggplot2)
library(patchwork)
library(broom)

# ==============================================================================
# Age/Period/Cohort Binning Helpers
# ==============================================================================

#' Create regular 5-year age groups
#'
#' @param age Numeric vector of ages
#' @param age_min Lower bound (inclusive), default 20
#' @param age_max Upper bound (inclusive, rounded up to next break), default 84
#' @return Factor with labels like "20-24", "25-29", ..., "80-84"
create_age_groups_5yr <- function(age, age_min = 20, age_max = 84) {
  breaks <- seq(age_min, age_max + 1, by = 5)
  labels <- paste0(head(breaks, -1), "-", tail(breaks, -1) - 1)
  cut(age, breaks = breaks, labels = labels, right = FALSE)
}


#' Map survey years to 5-year period bins
#'
#' @param year Numeric vector of survey years
#' @param survey Character: survey name (for NHANES special handling)
#' @return Integer vector of period midpoints (e.g., 2002 for 2000-2004)
map_to_period_5yr <- function(year, survey = "annual") {
  survey <- tolower(survey)

  if (survey == "nhanes") {
    # NHANES uses 2-year cycles; map to 5-year windows
    # Use midpoint-year representation for each NHANES cycle first
    # Then group into 5-year periods
    period <- case_when(
      year >= 1999 & year < 2004 ~ 2000L,
      year >= 2004 & year < 2009 ~ 2005L,
      year >= 2009 & year < 2014 ~ 2010L,
      year >= 2014 & year < 2019 ~ 2015L,
      year >= 2019 & year <= 2023 ~ 2020L,
      # Older NHANES if present
      year >= 1988 & year < 1994 ~ 1990L,
      year >= 1994 & year < 1999 ~ 1995L,
      TRUE ~ NA_integer_
    )
  } else {
    # Annual surveys: simple 5-year bins anchored to round years
    # 2000-2004 -> 2000, 2005-2009 -> 2005, etc.
    period <- as.integer(floor(year / 5) * 5)
  }

  period
}


#' Parse midpoint from "XX-YY" label
#'
#' @param group_label Character vector of "XX-YY" labels
#' @return Numeric midpoints: (XX + YY) / 2
get_midpoint <- function(group_label) {
  parts <- strsplit(as.character(group_label), "-")
  vapply(parts, function(p) {
    nums <- as.numeric(p)
    if (length(nums) == 2 && all(!is.na(nums))) {
      (nums[1] + nums[2]) / 2
    } else {
      NA_real_
    }
  }, numeric(1))
}


# ==============================================================================
# Data Preparation
# ==============================================================================

#' Prepare survey-weighted cell means for median polish
#'
#' @param df Data frame with srh, age, year, wt (and optionally strata, psu)
#' @param survey Character: survey name (for period mapping and design)
#' @param srh_var Character: name of SRH variable (default "srh")
#' @param age_min Minimum age (default 20)
#' @param age_max Maximum age (default 84)
#' @param use_full_design Logical: use strata/PSU if available? (default TRUE)
#' @return Tibble of cell means with age_group, period_5yr, midpoints, mean_srh, se, n
prepare_medpolish_data <- function(df,
                                   survey,
                                   srh_var = "srh",
                                   age_min = 20,
                                   age_max = 84,
                                   use_full_design = TRUE) {

  survey <- tolower(survey)

  # --- Validate required columns ---
  required <- c(srh_var, "age", "year", "wt")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # --- Filter and create bins ---
  df_prep <- df |>
    filter(
      !is.na(.data[[srh_var]]),
      !is.na(age), !is.na(year), !is.na(wt),
      age >= age_min, age <= age_max
    ) |>
    mutate(
      age_group_5yr = create_age_groups_5yr(age, age_min = age_min, age_max = age_max),
      period_5yr    = map_to_period_5yr(year, survey = survey)
    ) |>
    filter(!is.na(age_group_5yr), !is.na(period_5yr))

  # --- Compute midpoints ---
  df_prep <- df_prep |>
    mutate(
      age_midpoint    = get_midpoint(age_group_5yr),
      period_midpoint = as.numeric(period_5yr),
      cohort_midpoint = period_midpoint - age_midpoint
    )

  message("  ", toupper(survey), " prep: ",
          format(nrow(df_prep), big.mark = ","), " rows, ",
          length(unique(df_prep$age_group_5yr)), " age groups, ",
          length(unique(df_prep$period_5yr)), " periods")

  # --- Create survey design ---
  has_strata <- "strata" %in% names(df_prep) && !all(is.na(df_prep$strata))
  has_psu    <- "psu" %in% names(df_prep) && !all(is.na(df_prep$psu))

  if (use_full_design && has_strata && has_psu) {
    message("  Using full survey design (strata + PSU + weights)")
    options(survey.lonely.psu = "adjust")
    svy <- df_prep |>
      as_survey_design(
        ids    = psu,
        strata = strata,
        weights = wt,
        nest   = TRUE
      )
  } else {
    message("  Using weights-only design")
    svy <- df_prep |>
      as_survey_design(
        ids     = 1,
        weights = wt
      )
  }

  # --- Compute weighted cell means ---
  cell_means <- svy |>
    group_by(age_group_5yr, period_5yr, age_midpoint, period_midpoint, cohort_midpoint) |>
    summarise(
      mean_srh    = survey_mean(.data[[srh_var]], vartype = "se", na.rm = TRUE),
      n           = unweighted(n()),
      .groups = "drop"
    )

  # Warn on small cells

  small_cells <- cell_means |> filter(n < 30)
  if (nrow(small_cells) > 0) {
    warning("  ", nrow(small_cells), " cells with n < 30 (min n = ",
            min(small_cells$n), ")")
  }

  cell_means
}


# ==============================================================================
# Matrix Building & Median Polish
# ==============================================================================

#' Build three 2D matrices from cell means for median polish
#'
#' @param cell_means Tibble from prepare_medpolish_data()
#' @param outcome_var Character: column name for outcome (default "mean_srh")
#' @return Named list with AP, AC, PC matrices and their labels
build_apc_matrices <- function(cell_means, outcome_var = "mean_srh") {

  # Helper: sort columns of a pivoted tibble numerically by column name
  sort_numeric_cols <- function(wide_df, id_col) {
    other_cols <- setdiff(names(wide_df), id_col)
    sorted_cols <- other_cols[order(as.numeric(other_cols))]
    wide_df |> select(all_of(c(id_col, sorted_cols)))
  }

  # AP matrix: rows = age_midpoint, cols = period_midpoint
  # (residuals reveal cohort effects)
  ap_wide <- cell_means |>
    select(age_midpoint, period_midpoint, !!sym(outcome_var)) |>
    pivot_wider(
      names_from  = period_midpoint,
      values_from = !!sym(outcome_var)
    ) |>
    arrange(age_midpoint) |>
    sort_numeric_cols("age_midpoint")

  ap_mat <- as.matrix(ap_wide |> select(-age_midpoint))
  rownames(ap_mat) <- ap_wide$age_midpoint

  # AC matrix: rows = age_midpoint, cols = cohort_midpoint
  # (residuals reveal period effects)
  ac_wide <- cell_means |>
    select(age_midpoint, cohort_midpoint, !!sym(outcome_var)) |>
    pivot_wider(
      names_from  = cohort_midpoint,
      values_from = !!sym(outcome_var)
    ) |>
    arrange(age_midpoint) |>
    sort_numeric_cols("age_midpoint")

  ac_mat <- as.matrix(ac_wide |> select(-age_midpoint))
  rownames(ac_mat) <- ac_wide$age_midpoint

  # PC matrix: rows = period_midpoint, cols = cohort_midpoint
  # (residuals reveal age effects)
  pc_wide <- cell_means |>
    select(period_midpoint, cohort_midpoint, !!sym(outcome_var)) |>
    pivot_wider(
      names_from  = cohort_midpoint,
      values_from = !!sym(outcome_var)
    ) |>
    arrange(period_midpoint) |>
    sort_numeric_cols("period_midpoint")

  pc_mat <- as.matrix(pc_wide |> select(-period_midpoint))
  rownames(pc_mat) <- pc_wide$period_midpoint

  list(
    AP = list(mat = ap_mat, row_dim = "age_midpoint",  col_dim = "period_midpoint"),
    AC = list(mat = ac_mat, row_dim = "age_midpoint",  col_dim = "cohort_midpoint"),
    PC = list(mat = pc_mat, row_dim = "period_midpoint", col_dim = "cohort_midpoint")
  )
}


#' Run median polish on all three APC slices
#'
#' @param cell_means Tibble from prepare_medpolish_data()
#' @param outcome_var Character: column for outcome (default "mean_srh")
#' @return List with: mp_objects (raw medpolish), effects (tidy), matrices
run_medpolish_apc <- function(cell_means, outcome_var = "mean_srh") {

  matrices <- build_apc_matrices(cell_means, outcome_var = outcome_var)

  mp_objects <- list()
  tidy_effects <- list()

  for (slice_name in c("AP", "AC", "PC")) {
    mat <- matrices[[slice_name]]$mat
    mp <- medpolish(mat, na.rm = TRUE, trace.iter = FALSE)
    mp_objects[[slice_name]] <- mp

    # Row effects
    row_eff <- tibble(
      midpoint   = as.numeric(names(mp$row)),
      effect     = mp$row,
      dimension  = matrices[[slice_name]]$row_dim,
      slice      = slice_name,
      type       = "row"
    )

    # Column effects
    col_eff <- tibble(
      midpoint   = as.numeric(names(mp$col)),
      effect     = mp$col,
      dimension  = matrices[[slice_name]]$col_dim,
      slice      = slice_name,
      type       = "col"
    )

    tidy_effects[[slice_name]] <- bind_rows(row_eff, col_eff)
  }

  # Residuals from AP matrix → cohort residuals
  ap_resid <- tidy_residuals(mp_objects$AP, matrices$AP, third_dim = "cohort")
  # Residuals from AC matrix → period residuals
  ac_resid <- tidy_residuals(mp_objects$AC, matrices$AC, third_dim = "period")
  # Residuals from PC matrix → age residuals
  pc_resid <- tidy_residuals(mp_objects$PC, matrices$PC, third_dim = "age")

  list(
    mp_objects   = mp_objects,
    tidy_effects = bind_rows(tidy_effects),
    residuals    = list(AP = ap_resid, AC = ac_resid, PC = pc_resid),
    matrices     = matrices,
    overall      = list(
      AP = mp_objects$AP$overall,
      AC = mp_objects$AC$overall,
      PC = mp_objects$PC$overall
    )
  )
}


#' Tidy residuals from medpolish object
#'
#' @param mp medpolish object
#' @param mat_info List with mat, row_dim, col_dim
#' @param third_dim Character: name of the third (residual) dimension
#'   "cohort" for AP, "period" for AC, "age" for PC
#' @return Tibble with row/col midpoints, residual, and third_dim value
tidy_residuals <- function(mp, mat_info, third_dim = "cohort") {
  resid_mat <- mp$residuals
  row_names <- as.numeric(rownames(resid_mat))
  col_names <- as.numeric(colnames(resid_mat))

  tidied <- expand_grid(
    row_mid = row_names,
    col_mid = col_names
  ) |>
    mutate(
      residual = as.vector(t(resid_mat))
    ) |>
    filter(!is.na(residual))

  # Reconstruct third dimension
  if (third_dim == "cohort") {
    # AP: row=age, col=period => cohort = period - age
    tidied <- tidied |>
      mutate(
        age_midpoint    = row_mid,
        period_midpoint = col_mid,
        cohort_midpoint = col_mid - row_mid
      )
  } else if (third_dim == "period") {
    # AC: row=age, col=cohort => period = age + cohort
    tidied <- tidied |>
      mutate(
        age_midpoint    = row_mid,
        cohort_midpoint = col_mid,
        period_midpoint = row_mid + col_mid
      )
  } else if (third_dim == "age") {
    # PC: row=period, col=cohort => age = period - cohort
    tidied <- tidied |>
      mutate(
        period_midpoint = row_mid,
        cohort_midpoint = col_mid,
        age_midpoint    = row_mid - col_mid
      )
  }

  tidied |> select(-row_mid, -col_mid)
}


# ==============================================================================
# Effect Extraction & Linear Trends
# ==============================================================================

#' Extract mean residual-based effects per APC dimension
#'
#' For each dimension, averages residuals across the other two to get
#' the marginal effect. Uses AP residuals for cohort, AC for period, PC for age.
#'
#' @param mp_result List from run_medpolish_apc()
#' @return Tibble: value, effect, n_cells, dimension (stacked Age/Period/Cohort)
extract_medpolish_effects <- function(mp_result) {

  # Age effects: from PC residuals, grouped by age_midpoint
  age_eff <- mp_result$residuals$PC |>
    group_by(value = age_midpoint) |>
    summarise(effect = mean(residual, na.rm = TRUE),
              n_cells = n(), .groups = "drop") |>
    mutate(dimension = "Age")

  # Period effects: from AC residuals, grouped by period_midpoint
  period_eff <- mp_result$residuals$AC |>
    group_by(value = period_midpoint) |>
    summarise(effect = mean(residual, na.rm = TRUE),
              n_cells = n(), .groups = "drop") |>
    mutate(dimension = "Period")

  # Cohort effects: from AP residuals, grouped by cohort_midpoint
  cohort_eff <- mp_result$residuals$AP |>
    group_by(value = cohort_midpoint) |>
    summarise(effect = mean(residual, na.rm = TRUE),
              n_cells = n(), .groups = "drop") |>
    mutate(dimension = "Cohort")

  bind_rows(age_eff, period_eff, cohort_eff)
}


#' Fit linear trend to each APC effect dimension
#'
#' Fits lm(effect ~ value) for Age, Period, and Cohort
#'
#' @param effects_df Tibble from extract_medpolish_effects()
#' @return Tibble with broom::tidy() output per dimension
fit_effect_lm <- function(effects_df) {
  effects_df |>
    group_by(dimension) |>
    group_modify(~ {
      mod <- lm(effect ~ value, data = .x)
      broom::tidy(mod, conf.int = TRUE)
    }) |>
    ungroup()
}


# ==============================================================================
# Direct Row/Column Effects (from medpolish decomposition itself)
# ==============================================================================

#' Extract direct medpolish row/column effects as APC effects
#'
#' Unlike extract_medpolish_effects() which averages residuals, this extracts
#' the additive row and column effects that medpolish produces directly.
#' From the AP matrix: row effects = Age, col effects = Period.
#' From the AC matrix: row effects = Age, col effects = Cohort.
#' From the PC matrix: row effects = Period, col effects = Cohort.
#'
#' Each dimension appears twice (once as row, once as column in different slices).
#' Both estimates are returned so they can be compared.
#'
#' @param mp_result List from run_medpolish_apc()
#' @return Tibble: value, effect, dimension, source_slice, source_type
extract_direct_effects <- function(mp_result) {

  # AP slice: rows = age, cols = period
  ap <- mp_result$mp_objects$AP
  age_from_ap <- tibble(
    value        = as.numeric(names(ap$row)),
    effect       = as.numeric(ap$row),
    dimension    = "Age",
    source_slice = "AP",
    source_type  = "row"
  )
  period_from_ap <- tibble(
    value        = as.numeric(names(ap$col)),
    effect       = as.numeric(ap$col),
    dimension    = "Period",
    source_slice = "AP",
    source_type  = "col"
  )

  # AC slice: rows = age, cols = cohort
  ac <- mp_result$mp_objects$AC
  age_from_ac <- tibble(
    value        = as.numeric(names(ac$row)),
    effect       = as.numeric(ac$row),
    dimension    = "Age",
    source_slice = "AC",
    source_type  = "row"
  )
  cohort_from_ac <- tibble(
    value        = as.numeric(names(ac$col)),
    effect       = as.numeric(ac$col),
    dimension    = "Cohort",
    source_slice = "AC",
    source_type  = "col"
  )

  # PC slice: rows = period, cols = cohort
  pc <- mp_result$mp_objects$PC
  period_from_pc <- tibble(
    value        = as.numeric(names(pc$row)),
    effect       = as.numeric(pc$row),
    dimension    = "Period",
    source_slice = "PC",
    source_type  = "row"
  )
  cohort_from_pc <- tibble(
    value        = as.numeric(names(pc$col)),
    effect       = as.numeric(pc$col),
    dimension    = "Cohort",
    source_slice = "PC",
    source_type  = "col"
  )

  bind_rows(
    age_from_ap, age_from_ac,
    period_from_ap, period_from_pc,
    cohort_from_ac, cohort_from_pc
  )
}


#' Plot direct effects: two estimates per dimension overlaid
#'
#' @param direct_effects Tibble from extract_direct_effects()
#' @param survey Character: survey name for title
#' @return patchwork object (3 panels)
plot_direct_effects <- function(direct_effects, survey) {

  make_panel <- function(dim_name) {
    df_dim <- direct_effects |> filter(dimension == dim_name)
    color <- APC_COLORS[[dim_name]]

    ggplot(df_dim, aes(x = value, y = effect,
                       shape = source_slice, linetype = source_slice)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_line(color = color, linewidth = 0.7) +
      geom_point(color = color, size = 2.5) +
      scale_shape_manual(values = c("AP" = 16, "AC" = 17, "PC" = 15)) +
      scale_linetype_manual(values = c("AP" = "solid", "AC" = "dashed", "PC" = "dotted")) +
      labs(
        x = dim_name,
        y = "Direct Effect (SRH units)",
        title = dim_name,
        shape = "Source slice",
        linetype = "Source slice"
      ) +
      theme_srh() +
      theme(
        plot.title = element_text(color = color),
        legend.position = "bottom"
      )
  }

  p_age    <- make_panel("Age")
  p_period <- make_panel("Period")
  p_cohort <- make_panel("Cohort")

  combined <- p_age + p_period + p_cohort +
    plot_layout(guides = "collect") +
    plot_annotation(
      title    = paste0("Direct Median Polish Effects: ", toupper(survey)),
      subtitle = "Row/column effects from two medpolish slices per dimension",
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40")
      )
    ) &
    theme(legend.position = "bottom")

  combined
}


# ==============================================================================
# Variance Decomposition
# ==============================================================================

#' Compute variance decomposition from medpolish
#'
#' For each slice (AP, AC, PC), computes how much of the total cell-mean
#' variation is explained by row effects, column effects, and residuals.
#' Uses sum of squares decomposition.
#'
#' Note: Because median polish uses medians (not means), the decomposition
#' is not orthogonal — component percentages may not sum to exactly 100%.
#'
#' @param mp_result List from run_medpolish_apc()
#' @return Tibble: slice, component, ss, pct
compute_variance_decomposition <- function(mp_result) {

  decomp_rows <- list()

  for (slice_name in c("AP", "AC", "PC")) {
    mp <- mp_result$mp_objects[[slice_name]]
    mat <- mp_result$matrices[[slice_name]]$mat

    # Total SS of cell means (centered on overall)
    non_na <- !is.na(mat)
    overall <- mp$overall
    ss_total <- sum((mat[non_na] - overall)^2)

    # Row effects: each row effect replicated across columns
    row_eff_mat <- matrix(mp$row, nrow = length(mp$row), ncol = ncol(mat))
    ss_row <- sum(row_eff_mat[non_na]^2)

    # Column effects: each col effect replicated across rows
    col_eff_mat <- matrix(mp$col, nrow = nrow(mat), ncol = length(mp$col),
                          byrow = TRUE)
    ss_col <- sum(col_eff_mat[non_na]^2)

    # Residual SS
    ss_resid <- sum(mp$residuals[non_na]^2)

    row_dim <- mp_result$matrices[[slice_name]]$row_dim
    col_dim <- mp_result$matrices[[slice_name]]$col_dim

    # Clean dimension labels
    dim_label <- function(x) {
      gsub("_midpoint", "", x) |>
        stringr::str_to_title()
    }

    decomp_rows[[slice_name]] <- tibble(
      slice     = slice_name,
      component = c(dim_label(row_dim), dim_label(col_dim), "Residual", "Total"),
      ss        = c(ss_row, ss_col, ss_resid, ss_total),
      pct       = c(ss_row, ss_col, ss_resid, ss_total) / ss_total * 100
    )
  }

  bind_rows(decomp_rows)
}


# ==============================================================================
# Combined Publication Figure
# ==============================================================================

#' Combined 6-survey x 3-dimension figure
#'
#' Rows = surveys (in specified order), columns = Age | Period | Cohort.
#' Uses mean-residual effects from extract_medpolish_effects().
#'
#' @param all_effects Tibble with columns: value, effect, n_cells, dimension, survey
#' @param survey_order Character vector of survey names (for row order)
#' @return patchwork object
plot_medpolish_combined <- function(all_effects,
                                    survey_order = c("GSS", "NHANES", "MEPS",
                                                     "NHIS", "CPS", "BRFSS")) {

  all_effects <- all_effects |>
    mutate(survey = factor(survey, levels = survey_order))

  panels <- list()

  for (sv in survey_order) {
    for (dim_name in c("Age", "Period", "Cohort")) {
      df_cell <- all_effects |>
        filter(survey == sv, dimension == dim_name)

      color <- APC_COLORS[[dim_name]]

      # Only show x-axis label on bottom row
      is_bottom <- (sv == tail(survey_order, 1))
      # Only show y-axis label on left column
      is_left <- (dim_name == "Age")

      p <- ggplot(df_cell, aes(x = value, y = effect)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = "gray60", linewidth = 0.3) +
        geom_point(color = color, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE, color = color,
                    fill = color, alpha = 0.12, linewidth = 0.6) +
        theme_srh(base_size = 11) +
        theme(
          plot.margin = margin(2, 4, 2, 4),
          axis.title.x = if (is_bottom) {
            element_text(size = 9)
          } else {
            element_blank()
          },
          axis.title.y = if (is_left) {
            element_text(size = 9)
          } else {
            element_blank()
          }
        )

      # Add survey label as row title on left column
      if (is_left) {
        p <- p + labs(y = sv, x = if (is_bottom) dim_name else NULL)
      } else {
        p <- p + labs(y = NULL, x = if (is_bottom) dim_name else NULL)
      }

      panels[[paste0(sv, "_", dim_name)]] <- p
    }
  }

  # Arrange in grid: surveys as rows, dimensions as columns
  n_surveys <- length(survey_order)
  combined <- wrap_plots(panels, ncol = 3, nrow = n_surveys, byrow = TRUE) +
    plot_annotation(
      title = "Median Polish APC Effects Across Surveys",
      subtitle = paste0(
        "Columns: ",
        paste0(
          "<span style='color:", APC_COLORS, "'>**", names(APC_COLORS), "**</span>",
          collapse = " | "
        )
      ),
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = ggtext::element_markdown(size = 12, color = "gray30")
      )
    )

  combined
}


#' Fallback combined figure (no ggtext dependency)
#'
#' @param all_effects Tibble with columns: value, effect, n_cells, dimension, survey
#' @param survey_order Character vector of survey names (for row order)
#' @return patchwork object
plot_medpolish_combined_simple <- function(all_effects,
                                           survey_order = c("GSS", "NHANES", "MEPS",
                                                            "NHIS", "CPS", "BRFSS")) {

  all_effects <- all_effects |>
    mutate(survey = factor(survey, levels = survey_order))

  panels <- list()

  for (sv in survey_order) {
    for (dim_name in c("Age", "Period", "Cohort")) {
      df_cell <- all_effects |>
        filter(survey == sv, dimension == dim_name)

      color <- APC_COLORS[[dim_name]]

      is_bottom <- (sv == tail(survey_order, 1))
      is_left   <- (dim_name == "Age")
      is_top    <- (sv == survey_order[1])

      p <- ggplot(df_cell, aes(x = value, y = effect)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = "gray60", linewidth = 0.3) +
        geom_point(color = color, size = 1.8) +
        geom_smooth(method = "lm", se = TRUE, color = color,
                    fill = color, alpha = 0.12, linewidth = 0.6) +
        theme_srh(base_size = 11) +
        theme(
          plot.margin = margin(2, 4, 2, 4),
          axis.title.x = if (is_bottom) element_text(size = 10) else element_blank(),
          axis.title.y = if (is_left) element_text(size = 10) else element_blank(),
          plot.title = if (is_top) element_text(size = 11, color = color, face = "bold") else element_blank()
        )

      # Column headers on top row only
      if (is_top) p <- p + ggtitle(dim_name)

      # Row label as y-axis title on left column
      if (is_left) {
        p <- p + labs(y = sv, x = if (is_bottom) "Age" else NULL)
      } else {
        p <- p + labs(y = NULL, x = if (is_bottom) dim_name else NULL)
      }

      panels[[paste0(sv, "_", dim_name)]] <- p
    }
  }

  n_surveys <- length(survey_order)
  combined <- wrap_plots(panels, ncol = 3, nrow = n_surveys, byrow = TRUE) +
    plot_annotation(
      title    = "Median Polish APC Effects Across Surveys",
      subtitle = "Age (blue) | Period (green) | Cohort (pink) — mean residuals + linear trend",
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40")
      )
    )

  combined
}


# ==============================================================================
# Plotting Functions
# ==============================================================================

# APC dimension colors (consistent with 03_apc_analysis.R)
APC_COLORS <- c(
  "Age"    = "#0072B2",
  "Period" = "#009E73",
  "Cohort" = "#CC79A7"
)


#' Plot median polish effects (3-panel: Age | Period | Cohort)
#'
#' @param effects_df Tibble from extract_medpolish_effects()
#' @param survey Character: survey name for title
#' @return patchwork object
plot_medpolish_effects <- function(effects_df, survey) {

  make_panel <- function(dim_name) {
    df_dim <- effects_df |> filter(dimension == dim_name)
    color <- APC_COLORS[[dim_name]]

    ggplot(df_dim, aes(x = value, y = effect)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(color = color, size = 2.5) +
      geom_smooth(method = "lm", se = TRUE, color = color,
                  fill = color, alpha = 0.15, linewidth = 0.8) +
      labs(
        x = dim_name,
        y = "Residual Effect (SRH units)",
        title = dim_name
      ) +
      theme_srh() +
      theme(plot.title = element_text(color = color))
  }

  p_age    <- make_panel("Age")
  p_period <- make_panel("Period")
  p_cohort <- make_panel("Cohort")

  combined <- p_age + p_period + p_cohort +
    plot_annotation(
      title    = paste0("Median Polish APC Effects: ", toupper(survey)),
      subtitle = "Mean residuals from two-way median polish decomposition",
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40")
      )
    )

  combined
}


#' Plot median polish residual diagnostics
#'
#' @param mp_result List from run_medpolish_apc()
#' @param survey Character: survey name for title
#' @return patchwork object with 3 panels (AP, AC, PC residuals)
plot_medpolish_residuals <- function(mp_result, survey) {

  make_resid_panel <- function(slice_name, x_dim, x_label) {
    resid_df <- mp_result$residuals[[slice_name]]

    ggplot(resid_df, aes(x = .data[[x_dim]], y = residual)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(alpha = 0.4, size = 1.5, color = "gray40") +
      stat_summary(fun = mean, geom = "line", color = "firebrick",
                   linewidth = 0.8, group = 1) +
      stat_summary(fun = mean, geom = "point", color = "firebrick", size = 2) +
      labs(x = x_label, y = "Residual", title = paste0(slice_name, " Residuals")) +
      theme_srh()
  }

  p1 <- make_resid_panel("AP", "cohort_midpoint", "Cohort")
  p2 <- make_resid_panel("AC", "period_midpoint", "Period")
  p3 <- make_resid_panel("PC", "age_midpoint", "Age")

  combined <- p1 + p2 + p3 +
    plot_annotation(
      title    = paste0("Median Polish Residual Diagnostics: ", toupper(survey)),
      subtitle = "Cell residuals with mean trend (red line)",
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40")
      )
    )

  combined
}


# ==============================================================================
# Combined All-Survey Figures: Direct Effects & Residuals
# ==============================================================================

#' Combined direct effects figure: 6 surveys x 3 dimensions
#'
#' @param all_direct Tibble with columns: value, effect, dimension,
#'   source_slice, source_type, survey
#' @param survey_order Character vector for row ordering
#' @return patchwork object
plot_direct_effects_all_surveys <- function(all_direct,
                                            survey_order = c("GSS", "NHANES", "MEPS",
                                                             "NHIS", "CPS", "BRFSS")) {
  all_direct <- all_direct |>
    mutate(survey = factor(survey, levels = survey_order))

  panels <- list()

  for (sv in survey_order) {
    for (dim_name in c("Age", "Period", "Cohort")) {
      df_cell <- all_direct |>
        filter(survey == sv, dimension == dim_name)

      color <- APC_COLORS[[dim_name]]
      is_bottom <- (sv == tail(survey_order, 1))
      is_left   <- (dim_name == "Age")
      is_top    <- (sv == survey_order[1])

      p <- ggplot(df_cell, aes(x = value, y = effect,
                               shape = source_slice,
                               linetype = source_slice)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = "gray60", linewidth = 0.3) +
        geom_line(color = color, linewidth = 0.6) +
        geom_point(color = color, size = 1.5) +
        scale_shape_manual(values = c("AP" = 16, "AC" = 17, "PC" = 15)) +
        scale_linetype_manual(values = c("AP" = "solid", "AC" = "dashed",
                                         "PC" = "dotted")) +
        theme_srh(base_size = 11) +
        theme(
          plot.margin  = margin(2, 4, 2, 4),
          legend.position = "none",
          axis.title.x = if (is_bottom) element_text(size = 10) else element_blank(),
          axis.title.y = if (is_left) element_text(size = 10) else element_blank(),
          plot.title   = if (is_top) element_text(size = 11, color = color, face = "bold") else element_blank()
        )

      if (is_top) p <- p + ggtitle(dim_name)
      if (is_left) {
        p <- p + labs(y = sv, x = if (is_bottom) "Age" else NULL)
      } else {
        p <- p + labs(y = NULL, x = if (is_bottom) dim_name else NULL)
      }

      panels[[paste0(sv, "_", dim_name)]] <- p
    }
  }

  # Build a small legend from one panel's data
  legend_df <- tibble(
    x = c(1, 1, 1), y = c(1, 1, 1),
    source_slice = factor(c("AP", "AC", "PC"), levels = c("AP", "AC", "PC"))
  )
  p_legend <- ggplot(legend_df, aes(x, y, shape = source_slice,
                                    linetype = source_slice)) +
    geom_point() +
    scale_shape_manual(values = c("AP" = 16, "AC" = 17, "PC" = 15),
                       name = "Source slice") +
    scale_linetype_manual(values = c("AP" = "solid", "AC" = "dashed",
                                     "PC" = "dotted"),
                          name = "Source slice") +
    theme_void() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 10))

  legend_grob <- cowplot::get_legend(p_legend)

  n_surveys <- length(survey_order)
  grid <- wrap_plots(panels, ncol = 3, nrow = n_surveys, byrow = TRUE)

  # Use patchwork inset for legend if cowplot not available, else manual
  combined <- grid +
    plot_annotation(
      title    = "Direct Median Polish Effects Across Surveys",
      subtitle = "Row/column effects from medpolish; two source slices per dimension (solid vs dashed)",
      caption  = "Shapes: circle = AP slice, triangle = AC slice, square = PC slice",
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        plot.caption  = element_text(size = 10, color = "gray50")
      )
    )

  combined
}


#' Combined residual diagnostics figure: 6 surveys x 3 slices
#'
#' @param all_results List of per-survey results from run_all_medpolish()
#' @param survey_order Character vector for row ordering (lowercase)
#' @return patchwork object
plot_residuals_all_surveys <- function(all_results,
                                       survey_order = c("gss", "nhanes", "meps",
                                                        "nhis", "cps", "brfss")) {
  panels <- list()

  # Slice definitions: slice_name -> (x dimension column, x label)
  slice_defs <- list(
    AP = list(x_dim = "cohort_midpoint", x_label = "Cohort"),
    AC = list(x_dim = "period_midpoint", x_label = "Period"),
    PC = list(x_dim = "age_midpoint",    x_label = "Age")
  )

  for (sv in survey_order) {
    mp_result <- all_results[[sv]]$mp_result

    for (slice_name in c("AP", "AC", "PC")) {
      resid_df <- mp_result$residuals[[slice_name]]
      x_dim    <- slice_defs[[slice_name]]$x_dim
      x_label  <- slice_defs[[slice_name]]$x_label

      is_bottom <- (sv == tail(survey_order, 1))
      is_left   <- (slice_name == "AP")
      is_top    <- (sv == survey_order[1])

      # Color for the dimension revealed by this slice's residuals
      resid_color <- APC_COLORS[[x_label]]

      p <- ggplot(resid_df, aes(x = .data[[x_dim]], y = residual)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = "gray60", linewidth = 0.3) +
        geom_point(alpha = 0.35, size = 1, color = "gray50") +
        stat_summary(fun = mean, geom = "line",
                     color = resid_color, linewidth = 0.7, group = 1) +
        stat_summary(fun = mean, geom = "point",
                     color = resid_color, size = 1.5) +
        theme_srh(base_size = 11) +
        theme(
          plot.margin  = margin(2, 4, 2, 4),
          axis.title.x = if (is_bottom) element_text(size = 10) else element_blank(),
          axis.title.y = if (is_left) element_text(size = 10) else element_blank(),
          plot.title   = if (is_top) element_text(size = 11, face = "bold") else element_blank()
        )

      # Column headers on top row
      if (is_top) {
        p <- p + ggtitle(paste0(slice_name, " Residuals (", x_label, ")"))
      }

      # Row label on left column
      if (is_left) {
        p <- p + labs(y = toupper(sv), x = if (is_bottom) x_label else NULL)
      } else {
        p <- p + labs(y = NULL, x = if (is_bottom) x_label else NULL)
      }

      panels[[paste0(sv, "_", slice_name)]] <- p
    }
  }

  n_surveys <- length(survey_order)
  combined <- wrap_plots(panels, ncol = 3, nrow = n_surveys, byrow = TRUE) +
    plot_annotation(
      title    = "Median Polish Residual Diagnostics Across Surveys",
      subtitle = "Cell residuals (gray) with mean trend (colored line) per dimension",
      theme = theme(
        plot.title    = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40")
      )
    )

  combined
}


#' Plot one APC dimension across all surveys (faceted)
#'
#' @param all_effects Tibble with columns: value, effect, n_cells, dimension, survey
#' @param dimension Character: "Age", "Period", or "Cohort"
#' @return ggplot object
plot_medpolish_all_surveys <- function(all_effects, dimension) {

  df_dim <- all_effects |>
    filter(.data$dimension == .env$dimension)

  color <- APC_COLORS[[dimension]]

  ggplot(df_dim, aes(x = value, y = effect)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(color = color, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = color,
                fill = color, alpha = 0.15, linewidth = 0.7) +
    facet_wrap(~ survey, scales = "free", ncol = 3) +
    labs(
      x = dimension,
      y = "Residual Effect (SRH units)",
      title = paste0("Median Polish ", dimension, " Effects Across Surveys"),
      subtitle = "Mean residuals from two-way median polish; scales vary by survey"
    ) +
    theme_srh() +
    theme(
      strip.text  = element_text(size = rel(0.9), face = "bold"),
      plot.title  = element_text(color = color)
    )
}
