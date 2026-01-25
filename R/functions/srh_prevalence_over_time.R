# ==============================================================================
# srh_prevalence_over_time.R
# Compute Survey-Weighted Prevalence of Dichotomized SRH Over Time
#
# Purpose: For sensitivity analyses using dichotomized SRH outcomes.
#          Computes prevalence (proportion) of dichotomized SRH variable
#          by age group and year, analogous to mean SRH in Figure 1 Panel A.
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(survey)
library(srvyr)

# ------------------------------------------------------------------------------
# PREVALENCE SUMMARIZATION FUNCTION
# ------------------------------------------------------------------------------

#' Compute survey-weighted prevalence of dichotomized SRH by age group and year
#'
#' @description
#' Computes survey-weighted mean (proportion) of a binary SRH variable by
#' age group and year. This is analogous to summarize_srh_over_time() but
#' for dichotomized outcomes.
#'
#' @param data Data frame with columns: binary SRH variable, age_group, year, wt
#'             (and optional psu, strata)
#' @param survey_name Character string for labeling output (e.g., "NHIS")
#' @param srh_dichot_var Name of dichotomized SRH variable (default "srh_dichot")
#' @param age_group_var Name of age group variable (default "age_group")
#' @param year_var Name of year variable (default "year")
#' @param psu_var Name of PSU variable (default "psu", NULL if not available)
#' @param strata_var Name of strata variable (default "strata", NULL if not available)
#' @param wt_var Name of weight variable (default "wt")
#' @param ci_level Confidence level for intervals (default 0.95)
#' @param lonely_psu How to handle single-PSU strata (default "adjust")
#'
#' @return List with:
#'   - estimates: Data frame with age_group, year, prevalence, se, ci_lower, ci_upper
#'   - plot: Basic ggplot of prevalence by age group over time
#'
#' @details
#' The dichotomized SRH variable should be coded as 0/1 or logical (FALSE/TRUE).
#' Higher values should indicate "better" health (e.g., 1 = Good+ health).
#' The function computes the proportion with value = 1.
#'
summarize_srh_prevalence_over_time <- function(
    data,
    survey_name,
    srh_dichot_var = "srh_dichot",
    age_group_var = "age_group",
    year_var = "year",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    ci_level = 0.95,
    lonely_psu = "adjust",
    title_style = c("descriptive", "dataset")
) {

  title_style <- match.arg(title_style)

  # --- Input validation ---
  stopifnot(is.data.frame(data))
  stopifnot(srh_dichot_var %in% names(data))
  stopifnot(age_group_var %in% names(data))
  stopifnot(year_var %in% names(data))
  stopifnot(wt_var %in% names(data))

  # --- Set survey options ---
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # --- Check which design elements are available ---
  has_psu <- !is.null(psu_var) && psu_var %in% names(data)
  has_strata <- !is.null(strata_var) && strata_var %in% names(data)

  if (!has_psu || !has_strata) {
    message("Note: ", survey_name, " - Using weights-only design. ",
            "PSU: ", has_psu, ", Strata: ", has_strata)
  }

  # --- Prepare data ---
  # Standardize column names using rename
  rename_exprs <- list(
    .srh_dichot = rlang::sym(srh_dichot_var),
    .age_group = rlang::sym(age_group_var),
    .year = rlang::sym(year_var),
    .wt = rlang::sym(wt_var)
  )
  if (has_psu) rename_exprs$.psu <- rlang::sym(psu_var)
  if (has_strata) rename_exprs$.strata <- rlang::sym(strata_var)

  df_std <- data %>%
    rename(!!!rename_exprs) %>%
    filter(
      !is.na(.srh_dichot),
      !is.na(.age_group),
      !is.na(.year),
      !is.na(.wt), is.finite(.wt), .wt > 0
    )

  if (has_psu) df_std <- df_std %>% filter(!is.na(.psu))
  if (has_strata) df_std <- df_std %>% filter(!is.na(.strata))

  stopifnot(nrow(df_std) > 0)

  # Ensure dichotomized variable is numeric 0/1
  df_std <- df_std %>%
    mutate(.srh_dichot = as.numeric(.srh_dichot))

  # Check values are 0 or 1
  unique_vals <- unique(df_std$.srh_dichot)
  if (!all(unique_vals %in% c(0, 1))) {
    stop("Dichotomized SRH variable must contain only 0 and 1 values. ",
         "Found: ", paste(unique_vals, collapse = ", "))
  }

  # --- Create survey design ---
  if (has_psu && has_strata) {
    svy <- df_std %>%
      srvyr::as_survey_design(ids = .psu, strata = .strata, weights = .wt, nest = TRUE)
  } else if (has_psu) {
    svy <- df_std %>%
      srvyr::as_survey_design(ids = .psu, weights = .wt)
  } else if (has_strata) {
    svy <- df_std %>%
      srvyr::as_survey_design(ids = 1, strata = .strata, weights = .wt)
  } else {
    svy <- df_std %>%
      srvyr::as_survey_design(ids = 1, weights = .wt)
  }

  # --- Compute prevalence by age group and year ---
  estimates <- svy %>%
    mutate(.age_group = as.factor(.age_group)) %>%
    group_by(.age_group, .year) %>%
    summarise(
      prevalence = survey_mean(.srh_dichot, vartype = c("se", "ci"), level = ci_level, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      age_group = .age_group,
      year = .year,
      prevalence = prevalence,
      se = prevalence_se,
      ci_lower = prevalence_low,
      ci_upper = prevalence_upp
    ) %>%
    arrange(age_group, year)

  stopifnot(nrow(estimates) > 0)

  # --- Build basic plot ---
  source(here::here("R", "functions", "theme_srh.R"))

  plot_title <- if (title_style == "descriptive") {
    paste0(survey_name, ": Prevalence by age group")
  } else {
    survey_name
  }

  p <- ggplot(estimates, aes(x = year, y = prevalence, color = age_group, group = age_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    scale_color_manual(values = age_colors) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    labs(
      title = plot_title,
      x = "Survey year",
      y = "Prevalence",
      color = "Age group"
    ) +
    theme_minimal()

  list(estimates = estimates, plot = p)
}


# ------------------------------------------------------------------------------
# CHUNKED VERSION FOR LARGE DATASETS (e.g., BRFSS)
# ------------------------------------------------------------------------------

#' Compute survey-weighted prevalence by age group and year (chunked processing)
#'
#' @description
#' Processes large datasets year-by-year AND age-group-by-age-group to avoid
#' memory limits. Returns same structure as summarize_srh_prevalence_over_time().
#'
#' @inheritParams summarize_srh_prevalence_over_time
#' @param cache_dir Optional directory to cache intermediate results
#' @param force_recompute Force recomputation even if cache exists
#'
#' @return List with estimates data frame and basic plot
#'
summarize_srh_prevalence_over_time_chunked <- function(
    data,
    survey_name,
    srh_dichot_var = "srh_dichot",
    age_group_var = "age_group",
    year_var = "year",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt",
    ci_level = 0.95,
    lonely_psu = "adjust",
    title_style = c("descriptive", "dataset"),
    cache_dir = NULL,
    force_recompute = FALSE
) {

  title_style <- match.arg(title_style)

  # Optionally cache intermediate results
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    cache_file <- file.path(cache_dir, paste0(survey_name, "_", srh_dichot_var, "_prev.rds"))
    if (file.exists(cache_file) && !force_recompute) {
      message("Loading cached results from: ", cache_file)
      estimates <- readr::read_rds(cache_file)
      return(.build_prevalence_plot(estimates, survey_name, title_style))
    }
  }

  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  # Get unique years and age groups
  years <- sort(unique(data[[year_var]]))
  age_groups <- sort(unique(data[[age_group_var]]))

  message("Processing ", length(years), " years x ", length(age_groups),
          " age groups for ", survey_name, " (", srh_dichot_var, ")...")

  # Check which design elements are available
  vars_present <- names(data)
  has_psu <- !is.null(psu_var) && psu_var %in% vars_present
  has_strata <- !is.null(strata_var) && strata_var %in% vars_present
  has_wt <- wt_var %in% vars_present
  stopifnot(has_wt)

  # Process each year x age_group combination separately
  results_list <- vector("list", length(years) * length(age_groups))
  idx <- 0

  for (i in seq_along(years)) {
    yr <- years[i]

    for (j in seq_along(age_groups)) {
      ag <- age_groups[j]
      idx <- idx + 1

      # Subset to single year AND age group using base R for memory efficiency
      row_mask <- data[[year_var]] == yr & data[[age_group_var]] == ag
      data_chunk <- data[row_mask, , drop = FALSE]

      # Skip if no data
      if (nrow(data_chunk) == 0) {
        next
      }

      # Filter to valid observations using base R
      valid_mask <- !is.na(data_chunk[[srh_dichot_var]]) &
        !is.na(data_chunk[[wt_var]]) &
        is.finite(data_chunk[[wt_var]]) &
        data_chunk[[wt_var]] > 0

      if (has_psu) valid_mask <- valid_mask & !is.na(data_chunk[[psu_var]])
      if (has_strata) valid_mask <- valid_mask & !is.na(data_chunk[[strata_var]])

      data_chunk <- data_chunk[valid_mask, , drop = FALSE]

      if (nrow(data_chunk) == 0) {
        next
      }

      # Create survey design based on available elements
      if (has_psu && has_strata) {
        svy <- survey::svydesign(
          ids = as.formula(paste0("~", psu_var)),
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_chunk,
          nest = TRUE
        )
      } else if (has_psu) {
        svy <- survey::svydesign(
          ids = as.formula(paste0("~", psu_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_chunk
        )
      } else if (has_strata) {
        svy <- survey::svydesign(
          ids = ~1,
          strata = as.formula(paste0("~", strata_var)),
          weights = as.formula(paste0("~", wt_var)),
          data = data_chunk
        )
      } else {
        svy <- survey::svydesign(
          ids = ~1,
          weights = as.formula(paste0("~", wt_var)),
          data = data_chunk
        )
      }

      # Compute mean (= prevalence for 0/1 variable) using survey package directly
      srh_formula <- as.formula(paste0("~", srh_dichot_var))
      mean_result <- survey::svymean(srh_formula, svy, na.rm = TRUE)
      ci_result <- confint(mean_result, level = ci_level)

      results_list[[idx]] <- data.frame(
        age_group = ag,
        year = yr,
        prevalence = as.numeric(coef(mean_result)),
        se = as.numeric(survey::SE(mean_result)),
        ci_lower = ci_result[1],
        ci_upper = ci_result[2],
        stringsAsFactors = FALSE
      )

      # Aggressive cleanup
      rm(data_chunk, svy, mean_result, ci_result, valid_mask, row_mask)
    }

    # Force garbage collection after each year
    gc(verbose = FALSE)
  }

  # Combine all results
  estimates <- dplyr::bind_rows(results_list) %>%
    mutate(age_group = factor(age_group, levels = age_groups)) %>%
    arrange(age_group, year)

  stopifnot(nrow(estimates) > 0)

  # Cache if requested
  if (!is.null(cache_dir)) {
    readr::write_rds(estimates, cache_file)
    message("Cached results to: ", cache_file)
  }

  .build_prevalence_plot(estimates, survey_name, title_style)
}


# Helper function to build the prevalence plot
.build_prevalence_plot <- function(estimates, survey_name, title_style) {

  # Source colors if needed
  if (!exists("age_colors")) {
    source(here::here("R", "functions", "theme_srh.R"))
  }

  plot_title <- if (title_style == "descriptive") {
    paste0(survey_name, ": Prevalence by age group")
  } else {
    survey_name
  }

  p <- ggplot(estimates, aes(x = year, y = prevalence, color = age_group, group = age_group)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    scale_color_manual(values = age_colors) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
    labs(
      title = plot_title,
      x = "Survey year",
      y = "Prevalence",
      color = "Age group"
    ) +
    theme_minimal()

  list(estimates = estimates, plot = p)
}
