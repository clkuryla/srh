# ==============================================================================
# summarize_srh_categories.R
# Functions to compute survey-weighted proportions of SRH categories
# Used for supplemental categorical distribution figures
# ==============================================================================

library(tidyverse)
library(srvyr)

# ------------------------------------------------------------------------------
# Main function: Compute SRH category proportions by age group and year
# ------------------------------------------------------------------------------

#' Summarize SRH category proportions over time by age group
#'
#' @param data A data frame with survey data
#' @param survey_name String: name of the survey (e.g., "MEPS", "NHIS")
#' @param age_group_var Name of the age group variable (default: "age_group")
#' @param srh_var Name of the SRH variable (numeric 1-5, default: "srh")
#' @param year_var Name of the year variable (default: "year")
#' @param psu_var Name of the PSU variable (default: "psu", can be NULL)
#' @param strata_var Name of the strata variable (default: "strata", can be NULL)
#' @param wt_var Name of the weight variable (default: "wt")
#' @param srh_scale Either 5 (default) or 4 (for GSS)
#' @param lonely_psu How to handle lonely PSUs (default: "adjust")
#'
#' @return A tibble with columns: age_group, year, srh_cat, prop, se
#'   where prop is the proportion within each age_group x year cell
#'
#' @examples
#' # props <- summarize_srh_proportions(data_meps, "MEPS")
summarize_srh_proportions <- function(
  data,
  survey_name,
  age_group_var = "age_group",
  srh_var = "srh",
  year_var = "year",
  psu_var = "psu",
  strata_var = "strata",
  wt_var = "wt",
  srh_scale = 5,
  lonely_psu = "adjust"
) {

  stopifnot(srh_scale %in% c(4, 5))

  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  vars_present <- names(data)
  stopifnot(age_group_var %in% vars_present)
  stopifnot(srh_var %in% vars_present)
  stopifnot(year_var %in% vars_present)
  stopifnot(wt_var %in% vars_present)

  has_psu <- !is.null(psu_var) && psu_var %in% vars_present
  has_strata <- !is.null(strata_var) && strata_var %in% vars_present

  # Create SRH category labels based on scale
  if (srh_scale == 5) {
    srh_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")
  } else {
    srh_labels <- c("Poor", "Fair", "Good", "Excellent")
  }

  # Build rename list
  rename_exprs <- list(
    .age_group = rlang::sym(age_group_var),
    .srh = rlang::sym(srh_var),
    .year = rlang::sym(year_var),
    .wt = rlang::sym(wt_var)
  )
  if (has_psu) rename_exprs$.psu <- rlang::sym(psu_var)
  if (has_strata) rename_exprs$.strata <- rlang::sym(strata_var)

  # Prepare data
  df_std <- data %>%
    rename(!!!rename_exprs) %>%
    filter(
      !is.na(.srh), is.finite(.srh),
      .srh >= 1, .srh <= srh_scale,
      !is.na(.year), is.finite(.year),
      !is.na(.age_group),
      !is.na(.wt), is.finite(.wt), .wt > 0
    ) %>%
    mutate(
      .srh_cat = factor(.srh, levels = 1:srh_scale, labels = srh_labels)
    )

  if (has_psu) df_std <- df_std %>% filter(!is.na(.psu))
  if (has_strata) df_std <- df_std %>% filter(!is.na(.strata))

  stopifnot(nrow(df_std) > 0)

  # Create survey design
  if (has_psu && has_strata) {
    svy <- df_std %>%
      srvyr::as_survey_design(ids = .psu, strata = .strata, weights = .wt, nest = TRUE)
  } else if (has_psu) {
    message("Note: strata not available; using PSU + weights only design.")
    svy <- df_std %>%
      srvyr::as_survey_design(ids = .psu, weights = .wt)
  } else if (has_strata) {
    message("Note: PSU not available; using strata + weights only design.")
    svy <- df_std %>%
      srvyr::as_survey_design(ids = 1, strata = .strata, weights = .wt)
  } else {
    message("Note: PSU and strata not available; using weights-only design.")
    svy <- df_std %>%
      srvyr::as_survey_design(ids = 1, weights = .wt)
  }

  # Compute proportions within age_group x year
  estimates <- svy %>%
    group_by(.age_group, .year, .srh_cat) %>%
    summarise(
      prop = survey_prop(vartype = "se", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      age_group = .age_group,
      year = .year,
      srh_cat = .srh_cat,
      prop = prop,
      se = prop_se
    ) %>%
    arrange(age_group, year, srh_cat)

  stopifnot(nrow(estimates) > 0)

  estimates
}


# ------------------------------------------------------------------------------
# Compute age composition within SRH category (for Approach 2a)
# ------------------------------------------------------------------------------

#' Summarize age group composition within each SRH category over time
#'
#' @description For each SRH category and year, computes what proportion
#'   of respondents reporting that category come from each age group.
#'   Age groups sum to 100% within each srh_cat x year.
#'
#' @inheritParams summarize_srh_proportions
#'
#' @return A tibble with columns: srh_cat, year, age_group, prop, se
summarize_age_composition_by_srh <- function(
  data,
  survey_name,
  age_group_var = "age_group",
  srh_var = "srh",
  year_var = "year",
  psu_var = "psu",
  strata_var = "strata",
  wt_var = "wt",
  srh_scale = 5,
  lonely_psu = "adjust"
) {

  stopifnot(srh_scale %in% c(4, 5))

  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  vars_present <- names(data)
  stopifnot(age_group_var %in% vars_present)
  stopifnot(srh_var %in% vars_present)
  stopifnot(year_var %in% vars_present)
  stopifnot(wt_var %in% vars_present)

  has_psu <- !is.null(psu_var) && psu_var %in% vars_present
  has_strata <- !is.null(strata_var) && strata_var %in% vars_present

  if (srh_scale == 5) {
    srh_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")
  } else {
    srh_labels <- c("Poor", "Fair", "Good", "Excellent")
  }

  rename_exprs <- list(
    .age_group = rlang::sym(age_group_var),
    .srh = rlang::sym(srh_var),
    .year = rlang::sym(year_var),
    .wt = rlang::sym(wt_var)
  )
  if (has_psu) rename_exprs$.psu <- rlang::sym(psu_var)
  if (has_strata) rename_exprs$.strata <- rlang::sym(strata_var)

  df_std <- data %>%
    rename(!!!rename_exprs) %>%
    filter(
      !is.na(.srh), is.finite(.srh),
      .srh >= 1, .srh <= srh_scale,
      !is.na(.year), is.finite(.year),
      !is.na(.age_group),
      !is.na(.wt), is.finite(.wt), .wt > 0
    ) %>%
    mutate(
      .srh_cat = factor(.srh, levels = 1:srh_scale, labels = srh_labels)
    )

  if (has_psu) df_std <- df_std %>% filter(!is.na(.psu))
  if (has_strata) df_std <- df_std %>% filter(!is.na(.strata))

  stopifnot(nrow(df_std) > 0)

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

  # Group by srh_cat and year first, THEN by age_group
  # This makes age_group proportions sum to 100% within each srh_cat x year
  estimates <- svy %>%
    group_by(.srh_cat, .year, .age_group) %>%
    summarise(
      prop = survey_prop(vartype = "se", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      srh_cat = .srh_cat,
      year = .year,
      age_group = .age_group,
      prop = prop,
      se = prop_se
    ) %>%
    arrange(srh_cat, year, age_group)

  stopifnot(nrow(estimates) > 0)

  estimates
}


# ------------------------------------------------------------------------------
# Compute variance/entropy of SRH distribution (for Approach 6)
# ------------------------------------------------------------------------------

#' Compute variance and entropy of SRH distribution by age group and year
#'
#' @description Calculates the weighted variance and Shannon entropy of the
#'   SRH distribution for each age group and year. These measure the "spread"
#'   or polarization of responses.
#'
#' @inheritParams summarize_srh_proportions
#'
#' @return A tibble with columns: age_group, year, variance, entropy, n_weighted
summarize_srh_spread <- function(
  data,
  survey_name,
  age_group_var = "age_group",
  srh_var = "srh",
  year_var = "year",
  psu_var = "psu",
  strata_var = "strata",
  wt_var = "wt",
  srh_scale = 5,
  lonely_psu = "adjust"
) {

  stopifnot(srh_scale %in% c(4, 5))

  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = lonely_psu)
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  vars_present <- names(data)
  stopifnot(age_group_var %in% vars_present)
  stopifnot(srh_var %in% vars_present)
  stopifnot(year_var %in% vars_present)
  stopifnot(wt_var %in% vars_present)

  has_psu <- !is.null(psu_var) && psu_var %in% vars_present
  has_strata <- !is.null(strata_var) && strata_var %in% vars_present

  rename_exprs <- list(
    .age_group = rlang::sym(age_group_var),
    .srh = rlang::sym(srh_var),
    .year = rlang::sym(year_var),
    .wt = rlang::sym(wt_var)
  )
  if (has_psu) rename_exprs$.psu <- rlang::sym(psu_var)
  if (has_strata) rename_exprs$.strata <- rlang::sym(strata_var)

  df_std <- data %>%
    rename(!!!rename_exprs) %>%
    filter(
      !is.na(.srh), is.finite(.srh),
      .srh >= 1, .srh <= srh_scale,
      !is.na(.year), is.finite(.year),
      !is.na(.age_group),
      !is.na(.wt), is.finite(.wt), .wt > 0
    )

  if (has_psu) df_std <- df_std %>% filter(!is.na(.psu))
  if (has_strata) df_std <- df_std %>% filter(!is.na(.strata))

  stopifnot(nrow(df_std) > 0)

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

  # First get category proportions for entropy calculation
  props <- svy %>%
    group_by(.age_group, .year, .srh) %>%
    summarise(
      prop = survey_prop(na.rm = TRUE),
      .groups = "drop"
    )

  # Compute entropy: -sum(p * log(p)) for each age_group x year
  entropy_df <- props %>%
    group_by(.age_group, .year) %>%
    summarise(
      entropy = -sum(ifelse(prop > 0, prop * log(prop), 0)),
      .groups = "drop"
    )

  # Compute weighted variance of SRH
  variance_df <- svy %>%
    group_by(.age_group, .year) %>%
    summarise(
      variance = survey_var(.srh, na.rm = TRUE),
      n_weighted = survey_total(1),
      .groups = "drop"
    )

  # Combine
  estimates <- variance_df %>%
    left_join(entropy_df, by = c(".age_group", ".year")) %>%
    transmute(
      age_group = .age_group,
      year = .year,
      variance = variance,
      entropy = entropy,
      n_weighted = n_weighted
    ) %>%
    arrange(age_group, year)

  stopifnot(nrow(estimates) > 0)

  estimates
}
