# ==============================================================================
# 06_hosp_util_analysis.R
# Hospital Utilization Validity Analysis
#
# Purpose:
#   This is a VALIDITY TEST: Does self-rated health predict actual healthcare
#   utilization? We show that:
#   1. Coefficients: SRH predicts utilization consistently over time (SRH meaning
#      is stable)
#   2. Prevalence: How utilization patterns are changing by age group
#
# Model direction: utilization ~ SRH (NOT SRH ~ utilization)
#   - Negative coefficient = better SRH (higher value) -> less utilization
#   - This is the expected pattern for a valid SRH measure
#
# Variables analyzed (7 measures):
#   - hospitalized: Any hospital stay past year (binary)
#   - any_er: Any ER visit past year (binary)
#   - er_visits: Number of ER visits (midpoint-imputed from categorical bands)
#   - home_care: Received home care (binary)
#   - has_usual_care: Has usual place of care (binary)
#   - uninsured: No health insurance (binary)
#   - sickdays: Sick leave days (count)
#
# Note: 2019 NHIS redesign may cause discontinuity in some variables
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(srvyr)
library(here)
library(patchwork)

# Source paths and shared functions
source(here("R", "paths.R"))
source(here("R", "srh_common_functions.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "plot_utils.R"))
source(here("R", "functions", "regress_outcome_on_srh.R"))
source(here("R", "functions", "prevalence_by_age_year.R"))

# Set theme
theme_set(theme_srh())

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)

# Output directories
output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")

# Age group levels (Scheme B)
AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")


# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Which surveys to run
run_nhis <- TRUE
run_meps <- TRUE

# NHIS year filter - include all years with utilization data
# Note: 2019 redesign may cause discontinuity
nhis_start_year <- 1997

# Minimum sample size per age-year cell
min_n <- 50

# Hospital utilization variables common to NHIS and MEPS
# Note: sickdays and home_care removed (not available in MEPS)
util_vars_common <- c(
  hospitalized    = "Hospitalized",
  any_er          = "Any ER Visit",
  er_visits       = "ER Visits (count)",
  has_usual_care  = "Usual Care",
  uninsured       = "Uninsured"
)

# NHIS-only variables (retain for backward compatibility)
util_vars_nhis_only <- c(
  home_care       = "Home Care"
)

# MEPS satisfaction variables
satisfaction_vars <- c(
  provider_listens    = "Provider Listens",
  provider_respect    = "Provider Respect",
  provider_explains   = "Provider Explains",
  provider_confidence = "Provider Confidence"
)

# Combined for NHIS (includes home_care)
util_vars <- c(util_vars_common, util_vars_nhis_only)


# ==============================================================================
# PART 1: LOAD DATA
# ==============================================================================

message("\n========== Loading survey data ==========\n")

# Helper function to load survey data
load_survey_data <- function(survey_name, wrangling_script) {
  rds_path <- derived_path(paste0("data_", tolower(survey_name), ".rds"))

  if (file.exists(rds_path)) {
    message("Loading ", survey_name, " from saved RDS: ", rds_path)
    data <- readr::read_rds(rds_path)
  } else {
    message("Wrangling ", survey_name, " from raw data...")
    source(wrangling_script, local = TRUE)
    data <- get(paste0("data_", tolower(survey_name)))
  }

  message("  ", survey_name, ": ", nrow(data), " rows, years ",
          min(data$year), "-", max(data$year))
  return(data)
}

# Load NHIS
if (run_nhis) {
  data_nhis <- load_survey_data("NHIS",
    here("analysis", "data_wrangling_code", "wrangle_nhis.R"))
}

# Load MEPS (future)
if (run_meps) {
  data_meps <- load_survey_data("MEPS",
    here("analysis", "data_wrangling_code", "wrangle_meps.R"))
}


# ==============================================================================
# PART 2: PROCESS AND VERIFY DATA
# ==============================================================================

message("\n========== Processing data ==========\n")

# --- NHIS ---
if (run_nhis && exists("data_nhis")) {
  message("Processing NHIS...")

  # Filter to start year
  data_nhis <- data_nhis %>% filter(year >= nhis_start_year)

  # Add age group (scheme B) if not already present
  if (!"age_group" %in% names(data_nhis)) {
    data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")
  }

  message("  NHIS: ", nrow(data_nhis), " rows, years ",
          min(data_nhis$year), "-", max(data_nhis$year))

  # Check utilization variable availability
  message("\n  Utilization variable availability:")
  for (var in names(util_vars)) {
    if (var %in% names(data_nhis)) {
      n_valid <- sum(!is.na(data_nhis[[var]]))
      pct_valid <- round(100 * n_valid / nrow(data_nhis), 1)
      mean_val <- round(mean(data_nhis[[var]], na.rm = TRUE), 3)
      message("    ", var, ": ", n_valid, " valid (", pct_valid, "%), mean = ", mean_val)
    } else {
      message("    ", var, ": NOT FOUND")
    }
  }
}

# --- MEPS ---
if (run_meps && exists("data_meps")) {
  message("\nProcessing MEPS...")

  # Add age group (scheme B) if not already present
  if (!"age_group" %in% names(data_meps)) {
    data_meps <- add_age_group(data_meps, age_var = age, scheme = "B")
  }

  message("  MEPS: ", nrow(data_meps), " rows, years ",
          min(data_meps$year), "-", max(data_meps$year))

  # Check utilization variable availability
  message("\n  Utilization variable availability:")
  for (var in names(util_vars_common)) {
    if (var %in% names(data_meps)) {
      n_valid <- sum(!is.na(data_meps[[var]]))
      pct_valid <- round(100 * n_valid / nrow(data_meps), 1)
      mean_val <- round(mean(data_meps[[var]], na.rm = TRUE), 3)
      message("    ", var, ": ", n_valid, " valid (", pct_valid, "%), mean = ", mean_val)
    } else {
      message("    ", var, ": NOT FOUND")
    }
  }

  # Check satisfaction variable availability
  message("\n  Satisfaction variable availability:")
  for (var in names(satisfaction_vars)) {
    if (var %in% names(data_meps)) {
      n_valid <- sum(!is.na(data_meps[[var]]))
      pct_valid <- round(100 * n_valid / nrow(data_meps), 1)
      mean_val <- round(mean(data_meps[[var]], na.rm = TRUE), 3)
      message("    ", var, ": ", n_valid, " valid (", pct_valid, "%), mean = ", mean_val)
    } else {
      message("    ", var, ": NOT FOUND")
    }
  }
}


# ==============================================================================
# PART 3: RUN AGE-STRATIFIED REGRESSIONS (utilization ~ SRH)
# ==============================================================================
# This is a validity test: we regress utilization ON SRH to show that
# SRH predicts healthcare-seeking behavior. A negative coefficient means
# better SRH (higher value) -> less utilization, which is the expected pattern.
# ==============================================================================

message("\n========== Running regressions: utilization ~ SRH ==========\n")

coef_nhis <- NULL
coef_meps <- NULL

# --- NHIS Regressions ---
if (run_nhis && exists("data_nhis")) {
  message("\n--- NHIS: utilization ~ SRH Coefficients ---")

  coef_list <- list()

  for (i in seq_along(util_vars)) {
    var_name <- names(util_vars)[i]
    var_label <- util_vars[i]

    if (!var_name %in% names(data_nhis)) {
      message("  Skipping ", var_name, ": not found in data")
      next
    }

    # NOTE: Using regress_outcome_on_srh_by_age_year which fits:
    #   outcome ~ SRH (NOT SRH ~ outcome)
    # This tests whether SRH predicts utilization
    coef_result <- regress_outcome_on_srh_by_age_year(
      data = data_nhis,
      outcome_var = var_name,
      outcome_label = var_label,
      survey_name = "NHIS",
      psu_var = NULL,    # Using weights-only for computational efficiency
      strata_var = NULL,
      min_n = min_n
    )

    if (!is.null(coef_result)) {
      coef_list[[var_name]] <- coef_result %>%
        mutate(category = var_label)
    }
  }

  coef_nhis <- bind_rows(coef_list)
  message("\n  Total coefficient results: ", nrow(coef_nhis), " age-year-variable cells")
}

# --- MEPS Regressions ---
if (run_meps && exists("data_meps")) {
  message("\n--- MEPS: utilization ~ SRH Coefficients ---")

  coef_list_meps <- list()

  # Run regressions for common utilization variables
  for (i in seq_along(util_vars_common)) {
    var_name <- names(util_vars_common)[i]
    var_label <- util_vars_common[i]

    if (!var_name %in% names(data_meps)) {
      message("  Skipping ", var_name, ": not found in data")
      next
    }

    coef_result <- regress_outcome_on_srh_by_age_year(
      data = data_meps,
      outcome_var = var_name,
      outcome_label = var_label,
      survey_name = "MEPS",
      psu_var = NULL,    # Using weights-only for computational efficiency
      strata_var = NULL,
      min_n = min_n
    )

    if (!is.null(coef_result)) {
      coef_list_meps[[var_name]] <- coef_result %>%
        mutate(category = var_label)
    }
  }

  coef_meps <- bind_rows(coef_list_meps)
  message("\n  Total MEPS coefficient results: ", nrow(coef_meps), " age-year-variable cells")
}

# --- MEPS Satisfaction Regressions ---
coef_satisfaction <- NULL
if (run_meps && exists("data_meps")) {
  message("\n--- MEPS: satisfaction ~ SRH Coefficients ---")

  coef_list_satisfaction <- list()

  for (i in seq_along(satisfaction_vars)) {
    var_name <- names(satisfaction_vars)[i]
    var_label <- satisfaction_vars[i]

    if (!var_name %in% names(data_meps)) {
      message("  Skipping ", var_name, ": not found in data")
      next
    }

    coef_result <- regress_outcome_on_srh_by_age_year(
      data = data_meps,
      outcome_var = var_name,
      outcome_label = var_label,
      survey_name = "MEPS",
      psu_var = NULL,
      strata_var = NULL,
      min_n = min_n
    )

    if (!is.null(coef_result)) {
      coef_list_satisfaction[[var_name]] <- coef_result %>%
        mutate(category = var_label)
    }
  }

  coef_satisfaction <- bind_rows(coef_list_satisfaction)
  message("\n  Total satisfaction coefficient results: ", nrow(coef_satisfaction), " age-year-variable cells")
}


# ==============================================================================
# PART 4: RUN PREVALENCE/MEAN CALCULATIONS
# ==============================================================================

message("\n========== Computing prevalence/means by age group ==========\n")

prev_nhis <- NULL
prev_meps <- NULL

# --- NHIS Prevalence ---
if (run_nhis && exists("data_nhis")) {
  message("\n--- NHIS Utilization Prevalence ---")

  prev_list <- list()

  for (i in seq_along(util_vars)) {
    var_name <- names(util_vars)[i]
    var_label <- util_vars[i]

    if (!var_name %in% names(data_nhis)) {
      message("  Skipping ", var_name, ": not found in data")
      next
    }

    prev_result <- mean_by_age_year(
      data = data_nhis,
      var_name = var_name,
      var_label = var_label,
      survey_name = "NHIS",
      psu_var = NULL,
      strata_var = NULL,
      min_n = min_n
    )

    if (!is.null(prev_result)) {
      prev_list[[var_name]] <- prev_result %>%
        mutate(category = var_label)
    }
  }

  prev_nhis <- bind_rows(prev_list)
  message("\n  Total prevalence results: ", nrow(prev_nhis), " age-year-variable cells")
}

# --- MEPS Prevalence ---
if (run_meps && exists("data_meps")) {
  message("\n--- MEPS Utilization Prevalence ---")

  prev_list_meps <- list()

  for (i in seq_along(util_vars_common)) {
    var_name <- names(util_vars_common)[i]
    var_label <- util_vars_common[i]

    if (!var_name %in% names(data_meps)) {
      message("  Skipping ", var_name, ": not found in data")
      next
    }

    prev_result <- mean_by_age_year(
      data = data_meps,
      var_name = var_name,
      var_label = var_label,
      survey_name = "MEPS",
      psu_var = NULL,
      strata_var = NULL,
      min_n = min_n
    )

    if (!is.null(prev_result)) {
      prev_list_meps[[var_name]] <- prev_result %>%
        mutate(category = var_label)
    }
  }

  prev_meps <- bind_rows(prev_list_meps)
  message("\n  Total MEPS prevalence results: ", nrow(prev_meps), " age-year-variable cells")
}

# --- MEPS Satisfaction Prevalence ---
prev_satisfaction <- NULL
if (run_meps && exists("data_meps")) {
  message("\n--- MEPS Satisfaction Prevalence ---")

  prev_list_satisfaction <- list()

  for (i in seq_along(satisfaction_vars)) {
    var_name <- names(satisfaction_vars)[i]
    var_label <- satisfaction_vars[i]

    if (!var_name %in% names(data_meps)) {
      message("  Skipping ", var_name, ": not found in data")
      next
    }

    prev_result <- mean_by_age_year(
      data = data_meps,
      var_name = var_name,
      var_label = var_label,
      survey_name = "MEPS",
      psu_var = NULL,
      strata_var = NULL,
      min_n = min_n
    )

    if (!is.null(prev_result)) {
      prev_list_satisfaction[[var_name]] <- prev_result %>%
        mutate(category = var_label)
    }
  }

  prev_satisfaction <- bind_rows(prev_list_satisfaction)
  message("\n  Total satisfaction prevalence results: ", nrow(prev_satisfaction), " age-year-variable cells")
}


# ==============================================================================
# PART 5: SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

# NHIS Coefficients
if (!is.null(coef_nhis) && nrow(coef_nhis) > 0) {
  readr::write_csv(coef_nhis, file.path(tables_dir, paste0("fig_util_coef_nhis_", date_suffix, ".csv")))
  readr::write_rds(coef_nhis, file.path(tables_dir, paste0("fig_util_coef_nhis_", date_suffix, ".rds")))
  message("Saved: fig_util_coef_nhis_", date_suffix, " (.csv and .rds)")
}

# NHIS Prevalence
if (!is.null(prev_nhis) && nrow(prev_nhis) > 0) {
  readr::write_csv(prev_nhis, file.path(tables_dir, paste0("fig_util_prev_nhis_", date_suffix, ".csv")))
  readr::write_rds(prev_nhis, file.path(tables_dir, paste0("fig_util_prev_nhis_", date_suffix, ".rds")))
  message("Saved: fig_util_prev_nhis_", date_suffix, " (.csv and .rds)")
}

# MEPS Coefficients
if (!is.null(coef_meps) && nrow(coef_meps) > 0) {
  readr::write_csv(coef_meps, file.path(tables_dir, paste0("fig_util_coef_meps_", date_suffix, ".csv")))
  readr::write_rds(coef_meps, file.path(tables_dir, paste0("fig_util_coef_meps_", date_suffix, ".rds")))
  message("Saved: fig_util_coef_meps_", date_suffix, " (.csv and .rds)")
}

# MEPS Prevalence
if (!is.null(prev_meps) && nrow(prev_meps) > 0) {
  readr::write_csv(prev_meps, file.path(tables_dir, paste0("fig_util_prev_meps_", date_suffix, ".csv")))
  readr::write_rds(prev_meps, file.path(tables_dir, paste0("fig_util_prev_meps_", date_suffix, ".rds")))
  message("Saved: fig_util_prev_meps_", date_suffix, " (.csv and .rds)")
}

# Satisfaction Coefficients
if (!is.null(coef_satisfaction) && nrow(coef_satisfaction) > 0) {
  readr::write_csv(coef_satisfaction, file.path(tables_dir, paste0("fig_satisfaction_coef_", date_suffix, ".csv")))
  readr::write_rds(coef_satisfaction, file.path(tables_dir, paste0("fig_satisfaction_coef_", date_suffix, ".rds")))
  message("Saved: fig_satisfaction_coef_", date_suffix, " (.csv and .rds)")
}

# Satisfaction Prevalence
if (!is.null(prev_satisfaction) && nrow(prev_satisfaction) > 0) {
  readr::write_csv(prev_satisfaction, file.path(tables_dir, paste0("fig_satisfaction_prev_", date_suffix, ".csv")))
  readr::write_rds(prev_satisfaction, file.path(tables_dir, paste0("fig_satisfaction_prev_", date_suffix, ".rds")))
  message("Saved: fig_satisfaction_prev_", date_suffix, " (.csv and .rds)")
}


# ==============================================================================
# PART 6: CREATE FIGURES
# ==============================================================================

message("\n========== Creating figures ==========\n")

# --- Variable descriptions for y-axis labels ---
# For coefficient figure: these are coefficients of SRH on each outcome
# For prevalence figure: these are weighted means
var_descriptions_coef <- c(
  "Hospitalized"     = "Coef (SRH)",
  "Any ER Visit"     = "Coef (SRH)",
  "ER Visits (count)" = "Coef (SRH)",
  "Home Care"        = "Coef (SRH)",
  "Usual Care"       = "Coef (SRH)",
  "Uninsured"        = "Coef (SRH)"
)

var_descriptions_prev <- c(
  "Hospitalized"     = "Proportion",
  "Any ER Visit"     = "Proportion",
  "ER Visits (count)" = "Mean Count",
  "Home Care"        = "Proportion",
  "Usual Care"       = "Proportion",
  "Uninsured"        = "Proportion"
)

# --- Helper function to create a single panel ---
create_age_subplot <- function(
    data,
    y_var = "coefficient",
    show_title = FALSE,
    title = NULL,
    ylabel = NULL,
    base_size = 11,
    xlim = NULL,
    show_hline = TRUE,
    row_label = NULL
) {

  # Check if data has results
  if (is.null(data) || nrow(data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 5, color = "gray50") +
      theme_void()
    if (show_title && !is.null(title)) {
      p <- p + labs(title = title) +
        theme(plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5))
    }
    return(p)
  }

  left_margin <- if (!is.null(row_label)) 25 else 4

  p <- ggplot(data, aes(x = year, y = .data[[y_var]],
                        color = age_group, group = age_group)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    geom_point(size = 1.2, alpha = 0.8) +
    scale_color_manual(values = age_colors_oi, name = "Age Group") +
    labs(
      x = NULL,
      y = ylabel,
      title = if (show_title) title else NULL,
      tag = row_label
    )

  if (show_hline) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6)
  }

  if (!is.null(xlim)) {
    p <- p + scale_x_continuous(limits = xlim, breaks = scales::pretty_breaks(n = 4))
  } else {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }

  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.25),
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = base_size - 2),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      plot.margin = margin(4, 8, 4, left_margin),
      legend.position = "none",
      plot.tag = element_text(size = base_size + 1, face = "bold", angle = 90, vjust = 0.5),
      plot.tag.position = c(-0.06, 0.5)
    )

  return(p)
}

# --- Get year range ---
get_year_range <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(c(NA, NA))
  c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE))
}

xlim_nhis <- get_year_range(coef_nhis)
xlim_meps <- get_year_range(coef_meps)

# Common categories (shared between NHIS and MEPS) - 5 variables
categories_common <- c(
  "Hospitalized", "Any ER Visit", "ER Visits (count)",
  "Usual Care", "Uninsured"
)

# Satisfaction categories (MEPS only) - 4 variables
categories_satisfaction <- c(
  "Provider Listens", "Provider Respect",
  "Provider Explains", "Provider Confidence"
)

# Extended variable descriptions
var_descriptions_coef <- c(
  "Hospitalized"       = "Coef (SRH)",
  "Any ER Visit"       = "Coef (SRH)",
  "ER Visits (count)"  = "Coef (SRH)",
  "Usual Care"         = "Coef (SRH)",
  "Uninsured"          = "Coef (SRH)",
  "Provider Listens"   = "Coef (SRH)",
  "Provider Respect"   = "Coef (SRH)",
  "Provider Explains"  = "Coef (SRH)",
  "Provider Confidence" = "Coef (SRH)"
)

var_descriptions_prev <- c(
  "Hospitalized"       = "Proportion",
  "Any ER Visit"       = "Proportion",
  "ER Visits (count)"  = "Mean Count",
  "Usual Care"         = "Proportion",
  "Uninsured"          = "Proportion",
  "Provider Listens"   = "Proportion",
  "Provider Respect"   = "Proportion",
  "Provider Explains"  = "Proportion",
  "Provider Confidence" = "Proportion"
)


# ==============================================================================
# FIGURE: COEFFICIENTS - Combined NHIS + MEPS (5 common variables)
# ==============================================================================

message("\n--- Creating Combined Coefficient Figure ---\n")

fig_util_coef <- NULL

# Build NHIS row
coef_panels_nhis <- list()
if (!is.null(coef_nhis) && nrow(coef_nhis) > 0) {
  for (i in seq_along(categories_common)) {
    cat_name <- categories_common[i]
    cat_data <- coef_nhis %>% filter(category == cat_name)
    row_label <- if (i == 1) "NHIS" else NULL

    coef_panels_nhis[[cat_name]] <- create_age_subplot(
      cat_data,
      y_var = "coefficient",
      show_title = TRUE,
      title = cat_name,
      ylabel = var_descriptions_coef[cat_name],
      xlim = xlim_nhis,
      row_label = row_label
    )
  }
}

# Build MEPS row
coef_panels_meps <- list()
if (!is.null(coef_meps) && nrow(coef_meps) > 0) {
  for (i in seq_along(categories_common)) {
    cat_name <- categories_common[i]
    cat_data <- coef_meps %>% filter(category == cat_name)
    row_label <- if (i == 1) "MEPS" else NULL

    coef_panels_meps[[cat_name]] <- create_age_subplot(
      cat_data,
      y_var = "coefficient",
      show_title = FALSE,  # No titles on second row
      ylabel = var_descriptions_coef[cat_name],
      xlim = xlim_meps,
      row_label = row_label
    )
  }
}

# Assemble combined figure if we have both surveys
if (length(coef_panels_nhis) > 0 && length(coef_panels_meps) > 0) {
  row_nhis <- (coef_panels_nhis[[1]] | coef_panels_nhis[[2]] | coef_panels_nhis[[3]] |
               coef_panels_nhis[[4]] | coef_panels_nhis[[5]])
  row_meps <- (coef_panels_meps[[1]] | coef_panels_meps[[2]] | coef_panels_meps[[3]] |
               coef_panels_meps[[4]] | coef_panels_meps[[5]])

  fig_util_coef <- (row_nhis / row_meps / guide_area()) +
    plot_layout(heights = c(1, 1, 0.1), guides = "collect") +
    plot_annotation(
      title = "SRH Predicts Healthcare Utilization (NHIS & MEPS)",
      subtitle = "Model: utilization ~ SRH. Negative coefficient = better SRH (higher) -> less utilization.",
      caption = "This validity test shows SRH consistently predicts healthcare-seeking behavior across surveys.",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    ) &
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10)) &
    guides(color = guide_legend(nrow = 1))

} else if (length(coef_panels_nhis) > 0) {
  # NHIS only fallback
  row1 <- (coef_panels_nhis[[1]] | coef_panels_nhis[[2]] | coef_panels_nhis[[3]])
  row2 <- (coef_panels_nhis[[4]] | coef_panels_nhis[[5]] | plot_spacer())

  fig_util_coef <- (row1 / row2 / guide_area()) +
    plot_layout(heights = c(1, 1, 0.1), guides = "collect") +
    plot_annotation(
      title = "SRH Predicts Healthcare Utilization (NHIS)",
      subtitle = "Model: utilization ~ SRH. Negative coefficient = better SRH (higher) -> less utilization.",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    ) &
    theme(legend.position = "bottom") &
    guides(color = guide_legend(nrow = 1))
} else {
  message("No coefficient data to plot")
}


# ==============================================================================
# FIGURE: PREVALENCE - Combined NHIS + MEPS
# ==============================================================================

message("\n--- Creating Combined Prevalence Figure ---\n")

fig_util_prev <- NULL

# Build NHIS row
prev_panels_nhis <- list()
if (!is.null(prev_nhis) && nrow(prev_nhis) > 0) {
  for (i in seq_along(categories_common)) {
    cat_name <- categories_common[i]
    cat_data <- prev_nhis %>% filter(category == cat_name)
    row_label <- if (i == 1) "NHIS" else NULL

    prev_panels_nhis[[cat_name]] <- create_age_subplot(
      cat_data,
      y_var = "mean",
      show_title = TRUE,
      title = cat_name,
      ylabel = var_descriptions_prev[cat_name],
      xlim = xlim_nhis,
      show_hline = FALSE,
      row_label = row_label
    )
  }
}

# Build MEPS row
prev_panels_meps <- list()
if (!is.null(prev_meps) && nrow(prev_meps) > 0) {
  for (i in seq_along(categories_common)) {
    cat_name <- categories_common[i]
    cat_data <- prev_meps %>% filter(category == cat_name)
    row_label <- if (i == 1) "MEPS" else NULL

    prev_panels_meps[[cat_name]] <- create_age_subplot(
      cat_data,
      y_var = "mean",
      show_title = FALSE,
      ylabel = var_descriptions_prev[cat_name],
      xlim = xlim_meps,
      show_hline = FALSE,
      row_label = row_label
    )
  }
}

# Assemble combined figure
if (length(prev_panels_nhis) > 0 && length(prev_panels_meps) > 0) {
  row_nhis <- (prev_panels_nhis[[1]] | prev_panels_nhis[[2]] | prev_panels_nhis[[3]] |
               prev_panels_nhis[[4]] | prev_panels_nhis[[5]])
  row_meps <- (prev_panels_meps[[1]] | prev_panels_meps[[2]] | prev_panels_meps[[3]] |
               prev_panels_meps[[4]] | prev_panels_meps[[5]])

  fig_util_prev <- (row_nhis / row_meps / guide_area()) +
    plot_layout(heights = c(1, 1, 0.1), guides = "collect") +
    plot_annotation(
      title = "Hospital Utilization: Prevalence Trends by Age Group (NHIS & MEPS)",
      subtitle = "Survey-weighted means by age group and year",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    ) &
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(face = "bold", size = 11),
          legend.text = element_text(size = 10)) &
    guides(color = guide_legend(nrow = 1))

} else if (length(prev_panels_nhis) > 0) {
  # NHIS only fallback
  row1 <- (prev_panels_nhis[[1]] | prev_panels_nhis[[2]] | prev_panels_nhis[[3]])
  row2 <- (prev_panels_nhis[[4]] | prev_panels_nhis[[5]] | plot_spacer())

  fig_util_prev <- (row1 / row2 / guide_area()) +
    plot_layout(heights = c(1, 1, 0.1), guides = "collect") +
    plot_annotation(
      title = "Hospital Utilization: Prevalence Trends by Age Group (NHIS)",
      subtitle = "Survey-weighted means by age group and year",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    ) &
    theme(legend.position = "bottom") &
    guides(color = guide_legend(nrow = 1))
} else {
  message("No prevalence data to plot")
}


# ==============================================================================
# FIGURE: SATISFACTION COEFFICIENTS (MEPS only)
# ==============================================================================

message("\n--- Creating Satisfaction Coefficient Figure ---\n")

fig_satisfaction_coef <- NULL

if (!is.null(coef_satisfaction) && nrow(coef_satisfaction) > 0) {
  sat_coef_panels <- list()

  for (i in seq_along(categories_satisfaction)) {
    cat_name <- categories_satisfaction[i]
    cat_data <- coef_satisfaction %>% filter(category == cat_name)
    row_label <- if (i == 1) "MEPS" else NULL

    sat_coef_panels[[cat_name]] <- create_age_subplot(
      cat_data,
      y_var = "coefficient",
      show_title = TRUE,
      title = cat_name,
      ylabel = var_descriptions_coef[cat_name],
      xlim = xlim_meps,
      row_label = row_label
    )
  }

  if (length(sat_coef_panels) >= 4) {
    fig_satisfaction_coef <- (sat_coef_panels[[1]] | sat_coef_panels[[2]] |
                              sat_coef_panels[[3]] | sat_coef_panels[[4]]) /
      guide_area() +
      plot_layout(heights = c(1, 0.1), guides = "collect") +
      plot_annotation(
        title = "SRH Predicts Care Satisfaction (MEPS)",
        subtitle = "Model: satisfaction ~ SRH. Positive coefficient = better SRH -> more satisfaction.",
        caption = "CAHPS satisfaction items: Provider listens, shows respect, explains, gives confidence.",
        theme = theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
          plot.caption = element_text(size = 9, hjust = 0.5, color = "gray50"),
          plot.background = element_rect(fill = "white", color = NA)
        )
      ) &
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(face = "bold", size = 11),
            legend.text = element_text(size = 10)) &
      guides(color = guide_legend(nrow = 1))
  }
} else {
  message("No satisfaction coefficient data to plot")
}


# ==============================================================================
# FIGURE: SATISFACTION PREVALENCE (MEPS only)
# ==============================================================================

message("\n--- Creating Satisfaction Prevalence Figure ---\n")

fig_satisfaction_prev <- NULL

if (!is.null(prev_satisfaction) && nrow(prev_satisfaction) > 0) {
  sat_prev_panels <- list()

  for (i in seq_along(categories_satisfaction)) {
    cat_name <- categories_satisfaction[i]
    cat_data <- prev_satisfaction %>% filter(category == cat_name)
    row_label <- if (i == 1) "MEPS" else NULL

    sat_prev_panels[[cat_name]] <- create_age_subplot(
      cat_data,
      y_var = "mean",
      show_title = TRUE,
      title = cat_name,
      ylabel = var_descriptions_prev[cat_name],
      xlim = xlim_meps,
      show_hline = FALSE,
      row_label = row_label
    )
  }

  if (length(sat_prev_panels) >= 4) {
    fig_satisfaction_prev <- (sat_prev_panels[[1]] | sat_prev_panels[[2]] |
                              sat_prev_panels[[3]] | sat_prev_panels[[4]]) /
      guide_area() +
      plot_layout(heights = c(1, 0.1), guides = "collect") +
      plot_annotation(
        title = "Care Satisfaction Trends by Age Group (MEPS)",
        subtitle = "Survey-weighted proportion reporting positive response",
        theme = theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
          plot.background = element_rect(fill = "white", color = NA)
        )
      ) &
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(face = "bold", size = 11),
            legend.text = element_text(size = 10)) &
      guides(color = guide_legend(nrow = 1))
  }
} else {
  message("No satisfaction prevalence data to plot")
}


# ==============================================================================
# PART 7: SAVE FIGURES
# ==============================================================================

message("\n========== Saving figures ==========\n")

# Utilization Coefficient figure (NHIS + MEPS)
if (!is.null(fig_util_coef)) {
  ggsave(
    filename = file.path(output_dir, paste0("fig_util_coef_draft_", date_suffix, ".png")),
    plot = fig_util_coef,
    width = 14, height = 7, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_util_coef.png"),
    plot = fig_util_coef,
    width = 14, height = 7, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_util_coef.pdf"),
    plot = fig_util_coef,
    width = 14, height = 7
  )
  message("Saved: fig_util_coef (.png and .pdf)")
}

# Utilization Prevalence figure (NHIS + MEPS)
if (!is.null(fig_util_prev)) {
  ggsave(
    filename = file.path(output_dir, paste0("fig_util_prev_draft_", date_suffix, ".png")),
    plot = fig_util_prev,
    width = 14, height = 7, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_util_prev.png"),
    plot = fig_util_prev,
    width = 14, height = 7, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_util_prev.pdf"),
    plot = fig_util_prev,
    width = 14, height = 7
  )
  message("Saved: fig_util_prev (.png and .pdf)")
}

# Satisfaction Coefficient figure (MEPS only)
if (!is.null(fig_satisfaction_coef)) {
  ggsave(
    filename = file.path(output_dir, paste0("fig_satisfaction_coef_draft_", date_suffix, ".png")),
    plot = fig_satisfaction_coef,
    width = 14, height = 5, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_satisfaction_coef.png"),
    plot = fig_satisfaction_coef,
    width = 14, height = 5, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_satisfaction_coef.pdf"),
    plot = fig_satisfaction_coef,
    width = 14, height = 5
  )
  message("Saved: fig_satisfaction_coef (.png and .pdf)")
}

# Satisfaction Prevalence figure (MEPS only)
if (!is.null(fig_satisfaction_prev)) {
  ggsave(
    filename = file.path(output_dir, paste0("fig_satisfaction_prev_draft_", date_suffix, ".png")),
    plot = fig_satisfaction_prev,
    width = 14, height = 5, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_satisfaction_prev.png"),
    plot = fig_satisfaction_prev,
    width = 14, height = 5, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_satisfaction_prev.pdf"),
    plot = fig_satisfaction_prev,
    width = 14, height = 5
  )
  message("Saved: fig_satisfaction_prev (.png and .pdf)")
}


# ==============================================================================
# PART 8: VERIFICATION SUMMARY
# ==============================================================================

message("\n========== Verification Summary ==========\n")

# Check coefficient directions
check_coef_summary <- function(df, name) {
  if (is.null(df) || nrow(df) == 0) {
    message("  ", name, ": No data")
    return()
  }

  summary_df <- df %>%
    group_by(category, age_group) %>%
    summarise(
      n_years = n(),
      mean_coef = mean(coefficient, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(category) %>%
    summarise(
      n_age_groups = n_distinct(age_group),
      overall_mean = mean(mean_coef, na.rm = TRUE),
      .groups = "drop"
    )

  message("  ", name, ":")
  for (i in 1:nrow(summary_df)) {
    row <- summary_df[i, ]
    # Negative is expected (better SRH -> less utilization)
    # Exception: Usual Care may be positive (healthy people access preventive care)
    is_usual_care <- grepl("Usual", row$category)
    if (row$overall_mean < 0) {
      direction <- "NEGATIVE (expected)"
    } else if (is_usual_care) {
      direction <- "POSITIVE (may be ok for Usual Care)"
    } else {
      direction <- "POSITIVE (unexpected!)"
    }
    message("    ", row$category, ": mean coef = ", round(row$overall_mean, 3),
            " (", direction, "), ", row$n_age_groups, " age groups")
  }
}

# Check prevalence patterns
check_prev_summary <- function(df, name) {
  if (is.null(df) || nrow(df) == 0) {
    message("  ", name, ": No data")
    return()
  }

  summary_df <- df %>%
    group_by(category, age_group) %>%
    summarise(
      n_years = n(),
      mean_val = mean(mean, na.rm = TRUE),
      .groups = "drop"
    )

  message("  ", name, " prevalence ranges:")
  for (cat in unique(summary_df$category)) {
    cat_data <- summary_df %>% filter(category == cat)
    message("    ", cat, ": ", round(min(cat_data$mean_val), 3), " - ",
            round(max(cat_data$mean_val), 3))
  }
}

# Function to check satisfaction coefficient directions
check_satisfaction_summary <- function(df, name) {
  if (is.null(df) || nrow(df) == 0) {
    message("  ", name, ": No data")
    return()
  }

  summary_df <- df %>%
    group_by(category, age_group) %>%
    summarise(
      n_years = n(),
      mean_coef = mean(coefficient, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(category) %>%
    summarise(
      n_age_groups = n_distinct(age_group),
      overall_mean = mean(mean_coef, na.rm = TRUE),
      .groups = "drop"
    )

  message("  ", name, ":")
  for (i in 1:nrow(summary_df)) {
    row <- summary_df[i, ]
    # Positive is expected (better SRH -> more satisfaction)
    if (row$overall_mean > 0) {
      direction <- "POSITIVE (expected)"
    } else {
      direction <- "NEGATIVE (unexpected!)"
    }
    message("    ", row$category, ": mean coef = ", round(row$overall_mean, 3),
            " (", direction, "), ", row$n_age_groups, " age groups")
  }
}

message("\n--- Utilization Coefficient Check ---")
message("Expected: Negative coefficients (better SRH -> less utilization)")
message("Exception: 'Usual Care' may be positive (better SRH -> more preventive care)")
check_coef_summary(coef_nhis, "NHIS")
check_coef_summary(coef_meps, "MEPS")

message("\n--- Satisfaction Coefficient Check ---")
message("Expected: Positive coefficients (better SRH -> more satisfaction)")
check_satisfaction_summary(coef_satisfaction, "MEPS Satisfaction")

message("\n--- Utilization Prevalence Check ---")
check_prev_summary(prev_nhis, "NHIS")
check_prev_summary(prev_meps, "MEPS")

message("\n--- Satisfaction Prevalence Check ---")
check_prev_summary(prev_satisfaction, "MEPS Satisfaction")

# Sample size check
message("\n--- Sample Size Check ---")
if (!is.null(coef_nhis) && nrow(coef_nhis) > 0) {
  min_n_obs <- min(coef_nhis$n_unweighted, na.rm = TRUE)
  mean_n_obs <- round(mean(coef_nhis$n_unweighted, na.rm = TRUE))
  message("  NHIS coefficients: min n = ", min_n_obs, ", mean n = ", mean_n_obs)
}
if (!is.null(coef_meps) && nrow(coef_meps) > 0) {
  min_n_obs <- min(coef_meps$n_unweighted, na.rm = TRUE)
  mean_n_obs <- round(mean(coef_meps$n_unweighted, na.rm = TRUE))
  message("  MEPS coefficients: min n = ", min_n_obs, ", mean n = ", mean_n_obs)
}
if (!is.null(coef_satisfaction) && nrow(coef_satisfaction) > 0) {
  min_n_obs <- min(coef_satisfaction$n_unweighted, na.rm = TRUE)
  mean_n_obs <- round(mean(coef_satisfaction$n_unweighted, na.rm = TRUE))
  message("  MEPS satisfaction: min n = ", min_n_obs, ", mean n = ", mean_n_obs)
}

# Check for 2019 redesign effects
message("\n--- Survey Notes ---")
if (!is.null(coef_nhis)) {
  years_with_data <- unique(coef_nhis$year)
  if (2019 %in% years_with_data && 2018 %in% years_with_data) {
    message("  NHIS: Data spans 2018-2019 transition. Visual inspection recommended for discontinuity.")
  }
}
if (!is.null(coef_meps)) {
  message("  MEPS: Years ", min(coef_meps$year), "-", max(coef_meps$year))
}

message("\n========== Hospital Utilization Analysis Complete ==========\n")

# Clean up large objects
if (exists("data_nhis")) rm(data_nhis)
if (exists("data_meps")) rm(data_meps)
gc()
