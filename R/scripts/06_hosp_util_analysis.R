# ==============================================================================
# 06_hosp_util_analysis.R
# Hospital Utilization Analysis: Coefficient Stability and Prevalence Trends
#
# Purpose:
#   Show that SRH-utilization relationship is stable (meaning unchanged) and
#   how utilization patterns are changing by age group over time.
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
# Layout: Rows = Surveys (NHIS), Cols = Variables
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
source(here("R", "functions", "regress_covariate_by_year.R"))
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

# Which surveys to run (MEPS can be added later)
run_nhis <- TRUE
run_meps <- FALSE  # Set to TRUE when MEPS utilization vars are wrangled

# NHIS year filter - include all years with utilization data
# Note: 2019 redesign may cause discontinuity
nhis_start_year <- 1997

# Minimum sample size per age-year cell
min_n <- 50

# Hospital utilization variables to analyze
util_vars <- c(
  hospitalized    = "Hospitalized",
  any_er          = "Any ER Visit",
  er_visits       = "ER Visits (count)",
  home_care       = "Home Care",
  has_usual_care  = "Usual Care",
  uninsured       = "Uninsured",
  sickdays        = "Sick Days"
)


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


# ==============================================================================
# PART 3: RUN AGE-STRATIFIED REGRESSIONS (COEFFICIENTS)
# ==============================================================================

message("\n========== Running age-stratified regressions ==========\n")

coef_nhis <- NULL
coef_meps <- NULL

# --- NHIS Regressions ---
if (run_nhis && exists("data_nhis")) {
  message("\n--- NHIS Utilization Coefficients ---")

  coef_list <- list()

  for (i in seq_along(util_vars)) {
    var_name <- names(util_vars)[i]
    var_label <- util_vars[i]

    if (!var_name %in% names(data_nhis)) {
      message("  Skipping ", var_name, ": not found in data")
      next
    }

    coef_result <- regress_covariate_by_age_year(
      data = data_nhis,
      covariate_var = var_name,
      covariate_label = var_label,
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

# --- MEPS Regressions (future) ---
if (run_meps && exists("data_meps")) {
  message("\n--- MEPS Utilization Coefficients ---")
  # Similar structure as NHIS when MEPS vars are added
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


# ==============================================================================
# PART 5: SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

# Coefficients
if (!is.null(coef_nhis) && nrow(coef_nhis) > 0) {
  readr::write_csv(coef_nhis, file.path(tables_dir, paste0("fig_util_coef_nhis_", date_suffix, ".csv")))
  readr::write_rds(coef_nhis, file.path(tables_dir, paste0("fig_util_coef_nhis_", date_suffix, ".rds")))
  message("Saved: fig_util_coef_nhis_", date_suffix, " (.csv and .rds)")
}

# Prevalence
if (!is.null(prev_nhis) && nrow(prev_nhis) > 0) {
  readr::write_csv(prev_nhis, file.path(tables_dir, paste0("fig_util_prev_nhis_", date_suffix, ".csv")))
  readr::write_rds(prev_nhis, file.path(tables_dir, paste0("fig_util_prev_nhis_", date_suffix, ".rds")))
  message("Saved: fig_util_prev_nhis_", date_suffix, " (.csv and .rds)")
}


# ==============================================================================
# PART 6: CREATE FIGURES
# ==============================================================================

message("\n========== Creating figures ==========\n")

# --- Variable descriptions for y-axis labels ---
var_descriptions <- c(
  "Hospitalized"     = "Hospitalized (0/1)",
  "Any ER Visit"     = "Any ER (0/1)",
  "ER Visits (count)" = "ER Visits (count)",
  "Home Care"        = "Home Care (0/1)",
  "Usual Care"       = "Has Usual Care (0/1)",
  "Uninsured"        = "Uninsured (0/1)",
  "Sick Days"        = "Sick Days (count)"
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
    scale_color_manual(values = age_colors, name = "Age Group") +
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

# Categories in order for figure
categories_ordered <- c(
  "Hospitalized", "Any ER Visit", "ER Visits (count)", "Home Care",
  "Usual Care", "Uninsured", "Sick Days"
)


# ==============================================================================
# FIGURE: COEFFICIENTS (7 panels for NHIS)
# ==============================================================================

message("\n--- Creating Coefficient Figure ---\n")

if (!is.null(coef_nhis) && nrow(coef_nhis) > 0) {

  coef_panels <- list()

  for (i in seq_along(categories_ordered)) {
    cat_name <- categories_ordered[i]
    cat_data <- coef_nhis %>% filter(category == cat_name)

    # First panel gets row label
    row_label <- if (i == 1) "NHIS" else NULL

    coef_panels[[cat_name]] <- create_age_subplot(
      cat_data,
      y_var = "coefficient",
      show_title = TRUE,
      title = cat_name,
      ylabel = var_descriptions[cat_name],
      xlim = xlim_nhis,
      row_label = row_label
    )
  }

  # Assemble figure: 2 rows of panels (4 + 3)
  row1 <- (coef_panels[[1]] | coef_panels[[2]] | coef_panels[[3]] | coef_panels[[4]])
  row2 <- (coef_panels[[5]] | coef_panels[[6]] | coef_panels[[7]] | plot_spacer())

  fig_util_coef <- (row1 / row2 / guide_area()) +
    plot_layout(heights = c(1, 1, 0.1), guides = "collect") +
    plot_annotation(
      title = "Hospital Utilization: Coefficient Trends by Age Group (NHIS)",
      subtitle = "Note: Negative coefficients indicate worse SRH is associated with more utilization",
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

} else {
  message("No coefficient data to plot")
  fig_util_coef <- NULL
}


# ==============================================================================
# FIGURE: PREVALENCE (7 panels for NHIS)
# ==============================================================================

message("\n--- Creating Prevalence Figure ---\n")

if (!is.null(prev_nhis) && nrow(prev_nhis) > 0) {

  prev_panels <- list()

  for (i in seq_along(categories_ordered)) {
    cat_name <- categories_ordered[i]
    cat_data <- prev_nhis %>% filter(category == cat_name)

    # First panel gets row label
    row_label <- if (i == 1) "NHIS" else NULL

    prev_panels[[cat_name]] <- create_age_subplot(
      cat_data,
      y_var = "mean",
      show_title = TRUE,
      title = cat_name,
      ylabel = var_descriptions[cat_name],
      xlim = xlim_nhis,
      show_hline = FALSE,
      row_label = row_label
    )
  }

  # Assemble figure: 2 rows of panels (4 + 3)
  row1 <- (prev_panels[[1]] | prev_panels[[2]] | prev_panels[[3]] | prev_panels[[4]])
  row2 <- (prev_panels[[5]] | prev_panels[[6]] | prev_panels[[7]] | plot_spacer())

  fig_util_prev <- (row1 / row2 / guide_area()) +
    plot_layout(heights = c(1, 1, 0.1), guides = "collect") +
    plot_annotation(
      title = "Hospital Utilization: Prevalence Trends by Age Group (NHIS)",
      subtitle = "Note: Values represent survey-weighted means by age group and year",
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

} else {
  message("No prevalence data to plot")
  fig_util_prev <- NULL
}


# ==============================================================================
# PART 7: SAVE FIGURES
# ==============================================================================

message("\n========== Saving figures ==========\n")

# Coefficient figure
if (!is.null(fig_util_coef)) {
  ggsave(
    filename = file.path(output_dir, paste0("fig_util_coef_draft_", date_suffix, ".png")),
    plot = fig_util_coef,
    width = 14, height = 8, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_util_coef.png"),
    plot = fig_util_coef,
    width = 14, height = 8, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_util_coef.pdf"),
    plot = fig_util_coef,
    width = 14, height = 8
  )
  message("Saved: fig_util_coef (.png and .pdf)")
}

# Prevalence figure
if (!is.null(fig_util_prev)) {
  ggsave(
    filename = file.path(output_dir, paste0("fig_util_prev_draft_", date_suffix, ".png")),
    plot = fig_util_prev,
    width = 14, height = 8, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_util_prev.png"),
    plot = fig_util_prev,
    width = 14, height = 8, dpi = 300
  )
  ggsave(
    filename = file.path(output_dir, "fig_util_prev.pdf"),
    plot = fig_util_prev,
    width = 14, height = 8
  )
  message("Saved: fig_util_prev (.png and .pdf)")
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
    direction <- ifelse(row$overall_mean < 0, "NEGATIVE (expected)", "POSITIVE (unexpected)")
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

message("\n--- Coefficient Check (should mostly be negative for utilization) ---")
check_coef_summary(coef_nhis, "NHIS")

message("\n--- Prevalence Check ---")
check_prev_summary(prev_nhis, "NHIS")

# Sample size check
message("\n--- Sample Size Check ---")
if (!is.null(coef_nhis) && nrow(coef_nhis) > 0) {
  min_n_obs <- min(coef_nhis$n_unweighted, na.rm = TRUE)
  mean_n_obs <- round(mean(coef_nhis$n_unweighted, na.rm = TRUE))
  message("  NHIS coefficients: min n = ", min_n_obs, ", mean n = ", mean_n_obs)
}

# Check for 2019 redesign effects
message("\n--- 2019 Redesign Note ---")
if (!is.null(coef_nhis)) {
  years_with_data <- unique(coef_nhis$year)
  if (2019 %in% years_with_data && 2018 %in% years_with_data) {
    message("  Data spans 2018-2019 transition. Visual inspection recommended for discontinuity.")
  }
}

message("\n========== Hospital Utilization Analysis Complete ==========\n")

# Clean up large objects
rm(data_nhis); gc()
