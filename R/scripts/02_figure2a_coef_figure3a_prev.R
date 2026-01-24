# ==============================================================================
# 03_figure3_climbing.R
# Figure 3: Coefficient Stability by Age Group (Climbing Stairs Version)
# Figure 3B: Prevalence/Mean Trends by Age Group
#
# Purpose:
#   Same as 03_figure3.R but uses "Difficulty Climbing Stairs" instead of
#   generic physical health limitations:
#   - BRFSS: diffwalk (difficulty walking/climbing stairs)
#   - MEPS: ADCLIM (difficulty climbing stairs)
#   - NHIS: lawalkclimdif (difficulty walking/climbing without equipment)
#
# Layout (Transposed): Rows = Surveys, Cols = Categories
#   Row 1: BRFSS (Comorbidity Count | Mental Health | Difficulty Climbing)
#   Row 2: MEPS  (Comorbidity Count | Mental Health | Difficulty Climbing)
#   Row 3: NHIS  (Comorbidity Count | Mental Health | Difficulty Climbing)
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
# PART 1: LOAD DATA
# ==============================================================================

message("\n========== Loading survey data ==========\n")

# Configuration: Which surveys to process
run_nhis <- TRUE
run_meps <- TRUE
run_brfss <- TRUE

# NHIS year filter (use 2003 for comorbidity/mental; climbing starts 2010)
nhis_start_year <- 2003

# BRFSS year filter (use 2005 for comorbidity/mental; climbing starts 2013)
brfss_start_year <- 2005

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

# Load MEPS
if (run_meps) {
  data_meps <- load_survey_data("MEPS",
    here("analysis", "data_wrangling_code", "wrangle_meps.R"))
}

# Load BRFSS
if (run_brfss) {
  data_brfss <- load_survey_data("BRFSS",
    here("analysis", "data_wrangling_code", "wrangle_brfss.R"))
}


# ==============================================================================
# PART 2: ADD AGE GROUPS AND COMORBIDITY COUNTS
# ==============================================================================

message("\n========== Adding age groups and comorbidity counts ==========\n")

# --- NHIS ---
if (run_nhis && exists("data_nhis")) {
  message("Processing NHIS...")

  # Filter to start year (2003+; climbing variable starts 2010, others earlier)
  data_nhis <- data_nhis %>% filter(year >= nhis_start_year)

  # Add age group (scheme B)
  data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")

  # Create comorbidity count
  nhis_comorb_vars <- c("DIABETICEV", "HYPERTENEV", "CHEARTDIEV", "STROKEV",
                        "ARTHGLUPEV", "COPDEV", "CANCEREV", "ASTHMAEV")
  nhis_comorb_vars <- intersect(nhis_comorb_vars, names(data_nhis))
  message("  Found comorbidity vars: ", paste(nhis_comorb_vars, collapse = ", "))

  # NHIS uses IPUMS coding: 0=NIU, 1=No, 2=Yes, 7/8/9=Unknown
  data_nhis <- data_nhis %>%
    mutate(
      comorb_count = rowSums(across(all_of(nhis_comorb_vars), ~ as.numeric(.x == 2)),
                             na.rm = TRUE),
      # Scale K6 to 0-6 range
      k6_scaled = rescale_01(k6, min_val = 0, max_val = 24) * 6
      # lawalkclimdif is already coded 1-4 from wrangling
    )

  message("  NHIS: ", nrow(data_nhis), " rows, years ",
          min(data_nhis$year), "-", max(data_nhis$year))
  message("  lawalkclimdif distribution: ",
          paste(names(table(data_nhis$lawalkclimdif, useNA = "ifany")),
                table(data_nhis$lawalkclimdif, useNA = "ifany"), sep = "=", collapse = ", "))
}

# --- MEPS ---
if (run_meps && exists("data_meps")) {
  message("Processing MEPS...")

  # Filter to 2008+ for consistent variable availability
  data_meps <- data_meps %>% filter(year >= 2008)

  # Add age group (scheme B)
  data_meps <- add_age_group(data_meps, age_var = age, scheme = "B")

  # Create comorbidity count
  meps_comorb_vars <- c("DIABETICEV", "HYPERTENEV", "CHEARTDIEV", "STROKEV",
                        "ARTHGLUPEV", "CANCEREV", "ASTHMAEV")
  meps_comorb_vars <- intersect(meps_comorb_vars, names(data_meps))
  message("  Found comorbidity vars: ", paste(meps_comorb_vars, collapse = ", "))

  # MEPS uses standard coding: 0=No, 1=Yes for comorbidities
  # ADCLIM is coded: 0=No difficulty, 1=Some difficulty, 2=A lot of difficulty
  data_meps <- data_meps %>%
    mutate(
      comorb_count = rowSums(across(all_of(meps_comorb_vars), ~ as.numeric(.x == 1)),
                             na.rm = TRUE),
      # Scale K6 to 0-6 range
      K6SUM_scaled = rescale_01(K6SUM, min_val = 0, max_val = 24) * 6
      # ADCLIM is already in the data (0-2 scale)
    )

  message("  MEPS: ", nrow(data_meps), " rows, years ",
          min(data_meps$year), "-", max(data_meps$year))
  message("  ADCLIM distribution: ",
          paste(names(table(data_meps$ADCLIM, useNA = "ifany")),
                table(data_meps$ADCLIM, useNA = "ifany"), sep = "=", collapse = ", "))
}

# --- BRFSS ---
if (run_brfss && exists("data_brfss")) {
  message("Processing BRFSS...")

  # Filter to start year (2005+; climbing variable starts 2013, others earlier)
  data_brfss <- data_brfss %>% filter(year >= brfss_start_year)

  # Add age group (scheme B)
  data_brfss <- add_age_group(data_brfss, age_var = age, scheme = "B")

  # Create comorbidity count (excluding HTN which is only in odd years)
  brfss_comorb_vars <- c("diabetes_dx", "chd_dx", "stroke_dx", "arthritis_dx")
  brfss_comorb_vars <- intersect(brfss_comorb_vars, names(data_brfss))
  message("  BRFSS comorbidity vars (excl. HTN): ", paste(brfss_comorb_vars, collapse = ", "))

  # BRFSS uses 0=No, 1=Yes
  # diffwalk is binary: 0=No difficulty, 1=Has difficulty
  data_brfss <- data_brfss %>%
    mutate(
      comorb_count = rowSums(across(all_of(brfss_comorb_vars), ~ as.numeric(.x == 1)),
                             na.rm = TRUE)
      # diffwalk is already in the data (0/1)
    )

  message("  BRFSS: ", nrow(data_brfss), " rows, years ",
          min(data_brfss$year), "-", max(data_brfss$year))
  message("  diffwalk distribution: ",
          paste(names(table(data_brfss$diffwalk, useNA = "ifany")),
                table(data_brfss$diffwalk, useNA = "ifany"), sep = "=", collapse = ", "))
}


# ==============================================================================
# PART 3: RUN AGE-STRATIFIED REGRESSIONS (FIGURE 3)
# ==============================================================================

message("\n========== Running age-stratified regressions ==========\n")

# Initialize result objects
coef_nhis <- NULL
coef_meps <- NULL
coef_brfss <- NULL

# --- NHIS Regressions ---
if (run_nhis && exists("data_nhis")) {
  message("\n--- NHIS ---")

  # Chronic: Comorbidity count
  coef_nhis_chronic <- regress_covariate_by_age_year(
    data = data_nhis,
    covariate_var = "comorb_count",
    covariate_label = "Comorbidity Count",
    survey_name = "NHIS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Mental: K6 (scaled 0-6)
  coef_nhis_mental <- regress_covariate_by_age_year(
    data = data_nhis,
    covariate_var = "k6_scaled",
    covariate_label = "K6 (0-6)",
    survey_name = "NHIS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Functional: lawalkclimdif (1-4 scale)
  coef_nhis_climbing <- regress_covariate_by_age_year(
    data = data_nhis,
    covariate_var = "lawalkclimdif",
    covariate_label = "Difficulty Climbing (1-4)",
    survey_name = "NHIS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Combine and add category
  coef_nhis <- bind_rows(
    coef_nhis_chronic %>% mutate(category = "Comorbidity Count"),
    coef_nhis_mental %>% mutate(category = "Mental Health"),
    coef_nhis_climbing %>% mutate(category = "Difficulty Climbing Stairs")
  )
}

# --- MEPS Regressions ---
if (run_meps && exists("data_meps")) {
  message("\n--- MEPS ---")

  # Chronic: Comorbidity count
  coef_meps_chronic <- regress_covariate_by_age_year(
    data = data_meps,
    covariate_var = "comorb_count",
    covariate_label = "Comorbidity Count",
    survey_name = "MEPS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Mental: K6 (scaled 0-6)
  coef_meps_mental <- regress_covariate_by_age_year(
    data = data_meps,
    covariate_var = "K6SUM_scaled",
    covariate_label = "K6 (0-6)",
    survey_name = "MEPS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Functional: ADCLIM (0-2 scale)
  coef_meps_climbing <- regress_covariate_by_age_year(
    data = data_meps,
    covariate_var = "ADCLIM",
    covariate_label = "Difficulty Climbing (0-2)",
    survey_name = "MEPS",
    psu_var = NULL,
    strata_var = NULL
  )

  coef_meps <- bind_rows(
    coef_meps_chronic %>% mutate(category = "Comorbidity Count"),
    coef_meps_mental %>% mutate(category = "Mental Health"),
    coef_meps_climbing %>% mutate(category = "Difficulty Climbing Stairs")
  )
}

# --- BRFSS Regressions ---
if (run_brfss && exists("data_brfss")) {
  message("\n--- BRFSS ---")

  # Chronic: Comorbidity count
  coef_brfss_chronic <- regress_covariate_by_age_year(
    data = data_brfss,
    covariate_var = "comorb_count",
    covariate_label = "Comorbidity Count",
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Mental: Mental health days (0-30)
  coef_brfss_mental <- regress_covariate_by_age_year(
    data = data_brfss,
    covariate_var = "ment_bad",
    covariate_label = "Mental Days (0-30)",
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Functional: diffwalk (0/1 binary)
  coef_brfss_climbing <- regress_covariate_by_age_year(
    data = data_brfss,
    covariate_var = "diffwalk",
    covariate_label = "Difficulty Walking/Climbing (0/1)",
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  coef_brfss <- bind_rows(
    coef_brfss_chronic %>% mutate(category = "Comorbidity Count"),
    coef_brfss_mental %>% mutate(category = "Mental Health"),
    coef_brfss_climbing %>% mutate(category = "Difficulty Climbing Stairs")
  )
}


# ==============================================================================
# PART 4: RUN PREVALENCE/MEAN CALCULATIONS (FIGURE 3B)
# ==============================================================================

message("\n========== Computing prevalence/means by age group ==========\n")

prev_nhis <- NULL
prev_meps <- NULL
prev_brfss <- NULL

# --- NHIS Prevalence ---
if (run_nhis && exists("data_nhis")) {
  message("\n--- NHIS ---")

  prev_nhis_chronic <- mean_by_age_year(
    data = data_nhis,
    var_name = "comorb_count",
    var_label = "Comorbidity Count",
    survey_name = "NHIS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_nhis_mental <- mean_by_age_year(
    data = data_nhis,
    var_name = "k6_scaled",
    var_label = "K6 (0-6)",
    survey_name = "NHIS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_nhis_climbing <- mean_by_age_year(
    data = data_nhis,
    var_name = "lawalkclimdif",
    var_label = "Difficulty Climbing (1-4)",
    survey_name = "NHIS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_nhis <- bind_rows(
    prev_nhis_chronic %>% mutate(category = "Comorbidity Count"),
    prev_nhis_mental %>% mutate(category = "Mental Health"),
    prev_nhis_climbing %>% mutate(category = "Difficulty Climbing Stairs")
  )
}

# --- MEPS Prevalence ---
if (run_meps && exists("data_meps")) {
  message("\n--- MEPS ---")

  prev_meps_chronic <- mean_by_age_year(
    data = data_meps,
    var_name = "comorb_count",
    var_label = "Comorbidity Count",
    survey_name = "MEPS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_meps_mental <- mean_by_age_year(
    data = data_meps,
    var_name = "K6SUM_scaled",
    var_label = "K6 (0-6)",
    survey_name = "MEPS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_meps_climbing <- mean_by_age_year(
    data = data_meps,
    var_name = "ADCLIM",
    var_label = "Difficulty Climbing (0-2)",
    survey_name = "MEPS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_meps <- bind_rows(
    prev_meps_chronic %>% mutate(category = "Comorbidity Count"),
    prev_meps_mental %>% mutate(category = "Mental Health"),
    prev_meps_climbing %>% mutate(category = "Difficulty Climbing Stairs")
  )
}

# --- BRFSS Prevalence ---
if (run_brfss && exists("data_brfss")) {
  message("\n--- BRFSS ---")

  prev_brfss_chronic <- mean_by_age_year(
    data = data_brfss,
    var_name = "comorb_count",
    var_label = "Comorbidity Count",
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_brfss_mental <- mean_by_age_year(
    data = data_brfss,
    var_name = "ment_bad",
    var_label = "Mental Days (0-30)",
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_brfss_climbing <- mean_by_age_year(
    data = data_brfss,
    var_name = "diffwalk",
    var_label = "Difficulty Walking/Climbing (0/1)",
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_brfss <- bind_rows(
    prev_brfss_chronic %>% mutate(category = "Comorbidity Count"),
    prev_brfss_mental %>% mutate(category = "Mental Health"),
    prev_brfss_climbing %>% mutate(category = "Difficulty Climbing Stairs")
  )
}

# Free memory from large data objects
rm(data_nhis, data_meps, data_brfss); gc()


# ==============================================================================
# PART 5: SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

# Coefficients (Figure 2A)
if (!is.null(coef_nhis)) {
  readr::write_csv(coef_nhis, file.path(tables_dir, paste0("fig2a_coef_nhis_", date_suffix, ".csv")))
  readr::write_rds(coef_nhis, file.path(tables_dir, paste0("fig2a_coef_nhis_", date_suffix, ".rds")))
}
if (!is.null(coef_meps)) {
  readr::write_csv(coef_meps, file.path(tables_dir, paste0("fig2a_coef_meps_", date_suffix, ".csv")))
  readr::write_rds(coef_meps, file.path(tables_dir, paste0("fig2a_coef_meps_", date_suffix, ".rds")))
}
if (!is.null(coef_brfss)) {
  readr::write_csv(coef_brfss, file.path(tables_dir, paste0("fig2a_coef_brfss_", date_suffix, ".csv")))
  readr::write_rds(coef_brfss, file.path(tables_dir, paste0("fig2a_coef_brfss_", date_suffix, ".rds")))
}

# Prevalence (Figure 3A)
if (!is.null(prev_nhis)) {
  readr::write_csv(prev_nhis, file.path(tables_dir, paste0("fig3a_prev_nhis_", date_suffix, ".csv")))
  readr::write_rds(prev_nhis, file.path(tables_dir, paste0("fig3a_prev_nhis_", date_suffix, ".rds")))
}
if (!is.null(prev_meps)) {
  readr::write_csv(prev_meps, file.path(tables_dir, paste0("fig3a_prev_meps_", date_suffix, ".csv")))
  readr::write_rds(prev_meps, file.path(tables_dir, paste0("fig3a_prev_meps_", date_suffix, ".rds")))
}
if (!is.null(prev_brfss)) {
  readr::write_csv(prev_brfss, file.path(tables_dir, paste0("fig3a_prev_brfss_", date_suffix, ".csv")))
  readr::write_rds(prev_brfss, file.path(tables_dir, paste0("fig3a_prev_brfss_", date_suffix, ".rds")))
}


# ==============================================================================
# PART 6: CREATE FIGURES (TRANSPOSED LAYOUT)
# ==============================================================================
# TRANSPOSED LAYOUT: Rows = Surveys, Columns = Categories
# Row 1: BRFSS (Comorbidity Count | Mental Health | Difficulty Climbing)
# Row 2: MEPS  (Comorbidity Count | Mental Health | Difficulty Climbing)
# Row 3: NHIS  (Comorbidity Count | Mental Health | Difficulty Climbing)
# ==============================================================================

message("\n========== Creating Figure 3 (Climbing Version) ==========\n")

# --- Variable descriptions for each survey/category combination ---
var_descriptions <- list(
  # Chronic conditions
  BRFSS_chronic = "Comorbidity count",
  MEPS_chronic = "Comorbidity count",
  NHIS_chronic = "Comorbidity count",
  # Mental health
  BRFSS_mental = "Mental bad days (0-30)",
  MEPS_mental = "K6 distress (0-6)",
  NHIS_mental = "K6 distress (0-6)",
  # Difficulty climbing stairs
  BRFSS_climbing = "DIFFWALK (0/1)",
  MEPS_climbing = "ADCLIM (0-2)",
  NHIS_climbing = "LAWALKCLIMDIF (1-4)"
)

# --- Helper function to create a single panel ---
# row_label: if provided, adds survey name as a tag on the left side of the panel
create_age_subplot <- function(
    data,
    y_var = "coefficient",
    show_title = FALSE,
    title = NULL,
    ylabel = NULL,
    base_size = 13,
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

  # Add extra left margin if this panel has a row label

  left_margin <- if (!is.null(row_label)) 25 else 4

  p <- ggplot(data, aes(x = year, y = .data[[y_var]],
                        color = age_group, group = age_group)) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 1.5, alpha = 0.8) +
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
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = base_size - 1),
      axis.text = element_text(size = base_size - 1, color = "gray30"),
      plot.margin = margin(4, 12, 4, left_margin),
      legend.position = "none",
      plot.tag = element_text(size = base_size + 1, face = "bold", angle = 90, vjust = 0.5),
      plot.tag.position = c(-0.06, 0.5)
    )

  return(p)
}


# --- Calculate x-axis limits for each survey ---
get_year_range <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(c(NA, NA))
  c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE))
}

xlim_brfss <- get_year_range(coef_brfss)
xlim_meps <- get_year_range(coef_meps)
xlim_nhis <- get_year_range(coef_nhis)


# ==============================================================================
# FIGURE 3: COEFFICIENTS (TRANSPOSED)
# ==============================================================================

# Years to exclude for BRFSS comorbidity (coding problems)
brfss_comorb_exclude_years <- c(2006, 2008, 2010)

# --- Row 1: BRFSS (with row label) ---
p_brfss_chronic <- create_age_subplot(
  coef_brfss %>% filter(category == "Comorbidity Count", !year %in% brfss_comorb_exclude_years),
  show_title = TRUE, title = "Comorbidity Coefficient",
  ylabel = var_descriptions$BRFSS_chronic,
  xlim = xlim_brfss, row_label = "BRFSS"
)

p_brfss_mental <- create_age_subplot(
  coef_brfss %>% filter(category == "Mental Health"),
  show_title = TRUE, title = "Mental Health Coefficient",
  ylabel = var_descriptions$BRFSS_mental,
  xlim = xlim_brfss
)

p_brfss_climbing <- create_age_subplot(
  coef_brfss %>% filter(category == "Difficulty Climbing Stairs"),
  show_title = TRUE, title = "Climbing Stairs Coefficient",
  ylabel = var_descriptions$BRFSS_climbing,
  xlim = xlim_brfss
)

# --- Row 2: MEPS (with row label) ---
p_meps_chronic <- create_age_subplot(
  coef_meps %>% filter(category == "Comorbidity Count"),
  ylabel = var_descriptions$MEPS_chronic,
  xlim = xlim_meps, row_label = "MEPS"
)

p_meps_mental <- create_age_subplot(
  coef_meps %>% filter(category == "Mental Health"),
  ylabel = var_descriptions$MEPS_mental,
  xlim = xlim_meps
)

p_meps_climbing <- create_age_subplot(
  coef_meps %>% filter(category == "Difficulty Climbing Stairs"),
  ylabel = var_descriptions$MEPS_climbing,
  xlim = xlim_meps
)

# --- Row 3: NHIS (with row label) ---
p_nhis_chronic <- create_age_subplot(
  coef_nhis %>% filter(category == "Comorbidity Count"),
  ylabel = var_descriptions$NHIS_chronic,
  xlim = xlim_nhis, row_label = "NHIS"
)

p_nhis_mental <- create_age_subplot(
  coef_nhis %>% filter(category == "Mental Health"),
  ylabel = var_descriptions$NHIS_mental,
  xlim = xlim_nhis
)

p_nhis_climbing <- create_age_subplot(
  coef_nhis %>% filter(category == "Difficulty Climbing Stairs"),
  ylabel = var_descriptions$NHIS_climbing,
  xlim = xlim_nhis
)

# --- Assemble Figure 3 (transposed: rows=surveys, cols=categories) ---
fig3_climbing <- (
  (p_brfss_chronic | p_brfss_mental | p_brfss_climbing) /
  (p_meps_chronic | p_meps_mental | p_meps_climbing) /
  (p_nhis_chronic | p_nhis_mental | p_nhis_climbing) /
  guide_area()
) +
  plot_layout(heights = c(1, 1, 1, 0.08), guides = "collect") +
  plot_annotation(
    title = "Coefficient Trends by Age Group",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  ) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 14)) &
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# FIGURE 3B: PREVALENCE (TRANSPOSED)
# ==============================================================================

message("\n========== Creating Figure 3B (Climbing Version) ==========\n")

# --- Row 1: BRFSS (with row label) ---
p3b_brfss_chronic <- create_age_subplot(
  prev_brfss %>% filter(category == "Comorbidity Count", !year %in% brfss_comorb_exclude_years),
  y_var = "mean",
  show_title = TRUE, title = "Comorbidity Count",
  ylabel = var_descriptions$BRFSS_chronic,
  xlim = xlim_brfss, show_hline = FALSE, row_label = "BRFSS"
)

p3b_brfss_mental <- create_age_subplot(
  prev_brfss %>% filter(category == "Mental Health"),
  y_var = "mean",
  show_title = TRUE, title = "Mental Health",
  ylabel = var_descriptions$BRFSS_mental,
  xlim = xlim_brfss, show_hline = FALSE
)

p3b_brfss_climbing <- create_age_subplot(
  prev_brfss %>% filter(category == "Difficulty Climbing Stairs"),
  y_var = "mean",
  show_title = TRUE, title = "Difficulty Climbing Stairs",
  ylabel = var_descriptions$BRFSS_climbing,
  xlim = xlim_brfss, show_hline = FALSE
)

# --- Row 2: MEPS (with row label) ---
p3b_meps_chronic <- create_age_subplot(
  prev_meps %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  ylabel = var_descriptions$MEPS_chronic,
  xlim = xlim_meps, show_hline = FALSE, row_label = "MEPS"
)

p3b_meps_mental <- create_age_subplot(
  prev_meps %>% filter(category == "Mental Health"),
  y_var = "mean",
  ylabel = var_descriptions$MEPS_mental,
  xlim = xlim_meps, show_hline = FALSE
)

p3b_meps_climbing <- create_age_subplot(
  prev_meps %>% filter(category == "Difficulty Climbing Stairs"),
  y_var = "mean",
  ylabel = var_descriptions$MEPS_climbing,
  xlim = xlim_meps, show_hline = FALSE
)

# --- Row 3: NHIS (with row label) ---
p3b_nhis_chronic <- create_age_subplot(
  prev_nhis %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  ylabel = var_descriptions$NHIS_chronic,
  xlim = xlim_nhis, show_hline = FALSE, row_label = "NHIS"
)

p3b_nhis_mental <- create_age_subplot(
  prev_nhis %>% filter(category == "Mental Health"),
  y_var = "mean",
  ylabel = var_descriptions$NHIS_mental,
  xlim = xlim_nhis, show_hline = FALSE
)

p3b_nhis_climbing <- create_age_subplot(
  prev_nhis %>% filter(category == "Difficulty Climbing Stairs"),
  y_var = "mean",
  ylabel = var_descriptions$NHIS_climbing,
  xlim = xlim_nhis, show_hline = FALSE
)

# --- Assemble Figure 3B (transposed: rows=surveys, cols=categories) ---
fig3b_climbing <- (
  (p3b_brfss_chronic | p3b_brfss_mental | p3b_brfss_climbing) /
  (p3b_meps_chronic | p3b_meps_mental | p3b_meps_climbing) /
  (p3b_nhis_chronic | p3b_nhis_mental | p3b_nhis_climbing) /
  guide_area()
) +
  plot_layout(heights = c(1, 1, 1, 0.08), guides = "collect") +
  plot_annotation(
    title = "Health Trends by Age Group",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  ) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 13),
        legend.text = element_text(size = 14)) &
  guides(color = guide_legend(nrow = 1))


# ==============================================================================
# PART 7: SAVE FIGURES
# ==============================================================================

message("\n========== Saving figures ==========\n")

# Figure 2A (coefficients)
ggsave(
  filename = file.path(output_dir, paste0("fig2a_coef_draft_", date_suffix, ".png")),
  plot = fig3_climbing,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig2a_coef.png"),
  plot = fig3_climbing,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig2a_coef.pdf"),
  plot = fig3_climbing,
  width = 12, height = 10
)
message("Saved: fig2a_coef (.png and .pdf)")

# Figure 3A (prevalence)
ggsave(
  filename = file.path(output_dir, paste0("fig3a_prev_draft_", date_suffix, ".png")),
  plot = fig3b_climbing,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3a_prev.png"),
  plot = fig3b_climbing,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3a_prev.pdf"),
  plot = fig3b_climbing,
  width = 12, height = 10
)
message("Saved: fig3a_prev (.png and .pdf)")


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
    direction <- ifelse(row$overall_mean < 0, "negative", "positive")
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
    message("    ", cat, ": ", round(min(cat_data$mean_val), 2), " - ",
            round(max(cat_data$mean_val), 2))
  }
}

message("\n--- Coefficient Check (should be negative) ---")
check_coef_summary(coef_brfss, "BRFSS")
check_coef_summary(coef_meps, "MEPS")
check_coef_summary(coef_nhis, "NHIS")

message("\n--- Prevalence Check ---")
check_prev_summary(prev_brfss, "BRFSS")
check_prev_summary(prev_meps, "MEPS")
check_prev_summary(prev_nhis, "NHIS")

message("\n========== Figure 3 Climbing Version Complete ==========\n")
