# ==============================================================================
# 03_figure3.R
# Figure 3: Coefficient Stability by Age Group
# Figure 3B: Prevalence/Mean Trends by Age Group
#
# Purpose:
#   Figure 3: For each age group, show that coefficients of covariates on SRH
#             remain stable over time (i.e., "what SRH measures" is constant)
#   Figure 3B: Show prevalence/mean of covariates by age group over time
#              (i.e., what is actually changing)
#
# Layout: 3 rows (BRFSS, MEPS, NHIS) x 3 columns (Chronic, Mental, Functional)
#         Each cell has 7 lines colored by age group (18-29 to 80-89)
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

# NHIS year filter (K6 starts in 1997)
nhis_start_year <- 1997

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

  # Filter to years with K6
  data_nhis <- data_nhis %>% filter(year >= nhis_start_year)

  # Add age group (scheme B)
  data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")

  # Create comorbidity count
  # NHIS comorbidities: DIABETICEV, HYPERTENEV, CHEARTDIEV, STROKEV,
  #                     ARTHGLUPEV, COPDEV, CANCEREV, ASTHMAEV
  nhis_comorb_vars <- c("DIABETICEV", "HYPERTENEV", "CHEARTDIEV", "STROKEV",
                        "ARTHGLUPEV", "COPDEV", "CANCEREV", "ASTHMAEV")

  # Check which vars exist
  nhis_comorb_vars <- intersect(nhis_comorb_vars, names(data_nhis))
  message("  Found comorbidity vars: ", paste(nhis_comorb_vars, collapse = ", "))

  # Create count (sum of non-NA conditions)
  # NHIS uses IPUMS coding: 0=NIU, 1=No, 2=Yes, 7/8/9=Unknown
  data_nhis <- data_nhis %>%
    mutate(
      comorb_count = rowSums(across(all_of(nhis_comorb_vars), ~ as.numeric(.x == 2)),
                             na.rm = TRUE),
      # Scale K6 to 0-6 range
      k6_scaled = rescale_01(k6, min_val = 0, max_val = 24) * 6,
      # Harmonize flclimb: Pre-2019 NIU (FLCLIMB==0) treated as "no difficulty" = 1
      # Raw FLCLIMB coding: 0=NIU, 10=None, 20-22=Little/Some/Lot, 30=Somewhat, 40=Very, 50=Cannot
      # Post-2019: 0=NIU, 10=None, 20=Little, 30=Somewhat, 40=Very/Cannot
      flclimb_harmonized = case_when(
        FLCLIMB %in% c(97, 98, 99) ~ NA_real_,  # Unknown/refused
        FLCLIMB == 0 ~ 1,                        # NIU → no difficulty (key harmonization)
        FLCLIMB == 10 ~ 1,                       # No difficulty
        FLCLIMB %in% c(20, 21, 22) ~ 2,          # Little/some difficulty
        FLCLIMB == 30 ~ 3,                       # Somewhat difficult
        FLCLIMB %in% c(40, 50) ~ 4,              # Very difficult/cannot do
        TRUE ~ NA_real_
      )
    )

  message("  NHIS: ", nrow(data_nhis), " rows, years ",
          min(data_nhis$year), "-", max(data_nhis$year))
  message("  flclimb_harmonized distribution: ",
          paste(names(table(data_nhis$flclimb_harmonized)),
                table(data_nhis$flclimb_harmonized), sep = "=", collapse = ", "))
}

# --- MEPS ---
if (run_meps && exists("data_meps")) {
  message("Processing MEPS...")

  # Filter to 2007+ for consistent variable availability
  data_meps <- data_meps %>% filter(year >= 2007)

  # Add age group (scheme B)
  data_meps <- add_age_group(data_meps, age_var = age, scheme = "B")

  # Create comorbidity count
  # MEPS comorbidities: DIABETICEV, HYPERTENEV, CHEARTDIEV, STROKEV,
  #                     ARTHGLUPEV, CANCEREV, ASTHMAEV
  meps_comorb_vars <- c("DIABETICEV", "HYPERTENEV", "CHEARTDIEV", "STROKEV",
                        "ARTHGLUPEV", "CANCEREV", "ASTHMAEV")

  meps_comorb_vars <- intersect(meps_comorb_vars, names(data_meps))
  message("  Found comorbidity vars: ", paste(meps_comorb_vars, collapse = ", "))

  # MEPS uses standard coding: 0=No, 1=Yes for comorbidities
  data_meps <- data_meps %>%
    mutate(
      comorb_count = rowSums(across(all_of(meps_comorb_vars), ~ as.numeric(.x == 1)),
                             na.rm = TRUE),
      # Scale K6 to 0-6 range
      K6SUM_scaled = rescale_01(K6SUM, min_val = 0, max_val = 24) * 6,
      # Recode ANYLMT: 0=NIU->NA, 1=No limitation->0, 2=Has limitation->1
      anylmt_binary = case_when(
        ANYLMT == 1 ~ 0,
        ANYLMT == 2 ~ 1,
        TRUE ~ NA_real_
      )
    )

  message("  MEPS: ", nrow(data_meps), " rows, years ",
          min(data_meps$year), "-", max(data_meps$year))
}

# --- BRFSS ---
if (run_brfss && exists("data_brfss")) {
  message("Processing BRFSS...")

  # Add age group (scheme B)
  data_brfss <- add_age_group(data_brfss, age_var = age, scheme = "B")

  # Create comorbidity count
  # Variable selection rationale (see notes/fig3_reference.md):
  # - EXCLUDED: htn_dx (only available in odd years, causes zigzag)
  # - EXCLUDED: copd_dx, cancer_any_dx (only available 2011+)
  # - KEPT: 4 stable variables with good coverage from 2005+
  # BRFSS uses standard coding: 0=No, 1=Yes
  brfss_comorb_vars <- c("diabetes_dx", "chd_dx", "stroke_dx", "arthritis_dx")

  brfss_comorb_vars <- intersect(brfss_comorb_vars, names(data_brfss))
  message("  BRFSS comorbidity vars (excl. HTN): ", paste(brfss_comorb_vars, collapse = ", "))

  data_brfss <- data_brfss %>%
    mutate(
      comorb_count = rowSums(across(all_of(brfss_comorb_vars), ~ as.numeric(.x == 1)),
                             na.rm = TRUE)
    )

  message("  BRFSS: ", nrow(data_brfss), " rows, years ",
          min(data_brfss$year), "-", max(data_brfss$year))
}


# ==============================================================================
# PART 3: RUN AGE-STRATIFIED REGRESSIONS (FIGURE 3)
# ==============================================================================

message("\n========== Running age-stratified regressions ==========\n")

# Initialize result objects
coef_nhis <- NULL
coef_meps <- NULL
coef_brfss <- NULL

# --- NHIS Regressions (weights only for speed) ---
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

  # Functional: flclimb harmonized (ordinal 1-4, NIU treated as no difficulty)
  coef_nhis_functional <- regress_covariate_by_age_year(
    data = data_nhis,
    covariate_var = "flclimb_harmonized",
    covariate_label = "Difficulty Climbing (1-4)",
    survey_name = "NHIS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Combine and add category
  coef_nhis <- bind_rows(
    coef_nhis_chronic %>% mutate(category = "Comorbidity Count"),
    coef_nhis_mental %>% mutate(category = "Mental Health"),
    coef_nhis_functional %>% mutate(category = "Physical Health Limitations")
  )
}

# --- MEPS Regressions (weights only for speed) ---
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

  # Functional: Physical activity limitation (ADPALS, 0-4 scale)
  coef_meps_functional <- regress_covariate_by_age_year(
    data = data_meps,
    covariate_var = "ADPALS",
    covariate_label = "Physical Limitation (0-4)",
    survey_name = "MEPS",
    psu_var = NULL,
    strata_var = NULL
  )

  coef_meps <- bind_rows(
    coef_meps_chronic %>% mutate(category = "Comorbidity Count"),
    coef_meps_mental %>% mutate(category = "Mental Health"),
    coef_meps_functional %>% mutate(category = "Physical Health Limitations")
  )
}

# --- BRFSS Regressions (weights only) ---
if (run_brfss && exists("data_brfss")) {
  message("\n--- BRFSS ---")

  # Chronic: Comorbidity count (excl. HTN which is only available in odd years)
  # Also exclude even years 2002-2010 due to inconsistent variable coverage
  coef_brfss_chronic <- regress_covariate_by_age_year(
    data = data_brfss %>% filter(!(year %in% c(2002, 2004, 2006, 2008, 2010))),
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

  # Functional: Physical health bad days (0-30) - excludes mental health
  coef_brfss_functional <- regress_covariate_by_age_year(
    data = data_brfss,
    covariate_var = "phys_bad",
    covariate_label = "Physical Days (0-30)",
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  coef_brfss <- bind_rows(
    coef_brfss_chronic %>% mutate(category = "Comorbidity Count"),
    coef_brfss_mental %>% mutate(category = "Mental Health"),
    coef_brfss_functional %>% mutate(category = "Physical Health Limitations")
  )
}


# ==============================================================================
# PART 4: RUN PREVALENCE/MEAN CALCULATIONS (FIGURE 3B)
# ==============================================================================

message("\n========== Computing prevalence/means by age group ==========\n")

prev_nhis <- NULL
prev_meps <- NULL
prev_brfss <- NULL

# --- NHIS Prevalence (weights only for speed) ---
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

  prev_nhis_functional <- mean_by_age_year(
    data = data_nhis,
    var_name = "flclimb_harmonized",
    var_label = "Difficulty Climbing (1-4)",
    survey_name = "NHIS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_nhis <- bind_rows(
    prev_nhis_chronic %>% mutate(category = "Comorbidity Count"),
    prev_nhis_mental %>% mutate(category = "Mental Health"),
    prev_nhis_functional %>% mutate(category = "Physical Health Limitations")
  )
}

# --- MEPS Prevalence (weights only for speed) ---
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

  # Functional: Physical activity limitation (ADPALS, 0-4 scale)
  prev_meps_functional <- mean_by_age_year(
    data = data_meps,
    var_name = "ADPALS",
    var_label = "Physical Limitation (0-4)",
    survey_name = "MEPS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_meps <- bind_rows(
    prev_meps_chronic %>% mutate(category = "Comorbidity Count"),
    prev_meps_mental %>% mutate(category = "Mental Health"),
    prev_meps_functional %>% mutate(category = "Physical Health Limitations")
  )
}

# --- BRFSS Prevalence ---
if (run_brfss && exists("data_brfss")) {
  message("\n--- BRFSS ---")

  # Exclude even years 2002-2010 for chronic conditions (same as coefficient analysis)
  prev_brfss_chronic <- mean_by_age_year(
    data = data_brfss %>% filter(!(year %in% c(2002, 2004, 2006, 2008, 2010))),
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

  # Physical health bad days (0-30) - excludes mental health
  prev_brfss_functional <- mean_by_age_year(
    data = data_brfss,
    var_name = "phys_bad",
    var_label = "Physical Days (0-30)",
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  prev_brfss <- bind_rows(
    prev_brfss_chronic %>% mutate(category = "Comorbidity Count"),
    prev_brfss_mental %>% mutate(category = "Mental Health"),
    prev_brfss_functional %>% mutate(category = "Physical Health Limitations")
  )
}

# Free memory from large data objects
rm(data_nhis, data_meps, data_brfss); gc()


# ==============================================================================
# PART 5: SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

# Coefficients (Figure 3)
if (!is.null(coef_nhis)) {
  readr::write_csv(coef_nhis, file.path(tables_dir, paste0("fig3_coef_by_age_nhis_", date_suffix, ".csv")))
  readr::write_rds(coef_nhis, file.path(tables_dir, paste0("fig3_coef_by_age_nhis_", date_suffix, ".rds")))
}
if (!is.null(coef_meps)) {
  readr::write_csv(coef_meps, file.path(tables_dir, paste0("fig3_coef_by_age_meps_", date_suffix, ".csv")))
  readr::write_rds(coef_meps, file.path(tables_dir, paste0("fig3_coef_by_age_meps_", date_suffix, ".rds")))
}
if (!is.null(coef_brfss)) {
  readr::write_csv(coef_brfss, file.path(tables_dir, paste0("fig3_coef_by_age_brfss_", date_suffix, ".csv")))
  readr::write_rds(coef_brfss, file.path(tables_dir, paste0("fig3_coef_by_age_brfss_", date_suffix, ".rds")))
}

# Prevalence (Figure 3B)
if (!is.null(prev_nhis)) {
  readr::write_csv(prev_nhis, file.path(tables_dir, paste0("fig3b_prev_by_age_nhis_", date_suffix, ".csv")))
  readr::write_rds(prev_nhis, file.path(tables_dir, paste0("fig3b_prev_by_age_nhis_", date_suffix, ".rds")))
}
if (!is.null(prev_meps)) {
  readr::write_csv(prev_meps, file.path(tables_dir, paste0("fig3b_prev_by_age_meps_", date_suffix, ".csv")))
  readr::write_rds(prev_meps, file.path(tables_dir, paste0("fig3b_prev_by_age_meps_", date_suffix, ".rds")))
}
if (!is.null(prev_brfss)) {
  readr::write_csv(prev_brfss, file.path(tables_dir, paste0("fig3b_prev_by_age_brfss_", date_suffix, ".csv")))
  readr::write_rds(prev_brfss, file.path(tables_dir, paste0("fig3b_prev_by_age_brfss_", date_suffix, ".rds")))
}


# ==============================================================================
# PART 6: CREATE FIGURE 3 (COEFFICIENTS BY AGE GROUP)
# ==============================================================================

message("\n========== Creating Figure 3 ==========\n")

# --- Variable descriptions for each survey/category combination ---
# Short descriptions to appear under each panel title
var_descriptions <- list(
  # Chronic conditions
  BRFSS_chronic = "Comorbidity count",
  MEPS_chronic = "Comorbidity count",
  NHIS_chronic = "Comorbidity count",
  # Mental health
  BRFSS_mental = "Mental bad days (0-30)",
  MEPS_mental = "K6 distress (0-6)",
  NHIS_mental = "K6 distress (0-6)",
  # Functional health
  BRFSS_functional = "Physical bad days (0-30)",
  MEPS_functional = "Physical limitation (0-4)",
  NHIS_functional = "Difficulty climbing (1-4)"
)

# --- Helper function to create a single panel ---
create_age_subplot <- function(
    data,
    y_var = "coefficient",
    show_title = FALSE,
    title = NULL,
    subtitle = NULL,
    show_ylabel = FALSE,
    ylabel = "Coefficient",
    show_legend = FALSE,
    base_size = 11,
    xlim = NULL,
    show_hline = TRUE,
    discontinuity_year = NULL  # Add vertical line at discontinuity (e.g., 2019 NHIS redesign)
) {

  # Check if data has results
  if (is.null(data) || nrow(data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 4, color = "gray50") +
      theme_void()
    if (show_title && !is.null(title)) {
      p <- p + labs(title = title, subtitle = subtitle) +
        theme(plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = base_size - 1, hjust = 0.5, color = "gray50"))
    }
    return(p)
  }

  p <- ggplot(data, aes(x = year, y = .data[[y_var]],
                        color = age_group, group = age_group)) +
    geom_line(linewidth = 0.6, alpha = 0.8) +
    geom_point(size = 1.2, alpha = 0.8) +
    scale_color_manual(values = age_colors, name = "Age Group") +
    labs(
      x = NULL,
      y = if (show_ylabel) ylabel else NULL,
      title = if (show_title) title else NULL,
      subtitle = subtitle
    )

  if (show_hline) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.6)
  }

  # Add discontinuity annotation (e.g., NHIS 2019 redesign)
  if (!is.null(discontinuity_year)) {
    p <- p + geom_vline(xintercept = discontinuity_year, linetype = "dotted",
                        color = "red", alpha = 0.7, linewidth = 0.8)
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
      plot.subtitle = element_text(size = base_size - 1, hjust = 0.5, color = "gray50"),
      axis.title.y = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1, color = "gray30"),
      plot.margin = margin(4, 6, 4, 6),
      legend.position = if (show_legend) "bottom" else "none"
    )

  return(p)
}

# Helper to create empty placeholder
empty_subplot <- function(msg = "Not available", base_size = 11) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg, size = 4, color = "gray50") +
    theme_void() +
    theme(plot.margin = margin(4, 6, 4, 6))
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
# TRANSPOSED LAYOUT: Rows = Categories, Columns = Surveys
# Row 1: Comorbidity Count (BRFSS | MEPS | NHIS)
# Row 2: Mental Health (BRFSS | MEPS | NHIS)
# Row 3: Physical Health Limitations (BRFSS | MEPS | NHIS)
# ==============================================================================

# --- Row 1: Comorbidity Count ---
p_chronic_brfss <- create_age_subplot(
  coef_brfss %>% filter(category == "Comorbidity Count"),
  show_title = TRUE, title = "BRFSS",
  subtitle = var_descriptions$BRFSS_chronic,
  show_ylabel = TRUE, ylabel = "Comorbidity\nCount",
  xlim = xlim_brfss
)

p_chronic_meps <- create_age_subplot(
  coef_meps %>% filter(category == "Comorbidity Count"),
  show_title = TRUE, title = "MEPS",
  subtitle = var_descriptions$MEPS_chronic,
  xlim = xlim_meps
)

p_chronic_nhis <- create_age_subplot(
  coef_nhis %>% filter(category == "Comorbidity Count"),
  show_title = TRUE, title = "NHIS",
  subtitle = var_descriptions$NHIS_chronic,
  xlim = xlim_nhis
)

# --- Row 2: Mental Health ---
p_mental_brfss <- create_age_subplot(
  coef_brfss %>% filter(category == "Mental Health"),
  subtitle = var_descriptions$BRFSS_mental,
  show_ylabel = TRUE, ylabel = "Mental\nHealth",
  xlim = xlim_brfss
)

p_mental_meps <- create_age_subplot(
  coef_meps %>% filter(category == "Mental Health"),
  subtitle = var_descriptions$MEPS_mental,
  xlim = xlim_meps
)

p_mental_nhis <- create_age_subplot(
  coef_nhis %>% filter(category == "Mental Health"),
  subtitle = var_descriptions$NHIS_mental,
  xlim = xlim_nhis
)

# --- Row 3: Physical Health Limitations ---
p_func_brfss <- create_age_subplot(
  coef_brfss %>% filter(category == "Physical Health Limitations"),
  subtitle = var_descriptions$BRFSS_functional,
  show_ylabel = TRUE, ylabel = "Physical Health\nLimitations",
  xlim = xlim_brfss
)

p_func_meps <- create_age_subplot(
  coef_meps %>% filter(category == "Physical Health Limitations"),
  subtitle = var_descriptions$MEPS_functional,
  xlim = xlim_meps
)

p_func_nhis <- create_age_subplot(
  coef_nhis %>% filter(category == "Physical Health Limitations"),
  subtitle = var_descriptions$NHIS_functional,
  xlim = xlim_nhis
)

# --- Assemble Figure 3 (transposed: rows=categories, cols=surveys) ---
# Use patchwork's guide_area() for proper legend placement
fig3_grid <- ((p_chronic_brfss | p_chronic_meps | p_chronic_nhis) /
              (p_mental_brfss | p_mental_meps | p_mental_nhis) /
              (p_func_brfss | p_func_meps | p_func_nhis) /
              guide_area()) +
  plot_layout(heights = c(1, 1, 1, 0.1), guides = "collect") +
  plot_annotation(
    title = "Coefficient Stability Across Age Groups",
    subtitle = "Effect of each covariate on SRH (higher covariate = worse health → negative coefficient)",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    )
  ) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10)) &
  guides(color = guide_legend(nrow = 1))

fig3_final <- fig3_grid


# ==============================================================================
# PART 7: CREATE FIGURE 3B (PREVALENCE BY AGE GROUP)
# ==============================================================================

message("\n========== Creating Figure 3B ==========\n")

# --- Row 1: Comorbidity Count ---
p3b_chronic_brfss <- create_age_subplot(
  prev_brfss %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  show_title = TRUE, title = "BRFSS",
  subtitle = var_descriptions$BRFSS_chronic,
  show_ylabel = TRUE, ylabel = "Comorbidity\nCount",
  xlim = xlim_brfss, show_hline = FALSE
)

p3b_chronic_meps <- create_age_subplot(
  prev_meps %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  show_title = TRUE, title = "MEPS",
  subtitle = var_descriptions$MEPS_chronic,
  xlim = xlim_meps, show_hline = FALSE
)

p3b_chronic_nhis <- create_age_subplot(
  prev_nhis %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  show_title = TRUE, title = "NHIS",
  subtitle = var_descriptions$NHIS_chronic,
  xlim = xlim_nhis, show_hline = FALSE
)

# --- Row 2: Mental Health ---
p3b_mental_brfss <- create_age_subplot(
  prev_brfss %>% filter(category == "Mental Health"),
  y_var = "mean",
  subtitle = var_descriptions$BRFSS_mental,
  show_ylabel = TRUE, ylabel = "Mental\nHealth",
  xlim = xlim_brfss, show_hline = FALSE
)

p3b_mental_meps <- create_age_subplot(
  prev_meps %>% filter(category == "Mental Health"),
  y_var = "mean",
  subtitle = var_descriptions$MEPS_mental,
  xlim = xlim_meps, show_hline = FALSE
)

p3b_mental_nhis <- create_age_subplot(
  prev_nhis %>% filter(category == "Mental Health"),
  y_var = "mean",
  subtitle = var_descriptions$NHIS_mental,
  xlim = xlim_nhis, show_hline = FALSE
)

# --- Row 3: Physical Health Limitations ---
p3b_func_brfss <- create_age_subplot(
  prev_brfss %>% filter(category == "Physical Health Limitations"),
  y_var = "mean",
  subtitle = var_descriptions$BRFSS_functional,
  show_ylabel = TRUE, ylabel = "Physical Health\nLimitations",
  xlim = xlim_brfss, show_hline = FALSE
)

p3b_func_meps <- create_age_subplot(
  prev_meps %>% filter(category == "Physical Health Limitations"),
  y_var = "mean",
  subtitle = var_descriptions$MEPS_functional,
  xlim = xlim_meps, show_hline = FALSE
)

p3b_func_nhis <- create_age_subplot(
  prev_nhis %>% filter(category == "Physical Health Limitations"),
  y_var = "mean",
  subtitle = var_descriptions$NHIS_functional,
  xlim = xlim_nhis, show_hline = FALSE
)

# --- Assemble Figure 3B (transposed: rows=categories, cols=surveys) ---
# Use patchwork's guide_area() for proper legend placement
fig3b_grid <- ((p3b_chronic_brfss | p3b_chronic_meps | p3b_chronic_nhis) /
               (p3b_mental_brfss | p3b_mental_meps | p3b_mental_nhis) /
               (p3b_func_brfss | p3b_func_meps | p3b_func_nhis) /
               guide_area()) +
  plot_layout(heights = c(1, 1, 1, 0.1), guides = "collect") +
  plot_annotation(
    title = "Prevalence Trends by Age Group",
    subtitle = "Mean/prevalence of each covariate within age groups over time",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    )
  ) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10)) &
  guides(color = guide_legend(nrow = 1))

fig3b_final <- fig3b_grid


# ==============================================================================
# PART 8: SAVE FIGURES
# ==============================================================================

message("\n========== Saving figures ==========\n")

# Figure 3 (coefficients)
ggsave(
  filename = file.path(output_dir, paste0("fig3_coef_by_age_draft_", date_suffix, ".png")),
  plot = fig3_final,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3_coef_by_age.png"),
  plot = fig3_final,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3_coef_by_age.pdf"),
  plot = fig3_final,
  width = 12, height = 10
)
message("Saved: fig3_coef_by_age (.png and .pdf)")

# Figure 3B (prevalence)
ggsave(
  filename = file.path(output_dir, paste0("fig3b_prev_by_age_draft_", date_suffix, ".png")),
  plot = fig3b_final,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3b_prev_by_age.png"),
  plot = fig3b_final,
  width = 12, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3b_prev_by_age.pdf"),
  plot = fig3b_final,
  width = 12, height = 10
)
message("Saved: fig3b_prev_by_age (.png and .pdf)")


# ==============================================================================
# PART 9: VERIFICATION SUMMARY
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

message("\n========== Figure 3 & 3B Complete ==========\n")
