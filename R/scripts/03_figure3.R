# ==============================================================================
# 03_figure3.R
# Figure 3: Coefficient Stability
#
# Purpose: Show that coefficients of health covariates on SRH remain stable
#          over time, supporting the interpretation that "what SRH measures"
#          hasn't changed - only the prevalence of conditions has.
#
# Layout: 3 rows (BRFSS, MEPS, NHIS) × 3 columns (Mental Health, Comorbidities,
#         Functional Health)
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

# Set theme
theme_set(theme_srh())

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)

# Output directory
output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")


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
# Tries to load from saved RDS first, otherwise sources wrangling script
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

# Load BRFSS (memory-intensive)
if (run_brfss) {
  data_brfss <- load_survey_data("BRFSS",
    here("analysis", "data_wrangling_code", "wrangle_brfss.R"))
}


# ==============================================================================
# PART 2: DEFINE COVARIATE LISTS
# ==============================================================================

# Comorbidities: Use standardized names across surveys
# For binary variables, coefficient = effect of having condition vs not

# NHIS comorbidities (binary: 0=No, 1=Yes after recoding in wrangling)
nhis_comorbidities <- c(
  "DIABETICEV" = "Diabetes",
  "HYPERTENEV" = "Hypertension",
  "CHEARTDIEV" = "Heart Disease",
  "STROKEV" = "Stroke",
  "ARTHGLUPEV" = "Arthritis",
  "COPDEV" = "COPD",
  "CANCEREV" = "Cancer",
  "ASTHMAEV" = "Asthma"
)

nhis_mental_health <- c(
  "k6_scaled" = "K6 (scaled)"
)

nhis_functional <- c(
  "flclimb_binary" = "Difficulty Climbing"
)

# MEPS comorbidities
meps_comorbidities <- c(
  "DIABETICEV" = "Diabetes",
  "HYPERTENEV" = "Hypertension",
  "CHEARTDIEV" = "Heart Disease",
  "STROKEV" = "Stroke",
  "ARTHGLUPEV" = "Arthritis",
  "CANCEREV" = "Cancer",
  "ASTHMAEV" = "Asthma"
)

meps_mental_health <- c(
  "K6SUM" = "K6 Distress",
  "PHQ2" = "PHQ-2"
)

meps_functional <- c(
  "ANYLMT" = "Any Limitation",
  "ADPALS" = "Phys Accomplished Less",
  "ADMALS" = "Mental Accomplished Less"
)

# BRFSS comorbidities
brfss_comorbidities <- c(
  "diabetes_dx" = "Diabetes",
  "htn_dx" = "Hypertension",
  "chd_dx" = "Heart Disease",
  "stroke_dx" = "Stroke",
  "arthritis_dx" = "Arthritis",
  "copd_dx" = "COPD",
  "cancer_any_dx" = "Cancer"
)

brfss_mental_health <- c(
  "ment_bad" = "Mental Health Days"
)

brfss_functional <- c(
  "phys_bad" = "Physical Health Days",
  "activlim_bad" = "Activity Limitation Days"
)


# ==============================================================================
# PART 3: RUN REGRESSIONS
# ==============================================================================

message("\n========== Running regressions ==========\n")

# Initialize result objects
coef_nhis <- NULL
coef_meps <- NULL
coef_brfss <- NULL

# --- NHIS Regressions ---
if (run_nhis && exists("data_nhis")) {
  message("\n--- NHIS ---")

  # Filter NHIS to start at specified year
  data_nhis <- data_nhis %>% filter(year >= nhis_start_year)
  message("  Filtered NHIS to years >= ", nhis_start_year, " (", nrow(data_nhis), " rows)")

  coef_nhis_comorb <- regress_covariates_by_year(
    data = data_nhis,
    covariate_list = nhis_comorbidities,
    survey_name = "NHIS",
    srh_var = "srh",
    year_var = "year",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt"
  )

  # For mental health, add K6 scaled to 0-6 range (like MEPS)
  data_nhis_scaled <- data_nhis %>%
    mutate(
      k6_scaled = rescale_01(k6, min_val = 0, max_val = 24) * 6  # Rescale to 0-6
    )

  coef_nhis_mental <- regress_covariates_by_year(
    data = data_nhis_scaled,
    covariate_list = nhis_mental_health,
    survey_name = "NHIS"
  )

  # For functional health, create binary for difficulty climbing
  # FLCLIMB: 1=Not difficult, 2=Only a little, 3=Somewhat, 4=Very/Can't do
  # Binary: 1 = any difficulty (values 2-4)
  data_nhis_func <- data_nhis %>%
    mutate(
      flclimb_binary = case_when(
        flclimb == 1 ~ 0,  # No difficulty
        flclimb >= 2 ~ 1,  # Any difficulty
        TRUE ~ NA_real_
      )
    )

  coef_nhis_functional <- regress_covariates_by_year(
    data = data_nhis_func,
    covariate_list = nhis_functional,
    survey_name = "NHIS"
  )

  # Combine NHIS results
  coef_nhis <- bind_rows(coef_nhis_comorb, coef_nhis_mental, coef_nhis_functional)

  # Free memory
  rm(data_nhis, data_nhis_scaled, data_nhis_func); gc()
}


# --- MEPS Regressions ---
if (run_meps && exists("data_meps")) {
  message("\n--- MEPS ---")

  coef_meps_comorb <- regress_covariates_by_year(
    data = data_meps,
    covariate_list = meps_comorbidities,
    survey_name = "MEPS",
    srh_var = "srh",
    year_var = "year",
    psu_var = "psu",
    strata_var = "strata",
    wt_var = "wt"
  )

  # For mental health, rescale K6 and PHQ2 to 0-6 range for comparability
  data_meps_scaled <- data_meps %>%
    mutate(
      K6SUM_scaled = rescale_01(K6SUM, min_val = 0, max_val = 24) * 6,  # Rescale to 0-6
      PHQ2_scaled = PHQ2  # Already 0-6
    )

  coef_meps_mental <- regress_covariates_by_year(
    data = data_meps_scaled,
    covariate_list = c("K6SUM_scaled" = "K6 (scaled)", "PHQ2" = "PHQ-2"),
    survey_name = "MEPS"
  )

  coef_meps_functional <- regress_covariates_by_year(
    data = data_meps,
    covariate_list = meps_functional,
    survey_name = "MEPS"
  )

  # Combine MEPS results
  coef_meps <- bind_rows(coef_meps_comorb, coef_meps_mental, coef_meps_functional)

  # Free memory
  rm(data_meps, data_meps_scaled); gc()
}


# --- BRFSS Regressions ---
# BRFSS uses weights only (no strata/PSU for simplicity)
if (run_brfss && exists("data_brfss")) {
  message("\n--- BRFSS ---")
  message("  Using weights only (no strata/PSU)")

  coef_brfss_comorb <- regress_covariates_by_year(
    data = data_brfss,
    covariate_list = brfss_comorbidities,
    survey_name = "BRFSS",
    srh_var = "srh",
    year_var = "year",
    psu_var = NULL,      # No PSU
    strata_var = NULL,   # No strata
    wt_var = "wt"
  )

  # For mental health days, rescale to 0-1 (per 30 days) for interpretability
  data_brfss_scaled <- data_brfss %>%
    mutate(
      ment_bad_scaled = ment_bad / 30,  # 0-1 scale
      phys_bad_scaled = phys_bad / 30,
      activlim_bad_scaled = activlim_bad / 30
    )

  coef_brfss_mental <- regress_covariates_by_year(
    data = data_brfss_scaled,
    covariate_list = c("ment_bad_scaled" = "Mental Days (scaled)"),
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  coef_brfss_functional <- regress_covariates_by_year(
    data = data_brfss_scaled,
    covariate_list = c("phys_bad_scaled" = "Physical Days (scaled)",
                       "activlim_bad_scaled" = "Activity Days (scaled)"),
    survey_name = "BRFSS",
    psu_var = NULL,
    strata_var = NULL
  )

  # Combine BRFSS results
  coef_brfss <- bind_rows(coef_brfss_comorb, coef_brfss_mental, coef_brfss_functional)

  # Free memory
  rm(data_brfss, data_brfss_scaled); gc()
}


# ==============================================================================
# PART 4: SAVE COEFFICIENT TABLES
# ==============================================================================

message("\n========== Saving coefficient tables ==========\n")

if (!is.null(coef_nhis)) save_fig3_coefficients_table(coef_nhis, "NHIS", tables_dir)
if (!is.null(coef_meps)) save_fig3_coefficients_table(coef_meps, "MEPS", tables_dir)
if (!is.null(coef_brfss)) save_fig3_coefficients_table(coef_brfss, "BRFSS", tables_dir)


# ==============================================================================
# PART 5: CREATE PLOTS
# ==============================================================================

message("\n========== Creating plots ==========\n")

# Color palette for covariate categories
covariate_colors <- c(
  # Comorbidities (warm colors)
  "Diabetes" = "#D55E00",
  "Hypertension" = "#E69F00",
  "Heart Disease" = "#CC79A7",
  "Stroke" = "#882255",
  "Arthritis" = "#999999",
  "COPD" = "#117733",
  "Cancer" = "#332288",
  "Asthma" = "#88CCEE",
  # Mental health (blue tones)
  "Depression" = "#0072B2",
  "Anxiety" = "#56B4E9",
  "K6 (scaled)" = "#0072B2",
  "K6 Distress" = "#0072B2",
  "PHQ-2" = "#56B4E9",
  "Mental Days (scaled)" = "#0072B2",
  # Functional (green tones)
  "Any Limitation" = "#009E73",
  "Phys Accomplished Less" = "#44AA99",
  "Mental Accomplished Less" = "#117733",
  "Physical Days (scaled)" = "#009E73",
  "Activity Days (scaled)" = "#44AA99",
  "Difficulty Climbing" = "#009E73"
)

# Define covariate categories for filtering
comorbidity_labels <- c("Diabetes", "Hypertension", "Heart Disease", "Stroke",
                        "Arthritis", "COPD", "Cancer", "Asthma")
mental_labels <- c("Depression", "Anxiety", "K6 (scaled)", "K6 Distress",
                   "PHQ-2", "Mental Days (scaled)")
functional_labels <- c("Any Limitation", "Phys Accomplished Less",
                       "Mental Accomplished Less", "Physical Days (scaled)",
                       "Activity Days (scaled)", "Difficulty Climbing")


# --- Helper function to create a single panel ---
# Following figure_1_combined pattern: survey names on top row, row labels on left column
create_coef_subplot <- function(
    data,
    show_title = FALSE,
    title = NULL,
    show_ylabel = FALSE,
    ylabel = "Coefficient",
    show_legend = TRUE,
    base_size = 12,
    tilt_x_labels = 45,
    xlim = NULL  # Optional x-axis limits for alignment
) {

  # Check if data has results - create empty plot if not
  if (is.null(data) || nrow(data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 5, color = "gray50") +
      theme_void()
    if (show_title && !is.null(title)) {
      p <- p + labs(title = title) +
        theme(plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5))
    }
    return(p)
  }

  p <- ggplot(data, aes(x = year, y = coefficient,
                        color = covariate_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.5) +
    scale_color_manual(values = covariate_colors, name = NULL) +
    labs(
      x = NULL,
      y = if (show_ylabel) ylabel else NULL,
      title = if (show_title) title else NULL
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4), limits = xlim)

  # X-axis label styling
  x_tick_style <- if (tilt_x_labels != 0) {
    element_text(size = base_size - 2, angle = tilt_x_labels, hjust = 1, vjust = 1)
  } else {
    element_text(size = base_size - 2)
  }

  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.25),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = base_size + 1),
      axis.text = element_text(size = base_size - 1, color = "gray30"),
      axis.text.x = x_tick_style,
      plot.margin = margin(4, 6, 4, 6),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size),
      legend.key.size = unit(1, "lines"),
      legend.background = element_rect(fill = "white", color = NA)
    ) +
    guides(color = guide_legend(nrow = 2), fill = "none")

  return(p)
}

# Helper to create empty placeholder plot
empty_subplot <- function(show_title = FALSE, title = NULL, show_ylabel = FALSE,
                          ylabel = NULL, msg = "Not available", base_size = 12) {
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg,
             size = 5, color = "gray50") +
    theme_void() +
    theme(plot.margin = margin(4, 6, 4, 6))

  if (show_title && !is.null(title)) {
    p <- p + labs(title = title) +
      theme(plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5))
  }
  if (show_ylabel && !is.null(ylabel)) {
    p <- p + labs(y = ylabel) +
      theme(axis.title.y = element_text(size = base_size + 1, angle = 90))
  }
  return(p)
}


# ==============================================================================
# PART 6: COMBINE INTO 3×3 GRID (TRANSPOSED)
# ==============================================================================

message("\n========== Combining into grid ==========\n")

# Layout (transposed):
#   Rows: BRFSS, MEPS, NHIS (survey names as row labels on left)
#   Columns: Chronic Conditions, Mental Health, Functional Health (headers on top)

# Calculate x-axis limits for each row (survey) to align years within each row
get_year_range <- function(coef_df) {
  if (is.null(coef_df) || nrow(coef_df) == 0) return(c(NA, NA))
  c(min(coef_df$year, na.rm = TRUE), max(coef_df$year, na.rm = TRUE))
}

xlim_brfss <- get_year_range(coef_brfss)
xlim_meps <- get_year_range(coef_meps)
xlim_nhis <- get_year_range(coef_nhis)

message("X-axis limits: BRFSS=", paste(xlim_brfss, collapse="-"),
        ", MEPS=", paste(xlim_meps, collapse="-"),
        ", NHIS=", paste(xlim_nhis, collapse="-"))

# --- Row 1: BRFSS ---
# Column 1: Chronic Conditions
if (!is.null(coef_brfss)) {
  p_r1c1 <- create_coef_subplot(
    coef_brfss %>% filter(covariate_label %in% comorbidity_labels),
    show_title = TRUE, title = "Chronic Conditions",
    show_ylabel = TRUE, ylabel = "BRFSS",
    show_legend = TRUE, xlim = xlim_brfss
  )
} else {
  p_r1c1 <- empty_subplot(show_title = TRUE, title = "Chronic Conditions",
                          show_ylabel = TRUE, ylabel = "BRFSS")
}

# Column 2: Mental Health
if (!is.null(coef_brfss)) {
  p_r1c2 <- create_coef_subplot(
    coef_brfss %>% filter(covariate_label %in% mental_labels),
    show_title = TRUE, title = "Mental Health",
    show_ylabel = FALSE,
    show_legend = TRUE, xlim = xlim_brfss
  )
} else {
  p_r1c2 <- empty_subplot(show_title = TRUE, title = "Mental Health")
}

# Column 3: Functional Health
if (!is.null(coef_brfss)) {
  p_r1c3 <- create_coef_subplot(
    coef_brfss %>% filter(covariate_label %in% functional_labels),
    show_title = TRUE, title = "Functional Health",
    show_ylabel = FALSE,
    show_legend = TRUE, xlim = xlim_brfss
  )
} else {
  p_r1c3 <- empty_subplot(show_title = TRUE, title = "Functional Health")
}

# --- Row 2: MEPS ---
if (!is.null(coef_meps)) {
  p_r2c1 <- create_coef_subplot(
    coef_meps %>% filter(covariate_label %in% comorbidity_labels),
    show_title = FALSE,
    show_ylabel = TRUE, ylabel = "MEPS",
    show_legend = TRUE, xlim = xlim_meps
  )
} else {
  p_r2c1 <- empty_subplot(show_ylabel = TRUE, ylabel = "MEPS")
}

if (!is.null(coef_meps)) {
  p_r2c2 <- create_coef_subplot(
    coef_meps %>% filter(covariate_label %in% mental_labels),
    show_title = FALSE,
    show_ylabel = FALSE,
    show_legend = TRUE, xlim = xlim_meps
  )
} else {
  p_r2c2 <- empty_subplot()
}

if (!is.null(coef_meps)) {
  p_r2c3 <- create_coef_subplot(
    coef_meps %>% filter(covariate_label %in% functional_labels),
    show_title = FALSE,
    show_ylabel = FALSE,
    show_legend = TRUE, xlim = xlim_meps
  )
} else {
  p_r2c3 <- empty_subplot()
}

# --- Row 3: NHIS ---
if (!is.null(coef_nhis)) {
  p_r3c1 <- create_coef_subplot(
    coef_nhis %>% filter(covariate_label %in% comorbidity_labels),
    show_title = FALSE,
    show_ylabel = TRUE, ylabel = "NHIS",
    show_legend = TRUE, xlim = xlim_nhis
  )
} else {
  p_r3c1 <- empty_subplot(show_ylabel = TRUE, ylabel = "NHIS")
}

if (!is.null(coef_nhis)) {
  p_r3c2 <- create_coef_subplot(
    coef_nhis %>% filter(covariate_label %in% mental_labels),
    show_title = FALSE,
    show_ylabel = FALSE,
    show_legend = TRUE, xlim = xlim_nhis
  )
} else {
  p_r3c2 <- empty_subplot()
}

if (!is.null(coef_nhis)) {
  p_r3c3 <- create_coef_subplot(
    coef_nhis %>% filter(covariate_label %in% functional_labels),
    show_title = FALSE,
    show_ylabel = FALSE,
    show_legend = TRUE, xlim = xlim_nhis
  )
} else {
  p_r3c3 <- empty_subplot()
}


# --- Assemble the 3×3 grid ---
row1 <- p_r1c1 | p_r1c2 | p_r1c3
row2 <- p_r2c1 | p_r2c2 | p_r2c3
row3 <- p_r3c1 | p_r3c2 | p_r3c3

fig3_grid <- (row1 / row2 / row3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Figure 3: Stability of Covariate-SRH Associations Over Time",
    subtitle = "Coefficients from survey-weighted regressions: SRH ~ covariate",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 13, color = "gray40", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  ) &
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(t = 10),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.2, "lines"),
    legend.background = element_rect(fill = "white", color = NA)
  )


# ==============================================================================
# PART 7: SAVE FIGURE
# ==============================================================================

message("\n========== Saving figure ==========\n")

# Save draft with date
ggsave(
  filename = file.path(output_dir, paste0("fig3_coefficient_stability_draft_", date_suffix, ".png")),
  plot = fig3_grid,
  width = 14,
  height = 12,
  dpi = 300
)
message("Saved: fig3_coefficient_stability_draft_", date_suffix, ".png")

# Save final versions (will be overwritten as needed)
ggsave(
  filename = file.path(output_dir, "fig3_coefficient_stability.pdf"),
  plot = fig3_grid,
  width = 14,
  height = 12
)
ggsave(
  filename = file.path(output_dir, "fig3_coefficient_stability.png"),
  plot = fig3_grid,
  width = 14,
  height = 12,
  dpi = 300
)
message("Saved: fig3_coefficient_stability.pdf and .png")


# ==============================================================================
# PART 8: VERIFICATION SUMMARY
# ==============================================================================

message("\n========== Verification Summary ==========\n")

# Check coefficient directions (should be negative for most comorbidities)
check_directions <- function(coef_df, name) {
  if (is.null(coef_df) || nrow(coef_df) == 0) {
    message("  ", name, ": No data")
    return(invisible(NULL))
  }

  summary_df <- coef_df %>%
    group_by(covariate_label) %>%
    summarise(
      n_years = n(),
      mean_coef = mean(coefficient, na.rm = TRUE),
      min_coef = min(coefficient, na.rm = TRUE),
      max_coef = max(coefficient, na.rm = TRUE),
      all_negative = all(coefficient < 0),
      .groups = "drop"
    )

  message("  ", name, ":")
  for (i in 1:nrow(summary_df)) {
    row <- summary_df[i, ]
    direction <- ifelse(row$mean_coef < 0, "(-)", "(+)")
    stable <- ifelse(abs(row$max_coef - row$min_coef) < abs(row$mean_coef), "stable", "varying")
    message("    ", row$covariate_label, ": mean=", round(row$mean_coef, 3),
            " ", direction, " [", row$n_years, " years, ", stable, "]")
  }
}

check_directions(coef_nhis, "NHIS")
check_directions(coef_meps, "MEPS")
check_directions(coef_brfss, "BRFSS")

message("\n========== Figure 3 complete ==========\n")
