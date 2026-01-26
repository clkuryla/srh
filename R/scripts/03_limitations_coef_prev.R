# ==============================================================================
# 03_limitations_coef_prev.R
# Limitation Variables: Coefficient Stability & Prevalence Trends (MEPS Round Data)
#
# Purpose:
#   Analyzes functional limitation variables from MEPS round-level data to show:
#   - Coefficient stability: SRH ~ limitation relationship stable over time
#   - Prevalence trends: What's actually changing by age group
#
# Variables:
#   - ladl: ADL limitation
#   - laiadl: IADL limitation
#   - lmtphys: Physical limitation
#   - lmtwork: Work limitation
#   - lmtsoc: Social limitation
#   - lmtcog: Cognitive limitation
#   - mhlth_any_fairpoor: Fair/poor mental health
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
options(survey.lonely.psu = "adjust")

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
# VARIABLE DEFINITIONS
# ==============================================================================

# Limitation variables to analyze
# Note: Data now filtered to self-respondents only (PROXY == 1) in wrangling,
# so mhlth_any_fairpoor can be included without proxy response bias
limitation_vars <- c(
  "ladl",              # ADL limitation
  "laiadl",            # IADL limitation
  "lmtphys",           # Physical limitation
  "lmtwork",           # Work limitation
  "lmtsoc",            # Social limitation
  "lmtcog",            # Cognitive limitation
  "mhlth_any_fairpoor" # Fair/poor mental health (self-report only)
)

# Human-readable labels for plots
var_labels <- c(
  ladl = "ADL Limitation",
  laiadl = "IADL Limitation",
  lmtphys = "Physical Limitation",
  lmtwork = "Work Limitation",
  lmtsoc = "Social Limitation",
  lmtcog = "Cognitive Limitation",
  mhlth_any_fairpoor = "Fair/Poor Mental Health"
)


# ==============================================================================
# PART 1: LOAD DATA
# ==============================================================================

message("\n========== Loading MEPS round-level data ==========\n")

data_path <- derived_path("data_meps_round_personyear.rds")

if (!file.exists(data_path)) {
  stop("MEPS round-level data not found at: ", data_path,
       "\nRun the wrangling script first.")
}

data_raw <- readr::read_rds(data_path)
message("Loaded: ", nrow(data_raw), " person-years")

# Filter to adults 18-89 with valid weights and SRH
data <- data_raw %>%
  filter(
    age >= 18,
    age <= 89,
    wt > 0,
    !is.na(srh)
  )

message("After filtering (age 18-89, valid wt & SRH): ", nrow(data), " rows")
message("Year range: ", min(data$year), " - ", max(data$year))


# ==============================================================================
# PART 2: ADD AGE GROUPS
# ==============================================================================

message("\n========== Adding age groups (Scheme B) ==========\n")

data <- add_age_group(data, age_var = age, scheme = "B")

# Check age group distribution
age_dist <- data %>%
  count(age_group) %>%
  mutate(pct = round(100 * n / sum(n), 1))

message("Age group distribution:")
for (i in seq_len(nrow(age_dist))) {
  message("  ", age_dist$age_group[i], ": ",
          format(age_dist$n[i], big.mark = ","), " (", age_dist$pct[i], "%)")
}


# ==============================================================================
# PART 3: CHECK VARIABLE AVAILABILITY
# ==============================================================================

message("\n========== Checking limitation variables ==========\n")

# Check which variables exist and have valid data
var_summary <- tibble(variable = limitation_vars) %>%
  mutate(
    exists = variable %in% names(data),
    n_valid = map_int(variable, ~ if (.x %in% names(data)) sum(!is.na(data[[.x]])) else 0L),
    pct_valid = round(100 * n_valid / nrow(data), 1),
    n_positive = map_int(variable, ~ if (.x %in% names(data)) sum(data[[.x]] == 1, na.rm = TRUE) else 0L)
  )

message("Variable availability:")
for (i in seq_len(nrow(var_summary))) {
  row <- var_summary[i, ]
  if (row$exists) {
    message("  ", row$variable, ": ", format(row$n_valid, big.mark = ","),
            " valid (", row$pct_valid, "%), ",
            format(row$n_positive, big.mark = ","), " positive cases")
  } else {
    message("  ", row$variable, ": NOT FOUND IN DATA")
  }
}

# Filter to variables that exist
limitation_vars <- limitation_vars[limitation_vars %in% names(data)]
message("\nProceeding with ", length(limitation_vars), " variables")


# ==============================================================================
# PART 4: RUN COEFFICIENT ANALYSIS (FIGURE A)
# ==============================================================================

message("\n========== Running coefficient analysis ==========\n")

# Run age-stratified regressions for each limitation variable
coef_results <- map_dfr(limitation_vars, function(var) {

  message("\n--- Analyzing: ", var_labels[var], " ---")

  result <- regress_covariate_by_age_year(
    data = data,
    covariate_var = var,
    covariate_label = var_labels[var],
    survey_name = "MEPS",
    age_group_var = "age_group",
    age_groups = AGE_GROUPS,
    srh_var = "srh",
    year_var = "year",
    psu_var = NULL,   # MEPS round data: weights only
    strata_var = NULL,
    wt_var = "wt",
    min_n = 50
  )

  if (!is.null(result)) {
    result <- result %>%
      mutate(category = var_labels[var])
  }

  return(result)
})

message("\nCoefficient analysis complete: ", nrow(coef_results), " age-year cells")


# ==============================================================================
# PART 5: RUN PREVALENCE ANALYSIS (FIGURE B)
# ==============================================================================

message("\n========== Running prevalence analysis ==========\n")

# Compute weighted means for each limitation variable by age group and year
prev_results <- map_dfr(limitation_vars, function(var) {

  message("\n--- Computing prevalence: ", var_labels[var], " ---")

  result <- mean_by_age_year(
    data = data,
    var_name = var,
    var_label = var_labels[var],
    survey_name = "MEPS",
    age_group_var = "age_group",
    age_groups = AGE_GROUPS,
    year_var = "year",
    psu_var = NULL,   # MEPS round data: weights only
    strata_var = NULL,
    wt_var = "wt",
    min_n = 50
  )

  if (!is.null(result)) {
    result <- result %>%
      mutate(category = var_labels[var])
  }

  return(result)
})

message("\nPrevalence analysis complete: ", nrow(prev_results), " age-year cells")

# Free memory
rm(data_raw); gc()


# ==============================================================================
# PART 6: SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

# Coefficients
if (!is.null(coef_results) && nrow(coef_results) > 0) {
  readr::write_csv(coef_results,
    file.path(tables_dir, paste0("fig_lmt_coef_meps_", date_suffix, ".csv")))
  readr::write_rds(coef_results,
    file.path(tables_dir, paste0("fig_lmt_coef_meps_", date_suffix, ".rds")))
  message("Saved coefficient tables")
}

# Prevalence
if (!is.null(prev_results) && nrow(prev_results) > 0) {
  readr::write_csv(prev_results,
    file.path(tables_dir, paste0("fig_lmt_prev_meps_", date_suffix, ".csv")))
  readr::write_rds(prev_results,
    file.path(tables_dir, paste0("fig_lmt_prev_meps_", date_suffix, ".rds")))
  message("Saved prevalence tables")
}


# ==============================================================================
# PART 7: CREATE FIGURES
# ==============================================================================

message("\n========== Creating figures ==========\n")

# Get year range for x-axis
xlim <- c(min(data$year), max(data$year))

# --- Okabe-Ito palette for age groups ---
age_colors_oi <- c(
  "18-29" = "#D55E00",  # vermillion
  "30-39" = "#E69F00",  # orange
  "40-49" = "#F0E442",  # yellow
  "50-59" = "#009E73",  # bluish green
  "60-69" = "#56B4E9",  # sky blue
  "70-79" = "#0072B2",  # blue
  "80-89" = "#CC79A7"   # reddish purple
)

# --- Helper function for creating faceted age-line plots ---
create_faceted_plot <- function(
    data,
    y_var,
    y_label,
    title,
    show_hline = FALSE,
    free_y = TRUE
) {

  # Ensure age_group is ordered factor
  data <- data %>%
    mutate(age_group = factor(age_group, levels = AGE_GROUPS))

  p <- ggplot(data, aes(x = year, y = .data[[y_var]],
                        color = age_group, group = age_group)) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 1.2, alpha = 0.8) +
    facet_wrap(~ category, scales = if (free_y) "free_y" else "fixed", ncol = 4) +
    scale_color_manual(values = age_colors_oi, name = "Age Group") +
    scale_x_continuous(limits = xlim, breaks = scales::pretty_breaks(n = 4)) +
    labs(
      title = title,
      x = "Year",
      y = y_label
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.25),
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9, color = "gray30"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9)
    ) +
    guides(color = guide_legend(nrow = 1))

  if (show_hline) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed",
                        color = "gray50", alpha = 0.6)
  }

  return(p)
}


# --- Figure A: Coefficient Stability ---
message("Creating coefficient figure...")

fig_coef <- create_faceted_plot(
  data = coef_results,
  y_var = "coefficient",
  y_label = "Coefficient (SRH ~ Limitation)",
  title = "Coefficient Stability: Limitation Variables on SRH (MEPS)",
  show_hline = TRUE,
  free_y = TRUE
)


# --- Figure B: Prevalence Trends ---
message("Creating prevalence figure...")

fig_prev <- create_faceted_plot(
  data = prev_results,
  y_var = "mean",
  y_label = "Weighted Prevalence",
  title = "Prevalence Trends: Functional Limitations by Age Group (MEPS)",
  show_hline = FALSE,
  free_y = TRUE
)


# ==============================================================================
# PART 8: SAVE FIGURES
# ==============================================================================

message("\n========== Saving figures ==========\n")

# Coefficient figure
ggsave(
  filename = file.path(output_dir, paste0("fig_lmt_coef_draft_", date_suffix, ".png")),
  plot = fig_coef,
  width = 14, height = 6, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig_lmt_coef.png"),
  plot = fig_coef,
  width = 14, height = 6, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig_lmt_coef.pdf"),
  plot = fig_coef,
  width = 14, height = 6
)
message("Saved: fig_lmt_coef (.png and .pdf)")

# Prevalence figure
ggsave(
  filename = file.path(output_dir, paste0("fig_lmt_prev_draft_", date_suffix, ".png")),
  plot = fig_prev,
  width = 14, height = 6, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig_lmt_prev.png"),
  plot = fig_prev,
  width = 14, height = 6, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig_lmt_prev.pdf"),
  plot = fig_prev,
  width = 14, height = 6
)
message("Saved: fig_lmt_prev (.png and .pdf)")


# ==============================================================================
# PART 9: VERIFICATION SUMMARY
# ==============================================================================

message("\n========== Verification Summary ==========\n")

# --- Check coefficient signs (should be negative: limitation -> worse SRH) ---
message("\n--- Coefficient Direction Check ---")
message("(All should be negative: having limitation associated with lower SRH)")

coef_summary <- coef_results %>%
  group_by(category) %>%
  summarise(
    n_cells = n(),
    mean_coef = mean(coefficient, na.rm = TRUE),
    min_coef = min(coefficient, na.rm = TRUE),
    max_coef = max(coefficient, na.rm = TRUE),
    pct_negative = round(100 * mean(coefficient < 0, na.rm = TRUE), 1),
    .groups = "drop"
  )

for (i in seq_len(nrow(coef_summary))) {
  row <- coef_summary[i, ]
  direction <- ifelse(row$mean_coef < 0, "NEGATIVE (correct)", "POSITIVE (check!)")
  message("  ", row$category, ": mean = ", round(row$mean_coef, 3),
          " [", round(row$min_coef, 3), ", ", round(row$max_coef, 3), "]",
          " - ", direction, " (", row$pct_negative, "% negative)")
}


# --- Check prevalence patterns (should increase with age for most) ---
message("\n--- Prevalence by Age Check ---")

prev_age_summary <- prev_results %>%
  group_by(category, age_group) %>%
  summarise(
    mean_prev = mean(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = age_group, values_from = mean_prev)

message("\nPrevalence by age group (columns: 18-29 to 80-89):")
print(prev_age_summary, n = Inf)


# --- Check temporal trends in youth (18-29) ---
message("\n--- Youth (18-29) Temporal Trends ---")

youth_trends <- prev_results %>%
  filter(age_group == "18-29") %>%
  group_by(category) %>%
  summarise(
    first_year = min(year),
    last_year = max(year),
    first_prev = mean[year == first_year][1],
    last_prev = mean[year == last_year][1],
    change = last_prev - first_prev,
    pct_change = round(100 * (last_prev - first_prev) / first_prev, 1),
    .groups = "drop"
  )

for (i in seq_len(nrow(youth_trends))) {
  row <- youth_trends[i, ]
  direction <- ifelse(row$change > 0, "INCREASING",
               ifelse(row$change < 0, "DECREASING", "STABLE"))
  message("  ", row$category, ": ",
          round(row$first_prev, 3), " (", row$first_year, ") -> ",
          round(row$last_prev, 3), " (", row$last_year, ") - ",
          direction, " (", row$pct_change, "%)")
}


# --- Sample size check ---
message("\n--- Sample Size Check ---")

n_summary <- coef_results %>%
  group_by(category, age_group) %>%
  summarise(
    n_years = n(),
    median_n = median(n_unweighted),
    min_n = min(n_unweighted),
    .groups = "drop"
  ) %>%
  group_by(category) %>%
  summarise(
    total_cells = sum(n_years),
    min_n_any_cell = min(min_n),
    median_n_typical = median(median_n),
    .groups = "drop"
  )

for (i in seq_len(nrow(n_summary))) {
  row <- n_summary[i, ]
  message("  ", row$category, ": ", row$total_cells, " cells, ",
          "min n = ", row$min_n_any_cell, ", median n = ", round(row$median_n_typical))
}


message("\n========== Analysis Complete ==========\n")
message("Coefficient figure: output/figures/fig_lmt_coef.png")
message("Prevalence figure: output/figures/fig_lmt_prev.png")
message("Tables saved in: output/tables/")
