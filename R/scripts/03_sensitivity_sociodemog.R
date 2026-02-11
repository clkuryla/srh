# ==============================================================================
# 03_sensitivity_sociodemog.R
# Figure S1 & S2: Sociodemographic Sensitivity Analyses
#
# Purpose:
#   Create sensitivity analysis figures examining how sociodemographic
#   covariates (sex, race, education) relate to the SRH convergence phenomenon.
#
# Figure S1: Age coefficient on SRH, adjusted for each covariate
#   - 3 rows (Sex, Race, Education) x 6 columns (all surveys)
#   - Shows whether convergence persists after adjustment
#
# Figure S2: Covariate coefficients on SRH over time
#   - Panel A: Pooled across all ages
#   - Panel B: By age group (rainbow lines)
#
# Harmonized Variables:
#   - sex: "Male", "Female"
#   - race_includehisp: White, Black, AIAN, Asian, Hispanic, Other
#   - educ_3cat: 1=LT HS, 2=HS/Some college, 3=BA+
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(survey)
library(srvyr)
library(patchwork)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/srh_common_functions.R"))
source(here::here("R/functions/theme_srh.R"))
source(here::here("R/functions/regress_sociodemog_by_year.R"))

# Set theme
theme_set(theme_srh())

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)

# Output directories
fig_dir <- here::here("output", "figures")
tables_dir <- here::here("output", "tables")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")

# Age groups (Scheme B)
AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

cat("========================================\n")
cat("Sensitivity Analyses: Sociodemographic Covariates\n")
cat("Figure S1: Adjusted Age Coefficients\n")
cat("Figure S2: Covariate Effects\n")
cat("========================================\n\n")


# ==============================================================================
# PART 1: LOAD DATA
# ==============================================================================

cat("Loading data...\n")

# Define required variables for each survey
required_vars <- c("srh", "age", "year", "wt", "sex", "race_includehisp", "educ_3cat")

# Helper to load and prepare data
load_survey <- function(survey_name, has_psu_strata = TRUE) {
  data <- readr::read_rds(derived_path(paste0("data_", tolower(survey_name), ".rds")))

  # Select available variables
  vars_to_keep <- intersect(
    c(required_vars, "psu", "strata"),
    names(data)
  )
  data <- data %>% select(all_of(vars_to_keep))

  # Basic filters
  data <- data %>%
    filter(wt > 0) %>%
    drop_na(srh, age, year, wt)

  # Add age group
  data <- add_age_group(data, age_var = age, scheme = "B")

  # Ensure factor levels for covariates
  if ("sex" %in% names(data)) {
    data$sex <- factor(data$sex, levels = c("Male", "Female"))
  }

  if ("race_includehisp" %in% names(data)) {
    data$race_includehisp <- factor(
      data$race_includehisp,
      levels = c("White", "Black", "AIAN", "Asian", "Hispanic", "Other")
    )
  }

  if ("educ_3cat" %in% names(data)) {
    # Convert to factor with labels
    data$educ_3cat <- factor(
      data$educ_3cat,
      levels = c(1, 2, 3),
      labels = c("LT_HS", "HS_SomeColl", "BA_plus")
    )
  }

  cat("  ", survey_name, ": ", format(nrow(data), big.mark = ","), " obs, years ",
      min(data$year), "-", max(data$year), "\n")

  return(data)
}

# Load all 6 surveys
data_brfss <- load_survey("BRFSS", has_psu_strata = FALSE)
data_meps <- load_survey("MEPS", has_psu_strata = TRUE)
data_nhis <- load_survey("NHIS", has_psu_strata = TRUE)
data_cps <- load_survey("CPS", has_psu_strata = FALSE)
data_nhanes <- load_survey("NHANES", has_psu_strata = TRUE)
data_gss <- load_survey("GSS", has_psu_strata = FALSE)

cat("\n")


# ==============================================================================
# PART 2: CHECK VARIABLE AVAILABILITY
# ==============================================================================

cat("Checking variable availability...\n")

check_vars <- function(data, survey_name) {
  vars_present <- c(
    sex = "sex" %in% names(data) && sum(!is.na(data$sex)) > 0,
    race = "race_includehisp" %in% names(data) && sum(!is.na(data$race_includehisp)) > 0,
    educ = "educ_3cat" %in% names(data) && sum(!is.na(data$educ_3cat)) > 0
  )

  cat("  ", survey_name, ": sex=", vars_present["sex"],
      ", race=", vars_present["race"],
      ", educ=", vars_present["educ"], "\n")

  return(vars_present)
}

avail_brfss <- check_vars(data_brfss, "BRFSS")
avail_meps <- check_vars(data_meps, "MEPS")
avail_nhis <- check_vars(data_nhis, "NHIS")
avail_cps <- check_vars(data_cps, "CPS")
avail_nhanes <- check_vars(data_nhanes, "NHANES")
avail_gss <- check_vars(data_gss, "GSS")

cat("\n")


# ==============================================================================
# PART 3: RUN ADJUSTED AGE COEFFICIENT REGRESSIONS (FIGURE S1)
# ==============================================================================

cat("Running adjusted age coefficient regressions...\n\n")

# Covariate specifications
covariate_specs <- list(
  sex = list(label = "Sex", ref = "Male"),
  race_includehisp = list(label = "Race/Ethnicity", ref = "White"),
  educ_3cat = list(label = "Education", ref = "LT_HS")
)

# Function to run regressions for one survey
run_fig_s1_regressions <- function(data, survey_name, psu_var = NULL, strata_var = NULL) {
  results_list <- list()

  for (cv in names(covariate_specs)) {
    if (!cv %in% names(data) || all(is.na(data[[cv]]))) {
      message("  Skipping ", cv, " - not available in ", survey_name)
      next
    }

    cat("  ", survey_name, " - adjusted for ", covariate_specs[[cv]]$label, "...\n")

    result <- regress_age_adjusted_by_year(
      data = data,
      survey_name = survey_name,
      covariate_var = cv,
      covariate_label = covariate_specs[[cv]]$label,
      psu_var = psu_var,
      strata_var = strata_var
    )

    if (!is.null(result)) {
      results_list[[cv]] <- result
    }
  }

  if (length(results_list) > 0) {
    return(bind_rows(results_list))
  } else {
    return(NULL)
  }
}

# Run for each survey (BRFSS uses weights only per CLAUDE.md)
cat("--- BRFSS ---\n")
adj_coef_brfss <- run_fig_s1_regressions(data_brfss, "BRFSS", psu_var = NULL, strata_var = NULL)

cat("\n--- MEPS ---\n")
adj_coef_meps <- run_fig_s1_regressions(data_meps, "MEPS", psu_var = "psu", strata_var = "strata")

cat("\n--- NHIS ---\n")
adj_coef_nhis <- run_fig_s1_regressions(data_nhis, "NHIS", psu_var = "psu", strata_var = "strata")

cat("\n--- CPS ---\n")
adj_coef_cps <- run_fig_s1_regressions(data_cps, "CPS", psu_var = NULL, strata_var = NULL)

cat("\n--- NHANES ---\n")
adj_coef_nhanes <- run_fig_s1_regressions(data_nhanes, "NHANES", psu_var = "psu", strata_var = "strata")

cat("\n--- GSS ---\n")
adj_coef_gss <- run_fig_s1_regressions(data_gss, "GSS", psu_var = NULL, strata_var = NULL)

# Combine all results
adj_coef_all <- bind_rows(
  adj_coef_brfss,
  adj_coef_meps,
  adj_coef_nhis,
  adj_coef_cps,
  adj_coef_nhanes,
  adj_coef_gss
)

cat("\nFigure S1 regressions complete.\n")
cat("  Total rows: ", nrow(adj_coef_all), "\n\n")


# ==============================================================================
# PART 4: RUN COVARIATE COEFFICIENT REGRESSIONS (FIGURE S2)
# ==============================================================================

cat("Running covariate coefficient regressions...\n\n")

# Function to run covariate regressions for one survey
run_fig_s2_regressions <- function(data, survey_name, psu_var = NULL, strata_var = NULL,
                                    by_age_group = FALSE) {
  results_list <- list()

  for (cv in names(covariate_specs)) {
    if (!cv %in% names(data) || all(is.na(data[[cv]]))) {
      message("  Skipping ", cv, " - not available in ", survey_name)
      next
    }

    label <- covariate_specs[[cv]]$label
    ref <- covariate_specs[[cv]]$ref

    cat("  ", survey_name, " - ", label,
        if (by_age_group) " (by age group)" else " (pooled)", "...\n")

    if (by_age_group) {
      result <- regress_covariate_by_age_year_categorical(
        data = data,
        survey_name = survey_name,
        covariate_var = cv,
        covariate_label = label,
        reference_level = ref,
        psu_var = psu_var,
        strata_var = strata_var
      )
    } else {
      result <- regress_covariate_overall_by_year(
        data = data,
        survey_name = survey_name,
        covariate_var = cv,
        covariate_label = label,
        reference_level = ref,
        psu_var = psu_var,
        strata_var = strata_var
      )
    }

    if (!is.null(result)) {
      results_list[[cv]] <- result
    }
  }

  if (length(results_list) > 0) {
    return(bind_rows(results_list))
  } else {
    return(NULL)
  }
}

# --- Panel A: Pooled (all ages) ---
cat("=== Panel A: Pooled coefficients ===\n\n")

cat("--- BRFSS ---\n")
coef_pooled_brfss <- run_fig_s2_regressions(data_brfss, "BRFSS", psu_var = NULL, strata_var = NULL, by_age_group = FALSE)

cat("\n--- MEPS ---\n")
coef_pooled_meps <- run_fig_s2_regressions(data_meps, "MEPS", psu_var = "psu", strata_var = "strata", by_age_group = FALSE)

cat("\n--- NHIS ---\n")
coef_pooled_nhis <- run_fig_s2_regressions(data_nhis, "NHIS", psu_var = "psu", strata_var = "strata", by_age_group = FALSE)

cat("\n--- CPS ---\n")
coef_pooled_cps <- run_fig_s2_regressions(data_cps, "CPS", psu_var = NULL, strata_var = NULL, by_age_group = FALSE)

cat("\n--- NHANES ---\n")
coef_pooled_nhanes <- run_fig_s2_regressions(data_nhanes, "NHANES", psu_var = "psu", strata_var = "strata", by_age_group = FALSE)

cat("\n--- GSS ---\n")
coef_pooled_gss <- run_fig_s2_regressions(data_gss, "GSS", psu_var = NULL, strata_var = NULL, by_age_group = FALSE)

coef_pooled_all <- bind_rows(
  coef_pooled_brfss,
  coef_pooled_meps,
  coef_pooled_nhis,
  coef_pooled_cps,
  coef_pooled_nhanes,
  coef_pooled_gss
)

cat("\nPanel A complete. Total rows: ", nrow(coef_pooled_all), "\n\n")


# --- Panel B: By age group ---
cat("=== Panel B: Coefficients by age group ===\n\n")

cat("--- BRFSS ---\n")
coef_byage_brfss <- run_fig_s2_regressions(data_brfss, "BRFSS", psu_var = NULL, strata_var = NULL, by_age_group = TRUE)

cat("\n--- MEPS ---\n")
coef_byage_meps <- run_fig_s2_regressions(data_meps, "MEPS", psu_var = "psu", strata_var = "strata", by_age_group = TRUE)

cat("\n--- NHIS ---\n")
coef_byage_nhis <- run_fig_s2_regressions(data_nhis, "NHIS", psu_var = "psu", strata_var = "strata", by_age_group = TRUE)

cat("\n--- CPS ---\n")
coef_byage_cps <- run_fig_s2_regressions(data_cps, "CPS", psu_var = NULL, strata_var = NULL, by_age_group = TRUE)

cat("\n--- NHANES ---\n")
coef_byage_nhanes <- run_fig_s2_regressions(data_nhanes, "NHANES", psu_var = "psu", strata_var = "strata", by_age_group = TRUE)

cat("\n--- GSS ---\n")
coef_byage_gss <- run_fig_s2_regressions(data_gss, "GSS", psu_var = NULL, strata_var = NULL, by_age_group = TRUE)

coef_byage_all <- bind_rows(
  coef_byage_brfss,
  coef_byage_meps,
  coef_byage_nhis,
  coef_byage_cps,
  coef_byage_nhanes,
  coef_byage_gss
)

cat("\nPanel B complete. Total rows: ", nrow(coef_byage_all), "\n\n")


# ==============================================================================
# PART 5: SAVE TABLES
# ==============================================================================

cat("Saving coefficient tables...\n")

# Figure S1: Adjusted age coefficients
if (!is.null(adj_coef_all) && nrow(adj_coef_all) > 0) {
  readr::write_csv(
    adj_coef_all %>% mutate(across(where(is.numeric) & !matches("year|n_"), ~ round(.x, 6))),
    file.path(tables_dir, paste0("figS1_age_adjusted_coefficients_", date_suffix, ".csv"))
  )
  readr::write_rds(
    adj_coef_all,
    file.path(tables_dir, paste0("figS1_age_adjusted_coefficients_", date_suffix, ".rds"))
  )
  cat("  Saved: figS1_age_adjusted_coefficients_", date_suffix, ".csv/.rds\n")
}

# Figure S2 Panel A: Pooled covariate coefficients
if (!is.null(coef_pooled_all) && nrow(coef_pooled_all) > 0) {
  readr::write_csv(
    coef_pooled_all %>% mutate(across(where(is.numeric) & !matches("year|n_"), ~ round(.x, 6))),
    file.path(tables_dir, paste0("figS2a_covariate_pooled_coefficients_", date_suffix, ".csv"))
  )
  readr::write_rds(
    coef_pooled_all,
    file.path(tables_dir, paste0("figS2a_covariate_pooled_coefficients_", date_suffix, ".rds"))
  )
  cat("  Saved: figS2a_covariate_pooled_coefficients_", date_suffix, ".csv/.rds\n")
}

# Figure S2 Panel B: By-age covariate coefficients
if (!is.null(coef_byage_all) && nrow(coef_byage_all) > 0) {
  readr::write_csv(
    coef_byage_all %>% mutate(across(where(is.numeric) & !matches("year|n_"), ~ round(.x, 6))),
    file.path(tables_dir, paste0("figS2b_covariate_byage_coefficients_", date_suffix, ".csv"))
  )
  readr::write_rds(
    coef_byage_all,
    file.path(tables_dir, paste0("figS2b_covariate_byage_coefficients_", date_suffix, ".rds"))
  )
  cat("  Saved: figS2b_covariate_byage_coefficients_", date_suffix, ".csv/.rds\n")
}

cat("\n")


# ==============================================================================
# PART 6: CREATE FIGURE S1 (Adjusted Age Coefficients)
# ==============================================================================

cat("Creating Figure S1: Adjusted Age Coefficients...\n")

# Define survey order
survey_order <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
covariate_order <- c("Sex", "Race/Ethnicity", "Education")

# Create single panel for one survey x covariate
create_adj_coef_panel <- function(data, survey_name, covariate_name,
                                   show_title = FALSE, show_ylabel = FALSE) {

  plot_data <- data %>%
    filter(survey == survey_name, covariate_adjusted == covariate_name)

  if (nrow(plot_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 4, color = "gray50") +
      theme_void() +
      labs(title = if (show_title) survey_name else NULL)
    return(p)
  }

  p <- ggplot(plot_data, aes(x = year, y = coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.3, color = "#3C5488", linewidth = 0.4) +
    geom_point(size = 1.8, color = "#3C5488") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    labs(
      title = if (show_title) survey_name else NULL,
      x = NULL,
      y = if (show_ylabel) "Age coefficient" else NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 8, color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(2, 4, 2, 4)
    )

  return(p)
}

# Create row label grob
create_row_label <- function(label) {
  grid::textGrob(
    label,
    rot = 90,
    gp = grid::gpar(fontsize = 11, fontface = "bold")
  )
}

# Build Figure S1
if (!is.null(adj_coef_all) && nrow(adj_coef_all) > 0) {

  # Create panels for each covariate (row) x survey (column)
  panels_s1 <- list()

  for (i in seq_along(covariate_order)) {
    cv <- covariate_order[i]

    for (j in seq_along(survey_order)) {
      svy <- survey_order[j]

      # Show title only for first row
      show_title <- (i == 1)
      # Show y-label only for first column
      show_ylabel <- (j == 1)

      panels_s1[[paste0(cv, "_", svy)]] <- create_adj_coef_panel(
        adj_coef_all, svy, cv,
        show_title = show_title,
        show_ylabel = show_ylabel
      )
    }
  }

  # Arrange panels: 3 rows x 6 columns
  # Add row labels on the left

  row1_panels <- wrap_plots(
    panels_s1[paste0(covariate_order[1], "_", survey_order)],
    ncol = 6
  )

  row2_panels <- wrap_plots(
    panels_s1[paste0(covariate_order[2], "_", survey_order)],
    ncol = 6
  )

  row3_panels <- wrap_plots(
    panels_s1[paste0(covariate_order[3], "_", survey_order)],
    ncol = 6
  )

  # Combine with row labels
  fig_s1 <- (
    (wrap_elements(create_row_label("Adjusted for Sex")) | row1_panels) /
    (wrap_elements(create_row_label("Adjusted for Race")) | row2_panels) /
    (wrap_elements(create_row_label("Adjusted for Education")) | row3_panels)
  ) +
    plot_layout(widths = c(0.05, 1), heights = c(1, 1, 1)) +
    plot_annotation(
      title = "Figure S1: Age Coefficient on SRH, Adjusted for Sociodemographic Covariates",
      subtitle = "Horizontal dashed line at y=0 (no age effect)",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  # Save Figure S1
  ggsave(
    filename = file.path(fig_dir, paste0("figS1_age_adjusted_draft_", date_suffix, ".png")),
    plot = fig_s1,
    width = 14, height = 9, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS1_age_adjusted.png"),
    plot = fig_s1,
    width = 14, height = 9, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS1_age_adjusted.pdf"),
    plot = fig_s1,
    width = 14, height = 9
  )
  cat("  Saved: figS1_age_adjusted (.png and .pdf)\n")

} else {
  cat("  Warning: No data available for Figure S1\n")
}

cat("\n")


# ==============================================================================
# PART 7: CREATE FIGURE S2 (Covariate Effects)
# ==============================================================================

cat("Creating Figure S2: Covariate Effects...\n")

# --- Helper function for Panel A (pooled coefficients) ---
create_pooled_panel <- function(data, survey_name, covariate_label,
                                 show_title = FALSE, show_ylabel = FALSE) {

  plot_data <- data %>%
    filter(survey == survey_name, covariate_label == !!covariate_label)

  if (nrow(plot_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 4, color = "gray50") +
      theme_void() +
      labs(title = if (show_title) survey_name else NULL)
    return(p)
  }

  # For race, need different colors for different levels
  n_levels <- length(unique(plot_data$level))

  if (n_levels > 1) {
    # Multiple levels - use color by level
    p <- ggplot(plot_data, aes(x = year, y = coefficient, color = level, group = level)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      geom_line(linewidth = 0.8, alpha = 0.8) +
      geom_point(size = 1.5, alpha = 0.8) +
      scale_color_viridis_d(option = "D", end = 0.9) +
      labs(color = NULL)
  } else {
    # Single level (e.g., Female)
    p <- ggplot(plot_data, aes(x = year, y = coefficient)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      geom_line(linewidth = 0.8, color = "#3C5488") +
      geom_point(size = 1.5, color = "#3C5488")
  }

  p <- p +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    labs(
      title = if (show_title) survey_name else NULL,
      x = NULL,
      y = if (show_ylabel) "Coefficient" else NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 8, color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(2, 4, 2, 4),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.6, "lines")
    )

  return(p)
}


# --- Helper function for Panel B (by age group - rainbow lines) ---
create_byage_panel <- function(data, survey_name, covariate_label, level_name,
                                show_title = FALSE, show_ylabel = FALSE) {

  plot_data <- data %>%
    filter(survey == survey_name,
           covariate_label == !!covariate_label,
           level == level_name)

  if (nrow(plot_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 3, color = "gray50") +
      theme_void() +
      labs(title = if (show_title) paste0(survey_name, ": ", level_name) else NULL)
    return(p)
  }

  # Ensure age_group is ordered factor
  plot_data$age_group <- factor(
    plot_data$age_group,
    levels = AGE_GROUPS
  )

  p <- ggplot(plot_data, aes(x = year, y = coefficient,
                              color = age_group, group = age_group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 1.2, alpha = 0.8) +
    scale_color_manual(values = age_colors_oi, name = "Age Group") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    labs(
      title = if (show_title) paste0(survey_name, ": ", level_name) else level_name,
      x = NULL,
      y = if (show_ylabel) "Coefficient" else NULL
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.25),
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 7, color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(2, 3, 2, 3),
      legend.position = "none"
    )

  return(p)
}


# --- Build Figure S2 ---

if (!is.null(coef_pooled_all) && nrow(coef_pooled_all) > 0 &&
    !is.null(coef_byage_all) && nrow(coef_byage_all) > 0) {

  # =========== PANEL A: Pooled Coefficients ===========

  cat("  Creating Panel A (pooled)...\n")

  # Sex: Single coefficient (Female vs Male)
  sex_panels_a <- lapply(survey_order, function(svy) {
    create_pooled_panel(coef_pooled_all, svy, "Sex",
                        show_title = TRUE, show_ylabel = (svy == survey_order[1]))
  })

  row_sex_a <- wrap_plots(sex_panels_a, ncol = 6) +
    plot_annotation(subtitle = "Sex (Female vs Male)") &
    theme(legend.position = "none")

  # Race: Multiple coefficients (vs White)
  race_panels_a <- lapply(survey_order, function(svy) {
    create_pooled_panel(coef_pooled_all, svy, "Race/Ethnicity",
                        show_title = FALSE, show_ylabel = (svy == survey_order[1]))
  })

  row_race_a <- wrap_plots(race_panels_a, ncol = 6, guides = "collect") +
    plot_annotation(subtitle = "Race/Ethnicity (vs White)") &
    theme(legend.position = "bottom")

  # Education: Multiple coefficients (vs LT HS)
  educ_panels_a <- lapply(survey_order, function(svy) {
    create_pooled_panel(coef_pooled_all, svy, "Education",
                        show_title = FALSE, show_ylabel = (svy == survey_order[1]))
  })

  row_educ_a <- wrap_plots(educ_panels_a, ncol = 6, guides = "collect") +
    plot_annotation(subtitle = "Education (vs Less than HS)") &
    theme(legend.position = "bottom")

  # Combine Panel A
  panel_a <- (row_sex_a / row_race_a / row_educ_a) +
    plot_annotation(
      title = "Panel A: Covariate Coefficient on SRH Over Time (All Ages Pooled)",
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
      )
    )


  # =========== PANEL B: By Age Group ===========

  cat("  Creating Panel B (by age group)...\n")

  # Build all 24 panels for a 4-row x 6-column grid
  # Row 1: Sex (Female), Row 2: Black, Row 3: Hispanic, Row 4: Education BA+
  all_panels_b <- list()

  # Row 1: Sex
  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    p <- create_byage_panel(coef_byage_all, svy, "Sex", "Female",
                            show_title = TRUE, show_ylabel = (i == 1))
    all_panels_b[[length(all_panels_b) + 1]] <- p
  }

  # Row 2: Black
  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    p <- create_byage_panel(coef_byage_all, svy, "Race/Ethnicity", "Black",
                            show_title = FALSE, show_ylabel = (i == 1))
    all_panels_b[[length(all_panels_b) + 1]] <- p
  }

  # Row 3: Hispanic
  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    p <- create_byage_panel(coef_byage_all, svy, "Race/Ethnicity", "Hispanic",
                            show_title = FALSE, show_ylabel = (i == 1))
    all_panels_b[[length(all_panels_b) + 1]] <- p
  }

  # Row 4: Education BA+
  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    p <- create_byage_panel(coef_byage_all, svy, "Education", "BA_plus",
                            show_title = FALSE, show_ylabel = (i == 1))
    all_panels_b[[length(all_panels_b) + 1]] <- p
  }

  # Combine all 24 panels in a 4x6 grid
  panel_b <- wrap_plots(all_panels_b, ncol = 6, nrow = 4) +
    plot_annotation(
      title = "Panel B: Covariate Coefficient on SRH by Age Group",
      subtitle = paste0("Row 1: Sex | Row 2: Black vs White | ",
                        "Row 3: Hispanic vs White | Row 4: BA+ vs LT HS"),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0.5)
      )
    )

  # Add shared legend for age groups
  legend_plot <- ggplot(data.frame(age_group = factor(AGE_GROUPS, levels = AGE_GROUPS),
                                    y = 1:7),
                         aes(x = 1, y = y, color = age_group)) +
    geom_point(size = 3) +
    scale_color_manual(values = age_colors_oi, name = "Age Group") +
    guides(color = guide_legend(nrow = 1)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 11, face = "bold"))

  legend_grob <- cowplot::get_legend(legend_plot)


  # =========== Combine Panels A and B ===========

  cat("  Combining panels...\n")

  # Save Panel A and Panel B separately for flexibility
  ggsave(
    filename = file.path(fig_dir, paste0("figS2a_covariate_pooled_draft_", date_suffix, ".png")),
    plot = panel_a,
    width = 14, height = 10, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS2a_covariate_pooled.png"),
    plot = panel_a,
    width = 14, height = 10, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS2a_covariate_pooled.pdf"),
    plot = panel_a,
    width = 14, height = 10
  )
  cat("  Saved: figS2a_covariate_pooled (.png and .pdf)\n")

  # Panel B with legend
  panel_b_with_legend <- panel_b / wrap_elements(legend_grob) +
    plot_layout(heights = c(1, 0.08))

  ggsave(
    filename = file.path(fig_dir, paste0("figS2b_covariate_byage_draft_", date_suffix, ".png")),
    plot = panel_b_with_legend,
    width = 14, height = 12, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS2b_covariate_byage.png"),
    plot = panel_b_with_legend,
    width = 14, height = 12, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS2b_covariate_byage.pdf"),
    plot = panel_b_with_legend,
    width = 14, height = 12
  )
  cat("  Saved: figS2b_covariate_byage (.png and .pdf)\n")

  # Combined Figure S2 (both panels stacked)
  fig_s2_combined <- (panel_a / panel_b_with_legend) +
    plot_annotation(
      title = "Figure S2: Sociodemographic Covariate Effects on SRH Over Time",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  ggsave(
    filename = file.path(fig_dir, paste0("figS2_covariate_effects_draft_", date_suffix, ".png")),
    plot = fig_s2_combined,
    width = 14, height = 20, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS2_covariate_effects.png"),
    plot = fig_s2_combined,
    width = 14, height = 20, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS2_covariate_effects.pdf"),
    plot = fig_s2_combined,
    width = 14, height = 20
  )
  cat("  Saved: figS2_covariate_effects (.png and .pdf)\n")

} else {
  cat("  Warning: Insufficient data for Figure S2\n")
}

cat("\n")


# ==============================================================================
# PART 8: VERIFICATION SUMMARY
# ==============================================================================

cat("========================================\n")
cat("Verification Summary\n")
cat("========================================\n\n")

# Figure S1: Check adjusted coefficients
if (!is.null(adj_coef_all) && nrow(adj_coef_all) > 0) {
  cat("--- Figure S1: Adjusted Age Coefficients ---\n")

  adj_summary <- adj_coef_all %>%
    group_by(survey, covariate_adjusted) %>%
    summarise(
      n_years = n(),
      mean_coef = mean(coefficient, na.rm = TRUE),
      mean_se = mean(se, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\nMean adjusted age coefficient by survey and covariate:\n")
  print(adj_summary %>% pivot_wider(names_from = covariate_adjusted, values_from = c(mean_coef)))

  # Check if coefficients are trending toward zero (convergence still evident)
  cat("\nExpected: Adjusted coefficients should be similar to unadjusted (Figure 1B)\n")
  cat("if sociodemographic variables don't confound the age-SRH relationship.\n")
}

# Figure S2: Check covariate coefficients
if (!is.null(coef_pooled_all) && nrow(coef_pooled_all) > 0) {
  cat("\n--- Figure S2: Covariate Coefficients (Pooled) ---\n")

  coef_summary <- coef_pooled_all %>%
    group_by(covariate_label, level) %>%
    summarise(
      n_surveys = n_distinct(survey),
      n_obs = n(),
      mean_coef = mean(coefficient, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\nMean coefficient by covariate level (all surveys pooled):\n")
  print(coef_summary)

  cat("\nExpected coefficient directions:\n")
  cat("  - Female vs Male: typically slightly positive or near zero\n")
  cat("  - Non-White vs White: typically negative (worse SRH)\n")
  cat("  - Higher education vs LT HS: positive (better SRH)\n")
}

cat("\n========================================\n")
cat("Sensitivity Analyses Complete!\n")
cat("========================================\n")
cat("\nOutputs:\n")
cat("  Figures:\n")
cat("    output/figures/figS1_age_adjusted.{png,pdf}\n")
cat("    output/figures/figS2a_covariate_pooled.{png,pdf}\n")
cat("    output/figures/figS2b_covariate_byage.{png,pdf}\n")
cat("    output/figures/figS2_covariate_effects.{png,pdf}\n")
cat("  Tables:\n")
cat("    output/tables/figS1_age_adjusted_coefficients_", date_suffix, ".csv/.rds\n")
cat("    output/tables/figS2a_covariate_pooled_coefficients_", date_suffix, ".csv/.rds\n")
cat("    output/tables/figS2b_covariate_byage_coefficients_", date_suffix, ".csv/.rds\n")
cat("\n")
