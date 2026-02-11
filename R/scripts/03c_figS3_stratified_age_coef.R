# ==============================================================================
# 03c_figS3_stratified_age_coef.R
# Figure S3: Age Coefficient Stratified by Sociodemographic Groups
#
# Purpose:
#   Create a 3-panel figure showing age coefficients on SRH, stratified by
#   sociodemographic groups. This differs from Figure S1 (adjusted coefficients):
#   - Figure S1: srh ~ age + sex → one age coefficient (adjusted for sex)
#   - Figure S3: srh ~ age for Males only, srh ~ age for Females only → two coefficients
#
# Layout:
#   6 rows (surveys) × 3 columns (covariates) = 18 panels
#   - Column 1: Sex (2 lines: Male, Female)
#   - Column 2: Education (3 lines: LT HS, HS/Some Coll, BA+)
#   - Column 3: Race (4 lines: White, Black, Hispanic, Asian)
#
# Scientific Question:
#   Does the convergence phenomenon differ by sociodemographic group?
#   If all lines converge toward zero at similar rates → convergence is universal
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
library(cowplot)

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

cat("========================================\n")
cat("Figure S3: Age Coefficient Stratified by\n")
cat("           Sociodemographic Groups\n")
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
# PART 2: DEFINE STRATUM SPECIFICATIONS
# ==============================================================================

# Stratum specifications for Figure S3
# Each panel shows age coefficient stratified by stratum levels
stratum_specs <- list(
  sex = list(
    var = "sex",
    label = "Sex",
    levels = c("Male", "Female")
  ),
  education = list(
    var = "educ_3cat",
    label = "Education",
    levels = c("LT_HS", "HS_SomeColl", "BA_plus")
  ),
  race = list(
    var = "race_includehisp",
    label = "Race/Ethnicity",
    # Exclude AIAN and Other due to small N in many surveys
    levels = c("White", "Black", "Hispanic", "Asian")
  )
)

# Color palettes (Okabe-Ito colorblind-friendly)
stratum_colors <- list(
  sex = c(
    "Male" = "#0072B2",    # blue
    "Female" = "#D55E00"   # vermillion
  ),
  education = c(
    "LT_HS" = "#CC79A7",       # reddish purple
    "HS_SomeColl" = "#E69F00", # orange
    "BA_plus" = "#009E73"      # bluish green
  ),
  race = c(
    "White" = "#000000",    # black
    "Black" = "#56B4E9",    # sky blue
    "Hispanic" = "#E69F00", # orange
    "Asian" = "#009E73"     # bluish green
  )
)

# Display labels for stratum levels
stratum_display_labels <- list(
  sex = c("Male" = "Male", "Female" = "Female"),
  education = c("LT_HS" = "< HS", "HS_SomeColl" = "HS/Some Coll", "BA_plus" = "BA+"),
  race = c("White" = "White", "Black" = "Black", "Hispanic" = "Hispanic", "Asian" = "Asian")
)


# ==============================================================================
# PART 3: RUN STRATIFIED REGRESSIONS
# ==============================================================================

cat("Running stratified age coefficient regressions...\n\n")

# Function to run stratified regressions for one survey
run_stratified_regressions <- function(data, survey_name, psu_var = NULL, strata_var = NULL) {
  results_list <- list()

  for (spec_name in names(stratum_specs)) {
    spec <- stratum_specs[[spec_name]]

    if (!spec$var %in% names(data) || all(is.na(data[[spec$var]]))) {
      message("  Skipping ", spec$label, " - not available in ", survey_name)
      next
    }

    cat("  ", survey_name, " - stratified by ", spec$label, "...\n")

    result <- regress_age_by_stratum_year(
      data = data,
      survey_name = survey_name,
      stratum_var = spec$var,
      stratum_label = spec$label,
      stratum_levels = spec$levels,
      psu_var = psu_var,
      strata_var = strata_var
    )

    if (!is.null(result)) {
      results_list[[spec_name]] <- result
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
strat_coef_brfss <- run_stratified_regressions(data_brfss, "BRFSS", psu_var = NULL, strata_var = NULL)

cat("\n--- MEPS ---\n")
strat_coef_meps <- run_stratified_regressions(data_meps, "MEPS", psu_var = "psu", strata_var = "strata")

cat("\n--- NHIS ---\n")
strat_coef_nhis <- run_stratified_regressions(data_nhis, "NHIS", psu_var = "psu", strata_var = "strata")

cat("\n--- CPS ---\n")
strat_coef_cps <- run_stratified_regressions(data_cps, "CPS", psu_var = NULL, strata_var = NULL)

cat("\n--- NHANES ---\n")
strat_coef_nhanes <- run_stratified_regressions(data_nhanes, "NHANES", psu_var = "psu", strata_var = "strata")

cat("\n--- GSS ---\n")
strat_coef_gss <- run_stratified_regressions(data_gss, "GSS", psu_var = NULL, strata_var = NULL)

# Combine all results
strat_coef_all <- bind_rows(
  strat_coef_brfss,
  strat_coef_meps,
  strat_coef_nhis,
  strat_coef_cps,
  strat_coef_nhanes,
  strat_coef_gss
)

cat("\nStratified regressions complete.\n")
cat("  Total rows: ", nrow(strat_coef_all), "\n\n")


# ==============================================================================
# PART 4: RUN METAREGRESSIONS FOR EACH STRATUM
# ==============================================================================

cat("Running metaregressions (trend over time) for each stratum...\n\n")

# Run a simple linear trend for each survey × stratum combination
run_metaregression <- function(data) {
  # data should have columns: year, coefficient, se

  if (nrow(data) < 3) {
    return(data.frame(
      slope = NA_real_,
      slope_se = NA_real_,
      intercept = NA_real_,
      intercept_se = NA_real_,
      p_value = NA_real_,
      n_years = nrow(data)
    ))
  }

  # Weighted regression using inverse variance weights
  # Weight by 1/se^2
  data <- data %>% filter(!is.na(se) & se > 0)

  if (nrow(data) < 3) {
    return(data.frame(
      slope = NA_real_,
      slope_se = NA_real_,
      intercept = NA_real_,
      intercept_se = NA_real_,
      p_value = NA_real_,
      n_years = nrow(data)
    ))
  }

  tryCatch({
    fit <- lm(coefficient ~ year, data = data, weights = 1 / se^2)
    coefs <- summary(fit)$coefficients

    data.frame(
      slope = coefs["year", "Estimate"],
      slope_se = coefs["year", "Std. Error"],
      intercept = coefs["(Intercept)", "Estimate"],
      intercept_se = coefs["(Intercept)", "Std. Error"],
      p_value = coefs["year", "Pr(>|t|)"],
      n_years = nrow(data)
    )
  }, error = function(e) {
    data.frame(
      slope = NA_real_,
      slope_se = NA_real_,
      intercept = NA_real_,
      intercept_se = NA_real_,
      p_value = NA_real_,
      n_years = nrow(data)
    )
  })
}

# Run metaregressions for each survey × stratum_label × stratum_level
metareg_results <- strat_coef_all %>%
  group_by(survey, stratum_var, stratum_label, stratum_level) %>%
  do({
    run_metaregression(.)
  }) %>%
  ungroup()

cat("Metaregression complete.\n")
cat("  Total combinations: ", nrow(metareg_results), "\n\n")


# ==============================================================================
# PART 5: SAVE TABLES
# ==============================================================================

cat("Saving coefficient tables...\n")

# Stratified coefficients
if (!is.null(strat_coef_all) && nrow(strat_coef_all) > 0) {
  readr::write_csv(
    strat_coef_all %>% mutate(across(where(is.numeric) & !matches("year|n_"), ~ round(.x, 6))),
    file.path(tables_dir, paste0("figS3_age_coef_stratified_", date_suffix, ".csv"))
  )
  readr::write_rds(
    strat_coef_all,
    file.path(tables_dir, paste0("figS3_age_coef_stratified_", date_suffix, ".rds"))
  )
  cat("  Saved: figS3_age_coef_stratified_", date_suffix, ".csv/.rds\n")
}

# Metaregression results
if (!is.null(metareg_results) && nrow(metareg_results) > 0) {
  readr::write_csv(
    metareg_results %>% mutate(across(where(is.numeric), ~ round(.x, 6))),
    file.path(tables_dir, paste0("figS3_metaregression_by_stratum_", date_suffix, ".csv"))
  )
  readr::write_rds(
    metareg_results,
    file.path(tables_dir, paste0("figS3_metaregression_by_stratum_", date_suffix, ".rds"))
  )
  cat("  Saved: figS3_metaregression_by_stratum_", date_suffix, ".csv/.rds\n")
}

cat("\n")


# ==============================================================================
# PART 6: CREATE FIGURE S3
# ==============================================================================

cat("Creating Figure S3: Age Coefficient Stratified by Sociodemographic Groups...\n")

# Define survey order (rows)
survey_order <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")
stratum_order <- c("Sex", "Education", "Race/Ethnicity")

# Helper function to create a single panel
create_stratified_panel <- function(coef_data, metareg_data, survey_name, stratum_label_val,
                                     colors, display_labels,
                                     show_title = FALSE, show_ylabel = FALSE,
                                     show_legend = FALSE) {

  # Filter data for this panel
  plot_data <- coef_data %>%
    filter(survey == survey_name, stratum_label == stratum_label_val)

  meta_data <- metareg_data %>%
    filter(survey == survey_name, stratum_label == stratum_label_val)

  if (nrow(plot_data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 3.5, color = "gray50") +
      theme_void() +
      labs(title = if (show_title) survey_name else NULL)
    return(p)
  }

  # Map stratum levels to display labels
  plot_data <- plot_data %>%
    mutate(stratum_display = display_labels[stratum_level])

  # Ensure factor order for consistent coloring
  level_order <- names(colors)
  plot_data$stratum_level <- factor(plot_data$stratum_level, levels = level_order)
  plot_data$stratum_display <- factor(plot_data$stratum_display, levels = display_labels[level_order])

  # Get year range for trend lines
  year_range <- range(plot_data$year, na.rm = TRUE)
  year_seq <- seq(year_range[1], year_range[2], length.out = 50)

  # Build trend line data from metaregression
  trend_data <- meta_data %>%
    filter(!is.na(slope)) %>%
    rowwise() %>%
    do({
      row <- .
      data.frame(
        stratum_level = row$stratum_level,
        year = year_seq,
        fitted = row$intercept + row$slope * year_seq
      )
    }) %>%
    ungroup()

  if (nrow(trend_data) > 0) {
    trend_data$stratum_level <- factor(trend_data$stratum_level, levels = level_order)
    trend_data$stratum_display <- display_labels[as.character(trend_data$stratum_level)]
    trend_data$stratum_display <- factor(trend_data$stratum_display, levels = display_labels[level_order])
  }

  # Build plot
  p <- ggplot(plot_data, aes(x = year, y = coefficient,
                              color = stratum_level, group = stratum_level)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5)

  # Add metaregression trend lines
  if (nrow(trend_data) > 0) {
    p <- p +
      geom_line(data = trend_data, aes(x = year, y = fitted, color = stratum_level),
                linewidth = 0.9, alpha = 0.7)
  }

  # Add points
  p <- p +
    geom_point(size = 1.5, alpha = 0.8) +
    scale_color_manual(values = colors, labels = display_labels, name = NULL,
                       drop = FALSE) +
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
      plot.margin = margin(2, 4, 2, 4),
      legend.position = if (show_legend) "bottom" else "none",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.7, "lines")
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

# Create column label grob
create_col_label <- function(label) {
  grid::textGrob(
    label,
    gp = grid::gpar(fontsize = 11, fontface = "bold")
  )
}


# ==============================================================================
# BUILD FIGURE S3
# ==============================================================================

if (!is.null(strat_coef_all) && nrow(strat_coef_all) > 0) {

  # Create all 18 panels (6 surveys x 3 covariates)
  # Store in a flat list for wrap_plots
  all_panels <- list()

  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]

    for (j in seq_along(stratum_order)) {
      strat <- stratum_order[j]

      # Get the spec name from stratum_order
      spec_name <- switch(strat,
                          "Sex" = "sex",
                          "Education" = "education",
                          "Race/Ethnicity" = "race")

      # Show y-label only for first column
      show_ylabel <- (j == 1)

      p <- create_stratified_panel(
        coef_data = strat_coef_all,
        metareg_data = metareg_results,
        survey_name = svy,
        stratum_label_val = strat,
        colors = stratum_colors[[spec_name]],
        display_labels = stratum_display_labels[[spec_name]],
        show_title = FALSE,  # We'll add column headers separately
        show_ylabel = show_ylabel,
        show_legend = FALSE
      )

      # Add survey name as strip on left for first column
      if (j == 1) {
        p <- p + labs(tag = svy) +
          theme(plot.tag = element_text(size = 9, face = "bold", angle = 90),
                plot.tag.position = "left")
      }

      # Add column title for first row only
      if (i == 1) {
        p <- p + labs(title = strat) +
          theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5))
      }

      all_panels[[length(all_panels) + 1]] <- p
    }
  }

  # --- Build legend as a ggplot panel to embed directly in patchwork ---
  # This avoids the cowplot rel_heights issue where legends get cut off

  # Create a single legend panel with all three groups side by side
  make_legend_panel <- function(colors, display_labels, title) {
    df <- tibble(
      level = factor(names(colors), levels = names(colors)),
      x = seq_along(colors), y = 1
    )
    ggplot(df, aes(x, y, color = level)) +
      geom_point(size = 4) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, labels = display_labels, name = title) +
      guides(color = guide_legend(nrow = 1, override.aes = list(size = 4, linewidth = 1.2))) +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12, face = "bold"),
        legend.key.width = unit(1.5, "lines")
      )
  }

  p_leg_sex  <- make_legend_panel(stratum_colors$sex, stratum_display_labels$sex, "Sex")
  p_leg_educ <- make_legend_panel(stratum_colors$education, stratum_display_labels$education, "Education")
  p_leg_race <- make_legend_panel(stratum_colors$race, stratum_display_labels$race, "Race/Ethnicity")

  # Build a legend-only row: 3 guide_area plots
  # Use patchwork's guide_area() approach: embed legend plots in last row
  legend_row <- (p_leg_sex | p_leg_educ | p_leg_race) &
    theme(legend.position = "bottom")

  # Stack: 6x3 panel grid on top, legend row on bottom
  panel_grid <- wrap_plots(all_panels, ncol = 3, nrow = 6, byrow = TRUE)

  fig_s3 <- panel_grid / legend_row +
    plot_layout(heights = c(1, 0.08)) +
    plot_annotation(
      title = "Figure S3: Age Coefficient on SRH, Stratified by Sociodemographic Groups",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  # Save Figure S3
  fig_width <- 12
  fig_height <- 16
  ggsave(
    filename = file.path(fig_dir, paste0("figS3_age_coef_stratified_draft_", date_suffix, ".png")),
    plot = fig_s3,
    width = fig_width, height = fig_height, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS3_age_coef_stratified.png"),
    plot = fig_s3,
    width = fig_width, height = fig_height, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "figS3_age_coef_stratified.pdf"),
    plot = fig_s3,
    width = fig_width, height = fig_height
  )
  cat("  Saved: figS3_age_coef_stratified (.png and .pdf)\n")

} else {
  cat("  Warning: No data available for Figure S3\n")
}

cat("\n")


# ==============================================================================
# PART 7: VERIFICATION SUMMARY
# ==============================================================================

cat("========================================\n")
cat("Verification Summary\n")
cat("========================================\n\n")

if (!is.null(strat_coef_all) && nrow(strat_coef_all) > 0) {
  cat("--- Stratified Age Coefficients ---\n\n")

  # Summary by survey and stratum
  coef_summary <- strat_coef_all %>%
    group_by(survey, stratum_label, stratum_level) %>%
    summarise(
      n_years = n(),
      mean_coef = mean(coefficient, na.rm = TRUE),
      mean_se = mean(se, na.rm = TRUE),
      .groups = "drop"
    )

  cat("Mean age coefficient by survey × stratum:\n")
  print(coef_summary %>%
          pivot_wider(names_from = stratum_level, values_from = mean_coef) %>%
          arrange(stratum_label, survey))

  cat("\n--- Expected Patterns ---\n")
  cat("1. Age coefficient should be negative (older → worse SRH)\n")
  cat("2. Coefficient should trend toward zero over time (convergence)\n")
  cat("3. Check if all strata show similar convergence patterns\n")
}

if (!is.null(metareg_results) && nrow(metareg_results) > 0) {
  cat("\n--- Metaregression Slopes (trend over time) ---\n\n")

  # Show slopes for each stratum
  meta_summary <- metareg_results %>%
    filter(!is.na(slope)) %>%
    group_by(stratum_label, stratum_level) %>%
    summarise(
      n_surveys = n(),
      mean_slope = mean(slope, na.rm = TRUE),
      .groups = "drop"
    )

  cat("Mean slope (change in age coefficient per year):\n")
  print(meta_summary)

  cat("\nPositive slopes indicate convergence toward zero (age effect weakening)\n")
}

cat("\n========================================\n")
cat("Figure S3 Generation Complete!\n")
cat("========================================\n")
cat("\nOutputs:\n")
cat("  Figures:\n")
cat("    output/figures/figS3_age_coef_stratified.{png,pdf}\n")
cat("  Tables:\n")
cat("    output/tables/figS3_age_coef_stratified_", date_suffix, ".csv/.rds\n")
cat("    output/tables/figS3_metaregression_by_stratum_", date_suffix, ".csv/.rds\n")
cat("\n")
