# ==============================================================================
# 03d_sensitivity_sociodemog_combined.R
# Combined Sociodemographic Model: SRH ~ age + sex + race + education
#
# Purpose:
#   Fit a single model with ALL sociodemographic covariates simultaneously,
#   rather than one-at-a-time as in 03_sensitivity_sociodemog.R.
#
#   Produces:
#     - Figure S1c: Age coefficient on SRH, adjusted for ALL covariates
#     - Figure S2c Panel A: Covariate coefficients from full model (pooled)
#     - Figure S2c Panel B: Covariate coefficients from full model (by age group)
#     - Tables of all coefficient estimates
#
# Model specifications:
#   Pooled:    SRH ~ age + sex + race_includehisp + educ_3cat  (per year)
#   By age:    SRH ~ sex + race_includehisp + educ_3cat        (per age group x year)
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
fig_dir <- here::here("output", "sensitivity", "sociodemographic")
tables_dir <- here::here("output", "sensitivity", "sociodemographic")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")

# Age groups (Scheme B)
AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

cat("========================================\n")
cat("Combined Sociodemographic Model\n")
cat("SRH ~ age + sex + race + education\n")
cat("========================================\n\n")


# ==============================================================================
# PART 1: LOAD DATA
# ==============================================================================

cat("Loading data...\n")

required_vars <- c("srh", "age", "year", "wt", "sex", "race_includehisp", "educ_3cat")

load_survey <- function(survey_name) {
  data <- readr::read_rds(derived_path(paste0("data_", tolower(survey_name), ".rds")))

  vars_to_keep <- intersect(
    c(required_vars, "psu", "strata"),
    names(data)
  )
  data <- data %>% select(all_of(vars_to_keep))

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

data_brfss <- load_survey("BRFSS")
data_meps <- load_survey("MEPS")
data_nhis <- load_survey("NHIS")
data_cps <- load_survey("CPS")
data_nhanes <- load_survey("NHANES")
data_gss <- load_survey("GSS")

cat("\n")


# ==============================================================================
# PART 2: COVARIATE SPECIFICATIONS
# ==============================================================================

covariate_specs <- list(
  sex = list(label = "Sex", ref = "Male"),
  race_includehisp = list(label = "Race/Ethnicity", ref = "White"),
  educ_3cat = list(label = "Education", ref = "LT_HS")
)

survey_order <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")


# ==============================================================================
# PART 3: RUN FULL MODEL (POOLED) — SRH ~ age + sex + race + education
# ==============================================================================

cat("Running combined model: SRH ~ age + sex + race + education...\n\n")

# BRFSS uses weights only per CLAUDE.md
cat("--- BRFSS ---\n")
coef_combined_brfss <- regress_all_covariates_by_year(
  data_brfss, "BRFSS", covariate_vars = covariate_specs,
  psu_var = NULL, strata_var = NULL
)

cat("\n--- MEPS ---\n")
coef_combined_meps <- regress_all_covariates_by_year(
  data_meps, "MEPS", covariate_vars = covariate_specs,
  psu_var = "psu", strata_var = "strata"
)

cat("\n--- NHIS ---\n")
coef_combined_nhis <- regress_all_covariates_by_year(
  data_nhis, "NHIS", covariate_vars = covariate_specs,
  psu_var = "psu", strata_var = "strata"
)

cat("\n--- CPS ---\n")
coef_combined_cps <- regress_all_covariates_by_year(
  data_cps, "CPS", covariate_vars = covariate_specs,
  psu_var = NULL, strata_var = NULL
)

cat("\n--- NHANES ---\n")
coef_combined_nhanes <- regress_all_covariates_by_year(
  data_nhanes, "NHANES", covariate_vars = covariate_specs,
  psu_var = "psu", strata_var = "strata"
)

cat("\n--- GSS ---\n")
coef_combined_gss <- regress_all_covariates_by_year(
  data_gss, "GSS", covariate_vars = covariate_specs,
  psu_var = NULL, strata_var = NULL
)

coef_combined_all <- bind_rows(
  coef_combined_brfss,
  coef_combined_meps,
  coef_combined_nhis,
  coef_combined_cps,
  coef_combined_nhanes,
  coef_combined_gss
)

cat("\nPooled combined model complete. Total rows: ", nrow(coef_combined_all), "\n\n")


# ==============================================================================
# PART 4: RUN FULL MODEL BY AGE GROUP — SRH ~ sex + race + education
# ==============================================================================

cat("Running combined model by age group...\n\n")

cat("--- BRFSS ---\n")
coef_byage_brfss <- regress_all_covariates_by_age_year(
  data_brfss, "BRFSS", covariate_vars = covariate_specs,
  psu_var = NULL, strata_var = NULL
)

cat("\n--- MEPS ---\n")
coef_byage_meps <- regress_all_covariates_by_age_year(
  data_meps, "MEPS", covariate_vars = covariate_specs,
  psu_var = "psu", strata_var = "strata"
)

cat("\n--- NHIS ---\n")
coef_byage_nhis <- regress_all_covariates_by_age_year(
  data_nhis, "NHIS", covariate_vars = covariate_specs,
  psu_var = "psu", strata_var = "strata"
)

cat("\n--- CPS ---\n")
coef_byage_cps <- regress_all_covariates_by_age_year(
  data_cps, "CPS", covariate_vars = covariate_specs,
  psu_var = NULL, strata_var = NULL
)

cat("\n--- NHANES ---\n")
coef_byage_nhanes <- regress_all_covariates_by_age_year(
  data_nhanes, "NHANES", covariate_vars = covariate_specs,
  psu_var = "psu", strata_var = "strata"
)

cat("\n--- GSS ---\n")
coef_byage_gss <- regress_all_covariates_by_age_year(
  data_gss, "GSS", covariate_vars = covariate_specs,
  psu_var = NULL, strata_var = NULL
)

coef_byage_all <- bind_rows(
  coef_byage_brfss,
  coef_byage_meps,
  coef_byage_nhis,
  coef_byage_cps,
  coef_byage_nhanes,
  coef_byage_gss
)

cat("\nBy-age combined model complete. Total rows: ", nrow(coef_byage_all), "\n\n")


# ==============================================================================
# PART 5: SAVE TABLES
# ==============================================================================

cat("Saving coefficient tables...\n")

# Pooled combined model coefficients
if (!is.null(coef_combined_all) && nrow(coef_combined_all) > 0) {
  readr::write_csv(
    coef_combined_all %>% mutate(across(where(is.numeric) & !matches("year|n_"), ~ round(.x, 6))),
    file.path(tables_dir, paste0("combined_model_coefficients_", date_suffix, ".csv"))
  )
  readr::write_rds(
    coef_combined_all,
    file.path(tables_dir, paste0("combined_model_coefficients_", date_suffix, ".rds"))
  )
  cat("  Saved: combined_model_coefficients_", date_suffix, ".csv/.rds\n")
}

# By-age combined model coefficients
if (!is.null(coef_byage_all) && nrow(coef_byage_all) > 0) {
  readr::write_csv(
    coef_byage_all %>% mutate(across(where(is.numeric) & !matches("year|n_"), ~ round(.x, 6))),
    file.path(tables_dir, paste0("combined_model_byage_coefficients_", date_suffix, ".csv"))
  )
  readr::write_rds(
    coef_byage_all,
    file.path(tables_dir, paste0("combined_model_byage_coefficients_", date_suffix, ".rds"))
  )
  cat("  Saved: combined_model_byage_coefficients_", date_suffix, ".csv/.rds\n")
}

cat("\n")


# ==============================================================================
# PART 6: FIGURE — AGE COEFFICIENT FROM COMBINED MODEL
# ==============================================================================
# Analogous to Figure S1, but one row instead of three:
# the age coefficient from SRH ~ age + sex + race + education

cat("Creating Figure: Age Coefficient from Combined Model...\n")

if (!is.null(coef_combined_all) && nrow(coef_combined_all) > 0) {

  age_coef <- coef_combined_all %>% filter(covariate == "age")

  # One panel per survey
  create_age_panel <- function(data, survey_name, show_title = TRUE, show_ylabel = FALSE) {
    plot_data <- data %>% filter(survey == survey_name)

    if (nrow(plot_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data",
                        size = 4, color = "gray50") +
               theme_void() +
               labs(title = if (show_title) survey_name else NULL))
    }

    ggplot(plot_data, aes(x = year, y = coefficient)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "#E64B35", linewidth = 0.8) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                    width = 0.3, color = "#3C5488", linewidth = 0.4) +
      geom_point(size = 1.8, color = "#3C5488") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
      labs(
        title = if (show_title) survey_name else NULL,
        x = NULL,
        y = if (show_ylabel) "Age Coefficient" else NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12, color = "gray30"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = if (show_ylabel) element_text(size = 13, face = "bold") else element_blank(),
        plot.margin = margin(2, 4, 2, 4)
      )
  }

  age_panels <- lapply(seq_along(survey_order), function(i) {
    create_age_panel(age_coef, survey_order[i],
                     show_title = TRUE,
                     show_ylabel = (i == 1))
  })

  fig_age_combined <- wrap_plots(age_panels, ncol = 6) +
    plot_annotation(
      title = "Age Coefficient on SRH, Adjusted for Sex + Race + Education (Combined Model)",
      subtitle = "Red line: linear meta-regression trend; dashed line: y = 0 (no age effect)",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  ggsave(
    filename = file.path(fig_dir, paste0("fig_age_coef_combined_model_draft_", date_suffix, ".png")),
    plot = fig_age_combined,
    width = 16, height = 5, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "fig_age_coef_combined_model.png"),
    plot = fig_age_combined,
    width = 16, height = 5, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "fig_age_coef_combined_model.pdf"),
    plot = fig_age_combined,
    width = 16, height = 5
  )
  cat("  Saved: fig_age_coef_combined_model (.png and .pdf)\n")
}

cat("\n")


# ==============================================================================
# PART 7: FIGURE — COVARIATE COEFFICIENTS FROM COMBINED MODEL (POOLED)
# ==============================================================================
# Analogous to Figure S2 Panel A, but coefficients come from the full model

cat("Creating Figure: Covariate Coefficients from Combined Model (Pooled)...\n")

if (!is.null(coef_combined_all) && nrow(coef_combined_all) > 0) {

  covar_coef <- coef_combined_all %>% filter(covariate != "age")

  # Panel function for pooled covariate coefficients
  create_covar_pooled_panel <- function(data, survey_name, cov_label,
                                         show_title = FALSE, show_ylabel = FALSE) {
    plot_data <- data %>%
      filter(survey == survey_name, covariate_label == cov_label)

    if (nrow(plot_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data",
                        size = 4, color = "gray50") +
               theme_void() +
               labs(title = if (show_title) survey_name else NULL))
    }

    n_levels <- length(unique(plot_data$level))

    if (n_levels > 1) {
      p <- ggplot(plot_data, aes(x = year, y = coefficient, color = level, group = level)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
        geom_line(linewidth = 0.8, alpha = 0.8) +
        geom_point(size = 1.5, alpha = 0.8) +
        scale_color_viridis_d(option = "D", end = 0.9) +
        labs(color = NULL)
    } else {
      p <- ggplot(plot_data, aes(x = year, y = coefficient)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
        geom_line(linewidth = 0.8, color = "#3C5488") +
        geom_point(size = 1.5, color = "#3C5488")
    }

    p + scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
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
  }

  # Sex row
  sex_panels <- lapply(survey_order, function(svy) {
    create_covar_pooled_panel(covar_coef, svy, "Sex",
                               show_title = TRUE,
                               show_ylabel = (svy == survey_order[1]))
  })
  row_sex <- wrap_plots(sex_panels, ncol = 6) +
    plot_annotation(subtitle = "Sex (Female vs Male)") &
    theme(legend.position = "none")

  # Race row
  race_panels <- lapply(survey_order, function(svy) {
    create_covar_pooled_panel(covar_coef, svy, "Race/Ethnicity",
                               show_title = FALSE,
                               show_ylabel = (svy == survey_order[1]))
  })
  row_race <- wrap_plots(race_panels, ncol = 6, guides = "collect") +
    plot_annotation(subtitle = "Race/Ethnicity (vs White)") &
    theme(legend.position = "bottom")

  # Education row
  educ_panels <- lapply(survey_order, function(svy) {
    create_covar_pooled_panel(covar_coef, svy, "Education",
                               show_title = FALSE,
                               show_ylabel = (svy == survey_order[1]))
  })
  row_educ <- wrap_plots(educ_panels, ncol = 6, guides = "collect") +
    plot_annotation(subtitle = "Education (vs Less than HS)") &
    theme(legend.position = "bottom")

  # Combine
  fig_covar_pooled <- (row_sex / row_race / row_educ) +
    plot_annotation(
      title = "Covariate Coefficients on SRH from Combined Model (All Ages Pooled)",
      subtitle = "Model: SRH ~ age + sex + race + education",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  ggsave(
    filename = file.path(fig_dir, paste0("fig_covar_combined_pooled_draft_", date_suffix, ".png")),
    plot = fig_covar_pooled,
    width = 14, height = 10, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "fig_covar_combined_pooled.png"),
    plot = fig_covar_pooled,
    width = 14, height = 10, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "fig_covar_combined_pooled.pdf"),
    plot = fig_covar_pooled,
    width = 14, height = 10
  )
  cat("  Saved: fig_covar_combined_pooled (.png and .pdf)\n")
}

cat("\n")


# ==============================================================================
# PART 8: FIGURE — COVARIATE COEFFICIENTS BY AGE GROUP (RAINBOW LINES)
# ==============================================================================
# Analogous to Figure S2 Panel B

cat("Creating Figure: Covariate Coefficients by Age Group (Combined Model)...\n")

if (!is.null(coef_byage_all) && nrow(coef_byage_all) > 0) {

  create_byage_panel <- function(data, survey_name, cov_label, level_name,
                                  show_title = FALSE, show_ylabel = FALSE) {
    plot_data <- data %>%
      filter(survey == survey_name,
             covariate_label == !!cov_label,
             level == level_name)

    if (nrow(plot_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data",
                        size = 3, color = "gray50") +
               theme_void() +
               labs(title = if (show_title) paste0(survey_name, ": ", level_name) else NULL))
    }

    plot_data$age_group <- factor(plot_data$age_group, levels = AGE_GROUPS)

    ggplot(plot_data, aes(x = year, y = coefficient,
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
  }

  # Build 4-row x 6-column grid
  # Row 1: Sex (Female), Row 2: Black, Row 3: Hispanic, Row 4: Education BA+
  all_panels_b <- list()

  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    all_panels_b[[length(all_panels_b) + 1]] <-
      create_byage_panel(coef_byage_all, svy, "Sex", "Female",
                          show_title = TRUE, show_ylabel = (i == 1))
  }

  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    all_panels_b[[length(all_panels_b) + 1]] <-
      create_byage_panel(coef_byage_all, svy, "Race/Ethnicity", "Black",
                          show_title = FALSE, show_ylabel = (i == 1))
  }

  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    all_panels_b[[length(all_panels_b) + 1]] <-
      create_byage_panel(coef_byage_all, svy, "Race/Ethnicity", "Hispanic",
                          show_title = FALSE, show_ylabel = (i == 1))
  }

  for (i in seq_along(survey_order)) {
    svy <- survey_order[i]
    all_panels_b[[length(all_panels_b) + 1]] <-
      create_byage_panel(coef_byage_all, svy, "Education", "BA_plus",
                          show_title = FALSE, show_ylabel = (i == 1))
  }

  panel_b <- wrap_plots(all_panels_b, ncol = 6, nrow = 4) +
    plot_annotation(
      title = "Covariate Coefficients on SRH by Age Group (Combined Model)",
      subtitle = "Row 1: Female vs Male | Row 2: Black vs White | Row 3: Hispanic vs White | Row 4: BA+ vs LT HS\nModel: SRH ~ sex + race + education (within each age group)",
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0.5)
      )
    )

  # Add shared legend
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

  panel_b_with_legend <- panel_b / wrap_elements(legend_grob) +
    plot_layout(heights = c(1, 0.08))

  ggsave(
    filename = file.path(fig_dir, paste0("fig_covar_combined_byage_draft_", date_suffix, ".png")),
    plot = panel_b_with_legend,
    width = 14, height = 12, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "fig_covar_combined_byage.png"),
    plot = panel_b_with_legend,
    width = 14, height = 12, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "fig_covar_combined_byage.pdf"),
    plot = panel_b_with_legend,
    width = 14, height = 12
  )
  cat("  Saved: fig_covar_combined_byage (.png and .pdf)\n")
}

cat("\n")


# ==============================================================================
# PART 9: COMBINED FIGURE (ALL THREE PANELS)
# ==============================================================================

cat("Creating combined figure...\n")

if (exists("fig_age_combined") && exists("fig_covar_pooled") && exists("panel_b_with_legend")) {

  fig_all_combined <- (fig_age_combined / fig_covar_pooled / panel_b_with_legend) +
    plot_layout(heights = c(0.2, 0.4, 0.5)) +
    plot_annotation(
      title = "Sociodemographic Sensitivity: Combined Model (SRH ~ age + sex + race + education)",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  ggsave(
    filename = file.path(fig_dir, paste0("fig_combined_model_all_draft_", date_suffix, ".png")),
    plot = fig_all_combined,
    width = 16, height = 24, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "fig_combined_model_all.png"),
    plot = fig_all_combined,
    width = 16, height = 24, dpi = 300
  )
  ggsave(
    filename = file.path(fig_dir, "fig_combined_model_all.pdf"),
    plot = fig_all_combined,
    width = 16, height = 24
  )
  cat("  Saved: fig_combined_model_all (.png and .pdf)\n")
}

cat("\n")


# ==============================================================================
# PART 10: VERIFICATION SUMMARY
# ==============================================================================

cat("========================================\n")
cat("Verification Summary\n")
cat("========================================\n\n")

if (!is.null(coef_combined_all) && nrow(coef_combined_all) > 0) {

  # Age coefficient summary
  cat("--- Age Coefficient (from combined model) ---\n")
  age_summary <- coef_combined_all %>%
    filter(covariate == "age") %>%
    group_by(survey) %>%
    summarise(
      n_years = n(),
      mean_coef = round(mean(coefficient, na.rm = TRUE), 5),
      mean_se = round(mean(se, na.rm = TRUE), 5),
      .groups = "drop"
    )
  print(age_summary)
  cat("\n")

  # Covariate coefficient summary
  cat("--- Covariate Coefficients (from combined model) ---\n")
  covar_summary <- coef_combined_all %>%
    filter(covariate != "age") %>%
    group_by(survey, covariate_label, level) %>%
    summarise(
      n_years = n(),
      mean_coef = round(mean(coefficient, na.rm = TRUE), 4),
      mean_se = round(mean(se, na.rm = TRUE), 4),
      .groups = "drop"
    )
  print(covar_summary, n = 50)
  cat("\n")
}

if (!is.null(coef_byage_all) && nrow(coef_byage_all) > 0) {
  cat("--- By-Age Coefficients Summary ---\n")
  byage_summary <- coef_byage_all %>%
    group_by(survey, covariate_label) %>%
    summarise(
      n_cells = n(),
      mean_coef = round(mean(coefficient, na.rm = TRUE), 4),
      .groups = "drop"
    )
  print(byage_summary, n = 30)
  cat("\n")
}

cat("========================================\n")
cat("Done! Combined sociodemographic model complete.\n")
cat("========================================\n")
