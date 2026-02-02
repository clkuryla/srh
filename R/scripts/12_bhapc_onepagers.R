# ==============================================================================
# 12_bhapc_onepagers.R
# Create one-pager summaries combining all 6 surveys
#
# Output Types:
#   1. Table2 one-pager: Fixed effects + variance components
#   2. Variance decomposition one-pager
#   3. Age×Period interaction one-pager
#   4. Figure 2 combined: Mean SRH by age and period (assembled PNGs)
#   5. Figure 3 reshaped: 6 rows × 3 cols (Age, Period, Cohort effects)
#
# Survey order: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
# Note: GSS uses 4-point SRH scale (1-4) vs 5-point for others
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)
library(grid)
library(gridExtra)
library(png)

# Source project functions
source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "bhapc_model_fitting.R"))

# Set theme
theme_set(theme_srh())

# Survey order (fixed)
SURVEY_ORDER <- c("brfss", "meps", "nhis", "cps", "nhanes", "gss")
SURVEY_LABELS <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS*")

# Output directory
output_dir <- here("output", "bhapc_parallel")
onepager_dir <- file.path(output_dir, "onepagers")
if (!dir.exists(onepager_dir)) dir.create(onepager_dir, recursive = TRUE)

# Date suffix for drafts
date_suffix <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# PART 1: HELPER FUNCTIONS
# ==============================================================================

#' Check if survey outputs exist
#' @param survey Survey name (lowercase)
#' @param file_type Type of file to check
#' @return Logical
survey_output_exists <- function(survey, file_type = "table2") {
  survey_dir <- file.path(output_dir, survey)

  file_patterns <- list(
    table2 = paste0("table2_", survey, ".csv"),
    variance = paste0(survey, "_variance_decomposition.csv"),
    interaction = paste0("age_period_interaction_", survey, ".csv"),
    figure2 = paste0("figure2_", survey, ".png"),
    figure3 = paste0("figure3_", survey, ".png"),
    model = paste0(survey, "_bhapc_model.rds"),
    data = paste0(survey, "_bhapc_data.rds")
  )


  file_path <- file.path(survey_dir, file_patterns[[file_type]])
  file.exists(file_path)
}

#' Get path to survey output file
#' @param survey Survey name (lowercase)
#' @param file_type Type of file
#' @return Character path
get_survey_path <- function(survey, file_type) {
  survey_dir <- file.path(output_dir, survey)

  file_patterns <- list(
    table2 = paste0("table2_", survey, ".csv"),
    variance = paste0(survey, "_variance_decomposition.csv"),
    interaction = paste0("age_period_interaction_", survey, ".csv"),
    figure2 = paste0("figure2_", survey, ".png"),
    figure3 = paste0("figure3_", survey, ".png"),
    model = paste0(survey, "_bhapc_model.rds"),
    data = paste0(survey, "_bhapc_data.rds")
  )

  file.path(survey_dir, file_patterns[[file_type]])
}

#' Create placeholder plot for missing data
#' @param survey_label Survey label to display
#' @param message Additional message
#' @return ggplot object
create_placeholder <- function(survey_label, message = "Analysis in progress") {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1,
             fill = "gray90", color = "gray70") +
    annotate("text", x = 0.5, y = 0.6, label = survey_label,
             size = 6, fontface = "bold", color = "gray40") +
    annotate("text", x = 0.5, y = 0.4, label = message,
             size = 4, color = "gray50", fontface = "italic") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "gray90", color = "gray70"),
      plot.margin = margin(5, 5, 5, 5)
    )
}

#' Create placeholder table grob for missing data
#' @param survey_label Survey label
#' @return tableGrob object
create_placeholder_table <- function(survey_label) {
  df <- data.frame(
    ` ` = c(survey_label, "", "Analysis", "in progress"),
    check.names = FALSE
  )

  tableGrob(
    df,
    rows = NULL,
    theme = ttheme_minimal(
      core = list(
        fg_params = list(col = "gray50", fontface = "italic"),
        bg_params = list(fill = "gray90")
      ),
      colhead = list(
        fg_params = list(col = "gray50"),
        bg_params = list(fill = "gray90")
      )
    )
  )
}


# ==============================================================================
# PART 2: LOAD ALL CSV DATA
# ==============================================================================

message("\n========== Loading CSV data from all surveys ==========\n")

# Load table2 data
table2_list <- lapply(SURVEY_ORDER, function(s) {
  if (survey_output_exists(s, "table2")) {
    df <- read_csv(get_survey_path(s, "table2"), show_col_types = FALSE)
    message("  Loaded table2 for ", toupper(s))
    df
  } else {
    message("  Missing table2 for ", toupper(s))
    NULL
  }
})
names(table2_list) <- SURVEY_ORDER

# Load variance decomposition data
variance_list <- lapply(SURVEY_ORDER, function(s) {
  if (survey_output_exists(s, "variance")) {
    df <- read_csv(get_survey_path(s, "variance"), show_col_types = FALSE)
    message("  Loaded variance for ", toupper(s))
    df
  } else {
    message("  Missing variance for ", toupper(s))
    NULL
  }
})
names(variance_list) <- SURVEY_ORDER

# Load interaction data
interaction_list <- lapply(SURVEY_ORDER, function(s) {
  if (survey_output_exists(s, "interaction")) {
    df <- read_csv(get_survey_path(s, "interaction"), show_col_types = FALSE)
    message("  Loaded interaction for ", toupper(s))
    df
  } else {
    message("  Missing interaction for ", toupper(s))
    NULL
  }
})
names(interaction_list) <- SURVEY_ORDER


# ==============================================================================
# PART 3: TABLE2 ONE-PAGER
# ==============================================================================

message("\n========== Creating Table2 One-Pager ==========\n")

#' Format table2 for display
#' @param df Table2 data frame
#' @param survey_label Survey label
#' @return tableGrob
format_table2_grob <- function(df, survey_label) {
  if (is.null(df)) {
    return(create_placeholder_table(survey_label))
  }

  # Clean up the data for display
  display_df <- df %>%
    filter(!is.na(estimate) | effect %in% c("Fixed Effects", "Variance Components")) %>%
    mutate(
      effect = case_when(
        effect == "Fixed Effects" ~ "--- Fixed Effects ---",
        effect == "Variance Components" ~ "--- Variance ---",
        effect == "" ~ "",
        TRUE ~ effect
      ),
      estimate = case_when(
        is.na(estimate) ~ "",
        abs(estimate) < 0.001 ~ sprintf("%.2e", estimate),
        TRUE ~ sprintf("%.4f", estimate)
      ),
      ci_90 = ifelse(is.na(ci_90), "", ci_90)
    ) %>%
    select(Effect = effect, Est = estimate, `90% CI` = ci_90)

  # Create table grob
  tt <- ttheme_minimal(
    base_size = 9,
    core = list(
      fg_params = list(hjust = 0, x = 0.05),
      bg_params = list(fill = c("white", "gray95"))
    ),
    colhead = list(
      fg_params = list(fontface = "bold", hjust = 0.5),
      bg_params = list(fill = "gray85")
    )
  )

  # Add title
  title_grob <- textGrob(
    survey_label,
    gp = gpar(fontsize = 12, fontface = "bold"),
    hjust = 0.5
  )

  table_grob <- tableGrob(display_df, rows = NULL, theme = tt)

  # Combine title and table
  arrangeGrob(
    title_grob,
    table_grob,
    ncol = 1,
    heights = unit(c(0.8, 5), "cm")
  )
}

# Create table2 grobs for all surveys
table2_grobs <- mapply(
  format_table2_grob,
  table2_list,
  SURVEY_LABELS,
  SIMPLIFY = FALSE
)

# Arrange in 2x3 grid
table2_combined <- arrangeGrob(
  grobs = table2_grobs,
  ncol = 3, nrow = 2,
  top = textGrob(
    "BHAPC Model Results: Fixed Effects and Variance Components",
    gp = gpar(fontsize = 16, fontface = "bold")
  ),
  bottom = textGrob(
    "*GSS uses 4-point SRH scale (1-4); other surveys use 5-point scale (1-5)",
    gp = gpar(fontsize = 10, fontface = "italic", col = "gray40")
  )
)

# Save
ggsave(
  file.path(onepager_dir, paste0("onepager_table2_", date_suffix, ".png")),
  table2_combined, width = 15, height = 10, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_table2.png"),
  table2_combined, width = 15, height = 10, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_table2.pdf"),
  table2_combined, width = 15, height = 10
)
message("Saved: onepager_table2.png/pdf")


# ==============================================================================
# PART 3B: TABLE2 ONE-PAGER - ALTERNATIVE (SINGLE TABLE FORMAT)
# ==============================================================================

message("\n========== Creating Table2 One-Pager (Alternative) ==========\n")

# Combine all table2 data into single wide-format table
table2_combined_df <- bind_rows(

  lapply(seq_along(SURVEY_ORDER), function(i) {
    s <- SURVEY_ORDER[i]
    label <- SURVEY_LABELS[i]
    df <- table2_list[[s]]

    if (is.null(df)) {
      # Return placeholder row for missing survey
      tibble(
        Survey = label,
        `Age (linear)` = NA_character_,
        `Age (quadratic)` = NA_character_,
        `Log weight` = NA_character_,
        `Period var` = NA_character_,
        `Cohort var` = NA_character_,
        `Residual var` = NA_character_
      )
    } else {
      # Extract values
      age_lin <- df %>% filter(effect == "Age (linear)")
      age_quad <- df %>% filter(effect == "Age (quadratic)")
      log_wt <- df %>% filter(effect == "Log weight")
      period_var <- df %>% filter(effect == "Period variance")
      cohort_var <- df %>% filter(effect == "Cohort variance")
      resid_var <- df %>% filter(effect == "Residual variance")

      # Format with estimate and CI
      format_val <- function(row) {
        if (nrow(row) == 0 || is.na(row$estimate[1])) return("—")
        est <- row$estimate[1]
        ci <- row$ci_90[1]
        if (abs(est) < 0.001) {
          paste0(sprintf("%.2e", est), " ", ci)
        } else {
          paste0(sprintf("%.4f", est), " ", ci)
        }
      }

      tibble(
        Survey = label,
        `Age (linear)` = format_val(age_lin),
        `Age (quadratic)` = format_val(age_quad),
        `Log weight` = format_val(log_wt),
        `Period var` = format_val(period_var),
        `Cohort var` = format_val(cohort_var),
        `Residual var` = format_val(resid_var)
      )
    }
  })
)

# Create table grob
tt_alt <- ttheme_default(
  base_size = 10,
  core = list(
    fg_params = list(hjust = 0.5, x = 0.5),
    bg_params = list(fill = c("white", "gray95"))
  ),
  colhead = list(
    fg_params = list(fontface = "bold", hjust = 0.5),
    bg_params = list(fill = "gray80")
  ),
  rowhead = list(
    fg_params = list(fontface = "bold")
  )
)

table2_alt_grob <- tableGrob(table2_combined_df, rows = NULL, theme = tt_alt)

# Add title and footnote
table2_alt_combined <- arrangeGrob(
  textGrob(
    "BHAPC Model Results: Fixed Effects and Variance Components",
    gp = gpar(fontsize = 16, fontface = "bold")
  ),
  textGrob(
    "Estimates with 90% credible intervals",
    gp = gpar(fontsize = 11, col = "gray40")
  ),
  table2_alt_grob,
  textGrob(
    "*GSS uses 4-point SRH scale (1-4); other surveys use 5-point scale (1-5). Variance shown as value (% of total).",
    gp = gpar(fontsize = 9, fontface = "italic", col = "gray50")
  ),
  ncol = 1,
  heights = unit(c(1.2, 0.8, 6, 0.8), "cm")
)

# Save
ggsave(
  file.path(onepager_dir, paste0("onepager_table2_alt_", date_suffix, ".png")),
  table2_alt_combined, width = 16, height = 6, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_table2_alt.png"),
  table2_alt_combined, width = 16, height = 6, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_table2_alt.pdf"),
  table2_alt_combined, width = 16, height = 6
)
message("Saved: onepager_table2_alt.png/pdf")


# ==============================================================================
# PART 4: VARIANCE DECOMPOSITION ONE-PAGER
# ==============================================================================

message("\n========== Creating Variance Decomposition One-Pager ==========\n")

#' Format variance decomposition for display
#' @param df Variance decomposition data frame
#' @param survey_label Survey label
#' @return tableGrob
format_variance_grob <- function(df, survey_label) {
  if (is.null(df)) {
    return(create_placeholder_table(survey_label))
  }

  # Clean up for display
  display_df <- df %>%
    mutate(
      Component = case_when(
        component == "period_4yr" ~ "Period",
        component == "cohort_4yr" ~ "Cohort",
        component == "Residual" ~ "Residual",
        component == "Total" ~ "Total",
        component == "strata" ~ "Strata",
        component == "psu:strata" ~ "PSU:Strata",
        TRUE ~ component
      ),
      Variance = sprintf("%.4f", variance),
      `% Total` = sprintf("%.2f%%", pct_of_total),
      SD = sprintf("%.4f", sd)
    ) %>%
    select(Component, Variance, `% Total`, SD)

  # Create table grob
  tt <- ttheme_minimal(
    base_size = 9,
    core = list(
      fg_params = list(hjust = 0.5),
      bg_params = list(fill = c("white", "gray95"))
    ),
    colhead = list(
      fg_params = list(fontface = "bold"),
      bg_params = list(fill = "gray85")
    )
  )

  title_grob <- textGrob(
    survey_label,
    gp = gpar(fontsize = 12, fontface = "bold")
  )

  table_grob <- tableGrob(display_df, rows = NULL, theme = tt)

  arrangeGrob(
    title_grob,
    table_grob,
    ncol = 1,
    heights = unit(c(0.8, 5), "cm")
  )
}

# Create variance grobs
variance_grobs <- mapply(
  format_variance_grob,
  variance_list,
  SURVEY_LABELS,
  SIMPLIFY = FALSE
)

# Arrange in 2x3 grid
variance_combined <- arrangeGrob(
  grobs = variance_grobs,
  ncol = 3, nrow = 2,
  top = textGrob(
    "BHAPC Variance Decomposition by Survey",
    gp = gpar(fontsize = 16, fontface = "bold")
  ),
  bottom = textGrob(
    "*GSS uses 4-point SRH scale (1-4); other surveys use 5-point scale (1-5)",
    gp = gpar(fontsize = 10, fontface = "italic", col = "gray40")
  )
)

# Save
ggsave(
  file.path(onepager_dir, paste0("onepager_variance_", date_suffix, ".png")),
  variance_combined, width = 15, height = 10, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_variance.png"),
  variance_combined, width = 15, height = 10, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_variance.pdf"),
  variance_combined, width = 15, height = 10
)
message("Saved: onepager_variance.png/pdf")


# ==============================================================================
# PART 4B: VARIANCE DECOMPOSITION ONE-PAGER - ALTERNATIVE (SINGLE TABLE FORMAT)
# ==============================================================================

message("\n========== Creating Variance Decomposition One-Pager (Alternative) ==========\n")

# Combine all variance data into single wide-format table
variance_combined_df <- bind_rows(
  lapply(seq_along(SURVEY_ORDER), function(i) {
    s <- SURVEY_ORDER[i]
    label <- SURVEY_LABELS[i]
    df <- variance_list[[s]]

    if (is.null(df)) {
      # Return placeholder row for missing survey
      tibble(
        Survey = label,
        `Period Var` = "—",
        `Period %` = "—",
        `Cohort Var` = "—",
        `Cohort %` = "—",
        `Residual Var` = "—",
        `Residual %` = "—",
        `Total Var` = "—"
      )
    } else {
      # Extract values
      period <- df %>% filter(component == "period_4yr")
      cohort <- df %>% filter(component == "cohort_4yr")
      resid <- df %>% filter(component == "Residual")
      total <- df %>% filter(component == "Total")

      get_var <- function(row) {
        if (nrow(row) == 0) return("—")
        sprintf("%.4f", row$variance[1])
      }

      get_pct <- function(row) {
        if (nrow(row) == 0) return("—")
        sprintf("%.2f%%", row$pct_of_total[1])
      }

      tibble(
        Survey = label,
        `Period Var` = get_var(period),
        `Period %` = get_pct(period),
        `Cohort Var` = get_var(cohort),
        `Cohort %` = get_pct(cohort),
        `Residual Var` = get_var(resid),
        `Residual %` = get_pct(resid),
        `Total Var` = get_var(total)
      )
    }
  })
)

# Create table grob
variance_alt_grob <- tableGrob(variance_combined_df, rows = NULL, theme = tt_alt)

# Add title and footnote
variance_alt_combined <- arrangeGrob(
  textGrob(
    "BHAPC Variance Decomposition by Survey",
    gp = gpar(fontsize = 16, fontface = "bold")
  ),
  textGrob(
    "Variance components from Bayesian HAPC model",
    gp = gpar(fontsize = 11, col = "gray40")
  ),
  variance_alt_grob,
  textGrob(
    "*GSS uses 4-point SRH scale (1-4); other surveys use 5-point scale (1-5). Period and Cohort are random effect variances.",
    gp = gpar(fontsize = 9, fontface = "italic", col = "gray50")
  ),
  ncol = 1,
  heights = unit(c(1.2, 0.8, 6, 0.8), "cm")
)

# Save
ggsave(
  file.path(onepager_dir, paste0("onepager_variance_alt_", date_suffix, ".png")),
  variance_alt_combined, width = 14, height = 6, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_variance_alt.png"),
  variance_alt_combined, width = 14, height = 6, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_variance_alt.pdf"),
  variance_alt_combined, width = 14, height = 6
)
message("Saved: onepager_variance_alt.png/pdf")


# ==============================================================================
# PART 5: AGE×PERIOD INTERACTION ONE-PAGER
# ==============================================================================

message("\n========== Creating Age×Period Interaction One-Pager ==========\n")

#' Format interaction table for display
#' @param df Interaction data frame
#' @param survey_label Survey label
#' @return tableGrob
format_interaction_grob <- function(df, survey_label) {
  if (is.null(df)) {
    return(create_placeholder_table(survey_label))
  }

  # Clean up for display - show all periods
  display_df <- df %>%
    mutate(
      Period = as.character(period),
      `Age Slope` = sprintf("%.4f", age_slope),
      Interaction = sprintf("%.4f", interaction),
      `90% CI` = paste0("[", sprintf("%.4f", ci_10), ", ", sprintf("%.4f", ci_90), "]"),
      Sig = ifelse(significant == "*", "*", "")
    ) %>%
    select(Period, `Age Slope`, Interaction, `90% CI`, Sig)

  # Create table grob with smaller font for many rows
  base_size <- ifelse(nrow(display_df) > 10, 7, 8)

  tt <- ttheme_minimal(
    base_size = base_size,
    core = list(
      fg_params = list(hjust = 0.5),
      bg_params = list(fill = c("white", "gray95"))
    ),
    colhead = list(
      fg_params = list(fontface = "bold"),
      bg_params = list(fill = "gray85")
    )
  )

  title_grob <- textGrob(
    survey_label,
    gp = gpar(fontsize = 11, fontface = "bold")
  )

  table_grob <- tableGrob(display_df, rows = NULL, theme = tt)

  arrangeGrob(
    title_grob,
    table_grob,
    ncol = 1,
    heights = unit(c(0.6, 6), "cm")
  )
}

# Create interaction grobs
interaction_grobs <- mapply(
  format_interaction_grob,
  interaction_list,
  SURVEY_LABELS,
  SIMPLIFY = FALSE
)

# Arrange in 2x3 grid
interaction_combined <- arrangeGrob(
  grobs = interaction_grobs,
  ncol = 3, nrow = 2,
  top = textGrob(
    "Age × Period Interaction: Change in Age Slope Over Time",
    gp = gpar(fontsize = 16, fontface = "bold")
  ),
  bottom = textGrob(
    "*GSS uses 4-point SRH scale (1-4); other surveys use 5-point scale (1-5). Sig = 90% CI excludes 0.",
    gp = gpar(fontsize = 10, fontface = "italic", col = "gray40")
  )
)

# Save
ggsave(
  file.path(onepager_dir, paste0("onepager_interaction_", date_suffix, ".png")),
  interaction_combined, width = 16, height = 12, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_interaction.png"),
  interaction_combined, width = 16, height = 12, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_interaction.pdf"),
  interaction_combined, width = 16, height = 12
)
message("Saved: onepager_interaction.png/pdf")


# ==============================================================================
# PART 6: FIGURE 2 COMBINED ONE-PAGER (ASSEMBLED PNGs)
# ==============================================================================

message("\n========== Creating Figure 2 Combined One-Pager ==========\n")

#' Load PNG as raster and create ggplot
#' @param survey Survey name
#' @param survey_label Survey label for placeholder
#' @return ggplot object
load_figure2_as_plot <- function(survey, survey_label) {
  if (!survey_output_exists(survey, "figure2")) {
    return(create_placeholder(survey_label, "Figure not available"))
  }

  img_path <- get_survey_path(survey, "figure2")
  img <- png::readPNG(img_path)

  ggplot() +
    annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    coord_fixed(ratio = nrow(img) / ncol(img)) +
    theme_void() +
    theme(plot.margin = margin(2, 2, 2, 2))
}

# Create plot list
figure2_plots <- mapply(
  load_figure2_as_plot,
  SURVEY_ORDER,
  SURVEY_LABELS,
  SIMPLIFY = FALSE
)

# Combine using patchwork
figure2_combined <- (
  (figure2_plots[[1]] | figure2_plots[[2]] | figure2_plots[[3]]) /
  (figure2_plots[[4]] | figure2_plots[[5]] | figure2_plots[[6]])
) +
  plot_annotation(
    title = "Mean Self-Rated Health by Age and Period",
    subtitle = "Survey-weighted means with 95% confidence intervals",
    caption = "*GSS uses 4-point SRH scale (1-4); other surveys use 5-point scale (1-5)",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, hjust = 0.5, face = "italic", color = "gray40")
    )
  )

# Save
ggsave(
  file.path(onepager_dir, paste0("onepager_figure2_", date_suffix, ".png")),
  figure2_combined, width = 18, height = 12, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_figure2.png"),
  figure2_combined, width = 18, height = 12, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_figure2.pdf"),
  figure2_combined, width = 18, height = 12
)
message("Saved: onepager_figure2.png/pdf")


# ==============================================================================
# PART 7: FIGURE 3 RESHAPED - 6 ROWS × 3 COLUMNS (APC EFFECTS)
# ==============================================================================

message("\n========== Creating Figure 3 Reshaped One-Pager ==========\n")

# Colors for APC effects (consistent with existing figures)
APC_COLORS <- list(
  age = "#0072B2",     # Blue
  period = "#009E73",  # Green
  cohort = "#CC79A7"   # Pink
)

# Directory for cached effect extracts
effects_cache_dir <- file.path(onepager_dir, "effects_cache")
if (!dir.exists(effects_cache_dir)) dir.create(effects_cache_dir, recursive = TRUE)

#' Check if cached effects exist for a survey
#' @param survey Survey name
#' @return Logical
cached_effects_exist <- function(survey) {
  age_file <- file.path(effects_cache_dir, paste0(survey, "_age_effect.csv"))
  period_file <- file.path(effects_cache_dir, paste0(survey, "_period_effect.csv"))
  cohort_file <- file.path(effects_cache_dir, paste0(survey, "_cohort_effect.csv"))
  all(file.exists(c(age_file, period_file, cohort_file)))
}

#' Load cached effects for a survey
#' @param survey Survey name
#' @return List with age, period, cohort effects
load_cached_effects <- function(survey) {
  age_file <- file.path(effects_cache_dir, paste0(survey, "_age_effect.csv"))
  period_file <- file.path(effects_cache_dir, paste0(survey, "_period_effect.csv"))
  cohort_file <- file.path(effects_cache_dir, paste0(survey, "_cohort_effect.csv"))

  # Load and filter out any NA rows (spurious from model summary)
  list(
    age = read_csv(age_file, show_col_types = FALSE) %>%
      filter(!is.na(age)),
    period = read_csv(period_file, show_col_types = FALSE) %>%
      filter(!is.na(period)),
    cohort = read_csv(cohort_file, show_col_types = FALSE) %>%
      filter(!is.na(cohort))
  )
}

#' Save effects to cache
#' @param effects List with age, period, cohort effects
#' @param survey Survey name
save_effects_to_cache <- function(effects, survey) {
  if (is.null(effects)) return(invisible(NULL))

  age_file <- file.path(effects_cache_dir, paste0(survey, "_age_effect.csv"))
  period_file <- file.path(effects_cache_dir, paste0(survey, "_period_effect.csv"))
  cohort_file <- file.path(effects_cache_dir, paste0(survey, "_cohort_effect.csv"))

  write_csv(effects$age, age_file)
  write_csv(effects$period, period_file)
  write_csv(effects$cohort, cohort_file)

  message("  Saved effects to cache for ", toupper(survey))
}

#' Extract APC effects from model (memory-efficient)
#' @param survey Survey name
#' @return List with age, period, cohort effects data frames
extract_apc_effects <- function(survey) {
  if (!survey_output_exists(survey, "model") || !survey_output_exists(survey, "data")) {
    message("  Missing model/data for ", toupper(survey))
    return(NULL)
  }

  message("  Loading model for ", toupper(survey), "...")

  # Load model result object and data
  model_result <- readRDS(get_survey_path(survey, "model"))
  bhapc_df <- readRDS(get_survey_path(survey, "data"))

  # The saved model is a list wrapper - extract the actual model
  # Handle both cases: raw model or result$model
  if (inherits(model_result, "stanreg")) {
    model <- model_result
  } else if (is.list(model_result) && "model" %in% names(model_result)) {
    model <- model_result$model
  } else {
    message("  Unexpected model format for ", toupper(survey))
    return(NULL)
  }

  # Extract age effect
  age_range <- c(
    min(bhapc_df$age, na.rm = TRUE),
    max(bhapc_df$age, na.rm = TRUE)
  )
  age_effect <- compute_age_effect(model, bhapc_df, age_range = age_range)
  age_effect$survey <- survey

  # Extract random effects
  random_effects <- extract_random_effects(model, bhapc_df)

  period_effect <- random_effects$period_effects
  period_effect$survey <- survey

  cohort_effect <- random_effects$cohort_effects
  cohort_effect$survey <- survey

  # Clear model from memory
  rm(model, model_result)
  gc()

  message("  Extracted effects for ", toupper(survey))

  list(
    age = age_effect,
    period = period_effect,
    cohort = cohort_effect
  )
}

#' Get effects for a survey (from cache or by extraction)
#' @param survey Survey name
#' @param force_extract Force re-extraction even if cache exists
#' @return List with age, period, cohort effects
get_survey_effects <- function(survey, force_extract = FALSE) {
  # Check cache first
  if (!force_extract && cached_effects_exist(survey)) {
    message("  Loading cached effects for ", toupper(survey))
    return(load_cached_effects(survey))
  }

  # Extract from model
  effects <- tryCatch(
    extract_apc_effects(survey),
    error = function(e) {
      message("  Error extracting ", toupper(survey), ": ", e$message)
      NULL
    }
  )

  # Save to cache if successful
  if (!is.null(effects)) {
    save_effects_to_cache(effects, survey)
  }

  effects
}

# Check for --force-extract argument or environment variable
force_extract <- Sys.getenv("BHAPC_FORCE_EXTRACT", "FALSE") == "TRUE"
if (force_extract) {
  message("\nForce extraction enabled - will re-extract all effects from models\n")
} else {
  message("\nWill use cached effects where available (set BHAPC_FORCE_EXTRACT=TRUE to re-extract)\n")
}

# Extract or load effects from all available surveys
message("Getting APC effects from all surveys...\n")
effects_list <- lapply(SURVEY_ORDER, function(s) {
  get_survey_effects(s, force_extract = force_extract)
})
names(effects_list) <- SURVEY_ORDER

# Combine all effects to determine global axis limits
all_age <- bind_rows(lapply(effects_list, function(x) if (!is.null(x)) x$age else NULL))
all_period <- bind_rows(lapply(effects_list, function(x) if (!is.null(x)) x$period else NULL))
all_cohort <- bind_rows(lapply(effects_list, function(x) if (!is.null(x)) x$cohort else NULL))

# Filter out NA values (spurious rows from model summary)
all_period <- all_period %>% filter(!is.na(period))
all_cohort <- all_cohort %>% filter(!is.na(cohort))

# Calculate global limits for alignment
if (nrow(all_age) > 0) {
  age_xlim <- c(min(all_age$age, na.rm = TRUE), max(all_age$age, na.rm = TRUE))
  age_ylim <- c(
    min(all_age$ci_lower_centered, na.rm = TRUE),
    max(all_age$ci_upper_centered, na.rm = TRUE)
  ) * 1.1  # 10% padding
} else {
  age_xlim <- c(18, 89)
  age_ylim <- c(-1.5, 0.5)
}

if (nrow(all_period) > 0) {
  period_xlim <- c(min(all_period$period, na.rm = TRUE), max(all_period$period, na.rm = TRUE))
  period_ylim <- c(
    min(all_period$ci_lower_90, na.rm = TRUE),
    max(all_period$ci_upper_90, na.rm = TRUE)
  ) * 1.1
} else {
  period_xlim <- c(1970, 2025)
  period_ylim <- c(-0.2, 0.2)
}

if (nrow(all_cohort) > 0) {
  cohort_xlim <- c(min(all_cohort$cohort, na.rm = TRUE), max(all_cohort$cohort, na.rm = TRUE))
  cohort_ylim <- c(
    min(all_cohort$ci_lower_90, na.rm = TRUE),
    max(all_cohort$ci_upper_90, na.rm = TRUE)
  ) * 1.1
} else {
  cohort_xlim <- c(1880, 2010)
  cohort_ylim <- c(-0.2, 0.2)
}

message("\nGlobal axis limits:")
message("  Age X: ", paste(age_xlim, collapse = " - "))
message("  Age Y: ", paste(round(age_ylim, 2), collapse = " - "))
message("  Period X: ", paste(period_xlim, collapse = " - "))
message("  Cohort X: ", paste(cohort_xlim, collapse = " - "))

#' Create individual APC panel with consistent axes
#' @param effect_df Effect data frame
#' @param effect_type "age", "period", or "cohort"
#' @param survey_label Survey label (for row label)
#' @param show_title Show column title (TRUE for first row only)
#' @param show_row_label Show survey label on left
#' @return ggplot object
create_apc_panel <- function(effect_df, effect_type, survey_label,
                              show_title = FALSE, show_row_label = FALSE) {

  # Handle missing data
  if (is.null(effect_df) || nrow(effect_df) == 0) {
    p <- create_placeholder(
      if (show_row_label) survey_label else "",
      "Analysis in progress"
    )
    if (show_title) {
      titles <- c(age = "Age Effect", period = "Period Effect", cohort = "Cohort Effect")
      p <- p + labs(title = titles[[effect_type]])
    }
    return(p)
  }

  # Get appropriate color
  color <- APC_COLORS[[effect_type]]

  # Create panel based on effect type
  if (effect_type == "age") {
    p <- ggplot(effect_df, aes(x = age)) +
      geom_ribbon(aes(ymin = ci_lower_centered, ymax = ci_upper_centered),
                  fill = color, alpha = 0.2) +
      geom_line(aes(y = estimate_centered), color = color, linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
      scale_x_continuous(limits = age_xlim, breaks = seq(20, 85, by = 10)) +
      scale_y_continuous(limits = age_ylim) +
      labs(x = "Age", y = "Effect (rel. to 18)")

  } else if (effect_type == "period") {
    p <- ggplot(effect_df, aes(x = period, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
      geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                    width = 1.5, color = color, linewidth = 0.5) +
      geom_point(color = color, size = 2) +
      scale_x_continuous(limits = period_xlim,
                         breaks = seq(1970, 2025, by = 10)) +
      scale_y_continuous(limits = period_ylim) +
      labs(x = "Period", y = "Random effect")

  } else {  # cohort
    p <- ggplot(effect_df, aes(x = cohort, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
      geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                    width = 2, color = color, linewidth = 0.4, alpha = 0.7) +
      geom_point(color = color, size = 1.5) +
      scale_x_continuous(limits = cohort_xlim,
                         breaks = seq(1880, 2010, by = 20)) +
      scale_y_continuous(limits = cohort_ylim) +
      labs(x = "Birth Cohort", y = "Random effect")
  }

  # Add title for first row
  if (show_title) {
    titles <- c(age = "Age Effect", period = "Period Effect", cohort = "Cohort Effect")
    p <- p + labs(title = titles[[effect_type]])
  }

  # Style the panel
  p <- p +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.25),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.3),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.margin = margin(3, 5, 3, 5)
    )

  # Add row label if requested (as tag)
  if (show_row_label) {
    p <- p +
      labs(tag = survey_label) +
      theme(
        plot.tag = element_text(size = 11, face = "bold", angle = 90, vjust = 0.5),
        plot.tag.position = c(-0.08, 0.5)
      )
  }

  p
}

#' Create row of 3 APC panels for one survey
#' @param survey Survey name
#' @param survey_label Survey label
#' @param show_titles Show column titles (first row only)
#' @return patchwork object
create_survey_row <- function(survey, survey_label, show_titles = FALSE) {
  effects <- effects_list[[survey]]

  p_age <- create_apc_panel(
    if (!is.null(effects)) effects$age else NULL,
    "age", survey_label,
    show_title = show_titles,
    show_row_label = TRUE
  )

  p_period <- create_apc_panel(
    if (!is.null(effects)) effects$period else NULL,
    "period", survey_label,
    show_title = show_titles,
    show_row_label = FALSE
  )

  p_cohort <- create_apc_panel(
    if (!is.null(effects)) effects$cohort else NULL,
    "cohort", survey_label,
    show_title = show_titles,
    show_row_label = FALSE
  )

  p_age | p_period | p_cohort
}

# Create all survey rows
message("\nCreating survey rows for Figure 3...\n")
row_plots <- mapply(
  create_survey_row,
  SURVEY_ORDER,
  SURVEY_LABELS,
  c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),  # Only first row gets titles
  SIMPLIFY = FALSE
)

# Combine all rows
figure3_combined <- (
  row_plots[[1]] /
  row_plots[[2]] /
  row_plots[[3]] /
  row_plots[[4]] /
  row_plots[[5]] /
  row_plots[[6]]
) +
  plot_annotation(
    title = "Age, Period, and Cohort Effects on Self-Rated Health",
    subtitle = "BHAPC model estimates with 90% credible intervals (axes aligned across surveys)",
    caption = "*GSS uses 4-point SRH scale (1-4); other surveys use 5-point scale (1-5). Age effect centered at age 18.",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 0.5, face = "italic", color = "gray40"),
      plot.margin = margin(10, 10, 10, 30)  # Extra left margin for row labels
    )
  )

# Save
ggsave(
  file.path(onepager_dir, paste0("onepager_figure3_apc_", date_suffix, ".png")),
  figure3_combined, width = 15, height = 20, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_figure3_apc.png"),
  figure3_combined, width = 15, height = 20, dpi = 300
)
ggsave(
  file.path(onepager_dir, "onepager_figure3_apc.pdf"),
  figure3_combined, width = 15, height = 20
)
message("Saved: onepager_figure3_apc.png/pdf")


# ==============================================================================
# PART 8: SAVE COMBINED EFFECT SUMMARIES TO CSV
# ==============================================================================

message("\n========== Saving Combined Effect Summaries ==========\n")

# Combine all age effects
all_age_effects <- bind_rows(
  lapply(names(effects_list), function(s) {
    if (!is.null(effects_list[[s]])) {
      effects_list[[s]]$age %>%
        mutate(survey = s)
    } else {
      NULL
    }
  })
)

# Combine all period effects
all_period_effects <- bind_rows(
  lapply(names(effects_list), function(s) {
    if (!is.null(effects_list[[s]])) {
      effects_list[[s]]$period %>%
        mutate(survey = s)
    } else {
      NULL
    }
  })
)

# Combine all cohort effects
all_cohort_effects <- bind_rows(
  lapply(names(effects_list), function(s) {
    if (!is.null(effects_list[[s]])) {
      effects_list[[s]]$cohort %>%
        mutate(survey = s)
    } else {
      NULL
    }
  })
)

# Save combined files
write_csv(all_age_effects, file.path(onepager_dir, "all_surveys_age_effects.csv"))
write_csv(all_period_effects, file.path(onepager_dir, "all_surveys_period_effects.csv"))
write_csv(all_cohort_effects, file.path(onepager_dir, "all_surveys_cohort_effects.csv"))

message("Saved combined effect files:")
message("  all_surveys_age_effects.csv")
message("  all_surveys_period_effects.csv")
message("  all_surveys_cohort_effects.csv")

# Create a summary table of key statistics
summary_stats <- bind_rows(
  lapply(names(effects_list), function(s) {
    if (!is.null(effects_list[[s]])) {
      age_eff <- effects_list[[s]]$age
      period_eff <- effects_list[[s]]$period
      cohort_eff <- effects_list[[s]]$cohort

      tibble(
        survey = s,
        age_range = paste0(min(age_eff$age), "-", max(age_eff$age)),
        age_effect_at_80 = age_eff %>%
          filter(age == max(age[age <= 80])) %>%
          pull(estimate_centered) %>%
          round(3),
        n_periods = nrow(period_eff),
        period_range = paste0(min(period_eff$period), "-", max(period_eff$period)),
        period_var_range = paste0(
          round(min(period_eff$estimate), 4), " to ",
          round(max(period_eff$estimate), 4)
        ),
        n_cohorts = nrow(cohort_eff),
        cohort_range = paste0(min(cohort_eff$cohort), "-", max(cohort_eff$cohort)),
        cohort_var_range = paste0(
          round(min(cohort_eff$estimate), 4), " to ",
          round(max(cohort_eff$estimate), 4)
        )
      )
    } else {
      tibble(
        survey = s,
        age_range = NA_character_,
        age_effect_at_80 = NA_real_,
        n_periods = NA_integer_,
        period_range = NA_character_,
        period_var_range = NA_character_,
        n_cohorts = NA_integer_,
        cohort_range = NA_character_,
        cohort_var_range = NA_character_
      )
    }
  })
)

write_csv(summary_stats, file.path(onepager_dir, "all_surveys_apc_summary.csv"))
message("  all_surveys_apc_summary.csv")


# ==============================================================================
# PART 9: SUMMARY
# ==============================================================================

message("\n========== One-Pager Generation Complete ==========\n")

# List generated files
generated_files <- list.files(onepager_dir, pattern = "\\.png$|\\.pdf$")
message("Generated files in ", onepager_dir, ":")
for (f in generated_files) {
  message("  ", f)
}

# Summary of data availability
message("\nData availability by survey:")
for (i in seq_along(SURVEY_ORDER)) {
  s <- SURVEY_ORDER[i]
  label <- SURVEY_LABELS[i]
  has_data <- survey_output_exists(s, "table2")
  status <- ifelse(has_data, "Complete", "In progress")
  message("  ", label, ": ", status)
}

message("\n========== Done ==========\n")
