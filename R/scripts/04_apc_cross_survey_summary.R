# ==============================================================================
# 04_apc_cross_survey_summary.R
# Cross-Survey APC Summary and Visualization
# Author: Christine Lucille Kuryla
#
# Purpose: Load APC results from all surveys and create combined summary
# outputs including tables, figures, and a 1-pager with Lexis diagrams.
#
# Outputs:
#   - output/apc/tables/apc_summary_table.csv
#   - output/apc/tables/apc_summary_table.tex
#   - output/apc/figures/fig_apc_panel_a_interaction.png
#   - output/apc/figures/fig_apc_panel_b_variance.png
#   - output/apc/figures/fig_apc_combined.png
#   - output/apc/reports/apc_1pager_with_lexis.png
#   - output/apc/reports/supplement_apc_text.md
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)
library(gt)
library(scales)

source(here::here("R", "paths.R"))
source(here::here("R", "srh_common_functions.R"))

# ==============================================================================
# Configuration
# ==============================================================================

# Survey display order (for tables and figures)
SURVEY_DISPLAY_ORDER <- c("brfss", "nhis", "meps", "nhanes", "gss", "cps")
SURVEY_LABELS <- c(
  brfss = "BRFSS",
  nhis = "NHIS",
  meps = "MEPS",
  nhanes = "NHANES",
  gss = "GSS",
  cps = "CPS"
)

# APC colors
APC_COLORS <- c(
  "Age" = "#0072B2",      # blue
  "Period" = "#009E73",   # bluish green
  "Cohort" = "#CC79A7"    # reddish purple
)

# Output directories
OUTPUT_DIR <- here::here("output", "apc")
FIGURES_DIR <- file.path(OUTPUT_DIR, "figures")
TABLES_DIR <- file.path(OUTPUT_DIR, "tables")
REPORTS_DIR <- file.path(OUTPUT_DIR, "reports")

# Ensure directories exist
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORTS_DIR, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# Data Loading Functions
# ==============================================================================

#' Load APC results for a single survey
#'
#' @param survey_name Character: survey name (lowercase)
#' @return List with full results, or NULL if not found
load_survey_results <- function(survey_name) {
  file_path <- file.path(OUTPUT_DIR, paste0(survey_name, "_results_full.rds"))

  if (!file.exists(file_path)) {
    warning("Results not found for ", toupper(survey_name), ": ", file_path)
    return(NULL)
  }

  readRDS(file_path)
}


#' Load all APC results
#'
#' @param surveys Character vector of survey names to load
#' @return Named list of results
load_all_apc_results <- function(surveys = SURVEY_DISPLAY_ORDER) {
  results <- list()

  for (survey in surveys) {
    result <- load_survey_results(survey)
    if (!is.null(result)) {
      results[[survey]] <- result
    }
  }

  message("Loaded results for ", length(results), " surveys: ",
          paste(names(results), collapse = ", "))

  results
}


# ==============================================================================
# Summary Table Functions
# ==============================================================================

#' Extract summary statistics from results
#'
#' @param results Named list of survey results
#' @param outcome Character: "continuous" or "binary"
#' @return Tibble with summary statistics
extract_summary_stats <- function(results, outcome = "continuous") {
  stats_list <- list()

  for (survey in names(results)) {
    res <- results[[survey]]

    if (is.null(res)) next

    # Extract outcome-specific results
    outcome_res <- res[[outcome]]

    # Interaction test results
    int_test <- outcome_res$interaction$interaction_test
    if (!is.null(int_test)) {
      int_f <- int_test$F_statistic
      int_p <- int_test$p_value
    } else {
      int_f <- NA
      int_p <- NA
    }

    # HAPC variance decomposition
    vc <- outcome_res$hapc$variance_components
    if (!is.null(vc)) {
      period_var <- vc$variance[vc$component == "Period"]
      cohort_var <- vc$variance[vc$component == "Cohort"]
      period_pct <- vc$pct_of_total[vc$component == "Period"]
      cohort_pct <- vc$pct_of_total[vc$component == "Cohort"]
    } else {
      period_var <- cohort_var <- period_pct <- cohort_pct <- NA
    }

    # Model convergence
    ap_conv <- outcome_res$ap$converged %||% FALSE
    ac_conv <- outcome_res$ac$converged %||% FALSE
    int_conv <- outcome_res$interaction$converged %||% FALSE
    hapc_conv <- outcome_res$hapc$converged %||% FALSE

    stats_list[[survey]] <- tibble(
      survey = survey,
      survey_label = SURVEY_LABELS[survey],
      n_obs = res$n_obs,
      hapc_n = res$hapc_n,
      year_min = res$year_range[1],
      year_max = res$year_range[2],
      srh_scale = res$srh_scale,
      interaction_f = int_f,
      interaction_p = int_p,
      period_variance = period_var,
      cohort_variance = cohort_var,
      period_pct = period_pct,
      cohort_pct = cohort_pct,
      ap_converged = ap_conv,
      ac_converged = ac_conv,
      interaction_converged = int_conv,
      hapc_converged = hapc_conv
    )
  }

  bind_rows(stats_list) %>%
    mutate(survey = factor(survey, levels = SURVEY_DISPLAY_ORDER)) %>%
    arrange(survey)
}


#' Create summary table for publication
#'
#' @param results Named list of survey results
#' @return Tibble formatted for publication
create_summary_table <- function(results) {
  # Get stats for both outcomes
  continuous_stats <- extract_summary_stats(results, "continuous") %>%
    mutate(outcome = "continuous")

  binary_stats <- extract_summary_stats(results, "binary") %>%
    mutate(outcome = "binary")

  # Combine into wide format for table
  summary_wide <- continuous_stats %>%
    select(
      survey_label,
      n_obs,
      year_min, year_max,
      srh_scale,
      cont_int_f = interaction_f,
      cont_int_p = interaction_p,
      cont_period_pct = period_pct,
      cont_cohort_pct = cohort_pct
    ) %>%
    left_join(
      binary_stats %>%
        select(
          survey_label,
          bin_int_f = interaction_f,
          bin_int_p = interaction_p,
          bin_period_pct = period_pct,
          bin_cohort_pct = cohort_pct
        ),
      by = "survey_label"
    ) %>%
    mutate(
      # Format sample size
      n_formatted = format(n_obs, big.mark = ","),
      years = paste0(year_min, "-", year_max),
      # Format interaction test
      cont_int_str = case_when(
        is.na(cont_int_f) ~ "—",
        TRUE ~ paste0("F=", round(cont_int_f, 1), ", p", format_pvalue(cont_int_p))
      ),
      bin_int_str = case_when(
        is.na(bin_int_f) ~ "—",
        TRUE ~ paste0("F=", round(bin_int_f, 1), ", p", format_pvalue(bin_int_p))
      ),
      # Format variance percentages
      cont_var_str = case_when(
        is.na(cont_period_pct) ~ "—",
        TRUE ~ paste0(round(cont_period_pct, 1), "% / ", round(cont_cohort_pct, 1), "%")
      ),
      bin_var_str = case_when(
        is.na(bin_period_pct) ~ "—",
        TRUE ~ paste0(round(bin_period_pct, 1), "% / ", round(bin_cohort_pct, 1), "%")
      )
    )

  summary_wide
}


#' Format p-value for display
#'
#' @param p Numeric p-value
#' @return Character string
format_pvalue <- function(p) {
  case_when(
    is.na(p) ~ "—",
    p < 0.001 ~ "<.001",
    p < 0.01 ~ paste0("=", format(round(p, 3), nsmall = 3)),
    TRUE ~ paste0("=", format(round(p, 2), nsmall = 2))
  )
}


#' Save summary table in multiple formats
#'
#' @param summary_table Tibble with summary data
save_summary_table <- function(summary_table) {
  # CSV version (full data)
  csv_path <- file.path(TABLES_DIR, "apc_summary_table.csv")
  write_csv(summary_table, csv_path)
  message("Saved: ", csv_path)

  # Formatted table for display
  display_table <- summary_table %>%
    select(
      Survey = survey_label,
      N = n_formatted,
      Years = years,
      Scale = srh_scale,
      `Interaction (Cont.)` = cont_int_str,
      `Period/Cohort % (Cont.)` = cont_var_str,
      `Interaction (Binary)` = bin_int_str,
      `Period/Cohort % (Binary)` = bin_var_str
    )

  # LaTeX version
  tex_path <- file.path(TABLES_DIR, "apc_summary_table.tex")

  # Create simple LaTeX table
  tex_content <- create_latex_table(display_table)
  writeLines(tex_content, tex_path)
  message("Saved: ", tex_path)
}


#' Create LaTeX table from tibble
#'
#' @param df Data frame
#' @return Character string with LaTeX table
create_latex_table <- function(df) {
  # Header
  header <- "\\begin{table}[htbp]
\\centering
\\caption{Age-Period-Cohort Analysis Summary Across Six US Surveys}
\\label{tab:apc_summary}
\\begin{tabular}{lrrlllll}
\\toprule"

  # Column names
  col_names <- paste(names(df), collapse = " & ")
  col_line <- paste0(col_names, " \\\\")

  # Data rows
  data_rows <- apply(df, 1, function(row) {
    paste(row, collapse = " & ")
  })
  data_lines <- paste0(data_rows, " \\\\")

  # Footer
  footer <- "\\bottomrule
\\end{tabular}
\\begin{tablenotes}
\\small
\\item Note: Interaction test is for Age $\\times$ Period interaction. Period/Cohort \\% shows percentage of random effect variance from HAPC model.
\\end{tablenotes}
\\end{table}"

  paste(c(header, col_line, "\\midrule", data_lines, footer), collapse = "\n")
}


# ==============================================================================
# Visualization Functions
# ==============================================================================

#' Create forest plot of interaction test statistics
#'
#' Panel A: Shows F-statistics for Age x Period interaction
#'
#' @param results Named list of survey results
#' @return ggplot object
plot_interaction_forest <- function(results) {
  # Extract interaction test results for both outcomes
  plot_data <- bind_rows(
    extract_summary_stats(results, "continuous") %>%
      mutate(outcome = "Continuous SRH"),
    extract_summary_stats(results, "binary") %>%
      mutate(outcome = "Fair/Poor (Binary)")
  ) %>%
    filter(!is.na(interaction_f)) %>%
    mutate(
      survey_label = factor(survey_label, levels = rev(SURVEY_LABELS)),
      sig = case_when(
        interaction_p < 0.001 ~ "***",
        interaction_p < 0.01 ~ "**",
        interaction_p < 0.05 ~ "*",
        TRUE ~ ""
      )
    )

  p <- ggplot(plot_data, aes(x = interaction_f, y = survey_label, fill = outcome)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(
      aes(label = sig),
      position = position_dodge(width = 0.7),
      hjust = -0.3,
      size = 4
    ) +
    scale_fill_manual(
      values = c("Continuous SRH" = APC_COLORS["Period"],
                 "Fair/Poor (Binary)" = APC_COLORS["Cohort"]),
      name = "Outcome"
    ) +
    labs(
      title = "A. Age × Period Interaction Test",
      subtitle = "F-statistics from survey-weighted models",
      x = "F-statistic",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40")
    )

  p
}


#' Create variance decomposition plot
#'
#' Panel B: Shows period vs cohort variance from HAPC
#'
#' @param results Named list of survey results
#' @return ggplot object
plot_variance_decomposition <- function(results) {
  # Extract variance decomposition for both outcomes
  plot_data <- bind_rows(
    extract_summary_stats(results, "continuous") %>%
      mutate(outcome = "Continuous SRH") %>%
      select(survey_label, outcome, Period = period_pct, Cohort = cohort_pct),
    extract_summary_stats(results, "binary") %>%
      mutate(outcome = "Fair/Poor (Binary)") %>%
      select(survey_label, outcome, Period = period_pct, Cohort = cohort_pct)
  ) %>%
    pivot_longer(
      cols = c(Period, Cohort),
      names_to = "component",
      values_to = "pct"
    ) %>%
    filter(!is.na(pct)) %>%
    mutate(
      survey_label = factor(survey_label, levels = rev(SURVEY_LABELS)),
      component = factor(component, levels = c("Period", "Cohort"))
    )

  p <- ggplot(plot_data, aes(x = pct, y = survey_label, fill = component)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    facet_wrap(~outcome, scales = "free_x") +
    scale_fill_manual(
      values = c("Period" = APC_COLORS["Period"],
                 "Cohort" = APC_COLORS["Cohort"]),
      name = "Component"
    ) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "B. Variance Decomposition (HAPC Model)",
      subtitle = "Percentage of random effect variance",
      x = "% of Variance",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      strip.text = element_text(face = "bold")
    )

  p
}


#' Create combined APC figure
#'
#' @param results Named list of survey results
#' @return ggplot object (combined with patchwork)
create_combined_apc_figure <- function(results) {
  p_interaction <- plot_interaction_forest(results)
  p_variance <- plot_variance_decomposition(results)

  combined <- p_interaction / p_variance +
    plot_layout(heights = c(1, 1.2)) +
    plot_annotation(
      title = "Age-Period-Cohort Analysis Across Six US Surveys",
      subtitle = "Period effects dominate cohort effects in explaining SRH convergence",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40")
      )
    )

  combined
}


#' Save all figures
#'
#' @param results Named list of survey results
save_all_figures <- function(results) {
  # Panel A: Interaction forest plot
  p_int <- plot_interaction_forest(results)
  ggsave(
    file.path(FIGURES_DIR, "fig_apc_panel_a_interaction.png"),
    p_int,
    width = 8, height = 6, dpi = 300
  )
  message("Saved: fig_apc_panel_a_interaction.png")

  # Panel B: Variance decomposition
  p_var <- plot_variance_decomposition(results)
  ggsave(
    file.path(FIGURES_DIR, "fig_apc_panel_b_variance.png"),
    p_var,
    width = 10, height = 6, dpi = 300
  )
  message("Saved: fig_apc_panel_b_variance.png")

  # Combined figure
  p_combined <- create_combined_apc_figure(results)
  ggsave(
    file.path(FIGURES_DIR, "fig_apc_combined.png"),
    p_combined,
    width = 10, height = 10, dpi = 300
  )
  message("Saved: fig_apc_combined.png")

  # Also save PDF version
  ggsave(
    file.path(FIGURES_DIR, "fig_apc_combined.pdf"),
    p_combined,
    width = 10, height = 10
  )
  message("Saved: fig_apc_combined.pdf")
}


# ==============================================================================
# Report Generation Functions
# ==============================================================================

#' Generate supplement text summarizing APC results
#'
#' @param results Named list of survey results
#' @return Character string with markdown text
generate_supplement_text <- function(results) {
  stats_cont <- extract_summary_stats(results, "continuous")
  stats_bin <- extract_summary_stats(results, "binary")

  # Count significant interactions
  n_sig_cont <- sum(stats_cont$interaction_p < 0.05, na.rm = TRUE)
  n_sig_bin <- sum(stats_bin$interaction_p < 0.05, na.rm = TRUE)
  n_surveys <- nrow(stats_cont)

  # Average variance percentages
  avg_period_cont <- mean(stats_cont$period_pct, na.rm = TRUE)
  avg_cohort_cont <- mean(stats_cont$cohort_pct, na.rm = TRUE)
  avg_period_bin <- mean(stats_bin$period_pct, na.rm = TRUE)
  avg_cohort_bin <- mean(stats_bin$cohort_pct, na.rm = TRUE)

  text <- paste0(
    "## Age-Period-Cohort Analysis Results\n\n",

    "### Methods\n\n",
    "We conducted age-period-cohort (APC) analyses across six nationally representative US surveys ",
    "(", paste(stats_cont$survey_label, collapse = ", "), ") ",
    "to test whether the observed convergence in self-rated health (SRH) across age groups ",
    "reflects period effects (historical changes affecting all ages) or cohort effects ",
    "(generational differences). ",
    "For each survey, we fit four models:\n\n",
    "1. **Age + Period model**: Main effects of age group and survey year\n",
    "2. **Age + Cohort model**: Main effects of age group and birth cohort\n",
    "3. **Age × Period interaction model**: Tests whether age effects change over time\n",
    "4. **Hierarchical APC (HAPC) model**: Crossed random effects for period and cohort\n\n",
    "Models 1-3 used survey weights where available. The HAPC model was fit without weights ",
    "using restricted maximum likelihood estimation with crossed random effects. ",
    "For large surveys (BRFSS, NHIS, CPS), we used stratified subsampling (5-10%) for the HAPC model.\n\n",

    "### Results\n\n",
    "**Age × Period Interaction**: ",
    "The interaction term was statistically significant (p < 0.05) in ",
    n_sig_cont, " of ", n_surveys, " surveys for continuous SRH and ",
    n_sig_bin, " of ", n_surveys, " surveys for the Fair/Poor binary outcome. ",
    "This indicates that the age gradient in SRH has changed over time in most surveys.\n\n",

    "**Variance Decomposition**: ",
    "In the HAPC models, period random effects consistently explained more variance than cohort effects. ",
    "For continuous SRH, period effects explained an average of ",
    round(avg_period_cont, 1), "% of the random effect variance compared to ",
    round(avg_cohort_cont, 1), "% for cohort effects. ",
    "For the binary outcome, the pattern was similar (",
    round(avg_period_bin, 1), "% period vs. ",
    round(avg_cohort_bin, 1), "% cohort).\n\n",

    "### Interpretation\n\n",
    "These results support the interpretation that SRH convergence reflects period effects—",
    "historical changes that have affected how people at different ages rate their health—",
    "rather than cohort effects that would indicate persistent generational differences. ",
    "This is consistent with the hypothesis that changing health norms and expectations, ",
    "rather than true generational differences in health, underlie the observed convergence.\n\n",

    "---\n\n",
    "*Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "*\n"
  )

  text
}


#' Save supplement text
#'
#' @param results Named list of survey results
save_supplement_text <- function(results) {
  text <- generate_supplement_text(results)
  path <- file.path(REPORTS_DIR, "supplement_apc_text.md")
  writeLines(text, path)
  message("Saved: ", path)
}


#' Create combined 1-pager with Lexis diagrams
#'
#' Combines APC summary figures with existing Lexis diagrams
#'
#' @param results Named list of survey results
create_combined_1pager <- function(results) {
  # Check if Lexis diagrams exist
  lexis_files <- list.files(
    here::here("output", "figures"),
    pattern = "fig2_lexis.*\\.png$",
    full.names = TRUE
  )

  if (length(lexis_files) == 0) {
    message("No Lexis diagram files found. Creating 1-pager without Lexis.")
    # Just save the combined APC figure as the 1-pager
    p_combined <- create_combined_apc_figure(results)
    ggsave(
      file.path(REPORTS_DIR, "apc_1pager_with_lexis.png"),
      p_combined,
      width = 10, height = 10, dpi = 300
    )
    return(invisible(NULL))
  }

  # Use most recent Lexis file
  lexis_file <- lexis_files[length(lexis_files)]
  message("Using Lexis diagram: ", basename(lexis_file))

  # Load Lexis image
  # Note: This requires the magick package for image manipulation
  if (!requireNamespace("magick", quietly = TRUE)) {
    message("magick package not available. Saving APC figure only.")
    p_combined <- create_combined_apc_figure(results)
    ggsave(
      file.path(REPORTS_DIR, "apc_1pager_with_lexis.png"),
      p_combined,
      width = 10, height = 10, dpi = 300
    )
    return(invisible(NULL))
  }

  library(magick)

  # Create APC figure and save temporarily
  p_combined <- create_combined_apc_figure(results)
  temp_apc <- tempfile(fileext = ".png")
  ggsave(temp_apc, p_combined, width = 10, height = 10, dpi = 300)

  # Load both images
  img_apc <- image_read(temp_apc)
  img_lexis <- image_read(lexis_file)

  # Scale Lexis to match width
  img_lexis <- image_scale(img_lexis, image_info(img_apc)$width)

  # Stack vertically
  combined <- image_append(c(img_lexis, img_apc), stack = TRUE)

  # Save
  output_path <- file.path(REPORTS_DIR, "apc_1pager_with_lexis.png")
  image_write(combined, output_path)
  message("Saved: ", output_path)

  # Clean up temp file
  unlink(temp_apc)

  invisible(NULL)
}


# ==============================================================================
# Main Execution
# ==============================================================================

run_cross_survey_summary <- function() {
  message("\n", strrep("=", 70))
  message("CROSS-SURVEY APC SUMMARY")
  message("Starting at: ", Sys.time())
  message(strrep("=", 70))

  # Load all results
  results <- load_all_apc_results()

  if (length(results) == 0) {
    stop("No APC results found. Run 03_apc_analysis.R first.")
  }

  # Create and save summary table
  message("\n--- Creating summary table ---")
  summary_table <- create_summary_table(results)
  save_summary_table(summary_table)

  # Create and save figures
  message("\n--- Creating figures ---")
  save_all_figures(results)

  # Generate supplement text
  message("\n--- Generating supplement text ---")
  save_supplement_text(results)

  # Create combined 1-pager
  message("\n--- Creating combined 1-pager ---")
  create_combined_1pager(results)

  message("\n", strrep("=", 70))
  message("SUMMARY COMPLETE")
  message("Finished at: ", Sys.time())
  message(strrep("=", 70))

  invisible(results)
}

# Uncomment to run:
# results <- run_cross_survey_summary()
