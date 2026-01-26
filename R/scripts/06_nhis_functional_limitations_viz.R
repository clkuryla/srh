# ==============================================================================
# 06_nhis_functional_limitations_viz.R
# NHIS Functional Limitation Variables: Prevalence Visualization
#
# Purpose:
#   Recreate nhis_limitations_part1/2/3.png figures with Okabe-Ito color palette
#   for colorblind accessibility.
#
# Variables by Part:
#   Part 1: General Limitations
#     - lawashdresdif: Bathing/Dressing Difficulty
#     - lacomdifego: Communication Difficulty
#     - lamemcondif: Memory/Concentration Difficulty
#     - lawalkclimdif: Walking/Climbing Difficulty
#     - lamtwrk: Work Limitation
#
#   Part 2: Physical Limitations (Washington Group questions)
#     - walkdif12st1: Climb 12 Steps Difficulty
#     - lahanddif: Hand/Finger Difficulty
#     - lara2litrdif: Lift/Carry 25lbs Difficulty
#     - walkdif1bl1: Walk 1 Block Difficulty
#     - walkdif5bl1: Walk 5 Blocks Difficulty
#
#   Part 3: Memory & Other Limitations
#     - diserrandp: Difficulty Doing Errands
#     - lamemorcon: Memory Condition
#     - lamemdifoft: Memory Problems (Often+)
#     - lamemdifamt: Memory Problems (Some/A lot)
#     - walkdif1bl2: Walk 1 Block (alternate)
#     - lawalkclimper: Walk/Climb Period Limitation
#     - lawalkclimperq: Walk/Climb Permanent Limitation
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
source(here("R", "functions", "theme_srh.R"))
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
# VARIABLE DEFINITIONS BY PART
# ==============================================================================

# ----- BINARY PREVALENCE VERSIONS (% with any difficulty) -----

# Part 1: General Limitations (binary)
part1_vars_prev <- c(
  "lawashdresdif_prev" = "Bathing/Dressing Difficulty",
  "lacomdifego_prev"   = "Communication Difficulty",
  "lamemcondif_prev"   = "Memory/Concentration Difficulty",
  "lawalkclimdif_prev" = "Walking/Climbing Difficulty",
  "lamtwrk_prev"       = "Work Limitation"
)

# Part 2: Physical Limitations (binary)
part2_vars_prev <- c(
  "walkdif12st1_prev" = "Climb 12 Steps Difficulty",
  "lahanddif_prev"    = "Hand/Finger Difficulty",
  "lara2litrdif_prev" = "Lift/Carry 25lbs Difficulty",
  "walkdif1bl1_prev"  = "Walk 1 Block Difficulty",
  "walkdif5bl1_prev"  = "Walk 5 Blocks Difficulty"
)

# Part 3: Memory & Other Limitations (binary)
part3_vars_prev <- c(
  "diserrandp_prev"     = "Difficulty Doing Errands",
  "lamemorcon_prev"     = "Memory Condition",
  "lamemdifoft_prev"    = "Memory Problems (Often+)",
  "lamemdifamt_prev"    = "Memory Problems (Some/A lot)",
  "walkdif1bl2_prev"    = "Walk 1 Block (alternate)",
  "lawalkclimper_prev"  = "Walk/Climb Period Limitation",
  "lawalkclimperq_prev" = "Walk/Climb Permanent Limitation"
)

# ----- MEAN ORDINAL VERSIONS (mean of ordinal scale) -----

# Part 1: General Limitations (ordinal mean)
part1_vars_mean <- c(
  "lawashdresdif" = "Bathing/Dressing Difficulty",
  "lacomdifego"   = "Communication Difficulty",
  "lamemcondif"   = "Memory/Concentration Difficulty",
  "lawalkclimdif" = "Walking/Climbing Difficulty",
  "lamtwrk"       = "Work Limitation"
)

# Part 2: Physical Limitations (ordinal mean)
part2_vars_mean <- c(
  "walkdif12st1" = "Climb 12 Steps Difficulty",
  "lahanddif"    = "Hand/Finger Difficulty",
  "lara2litrdif" = "Lift/Carry 25lbs Difficulty",
  "walkdif1bl1"  = "Walk 1 Block Difficulty",
  "walkdif5bl1"  = "Walk 5 Blocks Difficulty"
)

# Part 3: Memory & Other Limitations (ordinal mean)
part3_vars_mean <- c(
  "diserrandp"     = "Difficulty Doing Errands",
  "lamemorcon"     = "Memory Condition",
  "lamemdifoft"    = "Memory Problems (Often+)",
  "lamemdifamt"    = "Memory Problems (Some/A lot)",
  "walkdif1bl2"    = "Walk 1 Block (alternate)",
  "lawalkclimper"  = "Walk/Climb Period Limitation",
  "lawalkclimperq" = "Walk/Climb Permanent Limitation"
)


# ==============================================================================
# LOAD DATA
# ==============================================================================

message("\n========== Loading NHIS data ==========\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

message("NHIS: ", nrow(data_nhis), " rows, years ",
        min(data_nhis$year), "-", max(data_nhis$year))

# Add age group if not present
if (!"age_group" %in% names(data_nhis)) {
  source(here("R", "srh_common_functions.R"))
  data_nhis <- add_age_group(data_nhis, age_var = age, scheme = "B")
}

# Ensure age_group is a factor with correct levels
data_nhis <- data_nhis %>%
  mutate(age_group = factor(age_group, levels = AGE_GROUPS))


# ==============================================================================
# CREATE BINARY VERSIONS FOR PREVALENCE
# ==============================================================================
# Most functional limitation variables are ordinal (1=No difficulty, 2+=Some difficulty)
# Convert to binary: 0 = no difficulty (value==1), 1 = any difficulty (value>1)
# Exception: lamemorcon and lawalkclimperq are already binary (0/1)

message("\n========== Creating binary versions for prevalence ==========\n")

# Variables that need conversion (ordinal where 1=No difficulty, >1=Any difficulty)
ordinal_vars <- c(
  "lawashdresdif", "lacomdifego", "lamemcondif", "lawalkclimdif",
  "walkdif12st1", "lahanddif", "lara2litrdif", "walkdif5bl1",
  "walkdif1bl1", "lamemdifamt", "walkdif1bl2", "lawalkclimper",
  "lamtwrk", "diserrandp", "lamemdifoft"
)

# Create binary versions: _prev suffix
for (v in ordinal_vars) {
  if (v %in% names(data_nhis)) {
    new_var <- paste0(v, "_prev")
    data_nhis[[new_var]] <- ifelse(data_nhis[[v]] > 1, 1,
                                    ifelse(data_nhis[[v]] == 1, 0, NA_real_))
    n_valid <- sum(!is.na(data_nhis[[new_var]]))
    if (n_valid > 0) {
      message("  Created ", new_var, ": ", n_valid, " valid obs")
    }
  }
}

# lamemorcon and lawalkclimperq are already 0/1, just copy with _prev suffix
if ("lamemorcon" %in% names(data_nhis)) {
  data_nhis$lamemorcon_prev <- data_nhis$lamemorcon
  message("  Copied lamemorcon_prev (already binary)")
}
if ("lawalkclimperq" %in% names(data_nhis)) {
  data_nhis$lawalkclimperq_prev <- data_nhis$lawalkclimperq
  message("  Copied lawalkclimperq_prev (already binary)")
}


# ==============================================================================
# HELPER FUNCTION: Compute prevalence and create plot
# ==============================================================================

#' Compute prevalence by age group and year for a set of variables
#' @param data Data frame with NHIS data
#' @param var_list Named vector: names are variable names, values are labels
#' @return Data frame with prevalence estimates
compute_prevalence <- function(data, var_list) {
  results <- list()

  for (i in seq_along(var_list)) {
    var_name <- names(var_list)[i]
    var_label <- var_list[i]

    message("\n--- Processing: ", var_name, " ---")

    # Check variable exists
    if (!var_name %in% names(data)) {
      message("  Variable not found. Skipping.")
      next
    }

    # Check year range
    valid_years <- data %>%
      filter(!is.na(.data[[var_name]])) %>%
      pull(year) %>%
      unique() %>%
      sort()

    if (length(valid_years) == 0) {
      message("  No valid data. Skipping.")
      next
    }

    message("  Years with data: ", min(valid_years), "-", max(valid_years))

    # Compute means
    result <- mean_by_age_year(
      data = data,
      var_name = var_name,
      var_label = var_label,
      survey_name = "NHIS",
      psu_var = "psu",
      strata_var = "strata",
      wt_var = "wt",
      min_n = 50
    )

    if (!is.null(result) && nrow(result) > 0) {
      results[[var_name]] <- result
    }
  }

  # Combine results
  if (length(results) > 0) {
    bind_rows(results)
  } else {
    NULL
  }
}


#' Create faceted plot with Okabe-Ito colors
#' @param prev_data Data frame from compute_prevalence()
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param y_type Either "prevalence" (0-1 shown as %) or "mean" (ordinal scale mean)
#' @return ggplot object
create_prevalence_plot <- function(prev_data, title, subtitle = NULL,
                                   y_type = c("prevalence", "mean")) {

  y_type <- match.arg(y_type)

  if (is.null(prev_data) || nrow(prev_data) == 0) {
    message("No data to plot.")
    return(NULL)
  }

  # Ensure variable_label is a factor for consistent ordering
  prev_data <- prev_data %>%
    mutate(variable_label = factor(variable_label, levels = unique(variable_label)))

  # Create plot with Okabe-Ito palette
  p <- ggplot(prev_data, aes(x = year, y = mean,
                              color = age_group, group = age_group)) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    geom_point(size = 1.5, alpha = 0.9) +
    facet_wrap(~ variable_label, scales = "free_y") +
    scale_color_manual(values = age_colors_oi, name = "Age Group") +
    scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
    theme_srh(base_size = 11) +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      strip.text = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40")
    )

  # Y-axis formatting based on type
if (y_type == "prevalence") {
    p <- p +
      scale_y_continuous(labels = scales::percent_format(scale = 100)) +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Year",
        y = "Prevalence (% with any difficulty)",
        caption = paste0("Source: NHIS | Survey-weighted estimates | Generated: ", Sys.Date())
      )
  } else {
    p <- p +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Year",
        y = "Mean Value (ordinal scale)",
        caption = paste0("Source: NHIS | Survey-weighted estimates | Generated: ", Sys.Date())
      )
  }

  return(p)
}


# ==============================================================================
# COMPUTE ESTIMATES - BINARY PREVALENCE VERSION
# ==============================================================================

message("\n========== Computing BINARY PREVALENCE (% with any difficulty) ==========\n")

message("\n--- Part 1 (Prevalence) ---")
prev_part1_bin <- compute_prevalence(data_nhis, part1_vars_prev)

message("\n--- Part 2 (Prevalence) ---")
prev_part2_bin <- compute_prevalence(data_nhis, part2_vars_prev)

message("\n--- Part 3 (Prevalence) ---")
prev_part3_bin <- compute_prevalence(data_nhis, part3_vars_prev)


# ==============================================================================
# COMPUTE ESTIMATES - MEAN ORDINAL VERSION
# ==============================================================================

message("\n========== Computing MEAN ORDINAL VALUES ==========\n")

message("\n--- Part 1 (Mean) ---")
prev_part1_mean <- compute_prevalence(data_nhis, part1_vars_mean)

message("\n--- Part 2 (Mean) ---")
prev_part2_mean <- compute_prevalence(data_nhis, part2_vars_mean)

message("\n--- Part 3 (Mean) ---")
prev_part3_mean <- compute_prevalence(data_nhis, part3_vars_mean)


# ==============================================================================
# CREATE FIGURES - BINARY PREVALENCE VERSION
# ==============================================================================

message("\n========== Creating PREVALENCE figures ==========\n")

fig_part1_prev <- create_prevalence_plot(
  prev_part1_bin,
  title = "Prevalence of Limitations by Age Group Over Time (NHIS)",
  subtitle = "Survey-weighted estimates | Binary: % with any difficulty",
  y_type = "prevalence"
)

fig_part2_prev <- create_prevalence_plot(
  prev_part2_bin,
  title = "Prevalence of Physical Limitations by Age Group Over Time (NHIS)",
  subtitle = "Survey-weighted estimates | Binary: % with any difficulty",
  y_type = "prevalence"
)

fig_part3_prev <- create_prevalence_plot(
  prev_part3_bin,
  title = "Prevalence of Memory & Other Limitations by Age Group Over Time (NHIS)",
  subtitle = "Survey-weighted estimates | Binary: % with any difficulty",
  y_type = "prevalence"
)


# ==============================================================================
# CREATE FIGURES - MEAN ORDINAL VERSION
# ==============================================================================

message("\n========== Creating MEAN figures ==========\n")

fig_part1_mean <- create_prevalence_plot(
  prev_part1_mean,
  title = "Mean Limitation Severity by Age Group Over Time (NHIS)",
  subtitle = "Survey-weighted estimates | Ordinal scale mean",
  y_type = "mean"
)

fig_part2_mean <- create_prevalence_plot(
  prev_part2_mean,
  title = "Mean Physical Limitation Severity by Age Group Over Time (NHIS)",
  subtitle = "Survey-weighted estimates | Ordinal scale mean",
  y_type = "mean"
)

fig_part3_mean <- create_prevalence_plot(
  prev_part3_mean,
  title = "Mean Memory & Other Limitation Severity by Age Group Over Time (NHIS)",
  subtitle = "Survey-weighted estimates | Ordinal scale mean",
  y_type = "mean"
)


# ==============================================================================
# SAVE FIGURES - PREVALENCE VERSION
# ==============================================================================

message("\n========== Saving PREVALENCE figures ==========\n")

if (!is.null(fig_part1_prev)) {
  ggsave(file.path(output_dir, "nhis_limitations_part1.png"),
         fig_part1_prev, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, "nhis_limitations_part1.pdf"),
         fig_part1_prev, width = 12, height = 8)
  message("Saved: nhis_limitations_part1.png and .pdf (prevalence)")
}

if (!is.null(fig_part2_prev)) {
  ggsave(file.path(output_dir, "nhis_limitations_part2.png"),
         fig_part2_prev, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, "nhis_limitations_part2.pdf"),
         fig_part2_prev, width = 12, height = 8)
  message("Saved: nhis_limitations_part2.png and .pdf (prevalence)")
}

if (!is.null(fig_part3_prev)) {
  ggsave(file.path(output_dir, "nhis_limitations_part3.png"),
         fig_part3_prev, width = 12, height = 10, dpi = 300)
  ggsave(file.path(output_dir, "nhis_limitations_part3.pdf"),
         fig_part3_prev, width = 12, height = 10)
  message("Saved: nhis_limitations_part3.png and .pdf (prevalence)")
}


# ==============================================================================
# SAVE FIGURES - MEAN VERSION
# ==============================================================================

message("\n========== Saving MEAN figures ==========\n")

if (!is.null(fig_part1_mean)) {
  ggsave(file.path(output_dir, "nhis_limitations_part1_mean.png"),
         fig_part1_mean, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, "nhis_limitations_part1_mean.pdf"),
         fig_part1_mean, width = 12, height = 8)
  message("Saved: nhis_limitations_part1_mean.png and .pdf")
}

if (!is.null(fig_part2_mean)) {
  ggsave(file.path(output_dir, "nhis_limitations_part2_mean.png"),
         fig_part2_mean, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, "nhis_limitations_part2_mean.pdf"),
         fig_part2_mean, width = 12, height = 8)
  message("Saved: nhis_limitations_part2_mean.png and .pdf")
}

if (!is.null(fig_part3_mean)) {
  ggsave(file.path(output_dir, "nhis_limitations_part3_mean.png"),
         fig_part3_mean, width = 12, height = 10, dpi = 300)
  ggsave(file.path(output_dir, "nhis_limitations_part3_mean.pdf"),
         fig_part3_mean, width = 12, height = 10)
  message("Saved: nhis_limitations_part3_mean.png and .pdf")
}


# ==============================================================================
# SAVE TABLES
# ==============================================================================

message("\n========== Saving tables ==========\n")

# Prevalence tables
if (!is.null(prev_part1_bin)) {
  readr::write_csv(prev_part1_bin,
    file.path(tables_dir, paste0("nhis_limitations_part1_prev_", date_suffix, ".csv")))
}
if (!is.null(prev_part2_bin)) {
  readr::write_csv(prev_part2_bin,
    file.path(tables_dir, paste0("nhis_limitations_part2_prev_", date_suffix, ".csv")))
}
if (!is.null(prev_part3_bin)) {
  readr::write_csv(prev_part3_bin,
    file.path(tables_dir, paste0("nhis_limitations_part3_prev_", date_suffix, ".csv")))
}

# Mean tables
if (!is.null(prev_part1_mean)) {
  readr::write_csv(prev_part1_mean,
    file.path(tables_dir, paste0("nhis_limitations_part1_mean_", date_suffix, ".csv")))
}
if (!is.null(prev_part2_mean)) {
  readr::write_csv(prev_part2_mean,
    file.path(tables_dir, paste0("nhis_limitations_part2_mean_", date_suffix, ".csv")))
}
if (!is.null(prev_part3_mean)) {
  readr::write_csv(prev_part3_mean,
    file.path(tables_dir, paste0("nhis_limitations_part3_mean_", date_suffix, ".csv")))
}

message("Saved tables with date suffix: ", date_suffix)


# ==============================================================================
# SUMMARY
# ==============================================================================

message("\n========== Summary ==========\n")

message("PREVALENCE figures (% with any difficulty):")
if (!is.null(prev_part1_bin)) {
  message("  Part 1: ", length(unique(prev_part1_bin$variable)), " variables, ",
          nrow(prev_part1_bin), " estimates")
}
if (!is.null(prev_part2_bin)) {
  message("  Part 2: ", length(unique(prev_part2_bin$variable)), " variables, ",
          nrow(prev_part2_bin), " estimates")
}
if (!is.null(prev_part3_bin)) {
  message("  Part 3: ", length(unique(prev_part3_bin$variable)), " variables, ",
          nrow(prev_part3_bin), " estimates")
}

message("\nMEAN figures (ordinal scale mean):")
if (!is.null(prev_part1_mean)) {
  message("  Part 1: ", length(unique(prev_part1_mean$variable)), " variables, ",
          nrow(prev_part1_mean), " estimates")
}
if (!is.null(prev_part2_mean)) {
  message("  Part 2: ", length(unique(prev_part2_mean$variable)), " variables, ",
          nrow(prev_part2_mean), " estimates")
}
if (!is.null(prev_part3_mean)) {
  message("  Part 3: ", length(unique(prev_part3_mean$variable)), " variables, ",
          nrow(prev_part3_mean), " estimates")
}

message("\nOkabe-Ito color palette applied:")
message("  18-29: #D55E00 (vermillion)")
message("  30-39: #E69F00 (orange)")
message("  40-49: #F0E442 (yellow)")
message("  50-59: #009E73 (bluish green)")
message("  60-69: #56B4E9 (sky blue)")
message("  70-79: #0072B2 (blue)")
message("  80-89: #CC79A7 (reddish purple)")

message("\n========== NHIS Functional Limitations Visualization Complete ==========\n")
