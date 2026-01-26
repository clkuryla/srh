# ==============================================================================
# 06_nhis_functional_limitations_combined.R
# NHIS Functional Limitations: Combined 5-Column Figures
#
# Creates combined figures with all functional limitation variables:
#   - Prevalence version (% with any difficulty)
#   - Mean ordinal version (scale mean)
#
# Requires: Run 06_nhis_functional_limitations_viz.R first to generate tables
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)

source(here("R", "paths.R"))
source(here("R", "functions", "theme_srh.R"))

# Output directory
output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")

# Date suffix for finding input files
date_suffix <- format(Sys.Date(), "%Y%m%d")

# Age group levels
AGE_GROUPS <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

# Year filter - start from 2010
MIN_YEAR <- 2010


# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

message("\n========== Loading prevalence and mean tables ==========\n")

# Find most recent files if today's don't exist
find_latest_file <- function(pattern, dir = tables_dir) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No files found matching pattern: ", pattern)
  }
  # Return most recently modified
  files[which.max(file.mtime(files))]
}

# Load prevalence tables
prev1_file <- find_latest_file("nhis_limitations_part1_prev_.*\\.csv")
prev2_file <- find_latest_file("nhis_limitations_part2_prev_.*\\.csv")
prev3_file <- find_latest_file("nhis_limitations_part3_prev_.*\\.csv")

message("Loading prevalence files:")
message("  ", basename(prev1_file))
message("  ", basename(prev2_file))
message("  ", basename(prev3_file))

prev1 <- read_csv(prev1_file, show_col_types = FALSE)
prev2 <- read_csv(prev2_file, show_col_types = FALSE)
prev3 <- read_csv(prev3_file, show_col_types = FALSE)

# Load mean tables
mean1_file <- find_latest_file("nhis_limitations_part1_mean_.*\\.csv")
mean2_file <- find_latest_file("nhis_limitations_part2_mean_.*\\.csv")
mean3_file <- find_latest_file("nhis_limitations_part3_mean_.*\\.csv")

message("\nLoading mean files:")
message("  ", basename(mean1_file))
message("  ", basename(mean2_file))
message("  ", basename(mean3_file))

mean1 <- read_csv(mean1_file, show_col_types = FALSE)
mean2 <- read_csv(mean2_file, show_col_types = FALSE)
mean3 <- read_csv(mean3_file, show_col_types = FALSE)


# ------------------------------------------------------------------------------
# COMBINE AND FILTER DATA
# ------------------------------------------------------------------------------

message("\n========== Combining and filtering data ==========\n")

# Combine prevalence data
prev_all <- bind_rows(prev1, prev2, prev3) %>%
  filter(year >= MIN_YEAR) %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    variable_label = factor(variable_label, levels = unique(variable_label))
  )

# Combine mean data
mean_all <- bind_rows(mean1, mean2, mean3) %>%
  filter(year >= MIN_YEAR) %>%
  mutate(
    age_group = factor(age_group, levels = AGE_GROUPS),
    variable_label = factor(variable_label, levels = unique(variable_label))
  )

message("Prevalence data: ", nrow(prev_all), " estimates, ",
        length(unique(prev_all$variable_label)), " variables")
message("Mean data: ", nrow(mean_all), " estimates, ",
        length(unique(mean_all$variable_label)), " variables")
message("Year range: ", MIN_YEAR, "-", max(prev_all$year))


# ------------------------------------------------------------------------------
# CREATE COMBINED PREVALENCE FIGURE
# ------------------------------------------------------------------------------

message("\n========== Creating combined prevalence figure ==========\n")

fig_prev_combined <- ggplot(prev_all, aes(x = year, y = mean,
                                           color = age_group, group = age_group)) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 5, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    title = "Prevalence of Functional Limitations by Age Group Over Time (NHIS)",
    subtitle = "Survey-weighted estimates | Binary: % with any difficulty | 2010-2024",
    x = "Year",
    y = "Prevalence (% with any difficulty)",
    caption = paste0("Source: NHIS | Generated: ", Sys.Date())
  ) +
  theme_srh(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7)
  ) +
  guides(color = guide_legend(nrow = 1))


# ------------------------------------------------------------------------------
# CREATE COMBINED MEAN FIGURE
# ------------------------------------------------------------------------------

message("\n========== Creating combined mean figure ==========\n")

fig_mean_combined <- ggplot(mean_all, aes(x = year, y = mean,
                                           color = age_group, group = age_group)) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.9) +
  facet_wrap(~ variable_label, ncol = 5, scales = "free_y") +
  scale_color_manual(values = age_colors_oi, name = "Age Group") +
  scale_x_continuous(breaks = seq(2010, 2024, by = 4)) +
  labs(
    title = "Mean Functional Limitation Severity by Age Group Over Time (NHIS)",
    subtitle = "Survey-weighted estimates | Ordinal scale mean | 2010-2024",
    x = "Year",
    y = "Mean Value (ordinal scale)",
    caption = paste0("Source: NHIS | Generated: ", Sys.Date())
  ) +
  theme_srh(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7)
  ) +
  guides(color = guide_legend(nrow = 1))


# ------------------------------------------------------------------------------
# SAVE FIGURES
# ------------------------------------------------------------------------------

message("\n========== Saving figures ==========\n")

# Prevalence combined
ggsave(file.path(output_dir, "nhis_limitations_combined.png"),
       fig_prev_combined, width = 14, height = 9, dpi = 300)
ggsave(file.path(output_dir, "nhis_limitations_combined.pdf"),
       fig_prev_combined, width = 14, height = 9)
message("Saved: nhis_limitations_combined.png and .pdf (prevalence)")

# Mean combined
ggsave(file.path(output_dir, "nhis_limitations_combined_mean.png"),
       fig_mean_combined, width = 14, height = 9, dpi = 300)
ggsave(file.path(output_dir, "nhis_limitations_combined_mean.pdf"),
       fig_mean_combined, width = 14, height = 9)
message("Saved: nhis_limitations_combined_mean.png and .pdf (mean)")


# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

message("\n========== Summary ==========\n")

message("Combined figures created with ", length(unique(prev_all$variable_label)),
        " variables in 5-column layout")
message("Year range: ", MIN_YEAR, "-", max(prev_all$year))
message("\nOutput files:")
message("  - nhis_limitations_combined.png/.pdf (prevalence)")
message("  - nhis_limitations_combined_mean.png/.pdf (mean ordinal)")

message("\nOkabe-Ito color palette:")
message("  18-29: #D55E00 (vermillion)")
message("  30-39: #E69F00 (orange)")
message("  40-49: #F0E442 (yellow)")
message("  50-59: #009E73 (bluish green)")
message("  60-69: #56B4E9 (sky blue)")
message("  70-79: #0072B2 (blue)")
message("  80-89: #CC79A7 (reddish purple)")

message("\n========== Done ==========\n")
