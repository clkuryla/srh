# ==============================================================================
# 00_setup.R
# Load packages and shared functions for SRH convergence paper
# Run this at the start of each session
# ==============================================================================

# ------------------------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(srvyr)
library(here)
library(patchwork)
library(scales)

# Optional: for specific analyses
# library(broom)
# library(marginaleffects)

# ------------------------------------------------------------------------------
# PATHS
# ------------------------------------------------------------------------------

# Project root (assumes you're in the project directory)
# here::here() handles this automatically

# Key directories
data_dir <- here("data", "processed")
output_dir <- here("output", "figures")

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# SHARED FUNCTIONS
# ------------------------------------------------------------------------------

source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "plot_utils.R"))
source(here("R", "functions", "plot_lexis.R"))

# ------------------------------------------------------------------------------
# GLOBAL OPTIONS
# ------------------------------------------------------------------------------

# ggplot default theme
theme_set(theme_srh())

# Suppress summarize() messages
options(dplyr.summarise.inform = FALSE)

# Seed for reproducibility
set.seed(20260116)

# ------------------------------------------------------------------------------
# VERIFICATION
# ------------------------------------------------------------------------------

cat("Setup complete.\n")
cat("  Data directory:", data_dir, "\n")
cat("  Output directory:", output_dir, "\n")
cat("  Theme: theme_srh()\n")
cat("  Palettes: age_colors, survey_colors\n")
cat("\nShared plotting functions loaded:\n")
cat("  - plot_trends()\n")
cat("  - plot_coefficient_single()\n")
cat("  - plot_coefficient_trends()\n")
cat("  - plot_trends_by_survey()\n")
cat("  - plot_lexis_surface()\n")
cat("  - plot_lexis_by_survey()\n")
cat("  - combine_panels()\n")
cat("  - save_figure()\n")
