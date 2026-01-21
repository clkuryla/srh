# Fetch NHANES data directly from CDC using nhanesA package
#
# This script downloads DEMO (demographics) and HUQ (health questionnaire)
# tables for all NHANES continuous cycles from 1999-2000 through 2021-2023.
#
# Data is cached as RDS files to avoid re-downloading.
#
# Author: Christine Lucille Kuryla
# Created: 2026-01-18

library(nhanesA)
library(tidyverse)
library(here)
source(here::here("R/paths.R"))

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Output directory for cached downloads
nhanes_direct_dir <- file.path(data_root(), "surveys", "NHANES_direct")

# Ensure output directory exists
if (!dir.exists(nhanes_direct_dir)) {
  dir.create(nhanes_direct_dir, recursive = TRUE)
}

# Define NHANES cycles to fetch
# Format: list(table_name, years_label, year_midpoint)
cycles <- tribble(
  ~demo_table, ~huq_table, ~years_label, ~year_midpoint,
  "DEMO",      "HUQ",      "1999-2000",  1999.5,
  "DEMO_B",    "HUQ_B",    "2001-2002",  2001.5,
  "DEMO_C",    "HUQ_C",    "2003-2004",  2003.5,
  "DEMO_D",    "HUQ_D",    "2005-2006",  2005.5,
  "DEMO_E",    "HUQ_E",    "2007-2008",  2007.5,
  "DEMO_F",    "HUQ_F",    "2009-2010",  2009.5,
  "DEMO_G",    "HUQ_G",    "2011-2012",  2011.5,
  "DEMO_H",    "HUQ_H",    "2013-2014",  2013.5,
  "DEMO_I",    "HUQ_I",    "2015-2016",  2015.5,
  "DEMO_J",    "HUQ_J",    "2017-2018",  2017.5,
  "P_DEMO",    "P_HUQ",    "2017-2020",  2018.5,  # Pre-pandemic
  "DEMO_L",    "HUQ_L",    "2021-2023",  2022.0   # Post-pandemic
)

# ------------------------------------------------------------------------------
# Helper function to fetch and cache a single table
# ------------------------------------------------------------------------------

fetch_and_cache <- function(table_name, years_label, force_refresh = FALSE) {

  # Construct output filename
  out_file <- file.path(nhanes_direct_dir, paste0(table_name, "_", years_label, ".rds"))

  # Check if already cached

  if (file.exists(out_file) && !force_refresh) {
    message("  Using cached: ", basename(out_file))
    return(readRDS(out_file))
  }

  # Download from CDC
  message("  Downloading: ", table_name, " (", years_label, ")...")

  tryCatch({
    data <- nhanes(table_name)

    # Save to cache
    saveRDS(data, out_file)
    message("  Saved: ", basename(out_file), " (", nrow(data), " rows)")

    return(data)

  }, error = function(e) {
    warning("  Failed to download ", table_name, ": ", e$message)
    return(NULL)
  })
}

# ------------------------------------------------------------------------------
# Main: Fetch all cycles
# ------------------------------------------------------------------------------

fetch_all_nhanes <- function(force_refresh = FALSE) {

  message("Fetching NHANES data from CDC via nhanesA package...")
  message("Output directory: ", nhanes_direct_dir)
  message("")

  results <- list()

  for (i in seq_len(nrow(cycles))) {
    cycle <- cycles[i, ]

    message("Cycle ", i, "/", nrow(cycles), ": ", cycle$years_label)

    # Fetch DEMO
    demo <- fetch_and_cache(cycle$demo_table, cycle$years_label, force_refresh)

    # Fetch HUQ
    huq <- fetch_and_cache(cycle$huq_table, cycle$years_label, force_refresh)

    results[[cycle$years_label]] <- list(
      demo = demo,
      huq = huq,
      year_midpoint = cycle$year_midpoint
    )

    message("")
  }

  message("Done! All NHANES data cached in: ", nhanes_direct_dir)

  invisible(results)
}

# ------------------------------------------------------------------------------
# Run if executed directly (not sourced)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  fetch_all_nhanes(force_refresh = FALSE)
}
