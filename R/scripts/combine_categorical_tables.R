# ==============================================================================
# combine_categorical_tables.R
# Combine per-survey categorical SRH tables into unified all-survey tables
#
# Input:  Individual survey tables from output/tables/supp_srh_*.rds
# Output: Combined tables with 'survey' column
#   - output/tables/supp_srh_props_all.rds
#   - output/tables/supp_srh_age_comp_all.rds
#   - output/tables/supp_srh_spread_all.rds
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(dplyr)
library(purrr)
library(here)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Survey order (same as fig_1_combined)
survey_order <- c("BRFSS", "MEPS", "NHIS", "GSS", "CPS", "NHANES")

# File patterns - map survey names to their file prefixes
survey_file_map <- list(
  BRFSS  = "brfss",
  MEPS   = "meps",
  NHIS   = "nhis",
  GSS    = "gss",

  CPS    = "cps",
  NHANES = "nhanes"
)

# Age group order (scheme B)
age_group_order <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

# SRH category order
srh_cat_order_5pt <- c("Poor", "Fair", "Good", "Very Good", "Excellent")
srh_cat_order_4pt <- c("Poor", "Fair", "Good", "Excellent")

# ------------------------------------------------------------------------------
# Helper: Find latest file matching pattern
# ------------------------------------------------------------------------------

find_latest_file <- function(table_type, survey_prefix, tables_dir = here::here("output", "tables")) {
  pattern <- sprintf("supp_srh_%s_%s_\\d{8}\\.rds$", table_type, survey_prefix)
  files <- list.files(tables_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    warning(sprintf("No files found for pattern: %s", pattern))
    return(NULL)
  }

  # Return most recent (sort by name descending)
  files <- sort(files, decreasing = TRUE)
  files[1]
}

# ------------------------------------------------------------------------------
# Load and combine tables
# ------------------------------------------------------------------------------

combine_tables <- function(table_type) {

  cat(sprintf("\n=== Combining %s tables ===\n", table_type))

  tables <- list()

  for (survey in survey_order) {
    prefix <- survey_file_map[[survey]]
    filepath <- find_latest_file(table_type, prefix)

    if (is.null(filepath)) {
      warning(sprintf("Skipping %s - no file found", survey))
      next
    }

    cat(sprintf("Loading: %s\n", basename(filepath)))

    tbl <- readRDS(filepath)
    tbl$survey <- survey
    tables[[survey]] <- tbl
  }

  if (length(tables) == 0) {
    stop(sprintf("No tables loaded for type: %s", table_type))
  }

  # Combine all tables
  combined <- bind_rows(tables)

  # Set factor levels for consistent ordering
  combined$survey <- factor(combined$survey, levels = survey_order)

  if ("age_group" %in% names(combined)) {
    combined$age_group <- factor(combined$age_group, levels = age_group_order)
  }

  if ("srh_cat" %in% names(combined)) {
    # Use 5-point order, GSS categories will naturally subset
    combined$srh_cat <- factor(combined$srh_cat, levels = srh_cat_order_5pt)
  }

  cat(sprintf("Combined: %d rows from %d surveys\n", nrow(combined), length(tables)))

  return(combined)
}

# ------------------------------------------------------------------------------
# Main: Combine all three table types
# ------------------------------------------------------------------------------

main <- function() {

  cat("\n========================================\n")
  cat("Combining categorical SRH tables\n")
  cat("========================================\n")

  # Combine props tables
  props_all <- combine_tables("props")

  # Combine age_comp tables
  age_comp_all <- combine_tables("age_comp")

  # Combine spread tables
  spread_all <- combine_tables("spread")

  # --- Save combined tables ---
  cat("\n=== Saving combined tables ===\n")

  out_dir <- here::here("output", "tables")

  saveRDS(props_all, file.path(out_dir, "supp_srh_props_all.rds"))
  cat("Saved: supp_srh_props_all.rds\n")

  saveRDS(age_comp_all, file.path(out_dir, "supp_srh_age_comp_all.rds"))
  cat("Saved: supp_srh_age_comp_all.rds\n")

  saveRDS(spread_all, file.path(out_dir, "supp_srh_spread_all.rds"))
  cat("Saved: supp_srh_spread_all.rds\n")

  # --- Print summary ---
  cat("\n=== Summary ===\n")

  cat("\nProps table:\n")
  cat(sprintf("  Surveys: %s\n", paste(levels(props_all$survey), collapse = ", ")))
  cat(sprintf("  Total rows: %d\n", nrow(props_all)))
  cat(sprintf("  Columns: %s\n", paste(names(props_all), collapse = ", ")))

  cat("\nAge composition table:\n")
  cat(sprintf("  Surveys: %s\n", paste(levels(age_comp_all$survey), collapse = ", ")))
  cat(sprintf("  Total rows: %d\n", nrow(age_comp_all)))

  cat("\nSpread table:\n")
  cat(sprintf("  Surveys: %s\n", paste(levels(spread_all$survey), collapse = ", ")))
  cat(sprintf("  Total rows: %d\n", nrow(spread_all)))

  # Check GSS has 4 categories
  gss_cats <- props_all %>%
    filter(survey == "GSS") %>%
    pull(srh_cat) %>%
    unique() %>%
    as.character() %>%
    na.omit()

  cat(sprintf("\nGSS SRH categories: %s\n", paste(gss_cats, collapse = ", ")))

  cat("\n========================================\n")
  cat("Done!\n")
  cat("========================================\n")

  invisible(list(
    props = props_all,
    age_comp = age_comp_all,
    spread = spread_all
  ))
}

# Run if called directly
if (sys.nframe() == 0) {
  main()
}
