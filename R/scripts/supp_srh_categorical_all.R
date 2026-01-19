# ==============================================================================
# supp_srh_categorical_all.R
# Generate Supplemental Figures: SRH Categorical Distribution for ALL Surveys
#
# Approaches implemented:
#   1. Facet by Age Group - Line plot of SRH category proportions over time
#   2a. Facet by SRH Category (Age Composition) - Who reports each rating
#   2b. Facet by SRH Category (Prevalence) - How each age group's prevalence changes
#   3. Stacked Area - Compositional view of SRH distribution
#   6. Variance/Entropy - Spread of SRH distribution over time
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(srvyr)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/functions/theme_srh.R"))
source(here::here("R/srh_common_functions.R"))
source(here::here("R/functions/summarize_srh_categories.R"))
source(here::here("R/functions/plot_srh_distribution.R"))

cat("Functions loaded.\n\n")

# ------------------------------------------------------------------------------
# Survey configuration
# ------------------------------------------------------------------------------

surveys <- list(
  MEPS = list(
    file = "data_meps.rds",
    srh_scale = 5,
    psu = "psu",
    strata = "strata",
    wt = "wt"
  ),
  NHIS = list(
    file = "data_nhis.rds",
    srh_scale = 5,
    psu = "psu",
    strata = "strata",
    wt = "wt"
  ),
  BRFSS = list(
    file = "data_brfss.rds",
    srh_scale = 5,
    psu = "psu",
    strata = "strata",
    wt = "wt"
  ),
  NHANES = list(
    file = "data_nhanes.rds",
    srh_scale = 5,
    psu = "psu",
    strata = "strata",
    wt = "wt"
  ),
  CPS = list(
    file = "data_cps.rds",
    srh_scale = 5,
    psu = NULL,  # CPS may not have complex design
    strata = NULL,
    wt = "wt"
  ),
  GSS = list(
    file = "data_gss.rds",
    srh_scale = 4,  # GSS has 4-point scale
    psu = NULL,
    strata = NULL,
    wt = "wt"
  )
)

# ------------------------------------------------------------------------------
# Function to process a single survey
# ------------------------------------------------------------------------------

process_survey <- function(survey_name, config, fig_dir, table_dir, draft_date) {

  cat("=== Processing", survey_name, "===\n")

  # Load data
  data_path <- derived_path(config$file)
  if (!file.exists(data_path)) {
    cat("  WARNING: Data file not found:", data_path, "\n")
    cat("  Skipping", survey_name, "\n\n")
    return(NULL)
  }

  cat("  Loading data...\n")
  data_raw <- readr::read_rds(data_path)
  cat("    Loaded", nrow(data_raw), "rows.\n")

  # Check required columns
  required_cols <- c("srh", "age", "year")
  if (!is.null(config$wt)) required_cols <- c(required_cols, config$wt)

  missing_cols <- setdiff(required_cols, names(data_raw))
  if (length(missing_cols) > 0) {
    cat("  WARNING: Missing columns:", paste(missing_cols, collapse = ", "), "\n")
    cat("  Skipping", survey_name, "\n\n")
    return(NULL)
  }

  # Prepare data
  data_prep <- data_raw %>%
    select(any_of(c("srh", "age", "year", config$psu, config$strata, config$wt))) %>%
    filter(
      !is.na(srh), srh >= 1, srh <= config$srh_scale,
      !is.na(age),
      !is.na(year)
    )

  # Handle weights
  if (!is.null(config$wt) && config$wt %in% names(data_prep)) {
    data_prep <- data_prep %>%
      filter(!is.na(.data[[config$wt]]), .data[[config$wt]] > 0)
  }

  # Add age groups
  data_prep <- data_prep %>%
    add_age_group(scheme = "B", new_var = age_group)

  cat("    After filtering:", nrow(data_prep), "rows.\n")
  cat("    Years:", min(data_prep$year), "-", max(data_prep$year), "\n")

  if (nrow(data_prep) < 100) {
    cat("  WARNING: Too few observations after filtering.\n")
    cat("  Skipping", survey_name, "\n\n")
    return(NULL)
  }

  # Compute proportions
  cat("  Computing proportions...\n")

  props_by_age <- tryCatch({
    summarize_srh_proportions(
      data = data_prep,
      survey_name = survey_name,
      psu_var = config$psu,
      strata_var = config$strata,
      wt_var = config$wt,
      srh_scale = config$srh_scale
    )
  }, error = function(e) {
    cat("    ERROR in proportions:", conditionMessage(e), "\n")
    return(NULL)
  })

  if (is.null(props_by_age)) {
    cat("  Skipping", survey_name, "due to errors.\n\n")
    return(NULL)
  }

  props_by_srh <- tryCatch({
    summarize_age_composition_by_srh(
      data = data_prep,
      survey_name = survey_name,
      psu_var = config$psu,
      strata_var = config$strata,
      wt_var = config$wt,
      srh_scale = config$srh_scale
    )
  }, error = function(e) {
    cat("    ERROR in age composition:", conditionMessage(e), "\n")
    return(NULL)
  })

  spread_stats <- tryCatch({
    summarize_srh_spread(
      data = data_prep,
      survey_name = survey_name,
      psu_var = config$psu,
      strata_var = config$strata,
      wt_var = config$wt,
      srh_scale = config$srh_scale
    )
  }, error = function(e) {
    cat("    ERROR in spread stats:", conditionMessage(e), "\n")
    return(NULL)
  })

  cat("    Proportions computed.\n")

  # Generate figures
  cat("  Generating figures...\n")

  survey_lower <- tolower(survey_name)

  # Approach 1
  fig1 <- plot_srh_by_age_group(props_by_age, survey_name, config$srh_scale, ncol = 7)

  # Approach 2a (only if props_by_srh exists)
  fig2a <- if (!is.null(props_by_srh)) {
    plot_srh_age_composition(props_by_srh, survey_name, ncol = config$srh_scale)
  } else NULL

  # Approach 2b
  fig2b <- plot_srh_category_by_age(props_by_age, survey_name, ncol = config$srh_scale)

  # Approach 3
  fig3 <- plot_srh_stacked_area(props_by_age, survey_name, config$srh_scale, ncol = 7)

  # Approach 6
  fig6_var <- if (!is.null(spread_stats)) {
    plot_srh_spread(spread_stats, survey_name, "variance")
  } else NULL

  fig6_ent <- if (!is.null(spread_stats)) {
    plot_srh_spread(spread_stats, survey_name, "entropy")
  } else NULL

  cat("    Figures generated.\n")

  # Save tables
  cat("  Saving outputs...\n")

  readr::write_rds(props_by_age, file.path(table_dir, paste0("supp_srh_props_", survey_lower, "_", draft_date, ".rds")))
  if (!is.null(props_by_srh)) {
    readr::write_rds(props_by_srh, file.path(table_dir, paste0("supp_srh_age_comp_", survey_lower, "_", draft_date, ".rds")))
  }
  if (!is.null(spread_stats)) {
    readr::write_rds(spread_stats, file.path(table_dir, paste0("supp_srh_spread_", survey_lower, "_", draft_date, ".rds")))
  }

  # Save figures
  ggsave(file.path(fig_dir, paste0("supp_srh_by_age_", survey_lower, ".png")),
         fig1, width = 16, height = 4, dpi = 300)
  ggsave(file.path(fig_dir, paste0("supp_srh_by_age_", survey_lower, ".pdf")),
         fig1, width = 16, height = 4)

  if (!is.null(fig2a)) {
    ggsave(file.path(fig_dir, paste0("supp_srh_age_comp_", survey_lower, ".png")),
           fig2a, width = 14, height = 6, dpi = 300)
    ggsave(file.path(fig_dir, paste0("supp_srh_age_comp_", survey_lower, ".pdf")),
           fig2a, width = 14, height = 6)
  }

  ggsave(file.path(fig_dir, paste0("supp_srh_cat_by_age_", survey_lower, ".png")),
         fig2b, width = 14, height = 6, dpi = 300)
  ggsave(file.path(fig_dir, paste0("supp_srh_cat_by_age_", survey_lower, ".pdf")),
         fig2b, width = 14, height = 6)

  ggsave(file.path(fig_dir, paste0("supp_srh_stacked_", survey_lower, ".png")),
         fig3, width = 16, height = 4, dpi = 300)
  ggsave(file.path(fig_dir, paste0("supp_srh_stacked_", survey_lower, ".pdf")),
         fig3, width = 16, height = 4)

  if (!is.null(fig6_var)) {
    ggsave(file.path(fig_dir, paste0("supp_srh_variance_", survey_lower, ".png")),
           fig6_var, width = 10, height = 6, dpi = 300)
    ggsave(file.path(fig_dir, paste0("supp_srh_variance_", survey_lower, ".pdf")),
           fig6_var, width = 10, height = 6)
  }

  if (!is.null(fig6_ent)) {
    ggsave(file.path(fig_dir, paste0("supp_srh_entropy_", survey_lower, ".png")),
           fig6_ent, width = 10, height = 6, dpi = 300)
    ggsave(file.path(fig_dir, paste0("supp_srh_entropy_", survey_lower, ".pdf")),
           fig6_ent, width = 10, height = 6)
  }

  cat("    Saved to", fig_dir, "\n\n")

  return(list(
    props_by_age = props_by_age,
    props_by_srh = props_by_srh,
    spread_stats = spread_stats
  ))
}

# ------------------------------------------------------------------------------
# Process all surveys
# ------------------------------------------------------------------------------

fig_dir <- here::here("output", "figures")
table_dir <- here::here("output", "tables")
draft_date <- format(Sys.Date(), "%Y%m%d")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

results <- list()

for (survey_name in names(surveys)) {
  results[[survey_name]] <- process_survey(
    survey_name = survey_name,
    config = surveys[[survey_name]],
    fig_dir = fig_dir,
    table_dir = table_dir,
    draft_date = draft_date
  )
}

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

cat("\n=== Summary ===\n")
for (survey_name in names(surveys)) {
  if (!is.null(results[[survey_name]])) {
    cat(survey_name, ": SUCCESS\n")
  } else {
    cat(survey_name, ": SKIPPED (missing data or errors)\n")
  }
}

cat("\nFigures saved to:", fig_dir, "\n")
cat("Tables saved to:", table_dir, "\n")
