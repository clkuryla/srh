# ==============================================================================
# supp_srh_categorical_meps.R
# Generate Supplemental Figures: SRH Categorical Distribution (MEPS)
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

cat("Functions loaded.\n")

# ------------------------------------------------------------------------------
# Load MEPS Data
# ------------------------------------------------------------------------------

cat("Loading MEPS data...\n")

data_meps <- readr::read_rds(derived_path("data_meps.rds"))

cat("  Loaded", nrow(data_meps), "rows.\n")

# Prepare data: select required variables and add age groups
data_meps_prep <- data_meps %>%
  select(srh, age, year, psu, strata, wt) %>%
  filter(
    !is.na(srh), srh >= 1, srh <= 5,
    !is.na(age),
    !is.na(year),
    !is.na(wt), wt > 0
  ) %>%
  add_age_group(scheme = "B", new_var = age_group)

cat("  After filtering:", nrow(data_meps_prep), "rows.\n")
cat("  Years:", min(data_meps_prep$year), "-", max(data_meps_prep$year), "\n")
cat("  Age groups:", paste(levels(data_meps_prep$age_group), collapse = ", "), "\n")

# ------------------------------------------------------------------------------
# Compute Survey-Weighted Proportions
# ------------------------------------------------------------------------------

cat("\nComputing survey-weighted proportions...\n")

# Approach 1, 2b, 3: SRH proportions within age group x year
props_by_age <- summarize_srh_proportions(
  data = data_meps_prep,
  survey_name = "MEPS",
  srh_scale = 5
)
cat("  Approach 1/2b/3 proportions:", nrow(props_by_age), "rows.\n")

# Approach 2a: Age group proportions within SRH category x year
props_by_srh <- summarize_age_composition_by_srh(
  data = data_meps_prep,
  survey_name = "MEPS",
  srh_scale = 5
)
cat("  Approach 2a proportions:", nrow(props_by_srh), "rows.\n")

# Approach 6: Variance/entropy of SRH distribution
spread_stats <- summarize_srh_spread(
  data = data_meps_prep,
  survey_name = "MEPS",
  srh_scale = 5
)
cat("  Approach 6 spread stats:", nrow(spread_stats), "rows.\n")

# ------------------------------------------------------------------------------
# Generate Figures
# ------------------------------------------------------------------------------

cat("\nGenerating figures...\n")

# --- Approach 1: Facet by Age Group ---
fig_approach1 <- plot_srh_by_age_group(
  data = props_by_age,
  survey_name = "MEPS",
  srh_scale = 5,
  base_size = 11,
  ncol = 7  # One row for all 7 age groups
)
cat("  Generated Approach 1 figure.\n")

# --- Approach 2a: Facet by SRH Category (Age Composition) ---
fig_approach2a <- plot_srh_age_composition(
  data = props_by_srh,
  survey_name = "MEPS",
  base_size = 11,
  ncol = 5
)
cat("  Generated Approach 2a figure.\n")

# --- Approach 2b: Facet by SRH Category (Age-Specific Prevalence) ---
fig_approach2b <- plot_srh_category_by_age(
  data = props_by_age,
  survey_name = "MEPS",
  base_size = 11,
  ncol = 5
)
cat("  Generated Approach 2b figure.\n")

# --- Approach 3: Stacked Area ---
fig_approach3 <- plot_srh_stacked_area(
  data = props_by_age,
  survey_name = "MEPS",
  srh_scale = 5,
  base_size = 11,
  ncol = 7  # One row for all 7 age groups
)
cat("  Generated Approach 3 figure.\n")

# --- Approach 6: Variance over time ---
fig_approach6_var <- plot_srh_spread(
  data = spread_stats,
  survey_name = "MEPS",
  metric = "variance",
  base_size = 11
)
cat("  Generated Approach 6 (variance) figure.\n")

# --- Approach 6: Entropy over time ---
fig_approach6_ent <- plot_srh_spread(
  data = spread_stats,
  survey_name = "MEPS",
  metric = "entropy",
  base_size = 11
)
cat("  Generated Approach 6 (entropy) figure.\n")

# ------------------------------------------------------------------------------
# Save Outputs
# ------------------------------------------------------------------------------

cat("\nSaving outputs...\n")

fig_dir <- here::here("output", "figures")
table_dir <- here::here("output", "tables")
draft_date <- format(Sys.Date(), "%Y%m%d")

# Ensure directories exist
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# --- Save computed proportions ---
readr::write_rds(props_by_age, file.path(table_dir, paste0("supp_srh_props_meps_", draft_date, ".rds")))
readr::write_csv(props_by_age, file.path(table_dir, paste0("supp_srh_props_meps_", draft_date, ".csv")))

readr::write_rds(props_by_srh, file.path(table_dir, paste0("supp_srh_age_comp_meps_", draft_date, ".rds")))
readr::write_csv(props_by_srh, file.path(table_dir, paste0("supp_srh_age_comp_meps_", draft_date, ".csv")))

readr::write_rds(spread_stats, file.path(table_dir, paste0("supp_srh_spread_meps_", draft_date, ".rds")))
readr::write_csv(spread_stats, file.path(table_dir, paste0("supp_srh_spread_meps_", draft_date, ".csv")))

cat("  Saved proportion tables.\n")

# --- Save figures (draft with date) ---
ggsave(
  filename = file.path(fig_dir, paste0("supp_srh_by_age_meps_", draft_date, ".png")),
  plot = fig_approach1,
  width = 16, height = 4, dpi = 300  # Wide single row
)

ggsave(
  filename = file.path(fig_dir, paste0("supp_srh_age_comp_meps_", draft_date, ".png")),
  plot = fig_approach2a,
  width = 14, height = 6, dpi = 300
)

ggsave(
  filename = file.path(fig_dir, paste0("supp_srh_cat_by_age_meps_", draft_date, ".png")),
  plot = fig_approach2b,
  width = 14, height = 6, dpi = 300
)

ggsave(
  filename = file.path(fig_dir, paste0("supp_srh_stacked_meps_", draft_date, ".png")),
  plot = fig_approach3,
  width = 16, height = 4, dpi = 300  # Wide single row
)

ggsave(
  filename = file.path(fig_dir, paste0("supp_srh_variance_meps_", draft_date, ".png")),
  plot = fig_approach6_var,
  width = 10, height = 6, dpi = 300
)

ggsave(
  filename = file.path(fig_dir, paste0("supp_srh_entropy_meps_", draft_date, ".png")),
  plot = fig_approach6_ent,
  width = 10, height = 6, dpi = 300
)

cat("  Saved draft figures.\n")

# --- Save final versions (no date) ---
ggsave(
  filename = file.path(fig_dir, "supp_srh_by_age_meps.png"),
  plot = fig_approach1,
  width = 16, height = 4, dpi = 300  # Wide single row
)
ggsave(
  filename = file.path(fig_dir, "supp_srh_by_age_meps.pdf"),
  plot = fig_approach1,
  width = 16, height = 4  # Wide single row
)

ggsave(
  filename = file.path(fig_dir, "supp_srh_age_comp_meps.png"),
  plot = fig_approach2a,
  width = 14, height = 6, dpi = 300
)
ggsave(
  filename = file.path(fig_dir, "supp_srh_age_comp_meps.pdf"),
  plot = fig_approach2a,
  width = 14, height = 6
)

ggsave(
  filename = file.path(fig_dir, "supp_srh_cat_by_age_meps.png"),
  plot = fig_approach2b,
  width = 14, height = 6, dpi = 300
)
ggsave(
  filename = file.path(fig_dir, "supp_srh_cat_by_age_meps.pdf"),
  plot = fig_approach2b,
  width = 14, height = 6
)

ggsave(
  filename = file.path(fig_dir, "supp_srh_stacked_meps.png"),
  plot = fig_approach3,
  width = 16, height = 4, dpi = 300  # Wide single row
)
ggsave(
  filename = file.path(fig_dir, "supp_srh_stacked_meps.pdf"),
  plot = fig_approach3,
  width = 16, height = 4  # Wide single row
)

ggsave(
  filename = file.path(fig_dir, "supp_srh_variance_meps.png"),
  plot = fig_approach6_var,
  width = 10, height = 6, dpi = 300
)
ggsave(
  filename = file.path(fig_dir, "supp_srh_variance_meps.pdf"),
  plot = fig_approach6_var,
  width = 10, height = 6
)

ggsave(
  filename = file.path(fig_dir, "supp_srh_entropy_meps.png"),
  plot = fig_approach6_ent,
  width = 10, height = 6, dpi = 300
)
ggsave(
  filename = file.path(fig_dir, "supp_srh_entropy_meps.pdf"),
  plot = fig_approach6_ent,
  width = 10, height = 6
)

cat("  Saved final figures.\n")

# ------------------------------------------------------------------------------
# Verification Summary
# ------------------------------------------------------------------------------

cat("\n=== Verification Summary ===\n")

# Check proportions sum to ~100% within groups
check_sums_age <- props_by_age %>%
  group_by(age_group, year) %>%
  summarise(total = sum(prop), .groups = "drop")

cat("Approach 1 proportions (should all be ~1.0):\n")
cat("  Min:", round(min(check_sums_age$total), 4),
    " Max:", round(max(check_sums_age$total), 4), "\n")

check_sums_srh <- props_by_srh %>%
  group_by(srh_cat, year) %>%
  summarise(total = sum(prop), .groups = "drop")

cat("Approach 2a proportions (should all be ~1.0):\n")
cat("  Min:", round(min(check_sums_srh$total), 4),
    " Max:", round(max(check_sums_srh$total), 4), "\n")

cat("\n=== Done ===\n")
cat("Figures saved to:", fig_dir, "\n")
cat("Tables saved to:", table_dir, "\n")
