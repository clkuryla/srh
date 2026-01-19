# supp_srh_categorical_cps.R - CPS Categorical Distribution
library(tidyverse)
library(here)
library(srvyr)

source(here::here("R/paths.R"))
ensure_dirs()
source(here::here("R/functions/theme_srh.R"))
source(here::here("R/srh_common_functions.R"))
source(here::here("R/functions/summarize_srh_categories.R"))
source(here::here("R/functions/plot_srh_distribution.R"))

cat("Functions loaded.\n")
survey_name <- "CPS"
srh_scale <- 5

cat("Loading CPS data...\n")
data_raw <- readr::read_rds(derived_path("data_cps.rds"))
cat("  Loaded", nrow(data_raw), "rows.\n")

# CPS may not have psu/strata
data_prep <- data_raw %>%
  select(any_of(c("srh", "age", "year", "psu", "strata", "wt"))) %>%
  filter(!is.na(srh), srh >= 1, srh <= srh_scale, !is.na(age), !is.na(year), !is.na(wt), wt > 0) %>%
  add_age_group(scheme = "B", new_var = age_group)

cat("  After filtering:", nrow(data_prep), "rows.\n")
cat("  Years:", min(data_prep$year), "-", max(data_prep$year), "\n")

cat("\nComputing proportions...\n")
psu_var <- if("psu" %in% names(data_prep)) "psu" else NULL
strata_var <- if("strata" %in% names(data_prep)) "strata" else NULL

props_by_age <- summarize_srh_proportions(data_prep, survey_name, psu_var = psu_var, strata_var = strata_var, srh_scale = srh_scale)
props_by_srh <- summarize_age_composition_by_srh(data_prep, survey_name, psu_var = psu_var, strata_var = strata_var, srh_scale = srh_scale)
spread_stats <- summarize_srh_spread(data_prep, survey_name, psu_var = psu_var, strata_var = strata_var, srh_scale = srh_scale)
cat("  Done.\n")

cat("\nGenerating figures...\n")
fig1 <- plot_srh_by_age_group(props_by_age, survey_name, srh_scale, ncol = 7)
fig2a <- plot_srh_age_composition(props_by_srh, survey_name, ncol = srh_scale)
fig2b <- plot_srh_category_by_age(props_by_age, survey_name, ncol = srh_scale)
fig3 <- plot_srh_stacked_area(props_by_age, survey_name, srh_scale, ncol = 7)
fig6_var <- plot_srh_spread(spread_stats, survey_name, "variance")
fig6_ent <- plot_srh_spread(spread_stats, survey_name, "entropy")

cat("\nSaving...\n")
fig_dir <- here::here("output", "figures")
survey_lower <- tolower(survey_name)

ggsave(file.path(fig_dir, paste0("supp_srh_by_age_", survey_lower, ".png")), fig1, width = 16, height = 4, dpi = 300)
ggsave(file.path(fig_dir, paste0("supp_srh_by_age_", survey_lower, ".pdf")), fig1, width = 16, height = 4)
ggsave(file.path(fig_dir, paste0("supp_srh_age_comp_", survey_lower, ".png")), fig2a, width = 14, height = 6, dpi = 300)
ggsave(file.path(fig_dir, paste0("supp_srh_age_comp_", survey_lower, ".pdf")), fig2a, width = 14, height = 6)
ggsave(file.path(fig_dir, paste0("supp_srh_cat_by_age_", survey_lower, ".png")), fig2b, width = 14, height = 6, dpi = 300)
ggsave(file.path(fig_dir, paste0("supp_srh_cat_by_age_", survey_lower, ".pdf")), fig2b, width = 14, height = 6)
ggsave(file.path(fig_dir, paste0("supp_srh_stacked_", survey_lower, ".png")), fig3, width = 16, height = 4, dpi = 300)
ggsave(file.path(fig_dir, paste0("supp_srh_stacked_", survey_lower, ".pdf")), fig3, width = 16, height = 4)
ggsave(file.path(fig_dir, paste0("supp_srh_variance_", survey_lower, ".png")), fig6_var, width = 10, height = 6, dpi = 300)
ggsave(file.path(fig_dir, paste0("supp_srh_variance_", survey_lower, ".pdf")), fig6_var, width = 10, height = 6)
ggsave(file.path(fig_dir, paste0("supp_srh_entropy_", survey_lower, ".png")), fig6_ent, width = 10, height = 6, dpi = 300)
ggsave(file.path(fig_dir, paste0("supp_srh_entropy_", survey_lower, ".pdf")), fig6_ent, width = 10, height = 6)

cat("\n=== Done:", survey_name, "===\n")
