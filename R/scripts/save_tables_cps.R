# Quick script to save CPS computed tables
library(tidyverse)
library(here)
library(srvyr)

source(here::here("R/paths.R"))
source(here::here("R/functions/theme_srh.R"))
source(here::here("R/srh_common_functions.R"))
source(here::here("R/functions/summarize_srh_categories.R"))

data_raw <- readr::read_rds(derived_path("data_cps.rds"))
data_prep <- data_raw %>%
  select(any_of(c("srh", "age", "year", "psu", "strata", "wt"))) %>%
  filter(!is.na(srh), srh >= 1, srh <= 5, !is.na(age), !is.na(year), !is.na(wt), wt > 0) %>%
  add_age_group(scheme = "B", new_var = age_group)

props_by_age <- summarize_srh_proportions(data_prep, "CPS", psu_var = NULL, strata_var = NULL, srh_scale = 5)
props_by_srh <- summarize_age_composition_by_srh(data_prep, "CPS", psu_var = NULL, strata_var = NULL, srh_scale = 5)
spread_stats <- summarize_srh_spread(data_prep, "CPS", psu_var = NULL, strata_var = NULL, srh_scale = 5)

draft_date <- format(Sys.Date(), "%Y%m%d")
table_dir <- here::here("output", "tables")
readr::write_rds(props_by_age, file.path(table_dir, paste0("supp_srh_props_cps_", draft_date, ".rds")))
readr::write_rds(props_by_srh, file.path(table_dir, paste0("supp_srh_age_comp_cps_", draft_date, ".rds")))
readr::write_rds(spread_stats, file.path(table_dir, paste0("supp_srh_spread_cps_", draft_date, ".rds")))
cat("CPS tables saved.\n")
