# Wrangle MEPS data
# https://meps.ipums.org/meps/

# 1. Download MEPS data and desired variables from IPUMS into a designated folder
# 2. Unzip it
# 3. Run this script

library(tidyverse)
library(here)
source(here::here("R/paths.R")) # To access path for data as depot_path
ensure_dirs()

data_meps_raw <- readr::read_csv(
  depot_path("surveys", "MEPS", "ipums_extracts", "meps_00001.csv"),
  show_col_types = FALSE
)

data_meps <- data_meps_raw %>%
  # WRANGLE HERE

readr::write_rds(data_meps, derived_path("meps_analytic.rds"))
