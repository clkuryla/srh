# Wrangle NHIS data
# https://nhis.ipums.org/nhis/

# 1. Download NHIS data and desired variables from IPUMS into a designated folder
# 2. Unzip it
# 3. Run this script

library(tidyverse)
library(here)
source(here::here("R/paths.R")) # To access path for data as depot_path
ensure_dirs()

data_nhis_raw <- readr::read_csv(
  depot_path("surveys", "NHIS", "nhis_00002.csv"),
  show_col_types = FALSE
)

data_nhis <- data_nhis_raw %>%
  # WRANGLE HERE

  
rm(data_nhis_raw)

readr::write_rds(data_nhis, derived_path("data_nhis.rds"))
