# Wrangle NHANES data from direct CDC downloads
#
# Data source: nhanesA package downloads cached in data_depot/surveys/NHANES_direct/
#
# This script processes NHANES DEMO and HUQ files to create a harmonized dataset
# matching the format expected by the SRH analysis pipeline.
#
# NHANES has a 5-point SRH scale:
#   Original (HUQ010): 1=Excellent, 2=Very Good, 3=Good, 4=Fair, 5=Poor
#   Recoded (srh):     5=Excellent, 4=Very Good, 3=Good, 2=Fair, 1=Poor (higher = better)
#
# Survey design notes:
#   - Uses MEC exam weights (WTMEC2YR for most cycles, WTMECPRP for pre-pandemic)
#   - Strata: SDMVSTRA
#   - PSU: SDMVPSU
#   - NHANES uses 2-year survey cycles
#
# Author: Christine Lucille Kuryla
# Created: 2026-01-18

library(tidyverse)
library(here)
source(here::here("R/paths.R"))
ensure_dirs()

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

nhanes_direct_dir <- file.path(data_root(), "surveys", "NHANES_direct")

# Define cycles to process
# Note: We include all available cycles; downstream analysis can filter as needed
cycles <- tribble(
  ~demo_file, ~huq_file, ~years_label, ~year_midpoint, ~weight_var,
  "DEMO_1999-2000.rds",      "HUQ_1999-2000.rds",      "1999-2000",  1999.5, "WTMEC2YR",
  "DEMO_B_2001-2002.rds",    "HUQ_B_2001-2002.rds",    "2001-2002",  2001.5, "WTMEC2YR",
  "DEMO_C_2003-2004.rds",    "HUQ_C_2003-2004.rds",    "2003-2004",  2003.5, "WTMEC2YR",
  "DEMO_D_2005-2006.rds",    "HUQ_D_2005-2006.rds",    "2005-2006",  2005.5, "WTMEC2YR",
  "DEMO_E_2007-2008.rds",    "HUQ_E_2007-2008.rds",    "2007-2008",  2007.5, "WTMEC2YR",
  "DEMO_F_2009-2010.rds",    "HUQ_F_2009-2010.rds",    "2009-2010",  2009.5, "WTMEC2YR",
  "DEMO_G_2011-2012.rds",    "HUQ_G_2011-2012.rds",    "2011-2012",  2011.5, "WTMEC2YR",
  "DEMO_H_2013-2014.rds",    "HUQ_H_2013-2014.rds",    "2013-2014",  2013.5, "WTMEC2YR",
  "DEMO_I_2015-2016.rds",    "HUQ_I_2015-2016.rds",    "2015-2016",  2015.5, "WTMEC2YR",
  "DEMO_J_2017-2018.rds",    "HUQ_J_2017-2018.rds",    "2017-2018",  2017.5, "WTMEC2YR",
  "P_DEMO_2017-2020.rds",    "P_HUQ_2017-2020.rds",    "2017-2020",  2018.5, "WTMECPRP",  # Pre-pandemic
  "DEMO_L_2021-2023.rds",    "HUQ_L_2021-2023.rds",    "2021-2023",  2022.0, "WTMEC2YR"   # Post-pandemic
)

# ------------------------------------------------------------------------------
# Process each cycle
# ------------------------------------------------------------------------------

process_cycle <- function(demo_file, huq_file, years_label, year_midpoint, weight_var) {

  message("Processing: ", years_label)

  # Load DEMO and HUQ
  demo_path <- file.path(nhanes_direct_dir, demo_file)
  huq_path <- file.path(nhanes_direct_dir, huq_file)

  if (!file.exists(demo_path) || !file.exists(huq_path)) {
    warning("  Missing files for ", years_label, ", skipping")
    return(NULL)
  }

  demo <- readRDS(demo_path)
  huq <- readRDS(huq_path)

  # Select and rename DEMO variables
  demo_vars <- c("SEQN", "RIDAGEYR", "RIAGENDR", "SDMVSTRA", "SDMVPSU")

  # Add the appropriate weight variable
  if (weight_var %in% names(demo)) {
    demo_vars <- c(demo_vars, weight_var)
  } else {
    warning("  Weight variable ", weight_var, " not found in ", demo_file)
    demo_vars <- c(demo_vars, "WTMEC2YR")  # Fallback
  }

  # Ensure all required variables exist
  missing_vars <- setdiff(demo_vars, names(demo))
  if (length(missing_vars) > 0) {
    warning("  Missing variables in DEMO: ", paste(missing_vars, collapse = ", "))
    demo_vars <- intersect(demo_vars, names(demo))
  }

  demo_select <- demo %>%
    select(all_of(demo_vars))

  # Select HUQ variables
  huq_select <- huq %>%
    select(SEQN, HUQ010)

  # Merge DEMO + HUQ by SEQN
  merged <- inner_join(demo_select, huq_select, by = "SEQN")

  # Add year midpoint
  merged$year_midpoint <- year_midpoint
  merged$years_label <- years_label
  merged$weight_var_used <- weight_var

  message("  Rows after merge: ", nrow(merged))

  return(merged)
}

# Process all cycles
all_data <- map2_dfr(
  cycles$demo_file, cycles$huq_file,
  ~ process_cycle(.x, .y,
                  cycles$years_label[cycles$demo_file == .x],
                  cycles$year_midpoint[cycles$demo_file == .x],
                  cycles$weight_var[cycles$demo_file == .x])
)

message("\nTotal rows before filtering: ", nrow(all_data))

# ------------------------------------------------------------------------------
# Harmonize and filter
# ------------------------------------------------------------------------------

# Rename weight columns to a common name
# Handle both WTMEC2YR and WTMECPRP
all_data <- all_data %>%
  mutate(
    wt = coalesce(WTMEC2YR, WTMECPRP)
  )

# Wrangle to match expected output format
data_nhanes <- all_data %>%
  # Convert HUQ010 from factor to numeric
  # Factor levels: 1=Excellent, 2=Very good, 3=Good, 4=Fair, 5=Poor, 6=Don't know
  mutate(
    huq010_num = as.numeric(HUQ010)
  ) %>%
  # Filter to adults with valid SRH
  filter(RIDAGEYR >= 18) %>%
  filter(!is.na(huq010_num)) %>%
  filter(huq010_num %in% 1:5) %>%  # Exclude "Don't know" (6) and any other invalid
  # Recode SRH so higher = better (1=Poor ... 5=Excellent)
  # Original: 1=Excellent, 2=Very good, 3=Good, 4=Fair, 5=Poor
  # Recoded: 5=Excellent, 4=Very good, 3=Good, 2=Fair, 1=Poor
  mutate(
    srh = 6L - huq010_num,
    srh_cat = factor(
      srh,
      levels = 1:5,
      labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")
    )
  ) %>%
  # Standardize variable names
  mutate(
    age = RIDAGEYR,
    year = year_midpoint,
    cohort = year - age,
    strata = SDMVSTRA,
    psu = SDMVPSU
  ) %>%
  # Sex recode
  mutate(
    sex = case_when(
      RIAGENDR == 1 ~ "Male",
      RIAGENDR == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  # Create age groups consistent with other surveys
  mutate(
    age_group = factor(
      cut(
        age,
        breaks = c(17, 29, 39, 49, 59, 69, 79, Inf),
        labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
        right = TRUE
      ),
      ordered = TRUE
    ),
    age_group_6 = factor(
      cut(
        age,
        breaks = c(17, 29, 39, 49, 59, 69, Inf),
        labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
        right = TRUE
      ),
      ordered = TRUE
    )
  ) %>%
  # Dataset identifier
  mutate(
    dataset = "NHANES"
  ) %>%
  # Select and reorder final columns
  select(
    dataset,
    SEQN,
    year,
    years_label,
    age,
    sex,
    srh,
    srh_cat,
    wt,
    strata,
    psu,
    cohort,
    age_group,
    age_group_6,
    weight_var_used
  )

message("Total rows after filtering: ", nrow(data_nhanes))

# ------------------------------------------------------------------------------
# Summary checks
# ------------------------------------------------------------------------------

message("\n=== Summary by cycle ===")
summary_by_cycle <- data_nhanes %>%
  group_by(years_label, year) %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    mean_srh = mean(srh, na.rm = TRUE),
    pct_missing_wt = mean(is.na(wt)) * 100,
    .groups = "drop"
  )
print(summary_by_cycle)

message("\n=== SRH distribution ===")
print(table(data_nhanes$srh, useNA = "ifany"))

message("\n=== Age range ===")
print(range(data_nhanes$age, na.rm = TRUE))

# ------------------------------------------------------------------------------
# Save output
# ------------------------------------------------------------------------------

output_path <- derived_path("data_nhanes_source.rds")
saveRDS(data_nhanes, output_path)
message("\nSaved to: ", output_path)

# Also save a version with only 1999-2018 for comparison with Kamaryn's data
data_nhanes_1999_2018 <- data_nhanes %>%
  filter(year <= 2017.5)

output_path_1999_2018 <- derived_path("data_nhanes_source_1999-2018.rds")
saveRDS(data_nhanes_1999_2018, output_path_1999_2018)
message("Saved 1999-2018 subset to: ", output_path_1999_2018)

message("\n=== Done! ===")
