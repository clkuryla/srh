# Master script: Run full data wrangling pipeline
#
# This script runs all data wrangling steps in the correct order to create
# harmonized datasets for the SRH analysis.
#
# Output:
#   - data_*.rds files: Full datasets with ALL covariates + harmonized variables
#   - essential_datasets/data_essential_*.rds: Slimmed versions with core variables only
#
# Pipeline order:
#   1. Individual survey wrangling (from raw data)
#   2. Harmonization scripts (add standardized variables)
#   3. Essential datasets (create slimmed versions)
#
# Author: Christine Lucille Kuryla
# Created: 2026-01-30

library(here)

cat("========================================\n")
cat("SRH Data Wrangling Pipeline\n
")
cat("========================================\n\n")

# Track timing
start_time <- Sys.time()

# ------------------------------------------------------------------------------
# Step 1: Individual Survey Wrangling
# ------------------------------------------------------------------------------
# These scripts read from raw data sources and create data_*.rds files
# with all survey-specific covariates preserved.

cat("=== Step 1: Individual Survey Wrangling ===\n\n")

# BRFSS - from raw XPT files (large, takes several minutes)
cat("Running wrangle_brfss.R...\n")
source(here("analysis/data_wrangling_code/wrangle_brfss.R"))
cat("  Done.\n\n")

# MEPS - from IPUMS extract
cat("Running wrangle_meps.R...\n")
source(here("analysis/data_wrangling_code/wrangle_meps.R"))
cat("  Done.\n\n")

# GSS - from gssr package
cat("Running wrangle_gss.R...\n")
source(here("analysis/data_wrangling_code/wrangle_gss.R"))
cat("  Done.\n\n")

# CPS - from IPUMS extract
cat("Running wrangle_cps.R...\n")
source(here("analysis/data_wrangling_code/wrangle_cps.R"))
cat("  Done.\n\n")

# NHIS - from IPUMS extract
cat("Running wrangle_nhis.R...\n")
source(here("analysis/data_wrangling_code/wrangle_nhis.R"))
cat("  Done.\n\n")

# NHANES - from nhanesA cached files (NOT Kamaryn's preprocessed data)
cat("Running wrangle_nhanes_source.R...\n")
source(here("analysis/data_wrangling_code/wrangle_nhanes_source.R"))
cat("  Done.\n\n")

# ------------------------------------------------------------------------------
# Step 2: Harmonization Scripts
# ------------------------------------------------------------------------------
# These scripts ADD harmonized variables to each data_*.rds file.
# They do NOT remove any existing variables.

cat("=== Step 2: Harmonization Scripts ===\n\n")

# Education harmonization
cat("Running wrangle_education.R...\n")
source(here("analysis/data_wrangling_code/wrangle_education.R"))
cat("  Done.\n\n")

# Sex harmonization
cat("Running wrangle_sex.R...\n")
source(here("analysis/data_wrangling_code/wrangle_sex.R"))
cat("  Done.\n\n")

# Race/ethnicity harmonization
cat("Running wrangle_race.R...\n")
source(here("analysis/data_wrangling_code/wrangle_race.R"))
cat("  Done.\n\n")

# ------------------------------------------------------------------------------
# Step 3: Essential Datasets
# ------------------------------------------------------------------------------
# Creates slimmed versions with only core harmonized variables.
# The full data_*.rds files are NOT modified.

cat("=== Step 3: Essential Datasets ===\n\n")

cat("Running create_essential_datasets.R...\n")
source(here("analysis/data_wrangling_code/create_essential_datasets.R"))
cat("  Done.\n\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

cat("========================================\n")
cat("Pipeline Complete!\n")
cat("========================================\n\n")

cat("Total time:", round(as.numeric(elapsed), 1), "minutes\n\n")

cat("Full datasets (all covariates preserved):\n")
cat("  - data_brfss.rds\n")
cat("  - data_cps.rds\n")
cat("  - data_gss.rds\n")
cat("  - data_meps.rds\n")
cat("  - data_nhanes.rds\n")
cat("  - data_nhis.rds\n\n")

cat("Essential datasets (core variables only):\n")
cat("  - essential_datasets/data_essential_*.rds\n\n")

cat("Harmonized variables added:\n")
cat("  - Education: educ_3cat, educ_4cat, educ_5cat, educ_orig\n")
cat("  - Sex: sex, sex_orig\n")
cat("  - Race: race_5cat, race_5cat_f, hispanic, hispanic_f,\n")
cat("          race_includehisp, race_includehisp_f\n")
