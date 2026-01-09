# Data Details
# =================

All data post-wrangle have "age", "year", "wt", "strata", "psu", and "srh", where SRH is reverse coded such that 1 = poor.

## Data locations

### MEPS
Wrangled: derived_path("data_meps.rds")
Raw: depot_path("surveys", "MEPS", "ipums_extracts", "meps_00001.csv")
Source: https://meps.ipums.org

### NHIS 
Wrangled: derived_path("data_nhis.rds")
Raw: depot_path("surveys", "NHIS", "nhis_00002.csv")
Source: https://nhis.ipums.org

