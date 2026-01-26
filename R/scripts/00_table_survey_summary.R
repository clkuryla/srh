# =============================================================================
# Table: Survey Dataset Summary
# Creates summary table of all 6 surveys used in analysis
# Author: Christine Kuryla
# =============================================================================

library(tidyverse)
library(here)
source(here::here("R/paths.R"))

# Define survey metadata
survey_info <- tribble(

~survey_key, ~survey_name, ~source_name,
"brfss",     "BRFSS",      "Behavioral Risk Factor Surveillance System (CDC)",
"meps",      "MEPS",       "Medical Expenditure Panel Survey (AHRQ)",
"nhis",      "NHIS",       "National Health Interview Survey (CDC/NCHS)",
"cps",       "CPS",        "Current Population Survey (Census/BLS)",
"nhanes",    "NHANES",     "National Health and Nutrition Examination Survey (CDC/NCHS)",
"gss",       "GSS",        "General Social Survey (NORC)"
)

# Function to summarize a dataset
summarize_survey <- function(survey_key) {

  # Use data_nhanes_source.rds for NHANES (has more recent data)
  if (survey_key == "nhanes") {
    path <- derived_path("data_nhanes_source.rds")
  } else {
    path <- derived_path(paste0("data_", survey_key, ".rds"))
  }

  if (!file.exists(path)) {
    warning("File not found: ", path)
    return(NULL)
  }

  data <- read_rds(path)

  # Calculate statistics
  years <- sort(unique(data$year))
  n_waves <- length(years)
  total_n <- nrow(data)

  resp_per_year <- data %>%
    group_by(year) %>%
    summarize(n = n(), .groups = "drop")

  avg_n_per_wave <- round(mean(resp_per_year$n))

 # Year range - handle NHANES 2-year cycles
  if (survey_key == "nhanes") {
    # NHANES uses midpoint years (e.g., 1999.5 for 1999-2000)
    # Convert to actual year ranges
    min_year <- floor(min(years))
    max_year <- ceiling(max(years))
    # 2022 is a single year due to COVID disruption
    if (max(years) == 2022) {
      year_range <- paste0(min_year, "-", max_year)
    } else {
      year_range <- paste0(min_year, "-", max_year)
    }
  } else {
    year_range <- paste0(min(years), "-", max(years))
  }

  tibble(
    survey_key = survey_key,
    year_range = year_range,
    n_waves = n_waves,
    avg_n_per_wave = avg_n_per_wave,
    total_n = total_n
  )
}

# Generate summary for all surveys
survey_summaries <- map_dfr(survey_info$survey_key, summarize_survey)

# Join with metadata and format
summary_table <- survey_info %>%
  left_join(survey_summaries, by = "survey_key") %>%
  select(
    Source = source_name,
    Survey = survey_name,
    `Year Range` = year_range,
    `# Waves` = n_waves,
    `Avg N/Wave` = avg_n_per_wave,
    `Total N` = total_n
  )

# Display
print(summary_table)

# Save
output_path <- here::here("output/tables/table_survey_summary.csv")
write_csv(summary_table, output_path)
cat("\nSaved to:", output_path, "\n")
