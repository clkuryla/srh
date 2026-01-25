# ==============================================================================
# 05_figure5_mortality_hr_prev.R
# Figure 5: SRH-Mortality Combined Figure (HR + Prevalence)
#
# Panel A: Hazard ratio by age group (SRH predictive ability)
# Panel B: Mortality prevalence by age group (context)
#
# Uses 15-year follow-up windows from NHIS mortality linkage data
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)

# Source shared functions
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "plot_mortality.R"))

# Output directory
output_dir <- here("output", "figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Analysis date for draft outputs
analysis_date <- format(Sys.Date(), "%Y%m%d")

# ------------------------------------------------------------------------------
# LOAD RESULTS
# ------------------------------------------------------------------------------

message("=== Loading mortality results ===")

# Load existing 15-year window results
results_15yr <- read_csv(
  here("output", "mortality_results_15yr_20260119.csv"),
  col_types = cols(
    start_year = col_integer(),
    end_year = col_integer(),
    window_length = col_integer(),
    follow_up_years = col_integer(),
    age_group = col_character(),
    coef = col_double(),
    hr = col_double(),
    conf_low = col_double(),
    conf_high = col_double(),
    p_value = col_double(),
    n = col_integer(),
    n_events = col_integer(),
    converged = col_logical()
  )
)

# Ensure age_group is ordered factor
results_15yr <- results_15yr %>%
  mutate(
    age_group = factor(age_group, levels = c(
      "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"
    ))
  )

message(sprintf("Loaded %d rows of results", nrow(results_15yr)))
message(sprintf("Window years: %d to %d", min(results_15yr$start_year), max(results_15yr$start_year)))
message(sprintf("Age groups: %s", paste(levels(results_15yr$age_group), collapse = ", ")))

# Quick summary
summary_stats <- results_15yr %>%
  filter(converged) %>%
  group_by(age_group) %>%
  summarise(
    mean_hr = mean(hr),
    mean_death_rate = mean(n_events / n) * 100,
    .groups = "drop"
  )

message("\nSummary by age group:")
print(summary_stats)

# ------------------------------------------------------------------------------
# GENERATE FIGURE 5
# ------------------------------------------------------------------------------

message("\n=== Generating Figure 5 ===")

fig5 <- plot_fig5_combined(
  results_15yr,
  window_length = 15,
  title = "SRH and Mortality by Age Group",
  subtitle = "NHIS 1986-2014, 15-year follow-up windows, survey-weighted Cox PH models"
)

# ------------------------------------------------------------------------------
# SAVE FIGURES
# ------------------------------------------------------------------------------

message("\n=== Saving figures ===")

# Draft version (with date)
ggsave(
  here(output_dir, paste0("fig5_mortality_hr_prev_draft_", analysis_date, ".png")),
  fig5,
  width = 10, height = 10, dpi = 300
)
message(sprintf("Saved draft: fig5_mortality_hr_prev_draft_%s.png", analysis_date))

# Final versions (no date, for publication)
ggsave(
  here(output_dir, "fig5_mortality_hr_prev.png"),
  fig5,
  width = 10, height = 10, dpi = 300
)

ggsave(
  here(output_dir, "fig5_mortality_hr_prev.pdf"),
  fig5,
  width = 10, height = 10
)

message("Saved final: fig5_mortality_hr_prev.png and fig5_mortality_hr_prev.pdf")

# ------------------------------------------------------------------------------
# SESSION INFO
# ------------------------------------------------------------------------------

message("\n=== Session Info ===")
sessionInfo()

message("\n=== Figure 5 generation complete ===")
