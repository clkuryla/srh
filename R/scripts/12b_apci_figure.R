# ==============================================================================
# 12b_apci_figure.R
# Combined APCI Grid Figure (4 rows x 6 survey columns)
#
# Loads pre-saved CSV results from output/apc/apci/ and generates a single
# publication-ready figure. No model re-fitting required.
#
# Layout:
#   Row 1: Cohort deviations (inter-cohort averages)    [#CC79A7 pink]
#   Row 2: Cohort slopes (intra-cohort dynamics)        [#E69F00 orange]
#   Row 3: Age main effects                             [#0072B2 blue]
#   Row 4: Period main effects                          [#009E73 green]
#   Columns: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)

source(here::here("R", "functions", "theme_srh.R"))
source(here::here("R", "functions", "plot_utils.R"))
source(here::here("R", "functions", "apci_analysis.R"))

# --- Load pre-saved APCI results ---
apci_output_dir <- here::here("output", "apc", "apci")

all_cohort_avgs    <- read_csv(file.path(apci_output_dir, "apci_cohort_avgs_all.csv"),
                               show_col_types = FALSE)
all_cohort_slopes  <- read_csv(file.path(apci_output_dir, "apci_cohort_slopes_all.csv"),
                               show_col_types = FALSE)
all_age_effects    <- read_csv(file.path(apci_output_dir, "apci_age_effects_all.csv"),
                               show_col_types = FALSE)
all_period_effects <- read_csv(file.path(apci_output_dir, "apci_period_effects_all.csv"),
                               show_col_types = FALSE)

message("Loaded APCI results: ",
        nrow(all_cohort_avgs), " cohort avgs, ",
        nrow(all_cohort_slopes), " cohort slopes, ",
        nrow(all_age_effects), " age effects, ",
        nrow(all_period_effects), " period effects")
message("Surveys: ", paste(sort(unique(all_cohort_avgs$survey)), collapse = ", "))

# --- Generate combined grid figure ---
p <- plot_apci_combined_grid(
  all_cohort_avgs    = all_cohort_avgs,
  all_cohort_slopes  = all_cohort_slopes,
  all_age_effects    = all_age_effects,
  all_period_effects = all_period_effects,
  survey_order = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
  base_size = 20
)

# --- Save ---
save_figure(p, "apci_combined_grid",
            path = apci_output_dir,
            width = 20, height = 16)

# Draft with date
ggsave(file.path(apci_output_dir,
                 paste0("apci_combined_grid_draft_",
                        format(Sys.Date(), "%Y%m%d"), ".png")),
       plot = p, width = 20, height = 16, dpi = 300)

message("Done! Figure saved to ", apci_output_dir)
