# ==============================================================================
# 11b_medpolish_figure.R
# Combined Median Polish Grid Figure (3 rows x 6 survey columns)
#
# Loads pre-saved CSV results from output/apc/medpolish/ and generates a single
# publication-ready figure. No model re-fitting required.
#
# Layout:
#   Row 1: Age effects        [#0072B2 blue]
#   Row 2: Period effects     [#009E73 green]
#   Row 3: Cohort effects     [#CC79A7 pink]
#   Columns: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
#
# Each panel shows two overlaid estimates from different medpolish slices
# (e.g., Age from AP and AC), with different shapes/linetypes.
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)

source(here::here("R", "functions", "theme_srh.R"))
source(here::here("R", "functions", "plot_utils.R"))
source(here::here("R", "functions", "medpolish_apc.R"))

# --- Load pre-saved medpolish results ---
mp_output_dir <- here::here("output", "apc", "medpolish")

all_direct_effects <- read_csv(
  file.path(mp_output_dir, "medpolish_direct_effects_all_surveys.csv"),
  show_col_types = FALSE
)

message("Loaded medpolish direct effects: ", nrow(all_direct_effects), " rows")
message("Surveys: ", paste(sort(unique(all_direct_effects$survey)), collapse = ", "))
message("Dimensions: ", paste(sort(unique(all_direct_effects$dimension)), collapse = ", "))

# --- Generate combined grid figure ---
p <- plot_medpolish_combined_grid(
  all_direct_effects = all_direct_effects,
  survey_order = c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS"),
  base_size = 20
)

# --- Save ---
save_figure(p, "medpolish_combined_grid",
            path = mp_output_dir,
            width = 20, height = 14)

# Draft with date
ggsave(file.path(mp_output_dir,
                 paste0("medpolish_combined_grid_draft_",
                        format(Sys.Date(), "%Y%m%d"), ".png")),
       plot = p, width = 20, height = 14, dpi = 300)

message("Done! Figure saved to ", mp_output_dir)
