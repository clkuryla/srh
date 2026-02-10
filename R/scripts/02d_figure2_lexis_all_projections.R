# ==============================================================================
# 02d_figure2_lexis_all_projections.R
#
# Combined 3x6 Lexis Figure: All Three APC Projections
#
# Purpose:
#   Generate a single stacked figure showing all three Lexis projections:
#     Row A: Year x Age      (from 02_figure2_lexis.R)
#     Row B: Cohort x Age    (from 02b_figure2_lexis_cohort.R)
#     Row C: Year x Cohort   (from 02c_figure2_lexis_period_cohort.R)
#
#   Survey columns: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
#   Shared turbo color scale (rescaled 0-1 SRH) across all 18 panels.
#
# Inputs:
#   - Wrangled survey data from data depot (_derived/srh_project/)
#
# Outputs:
#   - output/figures/fig2_lexis_all_projections.{png,pdf}
#   - output/figures/fig2_lexis_all_projections_draft_{date}.png
#
# Author: Christine Lucille Kuryla
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)
library(grid)

# Source project functions
source(here::here("R/paths.R"))
ensure_dirs()

source(here::here("R/functions/theme_srh.R"))
source(here::here("R/functions/plot_lexis.R"))

cat("====================================================\n")
cat("Figure 2 (All Projections): Combined 3x6 Lexis Grid\n")
cat("====================================================\n\n")


# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data...\n")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_meps <- readr::read_rds(derived_path("data_meps.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_brfss <- readr::read_rds(derived_path("data_brfss.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_gss <- readr::read_rds(derived_path("data_gss.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_cps <- readr::read_rds(derived_path("data_cps.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

data_nhanes <- readr::read_rds(derived_path("data_nhanes.rds")) |>
  select(srh, age, year, wt) |>
  filter(wt != 0) |>
  drop_na(srh, age, year, wt)

# BRFSS age recoding (consistent with all other Lexis scripts)
data_brfss <- data_brfss |>
  mutate(age = if_else(age >= 80, 82, age))

cat("  NHIS:   ", format(nrow(data_nhis), big.mark = ","), " obs\n")
cat("  MEPS:   ", format(nrow(data_meps), big.mark = ","), " obs\n")
cat("  BRFSS:  ", format(nrow(data_brfss), big.mark = ","), " obs\n")
cat("  GSS:    ", format(nrow(data_gss), big.mark = ","), " obs\n")
cat("  CPS:    ", format(nrow(data_cps), big.mark = ","), " obs\n")
cat("  NHANES: ", format(nrow(data_nhanes), big.mark = ","), " obs\n\n")


# ==============================================================================
# PREPARE ALL THREE PROJECTIONS OF LEXIS DATA
# ==============================================================================

# Common settings
age_bin   <- 5
year_bin  <- 5
cohort_bin <- 5
min_age   <- 20
max_age   <- 89
min_cell_n <- 30
min_cell_n_small <- 20  # GSS and NHANES

survey_datasets <- list(
  "BRFSS"  = list(data = data_brfss,  scale = "5", min_n = min_cell_n),
  "MEPS"   = list(data = data_meps,   scale = "5", min_n = min_cell_n),
  "NHIS"   = list(data = data_nhis,   scale = "5", min_n = min_cell_n),
  "CPS"    = list(data = data_cps,    scale = "5", min_n = min_cell_n),
  "NHANES" = list(data = data_nhanes, scale = "5", min_n = min_cell_n_small),
  "GSS"    = list(data = data_gss,    scale = "4", min_n = min_cell_n_small)
)

survey_order <- names(survey_datasets)

# --- Row A: Year x Age ---
cat("Preparing Row A: Year x Age...\n")
lexis_year_age <- lapply(survey_order, function(svy) {
  s <- survey_datasets[[svy]]
  cat("  ", svy, "...\n")
  prepare_lexis_data(
    s$data,
    age_binwidth  = age_bin,
    year_binwidth = year_bin,
    min_age = min_age, max_age = max_age,
    min_n = s$min_n,
    rescale_01 = TRUE, srh_scale = s$scale
  )
})
names(lexis_year_age) <- survey_order

# --- Row B: Cohort x Age ---
cat("Preparing Row B: Cohort x Age...\n")
lexis_cohort_age <- lapply(survey_order, function(svy) {
  s <- survey_datasets[[svy]]
  cat("  ", svy, "...\n")
  prepare_lexis_data_cohort(
    s$data,
    age_binwidth    = age_bin,
    cohort_binwidth = cohort_bin,
    min_age = min_age, max_age = max_age,
    min_n = s$min_n,
    rescale_01 = TRUE, srh_scale = s$scale
  )
})
names(lexis_cohort_age) <- survey_order

# --- Row C: Year x Cohort ---
cat("Preparing Row C: Year x Cohort...\n")
lexis_year_cohort <- lapply(survey_order, function(svy) {
  s <- survey_datasets[[svy]]
  cat("  ", svy, "...\n")
  prepare_lexis_data_period_cohort(
    s$data,
    year_binwidth   = year_bin,
    cohort_binwidth = cohort_bin,
    min_age = min_age, max_age = max_age,
    min_n = s$min_n,
    rescale_01 = TRUE, srh_scale = s$scale
  )
})
names(lexis_year_cohort) <- survey_order

cat("Done preparing data.\n\n")


# ==============================================================================
# COMPUTE GLOBAL SRH RANGE FOR SHARED COLOR SCALE
# ==============================================================================

all_srh_values <- c(
  unlist(lapply(lexis_year_age,    function(x) x$mean_srh)),
  unlist(lapply(lexis_cohort_age,  function(x) x$mean_srh)),
  unlist(lapply(lexis_year_cohort, function(x) x$mean_srh))
)

global_min <- min(all_srh_values, na.rm = TRUE)
global_max <- max(all_srh_values, na.rm = TRUE)

cat("Global SRH range across all 18 panels:\n")
cat("  Min: ", round(global_min, 3), "\n")
cat("  Max: ", round(global_max, 3), "\n\n")


# ==============================================================================
# BUILD 18 SUBPLOTS
# ==============================================================================

cat("Building 3x6 subplot grid...\n")

n_surveys <- length(survey_order)
base_size <- 16

row_a_plots <- vector("list", n_surveys)
row_b_plots <- vector("list", n_surveys)
row_c_plots <- vector("list", n_surveys)

for (i in seq_along(survey_order)) {
  svy <- survey_order[i]

  # Row A: Year x Age (survey name as column header)
  row_a_plots[[i]] <- create_lexis_subplot(
    data = lexis_year_age[[svy]],
    survey_name = svy,
    x_axis = "year",
    y_axis = "age",
    show_title = TRUE,
    show_legend = TRUE,
    show_cohort_lines = FALSE,
    color_scale = "turbo",
    reverse_colors = TRUE,
    shared_scale = TRUE,
    scale_limits = c(global_min, global_max),
    tilt_x_labels = 45,
    base_size = base_size
  )

  # Row B: Cohort x Age (no title — column headers from row A)
  row_b_plots[[i]] <- create_lexis_subplot(
    data = lexis_cohort_age[[svy]],
    survey_name = svy,
    x_axis = "cohort",
    y_axis = "age",
    show_title = FALSE,
    show_legend = TRUE,
    show_cohort_lines = FALSE,
    color_scale = "turbo",
    reverse_colors = TRUE,
    shared_scale = TRUE,
    scale_limits = c(global_min, global_max),
    tilt_x_labels = 45,
    base_size = base_size
  )

  # Row C: Year x Cohort (no title)
  row_c_plots[[i]] <- create_lexis_subplot(
    data = lexis_year_cohort[[svy]],
    survey_name = svy,
    x_axis = "year",
    y_axis = "cohort",
    show_title = FALSE,
    show_legend = TRUE,
    show_cohort_lines = FALSE,
    color_scale = "turbo",
    reverse_colors = TRUE,
    shared_scale = TRUE,
    scale_limits = c(global_min, global_max),
    tilt_x_labels = 45,
    base_size = base_size
  )
}

cat("Done building subplots.\n\n")


# ==============================================================================
# ASSEMBLE 3x6 GRID WITH ROW LABELS
# ==============================================================================

cat("Assembling combined figure...\n")

# --- Assemble rows ---
row_a <- wrap_plots(row_a_plots, ncol = n_surveys)
row_b <- wrap_plots(row_b_plots, ncol = n_surveys)
row_c <- wrap_plots(row_c_plots, ncol = n_surveys)

# --- Row labels (rotated text on left margin) ---
make_row_label <- function(label_text, base_size) {
  wrap_elements(full = grid::textGrob(
    label_text,
    rot = 90,
    gp = grid::gpar(fontsize = base_size + 4, fontface = "bold")
  ))
}

label_a <- make_row_label("Year \u00D7 Age", base_size)
label_b <- make_row_label("Cohort \u00D7 Age", base_size)
label_c <- make_row_label("Year \u00D7 Cohort", base_size)

# --- Combine: (label | row) for each row, stacked vertically ---
label_width <- 0.04

section_a <- (label_a | row_a) + plot_layout(widths = c(label_width, 1))
section_b <- (label_b | row_b) + plot_layout(widths = c(label_width, 1))
section_c <- (label_c | row_c) + plot_layout(widths = c(label_width, 1))

# Stack the three sections with shared legend
combined <- section_a / section_b / section_c +
  plot_layout(heights = c(1, 1, 1), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )

# --- Add overall title ---
combined <- combined +
  plot_annotation(
    title = "Lexis Diagrams of Mean SRH: Three APC Projections",
    subtitle = "Color: SRH rescaled 0\u20131",
    theme = theme(
      plot.title = element_text(
        size = base_size + 8, face = "bold", hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = base_size + 2, color = "gray40", hjust = 0.5
      ),
      plot.margin = margin(10, 10, 5, 10)
    )
  )

cat("Done assembling figure.\n\n")


# ==============================================================================
# SAVE FIGURE
# ==============================================================================

fig_dir <- here::here("output", "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

fig_width  <- 16
fig_height <- 14

cat("Saving combined 3x6 Lexis figure...\n")

# Final versions
ggsave(
  filename = file.path(fig_dir, "fig2_lexis_all_projections.png"),
  plot = combined,
  width = fig_width,
  height = fig_height,
  dpi = 300
)
cat("  Saved: fig2_lexis_all_projections.png\n")

ggsave(
  filename = file.path(fig_dir, "fig2_lexis_all_projections.pdf"),
  plot = combined,
  width = fig_width,
  height = fig_height
)
cat("  Saved: fig2_lexis_all_projections.pdf\n")

# Draft version with date
draft_date <- format(Sys.Date(), "%Y%m%d")
ggsave(
  filename = file.path(fig_dir, paste0("fig2_lexis_all_projections_draft_", draft_date, ".png")),
  plot = combined,
  width = fig_width,
  height = fig_height,
  dpi = 300
)
cat("  Saved: fig2_lexis_all_projections_draft_", draft_date, ".png\n\n")


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("====================================================\n")
cat("Figure 2 (All Projections) complete!\n")
cat("====================================================\n")
cat("\nOutputs:\n")
cat("  Combined: output/figures/fig2_lexis_all_projections.{png,pdf}\n")
cat("  Draft:    output/figures/fig2_lexis_all_projections_draft_", draft_date, ".png\n")
cat("\nLayout: 3 rows x 6 columns\n")
cat("  Row A: Year x Age      (period on x, age on y)\n")
cat("  Row B: Cohort x Age    (birth year on x, age on y)\n")
cat("  Row C: Year x Cohort   (period on x, birth year on y)\n")
cat("  Columns: ", paste(survey_order, collapse = ", "), "\n")
cat("  Shared turbo color scale (rescaled 0-1 SRH)\n")
cat("\n")
