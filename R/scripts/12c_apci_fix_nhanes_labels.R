# ==============================================================================
# 12c_apci_fix_nhanes_labels.R
# Correct the cohort labels in the SAVED APC-I outputs and regenerate the figures
# and report that read them.
#
# Bug (found 2026-08-15 during the APC hump-location diagnostics, srh-bhapc/
# analysis/apc_diagnostics/): extract_apci_results() labelled APCI's cohort
# index (the A+P-1 diagonals of the age x period grid, index = A + period_idx
# - age_idx) by joining to distinct(cohort_group, cohort_midpoint) with
# cohort_group = as.integer(factor(cohort_midpoint)). That is only right on a
# regular grid. NHANES's 5-yr periods have irregular midpoints
# (2002/2007/2012/2017/2021) -> 27 distinct row-level midpoints for 16
# diagonals, so the 16 diagonals were labelled with the 16 SMALLEST midpoints
# (1924.5 ... 1969.5) instead of the diagonal means (1924.5 ... 1998.5).
# Consequence: the NHANES x-axes of Fig S9 panels C and D (apci_combined_grid)
# were compressed by up to 29 years. Estimates, SEs and ordering are unaffected;
# the five annual surveys (regular grids) are unaffected.
#
# A second instance of the same positional-vs-label mismatch affects BRFSS: the
# wrangled BRFSS codes every age 80+ as 89, so the 5-yr bin "80-84" is empty and
# APCI's positional age level 13 is "85-89" (label 14). The saved age lookup was
# joined on the label, so APCI level 13 got NA (its estimate, -0.428, was dropped
# from Fig S9 panel A) and the BRFSS cohort diagonals were labelled as if a 14th
# age level existed (labels up to 5 years too early). Repaired here too: the
# BRFSS age table's index-13 row is labelled "85-89" (midpoint 87.5) and its
# cohort labels are rebuilt from the diagonal means (which for the 8 diagonals
# containing the 85-89 cell average cells 5 years apart; n_cells recorded).
#
# The code fix lives in R/functions/apci_analysis.R (build_apci_cohort_lookup()
# + positional age/period lookups in extract_apci_results()).
# The full NHANES dataset is not on this machine, so the model is NOT refit here;
# this script re-derives the correct diagonal labels from the SAVED age/period
# main-effect tables (which carry the level midpoints) and rewrites the CSVs.
#
# What it does:
#   1. Backs up the as-filed CSVs to output/apc/apci/asfiled_20260213/ (once;
#      never overwrites an existing backup).
#   2. For every survey rebuilds the diagonal cohort lookup with
#      build_apci_cohort_lookup(); asserts the annual surveys' labels are
#      unchanged and NHANES's are corrected as expected.
#   3. Rewrites apci_cohort_avgs_{survey}.csv, apci_cohort_slopes_{survey}.csv and
#      the *_all.csv files (cohort_midpoint, birth_year_approx, cohort_group).
#   4. Regenerates: apci_cohort_avgs_all_surveys.*, apci_cohort_slopes_all_surveys.*,
#      NHANES per-survey figures (cohort averages / slopes / combined), and the
#      Fig S9 grid via R/scripts/12b_apci_figure.R.
#
# Usage:  Rscript R/scripts/12c_apci_fix_nhanes_labels.R
# Author: Christine Lucille Kuryla (with Claude), 2026-08-15
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)

source(here::here("R", "functions", "theme_srh.R"))
source(here::here("R", "functions", "plot_utils.R"))
source(here::here("R", "functions", "apci_analysis.R"))   # build_apci_cohort_lookup(), plotting fns

apci_output_dir <- here::here("output", "apc", "apci")
backup_dir      <- file.path(apci_output_dir, "asfiled_20260213")
SURVEYS <- c("gss", "nhanes", "meps", "nhis", "cps", "brfss")

# ------------------------------------------------------------------------------
# 1. Backup (once)
# ------------------------------------------------------------------------------
to_backup <- c(paste0("apci_cohort_avgs_", SURVEYS, ".csv"),
               paste0("apci_cohort_slopes_", SURVEYS, ".csv"),
               "apci_cohort_avgs_all.csv", "apci_cohort_slopes_all.csv",
               "apci_age_effects_brfss.csv", "apci_age_effects_all.csv")
if (!dir.exists(backup_dir)) {
  dir.create(backup_dir)
  ok <- file.copy(file.path(apci_output_dir, to_backup), backup_dir, overwrite = FALSE)
  stopifnot(all(ok))
  writeLines(c("As-filed APC-I cohort tables (run of 2026-02-13) backed up before the",
               "cohort-label correction of 2026-08-15 (R/scripts/12c_apci_fix_nhanes_labels.R).",
               "NHANES cohort labels, BRFSS cohort labels and the BRFSS age-level-13",
               "label are the only differences between these and the corrected files."),
             file.path(backup_dir, "README.txt"))
  message("Backed up ", length(to_backup), " files to ", backup_dir)
} else {
  message("Backup dir exists; not overwriting: ", backup_dir)
}
# Always read the AS-FILED versions from the backup so the script is idempotent
read_backup <- function(f) read_csv(file.path(backup_dir, f), show_col_types = FALSE)

# ------------------------------------------------------------------------------
# 1b. BRFSS age table: label APCI's positional level 13 as "85-89" (see header)
# ------------------------------------------------------------------------------
fix_brfss_age <- function(df) {
  i <- which(df$age_index == 13)
  stopifnot(length(i) == 1, is.na(df$age_midpoint[i]), max(df$age_index) == 13,
            df$age_group_label[df$age_index == 12] == "75-79")
  df$age_group[i] <- 14; df$age_group_label[i] <- "85-89"; df$age_midpoint[i] <- 87.5
  df
}
age_b0 <- read_backup("apci_age_effects_brfss.csv")
age_b1 <- fix_brfss_age(age_b0)
write_csv(age_b1, file.path(apci_output_dir, "apci_age_effects_brfss.csv"), na = "")
age_all0 <- read_backup("apci_age_effects_all.csv")
age_all1 <- age_all0
rb <- which(age_all1$survey == "BRFSS")
age_all1[rb, setdiff(names(age_all1), "survey")] <- fix_brfss_age(age_all0[rb, setdiff(names(age_all0), "survey")])
stopifnot(sum(is.na(age_all1$age_midpoint)) == 0)
write_csv(age_all1, file.path(apci_output_dir, "apci_age_effects_all.csv"), na = "")
message("BRFSS age level 13 labelled 85-89.")

# ------------------------------------------------------------------------------
# 2. Rebuild diagonal lookups from the saved age/period main-effect tables
# ------------------------------------------------------------------------------
lookups <- map(SURVEYS, function(sv) {
  age <- read_csv(file.path(apci_output_dir, paste0("apci_age_effects_", sv, ".csv")), show_col_types = FALSE) |>
    distinct(age_index, age_midpoint) |> arrange(age_index)
  stopifnot(!anyNA(age$age_midpoint))
  per <- read_csv(file.path(apci_output_dir, paste0("apci_period_effects_", sv, ".csv")), show_col_types = FALSE) |>
    distinct(period_index, period_midpoint) |> arrange(period_index)
  build_apci_cohort_lookup(age, per) |> mutate(survey = toupper(sv), .before = 1)
}) |> set_names(SURVEYS)

relabel <- function(df, lk) {
  stopifnot(all(df$cohort_index %in% lk$cohort_index), nrow(df) == nrow(lk))
  df |>
    select(-cohort_midpoint, -cohort_group) |>
    left_join(lk |> select(cohort_index, cohort_group, cohort_midpoint), by = "cohort_index") |>
    mutate(birth_year_approx = round(cohort_midpoint)) |>
    relocate(cohort_group, cohort_midpoint, .after = p_value)
}

changes <- list()
for (sv in SURVEYS) {
  lk <- lookups[[sv]]
  a0 <- read_backup(paste0("apci_cohort_avgs_", sv, ".csv"))
  s0 <- read_backup(paste0("apci_cohort_slopes_", sv, ".csv"))
  a1 <- relabel(a0, lk); s1 <- relabel(s0, lk)
  # keep original column order
  a1 <- a1[, names(a0)]; s1 <- s1[, names(s0)]
  d <- max(abs(a1$cohort_midpoint - a0$cohort_midpoint))
  changes[[sv]] <- tibble(survey = toupper(sv), n_diagonals = nrow(lk),
                          max_label_change_yrs = d,
                          first_label = min(lk$cohort_midpoint), last_label = max(lk$cohort_midpoint))
  if (sv == "nhanes") {
    stopifnot(d > 20)                                     # the bug is real and corrected
    stopifnot(abs(max(lk$cohort_midpoint) - 1998.5) < 1e-9)
  } else if (sv == "brfss") {
    stopifnot(d > 4, d <= 5 + 1e-9)                       # BRFSS: up to one bin (5 y)
    stopifnot(abs(max(lk$cohort_midpoint) - 2004.5) < 1e-9)
  } else {
    stopifnot(d < 1e-9)                                   # GSS/MEPS/NHIS/CPS unchanged
  }
  # estimates untouched
  stopifnot(identical(a1$estimate, a0$estimate), identical(s1$estimate, s0$estimate))
  write_csv(a1, file.path(apci_output_dir, paste0("apci_cohort_avgs_", sv, ".csv")), na = "")
  write_csv(s1, file.path(apci_output_dir, paste0("apci_cohort_slopes_", sv, ".csv")), na = "")
}
changes <- bind_rows(changes)
print(changes)

# combined files: rebuild from the corrected per-survey files, preserving the
# as-filed row order (GSS, NHANES, MEPS, NHIS, CPS, BRFSS) and columns
all_a0 <- read_backup("apci_cohort_avgs_all.csv"); all_s0 <- read_backup("apci_cohort_slopes_all.csv")
all_a1 <- map_dfr(SURVEYS, ~ read_csv(file.path(apci_output_dir, paste0("apci_cohort_avgs_", .x, ".csv")),
                                      show_col_types = FALSE) |> mutate(survey = toupper(.x)))
all_s1 <- map_dfr(SURVEYS, ~ read_csv(file.path(apci_output_dir, paste0("apci_cohort_slopes_", .x, ".csv")),
                                      show_col_types = FALSE) |> mutate(survey = toupper(.x)))
stopifnot(identical(names(all_a1), names(all_a0)), identical(names(all_s1), names(all_s0)),
          nrow(all_a1) == nrow(all_a0), nrow(all_s1) == nrow(all_s0),
          identical(all_a1$survey, all_a0$survey), identical(all_a1$estimate, all_a0$estimate))
write_csv(all_a1, file.path(apci_output_dir, "apci_cohort_avgs_all.csv"), na = "")
write_csv(all_s1, file.path(apci_output_dir, "apci_cohort_slopes_all.csv"), na = "")
write_csv(changes, file.path(apci_output_dir, "apci_cohort_label_correction_20260815.csv"))
write_csv(bind_rows(lookups), file.path(apci_output_dir, "apci_cohort_diagonal_lookup.csv"))
message("CSVs rewritten.")

# ------------------------------------------------------------------------------
# 3. Regenerate figures that show cohort labels
# ------------------------------------------------------------------------------
sv_levels <- toupper(SURVEYS)
save_figure(plot_apci_cohort_avgs_all(all_a1, survey_order = sv_levels),
            "apci_cohort_avgs_all_surveys", path = apci_output_dir, width = 12, height = 8)
save_figure(plot_apci_cohort_slopes_all(all_s1, survey_order = sv_levels),
            "apci_cohort_slopes_all_surveys", path = apci_output_dir, width = 12, height = 8)

# NHANES per-survey figures (from the saved tables; same plotting functions as 12)
nh_results <- list(
  age_effects     = read_csv(file.path(apci_output_dir, "apci_age_effects_nhanes.csv"), show_col_types = FALSE),
  period_effects  = read_csv(file.path(apci_output_dir, "apci_period_effects_nhanes.csv"), show_col_types = FALSE),
  cohort_averages = read_csv(file.path(apci_output_dir, "apci_cohort_avgs_nhanes.csv"), show_col_types = FALSE) |>
    mutate(sig = replace_na(sig, "")),
  cohort_slopes   = read_csv(file.path(apci_output_dir, "apci_cohort_slopes_nhanes.csv"), show_col_types = FALSE) |>
    mutate(sig = replace_na(sig, ""))
)
save_figure(plot_apci_cohort_averages(nh_results, "nhanes"), "apci_cohort_averages_nhanes",
            path = apci_output_dir, width = 10, height = 6)
save_figure(plot_apci_cohort_slopes(nh_results, "nhanes"), "apci_cohort_slopes_nhanes",
            path = apci_output_dir, width = 10, height = 6)
save_figure(plot_apci_combined(nh_results, "nhanes"), "apci_combined_nhanes",
            path = apci_output_dir, width = 14, height = 12)

# BRFSS per-survey figures (age curve now includes 85-89; cohort labels corrected)
br_results <- list(
  age_effects     = read_csv(file.path(apci_output_dir, "apci_age_effects_brfss.csv"), show_col_types = FALSE),
  period_effects  = read_csv(file.path(apci_output_dir, "apci_period_effects_brfss.csv"), show_col_types = FALSE),
  cohort_averages = read_csv(file.path(apci_output_dir, "apci_cohort_avgs_brfss.csv"), show_col_types = FALSE) |>
    mutate(sig = replace_na(sig, "")),
  cohort_slopes   = read_csv(file.path(apci_output_dir, "apci_cohort_slopes_brfss.csv"), show_col_types = FALSE) |>
    mutate(sig = replace_na(sig, ""))
)
save_figure(plot_apci_main_effects(br_results, "brfss"), "apci_main_effects_brfss",
            path = apci_output_dir, width = 12, height = 5)
save_figure(plot_apci_cohort_averages(br_results, "brfss"), "apci_cohort_averages_brfss",
            path = apci_output_dir, width = 10, height = 6)
save_figure(plot_apci_cohort_slopes(br_results, "brfss"), "apci_cohort_slopes_brfss",
            path = apci_output_dir, width = 10, height = 6)
save_figure(plot_apci_combined(br_results, "brfss"), "apci_combined_brfss",
            path = apci_output_dir, width = 14, height = 12)
# cross-survey main-effects figure (BRFSS age curve now complete)
save_figure(plot_apci_main_effects_all(age_all1,
              read_csv(file.path(apci_output_dir, "apci_period_effects_all.csv"), show_col_types = FALSE),
              survey_order = sv_levels),
            "apci_main_effects_all_surveys", path = apci_output_dir, width = 12, height = 14)

# Fig S9 grid (reads the corrected *_all.csv files)
message("Re-running 12b_apci_figure.R for the Fig S9 grid ...")
source(here::here("R", "scripts", "12b_apci_figure.R"))

message("Done. Backup of as-filed tables: ", backup_dir)
