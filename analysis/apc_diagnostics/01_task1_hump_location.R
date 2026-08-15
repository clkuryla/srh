# ==============================================================================
# 01_task1_hump_location.R
# APC diagnostics — Task 1 (QUICK TIER): where does the cohort "hump" peak, and
# does that peak track each survey's observation-window centre (c-bar = p-bar - a-bar)
# or a fixed birth year (1955)?
#
# Spec: analysis/apc_diagnostics/INSTRUCTIONS.md (§3, quick tier per §1).
# Code map used for orientation: to_transfer/ronin_code_apc_etc.md (§1 BHAPC, §7 APC-I).
#
# NO MODEL FITTING HERE. Everything is extracted from SAVED artefacts:
#   * BHAPC: output/bhapc_full_random/<s>/<s>_bhapc_full_random_model.rds  (stanreg
#     fits from R/scripts/28b + 28c — contain the posterior draws, so peak CrIs are
#     computed per draw with no refitting) and <s>_bhapc_data.rds (the exact analysis
#     rows the model saw — window statistics are computed on these rows only).
#   * APC-I: analysis/apc_diagnostics/inputs/apci/*.csv (verbatim copies of
#     main/output/apc/apci/apci_cohort_{avgs,slopes}_all.csv; see provenance.txt).
#
# Outputs -> analysis/apc_diagnostics/output/task1/  (CSV tables, RDS of per-draw
# peaks, figures as PNG 300 dpi + PDF). The Quarto (apc_hump_location.qmd) reads
# ONLY these outputs.
#
# Author: Christine Lucille Kuryla (with Claude), 2026-08-15
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rstanarm)
  library(srvyr)
  library(patchwork)
})

source(here::here("R", "functions", "theme_srh.R"))
# For extract_variance_components() — used ONLY to re-derive the paper's variance
# shares from the saved fits (the §2.3 "right pipeline?" check). Nothing is refit.
source(here::here("R", "functions", "bhapc_model_fitting.R"))

t_script_start <- Sys.time()

# ------------------------------------------------------------------------------
# Configuration (all pre-stated rules live here)
# ------------------------------------------------------------------------------

DIAG_DIR   <- here::here("analysis", "apc_diagnostics")
BHAPC_DIR  <- here::here("output", "bhapc_full_random")
APCI_DIR   <- file.path(DIAG_DIR, "inputs", "apci")
OUT_DIR    <- file.path(DIAG_DIR, "output", "task1")
FIG_DIR    <- file.path(OUT_DIR, "figures")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

# Same display order as the paper's Fig S6/S7 (script 29) and Fig S9 (12b)
SURVEY_ORDER <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")

# theme_srh.R defines survey_shapes but (on this branch) no survey_colors, so a
# local Okabe-Ito palette is defined here for the six surveys.
survey_colors_local <- c(
  "BRFSS"  = "#D55E00",
  "MEPS"   = "#E69F00",
  "NHIS"   = "#009E73",
  "CPS"    = "#56B4E9",
  "NHANES" = "#0072B2",
  "GSS"    = "#CC79A7"
)

# Fixed-boomer reference birth year (INSTRUCTIONS §3)
BOOMER_REF <- 1955

# "Central region" for the quadratic-vertex fits: cohort bins whose label lies
# within these quantiles of the survey's respondent-cohort distribution.
# Primary rule = central 80 %; sensitivities = central 90 % and all bins.
CENTRAL_REGIONS <- list(
  central80 = c(0.10, 0.90),   # PRIMARY
  central90 = c(0.05, 0.95),
  all       = c(0.00, 1.00)
)
PRIMARY_REGION <- "central80"
MIN_BINS_FOR_PARABOLA <- 5
# Robustness rule (added after seeing that some APC-I curves plateau on one flank,
# which makes a global parabola non-concave): a LOCAL quadratic over the argmax bin
# +/- LOCAL_HALF_WIDTH bins (window clipped at the ends of the grid). This is a
# smoothed-argmax estimator of the peak location, insensitive to the global shape.
LOCAL_HALF_WIDTH <- 3

# APC-I sample restrictions (map / INSTRUCTIONS): ages 20-89, NHANES 20-79.
# Used to build a PROXY window for the APC-I sample from the BHAPC rows.
APCI_AGE_RANGE <- list(default = c(20, 89), nhanes = c(20, 79))
APCI_BIN_WIDTH <- 5

# Optional, clearly-labelled approximate uncertainty for the APC-I vertex:
# parametric bootstrap treating the cohort deviations as independent normals.
BOOT_SEED <- 20260815
N_BOOT    <- 2000

# BHAPC SRH scales (28b/28c SURVEY_CONFIG)
SRH_SCALE <- c(BRFSS = 5, MEPS = 5, NHIS = 5, CPS = 5, NHANES = 5, GSS = 4)

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------

#' Quadratic-vertex fit of a curve (or many curves at once) over a set of x values.
#' Y: matrix (n_draws x K) or vector (K). x: numeric (K). Returns b1, b2 (per year,
#' per year^2, in original x units) and the vertex x = -b1/(2 b2). Uses a centred /
#' scaled design for numerical stability, then back-transforms.
fit_vertex <- function(Y, x, scale_yrs = 10) {
  if (is.null(dim(Y))) Y <- matrix(Y, nrow = 1)
  stopifnot(ncol(Y) == length(x), length(x) >= 3)
  x0 <- mean(x)
  z  <- (x - x0) / scale_yrs
  X  <- cbind(1, z, z^2)
  # OLS for all rows at once: B (3 x n_draws) = (X'X)^-1 X' Y'
  B  <- solve(crossprod(X), crossprod(X, t(Y)))
  b1z <- B[2, ]; b2z <- B[3, ]
  # back-transform to per-year units around x0
  b2  <- b2z / scale_yrs^2
  b1  <- b1z / scale_yrs                     # slope at x0
  vertex <- x0 - b1 / (2 * b2)               # vertex in original x units
  tibble(b1 = b1, b2 = b2, vertex = vertex, concave = b2 < 0)
}

#' Local quadratic vertex around each row's argmax (window = argmax +/- hw bins,
#' clipped at the grid ends). Y: matrix (n x K); x: numeric (K). Rows sharing an
#' argmax bin share a design matrix, so the fit is done once per distinct argmax.
fit_local_vertex <- function(Y, x, hw = LOCAL_HALF_WIDTH, search = rep(TRUE, length(x))) {
  if (is.null(dim(Y))) Y <- matrix(Y, nrow = 1)
  K <- ncol(Y); stopifnot(K == length(x), length(search) == K, any(search))
  # argmax searched only over `search` bins (the central region); the window
  # itself may extend beyond them
  imax <- which(search)[max.col(Y[, search, drop = FALSE], ties.method = "first")]
  out <- tibble(b1 = rep(NA_real_, nrow(Y)), b2 = NA_real_, vertex = NA_real_, concave = NA,
                n_bins_window = NA_integer_)
  for (i in sort(unique(imax))) {
    w <- max(1, i - hw):min(K, i + hw)
    rows <- which(imax == i)
    if (length(w) < 3) next
    vf <- fit_vertex(Y[rows, w, drop = FALSE], x[w])
    out$b1[rows] <- vf$b1; out$b2[rows] <- vf$b2; out$vertex[rows] <- vf$vertex
    out$concave[rows] <- vf$concave; out$n_bins_window[rows] <- length(w)
  }
  out
}

#' Posterior summary of a numeric vector (median + 90 % interval)
q_summ <- function(v, prefix) {
  v <- v[is.finite(v)]
  out <- tibble(median = median(v), q05 = unname(quantile(v, 0.05)),
                q95 = unname(quantile(v, 0.95)), n = length(v))
  rename_with(out, ~ paste0(prefix, "_", .x))
}

#' Zero crossings of a piecewise-linear curve y(x) (linear interpolation).
find_crossings <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]
  o <- order(x); x <- x[o]; y <- y[o]
  out <- list()
  for (i in seq_len(length(x) - 1)) {
    if (y[i] == 0) next
    if (sign(y[i]) != sign(y[i + 1]) && y[i + 1] != 0) {
      xc <- x[i] + (0 - y[i]) / (y[i + 1] - y[i]) * (x[i + 1] - x[i])
      out[[length(out) + 1]] <- tibble(
        crossing = xc,
        direction = if (y[i] > 0) "pos_to_neg" else "neg_to_pos",
        x_left = x[i], x_right = x[i + 1], y_left = y[i], y_right = y[i + 1]
      )
    }
  }
  if (length(out) == 0) return(tibble(crossing = numeric(0), direction = character(0),
                                      x_left = numeric(0), x_right = numeric(0),
                                      y_left = numeric(0), y_right = numeric(0)))
  bind_rows(out)
}

save_fig <- function(p, name, width, height) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p, width = width, height = height, dpi = 300, bg = "white")
  ggsave(file.path(FIG_DIR, paste0(name, ".pdf")), p, width = width, height = height, bg = "white", device = cairo_pdf)
}

# ------------------------------------------------------------------------------
# 0. Reference numbers from the pipeline (for the §2.3 check)
# ------------------------------------------------------------------------------

paper_summary <- read_csv(file.path(BHAPC_DIR, "all_surveys_summary.csv"), show_col_types = FALSE) |>
  mutate(survey = toupper(survey))
stopifnot(all(SURVEY_ORDER %in% paper_summary$survey))

# ------------------------------------------------------------------------------
# 1 + 2. Per-survey: window statistics (from the exact BHAPC rows) and BHAPC
#        peak posterior (from the saved draws)
# ------------------------------------------------------------------------------

window_rows   <- list()
dp_by_list    <- list()
peak_rows     <- list()
draw_rows     <- list()
curve_rows    <- list()
varcheck_rows <- list()
runtime_rows  <- list()

for (sv in SURVEY_ORDER) {
  s <- tolower(sv)
  t0 <- Sys.time()
  message("\n==== ", sv, " ====")

  # ---- 1. exact analysis rows -------------------------------------------------
  d <- readRDS(file.path(BHAPC_DIR, s, paste0(s, "_bhapc_data.rds")))
  stopifnot(all(c("srh", "age", "year", "wt", "cohort_4yr", "period_4yr", "age_group") %in% names(d)))
  stopifnot(!anyNA(d$srh), !anyNA(d$age), !anyNA(d$year), !anyNA(d$wt))
  stopifnot(min(d$srh) >= 1, max(d$srh) <= SRH_SCALE[[sv]])
  # NOTE: some surveys carry rows with wt == 0 (NHIS ~51 %, NHANES ~7 %, CPS 2 rows);
  # prepare_bhapc_data() gave those lnWt = 0 and the published fits include them.
  # They stay in the UNWEIGHTED window statistics (they are rows the model saw);
  # the weights-only survey means below necessarily use the wt > 0 rows.
  stopifnot(all(d$wt >= 0))
  n_zero_wt <- sum(d$wt == 0)
  d <- d |> mutate(cohort_num = as.numeric(cohort_4yr), birth_year = year - age)
  stopifnot(!anyNA(d$cohort_num))

  # unweighted window statistics (what the unweighted-likelihood BHAPC "sees";
  # weights enter only via the lnWt covariate)
  p_bar <- mean(d$year); a_bar <- mean(d$age); c_bar <- mean(d$birth_year)
  stopifnot(abs(c_bar - (p_bar - a_bar)) < 1e-9)          # c-bar = p-bar - a-bar (exact)
  c_grid_bar  <- mean(d$cohort_num)                        # centre of the model's cohort grid
  grid_offset <- c_bar - c_grid_bar                        # birth-year scale = grid label + offset
  q_by   <- quantile(d$birth_year, c(0.05, 0.10, 0.90, 0.95))
  q_grid <- quantile(d$cohort_num, c(0.05, 0.10, 0.90, 0.95))

  # survey-weighted versions (weights-only design; wt is the (rescaled) analysis weight)
  des <- d |> filter(wt > 0) |> as_survey_design(weights = wt)
  w <- des |> summarise(p_bar_w = survey_mean(year, vartype = NULL),
                        a_bar_w = survey_mean(age, vartype = NULL),
                        c_bar_w = survey_mean(birth_year, vartype = NULL))
  stopifnot(abs(w$c_bar_w - (w$p_bar_w - w$a_bar_w)) < 1e-9)

  # proxy window for the APC-I sample: same rows, APC-I age filter
  ar <- if (s == "nhanes") APCI_AGE_RANGE$nhanes else APCI_AGE_RANGE$default
  dp <- d |> filter(age >= ar[1], age <= ar[2])
  q_by_p <- quantile(dp$birth_year, c(0.10, 0.90))
  dp_by_list[[sv]] <- dp$birth_year          # kept for the APC-I region sensitivities

  window_rows[[sv]] <- tibble(
    survey = sv, n_bhapc = nrow(d), n_zero_wt = n_zero_wt,
    year_min = min(d$year), year_max = max(d$year),
    age_min = min(d$age), age_max = max(d$age),
    n_cohort_bins = n_distinct(d$cohort_num),
    cohort_grid_min = min(d$cohort_num), cohort_grid_max = max(d$cohort_num),
    p_bar = p_bar, a_bar = a_bar, c_bar = c_bar,
    p_bar_w = w$p_bar_w, a_bar_w = w$a_bar_w, c_bar_w = w$c_bar_w,
    c_grid_bar = c_grid_bar, grid_offset = grid_offset,
    # second mechanical reference: centre of the observed cohort RANGE (level grid),
    # on the birth-year scale (grid label range midpoint + offset), and the
    # respondent birth-year range midpoint for comparison
    c_mid = (min(d$cohort_num) + max(d$cohort_num)) / 2 + grid_offset,
    by_min = min(d$birth_year), by_max = max(d$birth_year),
    by_mid = (min(d$birth_year) + max(d$birth_year)) / 2,
    by_q05 = q_by[[1]], by_q10 = q_by[[2]], by_q90 = q_by[[3]], by_q95 = q_by[[4]],
    grid_q05 = q_grid[[1]], grid_q10 = q_grid[[2]], grid_q90 = q_grid[[3]], grid_q95 = q_grid[[4]],
    apci_proxy_age_min = ar[1], apci_proxy_age_max = ar[2],
    n_apci_proxy = nrow(dp),
    p_bar_apci_proxy = mean(dp$year), a_bar_apci_proxy = mean(dp$age),
    c_bar_apci_proxy = mean(dp$birth_year),
    by_q10_apci_proxy = q_by_p[[1]], by_q90_apci_proxy = q_by_p[[2]]
  )
  message(sprintf("  n=%s  years %g-%g  p-bar=%.2f  a-bar=%.2f  c-bar=%.2f (wtd %.2f)  grid c-bar=%.2f  offset=%.2f",
                  format(nrow(d), big.mark = ","), min(d$year), max(d$year),
                  p_bar, a_bar, c_bar, w$c_bar_w, c_grid_bar, grid_offset))

  # ---- 2. saved fit: draws -----------------------------------------------------
  mr <- readRDS(file.path(BHAPC_DIR, s, paste0(s, "_bhapc_full_random_model.rds")))
  stopifnot(inherits(mr$model, "stanreg"), mr$model_type == "full_random",
            mr$n_obs == nrow(d))

  # (§2.3) re-derive the variance shares from the saved fit and compare with the
  # numbers behind the paper (all_surveys_summary.csv)
  vc <- extract_variance_components(mr$model)
  pct <- setNames(vc$pct_of_total, vc$component)
  ref <- paper_summary |> filter(survey == sv)
  varcheck_rows[[sv]] <- tibble(
    survey = sv,
    age_pct = pct[["age_group"]], period_pct = pct[["period_4yr"]], cohort_pct = pct[["cohort_4yr"]],
    age_pct_ref = ref$age_var_pct, period_pct_ref = ref$period_var_pct, cohort_pct_ref = ref$cohort_var_pct
  )
  stopifnot(abs(pct[["age_group"]]  - ref$age_var_pct)    < 0.06,
            abs(pct[["period_4yr"]] - ref$period_var_pct) < 0.06,
            abs(pct[["cohort_4yr"]] - ref$cohort_var_pct) < 0.06)
  message(sprintf("  variance shares reproduce: age %.2f / period %.2f / cohort %.2f (ref %.1f/%.1f/%.1f)",
                  pct[["age_group"]], pct[["period_4yr"]], pct[["cohort_4yr"]],
                  ref$age_var_pct, ref$period_var_pct, ref$cohort_var_pct))

  D_all <- as.matrix(mr$model)
  cc <- grep("^b\\[\\(Intercept\\) cohort_4yr:", colnames(D_all), value = TRUE)
  labs <- as.numeric(sub("^b\\[\\(Intercept\\) cohort_4yr:(.*)\\]$", "\\1", cc))
  stopifnot(!anyNA(labs), length(labs) == n_distinct(d$cohort_num))
  o <- order(labs); labs <- labs[o]; D <- D_all[, cc[o], drop = FALSE]
  n_draws <- nrow(D)
  rm(D_all)

  # draw means must equal the saved posterior-mean table (same object, no refit)
  csv <- read_csv(file.path(BHAPC_DIR, s, paste0(s, "_cohort_effects.csv")), show_col_types = FALSE) |>
    filter(!is.na(cohort)) |> arrange(cohort)             # drops the spurious Sigma[...] row
  stopifnot(identical(csv$cohort, labs))
  stopifnot(max(abs(colMeans(D) - csv$estimate)) < 1e-8)

  # bins per cohort (for the record + region definition on the grid)
  n_per_bin <- d |> count(cohort_num) |> arrange(cohort_num)
  stopifnot(identical(n_per_bin$cohort_num, labs))

  # per-draw argmax: over the primary central region (tail bins are identified from
  # a handful of extreme-age cells and are unstable); the unrestricted argmax is
  # kept for the record
  post_mean   <- colMeans(D)
  lim_p <- quantile(d$cohort_num, CENTRAL_REGIONS[[PRIMARY_REGION]])
  in_primary <- labs >= lim_p[[1]] & labs <= lim_p[[2]]
  argmax_draw     <- labs[in_primary][max.col(D[, in_primary, drop = FALSE], ties.method = "first")]
  argmax_draw_all <- labs[max.col(D, ties.method = "first")]
  argmax_pm       <- labs[in_primary][which.max(post_mean[in_primary])]
  argmax_pm_all   <- labs[which.max(post_mean)]

  # per-draw quadratic vertex over each region
  draw_tbl <- tibble(survey = sv, draw = seq_len(n_draws), argmax_grid = argmax_draw,
                     argmax_grid_unrestricted = argmax_draw_all)
  region_summ <- list()
  for (rn in names(CENTRAL_REGIONS)) {
    qq  <- CENTRAL_REGIONS[[rn]]
    lim <- quantile(d$cohort_num, qq)
    inr <- labs >= lim[[1]] & labs <= lim[[2]]
    stopifnot(sum(inr) >= MIN_BINS_FOR_PARABOLA)
    vf  <- fit_vertex(D[, inr, drop = FALSE], labs[inr])
    vpm <- fit_vertex(post_mean[inr], labs[inr])
    draw_tbl[[paste0("vertex_grid_", rn)]] <- vf$vertex
    draw_tbl[[paste0("b2_", rn)]]          <- vf$b2
    draw_tbl[[paste0("concave_", rn)]]     <- vf$concave
    vtx_conc <- vf$vertex[vf$concave]
    region_summ[[rn]] <- bind_cols(
      tibble(region = rn, region_lo_grid = lim[[1]], region_hi_grid = lim[[2]], n_bins_region = sum(inr),
             p_concave = mean(vf$concave), b2_median = median(vf$b2),
             b2_q05 = unname(quantile(vf$b2, 0.05)), b2_q95 = unname(quantile(vf$b2, 0.95)),
             frac_vertex_inside_region = mean(vtx_conc >= lim[[1]] & vtx_conc <= lim[[2]]),
             vertex_pm_grid = vpm$vertex, b2_pm = vpm$b2, concave_pm = vpm$concave),
      q_summ(vtx_conc, "vertex_grid")
    )
  }
  # local rule (argmax +/- LOCAL_HALF_WIDTH bins), per draw and for the posterior mean
  lv  <- fit_local_vertex(D, labs, search = in_primary)
  lpm <- fit_local_vertex(post_mean, labs, search = in_primary)
  draw_tbl[["vertex_grid_local"]] <- lv$vertex
  draw_tbl[["b2_local"]]          <- lv$b2
  draw_tbl[["concave_local"]]     <- lv$concave
  lv_conc <- lv$vertex[lv$concave %in% TRUE]
  region_summ[["local"]] <- bind_cols(
    tibble(region = "local", region_lo_grid = NA_real_, region_hi_grid = NA_real_,
           n_bins_region = 2 * LOCAL_HALF_WIDTH + 1,
           p_concave = mean(lv$concave %in% TRUE), b2_median = median(lv$b2, na.rm = TRUE),
           b2_q05 = unname(quantile(lv$b2, 0.05, na.rm = TRUE)), b2_q95 = unname(quantile(lv$b2, 0.95, na.rm = TRUE)),
           frac_vertex_inside_region = NA_real_,
           vertex_pm_grid = lpm$vertex, b2_pm = lpm$b2, concave_pm = lpm$concave),
    q_summ(lv_conc, "vertex_grid")
  )
  region_summ <- bind_rows(region_summ)

  peak_rows[[sv]] <- bind_cols(
    tibble(survey = sv, n_draws = n_draws, n_cohort_bins = length(labs),
           argmax_pm_grid = argmax_pm, argmax_pm_grid_unrestricted = argmax_pm_all,
           argmax_mode_grid = as.numeric(names(which.max(table(argmax_draw)))),
           p_argmax_unrestricted_in_region = mean(argmax_draw_all >= lim_p[[1]] & argmax_draw_all <= lim_p[[2]])),
    q_summ(argmax_draw, "argmax_grid"),
    q_summ(argmax_draw_all, "argmax_grid_unrestricted")
  ) |>
    cross_join(region_summ) |>
    mutate(grid_offset = grid_offset, c_bar = c_bar, c_grid_bar = c_grid_bar)

  draw_rows[[sv]] <- draw_tbl

  # posterior-mean curve for the companion figure (birth-year scale + c-bar aligned)
  curve_rows[[sv]] <- csv |>
    transmute(survey = sv, cohort_grid = cohort, estimate, ci_lower_90, ci_upper_90,
              n_bin = n_per_bin$n,
              birth_year = cohort_grid + grid_offset,
              rel_to_cbar = birth_year - c_bar,
              in_primary_region = cohort_grid >= quantile(d$cohort_num, CENTRAL_REGIONS[[PRIMARY_REGION]][1]) &
                                  cohort_grid <= quantile(d$cohort_num, CENTRAL_REGIONS[[PRIMARY_REGION]][2]))

  pr <- region_summ |> filter(region == PRIMARY_REGION)
  message(sprintf("  BHAPC peak (grid): argmax median %.0f [%.0f, %.0f]; vertex(%s) median %.1f [%.1f, %.1f], P(concave)=%.3f; post-mean argmax %.0f, vertex %.1f",
                  median(argmax_draw), quantile(argmax_draw, .05), quantile(argmax_draw, .95),
                  PRIMARY_REGION, pr$vertex_grid_median, pr$vertex_grid_q05, pr$vertex_grid_q95, pr$p_concave,
                  argmax_pm, pr$vertex_pm_grid))
  runtime_rows[[sv]] <- tibble(survey = sv, step = "load_fit_and_extract_peaks",
                               seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  rm(mr, D); invisible(gc())
}

window_stats   <- bind_rows(window_rows)
bhapc_peaks    <- bind_rows(peak_rows)
bhapc_draws    <- bind_rows(draw_rows)
bhapc_curves   <- bind_rows(curve_rows)
variance_check <- bind_rows(varcheck_rows)

# ------------------------------------------------------------------------------
# 3 + 4. APC-I: peak of inter-cohort deviations (panel C) and slope-flip (panel D)
# ------------------------------------------------------------------------------

apci_avgs <- read_csv(file.path(APCI_DIR, "apci_cohort_avgs_all.csv"), show_col_types = FALSE)
apci_slps <- read_csv(file.path(APCI_DIR, "apci_cohort_slopes_all.csv"), show_col_types = FALSE)
stopifnot(all(c("survey", "cohort_index", "cohort_midpoint", "estimate", "se") %in% names(apci_avgs)),
          all(c("survey", "cohort_index", "cohort_midpoint", "estimate", "se") %in% names(apci_slps)),
          setequal(unique(apci_avgs$survey), SURVEY_ORDER),
          setequal(unique(apci_slps$survey), SURVEY_ORDER))

# ---- APC-I cohort labels: verify against APCI's diagonal index ---------------
# History: on 2026-08-15 this analysis found that the saved APC-I cohort labels
# were built by joining APCI's diagonal index to distinct(cohort_group,
# cohort_midpoint), which mislabelled NHANES (irregular period midpoints; up to
# 29 y) and BRFSS (empty 80-84 bin -> positional age level 13 is 85-89; up to
# 5 y). The canonical CSVs on `main` were corrected the same day by
# main/R/scripts/12c_apci_fix_nhanes_labels.R (code fix: build_apci_cohort_lookup()
# and positional lookups in main/R/functions/apci_analysis.R). The as-filed copies
# are kept in inputs/apci/asfiled_20260213/. Here we only VERIFY that the labels
# in use equal the diagonal means in apci_cohort_diagonal_lookup.csv (written by
# 12c), so the script fails loudly if it is ever pointed at the old files.
diag_lookup <- read_csv(file.path(APCI_DIR, "apci_cohort_diagonal_lookup.csv"), show_col_types = FALSE)
verify_apci_labels <- function(df, what) {
  chk <- df |> select(survey, cohort_index, cohort_midpoint) |>
    inner_join(diag_lookup |> select(survey, cohort_index, diag_midpoint = cohort_midpoint),
               by = c("survey", "cohort_index"))
  stopifnot(nrow(chk) == nrow(df))
  bad <- chk |> filter(abs(cohort_midpoint - diag_midpoint) > 1e-6)
  if (nrow(bad) > 0) stop(what, ": ", nrow(bad), " cohort labels differ from APCI's diagonal means ",
                          "(surveys: ", paste(unique(bad$survey), collapse = ", "),
                          "). Are you using the as-filed (pre-2026-08-15) files?")
  invisible(TRUE)
}
verify_apci_labels(apci_avgs, "apci_cohort_avgs_all.csv")
verify_apci_labels(apci_slps, "apci_cohort_slopes_all.csv")
apci_avgs <- apci_avgs |> left_join(diag_lookup |> select(survey, cohort_index, n_cells), by = c("survey", "cohort_index"))
apci_slps <- apci_slps |> left_join(diag_lookup |> select(survey, cohort_index, n_cells), by = c("survey", "cohort_index"))

set.seed(BOOT_SEED)
apci_rows <- list(); crossing_rows <- list(); apci_curve_rows <- list()
for (sv in SURVEY_ORDER) {
  wst <- window_stats |> filter(survey == sv)
  a <- apci_avgs |> filter(survey == sv, !is.na(estimate), !is.na(cohort_midpoint)) |> arrange(cohort_midpoint)
  sl <- apci_slps |> filter(survey == sv, !is.na(estimate), !is.na(cohort_midpoint)) |> arrange(cohort_midpoint)
  stopifnot(nrow(a) >= MIN_BINS_FOR_PARABOLA)
  # bins are nominally 5 years apart; the diagonal means are irregular for NHANES
  # (period midpoints 2002/2007/2012/2017/2021) and BRFSS (empty 80-84 age bin),
  # so only monotonicity is asserted; +/- APCI_BIN_WIDTH remains the nominal
  # minimum uncertainty of an argmax bin
  spacing <- diff(a$cohort_midpoint)
  stopifnot(all(spacing > 0), all(spacing <= 2 * APCI_BIN_WIDTH + 1e-9))

  lo <- wst$by_q10_apci_proxy; hi <- wst$by_q90_apci_proxy       # central 80 % (proxy sample)
  inr <- a$cohort_midpoint >= lo & a$cohort_midpoint <= hi
  stopifnot(sum(inr) >= MIN_BINS_FOR_PARABOLA)

  argmax_c     <- a$cohort_midpoint[inr][which.max(a$estimate[inr])]   # within the central region
  argmax_c_all <- a$cohort_midpoint[which.max(a$estimate)]              # unrestricted (record only)
  vpt <- fit_vertex(a$estimate[inr], a$cohort_midpoint[inr])
  # region sensitivities mirroring the BHAPC rules
  lo90 <- quantile(dp_by_list[[sv]], 0.05); hi90 <- quantile(dp_by_list[[sv]], 0.95)
  inr90 <- a$cohort_midpoint >= lo90 & a$cohort_midpoint <= hi90
  vpt90  <- fit_vertex(a$estimate[inr90], a$cohort_midpoint[inr90])
  vptall <- fit_vertex(a$estimate, a$cohort_midpoint)
  # approximate uncertainty: independent-normal parametric bootstrap of the vertex
  E <- matrix(rnorm(N_BOOT * nrow(a), mean = rep(a$estimate, each = N_BOOT),
                    sd = rep(a$se, each = N_BOOT)), nrow = N_BOOT)       # N_BOOT x K
  vb <- fit_vertex(E[, inr, drop = FALSE], a$cohort_midpoint[inr])
  vb_conc <- vb$vertex[vb$concave]
  # local rule (argmax +/- LOCAL_HALF_WIDTH bins) on the point estimates + bootstrap
  vloc  <- fit_local_vertex(a$estimate, a$cohort_midpoint, search = inr)
  vlocb <- fit_local_vertex(E, a$cohort_midpoint, search = inr)
  vlocb_conc <- vlocb$vertex[vlocb$concave %in% TRUE]

  # slope-flip: interpolated crossings + linear-fit zero over the central region
  # (b) linear fit of slope on cohort over the central region -> zero crossing
  #     (the mechanical account predicts slope proportional to c - c-bar, i.e. linear)
  sl_r <- sl |> filter(cohort_midpoint >= lo, cohort_midpoint <= hi)
  stopifnot(nrow(sl_r) >= 3)
  lf <- lm(estimate ~ cohort_midpoint, data = sl_r)
  flip_linear <- unname(-coef(lf)[1] / coef(lf)[2])
  slope_of_slopes <- unname(coef(lf)[2])
  # (a) interpolated pos->neg crossing in the central region nearest the linear zero
  cr <- find_crossings(sl$cohort_midpoint, sl$estimate) |> mutate(survey = sv, .before = 1) |>
    mutate(in_central_region = crossing >= lo & crossing <= hi)
  crossing_rows[[sv]] <- cr
  cand <- cr |> filter(direction == "pos_to_neg", in_central_region)
  flip_interp <- if (nrow(cand) > 0) cand$crossing[which.min(abs(cand$crossing - flip_linear))] else NA_real_
  n_pos_to_neg_all <- sum(cr$direction == "pos_to_neg")

  apci_rows[[sv]] <- tibble(
    survey = sv, n_cohort_bins = nrow(a), bin_width = APCI_BIN_WIDTH,
    c_mid_apci = (min(a$cohort_midpoint) + max(a$cohort_midpoint)) / 2,   # APC-I level-grid centre
    region_lo = lo, region_hi = hi, n_bins_region = sum(inr),
    argmax = argmax_c, argmax_lo = argmax_c - APCI_BIN_WIDTH, argmax_hi = argmax_c + APCI_BIN_WIDTH,
    argmax_unrestricted = argmax_c_all,
    vertex = vpt$vertex, b2 = vpt$b2, concave = vpt$concave,
    vertex_central90 = vpt90$vertex, concave_central90 = vpt90$concave, n_bins_central90 = sum(inr90),
    vertex_all = vptall$vertex, concave_all = vptall$concave, b2_all = vptall$b2,
    vertex_boot_median = median(vb_conc), vertex_boot_q05 = unname(quantile(vb_conc, 0.05)),
    vertex_boot_q95 = unname(quantile(vb_conc, 0.95)), boot_p_concave = mean(vb$concave),
    vertex_local = vloc$vertex, b2_local = vloc$b2, concave_local = vloc$concave,
    n_bins_local = vloc$n_bins_window,
    vertex_local_boot_median = median(vlocb_conc),
    vertex_local_boot_q05 = unname(quantile(vlocb_conc, 0.05)),
    vertex_local_boot_q95 = unname(quantile(vlocb_conc, 0.95)),
    boot_p_concave_local = mean(vlocb$concave %in% TRUE),
    min_bin_spacing = min(spacing), median_bin_spacing = median(spacing),
    flip_interp = flip_interp, n_crossings_total = nrow(cr), n_pos_to_neg_total = n_pos_to_neg_all,
    n_pos_to_neg_in_region = nrow(cand),
    flip_linear = flip_linear, slope_of_slopes_per_yr = slope_of_slopes,
    flip_linear_inside_region = flip_linear >= lo & flip_linear <= hi
  )
  apci_curve_rows[[sv]] <- a |>
    transmute(survey = sv, cohort_index, cohort_midpoint, n_cells,
              estimate, ci_lower, ci_upper, se, p_value,
              rel_to_cbar = cohort_midpoint - wst$c_bar_apci_proxy, in_primary_region = inr)
  message(sprintf("  APC-I %s: argmax %.1f; vertex(central80) %.1f (concave=%s; boot 90%% [%.1f, %.1f]); vertex(local) %.1f (concave=%s); flip interp %.1f, linear %.1f",
                  sv, argmax_c, vpt$vertex, vpt$concave, quantile(vb_conc, .05), quantile(vb_conc, .95),
                  vloc$vertex, vloc$concave, flip_interp, flip_linear))
}
apci_peaks     <- bind_rows(apci_rows)
apci_crossings <- bind_rows(crossing_rows)
apci_curves    <- bind_rows(apci_curve_rows)

# ------------------------------------------------------------------------------
# 5. Summary table (one row per survey) — birth-year scale throughout
# ------------------------------------------------------------------------------

bp <- bhapc_peaks |> filter(region == PRIMARY_REGION)
bl <- bhapc_peaks |> filter(region == "local")
ba <- bhapc_peaks |> filter(region == "all")

hump_summary <- window_stats |>
  select(survey, n_bhapc, year_min, year_max, p_bar, a_bar, c_bar, c_bar_w, grid_offset, c_bar_apci_proxy,
         c_mid, by_min, by_max) |>
  left_join(bp |> transmute(
    survey,
    bhapc_argmax_median   = argmax_grid_median + grid_offset,
    bhapc_argmax_q05      = argmax_grid_q05 + grid_offset,
    bhapc_argmax_q95      = argmax_grid_q95 + grid_offset,
    bhapc_vertex_median   = vertex_grid_median + grid_offset,
    bhapc_vertex_q05      = vertex_grid_q05 + grid_offset,
    bhapc_vertex_q95      = vertex_grid_q95 + grid_offset,
    bhapc_p_concave       = p_concave,
    bhapc_b2_median       = b2_median,
    bhapc_argmax_unrestricted_median = argmax_grid_unrestricted_median + grid_offset,
    bhapc_p_argmax_unrestricted_in_region = p_argmax_unrestricted_in_region,
    bhapc_postmean_argmax = argmax_pm_grid + grid_offset,
    bhapc_postmean_vertex = vertex_pm_grid + grid_offset,
    bhapc_n_bins_region   = n_bins_region
  ), by = "survey") |>
  left_join(ba |> transmute(
    survey,
    bhapc_vertex_all_median = vertex_grid_median + grid_offset,
    bhapc_vertex_all_q05    = vertex_grid_q05 + grid_offset,
    bhapc_vertex_all_q95    = vertex_grid_q95 + grid_offset,
    bhapc_p_concave_all     = p_concave
  ), by = "survey") |>
  left_join(bl |> transmute(
    survey,
    bhapc_vertex_local_median = vertex_grid_median + grid_offset,
    bhapc_vertex_local_q05    = vertex_grid_q05 + grid_offset,
    bhapc_vertex_local_q95    = vertex_grid_q95 + grid_offset,
    bhapc_p_concave_local     = p_concave
  ), by = "survey") |>
  left_join(apci_peaks |> transmute(
    survey, c_mid_apci,
    apci_argmax = argmax, apci_argmax_lo = argmax_lo, apci_argmax_hi = argmax_hi,
    apci_argmax_unrestricted = argmax_unrestricted,
    apci_vertex = vertex, apci_vertex_concave = concave,
    apci_vertex_boot_q05 = vertex_boot_q05, apci_vertex_boot_q95 = vertex_boot_q95,
    apci_vertex_all = vertex_all, apci_vertex_all_concave = concave_all,
    apci_vertex_central90 = vertex_central90, apci_vertex_central90_concave = concave_central90,
    apci_vertex_local = vertex_local, apci_vertex_local_concave = concave_local,
    apci_vertex_local_boot_q05 = vertex_local_boot_q05, apci_vertex_local_boot_q95 = vertex_local_boot_q95,
    apci_flip_interp = flip_interp, apci_flip_linear = flip_linear
  ), by = "survey") |>
  mutate(
    ref_boomer = BOOMER_REF,
    d_bhapc_vertex_cbar  = bhapc_vertex_median - c_bar,
    d_bhapc_vertex_1955  = bhapc_vertex_median - BOOMER_REF,
    d_bhapc_argmax_cbar  = bhapc_argmax_median - c_bar,
    d_bhapc_argmax_1955  = bhapc_argmax_median - BOOMER_REF,
    d_bhapc_vlocal_cbar  = bhapc_vertex_local_median - c_bar,
    d_bhapc_vlocal_1955  = bhapc_vertex_local_median - BOOMER_REF,
    d_bhapc_vall_cbar    = bhapc_vertex_all_median - c_bar,
    d_bhapc_vall_1955    = bhapc_vertex_all_median - BOOMER_REF,
    d_apci_vall_cbar     = if_else(apci_vertex_all_concave, apci_vertex_all - c_bar_apci_proxy, NA_real_),
    d_apci_vall_1955     = if_else(apci_vertex_all_concave, apci_vertex_all - BOOMER_REF, NA_real_),
    d_apci_vertex_cbar   = if_else(apci_vertex_concave, apci_vertex - c_bar_apci_proxy, NA_real_),
    d_apci_vertex_1955   = if_else(apci_vertex_concave, apci_vertex - BOOMER_REF, NA_real_),
    d_apci_vlocal_cbar   = if_else(apci_vertex_local_concave, apci_vertex_local - c_bar_apci_proxy, NA_real_),
    d_apci_vlocal_1955   = if_else(apci_vertex_local_concave, apci_vertex_local - BOOMER_REF, NA_real_),
    d_apci_argmax_cbar   = apci_argmax - c_bar_apci_proxy,
    d_apci_argmax_1955   = apci_argmax - BOOMER_REF,
    d_flip_interp_cbar   = apci_flip_interp - c_bar_apci_proxy,
    d_flip_linear_cbar   = apci_flip_linear - c_bar_apci_proxy,
    # distances from the grid-centre reference
    d_bhapc_vertex_cmid  = bhapc_vertex_median - c_mid,
    d_bhapc_argmax_cmid  = bhapc_argmax_median - c_mid,
    d_bhapc_vlocal_cmid  = bhapc_vertex_local_median - c_mid,
    d_bhapc_vall_cmid    = bhapc_vertex_all_median - c_mid,
    d_apci_vall_cmid     = if_else(apci_vertex_all_concave, apci_vertex_all - c_mid_apci, NA_real_),
    d_apci_argmax_cmid   = apci_argmax - c_mid_apci,
    d_apci_vertex_cmid   = if_else(apci_vertex_concave, apci_vertex - c_mid_apci, NA_real_),
    d_apci_vlocal_cmid   = if_else(apci_vertex_local_concave, apci_vertex_local - c_mid_apci, NA_real_),
    d_flip_interp_cmid   = apci_flip_interp - c_mid_apci,
    d_flip_linear_cmid   = apci_flip_linear - c_mid_apci,
    bhapc_cri_covers_cmid = c_mid >= bhapc_vertex_q05 & c_mid <= bhapc_vertex_q95,
    bhapc_cri_covers_cbar = c_bar >= bhapc_vertex_q05 & c_bar <= bhapc_vertex_q95,
    bhapc_cri_covers_1955 = BOOMER_REF >= bhapc_vertex_q05 & BOOMER_REF <= bhapc_vertex_q95,
    # placeholders to be filled by Task 2 (null-simulation peaks)
    null_apci_peak_median = NA_real_, null_bhapc_peak = NA_real_
  ) |>
  mutate(survey = factor(survey, levels = SURVEY_ORDER)) |> arrange(survey) |> mutate(survey = as.character(survey))

# cross-survey "tracking" statistics per peak measure: regression slope of the peak
# on each window reference (b ~ 1: tracks the window; b ~ 0: fixed), and the
# cross-survey SD of (peak - reference) — small SD = the peak sits at a common
# distance from that reference in every survey (the reference explains the
# between-survey variation), whichever the constant offset is.
track_one <- function(label, y, x_cbar, x_cmid) {
  ok <- is.finite(y)
  y <- y[ok]; x_cbar <- x_cbar[ok]; x_cmid <- x_cmid[ok]
  m1 <- lm(y ~ x_cbar); m2 <- lm(y ~ x_cmid)
  tibble(peak_measure = label, n_surveys = length(y),
         mean_peak = mean(y), sd_peak = sd(y), range_peak = diff(range(y)),
         mean_offset_cbar = mean(y - x_cbar), sd_offset_cbar = sd(y - x_cbar),
         mean_offset_cmid = mean(y - x_cmid), sd_offset_cmid = sd(y - x_cmid),
         mean_offset_1955 = mean(y - BOOMER_REF), sd_offset_1955 = sd(y - BOOMER_REF),
         slope_cbar = unname(coef(m1)[2]), slope_cbar_se = unname(sqrt(diag(vcov(m1)))[2]), r2_cbar = summary(m1)$r.squared,
         slope_cmid = unname(coef(m2)[2]), slope_cmid_se = unname(sqrt(diag(vcov(m2)))[2]), r2_cmid = summary(m2)$r.squared,
         sd_cbar = sd(x_cbar), sd_cmid = sd(x_cmid))
}
H <- hump_summary
peak_tracking <- bind_rows(
  track_one("BHAPC argmax (posterior median)",              H$bhapc_argmax_median,       H$c_bar, H$c_mid),
  track_one("BHAPC vertex, central-80% (posterior median)", H$bhapc_vertex_median,       H$c_bar, H$c_mid),
  track_one("BHAPC vertex, all bins (posterior median)",    H$bhapc_vertex_all_median,   H$c_bar, H$c_mid),
  track_one("BHAPC vertex, local ±3 bins (posterior median)", H$bhapc_vertex_local_median, H$c_bar, H$c_mid),
  track_one("APC-I argmax bin",                             H$apci_argmax,               H$c_bar_apci_proxy, H$c_mid_apci),
  track_one("APC-I vertex, central-80% (concave fits only)", if_else(H$apci_vertex_concave, H$apci_vertex, NA_real_), H$c_bar_apci_proxy, H$c_mid_apci),
  track_one("APC-I vertex, all bins (concave fits only)",   if_else(H$apci_vertex_all_concave, H$apci_vertex_all, NA_real_), H$c_bar_apci_proxy, H$c_mid_apci),
  track_one("APC-I vertex, local ±3 bins",                  if_else(H$apci_vertex_local_concave, H$apci_vertex_local, NA_real_), H$c_bar_apci_proxy, H$c_mid_apci),
  track_one("APC-I slope-flip (interpolated)",              H$apci_flip_interp,          H$c_bar_apci_proxy, H$c_mid_apci),
  track_one("APC-I slope-flip (linear fit)",                H$apci_flip_linear,          H$c_bar_apci_proxy, H$c_mid_apci)
)

# ------------------------------------------------------------------------------
# Save tables
# ------------------------------------------------------------------------------

write_csv(hump_summary,     file.path(OUT_DIR, "hump_location_summary.csv"))
write_csv(window_stats,     file.path(OUT_DIR, "window_stats.csv"))
write_csv(bhapc_peaks,      file.path(OUT_DIR, "bhapc_peak_posterior.csv"))
write_csv(bhapc_curves,     file.path(OUT_DIR, "bhapc_cohort_curves.csv"))
write_csv(apci_peaks,       file.path(OUT_DIR, "apci_peak_summary.csv"))
write_csv(apci_crossings,   file.path(OUT_DIR, "apci_slope_crossings.csv"))
write_csv(apci_curves,      file.path(OUT_DIR, "apci_cohort_curves.csv"))
write_csv(variance_check,   file.path(OUT_DIR, "variance_share_check.csv"))
write_csv(peak_tracking,    file.path(OUT_DIR, "peak_tracking.csv"))
saveRDS(bhapc_draws,        file.path(OUT_DIR, "bhapc_peak_draws.rds"))

# ------------------------------------------------------------------------------
# 6. Figures
# ------------------------------------------------------------------------------

theme_diag <- function(base_size = 12) {
  theme_srh(base_size = base_size) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}

# ---- Fig 1 (key): observed peak vs c-bar ------------------------------------
fig1_df <- bind_rows(
  hump_summary |> transmute(survey, method = "BHAPC (quadratic vertex, posterior median, 90% CrI)",
                            x = c_bar, y = bhapc_vertex_median, lo = bhapc_vertex_q05, hi = bhapc_vertex_q95),
  hump_summary |> transmute(survey, method = "APC-I (argmax bin, ± one 5-yr bin width)",
                            x = c_bar_apci_proxy, y = apci_argmax, lo = apci_argmax_lo, hi = apci_argmax_hi)
) |> mutate(survey = factor(survey, levels = SURVEY_ORDER),
            method = factor(method, levels = c("BHAPC (quadratic vertex, posterior median, 90% CrI)",
                                               "APC-I (argmax bin, ± one 5-yr bin width)")))
fig1b_df <- bind_rows(
  hump_summary |> transmute(survey, method = "BHAPC (quadratic vertex, posterior median, 90% CrI)",
                            x = c_mid, y = bhapc_vertex_median, lo = bhapc_vertex_q05, hi = bhapc_vertex_q95),
  hump_summary |> transmute(survey, method = "APC-I (argmax bin, ± one 5-yr bin width)",
                            x = c_mid_apci, y = apci_argmax, lo = apci_argmax_lo, hi = apci_argmax_hi)
) |> mutate(survey = factor(survey, levels = SURVEY_ORDER), method = factor(method, levels = levels(fig1_df$method)))
rng <- range(c(fig1_df$x, fig1_df$lo, fig1_df$hi, fig1b_df$x, BOOMER_REF), na.rm = TRUE) + c(-2, 2)
mk_fig1_panel <- function(df, xlab, title, tag) ggplot(df, aes(x = x, y = y, colour = survey, shape = method)) +
  geom_abline(slope = 1, intercept = 0, colour = "gray35", linewidth = 0.7) +
  geom_hline(yintercept = BOOMER_REF, colour = "gray35", linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = rng[1] + 0.5, y = BOOMER_REF + 0.8, hjust = 0, size = 3.4, colour = "gray30",
           label = "fixed-generation reference: peak = 1955") +
  annotate("text", x = rng[1] + 5, y = rng[1] + 6.2, hjust = 0, size = 3.4, colour = "gray30", angle = 45,
           label = "mechanical reference: peak = window centre") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.6, linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 3.2, stroke = 1.1, fill = "white") +
  geom_text(data = df |> filter(grepl("BHAPC", method)) |>
              mutate(ny = case_when(survey == "NHANES" ~ 0.9, survey == "CPS" ~ -0.9, TRUE ~ 0)),
            aes(label = survey, y = y + ny), nudge_x = 0.9, hjust = 0, size = 3.4, show.legend = FALSE) +
  scale_colour_manual(values = survey_colors_local, name = NULL) +
  scale_shape_manual(values = c(16, 2), name = NULL) +
  coord_equal(xlim = rng, ylim = rng) +
  labs(x = xlab, y = "Estimated cohort-hump peak (birth year)", title = title, tag = tag) +
  guides(colour = "none", shape = guide_legend(nrow = 2)) +
  theme_diag(11)
fig1 <- (mk_fig1_panel(fig1_df, "Window centre  c̄ = p̄ − ā  (mean birth year of the analysis sample)",
                       "Reference: mean birth year of the sample", "A") |
         mk_fig1_panel(fig1b_df, "Centre of the observed cohort range  (min + max birth-year bin) / 2",
                       "Reference: centre of the observed cohort range", "B")) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Does the cohort hump peak track each survey's data window (identity line) or a fixed generation (dashed line at 1955)?",
    subtitle = "One point per survey and method. BHAPC: posterior median of the per-draw quadratic-vertex peak with 90% CrI. APC-I: argmax bin ± one 5-yr bin (both over the central-80% cohort region).",
    theme = theme(plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(size = 10, colour = "gray30"))) &
  theme(legend.position = "bottom")
save_fig(fig1, "fig1_peak_vs_cbar", width = 14, height = 8)

# ---- Fig 2 (companion): six cohort curves aligned on c - c-bar vs raw birth year ----
bc <- bhapc_curves |> mutate(survey = factor(survey, levels = SURVEY_ORDER))
ac <- apci_curves  |> mutate(survey = factor(survey, levels = SURVEY_ORDER))
mk_curve_panel <- function(df, xvar, ymin, ymax, xlab, title, vline_at, vline_lab) {
  ggplot(df, aes(x = .data[[xvar]], y = estimate, colour = survey, fill = survey, group = survey)) +
    geom_hline(yintercept = 0, colour = "gray60", linetype = "dotted") +
    geom_vline(xintercept = vline_at, colour = "gray35", linetype = "dashed") +
    annotate("text", x = vline_at, y = Inf, vjust = 1.4, hjust = -0.05, size = 3.2, colour = "gray30", label = vline_lab) +
    geom_ribbon(aes(ymin = .data[[ymin]], ymax = .data[[ymax]]), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    scale_colour_manual(values = survey_colors_local, name = NULL) +
    scale_fill_manual(values = survey_colors_local, name = NULL) +
    labs(x = xlab, y = "Cohort effect (SRH units)", title = title) +
    theme_diag(11)
}
f2a <- mk_curve_panel(bc, "rel_to_cbar", "ci_lower_90", "ci_upper_90",
                      "Birth year − c̄ (survey-specific centring)", "BHAPC cohort effects, aligned on c − c̄", 0, "c̄")
f2b <- mk_curve_panel(bc, "birth_year", "ci_lower_90", "ci_upper_90",
                      "Birth year", "BHAPC cohort effects, raw birth year", BOOMER_REF, "1955")
f2c <- mk_curve_panel(ac, "rel_to_cbar", "ci_lower", "ci_upper",
                      "Birth year − c̄ (APC-I proxy centring)", "APC-I inter-cohort deviations, aligned on c − c̄", 0, "c̄")
f2d <- mk_curve_panel(ac, "cohort_midpoint", "ci_lower", "ci_upper",
                      "Birth year", "APC-I inter-cohort deviations, raw birth year", BOOMER_REF, "1955")
fig2 <- (f2a | f2b) / (f2c | f2d) + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Cohort curves from all six surveys: aligned on each survey's window centre vs on raw birth year",
    subtitle = paste0("Under the mechanical account the curves should approximately superimpose when aligned on c − c̄ (left) but not on raw birth year (right);\n",
                      "the reverse under a fixed-generation account. BHAPC: posterior mean ± 90% CrI (4-yr bins, birth-year scale). APC-I: estimate ± 95% CI (5-yr bins)."),
    theme = theme(plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(size = 10, colour = "gray30"))) &
  theme(legend.position = "bottom")
save_fig(fig2, "fig2_cohort_curves_aligned", width = 13, height = 9.5)

# ---- Fig 3 (diagnostic): per-survey curve + parabola + peak posterior -------
para_rows <- list()
for (sv in SURVEY_ORDER) {
  cur <- bhapc_curves |> filter(survey == sv, in_primary_region)
  vf  <- fit_vertex(cur$estimate, cur$birth_year)
  x0  <- mean(cur$birth_year)
  # reconstruct the fitted parabola: y = b0 + b1 (x - x0) + b2 (x - x0)^2 ; b0 from mean of residual-free fit
  X   <- cbind(1, cur$birth_year - x0, (cur$birth_year - x0)^2)
  b   <- solve(crossprod(X), crossprod(X, cur$estimate))
  xs  <- seq(min(cur$birth_year) - 4, max(cur$birth_year) + 4, length.out = 100)
  para_rows[[sv]] <- tibble(survey = sv, x = xs, y = b[1] + b[2] * (xs - x0) + b[3] * (xs - x0)^2)
}
para <- bind_rows(para_rows) |> mutate(survey = factor(survey, levels = SURVEY_ORDER))
lines_df <- hump_summary |>
  transmute(survey, c_bar,
            v_med = bhapc_vertex_median, v_lo = bhapc_vertex_q05, v_hi = bhapc_vertex_q95) |>
  left_join(window_stats |> transmute(survey, reg_lo = by_q10, reg_hi = by_q90), by = "survey") |>
  mutate(survey = factor(survey, levels = SURVEY_ORDER))
f3_top <- ggplot(bc, aes(x = birth_year, y = estimate)) +
  geom_rect(data = lines_df, aes(xmin = reg_lo, xmax = reg_hi, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "gray92") +
  geom_hline(yintercept = 0, colour = "gray60", linetype = "dotted") +
  geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90), width = 0, colour = "#CC79A7", alpha = 0.7) +
  geom_point(colour = "#CC79A7", size = 1.5) +
  geom_line(data = para, aes(x = x, y = y), colour = "black", linewidth = 0.7) +
  geom_vline(data = lines_df, aes(xintercept = v_med), colour = "black", linewidth = 0.8) +
  geom_vline(data = lines_df, aes(xintercept = v_lo), colour = "black", linetype = "dotted") +
  geom_vline(data = lines_df, aes(xintercept = v_hi), colour = "black", linetype = "dotted") +
  geom_vline(data = lines_df, aes(xintercept = c_bar), colour = "#0072B2", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = BOOMER_REF, colour = "#D55E00", linetype = "longdash", linewidth = 0.7) +
  facet_wrap(~ survey, ncol = 6) +
  scale_x_continuous(breaks = seq(1900, 2000, 25)) +
  labs(x = NULL, y = "BHAPC cohort effect (SRH units)",
       title = "Per-survey BHAPC cohort curve (posterior mean, 90% CrI), fitted parabola over the central-80% region (shaded), and peak location",
       subtitle = "black solid = posterior-median vertex, dotted = 90% CrI; blue dashed = c̄ (window centre); orange long-dash = 1955") +
  theme_diag(10) + theme(strip.text = element_text(face = "bold"))
vd <- bhapc_draws |>
  select(survey, v = !!sym(paste0("vertex_grid_", PRIMARY_REGION)), conc = !!sym(paste0("concave_", PRIMARY_REGION))) |>
  filter(conc) |>
  left_join(window_stats |> select(survey, grid_offset), by = "survey") |>
  mutate(v = v + grid_offset, survey = factor(survey, levels = SURVEY_ORDER))
xr <- range(bc$birth_year)
f3_bot <- ggplot(vd, aes(x = v)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "gray55", colour = NA, boundary = 0) +
  geom_vline(data = lines_df, aes(xintercept = c_bar), colour = "#0072B2", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = BOOMER_REF, colour = "#D55E00", linetype = "longdash", linewidth = 0.7) +
  facet_wrap(~ survey, ncol = 6) +
  scale_x_continuous(breaks = seq(1900, 2000, 25)) +
  coord_cartesian(xlim = xr) +
  labs(x = "Birth year", y = "Posterior density of vertex", subtitle = "Per-draw quadratic-vertex peak (concave draws only)") +
  theme_diag(10) + theme(strip.text = element_blank())
fig3 <- f3_top / f3_bot + plot_layout(heights = c(2, 1))
save_fig(fig3, "fig3_vertex_fit_diagnostics", width = 16, height = 7)

# ---- Fig 4 (supplementary): every peak measure minus c-bar and minus 1955 ------
delta_df <- hump_summary |>
  transmute(survey,
            `BHAPC argmax (post. median)`        = d_bhapc_argmax_cbar,
            `BHAPC vertex, central-80% (post. median)` = d_bhapc_vertex_cbar,
            `BHAPC vertex, all bins (post. median)` = d_bhapc_vall_cbar,
            `BHAPC vertex, local ±3 bins (post. median)` = d_bhapc_vlocal_cbar,
            `APC-I argmax bin`                    = d_apci_argmax_cbar,
            `APC-I vertex, central-80%`           = d_apci_vertex_cbar,
            `APC-I vertex, all bins`              = d_apci_vall_cbar,
            `APC-I vertex, local ±3 bins`         = d_apci_vlocal_cbar,
            `APC-I slope-flip (interpolated)`     = d_flip_interp_cbar,
            `APC-I slope-flip (linear fit)`       = d_flip_linear_cbar) |>
  pivot_longer(-survey, names_to = "measure", values_to = "d_cbar") |>
  left_join(
    hump_summary |>
      transmute(survey,
                `BHAPC argmax (post. median)`        = d_bhapc_argmax_1955,
                `BHAPC vertex, central-80% (post. median)` = d_bhapc_vertex_1955,
                `BHAPC vertex, all bins (post. median)` = d_bhapc_vall_1955,
                `BHAPC vertex, local ±3 bins (post. median)` = d_bhapc_vlocal_1955,
                `APC-I argmax bin`                    = d_apci_argmax_1955,
                `APC-I vertex, central-80%`           = d_apci_vertex_1955,
                `APC-I vertex, all bins`              = d_apci_vall_1955,
                `APC-I vertex, local ±3 bins`         = d_apci_vlocal_1955,
                `APC-I slope-flip (interpolated)`     = apci_flip_interp - BOOMER_REF,
                `APC-I slope-flip (linear fit)`       = apci_flip_linear - BOOMER_REF) |>
      pivot_longer(-survey, names_to = "measure", values_to = "d_1955"),
    by = c("survey", "measure")) |>
  left_join(
    hump_summary |>
      transmute(survey,
                `BHAPC argmax (post. median)`        = d_bhapc_argmax_cmid,
                `BHAPC vertex, central-80% (post. median)` = d_bhapc_vertex_cmid,
                `BHAPC vertex, all bins (post. median)` = d_bhapc_vall_cmid,
                `BHAPC vertex, local ±3 bins (post. median)` = d_bhapc_vlocal_cmid,
                `APC-I argmax bin`                    = d_apci_argmax_cmid,
                `APC-I vertex, central-80%`           = d_apci_vertex_cmid,
                `APC-I vertex, all bins`              = d_apci_vall_cmid,
                `APC-I vertex, local ±3 bins`         = d_apci_vlocal_cmid,
                `APC-I slope-flip (interpolated)`     = d_flip_interp_cmid,
                `APC-I slope-flip (linear fit)`       = d_flip_linear_cmid) |>
      pivot_longer(-survey, names_to = "measure", values_to = "d_cmid"),
    by = c("survey", "measure")) |>
  pivot_longer(c(d_cbar, d_cmid, d_1955), names_to = "reference", values_to = "delta") |>
  mutate(reference = factor(reference, levels = c("d_cbar", "d_cmid", "d_1955"),
                            labels = c("peak − c̄  (mean birth year)", "peak − centre of cohort range", "peak − 1955  (fixed generation)")),
         measure = factor(measure, levels = rev(unique(measure))),
         survey = factor(survey, levels = SURVEY_ORDER),
         method = if_else(grepl("^BHAPC", measure), "BHAPC", "APC-I"))
fig4 <- ggplot(delta_df, aes(x = delta, y = measure, colour = survey, shape = survey)) +
  geom_vline(xintercept = 0, colour = "gray35", linewidth = 0.8) +
  geom_vline(xintercept = c(-5, 5), colour = "gray70", linetype = "dotted") +
  geom_point(size = 2.8, position = position_dodge(width = 0.55), stroke = 1) +
  scale_colour_manual(values = survey_colors_local, name = NULL) +
  scale_shape_manual(values = survey_shapes[SURVEY_ORDER], name = NULL) +
  facet_wrap(~ reference, ncol = 3) +
  labs(x = "Years", y = NULL,
       title = "Every peak / flip measure, as distance from two window-based references (left, middle) and from 1955 (right)",
       subtitle = "Values near 0 on the left/middle = mechanical fingerprint; near 0 on the right = fixed generation. Dotted lines at ± 5 years (≈ one cohort bin).") +
  theme_diag(11) + theme(strip.text = element_text(face = "bold"), panel.grid.major.y = element_line(colour = "gray92"))
save_fig(fig4, "fig4_peak_offsets_dotplot", width = 15, height = 7)

# ------------------------------------------------------------------------------
# Runtime / session record
# ------------------------------------------------------------------------------

runtime <- bind_rows(runtime_rows) |>
  bind_rows(tibble(survey = "ALL", step = "whole_script",
                   seconds = as.numeric(difftime(Sys.time(), t_script_start, units = "secs"))))
write_csv(runtime, file.path(OUT_DIR, "runtimes.csv"))
writeLines(c(
  paste0("Task 1 quick tier run: ", format(t_script_start, "%Y-%m-%d %H:%M:%S %Z"), " -> ",
         format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("Seeds: BOOT_SEED=", BOOT_SEED, " (APC-I vertex parametric bootstrap only; BHAPC peak extraction is deterministic given the saved draws)"),
  paste0("Primary vertex region: ", PRIMARY_REGION, " (", paste(CENTRAL_REGIONS[[PRIMARY_REGION]], collapse = "-"), " quantiles of the respondent cohort distribution)"),
  "", capture.output(sessionInfo())
), file.path(OUT_DIR, "run_info.txt"))

message("\nDone. Summary table:")
print(hump_summary |> select(survey, year_min, year_max, c_bar, bhapc_vertex_median, bhapc_vertex_q05, bhapc_vertex_q95,
                             apci_vertex, apci_flip_linear, d_bhapc_vertex_cbar, d_bhapc_vertex_1955), width = 200)
print(peak_tracking |> select(peak_measure, n_surveys, mean_offset_cbar, sd_offset_cbar, mean_offset_cmid, sd_offset_cmid, mean_offset_1955, sd_offset_1955, slope_cbar, slope_cbar_se, slope_cmid, slope_cmid_se), width = 250)
