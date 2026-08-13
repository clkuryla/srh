# ==============================================================================
# 03b_hapc_nakagawa_decomposition.R
#
# Reconstructs output/apc/tables/variance_decomposition_complete.csv — the
# frequentist HAPC variance shares plotted in the manuscript's S4.3 figures
# (04c_hapc_random_effects_summary.R and 04d_bhapc_random_effects_summary.R).
#
# Provenance: the committed CSV (f52d3b9, extended 6ab5472, 2026-01-28) was
# originally produced in an interactive session and had no generating script.
# Its derivation is a Nakagawa R^2 decomposition of the HAPC models fit by
# 03_apc_analysis.R (verified arithmetically against the committed values):
#
#   age_pct      = R2_marginal * 100          (fixed age_group effects)
#   period_pct   = (R2_conditional - R2_marginal) * 100 * period_var / (period_var + cohort_var)
#   cohort_pct   = (R2_conditional - R2_marginal) * 100 * cohort_var / (period_var + cohort_var)
#   residual_pct = 100 - age_pct - period_pct - cohort_pct
#
# which is algebraically each component's variance over the Nakagawa total
# (fixed + random + residual; pi^2/3 distribution-specific variance for the
# binomial models). R2 via performance::r2_nakagawa().
#
# Usage:
#   Rscript R/scripts/03b_hapc_nakagawa_decomposition.R            # all surveys
#   Rscript R/scripts/03b_hapc_nakagawa_decomposition.R nhis meps  # subset
#
# For each survey it uses the saved model objects
# output/apc/<survey>_results_{continuous,binary}.rds when present, and
# otherwise refits the HAPC models via the functions in 03_apc_analysis.R
# (same data path, filters, subsample fractions, and seeds).
#
# Output: output/apc/tables/variance_decomposition_complete.csv if that file
# does not exist yet; otherwise writes ..._reconstructed.csv alongside it and
# prints a cell-by-cell comparison, so the committed artifact is never
# silently overwritten.
# ==============================================================================

library(here)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(lme4)

# Sources functions + SURVEY_META/SURVEY_ORDER only; 03's run call is commented out
source(here::here("R", "scripts", "03_apc_analysis.R"))

if (!requireNamespace("performance", quietly = TRUE)) {
  stop("Package 'performance' is required: install.packages(\"performance\")")
}

# Row order of the committed CSV
CSV_SURVEY_ORDER <- c("gss", "nhanes", "meps", "brfss", "nhis", "cps")

TABLES_DIR <- here::here("output", "apc", "tables")
CANONICAL_CSV <- file.path(TABLES_DIR, "variance_decomposition_complete.csv")

#' One CSV row from a fitted HAPC model
#'
#' @param model lmerMod (continuous) or glmerMod (binary) from fit_hapc_model()
#' @param survey_label Upper-case survey name for the CSV
#' @param outcome_label "continuous" or "binary"
nakagawa_row <- function(model, survey_label, outcome_label) {
  r2 <- performance::r2_nakagawa(model)
  r2_marginal <- as.numeric(r2$R2_marginal)
  r2_conditional <- as.numeric(r2$R2_conditional)

  vc <- as.data.frame(VarCorr(model))
  period_var <- vc$vcov[vc$grp == "period"]
  cohort_var <- vc$vcov[vc$grp == "cohort_group"]

  re_pct <- (r2_conditional - r2_marginal) * 100
  age_pct <- r2_marginal * 100
  period_pct <- re_pct * period_var / (period_var + cohort_var)
  cohort_pct <- re_pct * cohort_var / (period_var + cohort_var)

  tibble(
    survey = survey_label,
    outcome = outcome_label,
    age_pct = age_pct,
    period_pct = period_pct,
    cohort_pct = cohort_pct,
    residual_pct = 100 - age_pct - period_pct - cohort_pct,
    r2_marginal = r2_marginal,
    r2_conditional = r2_conditional,
    period_var = period_var,
    cohort_var = cohort_var
  )
}

#' Retrieve (or refit) the two HAPC models for one survey
#'
#' Prefers the saved results objects from 03_apc_analysis.R; falls back to a
#' minimal refit (data load -> structure -> subsample -> fit_hapc_model) that
#' skips 03's survey-weighted AP/AC/interaction models.
get_hapc_models <- function(survey_name) {
  survey_name <- tolower(survey_name)
  meta <- SURVEY_META[[survey_name]]
  if (is.null(meta)) stop("Unknown survey: ", survey_name)

  saved <- list(
    continuous = here::here("output", "apc", paste0(survey_name, "_results_continuous.rds")),
    binary = here::here("output", "apc", paste0(survey_name, "_results_binary.rds"))
  )

  if (all(file.exists(unlist(saved)))) {
    message("  Using saved model objects for ", toupper(survey_name))
    res_cont <- readRDS(saved$continuous)
    res_bin <- readRDS(saved$binary)
    models <- list(continuous = res_cont$hapc$model, binary = res_bin$hapc$model)
    if (!is.null(models$continuous) && !is.null(models$binary)) {
      return(models)
    }
    message("  Saved objects lack fitted HAPC models; refitting instead")
  } else {
    message("  No saved model objects for ", toupper(survey_name), "; refitting")
  }

  df <- load_survey_data(survey_name)
  df_apc <- structure_apc_data(df, srh_scale = meta$srh_scale)
  df_hapc <- create_hapc_subsample(df_apc,
                                   fraction = meta$hapc_fraction,
                                   seed = meta$hapc_seed)
  message("  HAPC sample: ", format(nrow(df_hapc), big.mark = ","), " rows")

  list(
    continuous = fit_hapc_model(df_hapc, "continuous")$model,
    binary = fit_hapc_model(df_hapc, "binary")$model
  )
}

# ==============================================================================
# Run
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
surveys <- if (length(args) > 0) tolower(args) else CSV_SURVEY_ORDER
stopifnot(all(surveys %in% CSV_SURVEY_ORDER))

message("Nakagawa HAPC variance decomposition for: ",
        paste(toupper(surveys), collapse = ", "))

rows <- map(surveys, function(s) {
  message("\n=== ", toupper(s), " ===")
  models <- tryCatch(get_hapc_models(s), error = function(e) {
    message("  SKIPPED (", conditionMessage(e), ")")
    NULL
  })
  if (is.null(models)) return(NULL)
  bind_rows(
    nakagawa_row(models$continuous, toupper(s), "continuous"),
    nakagawa_row(models$binary, toupper(s), "binary")
  )
})
decomp <- bind_rows(rows)

if (nrow(decomp) == 0) stop("No surveys produced results.")

dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(CANONICAL_CSV)) {
  write_csv(decomp, CANONICAL_CSV)
  message("\nWrote ", CANONICAL_CSV)
} else {
  out_path <- file.path(TABLES_DIR, "variance_decomposition_complete_reconstructed.csv")
  write_csv(decomp, out_path)
  message("\nCanonical CSV already exists; wrote ", out_path)

  committed <- read_csv(CANONICAL_CSV, show_col_types = FALSE) %>%
    semi_join(decomp, by = c("survey", "outcome"))
  comparison <- committed %>%
    inner_join(decomp, by = c("survey", "outcome"),
               suffix = c("_committed", "_reconstructed"))
  num_cols <- setdiff(names(committed), c("survey", "outcome"))
  message("\nComparison vs committed values (per survey x outcome):")
  for (i in seq_len(nrow(comparison))) {
    diffs <- map_dbl(num_cols, function(col) {
      abs(comparison[[paste0(col, "_committed")]][i] -
            comparison[[paste0(col, "_reconstructed")]][i])
    })
    message(sprintf("  %s %s: max abs diff = %.3g (%s)",
                    comparison$survey[i], comparison$outcome[i],
                    max(diffs), num_cols[which.max(diffs)]))
  }
}

message("\nDone at ", Sys.time())
