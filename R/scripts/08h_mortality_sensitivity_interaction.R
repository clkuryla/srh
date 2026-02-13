# ==============================================================================
# 08h_mortality_sensitivity_interaction.R
# Formal SRH x Survey-Year Interaction Test for Mortality
#
# Purpose:
#   Test whether the SRH-mortality association changes over calendar time.
#   For each age group, fit one pooled Cox model with srh * year10 interaction
#   and use regTermTest() for a design-based Wald F-test.
#
# Motivation:
#   Panel A of fig_mortality_util_combined.png shows 10-year rolling-window HRs
#   where middle age groups (30-39 through 60-69) appear to strengthen in
#   predictive ability over calendar time. This script provides a formal test.
#
# Output:
#   output/sensitivity/mortality/changing_HR_per_age_group_1/
#     - srh_year_interaction_by_agegroup.csv        (publication table)
#     - srh_year_interaction_by_agegroup.rds         (fitted model list)
#     - fig_interaction_faceted.{png,pdf}            (7-panel faceted figure)
#     - fig_interaction_overlay.{png,pdf}            (all age groups on one plot)
#     - fig_interaction_bar.{png,pdf}                (bar chart of interaction HR)
#     - interaction_interpretation.txt               (programmatic interpretation)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(survival)
library(here)

# Source shared functions
source(here("R", "paths.R"))
source(here("R", "srh_common_functions.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "mortality_utils.R"))
source(here("R", "functions", "run_mortality_sensitivity.R"))
source(here("R", "functions", "plot_utils.R"))

# Set seed for reproducibility
set.seed(20260213)

# Output directory
output_dir <- here("output", "sensitivity", "mortality", "changing_HR_per_age_group_1")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Analysis date for draft outputs
analysis_date <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# 2. LOAD AND PREPARE DATA
# ==============================================================================

message("\n=== Loading NHIS data ===")

data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))
message(sprintf("Loaded %s records from data_nhis.rds",
                format(nrow(data_nhis), big.mark = ",")))

# Prepare mortality data (same sample as 08a)
message("\n=== Preparing mortality data ===")

nhis_mort <- prepare_mortality_from_nhis(
  data_nhis,
  min_age = 18,
  max_age = 89,
  age_scheme = "B",
  min_year = 1982,
  follow_up_end_year = 2019
)

rm(data_nhis)
gc(verbose = FALSE)


# ==============================================================================
# 3. APPLY 10-YEAR FOLLOW-UP (POOLED, NOT WINDOWED)
# ==============================================================================

message("\n=== Creating pooled dataset with 10-year follow-up ===")

nhis_pooled <- nhis_mort %>%
  mutate(
    # Max follow-up: 10 years from survey, capped at 2019
    max_follow_up_year = pmin(survey_year + 10L, 2019L),

    # Event indicator: died within follow-up
    event = case_when(
      mortality_status == 0L ~ 0L,
      is.na(death_year)     ~ 0L,
      death_year <= max_follow_up_year ~ 1L,
      TRUE ~ 0L
    ),

    # Age at end of observation (death or censoring)
    age_at_end = case_when(
      event == 1L ~ age_at_survey + (death_year - survey_year),
      TRUE        ~ age_at_survey + (max_follow_up_year - survey_year)
    ),

    # Year variable: centered at 1990, scaled per decade
    # srh main effect = HR at 1990; interaction = change per decade
    year10 = (survey_year - 1990) / 10
  ) %>%
  # Required for Surv(): age_at_end must exceed age_at_survey

  filter(age_at_end > age_at_survey)

message(sprintf(
  "Pooled dataset: %s records, %s deaths",
  format(nrow(nhis_pooled), big.mark = ","),
  format(sum(nhis_pooled$event), big.mark = ",")
))
message(sprintf(
  "Survey years: %d-%d | year10 range: %.1f to %.1f",
  min(nhis_pooled$survey_year), max(nhis_pooled$survey_year),
  min(nhis_pooled$year10), max(nhis_pooled$year10)
))

rm(nhis_mort)
gc(verbose = FALSE)


# ==============================================================================
# 4. HELPER FUNCTIONS
# ==============================================================================

#' Fit survey-weighted Cox model with srh * year10 interaction
#'
#' @param df Data frame (pooled with year10 variable)
#' @param age_grp Character: age group to filter to
#' @param min_n Minimum sample size (default 100)
#' @param min_events Minimum number of events (default 30)
#' @return List with fit object, coefficients, interaction test, vcov submatrix
fit_interaction_model <- function(df, age_grp, min_n = 100, min_events = 30) {

  df_age <- df %>% filter(age_group == age_grp)

  n <- nrow(df_age)
  n_events <- sum(df_age$event, na.rm = TRUE)

  if (n < min_n || n_events < min_events) {
    warning(sprintf("Insufficient data for %s: n=%d, events=%d", age_grp, n, n_events))
    return(list(
      age_group = age_grp, n = n, n_events = n_events,
      converged = FALSE, error_msg = "Insufficient data"
    ))
  }

  tryCatch({
    # Survey design (weights-only, same as all existing models)
    design <- svydesign(ids = ~1, weights = ~weight, data = df_age)

    # Fit Cox model: Surv(age time scale) ~ srh * year10
    fit <- svycoxph(
      Surv(age_at_survey, age_at_end, event) ~ srh * year10,
      design = design
    )

    # Design-based Wald F-test for interaction term
    int_test <- survey::regTermTest(fit, ~ srh:year10)

    # Extract summary
    fit_sum <- summary(fit)
    coef_mat <- fit_sum$coefficients

    # svycoxph uses "robust SE" column name; handle both possibilities
    se_col <- if ("robust SE" %in% colnames(coef_mat)) "robust SE" else "se(coef)"

    # Extract coefficients
    b_srh    <- coef_mat["srh", "coef"]
    se_srh   <- coef_mat["srh", se_col]
    p_srh    <- coef_mat["srh", "Pr(>|z|)"]

    b_year10    <- coef_mat["year10", "coef"]
    se_year10   <- coef_mat["year10", se_col]
    p_year10    <- coef_mat["year10", "Pr(>|z|)"]

    b_int    <- coef_mat["srh:year10", "coef"]
    se_int   <- coef_mat["srh:year10", se_col]
    p_int_z  <- coef_mat["srh:year10", "Pr(>|z|)"]

    # regTermTest p-value (design-based F-test) - extract as scalar
    p_int_F  <- as.numeric(int_test$p)

    # Variance-covariance matrix (for delta-method predictions)
    V <- vcov(fit)

    list(
      age_group  = age_grp,
      n          = n,
      n_events   = n_events,
      converged  = TRUE,
      fit        = fit,
      V          = V,
      b_srh      = b_srh,
      se_srh     = se_srh,
      p_srh      = p_srh,
      b_year10   = b_year10,
      se_year10  = se_year10,
      p_year10   = p_year10,
      b_int      = b_int,
      se_int     = se_int,
      p_int_z    = p_int_z,
      p_int_F    = p_int_F,
      F_stat     = as.numeric(int_test$Ftest[1, 1])
    )

  }, error = function(e) {
    warning(sprintf("Model failed for %s: %s", age_grp, conditionMessage(e)))
    list(
      age_group = age_grp, n = n, n_events = n_events,
      converged = FALSE, error_msg = conditionMessage(e)
    )
  })
}


#' Extract results from a fitted interaction model into a one-row tibble
#'
#' @param result List from fit_interaction_model()
#' @return One-row tibble with HRs, CIs, p-values
extract_results <- function(result) {
  if (!isTRUE(result$converged)) {
    return(tibble(
      age_group = result$age_group,
      n = result$n,
      n_events = result$n_events,
      converged = FALSE,
      b_srh = NA_real_, se_srh = NA_real_,
      HR_ref = NA_real_, HR_ref_low = NA_real_, HR_ref_high = NA_real_,
      p_srh = NA_real_,
      b_year10 = NA_real_, se_year10 = NA_real_,
      p_year10 = NA_real_,
      b_int = NA_real_, se_int = NA_real_,
      HR_mult_decade = NA_real_, HR_mult_decade_low = NA_real_,
      HR_mult_decade_high = NA_real_,
      p_int = NA_real_, p_int_z = NA_real_,
      F_stat = NA_real_
    ))
  }

  # HR at reference year (1990): exp(b_srh)
  HR_ref      <- exp(result$b_srh)
  HR_ref_low  <- exp(result$b_srh - 1.96 * result$se_srh)
  HR_ref_high <- exp(result$b_srh + 1.96 * result$se_srh)

  # Multiplicative change in HR per decade: exp(b_int)
  HR_mult_decade      <- exp(result$b_int)
  HR_mult_decade_low  <- exp(result$b_int - 1.96 * result$se_int)
  HR_mult_decade_high <- exp(result$b_int + 1.96 * result$se_int)

  tibble(
    age_group           = result$age_group,
    n                   = result$n,
    n_events            = result$n_events,
    converged           = TRUE,
    b_srh               = result$b_srh,
    se_srh              = result$se_srh,
    HR_ref              = HR_ref,
    HR_ref_low          = HR_ref_low,
    HR_ref_high         = HR_ref_high,
    p_srh               = result$p_srh,
    b_year10            = result$b_year10,
    se_year10           = result$se_year10,
    p_year10            = result$p_year10,
    b_int               = result$b_int,
    se_int              = result$se_int,
    HR_mult_decade      = HR_mult_decade,
    HR_mult_decade_low  = HR_mult_decade_low,
    HR_mult_decade_high = HR_mult_decade_high,
    p_int               = result$p_int_F,
    p_int_z             = result$p_int_z,
    F_stat              = result$F_stat
  )
}


#' Predict HR of SRH on mortality over calendar years using delta method
#'
#' @param result List from fit_interaction_model() (must have converged)
#' @param years Vector of survey years to predict for
#' @return Tibble with year, year10, logHR, HR, conf_low, conf_high
predict_hr_over_years <- function(result, years = 1982:2019) {
  if (!isTRUE(result$converged)) {
    return(tibble(
      age_group = result$age_group,
      year = integer(), year10 = numeric(),
      logHR = numeric(), HR = numeric(),
      conf_low = numeric(), conf_high = numeric()
    ))
  }

  b_srh <- result$b_srh
  b_int <- result$b_int
  V     <- result$V

  # 2x2 submatrix for (srh, srh:year10)
  idx <- c("srh", "srh:year10")
  V_sub <- V[idx, idx]

  purrr::map_dfr(years, function(y) {
    y10 <- (y - 1990) / 10
    # log(HR) = b_srh + b_int * y10
    logHR <- b_srh + b_int * y10

    # Delta-method variance: [1, y10] %*% V_sub %*% [1, y10]
    grad <- c(1, y10)
    var_logHR <- as.numeric(t(grad) %*% V_sub %*% grad)
    se_logHR  <- sqrt(var_logHR)

    tibble(
      age_group = result$age_group,
      year      = y,
      year10    = y10,
      logHR     = logHR,
      HR        = exp(logHR),
      conf_low  = exp(logHR - 1.96 * se_logHR),
      conf_high = exp(logHR + 1.96 * se_logHR)
    )
  })
}


# ==============================================================================
# 5. RUN MODELS
# ==============================================================================

message("\n=== Fitting interaction models by age group ===\n")

age_groups <- levels(nhis_pooled$age_group)
if (is.null(age_groups)) {
  age_groups <- sort(unique(nhis_pooled$age_group))
}

# Fit models
model_results <- purrr::map(age_groups, function(ag) {
  message(sprintf("  Fitting: %s ...", ag))
  fit_interaction_model(nhis_pooled, age_grp = ag)
}) %>%
  setNames(age_groups)

# Extract coefficient table
results_table <- purrr::map_dfr(model_results, extract_results)

# Add FDR-adjusted p-values across age groups
results_table <- results_table %>%
  mutate(p_int_fdr = p.adjust(p_int, method = "BH"))

message(sprintf("\n  Models converged: %d / %d",
                sum(results_table$converged), nrow(results_table)))

# Predict HR over years for all age groups
hr_predictions <- purrr::map_dfr(model_results, predict_hr_over_years)

# Ensure age_group is a factor with correct ordering
hr_predictions <- hr_predictions %>%
  mutate(age_group = factor(age_group, levels = age_groups))

results_table <- results_table %>%
  mutate(age_group = factor(age_group, levels = age_groups))


# ==============================================================================
# 6. OUTPUT A: PUBLICATION TABLE
# ==============================================================================

message("\n=== Saving publication table ===")

# Full numeric table
readr::write_csv(
  results_table,
  file.path(output_dir, "srh_year_interaction_by_agegroup.csv")
)

# Formatted version for display
format_p <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "< 0.001", sprintf("%.3f", signif(p, 3))))
}

results_formatted <- results_table %>%
  mutate(
    HR_ref_fmt        = sprintf("%.3f (%.3f, %.3f)", HR_ref, HR_ref_low, HR_ref_high),
    HR_mult_dec_fmt   = sprintf("%.3f (%.3f, %.3f)", HR_mult_decade, HR_mult_decade_low, HR_mult_decade_high),
    p_int_fmt         = format_p(p_int),
    p_int_fdr_fmt     = format_p(p_int_fdr)
  ) %>%
  select(age_group, n, n_events,
         b_srh, se_srh, HR_ref_fmt,
         b_int, se_int, HR_mult_dec_fmt,
         p_int_fmt, p_int_fdr_fmt, F_stat)

readr::write_csv(
  results_formatted,
  file.path(output_dir, "srh_year_interaction_by_agegroup_formatted.csv")
)

# Save fitted model list (without the full survey design objects for space)
model_summaries <- purrr::map(model_results, function(r) {
  r$fit <- NULL  # Remove fit object to save space
  r
})
readr::write_rds(
  model_summaries,
  file.path(output_dir, "srh_year_interaction_by_agegroup.rds")
)

message("Saved: srh_year_interaction_by_agegroup.{csv,rds}")


# ==============================================================================
# 7. OUTPUT B: VISUALIZATIONS
# ==============================================================================

message("\n=== Creating visualizations ===")

# --- Load rolling-window point estimates for overlay ---
rolling_file <- here("output", "mortality_results_all_windows_20260119.csv")
rolling_overlay <- NULL

if (file.exists(rolling_file)) {
  rolling_all <- readr::read_csv(rolling_file, show_col_types = FALSE)
  # Filter to 10-year windows for overlay
  rolling_overlay <- rolling_all %>%
    filter(window_length == 10, follow_up_years == 10, converged) %>%
    mutate(
      mid_year = start_year + (window_length - 1) / 2,
      age_group = factor(age_group, levels = age_groups)
    )
  message(sprintf("  Loaded %d rolling-window 10yr estimates for overlay",
                  nrow(rolling_overlay)))
} else {
  message("  Rolling-window file not found; skipping overlay points")
}


# --- Figure 1: Faceted by age group (primary figure) ---

p_faceted <- ggplot(hr_predictions,
                    aes(x = year, y = HR, color = age_group, fill = age_group)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.8)

# Add rolling-window overlay if available
if (!is.null(rolling_overlay)) {
  p_faceted <- p_faceted +
    geom_point(data = rolling_overlay,
               aes(x = mid_year, y = hr),
               alpha = 0.35, size = 1.5, shape = 16)
}

p_faceted <- p_faceted +
  facet_wrap(~age_group, ncol = 4, scales = "free_y") +
  scale_y_log10() +
  scale_color_age() +
  scale_fill_age() +
  labs(
    title = "SRH-Mortality Hazard Ratio Over Calendar Time",
    subtitle = "Model-predicted HR (line + 95% CI) with rolling-window estimates (points)",
    x = "Survey Year",
    y = "Hazard Ratio per 1-unit SRH (log scale)",
    color = "Age Group",
    caption = "Cox PH with age time scale, srh * year10 interaction, survey-weighted"
  ) +
  theme_srh(base_size = 14) +
  theme(legend.position = "none")  # Labels are in facet strips

save_figure(p_faceted, "fig_interaction_faceted",
            path = output_dir, width = 14, height = 9, dpi = 300)


# --- Figure 2: Overlay (all age groups on one plot, Panel A style) ---

p_overlay <- ggplot(hr_predictions,
                    aes(x = year, y = HR, color = age_group, fill = age_group)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8)

# Add rolling-window overlay if available
if (!is.null(rolling_overlay)) {
  p_overlay <- p_overlay +
    geom_point(data = rolling_overlay,
               aes(x = mid_year, y = hr, color = age_group),
               alpha = 0.3, size = 1.5, shape = 16)
}

p_overlay <- p_overlay +
  scale_y_log10() +
  scale_color_age() +
  scale_fill_age() +
  labs(
    title = "SRH-Mortality Hazard Ratio Over Calendar Time (All Age Groups)",
    subtitle = "Lines: model-predicted HR from srh * year10 interaction | Points: 10yr rolling-window HRs",
    x = "Survey Year",
    y = "Hazard Ratio per 1-unit SRH (log scale)",
    color = "Age Group",
    caption = "Cox PH with age time scale, survey-weighted"
  ) +
  theme_srh() +
  guides(fill = "none")

save_figure(p_overlay, "fig_interaction_overlay",
            path = output_dir, width = 12, height = 8, dpi = 300)


# --- Figure 3: Bar chart of interaction HR per decade ---

results_for_bar <- results_table %>%
  filter(converged)

p_bar <- ggplot(results_for_bar,
                aes(x = age_group, y = HR_mult_decade,
                    fill = age_group)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_errorbar(
    aes(ymin = HR_mult_decade_low, ymax = HR_mult_decade_high),
    width = 0.25, linewidth = 0.5
  ) +
  # Add significance stars
  geom_text(
    aes(label = ifelse(p_int_fdr < 0.001, "***",
                       ifelse(p_int_fdr < 0.01, "**",
                              ifelse(p_int_fdr < 0.05, "*", ""))),
        y = HR_mult_decade_high + 0.005),
    size = 5, vjust = 0
  ) +
  scale_fill_age() +
  labs(
    title = "Change in SRH-Mortality HR Per Decade",
    subtitle = "exp(interaction coefficient); < 1 means SRH becoming more predictive",
    x = "Age Group",
    y = "HR Multiplier Per Decade",
    caption = "Error bars: 95% CI | * p<0.05, ** p<0.01, *** p<0.001 (FDR-adjusted)"
  ) +
  theme_srh() +
  theme(legend.position = "none")

save_figure(p_bar, "fig_interaction_bar",
            path = output_dir, width = 10, height = 7, dpi = 300)


# ==============================================================================
# 8. OUTPUT C: INTERPRETATION TEXT
# ==============================================================================

message("\n=== Generating interpretation text ===")

# Identify significant age groups
sig_groups <- results_table %>%
  filter(converged, p_int_fdr < 0.05) %>%
  pull(age_group) %>%
  as.character()

nonsig_groups <- results_table %>%
  filter(converged, p_int_fdr >= 0.05) %>%
  pull(age_group) %>%
  as.character()

# Get example for the most significant group
if (length(sig_groups) > 0) {
  example_row <- results_table %>%
    filter(age_group %in% sig_groups) %>%
    slice_min(p_int, n = 1)

  example_text <- sprintf(
    "e.g., for ages %s: HR multiplied by %.3f per decade, 95%% CI [%.3f, %.3f], p %s",
    example_row$age_group,
    example_row$HR_mult_decade,
    example_row$HR_mult_decade_low,
    example_row$HR_mult_decade_high,
    format_p(example_row$p_int)
  )
} else {
  example_text <- "no age groups showed statistically significant interaction"
}

# Direction of significant interactions
if (length(sig_groups) > 0) {
  sig_directions <- results_table %>%
    filter(age_group %in% sig_groups) %>%
    summarise(
      n_neg = sum(b_int < 0),
      n_pos = sum(b_int > 0)
    )

  if (sig_directions$n_neg > 0 & sig_directions$n_pos == 0) {
    direction_text <- "the interaction was negative and statistically significant, indicating that SRH became increasingly predictive of subsequent mortality over time"
  } else if (sig_directions$n_pos > 0 & sig_directions$n_neg == 0) {
    direction_text <- "the interaction was positive and statistically significant, indicating that SRH became less predictive of subsequent mortality over time"
  } else {
    direction_text <- "the direction of the significant interaction varied across age groups"
  }
} else {
  direction_text <- "no statistically significant interaction was detected"
}

interpretation <- sprintf(
  paste0(
    "In survey-weighted Cox proportional hazard models (age time scale, 10-year ",
    "follow-up), we tested whether the SRH-mortality association changed over ",
    "calendar time by fitting srh * survey_year interactions within each age group. ",
    "Survey year was centered at 1990 and scaled per decade, so the interaction ",
    "coefficient represents the multiplicative change in the SRH hazard ratio per ",
    "decade of calendar time.\n\n",

    "For adults aged %s, %s (%s). ",
    "In contrast, the interaction was smaller and not statistically distinguishable ",
    "from zero for ages %s (FDR-adjusted p > 0.05).\n\n",

    "The design-based Wald F-test (regTermTest) was used for inference, with ",
    "Benjamini-Hochberg correction for multiple comparisons across %d age groups. ",
    "%d of %d age groups showed a statistically significant interaction at FDR < 0.05.\n\n",

    "Interpretation: An HR multiplier per decade < 1 means that the protective ",
    "association of higher SRH with lower mortality is strengthening over calendar ",
    "time (the HR moves further below 1 each decade). SRH is coded higher = better ",
    "(1=Poor, 5=Excellent)."
  ),
  if (length(sig_groups) > 0) paste(sig_groups, collapse = ", ") else "no age groups",
  direction_text,
  example_text,
  if (length(nonsig_groups) > 0) paste(nonsig_groups, collapse = ", ") else "none",
  nrow(results_table %>% filter(converged)),
  length(sig_groups),
  nrow(results_table %>% filter(converged))
)

writeLines(interpretation, file.path(output_dir, "interaction_interpretation.txt"))
message("Saved: interaction_interpretation.txt")


# ==============================================================================
# 9. VERIFICATION CHECKS
# ==============================================================================

message("\n=== Verification ===\n")

converged_results <- results_table %>% filter(converged)

# Check 1: All SRH main-effect coefficients should be negative
n_pos_srh <- sum(converged_results$b_srh > 0, na.rm = TRUE)
message(sprintf("  SRH coef positive (should be 0): %d / %d",
                n_pos_srh, nrow(converged_results)))
if (n_pos_srh > 0) {
  warning("Some SRH main effects are positive! Check these age groups:")
  print(converged_results %>% filter(b_srh > 0) %>% select(age_group, b_srh, HR_ref))
}

# Check 2: All HR_ref < 1
n_hr_above1 <- sum(converged_results$HR_ref >= 1, na.rm = TRUE)
message(sprintf("  HR_ref >= 1 (should be 0): %d / %d",
                n_hr_above1, nrow(converged_results)))

# Check 3: Significant interaction count
n_sig <- sum(converged_results$p_int_fdr < 0.05, na.rm = TRUE)
n_sig_nom <- sum(converged_results$p_int < 0.05, na.rm = TRUE)
message(sprintf("  Significant interactions (FDR < 0.05): %d / %d",
                n_sig, nrow(converged_results)))
message(sprintf("  Significant interactions (nominal < 0.05): %d / %d",
                n_sig_nom, nrow(converged_results)))

# Check 4: Interaction direction vs visual trend
message("\n  Interaction directions:")
converged_results %>%
  select(age_group, b_int, HR_mult_decade, p_int, p_int_fdr) %>%
  mutate(
    direction = ifelse(b_int < 0, "strengthening", "weakening"),
    sig = ifelse(p_int_fdr < 0.05, "***", ifelse(p_int < 0.05, "*", ""))
  ) %>%
  print(n = Inf)

# Print full results table
message("\n  Full results:")
converged_results %>%
  select(age_group, n, n_events, HR_ref, HR_mult_decade, p_int, p_int_fdr) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  print(n = Inf)

# Print interpretation
message("\n=== Interpretation ===\n")
cat(interpretation)

message("\n\n=== Analysis complete ===")
message(sprintf("Output directory: %s", output_dir))
