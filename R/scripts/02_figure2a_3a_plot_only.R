# ==============================================================================
# 02_figure2a_3a_plot_only.R
# Figure 2A (Coefficients) and Figure 3A (Prevalence)
#
# Purpose:
#   Compute and plot coefficient stability and prevalence trends using
#   Difficulty Climbing Stairs as the physical limitation measure.
#
# Variables:
#   - BRFSS: diffwalk (0/1)
#   - MEPS: ADCLIM (0-2)
#   - NHIS: lawalkclimdif (1-4)
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(survey)
library(srvyr)
library(here)
library(patchwork)

# Source paths and shared functions
source(here("R", "paths.R"))
source(here("R", "srh_common_functions.R"))
source(here("R", "functions", "theme_srh.R"))
source(here("R", "functions", "regress_covariate_by_year.R"))
source(here("R", "functions", "prevalence_by_age_year.R"))

# Set theme
theme_set(theme_srh())

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)

# Output directories
output_dir <- here("output", "figures")
tables_dir <- here("output", "tables")

# Current date for draft filenames
date_suffix <- format(Sys.Date(), "%Y%m%d")

# Year restrictions
nhis_start_year <- 2003
brfss_start_year <- 2005
meps_start_year <- 2008

# BRFSS comorbidity exclude years (coding issues)
brfss_comorb_exclude_years <- c(2006, 2008, 2010)


# ==============================================================================
# PART 1: LOAD AND PREPARE DATA
# ==============================================================================

message("\n========== Loading survey data ==========\n")

# --- Load BRFSS ---
data_brfss <- read_rds(derived_path("data_brfss.rds")) %>%
  filter(year >= brfss_start_year) %>%
  add_age_group(age_var = age, scheme = "B")

brfss_comorb_vars <- c("diabetes_dx", "chd_dx", "stroke_dx", "arthritis_dx")
brfss_comorb_vars <- intersect(brfss_comorb_vars, names(data_brfss))
data_brfss <- data_brfss %>%
  mutate(comorb_count = rowSums(across(all_of(brfss_comorb_vars), ~ as.numeric(.x == 1)), na.rm = TRUE))

message("BRFSS: ", nrow(data_brfss), " rows, years ", min(data_brfss$year), "-", max(data_brfss$year))

# --- Load MEPS ---
data_meps <- read_rds(derived_path("data_meps.rds")) %>%
  filter(year >= meps_start_year) %>%
  add_age_group(age_var = age, scheme = "B")

meps_comorb_vars <- c("DIABETICEV", "HYPERTENEV", "CHEARTDIEV", "STROKEV", "ARTHGLUPEV", "CANCEREV", "ASTHMAEV")
meps_comorb_vars <- intersect(meps_comorb_vars, names(data_meps))
data_meps <- data_meps %>%
  mutate(
    comorb_count = rowSums(across(all_of(meps_comorb_vars), ~ as.numeric(.x == 1)), na.rm = TRUE),
    K6SUM_scaled = rescale_01(K6SUM, min_val = 0, max_val = 24) * 6
  )

message("MEPS: ", nrow(data_meps), " rows, years ", min(data_meps$year), "-", max(data_meps$year))

# --- Load NHIS ---
data_nhis <- read_rds(derived_path("data_nhis.rds")) %>%
  filter(year >= nhis_start_year) %>%
  add_age_group(age_var = age, scheme = "B")

nhis_comorb_vars <- c("DIABETICEV", "HYPERTENEV", "CHEARTDIEV", "STROKEV", "ARTHGLUPEV", "COPDEV", "CANCEREV", "ASTHMAEV")
nhis_comorb_vars <- intersect(nhis_comorb_vars, names(data_nhis))
data_nhis <- data_nhis %>%
  mutate(
    comorb_count = rowSums(across(all_of(nhis_comorb_vars), ~ as.numeric(.x == 1)), na.rm = TRUE),
    k6_scaled = rescale_01(k6, min_val = 0, max_val = 24) * 6
  )

message("NHIS: ", nrow(data_nhis), " rows, years ", min(data_nhis$year), "-", max(data_nhis$year))


# ==============================================================================
# PART 2: RUN REGRESSIONS (COEFFICIENTS)
# ==============================================================================

message("\n========== Running regressions ==========\n")

# Helper to safely combine results
safe_bind <- function(...) {
  dfs <- list(...)
  dfs <- dfs[!sapply(dfs, is.null)]
  if (length(dfs) == 0) return(NULL)
  bind_rows(dfs)
}

# --- BRFSS ---
message("BRFSS regressions...")
coef_brfss <- safe_bind(
  regress_covariate_by_age_year(data_brfss, "comorb_count", "Comorbidity Count", "BRFSS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Comorbidity Count"),
  regress_covariate_by_age_year(data_brfss, "ment_bad", "Mental Days (0-30)", "BRFSS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Mental Health"),
  regress_covariate_by_age_year(data_brfss, "diffwalk", "Difficulty Climbing (0/1)", "BRFSS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Difficulty Climbing Stairs")
)

# --- MEPS ---
message("MEPS regressions...")
coef_meps <- safe_bind(
  regress_covariate_by_age_year(data_meps, "comorb_count", "Comorbidity Count", "MEPS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Comorbidity Count"),
  regress_covariate_by_age_year(data_meps, "K6SUM_scaled", "K6 (0-6)", "MEPS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Mental Health"),
  regress_covariate_by_age_year(data_meps, "ADCLIM", "Difficulty Climbing (0-2)", "MEPS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Difficulty Climbing Stairs")
)

# --- NHIS ---
message("NHIS regressions...")
coef_nhis <- safe_bind(
  regress_covariate_by_age_year(data_nhis, "comorb_count", "Comorbidity Count", "NHIS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Comorbidity Count"),
  regress_covariate_by_age_year(data_nhis, "k6_scaled", "K6 (0-6)", "NHIS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Mental Health"),
  regress_covariate_by_age_year(data_nhis, "lawalkclimdif", "Difficulty Climbing (1-4)", "NHIS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Difficulty Climbing Stairs")
)


# ==============================================================================
# PART 3: COMPUTE PREVALENCE/MEANS
# ==============================================================================

message("\n========== Computing prevalence/means ==========\n")

# --- BRFSS ---
message("BRFSS prevalence...")
prev_brfss <- safe_bind(
  mean_by_age_year(data_brfss, "comorb_count", "Comorbidity Count", "BRFSS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Comorbidity Count"),
  mean_by_age_year(data_brfss, "ment_bad", "Mental Days (0-30)", "BRFSS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Mental Health"),
  mean_by_age_year(data_brfss, "diffwalk", "Difficulty Climbing (0/1)", "BRFSS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Difficulty Climbing Stairs")
)

# --- MEPS ---
message("MEPS prevalence...")
prev_meps <- safe_bind(
  mean_by_age_year(data_meps, "comorb_count", "Comorbidity Count", "MEPS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Comorbidity Count"),
  mean_by_age_year(data_meps, "K6SUM_scaled", "K6 (0-6)", "MEPS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Mental Health"),
  mean_by_age_year(data_meps, "ADCLIM", "Difficulty Climbing (0-2)", "MEPS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Difficulty Climbing Stairs")
)

# --- NHIS ---
message("NHIS prevalence...")
prev_nhis <- safe_bind(
  mean_by_age_year(data_nhis, "comorb_count", "Comorbidity Count", "NHIS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Comorbidity Count"),
  mean_by_age_year(data_nhis, "k6_scaled", "K6 (0-6)", "NHIS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Mental Health"),
  mean_by_age_year(data_nhis, "lawalkclimdif", "Difficulty Climbing (1-4)", "NHIS", psu_var = NULL, strata_var = NULL) %>% mutate(category = "Difficulty Climbing Stairs")
)

# Free memory
rm(data_brfss, data_meps, data_nhis); gc()

message("\nData ready:")
message("  BRFSS coef: ", nrow(coef_brfss), " rows, years ", min(coef_brfss$year), "-", max(coef_brfss$year))
message("  MEPS coef: ", nrow(coef_meps), " rows, years ", min(coef_meps$year), "-", max(coef_meps$year))
message("  NHIS coef: ", nrow(coef_nhis), " rows, years ", min(coef_nhis$year), "-", max(coef_nhis$year))


# ==============================================================================
# PART 2: DEFINE HELPER PLOTTING FUNCTION
# ==============================================================================

# --- Variable descriptions for each survey/category combination ---
var_descriptions <- list(
  # Chronic conditions
  BRFSS_chronic = "Comorbidity count",
  MEPS_chronic = "Comorbidity count",
  NHIS_chronic = "Comorbidity count",
  # Mental health
  BRFSS_mental = "Mental bad days (0-30)",
  MEPS_mental = "K6 distress (0-6)",
  NHIS_mental = "K6 distress (0-6)",
  # Physical health
  BRFSS_physical = "Stairs difficulty (0/1)",
  MEPS_physical = "Stairs difficulty (0-2)",
  NHIS_physical = "Stairs difficulty (1-4)"
)

# --- Helper function to create a single panel ---
create_age_subplot <- function(
    data,
    y_var = "coefficient",
    show_title = FALSE,
    title = NULL,
    ylabel = NULL,
    base_size = 18,
    xlim = NULL,
    show_hline = TRUE,
    row_label = NULL
) {

  # Check if data has results
  if (is.null(data) || nrow(data) == 0) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No data",
               size = 5, color = "gray50") +
      theme_void()
    if (show_title && !is.null(title)) {
      p <- p + labs(title = title) +
        theme(plot.title = element_text(size = base_size + 1, face = "bold",
                                        hjust = 0.5))
    }
    return(p)
  }

  # Add extra left margin if this panel has a row label
  left_margin <- if (!is.null(row_label)) 25 else 4

  p <- ggplot(data, aes(x = year, y = .data[[y_var]],
                        color = age_group, group = age_group)) +
    geom_line(linewidth = 0.9, alpha = 0.8) +
    geom_point(size = 2.0, alpha = 0.8) +
    scale_color_manual(values = age_colors, name = "Age Group") +
    labs(
      x = NULL,
      y = ylabel,
      title = if (show_title) title else NULL,
      tag = row_label
    )

  if (show_hline) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed",
                        color = "gray50", alpha = 0.6)
  }

  if (!is.null(xlim)) {
    p <- p + scale_x_continuous(limits = xlim, breaks = scales::pretty_breaks(n = 4))
  } else {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }

  p <- p +
    theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.25),
      plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = base_size - 1),
      axis.text = element_text(size = base_size - 2, color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.ticks.x = element_line(color = "gray50", linewidth = 0.3),
      plot.margin = margin(4, 12, 4, left_margin),
      legend.position = "none",
      plot.tag = element_text(size = base_size + 1, face = "bold",
                              angle = 90, vjust = 0.5),
      plot.tag.position = c(-0.06, 0.5)
    )

  return(p)
}


# --- Calculate x-axis limits for each survey ---
get_year_range <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(c(NA, NA))
  c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE))
}

xlim_brfss <- get_year_range(coef_brfss)
xlim_meps <- get_year_range(coef_meps)
xlim_nhis <- get_year_range(coef_nhis)


# ==============================================================================
# PART 3: CREATE FIGURE 2A (COEFFICIENTS)
# ==============================================================================

message("\n========== Creating Figure 2A (Coefficients) ==========\n")

# Years to exclude for BRFSS comorbidity (coding problems)
brfss_comorb_exclude_years <- c(2006, 2008, 2010)

# --- Row 1: BRFSS ---
p_brfss_chronic <- create_age_subplot(
  coef_brfss %>% filter(category == "Comorbidity Count",
                        !year %in% brfss_comorb_exclude_years),
  show_title = TRUE, title = "Comorbidity Coefficient",
  ylabel = "Comorbidities",
  xlim = xlim_brfss, row_label = "BRFSS"
)

p_brfss_mental <- create_age_subplot(
  coef_brfss %>% filter(category == "Mental Health"),
  show_title = TRUE, title = "Mental Health Coefficient",
  ylabel = var_descriptions$BRFSS_mental,
  xlim = xlim_brfss
)

p_brfss_physical <- create_age_subplot(
  coef_brfss %>% filter(category == "Difficulty Climbing Stairs"),
  show_title = TRUE, title = "Physical Limit. Coefficient",
  ylabel = var_descriptions$BRFSS_physical,
  xlim = xlim_brfss
)

# --- Row 2: MEPS ---
p_meps_chronic <- create_age_subplot(
  coef_meps %>% filter(category == "Comorbidity Count"),
  ylabel = "Comorbidities",
  xlim = xlim_meps, row_label = "MEPS"
)

p_meps_mental <- create_age_subplot(
  coef_meps %>% filter(category == "Mental Health"),
  ylabel = var_descriptions$MEPS_mental,
  xlim = xlim_meps
)

p_meps_physical <- create_age_subplot(
  coef_meps %>% filter(category == "Difficulty Climbing Stairs"),
  ylabel = var_descriptions$MEPS_physical,
  xlim = xlim_meps
)

# --- Row 3: NHIS ---
p_nhis_chronic <- create_age_subplot(
  coef_nhis %>% filter(category == "Comorbidity Count"),
  ylabel = "Comorbidities",
  xlim = xlim_nhis, row_label = "NHIS"
)

p_nhis_mental <- create_age_subplot(
  coef_nhis %>% filter(category == "Mental Health"),
  ylabel = var_descriptions$NHIS_mental,
  xlim = xlim_nhis
)

p_nhis_physical <- create_age_subplot(
  coef_nhis %>% filter(category == "Difficulty Climbing Stairs"),
  ylabel = var_descriptions$NHIS_physical,
  xlim = xlim_nhis
)

# --- Assemble Figure 2A ---
# Use design matrix for explicit grid layout with aligned columns
# Design: 3 columns, guide_area spans all 3 columns in bottom row
design_2a <- "
ABC
DEF
GHI
JJJ
"
fig2a <- (
  p_brfss_chronic + p_brfss_mental + p_brfss_physical +
  p_meps_chronic + p_meps_mental + p_meps_physical +
  p_nhis_chronic + p_nhis_mental + p_nhis_physical +
  guide_area() +
  plot_layout(design = design_2a, heights = c(1, 1, 1, 0.12), guides = "collect")
) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  ) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.key.size = unit(2, "lines"),
        legend.key.width = unit(2.5, "lines")) &
  guides(color = guide_legend(nrow = 1,
                              override.aes = list(size = 4, linewidth = 1.5)))


# ==============================================================================
# PART 4: CREATE FIGURE 3A (PREVALENCE)
# ==============================================================================

message("\n========== Creating Figure 3A (Prevalence) ==========\n")

# --- Row 1: BRFSS ---
p3a_brfss_chronic <- create_age_subplot(
  prev_brfss %>% filter(category == "Comorbidity Count",
                        !year %in% brfss_comorb_exclude_years),
  y_var = "mean",
  show_title = TRUE, title = "Comorbidity Mean",
  ylabel = var_descriptions$BRFSS_chronic,
  xlim = xlim_brfss, show_hline = FALSE, row_label = "BRFSS"
)

p3a_brfss_mental <- create_age_subplot(
  prev_brfss %>% filter(category == "Mental Health"),
  y_var = "mean",
  show_title = TRUE, title = "Mental Health Mean",
  ylabel = var_descriptions$BRFSS_mental,
  xlim = xlim_brfss, show_hline = FALSE
)

p3a_brfss_physical <- create_age_subplot(
  prev_brfss %>% filter(category == "Difficulty Climbing Stairs"),
  y_var = "mean",
  show_title = TRUE, title = "Physical Limit. Mean",
  ylabel = var_descriptions$BRFSS_physical,
  xlim = xlim_brfss, show_hline = FALSE
)

# --- Row 2: MEPS ---
p3a_meps_chronic <- create_age_subplot(
  prev_meps %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  ylabel = var_descriptions$MEPS_chronic,
  xlim = xlim_meps, show_hline = FALSE, row_label = "MEPS"
)

p3a_meps_mental <- create_age_subplot(
  prev_meps %>% filter(category == "Mental Health"),
  y_var = "mean",
  ylabel = var_descriptions$MEPS_mental,
  xlim = xlim_meps, show_hline = FALSE
)

p3a_meps_physical <- create_age_subplot(
  prev_meps %>% filter(category == "Difficulty Climbing Stairs"),
  y_var = "mean",
  ylabel = var_descriptions$MEPS_physical,
  xlim = xlim_meps, show_hline = FALSE
)

# --- Row 3: NHIS ---
p3a_nhis_chronic <- create_age_subplot(
  prev_nhis %>% filter(category == "Comorbidity Count"),
  y_var = "mean",
  ylabel = var_descriptions$NHIS_chronic,
  xlim = xlim_nhis, show_hline = FALSE, row_label = "NHIS"
)

p3a_nhis_mental <- create_age_subplot(
  prev_nhis %>% filter(category == "Mental Health"),
  y_var = "mean",
  ylabel = var_descriptions$NHIS_mental,
  xlim = xlim_nhis, show_hline = FALSE
)

p3a_nhis_physical <- create_age_subplot(
  prev_nhis %>% filter(category == "Difficulty Climbing Stairs"),
  y_var = "mean",
  ylabel = var_descriptions$NHIS_physical,
  xlim = xlim_nhis, show_hline = FALSE
)

# --- Assemble Figure 3A ---
# Use design matrix for explicit grid layout with aligned columns
# Design: 3 columns, guide_area spans all 3 columns in bottom row
design_3a <- "
ABC
DEF
GHI
JJJ
"
fig3a <- (
  p3a_brfss_chronic + p3a_brfss_mental + p3a_brfss_physical +
  p3a_meps_chronic + p3a_meps_mental + p3a_meps_physical +
  p3a_nhis_chronic + p3a_nhis_mental + p3a_nhis_physical +
  guide_area() +
  plot_layout(design = design_3a, heights = c(1, 1, 1, 0.12), guides = "collect")
) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  ) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.key.size = unit(2, "lines"),
        legend.key.width = unit(2.5, "lines")) &
  guides(color = guide_legend(nrow = 1,
                              override.aes = list(size = 4, linewidth = 1.5)))


# ==============================================================================
# PART 5: SAVE FIGURES
# ==============================================================================

message("\n========== Saving figures ==========\n")

# Figure 2A (coefficients)
ggsave(
  filename = file.path(output_dir, paste0("fig2a_coef_draft_", date_suffix, ".png")),
  plot = fig2a,
  width = 16, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig2a_coef.png"),
  plot = fig2a,
  width = 16, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig2a_coef.pdf"),
  plot = fig2a,
  width = 16, height = 10
)
message("Saved: fig2a_coef (.png and .pdf)")

# Figure 3A (prevalence)
ggsave(
  filename = file.path(output_dir, paste0("fig3a_prev_draft_", date_suffix, ".png")),
  plot = fig3a,
  width = 16, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3a_prev.png"),
  plot = fig3a,
  width = 16, height = 10, dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig3a_prev.pdf"),
  plot = fig3a,
  width = 16, height = 10
)
message("Saved: fig3a_prev (.png and .pdf)")

message("\n========== Figure generation complete ==========\n")
