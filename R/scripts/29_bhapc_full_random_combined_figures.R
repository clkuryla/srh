# ==============================================================================
# 29_bhapc_full_random_combined_figures.R
# Combined figures for BHAPC Full Random Effects analysis
# ==============================================================================

library(tidyverse)
library(here)

source(here::here("R/functions/theme_srh.R"))

# ==============================================================================
# Configuration
# ==============================================================================

output_dir <- here::here("output/bhapc_full_random")
figures_dir <- file.path(output_dir, "figures")
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# Survey order for display
SURVEY_ORDER <- c("BRFSS", "MEPS", "NHIS", "CPS", "NHANES", "GSS")

# APC component colors (from existing code)
apc_colors <- c(
  "Age" = "#0072B2",    # blue
  "Period" = "#009E73", # bluish green
  "Cohort" = "#CC79A7"  # reddish purple
)

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Load variance decomposition data for a survey, or return placeholder if missing
#' @param survey_name Survey name (lowercase)
#' @return Tibble with variance data and status indicator
load_variance <- function(survey_name) {
  var_path <- file.path(output_dir, survey_name,
                        paste0(survey_name, "_variance_decomposition.csv"))

  if (file.exists(var_path)) {
    read_csv(var_path, show_col_types = FALSE) %>%
      mutate(survey = toupper(survey_name), status = "complete")
  } else {
    # Return placeholder with NA values
    tibble(
      component = c("age_group", "period_4yr", "cohort_4yr", "Residual", "Total"),
      variance = NA_real_,
      pct_of_total = NA_real_,
      sd = NA_real_,
      survey = toupper(survey_name),
      status = "pending"
    )
  }
}

# ==============================================================================
# PART 1: VARIANCE DECOMPOSITION BAR CHART
# ==============================================================================

cat("\n=== Creating Variance Decomposition Bar Chart ===\n")

# Load variance data from all surveys
all_variance <- map_dfr(tolower(SURVEY_ORDER), load_variance)

# Check status
status_summary <- all_variance %>%
  distinct(survey, status)
cat("\nSurvey Status:\n")
print(status_summary)

# Prepare data for plotting (filter to APC components only)
var_plot_data <- all_variance %>%
  filter(component %in% c("age_group", "period_4yr", "cohort_4yr")) %>%
  mutate(
    component = case_when(
      component == "age_group" ~ "Age",
      component == "period_4yr" ~ "Period",
      component == "cohort_4yr" ~ "Cohort"
    ),
    component = factor(component, levels = c("Age", "Period", "Cohort")),
    survey = factor(survey, levels = SURVEY_ORDER)
  )

cat("\nVariance data summary:\n")
print(var_plot_data %>% select(survey, component, pct_of_total, status))

# Determine completed surveys for subtitle
completed_surveys <- status_summary %>%
  filter(status == "complete") %>%
  pull(survey)
pending_surveys <- status_summary %>%
  filter(status == "pending") %>%
  pull(survey)

subtitle_text <- "Full random effects BHAPC model"

# Calculate y-axis limit for labels
max_pct <- max(var_plot_data$pct_of_total, na.rm = TRUE)
y_limit <- max_pct * 1.15

# Create grouped bar chart
p_var_comparison <- ggplot(var_plot_data, aes(x = survey, y = pct_of_total, fill = component)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.85) +
  geom_text(
    aes(label = ifelse(is.na(pct_of_total), "", paste0(round(pct_of_total, 1), "%"))),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 5.5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = apc_colors, name = "Component") +
  scale_y_continuous(
    limits = c(0, y_limit),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Variance Explained by Age, Period, and Cohort Effects",
    subtitle = subtitle_text,
    x = "",
    y = "% of Total Variance"
  ) +
  theme_srh(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 18, color = "gray40"),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 17, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

# Save outputs
ggsave(file.path(figures_dir, "variance_comparison_combined.png"),
       p_var_comparison, width = 14, height = 7, dpi = 300)
ggsave(file.path(figures_dir, "variance_comparison_combined.pdf"),
       p_var_comparison, width = 14, height = 7)

cat("\n=== Output saved ===\n")
cat("PNG:", file.path(figures_dir, "variance_comparison_combined.png"), "\n")
cat("PDF:", file.path(figures_dir, "variance_comparison_combined.pdf"), "\n")

# Print summary table
cat("\n=== Variance Summary (% of Total) ===\n")
var_summary <- var_plot_data %>%
  filter(!is.na(pct_of_total)) %>%
  select(survey, component, pct_of_total) %>%
  pivot_wider(names_from = component, values_from = pct_of_total)
print(var_summary)

# ==============================================================================
# PART 2: APC EFFECTS GRID
# ==============================================================================

cat("\n=== Creating APC Effects Grid ===\n")

#' Load APC effects data for a survey
#' @param survey_name Survey name (lowercase)
#' @param effect_type One of "age", "period", "cohort"
#' @return Tibble with effects data or NULL if missing
load_effects <- function(survey_name, effect_type) {
  file_path <- file.path(output_dir, survey_name,
                         paste0(survey_name, "_", effect_type, "_effects.csv"))

  if (file.exists(file_path)) {
    df <- read_csv(file_path, show_col_types = FALSE)
    df$survey <- toupper(survey_name)
    df$effect_type <- effect_type
    return(df)
  } else {
    return(NULL)
  }
}

# Load all effects data
all_age_effects <- map_dfr(tolower(SURVEY_ORDER), ~load_effects(.x, "age"))
all_period_effects <- map_dfr(tolower(SURVEY_ORDER), ~load_effects(.x, "period"))
all_cohort_effects <- map_dfr(tolower(SURVEY_ORDER), ~load_effects(.x, "cohort"))

cat("Age effects loaded:", n_distinct(all_age_effects$survey), "surveys\n")
cat("Period effects loaded:", n_distinct(all_period_effects$survey), "surveys\n")
cat("Cohort effects loaded:", n_distinct(all_cohort_effects$survey), "surveys\n")

# Standardize column names and combine
age_data <- all_age_effects %>%
  mutate(x_var = age_group, x_numeric = as.numeric(factor(age_group))) %>%
  select(survey, effect_type, x_var, x_numeric, estimate, ci_lower_90, ci_upper_90)

period_data <- all_period_effects %>%
  mutate(x_var = as.character(period), x_numeric = period) %>%
  select(survey, effect_type, x_var, x_numeric, estimate, ci_lower_90, ci_upper_90)

cohort_data <- all_cohort_effects %>%
  mutate(x_var = as.character(cohort), x_numeric = cohort) %>%
  select(survey, effect_type, x_var, x_numeric, estimate, ci_lower_90, ci_upper_90)

# Create individual plots for each component
create_age_plot <- function(survey_data, survey_name) {

  if (nrow(survey_data) == 0) return(NULL)

  ggplot(survey_data, aes(x = x_var, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                  width = 0.2, color = apc_colors["Age"], linewidth = 0.8) +
    geom_point(color = apc_colors["Age"], size = 3) +
    labs(x = "", y = "", title = survey_name) +
    theme_srh(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
      axis.text.y = element_text(size = 14),
      plot.margin = margin(5, 10, 5, 5)
    )
}

create_period_plot <- function(survey_data, survey_name) {
  if (nrow(survey_data) == 0) return(NULL)

  # Filter out NA rows and treat as categorical
  survey_data <- survey_data %>% filter(!is.na(x_numeric))

  ggplot(survey_data, aes(x = factor(x_numeric), y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                  width = 0.2, color = apc_colors["Period"], linewidth = 0.8) +
    geom_point(color = apc_colors["Period"], size = 3) +
    labs(x = "", y = "", title = survey_name) +
    theme_srh(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 14),
      plot.margin = margin(5, 10, 5, 5)
    )
}

create_cohort_plot <- function(survey_data, survey_name) {
  if (nrow(survey_data) == 0) return(NULL)

  # Filter out NA rows and treat as categorical
  survey_data <- survey_data %>% filter(!is.na(x_numeric))

  # For cohort, there are many bins - show every Nth label
  n_cohorts <- nrow(survey_data)
  label_every <- max(1, floor(n_cohorts / 8))  # Show ~8 labels
  x_labels <- survey_data$x_numeric
  x_labels_display <- ifelse(seq_along(x_labels) %% label_every == 1, x_labels, "")

  ggplot(survey_data, aes(x = factor(x_numeric), y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = ci_lower_90, ymax = ci_upper_90),
                  width = 0.3, color = apc_colors["Cohort"], linewidth = 0.6) +
    geom_point(color = apc_colors["Cohort"], size = 2) +
    scale_x_discrete(labels = x_labels_display) +
    labs(x = "", y = "", title = survey_name) +
    theme_srh(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 14),
      plot.margin = margin(5, 10, 5, 5)
    )
}

# Create placeholder plot for missing surveys
create_placeholder <- function(survey_name) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Pending",
             size = 8, color = "gray60") +
    labs(title = survey_name) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    )
}

# Generate plots for each survey and component
library(patchwork)

age_plots <- map(SURVEY_ORDER, function(s) {
  survey_data <- age_data %>% filter(survey == s)
  if (nrow(survey_data) > 0) {
    create_age_plot(survey_data, s)
  } else {
    create_placeholder(s)
  }
})

period_plots <- map(SURVEY_ORDER, function(s) {
  survey_data <- period_data %>% filter(survey == s)
  if (nrow(survey_data) > 0) {
    create_period_plot(survey_data, s)
  } else {
    create_placeholder(s)
  }
})

cohort_plots <- map(SURVEY_ORDER, function(s) {
  survey_data <- cohort_data %>% filter(survey == s)
  if (nrow(survey_data) > 0) {
    create_cohort_plot(survey_data, s)
  } else {
    create_placeholder(s)
  }
})

# Combine into 6x3 grid (surveys as rows, APC as columns)
# Column headers
col_header_age <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Age Effects",
           size = 7, fontface = "bold", color = apc_colors["Age"]) +
  theme_void()

col_header_period <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Period Effects",
           size = 7, fontface = "bold", color = apc_colors["Period"]) +
  theme_void()

col_header_cohort <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "Cohort Effects",
           size = 7, fontface = "bold", color = apc_colors["Cohort"]) +
  theme_void()

# Build the grid layout
# Header row
header_row <- col_header_age | col_header_period | col_header_cohort

# Data rows (one per survey)
survey_rows <- map(1:6, function(i) {
  age_plots[[i]] | period_plots[[i]] | cohort_plots[[i]]
})

# Combine all rows
p_apc_grid <- header_row /
  survey_rows[[1]] /
  survey_rows[[2]] /
  survey_rows[[3]] /
  survey_rows[[4]] /
  survey_rows[[5]] /
  survey_rows[[6]] +
  plot_layout(heights = c(0.5, rep(1, 6))) +
  plot_annotation(
    title = "Age, Period, and Cohort Effects Across Surveys",
    subtitle = "Full random effects BHAPC model (90% credible intervals)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5, color = "gray40")
    )
  )

# Save outputs
ggsave(file.path(figures_dir, "apc_effects_grid.png"),
       p_apc_grid, width = 16, height = 20, dpi = 300)
ggsave(file.path(figures_dir, "apc_effects_grid.pdf"),
       p_apc_grid, width = 16, height = 20)

cat("\n=== APC Grid saved ===\n")
cat("PNG:", file.path(figures_dir, "apc_effects_grid.png"), "\n")
cat("PDF:", file.path(figures_dir, "apc_effects_grid.pdf"), "\n")

cat("\n=== Script complete ===\n")
