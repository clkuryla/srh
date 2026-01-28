# ==============================================================================
# 04b_apc_summary_from_table.R
# Create APC Summary Figures from Pre-computed Variance Decomposition
# Author: Christine Lucille Kuryla
#
# Purpose: Generate summary figures using the pre-computed variance
# decomposition table, avoiding memory issues from loading large model files.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(patchwork)
})

source(here::here("R", "srh_common_functions.R"))

# Configuration
SURVEY_DISPLAY_ORDER <- c("brfss", "nhis", "meps", "nhanes", "gss", "cps")
SURVEY_LABELS <- c(brfss = "BRFSS", nhis = "NHIS", meps = "MEPS",
                   nhanes = "NHANES", gss = "GSS", cps = "CPS")

APC_COLORS <- c(Age = "#0072B2", Period = "#009E73", Cohort = "#CC79A7")

OUTPUT_DIR <- here::here("output", "apc")
FIGURES_DIR <- file.path(OUTPUT_DIR, "figures")
TABLES_DIR <- file.path(OUTPUT_DIR, "tables")

# Load pre-computed variance decomposition
decomp <- read_csv(file.path(TABLES_DIR, "variance_decomposition_complete.csv"), show_col_types = FALSE)

cat("=== Loading results for available surveys ===\n")
cat("Surveys:", paste(unique(decomp$survey), collapse = ", "), "\n\n")

# Prepare data for plotting
plot_data <- decomp %>%
  mutate(
    survey = toupper(survey),
    outcome_label = case_when(
      outcome == "continuous" ~ "Continuous SRH",
      outcome == "binary" ~ "Fair/Poor (Binary)",
      TRUE ~ outcome
    ),
    survey = factor(survey, levels = toupper(SURVEY_DISPLAY_ORDER[SURVEY_DISPLAY_ORDER %in% tolower(survey)]))
  )

# ==============================================================================
# Figure 1: Variance Decomposition Bar Chart
# ==============================================================================

plot_variance <- plot_data %>%
  select(survey, outcome_label, Age = age_pct, Period = period_pct, Cohort = cohort_pct) %>%
  pivot_longer(cols = c(Age, Period, Cohort), names_to = "Component", values_to = "Percent") %>%
  mutate(Component = factor(Component, levels = c("Age", "Period", "Cohort")))

p_variance <- ggplot(plot_variance, aes(x = Percent, y = fct_rev(survey), fill = Component)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~outcome_label) +
  scale_fill_manual(values = APC_COLORS, name = "Component") +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "%"), expand = c(0, 0.5)) +
  labs(
    title = "Variance Explained by Age, Period, and Cohort",
    subtitle = "HAPC model with Nakagawa R\u00b2 decomposition",
    x = "% of Total Variance Explained",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(FIGURES_DIR, "fig_apc_variance_decomposition.png"),
       p_variance, width = 10, height = 6, dpi = 300)
cat("Saved: fig_apc_variance_decomposition.png\n")

# ==============================================================================
# Figure 2: Period/Cohort Ratio
# ==============================================================================

plot_ratio <- plot_data %>%
  mutate(
    pc_ratio = period_pct / cohort_pct,
    pattern = if_else(pc_ratio > 1, "Period > Cohort", "Cohort > Period")
  )

p_ratio <- ggplot(plot_ratio, aes(x = pc_ratio, y = fct_rev(survey), fill = pattern)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  facet_wrap(~outcome_label) +
  scale_fill_manual(
    values = c("Period > Cohort" = unname(APC_COLORS["Period"]),
               "Cohort > Period" = unname(APC_COLORS["Cohort"])),
    name = "Pattern"
  ) +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5)) +
  labs(
    title = "Period vs Cohort: Which Dominates?",
    subtitle = "Ratio > 1 indicates period effects dominate; log scale",
    x = "Period / Cohort Ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(FIGURES_DIR, "fig_apc_period_cohort_ratio.png"),
       p_ratio, width = 10, height = 5, dpi = 300)
cat("Saved: fig_apc_period_cohort_ratio.png\n")

# ==============================================================================
# Combined Figure
# ==============================================================================

p_combined <- p_variance / p_ratio +
  plot_layout(heights = c(1.2, 1)) +
  plot_annotation(
    title = "Age-Period-Cohort Variance Decomposition Across US Surveys",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold")
    )
  )

ggsave(file.path(FIGURES_DIR, "fig_apc_combined.png"),
       p_combined, width = 10, height = 10, dpi = 300)
cat("Saved: fig_apc_combined.png\n")

# ==============================================================================
# Summary Table (CSV)
# ==============================================================================

summary_table <- plot_data %>%
  select(survey, outcome = outcome_label,
         age_pct, period_pct, cohort_pct, residual_pct,
         r2_marginal, r2_conditional) %>%
  mutate(
    pc_ratio = round(period_pct / cohort_pct, 2),
    pattern = if_else(pc_ratio > 1, "Period dominates", "Cohort dominates")
  )

write_csv(summary_table, file.path(TABLES_DIR, "apc_summary_table.csv"))
cat("Saved: apc_summary_table.csv\n")

# ==============================================================================
# Summary Report
# ==============================================================================

report <- c(
  "# APC Variance Decomposition Summary",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "## Key Findings",
  "",
  "### Variance Explained",
  "",
  "| Survey | Outcome | Age % | Period % | Cohort % | P/C Ratio |",
  "|--------|---------|-------|----------|----------|-----------|",
  apply(summary_table, 1, function(row) {
    sprintf("| %s | %s | %.1f | %.2f | %.2f | %.2f |",
            row["survey"], row["outcome"],
            as.numeric(row["age_pct"]),
            as.numeric(row["period_pct"]),
            as.numeric(row["cohort_pct"]),
            as.numeric(row["pc_ratio"]))
  }),
  "",
  "## Interpretation",
  "",
  "- **Age dominates**: Age explains 5-10% of variance across all surveys",
  "- **Period vs Cohort varies by survey**:",
  "  - NHANES, MEPS: Period > Cohort (P/C ratio > 1)",
  "  - GSS, BRFSS: Cohort > Period (P/C ratio < 1)",
  "",
  "This pattern suggests different mechanisms may be at play across surveys,",
  "or that the longer time spans in GSS/BRFSS allow cohort effects to emerge."
)

writeLines(report, file.path(OUTPUT_DIR, "reports", "apc_summary.md"))
cat("Saved: apc_summary.md\n")

cat("\n=== Summary complete ===\n")
