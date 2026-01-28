# ==============================================================================
# 08_bhapc_summary_figure.R
# Create comprehensive one-pager summary of NHANES BHAPC analysis
# Layout: Top row = Lexis, Descriptive, Age Slope by Period
#         Below: Age/Period/Cohort effects + Variance table stacked on left
#                Gloria Table 2 and Interaction table on right
# ==============================================================================

library(tidyverse)
library(here)
library(patchwork)
library(gridExtra)
library(grid)

output_dir <- here("output", "apc", "bhapc")

# ==============================================================================
# Load all data
# ==============================================================================

bhapc_df <- readRDS(here(output_dir, "nhanes_bhapc_data.rds"))
gradient_df <- read.csv(here(output_dir, "gradient_by_period_nhanes.csv"))
interaction_df <- read.csv(here(output_dir, "age_period_interaction_nhanes.csv"))
variance_df <- read.csv(here(output_dir, "nhanes_variance_decomposition.csv"))
gloria_table <- read.csv(here(output_dir, "table2_nhanes_gloria_format.csv"))
model_result <- readRDS(here(output_dir, "nhanes_bhapc_model.rds"))
model <- model_result$model
posterior <- as.matrix(model)

# ==============================================================================
# FIGURES
# ==============================================================================

# --- A. Lexis Diagram ---
lexis_data <- bhapc_df %>%
  mutate(
    age_num = as.numeric(gsub("-.*", "", as.character(age_group))),
    period_num = as.numeric(period_4yr)
  ) %>%
  group_by(period_num, age_num) %>%
  summarise(mean_srh = weighted.mean(srh, wt, na.rm = TRUE), .groups = "drop")

p_lexis <- ggplot(lexis_data, aes(x = period_num, y = age_num, fill = mean_srh)) +
  geom_tile() +
  geom_abline(intercept = seq(-2020, -1900, by = 10), slope = 1,
              color = "gray30", linetype = "dashed", alpha = 0.4, linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "SRH", limits = c(2.9, 3.8)) +
  scale_x_continuous(breaks = c(1999, 2007, 2015)) +
  labs(title = "Lexis Diagram", x = "Period", y = "Age") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.2, "cm"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

# --- B. Descriptive Means ---
means_data <- bhapc_df %>%
  group_by(age_group, period_4yr) %>%
  summarise(mean_srh = weighted.mean(srh, wt, na.rm = TRUE), .groups = "drop")

p_descriptive <- ggplot(means_data, aes(x = age_group, y = mean_srh,
                                         color = period_4yr, group = period_4yr)) +
  geom_line(linewidth = 0.4) +
  geom_point(size = 0.6) +
  scale_color_viridis_d(option = "D", direction = -1, name = "Period") +
  labs(title = "Mean SRH by Age & Period", x = "Age Group", y = "Mean SRH") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "right",
    legend.key.size = unit(0.3, "cm"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

# --- C. Age Slope by Period (interaction) ---
p_interaction <- ggplot(interaction_df, aes(x = factor(period), y = age_slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(group = 1), color = "#D55E00", linewidth = 0.5) +
  geom_point(size = 1.5, color = "#D55E00") +
  geom_errorbar(aes(ymin = ci_10, ymax = ci_90), width = 0.2, color = "#D55E00") +
  labs(title = "Age Slope by Period", x = "Period", y = "Age slope") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- D. Age Effect ---
age_coef <- mean(posterior[, "age"])
age_sq_coef <- mean(posterior[, "scale(age_squared)"])
age_sq_mean <- mean(bhapc_df$age_squared)
age_sq_sd <- sd(bhapc_df$age_squared)

ages <- 18:80
age_effect_samples <- sapply(ages, function(a) {
  linear <- posterior[, "age"] * (a - 18)
  scaled_sq <- (a^2 - age_sq_mean) / age_sq_sd
  scaled_sq_ref <- (18^2 - age_sq_mean) / age_sq_sd
  quadratic <- posterior[, "scale(age_squared)"] * (scaled_sq - scaled_sq_ref)
  linear + quadratic
})

age_df <- data.frame(
  age = ages,
  effect = colMeans(age_effect_samples),
  ci_lower = apply(age_effect_samples, 2, quantile, 0.05),
  ci_upper = apply(age_effect_samples, 2, quantile, 0.95)
)

p_age <- ggplot(age_df, aes(x = age, y = effect)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "#0072B2", alpha = 0.2) +
  geom_line(color = "#0072B2", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Age Effect", x = "Age", y = "Effect on SRH") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- E. Period Effects ---
period_cols <- grep("b\\[\\(Intercept\\) period_4yr:", colnames(posterior), value = TRUE)
period_names <- gsub(".*period_4yr:([0-9]+)\\]", "\\1", period_cols)

period_effects <- data.frame(
  period = as.numeric(period_names),
  estimate = sapply(period_cols, function(col) mean(posterior[, col])),
  ci_lower = sapply(period_cols, function(col) quantile(posterior[, col], 0.05)),
  ci_upper = sapply(period_cols, function(col) quantile(posterior[, col], 0.95))
)

p_period <- ggplot(period_effects, aes(x = factor(period), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, color = "#009E73") +
  geom_point(size = 1.5, color = "#009E73") +
  labs(title = "Period Effects", x = "Period", y = "Effect") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- F. Cohort Effects ---
cohort_cols <- grep("b\\[\\(Intercept\\) cohort_4yr:", colnames(posterior), value = TRUE)
cohort_names <- as.numeric(gsub(".*cohort_4yr:([0-9-]+)\\]", "\\1", cohort_cols))

cohort_effects <- data.frame(
  cohort = cohort_names,
  estimate = sapply(cohort_cols, function(col) mean(posterior[, col])),
  ci_lower = sapply(cohort_cols, function(col) quantile(posterior[, col], 0.05)),
  ci_upper = sapply(cohort_cols, function(col) quantile(posterior[, col], 0.95))
) %>% arrange(cohort)

p_cohort <- ggplot(cohort_effects, aes(x = cohort, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 2, color = "#CC79A7", alpha = 0.7) +
  geom_point(size = 0.8, color = "#CC79A7") +
  labs(title = "Cohort Effects", x = "Birth Cohort", y = "Effect") +
  scale_x_continuous(breaks = seq(1920, 2000, 20)) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

# ==============================================================================
# TABLES as ggplot grobs
# ==============================================================================

# --- Gloria Table 2: Full APC decomposition ---
gloria_display <- gloria_table %>%
  mutate(
    Effect = effect,
    Est. = ifelse(is.na(estimate), "", sprintf("%.2f", estimate)),
    `10%` = ifelse(is.na(ci_10), "", sprintf("%.2f", ci_10)),
    `90%` = ifelse(is.na(ci_90), "", sprintf("%.2f", ci_90)),
    ` ` = ifelse(sig == "*", "***", "")
  ) %>%
  select(Effect, Est., `10%`, `90%`, ` `)

t_gloria <- tableGrob(gloria_display, rows = NULL,
                      theme = ttheme_minimal(base_size = 9,
                                             core = list(fg_params = list(hjust = 0, x = 0.02),
                                                         bg_params = list(fill = "#f0f0f0")),
                                             colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold"),
                                                            bg_params = list(fill = "#d8d8d8"))))

# --- Variance Decomposition ---
var_table <- variance_df %>%
  filter(component != "Total") %>%
  mutate(
    Component = case_when(
      component == "period_4yr" ~ "Period",
      component == "cohort_4yr" ~ "Cohort",
      TRUE ~ "Residual"
    ),
    Var = sprintf("%.3f", variance),
    `%` = sprintf("%.1f%%", pct_of_total)
  ) %>%
  select(Component, Var, `%`)

t_variance <- tableGrob(var_table, rows = NULL,
                        theme = ttheme_minimal(base_size = 10,
                                               core = list(fg_params = list(hjust = 0, x = 0.05),
                                                           bg_params = list(fill = "#e8e8e8")),
                                               colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold"),
                                                              bg_params = list(fill = "#d0d0d0"))))

# --- Age×Period Interaction ---
int_table <- interaction_df %>%
  mutate(
    Period = as.character(period),
    Slope = sprintf("%.4f", age_slope),
    `vs 1999` = sprintf("%+.4f", interaction),
    ` ` = ifelse(significant == "*", "***", significant)
  ) %>%
  select(Period, Slope, `vs 1999`, ` `)

t_interaction <- tableGrob(int_table, rows = NULL,
                           theme = ttheme_minimal(base_size = 10,
                                                  core = list(fg_params = list(hjust = 0, x = 0.05),
                                                              bg_params = list(fill = "#f0f0f0")),
                                                  colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold"),
                                                                 bg_params = list(fill = "#d8d8d8"))))

# ==============================================================================
# Convert tables to ggplot objects
# ==============================================================================

wrap_table <- function(grob, title, valign = "center") {
  if (valign == "top") {
    # Get grob height and align to top
    grob_height <- sum(grob$heights)
    p <- ggplot() +
      annotation_custom(grob, ymin = 0.5, ymax = 1) +
      labs(title = title) +
      theme_void() +
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0)) +
      coord_cartesian(clip = "off") +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
  } else {
    p <- ggplot() +
      annotation_custom(grob) +
      labs(title = title) +
      theme_void() +
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0))
  }
  p
}

# Labels match new layout positions
p_t_variance <- wrap_table(t_variance, "Variance Decomposition")
p_t_gloria <- wrap_table(t_gloria, "BHAPC Model Effects")  # centered - it's tall enough
p_t_interaction <- wrap_table(t_interaction, "Age×Period Interaction", valign = "top")

# ==============================================================================
# Combine: Four-column layout
# Column 1: Variance Decomposition, Age Effect, Period Effects, Cohort Effects
# Column 2: BHAPC Model Effects (Gloria Table 2)
# Column 3: Age×Period Interaction table
# Column 4: Lexis, Descriptive, Age Slope by Period
# ==============================================================================

layout <- "
AAEEEEFFFGGG
AAEEEEFFFGGG
BBEEEEFFFGGG
BBEEEEFFFHHH
CCEEEEFFFHHH
CCEEEEFFFHHH
DDEEEEFFFIII
DDEEEEFFFIII
DDEEEEFFFIII
"

# Plots added in alphabetical order: A, B, C, D, E, F, G, H, I
# A = Variance Decomposition
# B = Age Effect
# C = Period Effects
# D = Cohort Effects
# E = BHAPC Model Effects (Gloria table)
# F = Age×Period Interaction table
# G = Lexis
# H = Mean SRH by Age & Period
# I = Age Slope by Period
combined <- p_t_variance + p_age + p_period + p_cohort +
  p_t_gloria + p_t_interaction +
  p_lexis + p_descriptive + p_interaction +
  plot_layout(design = layout) +
  plot_annotation(
    title = "NHANES BHAPC Analysis Summary",
    subtitle = "Bayesian Hierarchical Age-Period-Cohort decomposition of Self-Rated Health (N = 75,367)",
    caption = "Higher SRH = better health (1-5 scale). Error bars/ribbons show 90% CIs. * = significant (CI excludes 0).",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      plot.caption = element_text(size = 10, color = "gray50")
    )
  ) &
  theme(plot.margin = margin(2, 2, 2, 2))

# Save
ggsave(here(output_dir, "nhanes_bhapc_summary.png"), combined,
       width = 16, height = 11, dpi = 300)
message("Saved: nhanes_bhapc_summary.png")

ggsave(here(output_dir, "nhanes_bhapc_summary.pdf"), combined,
       width = 16, height = 11)
message("Saved: nhanes_bhapc_summary.pdf")

message("\nDone!")
