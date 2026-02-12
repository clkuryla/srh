# ==============================================================================
# export_metaregression_table.R
# Export metaregression results as a formatted Word (.docx) table
# ==============================================================================

library(tidyverse)
library(here)
library(officer)
library(flextable)

# Source paths
source(here::here("R/paths.R"))

# --- Load data ---
meta <- readr::read_csv(
  here::here("output", "tables", "fig1b_metaregression_20260116.csv"),
  show_col_types = FALSE
)

# --- Format table for publication ---
meta_formatted <- meta |>
  mutate(
    survey = if_else(survey == "Pooled (all surveys)", "Pooled", survey),
    slope_formatted = formatC(slope, format = "e", digits = 2),
    se_formatted = formatC(slope_se, format = "e", digits = 1),
    p_value_formatted = case_when(
      slope_p_value < 0.001 ~ "<0.001",
      TRUE ~ formatC(slope_p_value, format = "f", digits = 3)
    ),
    r_squared_formatted = formatC(r_squared, format = "f", digits = 2),
    year_range = paste0(year_min, "-", year_max)
  ) |>
  select(
    Survey = survey,
    `Slope (per year)` = slope_formatted,
    SE = se_formatted,
    `P-value` = p_value_formatted,
    R2 = r_squared_formatted,
    `N Years` = n_years,
    `Year Range` = year_range
  )

# Fix column header to use superscript later via flextable
names(meta_formatted)[names(meta_formatted) == "R2"] <- "R\u00B2"

# --- Build flextable ---
ft <- flextable(meta_formatted) |>
  set_caption(caption = "Table. Inverse-variance weighted metaregression of age coefficients on calendar year") |>
  theme_booktabs() |>
  fontsize(size = 10, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header") |>
  bold(i = nrow(meta_formatted), part = "body") |>
  hline(i = nrow(meta_formatted) - 1, part = "body",
        border = fp_border(color = "black", width = 1)) |>
  align(align = "center", part = "header") |>
  align(j = 1, align = "left", part = "body") |>
  align(j = 2:ncol(meta_formatted), align = "center", part = "body") |>
  autofit() |>
  width(j = 1, width = 1.0) |>
  add_footer_lines(
    "Note: Slope represents the change in the age coefficient (from SRH ~ age) per calendar year. Positive slopes indicate the negative age coefficient is trending toward zero (convergence). Pooled analysis combines estimates from all surveys."
  ) |>
  fontsize(size = 9, part = "footer") |>
  italic(part = "footer")

# --- Write to Word ---
out_path <- here::here("output", "tables", "metaregression_results.docx")

doc <- read_docx() |>
  body_add_flextable(ft)

print(doc, target = out_path)

cat("Saved:", out_path, "\n")
