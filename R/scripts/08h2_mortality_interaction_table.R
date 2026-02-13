# ==============================================================================
# 08h2_mortality_interaction_table.R
# Formatted GT table for SRH x Survey-Year Interaction Results
#
# Input:
#   output/sensitivity/mortality/changing_HR_per_age_group_1/
#     srh_year_interaction_by_agegroup.csv
#
# Output:
#   output/sensitivity/mortality/changing_HR_per_age_group_1/
#     table_interaction.{html,png}
#
# Author: Christine Lucille Kuryla
# ==============================================================================

library(tidyverse)
library(here)
library(gt)

source(here("R", "paths.R"))

# Output directory
output_dir <- here("output", "sensitivity", "mortality", "changing_HR_per_age_group_1")


# ==============================================================================
# LOAD DATA
# ==============================================================================

message("\n=== Loading interaction results ===\n")

results <- read_csv(
  file.path(output_dir, "srh_year_interaction_by_agegroup.csv"),
  show_col_types = FALSE
)

message(sprintf("Loaded %d rows", nrow(results)))


# ==============================================================================
# BUILD DISPLAY TABLE
# ==============================================================================

format_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "< 0.001",
    p < 0.01  ~ sprintf("%.3f", p),
    TRUE      ~ sprintf("%.3f", p)
  )
}

format_hr_ci <- function(hr, lo, hi) {
  sprintf("%.3f (%.3f, %.3f)", hr, lo, hi)
}

display <- results %>%
  filter(converged) %>%
  mutate(
    N = format(n, big.mark = ","),
    Deaths = format(n_events, big.mark = ","),
    `HR at 1990 (95% CI)` = format_hr_ci(HR_ref, HR_ref_low, HR_ref_high),
    `p (SRH)` = format_p(p_srh),
    `HR Multiplier per Decade (95% CI)` = format_hr_ci(
      HR_mult_decade, HR_mult_decade_low, HR_mult_decade_high
    ),
    `p (interaction)` = format_p(p_int),
    `FDR p` = format_p(p_int_fdr),
    `F` = sprintf("%.1f", F_stat),
    Sig = case_when(
      p_int_fdr < 0.001 ~ "***",
      p_int_fdr < 0.01  ~ "**",
      p_int_fdr < 0.05  ~ "*",
      TRUE              ~ ""
    )
  ) %>%
  select(
    `Age Group` = age_group,
    N, Deaths,
    `HR at 1990 (95% CI)`,
    `p (SRH)`,
    `HR Multiplier per Decade (95% CI)`,
    `F`,
    `p (interaction)`,
    `FDR p`,
    Sig
  )


# ==============================================================================
# CREATE GT TABLE
# ==============================================================================

message("=== Building GT table ===\n")

gt_table <- display %>%
  gt() %>%
  tab_header(
    title = md("**SRH \u00D7 Survey Year Interaction: Mortality Hazard Ratios by Age Group**"),
    subtitle = md(
      "NHIS 1986\u20132014, survey-weighted Cox PH (age time scale, 10-year follow-up)<br>SRH coded 1\u20135 (higher = better health); year centered at 1990, scaled per decade"
    )
  ) %>%
  # Column spanners
  tab_spanner(
    label = md("**Sample**"),
    columns = c(N, Deaths)
  ) %>%
  tab_spanner(
    label = md("**SRH Main Effect (at 1990)**"),
    columns = c(`HR at 1990 (95% CI)`, `p (SRH)`)
  ) %>%
  tab_spanner(
    label = md("**SRH \u00D7 Year Interaction**"),
    columns = c(`HR Multiplier per Decade (95% CI)`, `F`, `p (interaction)`, `FDR p`, Sig)
  ) %>%
  # Column labels
  cols_label(
    `Age Group` = md("**Age Group**"),
    N = md("**N**"),
    Deaths = md("**Deaths**"),
    `HR at 1990 (95% CI)` = md("**HR (95% CI)**"),
    `p (SRH)` = md("**p**"),
    `HR Multiplier per Decade (95% CI)` = md("**HR Multiplier (95% CI)**"),
    `F` = md("**F**"),
    `p (interaction)` = md("**p**"),
    `FDR p` = md("**FDR p**"),
    Sig = ""
  ) %>%
  # Bold age group
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `Age Group`)
  ) %>%
  # Alternating row shading
  tab_style(
    style = cell_fill(color = "#f0f4f8"),
    locations = cells_body(rows = c(2, 4, 6))
  ) %>%
  # Bold the HR values (number before the parenthesized CI)
  text_transform(
    locations = cells_body(
      columns = c(`HR at 1990 (95% CI)`, `HR Multiplier per Decade (95% CI)`)
    ),
    fn = function(x) {
      gsub("^(\\d+\\.\\d+)( \\()", "<b>\\1</b>\\2", x)
    }
  ) %>%
  # Highlight significant rows in the interaction columns
  tab_style(
    style = cell_text(weight = "bold", color = "#1a5276"),
    locations = cells_body(
      columns = c(`HR Multiplier per Decade (95% CI)`, `p (interaction)`, `FDR p`, Sig),
      rows = `FDR p` != "" & !grepl("^0\\.[1-9]|^1", `FDR p`)
    )
  ) %>%
  # Significance stars styling
  tab_style(
    style = cell_text(weight = "bold", size = px(16)),
    locations = cells_body(columns = Sig)
  ) %>%
  # Footnotes
  tab_footnote(
    footnote = md("**HR at 1990:** Hazard ratio per 1-unit increase in SRH at the reference year (1990). HR < 1 indicates higher SRH is protective against mortality.")
  ) %>%
  tab_footnote(
    footnote = md("**HR Multiplier per Decade:** exp(interaction coefficient). Values < 1 indicate SRH is becoming more predictive of mortality over calendar time (i.e., the HR moves further below 1 each decade).")
  ) %>%
  tab_footnote(
    footnote = md("**F:** Design-based Wald F-statistic from `regTermTest()`. **FDR p:** Benjamini-Hochberg adjusted across 7 age groups. \\* p < 0.05, \\*\\* p < 0.01, \\*\\*\\* p < 0.001.")
  ) %>%
  # Source note
  tab_source_note(
    source_note = md("Model: `svycoxph(Surv(age_at_survey, age_at_end, event) ~ srh * year10)` with `year10 = (survey_year - 1990) / 10`. Weights-only survey design (`ids = ~1`).")
  ) %>%
  # Table formatting
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(12),
    column_labels.font.size = px(12),
    data_row.padding = px(6),
    table.width = pct(100),
    table.border.top.color = "gray40",
    table.border.bottom.color = "gray40",
    heading.border.bottom.color = "gray60",
    column_labels.border.bottom.color = "gray60",
    footnotes.font.size = px(11),
    source_notes.font.size = px(10)
  )


# ==============================================================================
# SAVE
# ==============================================================================

message("\n=== Saving table ===\n")

# HTML
gtsave(gt_table, file.path(output_dir, "table_interaction.html"))
message("Saved: table_interaction.html")

# PNG
gtsave(gt_table, file.path(output_dir, "table_interaction.png"), vwidth = 1400)
message("Saved: table_interaction.png")


# ==============================================================================
# SAVE AS .DOCX VIA FLEXTABLE + OFFICER
# ==============================================================================

message("\n=== Building Word document ===\n")

library(flextable)
library(officer)

# Build flextable from display data
ft <- flextable(display) %>%
  # Column widths
  set_table_properties(layout = "autofit") %>%
  # Header labels
  set_header_labels(
    `Age Group` = "Age Group",
    N = "N",
    Deaths = "Deaths",
    `HR at 1990 (95% CI)` = "HR (95% CI)",
    `p (SRH)` = "p",
    `HR Multiplier per Decade (95% CI)` = "HR Multiplier (95% CI)",
    `F` = "F",
    `p (interaction)` = "p",
    `FDR p` = "FDR p",
    Sig = ""
  ) %>%
  # Add spanner row above
  add_header_row(
    values = c("", "Sample", "SRH Main Effect (at 1990)",
               "SRH \u00D7 Year Interaction"),
    colwidths = c(1, 2, 2, 5),
    top = TRUE
  ) %>%
  # Bold headers
  bold(part = "header") %>%
  # Bold age group column

  bold(j = "Age Group", part = "body") %>%
  # Bold significance stars
  bold(j = "Sig", part = "body") %>%
  # Alignment
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = c(2, 3), align = "right", part = "body") %>%
  align(j = c(4:10), align = "center", part = "body") %>%
  # Font
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 10, part = "body") %>%
  fontsize(size = 10, part = "header") %>%
  # Borders
  hline_top(border = fp_border(width = 2), part = "header") %>%
  hline_bottom(border = fp_border(width = 2), part = "body") %>%
  hline(i = 1, border = fp_border(width = 1), part = "header") %>%
  hline(i = 2, border = fp_border(width = 1), part = "header") %>%
  # Alternating row shading
  bg(i = c(2, 4, 6), bg = "#f0f4f8", part = "body") %>%
  # Padding
  padding(padding = 3, part = "all")

# Build Word document
doc <- read_docx() %>%
  # Title
  body_add_par(
    "SRH \u00D7 Survey Year Interaction: Mortality Hazard Ratios by Age Group",
    style = "heading 1"
  ) %>%
  body_add_par(
    paste0(
      "NHIS 1986\u20132014, survey-weighted Cox PH (age time scale, 10-year follow-up). ",
      "SRH coded 1\u20135 (higher = better health); year centered at 1990, scaled per decade."
    ),
    style = "Normal"
  ) %>%
  body_add_par("", style = "Normal") %>%
  # Table
  body_add_flextable(ft, align = "left") %>%
  body_add_par("", style = "Normal") %>%
  # Footnotes
  body_add_par("Notes:", style = "heading 2") %>%
  body_add_par(
    paste0(
      "HR at 1990: Hazard ratio per 1-unit increase in SRH at the reference year (1990). ",
      "HR < 1 indicates higher SRH is protective against mortality."
    ),
    style = "Normal"
  ) %>%
  body_add_par(
    paste0(
      "HR Multiplier per Decade: exp(interaction coefficient). Values < 1 indicate SRH ",
      "is becoming more predictive of mortality over calendar time (i.e., the HR moves ",
      "further below 1 each decade)."
    ),
    style = "Normal"
  ) %>%
  body_add_par(
    paste0(
      "F: Design-based Wald F-statistic from regTermTest(). ",
      "FDR p: Benjamini-Hochberg adjusted across 7 age groups. ",
      "* p < 0.05, ** p < 0.01, *** p < 0.001."
    ),
    style = "Normal"
  ) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(
    paste0(
      "Model: svycoxph(Surv(age_at_survey, age_at_end, event) ~ srh * year10) ",
      "with year10 = (survey_year - 1990) / 10. Weights-only survey design (ids = ~1)."
    ),
    style = "Normal"
  )

# Save
docx_path <- file.path(output_dir, "table_interaction.docx")
print(doc, target = docx_path)
message(sprintf("Saved: %s", docx_path))

message("\n=== Done ===")
