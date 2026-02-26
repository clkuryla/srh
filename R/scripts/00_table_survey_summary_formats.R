# =============================================================================
# Table Formatting Options: Survey Summary Table
# Demonstrates several R packages for publication-ready tables
# Author: Christine Kuryla
# =============================================================================

library(tidyverse)
library(here)
library(scales)

# Load the data
survey_raw <- read_csv(here::here("output/tables/table_survey_summary.csv"),
                       show_col_types = FALSE)

# Compute totals for caption (before formatting numbers as strings)
total_n_val     <- sum(survey_raw$`Total N`)
total_waves_val <- sum(survey_raw$`# Waves`)
caption_text <- paste0(
  "Table. Summary of survey datasets included in analyses. ",
  "Total: ", total_waves_val, " survey waves and ",
  comma(total_n_val), " participants across 6 nationally representative U.S. surveys."
)

# Format numbers with commas for display
survey_data <- survey_raw %>%
  mutate(
    `Avg N/Wave` = comma(`Avg N/Wave`),
    `Total N` = comma(`Total N`)
  )

# -----------------------------------------------------------------------------
# Option 1: gt (modern, highly customizable, great for HTML/PDF)
# -----------------------------------------------------------------------------

library(gt)

table_gt <- survey_data %>%
  gt() %>%
  tab_header(
    title = "Summary of Survey Datasets",
    subtitle = "Six nationally representative U.S. health surveys"
  ) %>%
  tab_source_note(
    source_note = "Note: SRH = self-rated health. All surveys use 5-point scale (1=Poor to 5=Excellent) except GSS (4-point, no 'Very Good')."
  ) %>%
  cols_label(
    Source = "Source",
    Survey = "Survey",
    `Year Range` = "Years",
    `# Waves` = "Waves",
    `Avg N/Wave` = "Avg N/Wave",
    `Total N` = "Total N"
  ) %>%
  cols_align(align = "left", columns = c(Source, Survey)) %>%
  cols_align(align = "center", columns = c(`Year Range`, `# Waves`)) %>%
  cols_align(align = "right", columns = c(`Avg N/Wave`, `Total N`)) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(12),
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    heading.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid"
  )

# Save gt table
gtsave(table_gt, here::here("output/tables/table_survey_summary_gt.html"))
gtsave(table_gt, here::here("output/tables/table_survey_summary_gt.png"))
cat("Saved: table_survey_summary_gt.html and .png\n")

# -----------------------------------------------------------------------------
# Option 2: kableExtra (great for LaTeX/PDF, also HTML)
# -----------------------------------------------------------------------------

library(kableExtra)

table_kable <- survey_data %>%
  kbl(
    caption = "Summary of Survey Datasets",
    col.names = c("Source", "Survey", "Years", "Waves", "Avg N/Wave", "Total N"),
    align = c("l", "l", "c", "c", "r", "r"),
    booktabs = TRUE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    latex_options = c("striped", "hold_position"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  footnote(
    general = "SRH = self-rated health. All surveys use 5-point scale (1=Poor to 5=Excellent) except GSS (4-point).",
    general_title = "Note: ",
    footnote_as_chunk = TRUE
  )

# Save kableExtra (HTML version)
tryCatch({
  save_kable(table_kable, here::here("output/tables/table_survey_summary_kable.html"))
  cat("Saved: table_survey_summary_kable.html\n")
}, error = function(e) {
  cat("Note: Could not save kable HTML (pandoc issue). Skipping.\n")
})

# -----------------------------------------------------------------------------
# Option 3: flextable (great for Word documents)
# -----------------------------------------------------------------------------

library(flextable)
library(officer)  # for fp_border

table_flex <- survey_data |>
  flextable() |>
  set_header_labels(
    Source      = "Source",
    Survey      = "Survey",
    `Year Range` = "Years",
    `# Waves`   = "Waves",
    `Avg N/Wave` = "Avg N/Wave",
    `Total N`   = "Total N"
  ) |>
  set_caption(caption = caption_text) |>
  theme_booktabs() |>
  fontsize(size = 10, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header") |>
  align(align = "center", part = "header") |>
  align(j = 1, align = "left",   part = "body") |>
  align(j = 2:6, align = "center", part = "body") |>
  autofit() |>
  width(j = 1, width = 2.5) |>
  add_footer_lines(
    "Note: SRH = self-rated health. All surveys use 5-point scale (1=Poor to 5=Excellent) except GSS (4-point, no 'Very Good')."
  ) |>
  fontsize(size = 9, part = "footer") |>
  italic(part = "footer")

# Save flextable
doc_flex <- read_docx() |> body_add_flextable(table_flex)
print(doc_flex, target = here::here("output/tables/table_survey_summary_flex.docx"))
save_as_image(table_flex, path = here::here("output/tables/table_survey_summary_flex.png"))
cat("Saved: table_survey_summary_flex.docx and .png\n")

# -----------------------------------------------------------------------------
# Option 4: huxtable (versatile, works across formats)
# -----------------------------------------------------------------------------
if (requireNamespace("huxtable", quietly = TRUE)) {
  library(huxtable)

  table_hux <- survey_data %>%
    as_hux() %>%
    set_bold(row = 1, value = TRUE) %>%
    set_bottom_border(row = 1, value = 0.4) %>%
    set_top_border(row = 1, value = 0.4) %>%
    set_bottom_border(row = nrow(.) , value = 0.4) %>%
    set_align(col = 1:2, value = "left") %>%
    set_align(col = 3:4, value = "center") %>%
    set_align(col = 5:6, value = "right") %>%
    set_caption("Summary of Survey Datasets") %>%
    set_font_size(10) %>%
    set_all_padding(2)

  # Add note as a new row
  table_hux <- table_hux %>%
    add_footnote("Note: All surveys use 5-point SRH scale except GSS (4-point).", border = 0)

  # Save huxtable
  quick_html(table_hux, file = here::here("output/tables/table_survey_summary_hux.html"))
  cat("Saved: table_survey_summary_hux.html\n")
} else {
  cat("Note: huxtable not installed. Skipping huxtable output.\n")
}

# -----------------------------------------------------------------------------
# Option 5: gt with enhanced styling for publication-ready images
# -----------------------------------------------------------------------------

table_gt_pub <- survey_data %>%
  gt() %>%
  tab_header(
    title = md("**Table 1.** Summary of Survey Datasets")
  ) %>%
  tab_source_note(
    source_note = md("*Note:* SRH = self-rated health. All surveys use 5-point scale (1=Poor to 5=Excellent) except GSS (4-point, no 'Very Good').")
  ) %>%
  cols_label(
    Source = "Source",
    Survey = "Survey",
    `Year Range` = "Years",
    `# Waves` = "Waves",
    `Avg N/Wave` = "Avg N/Wave",
    `Total N` = "Total N"
  ) %>%
  cols_align(align = "left", columns = c(Source, Survey)) %>%
  cols_align(align = "center", columns = c(`Year Range`, `# Waves`)) %>%
  cols_align(align = "right", columns = c(`Avg N/Wave`, `Total N`)) %>%
  cols_width(
    Source ~ px(280),
    Survey ~ px(70),
    `Year Range` ~ px(90),
    `# Waves` ~ px(60),
    `Avg N/Wave` ~ px(90),
    `Total N` ~ px(100)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Survey)
  ) %>%
  tab_options(
    table.font.size = px(13),
    table.font.names = "Arial",
    heading.title.font.size = px(15),
    heading.align = "left",
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    heading.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    table_body.hlines.color = "transparent",
    source_notes.font.size = px(11),
    data_row.padding = px(5)
  )

# Save publication-ready gt
gtsave(table_gt_pub, here::here("output/tables/table_survey_summary_gt_pub.html"))
gtsave(table_gt_pub, here::here("output/tables/table_survey_summary_gt_pub.png"), vwidth = 800, vheight = 400)
cat("Saved: table_survey_summary_gt_pub.html and .png (publication style)\n")

# -----------------------------------------------------------------------------
# Option 6: Minimalist academic style (gt)
# -----------------------------------------------------------------------------

table_gt_minimal <- survey_data %>%
  gt() %>%
  cols_label(
    Source = "Source",
    Survey = "Survey",
    `Year Range` = "Years",
    `# Waves` = "Waves",
    `Avg N/Wave` = "Avg N/Wave",
    `Total N` = "Total N"
  ) %>%
  cols_align(align = "left", columns = c(Source, Survey)) %>%
  cols_align(align = "center", columns = c(`Year Range`, `# Waves`)) %>%
  cols_align(align = "right", columns = c(`Avg N/Wave`, `Total N`)) %>%
  cols_width(
    Source ~ px(280),
    Survey ~ px(70),
    `Year Range` ~ px(90),
    `# Waves` ~ px(60),
    `Avg N/Wave` ~ px(90),
    `Total N` ~ px(100)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.font.size = px(12),
    table.font.names = "Times New Roman",
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "black",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    table_body.hlines.style = "none",
    table.background.color = "white",
    data_row.padding = px(4)
  )

gtsave(table_gt_minimal, here::here("output/tables/table_survey_summary_gt_minimal.html"))
gtsave(table_gt_minimal, here::here("output/tables/table_survey_summary_gt_minimal.png"), vwidth = 750, vheight = 350)
cat("Saved: table_survey_summary_gt_minimal.html and .png (minimal academic style)\n")

# -----------------------------------------------------------------------------
# Option 7: Publication flextable matching metaregression table style
# (Times New Roman, 10pt, booktabs, read_docx save)
# -----------------------------------------------------------------------------

table_flex_pub <- survey_data |>
  flextable() |>
  set_header_labels(
    Source      = "Source",
    Survey      = "Survey",
    `Year Range` = "Years",
    `# Waves`   = "Waves",
    `Avg N/Wave` = "Avg N/Wave",
    `Total N`   = "Total N"
  ) |>
  set_caption(caption = caption_text) |>
  theme_booktabs() |>
  fontsize(size = 10, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  bold(part = "header") |>
  align(align = "center", part = "header") |>
  align(j = 1, align = "left",   part = "body") |>
  align(j = 2:6, align = "center", part = "body") |>
  autofit() |>
  width(j = 1, width = 2.5) |>
  add_footer_lines(
    "Note: SRH = self-rated health. All surveys use 5-point scale (1=Poor to 5=Excellent) except GSS (4-point, no 'Very Good')."
  ) |>
  fontsize(size = 9, part = "footer") |>
  italic(part = "footer")

save_as_image(table_flex_pub,
              path = here::here("output/tables/table_survey_summary_flex_pub.png"),
              res = 300)

doc <- read_docx() |>
  body_add_flextable(table_flex_pub)
print(doc, target = here::here("output/tables/table_survey_summary_flex_pub.docx"))
cat("Saved: table_survey_summary_flex_pub.png (300 dpi) and .docx\n")

# -----------------------------------------------------------------------------
# Summary of packages
# -----------------------------------------------------------------------------

cat("\n")
cat("=======================================================\n")
cat("TABLE PACKAGE COMPARISON\n")
cat("=======================================================\n")
cat("\n")
cat("FILES CREATED:\n")
cat("\n")
cat("1. GT - Basic\n")
cat("   - table_survey_summary_gt.html\n")
cat("   - table_survey_summary_gt.png\n")
cat("\n")
cat("2. KABLEEXTRA\n")
cat("   - table_survey_summary_kable.html\n")
cat("\n")
cat("3. FLEXTABLE - Basic\n")
cat("   - table_survey_summary_flex.docx\n")
cat("   - table_survey_summary_flex.png\n")
cat("\n")
cat("4. HUXTABLE\n")
cat("   - table_survey_summary_hux.html\n")
cat("\n")
cat("5. GT - Publication style (RECOMMENDED FOR IMAGES)\n")
cat("   - table_survey_summary_gt_pub.html\n")
cat("   - table_survey_summary_gt_pub.png\n")
cat("\n")
cat("6. GT - Minimal academic style\n")
cat("   - table_survey_summary_gt_minimal.html\n")
cat("   - table_survey_summary_gt_minimal.png\n")
cat("\n")
cat("7. FLEXTABLE - Publication style (HIGH RES)\n")
cat("   - table_survey_summary_flex_pub.png (300 dpi)\n")
cat("   - table_survey_summary_flex_pub.docx\n")
cat("\n")
cat("=======================================================\n")
cat("RECOMMENDATIONS:\n")
cat("=======================================================\n")
cat("- Best image quality: gt_pub or flex_pub (300 dpi)\n")
cat("- For LaTeX/PDF journals: kableExtra\n")
cat("- For Word submissions: flextable\n")
cat("- Cleanest academic look: gt_minimal\n")
cat("=======================================================\n")
