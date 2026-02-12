# ==============================================================================
# export_sensitivity_metaregression_tables.R
# Export sensitivity analysis metaregression tables as a formatted Word (.docx)
# ==============================================================================

library(tidyverse)
library(here)
library(officer)
library(flextable)

# --- Helper: build a formatted flextable from a data frame ---
make_meta_ft <- function(df, caption, footer_note) {
  ft <- flextable(df) |>
    set_caption(caption = caption) |>
    theme_booktabs() |>
    fontsize(size = 10, part = "all") |>
    font(fontname = "Times New Roman", part = "all") |>
    bold(part = "header") |>
    align(align = "center", part = "header") |>
    align(j = 1, align = "left", part = "body") |>
    align(j = 2:ncol(df), align = "center", part = "body") |>
    autofit() |>
    width(j = 1, width = 1.0)

  if (!is.null(footer_note)) {
    ft <- ft |>
      add_footer_lines(footer_note) |>
      fontsize(size = 9, part = "footer") |>
      italic(part = "footer")
  }
  ft
}

# ==============================================================================
# 1. Sociodemographic metaregression (Figure S3)
# ==============================================================================

sociodem <- read_csv(
  here::here("..", "srh", "output", "sensitivity", "sociodemographic",
             "tables", "figS3_metaregression_by_stratum_20260211.csv"),
  show_col_types = FALSE
)

format_sociodem <- function(df, stratum_name) {
  df |>
    filter(stratum_label == stratum_name) |>
    mutate(
      slope_formatted  = formatC(slope, format = "e", digits = 2),
      se_formatted     = formatC(slope_se, format = "e", digits = 1),
      p_value_formatted = case_when(
        p_value < 0.001 ~ "<0.001",
        TRUE ~ formatC(p_value, format = "f", digits = 3)
      )
    ) |>
    select(
      Survey        = survey,
      Stratum       = stratum_level,
      `Slope (per year)` = slope_formatted,
      SE            = se_formatted,
      `P-value`     = p_value_formatted,
      `N Years`     = n_years
    )
}

sociodem_sex  <- format_sociodem(sociodem, "Sex")
sociodem_race <- format_sociodem(sociodem, "Race/Ethnicity")
sociodem_educ <- format_sociodem(sociodem, "Education")

# Clean up stratum labels for education
sociodem_educ <- sociodem_educ |>
  mutate(Stratum = case_match(
    Stratum,
    "BA_plus"     ~ "BA+",
    "HS_SomeColl" ~ "HS / Some College",
    "LT_HS"       ~ "< HS",
    .default      = Stratum
  ))

sociodem_footer <- "Note: Slope represents the change in the age coefficient (from SRH ~ age) per calendar year, within each stratum."

ft_sex  <- make_meta_ft(sociodem_sex,  "Table S3a. Metaregression by Sex",             sociodem_footer)
ft_race <- make_meta_ft(sociodem_race, "Table S3b. Metaregression by Race/Ethnicity",  sociodem_footer)
ft_educ <- make_meta_ft(sociodem_educ, "Table S3c. Metaregression by Education Level", sociodem_footer)

# ==============================================================================
# 2. Covariate-adjusted age coefficients (Figure S1)
# ==============================================================================

adj_coefs <- read_csv(
  here::here("..", "srh", "output", "sensitivity", "sociodemographic",
             "figS1_age_adjusted_coefficients_20260211.csv"),
  show_col_types = FALSE
)

format_adj_coefs <- function(df, covariate_name) {
  df |>
    filter(covariate_adjusted == covariate_name) |>
    mutate(
      coef_formatted = formatC(coefficient, format = "e", digits = 2),
      se_formatted   = formatC(se, format = "e", digits = 1),
      p_value_formatted = case_when(
        p_value < 0.001 ~ "<0.001",
        TRUE ~ formatC(p_value, format = "f", digits = 3)
      ),
      ci = paste0("[", formatC(ci_lower, format = "f", digits = 4), ", ",
                  formatC(ci_upper, format = "f", digits = 4), "]"),
      year = as.character(year)
    ) |>
    select(
      Survey     = survey,
      Year       = year,
      `Age Coef.` = coef_formatted,
      SE         = se_formatted,
      `P-value`  = p_value_formatted,
      `95% CI`   = ci,
      N          = n_unweighted
    )
}

adj_sex  <- format_adj_coefs(adj_coefs, "Sex")
adj_race <- format_adj_coefs(adj_coefs, "Race/Ethnicity")
adj_educ <- format_adj_coefs(adj_coefs, "Education")

adj_footer <- "Note: Age coefficient from SRH ~ age, adjusted for the indicated covariate. Survey-weighted estimates. N is unweighted sample size."

ft_adj_sex  <- make_meta_ft(adj_sex,  "Table S1a. Covariate-adjusted age coefficients (adjusted for Sex)",             adj_footer)
ft_adj_race <- make_meta_ft(adj_race, "Table S1b. Covariate-adjusted age coefficients (adjusted for Race/Ethnicity)",  adj_footer)
ft_adj_educ <- make_meta_ft(adj_educ, "Table S1c. Covariate-adjusted age coefficients (adjusted for Education Level)", adj_footer)

# ==============================================================================
# 3. Dichotomized metaregression
# ==============================================================================

dichot_dir <- here::here("output", "sensitivity", "dichotomized", "tables")

dichot_files <- list(
  "Good+"              = "fig1_dichot_good_plus_meta_20260211.rds",
  "Excellent"          = "fig1_dichot_excellent_meta_20260211.rds",
  "Excellent/Very Good" = "fig1_dichot_excellent_vgood_meta_20260211.rds",
  "Not Poor"           = "fig1_dichot_not_poor_meta_20260211.rds"
)

format_dichot <- function(df, threshold_label) {
  df |>
    mutate(
      threshold = threshold_label,
      slope_formatted = formatC(slope, format = "e", digits = 2),
      se_formatted    = formatC(slope_se, format = "e", digits = 1),
      p_value_formatted = case_when(
        slope_p_value < 0.001 ~ "<0.001",
        TRUE ~ formatC(slope_p_value, format = "f", digits = 3)
      ),
      r_squared_formatted = formatC(r_squared, format = "f", digits = 2),
      year_range = paste0(year_min, "-", year_max),
      coef_type = if_else(coefficient_type == "log_odds", "Log-odds", "Marginal")
    ) |>
    select(
      Survey        = survey,
      `Coef. Type`  = coef_type,
      `Slope (per year)` = slope_formatted,
      SE            = se_formatted,
      `P-value`     = p_value_formatted,
      R2            = r_squared_formatted,
      `N Years`     = n_years,
      `Year Range`  = year_range
    ) |>
    rename_with(~ "R\u00B2", .cols = "R2")
}

dichot_tables <- imap(dichot_files, \(file, label) {
  df <- readRDS(file.path(dichot_dir, file))
  format_dichot(df, label)
})

dichot_footer <- "Note: Slope represents the change in the age coefficient per calendar year using dichotomized SRH. Log-odds coefficients are from logistic regression; marginal coefficients are average marginal effects."

ft_dichot <- imap(dichot_tables, \(df, label) {
  make_meta_ft(
    df,
    caption = paste0("Table. Metaregression of dichotomized SRH (", label, ") age coefficients on year"),
    footer_note = dichot_footer
  )
})

# ==============================================================================
# 3. Assemble Word document
# ==============================================================================

doc <- read_docx()

# --- Sociodemographic section ---
doc <- doc |>
  body_add_par("Sensitivity Analysis: Sociodemographic Strata", style = "heading 1") |>
  body_add_par("") |>
  body_add_flextable(ft_sex) |>
  body_add_break() |>
  body_add_flextable(ft_race) |>
  body_add_break() |>
  body_add_flextable(ft_educ) |>
  body_add_break()

# --- Covariate-adjusted age coefficients section ---
doc <- doc |>
  body_add_par("Sensitivity Analysis: Covariate-Adjusted Age Coefficients", style = "heading 1") |>
  body_add_par("") |>
  body_add_flextable(ft_adj_sex) |>
  body_add_break() |>
  body_add_flextable(ft_adj_race) |>
  body_add_break() |>
  body_add_flextable(ft_adj_educ) |>
  body_add_break()

# --- Dichotomized section ---
doc <- doc |>
  body_add_par("Sensitivity Analysis: Dichotomized SRH", style = "heading 1") |>
  body_add_par("")

for (i in seq_along(ft_dichot)) {
  doc <- doc |> body_add_flextable(ft_dichot[[i]])
  if (i < length(ft_dichot)) {
    doc <- doc |> body_add_break()
  }
}

# --- Save ---
out_path <- here::here("output", "tables", "sensitivity_metaregression_results.docx")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
print(doc, target = out_path)

cat("Saved:", out_path, "\n")
