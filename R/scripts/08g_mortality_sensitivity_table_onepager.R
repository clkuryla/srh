# ==============================================================================
# 08g_mortality_sensitivity_table_onepager.R
# Mortality Sensitivity Analysis: Combined Table One-Pager
#
# Purpose:
#   Create a single-page summary with formatted HR tables for the most recent
#   10-year rolling window:
#     Table A: Adjusted models (Unadjusted, +Sex, +Education, +Race/Ethnicity)
#     Table B: Stratified by Sex
#     Table C: Stratified by Education
#     Table D: Stratified by Race/Ethnicity
#
# Input:
#   output/sensitivity/mortality/table_mort_hr_adjusted_wide_{date}.csv
#   output/sensitivity/mortality/table_mort_hr_stratified_sex_wide_{date}.csv
#   output/sensitivity/mortality/table_mort_hr_stratified_educ_wide_{date}.csv
#   output/sensitivity/mortality/table_mort_hr_stratified_race_wide_{date}.csv
#
# Output:
#   output/sensitivity/mortality/table_mort_sensitivity_onepager.{png,html}
#
# Author: Christine Lucille Kuryla
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(tidyverse)
library(here)
library(gt)

source(here("R", "paths.R"))

# Output directory
output_dir <- here("output", "sensitivity", "mortality")

# Analysis date
date_suffix <- format(Sys.Date(), "%Y%m%d")


# ==============================================================================
# LOAD TABLE DATA
# ==============================================================================

message("\n=== Loading table data ===\n")

# Find most recent files
find_latest <- function(pattern) {
  files <- list.files(output_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files matching: ", pattern)
  sort(files, decreasing = TRUE)[1]
}

adj_wide <- read_csv(find_latest("table_mort_hr_adjusted_wide_\\d+\\.csv"),
                     show_col_types = FALSE)
sex_wide <- read_csv(find_latest("table_mort_hr_stratified_sex_wide_\\d+\\.csv"),
                     show_col_types = FALSE)
educ_wide <- read_csv(find_latest("table_mort_hr_stratified_educ_wide_\\d+\\.csv"),
                      show_col_types = FALSE)
race_wide <- read_csv(find_latest("table_mort_hr_stratified_race_wide_\\d+\\.csv"),
                      show_col_types = FALSE)

# Also load long format for sample size info
adj_long <- read_csv(find_latest("table_mort_hr_adjusted_\\d+\\.csv"),
                     show_col_types = FALSE)

# Get the window used
most_recent_start <- adj_long %>%
  filter(model_label == "Unadjusted") %>%
  pull(n) %>%
  { adj_long %>% filter(model_label == "Unadjusted") }

# Extract window info from the adjusted results
adj_results_file <- find_latest("mortality_adjusted_results_\\d+\\.csv")
adj_results <- read_csv(adj_results_file, show_col_types = FALSE)
window_start <- adj_results %>%
  filter(model_label == "Unadjusted", converged) %>%
  pull(start_year) %>%
  max(na.rm = TRUE)
window_end <- window_start + 9

message(sprintf("Window: %d-%d", window_start, window_end))


# ==============================================================================
# BUILD COMBINED TABLE
# ==============================================================================

message("\n=== Building combined table ===\n")

# --- Prepare Table A: Adjusted ---
# Rename columns for clarity
table_a <- adj_wide %>%
  rename(`Age Group` = age_group) %>%
  mutate(Section = "A. Covariate-Adjusted Models", .before = 1)

# --- Prepare Table B: Sex ---
table_b <- sex_wide %>%
  rename(`Age Group` = age_group) %>%
  mutate(Section = "B. Stratified by Sex", .before = 1)

# --- Prepare Table C: Education ---
table_c <- educ_wide %>%
  rename(`Age Group` = age_group) %>%
  mutate(Section = "C. Stratified by Education", .before = 1)

# --- Prepare Table D: Race/Ethnicity ---
table_d <- race_wide %>%
  rename(`Age Group` = age_group) %>%
  mutate(Section = "D. Stratified by Race/Ethnicity", .before = 1)

# Get N and events from unadjusted model
n_info <- adj_long %>%
  filter(model_label == "Unadjusted") %>%
  select(`Age Group` = age_group, N = n, Deaths = n_events)


# ==============================================================================
# CREATE GT TABLES
# ==============================================================================

message("=== Creating GT tables ===\n")

# --- Table A: Adjusted ---
gt_a <- table_a %>%
  select(-Section) %>%
  left_join(n_info, by = "Age Group") %>%
  mutate(
    N = format(N, big.mark = ","),
    Deaths = format(Deaths, big.mark = ",")
  ) %>%
  gt() %>%
  tab_header(
    title = md("**SRH-Mortality Hazard Ratios: Sociodemographic Sensitivity Analysis**"),
    subtitle = md(sprintf("NHIS, %d\u2013%d, survey-weighted Cox PH | HR (95%% CI) per 1-unit increase in SRH (higher = better health)", window_start, window_end))
  ) %>%
  tab_spanner(
    label = md("**A. Covariate-Adjusted Models**"),
    columns = c(Unadjusted, `Adjusted: Sex`, `Adjusted: Education`, `Adjusted: Race/Ethnicity`)
  ) %>%
  cols_label(
    `Age Group` = md("**Age Group**"),
    Unadjusted = md("**Unadjusted**"),
    `Adjusted: Sex` = md("**+ Sex**"),
    `Adjusted: Education` = md("**+ Education**"),
    `Adjusted: Race/Ethnicity` = md("**+ Race/Ethnicity**"),
    N = md("**N**"),
    Deaths = md("**Deaths**")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `Age Group`)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f4f8"),
    locations = cells_body(rows = seq(2, 7, by = 2))
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    column_labels.font.size = px(12),
    data_row.padding = px(5),
    table.width = pct(100)
  )

# --- Table B: Stratified by Sex ---
gt_b <- table_b %>%
  select(-Section) %>%
  gt() %>%
  tab_spanner(
    label = md("**B. Stratified by Sex**"),
    columns = c(Male, Female)
  ) %>%
  cols_label(
    `Age Group` = md("**Age Group**"),
    Male = md("**Male**"),
    Female = md("**Female**")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `Age Group`)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f4f8"),
    locations = cells_body(rows = seq(2, 7, by = 2))
  ) %>%
  tab_options(
    table.font.size = px(13),
    column_labels.font.size = px(12),
    data_row.padding = px(5),
    table.width = pct(100)
  )

# --- Table C: Stratified by Education ---
gt_c <- table_c %>%
  select(-Section) %>%
  gt() %>%
  tab_spanner(
    label = md("**C. Stratified by Education**"),
    columns = c(`< HS`, `HS/Some college`, `College+`)
  ) %>%
  cols_label(
    `Age Group` = md("**Age Group**"),
    `< HS` = md("**< HS**"),
    `HS/Some college` = md("**HS/Some College**"),
    `College+` = md("**College+**")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `Age Group`)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f4f8"),
    locations = cells_body(rows = seq(2, 7, by = 2))
  ) %>%
  tab_options(
    table.font.size = px(13),
    column_labels.font.size = px(12),
    data_row.padding = px(5),
    table.width = pct(100)
  )

# --- Table D: Stratified by Race/Ethnicity ---
gt_d <- table_d %>%
  select(-Section) %>%
  gt() %>%
  tab_spanner(
    label = md("**D. Stratified by Race/Ethnicity**"),
    columns = c(`NH White`, `NH Black`, Hispanic, `NH Asian`, `NH AIAN`, `Other/Multi`)
  ) %>%
  cols_label(
    `Age Group` = md("**Age Group**"),
    `NH White` = md("**NH White**"),
    `NH Black` = md("**NH Black**"),
    Hispanic = md("**Hispanic**"),
    `NH Asian` = md("**NH Asian**"),
    `NH AIAN` = md("**NH AIAN**"),
    `Other/Multi` = md("**Other/Multi**")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `Age Group`)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f4f8"),
    locations = cells_body(rows = seq(2, 7, by = 2))
  ) %>%
  tab_source_note(
    source_note = md("All models: Surv(age_at_survey, age_at_end, death) ~ SRH. HR < 1 indicates better SRH is associated with lower mortality. Panel A adjusts for each covariate separately. Panels B\u2013D fit separate models within each stratum. Education available ~1997+; Race/Ethnicity from RACENEW/HISPETH.")
  ) %>%
  tab_options(
    table.font.size = px(13),
    column_labels.font.size = px(12),
    data_row.padding = px(5),
    table.width = pct(100)
  )


# ==============================================================================
# APPROACH: SINGLE COMBINED GT TABLE
# ==============================================================================

message("=== Building single combined table ===\n")

# Build one big table with row groupings
# Combine all data into a single data frame with a grouping column

combined_df <- bind_rows(
  adj_wide %>%
    left_join(n_info, by = c("age_group" = "Age Group")) %>%
    mutate(
      N = format(N, big.mark = ","),
      Deaths = format(Deaths, big.mark = ","),
      section = "A. Covariate-Adjusted Models"
    ),
  sex_wide %>%
    mutate(N = "", Deaths = "", section = "B. Stratified by Sex"),
  educ_wide %>%
    mutate(N = "", Deaths = "", section = "C. Stratified by Education"),
  race_wide %>%
    mutate(N = "", Deaths = "", section = "D. Stratified by Race/Ethnicity")
) %>%
  # Fill missing columns with empty strings
  mutate(across(everything(), ~replace_na(as.character(.), "")))

# Reorder columns for display
col_order <- c("section", "age_group",
               "Unadjusted", "Adjusted: Sex", "Adjusted: Education",
               "Adjusted: Race/Ethnicity",
               "Male", "Female",
               "< HS", "HS/Some college", "College+",
               "NH White", "NH Black", "Hispanic",
               "NH Asian", "NH AIAN", "Other/Multi",
               "N", "Deaths")

# Keep only columns that exist
col_order <- intersect(col_order, names(combined_df))
combined_df <- combined_df %>% select(all_of(col_order))

gt_combined <- combined_df %>%
  group_by(section) %>%
  gt() %>%
  tab_header(
    title = md("**SRH-Mortality Hazard Ratios: Sociodemographic Sensitivity Analysis**"),
    subtitle = md(sprintf("NHIS, %d\u2013%d, survey-weighted Cox PH<br>HR (95%% CI) per 1-unit increase in SRH (higher = better health)", window_start, window_end))
  ) %>%
  # Column labels
  cols_label(
    age_group = md("**Age Group**"),
    Unadjusted = md("**Unadj.**"),
    `Adjusted: Sex` = md("**+ Sex**"),
    `Adjusted: Education` = md("**+ Educ.**"),
    `Adjusted: Race/Ethnicity` = md("**+ Race**"),
    Male = md("**Male**"),
    Female = md("**Female**"),
    `< HS` = md("**< HS**"),
    `HS/Some college` = md("**HS/Some Coll.**"),
    `College+` = md("**College+**"),
    `NH White` = md("**NH White**"),
    `NH Black` = md("**NH Black**"),
    Hispanic = md("**Hispanic**"),
    `NH Asian` = md("**NH Asian**"),
    `NH AIAN` = md("**NH AIAN**"),
    `Other/Multi` = md("**Other**"),
    N = md("**N**"),
    Deaths = md("**Deaths**")
  ) %>%
  # Spanners for column groups
  tab_spanner(
    label = md("**Adjusted Models**"),
    columns = c(Unadjusted, `Adjusted: Sex`, `Adjusted: Education`, `Adjusted: Race/Ethnicity`)
  ) %>%
  tab_spanner(
    label = md("**By Sex**"),
    columns = c(Male, Female)
  ) %>%
  tab_spanner(
    label = md("**By Education**"),
    columns = c(`< HS`, `HS/Some college`, `College+`)
  ) %>%
  tab_spanner(
    label = md("**By Race/Ethnicity**"),
    columns = c(`NH White`, `NH Black`, Hispanic, `NH Asian`, `NH AIAN`, `Other/Multi`)
  ) %>%
  tab_spanner(
    label = md("**Sample**"),
    columns = c(N, Deaths)
  ) %>%
  # Bold age group column
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = age_group)
  ) %>%
  # Alternating row shading within each group
  tab_style(
    style = cell_fill(color = "#f0f4f8"),
    locations = cells_body(rows = seq(2, nrow(combined_df), by = 2))
  ) %>%
  # Row group styling
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(16)),
      cell_fill(color = "#d6e4f0")
    ),
    locations = cells_row_groups()
  ) %>%
  # Replace empty strings with em-dash; bold the HR value before the CI
  sub_missing(missing_text = "") %>%
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      ifelse(
        x == "",
        "\u2014",
        # Bold the HR (number before the parenthesized CI)
        gsub(
          "^(\\d+\\.\\d+)( \\()",
          "<b>\\1</b>\\2",
          x
        )
      )
    }
  ) %>%
  # Footnote
  tab_footnote(
    footnote = md("All models: Surv(age\\_survey, age\\_end, death) ~ SRH. HR < 1 = better SRH associated with lower mortality. Adjusted models add one covariate each. Stratified models fit within each subgroup. Education ~1997+. N and Deaths shown for unadjusted.")
  ) %>%
  # Formatting — larger sizes
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(14),
    column_labels.font.size = px(13),
    data_row.padding = px(5),
    row_group.padding = px(6),
    table.width = pct(100),
    table.border.top.color = "gray40",
    table.border.bottom.color = "gray40",
    heading.border.bottom.color = "gray60",
    column_labels.border.bottom.color = "gray60",
    row_group.border.top.color = "gray60"
  )


# ==============================================================================
# SAVE
# ==============================================================================

message("\n=== Saving table one-pager ===\n")

# Save as HTML
gtsave(
  gt_combined,
  file.path(output_dir, "table_mort_sensitivity_onepager.html")
)
message("Saved: table_mort_sensitivity_onepager.html")

# Save as PNG
gtsave(
  gt_combined,
  file.path(output_dir, "table_mort_sensitivity_onepager.png"),
  vwidth = 1900
)
message("Saved: table_mort_sensitivity_onepager.png")

# Draft with date
gtsave(
  gt_combined,
  file.path(output_dir, paste0("table_mort_sensitivity_onepager_draft_", date_suffix, ".png")),
  vwidth = 1900
)
message(sprintf("Saved: table_mort_sensitivity_onepager_draft_%s.png", date_suffix))

# Also save the individual tables as HTML for reference
gtsave(gt_a, file.path(output_dir, "table_mort_hr_adjusted_gt.html"))
gtsave(gt_b, file.path(output_dir, "table_mort_hr_stratified_sex_gt.html"))
gtsave(gt_c, file.path(output_dir, "table_mort_hr_stratified_educ_gt.html"))
gtsave(gt_d, file.path(output_dir, "table_mort_hr_stratified_race_gt.html"))
message("Saved individual GT tables as HTML")

message("\n=== Table one-pager complete ===")
