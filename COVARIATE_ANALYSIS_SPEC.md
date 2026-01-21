# Covariate Sensitivity Analysis Template

## Purpose

Generate standardized 4-page PDF reports analyzing how a covariate (e.g., education) 
relates to self-rated health trends and age gradients across survey years.
**All 6 datasets are displayed together on each page** via faceting for easy comparison.

## Key References

Before implementing, read these files:
- CLAUDE.md: Project instructions, variable definitions, age groups, SRH categories
- R/theme_srh.R: Color palettes, theme settings
- R/fig_1_combined.R: Template for multi-dataset faceted display
- R/paths.R: Must be loaded first

## Data Structure

### Loading Data


# Load paths first
source("R/paths.R")

For scripts that are using pre-wrangled data, the locations should be found through sourcing R/paths.R and then using the shortcuts. Example: library(here) source(here::here("R/paths.R")) # Source project functions ensure_dirs()

data_brfss <- readr::read_rds(derived_path("data_brfss.rds"))

data_meps <- readr::read_rds(derived_path("data_meps.rds"))

data_nhis <- readr::read_rds(derived_path("data_nhis.rds"))

data_cps <- readr::read_rds(derived_path("data_cps.rds"))

data_nhanes <- readr::read_rds(derived_path("data_nhanes.rds"))

data_gss <- readr::read_rds(derived_path("data_gss.rds"))

Make sure to drop NAs of srh, age, year, wt ( |> drop_na(srh, age, year, wt) )

# All datasets have identical variable names and format
datasets <- list(
  BRFSS = readr::read_rds(derived_path("data_meps.rds")),
  MEPS = readr::read_rds(derived_path("data_meps.rds")),
  NHIS = readr::read_rds(derived_path("data_meps.rds")),
  CPS = readr::read_rds(derived_path("data_meps.rds")),
  NHANES = readr::read_rds(derived_path("data_brfss.rds")),
  GSS = readr::read_rds(derived_path("data_nhis.rds"))
)

All datasets have identical variable names and format.

### Survey Design

For all datasets, use simple weighted design:
  svydesign(ids = ~1, weights = ~wt, data = data)

CRITICAL: For BRFSS, do NOT use nested/stratified design (ids = ~psu, strata = ~strata).
The computational burden is too high. Use simple weighted design only.

### Age Groups

Use 7 age groups (see theme_srh.R for colors). Ages 90+ are filtered out.
  age_breaks = c(18, 30, 40, 50, 60, 70, 80, 90)
  age_labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")

## Main Function Signature

run_covariate_sensitivity(
  datasets,                          # named list of data frames (all 6 surveys)
  covariate,                         # string: name of covariate column
  covariate_label = NULL,            # string: display name for figures
  weight_var = "wt",                 # string: name of weight column
  year_var = "year",
  age_var = "age",
  srh_var = "srh",
  representative_ages = c(25, 45, 65, 85),
  age_breaks = c(18, 30, 40, 50, 60, 70, 80, 90),
  age_labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89"),
  year_bins = NULL,                  # NULL for individual years, or vector of breakpoints
  output_dir = "output/"
)

## Output Structure

The key difference from single-dataset analysis: **all figures facet by dataset**
so that all 6 surveys appear on each page for direct comparison.

### Data Objects (returned as list)

For each of the following, results from all 6 datasets are combined into a single
tibble with a `dataset` column:

1. stratified_betas: β_age by dataset × year × covariate level
2. beta_trends: linear trend of β_age over time, per dataset × covariate level
3. predicted_srh: predicted SRH at representative ages by dataset × year × covariate level
4. beta_covariate: β_covariate by dataset × year
5. beta_interaction: β_age×covariate by dataset × year
6. variance_decomposition: R² values by dataset × year
7. srh_sd: SD of SRH by dataset × year × covariate level
8. srh_distribution: % in each SRH category by dataset × year × covariate level
9. sample_composition: % in each covariate level by dataset × year
10. sample_n: n by dataset × year × covariate level
11. missingness: % missing by dataset × year

### Figure Layout

Reference fig_1_combined.R for display approach. Each page shows all 6 datasets.

Page 1: Descriptive Trends
- Mean SRH by year × age group
- Facet grid: rows = datasets (6), columns = covariate levels (k)
- facet_grid(dataset ~ covariate_level)
- For k=3: 6×3 = 18 panels, each ~2.3" × 1.4"
- Use small point size (0.5-1), thin lines (0.3-0.5)
- Suppress CI ribbons to reduce clutter

Page 2: Age Gradient Analysis
- Top: β_age over time, one line per covariate level
  - Facet: rows = datasets (6), single column
  - Or: facet_wrap(~dataset, nrow=3, ncol=2) with covariate as color
- Bottom: Predicted SRH at representative ages
  - Facet grid: rows = datasets, columns = covariate levels
- May need to split into Page 2a (beta) and Page 2b (predicted)

Page 3: Model Diagnostics
- β_covariate over time: facet_wrap(~dataset, nrow=3, ncol=2)
- Interaction coefficients: facet_wrap(~dataset, nrow=3, ncol=2)
- Variance decomposition table: 
  - Option A: rows = covariate levels, columns = datasets
  - Option B: 2×3 grid of mini-tables
- May split figures and tables across pages

Page 4: Composition & Distribution
- SD of SRH over time: facet_grid(dataset ~ covariate_level) or facet_wrap
- SRH category distribution: facet_grid(dataset ~ covariate_level)
- Sample composition: facet_wrap(~dataset, nrow=3, ncol=2)
- Sample/missingness table: 2×3 mini-tables or compact summary
- Split across multiple pages as needed

### PDF Specifications

- Filename: {covariate}_sensitivity_all_surveys.pdf
- Page size: 8.5 x 11 inches (portrait letter) - better for screen reading
- Plot layout: rows = datasets (6), columns = covariate levels (k)
  - Reading across a row: compare covariate levels within same dataset
  - Reading down a column: compare same covariate level across datasets
- Table layout options:
  - Option A: rows = covariate levels, columns = datasets (compact summary)
  - Option B: separate mini-tables per dataset in 2×3 grid (detailed data)
- For 6 datasets × 3 covariate levels: each panel ~2.3" wide × 1.4" tall

## Modeling Details

- All models use survey weights via svyglm
- BRFSS: Use svydesign(ids = ~1, weights = ~wt) only—no strata/PSU
- Base model: srh ~ age
- Adjusted model: srh ~ age + covariate
- Interaction model: srh ~ age * covariate

## Figure Specifications

- Use theme_srh() from R/theme_srh.R
- Use age group colors from theme_srh.R
- Use SRH category colors from theme_srh.R
- Reference fig_1_combined.R for multi-dataset facet layout
- Dataset facet labels: BRFSS, MEPS, NHIS, CPS, NHANES, GSS
- Consistent axis scales across dataset facets where possible

### Compact Layout Guidelines (for portrait letter)

- Plots: rows = datasets (6), columns = covariate levels (k)
  - Use facet_grid(dataset ~ covariate_level)
  - Reading across: compare covariate levels within dataset
  - Reading down: compare datasets within covariate level
- Tables: rows = covariate levels, columns = datasets (or 2×3 mini-tables)
- Base font size: 7-8pt for axis labels, 8-9pt for facet strips
- Point size: 0.5-1, line width: 0.3-0.5
- Minimize whitespace: panel.spacing = unit(2, "pt")
- Legend position: bottom, horizontal, compact (may need to abbreviate)
- Strip text: may abbreviate dataset names if needed (e.g., "GSS" not "General Social Survey")
- For 18 panels (6×3): each panel approximately 2.3" wide × 1.4" tall