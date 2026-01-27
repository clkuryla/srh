# Covariate Sensitivity Analysis Specification

## Overview

This document specifies supplementary analyses examining how sociodemographic covariates (sex, race, education) relate to the self-rated health (SRH) convergence phenomenon documented in Figure 1. The goal is to create a **template** that accepts any categorical covariate and produces standardized output pages.

**Reference**: See `CLAUDE.md` for project context, `R/theme_srh.R` for colors and styling, and Figure 1 combined for the main phenomenon.

---

## Constants

### Datasets (6 total, display in this order)
```
BRFSS, MEPS, NHIS, CPS, NHANES, GSS
```

### Age Groups (7 total)
```
18-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80-89
```
Use standard age colors from `theme_srh.R` (Okabe-Ito palette).

### Covariates to Analyze

| Covariate | Categories (N) | Levels |
|-----------|----------------|--------|
| Sex | 2 | Male, Female |
| Education | 3 | Less than high school; High school or some college; Bachelor's degree or higher |
| Race | varies | (define per dataset harmonization) |

**Note**: For categorical regression coefficients, there will be N-1 coefficients (relative to reference category).

---

## Page 1: CONVERGENCE

**Purpose**: Stratify Figure 1a and 1b by covariate to assess whether convergence occurs within each subgroup.

### Row 1: Stratified Age Coefficients (analogous to Figure 1b)

**Layout**: 1 row × 6 columns (one panel per dataset)

**What to plot**: 
- Model: `SRH ~ age` (continuous age), run separately within each covariate stratum
- Y-axis: Coefficient of age (β_age)
- X-axis: Year
- Visual encoding:
  - One color per covariate stratum (e.g., for sex: one color for Male, another for Female)
  - Points = annual coefficient estimates
  - Lines = meta-regression trend through coefficients over time
  - **Solid line** if trend is statistically significant (p < 0.05)
  - **Dashed line** if trend is not significant

**Example for sex**: Each panel shows two sets of points (Male in blue, Female in red) and two trend lines, showing whether the age-SRH gradient is attenuating similarly for both sexes.

### Rows 2+: Stratified Figure 1a (SRH by age group over time)

**Layout**: N rows × 6 columns, where N = number of covariate categories

**What to plot**:
- Each row = one covariate stratum (e.g., Row 2 = Male, Row 3 = Female)
- Each column = one dataset
- Within each panel: Survey-weighted mean SRH over time, with one line per age group (7 lines in standard age colors)

**Example for sex**: 
- Row 2: Males only — 6 panels showing 7 age-colored lines each
- Row 3: Females only — 6 panels showing 7 age-colored lines each

---

## Page 2: COEFFICIENTS

**Purpose**: Examine how the covariate itself relates to SRH, and whether this relationship varies by age.

### SC1: Covariate Effect on SRH Over Time

**Model**: `SRH ~ covariate` (categorical)

**Layout**: (N-1) rows × 6 columns
- Rows = non-reference covariate categories
- Columns = datasets

**What to plot**:
- Y-axis: Coefficient (β) for that category vs. reference
- X-axis: Year
- One panel per category-dataset combination
- Include confidence intervals

**Example for education** (reference = Bachelor's+):
- Row 1: β for "Less than high school" over time
- Row 2: β for "High school/some college" over time

### SC2: Covariate Effect by Age Group

**Model**: `SRH ~ covariate`, run separately for each year × age group × dataset combination

**Layout**: 6 rows × 7 columns
- Rows = datasets
- Columns = age groups (18-29 through 80-89)

**What to plot**:
- Y-axis: Coefficient (β) for non-reference categories
- X-axis: Year  
- N-1 lines per panel, one for each non-reference covariate level
- Color by covariate category (use covariate colors, not age colors)

**Example for education** (reference = Bachelor's+):
- Each panel shows 2 lines: β for "Less than HS" and β for "HS/some college"
- Reveals whether education gaps in SRH differ by age and how this has changed over time

**Purpose**: Reveals whether the covariate-SRH relationship differs by age and how this has changed.

---

## Page 3: VARIANCE EXPLAINED

**Purpose**: Quantify how much variance age vs. covariate explains in SRH.

### SV1: Baseline — R² for Age Only

**Model**: `SRH ~ age`

**Layout**: Option A: 1×6 (one panel per dataset), Option B: All datasets on one figure (generate both)

**What to plot**: R² over time

**Note**: This is identical across all covariate analyses (sex, education, race) but is included on each page for context so the reader can compare covariate R² to age R² without flipping pages.

### SV2: R² for Covariate Only

**Model**: `SRH ~ covariate`

**Layout**: Same as SV1 (both 1×6 and combined versions)

**What to plot**: R² over time

### SV7: Model Comparison — Four Models

**Layout**: 1×6 (one panel per dataset)

**What to plot**: R² over time for 4 models on each panel:
1. `SRH ~ age`
2. `SRH ~ covariate`
3. `SRH ~ age + covariate`
4. `SRH ~ age + covariate + age×covariate`

**Visual encoding**: Different line types or colors for each model; include legend.

### SV8A: Incremental R² — Adding Covariate to Age

**Model comparison**: R²(`SRH ~ age + covariate`) − R²(`SRH ~ age`)

**Layout**: 1×6

**What to plot**: ΔR² over time

**Purpose**: Shows how much additional variance the covariate explains beyond age.

### SV8B: Incremental R² — Adding Interaction

**Model comparison**: R²(`SRH ~ age + covariate + age×covariate`) − R²(`SRH ~ age + covariate`)

**Layout**: 1×6

**What to plot**: ΔR² over time

**Purpose**: Shows whether the age-covariate interaction contributes meaningfully.

---

## Page 4: COVARIATE COMPOSITION

**Purpose**: Document how demographic composition of each dataset varies by age group over time.

### Layout: 6 rows × 7 columns
- Rows = datasets
- Columns = age groups

### What to plot:
- X-axis: Year
- Y-axis: Proportion (0 to 1)
- Stacked area chart showing proportion in each covariate category over time
- One color per category (e.g., for sex: Male/Female proportions)

**Purpose**: Context for interpreting results — if composition shifts over time or differs by age, this matters for understanding whether observed SRH changes reflect true health changes vs. compositional shifts.

---

## Page 5: SRH COMPOSITION BY COVARIATE

**Purpose**: Show SRH distribution within each covariate stratum.

### Layout: 6 columns × N rows
- Columns = datasets
- Rows = covariate categories

### What to plot:
- Stacked area graph over time
- Y-axis: Proportion
- Colors: SRH categories (Poor, Fair, Good, Very Good, Excellent) in standard SRH colors
- One panel per dataset-stratum combination

**Example for sex**:
- Row 1: Males — 6 panels showing SRH distribution evolution
- Row 2: Females — 6 panels showing SRH distribution evolution

---

## Page 6: FOREST PLOT

**Purpose**: Visualize how covariate effects on SRH change over time, with formal significance indicators.

### Model (run separately for each year × dataset)
```
SRH ~ age + covariate
```

Extract the coefficient(s) for `covariate` (N-1 coefficients for N categories, relative to reference level).

### Layout
- 6 adjacent forest plots (one per dataset), all aligned by year
- Rows = years in **reverse chronological order** (most recent at top)
- X-axis: Coefficient estimate with 95% CI
- For covariates with N-1 > 1 coefficients (e.g., education, race): use different colors or shapes for each level, or create separate forest plot panels per level

### Visual encoding
- **Solid point**: Significant (p < 0.05)
- **Open/hollow point**: Not significant
- Horizontal lines show 95% CI
- Vertical dashed line at x = 0 (null effect)

### Interpretation
- Each point shows the absolute covariate effect in that year (e.g., "in 2015, females had 0.12 higher SRH than males, controlling for age")
- Visual inspection of whether coefficients trend toward or away from zero over time
- Can add a trend line through points if desired to summarize the temporal pattern

### Optional enhancement
- Add a meta-regression trend line through the annual coefficients to formally test whether the covariate effect is changing over time
- Report slope and p-value of this trend in figure caption or accompanying table

---

## Implementation Notes

### Function Signature (suggested)
```r
run_covariate_sensitivity(

  covariate_name,      # e.g., "sex", "education", "race"
  covariate_var,       # column name in data
  covariate_levels,    # vector of category names in desired order
  reference_level,     # which level is reference (for coefficients)
  covariate_colors,    # named vector of colors for categories
  output_prefix,       # for file naming

  min_n = 30           # minimum cell size threshold
)
```

### Graceful Error Handling

**Small sample sizes**: 
- Before computing any estimate, check N in the relevant cell (year × age group × covariate stratum)
- If N < `min_n`, skip that cell and return NA
- Plotting functions should handle NAs gracefully (gaps in lines, no points)
- Consider logging which cells were skipped for transparency

**Missing covariate levels**:
- Some datasets may have NAs for certain covariate values or may not have all levels
- Filter to complete cases for each analysis
- If an entire covariate level is missing for a dataset, skip that level for that dataset
- Do not error out; continue with available data

**Model fitting failures**:
- Wrap model fitting in `tryCatch()`
- If a model fails to converge or errors, return NA for that estimate
- Log the failure for debugging

```r
# Example pattern
safe_fit <- function(formula, design) {

  tryCatch({
    svyglm(formula, design = design)
  }, error = function(e) {
    warning(paste("Model failed:", e$message))
    return(NULL)
  })
}
```

### Survey Weighting
- All analyses should use survey-weighted estimates
- For BRFSS: Use simplified design (`ids = ~1, weights = ~wt`) to avoid computational burden

### Color Conventions
- Age groups: Use `age_colors` from `theme_srh.R`
- SRH categories: Use `srh_colors` from `theme_srh.R`
- Covariate categories: Define per covariate (passed as argument)

### Race Variable Note
- Race operationalization may have 2, 5, or 6 categories depending on analysis choice
- Will be harmonized across datasets, but some cells may have small N or be entirely missing
- Code should handle variable number of categories gracefully (N-1 coefficients, N colors, etc.)

### Output
- Generate PDF with all pages for each covariate
- Also save individual figures as PNG for flexibility
- Suggested naming: `sensitivity_{covariate}_{page}.pdf`

### Output
- Generate PDF with all pages for each covariate
- Also save individual figures as PNG for flexibility
- Suggested naming: `sensitivity_{covariate}_{page}.pdf`

---

## Prompt Structure for Claude Code

Suggested breakdown into separate prompts:

1. **Prompt 1 — Data Functions**: Functions to compute stratified coefficients, R² values, compositions
2. **Prompt 2 — Convergence Page**: Figure 1a/1b stratified
3. **Prompt 3 — Coefficients Page**: SC1 and SC2
4. **Prompt 4 — Variance Page**: SV1, SV2, SV7, SV8A, SV8B
5. **Prompt 5 — Composition Pages**: Covariate composition and SRH composition
6. **Prompt 6 — Forest Plot**: Interaction coefficient visualization
7. **Prompt 7 — Assembly**: Master function that runs all pages and combines into PDF

---

## Quality Checks

Before finalizing, verify:
- [ ] All surveys show reasonable N per stratum-year-age combination
- [ ] Coefficient directions make substantive sense
- [ ] R² values are plausible (not negative, not >1)
- [ ] Forest plot CIs are reasonable widths
- [ ] Colors are consistent across all figures
- [ ] Figure labels clearly indicate covariate and stratum
