# SRH Convergence Paper - Figure Generation
# Author: Christine Lucille Kuryla
# Goal: Generate publication-ready figures for self rated health (SRH) manuscript


## Key Principle: Visual Cohesion

All figures should look like they belong together:
- Same ggplot theme across all figures
- Consistent color palette for age groups
- Consistent color/shape palette for surveys
- Matching fonts, sizes, axis styling
- Uniform panel spacing and labels
Use:
source(here::here("R/functions/theme_srh.R"))

## Organization
Use the shared functions in `R/functions/` for all plotting.

## Data locations of cleaned datasets

For scripts that are using pre-wrangled data, the locations should be found through sourcing R/paths.R and then using the shortcuts.
Example:
library(here)
source(here::here("R/paths.R")) # Source project functions
ensure_dirs()

data_nhis <- readr::read_rds(derived_path("data_nhis.rds")) 

data_brfss <- readr::read_rds(derived_path("data_brfss.rds")) 

data_meps <- readr::read_rds(derived_path("data_meps.rds")) 

data_gss <- readr::read_rds(derived_path("data_gss.rds")) 

data_cps <- readr::read_rds(derived_path("data_cps.rds")) 

data_nhanes <- readr::read_rds(derived_path("data_nhanes.rds"))


Make sure to drop NAs of srh, age, year, wt ( |> drop_na(srh, age, year, wt) )


---

## Code Standards

### Style
- Tidyverse throughout
- Explicit, readable code over clever code
- Comments explain the WHY, not the WHAT

### Survey Weights
- All estimates must be survey-weighted
- Use `survey` or `srvyr` packages
- Never use base R subsetting for subpopulations

### Reproducibility
- set.seed() for any stochastic elements
- Relative paths via here::here()
- Session info captured

---

## Working With Existing Code

Christine has extensive existing analyses in a separate (messy) repository.

**When referencing old code:**
- Use it to verify results match
- Extract logic, but rewrite cleanly
- Don't copy-paste messy code; reimplement with shared functions

**Verification approach:**
- After generating new figures, compare key numbers to old output
- Flag any discrepancies for review

---

## Figure Generation Workflow

### For each figure:
1. **Confirm data requirements** - Which surveys, years, variables?
2. **Verify data is loaded** - Check processed data exists
3. **Generate using shared functions** - Use theme_srh(), consistent palettes
4. **Review output** - Check against old results if available
5. **Save in multiple formats** - PNG for review, PDF for publication
6. Interpret results if possible

### Saving conventions (unless otherwise specified):
```r
# Review versions (with date)
ggsave("output/figures/fig1_convergence_draft_20260116.png", width = 10, height = 8, dpi = 300)

# Final versions (no date, will be overwritten)
ggsave("output/figures/fig1_convergence.pdf", width = 10, height = 8)
ggsave("output/figures/fig1_convergence.png", width = 10, height = 8, dpi = 300)
```

---

## Surveys Reference

| Survey | Years | Key Variables Available | Notes |
|--------|-------|------------------------|-------|
| NHIS | varies | SRH, comorbidities, functioning | 2019 redesign |
| MEPS | varies | SRH, PHQ, utilization, expenditure | 2017 instrument change |
| BRFSS | varies | SRH, mental health days, chronic conditions | 2011 cell phones |
| GSS | varies | SRH (4-point), social variables | Smaller N |
| NHANES | varies | SRH, biomarkers, clinical measures | 2-year cycles |
| CPS | varies | SRH in supplements | Limited health data |

---

## Interaction Protocol

### Do:
- Implement what's requested efficiently
- Use shared functions for consistency
- Flag if something seems off vs. expectations
- Suggest minor improvements if clearly better

### Don't:
- Suggest major new analyses (timeline is tight)
- Restructure the approach without asking
- Add complexity beyond what's needed for the figure

### When in doubt:
- Ask brief clarifying questions
- Offer the simplest path forward
- Note alternatives briefly 

---

## File Structure (evolving)

```
srh-paper/
├── R/
│   ├── functions/
│   │   ├── theme_srh.R     # Shared ggplot theme and palettes
│   │   ├── plot_utils.R    # Shared plotting functions
│   │   └── survey_utils.R  # Survey design helpers
│   └── scripts/
│       ├── 00_setup.R      # Load packages, set paths
│       ├── 01_figure1.R    # Convergence panels
│       ├── 02_figure2.R    # Lexis diagrams
│       ├── 03_figure3.R    # Coefficient trends
│       └── 04_figure4.R    # Prevalence trends
├── notes/
├── output/
│   └── figures/
├── reports/			     # Final quarto summaries
└── docs/
```

---

## Color Palettes

### Age groups (use consistently across all figures)
See R/srh_common_functions.R
source(here::here("R/functions/theme_srh.R"))

---

## Immediate Priorities

1. ✅ Set up shared theme and plotting functions
2. ✅ Figure 1: Convergence (both panels, all 6 surveys)
3. ✅ Figure 3: Coefficient stability (available covariates)
4. ✅ Figure 4: Prevalence trends (available covariates)
5. ⬜ Various sensitivity analyses
6. ⬜ Combine into publication-ready multi-panel figures


---

# Scientific Instructions

SRH multi-dataset scientific workflow (R)

This repo contains scientific analysis code for SRH using NHIS, MEPS, BRFSS, GSS, CPS, and NHANES.

## Follow invariants
- Preserve all invariants in `analysis/INVARIANTS.md`.
- If a requested change could alter estimands, SRH coding direction, Fair/Poor threshold, survey weighting/design, sample restrictions, or APC definitions, DO NOT GUESS. Ask clarifying questions in your response.

## SRH coding rules (non-negotiable)
- `srh` is numeric with higher = better.
- NHIS/MEPS/BRFSS/CPS/NHANES: 1=Poor … 5=Excellent
- GSS: 1=Poor … 4=Excellent (no “Very good”)
- Default outcome for main analyses: continuous `srh` within dataset.
- Sensitivity: `srh_fairpoor = (srh_num <= 2)`; do not change threshold unless explicitly asked. Do not run sensitivity analyses until explicitly asked.

## Cross-dataset comparability
- Do not compare raw mean `srh` across datasets with different scales unless explicitly instructed.
- Prefer `srh_01` (0–1 rescale) or `srh_fairpoor` for cross-dataset summaries, or an ordinal model that accounts for scale differences.

## Survey design
- Use weights/strata/PSU when available and valid. Do not drop design elements “to make it run.” EXCEPTION: For all analyses except Fig. 1, do not use nested/strata for BRFSS due to computational constraints; for BRFSS, just use wt. 
- If only weights are available, use weights-only design explicitly and label outputs.
- Use functions from libraries survey/srvyr/etc to run survey-design compatible analyses; do not run unweighted analyses if possible. If the requested analysis is only feasible without weights (for example, it requires a specialized package because the process is complicated), tell me that is the case and confirm that is ok before proceeding. Otherwise, restrict to using functions from the survey/srvyr packages. 

## Coding expectations
- Prefer minimal diffs over rewrites.
- Prefer tidyverse unless base R is clearer.
- Be explicit about NA handling. Do not introduce broad `drop_na()`/`complete.cases()` changes.
- When refactoring, preserve output column names, units, and grouping level unless explicitly told otherwise.
- Add small guardrail checks for required columns, SRH ranges, and dataset identifiers rather than silently coercing.



# Scientific R project instructions

- Prefer tidyverse (dplyr/tidyr/purrr/readr/ggplot2) unless base R is clearer.
- If uncertain, ask in the response instead of guessing.
- Never change statistical intent without being explicitly asked.
  If a requested code change could alter the estimator, test, or model meaning, ask for clarification in the response.
- Write code to be reproducible:
  - use set.seed() when randomness is involved
  - avoid absolute paths; prefer here::here() or data_depot or derived_path from R/paths.R 
  - keep functions pure where possible (no hidden global state)
- Be explicit about NA handling (na.rm=, drop_na vs replace_na, etc).
- When refactoring, preserve outputs: same columns, same units, same grouping.
  If you must change them, explain the change and why.
- When doing analyses (not final paper Figure generation) Add minimal checks (stopifnot/assertthat) around assumptions that could silently break.
- Encourage small functions, explicit inputs/outputs, informative errors, and reproducible scripts.

- This is a scientific R project.
- Must not change statistical intent or estimators unless explicitly asked. If there is something better, suggest it.
- Prefer minimal diffs.
- Preserve column names, units, grouping, and NA handling.
- If a change could alter results, ask instead of guessing.
- Refactors must preserve outputs unless explicitly told otherwise.

# Instructions — Scientific R workflow

- This is a scientific analysis repo.
- Never change statistical intent, estimators, contrasts, or inclusion/exclusion rules unless explicitly requested.
- Preserve output shape: column names, types, units, row counts (unless asked).
- Be explicit about NA handling and grouping behavior.
- Prefer minimal diffs over rewrites.
- Prefer tidyverse (dplyr/tidyr/purrr/readr/ggplot2) unless base R is clearer.
- If uncertain, ask questions in the response instead of guessing.
- During exploratory and sensitivity analyses, add small checks (rlang::abort/stopifnot) around fragile assumptions.
