# Copilot instructions — SRH multi-dataset scientific workflow (R)

This repo contains scientific analysis code for SRH using NHIS, MEPS, BRFSS, GSS, CPS, and NHANES.

## Follow invariants
- Preserve all invariants in `analysis/INVARIANTS.md`.
- If a requested change could alter estimands, SRH coding direction, Fair/Poor threshold, survey weighting/design, sample restrictions, or APC definitions, DO NOT GUESS. Ask clarifying questions in your response.

## SRH coding rules (non-negotiable)
- `srh_num` is numeric with higher = better.
- NHIS/MEPS/BRFSS/CPS/NHANES: 1=Poor … 5=Excellent
- GSS: 1=Poor … 4=Excellent (no “Very good”)
- Default outcome for main analyses: continuous `srh_num` within dataset.
- Sensitivity: `srh_fairpoor = (srh_num <= 2)`; do not change threshold.

## Cross-dataset comparability
- Do not compare raw mean `srh_num` across datasets with different scales unless explicitly instructed.
- Prefer `srh_01` (0–1 rescale) or `srh_fairpoor` for cross-dataset summaries, or an ordinal model that accounts for scale differences.

## Survey design
- Use weights/strata/PSU when available and valid. Do not drop design elements “to make it run.”
- If only weights are available, use weights-only design explicitly and label outputs.

## Coding expectations
- Prefer minimal diffs over rewrites.
- Prefer tidyverse unless base R is clearer.
- Be explicit about NA handling. Do not introduce broad `drop_na()`/`complete.cases()` changes.
- When refactoring, preserve output column names, units, and grouping level unless explicitly told otherwise.
- Add small guardrail checks for required columns, SRH ranges, and dataset identifiers rather than silently coercing.



# Scientific R project instructions for Copilot

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
- Add minimal checks (stopifnot/assertthat) around assumptions that could silently break.
- Encourage small functions, explicit inputs/outputs, informative errors, and reproducible scripts.

- This is a scientific R project.
- Copilot must not change statistical intent or estimators.
- Prefer minimal diffs.
- Preserve column names, units, grouping, and NA handling.
- If a change could alter results, ask instead of guessing.
- Refactors must preserve outputs unless explicitly told otherwise.

# Copilot instructions — Scientific R workflow

- This is a scientific analysis repo.
- Never change statistical intent, estimators, contrasts, or inclusion/exclusion rules unless explicitly requested.
- Preserve output shape: column names, types, units, row counts (unless asked).
- Be explicit about NA handling and grouping behavior.
- Prefer minimal diffs over rewrites.
- Prefer tidyverse (dplyr/tidyr/purrr/readr/ggplot2) unless base R is clearer.
- If uncertain, ask questions in the response instead of guessing.
- Add small checks (rlang::abort/stopifnot) around fragile assumptions.
