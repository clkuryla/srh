# Invariants — SRH Across NHIS/MEPS/BRFSS/GSS/CPS/NHANES

**Last updated:** 2026-01-08
**Purpose:** These are non‑negotiables. If a refactor/debug/AI edit violates an invariant, the analysis is wrong even if it runs.

---

## 0) Project intent 
- Primary question(s): The age-related gradient of self-rated health seems to be decreasing over the past several decades. This project aims to explore that phenomenon across six data sets, as well as look into details about what might be causing the phenomenon to occur. It will also explore other patterns associated with health and age over time, potentially using these found patterns as context for interpreting the phenomenon. It will do so in various ways. 
- Primary estimand(s): (default) **mean SRH score**; (sensitivity) **Fair/Poor binary**
- Output(s): Various outputs including figures, model summaries, datatables, and other useful output to be determined. 

---

## 1) Dataset identity invariants
This project uses **six survey datasets**: **NHIS, MEPS, BRFSS, GSS, CPS, NHANES**.

### Harmonized master fields (required in every derived dataset)
- `dataset` ∈ {NHIS, MEPS, BRFSS, GSS, CPS, NHANES}
- `year` = year of survey wave
- `age` = exact age at interview or best available
- `id` = respondent identifier **within dataset** (uniqueness documented per dataset). Note -- may not be available for all datasets. 
- Survey design fields (when available; see §4):
  - `wt` (weight)
  - `strata` (stratum)
  - `psu` (primary sampling unit)
  - `design_type` ∈ {`w_strata_psu`, `weights_only`, `unweighted`} (explicit)

**Forbidden changes**
- Do not silently alter `year` meaning, de-duplication rules, or person-level unit definition.

---

## 2) SRH semantics invariants (THIS IS SACRED)
### Harmonized SRH numeric score
We maintain a harmonized numeric variable:

- `srh_num` is **numeric** and always coded **higher = better**.

Coding rules:
- **NHIS/MEPS/BRFSS/CPS/NHANES:** `srh_num` ∈ {1,2,3,4,5} with  
  **1=Poor, 2=Fair, 3=Good, 4=Very good, 5=Excellent**
- **GSS:** `srh_num` ∈ {1,2,3,4} with  
  **1=Poor, 2=Fair, 3=Good, 4=Excellent** (no “Very good”)

Also store:
- `srh_scale_max` = 5 for NHIS/MEPS/BRFSS/CPS/NHANES; 4 for GSS
- Optional (recommended for cross-dataset comparability):  
  `srh_01 = (srh_num - 1) / (srh_scale_max - 1)`  (maps to [0,1])

### Fair/Poor sensitivity outcome
- `srh_fairpoor` (binary): 1 if SRH is **Fair or Poor**, 0 otherwise.  
  Operational rule: `srh_fairpoor = (srh_num <= 2)` for *all* datasets (given coding above).

### Missingness rules (explicit)
- Any “don’t know/refused/not ascertained” SRH codes must be recoded to `NA` (dataset-specific).
- Default analyses treat SRH missingness as: drop outcome-missing SRH or age only.

**Forbidden changes**
- Do not reverse SRH direction (ever) without explicitly changing this document + all interpretation code.
- Do not invent a “Very good” category for GSS. Do not impute it silently.
- Do not compare raw mean `srh_num` across datasets without an explicit comparability variable (`srh_01` or `srh_fairpoor`) OR a model that accounts for scale differences.

---

## 3) Default estimands & modeling invariants
### Default (main) analyses
- Default SRH outcome used: **continuous `srh`** within each dataset.
- If combining evidence across datasets, default comparability outcome is:
  - **`srh_01`** (preferred) OR **`srh_fairpoor`** (most interpretable/robust).

### Sensitivity analyses (expected in supplement)
- Binary: `srh_fairpoor`
- Ordinal model (optional): ordered logit/probit on SRH categories (dataset-specific scale)

**Forbidden changes**
- Do not silently switch from continuous to binary (or ordinal) in “main” outputs.

---

## 4) Survey design invariants (weights/strata/PSU when possible)
- Use `wt`, `strata`, `psu` **whenever they exist and are valid** for the dataset/year.
- If strata/PSU are unavailable but weights exist: use `design_type = weights_only`.
- If weights are not available/usable: `design_type = unweighted` and label outputs clearly.

Pooling / multi-year weights:
- Any weight scaling for pooled years/cycles (common in some surveys) must be:
  1) explicit in code, and
  2) documented in `analysis/WEIGHT_NOTES.md` (create if needed).

**Forbidden changes**
- Do not “drop survey design to make it run.”
- Do not mix weighted and unweighted results in the same figure/table without labeling.

---

## 5) APC / time-structure invariants (only if used)
If age–period–cohort structure is used:
- `period` = `year` (unless explicitly defined otherwise)
- `cohort` definition: `cohort = year - age` (document rounding/binning if any)
- Bin edges for age/period/cohort (if used) are inputs into the functions and may be changed for sensitivity analyses. 

**Forbidden changes**
- Do not change bin edges or cohort construction without treating it as a scientific change.

---

## 6) Output contracts (shape + meaning)
### Person-level harmonized file(s)
Per-dataset derived person-level file: `derived_path("data_<dataset>.rds")` 

Required columns:
- `dataset`, `year`, `age`, `id`
- `srh`, `srh_scale_max`, (optional) `srh_01`, `srh_fairpoor`
- `wt`, `strata`, `psu`, `design_type`
- Key covariates used in main models: none for now


**Forbidden changes**
- Do not rename these core variables or change grouping level without updating downstream code + documenting it here.

---

## 7) Verification checklist (run after any meaningful edit)
- [ ] SRH coding sanity:
  - Non‑GSS: `srh_num` in 1..5; GSS: in 1..4
  - `srh_fairpoor` equals `srh_num <= 2` wherever SRH non-missing
- [ ] SRH direction sanity: higher=better (spot-check distributions)
- [ ] Survey design sanity:
  - `w` nonnegative, not accidentally all 1s unless intended
  - `design_type` matches available fields
- [ ] Counts by dataset×year stable (or explained)
- [ ] Summary outputs have expected row counts and grouping
- [ ] One “golden” plot/table matches prior version within tolerance

---
