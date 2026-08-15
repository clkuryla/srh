# APC Diagnostics — Instructions for Claude Code (Hump-Location Extraction + Null Simulation)

**What this file is:** the authoritative specification for two diagnostic analyses supporting the SRH convergence paper's APC section. It lives in this repo so it can be referenced across sessions. Written 2026-08 by Christine Kuryla with Claude; the scientific rationale is developed in `article/apc_interpretation.md` in the paper folder (not required — §0 below is self-contained).

**Claude Code: how to use this file.**
- Read this file in full before doing anything. If your context is ever compacted or you are resuming in a new session, **re-read this file** — it is authoritative over any summarized memory of it.
- Work through §2 (orientation) first and report your plan before launching long-running fits.
- **This spec is designed to run across separate sessions, one task per session:** Session A = Task 1 (quick tier, from saved outputs); Session B = Task 2 (APC-I null simulations, and the merge of null peaks into Task 1's summary table/figure); later sessions = full-tier BHAPC nulls and the Task 2b positive control. Do only the task you were asked to do. Sessions communicate exclusively through saved artifacts (CSVs, `.rds`, figures, rendered Quartos) and the §7 progress log — start every session by reading this file and the log; end every session by writing the log.
- Append a dated entry to §7 (progress log) at the end of this file whenever you complete a milestone: seeds used, fits completed (with file paths), runtimes, and any deviations from spec. Never edit or delete prior log entries.

---

You are working in the repository containing the age-period-cohort (APC) analyses for the SRH convergence paper (Kuryla et al., six US surveys: BRFSS, MEPS, NHIS, CPS, NHANES, GSS). Your job is to produce two diagnostic analyses, each delivered as a rendered Quarto document with figures, tables, and clear scientific explanations:

1. **Task 1 — Hump location:** extract the exact peak location of the cohort "hump" for each survey from the fitted BHAPC and APC-I models, alongside each survey's observation window, and test whether the peaks track each survey's data-window center or a fixed birth-year.
2. **Task 2 — Null simulation:** show what the *existing* BHAPC and APC-I pipelines produce when fed data containing **no cohort process at all** — only age effects, period effects, and a steady age×period interaction. **You must adapt and reuse the repo's existing model code so results are directly comparable — do not write a new implementation of the models.**

## 0. Scientific context (read carefully — this is the point of everything below)

The paper's central finding: the cross-sectional age gradient in self-rated health (SRH), always negative, has steadily attenuated toward zero over ~5 decades in all six surveys (a linear-in-time flattening; year-specific age coefficients β_age(p) ≈ β₀ + γ(p − p̄), β₀ < 0, γ > 0). This is formally an age×period interaction.

The APC analyses in the paper (Bayesian hierarchical APC = BHAPC; APC-I per Luo & Hodges 2020) found: age dominates variance; cohort variance (0.4–2.1%) exceeds period variance (0.1–0.2%); the cohort effects form a smooth **inverted-U ("hump") peaking around 1940s–1950s births**; and APC-I intra-cohort slopes flip from positive (pre-~1950 cohorts) to negative (post-~1950). A co-author asked whether this means a baby-boomer cohort effect drives the convergence.

The key algebra: with cohort c = p − a, centered variables α = a − ā, π = p − p̄, κ = c − c̄ where c̄ = p̄ − ā, the polarization identity gives:

    α·π = ½(α² + π² − κ²)

So a pure, steady age×period interaction γ·α·π is *algebraically identical* to convex quadratics added to the age and period effects **plus a concave (inverted-U) quadratic cohort effect, −(γ/2)κ², peaking at c̄ = p̄ − ā** — the center of the survey's observed cohort distribution. In an additive APC model the α² and π² parts are absorbed invisibly into the flexible age/period main effects, so **the only visible fingerprint of the interaction is the cohort hump**. It also mechanically produces the intra-cohort slope sign-flip near c̄ (along a cohort diagonal, the deviation slope is γ[(a−ā) + (p−p̄)]).

This yields **falsifiable, survey-specific predictions under the "mechanical" (no-true-cohort-effect) account:**
- P1: The fitted cohort curve is a smooth inverted-U spanning the whole cohort range (not a localized bump over 1946–64).
- P2: Its peak sits near c̄ = p̄ − ā for *each survey* — and therefore **peaks should differ across surveys whose observation windows differ** (e.g., GSS starts 1972; MEPS starts 2000 — their c̄ differ by ~10 years).
- P3: Intra-cohort slopes flip sign near c̄.
- P4: Cohort variance exceeds period variance even with zero true cohort effects (cohort is the widest temporal axis: Var(c) = Var(a) + Var(p)).

Under a **genuine generational account**, by contrast, the peak should sit at the *same birth years in every survey* regardless of window (e.g., a fixed boomer peak ~1955).

Task 1 measures the observed peaks and windows. Task 2 generates the null reference: what peaks/slopes/variance shares the *actual pipelines* produce when the truth is a pure age×period interaction. Together they let us say, per survey, whether the observed cohort signatures are distinguishable from the mechanical fingerprint. An important caveat to carry into all interpretation: due to the classical APC identification problem, only the *curvature* of the cohort component is invariant; the *peak location* depends on the model's centering/shrinkage conventions — which is precisely why the null simulation (same code, same conventions) is the correct comparator for the observed peaks, better than the analytic c̄ itself.

## 1. Ground rules

- **Reuse, don't rewrite.** The scientific value of Task 2 depends on the simulated data flowing through the *identical* model code, priors, bins, iterations, and plotting as the real analyses. Refactor minimally if needed (e.g., wrap an existing script's model-fitting block into a function taking a data frame), keeping diffs small. Never change model specification, priors, binning, weights handling, or estimation settings.
- Tidyverse style; survey weights exactly as the existing code uses them; `set.seed()` for every stochastic step (record seeds in the Quarto); `here::here()` paths; no bare `tryCatch` swallowing errors — fail loudly.
- Add `stopifnot()` guards for fragile assumptions (expected columns, SRH ranges 1–5 / 1–4 for GSS, no missing weights).
- **Checkpoint everything.** Save every fitted model to `.rds` immediately; make the pipeline resumable (skip fits whose output file exists). BHAPC MCMC runs are long — run them in the background/sequentially, never inside the Quarto render (Quartos load saved fits).
- **Prototype, then commit.** First validate plumbing end-to-end with short chains (e.g., 2 chains × 500 iterations) on one survey (GSS or NHANES — smallest). Only after everything works, launch full-fidelity runs with the *original* settings.
- Before launching the full runs, print a short plan (which scripts you found, what you'll reuse, expected runtimes) — then proceed.
- **Tiered execution — do the QUICK TIER first and report before any MCMC.** BHAPC refits are very expensive; a provisional answer is wanted fast, with full-fidelity reruns later.
  - **Quick tier (no BHAPC fitting):** (a) Task 1 peak extraction from *saved* BHAPC output — a table of posterior means per cohort bin exists (and if the original `.rds` stanreg fits exist, prefer them: they contain the draws, enabling peak CrIs with no refitting). With means only, report peaks as argmax bin + quadratic-vertex fit, uncertainty ± one bin width, and note the CrIs are deferred. (b) Task 2 via **APC-I only** (cheap, frequentist): ≥20 null replicates per survey, hump + slope-flip signatures and null-calibrated peaks. Deliver the summary table and verdicts from this tier first.
  - **Full tier (later, on request):** full-fidelity BHAPC null runs per survey, per-draw peak CrIs for observed fits, and the Task 2b boomer-step positive control.

## 2. Orientation (do this first — via the code map, NOT repo exploration)

**Do not explore or search this repo broadly.** It contains many abandoned drafts of similar-looking APC scripts; silently using the wrong one invalidates comparability, which is the entire point of this work. The authoritative map of the relevant, actually-used code is:

    to_transfer/ronin_code_apc_etc.md        (i.e., srh/to_transfer/ronin_code_apc_etc.md)

1. Read the map in full. Use ONLY files it lists — for BHAPC fitting (rstanarm model: `SRH ~ ln(weight) + (1|age_group) + (1|period) + (1|cohort)`, 4 chains × 6000 iterations, 3000 warmup, adapt_delta 0.95), APC-I (`APCI` package v1.0.8, gaussian, weighted, 5-year bins, ages 20–89, NHANES 20–79, BRFSS 50% subsample), variance decomposition, the plotting code behind the paper's Figs S6/S7/S9, data prep, and saved fitted objects / posterior-mean tables. **Never source, copy from, or adapt any script not in the map, even if its filename looks right.**
2. If something you need is not covered by the map (a saved fit you can't find, a plotting function, a data-prep step), **stop and ask Christine which file to use** — do not hunt through the repo for candidates.
3. Verify you have the right pipeline by reproducing known numbers from the paper: age variance share 7.7–18.8% across surveys; cohort 0.4–2.1%; period 0.1–0.2% (NHANES the outlier with period > cohort). If your reproduced numbers disagree, stop and report — you may be pointed at a draft.
4. Identify the analysis datasets each fit used (post-restriction: adults, ages as binned in the code, non-missing SRH/age/year/weight) — Task 1's window statistics must be computed on **exactly those rows**.

**The manuscript (context for prose, NOT for code).** The draft article is at `article/SRH Draft 20260812.pdf` (a `.docx` sits alongside; prefer the PDF). **Do not read it before or during the code work** — it describes the same models in summary form and its shorthand can mislead implementation choices; the map and the actual code are authoritative for anything computational. **After** the analyses have run and the figures exist, read the article's APC material (main-text "Age-Period-Cohort (APC) Effect" section; supplement S4–S5; the APC parts of the extended methods) and use it to write the Quartos' motivation and interpretation prose in terms consistent with the paper's framing and terminology.

## 3. Task 1 — Extract the hump peak and the observation-window center

**Deliverable:** `apc_hump_location.qmd` → rendered HTML + `hump_location_summary.csv` + figures.

For each survey:

1. **Window statistics** (from the exact BHAPC analysis sample): year range; mean period p̄; mean age ā; mean birth cohort c̄ = mean(year − age) (equivalently p̄ − ā — verify the equality as a check). Compute both unweighted and survey-weighted versions; report both (they should be close; the unweighted one matches what the unweighted-likelihood model "sees" — note the BHAPC handles weights via the ln(weight) covariate, so the *unweighted* observation distribution is the relevant one for peak prediction; say this in the Quarto).
2. **BHAPC peak, with uncertainty.** From the saved posterior draws of the cohort random effects: for each posterior draw, find the cohort bin with the maximum effect (and, better, also compute a quadratic-vertex estimate by fitting a parabola to that draw's cohort-effect curve over its central region, giving a continuous peak estimate). Summarize the peak-location posterior: median and 90% interval, for both the argmax and vertex versions. Also record the curvature sign (confirm concavity).
3. **APC-I peak.** From the inter-cohort deviations (panel C estimates): argmax bin, plus a quadratic-vertex fit over the central region. Report with the bin width as minimum uncertainty.
4. **Slope-flip location (APC-I panel D):** the cohort at which intra-cohort slopes cross zero (linear interpolation between adjacent bins).

**The summary table** (one row per survey): year range | p̄ | ā | c̄ | BHAPC peak (median [90% CrI]) | APC-I peak | slope-flip cohort | fixed-boomer reference (1955) | distance of peak from c̄ | distance from 1955.

**The key figure:** scatter of observed peak (y) against c̄ (x), one point per survey with uncertainty bars, identity line overlaid, and a horizontal line at 1955. If peaks track the identity line (moving with each survey's window), that supports the mechanical account (P2); if they sit on the horizontal line (same birth year everywhere), that supports a genuine generational effect. Add a companion figure overlaying all six cohort-effect curves aligned on (c − c̄) — under the mechanical account they should approximately superimpose; aligned on raw birth year they should not.

**Explanations to include in the Quarto (write these as real prose, not boilerplate):** the algebra of §0 in brief; why the year range matters (each survey observes a different slice of cohorts, so its data-center cohort c̄ differs — GSS's earlier window predicts an earlier peak than MEPS/CPS under the mechanical account, which is why comparing peak *locations across surveys with different windows* is informative at all); the identification caveat (peak location is convention-dependent; the Task 2 null peaks are the calibrated reference — cross-reference them once Task 2 is done); and a pre-stated decision rule: peaks tracking c̄ within uncertainty → consistent with mechanical fingerprint; peaks fixed across surveys at a common birth year → evidence of genuine cohort structure; intermediate → report as mixed, no forcing.

## 4. Task 2 — Null simulation through the existing pipelines

**Deliverable:** `apc_null_simulation.qmd` → rendered HTML + saved fits + figures.

**Construction of the null datasets (per survey):**
1. Start from the survey's real BHAPC analysis dataset. Keep every respondent's actual age, year, and weight — this preserves the observation window, cell structure, and weights exactly.
2. Fit a **null generating model** to the real data containing NO cohort terms: survey-weighted regression of SRH on flexible age main effects (age-group factor at the same bins the APC code uses), flexible period main effects (period factor), and a single continuous age×period interaction (centered: `age_c:year_c`). Use `svyglm` with the same design the repo's APC prep uses (weights-only where that's the convention). Record the fitted γ̂ (the `age_c:year_c` coefficient) and check it is positive and of the same order as the paper's metaregression slopes (~1.3–2.7 × 10⁻⁴ per year).
3. Generate the simulated outcome: fitted value + Gaussian noise with SD equal to the model's residual SD (weighted). `set.seed(<recorded>)`. The result is a continuous outcome with, by construction, zero cohort process. (Run the *continuous-SRH* versions of the pipelines on it; skip any binary fair/poor variants.)
   - **Sanity check before using any simulated dataset:** the null must retain the real data's inverse SRH–age relationship and its attenuation. Rerun the year-specific survey-weighted regressions (SRH ~ age) on the simulated data and confirm β_age(p) is negative in every year and attenuates toward zero over time, resembling the real Fig 1B trajectory. If it doesn't, the generating model is misspecified — stop and fix before running any pipeline.
4. **Run the existing BHAPC and APC-I pipelines on the simulated data, unchanged** — same bins, priors, chains, iterations, subsampling conventions (BRFSS 50% subsample, NHANES 20–79 in APC-I, NHANES cycle midpoints), same variance-decomposition code, same plotting code.

**Replicates:** APC-I is cheap — run ≥ 20 simulation replicates per survey and summarize the distribution of (hump peak, slope-flip location, deviation magnitudes). BHAPC is expensive — run 1 full-fidelity replicate per survey (all six; if any single survey is computationally prohibitive even as run in the original analysis, do the rest and say so explicitly — no silent dropping). Use fresh seeds per replicate; record all.

**Outputs and figures:**
- The money figure: for each survey, the fitted cohort-effect curve from the **real** data and from the **null** simulation, side by side or overlaid (same y-scale). Expected under the mechanical account: the null run reproduces a smooth inverted-U of comparable magnitude and location.
- Variance decomposition, real vs null, per survey (age/period/cohort shares side by side) — expected: null reproduces cohort > period (P4).
- APC-I panels C and D, real vs null (with the ≥20-replicate band for the null).
- **Null-calibrated peak comparison (cross-link to Task 1):** per survey, the null-simulation peak (median across replicates for APC-I; the single BHAPC null peak) vs the observed peak vs c̄ vs 1955. Add these null peaks to Task 1's summary table and key figure — the null peak is the best available reference for "where the peak lands when there is no cohort effect," because it embeds the same truncation/shrinkage conventions as the real fits.
- Report any respect in which the null runs do NOT reproduce the observed cohort signatures (e.g., observed hump systematically offset from the null hump, or larger than the null can produce) — that residual is the honest estimate of *genuine* cohort structure, and is a finding, not a failure.

**Optional but highly recommended if compute allows — Task 2b, positive control:** a second simulation identical to the null but with a **true localized boomer effect** added (a step: +δ on SRH for cohorts born 1946–1964, δ chosen ≈ the observed hump height, NO age×period interaction term). Run it through the same pipelines. Expected: a localized bump with edges, peak fixed at ~1955 in every survey regardless of window, and no full-range parabola. Presenting the observed data bracketed by these two references (pure interaction vs pure boomer step) makes the diagnosis visually self-evident and is likely a supplement figure for the paper.

**Explanations to include in the Quarto:** what the null contains and does not contain (state plainly: "these data contain no cohort effects whatsoever"); the §0 algebra as the reason we predicted, before running, that the pipelines would nonetheless display a cohort hump and slope flip (state predictions P1–P4 *before* the results); the honest two-sidedness (the identity means a true concave cohort process would equally masquerade as an interaction — the simulation demonstrates sufficiency of the interaction, not absence of cohort effects); and a closing section stating, per survey, whether the observed cohort signatures are distinguishable from the null.

## 5. Quarto standards (both documents)

Structure: Motivation (short, with the algebra box) → Methods (what was reused from which scripts — name the files; what was new; seeds; runtimes) → Results (figures + tables with real captions) → Interpretation (against the pre-stated decision rules) → Limitations (peak-location identification caveat; ordinal SRH treated as continuous; single BHAPC replicate) → `sessionInfo()`. Figures saved as both PNG (300 dpi) and PDF alongside the HTML. Machine-readable CSVs for every summary table. Nice explanations means a reader who has NOT seen this prompt can follow why the analysis exists and what each outcome would mean.

## 6. Report back (final message of your run)

1. The completed summary table: per survey — year range, c̄, observed BHAPC peak [CrI], observed APC-I peak, slope-flip cohort, null-sim peak(s), distance-from-c̄ vs distance-from-1955.
2. One-sentence verdict per survey against the decision rules, and an overall verdict: do the observed cohort signatures track the survey windows (mechanical) or a fixed generation (genuine cohort), or mixed?
3. Whether the null simulations reproduced P1–P4, and any respect in which they did not.
4. Paths to the two rendered HTMLs, all figures, CSVs, and saved fits.
5. Anything you had to assume, skip, or could not verify — stated explicitly.

## 7. Progress log (append-only)

Claude Code: append dated entries below as milestones complete. Include: date/time, session context, seeds set, fits completed with output paths, runtimes, deviations from spec (with justification), and open items for the next session.

### 2026-08-15 — Session A: Task 1, QUICK TIER (hump-location extraction from saved outputs) — COMPLETE

**Context.** First session on this spec. Orientation done strictly via `to_transfer/ronin_code_apc_etc.md` (§1 BHAPC pipeline; §7 pointer to the APC-I code/outputs on the `main` worktree). No repo exploration; no model fitting; no MCMC.

**Inputs used (all verified on disk).**
- BHAPC: `output/bhapc_full_random/<s>/<s>_bhapc_full_random_model.rds` (6/6; the `fit_bhapc_full_random()` lists whose `$model` is a stanreg with **10,000 post-warmup draws** of every cohort intercept — so per-draw peak CrIs were computed with **no refitting**), `<s>_bhapc_data.rds` (exact analysis rows — window statistics computed on these), `<s>_cohort_effects.csv` (posterior means; the spurious `Sigma[...]` row dropped), `all_surveys_summary.csv`.
- APC-I: `main/output/apc/apci/apci_cohort_avgs_all.csv` (panel C) and `apci_cohort_slopes_all.csv` (panel D), copied verbatim with md5 provenance to `analysis/apc_diagnostics/inputs/apci/`. No `apci_full_*.rds` model objects or `df_prep` exist on this machine, so APC-I was handled at the point-estimate level (as the quick tier prescribes); the APC-I window c̄ is a proxy (BHAPC rows + APC-I age filter 20–89 / NHANES 20–79).
- §2.3 pipeline check passed: variance shares re-derived from the loaded fits with `extract_variance_components()` match `all_surveys_summary.csv` / Fig S7 in all six surveys (max |Δ| 0.046 pp; `variance_share_check.csv`); draw means match `<s>_cohort_effects.csv` to <1e-8.

**Deliverables (all under `analysis/apc_diagnostics/`).**
- `01_task1_hump_location.R` (analysis script; runtime ≈ 30 s), `apc_hump_location.qmd` → `apc_hump_location.html` (rendered, self-contained).
- `output/task1/hump_location_summary.csv` (one row per survey; 73 columns incl. placeholders `null_apci_peak_median`, `null_bhapc_peak` for Task 2), `window_stats.csv`, `bhapc_peak_posterior.csv`, `bhapc_peak_draws.rds` (per-draw argmax/vertex/curvature, 10,000 × 6 — reusable), `bhapc_cohort_curves.csv`, `apci_peak_summary.csv`, `apci_slope_crossings.csv`, `apci_cohort_curves.csv`, `apci_nhanes_label_correction.csv`, `peak_tracking.csv`, `variance_share_check.csv`, `runtimes.csv`, `run_info.txt` (sessionInfo).
- Figures (PNG 300 dpi + cairo PDF) in `output/task1/figures/`: `fig1_peak_vs_cbar` (key figure; panel A x = c̄, panel B x = centre of observed cohort range), `fig2_cohort_curves_aligned` (companion 2×2), `fig3_vertex_fit_diagnostics`, `fig4_peak_offsets_dotplot`.

**Seeds.** BHAPC peak extraction is deterministic given the saved draws. `set.seed(20260815)` for the (supplementary, approximate) APC-I vertex parametric bootstrap only.

**Pre-stated rules used.** All peak measures over the central-80 % respondent-cohort region (10th–90th pct); BHAPC per-draw argmax + OLS quadratic vertex (concave draws), posterior median + 90 % interval; sensitivities central-90 %, all bins, local argmax ± 3 bins; APC-I argmax bin ± one 5-yr bin (headline), quadratic vertices with the same region rules; slope-flip = zero of a linear fit of slope on cohort over the region (+ nearest interpolated pos→neg crossing). BHAPC peaks reported on the birth-year scale (grid label + survey-specific offset δ ≈ 1.1–2.0 y, since `cohort_4yr` = period-bin start − age midpoint). Second window reference added: centre of the observed cohort range (`c_mid`).

**Headline results.** BHAPC vertex peaks (birth year, median [90 % CrI]): BRFSS 1950.7 [1947.1, 1954.3]; MEPS 1959.7 [1953.3, 1965.9]; NHIS 1946.7 [1942.6, 1950.2]; CPS 1957.5 [1950.9, 1962.5]; NHANES 1958.1 [1949.0, 1963.5]; GSS 1947.9 [1945.5, 1950.4]. They track the window (slope on c̄ 1.02 ± 0.22; on range centre 0.91 ± 0.25) with a systematic early offset (−3.9 to −9.9 y from c̄, mean −6.3; −4.6 to +2.2 y from the range centre). APC-I argmax fixed at 1939.5–1944.5 in all six (not 1955); APC-I slope-flip (linear) 1946–1956 tracks c̄ (slope 0.70 ± 0.11) at −8 ± 1.9 y. Overall verdict: **mixed, leaning mechanical** — no support for a fixed 1955 peak; the constant offsets are the quantity Task 2's null must calibrate.

**Deviations from spec / judgement calls.** (1) Argmax and vertex restricted to the central-80 % region (tail bins unstable in both methods; unrestricted values kept in the CSVs). (2) A "local ±3-bin" vertex rule was added after seeing that some APC-I humps plateau on one flank (making the central-region parabola non-concave for MEPS/NHANES); it is reported as a robustness measure, never as the headline. (3) The key figure uses the APC-I argmax ± bin width (the spec's APC-I headline) rather than the APC-I vertex. (4) A weights-only `srvyr` design was used for the weighted window means (rows with wt = 0 excluded from the weighted means only).

**Issues discovered — please review.**
- **NHANES APC-I cohort labels are wrong in the saved CSVs and in Fig S9 panels C/D.** APCI indexes cohorts as the A+P−1 diagonals; `main/R/functions/apci_analysis.R::extract_apci_results()` labels them by joining the diagonal index to `distinct(cohort_group, cohort_midpoint)` where `cohort_group = as.integer(factor(cohort_midpoint))`. NHANES's irregular 5-yr period midpoints (2002/2007/2012/2017/2021) give 27 distinct midpoints for 16 diagonals, so the diagonals were labelled with the 16 smallest midpoints (1924.5…1969.5) instead of the diagonal means (1924.5…1998.5); the five annual surveys are unaffected (regular grids, verified). Relabelled here from the documented bin definitions (`apci_nhanes_label_correction.csv`; as-filed labels retained). Recommended: fix the labelling in `apci_analysis.R` and re-render Fig S9 (x-axes of NHANES C/D are compressed by up to 29 y; estimates unaffected).
- **Zero analysis weights**: 102,267/199,349 NHIS BHAPC rows (51 %) and 5,074/75,367 NHANES rows (6.7 %) have `wt = 0` (CPS 2 rows); `prepare_bhapc_data()` sets `lnWt = 0` for them and the published fits include them. Not changed here; flagged because Task 2's null generating model (`svyglm`) must decide how to treat those rows.
- `APCI` package is **not installed** on this machine (needed for Task 2's APC-I null replicates); `ggrepel`/`kableExtra` also absent (not needed).

**Open items for Session B (Task 2).** Run ≥20 APC-I null replicates per survey (install `APCI` v1.0.8), and merge the null peaks into `hump_location_summary.csv` (columns `null_apci_peak_median`, later `null_bhapc_peak`) and into Fig 1 / Fig 4; `bhapc_peak_draws.rds` and `bhapc_cohort_curves.csv` are ready for reuse. Note for the null: under a bilinear age×period interaction the APC-I inter-cohort deviation averaged along diagonals is a flat-topped plateau centred on the grid centre (numerically checked on 14×5 and 14×11 grids), so its null "peak" will be soft — report the plateau/argmax distribution, not a single number. Nothing was committed to git this session (working tree already carried 31 unrelated modified K6 files).

### 2026-08-15 (later, same session) — APC-I label bug FIXED on `main`; Task 1 refreshed on corrected inputs; APCI installed

**Requested by Christine after the Task 1 report.** Note for the record: Task 1's quick tier never calls `APCI` (it reads the saved panel-C/D estimates), so the package's absence had no effect on Task 1; it was flagged only because Task 2 needs it. `APCI` 1.0.8 (the paper's version, still current on CRAN) and `kableExtra` are now installed.

**Bug — full extent (verified in the APCI 1.0.8 source: `cohortdeviation()` uses `cindex[i, j] = A + j − i`, index 1 = oldest; `temp_model()` builds positional `as.factor()` levels; the `cohort` argument is not used for indexing).** `main/R/functions/apci_analysis.R::extract_apci_results()` joined APCI's diagonal index to `distinct(cohort_group, cohort_midpoint)` and APCI's positional age index to the integer *label*. Effects: **NHANES** cohort labels off by up to 29 y (16 diagonals labelled with the 16 smallest of 27 row-level midpoints); **BRFSS** — newly found while fixing — the wrangled BRFSS codes all ages 80+ as 89, so the 80–84 bin is empty and APCI's positional age level 13 is "85–89": that level had no label (its estimate −0.428 was dropped from Fig S9 panel A) and BRFSS cohort labels were up to 5 y too early (labels assumed a 14th age level). GSS/MEPS/NHIS/CPS: zero change (asserted). Estimates/SEs untouched.

**Fix (on the `main` worktree, uncommitted):**
- `main/R/functions/apci_analysis.R`: new `build_apci_cohort_lookup(age_lookup, period_lookup)` (diagonal means; keeps `n_cells`), and positional age/period lookups in `extract_apci_results()`; output column set unchanged. Comment added at `prep_apci_data()`'s `cohort_group`.
- New `main/R/scripts/12c_apci_fix_nhanes_labels.R` (idempotent): backs up as-filed tables to `main/output/apc/apci/asfiled_20260213/` (16 files + README); rewrites `apci_cohort_{avgs,slopes}_{survey,all}.csv` and `apci_age_effects_{brfss,all}.csv` (BRFSS level 13 = "85–89", 87.5) from the saved level midpoints (no refit; `data_nhanes.rds` absent here and not needed); writes `apci_cohort_diagonal_lookup.csv`, `apci_cohort_label_correction_20260815.csv`; regenerates `apci_cohort_avgs_all_surveys.*`, `apci_cohort_slopes_all_surveys.*`, `apci_main_effects_all_surveys.*`, NHANES + BRFSS per-survey figures, and re-runs `12b_apci_figure.R` → **`main/output/apc/apci/apci_combined_grid.{png,pdf}` = corrected Fig S9 (swap into the manuscript)**. `main/reports/apci_report.html` re-rendered (old copy `apci_report_asfiled_20260213.html`). Runtime 34 s.
- Pointer added to the code map (`to_transfer/ronin_code_apc_etc.md`, changelog "Update 2026-08-15" + §7).

**Task 1 refreshed.** `inputs/apci/` now holds the corrected `apci_cohort_{avgs,slopes}_all.csv` (+ `apci_cohort_diagonal_lookup.csv`, `apci_cohort_label_correction_20260815.csv`; as-filed copies moved to `inputs/apci/asfiled_20260213/`; provenance updated). `01_task1_hump_location.R` no longer relabels NHANES itself; it verifies every label against the diagonal lookup and stops if handed the old files (bin-spacing guard relaxed to monotone, since BRFSS/NHANES diagonal means are irregular by construction). Re-run (34 s) and Quarto re-rendered. Only BRFSS APC-I values changed: argmax 1939.5 → 1943.9, slope-flip (linear) 1949.1 → 1953.3, central-80% vertex now concave (1949.2). BHAPC results unchanged. Headline verdicts unchanged (BHAPC peaks track the window, slope 1.02 ± 0.22; APC-I argmax fixed at 1939.5–1944.5 in all six; APC-I slope-flip tracks c̄ at −7.3 ± 2 y).

**Open items for Session B:** unchanged from the previous entry, plus: commit the `main` fix (`apci_analysis.R`, `12c_…R`, corrected outputs, backups, report) and the `bhapc` Task 1 files when convenient — nothing was committed this session; swap the corrected Fig S9 into the manuscript.

### 2026-08-15 (addendum) — PDF rendering
`apc_hump_location.qmd` now also has a `typst` format (Quarto-bundled Typst; no LaTeX on this machine): `quarto render analysis/apc_diagnostics/apc_hump_location.qmd --to typst` → `apc_hump_location.pdf` (9 pp). Source tweaks for this (HTML unaffected): `\bar` → `\overline{}` in the math; a `tbl()` helper that emits auto-width Typst tables for PDF (two widest tables split/compacted there; nothing removed from the CSVs).
