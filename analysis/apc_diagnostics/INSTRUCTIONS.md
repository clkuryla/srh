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

*(no entries yet)*
