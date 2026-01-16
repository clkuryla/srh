Generate or modify a figure: $ARGUMENTS

## Quick Reference

**Available figures:**
- Figure 1: Convergence (mean SRH trends + age coefficient → 0)
- Figure 2: Lexis diagrams (APC, no diagonal = not cohort)
- Figure 3: Coefficient stability (covariates → SRH over time)
- Figure 4: Prevalence trends (covariate prevalence by age over time)

## For each figure request, I will:

1. **Confirm scope** - Which surveys, years, variables?
2. **Use shared functions** - theme_srh(), plot_trends(), etc.
3. **Generate code** - Clean, documented, using the standard structure
4. **Save outputs** - PNG (review) + PDF (publication)

## Standard output paths:
```r
# Review draft
"output/figures/fig[N]_[name]_draft_YYYYMMDD.png"

# Final version
"output/figures/fig[N]_[name].pdf"
"output/figures/fig[N]_[name].png"
```

## If requesting modifications:
- Tell me what to change (colors, labels, layout, etc.)
- I'll show the diff and update

## Cohesion reminders:
- All figures use theme_srh()
- Age groups use age_colors palette
- Surveys use survey_colors palette
- Save with save_figure() for consistent format
