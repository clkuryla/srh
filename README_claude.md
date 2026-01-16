# SRH Convergence Paper - Figure Generation

Clean repository for generating publication-ready figures.

## Quick Start

```bash
# In Claude Code
cd your-project
claude

# Load setup
> Run 00_setup.R first

# Generate a figure
> /figure 1 - convergence trends all surveys
```

## Figures

| Figure | Content | Surveys |
|--------|---------|---------|
| 1 | Convergence: Mean SRH trends + Age coefficient → 0 | All 6 |
| 2 | Lexis diagrams (no diagonal = not cohort) | All 6 |
| 3 | Coefficient stability over time | BRFSS, MEPS, NHIS, NHANES |
| 4 | Prevalence trends by age | BRFSS, MEPS, NHIS, NHANES |

## Structure

```
R/
├── functions/
│   ├── theme_srh.R      # Shared theme + color palettes
│   ├── plot_utils.R     # Trend plotting functions
│   └── plot_lexis.R     # Lexis diagram functions
└── scripts/
    ├── 00_setup.R       # Run first - loads everything
    ├── 01_figure1.R     # Convergence panels
    ├── 02_figure2.R     # Lexis diagrams
    ├── 03_figure3.R     # Coefficient trends
    └── 04_figure4.R     # Prevalence trends
```

## Shared Functions

```r
# All figures use these for consistency:
theme_srh()              # ggplot theme
scale_color_age()        # age group colors
scale_color_survey()     # survey colors
plot_trends()            # for Figures 1, 4
plot_coefficient_trends()# for Figure 3
plot_lexis_surface()     # for Figure 2
save_figure()            # PNG + PDF output
```

## Commands

| Command | Purpose |
|---------|---------|
| `/figure [N]` | Generate or modify a figure |
| `/verify` | Check results match old code |
| `/tweak` | Quick aesthetic changes |

## Color Palettes

**Age groups:** Red → Salmon → Yellow → Blue → Navy → Teal (young → old)

**Surveys:** Green (NHIS), Orange (MEPS), Purple (BRFSS), Pink (GSS), Lime (NHANES), Gold (CPS)

## Workflow

1. Load setup: `source("R/scripts/00_setup.R")`
2. Generate figure using shared functions
3. Verify against old results if available
4. Save with `save_figure(plot, "fig1_convergence")`
