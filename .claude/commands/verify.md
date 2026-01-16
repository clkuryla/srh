Verify that new results match previous analyses.

## Purpose

When regenerating figures from clean code, we need to verify that key numbers match the original messy code output. This prevents introducing errors during cleanup.

## Verification checklist

For Figure 1 (convergence):
- [ ] Mean SRH by age group at start year matches
- [ ] Mean SRH by age group at end year matches
- [ ] Age coefficient values match (within rounding)
- [ ] Trend direction is the same
- [ ] All 6 surveys show consistent patterns

For Figure 3 (coefficients):
- [ ] Coefficient directions match
- [ ] Rough magnitudes match
- [ ] Stability pattern (flat vs. changing) matches

For Figure 4 (prevalence):
- [ ] Prevalence estimates in reasonable range
- [ ] Trend directions match expectations
- [ ] Age group ordering makes sense

## How to verify

1. Run new code and save key statistics
2. Compare to output from old code (provide old output or file path)
3. Flag any discrepancies > 5% relative difference

## Provide me with:

- The figure/analysis to verify
- Key numbers from your old output (or path to old results file)
- I'll compare and flag any issues

If discrepancies found:
- Check for coding differences
- Check for data subsetting differences  
- Check for weight application differences
