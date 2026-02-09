#!/bin/bash
# Quick status check for BHAPC covariate analyses

echo "=== BHAPC Covariate Analysis Status ==="
echo "Time: $(date)"
echo ""

for survey in meps nhis; do
  log="output/bhapc_nhis_meps_covariates/${survey}/run.log"
  echo "--- ${survey^^} ---"
  if [ -f "$log" ]; then
    # Check if complete
    if grep -q "COMPLETE" "$log"; then
      echo "STATUS: COMPLETE"
      grep -E "(Cohort variance:|Finished at:)" "$log" | tail -2
    else
      # Get current model
      current_model=$(grep -E "Fitting (m[1-4]|M[1-4])" "$log" | tail -1)
      if [ -n "$current_model" ]; then
        echo "Current: $current_model"
      fi
      # Get latest iteration progress
      latest_iter=$(grep "Iteration:" "$log" | tail -1)
      if [ -n "$latest_iter" ]; then
        echo "Progress: $latest_iter"
      fi
    fi
  else
    echo "Log not found"
  fi
  echo ""
done

# Check if processes still running
echo "--- Active R Processes ---"
pgrep -f "17[ab]_bhapc" > /dev/null && echo "Scripts running" || echo "Scripts not running (may have finished or failed)"
