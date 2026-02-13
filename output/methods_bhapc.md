# BHAPC Methods Section

## SHORT VERSION (Main Methods Section)

To decompose variance in self-rated health (SRH) into age, period, and cohort components, we fitted Bayesian hierarchical age-period-cohort (BHAPC) models to each of six nationally representative surveys (NHIS, MEPS, BRFSS, GSS, NHANES, and CPS). Following Yang and Land (2006, 2013), we specified the model as: SRH_i = β_0 + β_1·ln(weight_i) + u_age[j] + u_period[k] + u_cohort[l] + ε_i, where u_age, u_period, and u_cohort are crossed random intercepts for age group, period, and birth cohort, respectively. Respondents were grouped into 4-year bins for all three temporal dimensions, with cohort calculated as period minus age midpoint. Models were estimated using Markov Chain Monte Carlo via rstanarm in R (Goodrich et al., 2020), with 4 chains of 6,000 iterations each and default weakly informative priors.

Variance decomposition partitioned total SRH variance into age, period, cohort, and residual components by extracting variance estimates from the fitted hierarchical model. The percentage attributable to each component was calculated as σ²_component / σ²_total × 100. Random effects for each age group, period, and birth cohort were extracted with 90% credible intervals (5th and 95th posterior percentiles). Effects are centered at zero, so positive values indicate higher-than-average SRH and negative values indicate lower-than-average SRH for that temporal grouping.

---

## LONG VERSION (Supplementary Methods)

### Bayesian Hierarchical Age-Period-Cohort (BHAPC) Model

To decompose variance in self-rated health (SRH) into age, period, and cohort components, we fitted Bayesian hierarchical age-period-cohort (BHAPC) models to each of six nationally representative surveys (NHIS, MEPS, BRFSS, GSS, NHANES, and CPS). Following the approach of Yang and Land (2006, 2013), we specified crossed random intercepts for period and cohort, which allows partial pooling of estimates within each temporal grouping factor while avoiding the identification problem inherent in classical APC models. Unlike standard HAPC models that treat age as a fixed polynomial effect, we extended the model to treat all three APC components as random effects, enabling direct comparison of their relative contributions to total variance.

The full random effects BHAPC model was specified as:

SRH_i = β_0 + β_1·ln(weight_i) + u_age[j] + u_period[k] + u_cohort[l] + ε_i

where u_age[j], u_period[k], and u_cohort[l] are random intercepts for age group j, period k, and birth cohort l, respectively, each drawn from a normal distribution with mean zero and estimated variance (e.g., u_age ~ N(0, σ²_age)). Respondents were grouped into 4-year age bands (18-21, 22-25, ..., 85-89), 4-year period bins based on survey year, and 4-year birth cohort bins calculated as period minus age midpoint. Survey weights were included as a log-transformed covariate (ln(weight)) to partially account for complex sampling designs, following Graf (2021).

Models were estimated using Markov Chain Monte Carlo (MCMC) via the rstanarm package (Goodrich et al., 2020) in R, which implements Hamiltonian Monte Carlo sampling through Stan (Carpenter et al., 2017). We ran 4 chains with 6,000 iterations each (3,000 warmup, 3,000 sampling), using an adapt_delta of 0.998 to minimize divergent transitions. Convergence was assessed using the potential scale reduction factor (R-hat < 1.01) and effective sample size (n_eff > 400). Default weakly informative priors were used, including LKJ priors on correlation matrices for random effects.

### Variance Decomposition

The variance decomposition partitions total variance in SRH into components attributable to age, period, cohort, and residual variation. For each fitted model, we extracted variance components using VarCorr() for random effect variances and sigma() for residual variance. The percentage of total variance explained by each component was calculated as:

% Variance_component = (σ²_component / σ²_total) × 100

where σ²_total = σ²_age + σ²_period + σ²_cohort + σ²_residual. This decomposition quantifies the relative importance of life course (age), historical context (period), and generational membership (cohort) in explaining variation in self-rated health.

### Random Effect Extraction and Credible Intervals

For the APC effects grid figure, we extracted posterior distributions of the random intercepts for each age group, period, and cohort level. Point estimates represent posterior means, and uncertainty is displayed using 90% credible intervals (5th and 95th percentiles of the posterior distribution). These intervals represent the range within which the true effect lies with 90% posterior probability, given the data and model assumptions. Effects are centered at zero (the grand mean), so positive values indicate higher-than-average SRH and negative values indicate lower-than-average SRH for that age group, period, or birth cohort.

### Figure Interpretation

**Figure: Variance Comparison (Bar Chart)** - This figure displays the percentage of total variance in SRH explained by age, period, and cohort effects across all six surveys. Bars are grouped by survey and colored by APC component. Age consistently explains the largest share of variance (6-19%), reflecting the strong biological and social gradients in health across the life course. Period and cohort effects are substantially smaller (<2.5%), suggesting that after accounting for age, temporal trends and generational differences contribute modestly to variation in self-rated health.

**Figure: APC Effects Grid** - This 6×3 panel figure displays the estimated random effects for age (left column), period (middle column), and cohort (right column) across all six surveys (rows). Points show posterior mean estimates, and error bars represent 90% credible intervals. X-axes are aligned across surveys within each column to facilitate comparison. Age effects show the expected decline in SRH with advancing age across all surveys. Period effects are generally small and fluctuate around zero, with modest negative trends in recent years in some surveys. Cohort effects reveal small generational differences, with some evidence of declining SRH among more recent birth cohorts.

---

## References

- Bell, A., & Jones, K. (2015). Age, period and cohort processes in longitudinal and life course analysis: A multilevel perspective. In *Handbook of Research Methods on Social Demography* (pp. 197-213). Edward Elgar Publishing.
- Carpenter, B., et al. (2017). Stan: A probabilistic programming language. *Journal of Statistical Software*, 76(1).
- Goodrich, B., et al. (2020). rstanarm: Bayesian applied regression modeling via Stan. R package version 2.21.1.
- Graf, G. H. (2021). [Dissertation or relevant publication on BHAPC methodology]
- Yang, Y., & Land, K. C. (2006). A mixed models approach to the age-period-cohort analysis of repeated cross-section surveys, with an application to data on trends in verbal test scores. *Sociological Methodology*, 36(1), 75-97.
- Yang, Y., & Land, K. C. (2013). *Age-Period-Cohort Analysis: New Models, Methods, and Empirical Applications*. CRC Press.

---

## Technical Parameters (for Supplementary Table)

| Parameter | Value | Justification |
|-----------|-------|---------------|
| Age bins | 4-year groups (18-21, 22-25, ..., 85-89) | Balances granularity with sample size per bin |
| Period bins | 4-year groups | Matches age bins for cohort calculation |
| Cohort calculation | period - age_midpoint | Standard APC identity |
| Iterations | 6,000 (3,000 warmup + 3,000 sampling) | Ensures convergence for complex hierarchical model |
| adapt_delta | 0.998 | Minimizes divergent transitions in Stan |
| Chains | 4 | Standard for assessing convergence |
| Credible intervals | 90% (5th, 95th percentiles) | Posterior quantile-based |
| Priors | Default weakly informative (rstanarm) | Sensitivity tested |
| Software | rstanarm 2.21.1, Stan, R 4.x | |
