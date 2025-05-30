
<!-- README.md is generated from README.Rmd. Please edit that file -->

# riskdiff <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jackmurphy2351/riskdiff/workflows/R-CMD-check/badge.svg)](https://github.com/jackmurphy2351/riskdiff/actions)
[![Codecov test
coverage](https://codecov.io/gh/jackmurphy2351/riskdiff/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jackmurphy2351/riskdiff?branch=main)
[![CRAN
Submission](https://img.shields.io/badge/CRAN-Submitted-yellow.svg)](https://cran.r-project.org/package=riskdiff)
[![R-CMD-check](https://github.com/jackmurphy2351/riskdiff/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jackmurphy2351/riskdiff/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jackmurphy2351/riskdiff/graph/badge.svg)](https://app.codecov.io/gh/jackmurphy2351/riskdiff)

<!-- badges: end -->

The **riskdiff** package provides robust methods for calculating risk
differences (also known as prevalence differences in cross-sectional
studies) using generalized linear models with automatic link function
selection.

## Features

- **🔧 Robust model fitting**: Automatically tries identity, log, and
  logit links with graceful fallback
- **📊 Stratified analysis**: Support for multi-level stratification
- **📋 Publication-ready output**: Formatted tables and confidence
  intervals
- **🛡️ Missing data handling**: Graceful handling of incomplete cases
- **⚙️ Flexible confidence intervals**: Support for different confidence
  levels
- **📈 Multiple link functions**: Automatic selection or manual
  specification

## Author

**John D. Murphy, MPH, PhD** ORCID:
[0000-0002-7714-9976](https://orcid.org/0000-0002-7714-9976)

## Installation

You can install the development version of riskdiff from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jackmurphy2351/riskdiff")
```

## Quick Start

``` r
library(riskdiff)

# Load example data
data(birthweight)

# Simple risk difference
result <- calc_risk_diff(
  data = birthweight,
  outcome = "low_birthweight",
  exposure = "smoking"
)
#> Waiting for profiling to be done...

print(result)
#> Risk Difference Analysis Results
#> ================================
#> 
#> Confidence level: 95% 
#> Number of comparisons: 1 
#> 
#>  Exposure Risk Difference          95% CI P-value    Model
#>   smoking           8.40% (5.21%, 11.97%)  <0.001 identity
```

## Key Functions

### Basic Usage

``` r
# Age-adjusted risk difference
rd_adjusted <- calc_risk_diff(
  data = birthweight,
  outcome = "low_birthweight", 
  exposure = "smoking",
  adjust_vars = "maternal_age"
)
#> Waiting for profiling to be done...

print(rd_adjusted)
#> Risk Difference Analysis Results
#> ================================
#> 
#> Confidence level: 95% 
#> Number of comparisons: 1 
#> 
#>  Exposure Risk Difference          95% CI P-value Model
#>   smoking           8.51% (2.77%, 14.25%)  <0.001   log
```

### Stratified Analysis

``` r
# Stratified by race
rd_stratified <- calc_risk_diff(
  data = birthweight,
  outcome = "low_birthweight",
  exposure = "smoking",
  adjust_vars = "maternal_age",
  strata = "race"
)
#> Waiting for profiling to be done...
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: algorithm stopped at boundary value
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: glm.fit: algorithm stopped at boundary value
#> Waiting for profiling to be done...

print(rd_stratified)
#> Risk Difference Analysis Results
#> ================================
#> 
#> Confidence level: 95% 
#> Number of comparisons: 4 
#> 
#>  Exposure Risk Difference           95% CI P-value    Model
#>   smoking          10.23%  (6.06%, 15.03%)  <0.001 identity
#>   smoking           8.05% (-5.25%, 21.34%)   0.024      log
#>   smoking           3.85%  (-1.38%, 9.08%)   0.211      log
#>   smoking          12.16% (-2.88%, 27.20%)   0.113 identity
```

### Table Creation

``` r
# Create a simple text table
cat(create_simple_table(rd_stratified, "Risk by Smoking Status and Race"))
#> Risk by Smoking Status and Race
#> ====================================================================================
#> Exposure             Risk Diff       95% CI                    P-value    Model     
#> ====================================================================================
#> smoking              10.23%          (6.06%, 15.03%)           <0.001     identity  
#> smoking              8.05%           (-5.25%, 21.34%)          0.024      log       
#> smoking              3.85%           (-1.38%, 9.08%)           0.211      log       
#> smoking              12.16%          (-2.88%, 27.20%)          0.113      identity  
#> ====================================================================================
```

``` r
# Create publication-ready table (requires kableExtra)
library(kableExtra)
create_rd_table(rd_stratified, caption = "Risk of Low Birth Weight by Smoking Status")
```

## Methodology

The package uses generalized linear models with different link
functions:

1.  **Identity link** (preferred): Directly estimates risk differences
2.  **Log link**: Estimates relative risks, transforms to risk
    differences  
3.  **Logit link**: Estimates odds ratios, transforms to risk
    differences

When the identity link fails to converge (common with binary outcomes),
the package automatically tries alternative approaches.

## Advanced Features

### Link Function Selection

``` r
# Force a specific link function
rd_logit <- calc_risk_diff(
  birthweight, 
  "low_birthweight", 
  "smoking",
  link = "logit"
)
#> Waiting for profiling to be done...

# Check which model was used
rd_logit$model_type
#> [1] "logit"
```

### Confidence Intervals

``` r
# 90% confidence intervals
rd_90 <- calc_risk_diff(
  birthweight,
  "low_birthweight", 
  "smoking",
  alpha = 0.10  # 1 - 0.10 = 90% CI
)
#> Waiting for profiling to be done...

print(rd_90)
#> Risk Difference Analysis Results
#> ================================
#> 
#> Confidence level: 90% 
#> Number of comparisons: 1 
#> 
#>  Exposure Risk Difference          95% CI P-value    Model
#>   smoking           8.40% (5.70%, 11.37%)  <0.001 identity
```

### Multiple Stratification

``` r
# Stratify by multiple variables
rd_multi <- calc_risk_diff(
  data = birthweight,
  outcome = "low_birthweight",
  exposure = "smoking", 
  strata = c("race", "education")
)
#> Waiting for profiling to be done...
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: glm.fit: algorithm did not converge
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: algorithm stopped at boundary value
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Warning: step size truncated: out of bounds
#> Warning: glm.fit: algorithm stopped at boundary value
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Warning: glm.fit: algorithm did not converge
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: glm.fit: algorithm did not converge
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: step size truncated due to divergence
#> Warning: glm.fit: algorithm did not converge
#> Waiting for profiling to be done...
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: step size truncated: out of bounds
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Waiting for profiling to be done...
#> Warning: step size truncated: out of bounds
#> Warning: glm.fit: algorithm stopped at boundary value
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: step size truncated: out of bounds
#> Warning: glm.fit: algorithm stopped at boundary value
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...

# Show first few results
head(rd_multi)
#> Risk Difference Analysis Results
#> ================================
#> 
#> Confidence level: 95% 
#> Number of comparisons: 6 
#> 
#>  Exposure Risk Difference            95% CI P-value    Model
#>   smoking          -2.02% (-13.08%, 20.56%)   0.800 identity
#>   smoking          11.82%   (3.02%, 22.73%)   0.019 identity
#>   smoking          12.23%   (6.14%, 19.59%)  <0.001 identity
#>   smoking           6.59%   (0.06%, 16.14%)   0.106 identity
#>   smoking         -20.83% (-37.08%, -4.59%)   0.012 identity
#>   smoking          31.03%   (6.97%, 55.10%)   0.011 identity
```

## Example Dataset

The package includes a simulated birth weight dataset with realistic
associations:

``` r
data(birthweight)
str(birthweight)
#> 'data.frame':    2500 obs. of  8 variables:
#>  $ id             : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ smoking        : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 2 1 ...
#>  $ maternal_age   : num  20 15 21 28 30 28 35 28 32 21 ...
#>  $ race           : Factor w/ 4 levels "White","Black",..: 1 1 1 1 1 1 1 2 3 1 ...
#>  $ education      : Factor w/ 4 levels "Less than HS",..: 3 4 4 4 2 2 2 2 3 2 ...
#>  $ prenatal_care  : Factor w/ 2 levels "Adequate","Inadequate": 1 1 1 1 1 1 1 2 1 1 ...
#>  $ parity         : num  3 1 2 0 0 1 3 0 0 2 ...
#>  $ low_birthweight: num  0 0 0 0 1 0 1 0 0 0 ...

# Summary statistics
table(birthweight$smoking, birthweight$low_birthweight)
#>      
#>          0    1
#>   No  1942  115
#>   Yes  381   62
```

## When to Use Risk Differences

Risk differences are particularly useful when:

- You want to communicate **absolute** rather than relative effects
- The outcome is relatively common (\>10%)
- You’re doing **causal inference** or policy analysis
- You need results that are easily interpretable by non-statisticians

## Comparison with Other Measures

| Measure | Interpretation | Best When |
|----|----|----|
| **Risk Difference** | Absolute change in risk | Outcome common, policy decisions |
| Risk Ratio | Relative change in risk | Outcome rare, etiologic research |
| Odds Ratio | Change in odds | Case-control studies, rare outcomes |

## Getting Help

- 📖 **Vignettes**: `browseVignettes("riskdiff")`
- 🐛 **Bug reports**: [GitHub
  Issues](https://github.com/jackmurphy2351/riskdiff/issues)
- 💡 **Feature requests**: [GitHub
  Issues](https://github.com/jackmurphy2351/riskdiff/issues)

## Citation

If you use this package in your research, please cite:

``` r
citation("riskdiff")
```

## Related Packages

- **epitools**: Basic epidemiological calculations
- **epi**: Extended epidemiological functions  
- **fmsb**: Medical statistics and epidemiology
- **Epi**: Statistical analysis in epidemiology

## Code of Conduct

Please note that the riskdiff project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
