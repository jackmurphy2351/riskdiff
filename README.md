<!-- README.md is generated from README.Rmd. Please edit that file -->

# riskdiff <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/riskdiff)](https://CRAN.R-project.org/package=riskdiff) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/riskdiff)](https://CRAN.R-project.org/package=riskdiff) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![R-CMD-check](https://github.com/jackmurphy2351/riskdiff/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jackmurphy2351/riskdiff/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/jackmurphy2351/riskdiff/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jackmurphy2351/riskdiff?branch=main)

<!-- badges: end -->

The **riskdiff** package provides robust methods for calculating risk differences (also known as prevalence differences in cross-sectional studies) using generalized linear models with automatic link function selection and **boundary detection**.

## ✨ Now Available on CRAN!

**riskdiff** v0.2.1 is now published on CRAN with cutting-edge boundary detection capabilities that identify when maximum likelihood estimates lie at the edge of the parameter space - a common issue with identity link models that other packages ignore.

## Features

-   **🎯 Smart boundary detection**: Automatically detects when GLMs hit parameter constraints
-   **🔧 Robust model fitting**: Tries identity, log, and logit links with graceful fallback\
-   **📊 Stratified analysis**: Support for multi-level stratification
-   **📋 Publication-ready output**: Formatted tables and confidence intervals
-   **🛡️ Missing data handling**: Graceful handling of incomplete cases
-   **⚙️ Flexible confidence intervals**: Robust methods for boundary cases
-   **📈 Multiple link functions**: Automatic selection with boundary-aware switching
-   **🔍 Transparent diagnostics**: Clear reporting of model methods and boundary issues

## Author

**John D. Murphy, MPH, PhD** ORCID: [0000-0002-7714-9976](https://orcid.org/0000-0002-7714-9976)

## Installation

### CRAN (Stable Release)

Install the latest stable version from CRAN:

``` r
install.packages("riskdiff")
```

### Development Version

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jackmurphy2351/riskdiff")
```

## Quick Start

``` r
library(riskdiff)

# Load example data
data(cachar_sample)

# Simple risk difference with boundary detection
result <- calc_risk_diff(
  data = cachar_sample,
  outcome = "abnormal_screen",
  exposure = "smoking"
)
#> Waiting for profiling to be done...

print(result)
#> Risk Difference Analysis Results (v0.2.1) 
#> ========================================= 
#> 
#> Confidence level: 95% 
#> Number of comparisons: 1 
#> 
#>  Exposure Risk Difference          95% CI P-value    Model    N Boundary
#>   smoking          10.68% (5.95%, 15.75%)  <0.001 identity 2500         
#>  Quality
#>     Good
```

## 🎯 Boundary Detection in Action

``` r
# Create data that challenges standard GLM methods
set.seed(123)
challenging_data <- data.frame(
  outcome = c(rep(0, 40), rep(1, 60)),  # High baseline risk
  exposure = factor(c(rep("No", 50), rep("Yes", 50))),
  age = rnorm(100, 45, 10)
)

# riskdiff handles this gracefully with boundary detection
result <- calc_risk_diff(
  data = challenging_data,
  outcome = "outcome", 
  exposure = "exposure",
  adjust_vars = "age",
  verbose = TRUE  # Shows diagnostic information
)
#> Warning in doTryCatch(return(expr), name, parentenv, handler): Possible
#> separation detected. Risk difference estimates may be unstable.
#> Formula: outcome ~ exposure + age
#> Sample size: 100
#> Trying identity link...
#> Using starting values: 0.2, 0.8, 0.004
#> Identity link error: cannot find valid starting values: please specify some
#> Trying log link...
#> log link error: no valid set of coefficients has been found: please supply starting values
#> Trying logit link...
#> Boundary detection results:
#>   Boundary detected: TRUE
#>   Boundary type: upper_boundary_near
#>   Probability range: [0.17876, 1]
#> Note: Model converged but MLE is on parameter space boundary.
#> Boundary type: upper_boundary_near
#> ✓logit link converged
#> ⚠ Perfect or quasi-perfect separation detected. Results may be unreliable.
#> ⚠ Boundary case detected: upper_boundary_near
#> Using robust CI method: bootstrap

print(result)
#> Risk Difference Analysis Results (v0.2.1) 
#> ========================================= 
#> 
#> Confidence level: 95% 
#> Number of comparisons: 1 
#> Boundary cases detected: 1 of 1 
#> Boundary CI method: auto 
#> 
#>  Exposure Risk Difference         95% CI P-value Model   N
#>  exposure          80.06% (0.00%, 0.00%)       — logit 100
#>               Boundary  Quality
#>  ⚠ upper_boundary_near Boundary
#> 
#> ⚠  Boundary Case Details:
#> ========================= 
#> Row 1 ( exposure ): Type: upper_boundary_near | CI method: bootstrap 
#> 
#> Boundary Type Guide:
#> • upper_bound: Fitted probabilities near 1 (risk saturation)
#> • lower_bound: Fitted probabilities near 0 (very rare outcomes)
#> • separation: Complete/quasi-separation detected
#> • both_bounds: Mixed boundary issues across observations
#> ⚠  indicates robust confidence intervals were used
#> 
#> Statistical Note:
#> ================
#> Standard asymptotic theory may not apply for boundary cases.
#> Confidence intervals use robust methods when boundary detected.
#> For failed analyses, consider alternative estimation approaches.

# Check if boundary cases were detected
if (any(result$on_boundary)) {
  cat("\n🚨 Boundary case detected! Using robust inference methods.\n")
  cat("Boundary type:", unique(result$boundary_type[result$on_boundary]), "\n")
  cat("CI method:", unique(result$ci_method[result$on_boundary]), "\n")
}
#> 
#> 🚨 Boundary case detected! Using robust inference methods.
#> Boundary type: upper_boundary_near 
#> CI method: bootstrap
```

## Key Functions

### Basic Usage with Enhanced Diagnostics

``` r
# Age-adjusted risk difference with boundary detection
rd_adjusted <- calc_risk_diff(
  data = cachar_sample,
  outcome = "abnormal_screen", 
  exposure = "smoking",
  adjust_vars = "age",
  boundary_method = "auto"  # Automatic robust method selection
)

print(rd_adjusted)
#> Risk Difference Analysis Results (v0.2.1) 
#> ========================================= 
#> 
#> Confidence level: 95% 
#> Number of comparisons: 1 
#> 
#>  Exposure Risk Difference         95% CI P-value Model    N Boundary Quality
#>   smoking          10.94% (0.00%, 0.00%)       — logit 2500             Good
```

### Stratified Analysis with Boundary Awareness

``` r
# Stratified by residence with boundary detection
rd_stratified <- calc_risk_diff(
  data = cachar_sample,
  outcome = "abnormal_screen",
  exposure = "smoking",
  adjust_vars = "age",
  strata = "residence"
)
#> Waiting for profiling to be done...

print(rd_stratified)
#> Risk Difference Analysis Results (v0.2.1) 
#> ========================================= 
#> 
#> Confidence level: 95% 
#> Number of comparisons: 3 
#> 
#>  Exposure Risk Difference           95% CI P-value    Model    N Boundary
#>   smoking          11.63%   (0.00%, 0.00%)       —    logit 2158         
#>   smoking           9.99% (-5.89%, 25.87%)   0.218 identity  251         
#>   smoking          -3.86%   (0.00%, 0.00%)       —      log   91         
#>  Quality
#>     Good
#>  Wide CI
#>     Good

# Summary of boundary cases across strata
boundary_summary <- rd_stratified[rd_stratified$on_boundary, 
                                  c("residence", "boundary_type", "ci_method")]
if (nrow(boundary_summary) > 0) {
  cat("\nBoundary cases by stratum:\n")
  print(boundary_summary)
}
```

### Table Creation with Boundary Indicators

``` r
# Create a simple text table with boundary information
cat(create_simple_table(rd_stratified, "Risk by Smoking Status and Residence"))
#> Risk by Smoking Status and Residence
#> ====================================================================================
#> Exposure             Risk Diff       95% CI                    P-value    Model     
#> ====================================================================================
#> smoking              11.63%          (0.00%, 0.00%)            NA         logit     
#> smoking              9.99%           (-5.89%, 25.87%)          0.218      identity  
#> smoking              -3.86%          (0.00%, 0.00%)            NA         log       
#> ====================================================================================
```

``` r
# Create publication-ready table (requires kableExtra)
library(kableExtra)
create_rd_table(rd_stratified, 
                caption = "Risk of Abnormal Screening Result by Smoking Status",
                include_model_type = TRUE)
```

## 🧠 Statistical Methodology

### GLM Approach with Boundary Detection

The package uses generalized linear models with different link functions:

1.  **Identity link** (preferred): Directly estimates risk differences
2.  **Log link**: Estimates relative risks, transforms to risk differences\
3.  **Logit link**: Estimates odds ratios, transforms to risk differences

**Key Innovation**: When models hit parameter space boundaries (common with identity links), the package: - 🔍 **Detects boundary cases** automatically - ⚠️ **Warns users** about potential inference issues\
- 🛡️ **Uses robust confidence intervals** when appropriate - 📊 **Reports methodology transparently**

### Boundary Detection Types

-   **Upper bound**: Fitted probabilities near 1 (risk saturation)
-   **Lower bound**: Fitted probabilities near 0 (risk floor)
-   **Separation**: Complete/quasi-separation in logistic models
-   **Both bounds**: Multiple boundary issues detected

## Advanced Features

### Boundary Method Control

``` r
# Force specific boundary handling
rd_conservative <- calc_risk_diff(
  cachar_sample,
  "abnormal_screen", 
  "smoking",
  boundary_method = "auto"  # Options: "auto", "profile", "wald"
)
#> Waiting for profiling to be done...

# Check which methods were used
table(rd_conservative$ci_method)
#> 
#> profile 
#>       1
```

### Link Function Selection with Boundary Awareness

``` r
# Force a specific link function
rd_logit <- calc_risk_diff(
  cachar_sample, 
  "abnormal_screen", 
  "smoking",
  link = "logit"
)

# Check which model was used and if boundaries detected
cat("Model used:", rd_logit$model_type, "\n")
#> Model used: logit
cat("Boundary detected:", rd_logit$on_boundary, "\n")
#> Boundary detected: FALSE
```

### Confidence Intervals with Robust Methods

``` r
# 90% confidence intervals with boundary detection
rd_90 <- calc_risk_diff(
  cachar_sample,
  "abnormal_screen", 
  "smoking",
  alpha = 0.10  # 1 - 0.10 = 90% CI
)
#> Waiting for profiling to be done...

print(rd_90)
#> Risk Difference Analysis Results (v0.2.1) 
#> ========================================= 
#> 
#> Confidence level: 90% 
#> Number of comparisons: 1 
#> 
#>  Exposure Risk Difference          95% CI P-value    Model    N Boundary
#>   smoking          10.68% (6.68%, 14.91%)  <0.001 identity 2500         
#>  Quality
#>     Good

# The package automatically uses appropriate CI methods for boundary cases
```

## 📊 Understanding Results

### Enhanced Result Structure

``` r
# Examine the enhanced result structure
data(cachar_sample)
result <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
#> Waiting for profiling to be done...
names(result)
#>  [1] "exposure_var"     "rd"               "ci_lower"         "ci_upper"        
#>  [5] "p_value"          "model_type"       "on_boundary"      "boundary_type"   
#>  [9] "boundary_warning" "ci_method"        "n_obs"

# Key columns:
# - on_boundary: Was a boundary case detected?
# - boundary_type: What type of boundary?
# - boundary_warning: Detailed diagnostic message
# - ci_method: Which CI method was used?
```

## Example Dataset

The package includes a realistic simulated cancer screening dataset:

``` r
data(cachar_sample)
str(cachar_sample)
#> 'data.frame':    2500 obs. of  12 variables:
#>  $ id                : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ age               : int  53 25 18 28 51 25 56 20 58 18 ...
#>  $ sex               : Factor w/ 2 levels "male","female": 2 1 2 2 1 2 1 1 1 1 ...
#>  $ residence         : Factor w/ 3 levels "rural","urban",..: 3 1 1 1 1 1 1 1 1 1 ...
#>  $ smoking           : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 2 1 1 1 ...
#>  $ tobacco_chewing   : Factor w/ 2 levels "No","Yes": 2 1 1 2 2 1 2 1 2 2 ...
#>  $ areca_nut         : Factor w/ 2 levels "No","Yes": 2 2 2 2 1 1 2 1 2 2 ...
#>  $ alcohol           : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 2 1 2 ...
#>  $ abnormal_screen   : int  0 0 0 0 0 0 1 0 1 0 ...
#>  $ head_neck_abnormal: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ age_group         : Factor w/ 3 levels "Under 40","40-60",..: 2 1 1 1 2 1 2 1 2 1 ...
#>  $ tobacco_areca_both: Factor w/ 2 levels "No","Yes": 2 1 1 2 1 1 2 1 2 2 ...

# Summary statistics showing realistic associations
table(cachar_sample$smoking, cachar_sample$abnormal_screen)
#>      
#>          0    1
#>   No  1851  317
#>   Yes  248   84

# Risk difference analysis
rd_analysis <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
#> Waiting for profiling to be done...
cat("Smoking increases risk of abnormal screening result by", 
    sprintf("%.1f", rd_analysis$rd * 100), "percentage points\n")
#> Smoking increases risk of abnormal screening result by 10.7 percentage points
```

## When to Use Risk Differences

Risk differences are particularly valuable when:

-   **Policy decisions**: You need the absolute impact size
-   **Clinical practice**: Communicating real-world effect sizes
-   **Common outcomes**: When outcome prevalence \> 10%
-   **Causal inference**: For intervention planning
-   **Public health**: When relative measures can mislead

## Comparison with Other Measures

| Measure | Interpretation | Best When | riskdiff Advantage |
|----|----|----|----|
| **Risk Difference** | Absolute change in risk | Common outcomes, policy | **Boundary detection** |
| Risk Ratio | Relative change in risk | Rare outcomes | Standard methods only |
| Odds Ratio | Change in odds | Case-control studies | Standard methods only |

## 🔬 Statistical Foundation

This package implements methods based on:

-   **Donoghoe & Marschner (2018)** - Robust GLM fitting methods for log-binomial models
-   **Marschner & Gillett (2012)** - Boundary detection for log-binomial models
-   **Rothman, Greenland & Lash (2008)** - Modern epidemiological methods
-   **Austin (2011)** - Propensity score methods for causal inference
-   **Hernán & Robins (2020)** - Causal inference methodology

## Getting Help

-   📖 **Vignettes**: `browseVignettes("riskdiff")`
-   🐛 **Bug reports**: [GitHub Issues](https://github.com/jackmurphy2351/riskdiff/issues)
-   💡 **Feature requests**: [GitHub Issues](https://github.com/jackmurphy2351/riskdiff/issues)
-   📧 **Questions**: Use GitHub Discussions
-   📋 **CRAN page**: <https://CRAN.R-project.org/package=riskdiff>

## Citation

If you use this package in your research, please cite:

``` r
citation("riskdiff")
```

## Related Packages

-   **epitools**: Basic epidemiological calculations (no boundary detection)
-   **epi**: Extended epidemiological functions (no boundary detection)
-   **fmsb**: Medical statistics and epidemiology (no boundary detection)
-   **Epi**: Statistical analysis in epidemiology (no boundary detection)

**riskdiff uniquely provides boundary detection for robust inference!**

## Code of Conduct

Please note that the riskdiff project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
