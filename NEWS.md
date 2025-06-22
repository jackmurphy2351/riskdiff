# riskdiff 0.2.0

## Major New Features: Causal Inference with IPTW

### Inverse Probability of Treatment Weighting (IPTW)

- **`calc_iptw_weights()`**: Calculate propensity scores and IPTW weights with multiple methods
  - Support for logistic, probit, and complementary log-log propensity score models
  - ATE, ATT, and ATC weight calculation
  - Stabilized weights to reduce variance
  - Automatic weight trimming for extreme values
  - Comprehensive diagnostics including balance assessment

- **`calc_risk_diff_iptw()`**: Estimate causal risk differences using IPTW
  - Direct estimation of average treatment effects (ATE)
  - Average treatment effect on treated (ATT) and controls (ATC)
  - Bootstrap confidence intervals available
  - Robust standard errors accounting for propensity score estimation

### Diagnostic and Visualization Tools

- **`check_iptw_assumptions()`**: Comprehensive assumption checking
  - Positivity assessment (propensity score overlap)
  - Covariate balance evaluation
  - Weight distribution diagnostics
  - Automated recommendations for common issues

- **`create_balance_plots()`**: Visualize covariate balance and propensity scores
  - Love plots showing standardized differences before/after weighting
  - Propensity score distribution plots by treatment group
  - ggplot2 integration with fallback to base R plots

### Enhanced S3 Methods

- **`print.iptw_result()`**: Formatted output for IPTW weight objects
- **`print.riskdiff_iptw_result()`**: Clean display of causal effect estimates  
- **`summary.riskdiff_iptw_result()`**: Comprehensive analysis summary with interpretation guidance

## Documentation and Examples

### New Vignette
- **"Causal Inference with IPTW in riskdiff"**: Comprehensive tutorial covering:
  - When and how to use IPTW methods
  - Different causal estimands (ATE/ATT/ATC)
  - Assumption checking and diagnostics
  - Troubleshooting common issues
  - Best practices for reporting
  - Comparison with traditional regression methods

### Enhanced Examples
- All IPTW functions include realistic examples using the birth weight dataset
- Examples demonstrate different weight types, propensity score methods, and diagnostic procedures
- Code examples show both basic usage and advanced options

## Statistical Foundations

The IPTW implementation is grounded in established causal inference literature:

- **Austin (2011)** for propensity score methods and balance assessment
- **Hernán & Robins (2020)** for modern causal inference framework  
- **Robins et al. (2000)** for marginal structural models and stabilized weights
- **Crump et al. (2009)** for weight trimming approaches

## Testing and Quality Assurance

### Comprehensive Test Suite
- 25+ new tests covering all IPTW functionality
- Edge case handling (small samples, missing data, extreme weights)
- Input validation and error handling
- Comparison tests between different weight types and methods
- Bootstrap confidence interval validation

### Code Quality
- Full compliance with R package standards
- Comprehensive input validation with informative error messages
- Robust handling of convergence issues and edge cases
- Memory-efficient implementation suitable for large datasets

## Technical Improvements

### Performance Optimizations
- Efficient weight calculation algorithms
- Optimized balance assessment for large numbers of covariates
- Minimal memory footprint for bootstrap procedures

### Dependencies
- Added ggplot2 to Suggests for enhanced visualizations
- Maintained lightweight dependency structure
- Graceful degradation when optional packages unavailable

## Backward Compatibility

- All existing `calc_risk_diff()` functionality preserved
- No breaking changes to existing API
- Existing code will continue to work without modification

## Migration Guide

For users wanting to adopt IPTW methods:

```r
# Traditional regression adjustment
rd_old <- calc_risk_diff(data, "outcome", "treatment", adjust_vars = covars)

# New IPTW approach for causal inference
rd_new <- calc_risk_diff_iptw(data, "outcome", "treatment", covariates = covars)
```

The IPTW approach provides stronger causal interpretation under appropriate assumptions, while traditional regression remains appropriate for descriptive or predictive analyses.

## Future Development

This release establishes the foundation for advanced causal inference methods:

- Planned features for v0.3.0 include doubly robust estimation
- Integration with machine learning approaches for propensity score estimation
- Support for time-varying treatments and marginal structural models
- Enhanced sensitivity analysis tools

## Acknowledgments

IPTW implementation developed with careful attention to methodological rigor and practical usability. Special thanks to the causal inference community for establishing the theoretical foundations that make these methods possible.

---

*For detailed examples and methodology, see the new vignette: `vignette("iptw-analysis", package = "riskdiff")`*

## New Features

### Boundary Detection Functions
- Added boundary detection capabilities for identity link GLMs
- Enhanced convergence diagnostics
- Improved parameter space monitoring

### Updated Dataset
- **BREAKING CHANGE**: Replaced `birthweight` dataset with `cachar_sample`
- `cachar_sample`: Synthetic cancer risk factor study data inspired by Northeast India epidemiological patterns
- Features authentic tobacco and areca nut use relationships
- More comprehensive variables for complex examples (11 variables vs 8 in birthweight)
- Higher outcome prevalence better suited for risk difference analysis
- Completely synthetic data eliminates privacy/ethical concerns

### Enhancements
- Improved starting value calculation for identity link models
- Enhanced documentation with authentic examples
- Updated all examples and tests to use new dataset

### Breaking Changes
- The `birthweight` dataset has been removed
- All examples now use `cachar_sample` with different variable names:
  - `low_birthweight` → `abnormal_screen`
  - `smoking` exposure remains available, plus new exposures: `areca_nut`, `tobacco_chewing`
  - Updated stratification variables: `race` → `sex`, `residence`, `age_group`

# riskdiff 0.1.0

## Initial CRAN Submission

### New Features

- `calc_risk_diff()`: Main function for calculating risk differences with robust model fitting
- `format_risk_diff()`: Format results for publication-ready display
- `create_rd_table()`: Create formatted tables using kableExtra
- `create_simple_table()`: Create simple text-based tables without dependencies
- Support for multiple link functions (identity, log, logit) with automatic fallback
- Stratified analysis with support for multiple stratification variables
- Robust handling of model convergence issues
- Comprehensive input validation and error handling

### Datasets

- `birthweight`: Simulated birth weight study data for examples and testing

### Documentation

- Comprehensive package documentation with examples
- Complete function documentation with realistic examples
- README with quick start guide and methodology overview

### Testing

- Comprehensive test suite with >90% coverage
- Tests for edge cases, error conditions, and different data scenarios
- Automated testing via GitHub Actions

## Development Notes

This package emerged from research needs in cancer epidemiology, specifically methods developed for calculating prevalence differences in cross-sectional cancer screening studies. The robust model fitting approaches were developed to handle common convergence issues encountered when using identity link functions with binary outcomes.

The synthetic `cachar_sample` dataset reflects authentic epidemiological patterns from Northeast India, including the region's distinctive tobacco and areca nut consumption patterns, while avoiding any ethical concerns through complete data synthesis.

### Key Methodological Contributions

- Addresses common convergence failures in identity link binomial GLMs
- Provides automatic fallback strategies across multiple link functions  
- Implements robust starting value calculation methods
- Supports complex stratification and adjustment scenarios
- Emphasizes absolute risk measures for public health decision-making

### Dataset Design Philosophy

The synthetic dataset was designed to:
- Reflect authentic cultural and epidemiological patterns
- Provide realistic examples for methodological demonstration
- Avoid privacy and ethical concerns through complete synthesis
- Support complex analytical scenarios (multiple exposures, stratification)
- Represent understudied populations (Northeast India tobacco patterns)
