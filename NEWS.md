# riskdiff 0.2.0

## Major New Features

### 🎯 Boundary Detection and Robust Inference

-   **Explicit boundary detection** for GLM edge cases where the maximum likelihood estimate lies at the boundary of the parameter space
-   **Automatic identification** of upper bounds (fitted probabilities near 1), lower bounds (fitted probabilities near 0), and separation issues
-   **Robust confidence intervals** with automatic method selection for boundary cases
-   **Enhanced diagnostic output** with boundary warnings and confidence interval method reporting

### 📊 Enhanced User Experience

-   **New `boundary_method` parameter** in `calc_risk_diff()` for controlling confidence interval methods in boundary cases
-   **Improved print method** with boundary case detection and detailed warnings
-   **Verbose output option** showing model convergence details and boundary detection process
-   **Automatic fallback strategies** across identity, log, and logit link functions with boundary-aware selection

## Statistical Improvements

-   **Methodologically rigorous** handling of boundary cases based on Marschner & Gillett (2012) and Donoghoe & Marschner (2018)
-   **Conservative confidence intervals** for boundary cases where standard asymptotic theory may not apply
-   **Transparent reporting** of which inference methods were used
-   **Graceful degradation** when models encounter convergence issues

## New Result Columns

Risk difference results now include: - `on_boundary`: Logical indicator of boundary cases - `boundary_type`: Type of boundary detected ("upper_bound", "lower_bound", "separation", "none") - `boundary_warning`: Detailed warning messages for boundary cases - `ci_method`: Confidence interval method used ("wald", "wald_conservative", "profile")

## Bug Fixes and Improvements

-   **Resolved GLM convergence issues** with enhanced starting value selection
-   **Improved error handling** for edge cases and small sample sizes
-   **Enhanced input validation** and user-friendly error messages
-   **Better handling of missing data** in stratified analyses

## Examples

``` r
# Boundary detection in action
data(birthweight)
result <- calc_risk_diff(birthweight, "low_birthweight", "smoking", verbose = TRUE)

# Check for boundary cases
if (any(result$on_boundary)) {
  cat("Boundary cases detected - using robust inference methods\n")
}

# Force specific boundary handling method
result_conservative <- calc_risk_diff(
  birthweight, 
  "low_birthweight", 
  "smoking",
  boundary_method = "auto"
)
```

## Under the Hood

-   **Comprehensive test suite** with 116+ tests covering boundary cases and edge conditions
-   **Clean R CMD check** with zero errors, warnings, or notes
-   **Full backward compatibility** - existing code continues to work unchanged
-   **Robust internal architecture** with modular boundary detection functions

------------------------------------------------------------------------

# riskdiff 0.1.0

## Initial CRAN Release

### New Features

-   `calc_risk_diff()`: Main function for calculating risk differences with robust model fitting
-   `format_risk_diff()`: Format results for publication-ready display\
-   `create_rd_table()`: Create formatted tables using kableExtra
-   `create_simple_table()`: Create simple text-based tables without dependencies
-   Support for multiple link functions (identity, log, logit) with automatic fallback
-   Stratified analysis with support for multiple stratification variables
-   Robust handling of model convergence issues
-   Comprehensive input validation and error handling

### Datasets

-   `birthweight`: Simulated birth weight study data for examples and testing

### Documentation

-   Comprehensive package documentation with examples
-   Vignette: "Introduction to riskdiff"
-   Complete function documentation with realistic examples
-   README with quick start guide and methodology overview

### Testing

-   Comprehensive test suite with \>90% coverage
-   Tests for edge cases, error conditions, and different data scenarios
-   Automated testing via GitHub Actions

## Development Notes

This package emerged from research conducted at the Cachar Cancer Hospital and Research Centre, specifically methods developed for calculating prevalence differences in cross-sectional cancer screening studies. The robust model fitting approaches were developed to handle common convergence issues encountered when using identity link functions with binary outcomes.

The package generalizes these methods to work with both prevalence differences (cross-sectional studies) and risk differences (prospective studies), hence the generic name "riskdiff".
