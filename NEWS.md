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
