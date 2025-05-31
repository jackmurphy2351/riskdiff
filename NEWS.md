# riskdiff 0.2.0

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
