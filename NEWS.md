# riskdiff 0.3.0

## Major New Features: Number Needed to Treat (NNT)

### 🔢 Number Needed to Treat Integration

- **`calc_risk_diff(..., nnt = TRUE)`**: Direct NNT calculation with robust confidence intervals
- **`calc_risk_diff_iptw(..., nnt = TRUE)`**: Causal NNT estimates using IPTW methodology
- **Automatic transformation**: Seamless conversion from risk differences using reciprocal method
- **Boundary handling**: Proper management of undefined NNT when risk differences ≈ 0
- **Statistical validity**: Confidence intervals transformed using delta method principles

### 📊 Enhanced Output and Formatting

- **`print.nnt_result()`**: Specialized print method for standard NNT results
- **`print.nnt_iptw_result()`**: Specialized print method for causal NNT results  
- **Enhanced `format_risk_diff()`**: Support for NNT formatting with appropriate precision
- **`summary.riskdiff_iptw_result()`**: Updated to handle both RD and NNT presentations
- **Interpretation guidance**: Automated contextual interpretation in print outputs

### 🧠 Clinical Decision Support

- **Actionable metrics**: NNT provides intuitive "number of patients to treat" interpretation
- **Causal NNT**: IPTW-based NNT represents true causal effects under exchangeability
- **Multiple estimands**: ATE, ATT, and ATC all support NNT transformation for targeted interpretation
- **Public health focus**: Designed specifically for intervention planning and resource allocation

## Enhanced Statistical Methods

### NNT-Specific Methodological Improvements

- **Threshold management**: Automatic handling of clinically meaningless small effect sizes
- **Infinite value handling**: Graceful management of undefined NNT cases
- **Bootstrap compatibility**: NNT transformation works seamlessly with bootstrap confidence intervals
- **Boundary detection integration**: Existing boundary detection system applies to NNT calculations

### Documentation and Examples

- **Comprehensive examples**: Both standard and IPTW NNT examples in all relevant functions
- **Vignette updates**: Integration into existing analysis workflows
- **Clinical interpretation**: Guidance on when NNT is more appropriate than risk differences
- **Causal inference context**: Clear explanation of NNT assumptions under IPTW

## API Consistency

### Backward Compatibility
- **Zero breaking changes**: All existing code continues to work unchanged
- **Optional parameter**: `nnt = FALSE` by default maintains existing behavior
- **Consistent interface**: Same parameter pattern across `calc_risk_diff()` and `calc_risk_diff_iptw()`
- **Preserved attributes**: All existing result attributes maintained

### Quality Assurance
- **Comprehensive testing**: Full test suite for NNT functionality across all scenarios
- **Edge case handling**: Robust behavior with extreme values, small samples, and boundary cases
- **Statistical validation**: Confidence interval transformations verified against theoretical expectations

---

# riskdiff 0.2.1

## Major New Features: Causal Inference with IPTW

### 🔬 Inverse Probability of Treatment Weighting (IPTW)

- **`calc_risk_diff_iptw()`**: Complete implementation for causal effect estimation in observational studies
- **Propensity Score Modeling**: Logistic regression with comprehensive diagnostics and balance assessment
- **Multiple Causal Estimands**: 
  - **ATE** (Average Treatment Effect): Population-level causal effects
  - **ATT** (Average Treatment Effect on Treated): Effects among those who received treatment
  - **ATC** (Average Treatment Effect on Controls): Effects among those who did not receive treatment
- **Weight Stabilization**: Stabilized IPTW weights with optional trimming for extreme values
- **Robust Inference**: Bootstrap and sandwich estimator confidence intervals accounting for propensity score uncertainty

### 🧪 Causal Inference Diagnostics

- **Covariate Balance Assessment**: Standardized mean differences before and after weighting
- **Effective Sample Size Calculation**: Proper accounting for weight-induced variance inflation  
- **Propensity Score Overlap**: Visual and numerical assessment of positivity assumption
- **Weight Distribution Analysis**: Comprehensive diagnostics for extreme weights
- **Balance Tables**: Publication-ready covariate balance summaries

## Enhanced Statistical Methods

### Boundary Detection System

- **Comprehensive Detection**: Automatic identification of statistical boundary conditions including:
  - Upper bound issues (fitted probabilities near 1)
  - Lower bound issues (fitted probabilities near 0)
  - Separation and quasi-separation scenarios
  - Integration with IPTW for robust causal estimation
- **Enhanced Confidence Intervals**: Robust interval estimation methods for boundary cases using profile likelihood
- **Automatic Fallback**: Intelligent model selection with detailed convergence diagnostics

### Improved Core Functionality

- **Enhanced Missing Data Handling**: More sophisticated approaches to incomplete covariate data
- **Improved Convergence Diagnostics**: Better detection and handling of model fitting challenges
- **Enhanced Validation**: More comprehensive input validation and informative error messages
- **Performance Optimization**: Improved computational efficiency for large epidemiological datasets

## Testing and Quality Assurance

### Comprehensive Test Suite (322+ Tests, Zero Failures)

- **IPTW-Specific Testing**: Extensive validation of causal inference methods including:
  - Propensity score model fitting under various scenarios
  - Weight calculation and stabilization accuracy
  - Covariate balance assessment correctness
  - Bootstrap confidence interval coverage properties
- **Boundary Condition Stress Testing**: Rigorous validation of challenging statistical scenarios
- **Missing Data Torture Tests**: Extensive validation across multiple missing data patterns
- **Real-World Dataset Integration**: Full compatibility testing with complex epidemiological data
- **Performance Testing**: Validation with large datasets and complex stratification

### Statistical Validation

- **Simulation Studies**: Validated against known data-generating processes with various confounding patterns
- **Literature Benchmarks**: Compared against established causal inference methods and results
- **Balance Assessment**: Comprehensive validation of covariate balance evaluation methods
- **Bootstrap Coverage**: Empirical validation of confidence interval coverage properties

## Documentation and Examples

### Enhanced Documentation

- **Causal Inference Methodology**: Detailed explanation of IPTW theory and implementation
- **Practical Examples**: Real-world applications using `cachar_sample` dataset
- **Best Practices Guide**: Recommendations for observational study analysis
- **Diagnostic Interpretation**: How to assess and interpret covariate balance and weight diagnostics

### Updated Examples

- **Observational Studies**: Complete workflow from confounding assessment to causal effect estimation
- **RCT Analysis**: Baseline prognostic factor adjustment in randomized trials
- **Sensitivity Analysis**: Approaches for assessing robustness to unmeasured confounding
- **Publication-Ready Output**: Formatted tables and visualizations for research dissemination

## Dataset Integration

### Enhanced cachar_sample Dataset

- **Full IPTW Compatibility**: Dataset optimized for demonstrating causal inference methods
- **Realistic Confounding Patterns**: Authentic relationships between covariates, treatments, and outcomes
- **Missing Data Scenarios**: Representative patterns for testing missing data handling
- **Multiple Treatment Variables**: Support for various causal questions and estimands

## API and Interface

### New Functions

- **`calc_risk_diff_iptw()`**: Main IPTW causal effect estimation function
- **`calc_iptw_weights()`**: Propensity score estimation and weight calculation
- **`assess_balance()`**: Covariate balance evaluation before and after weighting
- **Enhanced print methods**: Specialized output formatting for causal inference results

### Enhanced Existing Functions

- **`calc_risk_diff()`**: Improved boundary detection and convergence handling
- **`format_risk_diff()`**: Enhanced formatting with boundary condition information
- **`create_rd_table()`**: Support for IPTW results and causal inference formatting

## Statistical Foundation

### Literature Integration

All causal inference methods implemented according to established best practices:

- **Hernán & Robins (2020)**: Modern causal inference methodology
- **Rosenbaum & Rubin (1983)**: Propensity score theory and application
- **Austin (2011)**: IPTW implementation best practices  
- **Lunceford & Davidian (2004)**: Estimation methods for causal effects
- **Cole & Hernán (2008)**: Constructing inverse probability weights

### Methodological Rigor

- **Assumption Checking**: Tools for assessing key causal inference assumptions
- **Sensitivity Analysis**: Framework for evaluating robustness to violations
- **Effect Modification**: Support for subgroup analyses with proper causal interpretation
- **Publication Standards**: Output formatted according to epidemiological reporting guidelines

## Performance and Scalability

### Computational Efficiency

- **Large Dataset Support**: Optimized for epidemiological cohorts with 10,000+ observations
- **Memory Management**: Efficient handling of weight calculations and bootstrap procedures
- **Parallel Processing**: Support for multi-core bootstrap confidence interval calculation
- **Progress Tracking**: User feedback for long-running causal inference procedures

### Numerical Stability

- **Robust Weight Calculation**: Stable computation even with extreme propensity scores
- **Overflow Protection**: Safe handling of very large or small weights
- **Convergence Monitoring**: Comprehensive diagnostics for propensity score model fitting
- **Boundary Integration**: Seamless handling of boundary conditions in causal estimation

---

# riskdiff 0.1.0

## Initial CRAN Release

## Development Philosophy

The riskdiff package bridges the gap between traditional epidemiological methods and modern causal inference, making sophisticated statistical techniques accessible to public health researchers worldwide. Version 0.2.0 aims to democratise causal inference for global health research.
