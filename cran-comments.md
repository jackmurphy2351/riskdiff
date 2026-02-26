## Resubmission (v0.3.0) - Issues Resolved

This is a minor release focusing on robust boundary detection and numerical stability.

## Test environments
* local macOS Sonoma, R 4.3.2
* win-builder (devel and release)
* Ubuntu 22.04.3 LTS (on GitHub Actions), R 4.3.2

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
* Checking non-standard files/directories: 'CRAN-SUBMISSION'
  (This is an internal submission checklist and can be ignored.)

### Major Changes:
* Implemented an enhanced boundary detection algorithm for Risk Difference estimation.
* Added specific diagnostic flags for:
    * `large_standard_errors`: Detection of inflated variance due to multicollinearity.
    * `large_coefficients`: Detection of Divergent MLEs.
    * `separation`: Detection of perfect or quasi-perfect separation.
* Improved robust Confidence Interval fallbacks (Bootstrap/Wald) when profile likelihood fails.

### Internal Improvements:
* Expanded test suite to 600+ tests with 100% coverage on edge-case boundary detection.
* Fixed numerical stability issues in `.detect_separation` for small-sample oncology data.
