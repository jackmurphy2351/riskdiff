## Resubmission / Version 0.3.0
This is a resubmission addressing the NOTEs found in the preliminary check for version 0.3.0.

## Test environments
* local macOS Tahoe (Version 26.3), R 4.5.2
* win-builder (devel and release)
* GitHub Actions: ubuntu-latest (R-CMD-check)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

1. **Maintainer change**: The maintainer's email address has been updated from 
   'jackmurphy2351@gmail.com' to 'jackdmurphy@protonmail.com'. This is a 
   deliberate change by the author.

2. **Global function definition**: Added `importFrom("stats", "update")` to 
   the NAMESPACE and prefixed the call in `R/utils.R` with `stats::` to 
   resolve the 'no visible global function definition' NOTE.

3. **Possibly misspelled words**: 'NNT' is used in the DESCRIPTION. This is 
   a standard medical abbreviation for 'Number Needed to Treat' and is 
   used correctly in the context of this package.

## Major Changes in 0.3.0:
* Implemented enhanced boundary detection for Risk Difference estimation.
* Added specific diagnostics for 'large_standard_errors', 'large_coefficients', and 'separation'.
* Improved robust Confidence Interval fallbacks (Bootstrap/Wald) when profile likelihood fails.
