## Resubmission

This is a resubmission addressing the feedback received on 2025-05-29.

### Changes made in response to CRAN feedback:

1. **Added references to DESCRIPTION**: Added proper citations for methodological foundations:
   - Donoghoe & Marschner (2018) <doi:10.18637/jss.v086.i09> for robust GLM fitting methods
   - Rothman, Greenland & Lash (2008, ISBN:9780781755641) for epidemiological methods

2. **Added missing \value documentation**: 
   - Added comprehensive \value section to print.riskdiff_result.Rd explaining return structure and side effects

3. **Fixed \dontrun{} usage**:
   - Replaced \dontrun{} with \donttest{} in create_rd_table.Rd as the examples are executable but require optional dependency (kableExtra)

4. **Enhanced documentation**:
   - Expanded statistical methodology descriptions
   - Added detailed return value documentation across functions
   - Improved examples with clearer explanations

## Test environments

* local R installation: R 4.5.0 on macOS  
* GitHub Actions (ubuntu-latest): R-release, R-devel
* GitHub Actions (windows-latest): R-release  
* GitHub Actions (macOS-latest): R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

This is a new package, so there are no downstream dependencies to check.

## Additional notes

* All examples run successfully in < 5 seconds
* Package builds cleanly on all test platforms
* Test coverage remains > 90%
* No breaking changes to API
