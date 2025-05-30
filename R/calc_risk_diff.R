#' Calculate Risk Differences with Robust Model Fitting
#'
#' @description
#' Calculates risk differences (or prevalence differences for cross-sectional data)
#' using generalized linear models with identity, log, or logit links. Includes
#' support for stratification, adjustment variables, and robust model fitting.
#'
#' The function addresses common convergence issues with identity link binomial
#' GLMs by implementing a fallback strategy across multiple link functions,
#' similar to approaches described in Donoghoe & Marschner (2018) for relative
#' risk regression.
#'
#' @param data A data frame containing all necessary variables
#' @param outcome Character string naming the binary outcome variable (must be 0/1 or logical)
#' @param exposure Character string naming the exposure variable of interest
#' @param adjust_vars Character vector of variables to adjust for (default: NULL)
#' @param strata Character vector of stratification variables (default: NULL)
#' @param link Character string specifying link function: "auto", "identity", "log", or "logit" (default: "auto")
#' @param alpha Significance level for confidence intervals (default: 0.05)
#' @param verbose Logical indicating whether to print diagnostic messages (default: FALSE)
#'
#' @return
#' A tibble of class "riskdiff_result" containing the following columns:
#' \describe{
#'   \item{exposure_var}{Character. Name of exposure variable analyzed}
#'   \item{rd}{Numeric. Risk difference estimate (proportion scale, e.g. 0.05 = 5 percentage points)}
#'   \item{ci_lower}{Numeric. Lower bound of confidence interval}
#'   \item{ci_upper}{Numeric. Upper bound of confidence interval}
#'   \item{p_value}{Numeric. P-value for test of null hypothesis (risk difference = 0)}
#'   \item{model_type}{Character. Link function successfully used ("identity", "log", "logit", or error type)}
#'   \item{n_obs}{Integer. Number of observations used in analysis}
#'   \item{...}{Additional columns for stratification variables if specified}
#' }
#'
#' The returned object has attributes including the original function call and
#' alpha level used. Risk differences are on the probability scale where 0.05
#' represents a 5 percentage point difference.
#'
#' @details
#' ## Statistical Approach
#'
#' The function attempts to fit models in the following order:
#' 1. **Identity link**: Directly estimates risk differences using a binomial GLM with identity link
#' 2. **Log link**: Estimates relative risks, then transforms to risk differences via prediction
#' 3. **Logit link**: Estimates odds ratios, then transforms to risk differences via prediction
#'
#' ## Convergence Strategy
#'
#' Identity link models for binary outcomes often fail to converge due to
#' parameter constraints (fitted probabilities must be \\le{} 1). This package
#' implements robust starting values and automatic fallback to alternative
#' link functions when convergence fails, ensuring reliable estimation.
#'
#' ## Risk Difference Interpretation
#'
#' Risk differences represent absolute changes in probability. A risk difference
#' of 0.05 means the exposed group has a 5 percentage point higher risk than
#' the unexposed group. This is often more interpretable than relative measures
#' (risk ratios, odds ratios) for public health decision-making.
#'
#' @references
#' Donoghoe MW, Marschner IC (2018). "logbin: An R Package for Relative Risk
#' Regression Using the Log-Binomial Model." Journal of Statistical Software,
#' 86(9), 1-22. doi:10.18637/jss.v086.i09
#'
#' Rothman KJ, Greenland S, Lash TL (2008). Modern Epidemiology, 3rd edition.
#' Lippincott Williams & Wilkins.
#'
#' @examples
#' # Simple risk difference
#' data(birthweight)
#' rd_simple <- calc_risk_diff(
#'   data = birthweight,
#'   outcome = "low_birthweight",
#'   exposure = "smoking"
#' )
#' print(rd_simple)
#'
#' # Age-adjusted risk difference
#' rd_adjusted <- calc_risk_diff(
#'   data = birthweight,
#'   outcome = "low_birthweight",
#'   exposure = "smoking",
#'   adjust_vars = "maternal_age"
#' )
#' print(rd_adjusted)
#'
#' # Stratified analysis
#' rd_stratified <- calc_risk_diff(
#'   data = birthweight,
#'   outcome = "low_birthweight",
#'   exposure = "smoking",
#'   strata = "race"
#' )
#' print(rd_stratified)
#'
#' @export
calc_risk_diff <- function(data,
                           outcome,
                           exposure,
                           adjust_vars = NULL,
                           strata = NULL,
                           link = "auto",
                           alpha = 0.05,
                           verbose = FALSE) {

  # Input validation
  .validate_inputs(data, outcome, exposure, adjust_vars, strata, link, alpha)

  # Prepare data
  data_clean <- .prepare_data(data, outcome, exposure, adjust_vars, strata)

  # Handle stratification
  if (is.null(strata)) {
    groups <- list(data_clean)
    group_labels <- list(NULL)
  } else {
    data_grouped <- data_clean %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(strata)))
    groups <- dplyr::group_split(data_grouped)
    group_labels <- data_grouped %>%
      dplyr::group_keys() %>%
      as.list() %>%
      purrr::transpose()
  }

  # Analyze each stratum
  results <- purrr::map2_dfr(groups, group_labels, function(group_data, group_label) {
    .analyze_stratum(
      data = group_data,
      outcome = outcome,
      exposure = exposure,
      adjust_vars = adjust_vars,
      strata = strata,
      group_label = group_label,
      link = link,
      alpha = alpha,
      verbose = verbose
    )
  })

  # Add metadata as attributes
  attr(results, "call") <- match.call()
  attr(results, "alpha") <- alpha
  class(results) <- c("riskdiff_result", class(results))

  return(results)
}

#' Format Risk Difference Results for Display
#'
#' @description
#' Formats numerical values in risk difference results for presentation,
#' with appropriate percentage formatting and rounding.
#'
#' @param results Results tibble from calc_risk_diff()
#' @param digits Number of decimal places for percentages (default: 2)
#' @param p_accuracy Accuracy for p-values (default: 0.001)
#'
#' @return Tibble with additional formatted columns
#'
#' @examples
#' data(birthweight)
#' results <- calc_risk_diff(birthweight, "low_birthweight", "smoking")
#' formatted <- format_risk_diff(results)
#' print(formatted)
#'
#' @export
format_risk_diff <- function(results, digits = 2, p_accuracy = 0.001) {

  if (!inherits(results, "riskdiff_result")) {
    warning("Input does not appear to be from calc_risk_diff(). Proceeding anyway.")
  }

  # Use base R formatting to avoid scales issues
  results$rd_formatted <- sprintf(paste0("%.", digits, "f%%"), results$rd * 100)
  results$ci_formatted <- sprintf(
    paste0("(%.", digits, "f%%, %.", digits, "f%%)"),
    results$ci_lower * 100,
    results$ci_upper * 100
  )
  results$p_value_formatted <- ifelse(
    results$p_value < p_accuracy,
    paste0("<", p_accuracy),
    sprintf("%.3f", results$p_value)
  )

  return(results)
}

#' Print method for riskdiff_result objects
#'
#' @description
#' Prints risk difference results in a formatted, readable way showing
#' key statistics including risk differences, confidence intervals, and
#' model types used.
#'
#' @param x A riskdiff_result object from calc_risk_diff()
#' @param ... Additional arguments passed to print methods
#'
#' @return
#' Invisibly returns the original riskdiff_result object (x). Called primarily
#' for its side effect of printing formatted results to the console.
#'
#' @examples
#' data(birthweight)
#' result <- calc_risk_diff(birthweight, "low_birthweight", "smoking")
#' print(result)
#'
#' @export
print.riskdiff_result <- function(x, ...) {
  cat("Risk Difference Analysis Results\n")
  cat("================================\n\n")

  alpha <- attr(x, "alpha") %||% 0.05
  cat("Confidence level:", scales::percent(1 - alpha), "\n")
  cat("Number of comparisons:", nrow(x), "\n\n")

  # Create simple display without recursive calls
  display_data <- data.frame(
    Exposure = x$exposure_var,
    `Risk Difference` = sprintf("%.2f%%", x$rd * 100),
    `95% CI` = sprintf("(%.2f%%, %.2f%%)", x$ci_lower * 100, x$ci_upper * 100),
    `P-value` = ifelse(x$p_value < 0.001, "<0.001", sprintf("%.3f", x$p_value)),
    Model = x$model_type,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  print(display_data, row.names = FALSE, ...)

  invisible(x)
}
