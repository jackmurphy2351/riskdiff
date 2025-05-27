#' Calculate Risk Differences with Robust Model Fitting
#'
#' @description
#' Calculates risk differences (or prevalence differences for cross-sectional data)
#' using generalized linear models with identity, log, or logit links. Includes
#' support for stratification, adjustment variables, and robust model fitting.
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
#' @return A tibble containing:
#' \itemize{
#'   \item exposure_var: Name of exposure variable
#'   \item rd: Risk difference estimate
#'   \item ci_lower: Lower confidence interval
#'   \item ci_upper: Upper confidence interval
#'   \item p_value: P-value for test of no difference
#'   \item model_type: Link function used ("identity", "log", "logit")
#'   \item n_obs: Number of observations
#'   \item Additional columns for stratification variables if specified
#' }
#'
#' @details
#' The function attempts to fit models in the following order:
#' 1. Identity link (gives direct risk differences)
#' 2. Log link (relative risk model, transformed to risk differences)
#' 3. Logit link (odds ratio model, transformed to risk differences)
#'
#' For identity link models, robust starting values are calculated using
#' empirical proportions or logistic model transformations.
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
#' @param x A riskdiff_result object
#' @param ... Additional arguments passed to print
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
