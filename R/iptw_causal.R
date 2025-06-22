#' Calculate Standardized Risk Differences Using IPTW
#'
#' @description
#' Calculates standardized risk differences using inverse probability of treatment
#' weighting. This approach estimates causal effects under the assumption of no
#' unmeasured confounding by creating a pseudo-population where treatment assignment
#' is independent of measured confounders.
#'
#' @param data A data frame containing outcome, treatment, and covariate data
#' @param outcome Character string naming the binary outcome variable
#' @param treatment Character string naming the binary treatment variable
#' @param covariates Character vector of covariate names for propensity score model
#' @param iptw_weights Optional vector of pre-calculated IPTW weights
#' @param weight_type Type of weights if calculating: "ATE", "ATT", or "ATC" (default: "ATE")
#' @param ps_method Method for propensity score estimation (default: "logistic")
#' @param stabilize Whether to use stabilized weights (default: TRUE)
#' @param trim_weights Whether to trim extreme weights (default: TRUE)
#' @param alpha Significance level for confidence intervals (default: 0.05)
#' @param bootstrap_ci Whether to use bootstrap confidence intervals (default: FALSE)
#' @param boot_n Number of bootstrap replicates if bootstrap_ci=TRUE (default: 1000)
#' @param verbose Whether to print diagnostic information (default: FALSE)
#'
#' @return
#' A tibble of class "riskdiff_iptw_result" containing:
#' \describe{
#'   \item{treatment_var}{Character. Name of treatment variable}
#'   \item{rd_iptw}{Numeric. IPTW-standardized risk difference}
#'   \item{ci_lower}{Numeric. Lower confidence interval bound}
#'   \item{ci_upper}{Numeric. Upper confidence interval bound}
#'   \item{p_value}{Numeric. P-value for test of null hypothesis}
#'   \item{weight_type}{Character. Type of weights used}
#'   \item{effective_n}{Numeric. Effective sample size}
#'   \item{risk_treated}{Numeric. Risk in treated group}
#'   \item{risk_control}{Numeric. Risk in control group}
#' }
#'
#' @details
#' ## Causal Interpretation
#'
#' IPTW estimates causal effects by weighting observations to create balance
#' on measured confounders. The estimand depends on the weight type:
#' - **ATE**: Average treatment effect in the population
#' - **ATT**: Average treatment effect among those who received treatment
#' - **ATC**: Average treatment effect among those who did not receive treatment
#'
#' ## Standard Errors
#'
#' By default, uses robust (sandwich) standard errors that account for propensity
#' score estimation uncertainty. Bootstrap confidence intervals are available
#' as an alternative that may perform better with small samples.
#'
#' ## Assumptions
#'
#' 1. **No unmeasured confounding**: All confounders are measured and included
#' 2. **Positivity**: All subjects have non-zero probability of receiving either treatment
#' 3. **Correct model specification**: Propensity score model is correctly specified
#'
#' @examples
#' data(cachar_sample)
#'
#' # Standard ATE estimation
#' rd_iptw <- calc_risk_diff_iptw(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   treatment = "areca_nut",
#'   covariates = c("age", "sex", "residence", "smoking")
#' )
#' print(rd_iptw)
#'
#' # ATT estimation with bootstrap CI
#' rd_att <- calc_risk_diff_iptw(
#'   data = cachar_sample,
#'   outcome = "head_neck_abnormal",
#'   treatment = "tobacco_chewing",
#'   covariates = c("age", "sex", "residence", "areca_nut"),
#'   weight_type = "ATT",
#'   bootstrap_ci = TRUE,
#'   boot_n = 500
#' )
#' print(rd_att)
#'
#' @export
calc_risk_diff_iptw <- function(data,
                                outcome,
                                treatment,
                                covariates,
                                iptw_weights = NULL,
                                weight_type = "ATE",
                                ps_method = "logistic",
                                stabilize = TRUE,
                                trim_weights = TRUE,
                                alpha = 0.05,
                                bootstrap_ci = FALSE,
                                boot_n = 1000,
                                verbose = FALSE) {

  # Input validation
  .validate_iptw_rd_inputs(data, outcome, treatment, covariates, weight_type, alpha)

  # Calculate IPTW weights if not provided
  if (is.null(iptw_weights)) {
    if (verbose) cat("Calculating IPTW weights...\n")

    iptw_result <- calc_iptw_weights(
      data = data,
      treatment = treatment,
      covariates = covariates,
      method = ps_method,
      weight_type = weight_type,
      stabilize = stabilize,
      trim_weights = trim_weights,
      verbose = verbose
    )

    # IMPORTANT: Add the outcome back to the weighted data!
    # The IPTW data should have the same rows as original (after removing missing values)
    data_weighted <- iptw_result$data

    # Find which rows from original data are in the IPTW result
    # (should be the complete cases for treatment + covariates)
    original_subset <- data[complete.cases(data[c(treatment, covariates)]), ]
    data_weighted[[outcome]] <- original_subset[[outcome]]

    weights <- iptw_result$weights
    effective_n <- iptw_result$diagnostics$effective_n

    if (verbose) {
      cat("Propensity score model diagnostics:\n")
      print(iptw_result$diagnostics$balance_summary)
    }

  } else {
    # Use provided weights
    if (length(iptw_weights) != nrow(data)) {
      stop("Length of iptw_weights must equal number of rows in data", call. = FALSE)
    }

    data_weighted <- data
    data_weighted$iptw_weights <- iptw_weights
    weights <- iptw_weights
    effective_n <- sum(weights)^2 / sum(weights^2)
  }

  # Calculate weighted risk difference
  if (bootstrap_ci) {
    rd_result <- .calc_iptw_rd_bootstrap(
      data_weighted, outcome, treatment, weights, alpha, boot_n, verbose
    )
  } else {
    rd_result <- .calc_iptw_rd_analytic(
      data_weighted, outcome, treatment, weights, alpha, verbose
    )
  }

  # Add metadata
  result <- tibble::tibble(
    treatment_var = treatment,
    rd_iptw = rd_result$rd,
    ci_lower = rd_result$ci_lower,
    ci_upper = rd_result$ci_upper,
    p_value = rd_result$p_value,
    weight_type = weight_type,
    effective_n = effective_n,
    risk_treated = rd_result$risk_treated,
    risk_control = rd_result$risk_control
  )

  # Add attributes
  attr(result, "call") <- match.call()
  attr(result, "alpha") <- alpha
  attr(result, "bootstrap") <- bootstrap_ci
  if (bootstrap_ci) attr(result, "boot_n") <- boot_n

  class(result) <- c("riskdiff_iptw_result", class(result))

  if (verbose) {
    cat("\nIPTW Risk Difference Results:\n")
    cat("Risk difference:", round(rd_result$rd * 100, 2), "%\n")
    cat("95% CI: (", round(rd_result$ci_lower * 100, 2), "%, ",
        round(rd_result$ci_upper * 100, 2), "%)\n", sep = "")
    cat("P-value:", round(rd_result$p_value, 4), "\n")
  }

  return(result)
}

#' Print Method for IPTW Results
#'
#' @param x An iptw_result object
#' @param ... Additional arguments passed to print
#' @export
print.iptw_result <- function(x, ...) {
  cat("Inverse Probability of Treatment Weighting Results\n")
  cat("=================================================\n\n")

  cat("Propensity Score Model:", x$method, "regression\n")
  cat("Weight Type:", x$weight_type, "\n")
  cat("Sample Size:", nrow(x$data), "\n")
  cat("Effective Sample Size:", round(x$diagnostics$effective_n, 1), "\n\n")

  cat("Weight Summary:\n")
  print(summary(x$weights))
  cat("\n")

  cat("Covariate Balance:\n")
  print(x$diagnostics$balance_table[, c("variable", "std_diff_unweighted", "std_diff_weighted")],
        row.names = FALSE)

  invisible(x)
}

#' Print Method for IPTW Risk Difference Results
#'
#' @param x A riskdiff_iptw_result object
#' @param ... Additional arguments passed to print
#' @export
print.riskdiff_iptw_result <- function(x, ...) {
  cat("IPTW-Standardized Risk Difference Results\n")
  cat("=========================================\n\n")

  alpha <- attr(x, "alpha") %||% 0.05
  ci_level <- scales::percent(1 - alpha)

  cat("Treatment:", x$treatment_var, "\n")
  cat("Weight Type:", x$weight_type, "\n")
  cat("Effective Sample Size:", round(x$effective_n, 1), "\n\n")

  cat("Risk in Treated:", scales::percent(x$risk_treated, accuracy = 0.1), "\n")
  cat("Risk in Control:", scales::percent(x$risk_control, accuracy = 0.1), "\n")
  cat("Risk Difference:", scales::percent(x$rd_iptw, accuracy = 0.01), "\n")
  cat(ci_level, "CI: (", scales::percent(x$ci_lower, accuracy = 0.01),
      ", ", scales::percent(x$ci_upper, accuracy = 0.01), ")\n", sep = "")
  cat("P-value:", ifelse(x$p_value < 0.001, "<0.001", sprintf("%.3f", x$p_value)), "\n")

  if (attr(x, "bootstrap") %||% FALSE) {
    cat("\nNote: Confidence intervals based on", attr(x, "boot_n"), "bootstrap replicates\n")
  }

  invisible(x)
}
