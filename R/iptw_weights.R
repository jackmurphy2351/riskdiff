#' Calculate Propensity Scores and IPTW Weights
#'
#' @description
#' Calculates propensity scores and inverse probability of treatment weights
#' for use in standardized risk difference estimation. Implements multiple
#' approaches for weight calculation and includes diagnostic tools.
#'
#' @param data A data frame containing treatment and covariate data
#' @param treatment Character string naming the binary treatment variable
#' @param covariates Character vector of covariate names for propensity score model
#' @param method Method for propensity score estimation: "logistic" (default), "probit", or "cloglog"
#' @param weight_type Type of weights to calculate: "ATE" (average treatment effect, default),
#'   "ATT" (average treatment effect on treated), "ATC" (average treatment effect on controls)
#' @param stabilize Logical indicating whether to use stabilized weights (default: TRUE)
#' @param trim_weights Logical indicating whether to trim extreme weights (default: TRUE)
#' @param trim_quantiles Vector of length 2 specifying quantiles for weight trimming (default: c(0.01, 0.99))
#' @param verbose Logical indicating whether to print diagnostic information (default: FALSE)
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{data}{Original data with added propensity scores and weights}
#'   \item{ps_model}{Fitted propensity score model}
#'   \item{weights}{Vector of calculated weights}
#'   \item{ps}{Vector of propensity scores}
#'   \item{diagnostics}{List of diagnostic information including balance statistics}
#'   \item{method}{Method used for propensity score estimation}
#'   \item{weight_type}{Type of weights calculated}
#' }
#'
#' @details
#' ## Propensity Score Estimation
#'
#' The function fits a model predicting treatment assignment from covariates:
#' - **Logistic regression**: Standard approach, assumes logit link
#' - **Probit regression**: Uses probit link, may be more robust with extreme probabilities
#' - **Complementary log-log**: Useful when treatment is rare
#'
#' ## Weight Types
#'
#' - **ATE weights**: 1/pi(X) for treated, 1/(1-pi(X)) for controls
#' - **ATT weights**: 1 for treated, pi(X)/(1-pi(X)) for controls
#' - **ATC weights**: (1-pi(X))/pi(X) for treated, 1 for controls
#'
#' Where pi(X) is the propensity score (probability of treatment given X).
#'
#' ## Stabilized Weights
#'
#' When stabilize=TRUE, weights are multiplied by marginal treatment probabilities
#' to reduce variance while maintaining unbiasedness (Robins et al., 2000).
#'
#' ## Weight Trimming
#'
#' Extreme weights can cause instability. Trimming replaces weights outside
#' specified quantiles with the quantile values (Crump et al., 2009).
#'
#' @references
#' Austin PC (2011). "An Introduction to Propensity Score Methods for Reducing
#' the Effects of Confounding in Observational Studies." Multivariate Behavioral
#' Research, 46(3), 399-424. doi:10.1080/00273171.2011.568786
#'
#' Crump RK, Hotz VJ, Imbens GW, Mitnik OA (2009). "Dealing with Limited Overlap
#' in Estimation of Average Treatment Effects." Biometrika, 96(1), 187-199.
#'
#' Hernan MA, Robins JM (2020). Causal Inference: What If. Boca Raton: Chapman & Hall/CRC.
#'
#' Robins JM, Hernan MA, Brumback B (2000). "Marginal Structural Models and Causal
#' Inference in Epidemiology." Epidemiology, 11(5), 550-560.
#'
#' @examples
#' data(cachar_sample)
#'
#' # Calculate ATE weights for areca nut use
#' iptw_result <- calc_iptw_weights(
#'   data = cachar_sample,
#'   treatment = "areca_nut",
#'   covariates = c("age", "sex", "residence", "smoking"),
#'   weight_type = "ATE"
#' )
#'
#' # Check balance
#' print(iptw_result$diagnostics$balance_table)
#'
#' # Calculate ATT weights (effect on the treated)
#' iptw_att <- calc_iptw_weights(
#'   data = cachar_sample,
#'   treatment = "tobacco_chewing",
#'   covariates = c("age", "sex", "residence", "areca_nut"),
#'   weight_type = "ATT"
#' )
#'
#' @export
calc_iptw_weights <- function(data,
                              treatment,
                              covariates,
                              method = "logistic",
                              weight_type = "ATE",
                              stabilize = TRUE,
                              trim_weights = TRUE,
                              trim_quantiles = c(0.01, 0.99),
                              verbose = FALSE) {

  # Input validation
  .validate_iptw_inputs(data, treatment, covariates, method, weight_type, trim_quantiles)

  # Prepare data
  data_clean <- .prepare_iptw_data(data, treatment, covariates)

  if (verbose) {
    cat("Fitting propensity score model using", method, "regression\n")
    cat("Sample size:", nrow(data_clean), "\n")
    cat("Treatment prevalence:", round(mean(data_clean[[treatment]] == levels(data_clean[[treatment]])[2]), 3), "\n")
  }

  # Fit propensity score model
  ps_model <- .fit_propensity_model(data_clean, treatment, covariates, method)

  # Calculate propensity scores
  ps <- stats::predict(ps_model, type = "response")

  # Calculate weights
  weights <- .calculate_iptw_weights(data_clean[[treatment]], ps, weight_type, stabilize)

  # Trim weights if requested
  if (trim_weights) {
    weights <- .trim_weights(weights, trim_quantiles, verbose)
  }

  # Add weights and propensity scores to data
  data_with_weights <- data_clean
  data_with_weights$ps <- ps
  data_with_weights$iptw_weights <- weights

  # Calculate diagnostics
  diagnostics <- .calculate_iptw_diagnostics(data_with_weights, treatment, covariates, weights, ps)

  if (verbose) {
    cat("Weight summary:\n")
    print(summary(weights))
    cat("\nEffective sample size:", round(diagnostics$effective_n, 1), "\n")
    cat("Maximum standardized difference:", round(max(abs(diagnostics$balance_table$std_diff_weighted), na.rm = TRUE), 3), "\n")
  }

  # Return comprehensive results
  result <- list(
    data = data_with_weights,
    ps_model = ps_model,
    weights = weights,
    ps = ps,
    diagnostics = diagnostics,
    method = method,
    weight_type = weight_type,
    call = match.call()
  )

  class(result) <- "iptw_result"
  return(result)
}
