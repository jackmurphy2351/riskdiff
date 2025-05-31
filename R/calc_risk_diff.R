# Enhanced version of calc_risk_diff with boundary detection

#' Calculate Risk Differences with Robust Model Fitting and Boundary Detection
#'
#' @description
#' Calculates risk differences with enhanced boundary detection and robust
#' confidence interval methods. Version 0.2.0 adds explicit detection of
#' boundary cases where the MLE lies at the edge of the parameter space,
#' providing more reliable inference in these situations.
#'
#' @param data A data frame containing all necessary variables
#' @param outcome Character string naming the binary outcome variable
#' @param exposure Character string naming the exposure variable of interest
#' @param adjust_vars Character vector of variables to adjust for (default: NULL)
#' @param strata Character vector of stratification variables (default: NULL)
#' @param link Character string specifying link function preference
#' @param alpha Significance level for confidence intervals (default: 0.05)
#' @param boundary_method Method for boundary case CIs: "auto", "profile", "bootstrap" (default: "auto")
#' @param verbose Logical indicating whether to print diagnostic messages
#'
#' @return
#' A tibble of class "riskdiff_result" with additional boundary information:
#' \describe{
#'   \item{exposure_var}{Character. Name of exposure variable}
#'   \item{rd}{Numeric. Risk difference estimate}
#'   \item{ci_lower}{Numeric. Lower confidence interval bound}
#'   \item{ci_upper}{Numeric. Upper confidence interval bound}
#'   \item{p_value}{Numeric. P-value for test of RD = 0}
#'   \item{model_type}{Character. Link function used}
#'   \item{n_obs}{Integer. Number of observations}
#'   \item{on_boundary}{Logical. TRUE if MLE is on parameter space boundary}
#'   \item{boundary_type}{Character. Type of boundary: "none", "upper_bound", "lower_bound", "separation"}
#'   \item{boundary_warning}{Character. Warning message for boundary cases (if any)}
#'   \item{ci_method}{Character. Method used for confidence intervals}
#' }
#'
#' @details
#' ## New in Version 0.2.0: Boundary Detection
#'
#' This version adds explicit detection of boundary cases where the maximum
#' likelihood estimate lies at the edge of the valid parameter space. This
#' commonly occurs with:
#'
#' - **Identity link models**: When fitted probabilities approach 0 or 1
#' - **Log link models**: When fitted probabilities approach 1
#' - **Logit link models**: When complete or quasi-separation occurs
#'
#' ## Statistical Theory
#'
#' When the MLE is on the boundary, standard asymptotic theory may not apply:
#' - Wald confidence intervals can be too narrow or asymmetric
#' - Standard errors from the information matrix may be inappropriate
#' - The sampling distribution may not be normal
#'
#' ## Robust Inference Methods
#'
#' For boundary cases, the function implements:
#' - **Profile likelihood intervals** (preferred when computationally feasible)
#' - **Bootstrap confidence intervals** (robust but computationally intensive)
#' - **Modified Wald intervals** with boundary adjustments
#'
#' @references
#' Marschner IC, Gillett AC (2012). Relative Risk Regression: Reliable and
#' Flexible Methods for Log-Binomial Models. Biostatistics, 13(1), 179-192.
#'
#' Venzon DJ, Moolgavkar SH (1988). A Method for Computing Profile-Likelihood-Based
#' Confidence Intervals. Journal of the Royal Statistical Society, 37(1), 87-94.
#'
#' Donoghoe MW, Marschner IC (2018). logbin: An R Package for Relative Risk
#' Regression Using the Log-Binomial Model. Journal of Statistical Software,
#' 86(9), 1-22. doi:10.18637/jss.v086.i09
#'
#' @examples
#' data(cachar_sample)
#'
#' # Basic usage with boundary detection
#' result <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   exposure = "smoking"
#' )
#'
#' # Check for boundary cases
#' if (any(result$on_boundary)) {
#'   cat("Boundary case detected!\n")
#'   cat("Boundary type:", result$boundary_type[result$on_boundary], "\n")
#'   cat("CI method used:", result$ci_method[result$on_boundary], "\n")
#' }
#'
#' # Force profile likelihood CIs for all cases
#' result_profile <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   exposure = "smoking",
#'   boundary_method = "profile"
#' )
#'
#' @export
calc_risk_diff <- function(data,
                              outcome,
                              exposure,
                              adjust_vars = NULL,
                              strata = NULL,
                              link = "auto",
                              alpha = 0.05,
                              boundary_method = "auto",
                              verbose = FALSE) {

  # Input validation (same as before)
  .validate_inputs(data, outcome, exposure, adjust_vars, strata, link, alpha)

  # Additional validation for new parameters
  if (!boundary_method %in% c("auto", "profile", "bootstrap", "wald")) {
    stop("'boundary_method' must be one of: 'auto', 'profile', 'bootstrap', 'wald'",
         call. = FALSE)
  }

  # Prepare data (same as before)
  data_clean <- .prepare_data(data, outcome, exposure, adjust_vars, strata)

  # Handle stratification (same as before)
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

  # Analyze each stratum with enhanced boundary detection
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
      boundary_method = boundary_method,
      verbose = verbose
    )
  })

  # Add metadata as attributes
  attr(results, "call") <- match.call()
  attr(results, "alpha") <- alpha
  attr(results, "boundary_method") <- boundary_method
  class(results) <- c("riskdiff_result", class(results))

  # Print boundary warnings if any
  if (any(results$on_boundary, na.rm = TRUE)) {
    boundary_cases <- sum(results$on_boundary, na.rm = TRUE)
    message("Note: ", boundary_cases, " of ", nrow(results),
            " analyses had MLE on parameter space boundary. ",
            "Robust confidence intervals were used.")
  }

  return(results)
}

# Enhanced effect calculation with boundary detection
.calculate_main_effect <- function(model_result, data, exposure, alpha,
                                      boundary_method, verbose) {

  model <- model_result$model

  # Detect boundary first
  boundary_info <- .detect_boundary(model)

  if (verbose && boundary_info$on_boundary) {
    message("Boundary case detected: ", boundary_info$boundary_type)
    message("Using robust CI method: ",
            ifelse(boundary_method == "auto", "profile", boundary_method))
  }

  # Get confidence intervals - use boundary-aware method if needed
  ci_method <- if (boundary_info$on_boundary) {
    if (boundary_method == "auto") "profile" else boundary_method
  } else {
    "wald"
  }

  ci_level <- 1 - alpha

  if (ci_method == "wald" || !boundary_info$on_boundary) {
    ci <- .safe_confint(model, level = ci_level)
  } else {
    ci <- .calculate_boundary_ci(model, boundary_info, alpha, ci_method)
  }

  coefs <- summary(model)$coefficients

  # Find exposure coefficient (same as before)
  exposure_pattern <- paste0("^", exposure)
  exposure_idx <- grep(exposure_pattern, rownames(coefs))[1]

  if (is.na(exposure_idx)) {
    return(tibble::tibble(
      exposure_var = exposure,
      rd = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p_value = NA_real_,
      model_type = "coefficient_missing",
      on_boundary = FALSE,
      boundary_type = "none",
      boundary_warning = NULL,
      ci_method = "none"
    ))
  }

  # Calculate effect based on link function (same as before)
  if (model_result$type == "identity") {
    rd <- coefs[exposure_idx, 1]
    ci_lower <- ci[exposure_idx, 1]
    ci_upper <- ci[exposure_idx, 2]
    p_value <- coefs[exposure_idx, 4]
  } else {
    rd_result <- .transform_to_rd(model, data, exposure, model_result$type)
    rd <- rd_result$rd
    ci_lower <- rd_result$ci_lower
    ci_upper <- rd_result$ci_upper
    p_value <- coefs[exposure_idx, 4]
  }

  # Create enhanced result with boundary information
  tibble::tibble(
    exposure_var = exposure,
    rd = rd,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    model_type = model_result$type,
    on_boundary = boundary_info$on_boundary,
    boundary_type = boundary_info$boundary_type,
    boundary_warning = boundary_info$warning_message,
    ci_method = ci_method
  )
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
#' data(cachar_sample)
#' results <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
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
#' model types used. Version 0.2.0+ includes boundary case detection.
#'
#' @param x A riskdiff_result object from calc_risk_diff()
#' @param show_boundary Logical, whether to show boundary information when available (default: TRUE)
#' @param ... Additional arguments passed to print methods
#'
#' @return
#' Invisibly returns the original riskdiff_result object (x). Called primarily
#' for its side effect of printing formatted results to the console.
#'
#' @examples
#' data(cachar_sample)
#' result <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
#' print(result)
#'
#' # Show boundary information if available
#' print(result, show_boundary = TRUE)
#'
#' @export
print.riskdiff_result <- function(x, show_boundary = TRUE, ...) {

  # Detect if this is v0.2.0+ result with boundary information
  has_boundary_info <- "on_boundary" %in% names(x)
  package_version <- if (has_boundary_info) "v0.2.0+" else "v0.1.0"

  cat("Risk Difference Analysis Results (", package_version, ")\n", sep = "")
  cat(strrep("=", nchar(paste0("Risk Difference Analysis Results (", package_version, ")"))), "\n\n")

  alpha <- attr(x, "alpha") %||% 0.05
  cat("Confidence level:", scales::percent(1 - alpha), "\n")
  cat("Number of comparisons:", nrow(x), "\n")

  # Show boundary information if available
  if (has_boundary_info && show_boundary) {
    boundary_method <- attr(x, "boundary_method") %||% "auto"
    boundary_cases <- sum(x$on_boundary, na.rm = TRUE)

    if (boundary_cases > 0) {
      cat("Boundary cases detected:", boundary_cases, "of", nrow(x), "\n")
      cat("Boundary CI method:", boundary_method, "\n")
    }
  }

  cat("\n")

  # Create base display data (works for both versions)
  display_data <- data.frame(
    Exposure = x$exposure_var,
    `Risk Difference` = sprintf("%.2f%%", x$rd * 100),
    `95% CI` = sprintf("(%.2f%%, %.2f%%)", x$ci_lower * 100, x$ci_upper * 100),
    `P-value` = ifelse(x$p_value < 0.001, "<0.001", sprintf("%.3f", x$p_value)),
    Model = x$model_type,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Add boundary columns if available and requested
  if (has_boundary_info && show_boundary) {
    # Add boundary indicator column
    display_data$Boundary <- ifelse(x$on_boundary,
                                    paste0("[Uh oh]", x$boundary_type),
                                    "")

    # Add CI method column if available
    if ("ci_method" %in% names(x)) {
      display_data$`CI Method` <- x$ci_method
    }
  }

  # Print the main table
  print(display_data, row.names = FALSE, ...)

  # Print detailed boundary warnings if available
  if (has_boundary_info && show_boundary) {
    boundary_cases <- sum(x$on_boundary, na.rm = TRUE)

    if (boundary_cases > 0) {
      cat("\n")
      cat("Boundary Case Details:\n")
      cat("=====================\n")

      boundary_rows <- which(x$on_boundary)
      for (i in boundary_rows) {
        cat("Row", i, "(", x$exposure_var[i], "):")

        if ("boundary_warning" %in% names(x) &&
            !is.null(x$boundary_warning[i]) &&
            !is.na(x$boundary_warning[i])) {
          cat(" ", x$boundary_warning[i], "\n")
        } else {
          cat(" Boundary type:", x$boundary_type[i], "\n")
        }
      }

      cat("\nBoundary Type Guide:\n")
      cat("- upper_bound: Fitted probabilities near 1\n")
      cat("- lower_bound: Fitted probabilities near 0\n")
      cat("- separation: Complete/quasi-separation detected\n")
      cat("- both_bounds: Probabilities near both 0 and 1\n")
      cat("- [Uh oh] indicates robust confidence intervals were used\n")

      # Add methodological note
      cat("\nNote: Standard asymptotic theory may not apply for boundary cases.\n")
      cat("Confidence intervals use robust methods when boundary detected.\n")
    }
  }

  invisible(x)
}
