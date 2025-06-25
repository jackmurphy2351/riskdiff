#' Calculate Risk Differences with Robust Model Fitting and Boundary Detection
#'
#' @description
#' Calculates risk differences (or prevalence differences for cross-sectional data)
#' using generalized linear models with identity, log, or logit links. Version 0.2.1
#' includes enhanced boundary detection, robust confidence intervals, and improved
#' data quality validation to prevent extreme confidence intervals in stratified analyses.
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
#' @param boundary_method Method for handling boundary cases: "auto", "profile", "bootstrap", "wald" (default: "auto")
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
#'   \item{on_boundary}{Logical. TRUE if MLE is on parameter space boundary}
#'   \item{boundary_type}{Character. Type of boundary: "none", "upper_bound", "lower_bound", "separation", "both_bounds"}
#'   \item{boundary_warning}{Character. Warning message for boundary cases (if any)}
#'   \item{ci_method}{Character. Method used for confidence intervals ("wald", "profile", "bootstrap")}
#'   \item{...}{Additional columns for stratification variables if specified}
#' }
#'
#' The returned object has attributes including the original function call and
#' alpha level used. Risk differences are on the probability scale where 0.05
#' represents a 5 percentage point difference.
#'
#' @details
#' ## New in Version 0.2.1: Enhanced Stability and Quality Validation
#'
#' This version adds comprehensive data quality validation to prevent the
#' extreme confidence intervals that could occur in stratified analyses:
#'
#' ### Enhanced Data Validation:
#' - Pre-analysis checks for stratification feasibility
#' - Detection of small sample sizes within strata
#' - Identification of rare outcomes or unbalanced exposures
#' - Warning for potential separation issues
#'
#' ### Boundary Detection and Robust Inference:
#' When the MLE is on the boundary, standard asymptotic theory may not apply.
#' The function detects and handles:
#' - **upper_bound**: Fitted probabilities approaching 1
#' - **lower_bound**: Fitted probabilities approaching 0
#' - **separation**: Complete or quasi-perfect separation
#' - **both_bounds**: Mixed boundary issues
#'
#' ### Robust Confidence Intervals:
#' For boundary cases, implements:
#' - **Profile likelihood intervals** (preferred when feasible)
#' - **Bootstrap confidence intervals** (robust for complex cases)
#' - **Modified Wald intervals** with boundary adjustments
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
#' Marschner IC, Gillett AC (2012). "Relative Risk Regression: Reliable and
#' Flexible Methods for Log-Binomial Models." Biostatistics, 13(1), 179-192.
#'
#' Venzon DJ, Moolgavkar SH (1988). "A Method for Computing Profile-Likelihood-Based
#' Confidence Intervals." Journal of the Royal Statistical Society, 37(1), 87-94.
#'
#' Rothman KJ, Greenland S, Lash TL (2008). Modern Epidemiology, 3rd edition.
#' Lippincott Williams & Wilkins.
#'
#' @examples
#' # Simple risk difference
#' data(cachar_sample)
#' rd_simple <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   exposure = "areca_nut"
#' )
#' print(rd_simple)
#'
#' # Age-adjusted risk difference
#' rd_adjusted <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   exposure = "areca_nut",
#'   adjust_vars = "age"
#' )
#' print(rd_adjusted)
#'
#' # Stratified analysis with enhanced error checking and boundary detection
#' rd_stratified <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   exposure = "areca_nut",
#'   strata = "residence",
#'   verbose = TRUE  # See diagnostic messages and boundary detection
#' )
#' print(rd_stratified)
#'
#' # Check for boundary cases
#' if (any(rd_stratified$on_boundary)) {
#'   cat("Boundary cases detected!\n")
#'   boundary_rows <- which(rd_stratified$on_boundary)
#'   for (i in boundary_rows) {
#'     cat("Row", i, ":", rd_stratified$boundary_type[i], "\n")
#'   }
#' }
#'
#' # Force profile likelihood CIs for enhanced robustness
#' rd_profile <- calc_risk_diff(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   exposure = "areca_nut",
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

  # Enhanced input validation (includes data quality checks)
  .validate_inputs(data, outcome, exposure, adjust_vars, strata, link, alpha)

  # Additional validation for new parameters
  if (!boundary_method %in% c("auto", "profile", "bootstrap", "wald")) {
    stop("'boundary_method' must be one of: 'auto', 'profile', 'bootstrap', 'wald'",
         call. = FALSE)
  }

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

  # Enhanced post-analysis warnings
  if (nrow(results) > 1) {
    n_failed <- sum(results$model_type %in% c("failed", "insufficient_data"))
    n_boundary <- sum(results$on_boundary, na.rm = TRUE)
    n_wide_ci <- sum((results$ci_upper - results$ci_lower) > 0.5, na.rm = TRUE)

    if (n_failed > 0) {
      warning(sprintf("%d of %d analyses failed. Consider pooled analysis or different stratification.",
                      n_failed, nrow(results)))
    }

    if (n_boundary > 0) {
      message(.safe_warning(), " ", n_boundary, " of ", nrow(results),
              " analyses had MLE on parameter space boundary. ",
              "Robust confidence intervals were used.")
    }

    if (n_wide_ci > 0) {
      warning(sprintf("%d of %d estimates have very wide CIs (>50 pp). Consider larger sample sizes.",
                      n_wide_ci, nrow(results)))
    }
  } else {
    # Single analysis feedback
    if (any(results$on_boundary, na.rm = TRUE)) {
      if (verbose) {
        message(.safe_warning(), " Boundary case detected: ", results$boundary_type[1])
        message("Using robust CI method: ", results$ci_method[1])
      }
    }
  }

  return(results)
}

#' Format Risk Difference Results for Display
#'
#' @description
#' Formats numerical values in risk difference results for presentation,
#' with appropriate percentage formatting and rounding. Enhanced for v0.2.1
#' to handle boundary information and quality indicators with robust error handling.
#'
#' @param results Results tibble from calc_risk_diff()
#' @param digits Number of decimal places for percentages (default: 2)
#' @param p_accuracy Accuracy for p-values (default: 0.001)
#' @param show_ci_method Logical indicating whether to show CI method in output (default: FALSE)
#' @param show_quality Logical indicating whether to add quality indicators (default: TRUE)
#'
#' @return
#' Tibble with additional formatted columns including:
#' \describe{
#'   \item{rd_formatted}{Risk difference as formatted percentage string}
#'   \item{ci_formatted}{Confidence interval as formatted string}
#'   \item{p_value_formatted}{P-value with appropriate precision}
#'   \item{quality_indicator}{Quality assessment (if show_quality = TRUE)}
#'   \item{ci_method_display}{CI method information (if show_ci_method = TRUE)}
#' }
#'
#' @examples
#' data(cachar_sample)
#' results <- calc_risk_diff(cachar_sample, "abnormal_screen", "areca_nut")
#' formatted <- format_risk_diff(results)
#' print(formatted)
#'
#' # Show CI methods and quality indicators
#' formatted_detailed <- format_risk_diff(results, show_ci_method = TRUE, show_quality = TRUE)
#' print(formatted_detailed)
#'
#' # Customize formatting
#' formatted_custom <- format_risk_diff(results, digits = 3, p_accuracy = 0.01, show_quality = FALSE)
#' print(formatted_custom)
#'
#' @export
format_risk_diff <- function(results,
                             digits = 2,
                             p_accuracy = 0.001,
                             show_ci_method = FALSE,
                             show_quality = TRUE) {

  # Input validation
  if (!inherits(results, "riskdiff_result")) {
    warning("Input does not appear to be from calc_risk_diff(). Proceeding anyway.")
  }

  if (nrow(results) == 0) {
    warning("Empty results provided to format_risk_diff()")
    return(results)
  }

  # Core formatting using base R to avoid dependency issues
  results$rd_formatted <- sprintf(paste0("%.", digits, "f%%"), results$rd * 100)
  results$ci_formatted <- sprintf(
    paste0("(%.", digits, "f%%, %.", digits, "f%%)"),
    results$ci_lower * 100,
    results$ci_upper * 100
  )

  # Enhanced p-value formatting with robust NA handling
  results$p_value_formatted <- ifelse(
    is.na(results$p_value),
    "\u2014",  # em dash for missing p-values
    ifelse(
      results$p_value < p_accuracy,
      paste0("<", p_accuracy),
      sprintf("%.3f", results$p_value)
    )
  )

  # Add CI method information if requested and available
  if (show_ci_method && "ci_method" %in% names(results)) {
    results$ci_method_display <- ifelse(
      is.na(results$ci_method) | results$ci_method == "default",
      "Standard",
      stringr::str_to_title(results$ci_method)
    )
  }

  # Add quality indicator with comprehensive assessment
  if (show_quality) {
    results$quality_indicator <- .assess_result_quality(results)
  }

  # Add metadata about formatting
  attr(results, "formatting") <- list(
    digits = digits,
    p_accuracy = p_accuracy,
    show_ci_method = show_ci_method,
    show_quality = show_quality,
    formatted_at = Sys.time()
  )

  return(results)
}

#' Assess Quality of Risk Difference Results
#'
#' @description
#' Internal function to assess the quality and reliability of risk difference estimates
#' based on multiple criteria including sample size, CI width, boundary issues, and
#' model convergence.
#'
#' @param results Results tibble from calc_risk_diff()
#'
#' @return Character vector of quality assessments
#'
#' @keywords internal
.assess_result_quality <- function(results) {

  # Initialize quality vector
  quality <- character(nrow(results))

  # Required columns check
  required_cols <- c("rd", "ci_lower", "ci_upper", "model_type")
  missing_cols <- setdiff(required_cols, names(results))

  if (length(missing_cols) > 0) {
    warning("Missing required columns for quality assessment: ",
            paste(missing_cols, collapse = ", "))
    return(rep("Unknown", nrow(results)))
  }

  # Assess each result
  for (i in seq_len(nrow(results))) {

    # Check for failed analyses first
    if (is.na(results$rd[i]) ||
        results$model_type[i] %in% c("failed", "insufficient_data")) {
      quality[i] <- "Failed"
      next
    }

    # Calculate CI width if possible
    ci_width <- if (all(c("ci_lower", "ci_upper") %in% names(results))) {
      results$ci_upper[i] - results$ci_lower[i]
    } else {
      NA_real_
    }

    # Get sample size if available
    n_obs <- if ("n_obs" %in% names(results)) {
      results$n_obs[i]
    } else {
      NA_integer_
    }

    # Check boundary status
    on_boundary <- if ("on_boundary" %in% names(results)) {
      isTRUE(results$on_boundary[i])
    } else {
      FALSE
    }

    # Apply quality criteria in order of severity
    if (!is.na(ci_width) && ci_width > 0.5) {
      quality[i] <- "Very Wide CI"
    } else if (on_boundary) {
      quality[i] <- "Boundary"
    } else if (!is.na(n_obs) && n_obs < 30) {
      quality[i] <- "Small N"
    } else if (!is.na(ci_width) && ci_width > 0.25) {
      quality[i] <- "Wide CI"
    } else if (!is.na(ci_width) && ci_width > 0.15) {
      quality[i] <- "Acceptable"
    } else {
      quality[i] <- "Good"
    }
  }

  return(quality)
}

#' Get Quality Legend for Risk Difference Results
#'
#' @description
#' Returns a legend explaining the quality indicators used in formatted results.
#'
#' @return Character vector with quality indicator explanations
#'
#' @examples
#' quality_legend <- get_quality_legend()
#' cat(paste(quality_legend, collapse = "\n"))
#'
#' @export
get_quality_legend <- function() {
  c(
    "Quality Indicator Legend:",
    "=========================",
    "Good         - Narrow CI (<15pp), adequate sample, no boundary issues",
    "Acceptable   - Moderate CI (15-25pp), reasonable reliability",
    "Wide CI      - Wide CI (25-50pp), interpret with caution",
    "Very Wide CI - Very wide CI (>50pp), unreliable estimate",
    "Small N      - Sample size <30, limited precision",
    "Boundary     - MLE on parameter boundary, robust CIs used",
    "Failed       - Model convergence failed or insufficient data",
    "",
    "pp = percentage points; CI = confidence interval; MLE = maximum likelihood estimate"
  )
}

#' Print method for riskdiff_result objects
#'
#' @description
#' Prints risk difference results in a formatted, readable way showing
#' key statistics including risk differences, confidence intervals, model
#' types used, and enhanced boundary case diagnostics for v0.2.1+.
#'
#' @param x A riskdiff_result object from calc_risk_diff()
#' @param show_boundary Logical indicating whether to show boundary case details (default: TRUE)
#' @param show_quality Logical indicating whether to show quality indicators (default: TRUE)
#' @param ... Additional arguments passed to print methods
#'
#' @return
#' Invisibly returns the original riskdiff_result object (x). Called primarily
#' for its side effect of printing formatted results to the console.
#'
#' @examples
#' data(cachar_sample)
#' result <- calc_risk_diff(cachar_sample, "abnormal_screen", "areca_nut")
#' print(result)
#'
#' # Suppress boundary details for cleaner output
#' print(result, show_boundary = FALSE)
#'
#' @export
print.riskdiff_result <- function(x, show_boundary = TRUE, show_quality = TRUE, ...) {

  # Detect version based on available columns (robust detection)
  has_boundary_info <- "on_boundary" %in% names(x)
  has_quality_info <- "ci_method" %in% names(x)
  has_legacy_boundary <- "boundary_detected" %in% names(x)  # v0.2.0 style

  package_version <- if (has_boundary_info && has_quality_info) {
    "v0.2.1"
  } else if (has_boundary_info || has_legacy_boundary) {
    "v0.2.0"
  } else {
    "v0.1.0"
  }

  # Header with dynamic version
  header_text <- paste0("Risk Difference Analysis Results (", package_version, ")")
  cat(header_text, "\n")
  cat(strrep("=", nchar(header_text)), "\n\n")

  # Basic analysis info
  alpha <- attr(x, "alpha") %||% 0.05
  cat("Confidence level:", scales::percent(1 - alpha), "\n")
  cat("Number of comparisons:", nrow(x), "\n")

  # Enhanced summary information for v0.2.0+
  if ((has_boundary_info || has_legacy_boundary) && show_boundary) {

    # Handle both v0.2.0 and v0.2.1 boundary detection
    if (has_boundary_info) {
      # v0.2.1 style
      boundary_method <- attr(x, "boundary_method") %||% "auto"
      boundary_cases <- sum(x$on_boundary, na.rm = TRUE)
    } else if (has_legacy_boundary) {
      # v0.2.0 style - convert to v0.2.1 format
      boundary_method <- "legacy"
      boundary_cases <- sum(x$boundary_detected, na.rm = TRUE)
    } else {
      boundary_cases <- 0
    }

    failed_cases <- sum(x$model_type %in% c("failed", "insufficient_data"), na.rm = TRUE)

    if (boundary_cases > 0) {
      cat("Boundary cases detected:", boundary_cases, "of", nrow(x), "\n")
      if (has_boundary_info) {
        cat("Boundary CI method:", boundary_method, "\n")
      }
    }

    if (failed_cases > 0) {
      cat(.safe_cross(), " Failed analyses:", failed_cases, "of", nrow(x), "\n")
    }
  }

  cat("\n")

  # Create display data with robust column handling
  display_data <- data.frame(
    Exposure = x$exposure_var,
    `Risk Difference` = sprintf("%.2f%%", x$rd * 100),
    `95% CI` = sprintf("(%.2f%%, %.2f%%)", x$ci_lower * 100, x$ci_upper * 100),
    `P-value` = ifelse(
      is.na(x$p_value),
      .safe_em_dash(),
      ifelse(x$p_value < 0.001, "<0.001", sprintf("%.3f", x$p_value))
    ),
    Model = x$model_type,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Add sample size if available
  if ("n_obs" %in% names(x)) {
    display_data$N <- x$n_obs
  }

  # Add boundary indicators for v0.2.1
  if (has_boundary_info && show_boundary) {
    boundary_indicator <- ifelse(
      x$on_boundary,
      paste0(.safe_warning(), " ", x$boundary_type),
      ""
    )
    display_data$Boundary <- boundary_indicator
  } else if (has_legacy_boundary && show_boundary) {
    # Handle v0.2.0 boundary format
    boundary_indicator <- ifelse(
      x$boundary_detected,
      paste0(.safe_warning(), " ", x$boundary_type),
      ""
    )
    display_data$Boundary <- boundary_indicator
  }

  # Add quality assessment for v0.2.1
  if (show_quality && has_boundary_info) {
    quality_scores <- dplyr::case_when(
      is.na(x$rd) | x$model_type %in% c("failed", "insufficient_data") ~ "Failed",
      (x$ci_upper - x$ci_lower) > 0.5 ~ "Very Wide CI",
      x$on_boundary ~ "Boundary",
      "n_obs" %in% names(x) && x$n_obs < 30 ~ "Small N",
      (x$ci_upper - x$ci_lower) > 0.25 ~ "Wide CI",
      TRUE ~ "Good"
    )
    display_data$Quality <- quality_scores
  }

  # Print the main table
  print(display_data, row.names = FALSE, ...)

  # Detailed boundary case information (v0.2.1 style)
  if (has_boundary_info && show_boundary) {
    boundary_cases <- sum(x$on_boundary, na.rm = TRUE)
    failed_cases <- sum(x$model_type %in% c("failed", "insufficient_data"), na.rm = TRUE)

    if (boundary_cases > 0) {
      cat("\n")
      cat(.safe_warning(), " Boundary Case Details:\n")
      cat(strrep("=", 25), "\n")

      boundary_rows <- which(x$on_boundary)
      for (i in boundary_rows) {
        cat("Row", i, "(", x$exposure_var[i], "):")

        if ("boundary_warning" %in% names(x) &&
            !is.null(x$boundary_warning[i]) &&
            !is.na(x$boundary_warning[i])) {
          cat(" ", x$boundary_warning[i], "\n")
        } else {
          boundary_type <- if ("boundary_type" %in% names(x)) x$boundary_type[i] else "detected"
          ci_method <- if ("ci_method" %in% names(x)) x$ci_method[i] else "robust"
          cat(" Type:", boundary_type, "| CI method:", ci_method, "\n")
        }
      }

      # Educational content
      cat("\nBoundary Type Guide:\n")
      cat(paste0(.safe_bullet()), "upper_bound: Fitted probabilities near 1 (risk saturation)\n")
      cat(paste0(.safe_bullet()), "lower_bound: Fitted probabilities near 0 (very rare outcomes)\n")
      cat(paste0(.safe_bullet()), "separation: Complete/quasi-separation detected\n")
      cat(paste0(.safe_bullet()), "both_bounds: Mixed boundary issues across observations\n")
      cat(.safe_warning(), " indicates robust confidence intervals were used\n")
    }

    if (failed_cases > 0) {
      cat("\n")
      cat(.safe_cross(), " Failed Analysis Details:\n")
      cat(strrep("=", 27), "\n")

      failed_rows <- which(x$model_type %in% c("failed", "insufficient_data"))
      for (i in failed_rows) {
        n_obs_text <- if ("n_obs" %in% names(x)) paste0(" (n=", x$n_obs[i], ")") else ""
        cat("Row", i, "(", x$exposure_var[i], "): ", x$model_type[i], n_obs_text, "\n")
      }

      # Practical recommendations
      cat("\nRecommendations for failed analyses:\n")
      cat(paste0(.safe_bullet()), "Increase sample size or combine small strata\n")
      cat(paste0(.safe_bullet()), "Check for data quality issues\n")
      cat(paste0(.safe_bullet()), "Consider pooled (unstratified) analysis\n")
      cat(paste0(.safe_bullet()), "Use exact methods for very small samples\n")
    }

    # Enhanced methodological note
    if (boundary_cases > 0 || failed_cases > 0) {
      cat("\n")
      cat("Statistical Note:\n")
      cat("================\n")
      cat("Standard asymptotic theory may not apply for boundary cases.\n")
      cat("Confidence intervals use robust methods when boundary detected.\n")
      cat("For failed analyses, consider alternative estimation approaches.\n")
    }
  } else if (has_legacy_boundary && show_boundary) {
    # Simplified boundary reporting for v0.2.0
    boundary_cases <- sum(x$boundary_detected, na.rm = TRUE)
    if (boundary_cases > 0) {
      cat("\n")
      cat("Note:", boundary_cases, "of", nrow(x),
          "analyses had MLE on parameter space boundary. Robust confidence intervals were used.\n")
    }
  }

  invisible(x)
}
