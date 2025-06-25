#' Detect Parameter Space Boundary Issues
#'
#' @description
#' Detects when maximum likelihood estimates lie on or near the boundary
#' of the parameter space for log-binomial and identity link models.
#' Based on methods described in Donoghoe & Marschner (2018).
#'
#' @param model A fitted GLM object
#' @param data The data used to fit the model
#' @param tolerance Numeric tolerance for boundary detection (default: 1e-6)
#' @param verbose Logical indicating whether to print diagnostic information
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{boundary_detected}{Logical indicating if boundary was detected}
#'   \item{boundary_type}{Character describing the type of boundary issue}
#'   \item{boundary_parameters}{Character vector of parameters on boundary}
#'   \item{fitted_probabilities_range}{Numeric vector with min/max fitted probabilities}
#'   \item{separation_detected}{Logical indicating complete/quasi-separation}
#' }
#'
#' @references
#' Donoghoe MW, Marschner IC (2018). "logbin: An R Package for Relative Risk
#' Regression Using the Log-Binomial Model." Journal of Statistical Software,
#' 86(9), 1-22. doi:10.18637/jss.v086.i09
#'
#' @keywords internal
.detect_boundary <- function(model, data, tolerance = 1e-6, verbose = FALSE) {

  # Initialize return structure with safe defaults
  result <- list(
    boundary_detected = FALSE,
    boundary_type = "none",
    boundary_parameters = character(0),
    fitted_probabilities_range = c(NA_real_, NA_real_),
    separation_detected = FALSE
  )

  # Input validation
  if (!inherits(model, "glm")) {
    if (verbose) message("Input is not a GLM object")
    result$boundary_type <- "invalid_model"
    return(result)
  }

  if (!model$converged) {
    if (verbose) message("Model did not converge")
    result$boundary_detected <- TRUE
    result$boundary_type <- "non_convergence"
    return(result)
  }

  tryCatch({
    # Get fitted probabilities safely
    fitted_probs <- stats::fitted(model)

    if (length(fitted_probs) == 0) {
      result$boundary_type <- "no_fitted_values"
      return(result)
    }

    # Store probability range
    result$fitted_probabilities_range <- c(min(fitted_probs, na.rm = TRUE),
                                           max(fitted_probs, na.rm = TRUE))

    # Check for probabilities at or near boundaries [0,1]
    prob_min <- min(fitted_probs, na.rm = TRUE)
    prob_max <- max(fitted_probs, na.rm = TRUE)

    # Boundary detection logic
    boundary_detected <- FALSE
    boundary_type <- "none"
    boundary_params <- character(0)

    # Type 1: Probabilities at upper boundary (≥ 1)
    if (prob_max >= (1 - tolerance)) {
      boundary_detected <- TRUE
      if (prob_max >= 1) {
        boundary_type <- "upper_boundary_exact"
      } else {
        boundary_type <- "upper_boundary_near"
      }
    }

    # Type 2: Probabilities at lower boundary (≤ 0)
    if (prob_min <= tolerance) {
      boundary_detected <- TRUE
      if (prob_min <= 0) {
        if (boundary_detected && boundary_type != "none") {
          boundary_type <- "both_boundaries"
        } else {
          boundary_type <- "lower_boundary_exact"
        }
      } else {
        if (boundary_detected && boundary_type != "none") {
          boundary_type <- "both_boundaries"
        } else {
          boundary_type <- "lower_boundary_near"
        }
      }
    }

    # Type 3: Check for separation using model diagnostics
    separation_detected <- .detect_separation(model, data, verbose)

    if (separation_detected) {
      boundary_detected <- TRUE
      if (boundary_type == "none") {
        boundary_type <- "separation"
      } else {
        boundary_type <- paste0(boundary_type, "_with_separation")
      }
      result$separation_detected <- TRUE
    }

    # Type 4: Check coefficient magnitudes (very large coefficients suggest boundary)
    if (!boundary_detected) {
      coefs <- stats::coef(model)
      if (any(abs(coefs) > 10, na.rm = TRUE)) {  # Large coefficient threshold
        boundary_detected <- TRUE
        boundary_type <- "large_coefficients"
        boundary_params <- names(coefs)[abs(coefs) > 10]
      }
    }

    # Type 5: Check standard errors (very large SEs suggest boundary issues)
    if (!boundary_detected) {
      summary_obj <- summary(model)
      if (is.matrix(summary_obj$coefficients)) {
        ses <- summary_obj$coefficients[, "Std. Error"]
        if (any(ses > 10, na.rm = TRUE)) {  # Large SE threshold
          boundary_detected <- TRUE
          boundary_type <- "large_standard_errors"
          boundary_params <- names(ses)[ses > 10]
        }
      }
    }

    # Update result
    result$boundary_detected <- boundary_detected
    result$boundary_type <- boundary_type
    result$boundary_parameters <- boundary_params

    if (verbose) {
      message("Boundary detection results:")
      message("  Boundary detected: ", boundary_detected)
      message("  Boundary type: ", boundary_type)
      message("  Probability range: [", round(prob_min, 6), ", ", round(prob_max, 6), "]")
      if (separation_detected) {
        message("  Separation detected: TRUE")
      }
    }

  }, error = function(e) {
    if (verbose) message("Error in boundary detection: ", e$message)
    result$boundary_detected <- TRUE
    result$boundary_type <- "detection_error"
    result
  })

  return(result)
}

#' Detect Complete or Quasi-Separation
#'
#' @description
#' Detects complete or quasi-separation in logistic-type models,
#' which can cause boundary issues in parameter estimation.
#'
#' @param model A fitted GLM object
#' @param data The data used to fit the model
#' @param verbose Logical for diagnostic output
#'
#' @return Logical indicating if separation was detected
#'
#' @keywords internal
.detect_separation <- function(model, data, verbose = FALSE) {

  tryCatch({
    # Get model matrix and response
    X <- stats::model.matrix(model)
    y <- stats::model.response(stats::model.frame(model))

    if (is.null(X) || is.null(y) || nrow(X) == 0) {
      return(FALSE)
    }

    # Check for perfect prediction patterns
    # This is a simplified check - more sophisticated methods exist

    # Method 1: Check if any linear combination perfectly separates
    fitted_probs <- stats::fitted(model)

    # Look for patterns where fitted probabilities are very close to 0 or 1
    extreme_low <- fitted_probs < 1e-10
    extreme_high <- fitted_probs > (1 - 1e-10)

    if (any(extreme_low) || any(extreme_high)) {
      if (verbose) {
        message("Extreme fitted probabilities detected")
      }
      return(TRUE)
    }

    # Method 2: Check coefficient stability by examining condition number
    if (ncol(X) > 1) {
      tryCatch({
        cond_num <- kappa(crossprod(X))
        if (cond_num > 1e12) {  # Very high condition number suggests near-singularity
          if (verbose) {
            message("High condition number detected: ", round(cond_num, 2))
          }
          return(TRUE)
        }
      }, error = function(e) {
        # If we can't compute condition number, be conservative
        return(FALSE)
      })
    }

    return(FALSE)

  }, error = function(e) {
    if (verbose) message("Error in separation detection: ", e$message)
    return(FALSE)
  })
}

#' Get Valid Boundary Types
#'
#' @description
#' Returns the complete list of valid boundary types that can be
#' returned by the boundary detection function.
#'
#' @return Character vector of valid boundary type names
#'
#' @export
get_valid_boundary_types <- function() {
  c(
    "none",
    "upper_boundary_exact",
    "upper_boundary_near",
    "lower_boundary_exact",
    "lower_boundary_near",
    "both_boundaries",
    "separation",
    "upper_boundary_exact_with_separation",
    "upper_boundary_near_with_separation",
    "lower_boundary_exact_with_separation",
    "lower_boundary_near_with_separation",
    "both_boundaries_with_separation",
    "large_coefficients",
    "large_standard_errors",
    "non_convergence",
    "invalid_model",
    "no_fitted_values",
    "detection_error"
  )
}
