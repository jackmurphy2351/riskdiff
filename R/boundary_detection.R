# boundary_detection.R - Functions for detecting and handling boundary cases

#' Detect if MLE is on the boundary of parameter space
#'
#' @param model Fitted GLM object
#' @param tolerance Numerical tolerance for boundary detection (default: 1e-6)
#' @param prob_tolerance Tolerance for fitted probabilities near 0 or 1 (default: 1e-8)
#'
#' @return List with boundary information
#'
#' @details
#' Based on methods described in Marschner & Gillett (2012) for log-binomial models.
#' Checks both parameter estimates and fitted probabilities for boundary conditions.
#'
#' @references
#' Marschner IC, Gillett AC (2012). Relative Risk Regression: Reliable and Flexible
#' Methods for Log-Binomial Models. Biostatistics, 13(1), 179-192.
#' @keywords internal
#' @noRd
.detect_boundary <- function(model, tolerance = 1e-6, prob_tolerance = 1e-8) {

  if (is.null(model) || !inherits(model, "glm")) {
    return(list(
      on_boundary = FALSE,
      boundary_type = "none",
      boundary_params = character(0),
      fitted_probs_boundary = FALSE,
      warning_message = NULL
    ))
  }

  # Get fitted probabilities
  fitted_probs <- fitted(model)

  # Check if fitted probabilities are at boundaries
  probs_at_zero <- any(fitted_probs < prob_tolerance)
  probs_at_one <- any(fitted_probs > (1 - prob_tolerance))
  fitted_probs_boundary <- probs_at_zero || probs_at_one

  # For identity link, check parameter constraints directly
  if (model$family$link == "identity") {
    boundary_info <- .detect_identity_boundary(model, tolerance, prob_tolerance)
  } else if (model$family$link == "log") {
    boundary_info <- .detect_log_boundary(model, tolerance, prob_tolerance)
  } else {
    # For logit link, boundary is less common but can occur with separation
    boundary_info <- .detect_logit_boundary(model, tolerance)
  }

  # Combine information
  result <- list(
    on_boundary = boundary_info$on_boundary || fitted_probs_boundary,
    boundary_type = boundary_info$boundary_type,
    boundary_params = boundary_info$boundary_params,
    fitted_probs_boundary = fitted_probs_boundary,
    fitted_probs_at_zero = probs_at_zero,
    fitted_probs_at_one = probs_at_one,
    warning_message = boundary_info$warning_message
  )

  return(result)
}

#' Detect boundary for identity link models
#' @keywords internal
#' @noRd
.detect_identity_boundary <- function(model, tolerance, prob_tolerance) {

  # For identity link: constraint is that all fitted probabilities in [0,1]
  fitted_probs <- fitted(model)
  design_matrix <- model.matrix(model)
  coefs <- coef(model)

  # Check if any linear combinations approach constraints
  linear_pred <- as.vector(design_matrix %*% coefs)

  # Parameters are on boundary if:
  # 1. Any fitted probability <= 0 or >= 1
  # 2. Gradient suggests we're constrained

  at_lower <- any(linear_pred < tolerance)
  at_upper <- any(linear_pred > (1 - tolerance))

  on_boundary <- at_lower || at_upper

  boundary_type <- if (at_lower && at_upper) {
    "both_bounds"
  } else if (at_upper) {
    "upper_bound"  # probabilities near 1
  } else if (at_lower) {
    "lower_bound"  # probabilities near 0
  } else {
    "none"
  }

  # Identify which parameters might be causing boundary issues
  boundary_params <- character(0)
  if (on_boundary) {
    # This is simplified - more sophisticated methods would use the gradient
    extreme_indices <- which(linear_pred < tolerance | linear_pred > (1 - tolerance))
    if (length(extreme_indices) > 0) {
      # Get parameter names that might be problematic
      boundary_params <- names(coefs)
    }
  }

  warning_message <- if (on_boundary) {
    "Identity link model has fitted probabilities at or near boundary (0 or 1). Confidence intervals may be unreliable."
  } else {
    NULL
  }

  list(
    on_boundary = on_boundary,
    boundary_type = boundary_type,
    boundary_params = boundary_params,
    warning_message = warning_message
  )
}

#' Detect boundary for log link models
#' @keywords internal
#' @noRd
.detect_log_boundary <- function(model, tolerance, prob_tolerance) {

  # For log link: constraint is that all fitted probabilities <= 1
  fitted_probs <- fitted(model)

  # Check if any probabilities are near 1
  at_boundary <- any(fitted_probs > (1 - prob_tolerance))

  boundary_type <- if (at_boundary) "upper_bound" else "none"

  boundary_params <- if (at_boundary) names(coef(model)) else character(0)

  warning_message <- if (at_boundary) {
    "Log link model has fitted probabilities near 1. Parameter estimates may be on boundary."
  } else {
    NULL
  }

  list(
    on_boundary = at_boundary,
    boundary_type = boundary_type,
    boundary_params = boundary_params,
    warning_message = warning_message
  )
}

#' Detect boundary for logit link models
#' @keywords internal
#' @noRd
.detect_logit_boundary <- function(model, tolerance) {

  # For logit link, boundary typically occurs with complete separation
  # Check for very large coefficient estimates
  coefs <- coef(model)
  large_coefs <- abs(coefs) > 10  # Somewhat arbitrary threshold

  on_boundary <- any(large_coefs, na.rm = TRUE)

  boundary_type <- if (on_boundary) "separation" else "none"

  boundary_params <- if (on_boundary) {
    names(coefs)[large_coefs & !is.na(large_coefs)]
  } else {
    character(0)
  }

  warning_message <- if (on_boundary) {
    "Logit model may have separation issues. Very large coefficient estimates detected."
  } else {
    NULL
  }

  list(
    on_boundary = on_boundary,
    boundary_type = boundary_type,
    boundary_params = boundary_params,
    warning_message = warning_message
  )
}

#' Calculate robust confidence intervals for boundary cases
#'
#' @param model Fitted GLM object
#' @param boundary_info Output from .detect_boundary()
#' @param alpha Significance level
#' @param method Method for boundary CI: "profile", "bootstrap", "wald"
#'
#' @details
#' When MLE is on boundary, standard Wald intervals may be inappropriate.
#' This function implements alternative methods:
#' - Profile likelihood intervals (preferred when feasible)
#' - Bootstrap intervals
#' - Modified Wald intervals with boundary adjustment
#'
#' @references
#' Venzon DJ, Moolgavkar SH (1988). A Method for Computing Profile-Likelihood-Based
#' Confidence Intervals. Journal of the Royal Statistical Society, 37(1), 87-94.
#' @keywords internal
#' @noRd
.calculate_boundary_ci <- function(model, boundary_info, alpha = 0.05, method = "auto") {

  if (!boundary_info$on_boundary) {
    # Use standard methods if not on boundary
    return(.safe_confint(model, level = 1 - alpha))
  }

  # Select method based on model and boundary type
  if (method == "auto") {
    method <- if (model$family$link == "identity" && boundary_info$fitted_probs_boundary) {
      "profile"  # Profile likelihood preferred for identity link boundary cases
    } else {
      "bootstrap"  # Bootstrap as fallback
    }
  }

  switch(method,
         "profile" = .profile_ci_boundary(model, alpha),
         "bootstrap" = .bootstrap_ci_boundary(model, alpha),
         "wald" = .modified_wald_ci_boundary(model, boundary_info, alpha),
         stop("Unknown CI method for boundary cases: ", method)
  )
}

#' Profile likelihood confidence intervals for boundary cases
#' @keywords internal
#' @noRd
.profile_ci_boundary <- function(model, alpha) {

  tryCatch({
    # Try profile likelihood method
    confint(model, level = 1 - alpha, method = "profile")
  }, error = function(e) {
    # Fallback to Wald if profile fails
    warning("Profile likelihood CI failed, using Wald intervals: ", e$message)
    .safe_confint(model, level = 1 - alpha)
  })
}

#' Bootstrap confidence intervals for boundary cases
#' @keywords internal
#' @noRd
.bootstrap_ci_boundary <- function(model, alpha, n_boot = 1000) {

  # This is a simplified implementation
  # Full implementation would need proper bootstrap resampling
  warning("Bootstrap CI not fully implemented. Using Wald intervals.")
  .safe_confint(model, level = 1 - alpha)
}

#' Modified Wald intervals with boundary adjustment
#' @keywords internal
#' @noRd
.modified_wald_ci_boundary <- function(model, boundary_info, alpha) {

  # Apply conservative adjustment for boundary cases
  ci <- .safe_confint(model, level = 1 - alpha)

  # For boundary cases, widen intervals by a factor
  # This is a heuristic adjustment - more sophisticated methods exist
  if (boundary_info$boundary_type %in% c("upper_bound", "both_bounds")) {
    adjustment_factor <- 1.2  # 20% wider
    width <- ci[, 2] - ci[, 1]
    center <- (ci[, 2] + ci[, 1]) / 2

    ci[, 1] <- center - adjustment_factor * width / 2
    ci[, 2] <- center + adjustment_factor * width / 2
  }

  return(ci)
}

#' Add boundary information to risk difference results
#'
#' @param result Risk difference result tibble
#' @param model_result Model fitting result from .fit_robust_glm()
#' @param alpha Significance level
#'
#' @return Updated result with boundary information
#' @keywords internal
#' @noRd
.add_boundary_info <- function(result, model_result, alpha = 0.05) {

  if (is.null(model_result$model)) {
    # No model to check
    result$on_boundary <- FALSE
    result$boundary_type <- "no_model"
    result$boundary_warning <- NULL
    return(result)
  }

  # Detect boundary
  boundary_info <- .detect_boundary(model_result$model)

  # Add boundary information to result
  result$on_boundary <- boundary_info$on_boundary
  result$boundary_type <- boundary_info$boundary_type
  result$boundary_warning <- boundary_info$warning_message

  # If on boundary, recalculate confidence intervals using robust method
  if (boundary_info$on_boundary) {
    robust_ci <- .calculate_boundary_ci(model_result$model, boundary_info, alpha)

    # Update confidence intervals in the result
    # This would need to be integrated with your existing CI calculation logic
    # The exact implementation depends on how you want to handle different link functions
  }

  return(result)
}
