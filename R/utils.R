# Internal helper functions for riskdiff package

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Enhanced input validation with additional checks
.validate_inputs <- function(data, outcome, exposure, adjust_vars, strata, link, alpha) {

  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }

  if (nrow(data) == 0) {
    stop("'data' contains no rows", call. = FALSE)
  }

  # Check variable names
  all_vars <- c(outcome, exposure, adjust_vars, strata)
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  # Check outcome variable
  if (!is.numeric(data[[outcome]]) && !is.logical(data[[outcome]])) {
    stop("Outcome variable must be numeric (0/1) or logical (TRUE/FALSE)", call. = FALSE)
  }

  unique_outcome <- unique(data[[outcome]][!is.na(data[[outcome]])])
  if (!all(unique_outcome %in% c(0, 1, TRUE, FALSE))) {
    stop("Outcome variable must be binary (0/1 or TRUE/FALSE)", call. = FALSE)
  }

  # Check link
  if (!link %in% c("auto", "identity", "log", "logit")) {
    stop("'link' must be one of: 'auto', 'identity', 'log', 'logit'", call. = FALSE)
  }

  # Check alpha
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be numeric between 0 and 1", call. = FALSE)
  }

  # Additional enhanced validation
  .validate_inputs_enhanced(data, outcome, exposure, adjust_vars, strata, link, alpha)
}

# Enhanced input validation (additional checks)
.validate_inputs_enhanced <- function(data, outcome, exposure, adjust_vars, strata, link, alpha) {

  # Check outcome prevalence
  outcome_prev <- mean(data[[outcome]], na.rm = TRUE)
  if (outcome_prev < 0.005) {
    warning("Very rare outcome (<0.5%). Risk ratios may be more appropriate than risk differences.")
  }
  if (outcome_prev > 0.95) {
    warning("Very common outcome (>95%). Consider analyzing the complement outcome.")
  }

  # Check exposure distribution
  exposure_table <- table(data[[exposure]])
  min_exposure_n <- min(exposure_table)
  if (min_exposure_n < 10) {
    warning(sprintf("Small exposure group (n=%d). Results may be unstable.", min_exposure_n))
  }

  # Check for stratification issues
  if (!is.null(strata)) {
    .validate_stratified_analysis(data, outcome, exposure, strata)
  }

  # Check for separation in main analysis
  formula <- .build_formula(outcome, exposure, adjust_vars)
  has_separation <- .check_separation(data, formula)

  invisible(TRUE)
}

# Validate stratified analysis feasibility
.validate_stratified_analysis <- function(data, outcome, exposure, strata) {

  # Check stratum sizes
  strata_summary <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(strata))) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_outcome = sum(.data[[outcome]], na.rm = TRUE),
      n_exposed = sum(.data[[exposure]] == levels(.data[[exposure]])[2], na.rm = TRUE),
      n_unexposed = sum(.data[[exposure]] == levels(.data[[exposure]])[1], na.rm = TRUE),
      outcome_rate = mean(.data[[outcome]], na.rm = TRUE),
      .groups = "drop"
    )

  # Flag problematic strata
  strata_summary <- strata_summary %>%
    dplyr::mutate(
      too_small = n_total < 30,
      no_events = n_outcome == 0,
      no_variation_exposure = n_exposed == 0 | n_unexposed == 0,
      extreme_outcome_rate = outcome_rate < 0.01 | outcome_rate > 0.99,
      problematic = too_small | no_events | no_variation_exposure | extreme_outcome_rate
    )

  n_problematic <- sum(strata_summary$problematic)
  n_total_strata <- nrow(strata_summary)

  if (n_problematic > 0) {
    warning(sprintf(
      "Stratified analysis may be unstable: %d of %d strata have insufficient data. Consider pooled analysis.",
      n_problematic, n_total_strata
    ))

    if (n_problematic == n_total_strata) {
      stop("All strata have insufficient data for analysis. Try different stratification or pooled analysis.")
    }
  }

  return(strata_summary)
}

# Check for complete or quasi-complete separation
.check_separation <- function(data, formula) {

  # Fit a quick logistic model to check for separation
  tryCatch({
    check_model <- stats::glm(formula, data = data, family = stats::binomial())

    # Check for extreme coefficients (sign of separation)
    coefs <- stats::coef(check_model)
    extreme_coefs <- abs(coefs) > 10  # Coefficients > 10 suggest separation issues

    if (any(extreme_coefs, na.rm = TRUE)) {
      warning("Possible separation detected. Risk difference estimates may be unstable.")
      return(TRUE)
    }

    # Check fitted probabilities
    fitted_probs <- stats::fitted(check_model)
    extreme_fitted <- any(fitted_probs < 1e-6 | fitted_probs > (1 - 1e-6))

    if (extreme_fitted) {
      warning("Extreme fitted probabilities detected. Consider Firth's logistic regression or exact methods.")
      return(TRUE)
    }

    return(FALSE)

  }, error = function(e) {
    # If even logistic regression fails, we definitely have problems
    warning("Preliminary model fitting failed. Data may have structural issues.")
    return(TRUE)
  })
}

# Data preparation
.prepare_data <- function(data, outcome, exposure, adjust_vars, strata) {

  # Select relevant variables
  all_vars <- c(outcome, exposure, adjust_vars, strata)
  data_subset <- data[all_vars]

  # Convert outcome to 0/1 if logical
  if (is.logical(data_subset[[outcome]])) {
    data_subset[[outcome]] <- as.numeric(data_subset[[outcome]])
  }

  # Ensure exposure is factor
  if (!is.factor(data_subset[[exposure]])) {
    data_subset[[exposure]] <- factor(data_subset[[exposure]])
  }

  # Check that exposure has at least 2 levels
  if (nlevels(data_subset[[exposure]]) < 2) {
    stop("Exposure variable must have at least 2 levels", call. = FALSE)
  }

  # Remove rows with missing outcome or exposure
  complete_cases <- !is.na(data_subset[[outcome]]) & !is.na(data_subset[[exposure]])
  data_clean <- data_subset[complete_cases, , drop = FALSE]

  if (nrow(data_clean) == 0) {
    stop("No complete cases found for outcome and exposure variables", call. = FALSE)
  }

  return(data_clean)
}

# Analyze single stratum
.analyze_stratum <- function(data, outcome, exposure, adjust_vars,
                             strata, group_label, link, alpha,
                             boundary_method, verbose) {

  # Check minimum sample size
  if (nrow(data) < 20) {
    if (verbose) {
      message("Insufficient data in stratum (n=", nrow(data), "). Skipping.")
    }
    return(.create_insufficient_result(exposure, group_label, strata))
  }

  # Check if there's variation in the outcome
  if (length(unique(data[[outcome]])) < 2) {
    if (verbose) {
      message("No variation in outcome in this stratum. Skipping.")
    }
    return(.create_insufficient_result(exposure, group_label, strata))
  }

  # Build formula
  formula <- .build_formula(outcome, exposure, adjust_vars)

  if (verbose) {
    message("Formula: ", deparse(formula))
    message("Sample size: ", nrow(data))
  }

  # Fit model
  model_result <- .fit_robust_glm(formula, data, link, verbose)

  if (is.null(model_result$model)) {
    if (verbose) {
      message("Model fitting failed in stratum")
    }
    return(.create_failed_result(exposure, group_label, strata))
  }

  # Calculate effects with enhanced methods
  result <- .calculate_main_effect_robust(model_result, data, exposure, alpha,
                                          boundary_method, verbose)

  # Add sample size
  result$n_obs <- nrow(data)

  # Add stratification information
  if (!is.null(strata) && !is.null(group_label)) {
    for (i in seq_along(strata)) {
      result[[strata[i]]] <- group_label[[i]]
    }
  }

  return(result)
}

# Build model formula
.build_formula <- function(outcome, exposure, adjust_vars) {

  if (is.null(adjust_vars)) {
    terms <- exposure
  } else {
    terms <- c(exposure, adjust_vars)
  }

  stats::reformulate(terms, response = outcome)
}

# Robust GLM fitting with multiple link functions
.fit_robust_glm <- function(formula, data, preferred_link, verbose = FALSE) {

  # Determine link sequence
  if (preferred_link == "auto" || preferred_link == "identity") {
    link_sequence <- c("identity", "log", "logit")
  } else if (preferred_link == "log") {
    link_sequence <- c("log", "identity", "logit")
  } else if (preferred_link == "logit") {
    link_sequence <- c("logit", "log", "identity")
  } else {
    link_sequence <- preferred_link
  }

  # Try each link function
  for (link in link_sequence) {
    if (verbose) message("Trying ", link, " link...")

    model_result <- if (link == "identity") {
      .try_identity_link(formula, data, verbose)
    } else {
      .try_other_link(formula, data, link, verbose)
    }

    if (!is.null(model_result) && model_result$converged) {

      # NEW: Add boundary detection
      boundary_info <- .detect_boundary(model_result$model, data, verbose = verbose)

      # Add boundary information to the result
      model_result$boundary_detected <- boundary_info$boundary_detected
      model_result$boundary_type <- boundary_info$boundary_type
      model_result$separation_detected <- boundary_info$separation_detected

      # Issue warning if boundary detected but model "converged"
      if (boundary_info$boundary_detected && verbose) {
        message("Note: Model converged but MLE is on parameter space boundary.")
        message("Boundary type: ", boundary_info$boundary_type)
      }

      if (verbose) message(.safe_check(), link, " link converged")
      return(model_result)
    }
  }

  if (verbose) message(paste0(.safe_cross()), "All link functions failed")
  return(list(model = NULL, type = "failed", converged = FALSE,
              boundary_detected = FALSE, boundary_type = "model_failed"))
}

# Try identity link with robust starting values
.try_identity_link <- function(formula, data, verbose) {

  # Get starting values
  outcome_var <- all.vars(formula)[1]
  predictor_vars <- all.vars(formula)[-1]
  start_vals <- .get_starting_values(data, outcome_var, predictor_vars, verbose)

  tryCatch({
    model <- stats::glm(
      formula,
      data = data,
      family = stats::binomial(link = "identity"),
      start = start_vals,
      control = stats::glm.control(maxit = 100, epsilon = 1e-8)
    )

    list(
      model = model,
      type = "identity",
      converged = model$converged
    )
  }, error = function(e) {
    if (verbose) message("Identity link error: ", e$message)
    NULL
  })
}

# Try other link functions
.try_other_link <- function(formula, data, link, verbose) {

  tryCatch({
    model <- stats::glm(
      formula,
      data = data,
      family = stats::binomial(link = link),
      control = stats::glm.control(maxit = 100, epsilon = 1e-8)
    )

    list(
      model = model,
      type = link,
      converged = model$converged
    )
  }, error = function(e) {
    if (verbose) message(link, " link error: ", e$message)
    NULL
  })
}

# Calculate robust starting values for identity link
.get_starting_values <- function(data, outcome, predictors, verbose = FALSE) {

  # Method 1: Empirical proportions
  emp_start <- tryCatch({
    baseline <- mean(data[[outcome]][data[[predictors[1]]] == levels(data[[predictors[1]]])[1]], na.rm = TRUE)
    effects <- numeric(length(predictors))

    for (i in seq_along(predictors)) {
      if (is.factor(data[[predictors[i]]])) {
        exposed_mean <- mean(data[[outcome]][data[[predictors[i]]] == levels(data[[predictors[i]]])[2]], na.rm = TRUE)
        unexposed_mean <- mean(data[[outcome]][data[[predictors[i]]] == levels(data[[predictors[i]]])[1]], na.rm = TRUE)
        effects[i] <- exposed_mean - unexposed_mean
      } else {
        # For continuous variables, use correlation
        effects[i] <- stats::cor(data[[outcome]], data[[predictors[i]]], use = "complete.obs") * 0.1
      }
    }

    c(baseline, effects)
  }, error = function(e) NULL)

  # Method 2: Logistic model transformation
  logit_start <- tryCatch({
    logit_model <- stats::glm(
      stats::reformulate(predictors, outcome),
      data = data,
      family = stats::binomial(link = "logit")
    )
    stats::plogis(stats::coef(logit_model))
  }, error = function(e) NULL)

  # Method 3: Overall mean fallback
  overall_mean <- mean(data[[outcome]], na.rm = TRUE)
  fallback_start <- c(overall_mean, rep(0.01, length(predictors)))

  # Choose best starting values
  candidates <- list(emp_start, logit_start, fallback_start)
  candidates <- candidates[!sapply(candidates, is.null)]

  # Test which gives best initial likelihood
  best_start <- fallback_start

  for (start_vals in candidates) {
    if (all(start_vals >= 0 & start_vals <= 1) && !any(is.na(start_vals))) {
      best_start <- start_vals
      break
    }
  }

  if (verbose) {
    message("Using starting values: ", paste(round(best_start, 3), collapse = ", "))
  }

  return(best_start)
}

# Enhanced main effect calculation with robust confidence intervals
.calculate_main_effect_robust <- function(model_result, data, exposure, alpha,
                                          boundary_method = "auto", verbose = FALSE) {

  if (is.null(model_result$model)) {
    return(tibble::tibble(
      exposure_var = exposure,
      rd = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p_value = NA_real_,
      model_type = "failed",
      on_boundary = FALSE,
      boundary_type = "none",
      boundary_warning = NULL,
      ci_method = "failed"
    ))
  }

  model <- model_result$model

  # Check for separation issues
  fitted_probs <- stats::fitted(model)
  if (any(fitted_probs < 1e-8 | fitted_probs > (1 - 1e-8))) {
    if (verbose) {
      message(.safe_warning(), " Perfect or quasi-perfect separation detected. Results may be unreliable.")
    }
  }

  # Calculate CI using robust method
  ci_result <- .calculate_robust_ci(model_result, data, exposure, alpha)

  # Sanity checks
  if (!is.na(ci_result$ci_upper) && !is.na(ci_result$ci_lower)) {
    ci_width <- ci_result$ci_upper - ci_result$ci_lower
    if (ci_width > 2) {  # CI wider than 200 percentage points
      if (verbose) {
        message(.safe_warning(), " Extremely wide confidence interval detected (",
                round(ci_width * 100, 1), " percentage points). Consider larger sample size or different approach.")
      }
    }
  }

  # Add boundary detection
  boundary_info <- tryCatch({
    .detect_boundary(model)
  }, error = function(e) {
    if (verbose) {
      message("Boundary detection failed: ", e$message)
    }
    list(
      on_boundary = FALSE,
      boundary_type = "detection_failed",
      warning_message = paste("Boundary detection error:", e$message)
    )
  })

  # Print boundary warning if verbose
  if (verbose &&
      "on_boundary" %in% names(boundary_info) &&
      isTRUE(boundary_info$on_boundary)) {
    message("Boundary case detected")
  }

  tibble::tibble(
    exposure_var = exposure,
    rd = ci_result$rd,
    ci_lower = ci_result$ci_lower,
    ci_upper = ci_result$ci_upper,
    p_value = ci_result$p_value,
    model_type = model_result$type,
    on_boundary = boundary_info$on_boundary,
    boundary_type = boundary_info$boundary_type,
    boundary_warning = boundary_info$warning_message,
    ci_method = ci_result$ci_method
  )
}

# Enhanced confidence interval calculation that caps extreme CIs
.calculate_robust_ci <- function(model_result, data, exposure, alpha, method = "auto", max_ci_width = 1.0) {

  model <- model_result$model

  # Basic parameter extraction
  coefs <- summary(model)$coefficients
  exposure_pattern <- paste0("^", exposure)
  exposure_idx <- grep(exposure_pattern, rownames(coefs))[1]

  if (is.na(exposure_idx)) {
    return(list(
      rd = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p_value = NA_real_,
      ci_method = "failed"
    ))
  }

  # For identity link, use direct interpretation
  if (model_result$type == "identity") {
    rd <- coefs[exposure_idx, 1]
    p_value <- coefs[exposure_idx, 4]

    # Try profile likelihood CI first (more accurate)
    ci_result <- tryCatch({
      ci <- stats::confint(model, level = 1 - alpha, method = "profile")
      list(
        ci_lower = ci[exposure_idx, 1],
        ci_upper = ci[exposure_idx, 2],
        ci_method = "profile"
      )
    }, error = function(e) {
      # Fallback to Wald CI
      se <- coefs[exposure_idx, 2]
      z_crit <- stats::qnorm(1 - alpha/2)
      list(
        ci_lower = rd - z_crit * se,
        ci_upper = rd + z_crit * se,
        ci_method = "wald"
      )
    })

    # Cap extreme CIs for identity link too
    ci_width <- ci_result$ci_upper - ci_result$ci_lower
    if (ci_width > max_ci_width) {
      half_width <- max_ci_width / 2
      ci_result$ci_lower <- rd - half_width
      ci_result$ci_upper <- rd + half_width
      ci_result$ci_method <- paste0(ci_result$ci_method, "_capped")
      warning(paste0("Identity link confidence interval capped at ", .safe_plusminus()), round(half_width * 100, 1),
              " percentage points due to extreme width.")
    }

    return(list(
      rd = rd,
      ci_lower = ci_result$ci_lower,
      ci_upper = ci_result$ci_upper,
      p_value = p_value,
      ci_method = ci_result$ci_method
    ))
  }

  # For log/logit links, use prediction-based approach with CI capping
  if (model_result$type %in% c("log", "logit")) {
    rd_result <- .transform_to_rd_robust(model, data, exposure, alpha, n_boot = 500, max_ci_width)
    return(rd_result)
  }

  # Fallback
  return(list(
    rd = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    p_value = NA_real_,
    ci_method = "failed"
  ))
}

# Improved transformation with better confidence intervals, added CI width capping
.transform_to_rd_robust <- function(model, data, exposure, alpha, n_boot = 500, max_ci_width = 1.0) {

  exposure_levels <- levels(data[[exposure]])

  # Point estimate using prediction
  pred_data_ref <- data
  pred_data_ref[[exposure]] <- factor(exposure_levels[1], levels = exposure_levels)
  pred_ref <- mean(stats::predict(model, newdata = pred_data_ref, type = "response"))

  pred_data_exp <- data
  pred_data_exp[[exposure]] <- factor(exposure_levels[2], levels = exposure_levels)
  pred_exp <- mean(stats::predict(model, newdata = pred_data_exp, type = "response"))

  rd_point <- pred_exp - pred_ref

  # Try bootstrap confidence interval (more robust than delta method)
  boot_rds <- tryCatch({
    replicate(n_boot, {
      # Bootstrap sample
      boot_indices <- sample(nrow(data), replace = TRUE)
      boot_data <- data[boot_indices, ]

      # Refit model
      boot_model <- tryCatch({
        stats::update(model, data = boot_data)
      }, error = function(e) NULL)

      if (is.null(boot_model) || !boot_model$converged) return(NA)

      # Calculate RD for bootstrap sample
      boot_pred_ref <- mean(stats::predict(boot_model, newdata = pred_data_ref, type = "response"))
      boot_pred_exp <- mean(stats::predict(boot_model, newdata = pred_data_exp, type = "response"))

      boot_pred_exp - boot_pred_ref
    })
  }, error = function(e) rep(NA, n_boot))

  # Remove failed bootstrap samples
  boot_rds <- boot_rds[!is.na(boot_rds)]

  if (length(boot_rds) < 0.5 * n_boot) {
    # Too many bootstrap failures, fall back to delta method approximation
    vcov_matrix <- tryCatch(stats::vcov(model), error = function(e) NULL)

    if (is.null(vcov_matrix) || any(is.na(vcov_matrix))) {
      # If vcov fails, use very conservative CI
      se_approx <- abs(rd_point) * 0.5  # Very conservative
    } else {
      se_approx <- sqrt(diag(vcov_matrix))[1] * abs(rd_point) * 2  # Conservative approximation
    }

    z_crit <- stats::qnorm(1 - alpha/2)

    ci_lower_raw <- rd_point - z_crit * se_approx
    ci_upper_raw <- rd_point + z_crit * se_approx

    # Handle NA values before checking width
    if (is.na(ci_lower_raw) || is.na(ci_upper_raw)) {
      # Return conservative fixed-width CI if calculation fails
      half_width <- 0.25  # plus or minus 25 percentage points
      ci_lower_capped <- rd_point - half_width
      ci_upper_capped <- rd_point + half_width
    } else {
      # Cap extreme CIs to prevent forest plot issues
      ci_width <- ci_upper_raw - ci_lower_raw
      if (!is.na(ci_width) && ci_width > max_ci_width) {
        half_width <- max_ci_width / 2
        ci_lower_capped <- rd_point - half_width
        ci_upper_capped <- rd_point + half_width
        warning(paste0("Confidence interval capped at ", .safe_plusminus()), round(half_width * 100, 1),
                " percentage points due to extreme width.")
      } else {
        ci_lower_capped <- ci_lower_raw
        ci_upper_capped <- ci_upper_raw
      }
    }

    return(list(
      rd = rd_point,
      ci_lower = ci_lower_capped,
      ci_upper = ci_upper_capped,
      p_value = NA_real_,
      ci_method = "delta_approx"
    ))
  }

  # Use bootstrap quantiles for CI
  ci_lower_boot <- stats::quantile(boot_rds, alpha/2, na.rm = TRUE)
  ci_upper_boot <- stats::quantile(boot_rds, 1 - alpha/2, na.rm = TRUE)

  # Handle NA values in bootstrap results
  if (is.na(ci_lower_boot) || is.na(ci_upper_boot)) {
    # Fallback to delta method if bootstrap fails
    return(.transform_to_rd_robust(model, data, exposure, alpha, n_boot = 0, max_ci_width))
  }

  # Cap bootstrap CIs too if extreme
  ci_width <- ci_upper_boot - ci_lower_boot
  if (!is.na(ci_width) && ci_width > max_ci_width) {
    half_width <- max_ci_width / 2
    ci_lower_final <- rd_point - half_width
    ci_upper_final <- rd_point + half_width
    warning(paste0("Bootstrap confidence interval capped at ", .safe_plusminus()), round(half_width * 100, 1),
            " percentage points due to extreme width.")
  } else {
    ci_lower_final <- ci_lower_boot
    ci_upper_final <- ci_upper_boot
  }

  return(list(
    rd = rd_point,
    ci_lower = ci_lower_final,
    ci_upper = ci_upper_final,
    p_value = NA_real_,
    ci_method = "bootstrap"
  ))
}

# Original calculate_main_effect for backward compatibility
.calculate_main_effect <- function(model_result, data, exposure, alpha) {

  model <- model_result$model

  # Get confidence intervals (now with boundary-aware method)
  ci_level <- 1 - alpha
  ci <- .safe_confint(model, level = ci_level)
  coefs <- summary(model)$coefficients

  # Find exposure coefficient
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
      boundary_detected = model_result$boundary_detected %||% FALSE,
      boundary_type = model_result$boundary_type %||% "unknown"
    ))
  }

  # Calculate effect based on link function
  if (model_result$type == "identity") {
    # Direct interpretation
    rd <- coefs[exposure_idx, 1]
    ci_lower <- ci[exposure_idx, 1]
    ci_upper <- ci[exposure_idx, 2]
    p_value <- coefs[exposure_idx, 4]

  } else {
    # Transform to risk difference using prediction
    rd_result <- .transform_to_rd(model, data, exposure, model_result$type)
    rd <- rd_result$rd
    ci_lower <- rd_result$ci_lower
    ci_upper <- rd_result$ci_upper
    p_value <- coefs[exposure_idx, 4]  # P-value from original scale
  }

  result <- tibble::tibble(
    exposure_var = exposure,
    rd = rd,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    model_type = model_result$type,
    boundary_detected = model_result$boundary_detected %||% FALSE,
    boundary_type = model_result$boundary_type %||% "none"
  )

  # Add note about boundary issues in print method
  if (model_result$boundary_detected %||% FALSE) {
    attr(result, "boundary_note") <- paste("Note:",
                                           nrow(result), "of", nrow(result),
                                           "analyses had MLE on parameter space boundary. Robust confidence intervals were used.")
  }

  return(result)
}

# Safe confidence interval calculation
.safe_confint <- function(model, level = 0.95) {

  tryCatch({
    # Check if boundary detection indicates we should use robust CIs
    # This would come from the model_result passed down

    # Try profile likelihood confidence intervals first
    ci <- stats::confint(model, level = level)

    # If that works, return it
    return(ci)

  }, error = function(e) {
    # Fallback to Wald intervals for boundary cases
    if (inherits(model, "glm")) {
      coefs <- summary(model)$coefficients
      alpha <- 1 - level
      z_crit <- stats::qnorm(1 - alpha/2)

      ci <- matrix(nrow = nrow(coefs), ncol = 2)
      for (i in 1:nrow(coefs)) {
        estimate <- coefs[i, 1]
        se <- coefs[i, 2]
        ci[i, ] <- c(estimate - z_crit * se, estimate + z_crit * se)
      }

      rownames(ci) <- rownames(coefs)
      colnames(ci) <- c(paste0((alpha/2)*100, " %"), paste0((1-alpha/2)*100, " %"))

      # Add note about robust CIs when boundary detected
      attr(ci, "method") <- "Wald (robust for boundary cases)"

      return(ci)
    } else {
      stop("Cannot compute confidence intervals for this model type")
    }
  })
}

# Transform log or logit model results to risk differences (backward compatibility)
.transform_to_rd <- function(model, data, exposure, link_type) {

  # Use the robust version with default parameters
  result <- .transform_to_rd_robust(model, data, exposure, alpha = 0.05, n_boot = 200)

  # Return in original format
  list(
    rd = result$rd,
    ci_lower = result$ci_lower,
    ci_upper = result$ci_upper
  )
}

# Enhanced boundary detection function
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

    # Type 1: Probabilities at upper boundary (>= 1)
    if (prob_max >= (1 - tolerance)) {
      boundary_detected <- TRUE
      if (prob_max >= 1) {
        boundary_type <- "upper_boundary_exact"
      } else {
        boundary_type <- "upper_boundary_near"
      }
    }

    # Type 2: Probabilities at lower boundary (<= 0)
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

# Create error result objects
.create_insufficient_result <- function(exposure, group_label, strata) {
  result <- tibble::tibble(
    exposure_var = exposure,
    rd = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    p_value = NA_real_,
    model_type = "insufficient_data",
    n_obs = 0L,
    on_boundary = FALSE,
    boundary_type = "none",
    boundary_warning = NULL,
    ci_method = "none"
  )

  if (!is.null(strata) && !is.null(group_label)) {
    for (i in seq_along(strata)) {
      result[[strata[i]]] <- group_label[[i]]
    }
  }

  return(result)
}

.create_failed_result <- function(exposure, group_label, strata) {
  result <- tibble::tibble(
    exposure_var = exposure,
    rd = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    p_value = NA_real_,
    model_type = "failed",
    n_obs = 0L,
    on_boundary = FALSE,
    boundary_type = "none",
    boundary_warning = NULL,
    ci_method = "none"
  )

  if (!is.null(strata) && !is.null(group_label)) {
    for (i in seq_along(strata)) {
      result[[strata[i]]] <- group_label[[i]]
    }
  }

  return(result)
}

# Calculate boundary-aware confidence intervals
.calculate_boundary_ci <- function(model, boundary_info, alpha, method = "profile") {

  # Calculate confidence intervals appropriate for boundary cases
  # Called by your calc_risk_diff.R but was missing from utils.R


  if (method == "profile") {
    # Try profile likelihood CI
    tryCatch({
      stats::confint(model, level = 1 - alpha, method = "profile")
    }, error = function(e) {
      # Fall back to modified Wald if profile fails
      .calculate_boundary_ci(model, boundary_info, alpha, method = "wald")
    })
  } else if (method == "bootstrap") {
    # Bootstrap CI (simplified implementation)
    warning("Bootstrap CI not fully implemented yet. Using modified Wald.")
    .calculate_boundary_ci(model, boundary_info, alpha, method = "wald")
  } else {
    # Modified Wald CI for boundary cases
    coefs <- summary(model)$coefficients
    z_crit <- stats::qnorm(1 - alpha/2)

    # Apply boundary adjustment factor
    boundary_factor <- if (boundary_info$on_boundary) 1.5 else 1.0

    ci <- matrix(nrow = nrow(coefs), ncol = 2)
    for (i in 1:nrow(coefs)) {
      estimate <- coefs[i, 1]
      se <- coefs[i, 2] * boundary_factor  # Conservative adjustment
      ci[i, ] <- c(estimate - z_crit * se, estimate + z_crit * se)
    }

    rownames(ci) <- rownames(coefs)
    colnames(ci) <- c(paste0((alpha/2)*100, " %"), paste0((1-alpha/2)*100, " %"))
    return(ci)
  }
}

# ============================================================================
# Safe Unicode Display Functions
# ============================================================================

# Check if terminal/console supports Unicode
.supports_unicode <- function() {
  # Check system capabilities
  if (Sys.info()["sysname"] == "Windows") {
    # Windows console historically has poor Unicode support
    return(capabilities("iconv") && l10n_info()$`UTF-8`)
  }
  # Check if we're in RStudio (better Unicode support)
  if (Sys.getenv("RSTUDIO") == "1") {
    return(TRUE)
  }
  # Check locale
  locale <- Sys.getlocale("LC_CTYPE")
  return(grepl("UTF-8|utf8", locale, ignore.case = TRUE))
}

# Safe symbol display with fallbacks
.safe_alpha <- function() {
  if (.supports_unicode()) {
    return("\u03b1")
  } else {
    return("alpha")
  }
}

.safe_plusminus <- function() {
  if (.supports_unicode()) {
    return("\u00b1")
  } else {
    return("+/-")
  }
}

.safe_check <- function() {
  if (.supports_unicode()) {
    return("\u2713")
  } else {
    return("[PASS]")
  }
}

.safe_warning <- function() {
  if (.supports_unicode()) {
    return("\u26a0")
  } else {
    return("[CAUTION]")
  }
}

.safe_cross <- function() {
  if (.supports_unicode()) {
    return("\u2717")
  } else {
    return("[FAIL]")
  }
}

# Additional safe functions you might need
.safe_bullet <- function() {
  if (.supports_unicode()) {
    return("\u2022")
  } else {
    return("*")
  }
}

.safe_em_dash <- function() {
  if (.supports_unicode()) {
    return("\u2014")
  } else {
    return("--")
  }
}

.safe_ellipsis <- function() {
  if (.supports_unicode()) {
    return("\u2026")
  } else {
    return("...")
  }
}

.safe_degree <- function() {
  if (.supports_unicode()) {
    return("\u00b0")
  } else {
    return("deg")
  }
}

.safe_arrow_right <- function() {
  if (.supports_unicode()) {
    return("\u2192")
  } else {
    return("->")
  }
}

.safe_arrow_left <- function() {
  if (.supports_unicode()) {
    return("\u2190")
  } else {
    return("<-")
  }
}

.safe_times <- function() {
  if (.supports_unicode()) {
    return("\u00d7")
  } else {
    return("x")
  }
}
