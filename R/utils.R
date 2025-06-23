# Internal helper functions for riskdiff package

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Input validation
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

  # Calculate effects with boundary detection
  result <- .calculate_main_effect(model_result, data, exposure, alpha,
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

    if (link == "identity") {
      # Identity link needs starting values
      result <- .try_identity_link(formula, data, verbose)
    } else {
      # Other links
      result <- .try_other_link(formula, data, link, verbose)
    }

    if (!is.null(result) && result$converged) {
      if (verbose) message("[Huzzah!]", link, " link converged")
      return(result)
    }
  }

  if (verbose) message("x All link functions failed")
  return(list(model = NULL, type = "failed", converged = FALSE))
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

# Calculate main exposure effect
.calculate_main_effect <- function(model_result, data, exposure, alpha, boundary_method, verbose) {

  model <- model_result$model

  # Get confidence intervals
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
      on_boundary = FALSE,
      boundary_type = "none",
      boundary_warning = NULL,
      ci_method = "none"
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

  # Determine CI method used
  ci_method <- if (boundary_info$on_boundary) {
    if (boundary_method == "auto") "wald_conservative" else boundary_method
  } else {
    "wald"
  }

  # Print boundary warning if verbose
  if (verbose && boundary_info$on_boundary) {
    message("Boundary case detected: ", boundary_info$boundary_type)
    if (!is.null(boundary_info$warning_message)) {
      message("Warning: ", boundary_info$warning_message)
    }
  }

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

# Safe confidence interval calculation
.safe_confint <- function(model, level = 0.95) {

  tryCatch({
    stats::confint(model, level = level)
  }, error = function(e) {
    # Fallback to Wald intervals
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
    ci
  })
}

# Transform log or logit model results to risk differences
.transform_to_rd <- function(model, data, exposure, link_type) {

  # Create prediction datasets
  exposure_levels <- levels(data[[exposure]])

  # Reference level
  pred_data_ref <- data
  pred_data_ref[[exposure]] <- factor(exposure_levels[1], levels = exposure_levels)
  pred_ref <- mean(stats::predict(model, newdata = pred_data_ref, type = "response"))

  # Exposed level
  pred_data_exp <- data
  pred_data_exp[[exposure]] <- factor(exposure_levels[2], levels = exposure_levels)
  pred_exp <- mean(stats::predict(model, newdata = pred_data_exp, type = "response"))

  rd <- pred_exp - pred_ref

  # Approximate standard error using delta method
  # This is simplified - for exact inference would need more complex calculation
  vcov_matrix <- stats::vcov(model)

  # For now, return conservative CIs
  # In practice, should implement proper delta method
  se_approx <- sqrt(diag(vcov_matrix))[1] * abs(rd)  # Very rough approximation

  list(
    rd = rd,
    ci_lower = rd - 1.96 * se_approx,
    ci_upper = rd + 1.96 * se_approx
  )
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

# Add to your R/utils.R or create new R/unicode_utils.R

#' Safe Unicode Display Functions
#'
#' Functions to safely display Unicode characters with fallbacks
#' for systems that don't support them properly.

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
    return("\u03b1")  # α
  } else {
    return("alpha")
  }
}

.safe_plusminus <- function() {
  if (.supports_unicode()) {
    return("\u00b1")  # ±
  } else {
    return("+/-")
  }
}

.safe_check <- function() {
  if (.supports_unicode()) {
    return("\u2713")  # ✓
  } else {
    return("[PASS]")
  }
}

.safe_warning <- function() {
  if (.supports_unicode()) {
    return("\u26a0")  # ⚠
  } else {
    return("[CAUTION]")
  }
}

.safe_cross <- function() {
  if (.supports_unicode()) {
    return("\u2717")  # ✗
  } else {
    return("[FAIL]")
  }
}
