# Internal helper functions for IPTW functionality

#' Validate IPTW inputs
#' @noRd
.validate_iptw_inputs <- function(data, treatment, covariates, method, weight_type, trim_quantiles) {

  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }

  if (nrow(data) == 0) {
    stop("'data' contains no rows", call. = FALSE)
  }

  # Check variable names
  all_vars <- c(treatment, covariates)
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  # Check treatment variable
  if (!is.factor(data[[treatment]]) && !is.numeric(data[[treatment]]) && !is.logical(data[[treatment]])) {
    stop("Treatment variable must be factor, numeric (0/1), or logical", call. = FALSE)
  }

  # Convert treatment to factor if needed and check levels
  if (!is.factor(data[[treatment]])) {
    treatment_vals <- unique(data[[treatment]][!is.na(data[[treatment]])])
    if (!all(treatment_vals %in% c(0, 1, TRUE, FALSE))) {
      stop("Treatment variable must be binary (0/1, TRUE/FALSE, or two-level factor)", call. = FALSE)
    }
  } else {
    if (nlevels(data[[treatment]]) != 2) {
      stop("Treatment variable must have exactly 2 levels", call. = FALSE)
    }
  }

  # Check method
  if (!method %in% c("logistic", "probit", "cloglog")) {
    stop("'method' must be one of: 'logistic', 'probit', 'cloglog'", call. = FALSE)
  }

  # Check weight_type
  if (!weight_type %in% c("ATE", "ATT", "ATC")) {
    stop("'weight_type' must be one of: 'ATE', 'ATT', 'ATC'", call. = FALSE)
  }

  # Check trim_quantiles
  if (!is.numeric(trim_quantiles) || length(trim_quantiles) != 2) {
    stop("'trim_quantiles' must be a numeric vector of length 2", call. = FALSE)
  }

  if (any(trim_quantiles < 0) || any(trim_quantiles > 1) || trim_quantiles[1] >= trim_quantiles[2]) {
    stop("'trim_quantiles' must be between 0 and 1 with first < second", call. = FALSE)
  }
}

#' Validate IPTW risk difference inputs
#' @noRd
.validate_iptw_rd_inputs <- function(data, outcome, treatment, covariates, weight_type, alpha) {

  # Basic validation (reuse existing function)
  .validate_inputs(data, outcome, treatment, NULL, NULL, "auto", alpha)

  # Additional IPTW-specific validation
  if (!weight_type %in% c("ATE", "ATT", "ATC")) {
    stop("'weight_type' must be one of: 'ATE', 'ATT', 'ATC'", call. = FALSE)
  }

  # Check covariates
  missing_covs <- setdiff(covariates, names(data))
  if (length(missing_covs) > 0) {
    stop("Covariates not found in data: ", paste(missing_covs, collapse = ", "), call. = FALSE)
  }
}

#' Prepare data for IPTW analysis
#' @noRd
.prepare_iptw_data <- function(data, treatment, covariates) {

  # Select relevant variables
  all_vars <- c(treatment, covariates)
  data_subset <- data[all_vars]

  # Convert treatment to factor if needed
  if (!is.factor(data_subset[[treatment]])) {
    if (is.logical(data_subset[[treatment]])) {
      data_subset[[treatment]] <- factor(data_subset[[treatment]], levels = c(FALSE, TRUE), labels = c("0", "1"))
    } else {
      data_subset[[treatment]] <- factor(data_subset[[treatment]])
    }
  }

  # Ensure treatment has proper reference level (first level = control)
  treatment_levels <- levels(data_subset[[treatment]])
  if (length(treatment_levels) != 2) {
    stop("Treatment must have exactly 2 levels after conversion", call. = FALSE)
  }

  # Remove rows with missing data
  complete_cases <- complete.cases(data_subset)
  data_clean <- data_subset[complete_cases, , drop = FALSE]

  if (nrow(data_clean) == 0) {
    stop("No complete cases found", call. = FALSE)
  }

  # Check for sufficient variation in treatment
  treatment_table <- table(data_clean[[treatment]])
  if (any(treatment_table < 10)) {
    warning("Small treatment group detected (n < 10). Results may be unstable.", call. = FALSE)
  }

  return(data_clean)
}

#' Fit propensity score model
#' @noRd
.fit_propensity_model <- function(data, treatment, covariates, method) {

  # Build formula
  formula <- stats::reformulate(covariates, response = treatment)

  # Fit model based on method
  if (method == "logistic") {
    family <- stats::binomial(link = "logit")
  } else if (method == "probit") {
    family <- stats::binomial(link = "probit")
  } else if (method == "cloglog") {
    family <- stats::binomial(link = "cloglog")
  }

  model <- tryCatch({
    stats::glm(formula, data = data, family = family)
  }, error = function(e) {
    stop("Propensity score model failed to converge: ", e$message, call. = FALSE)
  })

  # Check for convergence
  if (!model$converged) {
    warning("Propensity score model did not converge. Results may be unreliable.", call. = FALSE)
  }

  # Check for separation
  fitted_ps <- stats::fitted(model)
  if (any(fitted_ps < 0.001) || any(fitted_ps > 0.999)) {
    warning("Extreme propensity scores detected. Consider model modification or sample restriction.", call. = FALSE)
  }

  return(model)
}

#' Calculate IPTW weights
#' @noRd
.calculate_iptw_weights <- function(treatment, ps, weight_type, stabilize) {

  # Convert treatment to numeric (1 = treated, 0 = control)
  if (is.factor(treatment)) {
    treatment_numeric <- as.numeric(treatment) - 1
  } else {
    treatment_numeric <- as.numeric(treatment)
  }

  # Calculate weights based on type
  if (weight_type == "ATE") {
    # ATE weights: 1/pi for treated, 1/(1-pi) for controls
    weights <- ifelse(treatment_numeric == 1, 1/ps, 1/(1-ps))

  } else if (weight_type == "ATT") {
    # ATT weights: 1 for treated, pi/(1-pi) for controls
    weights <- ifelse(treatment_numeric == 1, 1, ps/(1-ps))

  } else if (weight_type == "ATC") {
    # ATC weights: (1-pi)/pi for treated, 1 for controls
    weights <- ifelse(treatment_numeric == 1, (1-ps)/ps, 1)
  }

  # Stabilize weights if requested
  if (stabilize) {
    p_treat <- mean(treatment_numeric)

    if (weight_type == "ATE") {
      stabilized_weights <- ifelse(treatment_numeric == 1, p_treat/ps, (1-p_treat)/(1-ps))
    } else if (weight_type == "ATT") {
      stabilized_weights <- ifelse(treatment_numeric == 1, 1, p_treat * ps/((1-p_treat) * (1-ps)))
    } else if (weight_type == "ATC") {
      stabilized_weights <- ifelse(treatment_numeric == 1, (1-p_treat) * (1-ps)/(p_treat * ps), 1)
    }

    weights <- stabilized_weights
  }

  return(weights)
}

#' Trim extreme weights
#' @noRd
.trim_weights <- function(weights, trim_quantiles, verbose = FALSE) {

  # Calculate trimming bounds
  lower_bound <- stats::quantile(weights, trim_quantiles[1], na.rm = TRUE)
  upper_bound <- stats::quantile(weights, trim_quantiles[2], na.rm = TRUE)

  # Count trimmed observations
  n_trimmed_low <- sum(weights < lower_bound, na.rm = TRUE)
  n_trimmed_high <- sum(weights > upper_bound, na.rm = TRUE)

  if (verbose && (n_trimmed_low > 0 || n_trimmed_high > 0)) {
    cat("Trimmed", n_trimmed_low, "weights below", round(lower_bound, 3),
        "and", n_trimmed_high, "weights above", round(upper_bound, 3), "\n")
  }

  # Apply trimming
  weights_trimmed <- pmax(lower_bound, pmin(upper_bound, weights))

  return(weights_trimmed)
}

#' Calculate IPTW diagnostics
#' @noRd
.calculate_iptw_diagnostics <- function(data, treatment, covariates, weights, ps) {

  # Treatment as numeric
  treatment_numeric <- as.numeric(data[[treatment]]) - 1

  # Calculate effective sample size
  effective_n <- sum(weights)^2 / sum(weights^2)

  # Calculate covariate balance
  balance_results <- purrr::map_dfr(covariates, function(var) {
    .calculate_standardized_difference(data[[var]], treatment_numeric, weights)
  })
  balance_results$variable <- covariates

  # Reorder columns
  balance_table <- balance_results[, c("variable", "mean_control_unwt", "mean_treated_unwt",
                                       "std_diff_unweighted", "mean_control_wt", "mean_treated_wt",
                                       "std_diff_weighted")]

  # Summary statistics
  balance_summary <- data.frame(
    max_std_diff_unweighted = max(abs(balance_table$std_diff_unweighted), na.rm = TRUE),
    max_std_diff_weighted = max(abs(balance_table$std_diff_weighted), na.rm = TRUE),
    mean_abs_std_diff_unweighted = mean(abs(balance_table$std_diff_unweighted), na.rm = TRUE),
    mean_abs_std_diff_weighted = mean(abs(balance_table$std_diff_weighted), na.rm = TRUE)
  )

  # Propensity score overlap
  ps_overlap <- list(
    min_ps = min(ps),
    max_ps = max(ps),
    ps_in_treated = ps[treatment_numeric == 1],
    ps_in_control = ps[treatment_numeric == 0]
  )

  return(list(
    effective_n = effective_n,
    balance_table = balance_table,
    balance_summary = balance_summary,
    ps_overlap = ps_overlap,
    weight_summary = summary(weights)
  ))
}

#' Calculate standardized difference for a single variable
#' @noRd
.calculate_standardized_difference <- function(variable, treatment, weights) {

  # Handle factor variables differently
  if (is.factor(variable)) {
    # For factors, compare proportions of the most common level
    # Convert to numeric (1 for most common level, 0 otherwise)
    most_common_level <- names(sort(table(variable), decreasing = TRUE))[1]
    variable_binary <- as.numeric(variable == most_common_level)

    # Calculate as if binary variable
    return(.calculate_standardized_difference_numeric(variable_binary, treatment, weights))
  } else {
    # For numeric variables, use standard approach
    return(.calculate_standardized_difference_numeric(variable, treatment, weights))
  }
}

#' Calculate standardized difference for numeric variables
#' @noRd
.calculate_standardized_difference_numeric <- function(variable, treatment, weights) {

  # Unweighted means
  mean_control_unwt <- mean(variable[treatment == 0], na.rm = TRUE)
  mean_treated_unwt <- mean(variable[treatment == 1], na.rm = TRUE)

  # Unweighted pooled standard deviation
  var_control_unwt <- stats::var(variable[treatment == 0], na.rm = TRUE)
  var_treated_unwt <- stats::var(variable[treatment == 1], na.rm = TRUE)

  # Handle cases where variance is 0 or NA
  if (is.na(var_control_unwt) || is.na(var_treated_unwt) ||
      (var_control_unwt == 0 && var_treated_unwt == 0)) {
    pooled_sd_unwt <- 1  # Avoid division by zero
  } else {
    pooled_sd_unwt <- sqrt((var_control_unwt + var_treated_unwt) / 2)
  }

  # Unweighted standardized difference
  std_diff_unweighted <- (mean_treated_unwt - mean_control_unwt) / pooled_sd_unwt

  # Weighted means
  weights_control <- weights[treatment == 0]
  weights_treated <- weights[treatment == 1]

  mean_control_wt <- stats::weighted.mean(variable[treatment == 0], weights_control, na.rm = TRUE)
  mean_treated_wt <- stats::weighted.mean(variable[treatment == 1], weights_treated, na.rm = TRUE)

  # Weighted variances
  var_control_wt <- .weighted_var(variable[treatment == 0], weights_control)
  var_treated_wt <- .weighted_var(variable[treatment == 1], weights_treated)

  # Handle cases where weighted variance is 0 or NA
  if (is.na(var_control_wt) || is.na(var_treated_wt) ||
      (var_control_wt == 0 && var_treated_wt == 0)) {
    pooled_sd_wt <- 1  # Avoid division by zero
  } else {
    pooled_sd_wt <- sqrt((var_control_wt + var_treated_wt) / 2)
  }

  # Weighted standardized difference
  std_diff_weighted <- (mean_treated_wt - mean_control_wt) / pooled_sd_wt

  return(data.frame(
    mean_control_unwt = mean_control_unwt,
    mean_treated_unwt = mean_treated_unwt,
    std_diff_unweighted = std_diff_unweighted,
    mean_control_wt = mean_control_wt,
    mean_treated_wt = mean_treated_wt,
    std_diff_weighted = std_diff_weighted
  ))
}

#' Calculate weighted variance
#' @noRd
.weighted_var <- function(x, w) {
  # Remove missing values
  complete_idx <- !is.na(x) & !is.na(w) & w > 0
  x <- x[complete_idx]
  w <- w[complete_idx]

  if (length(x) < 2) return(0)  # Return 0 instead of NA for edge cases

  # Weighted mean
  w_mean <- stats::weighted.mean(x, w)

  # Weighted variance (using reliability weights formula)
  sum_w <- sum(w)
  sum_w_sq <- sum(w^2)

  # Avoid division by zero
  denominator <- sum_w - sum_w_sq/sum_w
  if (denominator <= 0) return(0)

  weighted_var <- sum(w * (x - w_mean)^2) / denominator

  return(max(0, weighted_var))  # Ensure non-negative
}

#' Calculate IPTW risk difference with analytic standard errors
#' @noRd
.calc_iptw_rd_analytic <- function(data, outcome, treatment, weights, alpha, verbose) {

  # Convert treatment to numeric
  if (is.factor(data[[treatment]])) {
    treatment_numeric <- as.numeric(data[[treatment]]) - 1
  } else {
    treatment_numeric <- as.numeric(data[[treatment]])
  }

  # Convert outcome to numeric (robust conversion)
  if (is.factor(data[[outcome]])) {
    # If factor, convert to numeric (assuming levels are "0", "1" or similar)
    outcome_numeric <- as.numeric(as.character(data[[outcome]]))
  } else if (is.logical(data[[outcome]])) {
    # If logical, convert TRUE/FALSE to 1/0
    outcome_numeric <- as.numeric(data[[outcome]])
  } else {
    # If already numeric
    outcome_numeric <- as.numeric(data[[outcome]])
  }

  # Check for conversion issues
  if (verbose) {
    cat("Outcome conversion check:\n")
    cat("Original outcome:", data[[outcome]][1:5], "\n")
    cat("Converted outcome:", outcome_numeric[1:5], "\n")
    cat("Any NAs in outcome?", any(is.na(outcome_numeric)), "\n")
  }

  # Check for sufficient data in each group
  n_treated <- sum(treatment_numeric == 1)
  n_control <- sum(treatment_numeric == 0)

  if (n_treated < 2 || n_control < 2) {
    if (verbose) cat("Insufficient data in treatment groups\n")
    return(list(
      rd = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p_value = NA_real_,
      risk_treated = NA_real_,
      risk_control = NA_real_,
      se = NA_real_
    ))
  }

  # Extract outcomes and weights by treatment group
  weights_treated <- weights[treatment_numeric == 1]
  weights_control <- weights[treatment_numeric == 0]
  outcomes_treated <- outcome_numeric[treatment_numeric == 1]
  outcomes_control <- outcome_numeric[treatment_numeric == 0]

  # NOW we can do verbose output since all variables are defined
  if (verbose) {
    cat("Sample sizes: Treated =", n_treated, ", Control =", n_control, "\n")
    cat("Treated outcomes:", outcomes_treated, "\n")
    cat("Treated weights:", weights_treated, "\n")
    cat("Control outcomes:", outcomes_control, "\n")
    cat("Control weights:", weights_control, "\n")
    cat("All finite (treated):", all(is.finite(outcomes_treated)), all(is.finite(weights_treated)), "\n")
    cat("All finite (control):", all(is.finite(outcomes_control)), all(is.finite(weights_control)), "\n")
  }

  # Calculate weighted means with error handling
  risk_treated <- tryCatch({
    if (length(outcomes_treated) > 0 && length(weights_treated) > 0 &&
        all(is.finite(outcomes_treated)) && all(is.finite(weights_treated)) &&
        sum(weights_treated) > 0) {
      sum(outcomes_treated * weights_treated) / sum(weights_treated)
    } else {
      NA_real_
    }
  }, error = function(e) {
    if (verbose) cat("Error calculating treated risk:", e$message, "\n")
    NA_real_
  })

  risk_control <- tryCatch({
    if (length(outcomes_control) > 0 && length(weights_control) > 0 &&
        all(is.finite(outcomes_control)) && all(is.finite(weights_control)) &&
        sum(weights_control) > 0) {
      sum(outcomes_control * weights_control) / sum(weights_control)
    } else {
      NA_real_
    }
  }, error = function(e) {
    if (verbose) cat("Error calculating control risk:", e$message, "\n")
    NA_real_
  })

  if (verbose) {
    cat("Risk in treated:", risk_treated, "\n")
    cat("Risk in control:", risk_control, "\n")
  }

  if (is.na(risk_treated) || is.na(risk_control)) {
    if (verbose) cat("Failed to calculate weighted means\n")
    return(list(
      rd = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p_value = NA_real_,
      risk_treated = risk_treated,
      risk_control = risk_control,
      se = NA_real_
    ))
  }

  rd <- risk_treated - risk_control

  # Simplified standard error calculation for small samples
  # Use basic weighted variance formula

  # Weighted residuals for treated group
  var_treated <- tryCatch({
    if (length(weights_treated) > 1) {
      sum(weights_treated^2 * (outcomes_treated - risk_treated)^2) / sum(weights_treated)^2
    } else {
      0
    }
  }, error = function(e) 0)

  # Weighted residuals for control group
  var_control <- tryCatch({
    if (length(weights_control) > 1) {
      sum(weights_control^2 * (outcomes_control - risk_control)^2) / sum(weights_control)^2
    } else {
      0
    }
  }, error = function(e) 0)

  # Standard error of the difference
  se_rd <- sqrt(var_treated + var_control)

  # Handle edge case where SE is 0 or very small
  if (is.na(se_rd) || se_rd < 1e-10) {
    se_rd <- abs(rd) * 0.1  # Conservative estimate: 10% of effect size
    if (se_rd < 0.01) se_rd <- 0.01  # Minimum SE
  }

  # Confidence interval
  z_crit <- stats::qnorm(1 - alpha/2)
  ci_lower <- rd - z_crit * se_rd
  ci_upper <- rd + z_crit * se_rd

  # P-value (two-sided test)
  z_stat <- rd / se_rd
  p_value <- 2 * (1 - stats::pnorm(abs(z_stat)))

  if (verbose) {
    cat("Risk difference:", rd, "\n")
    cat("Standard error:", se_rd, "\n")
    cat("Z-statistic:", z_stat, "\n")
    cat("P-value:", p_value, "\n")
  }

  return(list(
    rd = rd,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    risk_treated = risk_treated,
    risk_control = risk_control,
    se = se_rd
  ))
}

#' Calculate IPTW risk difference with bootstrap confidence intervals
#' @noRd
.calc_iptw_rd_bootstrap <- function(data, outcome, treatment, weights, alpha, boot_n, verbose) {

  # Original estimate
  original_result <- .calc_iptw_rd_analytic(data, outcome, treatment, weights, alpha, verbose = FALSE)

  if (verbose) {
    cat("Calculating bootstrap confidence intervals with", boot_n, "replicates...\n")
  }

  # Bootstrap replicates
  boot_rds <- numeric(boot_n)

  for (i in 1:boot_n) {
    # Bootstrap sample
    boot_idx <- sample(nrow(data), replace = TRUE)
    boot_data <- data[boot_idx, ]
    boot_weights <- weights[boot_idx]

    # Calculate RD for bootstrap sample
    tryCatch({
      boot_result <- .calc_iptw_rd_analytic(boot_data, outcome, treatment, boot_weights, alpha, verbose = FALSE)
      boot_rds[i] <- boot_result$rd
    }, error = function(e) {
      boot_rds[i] <- NA
    })
  }

  # Remove failed bootstrap samples
  boot_rds <- boot_rds[!is.na(boot_rds)]

  if (length(boot_rds) < boot_n * 0.5) {
    warning("More than 50% of bootstrap samples failed. Results may be unreliable.", call. = FALSE)
  }

  # Bootstrap confidence interval (percentile method)
  ci_lower <- stats::quantile(boot_rds, alpha/2, na.rm = TRUE)
  ci_upper <- stats::quantile(boot_rds, 1 - alpha/2, na.rm = TRUE)

  # P-value based on bootstrap distribution
  # Two-sided test: proportion of bootstrap estimates more extreme than 0
  p_value <- min(1, 2 * min(
    mean(boot_rds >= 0, na.rm = TRUE),
    mean(boot_rds <= 0, na.rm = TRUE)
  ))

  if (verbose) {
    cat("Successful bootstrap replicates:", length(boot_rds), "/", boot_n, "\n")
    cat("Bootstrap SE:", round(stats::sd(boot_rds, na.rm = TRUE), 4), "\n")
  }

  return(list(
    rd = original_result$rd,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    risk_treated = original_result$risk_treated,
    risk_control = original_result$risk_control,
    boot_estimates = boot_rds
  ))
}
