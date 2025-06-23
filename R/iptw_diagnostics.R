#' Create Balance Plots for IPTW Analysis
#'
#' @description
#' Creates visualizations to assess covariate balance before and after IPTW weighting.
#' Includes love plots (standardized differences) and propensity score distribution plots.
#'
#' @param iptw_result An iptw_result object from calc_iptw_weights()
#' @param plot_type Type of plot: "love" for standardized differences, "ps" for propensity score distributions, or "both"
#' @param threshold Threshold for acceptable standardized difference (default: 0.1)
#' @param save_plots Whether to save plots to files (default: FALSE)
#' @param plot_dir Directory to save plots if save_plots=TRUE (default: "plots")
#'
#' @return
#' A ggplot object (if plot_type is "love" or "ps") or a list of ggplot objects (if plot_type is "both").
#' If ggplot2 is not available, returns a message and creates base R plots.
#'
#' @details
#' ## Love Plot
#'
#' Shows standardized differences for each covariate before and after weighting.
#' Points represent standardized differences, with lines connecting before/after values.
#' Horizontal lines show common thresholds (0.1, 0.25) for acceptable balance.
#'
#' ## Propensity Score Plot
#'
#' Shows distributions of propensity scores by treatment group before and after weighting.
#' Good overlap indicates positivity assumption is met.
#'
#' @examples
#' \donttest{
#' data(cachar_sample)
#'
#' # Calculate IPTW weights
#' iptw_result <- calc_iptw_weights(
#'   data = cachar_sample,
#'   treatment = "areca_nut",
#'   covariates = c("age", "sex", "residence", "smoking")
#' )
#'
#' # Create balance plots
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plots <- create_balance_plots(iptw_result, plot_type = "both")
#'   print(plots$love_plot)
#'   print(plots$ps_plot)
#' }
#' }
#'
#' @export
create_balance_plots <- function(iptw_result,
                                 plot_type = "both",
                                 threshold = 0.1,
                                 save_plots = FALSE,
                                 plot_dir = "plots") {

  if (!inherits(iptw_result, "iptw_result")) {
    stop("Input must be an iptw_result object from calc_iptw_weights()", call. = FALSE)
  }

  if (!plot_type %in% c("love", "ps", "both")) {
    stop("plot_type must be 'love', 'ps', or 'both'", call. = FALSE)
  }

  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not available. Creating base R plots instead.")
    return(.create_base_balance_plots(iptw_result, plot_type, threshold))
  }

  plots <- list()

  # Love plot (standardized differences)
  if (plot_type %in% c("love", "both")) {
    plots$love_plot <- .create_love_plot(iptw_result, threshold)
  }

  # Propensity score distribution plot
  if (plot_type %in% c("ps", "both")) {
    plots$ps_plot <- .create_ps_plot(iptw_result)
  }

  # Save plots if requested
  if (save_plots) {
    .save_balance_plots(plots, plot_dir)
  }

  # Return appropriate object
  if (plot_type == "both") {
    return(plots)
  } else if (plot_type == "love") {
    return(plots$love_plot)
  } else {
    return(plots$ps_plot)
  }
}

#' Check IPTW Assumptions
#'
#' @description
#' Provides diagnostic checks for key IPTW assumptions including positivity,
#' balance, and model specification. Returns a comprehensive summary with
#' recommendations for potential issues.
#'
#' @param iptw_result An iptw_result object from calc_iptw_weights()
#' @param balance_threshold Threshold for acceptable standardized difference (default: 0.1)
#' @param extreme_weight_threshold Threshold for flagging extreme weights (default: 10)
#' @param verbose Whether to print detailed diagnostics (default: TRUE)
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{overall_assessment}{Character indicating "PASS", "CAUTION", or "FAIL"}
#'   \item{positivity}{List with positivity checks and recommendations}
#'   \item{balance}{List with balance assessment and problematic variables}
#'   \item{weights}{List with weight distribution diagnostics}
#'   \item{recommendations}{Character vector of specific recommendations}
#' }
#'
#' @examples
#' data(cachar_sample)
#'
#' iptw_result <- calc_iptw_weights(
#'   data = cachar_sample,
#'   treatment = "areca_nut",
#'   covariates = c("age", "sex", "residence", "smoking")
#' )
#'
#' # Check assumptions
#' assumptions <- check_iptw_assumptions(iptw_result)
#' print(assumptions$overall_assessment)
#' print(assumptions$recommendations)
#'
#' @export
check_iptw_assumptions <- function(iptw_result,
                                   balance_threshold = 0.1,
                                   extreme_weight_threshold = 10,
                                   verbose = TRUE) {

  if (!inherits(iptw_result, "iptw_result")) {
    stop("Input must be an iptw_result object from calc_iptw_weights()", call. = FALSE)
  }

  # Initialize results
  issues <- character(0)
  recommendations <- character(0)

  # 1. Positivity Assessment
  positivity <- .check_positivity(iptw_result)
  if (positivity$violation) {
    issues <- c(issues, "positivity")
    recommendations <- c(recommendations, positivity$recommendations)
  }

  # 2. Balance Assessment
  balance <- .check_balance(iptw_result, balance_threshold)
  if (balance$poor_balance) {
    issues <- c(issues, "balance")
    recommendations <- c(recommendations, balance$recommendations)
  }

  # 3. Weight Distribution Assessment
  weights_check <- .check_weight_distribution(iptw_result, extreme_weight_threshold)
  if (weights_check$extreme_weights) {
    issues <- c(issues, "extreme_weights")
    recommendations <- c(recommendations, weights_check$recommendations)
  }

  # Overall assessment
  if (length(issues) == 0) {
    overall <- "PASS"
  } else if (length(issues) == 1 && !("positivity" %in% issues)) {
    overall <- "CAUTION"
  } else {
    overall <- "FAIL"
  }

  # Compile results
  result <- list(
    overall_assessment = overall,
    positivity = positivity,
    balance = balance,
    weights = weights_check,
    recommendations = unique(recommendations),
    issues_detected = issues
  )

  if (verbose) {
    .print_assumption_results(result)
  }

  class(result) <- "iptw_assumptions"
  return(result)
}

#' Summary Method for IPTW Risk Difference Results
#'
#' @description
#' Provides a comprehensive summary of IPTW risk difference analysis including
#' effect estimates, diagnostics, and interpretation guidance.
#'
#' @param object A riskdiff_iptw_result object
#' @param ... Additional arguments (currently ignored)
#'
#' @return
#' Invisibly returns the input object. Called primarily for side effects (printing summary).
#'
#' @examples
#' data(cachar_sample)
#'
#' rd_iptw <- calc_risk_diff_iptw(
#'   data = cachar_sample,
#'   outcome = "abnormal_screen",
#'   treatment = "areca_nut",
#'   covariates = c("age", "sex", "residence", "smoking")
#' )
#'
#' summary(rd_iptw)
#'
#' @export
summary.riskdiff_iptw_result <- function(object, ...) {

  cat("IPTW Risk Difference Analysis Summary\n")
  cat("====================================\n\n")

  # Basic information
  cat("Treatment Variable:", object$treatment_var, "\n")
  cat("Weight Type:", object$weight_type, "weights\n")
  cat("Effective Sample Size:", round(object$effective_n, 1), "\n\n")

  # Effect estimates
  cat("Effect Estimates:\n")
  cat("-----------------\n")
  cat("Risk in Treated Group:", scales::percent(object$risk_treated, accuracy = 0.1), "\n")
  cat("Risk in Control Group:", scales::percent(object$risk_control, accuracy = 0.1), "\n")
  cat("Risk Difference:", scales::percent(object$rd_iptw, accuracy = 0.01), "\n")

  alpha <- attr(object, "alpha") %||% 0.05
  ci_level <- scales::percent(1 - alpha)
  cat(ci_level, "Confidence Interval: (",
      scales::percent(object$ci_lower, accuracy = 0.01), ", ",
      scales::percent(object$ci_upper, accuracy = 0.01), ")\n", sep = "")

  # Statistical significance
  p_val <- if (is.na(object$p_value)) 1.0 else object$p_value  # Default to 1 if NA
  cat("P-value:", ifelse(p_val < 0.001, "<0.001", sprintf("%.3f", p_val)), "\n")

  if (!is.na(p_val) && p_val < 0.05) {
    cat("Result: Statistically significant at ", .safe_alpha(), " = ", alpha, "\n", sep = "")
  } else {
    cat("Result: Not statistically significant at ", .safe_alpha(), " = ", alpha, "\n", sep = "")
  }

  # Interpretation
  cat("\nInterpretation:\n")
  cat("---------------\n")

  rd_percent <- object$rd_iptw * 100
  interpretation <- if (abs(rd_percent) < 1) {
    "small"
  } else if (abs(rd_percent) < 5) {
    "moderate"
  } else {
    "large"
  }

  direction <- if (object$rd_iptw > 0) "increased" else "decreased"

  cat("The", object$weight_type, "shows a", interpretation, direction, "risk of",
      sprintf("%.1f", abs(rd_percent)), "percentage points.\n")

  # Clinical significance note
  cat("\nNote: Statistical significance does not necessarily imply clinical\n")
  cat("significance. Consider the magnitude of effect in context.\n")

  # Bootstrap note if applicable
  if (attr(object, "bootstrap") %||% FALSE) {
    cat("\nConfidence intervals calculated using", attr(object, "boot_n"), "bootstrap replicates.\n")
  }

  invisible(object)
}

# Helper functions for diagnostics

#' Create love plot with ggplot2
#' @noRd
.create_love_plot <- function(iptw_result, threshold) {

  balance_data <- iptw_result$diagnostics$balance_table

  # Prepare data for plotting
  plot_data <- data.frame(
    variable = rep(balance_data$variable, 2),
    std_diff = c(balance_data$std_diff_unweighted, balance_data$std_diff_weighted),
    timing = factor(rep(c("Before Weighting", "After Weighting"), each = nrow(balance_data)),
                    levels = c("Before Weighting", "After Weighting"))
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = std_diff, y = variable, color = timing)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(group = variable), color = "gray50", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = c(-threshold, threshold),
                        linetype = "dashed", color = "red", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "solid", color = "black", alpha = 0.3) +
    ggplot2::scale_color_manual(values = c("Before Weighting" = "#E31A1C", "After Weighting" = "#1F78B4")) +
    ggplot2::labs(
      title = "Covariate Balance Before and After IPTW",
      subtitle = paste("Dashed lines show", .safe_plusminus(), threshold, "threshold"),
      x = "Standardized Difference",
      y = "Covariates",
      color = "Timing"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)
    )

  return(p)
}

#' Create propensity score distribution plot
#' @noRd
.create_ps_plot <- function(iptw_result) {

  # Prepare data
  treatment_var <- names(iptw_result$data)[sapply(iptw_result$data, is.factor) &
                                             sapply(iptw_result$data, function(x) nlevels(x) == 2)][1]

  plot_data <- data.frame(
    ps = iptw_result$ps,
    treatment = iptw_result$data[[treatment_var]],
    weights = iptw_result$weights
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = ps, fill = treatment)) +
    ggplot2::geom_density(alpha = 0.6) +
    ggplot2::scale_fill_manual(values = c("#E31A1C", "#1F78B4")) +
    ggplot2::labs(
      title = "Propensity Score Distribution by Treatment Group",
      subtitle = "Good overlap indicates positivity assumption is met",
      x = "Propensity Score",
      y = "Density",
      fill = "Treatment"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)
    )

  return(p)
}

#' Create base R balance plots
#' @noRd
.create_base_balance_plots <- function(iptw_result, plot_type, threshold) {

  if (plot_type %in% c("love", "both")) {
    # Love plot in base R
    balance_data <- iptw_result$diagnostics$balance_table

    par(mfrow = c(1, 1), mar = c(5, 8, 4, 2))

    plot(balance_data$std_diff_unweighted, 1:nrow(balance_data),
         col = "red", pch = 16, cex = 1.2,
         xlim = range(c(balance_data$std_diff_unweighted, balance_data$std_diff_weighted), na.rm = TRUE),
         ylim = c(0.5, nrow(balance_data) + 0.5),
         xlab = "Standardized Difference", ylab = "",
         main = "Covariate Balance Before and After IPTW",
         yaxt = "n")

    points(balance_data$std_diff_weighted, 1:nrow(balance_data),
           col = "blue", pch = 16, cex = 1.2)

    # Connect points with lines
    for (i in 1:nrow(balance_data)) {
      lines(c(balance_data$std_diff_unweighted[i], balance_data$std_diff_weighted[i]),
            c(i, i), col = "gray50", lwd = 0.5)
    }

    # Add threshold lines
    abline(v = c(-threshold, threshold), lty = 2, col = "red", lwd = 1)
    abline(v = 0, lty = 1, col = "black", lwd = 0.5)

    # Add y-axis labels
    axis(2, at = 1:nrow(balance_data), labels = balance_data$variable, las = 1, cex.axis = 0.8)

    # Add legend
    legend("topright", c("Before Weighting", "After Weighting"),
           col = c("red", "blue"), pch = 16, cex = 0.9)
  }

  if (plot_type %in% c("ps", "both")) {
    # PS distribution plot in base R
    treatment_var <- names(iptw_result$data)[sapply(iptw_result$data, is.factor) &
                                               sapply(iptw_result$data, function(x) nlevels(x) == 2)][1]

    ps_treated <- iptw_result$ps[iptw_result$data[[treatment_var]] == levels(iptw_result$data[[treatment_var]])[2]]
    ps_control <- iptw_result$ps[iptw_result$data[[treatment_var]] == levels(iptw_result$data[[treatment_var]])[1]]

    if (plot_type == "both") {
      par(mfrow = c(1, 2))
    }

    hist(ps_control, breaks = 20, col = rgb(1, 0, 0, 0.5), border = "red",
         main = "Propensity Score Distribution", xlab = "Propensity Score",
         xlim = c(0, 1))
    hist(ps_treated, breaks = 20, col = rgb(0, 0, 1, 0.5), border = "blue", add = TRUE)

    legend("topright",
           c(levels(iptw_result$data[[treatment_var]])[1], levels(iptw_result$data[[treatment_var]])[2]),
           col = c("red", "blue"), lty = 1, lwd = 3)
  }

  return("Base R plots created")
}

#' Check positivity assumption
#' @noRd
.check_positivity <- function(iptw_result) {

  ps <- iptw_result$ps

  # Check for extreme propensity scores
  min_ps <- min(ps)
  max_ps <- max(ps)

  # Flag if any PS < 0.01 or > 0.99
  violation <- min_ps < 0.01 || max_ps > 0.99

  # Additional checks
  extreme_low <- sum(ps < 0.05)
  extreme_high <- sum(ps > 0.95)
  total_extreme <- extreme_low + extreme_high

  recommendations <- character(0)

  if (violation) {
    recommendations <- c(recommendations,
                         "Consider restricting analysis to region of common support",
                         "Examine subjects with extreme propensity scores",
                         "Consider alternative modeling approaches")
  }

  if (total_extreme > nrow(iptw_result$data) * 0.05) {
    recommendations <- c(recommendations,
                         "More than 5% of subjects have extreme propensity scores",
                         "Consider trimming or alternative weighting schemes")
  }

  return(list(
    violation = violation,
    min_ps = min_ps,
    max_ps = max_ps,
    extreme_low = extreme_low,
    extreme_high = extreme_high,
    recommendations = recommendations
  ))
}

#' Check covariate balance
#' @noRd
.check_balance <- function(iptw_result, threshold) {

  balance_table <- iptw_result$diagnostics$balance_table

  # Check which variables have poor balance after weighting
  poor_balance_vars <- balance_table$variable[abs(balance_table$std_diff_weighted) > threshold]
  poor_balance <- length(poor_balance_vars) > 0

  max_std_diff <- max(abs(balance_table$std_diff_weighted), na.rm = TRUE)

  recommendations <- character(0)

  if (poor_balance) {
    recommendations <- c(recommendations,
                         paste("Poor balance detected for:", paste(poor_balance_vars, collapse = ", ")),
                         "Consider including interaction terms in propensity score model",
                         "Consider alternative methods (e.g., matching, stratification)")
  }

  return(list(
    poor_balance = poor_balance,
    poor_balance_vars = poor_balance_vars,
    max_std_diff = max_std_diff,
    recommendations = recommendations
  ))
}

#' Check weight distribution
#' @noRd
.check_weight_distribution <- function(iptw_result, extreme_threshold) {

  weights <- iptw_result$weights

  # Check for extreme weights
  extreme_weights <- any(weights > extreme_threshold)
  n_extreme <- sum(weights > extreme_threshold)

  # Weight distribution statistics
  weight_summary <- summary(weights)
  effective_n <- iptw_result$diagnostics$effective_n

  recommendations <- character(0)

  if (extreme_weights) {
    recommendations <- c(recommendations,
                         paste("Extreme weights detected:", n_extreme, "weights >", extreme_threshold),
                         "Consider trimming extreme weights",
                         "Examine subjects with extreme weights for model misspecification")
  }

  # Check effective sample size reduction
  original_n <- length(weights)
  eff_n_ratio <- effective_n / original_n

  if (eff_n_ratio < 0.5) {
    recommendations <- c(recommendations,
                         paste("Large effective sample size reduction (", round(eff_n_ratio * 100, 1), "%)", sep = ""),
                         "Consider alternative weighting schemes or model modifications")
  }

  return(list(
    extreme_weights = extreme_weights,
    n_extreme = n_extreme,
    weight_summary = weight_summary,
    effective_n = effective_n,
    eff_n_ratio = eff_n_ratio,
    recommendations = recommendations
  ))
}

#' Print IPTW assumption check results
#' @noRd
.print_assumption_results <- function(result) {

  cat("IPTW Assumptions Check\n")
  cat("======================\n\n")

  # Overall assessment
  status_color <- switch(result$overall_assessment,
                         "PASS" = .safe_check(),
                         "CAUTION" = .safe_warning(),
                         "FAIL" = .safe_cross()
  )

  cat("Overall Assessment:", status_color, result$overall_assessment, "\n\n")

  # Positivity
  cat("1. Positivity (Non-zero probability of treatment)\n")
  cat("   Propensity score range: [", round(result$positivity$min_ps, 3),
      ", ", round(result$positivity$max_ps, 3), "]\n", sep = "")
  cat("   Extreme values (PS < 0.05):", result$positivity$extreme_low, "\n")
  cat("   Extreme values (PS > 0.95):", result$positivity$extreme_high, "\n")
  cat("   Status:", ifelse(result$positivity$violation,
                           paste(.safe_warning(), "VIOLATION"),
                           paste(.safe_check(), "OK")), "\n\n")

  # Balance
  cat("2. Covariate Balance\n")
  cat("   Maximum standardized difference:", round(result$balance$max_std_diff, 3), "\n")
  if (result$balance$poor_balance) {
    cat("   Variables with poor balance:", paste(result$balance$poor_balance_vars, collapse = ", "), "\n")
  }
  cat("   Status:", ifelse(result$balance$poor_balance,
                           paste(.safe_warning(), "POOR BALANCE"),
                           paste(.safe_check(), "BALANCED")), "\n\n")

  # Weights
  cat("3. Weight Distribution\n")
  cat("   Weight range: [", round(min(result$weights$weight_summary), 2),
      ", ", round(max(result$weights$weight_summary), 2), "]\n", sep = "")
  cat("   Effective sample size ratio:", round(result$weights$eff_n_ratio, 3), "\n")
  if (result$weights$extreme_weights) {
    cat("   Extreme weights (>10):", result$weights$n_extreme, "\n")
  }
  cat("   Status:", ifelse(result$weights$extreme_weights,
                           paste(.safe_warning(), "EXTREME WEIGHTS"),
                           paste(.safe_check(), "OK")), "\n\n")

  # Recommendations
  if (length(result$recommendations) > 0) {
    cat("Recommendations:\n")
    for (i in seq_along(result$recommendations)) {
      cat("  ", i, ". ", result$recommendations[i], "\n", sep = "")
    }
  }
}

#' Save balance plots to files
#' @noRd
.save_balance_plots <- function(plots, plot_dir) {

  # Create directory if it doesn't exist
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }

  # Save plots
  if ("love_plot" %in% names(plots)) {
    ggplot2::ggsave(file.path(plot_dir, "iptw_love_plot.png"),
                    plots$love_plot, width = 8, height = 6, dpi = 300)
  }

  if ("ps_plot" %in% names(plots)) {
    ggplot2::ggsave(file.path(plot_dir, "iptw_ps_distribution.png"),
                    plots$ps_plot, width = 8, height = 6, dpi = 300)
  }

  cat("Plots saved to:", plot_dir, "\n")
}
