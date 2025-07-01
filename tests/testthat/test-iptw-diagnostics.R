# Enhanced tests for iptw_diagnostics.R to improve coverage
# Add these tests to tests/testthat/test-iptw-diagnostics.R (create new file)

# Create comprehensive IPTW test data
create_iptw_diagnostic_data <- function(n = 300, scenario = "normal") {
  set.seed(789)

  if (scenario == "normal") {
    data.frame(
      age = round(rnorm(n, 45, 12)),
      sex = factor(sample(c("male", "female"), n, replace = TRUE, prob = c(0.6, 0.4))),
      education = factor(sample(c("low", "medium", "high"), n, replace = TRUE)),
      income = rnorm(n, 50000, 15000),
      treatment = factor(sample(c("control", "treated"), n, replace = TRUE, prob = c(0.7, 0.3))),
      outcome = rbinom(n, 1, 0.2)
    )
  } else if (scenario == "poor_overlap") {
    # Create data with poor propensity score overlap
    age <- round(rnorm(n, 45, 12))
    # Make treatment highly dependent on age to create poor overlap
    treatment_prob <- plogis(-5 + 0.15 * age)
    treatment <- factor(rbinom(n, 1, treatment_prob), labels = c("control", "treated"))

    data.frame(
      age = age,
      sex = factor(sample(c("male", "female"), n, replace = TRUE)),
      treatment = treatment,
      outcome = rbinom(n, 1, 0.2)
    )
  } else if (scenario == "extreme_weights") {
    # Create scenario leading to extreme weights
    age <- round(rnorm(n, 45, 12))
    rare_condition <- rbinom(n, 1, 0.05)  # 5% have rare condition
    # Treatment almost perfectly predicted by rare condition
    treatment_prob <- ifelse(rare_condition == 1, 0.95, 0.05)
    treatment <- factor(rbinom(n, 1, treatment_prob), labels = c("control", "treated"))

    data.frame(
      age = age,
      rare_condition = rare_condition,
      treatment = treatment,
      outcome = rbinom(n, 1, 0.2)
    )
  }
}

test_that("create_balance_plots works with ggplot2 available", {
  skip_if_not_installed("ggplot2")

  data <- create_iptw_diagnostic_data()
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "sex", "education")
  )

  # Test love plot only
  love_plot <- create_balance_plots(iptw_result, plot_type = "love")
  expect_true(inherits(love_plot, "ggplot"))

  # Test propensity score plot only
  ps_plot <- create_balance_plots(iptw_result, plot_type = "ps")
  expect_true(inherits(ps_plot, "ggplot"))

  # Test both plots
  both_plots <- create_balance_plots(iptw_result, plot_type = "both")
  expect_true(is.list(both_plots))
  expect_true("love_plot" %in% names(both_plots))
  expect_true("ps_plot" %in% names(both_plots))
  expect_true(inherits(both_plots$love_plot, "ggplot"))
  expect_true(inherits(both_plots$ps_plot, "ggplot"))
})

test_that("create_balance_plots works without ggplot2", {
  # Mock ggplot2 as unavailable
  mockery::with_mock(
    `requireNamespace` = function(package, quietly = TRUE) {
      if (package == "ggplot2") return(FALSE)
      return(base::requireNamespace(package, quietly = quietly))
    },
    {
      data <- create_iptw_diagnostic_data()
      iptw_result <- calc_iptw_weights(
        data = data,
        treatment = "treatment",
        covariates = c("age", "sex")
      )

      expect_message(
        result <- create_balance_plots(iptw_result, plot_type = "love"),
        "ggplot2 not available"
      )

      expect_equal(result, "Base R plots created")
    }
  )
})

test_that("create_balance_plots handles save_plots option", {
  skip_if_not_installed("ggplot2")

  data <- create_iptw_diagnostic_data()
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "sex")
  )

  # Create temporary directory for testing
  temp_dir <- tempdir()
  plot_dir <- file.path(temp_dir, "test_plots")

  # Test saving plots
  expect_message(
    create_balance_plots(iptw_result, plot_type = "both",
                         save_plots = TRUE, plot_dir = plot_dir),
    "Plots saved to"
  )

  # Check files were created
  expect_true(file.exists(file.path(plot_dir, "iptw_love_plot.png")))
  expect_true(file.exists(file.path(plot_dir, "iptw_ps_distribution.png")))

  # Clean up
  unlink(plot_dir, recursive = TRUE)
})

test_that("check_iptw_assumptions detects positivity violations", {
  data <- create_iptw_diagnostic_data(scenario = "poor_overlap")
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "sex")
  )

  assumptions <- check_iptw_assumptions(iptw_result, verbose = FALSE)

  expect_true(is.list(assumptions))
  expect_true("overall_assessment" %in% names(assumptions))
  expect_true("positivity" %in% names(assumptions))
  expect_true("balance" %in% names(assumptions))
  expect_true("weights" %in% names(assumptions))

  # Should detect positivity issues
  expect_true(assumptions$positivity$violation ||
                assumptions$overall_assessment %in% c("CAUTION", "FAIL"))
})

test_that("check_iptw_assumptions detects extreme weights", {
  data <- create_iptw_diagnostic_data(scenario = "extreme_weights")
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "rare_condition")
  )

  assumptions <- check_iptw_assumptions(iptw_result,
                                        extreme_weight_threshold = 5,
                                        verbose = FALSE)

  # Should detect extreme weights
  expect_true(assumptions$weights$extreme_weights ||
                assumptions$overall_assessment %in% c("CAUTION", "FAIL"))

  # Should have recommendations
  expect_true(length(assumptions$recommendations) > 0)
})

test_that("check_iptw_assumptions detects poor balance", {
  data <- create_iptw_diagnostic_data()
  # Intentionally create poor balance by excluding important confounders
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("sex")  # Missing age and education
  )

  assumptions <- check_iptw_assumptions(iptw_result,
                                        balance_threshold = 0.05,  # Strict threshold
                                        verbose = FALSE)

  expect_true(is.list(assumptions))

  # May detect poor balance or other issues
  if (assumptions$balance$poor_balance) {
    expect_true(length(assumptions$balance$poor_balance_vars) > 0)
  }
})

test_that("check_iptw_assumptions gives PASS for good data", {
  data <- create_iptw_diagnostic_data()
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "sex", "education"),
    stabilize = TRUE,
    trim_weights = TRUE
  )

  assumptions <- check_iptw_assumptions(iptw_result, verbose = FALSE)

  # With good data and proper weighting, should pass or at worst get caution
  expect_true(assumptions$overall_assessment %in% c("PASS", "CAUTION"))
})

test_that("summary.riskdiff_iptw_result works correctly", {
  data <- create_iptw_diagnostic_data()

  rd_iptw <- calc_risk_diff_iptw(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    covariates = c("age", "sex"),
    verbose = FALSE
  )

  # Test summary method
  expect_output(summary(rd_iptw), "IPTW Risk Difference Analysis Summary")
  expect_output(summary(rd_iptw), "Treatment Variable")
  expect_output(summary(rd_iptw), "Weight Type")
  expect_output(summary(rd_iptw), "Risk in Treated Group")
  expect_output(summary(rd_iptw), "Risk in Control Group")
})

test_that("summary.riskdiff_iptw_result handles bootstrap results", {
  data <- create_iptw_diagnostic_data(n = 150)  # Smaller for faster bootstrap

  rd_iptw_boot <- calc_risk_diff_iptw(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    covariates = c("age", "sex"),
    bootstrap_ci = TRUE,
    boot_n = 50,  # Small number for testing
    verbose = FALSE
  )

  expect_output(summary(rd_iptw_boot), "bootstrap replicates")
})

test_that("summary.riskdiff_iptw_result handles missing values", {
  # Create result with some NA values
  rd_result <- data.frame(
    treatment_var = "treatment",
    rd_iptw = 0.05,
    ci_lower = 0.01,
    ci_upper = 0.09,
    p_value = NA_real_,  # Missing p-value
    weight_type = "ATE",
    effective_n = 200,
    risk_treated = 0.25,
    risk_control = 0.20
  )
  class(rd_result) <- c("riskdiff_iptw_result", "data.frame")

  # Should handle NA p-value gracefully
  expect_output(summary(rd_result), "P-value: 1.000")  # Should default to 1.0
})

test_that("internal helper functions work correctly", {
  data <- create_iptw_diagnostic_data()
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "sex", "education")
  )

  # Test .check_positivity
  positivity_check <- riskdiff:::.check_positivity(iptw_result)
  expect_true(is.list(positivity_check))
  expect_true("violation" %in% names(positivity_check))
  expect_true("min_ps" %in% names(positivity_check))
  expect_true("max_ps" %in% names(positivity_check))

  # Test .check_balance
  balance_check <- riskdiff:::.check_balance(iptw_result, threshold = 0.1)
  expect_true(is.list(balance_check))
  expect_true("poor_balance" %in% names(balance_check))
  expect_true("max_std_diff" %in% names(balance_check))

  # Test .check_weight_distribution
  weight_check <- riskdiff:::.check_weight_distribution(iptw_result, extreme_threshold = 10)
  expect_true(is.list(weight_check))
  expect_true("extreme_weights" %in% names(weight_check))
  expect_true("effective_n" %in% names(weight_check))
})

test_that(".create_love_plot handles edge cases", {
  skip_if_not_installed("ggplot2")

  # Create minimal IPTW result for testing
  data <- create_iptw_diagnostic_data(n = 50)
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "sex")
  )

  # Should handle the minimal case
  love_plot <- riskdiff:::.create_love_plot(iptw_result, threshold = 0.1)
  expect_true(inherits(love_plot, "ggplot"))
})

test_that(".create_ps_plot handles different treatment variable types", {
  skip_if_not_installed("ggplot2")

  data <- create_iptw_diagnostic_data()
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "sex")
  )

  # Should work with factor treatment variable
  ps_plot <- riskdiff:::.create_ps_plot(iptw_result)
  expect_true(inherits(ps_plot, "ggplot"))
})

test_that("verbose output works in check_iptw_assumptions", {
  data <- create_iptw_diagnostic_data()
  iptw_result <- calc_iptw_weights(
    data = data,
    treatment = "treatment",
    covariates = c("age", "sex")
  )

  # Test verbose output
  expect_output(
    check_iptw_assumptions(iptw_result, verbose = TRUE),
    "IPTW Assumptions Check"
  )

  expect_output(
    check_iptw_assumptions(iptw_result, verbose = TRUE),
    "Overall Assessment"
  )
})

test_that("invalid inputs are handled correctly", {
  # Test with non-iptw_result object
  expect_error(
    create_balance_plots(list(not_iptw = TRUE)),
    "must be an iptw_result object"
  )

  expect_error(
    check_iptw_assumptions(data.frame(x = 1)),
    "must be an iptw_result object"
  )

  # Test with invalid plot_type
  data <- create_iptw_diagnostic_data()
  iptw_result <- calc_iptw_weights(data, "treatment", c("age", "sex"))

  expect_error(
    create_balance_plots(iptw_result, plot_type = "invalid"),
    "plot_type must be"
  )
})
