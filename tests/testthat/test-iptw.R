# Tests for IPTW functionality

# Create test data (keeping this synthetic for testing purposes)
create_iptw_test_data <- function(n = 500) {
  set.seed(42)

  # Generate correlated covariates similar to cachar_sample structure
  age <- round(rnorm(n, 45, 12))
  age <- pmax(18, pmin(75, age))  # Realistic age range

  sex <- factor(sample(c("male", "female"), n, replace = TRUE, prob = c(0.76, 0.24)),
                levels = c("male", "female"))

  residence <- factor(sample(c("rural", "urban", "urban slum"), n, replace = TRUE, prob = c(0.87, 0.10, 0.03)),
                      levels = c("rural", "urban", "urban slum"))

  # Generate treatment with realistic propensity score model (areca nut use)
  logit_ps <- -1.2 + 0.02 * (age - 45) + 0.3 * (sex == "male") +
    0.2 * (residence == "urban") + rnorm(n, 0, 0.3)
  ps_true <- plogis(logit_ps)
  areca_nut <- factor(rbinom(n, 1, ps_true), levels = c(0, 1), labels = c("No", "Yes"))

  # Generate outcome (abnormal screening)
  logit_outcome <- -2.8 + 0.8 * (areca_nut == "Yes") + 0.015 * (age - 45) +
    0.4 * (sex == "male") + 0.2 * (residence == "urban")
  abnormal_screen <- rbinom(n, 1, plogis(logit_outcome))

  data.frame(
    id = 1:n,
    age = age,
    sex = sex,
    residence = residence,
    areca_nut = areca_nut,
    abnormal_screen = abnormal_screen
  )
}

test_that("calc_iptw_weights works with basic inputs", {
  data <- create_iptw_test_data()

  result <- calc_iptw_weights(
    data = data,
    treatment = "areca_nut",
    covariates = c("age", "sex", "residence")
  )

  expect_s3_class(result, "iptw_result")
  expect_true(is.list(result))
  expect_true(all(c("data", "ps_model", "weights", "ps", "diagnostics") %in% names(result)))
  expect_equal(length(result$weights), nrow(data))
  expect_true(all(result$weights > 0))
})

test_that("calc_iptw_weights handles different weight types", {
  data <- create_iptw_test_data()

  # Test ATE weights
  result_ate <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), weight_type = "ATE")
  expect_equal(result_ate$weight_type, "ATE")

  # Test ATT weights
  result_att <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), weight_type = "ATT")
  expect_equal(result_att$weight_type, "ATT")

  # Test ATC weights
  result_atc <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), weight_type = "ATC")
  expect_equal(result_atc$weight_type, "ATC")

  # Weights should be different
  expect_false(identical(result_ate$weights, result_att$weights))
  expect_false(identical(result_ate$weights, result_atc$weights))
})

test_that("calc_iptw_weights handles different propensity score methods", {
  data <- create_iptw_test_data()

  # Test logistic (default)
  result_logit <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), method = "logistic")
  expect_equal(result_logit$method, "logistic")

  # Test probit
  result_probit <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), method = "probit")
  expect_equal(result_probit$method, "probit")

  # Test cloglog
  result_cloglog <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), method = "cloglog")
  expect_equal(result_cloglog$method, "cloglog")
})

test_that("calc_risk_diff_iptw works with basic inputs", {
  data <- create_iptw_test_data()

  result <- calc_risk_diff_iptw(
    data = data,
    outcome = "abnormal_screen",
    treatment = "areca_nut",
    covariates = c("age", "sex", "residence")
  )

  expect_s3_class(result, "riskdiff_iptw_result")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_true(all(c("treatment_var", "rd_iptw", "ci_lower", "ci_upper", "p_value", "weight_type") %in% names(result)))
  expect_true(!is.na(result$rd_iptw))
})

test_that("calc_risk_diff_iptw works with pre-calculated weights", {
  data <- create_iptw_test_data()

  # Calculate weights separately
  iptw_result <- calc_iptw_weights(data, "areca_nut", c("age", "sex"))

  # Use pre-calculated weights
  result <- calc_risk_diff_iptw(
    data = data,
    outcome = "abnormal_screen",
    treatment = "areca_nut",
    covariates = c("age", "sex"),
    iptw_weights = iptw_result$weights
  )

  expect_s3_class(result, "riskdiff_iptw_result")
  expect_true(!is.na(result$rd_iptw))
})

test_that("calc_risk_diff_iptw works with bootstrap CI", {
  data <- create_iptw_test_data(n = 200)  # Smaller sample for faster testing

  result <- calc_risk_diff_iptw(
    data = data,
    outcome = "abnormal_screen",
    treatment = "areca_nut",
    covariates = c("age", "sex"),
    bootstrap_ci = TRUE,
    boot_n = 100  # Small number for testing
  )

  expect_s3_class(result, "riskdiff_iptw_result")
  expect_true(attr(result, "bootstrap"))
  expect_equal(attr(result, "boot_n"), 100)
})

test_that("check_iptw_assumptions works", {
  data <- create_iptw_test_data()

  iptw_result <- calc_iptw_weights(data, "areca_nut", c("age", "sex"))
  assumptions <- check_iptw_assumptions(iptw_result, verbose = FALSE)

  expect_s3_class(assumptions, "iptw_assumptions")
  expect_true(is.list(assumptions))
  expect_true(all(c("overall_assessment", "positivity", "balance", "weights", "recommendations") %in% names(assumptions)))
  expect_true(assumptions$overall_assessment %in% c("PASS", "CAUTION", "FAIL"))
})

test_that("IPTW functions validate inputs correctly", {
  data <- create_iptw_test_data()

  # Missing treatment variable
  expect_error(
    calc_iptw_weights(data, "missing_treatment", c("age", "sex")),
    "Variables not found"
  )

  # Invalid weight type
  expect_error(
    calc_iptw_weights(data, "areca_nut", c("age", "sex"), weight_type = "invalid"),
    "must be one of"
  )

  # Invalid method
  expect_error(
    calc_iptw_weights(data, "areca_nut", c("age", "sex"), method = "invalid"),
    "must be one of"
  )

  # Wrong length weights
  expect_error(
    calc_risk_diff_iptw(data, "abnormal_screen", "areca_nut", c("age", "sex"),
                        iptw_weights = c(1, 2, 3)),
    "Length of iptw_weights"
  )
})

test_that("IPTW handles edge cases gracefully", {
  # Small dataset
  small_data <- create_iptw_test_data(n = 50)

  result <- calc_iptw_weights(small_data, "areca_nut", c("age", "sex"))
  expect_s3_class(result, "iptw_result")

  # Dataset with missing values
  data_missing <- create_iptw_test_data()
  data_missing$age[1:50] <- NA

  result_missing <- calc_iptw_weights(data_missing, "areca_nut", c("age", "sex"))
  expect_true(nrow(result_missing$data) < nrow(data_missing))
})

test_that("print methods work for IPTW objects", {
  data <- create_iptw_test_data()

  # Test iptw_result print
  iptw_result <- calc_iptw_weights(data, "areca_nut", c("age", "sex"))
  expect_output(print(iptw_result), "Inverse Probability of Treatment Weighting")
  expect_output(print(iptw_result), "Propensity Score Model")

  # Test riskdiff_iptw_result print
  rd_result <- calc_risk_diff_iptw(data, "abnormal_screen", "areca_nut", c("age", "sex"))
  expect_output(print(rd_result), "IPTW-Standardized Risk Difference")
  expect_output(print(rd_result), "Risk in Treated")

  # Test summary method
  expect_output(summary(rd_result), "IPTW Risk Difference Analysis Summary")
})

test_that("stabilized vs unstabilized weights differ appropriately", {
  data <- create_iptw_test_data()

  # Unstabilized weights
  result_unstab <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), stabilize = FALSE)

  # Stabilized weights
  result_stab <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), stabilize = TRUE)

  # Weights should be different
  expect_false(identical(result_unstab$weights, result_stab$weights))

  # Stabilized weights should generally have smaller variance
  expect_true(stats::var(result_stab$weights) <= stats::var(result_unstab$weights))
})

test_that("weight trimming works correctly", {
  data <- create_iptw_test_data()

  # Without trimming
  result_no_trim <- calc_iptw_weights(data, "areca_nut", c("age", "sex"), trim_weights = FALSE)

  # With trimming
  result_trim <- calc_iptw_weights(data, "areca_nut", c("age", "sex"),
                                   trim_weights = TRUE, trim_quantiles = c(0.05, 0.95))

  # Range of trimmed weights should be smaller or equal
  expect_true(diff(range(result_trim$weights)) <= diff(range(result_no_trim$weights)))

  # Check that extreme values are actually trimmed
  q05 <- stats::quantile(result_no_trim$weights, 0.05)
  q95 <- stats::quantile(result_no_trim$weights, 0.95)

  expect_true(all(result_trim$weights >= q05))
  expect_true(all(result_trim$weights <= q95))
})

test_that("create_balance_plots handles missing ggplot2 gracefully", {
  data <- create_iptw_test_data()
  iptw_result <- calc_iptw_weights(data, "areca_nut", c("age", "sex"))

  # This should work whether ggplot2 is available or not
  plots <- create_balance_plots(iptw_result, plot_type = "love")

  # Should return something (either ggplot object or message)
  expect_true(!is.null(plots))
})

test_that("different estimands give sensible results", {
  data <- create_iptw_test_data()

  # Calculate all three estimands
  rd_ate <- calc_risk_diff_iptw(data, "abnormal_screen", "areca_nut", c("age", "sex"), weight_type = "ATE")
  rd_att <- calc_risk_diff_iptw(data, "abnormal_screen", "areca_nut", c("age", "sex"), weight_type = "ATT")
  rd_atc <- calc_risk_diff_iptw(data, "abnormal_screen", "areca_nut", c("age", "sex"), weight_type = "ATC")

  # All should be valid
  expect_false(is.na(rd_ate$rd_iptw))
  expect_false(is.na(rd_att$rd_iptw))
  expect_false(is.na(rd_atc$rd_iptw))

  # They don't have to be equal (and usually won't be)
  expect_true(is.numeric(rd_ate$rd_iptw))
  expect_true(is.numeric(rd_att$rd_iptw))
  expect_true(is.numeric(rd_atc$rd_iptw))
})

test_that("alpha level affects confidence intervals appropriately", {
  data <- create_iptw_test_data()

  # 95% CI
  result_95 <- calc_risk_diff_iptw(data, "abnormal_screen", "areca_nut", c("age", "sex"), alpha = 0.05)

  # 90% CI
  result_90 <- calc_risk_diff_iptw(data, "abnormal_screen", "areca_nut", c("age", "sex"), alpha = 0.10)

  # 90% CI should be narrower
  width_95 <- result_95$ci_upper - result_95$ci_lower
  width_90 <- result_90$ci_upper - result_90$ci_lower

  expect_true(width_90 < width_95)
  expect_equal(attr(result_90, "alpha"), 0.10)
})
