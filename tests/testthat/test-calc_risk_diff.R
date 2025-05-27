# Tests for calc_risk_diff function

# Create test data
create_test_data <- function(n = 1000) {
  set.seed(123)
  data.frame(
    id = 1:n,
    outcome = rbinom(n, 1, 0.2),
    exposure = factor(sample(c("No", "Yes"), n, replace = TRUE)),
    age = rnorm(n, 40, 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    stratum = factor(sample(c("A", "B"), n, replace = TRUE))
  )
}

test_that("calc_risk_diff works with basic inputs", {
  data <- create_test_data()

  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure"
  )

  expect_s3_class(result, "riskdiff_result")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_true(all(c("exposure_var", "rd", "ci_lower", "ci_upper", "p_value", "model_type") %in% names(result)))
})

test_that("calc_risk_diff handles adjustment variables", {
  data <- create_test_data()

  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure",
    adjust_vars = "age"
  )

  expect_s3_class(result, "riskdiff_result")
  expect_equal(nrow(result), 1)
})

test_that("calc_risk_diff handles stratification", {
  data <- create_test_data()

  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure",
    strata = "sex"
  )

  expect_s3_class(result, "riskdiff_result")
  expect_equal(nrow(result), 2)  # Two strata
  expect_true("sex" %in% names(result))
})

test_that("calc_risk_diff validates inputs properly", {
  data <- create_test_data()

  # Missing outcome variable
  expect_error(
    calc_risk_diff(data, "missing_var", "exposure"),
    "Variables not found"
  )

  # Non-binary outcome
  data$bad_outcome <- 1:nrow(data)
  expect_error(
    calc_risk_diff(data, "bad_outcome", "exposure"),
    "must be binary"
  )

  # Invalid link
  expect_error(
    calc_risk_diff(data, "outcome", "exposure", link = "invalid"),
    "must be one of"
  )
})

test_that("calc_risk_diff handles small samples gracefully", {
  data <- create_test_data(n = 10)

  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure"
  )

  expect_s3_class(result, "riskdiff_result")
  # Should still return a result, even if model fails
  expect_true(nrow(result) >= 1)
})

test_that("calc_risk_diff works with birthweight data", {
  skip_if_not_installed("riskdiff")
  data(birthweight, package = "riskdiff", envir = environment())

  result <- calc_risk_diff(
    data = birthweight,
    outcome = "low_birthweight",
    exposure = "smoking"
  )

  expect_s3_class(result, "riskdiff_result")
  expect_equal(nrow(result), 1)
  expect_true(!is.na(result$rd))
  expect_true(result$rd > 0)  # Smoking should increase risk
})

test_that("format_risk_diff works correctly", {
  data <- create_test_data()
  result <- calc_risk_diff(data, "outcome", "exposure")

  formatted <- format_risk_diff(result)

  expect_true(all(c("rd_formatted", "ci_formatted", "p_value_formatted") %in% names(formatted)))
  expect_true(is.character(formatted$rd_formatted))
  expect_true(is.character(formatted$ci_formatted))
  expect_true(is.character(formatted$p_value_formatted))
})

test_that("print method works", {
  data <- create_test_data()
  result <- calc_risk_diff(data, "outcome", "exposure")

  expect_output(print(result), "Risk Difference Analysis Results")
  expect_output(print(result), "Confidence level")
})

test_that("different link functions can be specified", {
  data <- create_test_data()

  # Test different link preferences
  result_auto <- calc_risk_diff(data, "outcome", "exposure", link = "auto")
  result_logit <- calc_risk_diff(data, "outcome", "exposure", link = "logit")

  expect_s3_class(result_auto, "riskdiff_result")
  expect_s3_class(result_logit, "riskdiff_result")
  expect_equal(result_logit$model_type, "logit")
})

test_that("confidence level can be changed", {
  data <- create_test_data()

  result_95 <- calc_risk_diff(data, "outcome", "exposure", alpha = 0.05)
  result_90 <- calc_risk_diff(data, "outcome", "exposure", alpha = 0.10)

  # 90% CI should be narrower than 95% CI
  ci_width_95 <- result_95$ci_upper - result_95$ci_lower
  ci_width_90 <- result_90$ci_upper - result_90$ci_lower

  expect_true(ci_width_90 < ci_width_95)
  expect_equal(attr(result_90, "alpha"), 0.10)
})
