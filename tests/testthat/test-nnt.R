# Tests for Number Needed to Treat functionality
test_that("calc_risk_diff with nnt = TRUE returns correct NNT values", {
  skip_if_not_installed("dplyr")

  # Create test data with known risk difference
  set.seed(123)
  n <- 1000
  test_data <- data.frame(
    outcome = c(rep(1, 20), rep(0, 80), rep(1, 30), rep(0, 870)),  # 20% vs 3%
    exposure = c(rep(1, 100), rep(0, 900))
  )

  # Calculate NNT
  suppressWarnings({
    result_nnt <- calc_risk_diff(test_data, "outcome", "exposure", nnt = TRUE)
  })

  # Expected risk difference ≈ 0.20 - 0.033 = 0.167
  # Expected NNT ≈ 1/0.167 ≈ 6
  expect_true(inherits(result_nnt, "nnt_result"))
  expect_true(result_nnt$rd > 5 && result_nnt$rd < 8)  # Approximate NNT
})

test_that("NNT handles edge cases appropriately", {
  # Test with very small risk difference
  small_rd_data <- data.frame(
    outcome = c(rep(1, 1), rep(0, 999), rep(1, 1), rep(0, 999)),
    exposure = c(rep(1, 1000), rep(0, 1000))
  )

  suppressWarnings({
    result_small <- calc_risk_diff(small_rd_data, "outcome", "exposure", nnt = TRUE)
  })

  # Should return Inf for very small RD
  expect_true(is.infinite(result_small$rd))
})

test_that("NNT confidence intervals are correctly computed", {
  data(cachar_sample)

  suppressWarnings({
    result_rd <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking")
    result_nnt <- calc_risk_diff(cachar_sample, "abnormal_screen", "smoking", nnt = TRUE)
  })

  # Check that NNT CI bounds are reciprocals (with swapping)
  if (!is.infinite(result_nnt$ci_lower) && !is.infinite(result_nnt$ci_upper)) {
    expect_equal(result_nnt$ci_lower, 1/abs(result_rd$ci_upper), tolerance = 0.01)
    expect_equal(result_nnt$ci_upper, 1/abs(result_rd$ci_lower), tolerance = 0.01)
  }
})
