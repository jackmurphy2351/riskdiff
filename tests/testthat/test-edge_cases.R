# ==============================================================================
# tests/testthat/test-edge_cases.R
# ==============================================================================

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

test_that("handles data with missing values correctly", {
  data <- create_test_data()
  data$outcome[1:50] <- NA
  data$exposure[51:100] <- NA

  result <- calc_risk_diff(data, "outcome", "exposure")

  expect_s3_class(result, "riskdiff_result")
  expect_true(result$n_obs < nrow(data))  # Should use complete cases only
})

test_that("handles constant outcomes gracefully", {
  data <- create_test_data()
  data$outcome <- 0  # All zeros

  result <- calc_risk_diff(data, "outcome", "exposure")

  expect_s3_class(result, "riskdiff_result")
  # Should handle gracefully even if model fails
  expect_true(nrow(result) == 1)
  expect_true(result$model_type %in% c("insufficient_data", "failed"))
})

test_that("handles single-level exposures", {
  data <- create_test_data()
  data$exposure <- factor("Yes")  # Only one level

  expect_error(
    calc_risk_diff(data, "outcome", "exposure"),
    "at least 2 levels"
  )
})

test_that("handles exposures with rare levels", {
  data <- create_test_data(n = 1000)
  # Make "Yes" very rare (only 2 observations)
  data$exposure <- factor(c(rep("No", 998), rep("Yes", 2)))

  result <- calc_risk_diff(data, "outcome", "exposure")

  expect_s3_class(result, "riskdiff_result")
  # Should still return a result, though it may not be reliable
  expect_true(nrow(result) == 1)
})

test_that("model convergence issues are handled", {
  # Create data likely to cause convergence issues
  data <- data.frame(
    outcome = c(rep(0, 100), rep(1, 10)),
    exposure = factor(c(rep("No", 100), rep("Yes", 10))),
    confounder = c(rep(0, 100), rep(1, 10))  # Perfect separation
  )

  result <- calc_risk_diff(data, "outcome", "exposure", adjust_vars = "confounder")

  expect_s3_class(result, "riskdiff_result")
  # Should handle convergence failure gracefully
  expect_true(nrow(result) == 1)
})

test_that("verbose output works", {
  data <- create_test_data()

  expect_message(
    calc_risk_diff(data, "outcome", "exposure", verbose = TRUE),
    "Formula"
  )
})

test_that("different alpha levels work", {
  data <- create_test_data()

  result_99 <- calc_risk_diff(data, "outcome", "exposure", alpha = 0.01)
  result_90 <- calc_risk_diff(data, "outcome", "exposure", alpha = 0.10)

  # 99% CI should be wider than 90% CI
  width_99 <- result_99$ci_upper - result_99$ci_lower
  width_90 <- result_90$ci_upper - result_90$ci_lower

  expect_true(width_99 > width_90)
})

test_that("logical outcomes are handled correctly", {
  data <- create_test_data()
  data$outcome_logical <- as.logical(data$outcome)

  result <- calc_risk_diff(data, "outcome_logical", "exposure")

  expect_s3_class(result, "riskdiff_result")
  expect_true(!is.na(result$rd))
})

test_that("character exposures are converted to factors", {
  data <- create_test_data()
  data$exposure_char <- as.character(data$exposure)

  result <- calc_risk_diff(data, "outcome", "exposure_char")

  expect_s3_class(result, "riskdiff_result")
  expect_true(!is.na(result$rd))
})

test_that("handles very small datasets", {
  # Dataset with minimal size
  small_data <- data.frame(
    outcome = c(0, 1, 0, 1, 1),
    exposure = factor(c("No", "No", "Yes", "Yes", "Yes"))
  )

  result <- calc_risk_diff(small_data, "outcome", "exposure")

  expect_s3_class(result, "riskdiff_result")
  # Should return insufficient data result
  expect_true(result$model_type == "insufficient_data")
})

test_that("handles datasets with extreme proportions", {
  data <- create_test_data()
  # Make outcome very rare (only 1% prevalence)
  data$outcome <- rbinom(nrow(data), 1, 0.01)

  result <- calc_risk_diff(data, "outcome", "exposure")

  expect_s3_class(result, "riskdiff_result")
  expect_true(nrow(result) == 1)
})

test_that("handles continuous adjustment variables", {
  data <- create_test_data()

  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure",
    adjust_vars = "age"
  )

  expect_s3_class(result, "riskdiff_result")
  expect_true(!is.na(result$rd))
})

test_that("handles multiple adjustment variables", {
  data <- create_test_data()

  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure",
    adjust_vars = c("age", "sex")
  )

  expect_s3_class(result, "riskdiff_result")
  expect_true(!is.na(result$rd))
})

test_that("handles stratification with small strata", {
  data <- create_test_data()
  data$rare_stratum <- factor(c(rep("Common", 990), rep("Rare", 10)))

  # Test that it runs without error
  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure",
    strata = "rare_stratum"
  )

  # Basic structure checks
  expect_s3_class(result, "riskdiff_result")
  expect_true(nrow(result) >= 1)

  # All model types should be valid
  valid_types <- c("insufficient_data", "failed", "identity", "log", "logit")
  expect_true(all(result$model_type %in% valid_types))
})

test_that("handles factor levels in different orders", {
  data <- create_test_data()
  # Reorder factor levels
  data$exposure <- factor(data$exposure, levels = c("Yes", "No"))

  result <- calc_risk_diff(data, "outcome", "exposure")

  expect_s3_class(result, "riskdiff_result")
  expect_true(!is.na(result$rd))
})

test_that("handles missing data in adjustment variables", {
  data <- create_test_data()
  data$age[1:100] <- NA  # Missing adjustment variable

  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure",
    adjust_vars = "age"
  )

  expect_s3_class(result, "riskdiff_result")
  expect_true(result$n_obs <= nrow(data))  # Should use complete cases
})

test_that("handles missing data in stratification variables", {
  data <- create_test_data()
  data$sex[1:100] <- NA  # Missing stratification variable

  result <- calc_risk_diff(
    data = data,
    outcome = "outcome",
    exposure = "exposure",
    strata = "sex"
  )

  expect_s3_class(result, "riskdiff_result")
  # Should have results for the non-missing strata only
  expect_true(nrow(result) >= 1)
})

test_that("handles data with unusual variable names", {
  data <- create_test_data()
  # Rename variables to have spaces and special characters
  names(data)[names(data) == "outcome"] <- "my outcome"
  names(data)[names(data) == "exposure"] <- "my.exposure"

  expect_error(
    calc_risk_diff(data, "my outcome", "my.exposure"),
    # This should work actually, but let's test with backticks
    NA
  )

  # Test that it works
  result <- calc_risk_diff(data, "my outcome", "my.exposure")
  expect_s3_class(result, "riskdiff_result")
})

test_that("all link functions can be forced", {
  data <- create_test_data()

  # Test that all link functions can be specified
  result_identity <- calc_risk_diff(data, "outcome", "exposure", link = "identity")
  result_log <- calc_risk_diff(data, "outcome", "exposure", link = "log")
  result_logit <- calc_risk_diff(data, "outcome", "exposure", link = "logit")

  expect_s3_class(result_identity, "riskdiff_result")
  expect_s3_class(result_log, "riskdiff_result")
  expect_s3_class(result_logit, "riskdiff_result")

  # Check that the correct model types are used (when they converge)
  if (result_logit$model_type != "failed") {
    expect_equal(result_logit$model_type, "logit")
  }
})

test_that("extreme confidence levels work", {
  data <- create_test_data()

  # Test very narrow CI
  result_narrow <- calc_risk_diff(data, "outcome", "exposure", alpha = 0.50)  # 50% CI

  # Test very wide CI
  result_wide <- calc_risk_diff(data, "outcome", "exposure", alpha = 0.001)  # 99.9% CI

  expect_s3_class(result_narrow, "riskdiff_result")
  expect_s3_class(result_wide, "riskdiff_result")

  # Wide CI should be wider than narrow CI
  width_narrow <- result_narrow$ci_upper - result_narrow$ci_lower
  width_wide <- result_wide$ci_upper - result_wide$ci_lower

  expect_true(width_wide > width_narrow)
})
