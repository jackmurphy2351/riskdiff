# Create test data
create_test_data <- function(n = 1000) {
  set.seed(123)
  data.frame(
    id = 1:n,
    outcome = rbinom(n, 1, 0.2),
    exposure = factor(sample(c("No", "Yes"), n, replace = TRUE)),
    age = rnorm(n, 40, 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE))
  )
}

test_that("create_simple_table works", {
  data <- create_test_data()
  result <- calc_risk_diff(data, "outcome", "exposure")

  table_output <- create_simple_table(result)

  expect_true(is.character(table_output))
  expect_true(nchar(table_output) > 0)
  expect_true(grepl("Risk Diff", table_output))
  expect_true(grepl("95% CI", table_output))
})

test_that("create_rd_table returns appropriate object", {
  data <- create_test_data()
  result <- calc_risk_diff(data, "outcome", "exposure")

  table <- create_rd_table(result)

  # Should return a useful object regardless of kableExtra availability
  expect_true(is.data.frame(table) || inherits(table, "kableExtra"))

  # Should have expected structure if it's a data frame
  if (is.data.frame(table)) {
    expect_true("Exposure" %in% names(table))
    expect_true("Risk Difference" %in% names(table))
  }
})

test_that("column name cleaning works", {
  original_names <- c("exposure_var", "rd_formatted", "ci_formatted", "p_value_formatted", "model_type")
  cleaned_names <- .clean_column_names(original_names)

  expected_names <- c("Exposure", "Risk Difference", "95% CI", "P-value", "Model")
  expect_equal(cleaned_names, expected_names)
})

test_that("strata detection works", {
  # Create result with stratification variables
  result <- data.frame(
    exposure_var = "smoking",
    rd = 0.05,
    sex = "Male",
    residence = "Urban"
  )

  detected_strata <- .detect_strata_vars(result)
  expect_true("sex" %in% detected_strata)
  expect_true("residence" %in% detected_strata)
  expect_false("exposure_var" %in% detected_strata)
})

test_that("formatting handles edge cases", {
  # Test with missing values
  result <- data.frame(
    exposure_var = "smoking",
    rd = c(0.05, NA),
    ci_lower = c(0.01, NA),
    ci_upper = c(0.09, NA),
    p_value = c(0.02, NA),
    model_type = c("identity", "failed")
  )
  class(result) <- c("riskdiff_result", "data.frame")

  formatted <- format_risk_diff(result)

  expect_true(is.data.frame(formatted))
  expect_equal(nrow(formatted), 2)
  expect_true(is.na(formatted$rd[2]))
})

test_that("print method works without errors", {
  data <- create_test_data()
  result <- calc_risk_diff(data, "outcome", "exposure")

  # Test that print doesn't error
  expect_output(print(result), "Risk Difference Analysis Results")
  expect_output(print(result), "Confidence level")
})
