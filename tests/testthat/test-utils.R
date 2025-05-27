# ==============================================================================
# tests/testthat/test-utils.R
# ==============================================================================

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

test_that("input validation works correctly", {
  data <- create_test_data()

  # Valid inputs should not error
  expect_silent(.validate_inputs(data, "outcome", "exposure", NULL, NULL, "auto", 0.05))

  # Invalid data frame
  expect_error(.validate_inputs("not_df", "outcome", "exposure", NULL, NULL, "auto", 0.05))

  # Missing variables
  expect_error(.validate_inputs(data, "missing", "exposure", NULL, NULL, "auto", 0.05))

  # Invalid alpha
  expect_error(.validate_inputs(data, "outcome", "exposure", NULL, NULL, "auto", 1.5))

  # Invalid link
  expect_error(.validate_inputs(data, "outcome", "exposure", NULL, NULL, "invalid", 0.05))
})

test_that("data preparation works correctly", {
  data <- create_test_data()
  data$outcome_logical <- as.logical(data$outcome)

  prepared <- .prepare_data(data, "outcome_logical", "exposure", "age", NULL)

  expect_true(is.data.frame(prepared))
  expect_true(is.numeric(prepared$outcome_logical))
  expect_true(is.factor(prepared$exposure))
  expect_true(all(prepared$outcome_logical %in% c(0, 1)))
})

test_that("formula building works correctly", {
  # Simple formula
  formula1 <- .build_formula("outcome", "exposure", NULL)
  expect_equal(deparse(formula1), "outcome ~ exposure")

  # With adjustment
  formula2 <- .build_formula("outcome", "exposure", "age")
  expect_equal(deparse(formula2), "outcome ~ exposure + age")
})

test_that("starting values calculation works", {
  data <- create_test_data()

  start_vals <- .get_starting_values(data, "outcome", "exposure")

  expect_true(is.numeric(start_vals))
  expect_equal(length(start_vals), 2)  # Intercept + 1 predictor
  expect_true(all(start_vals >= 0 & start_vals <= 1))
})

test_that("safe confint works", {
  data <- create_test_data()
  model <- stats::glm(outcome ~ exposure, data = data, family = stats::binomial())

  ci <- .safe_confint(model)

  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), 2)  # Intercept + exposure
  expect_equal(ncol(ci), 2)  # Lower and upper bounds
})
