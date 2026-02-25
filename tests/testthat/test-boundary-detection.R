# Create test data with various boundary scenarios
create_boundary_test_data <- function(scenario = "normal") {
  set.seed(123)
  n <- 200

  switch(scenario,
         "normal" = {
           data.frame(
             outcome = rbinom(n, 1, 0.3),
             exposure = factor(sample(c("No", "Yes"), n, replace = TRUE)),
             covariate = rnorm(n)
           )
         },
         "high_risk" = {
           # Data likely to cause upper boundary issues
           x <- rep(c(0, 1), each = n/2)
           prob <- plogis(-1 + 4*x)  # High baseline risk
           data.frame(
             outcome = rbinom(n, 1, prob),
             exposure = factor(c("No", "Yes")[x + 1], levels = c("No", "Yes")),
             covariate = rnorm(n)
           )
         },
         "perfect_separation" = {
           # Perfect separation scenario
           data.frame(
             outcome = c(rep(0, n/2), rep(1, n/2)),
             exposure = factor(c(rep("No", n/2), rep("Yes", n/2)), levels = c("No", "Yes")),
             covariate = c(rnorm(n/2, -2), rnorm(n/2, 2))
           )
         },
         "very_low_risk" = {
           # Very low baseline risk
           data.frame(
             outcome = rbinom(n, 1, 0.01),
             exposure = factor(sample(c("No", "Yes"), n, replace = TRUE)),
             covariate = rnorm(n)
           )
         }
  )
}

test_that("boundary detection function exists and works", {
  # Test that the boundary detection function exists
  expect_true(exists(".detect_boundary", where = asNamespace("riskdiff")))

  # Test with normal data
  data <- create_boundary_test_data("normal")

  # Try to fit a model
  model <- tryCatch({
    stats::glm(outcome ~ exposure + covariate,
               data = data,
               family = stats::binomial(link = "identity"))
  }, error = function(e) {
    # If identity link fails, try logit
    stats::glm(outcome ~ exposure + covariate,
               data = data,
               family = stats::binomial(link = "logit"))
  })

  # Test boundary detection
  boundary_info <- riskdiff:::.detect_boundary(model, data)

  expect_true(is.list(boundary_info))
  expect_true("boundary_detected" %in% names(boundary_info))
  expect_true("boundary_type" %in% names(boundary_info))
  expect_true(is.logical(boundary_info$boundary_detected))
  expect_true(is.character(boundary_info$boundary_type))
})

test_that("get_valid_boundary_types function works", {
  valid_types <- get_valid_boundary_types()

  expect_true(is.character(valid_types))
  expect_true(length(valid_types) > 0)
  expect_true("none" %in% valid_types)
  expect_true("upper_boundary_exact" %in% valid_types)
  expect_true("separation" %in% valid_types)
})

test_that("boundary detection returns valid boundary types", {
  valid_types <- get_valid_boundary_types()

  # Test with different scenarios
  scenarios <- c("normal", "high_risk", "very_low_risk")

  for (scenario in scenarios) {
    data <- create_boundary_test_data(scenario)

    # Try multiple link functions
    links <- c("identity", "logit", "log")

    for (link in links) {
      model <- tryCatch({
        if (link == "log") {
          # For log link, we need the logbin package approach or manual fitting
          stats::glm(outcome ~ exposure + covariate,
                     data = data,
                     family = stats::binomial(link = "logit"))  # Fallback to logit
        } else {
          stats::glm(outcome ~ exposure + covariate,
                     data = data,
                     family = stats::binomial(link = link))
        }
      }, error = function(e) NULL)

      if (!is.null(model)) {
        boundary_info <- riskdiff:::.detect_boundary(model, data, verbose = FALSE)

        # This is the key test that was failing
        expect_true(
          boundary_info$boundary_type %in% valid_types,
          info = paste("Scenario:", scenario, "Link:", link,
                       "Returned type:", boundary_info$boundary_type)
        )
      }
    }
  }
})

test_that("boundary detection handles edge cases gracefully", {
  valid_types <- get_valid_boundary_types()

  # Test with NULL model
  boundary_info <- riskdiff:::.detect_boundary(NULL, data.frame())
  expect_true(boundary_info$boundary_type %in% valid_types)
  expect_equal(boundary_info$boundary_type, "invalid_model")

  # Test with non-GLM object
  boundary_info <- riskdiff:::.detect_boundary(list(), data.frame())
  expect_true(boundary_info$boundary_type %in% valid_types)
  expect_equal(boundary_info$boundary_type, "invalid_model")

  # Test with minimal data
  small_data <- data.frame(
    outcome = c(0, 1),
    exposure = factor(c("No", "Yes"))
  )

  model <- tryCatch({
    stats::glm(outcome ~ exposure, data = small_data, family = stats::binomial())
  }, error = function(e) NULL)

  if (!is.null(model)) {
    boundary_info <- riskdiff:::.detect_boundary(model, small_data)
    expect_true(boundary_info$boundary_type %in% valid_types)
  }
})

test_that("boundary detection handles convergence edge cases systematically", {
  valid_types <- get_valid_boundary_types()

  # Create systematically challenging datasets
  test_scenarios <- list(
    high_risk = create_boundary_test_data("high_risk"),
    perfect_separation = create_boundary_test_data("perfect_separation"),
    very_low_risk = create_boundary_test_data("very_low_risk")
  )

  boundary_summary <- list()

  for (scenario_name in names(test_scenarios)) {
    data <- test_scenarios[[scenario_name]]

    # Try to fit model with identity link (most likely to have boundary issues)
    model <- tryCatch({
      stats::glm(outcome ~ exposure,
                 data = data,
                 family = stats::binomial(link = "identity"),
                 control = stats::glm.control(maxit = 100))
    }, error = function(e) {
      # If that fails, try logit as fallback
      tryCatch({
        stats::glm(outcome ~ exposure,
                   data = data,
                   family = stats::binomial(link = "logit"))
      }, error = function(e2) NULL)
    })

    if (!is.null(model)) {
      boundary_info <- riskdiff:::.detect_boundary(model, data, verbose = TRUE)
      boundary_summary[[scenario_name]] <- boundary_info$boundary_type

      # THE CRITICAL TEST THAT WAS FAILING:
      expect_true(
        boundary_info$boundary_type %in% valid_types,
        info = paste("Scenario:", scenario_name,
                     "returned boundary_type:", boundary_info$boundary_type,
                     "Valid types:", paste(valid_types, collapse = ", "))
      )
    } else {
      boundary_summary[[scenario_name]] <- "model_fit_failed"
    }
  }

  # Print summary for debugging
  cat("\nBoundary detection behavior summary:\n")
  for (scenario in names(boundary_summary)) {
    cat(scenario, ": detected", boundary_summary[[scenario]], "\n")
  }

  # Verify we detected some boundary issues
  detected_boundaries <- sapply(boundary_summary, function(x) x != "none" && x != "model_fit_failed")
  expect_true(any(detected_boundaries),
              info = "At least one test scenario should detect boundary issues")
})
