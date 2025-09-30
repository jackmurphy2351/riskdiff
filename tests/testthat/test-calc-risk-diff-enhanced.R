# Enhanced tests for calc_risk_diff.R to improve coverage
# Add these tests to tests/testthat/test-calc_risk_diff_enhanced.R (create new file)

# Create specialized test data for boundary conditions
create_boundary_test_data <- function(scenario = "normal") {
  set.seed(2025)
  n <- 200

  switch(scenario,
         "normal" = {
           data.frame(
             outcome = rbinom(n, 1, 0.2),
             exposure = factor(sample(c("No", "Yes"), n, replace = TRUE)),
             age = rnorm(n, 40, 10),
             sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
             region = factor(sample(c("North", "South", "East"), n, replace = TRUE))
           )
         },
         "high_prevalence" = {
           # High outcome prevalence to test upper boundary
           data.frame(
             outcome = rbinom(n, 1, 0.85),
             exposure = factor(sample(c("No", "Yes"), n, replace = TRUE)),
             age = rnorm(n, 40, 10)
           )
         },
         "rare_outcome" = {
           # Very rare outcome to test lower boundary
           data.frame(
             outcome = rbinom(n, 1, 0.02),
             exposure = factor(sample(c("No", "Yes"), n, replace = TRUE)),
             age = rnorm(n, 40, 10)
           )
         },
         "separation" = {
           # Perfect separation scenario
           exposure_vals <- rep(c("No", "Yes"), each = n/2)
           outcome_vals <- c(rep(0, n/2), rep(1, n/2))
         }
  )
}
