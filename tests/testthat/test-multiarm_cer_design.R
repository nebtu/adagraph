# Test multiarm_cer_design functionality

test_that("Uncorrelated multiarm design works (different controls)", {
  # Two treatments using different control groups - should be uncorrelated
  controls <- 2
  treatment_assoc <- c(1, 2)  # Treatment 1 uses control 1, treatment 2 uses control 2
  n_controls <- 50
  n_treatments <- 40
  weights <- c(0.5, 0.5)
  test_m <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.05
  alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  
  design <- multiarm_cer_design(
    controls = controls,
    treatment_assoc = treatment_assoc,
    n_controls = n_controls,
    n_treatments = n_treatments,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t
  )
  
  # Check basic properties
  expect_s3_class(design, c("multiarm_cer_design", "cer_design", "adagraph_design"))
  expect_equal(design$controls, controls)
  expect_equal(design$treatment_assoc, treatment_assoc)
  expect_equal(design$n_controls, rep(n_controls, controls))
  expect_equal(design$n_treatments, rep(n_treatments, length(treatment_assoc)))
  expect_equal(design$weights, weights)
  expect_equal(design$alpha, alpha)
  expect_equal(design$test_m, test_m)
  expect_equal(design$t, t)
  
  # Check correlation matrix - should be uncorrelated (NA off-diagonal)
  expected_correlation <- matrix(c(1, NA, NA, 1), nrow = 2, ncol = 2)
  expect_equal(design$correlation, expected_correlation)
  
  # Check that bounds matrices exist and have correct dimensions
  expect_true(is.matrix(design$bounds_1))
  expect_true(is.matrix(design$bounds_2))
  expect_equal(ncol(design$bounds_1), 2)  # Two hypotheses
  expect_equal(ncol(design$bounds_2), 2)  # Two hypotheses
})

test_that("Correlated multiarm design works (same control)", {
  # Two treatments using the same control group - should be correlated
  # Using equal sample sizes for control and treatments to get correlation = 1/2
  controls <- 1
  treatment_assoc <- c(1, 1)  # Both treatments use control 1
  n_controls <- 50
  n_treatments <- 50  # Equal to n_controls to get correlation = 1/2
  weights <- c(0.5, 0.5)
  test_m <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.05
  alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  
  design <- multiarm_cer_design(
    controls = controls,
    treatment_assoc = treatment_assoc,
    n_controls = n_controls,
    n_treatments = n_treatments,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t
  )
  
  # Check basic properties
  expect_s3_class(design, c("multiarm_cer_design", "cer_design", "adagraph_design"))
  expect_equal(design$controls, controls)
  expect_equal(design$treatment_assoc, treatment_assoc)
  expect_equal(design$n_controls, rep(n_controls, controls))
  expect_equal(design$n_treatments, rep(n_treatments, length(treatment_assoc)))
  
  # Check correlation matrix - should be correlated with correlation = 1/2
  expected_correlation <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
  expect_equal(design$correlation, expected_correlation, tolerance = 1e-10)
})

test_that("Different sample sizes work correctly", {
  # Test with different sample sizes for controls and treatments
  controls <- 2
  treatment_assoc <- c(1, 2)
  n_controls <- c(60, 70)  # Different control group sizes
  n_treatments <- c(30, 40)  # Different treatment group sizes
  weights <- c(0.6, 0.4)
  test_m <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.05
  alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  
  design <- multiarm_cer_design(
    controls = controls,
    treatment_assoc = treatment_assoc,
    n_controls = n_controls,
    n_treatments = n_treatments,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t
  )
  
  # Check that sample sizes are preserved correctly
  expect_equal(design$n_controls, n_controls)
  expect_equal(design$n_treatments, n_treatments)
  
  # Check that correlation matrix is still uncorrelated (different controls)
  expected_correlation <- matrix(c(1, NA, NA, 1), nrow = 2, ncol = 2)
  expect_equal(design$correlation, expected_correlation)
})

test_that("Complex correlated design with multiple treatments per control", {
  # Test with 3 treatments: 2 sharing control 1, 1 using control 2
  controls <- 2
  treatment_assoc <- c(1, 1, 2)  # Treatments 1&2 share control 1, treatment 3 uses control 2
  n_controls <- 50
  n_treatments <- 40
  weights <- c(1/3, 1/3, 1/3)
  test_m <- rbind(
    c(0, 0.5, 0.5),
    c(0.5, 0, 0.5),
    c(0.5, 0.5, 0)
  )
  alpha <- 0.05
  alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  
  design <- multiarm_cer_design(
    controls = controls,
    treatment_assoc = treatment_assoc,
    n_controls = n_controls,
    n_treatments = n_treatments,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t
  )
  
  # Check basic properties
  expect_equal(design$controls, controls)
  expect_equal(design$treatment_assoc, treatment_assoc)
  expect_equal(length(design$weights), 3)
  
  # Check correlation structure
  # Treatments 1 and 2 should be correlated (same control)
  # Treatment 3 should be uncorrelated with 1 and 2 (different control)
  expect_equal(design$correlation[1, 1], 1)
  expect_equal(design$correlation[2, 2], 1)
  expect_equal(design$correlation[3, 3], 1)
  
  # Treatments 1 and 2 are correlated
  expect_true(design$correlation[1, 2] > 0)
  expect_true(design$correlation[1, 2] < 1)
  expect_equal(design$correlation[1, 2], design$correlation[2, 1])
  
  # Treatment 3 is uncorrelated with 1 and 2
  expect_true(is.na(design$correlation[1, 3]))
  expect_true(is.na(design$correlation[3, 1]))
  expect_true(is.na(design$correlation[2, 3]))
  expect_true(is.na(design$correlation[3, 2]))
})

test_that("Parameter validation works correctly", {
  # Valid base parameters
  controls <- 1
  treatment_assoc <- c(1, 1)
  n_controls <- 50
  n_treatments <- 40
  weights <- c(0.5, 0.5)
  test_m <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.05
  alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  
  # Test invalid controls
  expect_error(
    multiarm_cer_design(
      controls = 0,
      treatment_assoc = treatment_assoc,
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_controls"
  )
  
  expect_error(
    multiarm_cer_design(
      controls = 1.5,
      treatment_assoc = treatment_assoc,
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_controls"
  )
  
  # Test invalid treatment_assoc
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = c(0, 1),
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_treatment_assoc"
  )
  
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = c(1, 2),  # treatment_assoc > controls
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_treatment_assoc"
  )
  
  # Test invalid sample sizes
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = treatment_assoc,
      n_controls = -1,
      n_treatments = n_treatments,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_n_controls"
  )
  
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = treatment_assoc,
      n_controls = n_controls,
      n_treatments = 0,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_n_treatments"
  )
  
  # Test invalid weights
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = treatment_assoc,
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = c(0.5),  # Wrong length
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_weights"
  )
  
  # Test invalid alpha
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = treatment_assoc,
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = weights,
      alpha = 1.5,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_alpha"
  )
  
  # Test invalid t
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = treatment_assoc,
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = 1.5
    ),
    class = "invalid_argument_t"
  )
  
  # Test invalid test_m dimensions
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = treatment_assoc,
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = weights,
      alpha = alpha,
      test_m = rbind(c(0, 1, 0), c(1, 0, 0), c(0, 0, 1)),  # 3x3 matrix for 2 treatments
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_test_m"
  )
  
  # Test invalid seq_bonf
  expect_error(
    multiarm_cer_design(
      controls = controls,
      treatment_assoc = treatment_assoc,
      n_controls = n_controls,
      n_treatments = n_treatments,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t,
      seq_bonf = "TRUE"
    ),
    class = "invalid_argument_seq_bonf"
  )
})

test_that("Single treatment works", {
  # Test edge case with single treatment
  controls <- 1
  treatment_assoc <- c(1)
  n_controls <- 50
  n_treatments <- 40
  weights <- c(1)
  test_m <- matrix(1)
  alpha <- 0.05
  alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  
  design <- multiarm_cer_design(
    controls = controls,
    treatment_assoc = treatment_assoc,
    n_controls = n_controls,
    n_treatments = n_treatments,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t
  )
  
  # Check basic properties
  expect_s3_class(design, c("multiarm_cer_design", "cer_design", "adagraph_design"))
  expect_equal(design$controls, controls)
  expect_equal(design$treatment_assoc, treatment_assoc)
  expect_equal(dim(design$correlation), c(1, 1))
  expect_equal(design$correlation[1, 1], 1)
})