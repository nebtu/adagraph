test_that("Simple design works", {
  correlation <- rbind(H1 = c(1, NA), H2 = c(NA, 1))
  weights <- c(2 / 3, 1 / 3)
  test_m <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.025
  design <- adagraph_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = test_m
  )
  expect_equal(design$correlation, correlation)
  expect_equal(design$weights, weights)
  expect_equal(design$test_m, test_m)
  expect_equal(design$alpha, alpha)
  expect_equal(unname(design$hyp_matrix), rbind(c(0, 1), c(1, 0), c(1, 1)))
  expect_equal(
    unname(design$weights_matrix),
    rbind(c(0, 1), c(1, 0), c(2 / 3, 1 / 3))
  )
  expect_equal(design$closed_matrix, rbind(c(2, 1), c(3, 3)))
})

test_that("Correct validation of adagraph_design", {
  correlation <- rbind(H1 = c(1, NA), H2 = c(NA, 1))
  weights <- c(2 / 3, 1 / 3)
  test_m <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.025
  expect_error(
    adagraph_design(
      correlation = rbind(c(1, NA)),
      weights = weights,
      alpha = alpha,
      test_m = test_m
    ),
    class = "invalid_argument_correlation"
  )
  expect_error(
    adagraph_design(
      correlation = "correlation",
      weights = weights,
      alpha = alpha,
      test_m = test_m
    ),
    class = "invalid_argument_correlation"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = 1,
      alpha = alpha,
      test_m = test_m
    ),
    class = "invalid_argument_weights"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = "0.0025",
      test_m = test_m
    ),
    class = "invalid_argument_alpha"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = 1.0025,
      test_m = test_m
    ),
    class = "invalid_argument_alpha"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      test_m = "test_m"
    ),
    class = "invalid_argument_test_m"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      test_m = rbind(c(1, 0, 0), c(1, 0, 0), c(1, 0, 0))
    ),
    class = "invalid_argument_test_m"
  )
})
