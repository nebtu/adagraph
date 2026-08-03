test_that("Simple design works", {
  correlation <- rbind(c(1, NA), c(NA, 1))
  weights <- c(2 / 3, 1 / 3)
  transitions <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.025
  design <- adagraph_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    transitions = transitions
  )
  expect_equal(unname(design$correlation), correlation)
  expect_equal(unname(design$weights), weights)
  expect_equal(unname(design$transitions), transitions)
  expect_equal(design$alpha, alpha)
  expect_equal(unname(design$hyp_matrix), rbind(c(0, 1), c(1, 0), c(1, 1)))
  expect_equal(
    unname(design$weights_matrix),
    rbind(c(0, 1), c(1, 0), c(2 / 3, 1 / 3))
  )
  expect_equal(unname(design$closed_matrix), rbind(c(2, 1), c(3, 3)))
})

test_that("Correct validation of adagraph_design", {
  correlation <- rbind(H1 = c(1, NA), H2 = c(NA, 1))
  weights <- c(2 / 3, 1 / 3)
  transitions <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.025
  expect_error(
    adagraph_design(
      correlation = rbind(c(1, NA)),
      weights = weights,
      alpha = alpha,
      transitions = transitions
    ),
    class = "adagraph_standardize_length"
  )
  expect_error(
    adagraph_design(
      correlation = "correlation",
      weights = weights,
      alpha = alpha,
      transitions = transitions
    ),
    class = "adagraph_standardize_length"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = "0.0025",
      transitions = transitions
    ),
    class = "adagraph_invalid_alpha"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = 1.0025,
      transitions = transitions
    ),
    class = "adagraph_invalid_alpha"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = "transitions"
    ),
    class = "adagraph_standardize_length"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = rbind(c(1, 0, 0), c(1, 0, 0), c(1, 0, 0))
    ),
    class = "adagraph_standardize_length"
  )
  expect_error(
    adagraph_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = transitions,
      names = 1
    ),
    class = "adagraph_invalid_names"
  )
})
