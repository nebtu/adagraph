# TODO: test that giving weight 0 gives correct bounds
#       test that single hypotheses give correct bounds
#       test how correlation NA or 0 works

test_that("single hypothesis works", {
  design <- cer_design(
    correlation = diag(1),
    weights = 1,
    alpha = 0.05,
    test_m = diag(1),
    alpha_spending_f = function(x, t) x * t,
    t = 0.5
  )

  expect_equal(unname(design$bounds_1), cbind(0.025))
  #expect_equal(unname(design$bounds_2), cbind(???))
})

test_that("Example from paper gives correct bounds", {
  design <- make_example_design()

  hyp_bound_1_pairs <- list(
    #see table 6 in paper
    "1,1,1,1" = c(0.000782, 0.000782, 0, 0),
    "0,1,1,1" = c(0, 0.001144, 0.000381, 0),
    "1,0,1,1" = c(0.001144, 0, 0, 0.000381),
    "1,1,0,1" = c(0.000782, 0.000782, 0, 0),
    "1,1,1,0" = c(0.000782, 0.000782, 0, 0),
    "0,0,1,1" = c(0, 0, 0.000782, 0.000782),
    "0,1,0,1" = c(0, 0.001525, 0, 0),
    "0,1,1,0" = c(0, 0.001144, 0.000381, 0),
    "1,0,0,1" = c(0.001144, 0, 0, 0.000381),
    "1,0,1,0" = c(0.001525, 0, 0, 0),
    "1,1,0,0" = c(0.000782, 0.000782, 0, 0),
    "0,0,0,1" = c(0, 0, 0, 0.001525),
    "0,0,1,0" = c(0, 0, 0.001525, 0),
    "0,1,0,0" = c(0, 0.001525, 0, 0),
    "1,0,0,0" = c(0.001525, 0, 0, 0)
  )

  expect_equal(dim(design$bounds_1), c(15, 4))
  for (i in 1:dim(design$bounds_1)[1]) {
    hyp <- paste(design$hyp_matrix[i, ], collapse = ",")
    expect_equal(round(design$bounds_1[i, ], 6), hyp_bound_1_pairs[[hyp]])
  }

  hyp_bound_2_pairs <- list(
    #see table 6 in paper
    "1,1,1,1" = c(0.0132, 0.0132, 0, 0),
    "0,1,1,1" = c(0, 0.0183, 0.00610, 0),
    "1,0,1,1" = c(0.0183, 0, 0, 0.00610),
    "1,1,0,1" = c(0.0132, 0.0132, 0, 0),
    "1,1,1,0" = c(0.0132, 0.0132, 0, 0),
    "0,0,1,1" = c(0, 0, 0.0132, 0.0132),
    "0,1,0,1" = c(0, 0.0245, 0, 0),
    "0,1,1,0" = c(0, 0.0183, 0.00610, 0),
    "1,0,0,1" = c(0.0183, 0, 0, 0.00610),
    "1,0,1,0" = c(0.0245, 0, 0, 0),
    "1,1,0,0" = c(0.0132, 0.0132, 0, 0),
    "0,0,0,1" = c(0, 0, 0, 0.0245),
    "0,0,1,0" = c(0, 0, 0.0245, 0),
    "0,1,0,0" = c(0, 0.0245, 0, 0),
    "1,0,0,0" = c(0.0245, 0, 0, 0)
  )

  expect_equal(dim(design$bounds_2), c(15, 4))
  for (i in 1:dim(design$bounds_2)[1]) {
    hyp <- paste(design$hyp_matrix[i, ], collapse = ",")
    expect_equal(round(design$bounds_2[i, ], 4), hyp_bound_2_pairs[[hyp]])
  }
})

test_that("Simple CER design works", {
  correlation <- rbind(H1 = c(1, NA), H2 = c(NA, 1))
  weights <- c(2 / 3, 1 / 3)
  test_m <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.025
  alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  design <- cer_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t
  )
  expect_s3_class(design, c("cer_design", "adagraph_design"))
  expect_equal(design$correlation, correlation)
  expect_equal(design$weights, weights)
  expect_equal(design$test_m, test_m)
  expect_equal(design$alpha, alpha)
  expect_equal(design$alpha_spending_f, alpha_spending_f)
  expect_equal(design$t, t)
})

test_that("Correct validation of cer_design", {
  correlation <- rbind(H1 = c(1, NA), H2 = c(NA, 1))
  weights <- c(2 / 3, 1 / 3)
  test_m <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.025
  alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  expect_error(
    cer_design(
      correlation = rbind(c(1, NA)),
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_correlation"
  )
  expect_error(
    cer_design(
      correlation = "correlation",
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_correlation"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = 1,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_weights"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = "0.05",
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_alpha"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = 1.05,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_alpha"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      test_m = "test_m",
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_test_m"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      test_m = rbind(c(1, 0, 0), c(1, 0, 0), c(1, 0, 0)),
      alpha_spending_f = alpha_spending_f,
      t = t
    ),
    class = "invalid_argument_test_m"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = "0.5"
    ),
    class = "invalid_argument_t"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = c(0.5, 0.5)
    ),
    class = "invalid_argument_t"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      test_m = test_m,
      alpha_spending_f = alpha_spending_f,
      t = 1.5
    ),
    class = "invalid_argument_t"
  )
  expect_error(
    cer_design(
      correlation = correlation,
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
