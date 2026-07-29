# TODO: test that giving weight 0 gives correct bounds
#       test that single hypotheses give correct bounds
#       test how correlation NA or 0 works

test_that("single hypothesis works", {
  design <- cer_design(
    correlation = diag(1),
    weights = 1,
    alpha = 0.05,
    transitions = diag(1),
    alpha_spending = function(x, t) x * t,
    t = 0.5
  )

  expect_equal(unname(design$bounds_1), cbind(0.025))
  #expect_equal(unname(design$bounds_2), cbind(???))
})

test_that("correlation = NA defaults to identity-diagonal NA matrix", {
  as <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))

  # Using correlation = NA (the default)
  design_na <- cer_design(
    weights = c(2 / 3, 1 / 3),
    transitions = rbind(c(0, 1), c(1, 0)),
    correlation = NA,
    alpha = 0.05,
    alpha_spending = as,
    t = 0.5
  )

  # Equivalent explicit correlation matrix
  design_explicit <- cer_design(
    weights = c(2 / 3, 1 / 3),
    transitions = rbind(c(0, 1), c(1, 0)),
    correlation = rbind(c(1, NA), c(NA, 1)),
    alpha = 0.05,
    alpha_spending = as,
    t = 0.5
  )

  expect_equal(design_na[["correlation"]], design_explicit[["correlation"]])
  expect_equal(design_na[["bounds_1"]], design_explicit[["bounds_1"]])
  expect_equal(design_na[["bounds_2"]], design_explicit[["bounds_2"]])
})

test_paper_example <- function() {
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
    expect_equal(
      round(unname(design$bounds_1[i, ]), 6),
      hyp_bound_1_pairs[[hyp]]
    )
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
    expect_equal(
      round(unname(design$bounds_2[i, ]), 4),
      hyp_bound_2_pairs[[hyp]]
    )
  }
}

test_that("Example from paper gives correct bounds", {
  test_paper_example()
})

test_that("Using future for cer design", {
  local_future_plan(future::sequential)

  test_paper_example()
})

test_that("Using future in parallel for cer design", {
  skip_on_cran()
  local_future_plan(future::multisession)

  test_paper_example()
})

test_that("Simple CER design works", {
  correlation <- rbind(c(1, NA), c(NA, 1))
  weights <- c(2 / 3, 1 / 3)
  transitions <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.025
  alpha_spending <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  design <- cer_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    transitions = transitions,
    alpha_spending = alpha_spending,
    t = t
  )
  expect_s3_class(design, c("cer_design", "adagraph_design"))
  expect_equal(unname(design$correlation), correlation)
  expect_equal(unname(design$weights), weights)
  expect_equal(unname(design$transitions), transitions)
  expect_equal(design$alpha, alpha)
  expect_equal(design$alpha_interim, alpha_spending(alpha, t))
  expect_equal(design$t, t)
})

test_that("Correct validation of cer_design", {
  correlation <- rbind(c(1, NA), c(NA, 1))
  weights <- c(2 / 3, 1 / 3)
  transitions <- rbind(c(0, 1), c(1, 0))
  alpha <- 0.025
  alpha_spending <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  t <- 0.5
  expect_error(
    cer_design(
      weights = weights,
      correlation = rbind(c(1, NA)),
      alpha = alpha,
      transitions = transitions,
      alpha_spending = alpha_spending,
      t = t
    ),
    class = "adagraph_standardize_length"
  )
  expect_error(
    cer_design(
      weights = weights,
      correlation = "correlation",
      alpha = alpha,
      transitions = transitions,
      alpha_spending = alpha_spending,
      t = t
    ),
    class = "adagraph_invalid_correlation"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = "0.05",
      transitions = transitions,
      alpha_spending = alpha_spending,
      t = t
    ),
    class = "adagraph_invalid_alpha"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = 1.05,
      transitions = transitions,
      alpha_spending = alpha_spending,
      t = t
    ),
    class = "adagraph_invalid_alpha"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = "transitions",
      alpha_spending = alpha_spending,
      t = t
    ),
    class = "adagraph_invalid_transitions"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = rbind(c(1, 0, 0), c(1, 0, 0), c(1, 0, 0)),
      alpha_spending = alpha_spending,
      t = t
    ),
    class = "adagraph_standardize_length"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = transitions,
      alpha_spending = alpha_spending,
      t = "0.5"
    ),
    class = "adagraph_invalid_t"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = transitions,
      alpha_spending = alpha_spending,
      t = c(0.5, 0.5)
    ),
    class = "adagraph_invalid_t"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = transitions,
      alpha_spending = alpha_spending,
      t = 1.5
    ),
    class = "adagraph_invalid_t"
  )
  expect_error(
    cer_design(
      correlation = correlation,
      weights = weights,
      alpha = alpha,
      transitions = transitions,
      alpha_spending = alpha_spending,
      t = t,
      seq_bonf = "TRUE"
    ),
    class = "adagraph_invalid_seq_bonf"
  )
  for (bad in list(0, -0.001, 0.025, 0.1, c(0.001, 0.002), "test_string")) {
    expect_error(
      cer_design(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        transitions = transitions,
        alpha_spending = bad,
        t = t,
        seq_bonf = TRUE
      ),
      class = "adagraph_invalid_alpha_spending"
    )
  }
})

test_that("scalar alpha_1 is equivalent to a constant spending function", {
  args <- list(
    correlation = rbind(c(1, NA), c(NA, 1)),
    weights = c(2 / 3, 1 / 3),
    transitions = rbind(c(0, 1), c(1, 0)),
    alpha = 0.025,
    t = 0.5
  )
  as <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  a1 <- as(0.025, 0.5)

  d_fun <- do.call(cer_design, c(args, alpha_spending = as))
  d_num <- do.call(cer_design, c(args, alpha_spending = a1))

  expect_equal(d_num$bounds_1, d_fun$bounds_1)
  expect_equal(d_num$bounds_2, d_fun$bounds_2)
  expect_equal(d_num$cJ1, d_fun$cJ1)
  expect_equal(d_num$cJ2, d_fun$cJ2)
})
