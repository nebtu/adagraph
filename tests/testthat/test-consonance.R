test_that("basic functionality", {
  design <- make_example_design()

  expect_equal(check_consonance(design, adapted = FALSE, stage = "both"), TRUE)
  expect_equal(check_consonance(design, adapted = FALSE, stage = "final"), TRUE)
  expect_equal(
    check_consonance(design, stage = "interim"),
    TRUE
  )
  expect_equal(
    check_consonance(design, adapted = FALSE, use_weights = TRUE),
    TRUE
  )
  expect_equal(
    .consonance_intersec(design$weights_matrix, design$hyp_matrix),
    rep(TRUE, 15)
  )

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  reallocated_t <- (1 / (2 / 35)) / (1 / (2 / 35) + 1 / (1 / 52 + 1 / 53))
  ad_t <- c(1, reallocated_t, 1, reallocated_t)
  design_adj <- cer_drop_hypotheses(design, c(1, 3)) |>
    cer_adapt(weights = c(0, 0.5, 0, 0.5), time = ad_t)

  expect_equal(
    .consonance_intersec(design_adj$ad_bounds_2, design$hyp_matrix),
    c(rep(TRUE, 2), FALSE, rep(TRUE, 2), rep(FALSE, 2), rep(TRUE, 8))
  )

  expect_equal(
    check_consonance(design_adj, adapted = TRUE, stage = "both"),
    FALSE
  )
  expect_equal(
    check_consonance(design_adj, adapted = TRUE, stage = "final"),
    FALSE
  )
  expect_equal(
    check_consonance(design_adj, adapted = TRUE, use_weights = TRUE),
    TRUE
  )
})

test_that("non-consonant design", {
  m <- rbind(
    H1 = c(0, 0, 1, 0),
    H2 = c(0, 0, 0, 1),
    H3 = c(0, 1, 0, 0),
    H4 = c(1, 0, 0, 0)
  )

  weights <- c(1 / 2, 1 / 2, 0, 0)
  t <- 0.5
  alpha = 0.025
  as <- function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))

  design <- multiarm_cer_design(
    controls = 2,
    treatment_assoc = c(1, 1, 2, 2),
    n_controls = 20,
    n_treatments = 100,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as
  )

  expect_equal(check_consonance(design), FALSE)
  expect_equal(
    .consonance_intersec(design$bounds_2, design$hyp_matrix),
    c(rep(TRUE, 12), rep(FALSE, 3))
  )
})

test_that("validity testing", {
  design <- make_example_design()

  expect_error(
    check_consonance(design, adapted = TRUE),
    class = "adagraph_no_adaptation"
  )

  expect_error(
    check_consonance(design, stage = "test"),
    class = "rlang_error"
  )

  expect_error(
    check_consonance(design, use_weights = "test"),
    class = "adagraph_invalid_argument"
  )
})
