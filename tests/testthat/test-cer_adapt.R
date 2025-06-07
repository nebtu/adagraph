test_that("dropping hypotheses works", {
  design <- make_example_design()
  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  design_adj <- cer_drop_hypotheses(design, c(TRUE, FALSE, FALSE, FALSE))

  expect_equal(design_adj$ad_weights, c(0, 0.75, 0.25, 0))

  expect_equal(
    unname(design_adj$ad_test_m),
    rbind(
      c(0, 0, 0, 0),
      c(0, 0, 1 / 3, 2 / 3),
      c(0, 1, 0, 0),
      c(0, 1 / 2, 1 / 2, 0)
    )
  )

  design_adj <- cer_drop_hypotheses(design, c(TRUE, FALSE, TRUE, FALSE))

  expect_equal(design_adj$ad_weights, c(0, 1, 0, 0))
  expect_equal(
    unname(design_adj$ad_test_m),
    rbind(c(0, 0, 0, 0), c(0, 0, 0, 1), c(0, 0, 0, 0), c(0, 1, 0, 0))
  )

  design_adj <- cer_adapt(design_adj, weights = c(0, 0.5, 0, 0.5), t = 0.4)

  expect_equal(design_adj$ad_t, 0.4)
  expect_equal(design_adj$ad_weights, c(0, 0.5, 0, 0.5))

  design_with_changes <- cer_design(
    correlation = design$correlation,
    weights = c(0, 0.5, 0, 0.5),
    alpha = design$alpha,
    test_m = design_adj$ad_test_m,
    alpha_spending_f = design$alpha_spending_f,
    t = 0.4
  )

  expect_equal(design_adj$ad_weights_matrix, design_with_changes$weights_matrix)
})

test_that("multiple adaptions work the same as one adaption", {
  design <- make_example_design()
  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  expect_equal(
    cer_drop_hypotheses(
      cer_drop_hypotheses(design, c(TRUE, FALSE, FALSE, FALSE)),
      c(FALSE, TRUE, FALSE, FALSE)
    ),
    cer_drop_hypotheses(design, c(TRUE, TRUE, FALSE, FALSE))
  )

  new_test_m <- rbind(
    H1 = c(0, 1 / 3, 2 / 3, 0),
    H2 = c(1 / 3, 0, 0, 2 / 3),
    H3 = c(0, 1 / 2, 0, 1 / 2),
    H4 = c(1 / 2, 0, 1 / 2, 0)
  )
  expect_equal(
    cer_adapt(
      cer_adapt(
        cer_adapt(design, test_m = new_test_m),
        weights = c(0, 0.5, 0, 0.5)
      ),
      t = 0.4
    ),
    cer_adapt(
      design,
      weights = c(0, 0.5, 0, 0.5),
      test_m = new_test_m,
      t = 0.4
    ),
    list_as_map = TRUE
  )
})

test_that("adapting bounds works", {
  design <- make_example_design()
  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))
  reallocated_t <- (1 / (2 / 35)) / (1 / (2 / 35) + 1 / (1 / 52 + 1 / 53))
  ad_t <- c(1, reallocated_t, 1, reallocated_t)
  design_adj <- cer_drop_hypotheses(design, c(TRUE, FALSE, TRUE, FALSE)) |>
    cer_adapt(weights = c(0, 0.5, 0, 0.5), t = ad_t)

  design_adj_bounds <- cer_adapt_bounds(design_adj)

  tcJ2v <- c(
    0.02371429,
    0,
    0.05413301,
    0.02439775,
    0.02746754,
    0.03825017,
    0.04193392
  )
  expect_equal(round(design_adj_bounds$ad_cJ2[1:7], 8), tcJ2v)
  #note that this also depends on the order of the intersection hypotheses to be the same to pass
})
