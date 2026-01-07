test_that("basic functionality", {
  design <- make_example_design()

  expect_equal(check_consonance(design, adapted = FALSE, stage = "both"), TRUE)
  expect_equal(check_consonance(design, adapted = FALSE, stage = "final"), TRUE)
  expect_equal(
    check_consonance(design, stage = "interim"),
    TRUE
  )
  expect_equal(check_consonance(design, adapted = FALSE, weights = TRUE), TRUE)
  expect_equal(
    .consonance_intersec(design$weights_matrix, design$hyp_matrix),
    rep(TRUE, 15)
  )

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  reallocated_t <- (1 / (2 / 35)) / (1 / (2 / 35) + 1 / (1 / 52 + 1 / 53))
  ad_t <- c(1, reallocated_t, 1, reallocated_t)
  design_adj <- cer_drop_hypotheses(design, c(TRUE, FALSE, TRUE, FALSE)) |>
    cer_adapt(weights = c(0, 0.5, 0, 0.5), time = ad_t)

  expect_equal(
    .consonance_intersec(design_adj$ad_bounds_2, design$hyp_matrix),
    c(rep(TRUE, 5), FALSE, rep(TRUE, 9))
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
    check_consonance(design_adj, adapted = TRUE, weights = TRUE),
    TRUE
  )
})
