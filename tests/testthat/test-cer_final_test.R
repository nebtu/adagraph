test_that("example from paper works", {
  design <- make_example_design()

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  reallocated_t <- (1 / (2 / 35)) / (1 / (2 / 35) + 1 / (1 / 52 + 1 / 53))
  ad_t <- c(1, reallocated_t, 1, reallocated_t)
  design_adj <- cer_drop_hypotheses(design, c(TRUE, FALSE, TRUE, FALSE)) |>
    cer_adapt(weights = c(0, 0.5, 0, 0.5), time = ad_t)

  design_tested <- cer_final_test(design_adj, c(NA, 0.0111, NA, 0.0234))
  expect_equal(design_tested$rej, c(T, T, F, T))
})

test_that("example from paper works as multiarm", {
  design <- make_example_multiarm()

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  reallocated_t <- (1 / (2 / 35)) / (1 / (2 / 35) + 1 / (1 / 52 + 1 / 53))
  ad_t <- c(1, reallocated_t, 1, reallocated_t)
  design_adj <- multiarm_drop_arms(
    design,
    c(1, 3),
    n_cont_2 = c(0, 53),
    n_treat_2 = c(0, 52, 0, 52)
  ) |>
    cer_adapt(weights = c(0, 0.5, 0, 0.5))

  expect_equal(design_adj$ad_t, ad_t)

  design_tested <- cer_final_test(design_adj, c(NA, 0.0111, NA, 0.0234))
  expect_equal(design_tested$rej, c(T, T, F, T))
})

test_that("warnings and errors are handled", {
  design <- make_example_design()

  expect_error(
    cer_final_test(design, c(NA, 0.0111, NA, 0.0234)),
    class = "wrong_sequence_no_interim_test"
  )

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  reallocated_t <- (1 / (2 / 35)) / (1 / (2 / 35) + 1 / (1 / 52 + 1 / 53))
  ad_t <- c(1, reallocated_t, 1, reallocated_t)
  design_adj <- cer_drop_hypotheses(
    design,
    c(TRUE, FALSE, TRUE, FALSE),
    adapt_bounds = FALSE
  ) |>
    cer_adapt(weights = c(0, 0.5, 0, 0.5), time = ad_t, adapt_bounds = FALSE)

  expect_error(
    cer_final_test(design_adj, c(NA, 0.0111, NA, 0.0234)),
    class = "wrong_sequence_bounds_outdated"
  )

  design_tested <- design_adj |>
    cer_adapt_bounds() |>
    cer_final_test(c(NA, 0.0111, NA, 0.0234))

  expect_warning(
    cer_final_test(design_tested, c(NA, 0.05, NA, 0.05)),
    class = "overwrites_final_result"
  )
})
