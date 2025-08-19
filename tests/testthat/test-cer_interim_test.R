test_that("example from paper works", {
  design <- make_example_design()

  p_val <- c(0.00045, 0.0952, 0.0225, 0.1104)
  design_tested <- cer_interim_test(design, p_val)

  expect_equal(design_tested$rej_interim, c(TRUE, FALSE, FALSE, FALSE))

  expect_equal(design_tested$p_values_interim, p_val)

  expected_cer <- c(
    0.05942545,
    0.21788263,
    0.14148870,
    0.07015712,
    0.07015712,
    0.11166979,
    0.11166979,
    1.00000000,
    1.00000000,
    1.00000000,
    1.00000000,
    1.00000000,
    1.00000000,
    1.00000000,
    1.00000000
  )
  expect_equal(round(design_tested$cer_vec, 8), expected_cer)
  #NOTE: this depends on the order of the intersection hypotheses to be the same to pass
})

test_that("warnings and errors are handled", {
  design <- make_example_design()

  p_val <- c(0.00045, 0.0952, 0.0225, 0.1104)
  design_tested <- cer_interim_test(design, p_val)

  expect_warning(
    design_tested |> cer_interim_test(p_val),
    class = "overwrites_interim_result"
  )

  reallocated_t <- (1 / (2 / 35)) / (1 / (2 / 35) + 1 / (1 / 52 + 1 / 53))
  ad_t <- c(1, reallocated_t, 1, reallocated_t)
  design_adj <- cer_drop_hypotheses(
    design_tested,
    c(TRUE, FALSE, TRUE, FALSE)
  ) |>
    cer_adapt(weights = c(0, 0.5, 0, 0.5), t = ad_t)
  design_tested <- cer_final_test(design_adj, c(NA, 0.0111, NA, 0.0234))

  expect_warning(
    design_tested |> cer_interim_test(c(0.00045, 0.0952, 0.0225, 0.1104)),
    class = "wrong_sequence_after_final"
  )
})
