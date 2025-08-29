test_that("print.adagraph_design produces stable output", {
  design <- adagraph_design(
    correlation = rbind(c(1, NA), c(NA, 1)),
    weights = c(0.5, 0.5),
    alpha = 0.05,
    test_m = rbind(c(0, 1), c(1, 0))
  )

  expect_snapshot_output(print(design))
})

test_that("print.cer_design produces stable output across lifecycle", {
  # Initial design
  design <- make_example_design()
  expect_snapshot_output(print(design))

  # After interim test
  p_val <- c(0.00045, 0.0952, 0.0225, 0.1104)
  design_i <- cer_interim_test(design, p_val)
  expect_snapshot_output(print(design_i))

  # After adaptation (change weights only; bounds recalculated)
  design_a <- cer_adapt(
    design_i,
    weights = c(1 / 3, 2 / 3, 0, 0),
    adapt_bounds = TRUE
  )
  expect_snapshot_output(print(design_a))

  # After final test
  p_final <- c(0.20, 0.011, 0.50, 0.030)
  design_f <- cer_final_test(design_a, p_final)
  expect_snapshot_output(print(design_f))
})

test_that("print.multiarm_cer_design produces stable output across lifecycle", {
  # Initial design
  design <- make_example_multiarm()
  expect_snapshot_output(print(design))

  # After interim test
  p_val <- c(0.00045, 0.0952, 0.0225, 0.1104)
  design_i <- cer_interim_test(design, p_val)
  expect_snapshot_output(print(design_i))

  # After adaptation via multiarm drop of an arm and sample-size reallocation
  design_a <- multiarm_drop_arms(
    design_i,
    arms = 1,
    n_cont_2 = c(60, 60),
    n_treat_2 = c(0, 60, 60, 60)
  )
  expect_snapshot_output(print(design_a))

  # After final test (NA for dropped arm)
  p_final <- c(NA, 0.012, 0.40, 0.025)
  design_f <- cer_final_test(design_a, p_final)
  expect_snapshot_output(print(design_f))
})
