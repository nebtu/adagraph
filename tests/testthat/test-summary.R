test_that("print.summary.cer_design produces stable output across lifecycle", {
  # Initial design
  design <- make_example_design()
  expect_snapshot_output(print(summary(design)))

  # After interim test
  p_val <- c(0.00045, 0.0952, 0.0225, 0.1104)
  design_i <- cer_interim_test(design, p_val)
  expect_snapshot_output(print(summary(design_i)))

  # After adaptation (change weights only; bounds recalculated)
  design_a <- cer_adapt(
    design_i,
    weights = c(1 / 3, 2 / 3, 0, 0),
    adapt_bounds = TRUE
  )
  expect_snapshot_output(print(summary(design_a)))

  # After final test
  p_final <- c(0.20, 0.011, 0.50, 0.030)
  design_f <- cer_final_test(design_a, p_final)
  expect_snapshot_output(print(summary(design_f)))
})

test_that("print.summary.multiarm_cer_design produces stable output across lifecycle", {
  # Initial design
  design <- make_example_multiarm()
  expect_snapshot_output(print(summary(design)))

  # After interim test
  p_val <- c(0.00045, 0.0952, 0.0225, 0.1104)
  design_i <- cer_interim_test(design, p_val)
  expect_snapshot_output(print(summary(design_i)))

  # After adaptation via multiarm drop of an arm and sample-size reallocation
  design_a <- multiarm_drop_arms(
    design_i,
    arms = 1,
    n_cont_2 = c(60, 60),
    n_treat_2 = c(0, 60, 60, 60)
  )
  expect_snapshot_output(print(summary(design_a)))

  # After final test (NA for dropped arm)
  p_final <- c(NA, 0.012, 0.40, 0.025)
  design_f <- cer_final_test(design_a, p_final)
  expect_snapshot_output(print(summary(design_f)))
})

test_that("print.summary.mame_design produces stable output across lifecycle", {
  # Initial design
  design <- make_example_mame()
  expect_snapshot_output(print(summary(design)))

  # After interim test
  p_val <- c(
    0.00045,
    0.0952,
    0.0225,
    0.1104,
    0.00045,
    0.0952,
    0.0225,
    0.1104
  )
  design_i <- cer_interim_test(design, p_val)
  expect_snapshot_output(print(summary(design_i)))

  n_subgroups <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, n = 100, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, n = 100, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, n = 140, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, n = 100, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, n = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, n = 0, check.names = FALSE)
  )
  # After adaptation via new n_subgroups
  design_a <- design_i |>
    mame_drop_arms("arm2") |>
    mame_adapt_n(
      ad_n_subgroups = n_subgroups
    )

  expect_snapshot_output(print(summary(design_a)))

  p_final <- c(
    0.00105,
    NA,
    0.0425,
    NA,
    0.00145,
    NA,
    0.0125,
    NA
  )

  design_f <- cer_final_test(design_a, p_final)
  expect_snapshot_output(print(summary(design_f)))
})
