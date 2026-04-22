test_that("intersection_hypotheses works as expected", {
  design <- make_example_design()

  expect_snapshot_value(intersection_hypotheses(design), style = "deparse")

  expect_snapshot_output(print(intersection_hypotheses(design)))

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))
  design_adj <- cer_drop_hypotheses(design, 1)
  design_tested <- cer_final_test(design_adj, c(NA, 0.0111, NA, 0.0234))

  expect_snapshot_value(
    intersection_hypotheses(design_tested),
    style = "deparse"
  )

  expect_snapshot_output(print(intersection_hypotheses(design_tested)))
})
