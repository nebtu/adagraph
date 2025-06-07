test_that("example from paper works", {
  design <- make_example_design()

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  expect_equal(design$rej_interim, c(TRUE, FALSE, FALSE, FALSE))

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
  expect_equal(round(design$cer_vec, 8), expected_cer)
  #note that this depends on the order of the intersection hypotheses to be the same to pass
})
