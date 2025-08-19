test_that("dropping arms works", {
  design <- make_example_multiarm()
  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  design_adj <- multiarm_drop_arms(
    design,
    1,
    n_cont_2 = c(60, 60),
    n_treat_2 = c(0, 60, 60, 60)
  )

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
  expect_equal(design_adj$ad_t[1], 1)

  expect_equal(
    design_adj$ad_n_treatments,
    c(50, 110, 110, 110)
  )

  expect_equal(
    design_adj$ad_n_controls,
    c(110, 110)
  )
  expect_equal(
    design_adj$n_cont_2,
    c(60, 60)
  )
  expect_equal(
    design_adj$n_treat_2,
    c(0, 60, 60, 60)
  )

  expect_true(
    all(names(design) %in% names(design_adj))
  )

  # Test that adaptions is properly set
  expect_true(design_adj$adaptions)

  # Test everything else is unchanged
  expect_equal(
    design_adj[setdiff(names(design), "adaptions")],
    design[setdiff(names(design), "adaptions")]
  )
})
