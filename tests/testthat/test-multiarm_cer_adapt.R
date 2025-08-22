test_that("redistributing n works", {
  expect_equal(
    redistribute_n(100, t = 1 / 2),
    50
  )

  expect_equal(
    redistribute_n(100, t = 0.329),
    68
  )

  expect_equal(
    redistribute_n(c(100, 100), t = 1 / 2, which_drop = 1),
    c(0, 100)
  )

  expect_equal(
    redistribute_n(c(100, 100, 100), t = 1 / 2, which_drop = 1),
    c(0, 75, 75)
  )

  expect_equal(
    redistribute_n(c(101, 100, 100), t = 1 / 2, which_drop = 1),
    c(0, 76, 75)
  )

  expect_equal(
    redistribute_n(c(180, 50, 100), t = 1 / 2, which_drop = 1),
    c(0, 55, 110)
  )

  expect_equal(
    redistribute_n(c(181, 50, 100), t = 1 / 2, which_drop = 1),
    c(0, 55, 111)
  )

  expect_equal(
    redistribute_n(
      c(181, 50, 100),
      t = 1 / 2,
      which_drop = 1,
      simple_redistribute = TRUE
    ),
    c(0, 56, 110)
  )

  n <- 2 * sample.int(1000, 10, replace = TRUE)
  which_drop <- sample(c(TRUE, FALSE), 1000, replace = TRUE)

  expect_equal(sum(redistribute_n(n, t = 1 / 2, which_drop = 1)), sum(n) / 2)

  expect_equal(
    redistribute_n(100, n1 = 32),
    68
  )

  expect_equal(
    redistribute_n(c(100, 100), n1 = c(30, 50), which_drop = 1),
    c(0, 120)
  )

  expect_equal(
    redistribute_n(c(100, 50, 100), n1 = c(10, 25, 50), which_drop = 1),
    c(0, 55, 110)
  )
})

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
