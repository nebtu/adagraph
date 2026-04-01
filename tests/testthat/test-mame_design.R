test_that("example stays constant", {
  des <- make_example_mame()
  des$alpha_spending_f <- NULL
  expect_snapshot_value(des, style = "json2", tolerance = 1e-5)
})

test_that("same as multiarm design", {
  m <- rbind(
    H1 = c(0, 1 / 2, 1 / 2, 0),
    H2 = c(1 / 2, 0, 0, 1 / 2),
    H3 = c(0, 1, 0, 0),
    H4 = c(1, 0, 0, 0)
  )
  weights <- c(1 / 2, 1 / 2, 0, 0)
  t <- 0.5
  alpha <- 0.025
  as <- function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))

  # n_subgroups <- rbind(
  #   data.frame(arm = "control", n = 70),
  #   data.frame(arm = "prim", n = 70),
  #   data.frame(arm = "sec", n = 70)
  # )

  des <- mame_design(
    arms = 2,
    endpoints = 2,
    subgroups = 0,
    n_control = 70,
    n_treatments = 70,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as,
    names = c("H1", "H2", "H3", "H4")
  )

  #TODO: not completely the same ofc
  expect_equal(des, make_example_multiarm())
})
