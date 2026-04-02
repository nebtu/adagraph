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

  des <- mame_design(
    arms = 2,
    endpoints = 2,
    subgroups = 0,
    n_control = 70,
    n_arms = 70,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as,
    names = c("H1", "H2", "H3", "H4")
  )
  des_multiarm <- make_example_multiarm()

  #elements that are supposed to be different
  # and alpha_spending_f, because it's environment can be different
  ignore_mame <- c(
    "alpha_spending_f",
    "n_subgroups",
    "names_arms",
    "names_endpoints",
    "names_subgroups"
  )
  ignore_multiarm <- c(
    "alpha_spending_f",
    "controls",
    "treatment_assoc",
    "n_controls",
    "n_treatments"
  )

  expect_mapequal(
    des[!(names(des) %in% ignore_mame)],
    des_multiarm[!(names(des_multiarm) %in% ignore_multiarm)]
  )
})
