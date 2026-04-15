test_that("can adapt n", {
  design <- make_example_mame() |>
    cer_interim_test(c(
      0.00045,
      0.0952,
      0.0225,
      0.1104,
      0.00045,
      0.0952,
      0.0225,
      0.1104
    ))

  n_subgroups_2 <- design$n_subgroups
  n_subgroups_2$n <- n_subgroups_2$n * 2

  design_ad <- mame_adapt_n(design, n_subgroups_2 = n_subgroups_2)
  expect_equal(design_ad$ad_correlation, design$correlation)
  expect_equal(design_ad$ad_t, rep(1 / 3, 8))
})

test_that("paper_example works", {
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
    n_control = 35,
    n_arms = 35,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as,
    names = c("H1", "H2", "H3", "H4")
  ) |>
    cer_interim_test(c(
      0.00045,
      0.0952,
      0.0225,
      0.1104
    ))

  n_subgroups_2 <- rbind(
    data.frame(arm = "control", n = 53),
    data.frame(arm = "A1", n = 0),
    data.frame(arm = "A2", n = 52)
  )

  des_ad <- des |>
    mame_drop_arms("A1") |>
    mame_adapt_n(n_subgroups_2 = n_subgroups_2)

  ad_m <- rbind(
    c(0, 0, 0, 0),
    c(0, 0, 0, 1),
    c(0, 0, 0, 0),
    c(0, 1, 0, 0)
  )
  reallocated_t <- (1 / (2 / 35)) / (1 / (2 / 35) + 1 / (1 / 52 + 1 / 53))
  expect_equal(unname(des_ad$ad_t), c(1, reallocated_t, 1, reallocated_t))
  expect_equal(unname(des_ad$ad_test_m), ad_m)
})

test_that("basic example dropping groups", {
  des <- make_example_mame() |>
    cer_interim_test(c(
      0.00045,
      0.0952,
      0.0225,
      0.1104,
      0.00045,
      0.0952,
      0.0225,
      0.1104
    ))

  des_ad <- des |> mame_drop_groups("HPV+")
  ad_m <- rbind(
    c(0, 1 / 2, 1 / 2, 0, 0, 0, 0, 0),
    c(1 / 2, 0, 0, 1 / 2, 0, 0, 0, 0),
    c(1 / 3, 2 / 3, 0, 0, 0, 0, 0, 0),
    c(2 / 3, 1 / 3, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0)
  )

  expect_equal(unname(des_ad$ad_weights), c(0.5, 0.5, 0, 0, 0, 0, 0, 0))
  expect_equal(unname(des_ad$ad_test_m), ad_m)
})


test_that("basic example dropping arms", {
  des <- make_example_mame() |>
    cer_interim_test(c(
      0.00045,
      0.0952,
      0.0225,
      0.1104,
      0.00045,
      0.0952,
      0.0225,
      0.1104
    ))

  des_ad <- des |> mame_drop_arms("arm2")
  #fmt: skip
  ad_m <- rbind(
    c(0    ,  0,1 / 2, 0,1 / 2,  0,      0,0),
    c(0    ,  0,0    , 0,0    ,  0,      0,0),
    c(1 / 3,  0,0    , 0,2 / 3,  0,      0,0),
    c(0    ,  0,0    , 0,0    ,  0,      0,0),
    c(1 / 2,  0,0    , 0,0    ,  0,  1 / 2,0),
    c(0    ,  0,0    , 0,0    ,  0,      0,0),
    c(2 / 3,  0,0    , 0,1 / 3,  0,      0,0),
    c(0    ,  0,0    , 0,0    ,  0,      0,0)
  )

  expect_equal(unname(des_ad$ad_weights), c(0.6, 0, 0, 0, 0.4, 0, 0, 0))
  expect_equal(unname(des_ad$ad_test_m), ad_m)
})

test_that("basic example dropping endpoints", {
  des <- make_example_mame() |>
    cer_interim_test(c(
      0.00045,
      0.0952,
      0.0225,
      0.1104,
      0.00045,
      0.0952,
      0.0225,
      0.1104
    ))

  des_ad <- des |> mame_drop_endpoints("sec")
  ad_m <- rbind(
    c(0, 1 / 3, 0, 0, 1 / 3, 1 / 3, 0, 0),
    c(1 / 3, 0, 0, 0, 1 / 3, 1 / 3, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0),
    c(1 / 3, 1 / 3, 0, 0, 0, 1 / 3, 0, 0),
    c(1 / 3, 1 / 3, 0, 0, 1 / 3, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0)
  )

  expect_equal(unname(des_ad$ad_weights), c(0.35, 0.35, 0, 0, 0.15, 0.15, 0, 0))
  expect_equal(unname(des_ad$ad_test_m), ad_m)
})
