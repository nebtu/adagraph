test_that("get_trial_data_gen returns correct format", {
  design <- make_example_trial()
  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))
  #fmt: skip
  effect_sizes <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE)
  )

  data_gen <- get_trial_data_gen(corr_endpoints, effect_sizes)
  data <- data_gen(100, design)

  expect_true(all(data >= 0 & data <= 1))
  # should have n rows and k hypotheses columns
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 8)
})

test_that("p-values are uniform under the null", {
  design <- make_example_trial()
  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))
  #fmt: skip
  effect_sizes <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE)
  )

  data_gen <- get_trial_data_gen(corr_endpoints, effect_sizes)

  withr::with_seed(42, {
    data <- data_gen(2000, design)
  })

  # under the null, p-values should be approximately uniform
  # mean should be close to 0.5
  for (j in seq_len(ncol(data))) {
    expect_gt(mean(data[, j]), 0.35)
    expect_lt(mean(data[, j]), 0.65)
  }
})

test_that("effect sizes produce small p-values", {
  design <- make_example_trial()
  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))
  #fmt: skip
  effect_sizes <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, prim = 0.5, sec = 0.5, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, prim = 0.5, sec = 0.5, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE)
  )

  data_gen <- get_trial_data_gen(corr_endpoints, effect_sizes)

  withr::with_seed(42, {
    data <- data_gen(500, design)
  })

  hyp_assoc <- design[["hyp_assoc"]]
  arm1_prim_total <- which(
    hyp_assoc[, "arm"] == "arm1" &
      hyp_assoc[, "endpoint"] == "prim" &
      hyp_assoc[, "group"] == "Total"
  )
  arm2_prim_total <- which(
    hyp_assoc[, "arm"] == "arm2" &
      hyp_assoc[, "endpoint"] == "prim" &
      hyp_assoc[, "group"] == "Total"
  )

  # arm1 has an effect, so mean p should be small
  expect_lt(mean(data[, arm1_prim_total]), 0.1)
  # arm2 has no effect, so mean p should be around 0.5
  expect_gt(mean(data[, arm2_prim_total]), 0.35)
})

test_that("binary endpoints work correctly", {
  design <- make_example_trial()
  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))
  #fmt: skip
  effect_sizes <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, prim = 0, sec = 0.3, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, prim = 0, sec = 0.3, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, prim = 0, sec = 0.6, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, prim = 0, sec = 0.6, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, prim = 0, sec = 0.3, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, prim = 0, sec = 0.3, check.names = FALSE)
  )

  data_gen <- get_trial_data_gen(
    corr_endpoints,
    effect_sizes,
    binary_endpoints = "sec"
  )

  withr::with_seed(42, {
    data <- data_gen(500, design)
  })

  expect_equal(nrow(data), 500)
  expect_equal(ncol(data), nrow(design[["hyp_assoc"]]))

  # all p-values should be in [0, 1]
  expect_true(all(data >= 0 & data <= 1))

  hyp_assoc <- design[["hyp_assoc"]]
  # arm1 has higher response rate on sec, should have small p-values
  arm1_sec_total <- which(
    hyp_assoc[, "arm"] == "arm1" &
      hyp_assoc[, "endpoint"] == "sec" &
      hyp_assoc[, "group"] == "Total"
  )
  # arm2 has same response rate as control, should be uniform
  arm2_sec_total <- which(
    hyp_assoc[, "arm"] == "arm2" &
      hyp_assoc[, "endpoint"] == "sec" &
      hyp_assoc[, "group"] == "Total"
  )

  expect_lt(mean(data[, arm1_sec_total]), 0.15)
  expect_gt(mean(data[, arm2_sec_total]), 0.35)
})

test_that("second_stage = TRUE uses ad_n_table", {
  design <- make_example_trial()
  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))
  #fmt: skip
  effect_sizes <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE)
  )

  data_gen_2 <- get_trial_data_gen(
    corr_endpoints,
    effect_sizes,
    second_stage = TRUE
  )

  # without ad_n_table set, should error
  expect_error(data_gen_2(10, design))

  # set ad_n_table and it should work
  design[["ad_n_table"]] <- design[["n_table"]]
  data <- data_gen_2(50, design)

  expect_equal(nrow(data), 50)
  expect_equal(ncol(data), nrow(design[["hyp_assoc"]]))
})

test_that("empty subgroup works as expected", {
  design <- make_example_trial()
  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))

  #fmt: skip
  ad_n_table <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, n = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, n = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, n = 0, check.names = FALSE)
  )

  design <- design |>
    cer_interim_test(c(
      0.00045,
      0.0952,
      0.0225,
      0.1104,
      0.00045,
      0.0952,
      0.0225,
      0.1104
    )) |>
    trial_drop_groups("HPV+") |>
    trial_adapt_n(ad_n_table = ad_n_table)

  #fmt: skip
  effect_sizes <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE)
  )
  data_gen_2 <- get_trial_data_gen(
    corr_endpoints,
    effect_sizes,
    second_stage = TRUE
  )

  data <- data_gen_2(50, design)

  expect_equal(nrow(data), 50)
  expect_true(all(is.na(data[, 5:8])))
  expect_true(all(is.numeric(data[, 1:4])))
})

test_that("subgroup p-values use correct subsets", {
  design <- make_example_trial()
  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))

  # effect only in HPV+ subgroup for arm1
  #fmt: skip
  effect_sizes <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, prim = 0.8, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, prim = 0, sec = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, prim = 0, sec = 0, check.names = FALSE)
  )

  data_gen <- get_trial_data_gen(corr_endpoints, effect_sizes)

  withr::with_seed(42, {
    data <- data_gen(500, design)
  })

  hyp_assoc <- design[["hyp_assoc"]]

  # arm1 prim in HPV+ subgroup should have very small p-values
  arm1_prim_hpv <- which(
    hyp_assoc[, "arm"] == "arm1" &
      hyp_assoc[, "endpoint"] == "prim" &
      hyp_assoc[, "group"] == "HPV+"
  )
  # arm1 prim in Total should also be somewhat small (diluted effect)
  arm1_prim_total <- which(
    hyp_assoc[, "arm"] == "arm1" &
      hyp_assoc[, "endpoint"] == "prim" &
      hyp_assoc[, "group"] == "Total"
  )

  expect_lt(mean(data[, arm1_prim_hpv]), 0.05)
  # total includes non-HPV+ with no effect, so weaker signal
  expect_gt(mean(data[, arm1_prim_total]), mean(data[, arm1_prim_hpv]))
})

test_that("no subgroups design works", {
  m <- rbind(
    c(0, 1 / 2, 1 / 2, 0),
    c(1 / 2, 0, 0, 1 / 2),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )
  as <- function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))

  design <- trial_design(
    arms = 2,
    endpoints = 2,
    n_control = 100,
    n_arms = 100,
    weights = c(1 / 2, 1 / 2, 0, 0),
    t = 0.5,
    alpha = 0.025,
    test_m = m,
    alpha_spending_f = as
  )

  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))
  effect_sizes <- data.frame(
    arm = c("control", "A1", "A2"),
    E1 = c(0, 0.4, 0),
    E2 = c(0, 0, 0)
  )

  data_gen <- get_trial_data_gen(corr_endpoints, effect_sizes)

  withr::with_seed(42, {
    data <- data_gen(500, design)
  })

  expect_equal(nrow(data), 500)
  expect_equal(ncol(data), 4)
  expect_true(all(data >= 0 & data <= 1))

  hyp_assoc <- design[["hyp_assoc"]]
  # A1 E1 should have small p-values
  a1_e1 <- which(hyp_assoc[, "arm"] == "A1" & hyp_assoc[, "endpoint"] == "E1")
  expect_lt(mean(data[, a1_e1]), 0.1)
  # A2 E1 should be uniform
  a2_e1 <- which(hyp_assoc[, "arm"] == "A2" & hyp_assoc[, "endpoint"] == "E1")
  expect_gt(mean(data[, a2_e1]), 0.35)
})
