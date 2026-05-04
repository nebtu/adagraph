test_that("data generation works", {
  design <- make_example_multiarm()
  data_gen <- get_data_gen(
    rbind(c(1, 0.5), c(0.5, 1)),
    rbind(
      c(1, 0, 0.5, 0),
      c(0, 1, 0, 0.5),
      c(0.5, 0, 1, 0),
      c(0, 0.5, 0, 1)
    ),
    c(1, 0, 0, 0),
    100,
    100
  )

  data <- data_gen(1000, design)

  expect_equal(dim(data), c(1000, 4))
  expect_lt(mean(data[, 1]), 0.05)

  data_gen_2 <- get_data_gen_2(
    rbind(c(1, 0.5), c(0.5, 1)),
    rbind(
      c(1, 0, 0.5, 0),
      c(0, 1, 0, 0.5),
      c(0.5, 0, 1, 0),
      c(0, 0.5, 0, 1)
    ),
    c(1, 0, 0, 0)
  )

  design_ad <- design |>
    cer_interim_test(data[1, ]) |>
    multiarm_drop_arms(
      2,
      n_cont_2 = c(50, 25),
      n_treat_2 = c(100, NA, 2000, 50)
    )

  data2 <- data_gen_2(100, design_ad)

  expect_equal(dim(data2), c(100, 4))
  expect_true(all(is.na(data2[, 2])))
})

test_simulation_workflow <- function() {
  design <- make_example_multiarm()
  corr_treatment <- rbind(
    c(1, 0, 0.5, 0),
    c(0, 1, 0, 0.5),
    c(0.5, 0, 1, 0),
    c(0, 0.5, 0, 1)
  )
  corr_control <- rbind(c(1, 0.5), c(0.5, 1))
  eff <- c(0, 0, 0, 0)

  data_gen <- get_data_gen(
    corr_control,
    corr_treatment,
    eff,
    100,
    100
  )

  adaptation <- function(design) {
    design |> multiarm_drop_arms(1)
  }

  data_gen_2 <- get_data_gen_2(corr_control, corr_treatment, eff)

  results_df <- sim_trial(
    design,
    10,
    10,
    adaptation,
    data_gen,
    data_gen_2,
    include_designs = TRUE
  )

  expect_equal(nrow(results_df), 100)
  #expect_equal(results_df$p_1, results_df$interim_p_1)
}

test_that("simulation workflow works", {
  test_simulation_workflow()
})

test_that("simulation workflow works in future", {
  local_future_plan(future::sequential)
  test_simulation_workflow()
})

test_that("simulation workflow in parallel", {
  local_future_plan(future::multisession)
  test_simulation_workflow()
})

test_that("simulation works with trial_design", {
  design <- make_example_trial()

  design <- make_example_trial()
  corr_endpoints <- rbind(c(1, 0.5), c(0.5, 1))
  #fmt: skip
  effect_sizes <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, prim = 0, sec = 0.2, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, prim = 0, sec = 0.2, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, prim = 1, sec = 0.6, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, prim = 1, sec = 0.6, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, prim = 0, sec = 0.2, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, prim = 0, sec = 0.2, check.names = FALSE)
  )

  data_gen <- get_trial_data_gen(
    corr_endpoints,
    effect_sizes,
    binary_endpoints = "sec"
  )

  ad_n_table <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, n = 80, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, n = 80, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, n = 0, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, n = 0, check.names = FALSE)
  )
  adaptation <- function(design) {
    design |> trial_drop_arms("arm2") |> trial_adapt_n(ad_n_table = ad_n_table)
  }

  data_gen_2 <- get_trial_data_gen(
    corr_endpoints,
    effect_sizes,
    binary_endpoints = "sec",
    second_stage = TRUE
  )

  results_df <- sim_trial(
    design,
    10,
    10,
    adaptation,
    data_gen,
    data_gen_2, #
    include_designs = TRUE
  )

  expect_equal(nrow(results_df), 100)
  expect_true(all(results_df[, c("p_2", "p_4", "p_6", "p_8")] == 1))
})
