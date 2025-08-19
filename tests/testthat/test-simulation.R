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
})

test_that("simulation workflow works", {
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

  adaption <- function(design) {
    design |> multiarm_drop_arms(1)
  }

  data_gen_2 <- get_data_gen_2(corr_control, corr_treatment, eff)

  results_df <- sim_trial(design, 10, 10, adaption, data_gen, data_gen_2)

  expect_equal(nrow(results_df), 100)
})
