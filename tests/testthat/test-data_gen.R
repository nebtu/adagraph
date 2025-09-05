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
