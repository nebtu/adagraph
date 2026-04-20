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

test_that("data generation for binary endpoints works", {
  design <- make_example_multiarm()

  data_gen <- get_data_gen(
    corr_control = rbind(c(1, 0.5), c(0.5, 1)),
    corr_treatment = rbind(
      c(1, 0, 0.5, 0),
      c(0, 1, 0, 0.5),
      c(0.5, 0, 1, 0),
      c(0, 0.5, 0, 1)
    ),
    eff = c(0, 1, 0, 0),
    100,
    100,
    binary = c(3, 4),
    bin_con_resp = 0.1,
    bin_treat_resp = c(0.1, 0.4)
  )

  set.seed(1)
  data <- data_gen(1000, design)

  expect_equal(dim(data), c(1000, 4))
  expect_lt(mean(data[, 2]), 0.05)
  expect_lt(mean(data[, 4]), 0.05)

  data_gen_2 <- get_data_gen_2(
    rbind(c(1, 0.5), c(0.5, 1)),
    rbind(
      c(1, 0, 0.5, 0),
      c(0, 1, 0, 0.5),
      c(0.5, 0, 1, 0),
      c(0, 0.5, 0, 1)
    ),
    c(1, 0, 0, 0),
    binary = c(3, 4),
    bin_con_resp = 0.1,
    bin_treat_resp = c(0.1, 0.4)
  )

  design_ad <- design |>
    cer_interim_test(data[961, ]) |>
    multiarm_drop_arms(
      c(1, 3),
      n_cont_2 = c(25, 50),
      n_treat_2 = c(NA, 100, NA, 2000)
    )

  data2 <- data_gen_2(100, design_ad)

  expect_equal(dim(data2), c(100, 4))
  expect_true(all(is.na(data2[, c(1, 3)])))
  expect_lt(mean(data[, 2]), 0.05)
  expect_lt(mean(data[, 4]), 0.05)
})

test_that("handle low p-values", {
  design <- make_example_multiarm()

  p_val <- c(1e-2, 1e-2, 1e-20, 1e-2)

  design_ad <- design |>
    cer_interim_test(p_val) |>
    multiarm_drop_arms(
      c(1, 2),
      n_cont_2 = c(25, 50),
      n_treat_2 = c(NA, NA, 100, 2000)
    )

  expect_equal(unname(design_ad$ad_weights), c(0, 0, 0.5, 0.5))

  #NOTE: Here we can not reject even with p value 0 because of floating point
  #arithmetic reasons. A more accurate calculation of the bound would be
  #necessary, but within these limits, the calculation of the bound does now
  #work
  #design_ad |> cer_final_test(c(NA, NA, 0, 0.5))
})
