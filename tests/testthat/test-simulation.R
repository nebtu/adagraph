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

  results_df <- sim_trial(
    design,
    10,
    10,
    adaption,
    data_gen,
    data_gen_2,
    include_designs = TRUE
  )

  expect_equal(nrow(results_df), 100)
  #expect_equal(results_df$p_1, results_df$interim_p_1)
})

test_that("simulation workflow works when only 1 hypothesis remains", {
  as <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))

  design <- multiarm_cer_design(
    controls = 1,
    treatment_assoc = c(1, 1),
    n_controls = 50,
    n_treatments = 50,
    weights = c(0.5, 0.5),
    alpha = 0.05,
    test_m = rbind(c(0, 1), c(1, 0)),
    alpha_spending_f = as,
    t = 0.5
  )
  adaption <- function(design) {
    design |> multiarm_drop_arms(1)
  }

  data_gen <- get_data_gen(
    matrix(1),
    rbind(
      c(1, 0.5),
      c(0.5, 1)
    ),
    c(0, 0),
    100,
    100
  )

  data_gen_2 <- get_data_gen_2(
    matrix(1),
    rbind(
      c(1, 0.5),
      c(0.5, 1)
    ),
    c(0, 0)
  )

  df <- sim_trial(
    design,
    10,
    10,
    adaption,
    data_gen,
    data_gen_2
  )

  #NOTE: this might change if we change how dropped hypotheses and p-values are
  #      handled
  expect_true(all(df$p_1 == 1))
})
